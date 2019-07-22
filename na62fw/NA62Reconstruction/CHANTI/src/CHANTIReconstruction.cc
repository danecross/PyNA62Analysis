//-------------------------------------------------------------------------------
// Created by Paolo Massarotti 2014 (paolo.massarotti@cern.ch)
//
//-------------------------------------------------------------------------------
/// \class CHANTIReconstruction
/// \Brief
/// Main CHANTI reconstruction: starts from CHANTI digi hits which are first ordered and merged, if it is possible, in physics hits (one RecoHit for
/// two electronic channel cossespondig to low and hig threshold. Reco Hits are collected in Clusters (group of hits near in time and position).
/// Hits belonging to Time Clusters are flagged as inelstic or muon cluster and represent the CHANTI Candidates.
/// Digi hits come from single ToT board channel.
/// \EndBrief
///
/// \Detailed
/// It makes use of base class NA62VReconstruction and of classes in NA62MC/CHANTI/Persistency (TRecoCHANTIEvent, TRecoCHANTICandidate, TRecoCHANTIHit).
/// The CHANTI reconstruction basic parameters are set in NA62Reconstruction/config/CHANTI.conf
/// \n
/// =========
/// \n
/// CHANTI.conf
/// \n
/// =========
/// \n
/// Description of parameters can be found in CHANTI.conf, here the parameters you may want to change to perform your own reconstruction:
/// \n
/// TimeWindow=10       --> used to build TimeCluster for hits inside TimeWindow width (+/- 0.5*TimeWindow)
/// \n
/// PlanesT0 (ns) --> used to center Leading Time of TDC around 0.
/// \n
/// =======================================================================================================================================================
/// \n
/// CHANTI Reconstruction
/// \n
/// Digi hits are ordered, according to time and Electronic Channel ID, and merged, if it is possible.
/// Then Digi hits are ordered, according to time and Physics Channel ID.
/// Two Electronic Digi, corresponding to the same Physics channel, give ONE Reco Hit.
/// \n
/// CHANTI Candidate
/// \n
/// A CHANTI Candidate can contain a view cluster or a xy cluster.
/// A view cluster is a group of adjacent bars (in the same plane and view) fired togheter.
/// A xy cluster is a pair of view clusters on the same plane but different view, with at least two bars crossing.
/// \n
/// Each candidate contain the following information:
/// XYMult : Type of cluster flag
/// XPCharge / YPCharge : Sum of the (Pseudo) charge collected in all the bars of the X/Y cluster (0 if none)
/// XPos / Ypos : X/Y position of the Center of (Pseudo) charge (-999 if none)
/// \n
/// The Pseudo charge is the charge calculated by the hit Time Of Treshold.
/// XYMult is 0 if the candidate is a view cluster, or 1 if it is a clean xy cluster.
/// XYMult can be >1 when there is an ambiguity on the xy cluster association (ghosts).
/// It contain the number of possible combination; for example 2 particle could fire
/// 2 cluster in the X view and 2 cluster in the Y view. Moreover, each X cluster could cross each Y cluster.
/// In this situation it will be generated 4 candidate containing all the X-Y combination, each with XYMult=4.
/// \n
/// /// T0 and Slewing correction modified by Marco Mirra (marco.mirra@cern.ch)
/// /// CHANTI Candidate modified by Domenico Di Filippo (domenico.difilippo@cern.ch)
/// \n
/// \EndDetailed

#include "Riostream.h"
#include "math.h"
#include "TDCBRawDecoder.hh"
#include "TString.h"
#include "TRegexp.h"
#include "TDCEvent.hh"
#include "CHANTIGeometry.hh"
#include "CHANTIReconstruction.hh"
#include "TCHANTIDigi.hh"
#include "TRecoCHANTIEvent.hh"
#include "TSlimRecoCHANTIEvent.hh"
#include "vector"
#include "cmath"
#include "CHANTIChannel.hh"
#include "TVector3.h"
#include "TSpecialTriggerEvent.hh"
#include "TTDCBSpecialTrigger.hh"
#include "TRandom3.h"
#include "NA62Buffer.hh"
#include "NA62BufferProto.hh"
#include "NA62ConditionsService.hh"
#include <vector>

typedef std::vector<TRecoCHANTIHit*> chanti_cluster;

class chanti_cluster_alias {
  public:
    std::vector<chanti_cluster*> all_cluster; // list of all the view cluster that generate the x-y cluster
    std::vector<chanti_cluster*> x_match_cluster; // x cluster of an xy associated cluster
    std::vector<chanti_cluster*> y_match_cluster; // y cluster of an xy associated cluster
    // x_match_cluster and y_match_cluster have the same size
    // If cluster.size()==1 (same of x_match_cluster.size()==0) it is a single-view (not-copuled) cluster
};

class chanti_clusterization {
  public:
    std::vector<chanti_cluster> view_cluster;
    std::vector<chanti_cluster_alias> xy_couple;
};

bool chanti_hit_is_good(TRecoCHANTIHit* hit, Bool_t TrailingOn) {
  if (hit->GetQualityFlag() == 5) return false;
  if (hit->GetThresholdFlag() == 1) return false;
  if (hit->GetTime() == -999) return false;
  if (TrailingOn && hit->GetTimeWidth() <= 0) return false;
  return true;
}

bool chanti_hit_are_in_time(TRecoCHANTIHit* h1, TRecoCHANTIHit* h2){

  if (TMath::Abs(h1->GetTime() - h2->GetTime()) < 10) return true;
  return false;
}

double chanti_hit_pseudo_charge(TRecoCHANTIHit* h){
  Double_t TimeWidth = (h->GetTimeWidth()>=0) ? h->GetTimeWidth():0.;
  return TMath::Exp(TimeWidth/38.0);
}

// TODO : PROPOSAL : put this in GetSideID !!!
short int chanti_hit_SMART_side_id(TRecoCHANTIHit* h1){

  int sid = h1->GetSideID();
  if (sid == 0) {
    int bid = TMath::Abs(h1->GetBarID());
    if (bid >= 05 && bid <= 11) sid = -1;
  }
  sid = -sid;

  return sid;
}

bool chanti_hit_are_in_same_plane(TRecoCHANTIHit* h1, TRecoCHANTIHit* h2){
  if (h1->GetPlaneID() != h2->GetPlaneID()) return false;
  return true;
}

bool chanti_hit_bar_are_in_same_view(TRecoCHANTIHit* h1, TRecoCHANTIHit* h2){

  if (!chanti_hit_are_in_same_plane(h1, h2)) return false;
  if (h1->GetRingType() != h2->GetRingType()) return false;
  return true;
}

bool chanti_bid_are_close(int b1, int b2){
  if (b1<0) b1 = -b1;
  if (b2<0) b2 = -b2;
  if (b1 == b2) return false;
  if (b1 < b2-1) return false;
  if (b1 > b2+1) return false;
  return true;
}

bool chanti_hit_bar_are_close(TRecoCHANTIHit* h1, TRecoCHANTIHit* h2){

  if (!chanti_hit_bar_are_in_same_view(h1, h2)) return false;
  if (!chanti_bid_are_close(h1->GetBarID(), h2->GetBarID())) return false;
  int s1 = chanti_hit_SMART_side_id(h1);
  int s2 = chanti_hit_SMART_side_id(h2);
  if (s1 != 0 && s2 != 0 && s1 != s2) return false;
  return true;
}

bool chanti_hit_bar_is_vertical(TRecoCHANTIHit* V){
  return (V->GetRingType() == kX);
}

bool chanti_hit_bar_is_horizontal(TRecoCHANTIHit* H){
  return (H->GetRingType() == kY);
}

bool chanti_hit_bar_is_cut(TRecoCHANTIHit* C){
  if (chanti_hit_bar_is_vertical(C)) return false;
  int b = C->GetBarID();
  if (b < 0) b = -b;
  return (b==11 || b==5);
}

bool chanti_hit_bar_is_long(TRecoCHANTIHit* L){
  int b = L->GetBarID();
  if (b < 0) b = -b;
  if (b<=4) return true;
  if (b>=12) return true;
  return false;
}

bool chanti_hit_bar_face_hole(TRecoCHANTIHit* H){
  if (chanti_hit_bar_is_long(H)) return false;
  if (chanti_hit_bar_is_cut(H)) return false;
  return true;
}

bool chanti_hit_bar_is_high(TRecoCHANTIHit* I){
  int b = I->GetBarID();
  if (b < 0) b = -b;
  if (b >= 8) return true;
  return false;
}

bool chanti_hit_bar_is_low(TRecoCHANTIHit* I){
  int b = I->GetBarID();
  if (b < 0) b = -b;
  if (b <= 8) return true;
  return false;
}

bool chanti_hit_bar_cover_up(TRecoCHANTIHit* U){
  if (chanti_hit_bar_is_vertical(U)) return chanti_hit_bar_is_high(U);
  int ss = chanti_hit_SMART_side_id(U);
  if (ss == 0) return true;
  if (ss == 1) return true;
  return false;
}

bool chanti_hit_bar_cover_down(TRecoCHANTIHit* D){
  if (chanti_hit_bar_is_vertical(D)) return chanti_hit_bar_is_low(D);
  int ss = chanti_hit_SMART_side_id(D);
  if (ss == 0) return true;
  if (ss == -1) return true;
  return false;
}

bool chanti_hit_bar_cover_left(TRecoCHANTIHit* L){
  if (chanti_hit_bar_is_horizontal(L)) return chanti_hit_bar_is_high(L);
  int ss = chanti_hit_SMART_side_id(L);
  if (ss == 0) return true;
  if (ss == 1) return true;
  return false;
}

bool chanti_hit_bar_cover_right(TRecoCHANTIHit* R){
  if (chanti_hit_bar_is_horizontal(R)) return chanti_hit_bar_is_low(R);
  int ss = chanti_hit_SMART_side_id(R);
  if (ss == 0) return true;
  if (ss == -1) return true;
  return false;
}

bool chanti_hit_bar_are_crossing(TRecoCHANTIHit* H, TRecoCHANTIHit* V){

  if (!chanti_hit_are_in_same_plane(H, V)) return false;
  if (chanti_hit_bar_are_in_same_view(H, V)) return false;
  if (chanti_hit_bar_is_horizontal(V)) {
    TRecoCHANTIHit* tmp = V;
    V = H; H = tmp;
  }

  if (chanti_hit_bar_face_hole(H) && chanti_hit_bar_face_hole(V)) return false;

  bool both_cover_up = (chanti_hit_bar_cover_up(H) && chanti_hit_bar_cover_up(V));
  bool both_cover_down = (chanti_hit_bar_cover_down(H) && chanti_hit_bar_cover_down(V));
  bool both_cover_left = (chanti_hit_bar_cover_left(H) && chanti_hit_bar_cover_left(V));
  bool both_cover_right = (chanti_hit_bar_cover_right(H) && chanti_hit_bar_cover_right(V));

  if (both_cover_up && both_cover_left) return true;
  if (both_cover_up && both_cover_right) return true;
  if (both_cover_down && both_cover_left) return true;
  if (both_cover_down && both_cover_right) return true;
  return false;
}

void chanti_cluster_generate(TRecoCHANTIEvent* rce, chanti_clusterization& clu, Bool_t TrailingON){
  for (int h = 0; h < rce->GetNHits(); h += 1) {
    TRecoCHANTIHit* hit = static_cast<TRecoCHANTIHit*>( rce->GetHit(h));
    if (!chanti_hit_is_good(hit,TrailingON)) continue;
    chanti_cluster newcluster;
    newcluster.push_back(hit);
    clu.view_cluster.push_back(newcluster);
  }
}

bool chanti_cluster_are_view_compatible(chanti_cluster& A, chanti_cluster& B){
  for (UInt_t a=0; a<A.size(); a+=1) for (UInt_t b=0; b<B.size(); b+=1) {
    if (!chanti_hit_are_in_same_plane(A[a], B[b])) continue;
    if (!chanti_hit_bar_are_in_same_view(A[a], B[b])) continue;
    if (!chanti_hit_bar_are_close(A[a], B[b])) continue;
    if (!chanti_hit_are_in_time(A[a], B[b])) continue;
    return true;
  }
  return false;
}

bool chanti_cluster_join_step(chanti_clusterization& clu){
  // note: return 'true' when it needs a new iteration
  bool joined = false;
  for (UInt_t i=0; i<clu.view_cluster.size(); i+=1) {
    if (clu.view_cluster[i].size() < 1) continue;
    for(UInt_t j=i+1; j<clu.view_cluster.size(); j+=1) {
      if (clu.view_cluster[j].size() < 1) continue;
      if (chanti_cluster_are_view_compatible(clu.view_cluster[i], clu.view_cluster[j])) {
        // JOIN and return for next step !
        for (UInt_t b=0; b<clu.view_cluster[j].size(); b+=1) {
          clu.view_cluster[i].push_back(clu.view_cluster[j][b]);
        }
        clu.view_cluster[j].clear();
        joined = true;
      }
    }
  }
  return joined;
}

void chanti_cluster_join(chanti_clusterization& clu){
  while (chanti_cluster_join_step(clu));
}

void chanti_cluster_remove_empty(chanti_clusterization& clu){
  chanti_clusterization newclusterization;
  for (UInt_t i=0; i<clu.view_cluster.size(); i+=1)
    if (clu.view_cluster[i].size() > 0)
      newclusterization.view_cluster.push_back(clu.view_cluster[i]);
  clu = newclusterization;
}

void chanti_xymatch_generate(chanti_clusterization& clu){
  for (UInt_t i=0; i< clu.view_cluster.size(); i+=1) {
    chanti_cluster_alias xyc;
    xyc.all_cluster.push_back(&(clu.view_cluster[i]));
    clu.xy_couple.push_back(xyc);
  }
}

bool chanti_cluster_are_xy_compatible(chanti_cluster& A, chanti_cluster& B){
  if (A.size() == 0 || B.size() == 0)
    return false;
  if (A[0]->GetRingType() == B[0]->GetRingType())
    return false;
  if (A[0]->GetPlaneID() != B[0]->GetPlaneID())
    return false;
  for (UInt_t hi=0; hi<A.size(); hi+=1)
    for (UInt_t hj=0; hj<B.size(); hj+=1)
      if (chanti_hit_bar_are_crossing(A[hi], B[hj]))
        if (chanti_hit_are_in_time(A[hi], B[hj]))
          return true;
  return false;
}

bool chanti_clusterization_xy_check(chanti_clusterization& clu, int i, int j){
  for (UInt_t I=0; I< clu.xy_couple[i].all_cluster.size(); I +=1)
    for (UInt_t J=0; J< clu.xy_couple[j].all_cluster.size(); J +=1)
      if (chanti_cluster_are_xy_compatible(*(clu.xy_couple[i].all_cluster[I]), *(clu.xy_couple[j].all_cluster[J])))
        return true;
  return false;
}

bool chanti_xymatch_join_step(chanti_clusterization& clu){
  bool joined = false;
  for (UInt_t i=0; i<clu.xy_couple.size(); i+=1) {
    if (clu.xy_couple[i].all_cluster.size() < 1) continue;
    for(UInt_t j=i+1; j<clu.xy_couple.size(); j+=1) {
      if (clu.xy_couple[j].all_cluster.size() < 1) continue;
      if (chanti_clusterization_xy_check(clu, i, j)) {
        // JOIN and return for next step !
        for (UInt_t b=0; b<clu.xy_couple[j].all_cluster.size(); b+=1) {
          clu.xy_couple[i].all_cluster.push_back(clu.xy_couple[j].all_cluster[b]);
        }
        clu.xy_couple[j].all_cluster.clear();
        joined = true;
      }
    }
  }
  return joined;
}

void chanti_xymatch_join(chanti_clusterization& clu){
  while (chanti_xymatch_join_step(clu));
}

void chanti_xymatch_remove_empty(chanti_clusterization& clu){
  std::vector< chanti_cluster_alias > newxyc;
  for (UInt_t i=0; i<clu.xy_couple.size(); i+=1) if (clu.xy_couple[i].all_cluster.size() > 0)
    newxyc.push_back(clu.xy_couple[i]);
  clu.xy_couple = newxyc;
}

void chanti_clusterization_generate_ghost(chanti_clusterization& clu){
  for (UInt_t i=0; i< clu.xy_couple.size(); i+=1){
    for (UInt_t j=0; j< clu.xy_couple[i].all_cluster.size(); j+=1){
      for (UInt_t k=j+1; k< clu.xy_couple[i].all_cluster.size(); k+=1){
        if (chanti_cluster_are_xy_compatible(*(clu.xy_couple[i].all_cluster[j]), *(clu.xy_couple[i].all_cluster[k]))) {
          if ((*(clu.xy_couple[i].all_cluster[j]))[0]->GetRingType() == kX && (*(clu.xy_couple[i].all_cluster[k]))[0]->GetRingType() == kY) {
            clu.xy_couple[i].x_match_cluster.push_back(clu.xy_couple[i].all_cluster[j]);
            clu.xy_couple[i].y_match_cluster.push_back(clu.xy_couple[i].all_cluster[k]);
          }
          if ((*(clu.xy_couple[i].all_cluster[j]))[0]->GetRingType() == kY && (*(clu.xy_couple[i].all_cluster[k]))[0]->GetRingType() == kX) {
            clu.xy_couple[i].x_match_cluster.push_back(clu.xy_couple[i].all_cluster[k]);
            clu.xy_couple[i].y_match_cluster.push_back(clu.xy_couple[i].all_cluster[j]);
          }
        }
      }
    }
  }
}

void chanti_clusterize(TRecoCHANTIEvent* rce, chanti_clusterization& clu, Bool_t TrailingON){
  // single-view cluster
  chanti_cluster_generate(rce, clu, TrailingON);
  chanti_cluster_join(clu);
  chanti_cluster_remove_empty(clu);
  // x-y cluster
  chanti_xymatch_generate(clu);
  chanti_xymatch_join(clu);
  chanti_xymatch_remove_empty(clu);
  // ghost
  chanti_clusterization_generate_ghost(clu);
}

void chanti_candidate_data_set(TRecoCHANTICandidate*ccand, chanti_cluster*xcluster, chanti_cluster*ycluster){

  // TODO : partial-merge to ProcessEvent for optimization (calculate cluster data only one time)

  Double_t wei=0.0, pos=0.0, timwei=0.0, tim=0.0;
  for (UInt_t i=0; i< xcluster->size(); i+=1) {
    TRecoCHANTIHit* hit = (*xcluster)[i];
    if (hit->GetRingType() != kX) continue;
    double w = chanti_hit_pseudo_charge(hit);
    wei += w;
    pos += hit->GetX() * w;
    timwei += w;
    tim += hit->GetTime() * w;
  }

  if (wei == 0) {
    ccand->SetXPCharge(-999);
    ccand->SetXPos(-999);
  } else {
    ccand->SetXPCharge(wei);
    ccand->SetXPos(pos/wei);
  }

  wei=pos=0;
  for (UInt_t i=0; i< ycluster->size(); i+=1) {
    TRecoCHANTIHit* hit = (*ycluster)[i];
    if (hit->GetRingType() != kY) continue;
    double w = chanti_hit_pseudo_charge(hit);
    wei += w;
    pos += hit->GetY() * w;
    timwei += w;
    tim += hit->GetTime() * w;
  }

  if (wei == 0) {
    ccand->SetYPCharge(-999);
    ccand->SetYPos(-999);
  } else {
    ccand->SetYPCharge(wei);
    ccand->SetYPos(pos/wei);
  }

  if (timwei == 0) {
    ccand->SetTime(-999);
  } else {
    ccand->SetTime(tim/timwei);
  }
}

const double chanti_fiber_distance = CHANTIGeometry::GetInstance()->GetTriangleBase()/2;

double chanti_hit_distance_to_fiber(double pos){
  if (pos < 0) pos = -pos;
  pos = pos - (((int)(pos / chanti_fiber_distance)) * chanti_fiber_distance);
  if (pos > chanti_fiber_distance / 2.0) pos = pos - chanti_fiber_distance;
  return pos;
}

bool chanti_hit_is_on_fiber(double pos) {
  pos = chanti_hit_distance_to_fiber(pos);
  if (pos > -0.0001 && pos < 0.0001) return true;
  return false;
}

static unsigned int seed_sprnd = 1; // initial seed must be non-zero

double chanti_hit_on_fiber_spread(){
  TRandom3* chanti_sprnd = new TRandom3();
  chanti_sprnd->SetSeed(seed_sprnd);
  double x,y;
  do {
    x = chanti_sprnd->Uniform(-chanti_fiber_distance/2.0, chanti_fiber_distance/2.0);
    y = TMath::Sin(TMath::Pi() * x / chanti_fiber_distance);
    y = y * y;
  } while (chanti_sprnd->Uniform(0.0, 1.0) < y);
  delete chanti_sprnd;
  seed_sprnd++;
  return x;
}

double chanti_hit_corrected_distance_to_fiber(double pos){
  if (chanti_hit_is_on_fiber(pos))
    pos += chanti_hit_on_fiber_spread();
  return pos;
}

int chanti_get_hit_index(TRecoVEvent* rce, TRecoCHANTIHit* recoHit) {
  for (int h = 0; h < rce->GetNHits(); h += 1)
    if (recoHit == (TRecoCHANTIHit*)(*(rce->GetHits()))[h])
      return h;
  std::cout << "[CHANTIReconstruction] Error: Hit not found!"<<rce->GetID()<< std::endl;
  return -1;
}

CHANTIReconstruction::CHANTIReconstruction( TFile* HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "CHANTI", ConfigFileName){

  // Initialize variables and histos
  fRecoEvent = new TRecoCHANTIEvent();
  fSlimRecoEvent = new TSlimRecoCHANTIEvent();
  fGeometry = CHANTIGeometry::GetInstance();
  fStep           = fGeometry->GetTriangleBase()/2;
  fGlobalShift    = fGeometry->GetSquareLength()/2 - fStep;
  fNPlanes = -999;
  fNRings = -999;
  fEffPlaneIndex = -999;
  fEnableSlewingCorr = kTRUE;
  fEnableTrailing = kTRUE;
  ResetHistograms();
  fEvaluateSlewingCorr = kFALSE;
  fPhotonsNumber = 0;
  fEnergyEdge = 0;
  ParseConfFile(ConfigFileName);
  fNTriggersPerBurst = 0;
}

void CHANTIReconstruction::Init(NA62VReconstruction* MainReco) {

  //common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  //if(((NA62Reconstruction *)fMainReco)->GetFillTimesEnabled()) fChannelHistograms = kTRUE;
  for (Int_t ich=0; ich<fNChannels; ich++) {
    Int_t ChID = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRemap(ich);
    fChannels[ich] = new CHANTIChannel(ChID, ich, fChannelHistograms);
    //static_cast<CHANTIChannel*>(fChannels[ich])->SetNewMinWidth(fNewMinWidth);
    //static_cast<CHANTIChannel*>(fChannels[ich])->SetNewMaxWidth(fNewMaxWidth);
  }
  ReadT0s();
  ReadChannelInfo();
  InitHistograms();
}

/********************************************//**
 * Destructor
 ************************************************/

CHANTIReconstruction::~CHANTIReconstruction(){
  DeleteHistograms();
  if(fPhotonsNumber){
    delete [] fPhotonsNumber;
    fPhotonsNumber = 0;
  }
  if(fEnergyEdge){
    delete [] fEnergyEdge;
    fEnergyEdge = 0;
  }
}

void CHANTIReconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  fNTriggersPerBurst = 0;
}

void CHANTIReconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
}

// Read CHANTI reconstruction parameters from a configuration file
void CHANTIReconstruction::ParseConfFile (TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }
  else std::cout << "[CHANTIReconstruction] Reading config file: " << ConfFileName << std::endl;

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("NPlanes")) {
      fNPlanes = TString(Line(TRegexp("[0-9]"))).Atoi();
      fNRings = 2*fNPlanes;
      continue;
    }
    else if (Line.BeginsWith("BurstLength")) {
      TObjArray *l = Line.Tokenize(" ");
      fBurstLength = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("ThFileName")) {
      TObjArray *l = Line.Tokenize(" ");
      fThFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("EffiPlane")) {
      fEffPlaneIndex = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnableSlewingCorrection")) {
      fEnableSlewingCorr = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnableTrailing")) {
      fEnableTrailing = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EvaluateSlewingCorrection")) {
      fEvaluateSlewingCorr = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("SlewingCorrectionFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fSlewingFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("MeanNphFileName")) {
      TObjArray *l = Line.Tokenize(" ");
      fMeanNphFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("SignalParametersFileName")) {
      TObjArray *l = Line.Tokenize(" ");
      fSignalParametersFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if(Line.BeginsWith("RingsMCToF=")){
      TObjArray * l = Line.Tokenize(" ");
      for(int j = 0; j < l->GetEntries() - 1; j++)
      {
        fRingsMCToF[j] = static_cast<TObjString*>(l->At(j+1))->GetString().Atof();
      }
      delete l;
    }
  }
  confFile.close();

  if (fEvaluateSlewingCorr) {
    fChannelHistograms = kTRUE;
    fEnableSlewingCorr = kFALSE; // disable existing slewing correction
  }

  if (!fThFileName.Length()) {
    std::cout << "[CHANTIReconstruction] Error: threshold file not defined"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (!fSlewingFileName.Length()) {
    std::cout << "[CHANTIReconstruction] Error: slewing correction file not defined" << std::endl;
    exit(kWrongConfiguration);
  }
  if (!fMeanNphFileName.Length()) {
    std::cout << "[CHANTIReconstruction] Error: mean number of photons file not defined" << std::endl;
    exit(kWrongConfiguration);
  }
  if (!fSignalParametersFileName.Length()) {
    std::cout << "[CHANTIReconstruction] Error: signal parameters file not defined" << std::endl;
    exit(kWrongConfiguration);
  }

  //Read parameters for the digitization of the signal
  NA62ConditionsService::GetInstance()->Open(fMeanNphFileName);
  Int_t Ndimension;
  Int_t Nbins;
  Double_t DummyNumber;
  NA62ConditionsService::GetInstance()->Get(fMeanNphFileName) >> Ndimension;
  NA62ConditionsService::GetInstance()->Get(fMeanNphFileName) >> Nbins;
  fNbinEnergy = Nbins;
  fPhotonsNumber = new Double_t[Nbins-1];
  fEnergyEdge = new Double_t[Nbins-1];
  NA62ConditionsService::GetInstance()->Get(fMeanNphFileName) >> DummyNumber;
  for (int iEneEdge = 0; iEneEdge<Nbins-1; iEneEdge++) {
    NA62ConditionsService::GetInstance()->Get(fMeanNphFileName) >> fEnergyEdge[iEneEdge];
  }
  for (int iNph = 0; iNph<Nbins-1; iNph++) {
    NA62ConditionsService::GetInstance()->Get(fMeanNphFileName) >> fPhotonsNumber[iNph];
  }
  NA62ConditionsService::GetInstance()->Close(fMeanNphFileName);

  NA62ConditionsService::GetInstance()->Open(fSignalParametersFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fSignalParametersFileName))) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("SlopeAmplNph")) {
	  TObjArray *l = Line.Tokenize(" ");
      fSlopeAmplNph = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("SigmaAmplSPE")) {
	  TObjArray *l = Line.Tokenize(" ");
      fSigmaAmplSPE = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("TauFall")) {
	  TObjArray *l = Line.Tokenize(" ");
      fTauFall = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("MeanNfr")) {
      TObjArray *l = Line.Tokenize(" ");
      fMeanNfr = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("SigmaNfr")) {
	  TObjArray *l = Line.Tokenize(" ");
      fSigmaNfr = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("Hysteresis")) {
	  TObjArray *l = Line.Tokenize(" ");
      fHysteresis = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("FiberLightSpeed")) {
      TObjArray *l = Line.Tokenize(" ");
      fFiberLightSpeed = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("ReflectionFactorBoard")) {
      TObjArray *l = Line.Tokenize(" ");
      fReflectionFactorBoard = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("SlopeEnergyNph")) {
	  TObjArray *l = Line.Tokenize(" ");
      fSlopeEnergyNph = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("OffsetEnergyNph")) {
	  TObjArray *l = Line.Tokenize(" ");
      fOffsetEnergyNph = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("CutOffFrequency")) {
	  TObjArray *l = Line.Tokenize(" ");
      fCutOffFrequency = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("ZeroFrequency")) {
	  TObjArray *l = Line.Tokenize(" ");
      fZeroFrequency = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      delete l;
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fSignalParametersFileName);
}

// Read threshold and slewing corrections
void CHANTIReconstruction::ReadChannelInfo() {
  TString Line;

  NA62ConditionsService::GetInstance()->Open(fThFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fThFileName))) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("ThMap_")) {
      Int_t group = TString(Line(TRegexp("[0-9]+"))).Atoi();
      TObjArray *l = Line.Tokenize(" ");
      for (Int_t i=0; i<16; i++) {
        if (16*group+i < fNChannels) {
          static_cast<CHANTIChannel*>(fChannels[16*group+i])->SetThreshold(static_cast<TObjString*>(l->At(i+1))->GetString().Atof());
        }
      }
      delete l;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fThFileName);

  NA62ConditionsService::GetInstance()->Open(fSlewingFileName);
  // ------------------------------------------------------------------------
  // The SlewingCorr.dat file has 4 column:
  //  SeqID    p0   p1   p2
  //-------------------------------------------------------------------------
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fSlewingFileName))){
    if (Line.BeginsWith("#")) continue;
    else{
      if(!fEnableSlewingCorr) continue;
      TObjArray *l = Line.Tokenize(" ");
      Int_t Ch = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
      Double_t p0 = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      Double_t p1 = static_cast<TObjString*>(l->At(2))->GetString().Atof();
      Double_t p2 = static_cast<TObjString*>(l->At(3))->GetString().Atof();
      if (Ch < fNChannels) static_cast<CHANTIChannel*>(fChannels[Ch])->SetSlewingCorrectionParameters(p0,p1,p2);
      delete l;
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fSlewingFileName);
}

//////////////////////////////////////////////////////////

TDetectorVEvent* CHANTIReconstruction::Trigger(TDetectorVEvent* tEvent, Event* /*tGenEvent*/){
  return tEvent;
}

TRecoVEvent* CHANTIReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  if(tEvent->IsA() == TSpecialTriggerEvent::Class()) {
    //Variable for the layers visualization
    double HalfHoleXmin = 47.5;
    double HalfHoleY1 = 33+fStep;
    double HalfHoleXmax = 47.5+fStep;
    double HalfHoleY2max = 2*33;
    double HalfHoleY2min = 33;
    //int NSpecialTrigger = 0;
    //////////////////////////////////////
    for(Int_t iSpecTrig=0;iSpecTrig<tEvent->GetNHits();iSpecTrig++){
      if(!isL0EOB(tEvent->GetTriggerType())) continue; //skip SOB
      TTDCBSpecialTrigger * SpecTrig = reinterpret_cast<TTDCBSpecialTrigger *>(tEvent->GetHit(iSpecTrig));
      if(!SpecTrig) continue;
      if(isL0EOB(SpecTrig->GetTriggerType())){ //get TEL62 EOB information
        PrimCounter* ChannelCounts = SpecTrig->GetCounter("CHANNEL_COUNT_L");
        if(ChannelCounts){
          CHANTIChannelID* CHANTICh = new CHANTIChannelID();
          for(UInt_t iEntry=0;iEntry<ChannelCounts->GetNEntries();iEntry++){
            Int_t ChannelID = ChannelCounts->GetChannelID(iEntry);
            Int_t NCounts   = ChannelCounts->GetValue(iEntry);
            if(ChannelID<0) continue; // Masked channel
            //cout << "CHANTI EOB counts: " << ChannelID << " " << NCounts << std::endl;
            CHANTICh->DecodeChannelID(ChannelID);
            Int_t PlaneID   = CHANTICh->GetPlaneID();
            Int_t RingType  = CHANTICh->GetRingType();
            Int_t SideID    = CHANTICh->GetSideID();
            Int_t BarID     = CHANTICh->GetBarID();
            Int_t RingID    = CHANTICh->GetRingID();
            Bool_t IsLowThr = (ChannelID%2==0);
            if (IsLowThr){
              if (RingType == kX){
                Double_t X = abs(BarID)*fStep - fGlobalShift;
                fHPositionLow[RingID][1]->Fill(X,NCounts);
                if (BarID%2!=0){
                  if (TMath::Abs(X)>HalfHoleXmax){
                    fHViewX1THRL[PlaneID][1]->Fill(X,-91.25,NCounts);
                    fHViewX1THRL[PlaneID][1]->Fill(X,0.,NCounts);
                    fHViewX1THRL[PlaneID][1]->Fill(X,91.25,NCounts);
                  } else {
                    if(SideID==kPositive) fHViewX1THRL[PlaneID][1]->Fill(X,91.25,NCounts);
                    else fHViewX1THRL[PlaneID][1]->Fill(X,-91.25,NCounts);
                  }
                } else {
                  if (TMath::Abs(X)>HalfHoleXmin){
                    fHViewX2THRL[PlaneID][1]->Fill(X,-91.25,NCounts);
                    fHViewX2THRL[PlaneID][1]->Fill(X,0.,NCounts);
                    fHViewX2THRL[PlaneID][1]->Fill(X,91.25,NCounts);
                  } else {
                    if(SideID==kPositive) fHViewX2THRL[PlaneID][1]->Fill(X,91.25,NCounts);
                    else fHViewX2THRL[PlaneID][1]->Fill(X,-91.25,NCounts);
                  }
                }
              } else {
                Double_t Y = abs(BarID)*fStep - fGlobalShift;
                fHPositionLow[RingID][1]->Fill(Y,NCounts);
                if (BarID%2==0){
                  if (TMath::Abs(Y)>HalfHoleY1){
                    fHViewY1THRL[PlaneID][1]->Fill(98.75,Y,NCounts);
                    fHViewY1THRL[PlaneID][1]->Fill(23.75,Y,NCounts);
                    fHViewY1THRL[PlaneID][1]->Fill(-23.75,Y,NCounts);
                    fHViewY1THRL[PlaneID][1]->Fill(-98.75,Y,NCounts);
                  } else {
                    if(SideID==kPositive) fHViewY1THRL[PlaneID][1]->Fill(98.75,Y,NCounts);
                    else fHViewY1THRL[PlaneID][1]->Fill(-98.75,Y,NCounts);
                  }
                } else {
                  if (TMath::Abs(Y)>HalfHoleY2max){
                    fHViewY2THRL[PlaneID][1]->Fill(98.75,Y,NCounts);
                    fHViewY2THRL[PlaneID][1]->Fill(23.75,Y,NCounts);
                    fHViewY2THRL[PlaneID][1]->Fill(-23.75,Y,NCounts);
                    fHViewY2THRL[PlaneID][1]->Fill(-98.75,Y,NCounts);
                  } else if (TMath::Abs(Y)<HalfHoleY2min){
                    if(SideID==kPositive) fHViewY2THRL[PlaneID][1]->Fill(98.75,Y,NCounts);
                    else fHViewY2THRL[PlaneID][1]->Fill(-98.75,Y,NCounts);
                  } else {
                    if(SideID==kPositive){
                      fHViewY2THRL[PlaneID][1]->Fill(98.75,Y,NCounts);
                      fHViewY2THRL[PlaneID][1]->Fill(23.75,Y,NCounts);
                    } else {
                      fHViewY2THRL[PlaneID][1]->Fill(-98.75,Y,NCounts);
                      fHViewY2THRL[PlaneID][1]->Fill(-23.75,Y,NCounts);
                    }
                  }
                }
              }
            } else if (RingType == kX){
              Double_t X = abs(BarID)*fStep - fGlobalShift;
              fHPositionHigh[RingID][1]->Fill(X,NCounts);
              if (BarID%2!=0){
                if (TMath::Abs(X)>HalfHoleXmax){
                  fHViewX1THRH[PlaneID][1]->Fill(X,-91.25,NCounts);
                  fHViewX1THRH[PlaneID][1]->Fill(X,0.,NCounts);
                  fHViewX1THRH[PlaneID][1]->Fill(X,91.25,NCounts);
                } else {
                  if(SideID==kPositive) fHViewX1THRH[PlaneID][1]->Fill(X,91.25,NCounts);
                  else fHViewX1THRH[PlaneID][1]->Fill(X,-91.25,NCounts);
                }
              } else {
                if (TMath::Abs(X)>HalfHoleXmin){
                  fHViewX2THRH[PlaneID][1]->Fill(X,-91.25,NCounts);
                  fHViewX2THRH[PlaneID][1]->Fill(X,0.,NCounts);
                  fHViewX2THRH[PlaneID][1]->Fill(X,91.25,NCounts);
                } else {
                  if(SideID==kPositive) fHViewX2THRH[PlaneID][1]->Fill(X,91.25,NCounts);
                  else fHViewX2THRH[PlaneID][1]->Fill(X,-91.25,NCounts);
                }
              }
            } else {
              Double_t Y = abs(BarID)*fStep - fGlobalShift;
              fHPositionHigh[RingID][1]->Fill(Y,NCounts);
              if (BarID%2==0){
                if (TMath::Abs(Y)>HalfHoleY1){
                  fHViewY1THRH[PlaneID][1]->Fill(98.75,Y,NCounts);
                  fHViewY1THRH[PlaneID][1]->Fill(23.75,Y,NCounts);
                  fHViewY1THRH[PlaneID][1]->Fill(-23.75,Y,NCounts);
                  fHViewY1THRH[PlaneID][1]->Fill(-98.75,Y,NCounts);
                } else {
                  if(SideID==kPositive) fHViewY1THRH[PlaneID][1]->Fill(98.75,Y,NCounts);
                  else fHViewY1THRH[PlaneID][1]->Fill(-98.75,Y,NCounts);
                }
              } else {
                if (TMath::Abs(Y)>HalfHoleY2max){
                  fHViewY2THRH[PlaneID][1]->Fill(98.75,Y,NCounts);
                  fHViewY2THRH[PlaneID][1]->Fill(23.75,Y,NCounts);
                  fHViewY2THRH[PlaneID][1]->Fill(-23.75,Y,NCounts);
                  fHViewY2THRH[PlaneID][1]->Fill(-98.75,Y,NCounts);
                } else if (TMath::Abs(Y)<HalfHoleY2min){
                  if(SideID==kPositive) fHViewY2THRH[PlaneID][1]->Fill(98.75,Y,NCounts);
                  else fHViewY2THRH[PlaneID][1]->Fill(-98.75,Y,NCounts);
                } else {
                  if(SideID==kPositive) {
                    fHViewY2THRH[PlaneID][1]->Fill(98.75,Y,NCounts);
                    fHViewY2THRH[PlaneID][1]->Fill(23.75,Y,NCounts);
                  } else {
                    fHViewY2THRH[PlaneID][1]->Fill(-98.75,Y,NCounts);
                    fHViewY2THRH[PlaneID][1]->Fill(-23.75,Y,NCounts);
                  }
                }
              }
            }
          }
          delete CHANTICh;
        }
      }
    }
    return 0;
  }

  //common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  for (Int_t ich=0; ich<fNChannels; ich++) fChannels[ich]->Reset();

  fHNTriggersPerBurst->Fill(fRecoEvent->GetBurstID());
  fNTriggersPerBurst++;

  TDCEvent* tCHANTIEvent = static_cast<TDCEvent*>(tEvent);
  InitRecoEvent();
  DeleteDigi(tCHANTIEvent);
  MergeDigi(tCHANTIEvent);
  DigiToReco(tCHANTIEvent);
  if(fEnableSlewingCorr) SlewingCorrection(tCHANTIEvent);
  ModeEvaluation(tCHANTIEvent);

  for(int iRing = 0; iRing < fNRings; iRing++) {
    if(fNHitRing[iRing] > 0) fNFiredRing ++;
  }
  for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
    if(fNHitPlane[iPlane] > 0) fNFiredPlane++;
  }

  FillingHisto(tCHANTIEvent);
  ResoTimeEvaluation(tCHANTIEvent);
  SingleMuonSelection( tCHANTIEvent);
  SingleMuonFillingHisto(tCHANTIEvent);
  SingleMuonResoTimeEvaluation(tCHANTIEvent);
  BarEfficiencyEvaluation(tCHANTIEvent);

  if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
    EfficiencyEvaluation(tCHANTIEvent);
  }

  chanti_clusterization chclu;
  chanti_clusterize(static_cast<TRecoCHANTIEvent*>(fRecoEvent), chclu, fEnableTrailing);

  for (UInt_t c=0; c< chclu.xy_couple.size(); c++) {
    //std::vector<chanti_cluster*>& thecluster = chclu.xy_couple[c].all_cluster;
    if(chclu.xy_couple[c].x_match_cluster.size() != chclu.xy_couple[c].y_match_cluster.size())
      std::cout << "[CHANTIReconstruction] Error: Wrong size CHANTI candidate in X or Y cluster"<< std::endl;
    UInt_t NumberOfGhost = chclu.xy_couple[c].x_match_cluster.size();
    for (UInt_t g=0; g< NumberOfGhost; g++) { // x-y clusters (NumberOfGhost==0 <-> cluster.size()==1)

      TRecoCHANTICandidate* CCand = static_cast<TRecoCHANTICandidate*>(fRecoEvent->AddCandidate());
      if (CCand) {
        for (UInt_t hx=0; hx< (*(chclu.xy_couple[c].x_match_cluster[g])).size(); hx++)
          CCand->AddHit(chanti_get_hit_index( fRecoEvent , (*(chclu.xy_couple[c].x_match_cluster[g]))[hx] ));
        for (UInt_t hy=0; hy< (*(chclu.xy_couple[c].y_match_cluster[g])).size(); hy++)
          CCand->AddHit(chanti_get_hit_index( fRecoEvent , (*(chclu.xy_couple[c].y_match_cluster[g]))[hy] ));
        CCand->SetXYMult(NumberOfGhost);
        chanti_candidate_data_set(CCand, chclu.xy_couple[c].x_match_cluster[g], chclu.xy_couple[c].y_match_cluster[g]);
        //
        // HISTOs
        // TODO : move somewhere else !!!
        Double_t X = chanti_hit_corrected_distance_to_fiber(CCand->GetXPos());
        Double_t Y = chanti_hit_corrected_distance_to_fiber(CCand->GetYPos());
        if(!((X>-95./2 && X<95./2) && (Y>-65./2 && Y<65./2))){
          fHXYCluster->Fill(X,Y, 1.0/CCand->GetXYMult());
          Int_t iPlane= static_cast<TRecoCHANTIHit*>(CCand->GetHit(0))->GetPlaneID();
          if(iPlane>= 0 && iPlane<6)
            fHXYClusterPerPlane[iPlane]->Fill(X, Y, 1.0/CCand->GetXYMult());
          else
            std::cout <<"[CHANTIReconstruction] Error: wrong plane ID in candidate [Event "<<fRecoEvent->GetID()<< "]" << std::endl;
          //
        }
      }
    }

    if (chclu.xy_couple[c].all_cluster.size() == 1) { // single-view clusters (NumberOfGhost==0 <-> cluster.size()==1)
      TRecoCHANTICandidate* CCand = static_cast<TRecoCHANTICandidate*>(fRecoEvent->AddCandidate());
      if (CCand) {
        for (UInt_t h=0; h< (*(chclu.xy_couple[c].all_cluster[0])).size(); h++) {
          CCand->AddHit(chanti_get_hit_index( fRecoEvent , (*(chclu.xy_couple[c].all_cluster[0]))[h] ));
        }
        CCand->SetXYMult(0);
        chanti_candidate_data_set(CCand, chclu.xy_couple[c].all_cluster[0], chclu.xy_couple[c].all_cluster[0]);
      }
    }
  }

  if((fNHitPlane[0] < 1) && (fNHitPlane[1] < 1) && (fNHitPlane[2] < 1) && (fNHitPlane[3] < 1) && (fNHitPlane[4] < 1) && (fNHitPlane[5] < 1)) return fRecoEvent;

  for(int iRing = 0; iRing < fNRings; iRing++){
    fHNHitsForRing[iRing]->Fill(fNHitRing[iRing]);
    fHNHitsForRingVSBurst[iRing]->Fill(fRecoEvent->GetBurstID(), fNHitRing[iRing]);
  }
  for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
    fHNHitsForPlane[iPlane]->Fill(fNHitPlane[iPlane]);
    fHNHitsForPlaneVSBurst[iPlane]->Fill(fRecoEvent->GetBurstID(), fNHitPlane[iPlane]);
  }

  fHNHit->Fill(fNElectronicHit);
  Int_t nROBoards = fRawDecoder->GetDecoder()->GetNROBoards();
  Int_t * nSlots  = fRawDecoder->GetDecoder()->GetNSlots();
  for(int iTell = 0; iTell<nROBoards; iTell++) fHNWordPerTell[iTell]->Fill(fNWordTell[iTell]*1000/ClockPeriod/nSlots[iTell]);
  for(int iTDCB = 0; iTDCB<4*nROBoards; iTDCB++) fHNWordPerTDCB[iTDCB]->Fill(fNWordTDCB[iTDCB]*1000/ClockPeriod/nSlots[iTDCB/4]);
  for(int iTDC = 0; iTDC<16*nROBoards;  iTDC++)  fHNWordPerTDC[iTDC]->Fill(fNWordTDC[iTDC]*1000/ClockPeriod/nSlots[iTDC/16]);

  return fRecoEvent;
}

void CHANTIReconstruction::InitRecoEvent(){

  for(int iPlane = 0; iPlane < fNPlanes; iPlane++) fNHitPlane[iPlane] = 0;
  fSingleMuon = false;
  fNFiredRing = 0;
  fNFiredPlane = 0;
  //fNHit = 0;
  fNElectronicHit = 0;
  Int_t nROBoards = fRawDecoder->GetDecoder()->GetNROBoards();
  for(int iTell = 0; iTell<nROBoards; iTell++) fNWordTell[iTell] = 0;
  for(int iTDCB = 0; iTDCB<4*nROBoards; iTDCB++) fNWordTDCB[iTDCB] = 0;
  for(int iTDC = 0; iTDC<16*nROBoards; iTDC++) fNWordTDC[iTDC] = 0;
  fHXHitEvent1->Reset();
  fHXHitEvent2->Reset();
  fHYHitEvent1->Reset();
  fHYHitEvent2->Reset();
  for(int iRing = 0; iRing < fNRings; iRing++)
  {
    for(int iBar = 0; iBar < 24; iBar++) BarFired[iRing][iBar] = 0;
    fNHitRing[iRing] = 0;
    fHPosition1[iRing]->Reset();
    fHPosition2[iRing]->Reset();

    if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
      fHEffi1Up[iRing]->Reset();
      fHEffi2Up[iRing]->Reset();
      fHEffi1Down[iRing]->Reset();
      fHEffi2Down[iRing]->Reset();
      fHEffiPosition1Up[iRing]->Reset();
      fHEffiPosition2Up[iRing]->Reset();
      fHEffiPosition1Down[iRing]->Reset();
      fHEffiPosition2Down[iRing]->Reset();
      fHEffiHigh1Up[iRing]->Reset();
      fHEffiHigh2Up[iRing]->Reset();
      fHEffiHigh1Down[iRing]->Reset();
      fHEffiHigh2Down[iRing]->Reset();
      fHEffiPositionHigh1Up[iRing]->Reset();
      fHEffiPositionHigh2Up[iRing]->Reset();
      fHEffiPositionHigh1Down[iRing]->Reset();
      fHEffiPositionHigh2Down[iRing]->Reset();
    }

    fHCorrTime[iRing]->Reset();
  }
}

void CHANTIReconstruction::DeleteDigi(TDCEvent * tCHANTIEvent){

  Int_t ChannelID = -999;
  //Double_t Time = -1e28;
  //Double_t TTrail = -1e28;
  Bool_t GoodEvent = true;
  //Bool_t BadEventBefore = false;

  tCHANTIEvent->GetHits()->Sort();

  for(Int_t iDigi = 0 ; iDigi < tCHANTIEvent->GetNHits() ; iDigi++){
    TCHANTIDigi *Digi = static_cast<TCHANTIDigi*>(tCHANTIEvent->GetHits()->At(iDigi));
    if(Digi->GetChannelID() < 100000){
      std::cout <<"[CHANTIReconstruction::DeleteDigi] CHECK THE CH MAP!!! ID = "<< std::endl;
      continue;
    }
    if(GoodEvent){
      ChannelID = Digi->GetChannelID();
      //Time = Digi->GetLeadingEdge();
      //TTrail = Digi->GetTrailingEdge();
    }
    if(iDigi+1 < tCHANTIEvent->GetNHits()){
      TCHANTIDigi *DigiCheck = static_cast<TCHANTIDigi*>(tCHANTIEvent->GetHits()->At(iDigi+1));
      /*if(((Time == DigiCheck->GetLeadingEdge() && Time != -1e+28) ||
        (TTrail == DigiCheck->GetTrailingEdge() && TTrail != -1e+28)) &&
        (abs(DigiCheck->GetChannelID() - ChannelID) != 1))
        {
        cout<<"DEBUG_1 Removing DIGI Channel ID "<<ChannelID<<"		DigiCheck = "<<DigiCheck->GetChannelID()<<endl;
        cout<<"DEBUG_1 Removing DIGI Lead "<<Time<<"		DigiCheck lead = "<<DigiCheck->GetLeadingEdge()<<endl;
        cout<<"DEBUG_1 Removing DIGI trail "<<TTrail<<"		DigiCheck trail = "<<DigiCheck->GetTrailingEdge()<<endl;
        GoodEvent = false;
        BadEventBefore = true;
        tCHANTIEvent->RemoveHit(iDigi);
        iDigi--;
        continue;
        }
        else
        {
        GoodEvent = true;
        if(BadEventBefore) {
        cout<<"DEBUG_2 Removing DIGI Channel ID "<<ChannelID<<"		DigiCheck = "<<DigiCheck->GetChannelID()<<endl;
        cout<<"DEBUG_2 Removing DIGI Lead "<<Time<<"		DigiCheck lead = "<<DigiCheck->GetLeadingEdge()<<endl;
        cout<<"DEBUG_2 Removing DIGI trail "<<TTrail<<"		DigiCheck trail = "<<DigiCheck->GetTrailingEdge()<<endl;
        BadEventBefore = false;
        tCHANTIEvent->RemoveHit(iDigi);
        iDigi--;
        continue;
        }
        }*/
      if((DigiCheck->GetChannelID() == ChannelID) && (Digi->GetLeadingEdge() > -1e20) && (Digi->GetTrailingEdge() > -1e20) && (DigiCheck->GetLeadingEdge() > -1e20) && (DigiCheck->GetTrailingEdge() > -1e20) && (Digi->GetLeadingEdge() < DigiCheck->GetLeadingEdge()) && (Digi->GetTrailingEdge() > DigiCheck->GetLeadingEdge()) && (Digi->GetTrailingEdge() < DigiCheck->GetTrailingEdge()))
      { //leading1 leading2 trailing1 trailing2

        std::cout <<"[CHANTIReconstruction] Bad Digi type 1: leading1 = "<<Digi->GetLeadingEdge()<<" leading2 = "<<DigiCheck->GetLeadingEdge()<< " trailing1 = "<<Digi->GetTrailingEdge()<<" trailing2 = "<<DigiCheck->GetTrailingEdge()<<" Event "<<fRecoEvent->GetID()<<" Channel "<<Digi->GetChannelID()<<" Problems with corresponding TDC"<< std::endl;
        tCHANTIEvent->RemoveHit(iDigi);
        tCHANTIEvent->RemoveHit(iDigi);
        iDigi--;
        continue;
      }
      else if((DigiCheck->GetChannelID() == ChannelID) && (Digi->GetLeadingEdge() > -1e20) && (Digi->GetTrailingEdge() > -1e20) && (DigiCheck->GetLeadingEdge() > -1e20) && (DigiCheck->GetTrailingEdge() > -1e20) && (Digi->GetLeadingEdge() < DigiCheck->GetLeadingEdge()) && (Digi->GetTrailingEdge() > DigiCheck->GetTrailingEdge()))
      { //leading1 leading2 trailing2 trailing1

        std::cout <<"[CHANTIReconstruction] Bad Digi type 2: leading1 = "<<Digi->GetLeadingEdge()<<" leading2 = "<<DigiCheck->GetLeadingEdge()<< " trailing2 = "<<DigiCheck->GetTrailingEdge()<<" trailing1 = "<<Digi->GetTrailingEdge()<<" Event "<<fRecoEvent->GetID()<<" Channel "<<Digi->GetChannelID()<<" Problems with corresponding TDC"<< std::endl;
        tCHANTIEvent->RemoveHit(iDigi);
        tCHANTIEvent->RemoveHit(iDigi);
        iDigi--;
        continue;
      }
      else if((DigiCheck->GetChannelID() == ChannelID) && (Digi->GetLeadingEdge() < -1e20) && (DigiCheck->GetLeadingEdge() > -1e20) && (DigiCheck->GetTrailingEdge() > -1e20) && (DigiCheck->GetLeadingEdge() < Digi->GetTrailingEdge()) && (DigiCheck->GetTrailingEdge() > Digi->GetTrailingEdge()))
      { //leading2 trailing1 trailing2

        std::cout <<"[CHANTIReconstruction] Bad Digi type 3: leading2 = "<<DigiCheck->GetLeadingEdge()<<" trailing1 = "<<Digi->GetTrailingEdge()<< " trailing2 = "<<DigiCheck->GetTrailingEdge()<<" Event "<<fRecoEvent->GetID()<<" Channel "<<Digi->GetChannelID()<<" Problems with corresponding TDC"<< std::endl;

        tCHANTIEvent->RemoveHit(iDigi+1); //no need to decrement iDigi this time
        continue;
      }
      else if((DigiCheck->GetChannelID() == ChannelID) && (Digi->GetLeadingEdge() > -1e20) && (Digi->GetTrailingEdge() > -1e20) && (DigiCheck->GetLeadingEdge() > -1e20) && (Digi->GetLeadingEdge() < DigiCheck->GetLeadingEdge()) && (Digi->GetTrailingEdge() > DigiCheck->GetLeadingEdge()))
      { //leading1 leading2 trailing1

        std::cout <<"[CHANTIReconstruction] Bad Digi type 4: leading1 = "<<Digi->GetLeadingEdge()<<" leading2 = "<<DigiCheck->GetLeadingEdge()<<" trailing1 = "<<Digi->GetTrailingEdge()<<" Event "<<fRecoEvent->GetID()<<" Channel "<<Digi->GetChannelID()<<" Problems with corresponding TDC"<< std::endl;

        tCHANTIEvent->RemoveHit(iDigi);
        tCHANTIEvent->RemoveHit(iDigi);
        iDigi--;
        continue;
      }
    }
  }
}

void CHANTIReconstruction::MergeDigi(TDCEvent * tCHANTIEvent){

  tCHANTIEvent->GetHits()->Sort();
  for(Int_t iDigi = 0 ; iDigi < tCHANTIEvent->GetNHits() ; iDigi++){
    TCHANTIDigi *Digi = static_cast<TCHANTIDigi*>(tCHANTIEvent->GetHits()->At(iDigi));
    if(Digi->GetChannelID() < 100000){
      std::cout <<"[CHANTIReconstruction::MergeDigi] CHECK THE CH MAP!!! ID = "<< std::endl;
      continue;
    }
    //fDigiSortFlag = 1;
    Digi->SetSortFlag(1);
    if(iDigi+1 < tCHANTIEvent->GetNHits()){
      TCHANTIDigi *DigiBis = static_cast<TCHANTIDigi*>(tCHANTIEvent->GetHits()->At(iDigi+1));
      if (DigiBis->GetChannelID() == Digi->GetChannelID()) {
        if((Digi->GetLeadingEdge() > -1e20) && (Digi->GetTrailingEdge()< -1e20) && (DigiBis->GetLeadingEdge()> -1e20) && (DigiBis->GetTrailingEdge()> -1e20) && (DigiBis->GetTrailingEdge() - Digi->GetLeadingEdge() < 120.))
        {//leading leading trailing
          Digi->SetTrailingEdge(DigiBis->GetTrailingEdge());
          tCHANTIEvent->RemoveHit(iDigi+1);
          iDigi--;
        }
        else if((Digi->GetLeadingEdge() > -1e20) && (Digi->GetTrailingEdge()> -1e20) && (DigiBis->GetLeadingEdge() < -1e20) && (Digi->GetLeadingEdge() < DigiBis->GetTrailingEdge()) && (DigiBis->GetTrailingEdge() - Digi->GetLeadingEdge() < 120.))
        {//leading trailing trailing
          Digi->SetTrailingEdge(DigiBis->GetTrailingEdge());
          tCHANTIEvent->RemoveHit(iDigi+1);
          iDigi--;
        }
        else if((Digi->GetLeadingEdge() > -1e20) && (DigiBis->GetLeadingEdge() < -1e20) && (Digi->GetTrailingEdge() < -1e20) && (Digi->GetLeadingEdge() < DigiBis->GetTrailingEdge()) && (DigiBis->GetTrailingEdge() - Digi->GetLeadingEdge() < 120.))//leading1 trailing2
        {
          //cout<<"[CHANTIReconstruction] Error: leading1 trailing2 Event "<<fRecoEvent->GetID()<<" Channel "<<Digi->GetChannelID()<<" Problems with corresponding TDC "<< std::endl;
          Digi->SetTrailingEdge(DigiBis->GetTrailingEdge());
          tCHANTIEvent->RemoveHit(iDigi+1);
          iDigi--;
        }
      }
    }
  }
}

void CHANTIReconstruction::DigiToReco(TDCEvent * tCHANTIEvent){
  Int_t NDigis=0;
  Int_t ThresholdFlag=0;
  Int_t ChannelID;
  Int_t RingID=0;
  Int_t BarID=0;
  //Int_t PlaneID=0;
  Double_t X=0.;
  Double_t Y=0.;
  Double_t Time=0.;
  Double_t TimeWidth;
  Double_t DeltaTime = 0;
  Double_t DeltaWidth = 0;
  Int_t QualityFlag=-1;
  Int_t ROCH;
  Double_t T0_1, T0_2;
  Int_t TellID, TDCBID, TDCID ;
  Int_t BarIndex;
  NDigis = tCHANTIEvent->GetNHits();
  TClonesArray & Digis = (* (tCHANTIEvent->GetHits()));
  Digis.Sort();
  for(Int_t iDigi = 0 ; iDigi < NDigis ; iDigi++){
    TCHANTIDigi *Digi = static_cast<TCHANTIDigi*>(Digis[iDigi]);
    TimeWidth = -999;
    ChannelID = Digi->GetChannelID();
    if(ChannelID>=100000){

      ROCH = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ChannelID);
      if(ROCH<0) {
        std::cout << "[CHANTIReconstruction] Error: wrong RO channel ID!" << std::endl;
        continue;
      }
      TRecoCHANTIHit* RecoHit = static_cast<TRecoCHANTIHit*>(fRecoEvent->AddHit(Digi));
      RecoHit->SetChannelID(ChannelID);
      RecoHit->DecodeChannelID();
      //Plane ID from 0 to 6
      //PlaneID = RecoHit->GetPlaneID();
      //Ring ID from 0 to 11
      RingID = RecoHit->GetRingID();
      //Bar ID from -11 to -5 and from 0 to 16
      BarID = RecoHit->GetBarID();
      BarIndex = (BarID > 0) ? BarID : abs(BarID)+12;
      //cout<<"Rind ID = "<<RingID<<" Bar ID = "<<BarID<<" Bar Index = "<<BarIndex<< std::endl;
      BarFired[RingID][BarIndex] = 1;
      if(iDigi+1 <NDigis){
        TCHANTIDigi *DigiBis = static_cast<TCHANTIDigi*>(Digis[iDigi+1]);
        if(((DigiBis->GetChannelID() - Digi->GetChannelID()) == 1) &&
            (Digi->GetLeadingEdge() > -1e20) &&
            (DigiBis->GetLeadingEdge() > -1e20)
            && (Digi->GetLeadingEdge() - DigiBis->GetLeadingEdge() < 50))
        {
          //////////////////////////////stesso canale fisico, entrambe le soglie superate/////////////////////////////////
          fChannels[ROCH]->AddHit();
          fChannels[ROCH+1]->AddHit();
          T0_1 = (fEnableT0) ? fChannels[ROCH]->GetT0() : 0.0;
          T0_2 = (fEnableT0) ? fChannels[ROCH+1]->GetT0() : 0.0;
          TellID = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTellID(ROCH);
          TDCBID = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTDCBID(ROCH);
          TDCID  = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTDCID(ROCH);
          fNWordTell[TellID - 1] += 2;
          fNWordTDCB[TDCBID - 1] += 2;
          fNWordTDC[TDCID - 1] += 2;
          Time = Digi->GetLeadingEdge() - GetT0Correction(Digi) - T0_1;
          DeltaTime = DigiBis->GetLeadingEdge() - T0_2 - Digi->GetLeadingEdge() + T0_1;
          ThresholdFlag = 2;
          iDigi ++;
          fNElectronicHit++;
          if((Digi->GetTrailingEdge() > -1e20) && (DigiBis->GetTrailingEdge() > -1e20)) {//both leading and trailing
            QualityFlag = 0;
            fNWordTell[TellID - 1] += 2;
            fNWordTDCB[TDCBID - 1] += 2;
            fNWordTDC[TDCID - 1] += 2;
          }
          else if(Digi->GetTrailingEdge() > -1e20) {//low th leading and trailing, high th only leading
            QualityFlag = 1;
            fNWordTell[TellID - 1] += 1;
            fNWordTDCB[TDCBID - 1] += 1;
            fNWordTDC[TDCID - 1] += 1;
          }
          else if(DigiBis->GetTrailingEdge() > -1e20) {//high th leading and trailing, low th only leading
            QualityFlag = 2;
            fNWordTell[TellID - 1] += 1;
            fNWordTDCB[TDCBID - 1] += 1;
            fNWordTDC[TDCID - 1] += 1;
          }
          else QualityFlag = 3;//low and high th leading only
          if(QualityFlag < 2) TimeWidth = Digi->GetTrailingEdge() - GetT0Correction(Digi) - T0_1 - Time;
          if(QualityFlag == 0) DeltaWidth = DigiBis->GetTrailingEdge() - DigiBis->GetLeadingEdge() - TimeWidth;
        }
        else {
          fChannels[ROCH]->AddHit();
          T0_1 = (fEnableT0) ? fChannels[ROCH]->GetT0() : 0.0 ;
          TellID = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTellID(ROCH);
          TDCBID = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTDCBID(ROCH);
          TDCID  = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTDCID(ROCH);
          fNWordTell[TellID - 1] += 1;
          fNWordTDCB[TDCBID - 1] += 1;
          fNWordTDC[TDCID - 1] += 1;
          Time = (Digi->GetLeadingEdge() > -1e20) ? Digi->GetLeadingEdge() - GetT0Correction(Digi) - T0_1: Digi->GetTrailingEdge() - GetT0Correction(Digi) - T0_1;
          if((Digi->GetLeadingEdge() > -1e20) && (Digi->GetTrailingEdge() > -1e20)){
            TimeWidth = Digi->GetTrailingEdge() - GetT0Correction(Digi) - T0_1 - Time;
            fNWordTell[TellID - 1] += 1;
            fNWordTDCB[TDCBID - 1] += 1;
            fNWordTDC[TDCID - 1] += 1;
          }
          ThresholdFlag = Digi->GetThresholdType();
          if(TimeWidth != -999) QualityFlag = 0;
          else if(Digi->GetLeadingEdge() > -1e20) QualityFlag = 4;
          else if(Digi->GetTrailingEdge() > -1e20) QualityFlag = 5;
        }
      }

      else {
        ROCH = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ChannelID);
        fChannels[ROCH]->AddHit();
        T0_1 = (fEnableT0) ? fChannels[ROCH]->GetT0() : 0.0 ;
        TellID = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTellID(ROCH);
        TDCBID = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTDCBID(ROCH);
        TDCID  = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetTDCID(ROCH);
        fNWordTell[TellID - 1] += 1;
        fNWordTDCB[TDCBID - 1] += 1;
        fNWordTDC[TDCID - 1] += 1;
        Time = (Digi->GetLeadingEdge() > -1e20) ? Digi->GetLeadingEdge() - GetT0Correction(Digi) - T0_1: Digi->GetTrailingEdge() - GetT0Correction(Digi) - T0_1;
        if((Digi->GetLeadingEdge() > -1e20) && (Digi->GetTrailingEdge() > -1e20)) {
          TimeWidth = Digi->GetTrailingEdge() - GetT0Correction(Digi) - T0_1 - Time;
          fNWordTell[TellID - 1] += 1;
          fNWordTDCB[TDCBID - 1] += 1;
          fNWordTDC[TDCID - 1] += 1;
        }
        ThresholdFlag = Digi->GetThresholdType();
        if(TimeWidth != -999) QualityFlag = 0;
        else if(Digi->GetLeadingEdge() > -1e20) QualityFlag = 4;
        else if(Digi->GetTrailingEdge() > -1e20) QualityFlag = 5;
      }
      /////////////////////////////////////////////////////////////////////////////////////////

      RecoHit->SetTime(Time);
      RecoHit->SetTimeWidth(TimeWidth);
      RecoHit->SetDeltaTime(DeltaTime);
      RecoHit->SetDeltaWidth(DeltaWidth);
      RecoHit->SetQualityFlag(QualityFlag);
      if(RecoHit->GetRingType() == kX)
      {
        X =  abs(BarID)*fStep - fGlobalShift;
        Y = -999;
      }
      else
      {
        X = -999;
        Y = abs(BarID)*fStep - fGlobalShift;
      }
      RecoHit->SetX(X);
      RecoHit->SetY(Y);
      RecoHit->SetZ(fGeometry->GetZPos_Ring(RingID));
      RecoHit->SetThresholdFlag(ThresholdFlag);

      //
      fNHitRing[RingID]++;
      fNHitPlane[int(RingID/2)]++;
      fNElectronicHit ++;
      if((BarID*10 + ThresholdFlag) < -150) std::cout <<"[CHANTIReconstruction::DigiToReco] CH ID = "<<Digi->GetChannelID()<< std::endl;
    }
    else std::cout <<"[CHANTIReconstruction::DigiToReco] CHECK THE CH MAP!!! ID = "<<ChannelID<< std::endl;
  }
}

void CHANTIReconstruction::SlewingCorrection(TDCEvent * /*tCHANTIEvent*/){
  Double_t TimeCorr=0., Corr=0.;
  Double_t V1=0., V2=0.;
  Double_t Time, TimeH, TimeWidth;
  Int_t RingID;
  Int_t ROCH;
  Int_t nHits = fRecoEvent->GetNHits();

  /////////////////////////////////////////////////////////////////////////////////
  ////////////////TSlewing correction using the two thresholds/////////////////////
  /////////////////////////////////////////////////////////////////////////////////
  if(nHits > 0){
    TClonesArray & RecoHits = (* (fRecoEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
      TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      Time = RecoHit->GetTime();
      TimeH = Time+RecoHit->GetDeltaTime();
      if(RecoHit->GetThresholdFlag() == 2){
        RingID = RecoHit->GetRingID();
        ROCH = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(RecoHit->GetChannelID());
        V1 = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetThreshold();
        V2 = static_cast<CHANTIChannel*>(fChannels[ROCH+1])->GetThreshold();
        //cout<<"DEBUG	V1 = "<<V1<<"		V2 = "<<V2<<endl;
        if((V1 - V2) != 0){
          TimeCorr = (V2*Time - V1*TimeH)/(V2-V1);
          RecoHit->SetTime(TimeCorr);
          fHCorrTime[RingID]->Fill(TimeCorr);
          if(RecoHit->GetQualityFlag() == 0){
            TimeWidth = RecoHit->GetTimeWidth();

            fHSlewingVSToTLow->Fill(TimeWidth,Time - TimeCorr);
            fHSlewingVSToTHigh->Fill(TimeWidth+RecoHit->GetDeltaWidth(),TimeH - TimeCorr);
          }
        }
      }
      else if(RecoHit->GetQualityFlag() == 0) {
        TimeWidth = RecoHit->GetTimeWidth();
        RingID = RecoHit->GetRingID();
        ROCH = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(RecoHit->GetChannelID());
        if(ROCH<0) {
          std::cout << "[CHANTIReconstruction] Error: wrong RO channel ID!" << std::endl;
          continue;
        }
        Corr = static_cast<CHANTIChannel*>(fChannels[ROCH])->GetSlewingCorrection(TimeWidth);
        if(ROCH%2 == 0) fHSlewingVSToTLowFit->Fill(TimeWidth,Corr);
        else fHSlewingVSToTHighFit->Fill(TimeWidth,Corr);
        TimeCorr = Time - Corr;
        RecoHit->SetTime(TimeCorr);
        if(fHCorrTime && fHCorrTime[RingID]) fHCorrTime[RingID]->Fill(TimeCorr);
      }
    }
  }
}


void CHANTIReconstruction::ModeEvaluation(TDCEvent * tCHANTIEvent){
  fModeX = 0;
  fModeY = 0;
  fContOfModeX = 0;
  fContOfModeY = 0;
  Double_t X,Y;
  Int_t RingID;
  Int_t nHits = fRecoEvent->GetNHits();

  if(nHits>0){
    for(int iRing = 0; iRing < fNRings; iRing++){
      fEntriesMode[iRing] = 0;
      fMode[iRing] = 0;
      //TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      fCorrTimeBinMode[iRing]= 0;
      fCorrTimeMode[iRing]= 0.0;
      fCorrTimeEntriesMode[iRing] = 0;
    }
    //
    TClonesArray & RecoHits = (* (fRecoEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
      TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      X = RecoHit->GetX();
      Y = RecoHit->GetY();
      RingID = RecoHit->GetRingID();
      if(RecoHit->GetRingType() == kX)
      {
        fHPosition1[RingID]->Fill(X);
        fHPosition2[RingID]->Fill(X);
        fHXHitEvent1->Fill(X);
        fHXHitEvent2->Fill(X);
      }
      else
      {
        fHPosition1[RingID]->Fill(Y);
        fHPosition2[RingID]->Fill(Y);
        fHYHitEvent1->Fill(Y);
        fHYHitEvent2->Fill(Y);
      }
    }
    //XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    int binOfModeX1 = fHXHitEvent1->GetMaximumBin();
    int binOfModeX2 = fHXHitEvent2->GetMaximumBin();
    double ModeX1 = fHXHitEvent1->GetXaxis()->GetBinCenter(binOfModeX1);
    double ModeX2 = fHXHitEvent2->GetXaxis()->GetBinCenter(binOfModeX2);
    int ContOfBinOfModeX1 = fHXHitEvent1->GetBinContent(binOfModeX1);
    int ContOfBinOfModeX2 = fHXHitEvent2->GetBinContent(binOfModeX2);
    ////////////////////////////////MAX BIN FOR THE MODE////////////////////
    fContOfModeX = (ContOfBinOfModeX1 > ContOfBinOfModeX2) ? ContOfBinOfModeX1 : ContOfBinOfModeX2;
    fModeX = (ContOfBinOfModeX1 > ContOfBinOfModeX2) ? ModeX1 : ModeX2 ;
    if(ContOfBinOfModeX1 == ContOfBinOfModeX2)
    {
      fContOfModeX = ContOfBinOfModeX2;
      fModeX = ModeX2;
      if (fContOfModeX > 2){
        TRandom3* TR3 = new TRandom3();
        TR3->SetSeed(tCHANTIEvent->GetTimeStamp());
        if(TR3->Rndm() >= 0.5)
        {
          fContOfModeX = ContOfBinOfModeX1;
          fModeX = ModeX1;
        }
        delete TR3;
      }
    }
    //YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
    int binOfModeY1 = fHYHitEvent1->GetMaximumBin();
    int binOfModeY2 = fHYHitEvent2->GetMaximumBin();
    double ModeY1 = fHYHitEvent1->GetXaxis()->GetBinCenter(binOfModeY1);
    double ModeY2 = fHYHitEvent2->GetXaxis()->GetBinCenter(binOfModeY2);
    int ContOfBinOfModeY1 = fHYHitEvent1->GetBinContent(binOfModeY1);
    int ContOfBinOfModeY2 = fHYHitEvent2->GetBinContent(binOfModeY2);
    ////////////////////////////////MAX BIN FOR THE MODE////////////////////
    fContOfModeY = (ContOfBinOfModeY1 > ContOfBinOfModeY2) ? ContOfBinOfModeY1 : ContOfBinOfModeY2;
    fModeY = (ContOfBinOfModeY1 > ContOfBinOfModeY2) ? ModeY1 : ModeY2 ;
    if(ContOfBinOfModeY1 == ContOfBinOfModeY2)
    {
      fContOfModeY = ContOfBinOfModeY2;
      fModeY = ModeY2;
      if (fContOfModeY > 2){
        TRandom3* TR3 = new TRandom3();
        TR3->SetSeed(tCHANTIEvent->GetTimeStamp());
        if(TR3->Rndm() >= 0.5)
        {
          fContOfModeY = ContOfBinOfModeY1;
          fModeY = ModeY1;
          fModeYID = 1;
        }
        delete TR3;
      }
    }

    //aggiungi i plots per le mode in Mode in XY in funzione del # di hit per la selezione di eventi di muone (non piu single muon)
    fHContOfModeX->Fill(fContOfModeX);
    fHContOfModeY->Fill(fContOfModeY);
    if(fNFiredRing == 12){
      fHContOfModeX1->Fill(fContOfModeX);
      fHContOfModeY1->Fill(fContOfModeY);
      if(fNElectronicHit > 35){
        fHContOfModeX2->Fill(fContOfModeX);
        fHContOfModeY2->Fill(fContOfModeY);
      }
    }
    if(fNElectronicHit > 35){
      fHContOfModeX3->Fill(fContOfModeX);
      fHContOfModeY3->Fill(fContOfModeY);
    }
    //////////////////////////////////////////////////////////////////////
    int BinMode1[12]={0};
    int BinMode2[12]={0};
    double Mode1[12]={0.0};
    double Mode2[12]={0.0};
    int EntriesMode1[12] = {0};
    int EntriesMode2[12] = {0};
    ////////////////////////////////MAX BIN FOR THE MODE////////////////////
    //////////////////////////////////////////////////////////////////////
    for(int iRing = 0; iRing < fNRings; iRing++){
      BinMode1[iRing] = fHPosition1[iRing]->GetMaximumBin();
      BinMode2[iRing] = fHPosition2[iRing]->GetMaximumBin();
      Mode1[iRing] = fHPosition1[iRing]->GetXaxis()->GetBinCenter(BinMode1[iRing]);
      Mode2[iRing] = fHPosition2[iRing]->GetXaxis()->GetBinCenter(BinMode2[iRing]);
      EntriesMode1[iRing] = fHPosition1[iRing]->GetBinContent(BinMode1[iRing]);
      EntriesMode2[iRing] = fHPosition2[iRing]->GetBinContent(BinMode2[iRing]);
      fEntriesMode[iRing] = (EntriesMode1[iRing] > EntriesMode2[iRing]) ? EntriesMode1[iRing] : EntriesMode2[iRing];
      fMode[iRing] = (EntriesMode1[iRing] > EntriesMode2[iRing]) ? Mode1[iRing] : Mode2[iRing] ;
      ////////////////////////////////////////////////////////////////////////////////////////////////
      fCorrTimeBinMode[iRing] = fHCorrTime[iRing]->GetMaximumBin();
      fCorrTimeMode[iRing] = fHCorrTime[iRing]->GetXaxis()->GetBinCenter(fCorrTimeBinMode[iRing]);
      fCorrTimeEntriesMode[iRing] = fHCorrTime[iRing]->GetBinContent(fCorrTimeBinMode[iRing]);
      //////////////////////////////////////////////////////////////////////////////////////////////////
    }
  }
}

void CHANTIReconstruction::EfficiencyEvaluation(TDCEvent * /*tCHANTIEvent*/){
  //////////////////////////////////////////////////////////////////////////////////////////////////
  Int_t nHits = fRecoEvent->GetNHits();

  if(nHits>0){

    Int_t NBins = fHPosition1[0]->GetNbinsX();
    Int_t EffiNumXUp[fNPlanes][NBins];
    Int_t EffiNumYUp[fNPlanes][NBins];
    Int_t NormSampleXUp[fNPlanes][NBins];
    Int_t NormSampleYUp[fNPlanes][NBins];
    //
    Int_t EffiNumXDown[fNPlanes][NBins];
    Int_t EffiNumYDown[fNPlanes][NBins];
    Int_t NormSampleXDown[fNPlanes][NBins];
    Int_t NormSampleYDown[fNPlanes][NBins];
    //
    Int_t EffiNumXY[fNPlanes][NBins][NBins];
    Int_t NormSampleXY[fNPlanes][NBins][NBins];
    ////////
    Int_t EffiNumHighXUp[fNPlanes][NBins];
    Int_t EffiNumHighYUp[fNPlanes][NBins];
    Int_t EffiNumHighXDown[fNPlanes][NBins];
    Int_t EffiNumHighYDown[fNPlanes][NBins];
    ///////
    Int_t RingID;
    Double_t X, Y, Time;
    for(int iPlane = 0; iPlane < fNPlanes; iPlane++) {
      for(int iBin = 0; iBin < NBins; iBin++) {
        EffiNumXUp[iPlane][iBin] = 0;
        EffiNumYUp[iPlane][iBin] = 0;
        EffiNumXDown[iPlane][iBin] = 0;
        EffiNumYDown[iPlane][iBin] = 0;
        //////
        EffiNumHighXUp[iPlane][iBin] = 0;
        EffiNumHighYUp[iPlane][iBin] = 0;
        EffiNumHighXDown[iPlane][iBin] = 0;
        EffiNumHighYDown[iPlane][iBin] = 0;
        //////
        NormSampleXUp[iPlane][iBin] = 0;
        NormSampleYUp[iPlane][iBin] = 0;
        NormSampleXDown[iPlane][iBin] = 0;
        NormSampleYDown[iPlane][iBin] = 0;
        for(int jBin = 0; jBin < NBins; jBin++) {
          EffiNumXY[iPlane][iBin][jBin] = 0;
          NormSampleXY[iPlane][iBin][jBin] = 0;
        }
      }
    }
    //
    TClonesArray & RecoHits = (* (fRecoEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
      TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      X = RecoHit->GetX();
      Y = RecoHit->GetY();
      Time = RecoHit->GetTime();
      RingID = RecoHit->GetRingID();
      if(Time < -10 || Time > 40){
        if(RecoHit->GetRingType() == kX) {
          if(RecoHit->GetSideID() == kPositive)  {
            fHEffi1Up[RingID]->Fill(X);
            fHEffi2Up[RingID]->Fill(X);
          }
          else{
            fHEffi1Down[RingID]->Fill(X);
            fHEffi2Down[RingID]->Fill(X);
          }
        }
        else{
          if(RecoHit->GetSideID() == kPositive)  {
            fHEffi1Up[RingID]->Fill(Y);
            fHEffi2Up[RingID]->Fill(Y);
          }
          else{
            fHEffi1Down[RingID]->Fill(Y);
            fHEffi2Down[RingID]->Fill(Y);
          }
        }
      }
      ///////
      if (RecoHit->GetThresholdFlag()==2 || RecoHit->GetThresholdFlag()==1){
        if(RecoHit->GetRingType() == kX) {
          if(RecoHit->GetSideID() == kPositive)  {
            fHEffiHigh1Up[RingID]->Fill(X);
            fHEffiHigh2Up[RingID]->Fill(X);
          }
          else{
            fHEffiHigh1Down[RingID]->Fill(X);
            fHEffiHigh2Down[RingID]->Fill(X);
          }
        }
        else{
          if(RecoHit->GetSideID() == kPositive)  {
            fHEffiHigh1Up[RingID]->Fill(Y);
            fHEffiHigh2Up[RingID]->Fill(Y);
          }
          else{
            fHEffiHigh1Down[RingID]->Fill(Y);
            fHEffiHigh2Down[RingID]->Fill(Y);
          }
        }
      }
    }
    //
    //
    //
    Double_t Position;
    for(int iRing = 0; iRing < fNRings; iRing++) {
      for(int iStep = 0; iStep < 17; iStep++) {
        Position = iStep*fStep - fGlobalShift;
        if(fHEffi1Up[iRing]->GetBinContent(iStep+1) > 0) fHEffiPosition1Up[iRing]->Fill(Position);
        if(fHEffi2Up[iRing]->GetBinContent(iStep+1) > 0) fHEffiPosition2Up[iRing]->Fill(Position);
        if(fHEffi1Down[iRing]->GetBinContent(iStep+1) > 0) fHEffiPosition1Down[iRing]->Fill(Position);
        if(fHEffi2Down[iRing]->GetBinContent(iStep+1) > 0) fHEffiPosition2Down[iRing]->Fill(Position);
        if(fHEffiHigh1Up[iRing]->GetBinContent(iStep+1) > 0) fHEffiPositionHigh1Up[iRing]->Fill(Position);
        if(fHEffiHigh2Up[iRing]->GetBinContent(iStep+1) > 0) fHEffiPositionHigh2Up[iRing]->Fill(Position);
        if(fHEffiHigh1Down[iRing]->GetBinContent(iStep+1) > 0) fHEffiPositionHigh1Down[iRing]->Fill(Position);
        if(fHEffiHigh2Down[iRing]->GetBinContent(iStep+1) > 0) fHEffiPositionHigh2Down[iRing]->Fill(Position);
      }
    }
    //
    //Normalizzation Sample Evaluation
    //
    Int_t iPlane;
    Double_t Step = fHEffiPosition1Up[0]->GetBinWidth(1);
    Double_t Minimum1 = fHEffiPosition1Up[0]->GetBinLowEdge(1);
    for(int iBin = 0; iBin < NBins; iBin++) {
      for(int iRing = 0; iRing < fNRings; iRing++) {
        iPlane = iRing/2;
        if((iPlane != 0) && (iRing%2 == 0) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXUp[0][iBin] += 1;
        if((iPlane != 0) && (iRing%2 == 1) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYUp[0][iBin] += 1;
        //
        if((iPlane != 0) && (iRing%2 == 0) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXDown[0][iBin] += 1;
        if((iPlane != 0) && (iRing%2 == 1) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYDown[0][iBin] += 1;
        //
        if((iPlane != 1) && (iRing%2 == 0) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXUp[1][iBin] += 1;
        if((iPlane != 1) && (iRing%2 == 1) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYUp[1][iBin] += 1;
        //
        if((iPlane != 1) && (iRing%2 == 0) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXDown[1][iBin] += 1;
        if((iPlane != 1) && (iRing%2 == 1) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYDown[1][iBin] += 1;
        //
        if((iPlane != 2) && (iRing%2 == 0) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXUp[2][iBin] += 1;
        if((iPlane != 2) && (iRing%2 == 1) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYUp[2][iBin] += 1;
        //
        if((iPlane != 2) && (iRing%2 == 0) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXDown[2][iBin] += 1;
        if((iPlane != 2) && (iRing%2 == 1) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYDown[2][iBin] += 1;
        //
        if((iPlane != 3) && (iRing%2 == 0) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXUp[3][iBin] += 1;
        if((iPlane != 3) && (iRing%2 == 1) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYUp[3][iBin] += 1;
        //
        if((iPlane != 3) && (iRing%2 == 0) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXDown[3][iBin] += 1;
        if((iPlane != 3) && (iRing%2 == 1) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYDown[3][iBin] += 1;
        //
        if((iPlane != 4) && (iRing%2 == 0) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXUp[4][iBin] += 1;
        if((iPlane != 4) && (iRing%2 == 1) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYUp[4][iBin] += 1;
        //
        if((iPlane != 4) && (iRing%2 == 0) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXDown[4][iBin] += 1;
        if((iPlane != 4) && (iRing%2 == 1) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYDown[4][iBin] += 1;
        //
        if((iPlane != 5) && (iRing%2 == 0) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXUp[5][iBin] += 1;
        if((iPlane != 5) && (iRing%2 == 1) && (fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYUp[5][iBin] += 1;
        //
        if((iPlane != 5) && (iRing%2 == 0) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleXDown[5][iBin] += 1;
        if((iPlane != 5) && (iRing%2 == 1) && (fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0)) NormSampleYDown[5][iBin] += 1;
        //
        if(iBin == 0){
          if(iRing%2 == 0){
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0))EffiNumXUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighXUp[iPlane][iBin] = 1;
          }
          if(iRing%2 == 1) {
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumYUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighYUp[iPlane][iBin] = 1;
          }
        }
        else if(iBin == 2){
          if(iRing%2 == 0){
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin+1) > 0 //
                )) EffiNumXUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin+1) > 0 //
                )) EffiNumHighXUp[iPlane][iBin] = 1;
          }
          if(iRing%2 == 1){
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin+1) > 0 //
                )) EffiNumYUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin+1) > 0 //
                )) EffiNumHighYUp[iPlane][iBin] = 1;
          }
        }
        else if(iBin == 6){
          if(iRing%2 == 0){
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin) > 0)) EffiNumXUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin) > 0)) EffiNumHighXUp[iPlane][iBin] = 1;
          }
          if(iRing%2 == 1) {
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin) > 0)) EffiNumYUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin) > 0)) EffiNumHighYUp[iPlane][iBin] = 1;
          }
        }
        else if(iBin+1 == NBins){
          if(iRing%2 == 0) {
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumXUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighXUp[iPlane][iBin] = 1;
          }
          if(iRing%2 == 1) {
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumYUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighYUp[iPlane][iBin] = 1;
          }
        }
        else{
          if(iRing%2 == 0){
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumXUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighXUp[iPlane][iBin] = 1;
          }
          if(iRing%2 == 1) {
            if ((fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumYUp[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighYUp[iPlane][iBin] = 1;
          }
        }

        if(iBin == 3){
          if(iRing%2 == 0){
            if ((fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin+1) > 0)) EffiNumXDown[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighXDown[iPlane][iBin] = 1;
          }
          if(iRing%2 == 1) {
            if ((fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin+1) > 0)) EffiNumYDown[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighYDown[iPlane][iBin] = 1;
          }
        }
        else if(iBin >3 && iBin <6){
          if(iRing%2 == 0) {
            if ((fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin+1) > 0)) EffiNumXDown[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighXDown[iPlane][iBin] = 1;
          }
          if(iRing%2 == 1) {
            if ((fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin+1) > 0)) EffiNumYDown[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighYDown[iPlane][iBin] = 1;
          }
        }
        else if(iBin == 6){
          if(iRing%2 == 0) {
            if ((fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  //fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumXDown[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  //fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighXDown[iPlane][iBin] = 1;
          }
          if(iRing%2 == 1) {
            if ((fHEffiPosition1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPosition2Down[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPosition2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumYDown[iPlane][iBin] = 1;
            if ((fHEffiPositionHigh1Down[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh1Up[iRing]->GetBinContent(iBin+1) > 0 ||
                  fHEffiPositionHigh2Down[iRing]->GetBinContent(iBin) > 0 ||
                  fHEffiPositionHigh2Up[iRing]->GetBinContent(iBin+1) > 0)) EffiNumHighYDown[iPlane][iBin] = 1;
          }
        }
      }
    }
    //
    //
    //
    for(int iBin = 0; iBin < NBins; iBin++) {
      for(int iPlane = 0; iPlane < fNPlanes; iPlane++) {
        //
        if(NormSampleXUp[iPlane][iBin] == 5){
          fHEffiPlotXDen[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
          if(EffiNumXUp[iPlane][iBin] == 1) fHEffiPlotXNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
          if(EffiNumHighXUp[iPlane][iBin] == 1) fHEffiPlotHighXNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
        }
        if(NormSampleYUp[iPlane][iBin] == 5) {
          fHEffiPlotYDen[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
          if(EffiNumYUp[iPlane][iBin] == 1) fHEffiPlotYNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
          if(EffiNumHighYUp[iPlane][iBin] == 1) fHEffiPlotHighYNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
        }
        if(NormSampleXDown[iPlane][iBin] == 5){
          fHEffiPlotXDen[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
          if(EffiNumXDown[iPlane][iBin] == 1) fHEffiPlotXNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
          if(EffiNumHighXDown[iPlane][iBin] == 1) fHEffiPlotHighXNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
        }
        if(NormSampleYDown[iPlane][iBin] == 5) {
          fHEffiPlotYDen[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
          if(EffiNumYDown[iPlane][iBin] == 1) fHEffiPlotYNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
          if(EffiNumHighYDown[iPlane][iBin] == 1) fHEffiPlotHighYNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1);
        }
      }
    }
    ///////////////////////////////////////////////////////////////////
    for(int iPlane = 0; iPlane < fNPlanes; iPlane++) {
      for(int iBin = 0; iBin < NBins; iBin++) {
        for(int jBin = 0; jBin < NBins; jBin++) {
          //if(!(iBin>2 && iBin<6 && jBin>2 && jBin<6)){}
          NormSampleXY[iPlane][iBin][jBin] = NormSampleXUp[iPlane][iBin] + NormSampleYUp[iPlane][jBin];
          EffiNumXY[iPlane][iBin][jBin] = EffiNumXUp[iPlane][iBin] + EffiNumYUp[iPlane][jBin];
          if(NormSampleXY[iPlane][iBin][jBin] == 10){
            fHEffiPlotXYDen[iPlane]->Fill((iBin+0.5)*Step + Minimum1,(jBin+0.5)*Step + Minimum1);
            if(EffiNumXY[iPlane][iBin][jBin] > 0) fHEffiPlotXYNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1,(jBin+0.5)*Step + Minimum1);
          }
          //
          NormSampleXY[iPlane][iBin][jBin] = NormSampleXUp[iPlane][iBin] + NormSampleYDown[iPlane][jBin];
          EffiNumXY[iPlane][iBin][jBin] = EffiNumXUp[iPlane][iBin] + EffiNumYDown[iPlane][jBin];
          if(NormSampleXY[iPlane][iBin][jBin] == 10){
            fHEffiPlotXYDen[iPlane]->Fill((iBin+0.5)*Step + Minimum1,(jBin+0.5)*Step + Minimum1);
            if(EffiNumXY[iPlane][iBin][jBin] > 0) fHEffiPlotXYNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1,(jBin+0.5)*Step + Minimum1);
          }
          NormSampleXY[iPlane][iBin][jBin] = NormSampleXDown[iPlane][iBin] + NormSampleYUp[iPlane][jBin];
          EffiNumXY[iPlane][iBin][jBin] = EffiNumXDown[iPlane][iBin] + EffiNumYUp[iPlane][jBin];
          if(NormSampleXY[iPlane][iBin][jBin] == 10){
            fHEffiPlotXYDen[iPlane]->Fill((iBin+0.5)*Step + Minimum1,(jBin+0.5)*Step + Minimum1);
            if(EffiNumXY[iPlane][iBin][jBin] > 0) fHEffiPlotXYNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1,(jBin+0.5)*Step + Minimum1);
          }
          //
          NormSampleXY[iPlane][iBin][jBin] = NormSampleXDown[iPlane][iBin] + NormSampleYDown[iPlane][jBin];
          EffiNumXY[iPlane][iBin][jBin] = EffiNumXDown[iPlane][iBin] + EffiNumYDown[iPlane][jBin];
          if(NormSampleXY[iPlane][iBin][jBin] == 10){
            fHEffiPlotXYDen[iPlane]->Fill((iBin+0.5)*Step + Minimum1,(jBin+0.5)*Step + Minimum1);
            if(EffiNumXY[iPlane][iBin][jBin] > 0) fHEffiPlotXYNum[iPlane]->Fill((iBin+0.5)*Step + Minimum1,(jBin+0.5)*Step + Minimum1);
          }
        }
      }
    }
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CHANTIReconstruction::BarEfficiencyEvaluation(TDCEvent * /*tCHANTIEvent*/){
  Int_t nHits = fRecoEvent->GetNHits();

  if(nHits>0){
    const Int_t NBars = 24;
    Int_t EffiNum[fNRings][NBars];
    Int_t NormSample[fNRings][NBars];
    //
    //Int_t EffiNumXY[fNRings][NBars][NBars];
    //Int_t NormSampleXY[fNRings][NBars][NBars];
    ///////
    for(int iRing = 0; iRing < fNRings-2; iRing++) {
      for(int iBar = 0; iBar < NBars; iBar++) {
        EffiNum[iRing][iBar] = 0;
        NormSample[iRing][iBar] = 0;
        //for(int jBar = 0; jBar < NBars; jBar++) {
          //EffiNumXY[iRing][iBar][jBar] = 0;
          //NormSampleXY[iRing][iBar][jBar] = 0;
        //}
      }
    }

    for(int iBar = 0; iBar < NBars; iBar++) {
      for(int iRing = 0; iRing < fNRings-2; iRing++) {
        for(int iPlane = 0; iPlane < fNPlanes-1; iPlane++) {
          Int_t jRing = -999;
          if(iRing%2 == 0) jRing = iPlane*2;
          else jRing = iPlane*2 + 1;
          if((iRing != jRing)  && (BarFired[jRing][iBar] == 1)) {
            NormSample[iRing][iBar] += 1;
          }
          else if((iRing == jRing) && (BarFired[jRing][iBar] == 1)) {
            EffiNum[iRing][iBar] = 1;
          }
        }
      }
    }
    for(int iBar = 0; iBar < NBars; iBar++) {
      for(int iRing = 0; iRing < fNRings-2; iRing++) {
        if((NormSample[iRing][iBar] == 4) && (EffiNum[iRing][iBar] == 1)) fHBarEffiNum[iRing]->Fill((iBar+1)*1.);
        if(NormSample[iRing][iBar] == 4) fHBarEffiDen[iRing]->Fill((iBar+1)*1.);
      }
    }
  }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CHANTIReconstruction::FillingHisto(TDCEvent * /*tCHANTIEvent*/){
  //Variable for the layers visualization
  const double HalfHoleXmin = 47.5;
  const double HalfHoleY1 = 33+fStep;
  const double HalfHoleXmax = 47.5+fStep;
  const double HalfHoleY2max = 2*33;
  const double HalfHoleY2min = 33;
  //////////////////////////////////////
  Int_t nHits = fRecoEvent->GetNHits();

  if(nHits>0){

    fHNRing->Fill(fNFiredRing);
    fHNPlane->Fill(fNFiredPlane);
    fHNRingVSNHit->Fill(fNElectronicHit,fNFiredRing);
    fHNPlaneVSNHit->Fill(fNElectronicHit,fNFiredPlane);
    if(fNFiredRing ==12) fHNHitAllRing->Fill(fNElectronicHit);
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    for(int iRing = 0; iRing < fNRings; iRing++) {
      if(fCorrTimeEntriesMode[iRing] >1 && fCorrTimeEntriesMode[0] > 1)
        fHCorrTimeVSZ->Fill(fGeometry->GetZPos_Ring(iRing) - fGeometry->GetZPos_Ring(0),fCorrTimeMode[iRing] - fCorrTimeMode[0]);
    }
    if(fContOfModeX>1) fHModeX->Fill(fModeX);
    if(fContOfModeY>1) fHModeY->Fill(fModeY);
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Int_t PlaneID;
    Int_t RingID;
    Int_t SideID;
    Int_t BarID;
    Int_t ThresholdFlag;
    Double_t Time;
    Double_t TimeWidth;
    Int_t QualityFlag;
    Double_t THit = -1e28;
    Double_t X,Y;
    Int_t Slot;
    Int_t SlotHit = -999;
    Double_t PseudoCharge = -1;
    Double_t THitWidth = -999;
    TClonesArray & RecoHits = (* (fRecoEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
      TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      PlaneID = RecoHit->GetPlaneID();
      RingID = RecoHit->GetRingID();
      SideID = RecoHit->GetSideID();
      BarID = RecoHit->GetBarID();
      ThresholdFlag = RecoHit->GetThresholdFlag();
      Time = RecoHit->GetTime();
      TimeWidth = RecoHit->GetTimeWidth();
      QualityFlag = RecoHit->GetQualityFlag();
      X = RecoHit->GetX();
      Y = RecoHit->GetY();
      Slot = static_cast<TDCVHit*>(RecoHit->GetDigi())->GetSlot();

      if(QualityFlag == 0){
        THit = Time;
        THitWidth = TimeWidth;
        SlotHit = Slot;
      }
      if(THitWidth>0) PseudoCharge = TMath::Exp(THitWidth/38);

      if(ThresholdFlag == 2){
        fHBarID[RingID]->Fill(BarID*10);
        fHBarID[RingID]->Fill(BarID*10+1);
      }
      else fHBarID[RingID]->Fill(BarID*10+ThresholdFlag);

      if((QualityFlag != 5) && (ThresholdFlag%2 == 0)) fHTimeVSSlot->Fill(Slot,Time);
      if(RecoHit->GetRingType() == kX)
      {
        fHXHit->Fill(X);
        if(ThresholdFlag == 0 || ThresholdFlag == 2) fHPositionLow[RingID][0]->Fill(X);
        if(ThresholdFlag == 1 || ThresholdFlag == 2) fHPositionHigh[RingID][0]->Fill(X);
        fHXHitEvent1->Fill(X);
        fHXHitEvent2->Fill(X);
      }
      else{
        fHYHit->Fill(Y);
        if(ThresholdFlag == 0 || ThresholdFlag == 2) fHPositionLow[RingID][0]->Fill(Y);
        if(ThresholdFlag == 1 || ThresholdFlag == 2) fHPositionHigh[RingID][0]->Fill(Y);
        fHYHitEvent1->Fill(Y);
        fHYHitEvent2->Fill(Y);
      }
      if(TimeWidth != -999){
        if(ThresholdFlag == 0){
          fHToT0->Fill(TimeWidth);
          fHToTRing0[RingID]->Fill(TimeWidth);
          fHToTPlane0[PlaneID]->Fill(TimeWidth);
        }
        else if(ThresholdFlag == 1){
          fHToT1->Fill(TimeWidth);
          fHToTRing1[RingID]->Fill(TimeWidth);
          fHToTPlane1[PlaneID]->Fill(TimeWidth);
        }
        else if(ThresholdFlag == 2){
          fHToT2->Fill(TimeWidth);
          fHToTRing2[RingID]->Fill(TimeWidth);
          fHToTPlane2[PlaneID]->Fill(TimeWidth);
        }
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////
      if(fCorrTimeEntriesMode[RingID] >1 && (QualityFlag != 5) && (ThresholdFlag ==0 || ThresholdFlag ==2))
        fHCorrTimeDispPlot[RingID]->Fill(fCorrTimeMode[RingID] - Time);
      ///////////////////////////////////////////////////////////////////////////////////////////////////////
      if(RecoHit->GetRingType() == kX)
      {
        if(fContOfModeX>6) fHDispPlotX->Fill(fModeX - X);
        if(fEntriesMode[RingID]>1) fHDispPlot[RingID]->Fill(fMode[RingID] - X);
        ///////////////////////////////////////////////////////////////////////////////////////////////////////
      }
      else
      {
        if(fContOfModeY>6) fHDispPlotY->Fill(fModeY - Y);
        if(fEntriesMode[RingID]>1)fHDispPlot[RingID]->Fill(fMode[RingID] - Y);
      }
      /////////////////////////////////////////////////////
      if((QualityFlag == 0) && (ThresholdFlag%2 == 0)){
        if (RecoHit->GetRingType() == kX){
          if (BarID%2!=0){
            if (TMath::Abs(X)>HalfHoleXmax){
              fHViewX1THRL[PlaneID][0]->Fill(X,-91.25);
              fHViewX1THRL[PlaneID][0]->Fill(X,0);
              fHViewX1THRL[PlaneID][0]->Fill(X,91.25);
            } else {
              if(SideID==0) fHViewX1THRL[PlaneID][0]->Fill(X,91.25);
              else fHViewX1THRL[PlaneID][0]->Fill(X,-91.25);
            }
          } else {
            if (TMath::Abs(X)>HalfHoleXmin){
              fHViewX2THRL[PlaneID][0]->Fill(X,-91.25);
              fHViewX2THRL[PlaneID][0]->Fill(X,0);
              fHViewX2THRL[PlaneID][0]->Fill(X,91.25);
            } else {
              if(RecoHit->GetSideID() == kPositive) fHViewX2THRL[PlaneID][0]->Fill(X,91.25);
              else fHViewX2THRL[PlaneID][0]->Fill(X,-91.25);
            }
          }
        } else {
          if (BarID%2==0){
            if (TMath::Abs(Y)>HalfHoleY1){
              fHViewY1THRL[PlaneID][0]->Fill(98.75,Y);
              fHViewY1THRL[PlaneID][0]->Fill(23.75,Y);
              fHViewY1THRL[PlaneID][0]->Fill(-23.75,Y);
              fHViewY1THRL[PlaneID][0]->Fill(-98.75,Y);
            } else {
              if(RecoHit->GetSideID() == kPositive) fHViewY1THRL[PlaneID][0]->Fill(98.75,Y);
              else fHViewY1THRL[PlaneID][0]->Fill(-98.75,Y);
            }
          } else {
            if (TMath::Abs(Y)>HalfHoleY2max){
              fHViewY2THRL[PlaneID][0]->Fill(98.75,Y);
              fHViewY2THRL[PlaneID][0]->Fill(23.75,Y);
              fHViewY2THRL[PlaneID][0]->Fill(-23.75,Y);
              fHViewY2THRL[PlaneID][0]->Fill(-98.75,Y);
            } else if (TMath::Abs(Y)<HalfHoleY2min){
              if(RecoHit->GetSideID() == kPositive) fHViewY2THRL[PlaneID][0]->Fill(98.75,Y);
              else fHViewY2THRL[PlaneID][0]->Fill(-98.75,Y);
            } else {
              if(RecoHit->GetSideID() == kPositive){
                fHViewY2THRL[PlaneID][0]->Fill(98.75,Y);
                fHViewY2THRL[PlaneID][0]->Fill(23.75,Y);
              } else {
                fHViewY2THRL[PlaneID][0]->Fill(-98.75,Y);
                fHViewY2THRL[PlaneID][0]->Fill(-23.75,Y);
              }
            }
          }
        }
      }
      if((QualityFlag == 0) && (ThresholdFlag > 0)){
        if (RecoHit->GetRingType() == kX){
          if (BarID%2!=0){
            if (TMath::Abs(X)>HalfHoleXmax){
              fHViewX1THRH[PlaneID][0]->Fill(X,-91.25);
              fHViewX1THRH[PlaneID][0]->Fill(X,0);
              fHViewX1THRH[PlaneID][0]->Fill(X,91.25);
            } else {
              if(RecoHit->GetSideID() == kPositive) fHViewX1THRH[PlaneID][0]->Fill(X,91.25);
              else fHViewX1THRH[PlaneID][0]->Fill(X,-91.25);
            }
          } else {
            if (TMath::Abs(X)>HalfHoleXmin){
              fHViewX2THRH[PlaneID][0]->Fill(X,-91.25);
              fHViewX2THRH[PlaneID][0]->Fill(X,0);
              fHViewX2THRH[PlaneID][0]->Fill(X,91.25);
            } else {
              if(RecoHit->GetSideID() == kPositive) fHViewX2THRH[PlaneID][0]->Fill(X,91.25);
              else fHViewX2THRH[PlaneID][0]->Fill(X,-91.25);
            }
          }
        } else {
          if (BarID%2==0){
            if (TMath::Abs(Y)>HalfHoleY1){
              fHViewY1THRH[PlaneID][0]->Fill(98.75,Y);
              fHViewY1THRH[PlaneID][0]->Fill(23.75,Y);
              fHViewY1THRH[PlaneID][0]->Fill(-23.75,Y);
              fHViewY1THRH[PlaneID][0]->Fill(-98.75,Y);
            } else {
              if(RecoHit->GetSideID() == kPositive) fHViewY1THRH[PlaneID][0]->Fill(98.75,Y);
              else fHViewY1THRH[PlaneID][0]->Fill(-98.75,Y);
            }
          } else {
            if (TMath::Abs(Y)>HalfHoleY2max){
              fHViewY2THRH[PlaneID][0]->Fill(98.75,Y);
              fHViewY2THRH[PlaneID][0]->Fill(23.75,Y);
              fHViewY2THRH[PlaneID][0]->Fill(-23.75,Y);
              fHViewY2THRH[PlaneID][0]->Fill(-98.75,Y);
            } else if (TMath::Abs(Y)<HalfHoleY2min){
              if(RecoHit->GetSideID() == kPositive) fHViewY2THRH[PlaneID][0]->Fill(98.75,Y);
              else fHViewY2THRH[PlaneID][0]->Fill(-98.75,Y);
            } else {
              if(RecoHit->GetSideID() == kPositive) {
                fHViewY2THRH[PlaneID][0]->Fill(98.75,Y);
                fHViewY2THRH[PlaneID][0]->Fill(23.75,Y);
              } else {
                fHViewY2THRH[PlaneID][0]->Fill(-98.75,Y);
                fHViewY2THRH[PlaneID][0]->Fill(-23.75,Y);
              }
            }
          }
        }
      }
      ////////////////////////////////////////////////////////////////
      if(THit >-80 && THit<175){
        if(SlotHit == -1 || SlotHit == 0){
          fHHitTimeIn[PlaneID]->Fill(THit);
        }
        else{
          fHHitTimeOut[PlaneID]->Fill(THit);
        }
        if(THitWidth != -999){
          if((SlotHit == -1 || SlotHit == 0) && PseudoCharge < 23 && PseudoCharge > 0) fHHitTimeInMU[PlaneID]->Fill(THit);
          else if((SlotHit == -1 || SlotHit == 0) && PseudoCharge > 23) fHHitTimeInAll[PlaneID]->Fill(THit);
          else if((SlotHit != -1 || SlotHit != 0) && PseudoCharge < 23 && PseudoCharge > 0) fHHitTimeOutMU[PlaneID]->Fill(THit);
          else if(SlotHit != -1 && SlotHit != 0 && PseudoCharge > 23) fHHitTimeOutAll[PlaneID]->Fill(THit);
        }
      }
      /////////////////////////////////////////////////////////////////
    }
  }
}

void CHANTIReconstruction::SingleMuonSelection(TDCEvent * /*tCHANTIEvent*/){

  Int_t nHits = fRecoEvent->GetNHits();

  if (nHits>0){

    Int_t DistModeX = 1;
    Int_t DistModeY = 1;
    Int_t AllTimeWidth = 1;
    //Double_t XTimeCorrection=0.0;
    //Double_t YTimeCorrection=0.0;
    Int_t RingID, QualityFlag;
    Double_t Time;
    Double_t ToT;
    fSingleMuon = false;
    fSingleMuonX = 0.;
    fSingleMuonY = 0.;
    fSingleMuonTime = 1e27;
    fSingleMuonTotalToT = 0.;
    for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
      fSingleMuonTotalXToT[iPlane] = 0;
      fSingleMuonTotalYToT[iPlane] = 0;
    }
    //////////////////////////////////////////////////////////////////////
    TClonesArray & RecoHits = (* (fRecoEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
      TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      if(RecoHit->GetTimeWidth() == -999) AllTimeWidth = 0;
      if(abs(fModeX - RecoHit->GetX())>fStep*1. && RecoHit->GetX() != -999) DistModeX = 0;
      if(abs(fModeY - RecoHit->GetY())>fStep*1. && RecoHit->GetY() != -999) DistModeY = 0;
    }
    if(DistModeX == 1 && DistModeY ==1 && fNFiredRing == 12) fSingleMuon = true;
    if(fSingleMuon){
      fSingleMuonX = fModeX;
      fSingleMuonY = fModeY;
      for(Int_t iHit = 0; iHit<nHits; iHit++){
        TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
        RingID = RecoHit->GetRingID();
        Time = RecoHit->GetTime();
        ToT = RecoHit->GetTimeWidth();
        QualityFlag = RecoHit->GetQualityFlag();
        if(RecoHit->GetRingType() == kX)
        {
          //XTimeCorrection = RecoHit->GetXYTimeCorrection(fModeY);
          //if((RingID == 0) && (QualityFlag != 5) && (fSingleMuonTime > Time - XTimeCorrection)) fSingleMuonTime = Time - XTimeCorrection; //Time position correction
          //if(QualityFlag != 5) RecoHit->SetTime(Time - XTimeCorrection); //Time position correction
          if((RingID == 0) && (QualityFlag != 5)&& (fSingleMuonTime > Time)) fSingleMuonTime = Time;
          if(AllTimeWidth == 1) fSingleMuonTotalXToT[RingID] += ToT;
        }
        else if(RecoHit->GetRingType() == kY)
        {
          //YTimeCorrection = RecoHit->GetXYTimeCorrection(fModeX);
          //if(QualityFlag != 5) RecoHit->SetTime(Time);
          if(AllTimeWidth == 1) fSingleMuonTotalYToT[RingID] += ToT;
        }
      }
      for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
        if(AllTimeWidth == 1) fSingleMuonTotalToT = fSingleMuonTotalXToT[iPlane] + fSingleMuonTotalYToT[iPlane];
      }
    }
  }
}

void CHANTIReconstruction::SingleMuonFillingHisto(TDCEvent * /*tCHANTIEvent*/){
  Int_t nHits = fRecoEvent->GetNHits();
  if(fSingleMuon){
    Int_t RingID, QualityFlag, SideID;
    Int_t ThresholdFlag, RingType;
    Double_t Distance;
    Double_t Time;
    Double_t X;
    Double_t Y;
    Double_t Length = 150;
    //
    fHModeYvsModeXSingleMuon->Fill(fModeX,fModeY);
    for(int iRing = 0; iRing < fNRings; iRing++) {
      if(fCorrTimeEntriesMode[iRing] >1 && fCorrTimeEntriesMode[0] > 1)
        fHCorrTimeVSZMU->Fill(fGeometry->GetZPos_Ring(iRing) - fGeometry->GetZPos_Ring(0),fCorrTimeMode[iRing] - fCorrTimeMode[0]);
    }
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    TClonesArray & RecoHits = (* (fRecoEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
      TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      QualityFlag = RecoHit->GetQualityFlag();
      RingID = RecoHit->GetRingID();
      ThresholdFlag = RecoHit->GetThresholdFlag();
      Time = RecoHit->GetTime();
      X = RecoHit->GetX();
      Y = RecoHit->GetY();
      SideID = RecoHit->GetSideID();
      RingType = RecoHit->GetRingType();

      if(RingType == kX){
        if(SideID == kPositive) Distance = Length - fSingleMuonY;
        else Distance = Length + fSingleMuonY;
      }
      else{
        if(SideID == kPositive) Distance = Length - fSingleMuonX;
        else Distance = Length + fSingleMuonX;
      }

      ///////////////////////////////////////////////////////////////////////////////////////////////////////
      if(fCorrTimeEntriesMode[RingID] >0 && (QualityFlag != 5) && (ThresholdFlag ==0 || ThresholdFlag ==2)) {
        fHCorrTimeDispPlotMU[RingID]->Fill(fCorrTimeMode[RingID] - Time);
        fHCorrTimeShiftMU[RingID]->Fill(fCorrTimeMode[RingID] - fCorrTimeMode[0]);
        fHTimeVsDistanceMU[RingID]->Fill(Distance, Time);
      }
      ///////////////////////////////////////////////////////////////////////////////////////////////////////
      if(RecoHit->GetRingType() == kX)
      {
        fHDispPlotXMU->Fill(fMode[RingID] - X);
        fHDispPlotMU[RingID]->Fill(fMode[RingID] - X);
      }
      else
      {
        fHDispPlotYMU->Fill(fMode[RingID] - Y);
        fHDispPlotMU[RingID]->Fill(fMode[RingID] - Y);
      }
      /////////////////////////////////////////////////////
    }
  }
}

void CHANTIReconstruction::ResoTimeEvaluation(TDCEvent * /*tCHANTIEvent*/){
  /////////////////////////////TIME RESOLUTION////////////////////////
  Int_t nHits = fRecoEvent->GetNHits();
  if(nHits > 0){

    Int_t ChannelID, ChannelIDbis;
    Int_t QualityFlag, QualityFlagbis;
    Int_t ThresholdFlag,ThresholdFlagbis;
    Double_t Time, Timebis;
    Int_t PlaneID, PlaneIDbis;
    TClonesArray & RecoHits = (* (fRecoEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
      TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      ChannelID = RecoHit->GetChannelID();
      QualityFlag = RecoHit->GetQualityFlag();
      Time = RecoHit->GetTime();
      PlaneID = RecoHit->GetPlaneID();
      ThresholdFlag = RecoHit->GetThresholdFlag();
      for(Int_t jHit = iHit+1; jHit<nHits; jHit++){
        TRecoCHANTIHit *RecoHitbis = static_cast<TRecoCHANTIHit*>(RecoHits[jHit]);
        ChannelIDbis = RecoHitbis->GetChannelID();
        QualityFlagbis = RecoHitbis->GetQualityFlag();
        Timebis = RecoHitbis->GetTime();
        PlaneIDbis = RecoHitbis->GetPlaneID();
        ThresholdFlagbis = RecoHitbis->GetThresholdFlag();
        if(QualityFlag != 0 || QualityFlagbis!= 0) continue ;//solo Hit con 2 (4) Edges
        if((abs(ChannelID - ChannelIDbis) == 10) && (ThresholdFlag == 2) && (ThresholdFlagbis == 2)) fHResoTime->Fill(Time - Timebis); //adjacent physical channels (only low threshold)
        if(PlaneID == PlaneIDbis) fHResoTimePlane[PlaneID]->Fill(Time - Timebis); //channels from same Plane
      }
    }
  }
}

void CHANTIReconstruction::SingleMuonResoTimeEvaluation(TDCEvent * /*tCHANTIEvent*/){
  /////////////////////////////TIME RESOLUTION////////////////////////
  Int_t nHits = fRecoEvent->GetNHits();
  if(fSingleMuon){
    Int_t ChannelID, ChannelIDbis;
    Int_t QualityFlag, QualityFlagbis;
    Int_t ThresholdFlag,ThresholdFlagbis;
    Double_t Time, Timebis;
    Int_t PlaneID, PlaneIDbis;
    TClonesArray & RecoHits = (* (fRecoEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
      TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      ChannelID = RecoHit->GetChannelID();
      QualityFlag = RecoHit->GetQualityFlag();
      Time = RecoHit->GetTime();
      PlaneID = RecoHit->GetPlaneID();
      ThresholdFlag = RecoHit->GetThresholdFlag();
      for(Int_t jHit = iHit+1; jHit<nHits; jHit++){
        TRecoCHANTIHit *RecoHitbis = static_cast<TRecoCHANTIHit*>(RecoHits[jHit]);
        ChannelIDbis = RecoHitbis->GetChannelID();
        QualityFlagbis = RecoHitbis->GetQualityFlag();
        Timebis = RecoHitbis->GetTime();
        PlaneIDbis = RecoHitbis->GetPlaneID();
        ThresholdFlagbis = RecoHitbis->GetThresholdFlag();
        if(QualityFlag != 0 || QualityFlagbis!= 0) continue ;//solo Hit con 2 (4) Edges
        if((abs(ChannelID - ChannelIDbis) == 10) && (ThresholdFlag == 2) && (ThresholdFlagbis == 2)) fHResoTimeMU->Fill(Time - Timebis); //adjacent physical channels (only low threshold)
        if(PlaneID == PlaneIDbis) fHResoTimePlaneMU[PlaneID]->Fill(Time - Timebis); //channels from same Plane
      }
    }
  }
}

void CHANTIReconstruction::EndProcessing(){
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  //if (fEvaluateT0) EvaluateT0s();
  SaveHistograms();
}

void CHANTIReconstruction::FillTimes(Double_t ReferenceTime) {

  // Common part for all the subdetectors
  NA62VReconstruction::FillTimes(ReferenceTime);

  Int_t NRecoHits = fRecoEvent->GetNHits();
  TClonesArray &Hits = (*(fRecoEvent->GetHits()));

  // RecoHit times wrt the reference time
  for (Int_t i=0; i<NRecoHits; i++) {
    TRecoCHANTIHit *Hit = static_cast<TRecoCHANTIHit*>( Hits[i]);
    if(Hit->GetThresholdFlag() == 2){
      Double_t Time            = Hit->GetTime();     // corrected for global T0 and channel T0s
      Double_t Width           = Hit->GetTimeWidth();
      Double_t dT1             = Time - ReferenceTime;
      Int_t    ch              = Hit->GetChannelID();
      Int_t    ROch            = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ch);
      Double_t T0              = (fEnableT0) ? fChannels[ROch]->GetT0() : 0.0;
      Double_t TimeNoCorr      = Time + T0;     // not corrected for global T0 and channel T0s
      Double_t dT1NoCorr       = TimeNoCorr - ReferenceTime;
      Double_t TimeHgTh        = Hit->GetTime();// corrected for global T0 and channel T0s
      Double_t WidthHgTh       = (Hit->GetQualityFlag() == 0) ? Hit->GetDeltaWidth() + Hit->GetTimeWidth() : -999;
      Double_t dT1HgTh         = TimeHgTh - ReferenceTime;
      Int_t    chHgTh          = Hit->GetChannelID() + 1;
      Int_t    ROchHgTh        = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(chHgTh);
      Double_t T0HgTh          = (fEnableT0) ? fChannels[ROchHgTh]->GetT0() : 0.0;
      Double_t TimeNoCorrHgTh  = TimeHgTh + T0HgTh;     // not corrected for global T0 and channel T0s
      Double_t dT1NoCorrHgTh   = TimeNoCorrHgTh - ReferenceTime;

      if (fChannelHistograms) {
	fChannels[ROch]->FillTime(Time, Width, ReferenceTime);
	fChannels[ROchHgTh]->FillTime(TimeHgTh, WidthHgTh, ReferenceTime);
      }

      fHRecoHitTimeWrtReference->Fill(dT1);
      if (fHRecoHitTimeWrtReferenceVsROChannel) {
	fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROch, dT1);
	fHRecoHitTimeWrtReferenceVsBurst->Fill(fRecoEvent->GetBurstID(), dT1);
      }
      if(Width > 0) fHRecoHitTimeWrtReferenceVsWidth->Fill(Width, dT1);

      fHRecoHitTimeWrtReferenceNoT0->Fill(dT1NoCorr);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) {
	fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROch, dT1NoCorr);
	fHRecoHitTimeWrtReferenceVsBurstNoT0->Fill(fRecoEvent->GetBurstID(), dT1NoCorr);
      }
      if(Width > 0) fHRecoHitTimeWrtReferenceVsWidthNoT0->Fill(Width, dT1NoCorr);

      fHRecoHitTimeWrtReference->Fill(dT1HgTh);
      if (fHRecoHitTimeWrtReferenceVsROChannel) {
	fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROchHgTh, dT1HgTh);
	fHRecoHitTimeWrtReferenceVsBurst->Fill(fRecoEvent->GetBurstID(), dT1HgTh);
      }
      if(WidthHgTh > 0) fHRecoHitTimeWrtReferenceVsWidth->Fill(WidthHgTh, dT1HgTh);

      fHRecoHitTimeWrtReferenceNoT0->Fill(dT1NoCorrHgTh);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) {
	fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROchHgTh, dT1NoCorrHgTh);
	fHRecoHitTimeWrtReferenceVsBurstNoT0->Fill(fRecoEvent->GetBurstID(), dT1NoCorrHgTh);
      }
      if(WidthHgTh > 0) fHRecoHitTimeWrtReferenceVsWidthNoT0->Fill(WidthHgTh, dT1NoCorrHgTh);
    }
    else if(Hit->GetThresholdFlag() == 0){
      Double_t Time            = Hit->GetTime();     // corrected for global T0 and channel T0s
      Double_t Width           = Hit->GetTimeWidth();
      Double_t dT1             = Time - ReferenceTime;
      Int_t    ch              = Hit->GetChannelID();
      Int_t    ROch            = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ch);
      Double_t T0              = (fEnableT0) ? fChannels[ROch]->GetT0() : 0.0;
      Double_t TimeNoCorr      = Time + T0;     // not corrected for global T0 and channel T0s
      Double_t dT1NoCorr       = TimeNoCorr - ReferenceTime;

      fHRecoHitTimeWrtReference->Fill(dT1);
      if (fHRecoHitTimeWrtReferenceVsROChannel) {
	fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROch, dT1);
	fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROch+1, dT1);
	fHRecoHitTimeWrtReferenceVsBurst->Fill(fRecoEvent->GetBurstID(), dT1);
      }
      if(Width > 0) fHRecoHitTimeWrtReferenceVsWidth->Fill(Width, dT1);

      fHRecoHitTimeWrtReferenceNoT0->Fill(dT1NoCorr);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) {
	fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROch, dT1NoCorr);
	fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROch+1, dT1NoCorr);
	fHRecoHitTimeWrtReferenceVsBurstNoT0->Fill(fRecoEvent->GetBurstID(), dT1NoCorr);
      }
      if(Width > 0) fHRecoHitTimeWrtReferenceVsWidthNoT0->Fill(Width, dT1NoCorr);
    }
  }

  // CHANTI candidate times wrt the Reference candidate
  for (Int_t iCand=0; iCand<fRecoEvent->GetNCandidates(); iCand++) {
    TRecoCHANTICandidate* CHANTICandidate = static_cast<TRecoCHANTICandidate*>(fRecoEvent->GetCandidate(iCand));
    //Channel Distance From SiPM Occupancy
    Int_t NumberOfGhost = CHANTICandidate->GetXYMult();
    if(NumberOfGhost ==1){
      Int_t CandNHit = CHANTICandidate->GetNHits();
      for (int CandHitIndex=0; CandHitIndex < CandNHit; CandHitIndex++){
	TRecoCHANTIHit* CCandRecoHit = static_cast<TRecoCHANTIHit*>(CHANTICandidate->GetHit(CandHitIndex));
	Int_t ROCH = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(CCandRecoHit->GetChannelID());
	Double_t Time = CCandRecoHit->GetTime();
	if (fChannelHistograms)	(static_cast<CHANTIChannel*>(fChannels[ROCH])->FillPosition(CHANTICandidate->GetXPos(),CHANTICandidate->GetYPos(),Time, ReferenceTime));
      }
    }
    Double_t dTcand = CHANTICandidate->GetTime()  - ReferenceTime;
    fHCandidateTimeWrtReference->Fill(dTcand);
    fHCandidateTimeWrtReferenceVsBurst->Fill(fRecoEvent->GetBurstID(), dTcand);
  }
}

/*
   void CHANTIReconstruction::EvaluateSlewingCorrections() {
   time_t now = time(0);
   ofstream outfile ("config/CHANTI-SlewingCorr.new");
   outfile << "### CHANTI slewing constants" << std::endl << endl;
   outfile << "# The slewing correction is a linear function of the signal width in"<< std::endl;
   outfile << "# the width interval of (MinWidth, MaxWidth) and constant outside this interval."<< std::endl;
   outfile << "# Data format: RO channel, corrections at width=MinWidth and width=MaxWidth."<< std::endl;
   outfile << "# The values of MinWidth and MaxWidth are indicated below."<< std::endl<<endl;
   outfile << "#\n# Generated on "<<asctime(localtime(&now));
   outfile << "#"<< std::endl;

   outfile << "MinWidth " << fNewMinWidth << std::endl;
   outfile << "MaxWidth " << fNewMaxWidth << std::endl;
   for (Int_t ich=0; ich<fNChannels; ich++) {
   if (fChannels[ich]->GetGeoChannelID()>0) {
   fChannels[ich]->EvaluateSlewingCorrection();
   outfile << Form("SlewingCorr %04d %6.4f %6.4f\n", ich,
   fChannels[ich]->GetNewSlewingCorr_MinWidth(), fChannels[ich]->GetNewSlewingCorr_MaxWidth());
   }
   }
   outfile.close();
   }
   */

void CHANTIReconstruction::InitHistograms()
{
  TDirectory *CHANTIDir = GetOrMakeDir(fHistoFile,"CHANTIMonitor");
  if (fChannelHistograms) {
    TDirectory *CHANTIChannelsDir = CHANTIDir->mkdir("CHANTIChannels");
    CHANTIChannelsDir->mkdir("T0");
    CHANTIChannelsDir->mkdir("Width");
    CHANTIChannelsDir->mkdir("Position");
    CHANTIChannelsDir->mkdir("SlewingCorr");
  }
  fHistoFile->cd("CHANTIMonitor");

  const Int_t NTrigs = 2;
  TString TrigSuffix[NTrigs] = {
    "",
    "EOB"
  };

  fHNTriggersPerBurst = new TH1D("NTriggersPerBurst", "Number of Triggers per Burst", 10001,-0.5,10000.5);
  fHNTriggersPerBurst->GetXaxis()->SetTitle("BurstID");
  fHNTriggersPerBurst->GetYaxis()->SetTitle("Triggers");

  fHXHit = new TH1D("XHitInCHANTI","X Hit in CHANTI",17,-134.,134.);
  fHXHit->GetXaxis()->SetTitle("X (mm)");
  fHXHit->GetYaxis()->SetTitle("Occupancy");
  fHYHit = new TH1D("YHitInCHANTI","Y Hit in CHANTI",17,-134.,134.);
  fHYHit->GetXaxis()->SetTitle("Y (mm)");
  fHYHit->GetYaxis()->SetTitle("Occupancy");
  fHXHitEvent1 = new TH1D("XHitInCHANTI1","X Hit in CHANTI 1",9,-fStep*9.5,fStep*8.5);
  fHXHitEvent2 = new TH1D("XHitInCHANTI2","X Hit in CHANTI 2",9,-fStep*8.5,fStep*9.5);
  fHYHitEvent1 = new TH1D("YHitInCHANTI1","Y Hit in CHANTI 1",9,-fStep*9.5,fStep*8.5);
  fHYHitEvent2 = new TH1D("YHitInCHANTI2","Y Hit in CHANTI 2",9,-fStep*8.5,fStep*9.5);
  fHDispPlotX = new TH1D("DispersionPlotX","Dispersion plot X",19,-fStep*9.5,fStep*9.5);
  fHDispPlotY = new TH1D("DispersionPlotY","Dispersion plot Y",19,-fStep*9.5,fStep*9.5);
  fHDispPlotXMU = new TH1D("DispersionPlotXMU","Dispersion plot X mu",19,-fStep*9.5,fStep*9.5);
  fHDispPlotYMU = new TH1D("DispersionPlotYMU","Dispersion plot Y mu",19,-fStep*9.5,fStep*9.5);
  fHNHit = new TH1D("NumberOflectronicHitInCHANTI","Number of Electornic Hit In CHANTI per event",150,-0.5,149.5);
  fHNHitAllRing = new TH1D("NumberOfElectronicHitInCHANTIAllRingFired","Number Of Electronic Hit In CHANTI per event All Ring Fired",150,-0.5,149.5);
  fHNRing = new TH1D("NumberOfFiredRingInCHANTI","Number Of Ring fired In CHANTI per event",15,-0.5,14.5);
  fHNPlane = new TH1D("NumberOfFiredPlaneInCHANTI","Number Of Plane fired In CHANTI per event",8,-0.5,7.5);
  fHNRingVSNHit = new TH2F("NOfFiredRingVSNHit","N Of Ring fired VS N Hit",150,-0.5,149.5,15,-0.5,14.5);
  fHNPlaneVSNHit = new TH2F("NOfFiredPlaneVSHit","N Of Plane fired VS N Hit",150,-0.5,149.5,8,-0.5,7.5);

  fHCorrTimeVSZ = new TH2F("CorrTimeVSZ","Time vs Z",200,-100.,1900.,300,-21.,279.);
  fHCorrTimeVSZMU = new TH2F("MUCorrTimeVSZ","Time vs Z",200,-100.,1900.,300,-21.,279.);
  fHContOfModeX = new TH1D("CountOfModeX0","Count Of Mode X 0",100,0.,100.);
  fHContOfModeX1 = new TH1D("CountOfModeX1","Count Of Mode X 1",100,0.,100.);
  fHContOfModeX2 = new TH1D("CountOfModeX2","Count Of Mode X 2",100,0.,100.);
  fHContOfModeX3 = new TH1D("CountOfModeX3","Count Of Mode X 3",100,0.,100.);
  fHContOfModeY = new TH1D("CountOfModeY0","Count Of Mode Y 0",100,0.,100.);
  fHContOfModeY1 = new TH1D("CountOfModeY1","Count Of Mode Y 1",100,0.,100.);
  fHContOfModeY2 = new TH1D("CountOfModeY2","Count Of Mode Y 2",100,0.,100.);
  fHContOfModeY3 = new TH1D("CountOfModeY3","Count Of Mode Y 3",100,0.,100.);

  if(fNRings>0){
    fHNHitsForRing = new TH1D* [fNRings];
    fHToTRing0 = new TH1D* [fNRings];
    fHToTRing1 = new TH1D* [fNRings];
    fHToTRing2 = new TH1D* [fNRings];
    fHBarID = new TH1D* [fNRings];
    fHCorrTimeShiftMU = new TH1D* [fNRings];
    fHCorrTime = new TH1D* [fNRings];
    fHCorrTimeDispPlot = new TH1D* [fNRings];
    fHCorrTimeDispPlotMU = new TH1D* [fNRings];
    fHPositionLow = new TH1D** [fNRings];
    fHPositionHigh = new TH1D** [fNRings];
    fHPosition1 = new TH1D* [fNRings];
    fHPosition2 = new TH1D* [fNRings];

    fHTimeVsDistanceMU = new TH2F* [fNRings];

    if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
      fHEffi1Up = new TH1D* [fNRings];
      fHEffi2Up = new TH1D* [fNRings];
      fHEffi1Down = new TH1D* [fNRings];
      fHEffi2Down = new TH1D* [fNRings];
      fHEffiPosition1Up = new TH1D* [fNRings];
      fHEffiPosition2Up = new TH1D* [fNRings];
      fHEffiPosition1Down = new TH1D* [fNRings];
      fHEffiPosition2Down = new TH1D* [fNRings];
      fHEffiHigh1Up = new TH1D* [fNRings];
      fHEffiHigh2Up = new TH1D* [fNRings];
      fHEffiHigh1Down = new TH1D* [fNRings];
      fHEffiHigh2Down = new TH1D* [fNRings];
      fHEffiPositionHigh1Up = new TH1D* [fNRings];
      fHEffiPositionHigh2Up = new TH1D* [fNRings];
      fHEffiPositionHigh1Down = new TH1D* [fNRings];
      fHEffiPositionHigh2Down = new TH1D* [fNRings];
    }
    fHDispPlot = new TH1D* [fNRings];
    fHDispPlotMU = new TH1D* [fNRings];
    fHNHitsForRingVSBurst = new TH1D* [fNRings];
    fHBarEffiNum = new TH1D* [fNRings];
    fHBarEffiDen = new TH1D* [fNRings];
  }
  else std::cout << "[CHANTIReconstruction] Error: Invalid number of fNRings: " << fNRings << std::endl;

  for(int iRing = 0; iRing < fNRings; iRing++){
    //
    fHBarEffiNum[iRing] = new TH1D(Form("NumBarEffiInRing%d",iRing+1),Form("Num Bar Efficiency In Ring %d per event",iRing+1),24,0.5,24.5);
    fHBarEffiDen[iRing] = new TH1D(Form("DenBarEffiInRing%d",iRing+1),Form("Den Bar Efficiency In Ring %d per event",iRing+1),24,0.5,24.5);;
    fHBarEffiNum[iRing]->GetXaxis()->SetTitle("Bar ID");
    fHBarEffiDen[iRing]->GetXaxis()->SetTitle("Bar ID");
    //
    fHPositionLow[iRing] = new TH1D*[NTrigs];
    fHPositionHigh[iRing] = new TH1D*[NTrigs];
    fHNHitsForRing[iRing] = new TH1D(Form("NumberOfHitInCHANTIRing%d",iRing+1),Form("Number Of Hit In CHANTI Ring%d per event",iRing+1),100,-0.5,99.5);
    fHToTRing0[iRing] = new TH1D(Form("ToTLInRing%d",iRing+1),Form("ToT In Ring%d per event",iRing+1),200,-0.5,199.5);
    fHToTRing1[iRing] = new TH1D(Form("ToTHInRing%d",iRing+1),Form("ToT In Ring%d per event",iRing+1),200,-0.5,199.5);
    fHToTRing2[iRing] = new TH1D(Form("ToTLBothInRing%d",iRing+1),Form("ToT In Ring%d per event",iRing+1),200,-0.5,199.5);
    fHBarID[iRing] = new TH1D(Form("Ring%dChannelOccupancy",iRing+1),Form("Ring%d Channel Occupancy",iRing+1),339,-169.5,169.5);
    fHBarID[iRing]->GetXaxis()->SetTitle("Bar ID");
    fHBarID[iRing]->GetYaxis()->SetTitle("# Hit");
    fHCorrTimeShiftMU[iRing] = new TH1D(Form("TimeModeRing%d_TimeMode0MU",iRing+1),Form("Time mode ring %d - Time mode 0 low Th MU",iRing+1),400,-1999.5*TdcCalib,2000.5*TdcCalib);
    fHCorrTime[iRing] = new TH1D(Form("CorrTimeInRing%d",iRing+1),Form("Time In Ring%d per event",iRing+1),200,-999.5*TdcCalib,1000.5*TdcCalib);
    fHCorrTimeDispPlot[iRing] = new TH1D(Form("CorrTimeDisp%d",iRing+1),Form("Time disp %d per event",iRing+1),800,-1999.5*TdcCalib,2000.5*TdcCalib);
    fHCorrTimeDispPlotMU[iRing] = new TH1D(Form("MUCorrTimeDisp%d",iRing+1),Form("MU Time disp %d per event",iRing+1),800,-1999.5*TdcCalib,2000.5*TdcCalib);
    fHTimeVsDistanceMU[iRing] = new TH2F(Form("TimeVsPositionInRing%d",iRing+1),Form("Time Vs Position In Ring%d",iRing+1),20,-fStep*0.5,fStep*19.5,400,-100.,100.);
    if(iRing%2 == 0){
      for(Int_t iTrig=0; iTrig<NTrigs;iTrig++){
        fHPositionLow[iRing][iTrig] = new TH1D(Form("XInPlane%d%s",iRing/2+1,TrigSuffix[iTrig].Data()),Form("X In Plane%d%s per event low th",iRing/2+1,TrigSuffix[iTrig].Data()),17,-134,134);
        fHPositionHigh[iRing][iTrig] = new TH1D(Form("XInPlane%dHighTh%s",iRing/2+1,TrigSuffix[iTrig].Data()),Form("X In Plane%d%s per event high th",iRing/2+1,TrigSuffix[iTrig].Data()),17,-134,134);
      }
      fHPosition1[iRing] = new TH1D(Form("XInPlane%d_1",iRing/2+1),Form("X In Plane%d per event 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
      fHPosition2[iRing] = new TH1D(Form("XInPlane%d_2",iRing/2+1),Form("X In Plane%d per event 2",iRing/2+1),9,-fStep*8.5,fStep*9.5);

      if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
        fHEffi1Up[iRing] = new TH1D(Form("XInPlane%d_1Up",iRing/2+1),Form("Y In Plane%d per event Up 1",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffi2Up[iRing] = new TH1D(Form("XInPlane%d_2Up",iRing/2+1),Form("Y In Plane%d per event Up 2",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffi1Down[iRing] = new TH1D(Form("XInPlane%d_1Down",iRing/2+1),Form("Y In Plane%d per event Down 1",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffi2Down[iRing] = new TH1D(Form("XInPlane%d_2Down",iRing/2+1),Form("Y In Plane%d per event Down 2",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiPosition1Up[iRing] = new TH1D(Form("XInPlane%d_1SideUp",iRing/2+1),Form("X In Plane%d per event Side Up 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
        fHEffiPosition2Up[iRing] = new TH1D(Form("XInPlane%d_2SideUp",iRing/2+1),Form("X In Plane%d per event Side Up 2",iRing/2+1),9,-fStep*8.5,fStep*9.5);
        fHEffiPosition1Down[iRing] = new TH1D(Form("XInPlane%d_1SideDown",iRing/2+1),Form("X In Plane%d per event Side Down 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
        fHEffiPosition2Down[iRing] = new TH1D(Form("XInPlane%d_2SideDown",iRing/2+1),Form("X In Plane%d per event 2 Side Down",iRing/2+1),9,-fStep*8.5,fStep*9.5);
        fHEffiHigh1Up[iRing] = new TH1D(Form("XInPlane%d_High1Up",iRing/2+1),Form("HighX In Plane%d per event Up 1",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiHigh2Up[iRing] = new TH1D(Form("XInPlane%d_High2Up",iRing/2+1),Form("HighX In Plane%d per event Up 2",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiHigh1Down[iRing] = new TH1D(Form("XInPlane%d_High1Down",iRing/2+1),Form("HighX In Plane%d per event Down 1",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiHigh2Down[iRing] = new TH1D(Form("XInPlane%d_High2Down",iRing/2+1),Form("HighX In Plane%d per event Down 2",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiPositionHigh1Up[iRing] = new TH1D(Form("XInPlane%d_High1SideUp",iRing/2+1),Form("HighX In Plane%d per event Side Up 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
        fHEffiPositionHigh2Up[iRing] = new TH1D(Form("XInPlane%d_High2SideUp",iRing/2+1),Form("HighX In Plane%d per event Side Up 2",iRing/2+1),9,-fStep*8.5,fStep*9.5);
        fHEffiPositionHigh1Down[iRing] = new TH1D(Form("XInPlane%d_High1SideDown",iRing/2+1),Form("HighX In Plane%d per event Side Down 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
        fHEffiPositionHigh2Down[iRing] = new TH1D(Form("XInPlane%d_High2SideDown",iRing/2+1),Form("HighX In Plane%d per event Side Down 2",iRing/2+1),9,-fStep*8.5,fStep*9.5);
      }
      fHDispPlot[iRing] = new TH1D(Form("DispersionPlotXInPlane%d",iRing/2+1),Form("Dispersion plot X In Plane %d",iRing/2+1),19,-fStep*9.5,fStep*9.5);
      fHDispPlotMU[iRing] = new TH1D(Form("DispersionPlotXInPlane%d MU",iRing/2+1),Form("Dispersion plot X In Plane %d MU",iRing/2+1),19,-fStep*9.5,fStep*9.5);
    }else{
      for(Int_t iTrig=0; iTrig<NTrigs;iTrig++){
        fHPositionLow[iRing][iTrig] = new TH1D(Form("YInPlane%d%s",iRing/2+1,TrigSuffix[iTrig].Data()),Form("Y In Plane%d%s per event low th",iRing/2+1,TrigSuffix[iTrig].Data()),17,-134,134);
        fHPositionHigh[iRing][iTrig] = new TH1D(Form("YInPlane%dHighTh%s",iRing/2+1,TrigSuffix[iTrig].Data()),Form("Y In Plane%d%s per event high th",iRing/2+1,TrigSuffix[iTrig].Data()),17,-134,134);
      }
      fHPosition1[iRing] = new TH1D(Form("YInPlane%d_1",iRing/2+1),Form("Y In Plane%d per event 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
      fHPosition2[iRing] = new TH1D(Form("YInPlane%d_2",iRing/2+1),Form("Y In Plane%d per event 2",iRing/2+1),9,-fStep*8.5,fStep*9.5);
      if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
        fHEffi1Up[iRing] = new TH1D(Form("YInPlane%d_1Up",iRing/2+1),Form("Y In Plane%d per event Up 1",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffi2Up[iRing] = new TH1D(Form("YInPlane%d_2Up",iRing/2+1),Form("Y In Plane%d per event Up 2",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffi1Down[iRing] = new TH1D(Form("YInPlane%d_1Down",iRing/2+1),Form("Y In Plane%d per event Down 1",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffi2Down[iRing] = new TH1D(Form("YInPlane%d_2Down",iRing/2+1),Form("Y In Plane%d per event Down 2",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiPosition1Up[iRing] = new TH1D(Form("YInPlane%d_1SideUp",iRing/2+1),Form("Y In Plane%d per event Side Up 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
        fHEffiPosition2Up[iRing] = new TH1D(Form("YInPlane%d_2SideUp",iRing/2+1),Form("Y In Plane%d per event Side Up 2",iRing/2+1),9,-fStep*8.5,fStep*9.5);
        fHEffiPosition1Down[iRing] = new TH1D(Form("YInPlane%d_1SideDown",iRing/2+1),Form("Y In Plane%d per event Side Down 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
        fHEffiPosition2Down[iRing] = new TH1D(Form("YInPlane%d_2SideDown",iRing/2+1),Form("Y In Plane%d per event 2 Side Down",iRing/2+1),9,-fStep*8.5,fStep*9.5);
        fHEffiHigh1Up[iRing] = new TH1D(Form("YInPlane%d_High1Up",iRing/2+1),Form("HighY In Plane%d per event Up 1",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiHigh2Up[iRing] = new TH1D(Form("YInPlane%d_High2Up",iRing/2+1),Form("HighY In Plane%d per event Up 2",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiHigh1Down[iRing] = new TH1D(Form("YInPlane%d_High1Down",iRing/2+1),Form("HighY In Plane%d per event Down 1",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiHigh2Down[iRing] = new TH1D(Form("YInPlane%d_High2Down",iRing/2+1),Form("HighY In Plane%d per event Down 2",iRing/2+1),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
        fHEffiPositionHigh1Up[iRing] = new TH1D(Form("YInPlane%d_High1SideUp",iRing/2+1),Form("HighY In Plane%d per event Side Up 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
        fHEffiPositionHigh2Up[iRing] = new TH1D(Form("YInPlane%d_High2SideUp",iRing/2+1),Form("HighY In Plane%d per event Side Up 2",iRing/2+1),9,-fStep*8.5,fStep*9.5);
        fHEffiPositionHigh1Down[iRing] = new TH1D(Form("YInPlane%d_High1SideDown",iRing/2+1),Form("HighY In Plane%d per event Side Down 1",iRing/2+1),9,-fStep*9.5,fStep*8.5);
        fHEffiPositionHigh2Down[iRing] = new TH1D(Form("YInPlane%d_High2SideDown",iRing/2+1),Form("HighY In Plane%d per event Side Down 2",iRing/2+1),9,-fStep*8.5,fStep*9.5);
      }
      fHDispPlot[iRing] = new TH1D(Form("DispersionPlotYInPlane%d",iRing/2+1),Form("Dispersion plot Y In Plane %d",iRing/2+1),19,-fStep*9.5,fStep*9.5);
      fHDispPlotMU[iRing] = new TH1D(Form("DispersionPlotYInPlane%dMU",iRing/2+1),Form("Dispersion plot Y In Plane %d MU",iRing/2+1),19,-fStep*9.5,fStep*9.5);
      //
    }
    fHNHitsForRingVSBurst[iRing]= new TH1D(Form("NHitsForRing%dVSBurst",iRing+1),Form("N Hit in Layer %d VS Burst",iRing+1),100,0.5,100.5);
  }
  //
  fHRecoHitTimeWrtReference = new TH1D("RecoHitTimeWrtReference", "Reco Hit Time Wrt Reference",120, -30, 30);
  fHRecoHitTimeWrtReferenceVsBurst = new TH2F
    ("RecoHitTimeWrtReferenceVsBurst", "Reco Hit Time Wrt Reference Vs Burst", 100, 0.5, 100.5, 120, -30, 30);
  fHRecoHitTimeWrtReferenceVsWidth = new TH2F
    ("RecoHitTimeWrtReferenceVsWidth", "Reco Hit Time Wrt Reference Vs Width", 195, 4.5, 199.5, 120, -30, 30);
  //
  fHRecoHitTimeWrtReferenceNoT0 = new TH1D("RecoHitTimeWrtReferenceNoT0", "Reco Hit Time No Corr Wrt Reference",120, -30, 30);
  fHRecoHitTimeWrtReferenceVsBurstNoT0 = new TH2F
    ("RecoHitTimeWrtReferenceVsBurstNoT0", "Reco Hit Time No Corr Wrt Reference Vs Burst", 100, 0.5, 100.5, 120, -30, 30);
  fHRecoHitTimeWrtReferenceVsWidthNoT0 = new TH2F
    ("RecoHitTimeWrtReferenceVsWidthNoT0", "Reco Hit Time No Corr Wrt Reference Vs Width", 195, 4.5, 199.5, 120, -30, 30);
  //
  fHCandidateTimeWrtReference = new TH1D
    ("CandidateTimeWrtReference", "Candidate Time Wrt Reference",120, -30, 30);
  fHCandidateTimeWrtReferenceVsBurst = new TH2F
    ("CandidateTimeWrtReferenceVsBurst", "Candidate Time Wrt Reference Vs Burst", 100, 0.5, 100.5, 120, -30, 30);
  //
  Double_t xbins[5];
  Double_t ybins[4];
  xbins[0]=-150;
  xbins[1]=-47.5;
  xbins[2]=0;
  xbins[3]=47.5;
  xbins[4]=150;
  ybins[0]=-150;
  ybins[1]=-32.5;
  ybins[2]=32.5;
  ybins[3]=150;
  //

  if(fNPlanes>0){
    fHHitTimeIn = new TH1D* [fNPlanes];
    fHHitTimeOut = new TH1D* [fNPlanes];
    fHHitTimeInMU = new TH1D* [fNPlanes];
    fHHitTimeOutMU = new TH1D* [fNPlanes];
    fHHitTimeInAll = new TH1D* [fNPlanes];
    fHHitTimeOutAll = new TH1D* [fNPlanes];
    //
    if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
      fHEffiPlotXDen = new TH1D* [fNPlanes];
      fHEffiPlotYDen = new TH1D* [fNPlanes];
      fHEffiPlotXNum = new TH1D* [fNPlanes];
      fHEffiPlotYNum = new TH1D* [fNPlanes];
      fHEffiPlotXYDen = new TH2F* [fNPlanes];
      fHEffiPlotXYNum = new TH2F* [fNPlanes];
      fHEffiPlotHighXNum = new TH1D* [fNPlanes];
      fHEffiPlotHighYNum = new TH1D* [fNPlanes];
    }
    //
    fHNHitsForPlane = new TH1D* [fNPlanes];
    fHToTPlane0 = new TH1D* [fNPlanes];
    fHToTPlane1 = new TH1D* [fNPlanes];
    fHToTPlane2 = new TH1D* [fNPlanes];
    /////////////////////////////////////////////
    fHViewX1THRL = new TH2F** [fNPlanes];
    fHViewX2THRL = new TH2F** [fNPlanes];
    fHViewY1THRL = new TH2F** [fNPlanes];
    fHViewY2THRL = new TH2F** [fNPlanes];

    fHViewX1THRH = new TH2F** [fNPlanes];
    fHViewX2THRH = new TH2F** [fNPlanes];
    fHViewY1THRH = new TH2F** [fNPlanes];
    fHViewY2THRH = new TH2F** [fNPlanes];

    fHResoTimePlane = new TH1D* [fNPlanes];
    fHResoTimePlaneMU = new TH1D* [fNPlanes];
    fHXYClusterPerPlane = new TH2F* [fNPlanes];
    fHNHitsForPlaneVSBurst = new TH1D* [fNPlanes];
  }
  else std::cout << "[CHANTIReconstruction] Error: Invalid number of fNPlanes: " << fNPlanes << std::endl;

  for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
    //
    fHHitTimeIn[iPlane] = new TH1D(Form("HitIn%d",iPlane+1),Form("Hit In %d",iPlane+1),800,0.,800.);
    fHHitTimeOut[iPlane] = new TH1D(Form("HitOut%d",iPlane+1),Form("Hit In %d",iPlane+1),800,0.,800.);
    fHHitTimeInMU[iPlane] = new TH1D(Form("HitInMU%d",iPlane+1),Form("Hit In MU %d",iPlane+1),800,0.,800.);
    fHHitTimeOutMU[iPlane] = new TH1D(Form("HitOutMU%d",iPlane+1),Form("Hit In MU %d",iPlane+1),800,0.,800.);
    fHHitTimeInAll[iPlane] = new TH1D(Form("HitInAll%d",iPlane+1),Form("Hit In All %d",iPlane+1),800,0.,800.);
    fHHitTimeOutAll[iPlane] = new TH1D(Form("HitOutAll%d",iPlane+1),Form("Hit In All %d",iPlane+1),800,0.,800.);
    //
    if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
      fHEffiPlotXDen[iPlane] = new TH1D(Form("EffiDenX%d",iPlane+1),Form("Den Efficiency X %d",iPlane+1),9,-fStep*9.5,fStep*8.5);
      fHEffiPlotYDen[iPlane] = new TH1D(Form("EffiDenY%d",iPlane+1),Form("Den Efficiency Y %d",iPlane+1),9,-fStep*9.5,fStep*8.5);
      fHEffiPlotXNum[iPlane] = new TH1D(Form("EffiNumX%d",iPlane+1),Form("Num Efficiency X %d",iPlane+1),9,-fStep*9.5,fStep*8.5);
      fHEffiPlotYNum[iPlane] = new TH1D(Form("EffiNumY%d",iPlane+1),Form("Num Efficiency Y %d",iPlane+1),9,-fStep*9.5,fStep*8.5);
      fHEffiPlotHighXNum[iPlane] = new TH1D(Form("EffiNumHighX%d",iPlane+1),Form("Num Efficiency HighX %d",iPlane+1),9,-fStep*9.5,fStep*8.5);
      fHEffiPlotHighYNum[iPlane] = new TH1D(Form("EffiNumHighY%d",iPlane+1),Form("Num Efficiency HighY %d",iPlane+1),9,-fStep*9.5,fStep*8.5);
      fHEffiPlotXYDen[iPlane] = new TH2F(Form("EffiDenXY%d",iPlane+1),Form("Den Efficiency XY %d",iPlane+1),9,-fStep*9.5,fStep*8.5,9,-fStep*9.5,fStep*8.5);
      fHEffiPlotXYNum[iPlane] = new TH2F(Form("EffiNumXY%d",iPlane+1),Form("Num Efficiency XY %d",iPlane+1),9,-fStep*9.5,fStep*8.5,9,-fStep*9.5,fStep*8.5);
    }
    //
    fHNHitsForPlane[iPlane] = new TH1D(Form("NHitCHANTIPlane%dPerEvent",iPlane+1),Form("Number Of Hit In CHANTI Plane%d per event",iPlane+1),100,-0.5,99.5);
    fHToTPlane0[iPlane] = new TH1D(Form("ToTLInPlane%dPerEvent",iPlane+1),Form("ToT In Plane%d per event",iPlane+1),200,-0.5,199.5);
    fHToTPlane1[iPlane] = new TH1D(Form("ToTHInPlane%dPerEvent",iPlane+1),Form("ToT In Plane%d per event",iPlane+1),200,-0.5,199.5);
    fHToTPlane2[iPlane] = new TH1D(Form("ToTLBothInPlane%dPerEvent",iPlane+1),Form("ToT In Plane%d per event",iPlane+1),200,-0.5,199.5);

    fHResoTimePlane[iPlane] = new TH1D(Form("TimeResoPlane%d",iPlane+1),Form("Time reso Plane %d",iPlane+1),400,-199.5*TdcCalib,200.5*TdcCalib);
    fHResoTimePlaneMU[iPlane] = new TH1D(Form("TimeResoPlane%dMU",iPlane+1),Form("Time reso Plane %d MU",iPlane+1),400,-199.5*TdcCalib,200.5*TdcCalib);
    fHXYClusterPerPlane[iPlane] = new TH2F(Form("XYClusterInPlane%d",iPlane+1),Form("XYCluster In Plane %d",iPlane+1),201,-fStep*10.5,fStep*10.5,201,-fStep*10.5,fStep*10.5);
    fHXYClusterPerPlane[iPlane]->GetXaxis()->SetTitle("X (mm)");
    fHXYClusterPerPlane[iPlane]->GetYaxis()->SetTitle("Y (mm)");
    fHNHitsForPlaneVSBurst[iPlane]= new TH1D(Form("NHitsForPlane%dVSBurst",iPlane+1),Form("N Hit in Plane %d VS Burst",iPlane+1),100,0.5,100.5);
    fHNHitsForPlaneVSBurst[iPlane]->GetXaxis()->SetTitle("Burst ID");
    fHNHitsForPlaneVSBurst[iPlane]->GetYaxis()->SetTitle("# Hit");

    fHViewX1THRL[iPlane] = new TH2F*[NTrigs];
    fHViewX2THRL[iPlane] = new TH2F*[NTrigs];
    fHViewY1THRL[iPlane] = new TH2F*[NTrigs];
    fHViewY2THRL[iPlane] = new TH2F*[NTrigs];
    fHViewX1THRH[iPlane] = new TH2F*[NTrigs];
    fHViewX2THRH[iPlane] = new TH2F*[NTrigs];
    fHViewY1THRH[iPlane] = new TH2F*[NTrigs];
    fHViewY2THRH[iPlane] = new TH2F*[NTrigs];

    for(Int_t iTrig=0; iTrig<NTrigs;iTrig++){

      fHViewX1THRL[iPlane][iTrig] = new TH2F(Form("BarOccupancyForX1viewAndLowTHRInPlane%d%s",iPlane+1,TrigSuffix[iTrig].Data()),Form("Bar rate (KHz) for X1 view and low THR In Plane %d %s",iPlane+1,TrigSuffix[iTrig].Data()),8 ,-132, 132, 3, ybins);
      fHViewX2THRL[iPlane][iTrig] = new TH2F(Form("BarOccupancyForX2viewAndLowTHRInPlane%d%s",iPlane+1,TrigSuffix[iTrig].Data()),Form("Bar rate (KHz) for X2 view and low THR In Plane %d %s",iPlane+1,TrigSuffix[iTrig].Data()),9 ,-148.5, 148.5, 3, ybins);
      fHViewY1THRL[iPlane][iTrig] = new TH2F(Form("BarOccupancyForY1viewAndLowTHRInPlane%d%s",iPlane+1,TrigSuffix[iTrig].Data()),Form("Bar rate (KHz) for Y1 view and low THR In Plane %d %s",iPlane+1,TrigSuffix[iTrig].Data()), 4, xbins, 9,-148.5, 148.5);
      fHViewY2THRL[iPlane][iTrig] = new TH2F(Form("BarOccupancyForY2viewAndLowTHRInPlane%d%s",iPlane+1,TrigSuffix[iTrig].Data()),Form("Bar rate (KHz) for Y2 view and low THR In Plane %d %s",iPlane+1,TrigSuffix[iTrig].Data()), 4, xbins, 8 ,-132, 132);

      fHViewX1THRL[iPlane][iTrig]->GetXaxis()->SetTitle("X (mm)");
      fHViewX1THRL[iPlane][iTrig]->GetYaxis()->SetTitle("Y (mm)");
      fHViewY1THRL[iPlane][iTrig]->GetXaxis()->SetTitle("X (mm)");
      fHViewY1THRL[iPlane][iTrig]->GetYaxis()->SetTitle("Y (mm)");
      fHViewX2THRL[iPlane][iTrig]->GetXaxis()->SetTitle("X (mm)");
      fHViewX2THRL[iPlane][iTrig]->GetYaxis()->SetTitle("Y (mm)");
      fHViewY2THRL[iPlane][iTrig]->GetXaxis()->SetTitle("X (mm)");
      fHViewY2THRL[iPlane][iTrig]->GetYaxis()->SetTitle("Y (mm)");

      fHViewX1THRH[iPlane][iTrig] = new TH2F(Form("BarOccupancyForX1viewAndHighTHRInPlane%d%s",iPlane+1,TrigSuffix[iTrig].Data()),Form("Barrate (KHz) for X1 view and high THR In Plane %d %s",iPlane+1,TrigSuffix[iTrig].Data()),8 ,-132, 132, 3, ybins);
      fHViewX2THRH[iPlane][iTrig] = new TH2F(Form("BarOccupancyForX2viewAndHighTHRInPlane%d%s",iPlane+1,TrigSuffix[iTrig].Data()),Form("Barrate (KHz) for X2 view and high THR In Plane %d %s",iPlane+1,TrigSuffix[iTrig].Data()),9 ,-148.5, 148.5, 3, ybins);
      fHViewY1THRH[iPlane][iTrig] = new TH2F(Form("BarOccupancyForY1viewAndHighTHRInPlane%d%s",iPlane+1,TrigSuffix[iTrig].Data()),Form("Barrate (KHz) for Y1 view and high THR In Plane %d %s",iPlane+1,TrigSuffix[iTrig].Data()), 4, xbins, 9,-148.5, 148.5);
      fHViewY2THRH[iPlane][iTrig] = new TH2F(Form("BarOccupancyForY2viewAndHighTHRInPlane%d%s",iPlane+1,TrigSuffix[iTrig].Data()),Form("Barrate (KHz) for Y2 view and high THR In Plane %d %s",iPlane+1,TrigSuffix[iTrig].Data()), 4, xbins, 8 ,-132, 132);

      fHViewX1THRH[iPlane][iTrig]->GetXaxis()->SetTitle("X (mm)");
      fHViewX1THRH[iPlane][iTrig]->GetYaxis()->SetTitle("Y (mm)");
      fHViewY1THRH[iPlane][iTrig]->GetXaxis()->SetTitle("X (mm)");
      fHViewY1THRH[iPlane][iTrig]->GetYaxis()->SetTitle("Y (mm)");
      fHViewX2THRH[iPlane][iTrig]->GetXaxis()->SetTitle("X (mm)");
      fHViewX2THRH[iPlane][iTrig]->GetYaxis()->SetTitle("Y (mm)");
      fHViewY2THRH[iPlane][iTrig]->GetXaxis()->SetTitle("X (mm)");
      fHViewY2THRH[iPlane][iTrig]->GetYaxis()->SetTitle("Y (mm)");
    }
  }
  fHToT0 = new TH1D("ToTLInCHANTILowThreshold","ToT In CHANTI low threshold",200,-50.5*TdcCalib,1949.5*TdcCalib);
  fHToT1 = new TH1D("ToTHInCHANTIHighThreshold","ToT In CHANTI high threshold",200,-50.5*TdcCalib,1949.5*TdcCalib);
  fHToT2 = new TH1D("ToTLBothInCHANTI","ToT In CHANTI low threshold",200,-50.5*TdcCalib,1949.5*TdcCalib);
  fHResoTime = new TH1D("TimeReso","Time reso",400,-199.5*TdcCalib,200.5*TdcCalib);
  fHResoTimeMU = new TH1D("TimeResoMU","Time reso MU",400,-199.5*TdcCalib,200.5*TdcCalib);
  fHTimeVSSlot = new TH2F("TimevsSlot","Time vs Slot",100,-55.5,44.5,7000,-1999.5*TdcCalib,5000.5*TdcCalib);
  fHSlewingVSToTLow = new TH2F("SlewingCorrectionvsToTLowTh","Slewing correction vs ToT low th",2000,-0.5*TdcCalib,1999.5*TdcCalib,500,-0.5*TdcCalib,499.5*TdcCalib);
  fHSlewingVSToTHigh = new TH2F("SlewingCorrectionvsToTHighTh","Slewing correction vs ToT high th",2000,-0.5*TdcCalib,1999.5*TdcCalib,500,-0.5*TdcCalib,499.5*TdcCalib);
  fHSlewingVSToTLowFit = new TH2F("SlewingCorrectionvsToTLowThFit","Slewing correction vs ToT low th Fit",2000,-0.5*TdcCalib,1999.5*TdcCalib,500,-0.5*TdcCalib,499.5*TdcCalib);
  fHSlewingVSToTHighFit = new TH2F("SlewingCorrectionvsToTHighThFit","Slewing correction vs ToT high th Fit",2000,-0.5*TdcCalib,1999.5*TdcCalib,500,-0.5*TdcCalib,499.5*TdcCalib);
  fHModeX = new TH1D("ModeXInCHANTI","Mode X In CHANTI",20,-fStep*10,fStep*10);
  fHModeY = new TH1D("ModeYInCHANTI","Mode Y In CHANTI",20,-fStep*10,fStep*10);

  fHModeYvsModeXSingleMuon = new TH2F("ModeYvsModeXMU","Mode Y vs Mode X MU",201,-fStep*10.5,fStep*10.5,201,-fStep*10.5,fStep*10.5);
  fHXYCluster = new TH2F("XYCluster","XYCluster",201,-fStep*10.5,fStep*10.5,201,-fStep*10.5,fStep*10.5);
  fHXYCluster->GetXaxis()->SetTitle("X (mm)");
  fHXYCluster->GetYaxis()->SetTitle("Y (mm)");

  //,320,-160,160,320,-160,160);

  Int_t nROBoards = fRawDecoder->GetDecoder()->GetNROBoards();
  fHNWordPerTell = new TH1D* [nROBoards];
  fHNWordPerTDCB = new TH1D* [4*nROBoards];
  fHNWordPerTDC = new TH1D* [16*nROBoards];

  for(int iTell = 0; iTell<nROBoards; iTell++){
    fHNWordPerTell[iTell] = new TH1D(Form("NWordPerEventonTell%d",iTell+1), Form("Number of Word on Tell %d per Event",iTell+1), 160,-2.5,797.5);
    fHNWordPerTell[iTell]->GetXaxis()->SetTitle(Form("Word on Tell %d (MW/s)",iTell + 1));
  }
  for(int iTDCB = 0; iTDCB<4*nROBoards; iTDCB++){
    fHNWordPerTDCB[iTDCB] = new TH1D(Form("NWordPerEventonTDCB%d",iTDCB+1), Form("Number of Word on TDCB %d per Event",iTDCB+1), 201,-1.,401.);
    fHNWordPerTDCB[iTDCB]->GetXaxis()->SetTitle(Form("Word on TDCB %d (MW/s)",iTDCB + 1));
  }
  for(int iTDC = 0; iTDC<16*nROBoards; iTDC++){
    fHNWordPerTDC[iTDC] = new TH1D(Form("NWordPerEventonTDC%d",iTDC+1), Form("Number of Word on TDC %d per Event",iTDC+1), 80,-.5,79.5);
    fHNWordPerTDC[iTDC]->GetXaxis()->SetTitle(Form("Word on TDC %d (MW/s)",iTDC + 1));
  }

  fHistoFile->cd("/");
}

void CHANTIReconstruction::SaveHistograms()
{
  const Int_t NTrigs = 2;
  fHistoFile->cd("CHANTIMonitor");
  fHXHit->Write();
  fHYHit->Write();
  fHDispPlotX->Write();
  fHDispPlotY->Write();
  fHDispPlotXMU->Write();
  fHDispPlotYMU->Write();
  fHNHit->Write();
  fHNHitAllRing->Write();
  fHContOfModeX->Write();
  fHContOfModeX1->Write();
  fHContOfModeX2->Write();
  fHContOfModeX3->Write();
  fHContOfModeY->Write();
  fHContOfModeY1->Write();
  fHContOfModeY2->Write();
  fHContOfModeY3->Write();

  for(int iRing = 0; iRing < fNRings; iRing++){
    fHNHitsForRing[iRing]->Write();
    fHToTRing0[iRing]->Write();
    fHToTRing1[iRing]->Write();
    fHToTRing2[iRing]->Write();
    for(UInt_t iTrig=0;iTrig<NTrigs;iTrig++){
      fHPositionLow[iRing][iTrig]->Write();
      fHPositionHigh[iRing][iTrig]->Write();
    }
    fHCorrTimeDispPlot[iRing]->Write();
    fHCorrTimeDispPlotMU[iRing]->Write();
    fHDispPlot[iRing]->Write();
    fHDispPlotMU[iRing]->Write();
    fHBarID[iRing]->Write();
    fHCorrTimeShiftMU[iRing]->Write();
    fHNHitsForRingVSBurst[iRing]->Write();
    fHBarEffiNum[iRing]->Write();
    fHBarEffiDen[iRing]->Write();

    fHTimeVsDistanceMU[iRing]->Write();

  }
  for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
    //
    fHHitTimeIn[iPlane]->Write();
    fHHitTimeOut[iPlane]->Write();
    fHHitTimeInMU[iPlane]->Write();
    fHHitTimeInAll[iPlane]->Write();
    fHHitTimeOutMU[iPlane]->Write();
    fHHitTimeOutAll[iPlane]->Write();
    //
    if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
      fHEffiPlotXDen[iPlane]->Write();
      fHEffiPlotYDen[iPlane]->Write();
      fHEffiPlotXNum[iPlane]->Write();
      fHEffiPlotYNum[iPlane]->Write();
      fHEffiPlotHighXNum[iPlane]->Write();
      fHEffiPlotHighYNum[iPlane]->Write();
      fHEffiPlotXYDen[iPlane]->Write();
      fHEffiPlotXYNum[iPlane]->Write();
    }
    //
    fHToTPlane0[iPlane]->Write();
    fHToTPlane1[iPlane]->Write();
    fHToTPlane2[iPlane]->Write();
    fHNHitsForPlane[iPlane]->Write();
    fHResoTimePlane[iPlane]->Write();
    fHResoTimePlaneMU[iPlane]->Write();
    fHXYClusterPerPlane[iPlane]->Write();
    fHNHitsForPlaneVSBurst[iPlane]->Write();


    for(UInt_t iTrig=0;iTrig<NTrigs;iTrig++){
      fHViewX1THRL[iPlane][iTrig]->Write();
      fHViewX2THRL[iPlane][iTrig]->Write();
      fHViewY1THRL[iPlane][iTrig]->Write();
      fHViewY2THRL[iPlane][iTrig]->Write();
      fHViewX1THRH[iPlane][iTrig]->Write();
      fHViewX2THRH[iPlane][iTrig]->Write();
      fHViewY1THRH[iPlane][iTrig]->Write();
      fHViewY2THRH[iPlane][iTrig]->Write();
    }
  }
  fHCorrTimeVSZ->Write();
  fHCorrTimeVSZMU->Write();
  fHToT0->Write();
  fHToT1->Write();
  fHToT2->Write();
  fHResoTime->Write();
  fHResoTimeMU->Write();
  fHTimeVSSlot->Write();
  fHSlewingVSToTLow->Write();
  fHSlewingVSToTHigh->Write();
  fHSlewingVSToTLowFit->Write();
  fHSlewingVSToTHighFit->Write();
  fHNRing->Write();
  fHNPlane->Write();
  fHNRingVSNHit->Write();
  fHNPlaneVSNHit->Write();
  fHModeX->Write();
  fHModeY->Write();
  fHModeYvsModeXSingleMuon->Write();
  fHXYCluster->Write();

  fHRecoHitTimeWrtReference->Write();
  fHRecoHitTimeWrtReferenceVsBurst->Write();
  fHRecoHitTimeWrtReferenceVsWidth->Write();
  fHRecoHitTimeWrtReferenceNoT0->Write();
  fHRecoHitTimeWrtReferenceVsBurstNoT0->Write();
  fHRecoHitTimeWrtReferenceVsWidthNoT0->Write();
  fHCandidateTimeWrtReference->Write();
  fHCandidateTimeWrtReferenceVsBurst->Write();
  fHNTriggersPerBurst->Write();

  Int_t nROBoards = fRawDecoder->GetDecoder()->GetNROBoards();
  for(int iTell = 0; iTell<nROBoards; iTell++) fHNWordPerTell[iTell]->Write();
  for(int iTDCB = 0; iTDCB<4*nROBoards; iTDCB++)  fHNWordPerTDCB[iTDCB]->Write();
  for(int iTDC = 0; iTDC<16*nROBoards;iTDC++)fHNWordPerTDC[iTDC]->Write();

  if (fChannelHistograms) {
    for (Int_t iCh=0; iCh<fNChannels; iCh++) fChannels[iCh]->Write(fHistoFile);
  }

  fHistoFile->cd("/");
}

void CHANTIReconstruction::DeleteHistograms() {

  const Int_t NTrigs = 2;

  if(fHXHit)           delete fHXHit;
  if(fHYHit)           delete fHYHit;
  if(fHXHitEvent1)     delete fHXHitEvent1;
  if(fHXHitEvent2)     delete fHXHitEvent2;
  if(fHYHitEvent1)     delete fHYHitEvent1;
  if(fHYHitEvent2)     delete fHYHitEvent2;
  if(fHDispPlotX)      delete fHDispPlotX;
  if(fHDispPlotY)      delete fHDispPlotY;
  if(fHDispPlotXMU)    delete fHDispPlotXMU;
  if(fHDispPlotYMU)    delete fHDispPlotYMU;
  if(fHNHit)           delete fHNHit;
  if(fHNHitAllRing)    delete fHNHitAllRing;
  if(fHNRing)          delete fHNRing;
  if(fHNPlane)         delete fHNPlane;
  if(fHNRingVSNHit)    delete fHNRingVSNHit;
  if(fHNPlaneVSNHit)   delete fHNPlaneVSNHit;
  if(fHCorrTimeVSZ)    delete fHCorrTimeVSZ;
  if(fHCorrTimeVSZMU)  delete fHCorrTimeVSZMU;
  if(fHContOfModeX)    delete fHContOfModeX;
  if(fHContOfModeX1)   delete fHContOfModeX1;
  if(fHContOfModeX2)   delete fHContOfModeX2;
  if(fHContOfModeX3)   delete fHContOfModeX3;
  if(fHContOfModeY)    delete fHContOfModeY;
  if(fHContOfModeY1)   delete fHContOfModeY1;
  if(fHContOfModeY2)   delete fHContOfModeY2;
  if(fHContOfModeY3)   delete fHContOfModeY3;
  if(fHXYCluster)      delete fHXYCluster;
  if(fHTimeVSSlot)     delete fHTimeVSSlot;

  if(fHRecoHitTimeWrtReference)            delete fHRecoHitTimeWrtReference;
  if(fHRecoHitTimeWrtReferenceVsBurst)     delete fHRecoHitTimeWrtReferenceVsBurst;
  if(fHRecoHitTimeWrtReferenceVsWidth)     delete fHRecoHitTimeWrtReferenceVsWidth;
  if(fHRecoHitTimeWrtReferenceNoT0)        delete fHRecoHitTimeWrtReferenceNoT0;
  if(fHRecoHitTimeWrtReferenceVsBurstNoT0) delete fHRecoHitTimeWrtReferenceVsBurstNoT0;
  if(fHRecoHitTimeWrtReferenceVsWidthNoT0) delete fHRecoHitTimeWrtReferenceVsWidthNoT0;
  if(fHCandidateTimeWrtReference)          delete fHCandidateTimeWrtReference;
  if(fHCandidateTimeWrtReferenceVsBurst)   delete fHCandidateTimeWrtReferenceVsBurst;
  if(fHNTriggersPerBurst)                  delete fHNTriggersPerBurst;

  for(int iRing = 0; iRing < fNRings; iRing++){

    if(fHBarEffiNum && fHBarEffiNum[iRing])                 delete fHBarEffiNum[iRing];
    if(fHBarEffiDen && fHBarEffiDen[iRing])                 delete fHBarEffiDen[iRing];
    if(fHNHitsForRing && fHNHitsForRing[iRing])             delete fHNHitsForRing[iRing];
    if(fHToTRing0 && fHToTRing0[iRing])                     delete fHToTRing0[iRing];
    if(fHToTRing1 && fHToTRing1[iRing])                     delete fHToTRing1[iRing];
    if(fHToTRing2 && fHToTRing2[iRing])                     delete fHToTRing2[iRing];
    if(fHBarID && fHBarID[iRing])                           delete fHBarID[iRing];
    if(fHCorrTimeShiftMU && fHCorrTimeShiftMU[iRing])       delete fHCorrTimeShiftMU[iRing];
    if(fHCorrTime && fHCorrTime[iRing])                     delete fHCorrTime[iRing];
    if(fHCorrTimeDispPlot && fHCorrTimeDispPlot[iRing])     delete fHCorrTimeDispPlot[iRing];
    if(fHCorrTimeDispPlotMU && fHCorrTimeDispPlotMU[iRing]) delete fHCorrTimeDispPlotMU[iRing];
    for(UInt_t iTrig=0;iTrig<NTrigs;iTrig++){
      if(fHPositionLow && fHPositionLow[iRing])             delete fHPositionLow[iRing][iTrig];
      if(fHPositionHigh && fHPositionHigh[iRing])           delete fHPositionHigh[iRing][iTrig];
    }
    if(fHPositionLow && fHPositionLow[iRing])               delete [] fHPositionLow[iRing];
    if(fHPositionHigh && fHPositionHigh[iRing])             delete [] fHPositionHigh[iRing];
    if(fHPosition1 && fHPosition1[iRing])                   delete fHPosition1[iRing];
    if(fHPosition2 && fHPosition2[iRing])                   delete fHPosition2[iRing];

    if(fHTimeVsDistanceMU && fHTimeVsDistanceMU[iRing])     delete fHTimeVsDistanceMU[iRing];

    if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){
      if(fHEffi1Up && fHEffi1Up[iRing])                     delete fHEffi1Up[iRing];
      if(fHEffi2Up && fHEffi2Up[iRing])                     delete fHEffi2Up[iRing];
      if(fHEffi1Down && fHEffi1Down[iRing])                 delete fHEffi1Down[iRing];
      if(fHEffi2Down && fHEffi2Down[iRing])                 delete fHEffi2Down[iRing];
      if(fHEffiPosition1Up && fHEffiPosition1Up[iRing])     delete fHEffiPosition1Up[iRing];
      if(fHEffiPosition2Up && fHEffiPosition2Up[iRing])     delete fHEffiPosition2Up[iRing];
      if(fHEffiPosition1Down && fHEffiPosition1Down[iRing]) delete fHEffiPosition1Down[iRing];
      if(fHEffiPosition2Down && fHEffiPosition2Down[iRing]) delete fHEffiPosition2Down[iRing];
      if(fHEffiHigh1Up && fHEffiHigh1Up[iRing])                     delete fHEffiHigh1Up[iRing];
      if(fHEffiHigh2Up && fHEffiHigh2Up[iRing])                     delete fHEffiHigh2Up[iRing];
      if(fHEffiHigh1Down && fHEffiHigh1Down[iRing])                 delete fHEffiHigh1Down[iRing];
      if(fHEffiHigh2Down && fHEffiHigh2Down[iRing])                 delete fHEffiHigh2Down[iRing];
      if(fHEffiPositionHigh1Up && fHEffiPositionHigh1Up[iRing])     delete fHEffiPositionHigh1Up[iRing];
      if(fHEffiPositionHigh2Up && fHEffiPositionHigh2Up[iRing])     delete fHEffiPositionHigh2Up[iRing];
      if(fHEffiPositionHigh1Down && fHEffiPositionHigh1Down[iRing]) delete fHEffiPositionHigh1Down[iRing];
      if(fHEffiPositionHigh2Down && fHEffiPositionHigh2Down[iRing]) delete fHEffiPositionHigh2Down[iRing];
    }

    if(fHDispPlot && fHDispPlot[iRing])                       delete fHDispPlot[iRing];
    if(fHDispPlotMU && fHDispPlotMU[iRing])                   delete fHDispPlotMU[iRing];
    if(fHNHitsForRingVSBurst && fHNHitsForRingVSBurst[iRing]) delete fHNHitsForRingVSBurst[iRing];

  }

  for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
    //
    if(fHHitTimeIn && fHHitTimeIn[iPlane])            delete fHHitTimeIn[iPlane];
    if(fHHitTimeOut && fHHitTimeOut[iPlane])          delete fHHitTimeOut[iPlane];
    if(fHHitTimeInMU && fHHitTimeInMU[iPlane])        delete fHHitTimeInMU[iPlane];
    if(fHHitTimeOutMU && fHHitTimeOutMU[iPlane])      delete fHHitTimeOutMU[iPlane];
    if(fHHitTimeInAll && fHHitTimeInAll[iPlane])      delete fHHitTimeInAll[iPlane];
    if(fHHitTimeOutAll && fHHitTimeOutAll[iPlane])    delete fHHitTimeOutAll[iPlane];
    //
    if(fEffPlaneIndex>=0 && fEffPlaneIndex<=5){
      if(fHEffiPlotXDen && fHEffiPlotXDen[iPlane])    delete fHEffiPlotXDen[iPlane];
      if(fHEffiPlotYDen && fHEffiPlotYDen[iPlane])    delete fHEffiPlotYDen[iPlane];
      if(fHEffiPlotXNum && fHEffiPlotXNum[iPlane])    delete fHEffiPlotXNum[iPlane];
      if(fHEffiPlotYNum && fHEffiPlotYNum[iPlane])    delete fHEffiPlotYNum[iPlane];
      if(fHEffiPlotHighXNum && fHEffiPlotHighXNum[iPlane])    delete fHEffiPlotHighXNum[iPlane];
      if(fHEffiPlotHighYNum && fHEffiPlotHighYNum[iPlane])    delete fHEffiPlotHighYNum[iPlane];
      if(fHEffiPlotXYDen && fHEffiPlotXYDen[iPlane])  delete fHEffiPlotXYDen[iPlane];
      if(fHEffiPlotXYNum && fHEffiPlotXYNum[iPlane])  delete fHEffiPlotXYNum[iPlane];
    }
    //

    if(fHNHitsForPlane && fHNHitsForPlane[iPlane]) delete fHNHitsForPlane[iPlane];
    if(fHToTPlane0 && fHToTPlane0[iPlane]) delete fHToTPlane0[iPlane];
    if(fHToTPlane1 && fHToTPlane1[iPlane]) delete fHToTPlane1[iPlane];
    if(fHToTPlane2 && fHToTPlane2[iPlane]) delete fHToTPlane2[iPlane];

    if(fHResoTimePlane && fHResoTimePlane[iPlane])               delete fHResoTimePlane[iPlane];
    if(fHResoTimePlaneMU && fHResoTimePlaneMU[iPlane])           delete fHResoTimePlaneMU[iPlane];
    if(fHXYClusterPerPlane && fHXYClusterPerPlane[iPlane])       delete fHXYClusterPerPlane[iPlane];
    if(fHNHitsForPlaneVSBurst && fHNHitsForPlaneVSBurst[iPlane]) delete fHNHitsForPlaneVSBurst[iPlane];

    for(UInt_t iTrig=0;iTrig<NTrigs;iTrig++){
      if(fHViewX1THRL && fHViewX1THRL[iPlane]) delete fHViewX1THRL[iPlane][iTrig];
      if(fHViewX2THRL && fHViewX2THRL[iPlane]) delete fHViewX2THRL[iPlane][iTrig];
      if(fHViewY1THRL && fHViewY1THRL[iPlane]) delete fHViewY1THRL[iPlane][iTrig];
      if(fHViewY2THRL && fHViewY2THRL[iPlane]) delete fHViewY2THRL[iPlane][iTrig];
      if(fHViewX1THRH && fHViewX1THRH[iPlane]) delete fHViewX1THRH[iPlane][iTrig];
      if(fHViewX2THRH && fHViewX2THRH[iPlane]) delete fHViewX2THRH[iPlane][iTrig];
      if(fHViewY1THRH && fHViewY1THRH[iPlane]) delete fHViewY1THRH[iPlane][iTrig];
      if(fHViewY2THRH && fHViewY2THRH[iPlane]) delete fHViewY2THRH[iPlane][iTrig];
    }
    if(fHViewX1THRL && fHViewX1THRL[iPlane]) delete [] fHViewX1THRL[iPlane];
    if(fHViewX2THRL && fHViewX2THRL[iPlane]) delete [] fHViewX2THRL[iPlane];
    if(fHViewY1THRL && fHViewY1THRL[iPlane]) delete [] fHViewY1THRL[iPlane];
    if(fHViewY2THRL && fHViewY2THRL[iPlane]) delete [] fHViewY2THRL[iPlane];
    if(fHViewX1THRH && fHViewX1THRH[iPlane]) delete [] fHViewX1THRH[iPlane];
    if(fHViewX2THRH && fHViewX2THRH[iPlane]) delete [] fHViewX2THRH[iPlane];
    if(fHViewY1THRH && fHViewY1THRH[iPlane]) delete [] fHViewY1THRH[iPlane];
    if(fHViewY2THRH && fHViewY2THRH[iPlane]) delete [] fHViewY2THRH[iPlane];

  }

  Int_t nROBoards = 0;
  if(fRawDecoder && fRawDecoder->GetDecoder()) nROBoards = fRawDecoder->GetDecoder()->GetNROBoards();
  for(int iTell = 0; iTell<nROBoards; iTell++)
  {
    if(fHNWordPerTell && fHNWordPerTell[iTell]) delete fHNWordPerTell[iTell];
  }
  for(int iTDCB = 0; iTDCB<4*nROBoards; iTDCB++)
  {
    if(fHNWordPerTDCB && fHNWordPerTDCB[iTDCB]) delete fHNWordPerTDCB[iTDCB];
  }
  for(int iTDC = 0; iTDC<16*nROBoards;iTDC++)
  {
    if(fHNWordPerTDC && fHNWordPerTDC[iTDC])   delete fHNWordPerTDC[iTDC];
  }

  if(fHBarEffiNum         )     delete [] fHBarEffiNum;
  if(fHBarEffiDen         )     delete [] fHBarEffiDen;
  if(fHNHitsForRing       )     delete [] fHNHitsForRing;
  if(fHToTRing0           )     delete [] fHToTRing0;
  if(fHToTRing1           )     delete [] fHToTRing1;
  if(fHToTRing2           )     delete [] fHToTRing2;
  if(fHBarID              )     delete [] fHBarID;
  if(fHCorrTimeShiftMU    )     delete [] fHCorrTimeShiftMU;
  if(fHCorrTime           )     delete [] fHCorrTime;
  if(fHCorrTimeDispPlot   )     delete [] fHCorrTimeDispPlot;
  if(fHCorrTimeDispPlotMU )     delete [] fHCorrTimeDispPlotMU;
  if(fHPositionLow        )     delete [] fHPositionLow;
  if(fHPositionHigh       )     delete [] fHPositionHigh;
  if(fHPosition1          )     delete [] fHPosition1;
  if(fHPosition2          )     delete [] fHPosition2;

  if(fHTimeVsDistanceMU   )     delete [] fHTimeVsDistanceMU;

  if(fEffPlaneIndex>=0 && fEffPlaneIndex <=5){

    if(fHEffi1Up        )        delete [] fHEffi1Up;
    if(fHEffi2Up        )        delete [] fHEffi2Up;
    if(fHEffi1Down      )        delete [] fHEffi1Down;
    if(fHEffi2Down      )        delete [] fHEffi2Down;
    if(fHEffiPosition1Up)        delete [] fHEffiPosition1Up;
    if(fHEffiPosition2Up)        delete [] fHEffiPosition2Up;
    if(fHEffiPosition1Down)      delete [] fHEffiPosition1Down;
    if(fHEffiPosition2Down)      delete [] fHEffiPosition2Down;
    if(fHEffiHigh1Up        )        delete [] fHEffiHigh1Up;
    if(fHEffiHigh2Up        )        delete [] fHEffiHigh2Up;
    if(fHEffiHigh1Down      )        delete [] fHEffiHigh1Down;
    if(fHEffiHigh2Down      )        delete [] fHEffiHigh2Down;
    if(fHEffiPositionHigh1Up)        delete [] fHEffiPositionHigh1Up;
    if(fHEffiPositionHigh2Up)        delete [] fHEffiPositionHigh2Up;
    if(fHEffiPositionHigh1Down)      delete [] fHEffiPositionHigh1Down;
    if(fHEffiPositionHigh2Down)      delete [] fHEffiPositionHigh2Down;
  }

  if(fHDispPlot     )             delete [] fHDispPlot;
  if(fHDispPlotMU   )             delete [] fHDispPlotMU;
  if(fHHitTimeIn    )             delete [] fHHitTimeIn;
  if(fHHitTimeOut   )             delete [] fHHitTimeOut;
  if(fHHitTimeInMU  )             delete [] fHHitTimeInMU;
  if(fHHitTimeOutMU )             delete [] fHHitTimeOutMU;
  if(fHHitTimeInAll )             delete [] fHHitTimeInAll;
  if(fHHitTimeOutAll)             delete [] fHHitTimeOutAll;
  //
  if(fEffPlaneIndex>=0 && fEffPlaneIndex<=5){
    if(fHEffiPlotXDen )          delete [] fHEffiPlotXDen;
    if(fHEffiPlotYDen )          delete [] fHEffiPlotYDen;
    if(fHEffiPlotXNum )          delete [] fHEffiPlotXNum;
    if(fHEffiPlotYNum )          delete [] fHEffiPlotYNum;
    if(fHEffiPlotHighXNum )      delete [] fHEffiPlotHighXNum;
    if(fHEffiPlotHighYNum )      delete [] fHEffiPlotHighYNum;
    if(fHEffiPlotXYDen)          delete [] fHEffiPlotXYDen;
    if(fHEffiPlotXYNum)          delete [] fHEffiPlotXYNum;
  }
  //
  if(fHNHitsForPlane    )          delete [] fHNHitsForPlane;
  if(fHToTPlane0        )          delete [] fHToTPlane0;
  if(fHToTPlane1        )          delete [] fHToTPlane1;
  if(fHToTPlane2        )          delete [] fHToTPlane2;
  if(fHViewX1THRL         )        delete [] fHViewX1THRL;
  if(fHViewX2THRL         )        delete [] fHViewX2THRL;
  if(fHViewY1THRL         )        delete [] fHViewY1THRL;
  if(fHViewY2THRL         )        delete [] fHViewY2THRL;
  if(fHViewX1THRH         )        delete [] fHViewX1THRH;
  if(fHViewX2THRH         )        delete [] fHViewX2THRH;
  if(fHViewY1THRH         )        delete [] fHViewY1THRH;
  if(fHViewY2THRH         )        delete [] fHViewY2THRH;
  if(fHResoTimePlane        )      delete [] fHResoTimePlane;
  if(fHResoTimePlaneMU      )      delete [] fHResoTimePlaneMU;
  if(fHXYClusterPerPlane    )      delete [] fHXYClusterPerPlane;
  if(fHNHitsForRingVSBurst    )    delete [] fHNHitsForRingVSBurst;
  if(fHNHitsForPlaneVSBurst )      delete [] fHNHitsForPlaneVSBurst;
  if(fHNWordPerTell)               delete [] fHNWordPerTell;
  if(fHNWordPerTDCB)               delete [] fHNWordPerTDCB;
  if(fHNWordPerTDC )               delete [] fHNWordPerTDC;
}


void CHANTIReconstruction::ResetHistograms() {

  fHXHit = 0;
  fHYHit = 0;
  fHXHitEvent1 = 0;
  fHXHitEvent2 = 0;
  fHYHitEvent1 = 0;
  fHYHitEvent2 = 0;
  fHDispPlotX = 0;
  fHDispPlotY = 0;
  fHDispPlotXMU = 0;
  fHDispPlotYMU = 0;
  fHNHit = 0;
  fHNHitAllRing = 0;
  fHNRing = 0;
  fHNPlane = 0;
  fHNRingVSNHit = 0;
  fHNPlaneVSNHit = 0;
  fHCorrTimeVSZ = 0;
  fHCorrTimeVSZMU = 0;
  fHContOfModeX = 0;
  fHContOfModeX1 = 0;
  fHContOfModeX2 = 0;
  fHContOfModeX3 = 0;
  fHContOfModeY = 0;
  fHContOfModeY1 = 0;
  fHContOfModeY2 = 0;
  fHContOfModeY3 = 0;
  fHXYCluster = 0;
  fHTimeVSSlot = 0;
  fHRecoHitTimeWrtReference = 0;
  fHRecoHitTimeWrtReferenceVsBurst = 0;
  fHRecoHitTimeWrtReferenceVsWidth = 0;
  fHRecoHitTimeWrtReferenceNoT0 = 0;
  fHRecoHitTimeWrtReferenceVsBurstNoT0 = 0;
  fHRecoHitTimeWrtReferenceVsWidthNoT0 = 0;
  fHCandidateTimeWrtReference = 0;
  fHCandidateTimeWrtReferenceVsBurst = 0;
  fHNTriggersPerBurst = 0;
  fHNHitsForRing = 0;
  fHBarEffiNum = 0;
  fHBarEffiDen = 0;
  fHToTRing0 = 0;
  fHToTRing1 = 0;
  fHToTRing2 = 0;
  fHBarID = 0;
  fHCorrTimeShiftMU = 0;
  fHCorrTime = 0;
  fHCorrTimeDispPlot = 0;
  fHCorrTimeDispPlotMU = 0;
  fHPositionLow = 0;
  fHPositionHigh = 0;
  fHPosition1 = 0;
  fHPosition2 = 0;

  fHTimeVsDistanceMU= 0;

  fHEffi1Up = 0;
  fHEffi2Up = 0;
  fHEffi1Down = 0;
  fHEffi2Down = 0;
  fHEffiPosition1Up = 0;
  fHEffiPosition2Up = 0;
  fHEffiPosition1Down = 0;
  fHEffiPosition2Down = 0;
  fHEffiHigh1Up = 0;
  fHEffiHigh2Up = 0;
  fHEffiHigh1Down = 0;
  fHEffiHigh2Down = 0;
  fHEffiPositionHigh1Up = 0;
  fHEffiPositionHigh2Up = 0;
  fHEffiPositionHigh1Down = 0;
  fHEffiPositionHigh2Down = 0;
  fHDispPlot = 0;
  fHDispPlotMU = 0;
  fHNHitsForRingVSBurst = 0;
  fHHitTimeIn = 0;
  fHHitTimeOut = 0;
  fHHitTimeInMU = 0;
  fHHitTimeOutMU = 0;
  fHHitTimeInAll = 0;
  fHHitTimeOutAll = 0;
  fHEffiPlotXDen = 0;
  fHEffiPlotYDen = 0;
  fHEffiPlotXNum = 0;
  fHEffiPlotYNum = 0;
  fHEffiPlotHighXNum = 0;
  fHEffiPlotHighYNum = 0;
  fHEffiPlotXYDen = 0;
  fHEffiPlotXYNum = 0;
  fHNHitsForPlane = 0;
  fHToTPlane0 = 0;
  fHToTPlane1 = 0;
  fHToTPlane2 = 0;
  fHViewX1THRL = 0;
  fHViewX2THRL = 0;
  fHViewY1THRL = 0;
  fHViewY2THRL = 0;
  fHViewX1THRH = 0;
  fHViewX2THRH = 0;
  fHViewY1THRH = 0;
  fHViewY2THRH = 0;
  fHResoTimePlane = 0;
  fHResoTimePlaneMU = 0;
  fHXYClusterPerPlane = 0;
  fHNHitsForPlaneVSBurst = 0;
  fHNWordPerTell = 0;
  fHNWordPerTDCB = 0;
  fHNWordPerTDC = 0;
}
