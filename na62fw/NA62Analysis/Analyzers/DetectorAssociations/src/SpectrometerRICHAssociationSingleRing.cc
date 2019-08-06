// ---------------------------------------------------------------
//
// History:
// 
// Copied from the code SpectrometerRICHAssociation created
// by Evgueni Goudzovski
//
// Modified by F. Brizioli (june 2019)
// ---------------------------------------------------------------

/// \class SpectrometerRICHAssociationSingleRing
/// \Brief
/// Geometrical association of "single ring" RICH candidates and hits to Spectrometer candidates
/// \EndBrief
/// \Detailed
/// The output is a SpectrometerRICHAssociation structure.
/// An example of use is available in the SpectrometerRICHEventMonitor class.
/// Below is a basic example if use.
/// \code
///  std::vector<SpectrometerRICHAssociationOutputSingleRing> SpecRICHSingleRing =
///    *(std::vector<SpectrometerRICHAssociationOutputSingleRing>*)GetOutput("SpectrometerRICHAssociationSingleRing.Output");
///  for (UInt_t itrack=0; itrack<SpecRICH.size(); itrack++) {
///    SpectrometerRICHAssociationOutputSingleRing sr = SpecRICHSingleRing[itrack];
///    // ...
///  }
/// \endcode
///
/// Also negative values for RICH mass are defined, in order to preserve the sign of mass2:
/// mass = sqrt(fabs(mass2)); if (mass2 < 0) mass = -1.*mass;
/// or, viceversa:
/// mass2 = mass*mass; if (mass < 0) mass2 = -1.*mass2;
/// 
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "SpectrometerRICHAssociationSingleRing.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "NA62ConditionsService.hh"
#include "RICHParameters.hh"
#include "RICHSingleRingTrkSeededFit.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerRICHAssociationSingleRing::SpectrometerRICHAssociationSingleRing(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerRICHAssociationSingleRing"), fRefIndex(0.), fPoisson(nullptr),
  fSingleRingTime(0.), fSingleRingDist(0.), fSingleRingDTime(0.), fSingleRingRadius(0.),
  fSingleRingCenterX(9999999.), fSingleRingCenterY(9999999.), 
  fSingleRingChi2(0.), fSingleRingNHits(0), fElectronRingRadius(0.){

  fContainer.clear();
  
  if (!GetIsTree()) return;
  
  RequestTree("RICH",         new TRecoRICHEvent,         "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  fPoisson = new TF1("PoissonFunction", "TMath::Poisson(x, [0])", 0, 100);
  fRICHMirrorZPos = GeometricAcceptance::GetInstance()->GetZRICHMirror();
  fFocalLength    = 17020.0;

  fSingleRing = RICHSingleRingTrkSeededFit::GetInstance();
}

SpectrometerRICHAssociationSingleRing::~SpectrometerRICHAssociationSingleRing() {
  if (fPoisson) delete fPoisson;
}

void SpectrometerRICHAssociationSingleRing::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void SpectrometerRICHAssociationSingleRing::InitHist() {

  NA62ConditionsService::GetInstance()->SetCurrentRunID(GetRunID());
  for (Int_t i=0; i<20; i++) fMirrorNumber[i] = RICHParameters::GetInstance()->GetMirrorNumber(i);
  for (Int_t i=0; i<25; i++) fMirrorAlignment[i][0] = fMirrorAlignment[i][1] = 0.0;

  // Initialize mirror positions
  for (Int_t i=0; i<20; i++) { // sequential numbering used in this class
    fMirrorPosition[i][0] = RICHParameters::GetInstance()->GetMirrorCentreX(fMirrorNumber[i]);
    fMirrorPosition[i][1] = RICHParameters::GetInstance()->GetMirrorCentreY(fMirrorNumber[i]);
  }
  for (Int_t i=0; i<25; i++) { // "standard" non-sequential numbering
    fMirrorSide[i] = -999;
    if (RICHParameters::GetInstance()->MirrorExists(i)) {
      fMirrorSide[i] = RICHParameters::GetInstance()->JuraMirror(i) ? 0 : 1;
    }
  }

  // Semihex mirrors: non-standard centres (defined as averages of other mirrors).
  // The centres are evaluated for full mirrors, not half-mirrors, so x is close to 0.

  fMirrorPosition[18][0]=fMirrorPosition[1][0]; // x of Mirror 17
  fMirrorPosition[18][1]=(fMirrorPosition[7][1]+fMirrorPosition[8][1])/2.;
  fMirrorPosition[19][0]=fMirrorPosition[16][0]; // x of Mirror 22
  fMirrorPosition[19][1]=(fMirrorPosition[9][1]+fMirrorPosition[10][1])/2.;

  //////////////////////////////////

  if (!GetIsTree()) return;
 
  BookHisto(new TH1I("hNHits","hNHits", 100, 0,100));
  BookHisto(new TH1I("hNTimeCand","hNTimeCand", 10, 0,10));
  BookHisto(new TH1F("hSingleRing_Radius","hSingleRing_Radius", 4000, 0.,400.));
  BookHisto(new TH1F("hSingleRing_nHits","hSingleRing_nHits", 50, 0.,50.));
  BookHisto(new TH2F("hSingleRing_nHits_vs_p","hSingleRing_nHits_vs_p",180, 0,90,50,0,50));
  BookHisto(new TH2F("hSingleRing_MissMass2_vs_EoP","hSingleRing_MissMass2_vs_EoP",120,0,1.2,30,-0.15,0.15));
  BookHisto(new TH1F("hSingleRing_RICHMass","hSingleRing_RICHMass", 200, 0.,0.20));
  BookHisto(new TH1F("hSingleRing_RICHMass2","hSingleRing_RICHMass2",  100, -0.05,0.05));
  BookHisto(new TH1F("hSingleRing_Dist","hSingleRing_Dist", 100, 0.,50.));
  BookHisto(new TH1F("hSingleRing_DTime","hSingleRing_DTime", 100, -50.,50.));
  BookHisto(new TH2F("hSingleRing_DTime_vs_Dist","hSingleRing_DTime_vs_Dist", 100, 0., 50., 100, -50., 50.)); 

}

void SpectrometerRICHAssociationSingleRing::StartOfRunUser() {
  if (!GetIsTree()) return;
  // Int_t RunNumber = GetRunID();
  // Read mirror angular alignment constants from via the database interface.
  // These constants [mm] are added to the expected ring centre coordinates.
  /*   not used by now
  for (Int_t i=0; i<25; i++) {
    for (Int_t j=0; j<2; j++) {
      fMirrorAlignment[i][j] = RICHParameters::GetInstance()->GetMirrorAlignmentConstant(RunNumber, i, j);
    }
  }
  */
}
void SpectrometerRICHAssociationSingleRing::StartOfBurstUser() {
  if (!GetIsTree()) return;
  ////////////////////////////////////////////////////////////
  // Update the parameters of the electron ring for each burst  
  Int_t  RunNumber = GetRunID();
  time_t BurstTime = GetEventHeader()->GetBurstTime();
  // read parameters from the DB
  fElectronRingRadius = RICHParameters::GetInstance()->GetElectronRingRadius(RunNumber, BurstTime);

  // Apply empirical scale factors for MC (Evgueni, 1/8/19).
  // This is to be tuned more carefully with the RICHElectronRadius analyzer.
  if (GetWithMC()) {
    fElectronRingRadius *= 0.999757; // this leads to 189.6 mm for run 6610
  }

  fRefIndex = 1.0/(cos(atan(fElectronRingRadius/fFocalLength)));
  // Printout with -v2 or higher verbosity
  cout << user() <<
    Form("Run=%d, BurTime=%ld, Radius=%6.2fmm, RefIndex=%5.2fppm",
	 RunNumber, BurstTime, fElectronRingRadius, (fRefIndex-1.0)*1.0e6) << endl;
}

void SpectrometerRICHAssociationSingleRing::EndOfJobUser() {
  SaveAllPlots();
}

void SpectrometerRICHAssociationSingleRing::Process(Int_t) {
  SetOutputState("Output", kOValid);
  fContainer.clear();

  if (!GetIsTree()) return;

  TRecoSpectrometerEvent* STRAWEvent = static_cast<TRecoSpectrometerEvent*>(GetEvent("Spectrometer"));
  TRecoRICHEvent*         RICHEvent  = static_cast<TRecoRICHEvent*>(GetEvent("RICH"));
  Int_t nRichHits = RICHEvent->GetNHits();
  Int_t nGoodRichHits = 0;
  for (Int_t i=0; i<nRichHits; i++) {
    TRecoRICHHit* hit = static_cast<TRecoRICHHit*>(RICHEvent->GetHit(i));
    if (hit->GetOrSuperCellID()==0){
      nGoodRichHits++;
    }
  }

  std::vector<SpectrometerCHODAssociationOutput> SpecCHOD =
   *(std::vector<SpectrometerCHODAssociationOutput>*)GetOutput("SpectrometerCHODAssociation.Output");

  FillHisto("hNTimeCand", RICHEvent->GetNPMTimeCandidates());
  FillHisto("hNHits", nGoodRichHits);

  for (Int_t iTrack=0; iTrack<STRAWEvent->GetNCandidates(); iTrack++) {
    TRecoSpectrometerCandidate* Scand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(iTrack));
    // Double_t xc   = fFocalLength * Scand->GetSlopeXAfterMagnet();
    // Double_t yc   = fFocalLength * Scand->GetSlopeYAfterMagnet();
    Double_t Time = Scand->GetTime();
    SpectrometerRICHAssociationOutputSingleRing Asso(iTrack, Time, Scand->GetMomentum());

    ///////////////////////////////////////////////////////////
    // Match track to rings produced by the RICHSingleRingTrkSeededFit 
    // Find the closest ring to the track
    Double_t MinXDistanceTrackRing = 9999;
    Double_t MinYDistanceTrackRing = 9999;

    Int_t SingleRingID = -1;
    Double_t TrackTime = -999.;
    if (SpecCHOD[iTrack].GetNAssociationRecords()>0) {
      TrackTime = SpecCHOD[iTrack].GetBestAssociationRecord()->GetCHODCandidateTime();
    }
    else {
      TrackTime = Time;
    }
    SingleRingID = TrackSingleRingMatching(TrackTime,RICHEvent,Scand); 
    Asso.SetRingID(SingleRingID);
    /*
    if (SingleRingID>-1) {
      TRecoRICHCandidate* rc = static_cast<TRecoRICHCandidate*>(RICHEvent->GetPMTimeCandidate(SingleRingID));
      MinXDistanceTrackRing = rc->GetRingCenter().X()-xc;
      MinYDistanceTrackRing = rc->GetRingCenter().Y()-yc;
      fSingleRingDist = sqrt(MinXDistanceTrackRing*MinXDistanceTrackRing + MinYDistanceTrackRing*MinYDistanceTrackRing);
    }
    */
    
    FillHisto("hSingleRing_Dist", fSingleRingDist);
    FillHisto("hSingleRing_DTime", fSingleRingDTime);
    FillHisto("hSingleRing_DTime_vs_Dist", fSingleRingDist, fSingleRingDTime);
    Asso.SetMinDistanceTrackRing (fSingleRingDist);

    
    Asso.SetMinXDistanceTrackRing(MinXDistanceTrackRing);
    Asso.SetMinYDistanceTrackRing(MinYDistanceTrackRing);

    /////////////////////////////////////////////////////////////////////
    // If a ring is found close enough, fill in the relevant data fields.
    // In this case, track to RICH ring association is successful.

    Double_t RICHMass = -999999.9;
    Double_t RICHMass2 = -999999.9;
    Double_t cosTheta = cos(fSingleRingRadius/fFocalLength);

    // Double_t sinTheta = sin(fSingleRingRadius/fFocalLength);
    Double_t n2 = fRefIndex*fRefIndex;
    if(SingleRingID>-1 && fSingleRingDist<15.0) {
      Double_t ptrack = Scand->GetMomentum();
      RICHMass2 = ptrack*ptrack*(n2* cosTheta* cosTheta -1);
      RICHMass = sqrt(fabs(RICHMass2));
      if(RICHMass2<0) RICHMass = -1.0*RICHMass;
      
      // TRecoRICHCandidate* rc = static_cast<TRecoRICHCandidate*>(RICHEvent->GetPMTimeCandidate(SingleRingID));
      //Double_t RRing = fSingleRingRadius;
      /*
	Asso.SetRingRadius  (RRing);
	Asso.SetRingChi2    (fSingleRingChi2);
	Asso.SetRingTime    (rc->GetTime());
	Asso.SetRingPosition(rc->GetRingCenter());
	Asso.SetMass(RICHMass);
	Asso.SetMass2(RICHMass2);
      */
      
      Asso.SetRingRadius  (fSingleRingRadius);
      Asso.SetRingChi2    (fSingleRingChi2);
      Asso.SetRingTime    (fSingleRingTime);
      Asso.SetRingPosition(TVector2(fSingleRingCenterX,fSingleRingCenterY));
      Asso.SetMass(RICHMass);
      Asso.SetMass2(RICHMass2);
      
      FillHisto("hSingleRing_RICHMass",RICHMass);
      FillHisto("hSingleRing_Radius",fSingleRingRadius);
      FillHisto("hSingleRing_nHits",fSingleRingNHits);
      FillHisto("hSingleRing_nHits_vs_p", ptrack,fSingleRingNHits);
      Asso.SetNHits(fSingleRingNHits);
    }
    fContainer.push_back(Asso);
  }
}

Int_t SpectrometerRICHAssociationSingleRing::TrackSingleRingMatching(Double_t TrackTime, TRecoRICHEvent* event, TRecoSpectrometerCandidate *thisTrack) {
  // Int_t RunNumber = GetEventHeader()->GetRunID();
  TVector3 posTrackAtMirror =  TVector3(thisTrack->xAt(fRICHMirrorZPos), thisTrack->yAt(fRICHMirrorZPos),fRICHMirrorZPos);
  Int_t mirrorid = MirrorSurface(posTrackAtMirror.X(),posTrackAtMirror.Y());
  TVector3 deltaVers = (posTrackAtMirror-TVector3(0.,0.,202873.)).Unit();
  TVector3 versorTrack = (thisTrack->GetThreeMomentumAfterMagnet()).Unit();
  TVector3 S = ((versorTrack.Dot(deltaVers))/(deltaVers.Mag2()))*(deltaVers);
  TVector3 d = S-versorTrack;
  TVector3 res = S+d;
  versorTrack = res.Unit();
  TVector3 pmtPos = posTrackAtMirror+(-fFocalLength/versorTrack.Z())*versorTrack;

  if (mirrorid==99) return -1;
  Int_t SRNHits;
  Double_t SRRadius, SRChi2, SRTime, SRCenterX, SRCenterY;
 
  // Associate tracks with a good ring
  Double_t ttime = TrackTime;
  Double_t mindist = 9999999.;
  Double_t mintime = 9999999.;
  Double_t minSRtime = 9999999.;
  Int_t minidring = -1;
  Double_t minchi2 = 9999999.;
  TVector2 mindeltapos(-9999999.,-9999999.);
  // TVector2 minrcenter(-9999999.,-9999999.);
  Int_t minNhits =-1;
  Double_t minradius = 0.;
  Double_t minringchi2 = 99999999.;
  Double_t minrcenterX = 99999999.;
  Double_t minrcenterY = 99999999.;
  for (Int_t jring=0; jring<event->GetNPMTimeCandidates(); jring++) {
    TRecoRICHCandidate *ring = static_cast<TRecoRICHCandidate*>(event->GetPMTimeCandidate(jring));
    Double_t goodFit = fSingleRing->Chi2Fit(event,ring,pmtPos,SRNHits, SRCenterX, SRCenterY, SRRadius, SRChi2, SRTime); 
    if (!goodFit) continue;
    //  TVector2 ringcenter = SRCenter;
    TVector2 deltapos(SRCenterX-pmtPos.X(),SRCenterY-pmtPos.Y());
    Double_t dist = deltapos.Mod();
    //Double_t dtime = ring->GetTime()-ttime;
    Double_t dtime = SRTime-ttime;
    Double_t chi2rich = dtime*dtime/(2*6*6) +dist*dist/(4*1.6*1.6);
    if (chi2rich<minchi2) {
      minchi2 = chi2rich;
      mindist = dist;
      mintime = dtime;
      mindeltapos = deltapos;
      minrcenterX = SRCenterX;
      minrcenterY = SRCenterY;
      minradius = SRRadius;
      minringchi2 = SRChi2;
      minNhits = SRNHits;
      minSRtime = SRTime;
      minidring = jring;
    }  
  }
  if (minidring>-1) {
    fSingleRingTime = minSRtime;
    fSingleRingDist = mindist;
    fSingleRingDTime = mintime;
    fSingleRingRadius = minradius;
    fSingleRingChi2 =  minringchi2;
    fSingleRingNHits = minNhits;
    fSingleRingCenterX = minrcenterX;
    fSingleRingCenterY = minrcenterY;
  }
  else {
    minidring = -1;
  }
  return minidring;
}

Int_t SpectrometerRICHAssociationSingleRing::MirrorSurface(Double_t xin, Double_t yin) { // mirror ID from track information
  Double_t dmin = 1.0e12;
  Int_t mirrorID = 99;
  for (Int_t im=0; im<20; im++) {
    Double_t d =
      pow(fMirrorPosition[im][0]-xin, 2) +
      pow(fMirrorPosition[im][1]-yin, 2);
    if (d<dmin) {
      dmin = d;
      mirrorID = fMirrorNumber[im];
    }
  } // end of loop on mirrors
  return mirrorID;
}
