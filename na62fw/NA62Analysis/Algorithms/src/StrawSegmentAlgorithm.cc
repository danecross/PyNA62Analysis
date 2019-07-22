/// \class StrawSegmentAlgorithm
/// \Brief
/// Spectrometer segment reconstruction
/// \EndBrief
/// \Detailed
/// Spectrometer segment reconstruction
/// \author Michal Koval (michal.koval@cern.ch)
/// \EndDetailed

#include "StrawSegmentAlgorithm.hh"
#include "TSystem.h"
#include "NA62ConditionsService.hh"

#include <memory>
#include <numeric>

using namespace std;

StrawSegmentAlgorithm::StrawSegmentAlgorithm
(BaseAnalysis *ba, Analyzer* ana, const string &name)
  : Algorithm(ba, ana, name),
    fPar(SpectrometerParameters::GetInstance()),
    fGeo(SpectrometerGeometry::GetInstance())
{
  // set default Straw config file path
  TString configName=gSystem->Getenv("NA62RECOSOURCE");
  configName.Append("/config/Spectrometer.conf");
  fConfFileName = configName;
  fIsInitialized = false;
  // run and burst id, input=MC?
  fRunID = -1;
  fBurstID = -1;
  fIsMC = false;
  // debug printout flag
  fDebugMode = false;
  fFlag = 0;

  InitSegmentVariables();
  InitGeometryVariables();
  fStrawCandidateHit = new PnnStrawHit();
  fStrawNoCandidateHit = new PnnStrawHit();
  for (int jc=0; jc<4; jc++)
    for (int jv=0; jv<4; jv++)
      fStrawCluster[jc][jv] = new PnnStrawCluster(jc,jv);

  fXCenter[0] = 101.2;
  fXCenter[1] = 114.4;
  fXCenter[2] = 92.4;
  fXCenter[3] = 52.8;
  fZStation[0] = 183508.;
  fZStation[1] = 194066.;
  fZStation[2] = 204459.;
  fZStation[3] = 218885.;
}

void StrawSegmentAlgorithm::Init()
{
  // parse spectrometer config file
  fPar.ParseConfFile(fConfFileName);
  fPar.SetIsRawData(!fIsMC);
  fPar.SetT0FileName("Spectrometer-T0.dat");
  fPar.SetAllT0s();
  fPar.SetRT(); // Read RT parameters
  fPar.SetXT(); // Read XT parameters
  fPar.SetAlignment(); // Read alignment offsets
  fPar.SetZPositionCorrection();
  fGeo.CreateGeometry();
  fIsInitialized = true;
  // local function from Giuseppe's segments, TODO: remove and use standard Geometry functions
  CreateStrawGeometry();
}

void StrawSegmentAlgorithm::Process(TRecoSpectrometerEvent *event, Int_t trackID)
{
  if (!GetIsTree()) return;
  if (!GetEventHeader()) return; // do not run on NA62MC (i.e. not NA62Reco) output

  if (fRunID != GetRunID() || fBurstID != GetBurstID() || fIsMC != GetWithMC() ) {
    fRunID = GetRunID();
    fBurstID = GetBurstID();
    fIsMC = GetWithMC();
  }
  if (!fIsInitialized) Init();

  Clear();
  fSpectrometerEvent = event;
  AnalyzeHits(trackID);
}

void StrawSegmentAlgorithm::OneViewSegment(int cH){
  int nS = cH==1 ? 0 : 1;
  double zcoor[3] = {fVertex[4],fChamberZPosition[cH-1],fChamberZPosition[cH]};
  double er1 = 2.;
  double er2 = 2.;
  double evt = 1.0;
  double error[3] = {evt,er1,er2};
  for (auto &iH : fChamberHit[cH-1]) {
    double iT = iH.GetTrailingTime()-155.;
    for (auto &jH : fChamberHit[cH]) {
      double jT = jH.GetTrailingTime()-155.;
      double xcoor[3] = {fVertex[2],iH.GetXcoor(),jH.GetXcoor()};
      double ycoor[3] = {fVertex[3],iH.GetYcoor(),jH.GetYcoor()};
      double slopex = -99999.;
      double posx = -99999.;

      // Fit and condition on slopes
      double chi2x = cH==1 ? Chi2LinearFit(xcoor,zcoor,error,&slopex,&posx) : 0.;
      if (cH==1 && fabs(slopex)>0.02) continue;
      double slopey = -99999.;
      double posy = -99999.;
      double chi2y = Chi2LinearFit(ycoor,zcoor,error,&slopey,&posy);
      if (fabs(slopey)>0.02) continue;

      // Distance from the track(s)
      double xpos1 = GetLocalCoordinate(fThisTrack,2,zcoor[1]);
      double ypos1 = GetLocalCoordinate(fThisTrack,3,zcoor[1]);
      double xpos2 = GetLocalCoordinate(fThisTrack,2,zcoor[2]);
      double ypos2 = GetLocalCoordinate(fThisTrack,3,zcoor[2]);
      double dist1 = (xpos1-xcoor[1])*(xpos1-xcoor[1])+(ypos1-ycoor[1])*(ypos1-ycoor[1]); // distance2 hit-track at CH1(3)
      double dist2 = (xpos2-xcoor[2])*(xpos2-xcoor[2])+(ypos2-ycoor[2])*(ypos2-ycoor[2]); // distance2 hit-track at CH2(4)
      double avdist = 0.5*(dist1+dist2); // average distance2 from the track
      if (avdist<30.*30.) continue;
      // Conditions on segments 34
      double outvar[6] = {99999.,99999.,0.,99999.,99999.,0};
      double deltatheta = 999999.;
      double pexp = 999999.;
      if (cH==3) {
        double xslopea = ((xcoor[2]-xcoor[1])/(zcoor[2]-zcoor[1]));
        double zmag = 0.5*(196350+197650)-5.;
        double xatmag = xcoor[1]+xslopea*(zmag-zcoor[1]);
        double xslopeb = (xcoor[0]-xatmag)/(zcoor[0]-zmag);
        deltatheta = xslopeb-xslopea;
        pexp = 1000*1300.*TMath::C()*1.e-9*1.e-4 *1.e-2*0.6928*1000./deltatheta;
        if (pexp>10000. || pexp<-90000.) continue;
      }

      // Minimum condition
      if ((chi2x+chi2y)<fMinChi2[nS]) {
        fMinChi2[nS] = chi2x+chi2y;
        fMinDist[nS] = avdist;
        fMinTime[nS] = fabs(fVertex[5]-0.5*(iT+jT));
        fMinXCoor[0][nS] = xcoor[1];
        fMinYCoor[0][nS] = ycoor[1];
        fMinXCoor[1][nS] = xcoor[2];
        fMinYCoor[1][nS] = ycoor[2];
        fMinSlopeX[nS] = cH==1 ? slopex : deltatheta;
        fMinSlopeY[nS] = slopey;
        fPExp = pexp;
        fMinLKrDT = outvar[1]-fVertex[5];
        fMinLKrDX = outvar[3];
        fMinLKrDY = outvar[4];
        fMinLKrD = outvar[0];
        if (cH==3) {
          fMinYCh1 = posy+slopey*zcoor[1];
        }
        fSaveTime = jT;
      }
    }
  }
}

void StrawSegmentAlgorithm::AnalyzeHits(int idtrack)
{
  fThisTrack = dynamic_cast<TRecoSpectrometerCandidate*>(
    fSpectrometerEvent->GetCandidate(idtrack));
  fThisTrack->SetEvent(fSpectrometerEvent);
  FillTrackHitsArray();
  FillNoTrackHitsArray();
}

void StrawSegmentAlgorithm::FillTrackHitsArray()
{
  for (Int_t j=0; j<fThisTrack->GetNHits(); j++) {
    Int_t idHit = GetTrackHitIndex(j); // id of the hit j-esimo belonging to the track
    fStrawCandidateHit->SetIndex(j,idHit);
  }
}

int StrawSegmentAlgorithm::GetTrackHitIndex(int jHit){
  Int_t *hitIndex = (Int_t *)fThisTrack->GetHitsIndexes();
  Int_t hitID = hitIndex[jHit];
  return hitID;
}

void StrawSegmentAlgorithm::FillTrackHitsArray(TRecoSpectrometerCandidate* tr){
  int *hitIndex = (Int_t *)tr->GetHitsIndexes();
  for (Int_t j=0; j<fThisTrack->GetNHits(); j++) {
    if (j<fThisTrack->GetNHits()) {
      fStrawCandidateHit->SetIndex(j,GetTrackHitIndex(j));
    } else {
      fStrawCandidateHit->SetIndex(j,hitIndex[j-fThisTrack->GetNHits()]);
    }
  }
}

void StrawSegmentAlgorithm::FillNoTrackHitsArray(){
  //    TClonesArray& Hits = (*(fSpectrometerEvent->GetHits()));
  Int_t jNoTrack = 0;
  for (Int_t j=0; j < fSpectrometerEvent->GetNHits(); j++) {
    Bool_t trackhit = false;
    for (Int_t k=0; k<fThisTrack->GetNHits(); k++) { // Check if the hit j belongs to the track
      if (j==fStrawCandidateHit->GetIndex(k)) {
        trackhit=true;
        break;
      }
    }
    if (trackhit) continue;
    if (jNoTrack>=500) continue; // Protection against crowded events
    //                      TRecoSpectrometerHit *hit = (TRecoSpectrometerHit*)Hits[j];
    ////    if (hit->GetLocalPosition().Z()==0) continue;
    fStrawNoCandidateHit->SetIndex(jNoTrack,j);
    ////    cout << "not matching track " << jNoTrack << " " << j << " " << hit->GetTime() << " " <<
    ////                                     hit->GetStrawID() << " " << hit->GetPlaneID() << " " << hit->GetHalfViewID() << " " << hit->GetViewID() << " " << hit->GetChamberID() << " " <<
    ////                                     hit->GetLocalPosition().X() << " " << hit->GetLocalPosition().Z() << " " <<
    ////                                     fStrawHitLocalX[hit->GetChamberID()][hit->GetViewID()][2*hit->GetHalfViewID()+hit->GetPlaneID()][hit->GetStrawID()] << " " <<
    ////                                     fStrawHitGlobalZ[hit->GetChamberID()][hit->GetViewID()][2*hit->GetHalfViewID()+hit->GetPlaneID()][hit->GetStrawID()] << endl;
    jNoTrack++;
  }
  fNNoTrackHits = jNoTrack;
  if (!fNNoTrackHits) return;
  SolveLR();
  BuildChamberHit(0);
  BuildChamberHit(1);
  BuildChamberHit(2);
  BuildChamberHit(3);
}

double StrawSegmentAlgorithm::SigmaRadius(Double_t radius){
  Double_t sigRad1 = 0.7-0.363*radius;
  Double_t sigRad2 = 0.476-0.147*radius+0.0092*radius*radius+0.00135*radius*radius*radius;
  Double_t sigRad3 = 0.1218;
  double out = 0.;
  if (radius==0){
    out = 5./sqrt(12.);
  }else if(radius<1 && radius>0){
    out = sqrt(sigRad1*sigRad1);
  }else if(radius>=1 && radius<4){
    out = sqrt(sigRad2*sigRad2);
  }else if(radius>=4){
    out = sqrt(sigRad3*sigRad3);
  }else{
    out = 0.;
  };
  return out;
}

void StrawSegmentAlgorithm::CreateStrawGeometry(){
  Int_t nStraws = 122;

  // X coordinate of the planes
  Double_t xOffset[4][4];
  xOffset[0][0] = -1058.2;
  xOffset[0][1] = -1067.0;
  xOffset[0][2] = -1071.4;
  xOffset[0][3] = -1062.6;
  xOffset[1][0] = -1062.6;
  xOffset[1][1] = -1071.4;
  xOffset[1][2] = -1067.0;
  xOffset[1][3] = -1058.2;
  xOffset[2][0] = -1058.2;
  xOffset[2][1] = -1067.0;
  xOffset[2][2] = -1071.4;
  xOffset[2][3] = -1062.6;
  xOffset[3][0] = -1080.2;
  xOffset[3][1] = -1089.0;
  xOffset[3][2] = -1084.6;
  xOffset[3][3] = -1075.8;

  double xoffch[4][4];
  xoffch[0][0] = -0.0384875 -0.015344;
  xoffch[0][1] = +0.0692588 +0.0164608;
  xoffch[0][2] = -0.145934  -0.0275812;
  xoffch[0][3] = +0.0115516 +0.0152055;
  xoffch[1][0] = +0.212612  +0.0459039;
  xoffch[1][1] = +0.0298165 +0.0154931;
  xoffch[1][2] = +0.132463  +0.0239249;
  xoffch[1][3] = -0.00204968-0.0177563;
  xoffch[2][0] = +0.00857821-0.0183119;
  xoffch[2][1] = +0.0357917 -0.0144992;
  xoffch[2][2] = -0.296724  -0.0356126;
  xoffch[2][3] = +0.0468851 -0.000487492;
  xoffch[3][0] = +0.0425757 -0.00282565;
  xoffch[3][1] = +0.0893313 +0.0230893;
  xoffch[3][2] = -0.0186113 -0.00382283;
  xoffch[3][3] = -0.0292184 +0.00744541;

  double xstrawalign[64][123];
  TString line;
  TString Spectrometer_Alignment = "Spectrometer-Alignment.dat";

  if(NA62ConditionsService::GetInstance()->Open(Spectrometer_Alignment)==kSuccess){
    while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(Spectrometer_Alignment))) {
      if (line.BeginsWith("#")) continue;
      TObjArray * l = line.Tokenize(" ");
      Int_t planeid = ((TObjString*)(l->At(0)))->GetString().Atoi();
      Int_t strawid = ((TObjString*)(l->At(1)))->GetString().Atoi();
      Double_t align1 = ((TObjString*)(l->At(2)))->GetString().Atof();
      Double_t align2 = ((TObjString*)(l->At(3)))->GetString().Atof();
      Double_t align3 = ((TObjString*)(l->At(4)))->GetString().Atof();
      xstrawalign[planeid][strawid] = align1+align2+align3;
      if (planeid==63 && strawid==122) break;
    }
  }
  NA62ConditionsService::GetInstance()->Close(Spectrometer_Alignment);

  double viewSpacing[2];
  viewSpacing[0]       = 57.0;
  viewSpacing[1]       = 233.0;

  // Z coordinate of the plabes
  Double_t zview[4] = {-viewSpacing[0]-viewSpacing[1]/2,-viewSpacing[1]/2,viewSpacing[1]/2,viewSpacing[0]+viewSpacing[1]/2};
  Double_t zplan[4] = {-18.5,-7.5,7.5,18.5};

  // Z chamber
  double chamberZPosition[4];
  chamberZPosition[0] = 0.5 * (183311.1 + 183704.9);
  chamberZPosition[1] = 0.5 * (193864.1 + 194262.9)+2.5;
  chamberZPosition[2] = 0.5 * (204262.1 + 204655.9);
  chamberZPosition[3] = 0.5 * (218688.1 + 219081.9);
  Double_t zchoffset[4] = {-0.1,-3.1,-9.1,-9.3};
  for (int jc=0; jc<4; jc++) fChamberZPosition[jc] = chamberZPosition[jc];

  double strawSpacing = 17.6;

  // Straw Positions
  for (Int_t jChamber=0; jChamber<4; jChamber++) {
    for (Int_t jView=0; jView<4; jView++) {
      for (Int_t jPlane=0; jPlane<4; jPlane++) {
        //          cout << jChamber << " " << jView << " " << jPlane << " " << jStraw << " " << fStrawHitLocalX[jChamber][jView][jPlane][jStraw] << " " << fStrawHitGlobalZ[jChamber][jView][jPlane][jStraw] << endl;
        Double_t zcoord = chamberZPosition[jChamber]+zview[jView]+zplan[jPlane]+zchoffset[jChamber];
        for (Int_t jStraw=0; jStraw<nStraws; jStraw++) {
          int planeid = 16*jChamber+4*jView+jPlane;
          fStrawHitLocalX[jChamber][jView][jPlane][jStraw] = strawSpacing*jStraw+xOffset[jView][jPlane]+xoffch[jChamber][jView]-xstrawalign[planeid][jStraw+1];
          fStrawHitGlobalZ[jChamber][jView][jPlane][jStraw] = zcoord;
        }
      }
    }
  }

  fHoleChamberMax[0][0] = 134.2;
  fHoleChamberMax[0][1] = 134.2;
  fHoleChamberMax[0][2] = 165.0;
  fHoleChamberMax[0][3] = 63.8;
  fHoleChamberMin[0][0] = 6.6;
  fHoleChamberMin[0][1] = 6.6;
  fHoleChamberMin[0][2] = 37.4;
  fHoleChamberMin[0][3] = -63.8;
  fHoleChamberMax[1][0] = 143.0;
  fHoleChamberMax[1][1] = 143.0;
  fHoleChamberMax[1][2] = 178.2;
  fHoleChamberMax[1][3] = 63.8;
  fHoleChamberMin[1][0] = 15.4;
  fHoleChamberMin[1][1] = 15.4;
  fHoleChamberMin[1][2] = 50.6;
  fHoleChamberMin[1][3] = -63.8;
  fHoleChamberMax[2][0] = 129.8;
  fHoleChamberMax[2][1] = 129.8;
  fHoleChamberMax[2][2] = 156.2;
  fHoleChamberMax[2][3] = 63.8;
  fHoleChamberMin[2][0] = 2.2;
  fHoleChamberMin[2][1] = 2.2;
  fHoleChamberMin[2][2] = 28.6;
  fHoleChamberMin[2][3] = -63.8;
  fHoleChamberMax[3][0] = 103.4;
  fHoleChamberMax[3][1] = 103.4;
  fHoleChamberMax[3][2] = 116.6;
  fHoleChamberMax[3][3] = 63.8;
  fHoleChamberMin[3][0] = -24.2;
  fHoleChamberMin[3][1] = -24.2;
  fHoleChamberMin[3][2] = -11.0;
  fHoleChamberMin[3][3] = -63.8;
  fViewPlaneTransverseSize = (120-1)*17.6+2*(4.875+2*0.00005+0.036+0.00002);

}

void StrawSegmentAlgorithm::SolveLR(){
  // Sort hits per view
  vector<int> idHP[4][4][4];
  int jNCl[4][4]; // counter of cluster hits per chamber view
  for (int jc=0; jc<4; jc++) {
    for (int jv=0; jv<4; jv++) {
      jNCl[jc][jv] = 0;
      for (int jp=0; jp<4; jp++) {
        idHP[jc][jv][jp].resize(fNNoTrackHits);
        iota(begin(idHP[jc][jv][jp]),end(idHP[jc][jv][jp]),0);
        PlaneChamberCondition ncp(jc,jv,jp,fSpectrometerEvent,fStrawNoCandidateHit);
        idHP[jc][jv][jp].erase(remove_if(begin(idHP[jc][jv][jp]),end(idHP[jc][jv][jp]),ncp),end(idHP[jc][jv][jp]));
      }
    }
  }
  // Pairing
  for (int jc=0; jc<4; jc++) {
    for (int jv=0; jv<4; jv++) {
      for (int jp=0; jp<4; jp++) {
        for (int jh=0; jh<(int)idHP[jc][jv][jp].size(); jh++) { // start loop hits first plane
          if (fStrawNoCandidateHit->GetPaired(idHP[jc][jv][jp][jh])==3) continue;
          TRecoSpectrometerHit *hit = static_cast<TRecoSpectrometerHit*>(fSpectrometerEvent->GetHit(fStrawNoCandidateHit->GetIndex(idHP[jc][jv][jp][jh])));
          double hpos[2] = {fStrawHitGlobalZ[jc][jv][jp][hit->GetStrawID()],fStrawHitLocalX[jc][jv][jp][hit->GetStrawID()]};
          // double tpos = GetLocalCoordinate(fThisTrack,jv,hpos[0]);
          // if (fabs(tpos-hpos[1])<30.) continue;
          if (hit->GetDriftTime()<0||hit->GetDriftTime()>170) continue;
          if (!hit->GetEdgeStatus()) continue;
          double hitTTime = hit->GetEdgeStatus() ? hit->GetDriftTime()+hit->GetTimeWidth() : 170.;
          // Look for doublets
          for (int jp1=jp+1; jp1<4; jp1++) { // start loop next1 plane
            for (int jh1=0; jh1<(int)idHP[jc][jv][jp1].size(); jh1++) { // start loop hits next1 plane
              if (fStrawNoCandidateHit->GetPaired(idHP[jc][jv][jp][jh])==3) continue;
              if (fStrawNoCandidateHit->GetPaired(idHP[jc][jv][jp1][jh1])==3) continue;
              TRecoSpectrometerHit *hit1 = static_cast<TRecoSpectrometerHit*>(fSpectrometerEvent->GetHit(fStrawNoCandidateHit->GetIndex(idHP[jc][jv][jp1][jh1])));
              double hpos1[2] = {fStrawHitGlobalZ[jc][jv][jp1][hit1->GetStrawID()],fStrawHitLocalX[jc][jv][jp1][hit1->GetStrawID()]};
              //              if (fabs(GetLocalCoordinate(fThisTrack,jv,hpos1[0])-hpos1[1])<30.) continue;
              if (hit1->GetDriftTime()<0||hit1->GetDriftTime()>170) continue;
              if (!hit1->GetEdgeStatus()) continue;
              double deltaStrawX = hpos[1]-hpos1[1];
              if (fabs(deltaStrawX)>9.) continue;
              double hitTTime1 = hit1->GetEdgeStatus() ? hit1->GetDriftTime()+hit1->GetTimeWidth() : 170.; // Trailing time
              bool trailing_pair = hit->GetEdgeStatus() && hit1->GetEdgeStatus();
              if (trailing_pair && fabs(hitTTime1-hitTTime)>=150.) continue; // Cut on the hit time if the trailing time exists for all the hits
              // Look for triplets
              double fPos(99999.);
              double fPos1(99999.);
              double fPos2(99999.);
              double slope(99999.);
              int matchHitId(-1);
              int lastPlane(-1);
              int lastHit(-1);
              double chi2min(999999.);
              double avTTimemin = -99999.;
              bool trtripletmin = 0;
              for (int jp2=jp1+1; jp2<4; jp2++) { // start loop next2 plane
                for (int jh2=0; jh2<(int)idHP[jc][jv][jp2].size(); jh2++) { // start loop hits next2 plane
                  if (fStrawNoCandidateHit->GetPaired(idHP[jc][jv][jp][jh])==3) continue;
                  if (fStrawNoCandidateHit->GetPaired(idHP[jc][jv][jp1][jh1])==3) continue;
                  if (fStrawNoCandidateHit->GetPaired(idHP[jc][jv][jp2][jh2])==3) continue;
                  TRecoSpectrometerHit *hit2 = static_cast<TRecoSpectrometerHit*>(fSpectrometerEvent->GetHit(fStrawNoCandidateHit->GetIndex(idHP[jc][jv][jp2][jh2])));
                  double hpos2[2] = {fStrawHitGlobalZ[jc][jv][jp2][hit2->GetStrawID()],fStrawHitLocalX[jc][jv][jp2][hit2->GetStrawID()]};
                  //                  if (fabs(GetLocalCoordinate(fThisTrack,jv,hpos2[0])-hpos2[1])<30.) continue;
                  if (hit2->GetDriftTime()<0||hit2->GetDriftTime()>170) continue;
                  if (!hit2->GetEdgeStatus()) continue;
                  if (fabs(hpos2[1]-hpos1[1])>9.) continue;
                  double tSum3 = hit->GetWireDistance()+hit1->GetWireDistance()+hit2->GetWireDistance();
                  double hitTTime2 = hit2->GetEdgeStatus() ? hit2->GetDriftTime()+hit2->GetTimeWidth() : 170.;
                  bool trailing_triplet = trailing_pair && hit2->GetEdgeStatus();
                  if (trailing_triplet && fabs(hitTTime2-hitTTime1)>=150.) continue; // Cut on the hit time if the trailing time exists for all the hits
                  if (trailing_triplet && fabs(hitTTime2-hitTTime)>=150.) continue; // Cut on the hit time if the trailing time exists for all the hits
                  if (tSum3<7.5 || tSum3>11.) continue;
                  double vrad[3] = {hit->GetWireDistance(),hit1->GetWireDistance(),hit2->GetWireDistance()};
                  double xval[3] = {99999.,99999.,99999.};
                  double chi2 = ChooseTheCombination(hpos,hpos1,hpos2,vrad,xval,&slope);
                  double avTTime = trailing_triplet ? (hitTTime+hitTTime1+hitTTime2)/3.-170. : 0;
                  chi2 += fabs(avTTime)/20.; // If no trailing in one of the hits the trailing time is not used for chi2
                  if (chi2<chi2min && chi2<9. && chi2>=0) {
                    fPos = xval[0];
                    fPos1 = xval[1];
                    fPos2 = xval[2];
                    matchHitId = fStrawNoCandidateHit->GetIndex(idHP[jc][jv][jp2][jh2]);
                    lastPlane = jp2;
                    lastHit = jh2;
                    chi2min = chi2;
                    avTTimemin = trailing_triplet ? avTTime+170. : avTTime;
                    trtripletmin = trailing_triplet;
                  }
                } // end loop hits in next2 plane
              } // end loop next2 plane
              // Triplet found and saved
              if (matchHitId>-1) {
                TRecoSpectrometerHit *matchHit = static_cast<TRecoSpectrometerHit*>(fSpectrometerEvent->GetHit(matchHitId));
                //                fStrawHitLocalX[jc][jv][jp][hit->GetStrawID()] = fPos;
                //                fStrawHitLocalX[jc][jv][jp1][hit1->GetStrawID()] = fPos1;
                //                fStrawHitLocalX[jc][jv][lastPlane][matchHit->GetStrawID()] = fPos2;
                if (jNCl[jc][jv]<50) {
                  fStrawCluster[jc][jv]->SetNHits(jNCl[jc][jv],3);
                  fStrawCluster[jc][jv]->SetIndex(0,jNCl[jc][jv],idHP[jc][jv][jp][jh]);
                  fStrawCluster[jc][jv]->SetIndex(1,jNCl[jc][jv],idHP[jc][jv][jp1][jh1]);
                  fStrawCluster[jc][jv]->SetIndex(2,jNCl[jc][jv],idHP[jc][jv][lastPlane][lastHit]);
                  fStrawCluster[jc][jv]->SetX(jNCl[jc][jv],(1./3.)*(fPos+fPos1+fPos2));
                  fStrawCluster[jc][jv]->SetZ(jNCl[jc][jv],(1./3.)*(fStrawHitGlobalZ[jc][jv][jp][hit->GetStrawID()]+
                                                                    fStrawHitGlobalZ[jc][jv][jp1][hit1->GetStrawID()]+
                                                                    fStrawHitGlobalZ[jc][jv][lastPlane][matchHit->GetStrawID()]));
                  fStrawCluster[jc][jv]->SetT(jNCl[jc][jv],avTTimemin);
                  fStrawCluster[jc][jv]->SetEdge(jNCl[jc][jv],trtripletmin);
                  fStrawCluster[jc][jv]->SetQuality(jNCl[jc][jv],chi2min);
                }
                fStrawNoCandidateHit->SetPaired(idHP[jc][jv][jp][jh],3);
                fStrawNoCandidateHit->SetPaired(idHP[jc][jv][jp1][jh1],3);
                fStrawNoCandidateHit->SetPaired(idHP[jc][jv][lastPlane][lastHit],3);
                jNCl[jc][jv]++;
              }
              // Define and store doublets
              if (matchHitId==-1) {
                int sign = -1;
                int isign = deltaStrawX ? deltaStrawX/fabs(deltaStrawX) : 0;
                int hitPlanes[2] = {jp,jp1};
                double hitWireDistance[2] = {hit->GetWireDistance(),hit1->GetWireDistance()};
                bool paired = 0;
                if (Pairing(hitWireDistance,hitPlanes)) {
                  fPos = hpos[1]+isign*sign*hitWireDistance[0];
                  fPos1 = hpos1[1]-isign*sign*hitWireDistance[1];
                  if (fabs(deltaStrawX-isign*4.4)<1) {
                    if (hitWireDistance[1]>4.45) fPos = hpos[1]-isign*sign*hitWireDistance[0];
                    if (hitWireDistance[0]>4.45) fPos1 = hpos1[1]+isign*sign*hitWireDistance[1];
                  }
                  paired = 1;
                }
                if (paired) {
                  double sumwire = hitWireDistance[0]+hitWireDistance[1];
                  double sr1 = SigmaRadius(hitWireDistance[0]);
                  double sr2 = SigmaRadius(hitWireDistance[1]);
                  double avttime = trailing_pair ? (hitTTime+hitTTime1)/2. : 170.;
                  double timechi2 = trailing_pair ? fabs(avttime-170.)/25. : 0;
                  double s2sumwire = sr1*sr1+sr2*sr2;
                  double sumchi2 = sumwire>7.5 ? fabs(sumwire-2*4.4)/sqrt(s2sumwire) : fabs(sumwire-4.4)/sqrt(s2sumwire);
                  if ((sumchi2<5 && timechi2<4.) || (sumchi2>=5 && timechi2<1.5)) {
                    if (((sumwire>7.5 && sumchi2+timechi2<2.5) || (sumwire<7.5))) {
                      //                      fStrawHitLocalX[jc][jv][jp][hit->GetStrawID()] = fPos;
                      //                      fStrawHitLocalX[jc][jv][jp1][hit1->GetStrawID()] = fPos1;
                      if (jNCl[jc][jv]<50) {
                        fStrawCluster[jc][jv]->SetNHits(jNCl[jc][jv],2);
                        fStrawCluster[jc][jv]->SetIndex(0,jNCl[jc][jv],idHP[jc][jv][jp][jh]);
                        fStrawCluster[jc][jv]->SetIndex(1,jNCl[jc][jv],idHP[jc][jv][jp1][jh1]);
                        fStrawCluster[jc][jv]->SetX(jNCl[jc][jv],0.5*(fPos+fPos1));
                        fStrawCluster[jc][jv]->SetZ(jNCl[jc][jv],0.5*(fStrawHitGlobalZ[jc][jv][jp][hit->GetStrawID()]+fStrawHitGlobalZ[jc][jv][jp1][hit1->GetStrawID()]));
                        fStrawCluster[jc][jv]->SetT(jNCl[jc][jv],avttime);
                        fStrawCluster[jc][jv]->SetEdge(jNCl[jc][jv],trailing_pair);
                        fStrawCluster[jc][jv]->SetQuality(jNCl[jc][jv],sumchi2+timechi2);
                      }
                      fStrawNoCandidateHit->SetPaired(idHP[jc][jv][jp][jh],2);
                      fStrawNoCandidateHit->SetPaired(idHP[jc][jv][jp1][jh1],2);
                      jNCl[jc][jv]++;
                    }
                  } // End condition on timechi2
                }
              }
            } // end loop hits in next1 plane
          } // end loop next1 plane
        } // end loop hit in plane
      } // end loop plane
      fStrawCluster[jc][jv]->SetNClusters(jNCl[jc][jv]);
    }
  }
  // Set number of clusters and store not paired clusters
  for (int jc=0; jc<4; jc++) {
    for (int jv=0; jv<4; jv++) {
      for (int jp=0; jp<4; jp++) {
        for (int jh=0; jh<(int)idHP[jc][jv][jp].size(); jh++) {
          if (fStrawNoCandidateHit->GetPaired(idHP[jc][jv][jp][jh])>0) continue;
          TRecoSpectrometerHit *hit = static_cast<TRecoSpectrometerHit*>(fSpectrometerEvent->GetHit(fStrawNoCandidateHit->GetIndex(idHP[jc][jv][jp][jh])));
          if (hit->GetDriftTime()<0||hit->GetDriftTime()>170) continue;
          if (!hit->GetEdgeStatus()) continue;
          fStrawNoCandidateHit->SetPaired(idHP[jc][jv][jp][jh],1);
          double ttime = hit->GetEdgeStatus() ? hit->GetDriftTime()+hit->GetTimeWidth() : 170.;
          if (jNCl[jc][jv]<50) {
            fStrawCluster[jc][jv]->SetNHits(jNCl[jc][jv],1);
            fStrawCluster[jc][jv]->SetIndex(0,jNCl[jc][jv],idHP[jc][jv][jp][jh]);
	    double hpos[2] = {fStrawHitGlobalZ[jc][jv][jp][hit->GetStrawID()],fStrawHitLocalX[jc][jv][jp][hit->GetStrawID()]};
	    // double tpos = GetLocalCoordinate(fThisTrack,jv,hpos[0]);
	    // if (fabs(tpos-hpos[1])<30.) continue;
            fStrawCluster[jc][jv]->SetX(jNCl[jc][jv],hpos[1]);
            fStrawCluster[jc][jv]->SetZ(jNCl[jc][jv],hpos[0]);
            fStrawCluster[jc][jv]->SetT(jNCl[jc][jv],ttime);
            fStrawCluster[jc][jv]->SetEdge(jNCl[jc][jv],hit->GetEdgeStatus());
            fStrawCluster[jc][jv]->SetQuality(jNCl[jc][jv],99999.);
          }
          jNCl[jc][jv]++;
        }
      }
      fStrawCluster[jc][jv]->SetNClusters(jNCl[jc][jv]);
    }
  }
}

double StrawSegmentAlgorithm::GetLocalCoordinate(TRecoSpectrometerCandidate* tr, int v, double zh){
  double sq2 = sqrt(2.);
  TVector3 pos(tr->xAt(zh), tr->yAt(zh), zh);
  if (v==0) return (pos.X()-pos.Y())/sq2;
  if (v==1) return (pos.X()+pos.Y())/sq2;
  if (v==2) return pos.X();
  if (v==3) return pos.Y();
  return -9999.;
}

//Best triplet assignment according to the chi2
double StrawSegmentAlgorithm::ChooseTheCombination(double* h0, double* h1, double* h2, double* radius, double* xGood, double* slopeGood){
  int c[3];
  double z[3] = {h0[0],h1[0],h2[0]};
  double xs[3] = {h0[1],h1[1],h2[1]};
  double chi2min(999999999.);
  int icounter(1);
  for (Int_t jc=0;jc<8;jc++){
    c[0] = jc<4 ? 1 : -1;
    c[1] = (jc==0 || jc==1 || jc==4 || jc==5) ? 1 : -1;
    c[2] = icounter;
    icounter = - icounter;
    if (c[0]==c[1] && c[0]==c[2]) continue;
    double x[3],error[3];
    for (Int_t j=0;j<3;j++){
      x[j] = xs[j]+c[j]*radius[j];
      error[j] = SigmaRadius(radius[j]);
    }
    double slope = -99999.;
    double pos = -99999.;
    double chi2 = Chi2LinearFit(x,z,error,&slope,&pos);
    if (chi2<chi2min && fabs(slope)<=0.1){
      chi2min = chi2;
      for (Int_t jg=0; jg<3; jg++) xGood[jg] = x[jg];
      *slopeGood = slope;
    }
  }
  return chi2min;
}

double StrawSegmentAlgorithm::Chi2LinearFit(double *x, double *z, double *error, double *sss, double *position){
  Double_t chi22=0.,sumx=0.,sumy=0.,sumxy=0.,sumx2=0.;
  for(Int_t i=0;i<3;i++) {
    sumy += x[i];
    sumx += z[i];
    sumx2 += z[i]*z[i];
    sumxy += (z[i]*x[i]);
  }
  Double_t den = 3*sumx2-(sumx*sumx);
  Double_t num1 = sumy*sumx2-sumx*sumxy;
  Double_t q = num1/den;
  Double_t num = 3*sumxy-sumx*sumy;
  Double_t mmm = num/den;
  *sss = mmm;
  *position = q;
  for(Int_t i=0;i<3;i++) chi22 += (x[i]-q-mmm*z[i])*(x[i]-q-mmm*z[i])/(error[i]*error[i]);
  return chi22;
}

bool StrawSegmentAlgorithm::Pairing(double *dist, int *hits){
  Double_t sumdist = dist[0]+dist[1];

  if ((dist[0]<1.8 && dist[1]>3.8) || (dist[0]>3.8 && dist[1]<1.8)) {
    if (hits[0]==0 && hits[1]==2) return true;
    if (hits[0]==0 && hits[1]==3) return true;
    if (hits[0]==1 && hits[1]==2) return true;
    if (hits[0]==1 && hits[1]==3) return true;
  }

  if (dist[0]>3.5 && dist[1]>3.5 && sumdist>8.2 && sumdist<9.3) {
    if (hits[0]==0 && hits[1]==1) return true;
    if (hits[0]==2 && hits[1]==3) return true;
  }

  if (dist[0]<=3.8 && dist[1]<=3.8) {
    if (hits[0]==0 && hits[1]==2) {
      if (sumdist<7.0 && sumdist>0.5) return true;
    }
    if (hits[0]==0 && hits[1]==3) {
      if (sumdist<7.5 && sumdist>0.3) return true;
    }
    if (hits[0]==1 && hits[1]==2) {
      if (sumdist<7.5 && sumdist>0.3) return true;
    }
    if (hits[0]==1 && hits[1]==3) {
      if (sumdist<7.0 && sumdist>0.5) return true;
    }
  }

  return false;
}

/////////////////// CHAMBER HITS ////////////////////////////////////////
void StrawSegmentAlgorithm::BuildChamberHit(int cH){
  bool trailing_triplet = false;
  bool trailing_quadruplet = false;

  for (int jv=0; jv<4; jv++) {
    if (fStrawCluster[cH][jv]->GetNClusters()>=50) return;
  }

  int jView = 0;
  while(jView<=3) {
    int nClusterView = fStrawCluster[cH][jView]->GetNClusters()<50 ? fStrawCluster[cH][jView]->GetNClusters() : 50;
    for (int j(0); j<nClusterView; j++) {
      PnnStrawIntersection inters;
      inters.Clear();
      ////      for (int kk=0; kk<fStrawCluster[cH][jView]->GetNHits(j);kk++) {
      ////       int id = fStrawCluster[cH][jView]->GetIndex(kk,j);
      ////       int idHit = fStrawNoCandidateHit->GetIndex(id);
      ////       TRecoSpectrometerHit *hit = (TRecoSpectrometerHit *)fSpectrometerEvent->GetHit(idHit);
      ////       cout << cH << " " << jView << " " << hit->GetChamberID() << " " << hit->GetViewID() << endl;
      ////      }
      //<<      if (fStrawCluster[cH][jView]->GetNHits(j)<2) continue;
      int nextView = jView+1;
      while(nextView<=3) {
        int nClusterNextView = fStrawCluster[cH][nextView]->GetNClusters()<50 ? fStrawCluster[cH][nextView]->GetNClusters() : 50;
        for (int k(0); k<nClusterNextView; k++) {
          //<<          if (fStrawCluster[cH][nextView]->GetNHits(k)<2) continue;
          int intType2 = IntType2(jView,nextView);
          bool trailing_pair = (fStrawCluster[cH][jView]->GetEdge(j) && fStrawCluster[cH][nextView]->GetEdge(k));
          if (trailing_pair && fabs(fStrawCluster[cH][jView]->GetT(j)-fStrawCluster[cH][nextView]->GetT(k))>=200.) continue; // It depends on the inclusion of the single hits (standard reco 100. without single hits)
          double xyinter[4] = {-9999.,-9999.,-9999.,-9999.};
          ComputeCoordinate(intType2,fStrawCluster[cH][jView]->GetX(j),fStrawCluster[cH][nextView]->GetX(k),xyinter); // transforms the coordinate in xyuv
          double tinter2 = trailing_pair ? fStrawCluster[cH][jView]->GetT(j)+fStrawCluster[cH][nextView]->GetT(k) : -2*999999.;
          inters.SetTrailingTime(tinter2/2.);
          inters.SetQuality(9999);
          inters.SetSubType(intType2);
          inters.SetCoordinate(xyinter);
          int next2nextView = nextView+1;
          while(next2nextView<=3){ // Look for 3-view intersections (only 1 per 3-view intersection)
            double mindist3(99999.);
            int mini(-1);
            int intType3(-1);
            int nClusterNext2NextView = fStrawCluster[cH][next2nextView]->GetNClusters()<50 ? fStrawCluster[cH][next2nextView]->GetNClusters() : 50;
            for (int i(0); i<nClusterNext2NextView; i++) {
              //<<              if (fStrawCluster[cH][next2nextView]->GetNHits(i)<2) continue;
              trailing_triplet = (trailing_pair && fStrawCluster[cH][next2nextView]->GetEdge(i));
              //              if (trailing_triplet && fStrawCluster[cH][next2nextView]->GetT(i)-fStrawCluster[cH][nextView]->GetT(k)==0) cout << fStrawCluster[cH][next2nextView]->GetT(i) << " " << fStrawCluster[cH][nextView]->GetT(k) << " " << fEventNumber << " " << fStrawCluster[cH][jView]->GetEdge(j) << " " << fStrawCluster[cH][nextView]->GetEdge(k) << " " << fStrawCluster[cH][next2nextView]->GetEdge(i) << endl;
              if (trailing_triplet && fabs(fStrawCluster[cH][next2nextView]->GetT(i)-fStrawCluster[cH][jView]->GetT(j))>=200.) continue;
              if (trailing_triplet && fabs(fStrawCluster[cH][next2nextView]->GetT(i)-fStrawCluster[cH][nextView]->GetT(k))>=200.) continue;
              if (trailing_triplet && fabs(tinter2/2.-fStrawCluster[cH][next2nextView]->GetT(i))>=200.) continue;
              double distance3(99999.);
              intType3 = IntType3(intType2,next2nextView);
              if (!IntersectionQuality(intType3,xyinter,fStrawCluster[cH][next2nextView]->GetX(i),&distance3)) continue; // distance from the 2-view intersection
              double tave3 = trailing_triplet ? (tinter2+fStrawCluster[cH][next2nextView]->GetT(i))/3 : -999999.;
              double stave3 = (fStrawCluster[cH][next2nextView]->GetT(i)-tave3)*(fStrawCluster[cH][next2nextView]->GetT(i)-tave3)+
                (fStrawCluster[cH][jView]->GetT(j)-tave3)*(fStrawCluster[cH][jView]->GetT(j)-tave3)+
                (fStrawCluster[cH][nextView]->GetT(k)-tave3)*(fStrawCluster[cH][nextView]->GetT(k)-tave3);
              stave3 /= 3.;
              double quality3 = distance3; // Cluster quality not used because of the inclusion of single hits
              if (trailing_triplet) quality3 = distance3+sqrt(stave3)/20.;
              ////              quality3 /= (fStrawCluster[cH][next2nextView]->GetNHits(i)/2.+fStrawCluster[cH][nextView]->GetNHits(k)/2.+fStrawCluster[cH][jView]->GetNHits(j)/2.)/3.;
              if (quality3<mindist3) { mindist3=quality3; mini=i; }
            } // end cluster loop in 3-view search
            if (mini>-1) { // Search for a 4-view intersection only if a 3-view intersection exists
              trailing_triplet = (trailing_pair && fStrawCluster[cH][next2nextView]->GetEdge(mini));
              double tinter3 = trailing_triplet ? tinter2+fStrawCluster[cH][next2nextView]->GetT(mini) : -3*999999.;
              UpdateCoordinate(intType3,xyinter,fStrawCluster[cH][next2nextView]->GetX(mini));
              Int_t next2next2nextView = next2nextView+1;
              Int_t intType4(15);
              while(next2next2nextView<=3){ // Look for 4-view intersections (only 1 per 4-view intersection)
                double mindist4(99999.);
                int minl(-1);
                int nClusterNext2Next2NextView = fStrawCluster[cH][next2next2nextView]->GetNClusters() ? fStrawCluster[cH][next2next2nextView]->GetNClusters() : 50;
                for (Int_t l=0; l<nClusterNext2Next2NextView; l++){
                  double distance4(99999.);
                  //<<                  if (fStrawCluster[cH][next2next2nextView]->GetNHits(l)<2) continue;
                  trailing_quadruplet = (trailing_triplet && fStrawCluster[cH][next2next2nextView]->GetEdge(l));
                  if (trailing_quadruplet && fabs(fStrawCluster[cH][next2next2nextView]->GetT(l)-fStrawCluster[cH][jView]->GetT(j))>=200.) continue;
                  if (trailing_quadruplet && fabs(fStrawCluster[cH][next2next2nextView]->GetT(l)-fStrawCluster[cH][nextView]->GetT(k))>=200.) continue;
                  if (trailing_quadruplet && fabs(fStrawCluster[cH][next2next2nextView]->GetT(l)-fStrawCluster[cH][next2nextView]->GetT(mini))>=200.) continue;
                  if (trailing_quadruplet && fabs(tinter3/3.-fStrawCluster[cH][next2next2nextView]->GetT(l))>=200.) continue;
                  if (!IntersectionQuality(intType4,xyinter,fStrawCluster[cH][next2next2nextView]->GetX(l),&distance4)) continue;
                  double tave4 = trailing_quadruplet ? (tinter3+fStrawCluster[cH][next2next2nextView]->GetT(l))/4. : -999999.;
                  double stave4 = (fStrawCluster[cH][next2nextView]->GetT(mini)-tave4)*(fStrawCluster[cH][next2nextView]->GetT(mini)-tave4)+
                    (fStrawCluster[cH][jView]->GetT(j)-tave4)*(fStrawCluster[cH][jView]->GetT(j)-tave4)+
                    (fStrawCluster[cH][nextView]->GetT(k)-tave4)*(fStrawCluster[cH][nextView]->GetT(k)-tave4)+
                    (fStrawCluster[cH][next2next2nextView]->GetT(l)-tave4)*(fStrawCluster[cH][next2next2nextView]->GetT(l)-tave4);
                  stave4 /= 4.;
                  double quality4 = distance4/2.;
                  if (trailing_quadruplet) quality4 = distance4/2.+sqrt(stave4)/15.;
                  ////                  quality4 /= (fStrawCluster[cH][next2next2nextView]->GetNHits(l)/2.+fStrawCluster[cH][next2nextView]->GetNHits(mini)/2.+fStrawCluster[cH][nextView]->GetNHits(k)/2.+fStrawCluster[cH][jView]->GetNHits(j)/2.)/4.;
                  if (quality4<mindist4) { mindist4=quality4; minl=l; }
                }
                if (minl>-1) { // Store 4-view hit
                  trailing_quadruplet = (trailing_triplet && fStrawCluster[cH][next2next2nextView]->GetEdge(minl));
                  double tinter4 = trailing_quadruplet ? tinter3+fStrawCluster[cH][next2next2nextView]->GetT(minl) : -4*999999.;
                  UpdateCoordinate(15,xyinter,fStrawCluster[cH][next2next2nextView]->GetX(minl));
                  int arrayId[9] = {j,k,mini,minl,jView,nextView,next2nextView,next2next2nextView,intType4};
                  double variable[2] = {mindist4,tinter4/4};
                  if (StoreHit(cH,variable,arrayId,xyinter)) {
                    inters.SetSubType(arrayId[8]);
                    inters.SetCoordinate(xyinter);
                    for (int jvar=0; jvar<4; jvar++){
                      inters.SetClusterId(arrayId[jvar]);
                      inters.SetViewId(arrayId[jvar+4]);
                    }
                    inters.SetQuality(variable[0]);
                    inters.SetTrailingTime(variable[1]);
                    fChamberHit[cH].push_back(inters);
                    inters.Clear();
                    intType3 = -1;
                  }
                }
                next2next2nextView++;
              } // End 4-view search
              if (intType3>-1) { // Store 3-view hit only if the corresponding 4 view hit does not exist
                int arrayId[10] = {j,k,mini,-1,jView,nextView,next2nextView,-1,intType3};
                double variable[2] = {mindist3,tinter3/3};
                if (StoreHit(cH,variable,arrayId,xyinter)){
                  inters.SetSubType(arrayId[8]);
                  inters.SetCoordinate(xyinter);
                  for (Int_t jvar=0; jvar<3; jvar++){
                    inters.SetClusterId(arrayId[jvar]);
                    inters.SetViewId(arrayId[jvar+4]);
                  }
                  inters.SetQuality(variable[0]);
                  inters.SetTrailingTime(variable[1]);
                  fChamberHit[cH].push_back(inters);
                  inters.Clear();
                  intType2 = -1;
                }
              }
            } // end if (mini>-1)
            next2nextView++;
          } // End 3-view search
          if (intType2>-1) { // Store 2-view hit only if the corresponding 3 view hit does not exist
            if (AcceptanceTwoView(cH,xyinter)) {
              int arrayId[10] = {j,k,-1,-1,jView,nextView,-1,-1,intType2};
              double variable[2] = {9999.,tinter2/2};
              if (StoreHit(cH,variable,arrayId,xyinter)){
                for (int jvar=0; jvar<2; jvar++){
                  inters.SetClusterId(arrayId[jvar]);
                  inters.SetViewId(arrayId[jvar+4]);
                }
                ////                inters.SetQuality(1./(0.5*(fStrawCluster[cH][nextView]->GetNHits(k)/2.+fStrawCluster[cH][jView]->GetNHits(j)/2.)));
                inters.SetTrailingTime(variable[1]);
                fChamberHit[cH].push_back(inters);
                inters.Clear();
              }
            } else intType2 = -1;
          }
          inters.Clear();
        } // end cluster loop in 2-view search
        nextView++;
      } // End 2-view search
    } // end cluster loop
    jView++;
  } // End single view search
}

int StrawSegmentAlgorithm::IntType1(Int_t v1){
  if (v1==0) return 20;
  if (v1==1) return 21;
  if (v1==2) return 22;
  if (v1==3) return 23;
  return -1;
}

int StrawSegmentAlgorithm::IntType2(int v1, int v2){
  if (v1==0 && v2==2) return 1; // ux
  if (v1==0 && v2==3) return 3; // uy
  if (v1==1 && v2==2) return 2; // vx
  if (v1==1 && v2==3) return 4; // vy
  if (v1==2 && v2==3) return 0; // xy
  return 5; // uv
}

int StrawSegmentAlgorithm::IntType3(Int_t intType2, Int_t view){
  if (intType2==5 && view==2) return 11; // uv+x
  if (intType2==5 && view==3) return 7;  // uv+y
  if (intType2==2 && view==3) return 13; // vx+y
  if (intType2==1 && view==3) return 14; // ux+y
  return -1;
}

void StrawSegmentAlgorithm::ComputeCoordinate(const int &intType1, const double &fX, double *p){
  switch (intType1){
  case 20: // only u
    *(p+2) = fX;
    break;

  case 21: // only v
    *(p+3) = fX;
    break;

  case 22: // only x
    *p = fX;
    break;

  case 23: // only y
    *(p+1) = fX;
    break;
  };
}

void StrawSegmentAlgorithm::ComputeCoordinate(const int &intType2, const double &fX1, const double &fX2, double *p){
  double p1 = intType2==0 || intType2==5 ? fX1 : fX2;
  double p2 = intType2==0 || intType2==5 ? fX2 : fX1;
  double sq2 = sqrt(2.);

  switch (intType2){
  case 0: *p=p1;          *(p+1)=p2;           *(p+2)=(p1-p2)/sq2; *(p+3)=(p1+p2)/sq2; break; // xy --
  case 1: *p=p1;          *(p+1)=p1-p2*sq2;    *(p+2)=p2;          *(p+3)=p1*sq2-p2;   break; // ux --
  case 2: *p=p1;          *(p+1)=-p1+p2*sq2;   *(p+2)=p1*sq2-p2;   *(p+3)=p2;          break; // vx --
  case 3: *p=p1+p2*sq2;   *(p+1)=p1;           *(p+2)=p2;          *(p+3)=p1*sq2+p2;   break; // uy --
  case 4: *p=-p1+p2*sq2;  *(p+1)=p1;           *(p+2)=-p1*sq2+p2;  *(p+3)=p2;          break; // vy
  case 5: *p=(p1+p2)/sq2; *(p+1)=(-p1+p2)/sq2; *(p+2)=p1;          *(p+3)=p2;          break; // uv
  }
}

int StrawSegmentAlgorithm::IntersectionQuality(int type, double *xyinter, double xcluster, double *qual){
  switch (type){

  case 15: // xyuv: uvx + y -> check on y
  {
    double dcoor[4] = {-9999.,-9999.,-9999.,-9999.};
    if ((*qual=fabs(xyinter[1]-xcluster))>25.) return 0;
    double sq2 = sqrt(2.);
    dcoor[0] = fabs(xyinter[0]-(xyinter[2]+xyinter[3])/sq2);
    dcoor[1] = fabs(xyinter[1]-(-xyinter[2]+xyinter[3])/sq2);
    dcoor[2] = fabs(xyinter[2]-(xyinter[0]-xcluster)/sq2);
    dcoor[3] = fabs(xyinter[3]-(xyinter[0]+xcluster)/sq2);
    double mean(dcoor[0]);
    double dist(dcoor[0]);
    for (Int_t j=1; j<4; j++) {
      mean += dcoor[j];
      if (dcoor[j]>dist) dist = dcoor[j];
    }
    mean /= 4.;
    // double sigm2(0.);
    // for (Int_t j=0; j<4; j++) {
    //   sigm2 += (dcoor[j]-mean)*(dcoor[j]-mean);
    // }
    if (dist>25.) return 0; // Changed from 20 to 25 wrt standard reconstruction
    if (mean>12.) return 0;
  }
  break;

  case 14: // xyu: ux + y
    if ((*qual=fabs(xyinter[1]-xcluster))>20.) return 0;
    break;

  case 13: // xyv: vx + y -> check on y
    if ((*qual=fabs(xyinter[1]-xcluster))>20.) return 0;
    break;

  case 7:  // yuv: uv + y -> check on y
    if ((*qual=fabs(xyinter[1]-xcluster))>20.) return 0;
    break;

  case 11: // xuv: uv + x -> check on x
    if ((*qual=fabs(xyinter[0]-xcluster))>20.) return 0;
    break;

  default:
    return 0;
  }

  return 1;
}

void StrawSegmentAlgorithm::UpdateCoordinate(int type, double *xyinter, double xcluster){
  switch (type){

  case 15: case 14: case 13: case 7: // xyuv, xyu, xyv, yuv
    xyinter[1]=xcluster;
    break;

  case 11:
    xyinter[0]=xcluster;
    break;

  default:
    break;
  }
}

bool StrawSegmentAlgorithm::AcceptanceTwoView(int cH, double *xyinter){
  if (xyinter[0]*xyinter[0]+xyinter[1]*xyinter[1]>=1000*1000) return false;
  if (cH==0 && (xyinter[0]-fXCenter[0])*(xyinter[0]-fXCenter[0])+xyinter[1]*xyinter[1]>=250.*250.) return false;
  if (cH==1 && (xyinter[0]-fXCenter[1])*(xyinter[0]-fXCenter[1])+xyinter[1]*xyinter[1]>=250.*250.) return false;
  if (cH==2 && (xyinter[0]-fXCenter[2])*(xyinter[0]-fXCenter[2])+xyinter[1]*xyinter[1]>=150.*150.) return false;
  if (cH==3 && (xyinter[0]-fXCenter[3])*(xyinter[0]-fXCenter[3])+xyinter[1]*xyinter[1]>=150.*150.) return false;
  //  if (StrawAcceptance(cH,xyinter,12)) return true;
  //  if (StrawAcceptance(cH,xyinter,13)) return true;
  //  if (StrawAcceptance(cH,xyinter,4)) return true;
  //  return false;
  return true;
}

int StrawSegmentAlgorithm::StoreHit(int cH, double *var, int *arrayId, double *xyinter){
  // Intersection type
  int thistype(0);
  for (int j=0; j<4; j++) {
    if (arrayId[j]>-1) thistype++;
  }

  for (int jHit=0; jHit<(int)fChamberHit[cH].size(); jHit++) {

    // Cluster in common with existing hits
    int ncommon(0);
    for (int jCluster=0; jCluster<GetHit(cH,jHit)->GetType(); jCluster++){
      for (int jCluster1=0; jCluster1<4; jCluster1++){
        if (arrayId[jCluster1]==-1) continue;
        if (GetHit(cH,jHit)->GetViewId(jCluster)==arrayId[4+jCluster1] &&
            GetHit(cH,jHit)->GetClusterId(jCluster)==arrayId[jCluster1]) ncommon++;
      }
    }

    // Distance between existing hits
    double oldxyinter[2] = {GetHit(cH,jHit)->GetXcoor(),GetHit(cH,jHit)->GetYcoor()};
    double dist(0);
    for (Int_t j=0; j<2; j++) dist += (xyinter[j]-oldxyinter[j])*(xyinter[j]-oldxyinter[j]);

    // Conditions
    int replace(0);
    if (dist>25.) {
      if (ncommon==0) continue;
      if (ncommon==1) {
        if (thistype==2) {
          if (GetHit(cH,jHit)->GetType()==2) {
            if (StrawAcceptance(cH,xyinter,12)==1 && StrawAcceptance(cH,oldxyinter,12)==0) {
              fChamberHit[cH].erase(fChamberHit[cH].begin()+jHit);
              jHit--;
              continue;
            }
            if (StrawAcceptance(cH,xyinter,12)==0 && StrawAcceptance(cH,oldxyinter,12)==1) return 0;
            if (StrawAcceptance(cH,xyinter,12)==0 && StrawAcceptance(cH,oldxyinter,12)==0) continue;
            if (StrawAcceptance(cH,xyinter,12)==1 && StrawAcceptance(cH,oldxyinter,12)==1) continue;
          }
          if (GetHit(cH,jHit)->GetType()>2) {
            if (StrawAcceptance(cH,xyinter,4)==1) return 0; // if the 2 view hit is in regions where 4 are expected reject the hit if has a straw in common with a > 2 view hit
            if (StrawAcceptance(cH,xyinter,4)==0) { // if the 2 view hit is not in regions where 4 are expected
              if (GetHit(cH,jHit)->GetType()==3) { // if the other hit is 3 view
                if (StrawAcceptance(cH,oldxyinter,13)==1) return 0; // reject the 2 view hit if the other hit is in a 3 view hit region
                else if (StrawAcceptance(cH,oldxyinter,13)==0) {
                  if (StrawAcceptance(cH,xyinter,12)==1) replace = 1; // keep the 2 view and reject the 3 view only if the 2 view is in a 2 view region and the 3 view is not in a 3 view region
                  else return 0;
                } else return 0;
              }
              if (GetHit(cH,jHit)->GetType()==4) { // if the other hit is 4 view
                if (StrawAcceptance(cH,oldxyinter,4)==1) return 0; // reject the 2 view hit if the other hit is in a 4 view hit region
                else if (StrawAcceptance(cH,oldxyinter,4)==0) {
                  if (StrawAcceptance(cH,xyinter,12)==1) replace = 1; // keep the 2 view and reject the 4 view only if the 2 view is in a 2 view region and the 4 view is not in a 4 view region
                  else return 0;
                } else return 0;
              }
            }
          }
        }
        if (thistype>2) {
          if (GetHit(cH,jHit)->GetType()>2) {
            if (thistype==3 && GetHit(cH,jHit)->GetType()==3) {
              if (StrawAcceptance(cH,xyinter,13)==1 && StrawAcceptance(cH,oldxyinter,13)==0) replace = 1;
              if (StrawAcceptance(cH,xyinter,13)==0 && StrawAcceptance(cH,oldxyinter,13)==1) return 0;
              if (StrawAcceptance(cH,xyinter,13)==0 && StrawAcceptance(cH,oldxyinter,13)==0) continue;
              if (StrawAcceptance(cH,xyinter,13)==1 && StrawAcceptance(cH,oldxyinter,13)==1) continue;
            }
            if (thistype==3 && GetHit(cH,jHit)->GetType()==4) {
              if (StrawAcceptance(cH,xyinter,13)==1 && StrawAcceptance(cH,oldxyinter,4)==0) replace = 1;
              if (StrawAcceptance(cH,xyinter,13)==0 && StrawAcceptance(cH,oldxyinter,4)==1) return 0;
              if (StrawAcceptance(cH,xyinter,13)==0 && StrawAcceptance(cH,oldxyinter,4)==0) continue;
              if (StrawAcceptance(cH,xyinter,13)==1 && StrawAcceptance(cH,oldxyinter,4)==1) continue;
            }
            if (thistype==4 && GetHit(cH,jHit)->GetType()==3) {
              if (StrawAcceptance(cH,xyinter,4)==1 && StrawAcceptance(cH,oldxyinter,13)==0) replace = 1;
              if (StrawAcceptance(cH,xyinter,4)==0 && StrawAcceptance(cH,oldxyinter,13)==1) return 0;
              if (StrawAcceptance(cH,xyinter,4)==0 && StrawAcceptance(cH,oldxyinter,13)==0) continue;
              if (StrawAcceptance(cH,xyinter,4)==1 && StrawAcceptance(cH,oldxyinter,13)==1) continue;
            }
            if (thistype==4 && GetHit(cH,jHit)->GetType()==4) continue;
          }
          if (GetHit(cH,jHit)->GetType()==2) {
            if (StrawAcceptance(cH,oldxyinter,4)==1) replace = 1;
            if (StrawAcceptance(cH,oldxyinter,4)==0) {
              if (thistype==3) { // if the other hit is 3 view
                if (StrawAcceptance(cH,xyinter,13)==1) replace = 1; // reject the 2 view hit is the other hit is in a 3 view hit region
                else if (StrawAcceptance(cH,xyinter,13)==0) {
                  if (StrawAcceptance(cH,oldxyinter,12)==1) return 0; // keep the 2 view and reject the 3 view only if the 2 view is in a 2 view region and the 3 view is not in a 3 view region
                  else replace = 1;
                } else replace = 1;
              }
              if (thistype==4) { // if the other hit is 4 view
                if (StrawAcceptance(cH,xyinter,4)==1) replace = 1; // reject the 2 view hit is the other hit is in a 4 view hit region
                else if (StrawAcceptance(cH,xyinter,4)==0) {
                  if (StrawAcceptance(cH,oldxyinter,12)==1) return 0; // keep the 2 view and reject the 4 view only if the 2 view is in a 2 view region and the 4 view is not in a 4 view region
                  else replace = 1;
                } else replace = 1;
              }
            }
          }
        }
      }

    }
    if (replace) {
      fChamberHit[cH].erase(fChamberHit[cH].begin()+jHit);
      jHit--;
      continue;
    }
    if (dist>25.) continue; // keep only those few cases with hits at 3 or 4 views with 2 views in commons

    // Here only if dist <= 25.
    if (thistype>2 && GetHit(cH,jHit)->GetType()>2) {
      if (thistype==GetHit(cH,jHit)->GetType()) { // Same quality hits close each other
        if (var[0]<GetHit(cH,jHit)->GetQuality()) replace = 1;
        else return 0;
      } else if (thistype<GetHit(cH,jHit)->GetType()) return 0; // a bit naive. Use geometrical constrains to improve ?
      else replace = 1;
    }
    if (thistype>2 &&  GetHit(cH,jHit)->GetType()==2) {
      replace = 1;
    }
    if (thistype==2 &&  GetHit(cH,jHit)->GetType()>2) {
      return 0;
    }
    if (thistype==2 &&  GetHit(cH,jHit)->GetType()==2) {
      if (StrawAcceptance(cH,xyinter,12)==1 && StrawAcceptance(cH,oldxyinter,12)==0) replace = 1;
      if (StrawAcceptance(cH,xyinter,12)==0 && StrawAcceptance(cH,oldxyinter,12)==1) return 0;
      if (StrawAcceptance(cH,xyinter,12)==1 && StrawAcceptance(cH,oldxyinter,12)==1) return 0;
      if (StrawAcceptance(cH,xyinter,12)==0 && StrawAcceptance(cH,oldxyinter,12)==0) return 0;
    }

    if (replace) {
      fChamberHit[cH].erase(fChamberHit[cH].begin()+jHit);
      jHit--;
    }

  }

  return 1; // Store as a new hit
}

int StrawSegmentAlgorithm::StrawAcceptance(int N, double *coor, int Zone){
  double sq2 = sqrt(2.);
  double invsq2 = 1./sq2;
  double a[4] = {invsq2,invsq2,1,0};
  double b[4] = {-invsq2,invsq2,0,1};
  double c[4] = {invsq2,-invsq2,0,1};
  double d[4] = {invsq2,invsq2,1,0};
  int viewflag[4] = {0,0,0,0};
  for (Int_t jView=0; jView<4; jView++) {
    double posView = a[jView]*coor[0]+b[jView]*coor[1];
    double posAlongStraw = c[jView]*coor[0]+d[jView]*coor[1];
    if (((posView>fHoleChamberMax[N][jView] && posView<0.5*fViewPlaneTransverseSize) || (posView<fHoleChamberMin[N][jView] && posView>-0.5*fViewPlaneTransverseSize)) && fabs(posAlongStraw)<0.5*2100.) viewflag[jView] = 1;
  }
  int Vu = viewflag[0];
  int Vv = viewflag[1];
  int Vx = viewflag[2];
  int Vy = viewflag[3];

  // Zones
  switch (Zone)
    {
    case 1:  // At least 1 view
      if (Vx || Vy || Vu || Vv)                      return 1;
      return 0;
    case 2:  // At least 2 views
      if ( (Vx && Vy) || (Vx && Vu) || (Vx && Vv) ||
           (Vy && Vu) || (Vy && Vv) || (Vu && Vv) )  return 1;
      return 0;
    case 3:  // At least 3 views
      if ( (Vx && Vy && Vu) || (Vx && Vy && Vv) ||
           (Vx && Vu && Vv) || (Vy && Vu && Vv))     return 1;
      return 0;
    case 4:  // Four views only
      if (Vx && Vy && Vu && Vv)                      return 1;
      return 0;
    case 11: // One view only
      if ( ((Vx && !Vy && !Vu && !Vv) ||
            (!Vx && Vy && !Vu && !Vv) ||
            (!Vx && !Vy && Vu && !Vv) ||
            (!Vx && !Vy && !Vu && Vv)))              return 1;
      return 0;
    case 12: // Two views only
      if ( ((Vx && Vy && !Vu && !Vv) ||
            (Vx && !Vy && Vu && !Vv) ||
            (Vx && !Vy && !Vu && Vv) ||
            (!Vx && Vy && Vu && !Vv) ||
            (!Vx && Vy && !Vu && Vv) ||
            (!Vx && !Vy && Vu && Vv)))               return 1;
      return 0;
    case 13: // Three views only
      if ( ((Vx && Vy && Vu && !Vv) ||
            (Vx && Vy && !Vu && Vv) ||
            (Vx && !Vy && Vu && Vv) ||
            (!Vx && Vy && Vu && Vv)))                return 1;
      return 0;
    default:
      return 0;
    }

  return 0;
}

bool StrawSegmentAlgorithm::ReconstructSegments(TVector3 *vertex, double timeGTK){
  bool isSegments = false;

  int returnVal = 999999.;

  InitSegmentVariables();
  double sq2 = sqrt(2.);
  fVertex[0] = (vertex->X()-vertex->Y())/sq2;
  fVertex[1] = (vertex->X()+vertex->Y())/sq2;
  fVertex[2] = vertex->X();
  fVertex[3] = vertex->Y();
  fVertex[4] = vertex->Z();
  fVertex[5] = timeGTK;

  // One view segments using chambers 01 (uvxy-z planes) and/or chambers 23 (y-z plane only)
  ////  OneViewSegment(1,-1,-1);
  ////  OneViewSegment(3,-1,-1);
  OneViewSegment(1);
  OneViewSegment(3);

  // y-view segment in 23 starting from not y-view hits
  ////  MultipleViewSegment(3,-1,-1,-9999.,-9999.,-9999.,-9999.);

  if (fFlag==1 || fFlag==2) {
    if (fMinTime[0]<35.-1.4*fMinChi2[0]) { // Segment in chamber 12
      fMinIB = fMinChi2[0];
    }
    if ((fMinTime[0]<35.-1.4*fMinChi2[0]) || (fMinTime[1]<40.-5.7*fMinChi2[1])) { // Segment in chamber 34
      //      fMinIB = fMinChi2[1];
      fMinIB = fabs(fMinTime[1]);
    }
  }

  //  return fMinIB;
  if ((fMinTime[0]<35.-1.4*fMinChi2[0]) || (fMinTime[1]<40.-5.7*fMinChi2[1])) returnVal = 0.;
  if(returnVal<0.5) isSegments = true;
  return isSegments;
}

void StrawSegmentAlgorithm::Clear()
{
  fNNoTrackHits = 0;
  fStrawCandidateHit->Clear();
  fStrawNoCandidateHit->Clear();
  for(int jc=0; jc<4; jc++){
    for(int jv=0; jv<4; jv++){
      fStrawCluster[jc][jv]->Clear();
    };
  };
  for (int jc=0; jc<4; jc++)  fChamberHit[jc].clear();
}

void StrawSegmentAlgorithm::InitSegmentVariables(){
  fMinIB = 9999999.;
  fMinChi2[0] = 999999.;
  fMinDist[0] = 999999.;
  fMinTime[0] = 999999.;
  fMinChi2[1] = 999999.;
  fMinDist[1] = 999999.;
  fMinTime[1] = 999999.;
  fMinSlopeX[0] = -999999.;
  fMinSlopeY[0] = -999999.;
  fMinSlopeX[1] = -999999.;
  fMinSlopeY[1] = -999999.;
  fPExp = 999999.;
  for (int js=0; js<2; js++) {
    for (int jc=0; jc<2; jc++) {
      fMinXCoor[jc][js] = 999999.;
      fMinYCoor[jc][js] = 999999.;
    }
  }
  fMinYCh1 = -9999999.;
  fMinLKrDT = 999999.;
  fMinLKrD = 999999.;
  fMinLKrDX = 999999.;
  fMinLKrDY = 999999.;
  fSaveTime = 999999.;
}

void StrawSegmentAlgorithm::InitGeometryVariables(){
  fViewPlaneTransverseSize = 0.;
  for(int i=0; i<4; i++){
    fChamberZPosition[i] = 0.;
    for(int j=0; j<4; j++){
      fHoleChamberMin[i][j] = 0.;
      fHoleChamberMax[i][j] = 0.;
      for(int k=0; k<4; k++){
	for(int l=0; l<122; l++){
	  fStrawHitLocalX[i][j][k][l] = 0.;
	  fStrawHitGlobalZ[i][j][k][l] = 0.;
	};
      };
    };
  };
}
