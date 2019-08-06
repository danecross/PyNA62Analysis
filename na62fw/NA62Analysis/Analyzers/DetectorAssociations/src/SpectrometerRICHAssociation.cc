/// \class SpectrometerRICHAssociation
/// \Brief
/// Calculates particle identification likelihoods for spectrometer tracks using RICH hits
/// \EndBrief
/// \Detailed
/// The routine extrapolates each spectrometer track to the RICH, predicts the
/// ring centre and the radii for the electron, muon, pion, and kaon
/// hypotheses, and compares the number of predicted and observed hits
/// around the ring radii in a likelihood calculation.
/// It returns 5 double numbers, between 0 and 1, expressing the likelihoods
/// of the track to be a background (i.e. an infinitely heavy particle below Cherenkov threshld),
/// electron, muon, pion, and kaon.
/// The most likely hypothesis is also returned, and the success flag is returned
/// (it is false in case there are no RICH hits in the event).
/// The most likely hypothesis can be 0, 1, 2, 3, 4, as
/// defined in ToolsLib/include/SpectrometerRICHAssociationOutput.hh.
/// Several likelihoods can be equal to 1 (this happens for non-electron tracks below Cherenkov threshold).
/// In this case, the most likely hypothesis is set to kRICHHypothesisMultiple (numerically equal to 99).
/// In case of success flag set to false, the most likely hypothesis returned
/// is kRICHHypothesisBackground (numerically 0).
/// The output for each track is written into a SpectrometerRICHAssociationOutput container,
/// and can be accessed in the following way:
/// \code
/// std::vector<SpectrometerRICHAssociationOutput> SpecRICH =
///  *(std::vector<SpectrometerRICHAssociationOutput>*)GetOutput("SpectrometerRICHAssociation.Output");
/// \endcode
/// Most of the information contained in this container is propagated into DownstreamTrack.
/// Matematics is described in: U.Muller et al., NIMA 343 (1994) pp 279-283.
/// \author Jurgen Engelfried (jurgen@ifisica.uaslp.mx); transported to SELEX code in ~1994;
/// transported to NA62 code in September 2015; integrated with NA62Analysis by Evgueni Goudzovski (2016)
/// \todo Interface for MC electron ring parameters (they are hard-coded now)
/// \todo Allow for an external reference time to be supplied (NARKD-851)
/// \EndDetailed

#include <stdlib.h>
#include <fstream>
#include "SpectrometerRICHAssociation.hh"
#include "Event.hh"
#include "BaseAnalysis.hh"
#include "TObjArray.h"
#include "TObjString.h"
#include "NA62Exceptions.hh"
#include "RICHParameters.hh"
#include "NA62ConditionsService.hh"
#include "GeometricAcceptance.hh"

#include "TRecoRICHEvent.hh"
#include "TRecoSpectrometerEvent.hh"
#include "TRecoCHODEvent.hh"
#include "Stream.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerRICHAssociation::SpectrometerRICHAssociation(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerRICHAssociation") {
  if (!GetIsTree()) return;

  RequestTree("RICH",         new TRecoRICHEvent);
  RequestTree("Spectrometer", new TRecoSpectrometerEvent);
  RequestTree("CHOD",         new TRecoCHODEvent);
  RequestL0Data();

  AddParam("CalibrationMode", &CalibrationMode, true);
  AddParam("XMoveJura",       &XMoveJura,       127.0);

  fRevision = "";

  // Timing parameters
  fTimeDiffRICHCHODSigma  =  3.0; // resolution is 0.4 ns
  fTimeDiffRICHStrawSigma = 10.0; // resolution is 1.2 ns

  // RICH parameters
  fFocalLength       = 17020.0;// [mm]
  fMirrorPanelRadius = 1520.; // Approx total mirror size
  fPMTDiskRadius     = 300.;
  fSpacialResolution = 5.5; // single hit resolution

  fMirrorZ  = GeometricAcceptance::GetInstance()->GetZRICHMirror();
  fChodZPos = 0.5*(GeometricAcceptance::GetInstance()->GetZCHODVPlane() +
		   GeometricAcceptance::GetInstance()->GetZCHODHPlane());

  MirrorX = 0.; // General Alignment of Mirror Center of Curvature
  MirrorY = 0.; // after recon alignment
  //  MirrorCenterOfCurvature_Jura_Lab[0]=1.76*m;
  //  MirrorCenterOfCurvature_Jura_Lab[1]=0.*m;
  //  MirrorCenterOfCurvature_Saleve_Lab[0]=-1.39*m;
  //  MirrorCenterOfCurvature_Saleve_Lab[1]=0.*m;
  //  XMove[0] = 127.; // Center of Curvatures of Jura/Salve
  //  XMove[0] = 0.; // Center of Curvatures of Jura/Salve
  XMove[0] = XMoveJura; // Center of Curvatures of Jura/Salve
  XMove[1] = 177.; // used for ring correction on PMT disks
  YMove[0] = 0.0;
  YMove[1] = 0.0;

  fDebug = false;
}

void SpectrometerRICHAssociation::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void SpectrometerRICHAssociation::InitHist() {

  /////////////////////////////////////////
  // Initialization of the mirror positions

  NA62ConditionsService::GetInstance()->SetCurrentRunID(GetRunID());
  for (Int_t i=0; i<20; i++) { // sequential numbering used in this class
    fMirrorNumber[i]      = RICHParameters::GetInstance()->GetMirrorNumber(i);
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
  // The centres are evaluated assuming full mirrors, not half-mirrors, so x is close to 0.

  fMirrorPosition[18][0]=fMirrorPosition[1][0]; // x of Mirror 17
  fMirrorPosition[18][1]=0.5*(fMirrorPosition[7][1]+fMirrorPosition[8][1]);
  fMirrorPosition[19][0]=fMirrorPosition[16][0]; // x of Mirror 22
  fMirrorPosition[19][1]=0.5*(fMirrorPosition[9][1]+fMirrorPosition[10][1]);

  // Default mirror alignment parameters (overwritten with those read from DB for each run).
  // The ExtraOffset parameters are not used.
  for (Int_t i=0; i<25; i++) fMirrorAlignment[i][0] = fMirrorAlignment[i][1] = 0.0;
  fExtraOffsetX[0] = fExtraOffsetY[0] = 0.0;
  fExtraOffsetX[1] = fExtraOffsetY[1] = 0.0;

  /////////////////////////////////////////////////////////

  if (!GetIsTree()) return;
  if (CalibrationMode) {
    BookHisto(new TH1F("ExtrapolChodX","Track Extrapolated to CHOD",1000,-500.,500.));
    BookHisto(new TH1F("ExtrapolChodY","Track Extrapolated to CHOD",1000,-500.,500.));
    BookHisto(new TH1F("TimeRICHCHOD","Time Diff RICH - CHOD",1000,-50.,50.));
    BookHisto(new TH1F("TimeRICHStraw","Time Diff RICH - Straw",1000,-50.,50.));
    BookHisto(new TH1F("TimeRICHStrawWithChod","Time Diff RICH - Straw with CHOD match",1000,-50.,50.));
    BookHisto(new TH2F("TimeRICHStrawChi2WithChod","Time Diff RICH - Straw vs Track Chi2 with CHOD match",1000,-50.,50.,100,0.,40.));
    BookHisto(new TH2F("TimeRICHStrawChi2","Time Diff RICH - Straw vs Track Chi2",1000,-50.,50.,100,0.,40.));
    BookHisto(new TH1F("TimeRICH","RICH Hit Time",1000,-200.,200.));
    BookHisto(new TH1F("TimeTrigger","Trigger Time",1000,-200.,200.));
    BookHisto(new TH1F("TimeTriggerC","Trigger Time Control",1000,-200.,200.));
    BookHisto(new TH1F("TimeRICHL0TP","Time Diff RICH Hit - L0TPTime",1000,-50.,50.));
    BookHisto(new TH1F("TimeRICHL0TPC","Time Diff RICH Hit - L0TPTime Control Trigger",1000,-50.,50.));
    BookHisto(new TH1F("NObs","Number of Observed Hits",300,-0.5,299.5));
    BookHisto(new TH1F("NHitsAssigned","Number Hits Assigned to MPHyp",50,-0.5,49.5));
    BookHisto(new TH1F("N0","Number of Hits on beta=1",100,0.,30.));
  }
}

void SpectrometerRICHAssociation::StartOfRunUser() {

  if (!GetIsTree()) return;

  ////////////////////////////////////////////////////////////////////
  // Set a wider RICH-CHOD matching window for pre-v1.0.6 MC samples,
  // to account for a CHOD reco bug (NARKD-1044).
  // The residual bug NARKD-1094 does not affect this, see NARKD-1095.

  fRevision = GetStreamInfo()->GetRecoInfo().GetRevision();
  if (GetWithMC()) {
    if (fRevision=="v0.11.1" || fRevision=="v0.11.2" || fRevision=="v0.11.3" ||
	fRevision=="v1.0.0" || fRevision=="v1.0.1" ||
	fRevision=="v1.0.2" || fRevision=="v1.0.4")
    fTimeDiffRICHCHODSigma = 5.0;
  }

  Int_t RunNumber = GetRunID();

  // Read mirror angular alignment constants from via the database interface.
  // These constants [mm] are added to the expected ring centre coordinates.
  for (Int_t i=0; i<25; i++) {
    for (Int_t j=0; j<2; j++) {
      fMirrorAlignment[i][j] = RICHParameters::GetInstance()->GetMirrorAlignmentConstant(RunNumber, i, j);
    }
  }
}

void SpectrometerRICHAssociation::StartOfBurstUser() {
  if (!GetIsTree()) return;

  ////////////////////////////////////////////////////////////
  // Update the parameters of the electron ring for each burst

  Int_t  RunNumber = GetRunID();
  time_t BurstTime = GetEventHeader()->GetBurstTime();
  fElectronRingRadius = RICHParameters::GetInstance()->GetElectronRingRadius(RunNumber, BurstTime);
  fElectronRingNhits  = RICHParameters::GetInstance()->GetElectronRingNHits (RunNumber, BurstTime);

  // Apply empirical scale factors for MC (Evgueni, 1/8/19).
  // This is to be tuned more carefully with the RICHElectronRadius analyzer.
  // Number of photons: 13.5 for run 6610, extrapolated by Slava comparing RICHElectronRadius vs Jurgen's private tool.
  if (GetWithMC()) {
    fElectronRingRadius *= 0.999757; // this leads to 189.6 mm for run 6610
    fElectronRingNhits  *= 1.047;    // this leads to 13.5 for run 6610
  }

  fRefIndex = 1.0/(cos(atan(fElectronRingRadius/fFocalLength)));

  // Printout with -v2 or higher verbosity
  cout << user() <<
    Form("Run=%d, BurTime=%ld, Radius=%6.2fmm, RefIndex=%5.2fppm, Nhits=%5.2f",
	 RunNumber, BurstTime, fElectronRingRadius, (fRefIndex-1.0)*1.0e6, fElectronRingNhits) << endl;
}

void SpectrometerRICHAssociation::PrintMirrorConstants() {
  if (!TestLevel(Verbosity::kUserNormal)) return;
  cout << "RICH mirror positions & alignment" << endl;
  for (Int_t im=0; im<20; im++) {
    printf("%2d: Mirror %2d position %8.2f %8.2f\n",
	   im, fMirrorNumber[im], fMirrorPosition[im][0], fMirrorPosition[im][1]);
  }
  for (Int_t im=1; im<25; im++) {
    printf("Mirror %2d alignmnent %8.2f %8.2f\n",
	   im, fMirrorAlignment[im][0], fMirrorAlignment[im][1]);
  }
  printf("ExtraOffsets: Jura %8.2f %8.2f, Saleve %8.2f %8.2f\n",
	 fExtraOffsetX[0], fExtraOffsetY[0], fExtraOffsetX[1], fExtraOffsetY[1]);
}

Double_t SpectrometerRICHAssociation::RICHCrossing
(Double_t xc1, Double_t yc1, Double_t r1, TVector2 c2, Double_t r2) {
  // Calculates areal fraction of disk 2, intersecting disk 1
  Double_t d = sqrt((xc1-c2.X())*(xc1-c2.X())+(yc1-c2.Y())*(yc1-c2.Y()));
  if (d>=(r1+r2)) return 0.0;
  if (d<=(r1-r2)) return 1.0;
  Double_t t1h = acos((d*d+r1*r1-r2*r2)/(2.*d*r1));
  Double_t t2h = acos((d*d-r1*r1+r2*r2)/(2.*d*r2));
  return (r1*r1*(t1h-sin(t1h)*cos(t1h)) + r2*r2*(t2h-sin(t2h)*cos(t2h)))/(TMath::Pi()*r2*r2);
}

void SpectrometerRICHAssociation::ComputeLikelihoods
(Int_t iTrack, SpectrometerRICHAssociationOutput* Result) {
  //
  // Subroutine to calculate Likelihoods for all tracks pointing to the RICH.
  // Uses Spectrometer tracks, and calculates likelihoods (normalized to 1)
  // for every track being an electron, muon, pion, or simply noise.
  //
  // Matematics taken from paper: U.Muller et al., NIMA 343 (1994) pp 279-283
  // transported to SELEX code by Jurgen Engelfried, ~1994
  // transported to NA62 code by Jurgen Engelfried, September 2015
  //

  Double_t Masses[MaxHypos] = {0.0,MEL,MMU,MPI,MKCH}; // #0 is never used, "background" is an infinitely heavy particle
  Double_t RICH_Likelihood[MaxHypos];
  TVector2 PredictedCenter, TrackOnMirror;

  // Parameters for this algorithm
  Double_t SearchRegionHW = 30.;
  Double_t SearchRegionBackHW = 50.;
  Double_t sigmin = 0.1;

  TRecoSpectrometerEvent *SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoRICHEvent *RICHEvent = GetEvent<TRecoRICHEvent>();
  TRecoCHODEvent *CHODEvent = GetEvent<TRecoCHODEvent>();

  // fValid is defined as "there is at least one RICH hits in the event".
  // Is is propagated by DownstreamTrackBuilder into DownstreamTrack::fRICHAssociationSuccessful.
  // If fValid==false, then the predicted ring centre is not computed (though it can!).

  Result->SetValid(false);
  Result->SetMostLikelyHypothesis(kRICHHypothesisBackground);
  for (Int_t ihyp=0; ihyp<MaxHypos; ihyp++) RICH_Likelihood[ihyp] = -1.0;
  Result->SetTrackID(iTrack);
  Int_t NRichHits = RICHEvent->GetNHits();
  if (!NRichHits) return;

  // Get the straw track information
  TRecoSpectrometerCandidate* Scand =
    static_cast<TRecoSpectrometerCandidate*>(SpectrometerEvent->GetCandidate(iTrack));
  Double_t xSlope    = Scand->GetSlopeXAfterMagnet();
  Double_t ySlope    = Scand->GetSlopeYAfterMagnet();
  Double_t txpos     = Scand->GetPositionAfterMagnet().X();
  Double_t typos     = Scand->GetPositionAfterMagnet().Y();
  Double_t tzpos     = Scand->GetPositionAfterMagnet().Z();
  Double_t TMom      = Scand->GetMomentum();
  Double_t TrackChi2 = Scand->GetChi2();
  Double_t StrawTrackTime = Scand->GetLeadingTime();
  Double_t TrackTime = StrawTrackTime;

  Bool_t HaveCHODTrackTime = false;
  Double_t XonChod = (txpos + xSlope*(fChodZPos-tzpos));
  Double_t YonChod = (typos + ySlope*(fChodZPos-tzpos));
  Int_t NCCand = CHODEvent->GetNCandidates();
  for (Int_t iCand=0; iCand<NCCand; iCand++) {
    TRecoCHODCandidate* Ccand = static_cast<TRecoCHODCandidate*>(CHODEvent->GetCandidate(iCand));
    Double_t xPosC = Ccand->GetHitPosition().X();
    Double_t yPosC = Ccand->GetHitPosition().Y();
    if (CalibrationMode) {
      FillHisto("ExtrapolChodX", XonChod-xPosC);
      FillHisto("ExtrapolChodY", YonChod-yPosC);
    }
    if (fabs(XonChod-xPosC)<80. && fabs(YonChod-yPosC)<80.) { // associate
      TrackTime = Ccand->GetTime();
      HaveCHODTrackTime = true;
    }
  }

  if (CalibrationMode) {

    L0TPData* L0data = GetL0Data();
    Double_t TriggerTime = L0data->GetReferenceFineTime()*TdcCalib;
    UInt_t TriggerType = L0data->GetDataType();
    Bool_t ControlTrigger = TriggerType & 0x10;
    if (ControlTrigger) FillHisto("TimeTriggerC",TriggerTime);
    else FillHisto("TimeTrigger",TriggerTime);

    Double_t GroupTimeCut = 10.;
    Int_t NTimeGroups = 0;
    Double_t GroupTime[10] = {-10000.,-10000.,-10000.,-10000.,-10000.,-10000.,-10000.,-10000.,-10000.};
    Int_t NMembers[10] = {0,0,0,0,0,0,0,0,0,0};
    Double_t SumTime[10] = {0.,0.,0.,0.,0.,0.,0.,0.,0.,0.};
    for (Int_t iHit=0; iHit<NRichHits; ++iHit) {
      // order hits in time and find groups
      TRecoRICHHit* RecoHit = static_cast<TRecoRICHHit*>(RICHEvent->GetHit(iHit));
      Int_t ntg=0;
      if (RecoHit->GetOrSuperCellID()==0) {
	Double_t hTime = RecoHit->GetTime();
        FillHisto("TimeRICH",hTime);
        if (ControlTrigger) FillHisto("TimeRICHL0TPC",hTime-TriggerTime);
        else FillHisto("TimeRICHL0TP",hTime-TriggerTime);
	Bool_t GroupExists = false;
	for (Int_t itg=0;itg<NTimeGroups;itg++) {
	  if (abs(hTime-GroupTime[itg])<GroupTimeCut) {
	    GroupExists = true;
	    ntg = itg;
	  }
	}
	if (!GroupExists) {
	  GroupTime[NTimeGroups] = hTime;
	  ntg = NTimeGroups;
	  NTimeGroups++;
	}
	NMembers[ntg]++;
	SumTime[ntg] = SumTime[ntg] + hTime;
	GroupTime[ntg] = SumTime[ntg] / NMembers[ntg];
      }
    }
    //  printf("\n\nFound %d Timegroups, ",NTimeGroups);
    for (Int_t itg=0;itg<NTimeGroups;itg++) {
      // printf(" %d hits with %f\n",NMembers[itg],GroupTime[itg]);
      if (HaveCHODTrackTime) {
        FillHisto("TimeRICHCHOD",GroupTime[itg]-TrackTime);
        FillHisto("TimeRICHStrawWithChod",GroupTime[itg]-StrawTrackTime);
        FillHisto("TimeRICHStrawChi2WithChod",GroupTime[itg]-StrawTrackTime,TrackChi2);
      }
      else {
        FillHisto("TimeRICHStraw",GroupTime[itg]-StrawTrackTime);
        FillHisto("TimeRICHStrawChi2",GroupTime[itg]-StrawTrackTime,TrackChi2);
      }
    }
  }

  // Calculate where the track hits the mirror array, and which mirror was hit
  TrackOnMirror.Set(txpos + (fMirrorZ-tzpos)*xSlope, typos + (fMirrorZ-tzpos)*ySlope);
  Result->SetTrackPosOnMirror(TrackOnMirror);
  Double_t dm = sqrt(pow(MirrorX-TrackOnMirror.X(), 2) +
		     pow(MirrorY-TrackOnMirror.Y(), 2));
  Result->SetTrackMirrorID(0);
  Double_t dmin = 1.0e12;
  for (Int_t im=0; im<20; im++) {
    Double_t d =
      pow(fMirrorPosition[im][0]-TrackOnMirror.X(), 2) +
      pow(fMirrorPosition[im][1]-TrackOnMirror.Y(), 2);
    if (d<dmin) {
      dmin = d;
      Result->SetTrackMirrorID(fMirrorNumber[im]);
    }
  }
  Result->SetDistToMirrorCentre(sqrt(dmin));

  // Predict the Center of the Ring, taking into account the alignment of the
  //  actual hit mirror.  Also correct for fExtraOffset due to average alignment
  //  of the two different side with mirror #12 and #14 in 2015.
  //    Double_t xc = fFocalLength*xSlope - fExtraOffsetX[fMirrorSide[Result->TrackOnMirror]] + MirrorAlignment[Result->TrackOnMirror][0];
  //    Double_t yc = fFocalLength*ySlope - fExtraOffsetY[fMirrorSide[Result->TrackOnMirror]] + MirrorAlignment[Result->TrackOnMirror][1];
  Double_t xc = fFocalLength*xSlope + fMirrorAlignment[Result->GetTrackMirrorID()][0];
  Double_t yc = fFocalLength*ySlope + fMirrorAlignment[Result->GetTrackMirrorID()][1];
  PredictedCenter.Set(xc,yc);
  Result->SetPredictedCentre(PredictedCenter);

  // Calculate the expected ring radius for the e,mu,pi,K hypotheses
  Double_t PredictedRadius[MaxHypos];
  for (Int_t ihyp=1; ihyp<MaxHypos; ihyp++) {
    PredictedRadius[ihyp] = 0.0;
    if (TMom > Masses[ihyp]/sqrt(fRefIndex*fRefIndex-1.0)) {
      Double_t Beta = TMom / sqrt(TMom*TMom + Masses[ihyp]*Masses[ihyp]);
      Double_t ThetaC = acos(1.0 / fRefIndex / Beta);
      PredictedRadius[ihyp] = fFocalLength * ThetaC;
    }
    Result->SetPredictedRadius(ihyp, PredictedRadius[ihyp]);
  }

  if (fDebug) printf("Predicted Center, Radius, Momentum: %f %f %f %f\n",PredictedCenter.X(),PredictedCenter.Y(),PredictedRadius[1],TMom);

  // Calculate the fractions of the disk on the mirror array and for the
  // ring on the PMT array. Needed if the tracks are in the limits of
  // the RICH geometrical acceptance. Lowers number of expected hits.
  Double_t Ric_Fract[MaxHypos][2];
  Double_t MirrorFrac[MaxHypos][2];
  Double_t PMTFrac[MaxHypos][2];
  Double_t side[2];
  for (Int_t ihyp=1; ihyp<MaxHypos; ihyp++) {
    for (Int_t ipmt=0; ipmt<2; ipmt++) { // Saleve/Jura
      Ric_Fract[ihyp][ipmt] = 0.0;
      MirrorFrac[ihyp][ipmt] = 0.;
      PMTFrac[ihyp][ipmt] = 0.;
      side[ipmt] = 0.;
    }
    if (PredictedRadius[ihyp]>0.) {
      // How much of the disk is on the mirrors?
      // Calculate fraction of intersecting areas of one disk (Cherenkov light)
      // with two half disks (representing the mirrors)
      if (dm < (PredictedRadius[ihyp]+fMirrorPanelRadius)) {
	// first: how much falls on the jura/saleve side of the mirrors?
	if (dm < fMirrorPanelRadius-PredictedRadius[ihyp]) { // Full on mirrors
	  if ((TrackOnMirror.X()-PredictedRadius[ihyp])>MirrorX) { // full on Jura
	    side[0] = 1.;
	    side[1] = 0.;
	  }
	  else if (TrackOnMirror.X()+PredictedRadius[ihyp]<MirrorX) { // full on Saleve
	    side[0] = 0.;
	    side[1] = 1.;
	  }
	  else {
	    Double_t th = acos((TrackOnMirror.X()-MirrorX)/PredictedRadius[ihyp]);
	    if (TrackOnMirror.X()<MirrorX) {
	      side[0] = PredictedRadius[ihyp]*PredictedRadius[ihyp]*(th-sin(th)*cos(th))/(TMath::Pi()*PredictedRadius[ihyp]*PredictedRadius[ihyp]);
	      side[1] = 1. - side[0];
	    }
	    else {
	      side[1] = PredictedRadius[ihyp]*PredictedRadius[ihyp]*(th-sin(th)*cos(th))/(TMath::Pi()*PredictedRadius[ihyp]*PredictedRadius[ihyp]);
	      side[0] = 1. - side[1];
	    } // left/right
	  }
	}
	else { // only partly on mirror array
	  if ((TrackOnMirror.X()-PredictedRadius[ihyp])>MirrorX) { // on Jura side
	    side[0] = 1.;
	    side[1] = 0.;
	  }
	  else if (TrackOnMirror.X()+PredictedRadius[ihyp]<MirrorX) { // on Saleve side
	    side[0] = 0.;
	    side[1] = 1.;
	  }
	  else { // complicated case: in middle high/low: Just take the ratio of x's for now
	    if ((TrackOnMirror.X()-MirrorX)>0) {
	      side[0] = (TrackOnMirror.X()-MirrorX)/PredictedRadius[ihyp];
	      side[1] = 1. - side[0];
	    }
	    else {
	      side[1] = (MirrorX-TrackOnMirror.X())/PredictedRadius[ihyp];
	      side[0] = 1. - side[1];
	    }
	  }
	}
	if (fDebug) printf("ihyp %d dm %f side %f %f\n",ihyp,dm,side[0],side[1]);
	Double_t frac = RICHCrossing(MirrorX,MirrorY,fMirrorPanelRadius,TrackOnMirror,PredictedRadius[ihyp]);
	MirrorFrac[ihyp][0] = frac*side[0];
	MirrorFrac[ihyp][1] = frac*side[1];
	// if (fDebug) printf("ihyp %d t1h %f t2h %f fracs %f %f\n",ihyp,t1h,t2h,MirrorFrac[ihyp][0],MirrorFrac[ihyp][1]);
	// How much of the ring falls onto the PMT array?
	// Calculate fraction of the cirumference (ring) falling onto a disk
	// (PMT array).  The calculation for the angles is the same as above.
	// Only relevant here is t2.  But careful:  We have different disks, centers.
	Result->SetPredictedCentreJura(999999.,999999.);
	Result->SetPredictedCentreSaleve(999999.,999999.);
	for (Int_t ipmt=0; ipmt<2; ipmt++) {
	  if (MirrorFrac[ihyp][ipmt] > 0.0) { // Center of Ring on PMT array
	    Double_t xcen = PredictedCenter.X() + XMove[ipmt];
	    Double_t ycen = PredictedCenter.Y() + YMove[ipmt];
	    Double_t dp = sqrt(xcen*xcen + ycen*ycen);
	    if (ipmt==0) Result->SetPredictedCentreJura(xcen,ycen);
	    else Result->SetPredictedCentreSaleve(xcen,ycen);
	    if (dp < (fPMTDiskRadius-PredictedRadius[ihyp])) PMTFrac[ihyp][ipmt] = 1.0;
	    else {
	      if (dp < (fPMTDiskRadius+PredictedRadius[ihyp])) {
		PMTFrac[ihyp][ipmt] = acos((dp*dp-fPMTDiskRadius*fPMTDiskRadius+PredictedRadius[ihyp]*PredictedRadius[ihyp])/(2.*dp*PredictedRadius[ihyp]))/TMath::Pi();
	      } // some ring on PMT array
	    } // not full coverage
	  } // some light from mirrors to this pmt array
	} // loop of pmt arrays
      } // some light on mirrors
    } // above threshold
    Ric_Fract[ihyp][0] = MirrorFrac[ihyp][0]*PMTFrac[ihyp][0];
    Ric_Fract[ihyp][1] = MirrorFrac[ihyp][1]*PMTFrac[ihyp][1];
    Result->SetMirrorFraction(ihyp, 0, MirrorFrac[ihyp][0]);
    Result->SetMirrorFraction(ihyp, 1, MirrorFrac[ihyp][1]);
    Result->SetPMTFraction(ihyp, 0, PMTFrac[ihyp][0]);
    Result->SetPMTFraction(ihyp, 1, PMTFrac[ihyp][1]);
  } // loop over hypotheses

  if (fDebug) {
    for (Int_t ihyp=0; ihyp<MaxHypos; ihyp++) {
      printf ("Hypo %d, Ric_Fract %f %f\n",ihyp,Ric_Fract[ihyp][0],Ric_Fract[ihyp][1]);
      printf ("Hypo %d, MirrorFract %f %f\n",ihyp,MirrorFrac[ihyp][0],MirrorFrac[ihyp][1]);
      printf ("Hypo %d, PMTFract %f %f\n",ihyp,PMTFrac[ihyp][0],PMTFrac[ihyp][1]);
    }
  }

  // Now we have all predictions done. We will now prepare the likelihoods.

  // Int_t NHitTrack[MaxHypos];
  Double_t RMax = PredictedRadius[1]; // Electron;

  // Define interesting region
  Double_t reg1 = 0.;
  for (Int_t ihyp=1;ihyp<MaxHypos;ihyp++) {
    // NHitTrack[ihyp] = 0;
    if (PredictedRadius[ihyp]>0.) reg1 = PredictedRadius[ihyp] - SearchRegionHW;
  }
  if (reg1<0) reg1 = 0.;
  Double_t reg2 = RMax + SearchRegionHW;

  // Calculate area of search region
  Double_t frac1 = RICHCrossing(0.,0.,fPMTDiskRadius,PredictedCenter,reg1);
  Double_t frac2 = RICHCrossing(0.,0.,fPMTDiskRadius,PredictedCenter,reg2);
  Double_t Areas = TMath::Pi()*(reg2*reg2*frac2 - reg1*reg1*frac1);

  // Calculate area of background region
  Double_t frac3 = RICHCrossing(0.,0.,fPMTDiskRadius,PredictedCenter,reg2+SearchRegionBackHW);
  Double_t Areab = TMath::Pi()*((reg2+SearchRegionBackHW)*(reg2+SearchRegionBackHW)*frac3 - reg2*reg2*frac2);

  // Find hits in background region, as well as in signal region
  Int_t NObsBack = 0, NObs = 0;
  Int_t Back_Point[500];
  Double_t Dist[500], xPosFit[500], yPosFit[500];
  if (fDebug) printf("RICH NHits %d\n",NRichHits);
  Int_t NRICHHits=0, NRICHHitsInTime=0;
  for (Int_t iHit=0; iHit<NRichHits; iHit++) {
    TRecoRICHHit* RecoHit = static_cast<TRecoRICHHit*>(RICHEvent->GetHit(iHit));
    if (RecoHit->GetOrSuperCellID()==0) {
      //Int_t iside = 0; // Jura
      //if (ChannelID>1023) iside = 1; // Saleve
      Double_t HitTime = RecoHit->GetTime();
      if (fDebug) printf("HitTime %f, TrackTime %f, %f %f\n",HitTime, TrackTime,
			 HitTime-TrackTime, fTimeDiffRICHCHODSigma);
      NRICHHits++;
      if (( HaveCHODTrackTime && fabs(HitTime-TrackTime)<fTimeDiffRICHCHODSigma) ||
	  (!HaveCHODTrackTime && fabs(HitTime-TrackTime)<fTimeDiffRICHStrawSigma)) {
	NRICHHitsInTime++;
	// xPosFit[NObs] = RecoHit->GetFitPosition().X() - fExtraOffsetX[iside];
	// yPosFit[NObs] = RecoHit->GetFitPosition().Y() - fExtraOffsetY[iside];
	// NB: one has to use GetFitPosition() corrected for mirror inclination, not GetPosition()!
	xPosFit[NObs] = RecoHit->GetFitPosition().X();
	yPosFit[NObs] = RecoHit->GetFitPosition().Y();
	Double_t d = sqrt((xPosFit[NObs]-PredictedCenter.X())*(xPosFit[NObs]-PredictedCenter.X()) + (yPosFit[NObs]-PredictedCenter.Y())*(yPosFit[NObs]-PredictedCenter.Y()));
	if (fDebug) printf("x %f, y %f, xc %f, yc %f, d %f %f %f, reg1 %f, reg2 %f, RegionBack %f\n",xPosFit[NObs],yPosFit[NObs],PredictedCenter.X(),PredictedCenter.Y(),d,xPosFit[NObs]-PredictedCenter.X(),yPosFit[NObs]-PredictedCenter.Y(),reg1,reg2,reg2+SearchRegionBackHW);
	if (d>reg2 && d<(reg2 + SearchRegionBackHW)) {
	  NObsBack++;
	  if (fDebug) printf("NBack %d\n",NObsBack);
	}
	if (d>reg1 && d<reg2) {
	  Dist[NObs] = d;
	  Back_Point[NObs] = iHit;
	  NObs++;
	  if (fDebug) printf("NObs %d\n",NObs);
	  if (CalibrationMode) FillHisto("NObs",NObs);
	}
      } // Hit time and track time coincide
    } // PMT Hits
  } // RICH hits
  if (fDebug) printf("NObs %d NObsBack %d\n",NObs, NObsBack);
  Result->SetTrackTimeForAssociation(TrackTime);
  Result->SetNInTimeHits(NRICHHitsInTime);
  Result->SetNOutOfTimeHits(NRICHHits-NRICHHitsInTime);
  Result->SetNObservedHits(NObs);
  Result->SetNBackgroundHits(NObsBack);

  // Calculate background density
  Double_t BackGroundDens = NObsBack/Areab;
  Double_t ExpectedBackground = BackGroundDens * Areas;

  Double_t rl[MaxHypos];
  // Calculate background likelihood first
  rl[0] = log(BackGroundDens)*NObs;
  Double_t rlmax = rl[0];
  Int_t RICH_Id = 0;

  // Calculate the likelihoods for all real hypotheses
  Double_t ExpectedSignal[MaxHypos]; // these are the expected numbers of hits in each hypothesis
  Int_t HitsAssigned[MaxHypos][500];
  Int_t NHitsAssigned[MaxHypos];
  ExpectedSignal[0] = ExpectedBackground; // expected number of hits in the background hypothesis
  NHitsAssigned[0]  = 0;                  // no hits are assigned to the background hypothesis
  for (Int_t ihyp=1; ihyp<MaxHypos; ihyp++) { // loop over all hypotheses except background
    ExpectedSignal[ihyp] = 0.0;
    NHitsAssigned[ihyp] = 0;
    if (PredictedRadius[ihyp] > 0.) {
      Double_t fgauss1 =
	TMath::Freq((reg2-PredictedRadius[ihyp])/fSpacialResolution) -
	TMath::Freq((reg1-PredictedRadius[ihyp])/fSpacialResolution);
      Double_t phratio = (PredictedRadius[ihyp]/RMax)*(PredictedRadius[ihyp]/RMax);
      Double_t ringd = phratio/(TMath::Pi()*2.);
      Double_t SignalDensity = fElectronRingNhits*ringd;
      ExpectedSignal[ihyp] = fElectronRingNhits*(Ric_Fract[ihyp][0]+Ric_Fract[ihyp][1])*phratio;
      if (fDebug) printf("ihyp %d, ExpectedSignal %f\n",ihyp,ExpectedSignal[ihyp]);
      rl[ihyp] = 0.;
      for (Int_t n=0;n<NObs;n++) {
	rl[ihyp] = rl[ihyp] + log(((SignalDensity/Dist[n])*TMath::Gaus(Dist[n],PredictedRadius[ihyp],fSpacialResolution,false))/fgauss1 + BackGroundDens);
	if (fDebug) printf("R %f, res %f, dist %f, sigmin %f, Gauss %f\n",PredictedRadius[ihyp],fSpacialResolution,Dist[n],sigmin,TMath::Gaus(Dist[n],PredictedRadius[ihyp],fSpacialResolution,false));
	if (TMath::Gaus(Dist[n],PredictedRadius[ihyp],fSpacialResolution,false)>sigmin) {
	  HitsAssigned[ihyp][NHitsAssigned[ihyp]] = n;
	  NHitsAssigned[ihyp]++;
	} // assign hit
      } // loop over observed hits
      rl[ihyp] = rl[ihyp] - ExpectedSignal[ihyp];
      if (rl[ihyp]>rlmax) {
	rlmax = rl[ihyp];
	RICH_Id = ihyp;
      }
    } // above threshold
    else {
      rl[ihyp] = rl[0];
      ExpectedSignal[ihyp] = 0.0;
    } // below threshold
    Result->SetNExpectedSignalHits(ihyp, ExpectedSignal[ihyp]);
  } // loop over hypotheses

  // Normalize likelihoods and calculate a "fit" radius
  // Do ih = 0, max_ric_hypos ! Once more to normalize and calculate Rfit
  for (Int_t ihyp=0; ihyp<MaxHypos; ihyp++) {
    Double_t rnorm = TMath::Max(rl[ihyp]-rlmax,-85.);
    RICH_Likelihood[ihyp] = exp(rnorm);
    Result->SetLikelihood(ihyp, RICH_Likelihood[ihyp]);
    Result->SetNHitsAssigned(ihyp, NHitsAssigned[ihyp]);
  }

  Double_t RICH_Rad_Fit = 999999.;
  Double_t RICH_Rad_Fit_Err = 999999.;
  Double_t RICH_Center_Fit[2] = {999999.,999999.};
  Double_t RICH_Center_Fit_Err[2] = {999999.,999999.};
  Double_t RICH_Rad_Fit_Chi2 = 999999.;

  // Set the most likely hypothesis
  Double_t rmaxl = 0.0;
  Int_t NEquallyLikely = 0;
  for (Int_t ihyp=0; ihyp<MaxHypos; ihyp++) {
    if (RICH_Likelihood[ihyp]>rmaxl) {
      rmaxl = RICH_Likelihood[ihyp];
      Result->SetMostLikelyHypothesis(ihyp);
    }
    if (RICH_Likelihood[ihyp]==1.0) NEquallyLikely++;
  }
  // In case of several most likely hypotheses (with L=1), as happens for non-electrons
  // below threshold, set the "multiple hypothesis" state
  if (NEquallyLikely>1) Result->SetMostLikelyHypothesis(kRICHHypothesisMultiple);

  // Compute the ring time in each hypothesis by averaging the times of assigned hits
  for (Int_t ihyp=1; ihyp<MaxHypos; ihyp++) { // running from 1 as hypothesis #0 is background
    Double_t RingTime = 0.0;
    for (Int_t i=0; i<NHitsAssigned[ihyp]; i++) {
      Int_t iHit = Back_Point[HitsAssigned[ihyp][i]];
      TRecoRICHHit* RecoHit = static_cast<TRecoRICHHit*>(RICHEvent->GetHit(iHit));
      RingTime += RecoHit->GetTime();
      Result->AssignHit(ihyp, RecoHit);
    }
    if (NHitsAssigned[ihyp]>0) RingTime /= (1.0*NHitsAssigned[ihyp]); else RingTime = -999.0;
    Result->SetRingTime(ihyp, RingTime);
  }

  if (fDebug) printf("RICH_Id %d NHits %d\n", RICH_Id, NHitsAssigned[RICH_Id]);

  if (RICH_Id>0) {
    if (NHitsAssigned[RICH_Id]<4) { // Make a simple ring calculation: no ring (x,y), (dx,dy,dR)
      Double_t R0 = 0.;
      Double_t R1 = 0.;
      Double_t R2 = 0.;
      for (Int_t i=0; i<NHitsAssigned[RICH_Id]; i++) {
	R0 += 1.0;
	R1 += Dist[HitsAssigned[RICH_Id][i]];
	R2 += Dist[HitsAssigned[RICH_Id][i]]*Dist[HitsAssigned[RICH_Id][i]];
      }
      R0 = TMath::Max(1.0, R0);
      RICH_Rad_Fit = R1/R0;
      RICH_Rad_Fit_Chi2 = (R2-R1*R1/R0)/TMath::Max(1.,R0-1.)/(1.6129/4.)/(1.6129/4.);
    }
    else { // there are >=4 hits: perform the CooperRingFit
      Double_t xPosHere[100], yPosHere[100];
      for (Int_t i=0; i<NHitsAssigned[RICH_Id]; i++) {
        xPosHere[i] = xPosFit[HitsAssigned[RICH_Id][i]];
        yPosHere[i] = yPosFit[HitsAssigned[RICH_Id][i]];
      }
      RICH_Rad_Fit_Chi2 = CooperRingFit
	(NHitsAssigned[RICH_Id], xPosHere, yPosHere,
	 RICH_Center_Fit, RICH_Center_Fit_Err, RICH_Rad_Fit, RICH_Rad_Fit_Err);
    }

    if (CalibrationMode) {
      FillHisto("NHitsAssigned", NHitsAssigned[RICH_Id]);
      Double_t N0 = NHitsAssigned[RICH_Id]*
	(sin(PredictedRadius[1]/fFocalLength)/
	 sin(PredictedRadius[RICH_Id]/fFocalLength))*
	(sin(PredictedRadius[1]/fFocalLength)/sin(PredictedRadius[RICH_Id]/fFocalLength));
      FillHisto("N0", N0);
    }
  }
  else { // RICH_Id = 0
    RICH_Likelihood[0] = 1.0;
    Result->SetLikelihood(0, RICH_Likelihood[0]);
  }

  Result->SetRingRadius(RICH_Rad_Fit);
  Result->SetRingRadiusError(RICH_Rad_Fit_Err); // defined for >=4 hits only
  Result->SetRingCentre(RICH_Center_Fit[0], RICH_Center_Fit[1]); // defined for >=4 hits only
  Result->SetRingCentreError(RICH_Center_Fit_Err[0], RICH_Center_Fit_Err[1]); // defined for >=4 hits only
  Result->SetRingFitChi2(RICH_Rad_Fit_Chi2);
  Result->SetValid(true); // means "there is at least one RICH hit in the event"
}

void SpectrometerRICHAssociation::Process(Int_t) {
  SetOutputState("Output", kOValid);
  fContainer.clear();

  if (!GetIsTree()) return;
  TRecoSpectrometerEvent *SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  SpectrometerRICHAssociationOutput ret;
  for (Int_t iTrack=0; iTrack<SpectrometerEvent->GetNCandidates(); iTrack++) {
    ret.Clear();
    ComputeLikelihoods(iTrack, &ret);
    fContainer.push_back(ret);
  }
}

void SpectrometerRICHAssociation::EndOfJobUser() {
  SaveAllPlots();
}

Double_t SpectrometerRICHAssociation::CooperRingFit
(Int_t NPoints, Double_t* XPos, Double_t* YPos, Double_t* Center,
 Double_t *Sig_Center, Double_t &Radius, Double_t &Sig_Radius) {
  //
  // Input:  NPoints with XPos and YPos coordinates.
  // Output: Return Chi2 of "Fit".
  //         also Center coordinates and Ring Radius with errors.
  //
  //  Routine is not a real "Fit".  Minimizing the square of the distance
  //  makes this a linear problem, and the "Fit" is just a linear regression.
  //  Original idea from J.F. Crawford, NIM 211 (1983) pp 223-225.
  //  Extension by P.S. Cooper to include also Chi2 and errors on the
  //  parameters (P.S.Cooper, SELEX HNote ??)
  //  Programmed by Jurgen Engelfried, September 29, 2015
  //

  if (NPoints<4) return 1000000.;

  //Int_t RunID   = GetEventHeader()->GetRunID();
  //Int_t BurstID = GetEventHeader()->GetBurstID();
  //Int_t EventID = GetEventHeader()->GetEventNumber();
  //printf(" Found Event: Run %d, Burst %d, Event %d\n\n",RunID,BurstID,EventID);

  // first get average positions in x and y
  Double_t xSum = 0.;
  Double_t ySum = 0.;
  for (int ip = 0; ip < NPoints; ip++) {
    xSum += XPos[ip];
    ySum += YPos[ip];
    //printf("In CRF: i %d, x %.2f, y %.2f\n",ip,XPos[ip],YPos[ip]);
  }
  Double_t xAvg = xSum / NPoints;
  Double_t yAvg = ySum / NPoints;

  // Now calculate the relevant sums for the linear regression
  Double_t Suu = 0., Svv = 0, Suv = 0.;
  Double_t Suuu = 0., Svvv = 0., Suuv = 0., Suvv = 0.;
  for (int ip = 0; ip < NPoints; ip++) {
    Double_t ui = XPos[ip] - xAvg;
    Double_t vi = YPos[ip] - yAvg;
    Suu = Suu + ui*ui;
    Svv = Svv + vi*vi;
    Suv = Suv + ui*vi;
    Suuu = Suuu + ui*ui*ui;
    Svvv = Svvv + vi*vi*vi;
    Suuv = Suuv + ui*ui*vi;
    Suvv = Suvv + vi*vi*ui;
  }

  // Normalize the sums
  Double_t cc1 = 0.5*(Suuu + Suvv);
  Double_t cc2 = 0.5*(Svvv + Suuv);
  Double_t uc = ((Suv*cc2 - Svv*cc1) / (Suv*Suv - Suu*Svv));
  Double_t vc = ((Suv*cc1 - Suu*cc2) / (Suv*Suv - Suu*Svv));
  Double_t alpha = uc*uc + vc*vc + (Suu + Svv) / NPoints;

  // Compute radius and center
  Center[0] = uc + xAvg;
  Center[1] = vc + yAvg;
  Radius = sqrt(alpha);

  // Calculate Chi2
  Double_t chiSq = 0.;
  Double_t sigma = 4.5; // = 2.0*9.0/sqrt(16) 18mm being the size of the PMTs
  Double_t weight = 1./sigma/sigma;
  Double_t sx=0., sy=0., sx2=0., sy2=0., sxy=0.;
  for (Int_t ip = 0; ip < NPoints; ip++) {
    Double_t dX = (XPos[ip] - Center[0])/Radius;
    Double_t dY = (YPos[ip] - Center[1])/Radius;
    sx  += weight*dX;
    sx2 += weight*dX*dX;
    sy  += weight*dY;
    sy2 += weight*dY*dY;
    sxy += weight*dX*dY;
    chiSq += ((1. - dX*dX - dY*dY)*(1. - dX*dX - dY*dY));
  }
  chiSq = chiSq*pow((Radius/2./sigma),2.);

  // Calculate uncertainties
  Double_t s1  = weight*NPoints;
  Double_t det = s1*(sx2*sy2-sxy*sxy) - sx*(sx*sy2-sxy*sy) + sy*(sx*sxy-sx2*sy);
  Sig_Radius = sqrt((sx2*sy2-sxy*sxy)/det);
  Sig_Center[0] = sqrt((s1*sy2-sy*sy)/det);
  Sig_Center[1] = sqrt((s1*sx2-sx*sx)/det);

  //printf("Centre %10.2f+/-%10.2f %10.2f+/-%10.2f, Radius %10.2f+/-%10.2f Rchi2 %10.3f\n",
  //  Center[0],Sig_Center[0],Center[1],Sig_Center[1],Radius,Sig_Radius,chiSq/(NPoints-3));
  return chiSq;
}
