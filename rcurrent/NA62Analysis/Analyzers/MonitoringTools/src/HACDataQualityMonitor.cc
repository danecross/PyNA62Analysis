#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <TChain.h>
#include "HACDataQualityMonitor.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "BaseAnalysis.hh"
#include "TLine.h"
#include "NA62Global.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "ConfigSettings.hh"
#include "K3piWithMissingPionInfo.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

//Loop over all pair of straw candidates and check if any pair forms a good vertex inside the fiducial region.
//The tracks from such a pair should have the same ktag candidate to which a GTK candidate corresponds.
//Compute the missing mass using the pair of two tracks and gtk candidate and check if it is consistent with pi+ mass
//If yes, propagat the "missing" track from fiducial region to front face of HASC.
//Check if projection is in acceptance. If it is, check for HASC signal.

HACDataQualityMonitor::HACDataQualityMonitor(Core::BaseAnalysis *ba) : Analyzer(ba, "HACDataQualityMonitor") {
  Configuration::ConfigSettings::SetNoSkipBadBurst(true); // do not skip bad bursts

  RequestTree("Cedar", new TRecoCedarEvent);
  RequestTree("CHOD", new TRecoCHODEvent);
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent);
  RequestTree("HAC", new TRecoHACEvent);
  RequestTree("IRC", new TRecoIRCEvent);
  RequestTree("LAV", new TRecoLAVEvent);
  RequestTree("MUV3", new TRecoMUV3Event);
  RequestTree("NewCHOD", new TRecoNewCHODEvent);
  RequestTree("SAC", new TRecoSACEvent);
  RequestTree("Spectrometer", new TRecoSpectrometerEvent);

  RequestL0Data();
  RequestL0SpecialTrigger();
  RequestBeamSpecialTrigger();

  fOutPDFFileName = fAnalyzerName + ".pdf";
  fTimeWindow = 20.;

  AddParam("ArgonionCountsMin", &fArgonionCountsMin, 1.e5);
  AddParam("NSelectedTriggersMin", &fNSelectedTriggersMin, 1.e3);
  AddParam("EfficiencyThreshold", &fEfficiencyThreshold, 0.02);

  GeometricAcceptance *geom = GeometricAcceptance::GetInstance();
  fZdetectors[0] = geom->GetZStraw(0);
  fDetectorsName[0] = "STRAW0";
  fDetectorsID[0] = kSpectrometer;
  fZdetectors[1] = geom->GetZStraw(1);
  fDetectorsName[1] = "STRAW1";
  fDetectorsID[1] = kNewCHOD;
  fZdetectors[2] = geom->GetZStraw(2);
  fDetectorsName[2] = "STRAW2";
  fDetectorsID[2] = kCHOD;
  fZdetectors[3] = geom->GetZStraw(3);
  fDetectorsName[3] = "STRAW3";
  fDetectorsID[3] = kLKr;
  fZdetectors[4] = geom->GetZNewCHOD();
  fDetectorsName[4] = "NewCHOD";
  fDetectorsID[4] = kMUV1;
  fZdetectors[5] = geom->GetZCHODVPlane();
  fDetectorsName[5] = "CHOD";
  fDetectorsID[5] = kMUV2;
  fZdetectors[6] = geom->GetZLKr();
  fDetectorsName[6] = "LKr";
  fDetectorsID[6] = kMUV3;
  fZdetectors[7] = geom->GetZMUV1();
  fDetectorsName[7] = "MUV1";
  fZdetectors[8] = geom->GetZMUV2();
  fDetectorsName[8] = "MUV2";
  fZdetectors[9] = geom->GetZMUV3();
  fDetectorsName[9] = "MUV3";

  fXstartHAC = -43.6;
  fXstopHAC = -23.6;
  fYstartHAC = -10.0;
  fYstopHAC = 10.0;

  fZmag[0] = 19634.5;
  fDmag[0] = 130.0;
  fBmag[0] = 0.6928;

  fZmag[1] = 24820.0;
  fDmag[1] = 200.0;
  fBmag[1] = 1.7012;

  fCanvas = nullptr;
  fHExpected_back = nullptr;
  fHExpected_middle = nullptr;
  fHExpected_front = nullptr;

  fHMatched_back = nullptr;
  fHMatched_front = nullptr;
  fHMatched_middle = nullptr;

  fHEfficiency_back = nullptr;
  fHEfficiency_middle = nullptr;
  fHEfficiency_front = nullptr;

  fHArgonionCountsVsBurstID = nullptr;
  fHEfficiencyVsBurstID = nullptr;
  fHExpectedVsBurstID = nullptr;
  fHNSelectedTriggersVsBurstID = nullptr;
  fHNTriggersVsBurstID = nullptr;
}

HACDataQualityMonitor::~HACDataQualityMonitor() {
}

void HACDataQualityMonitor::InitOutput() {
}

void HACDataQualityMonitor::InitHist() {
  Double_t XMin = -4999.5;
  Double_t XMax = 5000.5;
  Int_t fNBinsX = 500;
  Double_t YMin = -4999.5;
  Double_t YMax = 5000.5;
  Int_t fNBinsY = 500;
  Int_t fMaxNBursts = 5000;
  fReadingData = GetIsTree();

  if (fReadingData) {
    std::cout << user_normal() << "Reading reconstructed data" << std::endl;

    //for (Int_t i = 0; i < 10; i++) {
    //  BookHisto(new TH2F(Form("MissingTrack/hPredictedPositionAt_%s", fDetectorsName[i]),
    //    Form("hMissingTrack_PredictedPositionAt_%s", fDetectorsName[i]), 1000, -500., 500., 1000, -500., 500.));
    //}
    //BookHisto(new TH1D("MissingTrack/hMissingMass_DsAcc", "hMissingMass_DsAcc", 200, 0.01, 0.03));
    //BookHisto(new TH2D("MissingTrack/hMissingMassVsP_DsAcc", "hMissingMassVsP_DsAcc", 150, 0., 75., 200, 0.01, 0.03));
    BookHisto(new TH2D("MissingTrack/hPosAtHASCMag", "hPosAtHASCMag", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    //BookHisto(new TH2D("MissingTrack/hZexitVsP", "hZexitVsP", 150, 0., 75., 500, 250000, 255000));
    //Efficiency histograms
    BookHisto(new TH1D("Efficiency/hChargeMatchedCandidate", "hChargeMatchedCandidate", 1000, 0., 1000.));
    BookHisto(new TH1D("Efficiency/hDeltaTcandTrack", "hDeltaTcandTrack", 400, -10., 10.));
    BookHisto(new TH1D("Efficiency/hPmiss_outside_acc", "hPmiss_outside_acc", 200, 0., 100.));

    BookHisto(new TH2D("Efficiency/hExpected_front", "hExpected_front", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto(new TH2D("Efficiency/hExpected_middle", "hExpected_middle", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto(new TH2D("Efficiency/hExpected_back", "hExpected_back", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));

    BookHisto(new TH2D("Efficiency/hMatched_front", "hMatched_front", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto(new TH2D("Efficiency/hMatched_middle", "hMatched_middle", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto(new TH2D("Efficiency/hMatched_back", "hMatched_back", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto(new TH1D("Efficiency/hMatchedVsBurstID", "hMatchedVsBurstID", fMaxNBursts, -0.5, fMaxNBursts - 0.5));
    BookHisto(new TH1D("Efficiency/hIsInAcceptanceHASC", "hIsInAcceptanceHASC", 8, 0, 8));

    BookHisto(new TH1D("Efficiency/hMatched_Pion_Momentum", "hMatched_Pion_Momentum", 150, 0., 75.));
    BookHisto(new TH1D("Efficiency/hUnmatched_Pion_Momentum", "hUnmatched_Pion_Momentum", 150, 0., 75.));
    BookHisto(new TH1D("Efficiency/hMatchedPion_MMiss2", "hMatchedPion_MMiss2", 200, 0.0169, 0.0225));

    BookHisto("Efficiency/hEfficiencyVsBurstID", new TGraphErrors());
    fHEfficiencyVsBurstID = (TGraphErrors*) fHisto.GetTGraph("Efficiency/hEfficiencyVsBurstID");
    fHEfficiencyVsBurstID->SetName("hEfficiencyVsBurstID");
    fHEfficiencyVsBurstID->Set(0);

    BookHisto("Efficiency/hExpectedVsBurstID", new TGraphErrors());
    fHExpectedVsBurstID = (TGraphErrors*) fHisto.GetTGraph("Efficiency/hExpectedVsBurstID");
    fHExpectedVsBurstID->SetName("hExpectedVsBurstID");
    fHExpectedVsBurstID->Set(0);

    BookHisto("Efficiency/hArgonionCountsVsBurstID", new TGraphErrors());
    fHArgonionCountsVsBurstID = (TGraphErrors*) fHisto.GetTGraph("Efficiency/hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->SetName("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->Set(0);

    BookHisto("Efficiency/hNTriggersVsBurstID", new TGraphErrors());
    fHNTriggersVsBurstID = (TGraphErrors*) fHisto.GetTGraph("Efficiency/hNTriggersVsBurstID");
    fHNTriggersVsBurstID->SetName("hNTriggersVsBurstID");
    fHNTriggersVsBurstID->Set(0);

    BookHisto("Efficiency/hNSelectedTriggersVsBurstID", new TGraphErrors());
    fHNSelectedTriggersVsBurstID = (TGraphErrors*) fHisto.GetTGraph("Efficiency/hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->SetName("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->Set(0);
  } else {
    std::cout << user_normal() << "Reading my own output" << std::endl;

    fHExpected_back = (TH2D*) RequestHistogram(fAnalyzerName, "Efficiency/hExpected_back", true);
    fHExpected_middle = (TH2D*) RequestHistogram(fAnalyzerName, "Efficiency/hExpected_middle", true);
    fHExpected_front = (TH2D*) RequestHistogram(fAnalyzerName, "Efficiency/hExpected_front", true);
    fHMatched_back = (TH2D*) RequestHistogram(fAnalyzerName, "Efficiency/hMatched_back", true);
    fHMatched_middle = (TH2D*) RequestHistogram(fAnalyzerName, "Efficiency/hMatched_middle", true);
    fHMatched_front = (TH2D*) RequestHistogram(fAnalyzerName, "Efficiency/hMatched_front", true);
    fHTiming = (TH1D*) RequestHistogram(fAnalyzerName, "Efficiency/hDeltaTcandTrack", true);
    fHCharge = (TH1D*) RequestHistogram(fAnalyzerName, "Efficiency/hChargeMatchedCandidate",true);

    if (!fHExpected_back || !fHExpected_back->InheritsFrom("TH1")){
      std::cout<< user_normal() << "Asked to read my own input but cannot find it"<<std::endl;
      return;
    }

    fHEfficiencyVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "Efficiency/hEfficiencyVsBurstID", true);
    fHExpectedVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "Efficiency/hExpectedVsBurstID", true);
    fHArgonionCountsVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "Efficiency/hArgonionCountsVsBurstID", true);
    fHNTriggersVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "Efficiency/hNTriggersVsBurstID", true);
    fHNSelectedTriggersVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "Efficiency/hNSelectedTriggersVsBurstID", true);

    BookHisto(new TH2D("Efficiency/hEfficiency_back", "hEfficiency_back",
      fHExpected_back->GetNbinsX(), fHExpected_back->GetXaxis()->GetXmin(), fHExpected_back->GetXaxis()->GetXmax(),
      fHExpected_back->GetNbinsY(), fHExpected_back->GetYaxis()->GetXmin(), fHExpected_back->GetYaxis()->GetXmax()));
    BookHisto(new TH2D("Efficiency/hEfficiency_middle", "hEfficiency_middle",
      fHExpected_back->GetNbinsX(), fHExpected_back->GetXaxis()->GetXmin(), fHExpected_back->GetXaxis()->GetXmax(),
      fHExpected_back->GetNbinsY(), fHExpected_back->GetYaxis()->GetXmin(), fHExpected_back->GetYaxis()->GetXmax()));
    BookHisto(new TH2D("Efficiency/hEfficiency_front", "hEfficiency_front",
      fHExpected_back->GetNbinsX(), fHExpected_back->GetXaxis()->GetXmin(), fHExpected_back->GetXaxis()->GetXmax(),
      fHExpected_back->GetNbinsY(), fHExpected_back->GetYaxis()->GetXmin(), fHExpected_back->GetYaxis()->GetXmax()));

    fHEfficiency_back = (TH2D*) fHisto.GetTH1("Efficiency/hEfficiency_back");
    fHEfficiency_middle = (TH2D*) fHisto.GetTH1("Efficiency/hEfficiency_middle");
    fHEfficiency_front = (TH2D*) fHisto.GetTH1("Efficiency/hEfficiency_front");
  }
}

void HACDataQualityMonitor::ProcessSpecialTriggerUser(int /*iEvent*/, unsigned int triggerType) {
  if (triggerType != 0x23) return; // only EOB
  fArgonionCounts = GetBeamSpecialTrigger()->GetCountsARGONION() / 1.e9;
}

void HACDataQualityMonitor::Process(int iEvent) {
  if (!fReadingData) return;
  if (fMCSimple.fStatus == MCSimple::kMissing) {
    printIncompleteMCWarning(iEvent);
    return;
  }
  fHACEvent = GetEvent<TRecoHACEvent>();

  fNTriggers++;
  Int_t L0DataType = GetL0Data()->GetDataType();
  Int_t L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t PhysicsData = L0DataType & 0x1;
  Bool_t CTRLTrigger = L0DataType & 0x10;
  //Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0x1)) || CTRLTrigger; //MASK0 only + CTRL
  Bool_t TriggerOK = (PhysicsData && (L0TriggerWord & 0xFF)) || CTRLTrigger; //ALL MASKS + CTRL
  if (!TriggerOK) return; // process control triggers and selected MASKS only
  fNSelectedTriggers++;

  fMissingPion = *GetOutput<std::vector<K3piWithMissingPionInfo>>("K3piWithMissingPionSelection.MissingPions");
  for (UInt_t iMiss = 0; iMiss < fMissingPion.size(); iMiss++)
    MatchMissingPion(fMissingPion[iMiss]);
}

void HACDataQualityMonitor::MatchMissingPion(const K3piWithMissingPionInfo &missingPion) {
  fNExpectedPerBurst++;
  //popagate "missing" track at front of HASC Magnet
  Double_t thetaxafter = -999.;
  TVector3 posmiss = 0.1 * missingPion.GetPairKaonVertex();
  TVector3 pmommiss = 0.001*missingPion.GetMissingPionMomentum().Vect();
  TVector3 posAtMagnet = Propagate(posmiss, pmommiss, 1, fZmag[1], 1, thetaxafter);
  FillHisto("MissingTrack/hPosAtHASCMag", posAtMagnet.X() * 10, posAtMagnet.Y()*10.);

  pmommiss.SetZ(sqrt((pmommiss.Mag2() - pmommiss.Y() * pmommiss.Y())/(1. + pow(thetaxafter, 2))));
  pmommiss.SetX(pmommiss.Z() * thetaxafter);
  //propagate from front of magnet to front face of hasc. If track is not in magnet acceptance continue normal propagation
  TVector3 posAtHASC_front;
  TVector3 posAtHASC_middle;
  TVector3 posAtHASC_back;
  //propagate at various hasc planes.
  Int_t flagIsInMag = fabs(posAtMagnet.X()) > 42. || fabs(posAtMagnet.Y()) > 20. ? 0 : 1;
  posAtHASC_front = Propagate(posAtMagnet, pmommiss, 1, fZstartHAC, flagIsInMag, thetaxafter);
  posAtHASC_middle = Propagate(posAtMagnet, pmommiss, 1, fZstartHAC + 65., flagIsInMag, thetaxafter);
  posAtHASC_back = Propagate(posAtMagnet, pmommiss, 1, fZstartHAC + 130.0, flagIsInMag , thetaxafter);

  FillHisto("Efficiency/hExpected_front", posAtHASC_front.X() * 10., posAtHASC_front.Y() * 10.);
  FillHisto("Efficiency/hExpected_middle", posAtHASC_middle.X() * 10., posAtHASC_middle.Y() * 10.);
  FillHisto("Efficiency/hExpected_back", posAtHASC_back.X() * 10., posAtHASC_back.Y() * 10.);

  if (fHACEvent->GetNHits() < 1){
    FillHisto("Efficiency/hUnmatched_Pion_Momentum", 0.001*missingPion.GetMissingPionMomentum().Vect().Mag());
    return;
  }
  Double_t timeRef = missingPion.GetKaonTime();
  Double_t mindt = 9999.9999;
  Double_t dt;
  Int_t idMatch = -1;
  for (Int_t iHit = 0; iHit < fHACEvent->GetNHits(); iHit++){
    TRecoHACHit *hit = static_cast<TRecoHACHit*>(fHACEvent->GetHit(iHit));
    dt = hit->GetTime() - timeRef;
    if (fabs(dt) < fabs(mindt)){
      mindt = dt;
      idMatch = iHit;
    }
  }
  FillHisto("Efficiency/hDeltaTcandTrack", mindt);
  if (fabs(mindt) > 5.){
    FillHisto("Efficiency/hUnmatched_Pion_Momentum", 0.001*missingPion.GetMissingPionMomentum().Vect().Mag());
    return;
  }
  FillHisto("Efficiency/hMatched_Pion_Momentum", 0.001*missingPion.GetMissingPionMomentum().Vect().Mag());
  FillHisto("Efficiency/hMatchedPion_MMiss2", (1.e-6)*missingPion.GetMissingPionMomentum().M2());
  Double_t chargeMatched = ComputeCandidateCharge(idMatch);
  FillHisto("Efficiency/hChargeMatchedCandidate", chargeMatched);
  FillHisto("Efficiency/hMatched_front", posAtHASC_front.X() * 10., posAtHASC_front.Y() * 10.);
  FillHisto("Efficiency/hMatched_middle", posAtHASC_middle.X() * 10., posAtHASC_middle.Y() * 10.);
  FillHisto("Efficiency/hMatched_back", posAtHASC_back.X() * 10., posAtHASC_back.Y() * 10.);
  fNMatchedPerBurst++;
}

Double_t HACDataQualityMonitor::ComputeCandidateCharge(const Int_t idHitMatch) {
  Double_t refTime = (static_cast<TRecoHACHit*> (fHACEvent->GetHit(idHitMatch)))->GetTime();
  Double_t charge = 0.;
  for (Int_t iHit = 0; iHit < fHACEvent->GetNHits(); iHit++){
    if (iHit == idHitMatch)
      continue;
    TRecoHACHit *hit = static_cast<TRecoHACHit*>(fHACEvent->GetHit(iHit));
    Double_t dt = hit->GetTime() - refTime;
    if (fabs(dt) > 5.)
      continue;
    charge += hit->GetChargeModuleSection();
  }

  return charge;
}


Int_t HACDataQualityMonitor::IsInAcceptanceDownstream(TVector3 pmom, TVector3 vertex, Int_t charge) {
  TVector3 position;
  Double_t thetaxafter = 0.;

  for (Int_t i = 0; i < 7; i++) {
    if (fDetectorsID[i] == kSpectrometer) {
      for (Int_t ichamber = 0; ichamber < 4; ichamber++) {
        position = Propagate(vertex, pmom, charge, 0.1 * fZdetectors[ichamber], 1, thetaxafter);
        Bool_t isDetector = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10, position.Y()*10, kSpectrometer, ichamber);
        FillHisto(Form("MissingTrack/hPredictedPositionAt_%s", fDetectorsName[ichamber]), position.X()*10., position.Y()*10.);
        if (isDetector)
          return 1;
      }
    } else {
      position = Propagate(vertex, pmom, charge, 0.1 * fZdetectors[i + 3], 1, thetaxafter);
      Bool_t isDetector = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10, position.Y()*10, fDetectorsID[i]);
      FillHisto(Form("MissingTrack/hPredictedPositionAt_%s", fDetectorsName[3 + i]), position.X()*10., position.Y()*10.);
      if (isDetector)
        return 1;
    }
  }
  return 0;
}

//Method to propagate charged particle with given momentum along beam line (by Giuseppe).
//Takes into consideration the STRAW Magnet and The magnet before HAC.

TVector3 HACDataQualityMonitor::Propagate(const TVector3& position, const TVector3& momentum, const Int_t& charge, const Double_t& fZEnd,
  const Int_t &flagInMag, Double_t& thetaXafter) {
  TVector3 fPosExp;
  Double_t fEC = TMath::C()* 1.e-9 * 1.e-4 * 1.e-2;
  Int_t fPartQ = charge;
  TVector3 fB;
  Double_t dMag = -999.;
  Double_t zMag = -999.;

  Double_t zMagStraw = 0.1 * 196345;
  Double_t dMagStraw = 0.1 * 1300;
  Double_t bMagStraw = 0.6928;

  Double_t zMagHAC = 0.1 * 248200.;
  Double_t dMagHAC = 0.1 * 2000.;
  Double_t bMagHAC = 1.7012;

  TVector3 tempMom = {momentum.X(), momentum.Y(), momentum.Z()};
  TVector3 tempPos = {position.X(), position.Y(), position.Z()};
  if (fZEnd <= zMagHAC) {
    zMag = zMagStraw;
    dMag = dMagStraw;
    fB = {0., bMagStraw * 10000, 0.};
  } else if (position.Z() > zMagStraw) {
    zMag = zMagHAC;
    dMag = dMagHAC;
    fB = {0., bMagHAC * 10000, 0.};
  } else {
    tempPos = Propagate(position, momentum, fPartQ, zMagHAC, flagInMag, thetaXafter);
    tempMom.SetZ(sqrt((pow(momentum.Mag(), 2) - pow(momentum.Y(), 2)) / (1 + pow(thetaXafter, 2))));
    tempMom.SetX(tempMom.Z() * thetaXafter);
    zMag = zMagHAC;
    dMag = dMagHAC;
    fB = {0.,bMagHAC * 10000., 0.};
  }

  Double_t fStartX = tempPos.X();
  Double_t fStartY = tempPos.Y();
  Double_t fStartZ = tempPos.Z();
Double_t fPartP = tempMom.Mag();
  Double_t fPartThetaX = tempMom.X() / tempMom.Z();
  Double_t fPartThetaY = tempMom.Y() / tempMom.Z();

  // fZEnd before magnet
  if ((fZEnd <= zMag && fStartZ <= zMag) || (fZEnd > zMag && fStartZ > zMag) || fPartQ == 0) {
    fPosExp.SetX(fStartX + fPartThetaX * (fZEnd - fStartZ));
    fPosExp.SetY(fStartY + fPartThetaY * (fZEnd - fStartZ));
    fPosExp.SetZ(fZEnd);
    //    return fPosExp*10;
    thetaXafter = fPartThetaX;
    return fPosExp;
  }

  // fZEnd after MNP33
  fPosExp.SetX(fStartX + fPartThetaX * (zMag - fStartZ));
  fPosExp.SetY(fStartY + fPartThetaY * (zMag - fStartZ));
  fPosExp.SetZ(zMag);
  TVector3 fP;
  fP.SetZ(fPartP / sqrt(1. + fPartThetaX * fPartThetaX + fPartThetaY * fPartThetaY));
  fP.SetX(fP.Z() * fPartThetaX);
  fP.SetY(fP.Z() * fPartThetaY);
  Int_t qb = fB.Y() > 0 ? 1 : -1;
  Double_t rho = (fP.Cross(fB)).Mag() / (fPartQ * fEC * fB.Mag2());
  Double_t delta = dMag / rho;
  Double_t sint = sin(atan(fPartThetaX));
  Double_t cost = cos(atan(fPartThetaX));
  Double_t dx = qb * rho * (-cost + sqrt(1 - (delta - qb * sint)*(delta - qb * sint)));
  fPosExp.SetX(fPosExp.X() + dx);
  fPosExp.SetY(fPosExp.Y() + fPartThetaY * dMag);
  fPosExp.SetZ(fPosExp.Z() + dMag);
  Double_t fThetaXAfter = -qb * (delta - qb * sint) / sqrt(1. - (delta - qb * sint)*(delta - qb * sint));
  fPosExp.SetX(fPosExp.X() + fThetaXAfter * (fZEnd - fPosExp.Z()));
  fPosExp.SetY(fPosExp.Y() + fPartThetaY * (fZEnd - fPosExp.Z()));
  fPosExp.SetZ(fZEnd);
  thetaXafter = fThetaXAfter;

  //  return fPosExp*10;
  return fPosExp;
}

void HACDataQualityMonitor::EndOfJobUser() {
  gErrorIgnoreLevel = 5000;

  if (!fReadingData) {
    if (!fHExpected_back || !(fHExpected_back->InheritsFrom("TH1"))) {
      std::cout << user_normal() << "Asked to read my own output but cannot find it" << std::endl;
      return;
    }
    // Rebin histos to get enough statistics in each bin
    // Rebin to 1binx * 1biny = 10cm*10cm
    fHExpected_front->GetXaxis()->SetRangeUser(-449.5, -150.5);
    fHExpected_front ->Rebin2D(50, 50);
    fHExpected_middle->GetXaxis()->SetRangeUser(-449.5, -150.5);
    fHExpected_middle ->Rebin2D(50, 50);
    fHExpected_back->GetXaxis()->SetRangeUser(-449.5, -150.5);
    fHExpected_back ->Rebin2D(50, 50);

    fHMatched_front ->GetXaxis()->SetRangeUser(-449.5, -150.5);
    fHMatched_front ->Rebin2D(50, 50);
    fHMatched_middle->GetXaxis()->SetRangeUser(-449.5, -150.5);
    fHMatched_middle ->Rebin2D(50, 50);
    fHMatched_back ->GetXaxis()->SetRangeUser(-449.5, -150.5);
    fHMatched_back ->Rebin2D(50, 50);

    fHEfficiency_back->Divide(fHMatched_back, fHExpected_back, 1., 1., "B");
    fHEfficiency_middle->Divide(fHMatched_middle, fHExpected_middle, 1., 1., "B");
    fHEfficiency_front->Divide(fHMatched_front, fHExpected_front, 1., 1., "B");

    //Create Bad burst list
    CreateBadBurstList();

    //Remove Empty bursts from TGraph
    for (Int_t iPoint = 0; iPoint < fHEfficiencyVsBurstID->GetN(); iPoint++) {
      double BurstID = 0., NSelectedTriggers = 0.;
      fHNSelectedTriggersVsBurstID->GetPoint(iPoint, BurstID, NSelectedTriggers);
      if (NSelectedTriggers < fNSelectedTriggersMin) {
        fHEfficiencyVsBurstID->RemovePoint(iPoint);
        fHExpectedVsBurstID->RemovePoint(iPoint);
        fHArgonionCountsVsBurstID->RemovePoint(iPoint);
        fHNTriggersVsBurstID->RemovePoint(iPoint);
        fHNSelectedTriggersVsBurstID->RemovePoint(iPoint);
        iPoint--;
      }
    }
    BuildPFDReport();
  }

  SaveAllPlots();
  gErrorIgnoreLevel = -1; // restore the default
}

void HACDataQualityMonitor::StartOfBurstUser() {
  fBurstID = GetBurstID();
  fArgonionCounts = 0.;
  fNTriggers = 0;
  fNSelectedTriggers = 0;
  fNMatchedPerBurst = 0.;
  fNExpectedPerBurst = 0.;

  UInt_t runID = GetRunID();
  if (runID > 7000)
    fZstartHAC = 25300.9;
  else
    fZstartHAC = 25380.9;
}

void HACDataQualityMonitor::EndOfBurstUser() {
  if (fReadingData) {
    double Efficiency = 0., eEfficiency = 0.;
    if (fNExpectedPerBurst) {
      Efficiency = fNMatchedPerBurst / fNExpectedPerBurst;
      eEfficiency = sqrt(Efficiency * (1. - Efficiency) / fNExpectedPerBurst);
    } else {
      fHExpectedVsBurstID->Set(fHExpectedVsBurstID->GetN() + 1);
      fHExpectedVsBurstID->SetPoint(fHExpectedVsBurstID->GetN() - 1, fBurstID, fNExpectedPerBurst);
      fHExpectedVsBurstID->SetPointError(fHExpectedVsBurstID->GetN() - 1, 0, 0);
      return;
    }
    fHEfficiencyVsBurstID->Set(fHEfficiencyVsBurstID->GetN() + 1);
    fHEfficiencyVsBurstID->SetPoint(fHEfficiencyVsBurstID->GetN() - 1, fBurstID, Efficiency);
    fHEfficiencyVsBurstID->SetPointError(fHEfficiencyVsBurstID->GetN() - 1, 0, eEfficiency);

    fHExpectedVsBurstID->Set(fHExpectedVsBurstID->GetN() + 1);
    fHExpectedVsBurstID->SetPoint(fHExpectedVsBurstID->GetN() - 1, fBurstID, fNExpectedPerBurst);
    fHExpectedVsBurstID->SetPointError(fHExpectedVsBurstID->GetN() - 1, 0, 1 / sqrt(fNExpectedPerBurst));

    fHArgonionCountsVsBurstID->Set(fHArgonionCountsVsBurstID->GetN() + 1);
    fHArgonionCountsVsBurstID->SetPoint(fHArgonionCountsVsBurstID->GetN() - 1, fBurstID, fArgonionCounts);
    fHArgonionCountsVsBurstID->SetPointError(fHArgonionCountsVsBurstID->GetN() - 1, 0, 0);

    fHNTriggersVsBurstID->Set(fHNTriggersVsBurstID->GetN() + 1);
    fHNTriggersVsBurstID->SetPoint(fHNTriggersVsBurstID->GetN() - 1, fBurstID, fNTriggers);
    fHNTriggersVsBurstID->SetPointError(fHNTriggersVsBurstID->GetN() - 1, 0, 0);

    fHNSelectedTriggersVsBurstID->Set(fHNSelectedTriggersVsBurstID->GetN() + 1);
    fHNSelectedTriggersVsBurstID->SetPoint(fHNSelectedTriggersVsBurstID->GetN() - 1, fBurstID, fNSelectedTriggers);
    fHNSelectedTriggersVsBurstID->SetPointError(fHNSelectedTriggersVsBurstID->GetN() - 1, 0, 0);
  }
}

void HACDataQualityMonitor::BuildPFDReport() {
  if(fCanvas) delete fCanvas;
  fCanvas = new TCanvas("Canvas");

  if (fHEfficiencyVsBurstID->GetN()) {
    fHEfficiencyVsBurstID->SetTitle(Form("HASC efficiency Vs BurstID for run %d", GetRunID()));
    fHEfficiencyVsBurstID->Draw("AP");
    fHEfficiencyVsBurstID->SetMarkerStyle(20);
    fHEfficiencyVsBurstID->SetMarkerSize(0.3);
    fHEfficiencyVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEfficiencyVsBurstID->GetYaxis()->SetTitle("Efficiency");
    fHEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0., 0.1);
    TLine* EfficiencyThrBurst = new TLine(fHEfficiencyVsBurstID->GetXaxis()->GetXmin(), fEfficiencyThreshold, fHEfficiencyVsBurstID->GetXaxis()->GetXmax(), fEfficiencyThreshold);
    EfficiencyThrBurst->SetLineColor(kRed);
    EfficiencyThrBurst->Draw();
    fCanvas->Print(Form(fOutPDFFileName + "("), "pdf");
    delete fCanvas;

    fCanvas = new TCanvas("Canvas");
    fHExpectedVsBurstID->SetTitle(Form("Number of expected pions Vs BurstID for run %d", GetRunID()));
    fHExpectedVsBurstID->Draw("AP");
    fHExpectedVsBurstID->SetMarkerStyle(20);
    fHExpectedVsBurstID->SetMarkerSize(0.3);
    fHExpectedVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHExpectedVsBurstID->GetYaxis()->SetTitle("# Expected");
    fCanvas->Print(fOutPDFFileName, "pdf");
    delete fCanvas;

    fCanvas = new TCanvas("Canvas");
    fHTiming->SetTitle(Form("HASC candidate time wrt kaon time for run %d", GetRunID()));
    fHTiming->Draw();
    fHTiming->GetXaxis()->SetTitle("#Delta t (ns)");
    fCanvas->Print(fOutPDFFileName, "pdf");
    delete fCanvas;


    fCanvas = new TCanvas("Canvas");
    fHCharge->SetTitle(Form("Matched candidate charge for run %d", GetRunID()));
    fHCharge->Draw();
    fHCharge->GetXaxis()->SetTitle("Charge (pC)");
    fCanvas->Print(fOutPDFFileName, "pdf");
  }
  fCanvas->Print(Form(fOutPDFFileName + "]"), "pdf");
  delete fCanvas;
}

void HACDataQualityMonitor::CreateBadBurstList() {
  Int_t NEmptyBursts = 0, NBadLowStat = 0, NBadEfficiency = 0;
  ofstream BadBurstList;
  BadBurstList.open(Form("HACMonitor.BadBursts.thrs%.3f.dat", fEfficiencyThreshold), ios::app);
  for (Int_t iPoint = 0; iPoint < fHEfficiencyVsBurstID->GetN(); iPoint++) {
    double BurstID = 0., Efficiency = 0., Argonion = 0., NTriggers = 0., NSelectedTriggers = 0., fNExpected = 0.;
    fHEfficiencyVsBurstID->GetPoint(iPoint, BurstID, Efficiency);
    fHExpectedVsBurstID->GetPoint(iPoint, BurstID, fNExpected);
    fHArgonionCountsVsBurstID->GetPoint(iPoint, BurstID, Argonion);
    fHNTriggersVsBurstID->GetPoint(iPoint, BurstID, NTriggers);
    fHNSelectedTriggersVsBurstID->GetPoint(iPoint, BurstID, NSelectedTriggers);
    double eEfficiency = fHEfficiencyVsBurstID->GetErrorY(iPoint);
    if (NSelectedTriggers < fNSelectedTriggersMin) {
      if (NTriggers == 0 && Argonion == 0) { // Corrupted file
        BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " BADFILE " << std::endl;
      } else if (Argonion == 0) { // No Argonion info
        BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " NO_ARGN " << std::endl;
      } else if (Argonion * 1.e9 < fArgonionCountsMin) { // Low number of triggers due to no beam
        BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " LOWARGN " << Argonion * 1.e9 << std::endl;
      } else {
        BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " LOWTRIG " << NSelectedTriggers << std::endl;
      }
      NEmptyBursts++;
    } else if (fNExpected < 10) {
      BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " LOWSTAT " << fNExpectedPerBurst << std::endl;
      NBadLowStat++;
    } else if (Efficiency < fEfficiencyThreshold) { // bad HASC efficiency
      BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " EFFI " << Efficiency << " +/- " << eEfficiency << std::endl;
      NBadEfficiency++;
    }
  }
  BadBurstList.close();
  std::cout << user_standard() << "***** BADBURST SUMMARY ***** " << std::endl;
  std::cout << user_standard() << "Bad(Effi):  " << NBadEfficiency << std::endl;
  std::cout << user_standard() << "Bad(Expected): " << NBadLowStat << std::endl;
  std::cout << user_standard() << "Bad(HACTotal): " << NBadEfficiency;
  std::cout << user_standard() << " over " << fHEfficiencyVsBurstID->GetN() - NEmptyBursts << " non-empty bursts (NEmptyBursts: " << NEmptyBursts << ")" << std::endl;
  std::cout << user_standard() << "**************************** " << std::endl;
}
