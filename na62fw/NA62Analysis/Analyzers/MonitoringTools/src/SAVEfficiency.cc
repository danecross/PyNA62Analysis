// ------------------------------------------------------------
// IRC SAC and SAV efficiency studies
// It creates bad burst list and pdf report for IRC SAC and SAV
// ----------------------
// Created by Francesco Brizioli (francesco.brizioli@cern.ch)
// March 2019
// ----------------------
/// \author Francesco Brizioli (francesco.brizioli@cern.ch)
// ------------------------------------------------------------

#include <iostream>
#include <TChain.h>
#include <TLine.h>
#include "SAVEfficiency.hh"
#include "Event.hh"
#include "ConfigSettings.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "EnergyCluster.hh"
#include "SpectrometerCalorimetersAssociation.hh"
#include "K2piSelectionWithMissingPhoton.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SAVEfficiency::SAVEfficiency(Core::BaseAnalysis *ba) : Analyzer(ba, "SAVEfficiency"){
  RequestTree("LAV",    new TRecoLAVEvent,    "Reco");
  RequestTree("IRC",    new TRecoIRCEvent,    "Reco");
  RequestTree("SAC",    new TRecoSACEvent,    "Reco");
  RequestTree("SAV",    new TRecoSAVEvent,    "Reco");

  RequestL0Data();

  Configuration::ConfigSettings::SetNoSkipBadBurst(true); // do not skip bad bursts

  AddParam("TriggerMask", &fTriggerMask, 0xFF);
  AddParam("SAVDeltaTime", &fSAVDeltaTime, 5.0);
  // set thresholds to be used in CreateBadBurstList() and BuildPDFReport()
  AddParam("NSelectedTriggersMin", &fNSelectedTriggersMin, 1000);
  AddParam("IRCEfficiencyThreshold", &fIRCEfficiencyThreshold, 0.6);
  AddParam("SACEfficiencyThreshold", &fSACEfficiencyThreshold, 0.2);
  AddParam("IRCCreamEfficiencyThreshold", &fIRCCreamEfficiencyThreshold, 0.5);
  AddParam("SACCreamEfficiencyThreshold", &fSACCreamEfficiencyThreshold, 0.2);
  AddParam("IRCExpectedMin", &fIRCExpectedMin, 3);
  AddParam("SACExpectedMin", &fSACExpectedMin, 2);

  fOutPDFFileName = fAnalyzerName + ".pdf";
}

SAVEfficiency::~SAVEfficiency() {
}

void SAVEfficiency::StartOfBurstUser() {

  fNSelectedTriggers = 0;
  fExpectedIRC = 0;
  fExpectedSAC = 0;
  fExpectedIRCErr = 0.0;
  fExpectedSACErr = 0.0;
  fSignalInIRC = 0;
  fSignalInSAC = 0;
  fSignalInIRCCream = 0;
  fSignalInSACCream = 0;
  fIRCEfficiency = -1.0 ;
  fSACEfficiency = -1.0 ;
  fIRCEfficiencyErr = 0.0 ;
  fSACEfficiencyErr = 0.0 ;
  fIRCCreamEfficiency = -1.0 ;
  fSACCreamEfficiency = -1.0 ;
  fIRCCreamEfficiencyErr = 0.0 ;
  fSACCreamEfficiencyErr = 0.0 ;
}

void SAVEfficiency::InitHist(){
  fReadingData = GetIsTree();

  if (fReadingData){
    BookHisto("hIRCmatchedTime", new TH1D("hIRCmatchedTime","Closest Time in IRC - EventTime; T [ns]", 200, -10, 10));
    BookHisto("hSACmatchedTime", new TH1D("hSACmatchedTime","Closest Time in SAC - EventTime; T [ns]", 200, -10, 10));

    BookHisto("hLostPhotonIRCPosition", new TH2D("hLostPhotonIRCPosition", "Lost Photon Position at IRC Z plane;[mm];[mm]", 300,-1500, 1500, 300, -1500, 1500));
    BookHisto("hLostPhotonIRCPosition_InIRC", new TH2D("hLostPhotonIRCPosition_InIRC", "Lost Photon Position at IRC Z plane;[mm];[mm]", 300,-150, 150, 300, -150, 150));
    BookHisto("hLostPhotonEnergy_InIRC", new TH1D("hLostPhotonEnergy_InIRC","Lost Photon Energy; E [GeV]", 150, 0, 75));
    BookHisto("hLostPhotonSACPosition", new TH2D("hLostPhotonSACPosition", "Lost Photon Position at SAC Z plane;[mm];[mm]", 300,-1500, 1500, 300, -1500, 1500));
    BookHisto("hLostPhotonSACPosition_InSAC", new TH2D("hLostPhotonSACPosition_InSAC", "Lost Photon Position at SAC Z plane;[mm];[mm]", 300,-150, 150, 300, -150, 150));
    BookHisto("hLostPhotonEnergy_InSAC", new TH1D("hLostPhotonEnergy_InSAC","Lost Photon Energy; E [GeV]", 150, 0, 75));
    BookHisto("hIRCandSACefficiency", new TH1D("hIRCandSACefficiency", "0:all, 2:IRC_exp, 3:IRC, 4:IRCCream, 6:SAC_exp, 7:SAC, 8:SACCream", 11, -1.5, 9.5));

    BookHisto("hNSelectedTriggersVsBurstID", new TGraphErrors());
    fHNSelectedTriggersVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->SetName("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->Set(0);

    BookHisto("hIRCExpectedVsBurstID", new TGraphErrors());
    fHIRCExpectedVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hIRCExpectedVsBurstID");
    fHIRCExpectedVsBurstID->SetName("hIRCExpectedVsBurstID");
    fHIRCExpectedVsBurstID->Set(0);

    BookHisto("hSACExpectedVsBurstID", new TGraphErrors());
    fHSACExpectedVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hSACExpectedVsBurstID");
    fHSACExpectedVsBurstID->SetName("hSACExpectedVsBurstID");
    fHSACExpectedVsBurstID->Set(0);

    BookHisto("hIRCEfficiencyVsBurstID", new TGraphErrors());
    fHIRCEfficiencyVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hIRCEfficiencyVsBurstID");
    fHIRCEfficiencyVsBurstID->SetName("hIRCEfficiencyVsBurstID");
    fHIRCEfficiencyVsBurstID->Set(0);

    BookHisto("hSACEfficiencyVsBurstID", new TGraphErrors());
    fHSACEfficiencyVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hSACEfficiencyVsBurstID");
    fHSACEfficiencyVsBurstID->SetName("hSACEfficiencyVsBurstID");
    fHSACEfficiencyVsBurstID->Set(0);

    BookHisto("hIRCCreamEfficiencyVsBurstID", new TGraphErrors());
    fHIRCCreamEfficiencyVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hIRCCreamEfficiencyVsBurstID");
    fHIRCCreamEfficiencyVsBurstID->SetName("hIRCCreamEfficiencyVsBurstID");
    fHIRCCreamEfficiencyVsBurstID->Set(0);

    BookHisto("hSACCreamEfficiencyVsBurstID", new TGraphErrors());
    fHSACCreamEfficiencyVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hSACCreamEfficiencyVsBurstID");
    fHSACCreamEfficiencyVsBurstID->SetName("hSACCreamEfficiencyVsBurstID");
    fHSACCreamEfficiencyVsBurstID->Set(0);

  }

  else{ // histo mode
    std::cout << user_normal() << "Reading my own output" << std::endl;
    fHNSelectedTriggersVsBurstID = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNSelectedTriggersVsBurstID", true);
    fHIRCEfficiencyVsBurstID = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hIRCEfficiencyVsBurstID", true);
    fHIRCExpectedVsBurstID = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hIRCExpectedVsBurstID", true);
    fHSACEfficiencyVsBurstID = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hSACEfficiencyVsBurstID", true);
    fHSACExpectedVsBurstID = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hSACExpectedVsBurstID", true);
    fHIRCCreamEfficiencyVsBurstID = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hIRCCreamEfficiencyVsBurstID", true);
    fHSACCreamEfficiencyVsBurstID = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hSACCreamEfficiencyVsBurstID", true);
  }

}

void SAVEfficiency::InitOutput() {
}

void SAVEfficiency::Process(Int_t) {

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t PhysicsData   = L0DataType & 0x1;
  Bool_t CTRLTrigger   = L0DataType & 0x10;
  Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0xFF)) || CTRLTrigger; //ALL MASKS + CTRL
  if (!TriggerOK) return; // process control triggers and selected MASKS only
  fNSelectedTriggers++;

  fRunID = GetRunID();
  fBurstID = GetBurstID();

  TRecoLAVEvent*    LAVEvent    = static_cast<TRecoLAVEvent*>(GetEvent("LAV"));
  TRecoIRCEvent*    IRCEvent    = static_cast<TRecoIRCEvent*>(GetEvent("IRC"));
  TRecoSACEvent*    SACEvent    = static_cast<TRecoSACEvent*>(GetEvent("SAC"));
  TRecoSAVEvent*    SAVEvent    = static_cast<TRecoSAVEvent*>(GetEvent("SAV"));

  // Get output from K2piSelectionWithMissingPhoton
  TLorentzVector LostPhoton;
  TVector3 Vertex ;
  Double_t EventTime ;
  const K2piSelectionWithMissingPhotonOutput K2piWithMissingPhoton =
    *static_cast<const K2piSelectionWithMissingPhotonOutput*>(GetOutput("K2piSelectionWithMissingPhoton.SelectedK2piWithMissingPhoton"));
  LostPhoton = K2piWithMissingPhoton.MissingPhotonFourMomentum ;
  Vertex = K2piWithMissingPhoton.DecayVertex ;
  EventTime = K2piWithMissingPhoton.EventTime ;
  if(LostPhoton.E()<6000.0) return ; // Lost Photon Min Energy: 6 GeV

  // ******************************* LAV - IRC - SAC *************************************************************

  // LAV veto (with timing)
  LAVMatching* pLAVMatching = *(LAVMatching**)GetOutput("PhotonVetoHandler.LAVMatching");
  pLAVMatching->SetReferenceTime(EventTime);
  Bool_t LAVmatched = pLAVMatching->LAVHasTimeMatching(LAVEvent);
  Double_t LAVmatchedTime;
  if(LAVmatched) {
    LAVmatchedTime = pLAVMatching->GetBestTimeOfMatchedBlocks();
    if (fabs(LAVmatchedTime)<3.0 || GetWithMC()) return ; // 3 ns ---> Giuseppe and Rado
  }

  // IRC and SAC veto (with timing) NOT APPLIED
  SAVMatching* pSAVMatching = *(SAVMatching**)GetOutput("PhotonVetoHandler.SAVMatching");
  pSAVMatching->SetReferenceTime(EventTime);
  pSAVMatching->SetIRCTimeCuts(10.0, 10.0); // look for association in +- 10 ns
  pSAVMatching->SetSACTimeCuts(10.0, 10.0);
  Bool_t SAVmatched = pSAVMatching->SAVHasTimeMatching(IRCEvent, SACEvent);
  Double_t IRCmatchedTime = 9999.9 ;
  Double_t SACmatchedTime = 9999.9 ;
  if (SAVmatched) {
    IRCmatchedTime = pSAVMatching->GetBestTimeOfIRCMatchedBlocks();
    SACmatchedTime = pSAVMatching->GetBestTimeOfSACMatchedBlocks();
    // if (fabs(IRCmatchedTime)<7.0 || fabs(SACmatchedTime)<7.0 || GetWithMC()) return ; // 7 ns ---> Giuseppe and Rado
  }

  Bool_t isIRCMatched = false;
  Bool_t isSACMatched = false;
  if(fabs(IRCmatchedTime)<fSAVDeltaTime) isIRCMatched = true ;
  if(fabs(SACmatchedTime)<fSAVDeltaTime) isSACMatched = true ;
  FillHisto("hIRCmatchedTime",IRCmatchedTime);
  FillHisto("hSACmatchedTime",SACmatchedTime);

  Bool_t isIRCCreamMatched = false;
  Bool_t isSACCreamMatched = false;
  Int_t isSAVCreamMatching = SAVCreamMatching(SAVEvent, EventTime, fSAVDeltaTime, 1000.0); // SAV min energy = 1 GeV
  if(isSAVCreamMatching==1) isIRCCreamMatched = true ;
  if(isSAVCreamMatching==2) isSACCreamMatched = true ;
  if(isSAVCreamMatching==3){ isIRCCreamMatched = true ; isSACCreamMatched = true ; }

  // IRC geometry
  Double_t IRC_Z = GeometricAcceptance::GetInstance()->GetZIRC(); // 239700 mm
  Double_t IRC_Rmin = GeometricAcceptance::GetInstance()->GetIRCRmin(); // 60 mm
  Double_t IRC_Rmax = GeometricAcceptance::GetInstance()->GetIRCRmax(); // 145 mm

  // lost photon estrapolation at IRC Z plane
  Double_t IRC_X = Vertex.X()+(IRC_Z-Vertex.Z())*LostPhoton.Px()/LostPhoton.Pz();
  Double_t IRC_Y = Vertex.Y()+(IRC_Z-Vertex.Z())*LostPhoton.Py()/LostPhoton.Pz();
  FillHisto("hLostPhotonIRCPosition",IRC_X,IRC_Y);
  Double_t IRC_R = sqrt(IRC_X*IRC_X+IRC_Y*IRC_Y);
  Bool_t isLostPhotonInIRC = false;
  if(IRC_R>IRC_Rmin && IRC_R<IRC_Rmax) isLostPhotonInIRC = true ;

  // SAC geometry
  Double_t SAC_Z = GeometricAcceptance::GetInstance()->GetZSAC(); // 260956  mm
  Double_t SAC_Rmax = GeometricAcceptance::GetInstance()->GetSACRmax(); // 100  mm

  // lost photon estrapolation at SAC Z plane
  Double_t SAC_X = Vertex.X()+(SAC_Z-Vertex.Z())*LostPhoton.Px()/LostPhoton.Pz();
  Double_t SAC_Y = Vertex.Y()+(SAC_Z-Vertex.Z())*LostPhoton.Py()/LostPhoton.Pz();
  FillHisto("hLostPhotonSACPosition",SAC_X,SAC_Y);
  Double_t SAC_R = sqrt(SAC_X*SAC_X+SAC_Y*SAC_Y);
  Bool_t isLostPhotonInSAC = false;
  if(SAC_R<SAC_Rmax) isLostPhotonInSAC = true ;

  FillHisto("hIRCandSACefficiency",0);

  if (isLostPhotonInIRC){
    FillHisto("hLostPhotonEnergy_InIRC", 0.001*LostPhoton.E()); // [GeV]
    FillHisto("hLostPhotonIRCPosition_InIRC",IRC_X,IRC_Y);
    FillHisto("hIRCandSACefficiency",2);
    fExpectedIRC++;
    if (isIRCMatched){
      fSignalInIRC++;
      FillHisto("hIRCandSACefficiency",3);
    }
    if (isIRCCreamMatched){
      fSignalInIRCCream++;
      FillHisto("hIRCandSACefficiency",4);
    }
  }
  if (isLostPhotonInSAC){
    FillHisto("hLostPhotonEnergy_InSAC", 0.001*LostPhoton.E()); // [GeV]
    FillHisto("hLostPhotonSACPosition_InSAC",SAC_X,SAC_Y);
    FillHisto("hIRCandSACefficiency",6);
    fExpectedSAC++;
    if (isSACMatched){
      fSignalInSAC++;
      FillHisto("hIRCandSACefficiency",7);
    }
    if (isSACCreamMatched){
      fSignalInSACCream++;
      FillHisto("hIRCandSACefficiency",8);
    }
  }

} // end Process Function

void SAVEfficiency::EndOfBurstUser() {

  if(!fReadingData) return;

  // IRC and SAC efficiency calculation (burst by burst)
  if(fExpectedIRC>0){
    fExpectedIRCErr = sqrt(fExpectedIRC);
    fIRCEfficiency = fSignalInIRC/(1.0*fExpectedIRC);
    fIRCEfficiencyErr = sqrt(fIRCEfficiency*(1.0-fIRCEfficiency)/(1.0*fExpectedIRC));
    fIRCCreamEfficiency = fSignalInIRCCream/(1.0*fExpectedIRC);
    fIRCCreamEfficiencyErr = sqrt(fIRCCreamEfficiency*(1.0-fIRCCreamEfficiency)/(1.0*fExpectedIRC));
  }
  if(fExpectedSAC>0){
    fExpectedSACErr = sqrt(fExpectedSAC);
    fSACEfficiency = fSignalInSAC/(1.0*fExpectedSAC);
    fSACEfficiencyErr = sqrt(fSACEfficiency*(1.0-fSACEfficiency)/(1.0*fExpectedSAC));
    fSACCreamEfficiency = fSignalInSACCream/(1.0*fExpectedSAC);
    fSACCreamEfficiencyErr = sqrt(fSACCreamEfficiency*(1.0-fSACCreamEfficiency)/(1.0*fExpectedSAC));
  }

  // fill TGraphErrors
  fHNSelectedTriggersVsBurstID->Set(fHNSelectedTriggersVsBurstID->GetN()+1);
  fHNSelectedTriggersVsBurstID->SetPoint(fHNSelectedTriggersVsBurstID->GetN()-1,fBurstID,fNSelectedTriggers);
  fHNSelectedTriggersVsBurstID->SetPointError(fHNSelectedTriggersVsBurstID->GetN()-1,0,sqrt(fNSelectedTriggers));

  fHIRCExpectedVsBurstID->Set(fHIRCExpectedVsBurstID->GetN()+1);
  fHIRCExpectedVsBurstID->SetPoint(fHIRCExpectedVsBurstID->GetN()-1,fBurstID,fExpectedIRC);
  fHIRCExpectedVsBurstID->SetPointError(fHIRCExpectedVsBurstID->GetN()-1,0,fExpectedIRCErr);

  fHSACExpectedVsBurstID->Set(fHSACExpectedVsBurstID->GetN()+1);
  fHSACExpectedVsBurstID->SetPoint(fHSACExpectedVsBurstID->GetN()-1,fBurstID,fExpectedSAC);
  fHSACExpectedVsBurstID->SetPointError(fHSACExpectedVsBurstID->GetN()-1,0,fExpectedSACErr);

  fHIRCEfficiencyVsBurstID->Set(fHIRCEfficiencyVsBurstID->GetN()+1);
  fHIRCEfficiencyVsBurstID->SetPoint(fHIRCEfficiencyVsBurstID->GetN()-1,fBurstID,fIRCEfficiency);
  fHIRCEfficiencyVsBurstID->SetPointError(fHIRCEfficiencyVsBurstID->GetN()-1,0,fIRCEfficiencyErr);

  fHSACEfficiencyVsBurstID->Set(fHSACEfficiencyVsBurstID->GetN()+1);
  fHSACEfficiencyVsBurstID->SetPoint(fHSACEfficiencyVsBurstID->GetN()-1,fBurstID,fSACEfficiency);
  fHSACEfficiencyVsBurstID->SetPointError(fHSACEfficiencyVsBurstID->GetN()-1,0,fSACEfficiencyErr);

  fHIRCCreamEfficiencyVsBurstID->Set(fHIRCCreamEfficiencyVsBurstID->GetN()+1);
  fHIRCCreamEfficiencyVsBurstID->SetPoint(fHIRCCreamEfficiencyVsBurstID->GetN()-1,fBurstID,fIRCCreamEfficiency);
  fHIRCCreamEfficiencyVsBurstID->SetPointError(fHIRCCreamEfficiencyVsBurstID->GetN()-1,0,fIRCCreamEfficiencyErr);

  fHSACCreamEfficiencyVsBurstID->Set(fHSACCreamEfficiencyVsBurstID->GetN()+1);
  fHSACCreamEfficiencyVsBurstID->SetPoint(fHSACCreamEfficiencyVsBurstID->GetN()-1,fBurstID,fSACCreamEfficiency);
  fHSACCreamEfficiencyVsBurstID->SetPointError(fHSACCreamEfficiencyVsBurstID->GetN()-1,0,fSACCreamEfficiencyErr);

}

void SAVEfficiency::EndOfJobUser() {

  if (fReadingData) {
    SaveAllPlots();
  }

  else{ // histo mode

    CreateBadBurstList();

    // Remove empty bursts from the TGraphs
    for(Int_t iPoint=0;iPoint<fHIRCEfficiencyVsBurstID->GetN();iPoint++){
      double  BurstID=0., NSelectedTriggers=0.;
      fHNSelectedTriggersVsBurstID->GetPoint(iPoint,BurstID,NSelectedTriggers);
      if(NSelectedTriggers<fNSelectedTriggersMin){
	fHNSelectedTriggersVsBurstID->RemovePoint(iPoint);
        fHIRCEfficiencyVsBurstID->RemovePoint(iPoint);
	fHSACEfficiencyVsBurstID->RemovePoint(iPoint);
	fHIRCCreamEfficiencyVsBurstID->RemovePoint(iPoint);
        fHSACCreamEfficiencyVsBurstID->RemovePoint(iPoint);
	fHIRCExpectedVsBurstID->RemovePoint(iPoint);
	fHSACExpectedVsBurstID->RemovePoint(iPoint);
        iPoint--;
      }
    }

    BuildPDFReport();
  }

  return;
}

void SAVEfficiency::CreateBadBurstList() {

  Int_t NBadIRCEfficiency=0;
  Int_t NBadSACEfficiency=0;
  Int_t NBadSAVEfficiency=0;
  Int_t NBadIRCLowStat=0;
  Int_t NBadSACLowStat=0;
  Int_t NBadLowTrig=0;
  ofstream BadBurstListSAV;
  BadBurstListSAV.open("SAVEfficiency.BadBursts.dat");
  for(Int_t iPoint=0;iPoint<fHNSelectedTriggersVsBurstID->GetN();iPoint++){
    double BurstID=0.,  NSelectedTriggers=0. ;
    double IRCEfficiency=0., eIRCEfficiency=0., IRCExpected=0., IRCCreamEfficiency=0., eIRCCreamEfficiency=0. ;
    double SACEfficiency=0., eSACEfficiency=0., SACExpected=0., SACCreamEfficiency=0., eSACCreamEfficiency=0. ;
    fHNSelectedTriggersVsBurstID->GetPoint(iPoint,BurstID,NSelectedTriggers);
    fHIRCEfficiencyVsBurstID->GetPoint(iPoint,BurstID,IRCEfficiency);
    fHIRCExpectedVsBurstID->GetPoint(iPoint,BurstID,IRCExpected);
    eIRCEfficiency = fHIRCEfficiencyVsBurstID->GetErrorY(iPoint);
    fHIRCCreamEfficiencyVsBurstID->GetPoint(iPoint,BurstID,IRCCreamEfficiency);
    eIRCCreamEfficiency = fHIRCCreamEfficiencyVsBurstID->GetErrorY(iPoint);
    fHSACEfficiencyVsBurstID->GetPoint(iPoint,BurstID,SACEfficiency);
    fHSACExpectedVsBurstID->GetPoint(iPoint,BurstID,SACExpected);
    eSACEfficiency = fHSACEfficiencyVsBurstID->GetErrorY(iPoint);
    fHSACCreamEfficiencyVsBurstID->GetPoint(iPoint,BurstID,SACCreamEfficiency);
    eSACCreamEfficiency = fHSACCreamEfficiencyVsBurstID->GetErrorY(iPoint);

    if(NSelectedTriggers<fNSelectedTriggersMin){ //LowTrig
      BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " IRC SAC SAV LowTriggers : "<< NSelectedTriggers << std::endl;
      NBadLowTrig++;
    }
    else if(IRCExpected<fIRCExpectedMin && SACExpected<fSACExpectedMin) { // LowStat both
      BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " IRC SAC SAV LowStat --> expec evts in I: "<< IRCExpected << " ;  expec evts in S: "<< SACExpected  << std::endl;
      NBadIRCLowStat++;
      NBadSACLowStat++;
    }
    else if(IRCExpected<fIRCExpectedMin) { // LowStat IRC only
      BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " IRC SAV LowStat --> expec evts in I: "<< IRCExpected << " ; expec evts in S: "<< SACExpected  << std::endl;
      NBadIRCLowStat++;
    }
    else if(SACExpected<fSACExpectedMin) { // LowStat SAC only
      BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " SAC SAV LowStat --> expec evts in I: "<< IRCExpected << " ; expec evts in S: "<< SACExpected  << std::endl;
      NBadSACLowStat++;
    }
    else{ // LowEff
      Bool_t isIRCLowEff = false;
      Bool_t isSACLowEff = false;
      Bool_t isSAVLowEff = false;
      if(IRCEfficiency<fIRCEfficiencyThreshold) isIRCLowEff = true;
      if(SACEfficiency<fSACEfficiencyThreshold) isSACLowEff = true;
      if(IRCCreamEfficiency<fIRCCreamEfficiencyThreshold || SACCreamEfficiency<fSACCreamEfficiencyThreshold) isSAVLowEff = true;

      if(isIRCLowEff && isSACLowEff && isSAVLowEff){
	BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " IRC SAC SAV LowEff --> I_tel62_Eff = " << IRCEfficiency << " +/- " << eIRCEfficiency << " ; I_cream_Eff = " << IRCCreamEfficiency << " +/- " << eIRCCreamEfficiency << " ; S_tel62_Eff = " << SACEfficiency << " +/- " << eSACEfficiency << " ; S_cream_Eff = " << SACCreamEfficiency << " +/- " << eSACCreamEfficiency << std::endl;
        NBadIRCEfficiency++;
	NBadSACEfficiency++;
	NBadSAVEfficiency++;
      }
      else if(isIRCLowEff && isSACLowEff){
	BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " IRC SAC LowEff --> I_tel62_Eff = " << IRCEfficiency << " +/- " << eIRCEfficiency << " ; I_cream_Eff = " << IRCCreamEfficiency << " +/- " << eIRCCreamEfficiency << " ; S_tel62_Eff = " << SACEfficiency << " +/- " << eSACEfficiency << " ; S_cream_Eff = " << SACCreamEfficiency << " +/- " << eSACCreamEfficiency << std::endl;
        NBadIRCEfficiency++;
	NBadSACEfficiency++;
      }
      else if(isIRCLowEff && isSAVLowEff){
	BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " IRC SAV LowEff --> I_tel62_Eff = " << IRCEfficiency << " +/- " << eIRCEfficiency << " ; I_cream_Eff = " << IRCCreamEfficiency << " +/- " << eIRCCreamEfficiency << " ; S_tel62_Eff = " << SACEfficiency << " +/- " << eSACEfficiency << " ; S_cream_Eff = " << SACCreamEfficiency << " +/- " << eSACCreamEfficiency << std::endl;
        NBadIRCEfficiency++;
	NBadSAVEfficiency++;
      }
      else if(isSACLowEff && isSAVLowEff){
	BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " SAC SAV LowEff --> I_tel62_Eff = " << IRCEfficiency << " +/- " << eIRCEfficiency << " ; I_cream_Eff = " << IRCCreamEfficiency << " +/- " << eIRCCreamEfficiency << " ; S_tel62_Eff = " << SACEfficiency << " +/- " << eSACEfficiency << " ; S_cream_Eff = " << SACCreamEfficiency << " +/- " << eSACCreamEfficiency << std::endl;
        NBadSACEfficiency++;
	NBadSAVEfficiency++;
      }
      else if(isIRCLowEff){
	BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " IRC LowEff --> I_tel62_Eff = " << IRCEfficiency << " +/- " << eIRCEfficiency << " ; I_cream_Eff = " << IRCCreamEfficiency << " +/- " << eIRCCreamEfficiency << " ; S_tel62_Eff = " << SACEfficiency << " +/- " << eSACEfficiency << " ; S_cream_Eff = " << SACCreamEfficiency << " +/- " << eSACCreamEfficiency << std::endl;
        NBadIRCEfficiency++;
      }
      else if(isSACLowEff){
	BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " SAC LowEff --> I_tel62_Eff = " << IRCEfficiency << " +/- " << eIRCEfficiency << " ; I_cream_Eff = " << IRCCreamEfficiency << " +/- " << eIRCCreamEfficiency << " ; S_tel62_Eff = " << SACEfficiency << " +/- " << eSACEfficiency << " ; S_cream_Eff = " << SACCreamEfficiency << " +/- " << eSACCreamEfficiency << std::endl;
        NBadSACEfficiency++;
      }
      else if(isSAVLowEff){
	BadBurstListSAV << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " SAV LowEff --> I_tel62_Eff = " << IRCEfficiency << " +/- " << eIRCEfficiency << " ; I_cream_Eff = " << IRCCreamEfficiency << " +/- " << eIRCCreamEfficiency << " ; S_tel62_Eff = " << SACEfficiency << " +/- " << eSACEfficiency << " ; S_cream_Eff = " << SACCreamEfficiency << " +/- " << eSACCreamEfficiency << std::endl;
        NBadSAVEfficiency++;
      }
      else{}
    }
  }
  BadBurstListSAV.close();

  std::cout << user_standard() << "***** IRC-SAC BAD BURST SUMMARY ***** " << std::endl;
  std::cout << user_standard() << "Bad(LowTrig):  " << NBadLowTrig << std::endl;
  std::cout << user_standard() << "Bad(IRCLowStat):  " << NBadIRCLowStat << std::endl;
  std::cout << user_standard() << "Bad(SACLowStat):  " << NBadSACLowStat << std::endl;
  std::cout << user_standard() << "Bad(IRCLowEff):  " << NBadIRCEfficiency << std::endl;
  std::cout << user_standard() << "Bad(SACLowEff):  " << NBadSACEfficiency << std::endl;
  std::cout << user_standard() << "Bad(SAVLowEff):  " << NBadSAVEfficiency << std::endl;
  return;
}

void SAVEfficiency::BuildPDFReport() {

  TCanvas *canvas = new TCanvas("Canvas");
  canvas->Print(Form(fOutPDFFileName + "["), "pdf");

  // IRCExpected Vs BurstID
  if(fHIRCExpectedVsBurstID && fHIRCExpectedVsBurstID->GetN()){
    // TLegend* Legend = new TLegend(0.13,0.13,0.3,0.25);
    // Legend->SetFillColor(kWhite);
    fHIRCExpectedVsBurstID->SetTitle(Form("IRC expected Vs BurstID for run %d",GetRunID()));
    fHIRCExpectedVsBurstID->Draw("AP");
    fHIRCExpectedVsBurstID->SetMarkerStyle(20);
    fHIRCExpectedVsBurstID->SetMarkerSize(0.8);
    fHIRCExpectedVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHIRCExpectedVsBurstID->GetYaxis()->SetTitle("IRC Expected");
    fHIRCExpectedVsBurstID->GetYaxis()->SetRangeUser(0.,140);
    // Legend->AddEntry(fHIRCExpectedVsBurstID,"IRC","pl");
    TLine* IRCExpectedThrBurst = new TLine(fHIRCExpectedVsBurstID->GetXaxis()->GetXmin(),(fIRCExpectedMin-0.5),fHIRCExpectedVsBurstID->GetXaxis()->GetXmax(),(fIRCExpectedMin-0.5));
    IRCExpectedThrBurst->SetLineColor(kRed);
    IRCExpectedThrBurst->Draw();
    // Legend->Draw();
    canvas->Print(fOutPDFFileName, "pdf");
    delete IRCExpectedThrBurst;
    // delete Legend;
  }

  // IRCEfficiency Vs BurstID
  if(fHIRCEfficiencyVsBurstID && fHIRCEfficiencyVsBurstID->GetN()){
    // TLegend* Legend = new TLegend(0.13,0.13,0.3,0.25);
    // Legend->SetFillColor(kWhite);
    fHIRCEfficiencyVsBurstID->SetTitle(Form("IRC efficiency Vs BurstID for run %d",GetRunID()));
    fHIRCEfficiencyVsBurstID->Draw("AP");
    fHIRCEfficiencyVsBurstID->SetMarkerStyle(20);
    fHIRCEfficiencyVsBurstID->SetMarkerSize(0.8);
    fHIRCEfficiencyVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHIRCEfficiencyVsBurstID->GetYaxis()->SetTitle("IRC Efficiency");
    fHIRCEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    // Legend->AddEntry(fHIRCEfficiencyVsBurstID,"IRC","pl");
    TLine* IRCEfficiencyThrBurst = new TLine(fHIRCEfficiencyVsBurstID->GetXaxis()->GetXmin(),fIRCEfficiencyThreshold,fHIRCEfficiencyVsBurstID->GetXaxis()->GetXmax(),fIRCEfficiencyThreshold);
    IRCEfficiencyThrBurst->SetLineColor(kRed);
    IRCEfficiencyThrBurst->Draw();
    // Legend->Draw();
    canvas->Print(fOutPDFFileName, "pdf");
    delete IRCEfficiencyThrBurst;
    // delete Legend;
  }

  // IRCCreamEfficiency Vs BurstID
  if(fHIRCCreamEfficiencyVsBurstID && fHIRCCreamEfficiencyVsBurstID->GetN()){
    // TLegend* Legend = new TLegend(0.13,0.13,0.3,0.25);
    // Legend->SetFillColor(kWhite);
    fHIRCCreamEfficiencyVsBurstID->SetTitle(Form("IRCCream efficiency Vs BurstID for run %d",GetRunID()));
    fHIRCCreamEfficiencyVsBurstID->Draw("AP");
    fHIRCCreamEfficiencyVsBurstID->SetMarkerStyle(20);
    fHIRCCreamEfficiencyVsBurstID->SetMarkerSize(0.8);
    fHIRCCreamEfficiencyVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHIRCCreamEfficiencyVsBurstID->GetYaxis()->SetTitle("IRCCream Efficiency");
    fHIRCCreamEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    // Legend->AddEntry(fHIRCCreamEfficiencyVsBurstID,"IRCCream","pl");
    TLine* IRCCreamEfficiencyThrBurst = new TLine(fHIRCCreamEfficiencyVsBurstID->GetXaxis()->GetXmin(),fIRCCreamEfficiencyThreshold,fHIRCCreamEfficiencyVsBurstID->GetXaxis()->GetXmax(),fIRCCreamEfficiencyThreshold);
    IRCCreamEfficiencyThrBurst->SetLineColor(kRed);
    IRCCreamEfficiencyThrBurst->Draw();
    // Legend->Draw();
    canvas->Print(fOutPDFFileName, "pdf");
    delete IRCCreamEfficiencyThrBurst;
    // delete Legend;
  }

  // SACExpected Vs BurstID
  if(fHSACExpectedVsBurstID && fHSACExpectedVsBurstID->GetN()){
    // TLegend* Legend = new TLegend(0.13,0.13,0.3,0.25);
    // Legend->SetFillColor(kWhite);
    fHSACExpectedVsBurstID->SetTitle(Form("SAC expected Vs BurstID for run %d",GetRunID()));
    fHSACExpectedVsBurstID->Draw("AP");
    fHSACExpectedVsBurstID->SetMarkerStyle(20);
    fHSACExpectedVsBurstID->SetMarkerSize(0.8);
    fHSACExpectedVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHSACExpectedVsBurstID->GetYaxis()->SetTitle("SAC Expected");
    fHSACExpectedVsBurstID->GetYaxis()->SetRangeUser(0.,80);
    // Legend->AddEntry(fHSACExpectedVsBurstID,"SAC","pl");
    TLine* SACExpectedThrBurst = new TLine(fHSACExpectedVsBurstID->GetXaxis()->GetXmin(),(fSACExpectedMin-0.5),fHSACExpectedVsBurstID->GetXaxis()->GetXmax(),(fSACExpectedMin-0.5));
    SACExpectedThrBurst->SetLineColor(kRed);
    SACExpectedThrBurst->Draw();
    // Legend->Draw();
    canvas->Print(fOutPDFFileName, "pdf");
    delete SACExpectedThrBurst;
    // delete Legend;
  }

  // SACEfficiency Vs BurstID
  if(fHSACEfficiencyVsBurstID && fHSACEfficiencyVsBurstID->GetN()){
    // TLegend* Legend = new TLegend(0.13,0.13,0.3,0.25);
    // Legend->SetFillColor(kWhite);
    fHSACEfficiencyVsBurstID->SetTitle(Form("SAC efficiency Vs BurstID for run %d",GetRunID()));
    fHSACEfficiencyVsBurstID->Draw("AP");
    fHSACEfficiencyVsBurstID->SetMarkerStyle(20);
    fHSACEfficiencyVsBurstID->SetMarkerSize(0.8);
    fHSACEfficiencyVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHSACEfficiencyVsBurstID->GetYaxis()->SetTitle("SAC Efficiency");
    fHSACEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    // Legend->AddEntry(fHSACEfficiencyVsBurstID,"SAC","pl");
    TLine* SACEfficiencyThrBurst = new TLine(fHSACEfficiencyVsBurstID->GetXaxis()->GetXmin(),fSACEfficiencyThreshold,fHSACEfficiencyVsBurstID->GetXaxis()->GetXmax(),fSACEfficiencyThreshold);
    SACEfficiencyThrBurst->SetLineColor(kRed);
    SACEfficiencyThrBurst->Draw();
    // Legend->Draw();
    canvas->Print(fOutPDFFileName, "pdf");
    delete SACEfficiencyThrBurst;
    // delete Legend;
  }

  // SACCreamEfficiency Vs BurstID
  if(fHSACCreamEfficiencyVsBurstID && fHSACCreamEfficiencyVsBurstID->GetN()){
    // TLegend* Legend = new TLegend(0.13,0.13,0.3,0.25);
    // Legend->SetFillColor(kWhite);
    fHSACCreamEfficiencyVsBurstID->SetTitle(Form("SACCream efficiency Vs BurstID for run %d",GetRunID()));
    fHSACCreamEfficiencyVsBurstID->Draw("AP");
    fHSACCreamEfficiencyVsBurstID->SetMarkerStyle(20);
    fHSACCreamEfficiencyVsBurstID->SetMarkerSize(0.8);
    fHSACCreamEfficiencyVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHSACCreamEfficiencyVsBurstID->GetYaxis()->SetTitle("SACCream Efficiency");
    fHSACCreamEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    // Legend->AddEntry(fHSACCreamEfficiencyVsBurstID,"SACCream","pl");
    TLine* SACCreamEfficiencyThrBurst = new TLine(fHSACCreamEfficiencyVsBurstID->GetXaxis()->GetXmin(),fSACCreamEfficiencyThreshold,fHSACCreamEfficiencyVsBurstID->GetXaxis()->GetXmax(),fSACCreamEfficiencyThreshold);
    SACCreamEfficiencyThrBurst->SetLineColor(kRed);
    SACCreamEfficiencyThrBurst->Draw();
    // Legend->Draw();
    canvas->Print(fOutPDFFileName, "pdf");
    delete SACCreamEfficiencyThrBurst;
    // delete Legend;
  }

  canvas->Print(Form(fOutPDFFileName + "]"), "pdf");
  delete canvas;

  return;
}



Int_t SAVEfficiency::SAVCreamMatching(TRecoSAVEvent* event, Double_t reftime, Double_t TimeCut, Double_t EnergyCut ) {
  // return: 0 = no matching, 1 = only IRC, 2 = only SAC, 3 = both IRC and SAC

  if (!event->GetNHits()) return 0;

  TClonesArray& Hits = (*(event->GetHits()));
  Double_t mintime_irc = 999999.;
  Double_t mintime_sac = 999999.;
  Bool_t ismin_sac = 0;
  Bool_t ismin_irc = 0;
  for (Int_t jHit=0; jHit<event->GetNHits(); jHit++) {
    TRecoSAVHit *hit = static_cast<TRecoSAVHit*>(Hits[jHit]);
    Int_t detectorID = hit->GetDetector();
    Double_t dtime = hit->GetTime()-reftime;
    Double_t energy = hit->GetEnergy();
    if (detectorID==0) { // sac
      if (fabs(dtime)<fabs(mintime_sac) && energy>EnergyCut) {
	mintime_sac = dtime;
	ismin_sac = 1;
      }
    }
    if (detectorID==1) { // irc
      if (fabs(dtime)<fabs(mintime_irc) && energy>EnergyCut) {
	mintime_irc = dtime;
	ismin_irc = 1;
      }
    }
  }

  Bool_t is_hit_sac = 0;
  Bool_t is_hit_irc = 0;
  if (ismin_sac && fabs(mintime_sac)<TimeCut) is_hit_sac = 1;
  if (ismin_irc && fabs(mintime_irc)<TimeCut) is_hit_irc = 1;

  if (is_hit_irc && !is_hit_sac) return 1;
  if (!is_hit_irc && is_hit_sac) return 2;
  if (is_hit_irc && is_hit_sac) return 3;

  return 0;

}
