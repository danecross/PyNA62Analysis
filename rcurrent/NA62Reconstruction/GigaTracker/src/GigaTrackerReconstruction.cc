// --------------------------------------------------------------
// 
// \Brief
// The GTK reconstruction algorithm
// \EndBrief
// \Detailed
// Builds GTK candidates (i.e. GTK tracks) using hits within +-10 ns from the reference time.
// \endcode
// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)
//
// History:
//
// Modified by Alina Kleimenova (alina.kleimenova@cern.ch) Sep. 2017
// - add table to the OM
//
// Modified by Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch) Sep. 2015
// - read 2015 data
//
// Modified by Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch) May. 2014
// - read 2014 data
//
// Modified by Bob Velghe (bob.velghe@cern.ch) Aug. 2014
// - Add hooks for the online monitor 
//
// Modified by Bob Velghe 2014-08-04
// - Add misalignment (time and position) parameters
//
// Modified by Bob Velghe (bob.velghe@cern.ch) 2014-02-28
// - Fix the units (GeV/c -> MeV/c)
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-10-29
//
// Modified by Simone Bifani (Simone.Bifani@cern.ch) 2009-04-24
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------

/// \class GigaTrackerReconstruction
/// Together with GigaTrackerCluster, this is a core part of the GigaTracker reconstruction
/// \EndBrief

#include "Riostream.h"
#include "TRegexp.h"
#include "math.h"
#include "TGigaTrackerHit.hh"
#include "GigaTrackerCluster.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "TGigaTrackerSpecialTriggerEvent.hh"
#include "NA62BufferProto.hh"
#include <vector>
#include <algorithm>

#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "NA62ConditionsService.hh"
#include "GigaTrackerReconstruction.hh"
#include "Tools.hh" //FIXME Move me
#include "RG_Utilities.hh"
#include "GigaTrackerDAQBoardTimeStamp.hh"
#include "GigaTrackerErrorsHandler.hh"


using namespace std;
using namespace NA62Tools;
//==============================================
void GigaTrackerReconstruction::FillTimes(Double_t ReferenceTime){
  NA62VReconstruction::FillTimes(ReferenceTime);
  for (Int_t iHit=0; iHit<fRecoEvent->GetNHits(); iHit++) {
    TRecoGigaTrackerHit *RecoHit = static_cast<TRecoGigaTrackerHit*>(fRecoEvent->GetHit(iHit));
    Int_t ChID = RecoHit->GetChannelID();
    Int_t ROCh = fRawDecoder->GetDecoder()->GetChannelRO(ChID);
    Double_t Time  = RecoHit->GetTime();
    Double_t RawTime  = RecoHit->GetRawTime();
    if (fHRecoHitTimeWrtReferenceVsROChannel) fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROCh, Time-ReferenceTime);
    if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROCh, RawTime-ReferenceTime);
  }
  return;
}

//==============================================
GigaTrackerReconstruction::GigaTrackerReconstruction(TFile* HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "GigaTracker", ConfigFileName),
  fParameters(nullptr)
{
  // INSTANCIATE PRIVATE OBJECTS
  fRecoEvent = new TRecoGigaTrackerEvent();
  fRunID = -1;
  mHistErrSet = false;
  fDBFileName = "";
  //  fDisableCandidates = 0;
  
  // Algorithm parameters 
  fClight            = TMath::C();
  fTimeWindow        = -999.; // ns
  fTimeWindowTrigger = -999.; // ns
  fXWindow           = -999.; // mm
  fYWindow           = -999.; // mm
  fChi2X             = -999.; // chi2 cut
  fChi2Y             = -999.; // chi2 cut
  fChi2T             = -999.; // chi2 cut

  fEnableRawTimeCorrections = false;
  fNHitsMax = -999;

  for(int i(0);i<3;i++){
    // Reset station (x,y) offsets
    for(int k(0);k<18000;k++)    fT0[i][k]=0; //pixel tzero
    for(int j(0);j<10;j++)       fTW[i][j]=NULL; //chip time walk
  }

  ParseConfFile(ConfigFileName);
}


//==============================================
GigaTrackerReconstruction::~GigaTrackerReconstruction() {
  for(int i(0);i<3;i++){
    for(int j(0);j<10;j++){
      if(fTW[i][j]) delete fTW[i][j];
    }
  }
}

//==============================================
void GigaTrackerReconstruction::Init(NA62VReconstruction* MainReco){
  //common part for all the subdetectors 
  NA62VReconstruction::Init(MainReco);
  fMC = 0;
  if (!static_cast<NA62Reconstruction*>(MainReco)->GetIsRawData()) fMC = 1;
  if (!fMC){
    LoadTW();
    LoadT0();
  }
  InitHistograms();
}

//==============================================
TDetectorVEvent * GigaTrackerReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/){
  return tEvent;
}

//==============================================
void GigaTrackerReconstruction::ParseConfFile(TString ConfFileName){
  /// \MemberDescr
  /// \param ConfFileName   Name of the configuration file.
  ///
  /// Read the reconstruction configuration file NA62Reconstruction/conf/GigaTracker.conf.62Reconstruction/GigaTracker/src/GigaTrackerReconstruction.cc
  /// \EndMemberDescr

  std::ifstream confFile(ConfFileName.Data());
  if(!confFile.is_open()) {
    perror(Form("Configuration File : %s",ConfFileName.Data()));
    exit(kWrongConfiguration);
  }
  
  TString Line;
  TObjArray * line;
  while(Line.ReadLine(confFile)){
    if(Line.BeginsWith("#")) continue;
    if(Line.BeginsWith("DBFileName")){
      line = Line.Tokenize(" ");
      fDBFileName = static_cast<TObjString*>(line->At(2))->GetString();
      delete line;
    } 
    else if (Line.BeginsWith("EnableRawTimeCorrections")) {
      fEnableRawTimeCorrections = TString(Line(TRegexp("[0-1]"))).Atoi();
    }
    else if (Line.BeginsWith("NHitsMax")) {
      fNHitsMax = TString(Line(TRegexp("[0-9]+"))).Atoi();
    }
    else if (Line.BeginsWith("TimeWindowTrigger")) {
      fTimeWindowTrigger = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if (Line.BeginsWith("TimeWindow")) {
      fTimeWindow = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if (Line.BeginsWith("XWindow")) {
      fXWindow = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if (Line.BeginsWith("YWindow")) {
      fYWindow = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if (Line.BeginsWith("Chi2X")) {
      line = Line.Tokenize(" ");
      fChi2X = static_cast<TObjString*>(line->At(1))->GetString().Atof();
      delete line;
    }
    else if (Line.BeginsWith("Chi2Y")) {
      line = Line.Tokenize(" ");
      fChi2Y = static_cast<TObjString*>(line->At(1))->GetString().Atof();
      delete line;
    }
    else if (Line.BeginsWith("Chi2T")) {
      line = Line.Tokenize(" ");
      fChi2T = static_cast<TObjString*>(line->At(1))->GetString().Atof();
      delete line;
    }
  } 
  confFile.close();
  return;
}

//==============================================
void GigaTrackerReconstruction::LoadTW() {
  TString Line;

  TString TimeWalkFileName = "GigaTracker-TimeWalk.dat";
  //TW Binning
  if(NA62ConditionsService::GetInstance()->Open(TimeWalkFileName)!=kSuccess) return;
  double twBins[412];
  int nBins(0);
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(TimeWalkFileName))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if (Line.BeginsWith("TimeWalkBinning")){
      TObjArray *tok = Line.Tokenize(" ");
      twBins[nBins] = static_cast<TObjString*>(tok->At(1))->GetString().Atof();
      nBins++;
      delete tok;
    }
  }
  NA62ConditionsService::GetInstance()->Close(TimeWalkFileName);

  //TW
  if(NA62ConditionsService::GetInstance()->Open(TimeWalkFileName)!=kSuccess) return;
  for (int iS(0); iS < 3; iS++) {
    for (int iC(0); iC < 10; iC++) {
      fTW[iS][iC] = new TGraph(nBins);
    }
  }
  int iLine(0);
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(TimeWalkFileName))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if (Line.BeginsWith("TimeWalk") && !Line.BeginsWith("TimeWalkBinning")){
      TObjArray *tok = Line.Tokenize(" ");
      Int_t StationID = iLine/(10*(nBins-1));
      Int_t ChipID    = (iLine/(nBins-1))%10;
      Int_t iBins = iLine%(nBins-1);
      fTW[StationID][ChipID]->SetPoint(iBins,twBins[iBins],(static_cast<TObjString*>(tok->At(1))->GetString().Atof()));
      iLine++;
      delete tok;
    }
  }
  NA62ConditionsService::GetInstance()->Close(TimeWalkFileName);
}

void GigaTrackerReconstruction::LoadT0(){
  TString Line;
  //T0
  if(NA62ConditionsService::GetInstance()->Open(fT0FileName)!=kSuccess) return;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fT0FileName))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    TObjArray *tok = Line.Tokenize(" ");
    if(tok->GetEntries()>1){
      Int_t    ChID = static_cast<TObjString*>(tok->At(0))->GetString().Atoi();
      Double_t t0   = static_cast<TObjString*>(tok->At(1))->GetString().Atof();
      Int_t iS  = ChID/100000;
      Int_t uid = ChID%100000;
      if(abs(fT0[iS][uid])>0.0001) cerr<<__FILE__<<" line "<<__LINE__<<" suspicious t0 overwriting ("<< fT0[iS][uid]<<") please check uid"<<uid<<" Station"<<iS<<endl;
      fT0[iS][uid] = t0;
    }
    delete tok;
  }
  NA62ConditionsService::GetInstance()->Close(fT0FileName);      
}

void GigaTrackerReconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  fRunID = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  fParameters = GigaTrackerParameterTools::GetInstance(fRunID,fMC);
  fNStations = fParameters->GetNStations();
}

//==============================================
void GigaTrackerReconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
  mHistErrSet = 0;
}
//==============================================
TRecoVEvent * GigaTrackerReconstruction::ProcessEOBEvent(TDetectorVEvent* tEvent, Event* /*tGenEvent*/){

  TGigaTrackerSpecialTriggerEvent* specialEvt =  static_cast<TGigaTrackerSpecialTriggerEvent*>( tEvent);
  printf("-Total Number of Hits seen in GTK\n");
  for (int i(0); i<3;i++){
    printf(" GTK%d\n  half1: ",i+1);
    UInt_t tot(0);
    for (int j(0); j<10;j++){
      printf("%10d ",specialEvt->GetNHits(i,j,0));
      tot+=specialEvt->GetNHits(i,j);
      // EOB coarse hitmap half chip ID (0,1) vs chip ID (0,9)
      for (int k(0); k<2; k++) {
        //fHCoarseHitMap[i]->Fill(j,k,specialEvt->GetNHits(i,j,k));
        if (j<5){
          Double_t iHalfChip = j+(1-k)/2.;
          fHCoarseHitMap[i]->Fill(iHalfChip,0.,specialEvt->GetNHits(i,j,k));
        }
        else{
          Double_t iHalfChip = j-5+k/2.;
          fHCoarseHitMap[i]->Fill(iHalfChip,1.,specialEvt->GetNHits(i,j,k));
        }
      }
    }
    printf("\n  half2: ");
    for (int j(0); j<10;j++)       printf("%10d ",specialEvt->GetNHits(i,j,1));
    printf("\n  sum  : ");
    for (int j(0); j<10;j++)       printf("%10d ",specialEvt->GetNHits(i,j));
    printf("\n  total:  %u \n",tot);
  }
  printf("-Firmware Version\n");
  for (int i(0); i<3;i++){
    printf(" GTK%d:\n  half1:  ",i+1);
    for (int j(0); j<10;j++)      printf("%10u ",specialEvt->GetFW(i,j,0) );
    printf("\n  half2:  ");
    for (int j(0); j<10;j++)      printf("%10u ",specialEvt->GetFW(i,j,1) );
    printf("\n");
  }
  return 0;
}

//==============================================
TRecoVEvent * GigaTrackerReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){
  if (tEvent->IsA()->InheritsFrom("TSpecialTriggerEvent")){
    if(!isL0EOB(tEvent->GetTriggerType())) return 0; //skip SOB  
    return ProcessEOBEvent(tEvent,tGenEvent);
  }

  // Common part for all the subdetectors 
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  if(fMC && fRunID == -1 ) StartOfBurst();

  // Trigger Time
  EventHeader * rawHeader = NA62RecoManager::GetInstance()->GetEventHeader();

  TGigaTrackerEvent* tGigaTrackerEvent = static_cast<TGigaTrackerEvent*>(tEvent);
  Int_t NDigis = tGigaTrackerEvent->GetNHits();
  TClonesArray & Digis = (* (tGigaTrackerEvent->GetHits()));
  fRefTime = rawHeader->GetFineTime() * TdcCalib;
  
  // DIGI to RECO applying Tzero and Time walk
  for(int i(0);i<3;i++){
    for(int j(0);j<11;j++){
      fNHitsPerTriggerAll[i][j] = 0;
      fNHitsPerTriggerOOBAll[i][j] = 0;
      fNHitsPerTrigger[i][j] = 0;
      fNHitsPerTriggerOOB[i][j] = 0;
    }
  }
  for(Int_t iDigi = 0 ; iDigi < NDigis ; iDigi++){

    // Add Hit ----------------------------------------------------
    TGigaTrackerDigi * Digi = static_cast<TGigaTrackerDigi*>(Digis[iDigi]);
    if(Digi==NULL) {
      RG_REPORT_ERROR_MSG("Requested Digi but NULL Pointer returned");
      return fRecoEvent;
    }
    TRecoGigaTrackerHit* RecoHit = static_cast<TRecoGigaTrackerHit*>(fRecoEvent->AddHit(Digi));
    RecoHit->DecodeChannelID();
    (*(GigaTrackerChannelID*) RecoHit) = (*(GigaTrackerChannelID*) Digi);
    (*(TVHit*) RecoHit) = (*(TVHit*) Digi);
    //-------------------------------------------------------------
    
    // Is hit pile up?---------------------------------------------
    RecoHit->SetIsPileUpHit(Digi->GetIsPileUp());
    //-------------------------------------------------------------
    
    // Hit position-------------------------------------------------
    int iS = Digi->GetStationNo();
    int iC = Digi->GetChipID();
    int iPixID = Digi->GetPixelID();
    TVector3 PixelPosition = Digi->GetRawPosition();

    // Correct hit positions for station offsets
    PixelPosition[0] -=  fParameters->GetGigaTrackerStationMisalignment(iS).X();
    PixelPosition[1] -=  fParameters->GetGigaTrackerStationMisalignment(iS).Y();

    RecoHit->SetPosition(PixelPosition);
    //-----------------------------------------------------------

    // Time and ToT-----------------------------------------------
    Double_t Time(0), RawTime(0), AbsTime(0), ToT(0);
    if(!fMC){ // do I leave it here as it is?
      Time =  GetRecoTime(Digi);
      RawTime = GetRawRecoTime(Digi);
      AbsTime =  Digi->GetAbsLeadingEdge();
      ToT = Digi->GetTrailingEdge() - Digi->GetLeadingEdge();
    }
    else{
      Time = Digi->GetLeadingEdge();
      RawTime = Time;
      AbsTime = 0;
      ToT = 0;
    }

    RecoHit->SetTime(Time);
    RecoHit->SetRawTime(RawTime);
    RecoHit->SetToT(ToT);

    //-----------------------------------------------------------


    //PLOTS------------------------------------------------------
    fNHitsPerTriggerAll[iS][iC] += 1;
    fNHitsPerTriggerAll[iS][10] += 1;
    if ((rawHeader->GetTriggerType()&0xff)==0x08 || (rawHeader->GetTriggerType()&0xff)==0x31){
      fNHitsPerTriggerOOBAll[iS][iC] += 1;
      fNHitsPerTriggerOOBAll[iS][10] += 1;
    }

    if(iPixID<0 || iPixID>18000) continue;
    fNHitsPerTrigger[iS][iC] += 1;
    fNHitsPerTrigger[iS][10] += 1;
    fHHitMap[iS]->Fill(iPixID%200,iPixID/200);
    fHAbsTimeProfile[iS]->Fill(AbsTime*1e-9);
    fHAbsTimeProfileZoom[iS]->Fill(AbsTime*1e-9);
    fHToT[iS][iC]->Fill(ToT);
    fHToT[iS][10]->Fill(ToT);

    fHTime[iS][iC]->Fill(Time-rawHeader->GetFineTime()*ClockPeriod/256.0);
    fHTime[iS][10]->Fill(Time-rawHeader->GetFineTime()*ClockPeriod/256.0);
    fHTimeProfile[iS][iC]->Fill(AbsTime*1e-9,Time-rawHeader->GetFineTime()*ClockPeriod/256.0);
    fHTimeProfile[iS][10]->Fill(AbsTime*1e-9,Time-rawHeader->GetFineTime()*ClockPeriod/256.0);

    fHRawTime[iS][iC]->Fill(RawTime-rawHeader->GetFineTime()*ClockPeriod/256.0);
    fHRawTime[iS][10]->Fill(RawTime-rawHeader->GetFineTime()*ClockPeriod/256.0);
    fHRawTimeProfile[iS][iC]->Fill(AbsTime*1e-9,RawTime-rawHeader->GetFineTime()*ClockPeriod/256.0);
    fHRawTimeProfile[iS][10]->Fill(AbsTime*1e-9,RawTime-rawHeader->GetFineTime()*ClockPeriod/256.0);
    
    if ((rawHeader->GetTriggerType()&0xff)==0x08 || (rawHeader->GetTriggerType()&0xff)==0x31){
      fNHitsPerTriggerOOB[iS][iC] += 1;
      fNHitsPerTriggerOOB[iS][10] += 1;
      fHHitMapOOB[iS]->Fill(iPixID%200,iPixID/200);
      fHAbsTimeProfileOOB[iS]->Fill(AbsTime*1e-9);
      //fHAbsTimeProfileZoomOOB[iS]->Fill((rawHeader->GetTimeStamp()*ClockPeriod + Time)*1e-9);
      fHAbsTimeProfileZoomOOB[iS]->Fill(AbsTime*1e-9);
      fHToTOOB[iS][iC]->Fill(ToT);
      fHToTOOB[iS][10]->Fill(ToT);
      fHTimeOOB[iS][iC]->Fill(Time);
      fHTimeOOB[iS][10]->Fill(Time-rawHeader->GetFineTime()*ClockPeriod/256.0);
    }
  }

  for(int iS(0);iS<3;iS++){
    for(int iC(0);iC<11;iC++){
      fHNHitsPerTrigger[iS][iC]->Fill(fNHitsPerTrigger[iS][iC]);
      if ((rawHeader->GetTriggerType()&0xff)==0x08 || (rawHeader->GetTriggerType()&0xff)==0x31)
        fHNHitsPerTriggerOOB[iS][iC]->Fill(fNHitsPerTriggerOOB[iS][iC]);
    }
  }
  //-----------------------------------------------------------

  ReconstructCandidates(fRecoEvent);
  if(tGenEvent == 0) {
    return fRecoEvent; // Stop here if data events.
  }

  //////////////////////////////////////////////////////////////////////////////////////////////
  // Fill histograms with MCTruth information 

  TClonesArray& KinePartsArray = *(tGenEvent->GetKineParts());
  TVector3 TrueMomentum;

  for(Int_t iPart=0; iPart < tGenEvent->GetNKineParts() ; iPart++){
    KinePart* Particle = static_cast<KinePart*>(KinePartsArray[iPart]);

    if(Particle->GetID() == 1){

      TrueMomentum = Particle->GetFinalMomentum();

      if(TrueMomentum.Z() != 0.){
        fHTrueMomentum->Fill(TrueMomentum.Mag()*1e-3);
        fHTrueThetaX->Fill(1e6 * TMath::ATan(TrueMomentum.X()/TrueMomentum.Z()));
        fHTrueThetaY->Fill(1e6 * TMath::ATan(TrueMomentum.Y()/TrueMomentum.Z()));
        fHTrueMomentumX->Fill(TrueMomentum.X());
        fHTrueMomentumY->Fill(TrueMomentum.Y());
        fHTrueMomentumZ->Fill(TrueMomentum.Z());
      }
    }
  }

  //
  // Burst / stats
  //
  tEvent->GetBurstID();
  return fRecoEvent;
}

Double_t GigaTrackerReconstruction::GetRecoTime(TGigaTrackerDigi *Digi){
  if (fMC) return Digi->GetLeadingEdge();
  int iS = Digi->GetStationNo();
  int iC = Digi->GetChipID();
  int iPixID = Digi->GetPixelID();
  Double_t Time = Digi->GetLeadingEdge();
  Double_t ToT = Digi->GetTrailingEdge() - Digi->GetLeadingEdge();
  Time = Time - fTW[iS][iC]->Eval(ToT);
  Time = Time - fT0[iS][iPixID];
  Time = Time - GetT0Correction(Digi);
  return Time;
}

//==============================================
Double_t GigaTrackerReconstruction::GetRawRecoTime(TGigaTrackerDigi *Digi){
  if (fMC) return Digi->GetLeadingEdge();
  Double_t Time = Digi->GetLeadingEdge();
  Time = Time - GetT0Correction(Digi);
  return Time;
}

//==============================================
void GigaTrackerReconstruction::EndProcessing() {
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  SaveHistograms();
  //  GTK::GigaTrackerDAQBoardTimeStamp::PrintErrorSummary();
  GTK::GigaTrackerErrorsHandler::GetInstance()->PrintSummaryTable("GTK Errors");
  GTK::GigaTrackerErrorsHandler::GetInstance()->Reset();
}

//==============================================
void GigaTrackerReconstruction::InitHistograms(){
  GetOrMakeDir(fHistoFile,"GigaTrackerMonitor")->cd();
  GetOrMakeDir(fHistoFile,"GigaTrackerMonitor/Reco")->cd();
  
  for(int iS(0); iS<3; iS++){
    // This histo is for EOB monitor:
    fHCoarseHitMap[iS]          = new TH2F(Form("CoarseHitMap_%d",iS+1),Form("Coarse Hit Map in GTK%d; X; Y",iS+1),10,0.,5., 2,-0.5,1.5);

    fHHitMap[iS]                = new TH2F(Form("HitMap_%d",iS+1),Form("Hit Map in GTK%d;x [Pixel];y [Pixel]",iS+1),200,0,200,90,0,90);
    fHAbsTimeProfile[iS]        = new TH1D(Form("AbsTimeProfile_%d",iS+1),Form("Time Profile in GTK%d;t [s]; count",iS+1),1500,0,6.5);   
    fHAbsTimeProfileZoom[iS]    = new TH1D(Form("AbsTimeProfileZoom_%d",iS+1),Form("Zoom Time Profile in GTK%d;t [s]; count",iS+1),500,3.0,3.1);   
    fHHitMapOOB[iS]             = new TH2F(Form("OOB_HitMap_%d",iS+1),Form("Hit Map in GTK%d;x [Pixel];y [Pixel]",iS+1),200,0,200,90,0,90);    
    fHAbsTimeProfileOOB[iS]     = new TH1D(Form("OOB_AbsTimeProfile_%d",iS+1),Form("Time Profile in GTK%d;t [s]; count",iS+1),1500,0,6.5);  
    fHAbsTimeProfileZoomOOB[iS] = new TH1D(Form("OOB_AbsTimeProfileZoom_%d",iS+1),Form("Zoom Time Profile in GTK%d;t [s]; count",iS+1),500,0.5,0.6);    
    TString SuffixName;
    TString SuffixTitle;
    for(int iC(0); iC<11; iC++){
      if (iC == 10) {
        SuffixName = "_all";
        SuffixTitle = "";
      }
      else {
        SuffixName = Form("-%d",iC);
        SuffixTitle = Form(" Chip %d",iC);
      }
      
      fHToT[iS][iC]                = new TH1D(Form("ToT_%d%s",iS+1,SuffixName.Data()),
                                              Form("ToT in GTK%d%s;ToT [ns]; count",iS+1,SuffixTitle.Data()),256,0,200);
      fHTime[iS][iC]               = new TH1D(Form("Time_%d%s",iS+1,SuffixName.Data()),
                                              Form("Dt in GTK%d%s;t_{hit} - t^{tf}_{trigg} [ns]; count",iS+1, SuffixTitle.Data()),800,-100,100.);
      fHTimeProfile[iS][iC]        = new TH2F(Form("TimeProfile_%d%s",iS+1,SuffixName.Data()),
                                              Form("GTK%d%s;Time [s];t_{hit} - t^{tf}_{trigg} [ns]",iS+1,SuffixTitle.Data()),70,0,7,800,-100,100.);

      fHRawTime[iS][iC]            = new TH1D(Form("Raw_Time_%d%s",iS+1,SuffixName.Data()),
                                              Form("GTK%d%s;t^{raw}_{hit}  - t_{trigger}[ns]; count",iS+1,SuffixTitle.Data()),800,-100,100.);
      fHRawTimeProfile[iS][iC]     = new TH2F(Form("Raw_TimeProfile_%d%s",iS+1,SuffixName.Data()),
                                              Form("GTK%d%s;Time [s];t^{raw}_{hit}  - t_{trigger}[ns] [ns]",iS+1,SuffixTitle.Data()),70,0,7,800,-100,100.);

      fHNHitsPerTrigger[iS][iC]    = new TH1D(Form("NbHitsPerTrigger_%d%s",iS+1,SuffixName.Data()),
                                              Form("Nb Hits Per Trigger in GTK%d%s;Nb Hits / Nb Triggers; count",iS+1,SuffixTitle.Data()),100,-0.5,99.5);

      fHToTOOB[iS][iC]             = new TH1D(Form("OOB_ToT_%d%s",iS+1,SuffixName.Data()),
                                              Form("ToT in GTK%d%s;ToT [ns]; count",iS+1, SuffixTitle.Data()),256,0,200);
      fHTimeOOB[iS][iC]            = new TH1D(Form("OOB_Time_%d%s",iS+1,SuffixName.Data()),
                                              Form("Dt in GTK%d%s;t_{hit} - t_{trigger} [ns]; count",iS+1,SuffixTitle.Data()),800,-100,100.);
      fHNHitsPerTriggerOOB[iS][iC] = new TH1D(Form("OOB_NbHitsPerTrigger_%d%s",iS+1,SuffixName.Data()),
                                              Form(" Nb Hits Per Trigger in GTK%d%s;Nb Hits / Nb Triggers; count",iS+1,SuffixTitle.Data()),50,-0.5,49.5);

      fHNHitsPerTrigger[iS][iC]->StatOverflows(true);
      fHNHitsPerTriggerOOB[iS][iC]->StatOverflows(true);
    }
  }


  GetOrMakeDir(fHistoFile,"GigaTrackerMonitor/MC")->cd();
  fHTrueMomentum  = new TH1D("TrueMomentum","True Candidate Track Momentum; p [MeV/c]",200,60.,90.);
  fHTrueThetaX    = new TH1D("TrueThetaX","True Candidate Track X Angle; #theta_{X} [#murad]",500,300,2300.);
  fHTrueThetaY    = new TH1D("TrueThetaY","True Candidate Track Y Angle; #theta_{Y} [#murad]",500,-1000.,1000.);
  fHTrueMomentumX = new TH1D("TrueMomentumX","True Candidate Track Momentum (X); p_{X} [MeV/c]",500,0.,180.);
  fHTrueMomentumY = new TH1D("TrueMomentumY","True Candidate Track Momentum (Y); p_{Y} [MeV/c]",500,-75.,75.);
  fHTrueMomentumZ = new TH1D("TrueMomentumZ","True Candidate Track Momentum (Z); p_{Z} [MeV/c]",500,70000.,8000.);


  fHistoFile->cd("/");
}

// EOB Monitor
//==============================================
TH2F  * GigaTrackerReconstruction::GetCoarseHitMap(int station){
  if(station <0 || station >2) return NULL;
  return fHCoarseHitMap[station];
}

// Online Monitor Hooks
//==============================================
TH2F  * GigaTrackerReconstruction::GetHitMap(int station){
  if(station <0 || station >2) return NULL;
  return fHHitMap[station];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetAbsTimeProfile(int station){
  if(station <0 || station >2) return NULL;
  return fHAbsTimeProfile[station];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetAbsTimeProfileZoom(int station){
  if(station <0 || station >2) return NULL;
  return fHAbsTimeProfileZoom[station];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetToT(int station, int chip = 10){
  if(station <0 || station >2) return NULL;
  if(chip <0 || chip >10) return NULL;
  return fHToT[station][chip];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetTime(int station, int chip = 10){
  if(station <0 || station >2) return NULL;
  if(chip <0 || chip >10) return NULL;
  return fHTime[station][chip];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetNHitsPerTrigger(int station, int chip = 10){
  if(station <0 || station >2) return NULL;
  if(chip <0 || chip >10) return NULL;
  return fHNHitsPerTrigger[station][chip];
}

//==============================================
TH2F  * GigaTrackerReconstruction::GetHitMapOOB(int station){
  if(station <0 || station >2) return NULL;
  return fHHitMapOOB[station];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetAbsTimeProfileOOB(int station){
  if(station <0 || station >2) return NULL;
  return fHAbsTimeProfileOOB[station];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetAbsTimeProfileZoomOOB(int station){
  if(station <0 || station >2) return NULL;
  return fHAbsTimeProfileZoomOOB[station];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetToTOOB(int station, int chip = 10){
  if(station <0 || station >2) return NULL;
  if(chip <0 || chip >10) return NULL;
  return fHToTOOB[station][chip];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetTimeOOB(int station, int chip = 10){
  if(station <0 || station >2) return NULL;
  if(chip <0 || chip >10) return NULL;
  return fHTimeOOB[station][chip];
}

//==============================================
TH1D  * GigaTrackerReconstruction::GetNHitsPerTriggerOOB(int station, int chip = 10){
  if(station <0 || station >2) return NULL;
  if(chip <0 || chip >10) return NULL;
  return fHNHitsPerTriggerOOB[station][chip];
}

//==============================================
TH2F * GigaTrackerReconstruction::GetDtOverburst(int station){
  if(station <0 || station >2) return NULL;
  return fHTimeProfile[station][10];
}

//==============================================
Double_t GigaTrackerReconstruction::GetNoiseThreshold(){
  UInt_t NPeriodicTriggers =
    static_cast<NA62Reconstruction*>(fMainReco->GetMainReco())->GetNProcessedPeriodicTriggerEventsInFile();
  if (NPeriodicTriggers!=0) return 0.6*NPeriodicTriggers;
  return 300.;
}

//==============================================
void GigaTrackerReconstruction::SaveHistograms(){

  // RECO HISTOS
  fHistoFile->cd("GigaTrackerMonitor/Reco");

  for(Int_t iS=0; iS < 3 ;iS++){
    fHCoarseHitMap[iS]->Write();
    fHHitMap[iS]->Write();
    fHAbsTimeProfile[iS]->Write();
    fHAbsTimeProfileZoom[iS]->Write();
    fHHitMapOOB[iS]->Write();
    fHAbsTimeProfileOOB[iS]->Write();
    fHAbsTimeProfileZoomOOB[iS]->Write();
    for(int iC(0); iC<11; iC++){
      fHToT[iS][iC]->Write();
      fHTime[iS][iC]->Write();
      fHTimeProfile[iS][iC]->Write();
      fHRawTime[iS][iC]->Write();
      fHRawTimeProfile[iS][iC]->Write();
      fHNHitsPerTrigger[iS][iC] ->Write();
      fHToTOOB[iS][iC]->Write();
      fHTimeOOB[iS][iC]->Write();
      fHNHitsPerTriggerOOB[iS][iC] ->Write();   
    }
  }      

  // MC HISTOS
  fHistoFile->cd("GigaTrackerMonitor/MC");
  fHTrueMomentum->Write();
  fHTrueThetaX->Write();
  fHTrueThetaY->Write();
  fHTrueMomentumX->Write();
  fHTrueMomentumY->Write();
  fHTrueMomentumZ->Write();

  fHistoFile->cd("/");
}

void GigaTrackerReconstruction::ReconstructCandidates(TRecoVEvent*){
  // BUILD CANDIDATES
  EventHeader * rawHeader = NA62RecoManager::GetInstance()->GetEventHeader();
  vector<int> vT;
  for(Int_t i=0; i<fRecoEvent->GetNHits(); i++) {
    TRecoGigaTrackerHit* Hit = static_cast<TRecoGigaTrackerHit*>(fRecoEvent->GetHit(i));
    if (fabs(Hit->GetTime()-fRefTime)<=fTimeWindowTrigger) vT.push_back(i);
  }
  if (vT.size()>fNHitsMax){
    rawHeader->UpdateEventQualityMask(kGigaTracker);
    return;
  }

  TimeOrder    to(fRecoEvent);
  XOrder       xo(fRecoEvent);
  YOrder       yo(fRecoEvent);
  StationOrder so(fRecoEvent);

  // Regroup hit per Block
  // i.e. hits that could come from the same particle
  vector<vector<int>> vBt;
  vector<vector<int>>::iterator iBtxy;
  Clusterize(vT, vBt, fTimeWindow, to, 1); //make [time] block

  // Building Candidate from Block
  multimap<int, Cluster> mC;
  vector<vector<int>> aB, vCx, vCxy;
  vector<vector<int>>::iterator iS, iCx, iCxy;
  vector<int>::iterator iHit;
  for (iBtxy = vBt.begin(); iBtxy!=vBt.end(); ++iBtxy) { // { h1,h0,h0,h2... }
    mC.clear();
    // Clustering Hits
    aB.clear();
    // Split block in station{ (h0,h0..), (h1,h1..), (h2,h2..) }
    Clusterize(*iBtxy, aB, 0, so, 0);
    for (iS = aB.begin(); iS!=aB.end(); ++iS) { //iS contains hit from one station (h0,h0...)

      // Split iS in space (x then y) clusters
      vCx.clear(); vCxy.clear();
      Clusterize(*iS, vCx, 0.410, xo, 0);
      for(iCx = vCx.begin(); iCx!=vCx.end(); ++iCx)  Clusterize(*iCx, vCxy,0.410, yo, 0);

      // Merge hits in Cluster
      for (iCxy = vCxy.begin(); iCxy!=vCxy.end(); ++iCxy) {
        Cluster cluster(fRecoEvent);
        for (iHit=iCxy->begin(); iHit!=iCxy->end(); ++iHit) {
          cluster.add(*iHit);
        }
        mC.insert(pair<const int,Cluster>(cluster.S, cluster));
      }
    }

    // Building Candidate
    // Only one case for now: GTK123
    int NC0 = mC.count(0);
    int NC1 = mC.count(1);
    int NC2 = mC.count(2);
    int cType = 123;

    TRecoGigaTrackerCandidate* newCand;
    pair <multimap<int, Cluster>::iterator, multimap<int, Cluster>::iterator> ii0,ii1,ii2;
    multimap<int,Cluster>::iterator i0,i1,i2;
    //############## Only GTK 123 ##############
    if (NC0>0  && NC1>0  && NC2>0){
      ii0=mC.equal_range(0);  ii1=mC.equal_range(1);  ii2=mC.equal_range(2);
      for(i0=ii0.first; i0!=ii0.second; ++i0){
        for(i1=ii1.first; i1!=ii1.second; ++i1){
          for(i2=ii2.first; i2!=ii2.second; ++i2){
            Cluster cs[3] = {i0->second, i1->second, i2->second};
            newCand = static_cast<TRecoGigaTrackerCandidate*>(fRecoEvent->AddCandidate());
            for(int s(0); s<3; s++){
              TVector3 pos(cs[s].X,cs[s].Y,cs[s].Z);
              newCand->SetType(cType);
              newCand->SetPosition(s,pos);
              newCand->SetTimeStation(s,cs[s].T);
              for(iHit = cs[s].hits.begin();iHit != cs[s].hits.end(); ++iHit) newCand->AddHit(*iHit);
            }

            BuildCandidate(newCand);

            //tests a la giuseppe
            double pkaon = newCand->GetMomentum().Mag();
            double dxdz = newCand->GetMomentum().X()/newCand->GetMomentum().Z();
            double dydz = newCand->GetMomentum().Y()/newCand->GetMomentum().Z();
            double chi2Time = newCand->GetChi2Time();

            if (pkaon<72000 || pkaon>78000 ||
                dxdz>0.0016 || dxdz<0.0009 ||
                dydz>0.0004 || dydz<-0.0003||
                chi2Time>fChi2T){
              fRecoEvent->RemoveCandidate(fRecoEvent->GetNCandidates()-1);
            }
          }
        }
      } 
    }
    mC.clear();
  }
}

void GigaTrackerReconstruction::BuildCandidate(TRecoGigaTrackerCandidate* cand){
  Double_t BLbend    = -1.0 * fParameters->GetGigaTrackerMCBMagnetFieldStrength(0) * 
                       fParameters->GetGigaTrackerMCBMagnetLength().Z();
  Double_t DeltaBend = fParameters->GetGigaTrackerMCBMagnetPosition(1).Z() - 
                       fParameters->GetGigaTrackerMCBMagnetPosition(0).Z();
  Double_t Beta      = 1e-3 * fClight * BLbend * DeltaBend;
  Double_t BLtrim    = fParameters->GetGigaTrackerMDXMagnetFieldStrength() * 
                       fParameters->GetGigaTrackerMDXMagnetLength().Z();
  Double_t Delta12   = fParameters->GetGigaTrackerStationPositionRaw(1).Z() - 
                       fParameters->GetGigaTrackerStationPositionRaw(0).Z();
  Double_t Delta13   = fParameters->GetGigaTrackerStationPositionRaw(2).Z() - 
                       fParameters->GetGigaTrackerStationPositionRaw(0).Z();
  Double_t Delta23   = fParameters->GetGigaTrackerStationPositionRaw(2).Z() - 
                       fParameters->GetGigaTrackerStationPositionRaw(1).Z();
  Double_t X[3], Xshift[3], Y[3], T[3];
  for (Int_t iStation=0; iStation<fNStations; iStation++) {
    X[iStation] = cand->GetPosition(iStation).X();
    Xshift[iStation] = X[iStation];
    Y[iStation] = cand->GetPosition(iStation).Y();
    T[iStation] = cand->GetTimeStation(iStation); // ns
  }
  Double_t alpha = (fParameters->GetGigaTrackerStationPositionRaw(1).Z() - 
                    fParameters->GetGigaTrackerStationPositionRaw(0).Z()) / 
                   (fParameters->GetGigaTrackerStationPositionRaw(2).Z() - 
                    fParameters->GetGigaTrackerStationPositionRaw(0).Z());
  Double_t p = Beta / (Y[0] * (1.0-alpha) - Y[1] - fParameters->GetGigaTrackerStationPositionRaw(1).Y() + (alpha*Y[2]));
  Double_t ShiftTrim[3] = {0.,0.,0.};
  for (Int_t i=0; i<fNStations; i++) {
    Double_t DeltaTrim = fParameters->GetGigaTrackerStationPositionCorrected(i).Z() - 
                         fParameters->GetGigaTrackerMDXMagnetPosition().Z();
    ShiftTrim[i] = -((1e-3*fClight * BLtrim) / p) * DeltaTrim;
    if (i!=(fNStations-1)) Xshift[i] += ShiftTrim[i];   // Correct the TRIM5 effect for GTK1 and 2
  }

  // ---  Candidate Time
  // Double_t Time = (T[0] + T[1] + T[2]) / 3.0; //ns (old implemenation)
  // Weighted average to deal with double pixel hits 
  // 1/ count pixels per hit
  Int_t nPixels[3]={0,0,0};
  Bool_t IsPileUpHit[3] = {false, false, false};
  std::vector<Int_t> HitIndex[3];
  for (int iH=0; iH<cand->GetNHits(); iH++) {
    TRecoGigaTrackerHit * fGTKHit = static_cast<TRecoGigaTrackerHit*>(cand->GetHit(iH));
    Int_t iS = fGTKHit->GetStationNo();
    nPixels[iS]++;
    HitIndex[iS].push_back(iH);
    if ( fGTKHit->GetIsPileUpHit() ) IsPileUpHit[iS] = true; 
  }	
  // 2/ assign to the time measurement in each station an uncertainty based on the number of pixel/hit
  // DATA (2017)
  // 1-pixel hit: 0.135 ns (default)
  // 2-pixel hit: 0.180 ns (+35% degradation) 
  // pile-up hit: 0.600 ns (educated guess...)
  Double_t errT[3]={0.125, 0.125, 0.125}; // by default
  for (Int_t iS=0; iS<fNStations; iS++) {
    if (fMC) {
      errT[iS] = fParameters->GetSigmaT(iS); // in order to match the smearing in digitizer 
      continue;  
    }
    errT[iS] = nPixels[iS]==1 ? 0.135 : 0.180;
    if ( IsPileUpHit[iS] ) errT[iS] = 0.600;
  }

  // 3/ weighted average
  Double_t wSum(0.), tSum(0.);
  for (Int_t iS=0; iS<3; iS++) {
    wSum += 1./(errT[iS]*errT[iS]);
    tSum += T[iS]/(errT[iS]*errT[iS]);
  }  
  Double_t Time = tSum/wSum; // ns
  Double_t sigmaT = 1./TMath::Sqrt(wSum);

  // Constraints on relative cluster times
  Double_t chi2Time = TMath::Power((T[0] - Time)/errT[0], 2.) + TMath::Power((T[1] - Time)/errT[1], 2.) + TMath::Power((T[2] -Time)/errT[2], 2.);

  // --- Candidate ZX view
  // Track fitting to straight lines for horizontal view (X), with specific offset for GTK1 and GTK2 (trim effect corrected)
  Double_t a,b,rho,chi2X;
  Double_t sigmaX[3];
  for (int iS=0; iS<3; iS++) { // begin of loop on iS
    // 1-pixel hit
    if ( nPixels[iS]==1 ) {
      sigmaX[iS] = 0.0866; // 300um pixel
      // 400um pixel?
      Int_t itH = HitIndex[iS][0];
      TRecoGigaTrackerHit * fGTKHit = static_cast<TRecoGigaTrackerHit*>(cand->GetHit(itH));
      Int_t iCol = fGTKHit->GetColumn();
      if ( iCol>0 && iCol<199 && (iCol%40==0 || iCol%40==39) ) {
  	    sigmaX[iS]=0.1155;
      } 
    }
    else  sigmaX[iS] = 0.0866; 
  } // end of loop on iS

  // Add Multiple Scattering
  // was: GTK2 = 680e-3; GTK3 = 580e-3, assuming cooling plate thicknesses as 380mum and 280mum respectively 
  Double_t GTKthickness[2] = {0.,0.}; // thickness in mm for GTK2 and GTK3 = Z_chip + Z_sensor + Z_cooling_plate  
  Double_t theta[2];
  for (Int_t i=0; i<2; i++){
    GTKthickness[i] = fParameters->GetGigaTrackerSensorLength(i+1).Z() + 
                      fParameters->GetGigaTrackerChipLength(i+1).Z() + 
                      fParameters->GetCoolingPlateLength(i+1).Z() - 
                      fParameters->GetCoolingPlateBottomDepth(i+1) - 
                      fParameters->GetCoolingPlateTopDepth(i+1);
    theta[i]        = 13.6e6/p * TMath::Sqrt(GTKthickness[i]/93.7) *
                      (1 + 0.038 * TMath::Log(GTKthickness[i]/93.7));
  }

  sigmaX[1] = TMath::Sqrt( sigmaX[1]*sigmaX[1]
  			   + 0.002*ShiftTrim[1]*0.002*ShiftTrim[1]
  			   + (theta[1] * Delta23)*(theta[1] * Delta23));
  sigmaX[0] = TMath::Sqrt( sigmaX[0]*sigmaX[0]
  			   + 0.002*ShiftTrim[0]*0.002*ShiftTrim[0]
  			   + (theta[1] * Delta13)*(theta[1] * Delta13)
  			   + (theta[0] * Delta12)*(theta[0] * Delta12));


  Double_t z[3] = {0, Delta12, Delta13};
  LinearLeastSquareFit(z,Xshift,3,sigmaX,a,b,rho,chi2X);
  //Double_t dxdz = (X[2] - X[0]) / fDelta13 - (((1e-3*fClight * fBLtrim) / p) * (1. - (fDeltaTrim[2] / fDelta13)));


  // Compute kinematics and time
  Double_t dydz = (Y[2] - Y[0]) / Delta13;
  Double_t dxdz = b;
  Double_t pz = p / TMath::Sqrt(1. + dxdz * dxdz + dydz * dydz);
  TVector3 Momentum;
  Momentum.SetXYZ(1e-6*pz * dxdz, 1e-6*pz * dydz, 1e-6*pz);


  // Constraints on vertical view (Y)
  Double_t sigmaY12 = 1.42;
  Double_t sigmaY23 = 1.20;
  Double_t chi2Y = TMath::Power((Y[1] - Y[0])/sigmaY12 , 2.) + TMath::Power((Y[2] - Y[1])/sigmaY23 , 2.);

  // Global Chi2
  Double_t chi2 = chi2X + chi2Y + chi2Time;

  // Candidate
  cand->SetMomentum(Momentum);
  cand->SetTime(Time);
  cand->SetChi2X(chi2X);
  cand->SetChi2Y(chi2Y);
  cand->SetChi2Time(chi2Time);
  cand->SetChi2(chi2);
  cand->SetTimeError(sigmaT);
}




void GigaTrackerReconstruction::LinearLeastSquareFit(Double_t *x, Double_t *y, Int_t Nsample, Double_t *sigma, Double_t &a, Double_t &b, Double_t &rho, Double_t &chi2){
// least square method applied to straight line (Y = a + b*X) weighted by the errors
    Double_t sumx = 0.;
    Double_t sumy = 0.;
    Double_t sumxy= 0.;
    Double_t sumxx= 0.;
    Double_t sums = 0.;

    // accumulate
    for(Int_t i=0; i < Nsample ; i++){
      Double_t wgt = 1./(sigma[i]*sigma[i]);
      sumx  += x[i]*wgt;
      sumxx += x[i]*x[i]*wgt; 
      sumy  += y[i]*wgt; 
      sumxy += x[i]*y[i]*wgt; 
      sums  += wgt;
    }
    a = (sumxx*sumy  - sumx*sumxy) / (sums*sumxx - sumx*sumx);
    b = (sums *sumxy - sumx*sumy ) / (sums*sumxx - sumx*sumx);

    rho = - sumx / TMath::Sqrt(sums*sumxx);

    chi2 = 0;
    for(Int_t i=0; i < Nsample ; i++){
      chi2 += ((y[i] - a - b*x[i])/sigma[i]) * ((y[i] - a - b*x[i])/sigma[i]) ;
    }

    return;
}


template<typename Order> void GigaTrackerReconstruction::Clusterize
(vector<int> v, vector<vector<int>>& clusters, double minDist, Order order, int minCont) {
  if (v.size()==0) return;

  std::sort(v.begin(),v.end(),order); //sort first mpt 13/12/2016

  vector<int> aCluster;
  vector<int>::iterator iV = v.begin(); //sort first mpt 13/12/2016

  while(iV!=v.end()){
    //seed
    aCluster.clear();
    int seed = (*iV);
    //clusterize
    while(iV!=v.end() && order.dist(seed,(*iV))<=minDist ) {
      aCluster.push_back(*iV);
      seed = (*iV); // mpt 13/12/2016
      ++iV;
    }
    //store clusters
    if(aCluster.size()>(UInt_t)minCont) clusters.push_back(aCluster);
    aCluster.clear();
  }
  return;
}
