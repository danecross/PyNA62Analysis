//
// Created by B. Velghe (bob.velghe@cern.ch) - Dec. 2014
// Modified by M. Perrin-Terrin (mathieu.perrin-terrin@cern.ch) - Jun 2015
//
// To be done (M. Perrin-Terrin - Sep 2015):
//  -include the printout level into RG_Utilities
//  -keep track decoding error and store then in histos
//  -add delay and sensor orientation in the DB
//  -write a new class for TGigaTrackerDigiEvent (now it is simply a typedef)
//
// Modified by A. Kleimenova (alina.kleimenova@cern.ch) - Feb 2018
// NOTE: To keep hits of noisy pixels change fMaskNoisyPixels=true to
// fMaskNoisyPixels=false
//
/// \class GTKRawDecoder
/// \Brief
/// Decoder for Trigger Matched GTK Data - Format of September 11th 2015
/// \EndBrief

#include "NA62BufferProto.hh" //for isL0SpecialTrigger
#include "NA62ConditionsService.hh"

#include "GTKRawDecoder.hh"
#include "TGigaTrackerDigi.hh"
#include "NA62Reconstruction.hh"
#include "GigaTrackerReconstruction.hh"
#include "TDigiGigaTrackerError.hh"
#include "TGigaTrackerSpecialTriggerEvent.hh"
#include "TGigaTrackerDigiEvent.hh"
#include "NA62BufferProto.hh"

#include "RG_Utilities.hh" //FIXME
#include "GTKTypeDefs.hh"
#include "GigaTrackerHitsBlock.hh"
#include <vector>
#include "GigaTrackerErrorsHandler.hh"
#include "TRegexp.h"
#include <fstream>
#include <ostream>

using namespace GTK;
using namespace std;

GTKRawDecoder::GTKRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "GTK", 1) {
  fMaskNoisyPixels=true;    
  fNoisyPixelListName = "";   
  fReadNoisyPixelList.clear();
  fDataFormat = 0;
  for(int i(0);i<17;i++) fLimiterThreshold[i] = 20;
  for(int i(0);i<3;i++)  fOffTimeStamp[i] = 0;

  //----------- Init common parameters and instances ----------//

  NA62VRawDecoder::CreateObjects();
  ParseRawDecoderSettingsFile(fReco->GetRawDecoderSettingsFileName());

  mGTKBlock  = new GTK::GigaTrackerNa62DataBlock(fDataFormat);
  fDigiEvent = new TGigaTrackerDigiEvent(TGigaTrackerDigi::Class());

  fSpecialTriggerEvent = new TGigaTrackerSpecialTriggerEvent(TSpecialTrigger::Class());
  fErrorHeaderHitTS  = GTK::GigaTrackerErrorsHandler::GetInstance()->RegisterError("GTKRawDecoder_ErrorIncTSHitHeader");  

  fErrorIncChip = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GTKRawDecoder_IncChip");
  fErrorIncSubId = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GTKRawDecoder_SubId");
  fErrorMissingChip = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GTKRawDecoder_MissingChip");

  GigaTrackerErrorsHandler::GetInstance()->RegisterChipsError("GTKRawDecoder_FCLSB",fErrorIncFCLSB);
  GigaTrackerErrorsHandler::GetInstance()->RegisterChipsError("GTKRawDecoder_TS",fErrorIncTS);
  GigaTrackerErrorsHandler::GetInstance()->RegisterChipsError("GTKRawDecoder_EvtNb",fErrorIncEvtNb);
  GigaTrackerErrorsHandler::GetInstance()->RegisterChipsWarning("GTKRawDecoder_Limiter",fErrorLimiter);
  GigaTrackerErrorsHandler::GetInstance()->RegisterChipsError("GTKRawDecoder_ErrorIncNTrigg",fErrorIncNTrigg);
  GigaTrackerErrorsHandler::GetInstance()->RegisterChipsError("GTKRawDecoder_DuplicatedHits",fErrorDuplicated);
  //fErrorIncTrigger = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GTKRawDecoder_Trigger");

  //Call this class constructor to register all possible erros
  GigaTrackerDAQBoardTimeStamp ts;
  GigaTrackerDAQBoardTrailerOne t1;
  GigaTrackerDAQBoardTrailerTwo t2;
  GigaTrackerDAQBoardTrailerThree t3;
  GigaTrackerNa62DataBlock db; //this calls HeaderL0/L1, 

  int nbErrors = GTK::GigaTrackerErrorsHandler::GetInstance()->GetNErrors() +  GTK::GigaTrackerErrorsHandler::GetInstance()->GetNChipErrors();

  std::cout<<"Nb Errors Monitored: "<<nbErrors<<std::endl;
  fHDecoderErrors = new TH2F("DecoderErrors","Errors in Decoding GTK Data",31, -0.5,30.5,nbErrors, 0.5, float(nbErrors)+0.5);
  GTK::GigaTrackerErrorsHandler::GetInstance()->SetErrorHisto(fHDecoderErrors);

  for (int iS(0); iS<3; iS++)
    fChannelProfile[iS] = new TH1F
      (Form("ChannelProfileGTK%d",iS+1),
       Form("ChannelProfileGTK%d for periodic triggers, per trigger",iS+1), 18000, -0.5, 17999.5);

  for(int i(0);i<3;i++){
    for(int k(0);k<18000;k++)    fT0[i][k]=0; //pixel tzero
  }
  LoadTW();
  LoadT0();
}

GTKRawDecoder::~GTKRawDecoder() {
  delete mGTKBlock;
  for(int i(0);i<3;i++){
    for(int j(0);j<10;j++){
      fTW[i][j]->Delete();
    }
  }   
}


//==============================================
void GTKRawDecoder::LoadTW() {
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

void GTKRawDecoder::LoadT0() {
  TString Line;
  //T0
  if(NA62ConditionsService::GetInstance()->Open(fReco->GetT0FileName())!=kSuccess) return;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fReco->GetT0FileName()))){
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
  NA62ConditionsService::GetInstance()->Close(fReco->GetT0FileName());      
}

Int_t GTKRawDecoder::GetNCriticalErrorTypes() {
  if (!fHDecoderErrors) return 0;
  return GTK::GigaTrackerErrorsHandler::GetInstance()->GetNbFatalErrors(); //all the errors are considered criticals
}

void GTKRawDecoder::StartOfBurst() { // called for RawData only
  for (Int_t i=0; i<3; i++) fChannelProfile[i]->Reset();
  fReadNoisyPixelList.clear();
  if (fMaskNoisyPixels){
    fNoisyPixelListName = "GigaTracker-NoisyPixels.dat";
    if(NA62ConditionsService::GetInstance()->Open(fNoisyPixelListName)==kSuccess){
      std::cout << "[GTKRawDecoder] Reading List of noisy pixels " << std::endl;
      TString Line;
      UInt_t SID=-1;
      int NNoisy=0;
      int UID=-1;
      Bool_t BurstFound=false;
      while((Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fNoisyPixelListName))) && !BurstFound){
        if (Line.BeginsWith("#")) continue;
        TObjArray *tok = Line.Tokenize(" ");
        Int_t ReadRunID=0, ReadBurstID=0;
        if(tok->GetEntries()>0) ReadRunID     = static_cast<TObjString*>(tok->At(0))->GetString().Atoi();
        if(tok->GetEntries()>1) ReadBurstID   = static_cast<TObjString*>(tok->At(1))->GetString().Atoi();
        if(ReadRunID==fDigiEvent->GetRunID() && ReadBurstID==fDigiEvent->GetBurstID()){
          if(tok->GetEntries()>2) NNoisy = static_cast<TObjString*>(tok->At(2))->GetString().Atoi();
          for (int i(0); i<NNoisy; i++){
            SID = static_cast<TObjString*>(tok->At(3+3*i))->GetString().Atoi();
            UID = static_cast<TObjString*>(tok->At(4+3*i))->GetString().Atoi();
            fReadNoisyPixelList.push_back(std::make_pair(SID, UID));
          }
        }
        else BurstFound=true;
        delete tok;
      }
      NA62ConditionsService::GetInstance()->Close(fNoisyPixelListName);
    }
  }
}

void GTKRawDecoder::EndOfBurst() { // called for RawData only; not called with "-n XXX"
  // Normalize channel profiles per trigger
  UInt_t NPeriodicTriggers =
    static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetNProcessedPeriodicTriggerEventsInFile();
  Double_t ScaleFactor = 1.0/NPeriodicTriggers;
  for (Int_t iS=0; iS<3; iS++) {
    fChannelProfile[iS]->Scale(ScaleFactor);
    for (Int_t iX=0; iX<18000; iX++) fChannelProfile[iS]->SetBinError(iX, 0.0);
  }

  ///////////////////////////////////////////////////////////////////
  // Create the list of noisy pixels from out-of-beam channel profile
  // E Goudzovski (8 Feb 2018)
  if (!fReadNoisyPixelList.size()){
    std::ofstream outfile(Form("GigaTracker-NoisyPixels.run%06d_%04d-run%06d_%04d.dat",
                               fDigiEvent->GetRunID(),fDigiEvent->GetBurstID(),fDigiEvent->GetRunID(),fDigiEvent->GetBurstID()));
    outfile << "### [GTKRawDecoder] List of noisy pixels for run "
      << fDigiEvent->GetRunID() << ", burst "<< fDigiEvent->GetBurstID() << std::endl;
    outfile << "# Format: RunID BurstID fNNoisyPixels StationID UID Rate" << std::endl;
    std::vector<std::pair<Int_t, Int_t>> fWriteNoisyPixelList;

    outfile << Form("%06d", fDigiEvent->GetRunID()) << " " << Form("%04d",fDigiEvent->GetBurstID()) << " ";
    for (Int_t iS=0; iS<3; iS++) {
      for (Int_t iX=0; iX<18000; iX++) {
        if (fChannelProfile[iS]->GetBinContent(iX+1) > 0.06) { // max number of hits per trigger; UPD changed from 0.5% to 6% 
          fWriteNoisyPixelList.push_back(std::make_pair(iS, iX));
        }
      }
    }
    outfile << fWriteNoisyPixelList.size();
    for (UInt_t i(0); i<fWriteNoisyPixelList.size();i++)
      outfile << " " << fWriteNoisyPixelList[i].first << " " << fWriteNoisyPixelList[i].second << " "
							<< fChannelProfile[fWriteNoisyPixelList[i].first]->GetBinContent(fWriteNoisyPixelList[i].second+1); 
    outfile << std::endl;
    outfile.close();

  }
}

TDetectorVEvent * GTKRawDecoder::DecodeNextEvent(UInt_t* inBuff, EventHeader* Header, UInt_t* NextOffset) {

  GigaTrackerErrorsHandler::GetInstance()->SetEvent(static_cast<TGigaTrackerDigiEvent*>(fDigiEvent));

  Bool_t FlagSpecialTrigger=kFALSE;
  GTK::GigaTrackerErrorsHandler::GetInstance()->SetOutputLevel(fWarningsLevel);
  fDigiEvent->Clear();
  fDigiEvent->SetTimeStamp(Header->GetTimeStamp());
  fDigiEvent->SetStartByte(Header->GetStartByte());
  fDigiEvent->SetIsMC(kFALSE);
  fDigiEvent->SetID(Header->GetEventNumber());
  fDigiEvent->SetBurstID(Header->GetBurstID());
  fDigiEvent->SetRunID(Header->GetRunID());
  fDigiEvent->SetTriggerType(Header->GetTriggerType());

  if(isL0SpecialFrame(Header->GetTriggerType()<<2)) {
    FlagSpecialTrigger=kTRUE;
    fSpecialTriggerEvent->Clear();
    fSpecialTriggerEvent->SetTimeStamp(Header->GetTimeStamp());
    fSpecialTriggerEvent->SetStartByte(Header->GetStartByte());
    fSpecialTriggerEvent->SetIsMC(kFALSE);
    fSpecialTriggerEvent->SetID(Header->GetEventNumber());
    fSpecialTriggerEvent->SetBurstID(Header->GetBurstID());
    fSpecialTriggerEvent->SetRunID(Header->GetRunID());
    fSpecialTriggerEvent->SetTriggerType(Header->GetTriggerType()<<2);
  }

  // ALL TRIGGERS
  UInt_t* Current = inBuff;
  int CurrentOffset(0);
  GTK::GigaTrackerHitsBlock* hits;
  GTK::GigaTrackerNa62HeaderBase* header;

  int NHitsPerHalfROBoard[fNROMezzanines][2];
  memset(NHitsPerHalfROBoard,0,sizeof(NHitsPerHalfROBoard));

  const int buffLen = 1000;
  char errBuff[buffLen];
  int nPC(0);
  while(Current < NextOffset){
    nPC++;
    mGTKBlock->Clear();
    GTK::u8* Buffer = reinterpret_cast<GTK::u8*>(Current);
    int rv = mGTKBlock->ReadFromBuffer(Buffer,CurrentOffset);
    Current = Current + CurrentOffset/4;


    if(rv!=0){
      int len = sprintf(errBuff,"Error (%d) in reading GTK data block concerned:\n",rv);
      for(int iB=0 ; iB<=Current-inBuff; iB = iB+2) {
        len+=snprintf(errBuff+len,buffLen-len,"  0x%8.8x ", inBuff[iB]);
        len+=snprintf(errBuff+len,buffLen-len,"  0x%8.8x \n", inBuff[iB+1]);
        if(len>buffLen-40) {
          snprintf(errBuff+len,buffLen-len,"...");
          break;
        }
      }
      REPORT_ERROR(rv,errBuff);
      if(rv > 0 ) { //positive error code are Errors negative are Warnings
        fNCriticalErrors[0]++;
        if (FlagSpecialTrigger) return fSpecialTriggerEvent;
        return fDigiEvent;
      }
    }

    hits = mGTKBlock->GetGigaTrackerHitsBlock();
    header = mGTKBlock->GetHeader();

    std::vector<GTK::GigaTrackerDAQBoardTimeStamp*> timeStamps =  hits->GetTimeStamps();;
    std::vector<GTK::GigaTrackerDAQBoardTimeStamp*>::iterator iTS;
    for(iTS = timeStamps.begin(); iTS != timeStamps.end(); ++iTS){
      for(int pu(0); pu < ((*iTS)->GetNbPileUpAddress() + (*iTS)->GetNbHitArbiterAddress()) ;++pu){
        Bool_t IsNoisy=false;
        for (UInt_t i(0);i<fReadNoisyPixelList.size(); i++){
          if ((*iTS)->GetPixelUID(pu) == fReadNoisyPixelList[i].second &&
              ((*iTS)->GetStationId()-1) == fReadNoisyPixelList[i].first){
            IsNoisy=true;
            break;
          }		
        }	  
        if (!IsNoisy){
          TGigaTrackerDigi *GTKDigi = static_cast<TGigaTrackerDigi*>(fDigiEvent->AddHit());
          // DIGIS CREATION
          // first the hits, then the pile ups

          GTKDigi->SetChipID            ( (*iTS)->GetChipId());
          GTKDigi->SetStationNo         ( (*iTS)->GetStationId()-1);
          GTKDigi->SetFrameCounter      ( (*iTS)->GetFrameCounter() );
          GTKDigi->SetPixelAddress      ( (*iTS)->GetPixelAddress() ); 
          GTKDigi->SetHitArbiterAddress ( (*iTS)->GetHitArbiterAddress() );
          GTKDigi->SetPileUpAddress     ( (*iTS)->GetPileUpAddress() );
          GTKDigi->SetLeadingSelector   ( (*iTS)->GetLeadingSelector() );
          GTKDigi->SetLeadingCoarse     ( (*iTS)->GetLeadingCoarse() ); 
          GTKDigi->SetLeadingFine       ( (*iTS)->GetLeadingFine() );
    
          GTKDigi->SetTotCoarse         ( (*iTS)->GetTotCoarse() ); 
          GTKDigi->SetTotFine           ( (*iTS)->GetTotFine() ); 
          GTKDigi->SetSourceId          ( 0x08 );
          GTKDigi->SetLeadingEdge       ( (*iTS)->GetLeadingTime() );  //ns
          GTKDigi->SetTrailingEdge      ( (*iTS)->GetTrailingTime() );  //ns
          GTKDigi->SetTotSelector       ( (*iTS)->GetTotSelector() );

          GTKDigi->SetIsPileUp          ( false );

          //parity is normally 0 or 1, put to to tell that it has no meaning
          if( (*iTS)->GetNbHitArbiterAddress() > 1 || pu >= (*iTS)->GetNbHitArbiterAddress() ){
            GTKDigi->SetIsPileUp        ( true );
            GTKDigi->SetTrailingEdge    ( (*iTS)->GetLeadingTime() + 15.00 );  //ns
          }

          GTKDigi->SetAbsLeadingEdge    ( (*iTS)->GetLeadingTime()  + (*iTS)->GetL1ATimeStamp()*ClockPeriod );  //ns
          GTKDigi->SetAbsTrailingEdge   ( (*iTS)->GetTrailingTime() + (*iTS)->GetL1ATimeStamp()*ClockPeriod );  //ns

          GTKDigi->SetPixelID           ( (*iTS)->GetPixelUID(pu) );
          GTKDigi->EncodeChannelID();

          Double_t Time = (*iTS)->GetLeadingTime();
          if(static_cast<GigaTrackerReconstruction*>(fReco)->GetEnableRawTimeCorrections()){ // Correct raw time with FineT0 and TimeWalk corrections
            Double_t ToT = (*iTS)->GetTrailingTime() - (*iTS)->GetLeadingTime();
            int iS = GTKDigi->GetStationNo();
            int iC = GTKDigi->GetChipID();
            int iPixID = GTKDigi->GetPixelID();
            Time = Time - fTW[iS][iC]->Eval(ToT);
            Time = Time - fT0[iS][iPixID];
          }
          GTKDigi->SetTime(Time);

          // For periodic triggers, record the channel for noisy pixel identification
          if((Header->GetTriggerType()&0xff)==0x08 || (Header->GetTriggerType()&0xff)==0x31){ //0x08 for 2016
            int pixel =  GTKDigi->GetChannelID();
            int gtk   =  int(pixel)/100000;
            int chip  = (int(pixel)%100000)/10000;
            int pix   =  int(pixel)%10000;
            int x     = pix%40 + (chip%5)*40;
            int y     = pix/40 + (chip/5)*45;
            int uid   = x + y*200;
            fChannelProfile[gtk]->Fill(uid);
          }
        }
      }
    }

    // set the hit counts between l0
    if (!FlagSpecialTrigger) {
      std::vector<GTK::GigaTrackerDAQBoardTrailerTwo*> trailerTwos =  hits->GetTrailersTwo();
      std::vector<GTK::GigaTrackerDAQBoardTrailerTwo*>::iterator iT2;
      int gtk = header->GetSourceSubID()/2;
      int halves[3][10];
      for(int i(0); i<3;i++){
        for(int j(0); j<10;j++){
          halves[i][j] = 0;
        }
      }

      for(iT2 = trailerTwos.begin(); iT2 != trailerTwos.end(); ++iT2){
        int chip = (*iT2)->GetChipId();
        static_cast<TGigaTrackerDigiEvent*>(fDigiEvent)->SetHitNbFromPrevL0(gtk, chip, halves[gtk][chip], (*iT2)->GetHitNbFromPrev());
        halves[gtk][chip] = 1;
      }
    }

    //EOB Trigger
    if ( (Header->GetTriggerType()&0xFF) == 0x23 || (Header->GetTriggerType()&0xFF) == 0x22 ) {

      TSpecialTrigger *SpecTrig= static_cast<TSpecialTrigger*>( fSpecialTriggerEvent->AddSpecialTrigger());
      SpecTrig->SetTimeStamp(Header->GetTimeStamp());
      SpecTrig->SetTriggerType(Header->GetTriggerType()<<2);

      //specific to GTK FW version and Hits Number
      //set the hit counts between l0
      if ( (Header->GetTriggerType()&0xFF) == 0x23 ) {
        std::vector<GTK::GigaTrackerDAQBoardTrailerThree*> trailerThrees =  hits->GetTrailersThree();
        std::vector<GTK::GigaTrackerDAQBoardTrailerThree*>::iterator iT3;
        int gtk = header->GetSourceSubID()/2;
        int halves[3][10];
        for(int i(0); i<3;i++){
          for(int j(0); j<10;j++){
            halves[i][j] = 0;
          }
        }

        for(iT3 = trailerThrees.begin(); iT3 != trailerThrees.end(); ++iT3){
          int chip = (*iT3)->GetChipId();
          static_cast<TGigaTrackerSpecialTriggerEvent*>(fSpecialTriggerEvent)->SetNHits(gtk, chip, halves[gtk][chip], (*iT3)->GetHitNb());
          static_cast<TGigaTrackerSpecialTriggerEvent*>(fSpecialTriggerEvent)->SetFW(gtk, chip, halves[gtk][chip], (*iT3)->GetFW());
          halves[gtk][chip] = 1;
        }
      }
    }

    // CONSISTENCY CHECKS
    rv = ConsistencyCheck(Header);
    if(rv!=0){
      int len = sprintf(errBuff,"Error (%d) in Consistency Check, data block concerned:\n",rv);
      for(int iB=0 ; iB<Current-inBuff; iB = iB+4) {
        len+=snprintf(errBuff+len,buffLen-len,"  0x%8.8x ", inBuff[iB]);
        len+=snprintf(errBuff+len,buffLen-len,"  0x%8.8x ", inBuff[iB+1]);
        len+=snprintf(errBuff+len,buffLen-len,"  0x%8.8x ", inBuff[iB+2]);
        len+=snprintf(errBuff+len,buffLen-len,"  0x%8.8x \n", inBuff[iB+3]);

        if(len>buffLen-20) {
          snprintf(errBuff+len,buffLen-len,"  ...");
          break;
        }
      }
      REPORT_ERROR(rv,errBuff);
      if(rv>0) fNCriticalErrors[0]++;
    }

    //CLEANING
    hits->Clear();
    header->Clear();
  }

  // Check there are 6 PC
  if(nPC!=6){
    sprintf(errBuff, "Missing PC: %d instead of 6",nPC);
    REPORT_NEW_ERROR(fErrorMissingChip,errBuff);
    fNCriticalErrors[0]++;
  }
  if (FlagSpecialTrigger) return fSpecialTriggerEvent;
  return fDigiEvent;
}

void GTKRawDecoder::ParseRawDecoderSettingsFile(TString RawDecFileName){
  TString Line;
  if(NA62ConditionsService::GetInstance()->Open(RawDecFileName)!=kSuccess) return;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(RawDecFileName))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if(Line.BeginsWith("DataFormat=")){
      fDataFormat = TString(Line(TRegexp("[0-9]+"))).Atoi();
      std::cout<<"GTK Format is set to L"<<fDataFormat<<std::endl;
    }
    else if (Line.BeginsWith("LimitersThresholds")){
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries();
      if(NEntries != 32) std::cout<<"Missing limiter threshold value for GTK, assign them to default value"<<std::endl;
      for(int ii(2); ii<NEntries; ii++){
        fLimiterThreshold[ii-2] = static_cast<TObjString*>(l->At(ii))->GetString().Atoi();
      }
    }
    else if (Line.BeginsWith("OffsetTimeStamp")){
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries();
      if(NEntries != 5) std::cout<<"Missing offset time stamp for GTK, assign them to default value"<<std::endl;
      for(int ii(2); ii<NEntries; ii++){
        fOffTimeStamp[ii-2] = static_cast<TObjString*>(l->At(ii))->GetString().Atoi();
        std::cout<<"Time Stamp offset GTK "<<ii-2<<": "<<fOffTimeStamp[ii-2]<< " (for RawDecoder consistency checks)"<<std::endl;
      }
    }
  }
  NA62ConditionsService::GetInstance()->Close(RawDecFileName);
}

int GTKRawDecoder::ConsistencyCheck( EventHeader* rawHeader ){
  char errBuff[200];
  int rv(0);

  GTK::GigaTrackerHitsBlock* hits = mGTKBlock->GetGigaTrackerHitsBlock();
  GTK::GigaTrackerNa62HeaderBase* header = mGTKBlock->GetHeader();

  std::vector<GTK::GigaTrackerDAQBoardTimeStamp*> timeStamps =  hits->GetTimeStamps();;
  std::vector<GTK::GigaTrackerDAQBoardTimeStamp*>::iterator iTS;
  std::vector<GTK::GigaTrackerDAQBoardTimeStamp*>::iterator iTS2;

  int subId = header->GetSourceSubID();

  unsigned int evtTS = rawHeader->GetTimeStamp();
  unsigned int evtNb = rawHeader->GetEventNumber();

  //TS based check
  for(iTS = timeStamps.begin(); iTS != timeStamps.end(); ++iTS){
    int chip = (*iTS)->GetChipId();
    int station = (*iTS)->GetStationId()-1;

    //check TS chipId are consistent with T1 and T2 chipId
    if(chip != int((*iTS)->GetTrailerOne()->GetChipId())  ||
        chip != int((*iTS)->GetTrailerTwo()->GetChipId())  ){
      sprintf(errBuff, "Evt %u : Inconsistent ChipId: hit = %d, t1 = %d, t2 = %d",evtNb,chip, int((*iTS)->GetTrailerOne()->GetChipId()), int((*iTS)->GetTrailerTwo()->GetChipId()) );
      REPORT_NEW_ERROR(fErrorIncChip,errBuff); //General Error as we don't know then which data correspond to which chip
      rv = fErrorIncChip;
    } 

    //check TS chip and station are consistent with HeaderL1 subid (i.e pc)
    if( fDataFormat >=1 && (subId/2 != station  || (subId%2)*5 > chip || chip >= (1+subId%2)*5 ) ){ 
      sprintf(errBuff, "Evt %u : Inconsistent SUBID with ChipId StationId: subId = %d  chip = %d, station = %d",evtNb,subId,chip,station);
      REPORT_NEW_ERROR(fErrorIncSubId,errBuff); //General Error as we don't know then which data correspond to which chip
      rv = fErrorIncSubId;
    } 
    if( fDataFormat == 0 && (subId-1 != station ) ){
      sprintf(errBuff, "Evt %u : Inconsistent SUBID with StationId: subId = %d , station = %d",evtNb,subId-1,station);
      REPORT_NEW_ERROR(fErrorIncSubId,errBuff); //General Error as we don't know then which data correspond to which chip
      rv = fErrorIncSubId;
    }
    if (rv!=0) return rv; //return as we no longer know which chip correspond to the data

    //check that TS frame LSB match T1 TS-L1A
    unsigned int ts = (*iTS)->GetTrailerOne()->GetL1ATimeStamp();
    unsigned int fc = ts/256;
    unsigned int  lsb = (*iTS)->GetFrameCountLSB();
    if( (fc & 0x0000000f)!= (lsb& 0x0000000f) ){
      sprintf(errBuff, "Evt %u : Inconsistent Frame Counter LSB with L1A TS: fc_lsb = %u  ts_lsb = %u",evtNb,(fc & 0x0000000f), (lsb& 0x0000000f));
      REPORT_NEW_ERROR(fErrorIncFCLSB[station*10+chip],errBuff);
      rv = fErrorIncFCLSB[station*10+chip];
    }

    //check that nb L1A in T1 = Board L1A count in T2
    if(!(*iTS)->GetTrailerTwo()->IsEOB() && ((*iTS)->GetTrailerOne()->GetL1AEvtCounter()+1) != (*iTS)->GetTrailerTwo()->GetL1ANb() ){
      std::sprintf(errBuff,"Nb of L1A in T1 should match  T2 L1 Accept Count: (%u +1!=%u)",(*iTS)->GetTrailerOne()->GetL1AEvtCounter()+1, (*iTS)->GetTrailerTwo()->GetL1ANb());
      REPORT_NEW_ERROR(fErrorIncNTrigg[station*10+chip],errBuff);
      rv =fErrorIncNTrigg[station*10+chip];
    }

    //check if there are duplicated words
    for(iTS2 = iTS + 1; iTS2 != timeStamps.end();++iTS2){
      if( (**iTS2) == (**iTS) ) {
        u32* pu32 = (u32*) &((*iTS2)->GetBuffer()[0]);
        std::sprintf(errBuff,"Duplicated hits found %#08x %#08x",pu32[0], pu32[1]);
        REPORT_NEW_ERROR(fErrorDuplicated[station*10+chip],errBuff);
        rv =fErrorDuplicated[station*10+chip];
        break;
      }
    }    
  }

  //T1 based checks
  std::vector<GTK::GigaTrackerDAQBoardTrailerOne*> trailers1 =  hits->GetTrailersOne();;
  std::vector<GTK::GigaTrackerDAQBoardTrailerOne*>::iterator iT1;
  int station(0);
  if(fDataFormat == 0) station = subId;
  if(fDataFormat >= 1) station = subId/2;

  for(iT1 = trailers1.begin(); iT1 != trailers1.end(); ++iT1){
    if(isL0SpecialFrame(rawHeader->GetTriggerType()<<2)) continue;
    int chip = (*iT1)->GetChipId();
    //check that Event TS match T1 TS-L1A
    if( (*iT1)->GetL1ATimeStamp() != evtTS - fOffTimeStamp[station] ){
      sprintf(errBuff, "Evt %u : Inconsistent L1A-TS with Evt-TS: %u vs %u - %u (station %d, subId %d) = %d",evtNb,(*iT1)->GetL1ATimeStamp(),evtTS, fOffTimeStamp[station],station,subId,int((*iT1)->GetL1ATimeStamp()) - int(evtTS) + int(fOffTimeStamp[station]) );
      REPORT_NEW_ERROR(fErrorIncTS[station*10+chip],errBuff);
      rv = fErrorIncTS[station*10+chip];
    }
    //check that Event Number match T1 Evt Number
    if( (*iT1)->GetL1AEvtCounter()!= evtNb ){
      sprintf(errBuff, "Evt %u : Inconsistent L1A-EvtNb with Evt-Nb: %u vs %u",evtNb,(*iT1)->GetL1AEvtCounter(), evtNb);
      REPORT_NEW_ERROR(fErrorIncEvtNb[station*10+chip],errBuff);
      rv = fErrorIncEvtNb[station*10+chip];
    }
  } 

  //T2 based checks
  std::vector<GTK::GigaTrackerDAQBoardTrailerTwo*> trailers2 =  hits->GetTrailersTwo();;
  std::vector<GTK::GigaTrackerDAQBoardTrailerTwo*>::iterator iT2;
  for(iT2 = trailers2.begin(); iT2 != trailers2.end(); ++iT2){

    int chip = (*iT2)->GetChipId();
    // check that the hit limiter is not reached
    if(!(*iT2)->IsEOB() && int((*iT2)->GetHitNb()) >= fLimiterThreshold[station*10+chip] ){
      sprintf(errBuff, "Evt %u : Hit Limiter Reached in GTK%d chip%d %d vs %d",evtNb, station, chip, int((*iT2)->GetHitNb()),fLimiterThreshold[station*10+chip] );
      REPORT_NEW_ERROR(fErrorLimiter[station*10+chip],errBuff);
      rv = fErrorLimiter[station*10+chip];
    }
  }

  //make sure all half chips are here
  if (hits->GetTrailersOne().size() != 10 || hits->GetTrailersTwo().size() != 10){
    sprintf(errBuff, "Evt %u : Station%d Missing Half-Chip(s) in PC (%d instead of 10) ", evtNb,station,int(hits->GetTrailersOne().size()));
    REPORT_NEW_ERROR(fErrorMissingChip,errBuff);
    rv = fErrorMissingChip;
  }

  return rv;
}

void GTKRawDecoder::EndProcessing() {
  NA62VRawDecoder::EndProcessing();
  // Save the channel profiles for the last burst processed
  // (normally, one burst is processed at a time).
  // This is to keep track of the noisy pixel evaluation.
  for (Int_t i=0; i<3; i++) fChannelProfile[i]->Write();
}
