#include "NA62VRawDecoder.hh"
#include "TCHANTIDigi.hh"
#include "TLAVDigi.hh"
#include "TLKrDigi.hh"
#include "TIRCDigi.hh"
#include "TMUV0Digi.hh"
#include "TCHODDigi.hh"
#include "TNewCHODDigi.hh"
#include "TSACDigi.hh"
#include "THACDigi.hh"
#include "TSpectrometerDigi.hh"
#include "TRegexp.h"
#include "LKrParameters.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"

NA62VRawDecoder::NA62VRawDecoder(NA62VReconstruction* Reco, TString Name, Int_t NMezzPerFullBoard) :
  NA62VNamedModule(Name),
  fReco(Reco),
  fDigiEvent(0),
  fSpecialTriggerEvent(0),
  fDecoder(0),
  fNROBoards(0),
  fNROMezzanines(0),
  fNROMezzaninesPerFullBoard(NMezzPerFullBoard),
  fROMezzanineMasksPerBoard(0),
  fChannelRemap(0),
  fChannelRO(0),
  fNROChannels(0),
  fChannelROSize(0),
  fNSlots(0),
  fNSlotsMax(0),
  fLastSlotID(0),
  fLastSlotIDMax(0),
  fIsAGuestOrHostDetector(false),
  fROMezzaninesT0(0),
  fWarningsLevel(1),
  fNCriticalErrors(0),
  fNQualityWarnings(0),
  fNWrongSlots(0),
  fNTotalSlots(0),
  fNHitsFromMaskedChannels(0),

  // Reset histos
  fHDecoderErrors(0),
  fHDigiTimeRaw(0),
  fHDigiTimeRawFine(0),
  fHDigiTimeRawFineVsROChannel(0)
{

  fNROBoardsPerStation.clear();
}

void NA62VRawDecoder::CreateObjects(){ //called for actual decoders only

  TFile *histoFile=fReco->GetHistoFile();
  if(!histoFile) return;
  // switch to a subdirectory in the output file to avoid histogram name clash from decoders
  // base class is instatiated first

  TDirectory * monitorDir =histoFile->GetDirectory(fReco->GetName()+"Monitor");
  if (!monitorDir) histoFile->mkdir(fReco->GetName()+"Monitor")->cd();
  else monitorDir->cd();

  //----------- Parse config file for common parameters ----------//

  ParseRawDecoderSettingsFile(fReco->GetRawDecoderSettingsFileName());

  //--------------------------------------------------------------//

  if(fNROMezzanines){
    fROMezzaninesT0          = new Double_t[fNROMezzanines];
    fNCriticalErrors         = new Int_t[fNROMezzanines];
    fNQualityWarnings        = new Int_t[fNROMezzanines];
    fNWrongSlots             = new Int_t[fNROMezzanines];
    fNTotalSlots             = new Int_t[fNROMezzanines];
    fNHitsFromMaskedChannels = new Int_t[fNROMezzanines];

    Int_t NGroups = fNROMezzanines/16;
    if(fNROMezzanines%16) NGroups++;
    if(NA62ConditionsService::GetInstance()->Open(fReco->GetCoarseT0FileName())!=kSuccess){
      for(Int_t iGroup=0;iGroup<NGroups;iGroup++){
        for (Int_t iMezzanine=0;iMezzanine<16;iMezzanine++){
          if(16*iGroup+iMezzanine<fNROMezzanines) fROMezzaninesT0[16*iGroup+iMezzanine] = 0.; //set mezzanine T0s to 0
        }
      }
    }
    else {
      TString Line;
      while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fReco->GetCoarseT0FileName()))){
        if(Line.BeginsWith("#")) continue; //COMMENT LINE
        else if(Line.BeginsWith("MezzaninesT0_")){
          for(Int_t iGroup=0;iGroup<NGroups;iGroup++){
            if(Line.BeginsWith(Form("MezzaninesT0_%02d=",iGroup))){
              TObjArray * l = Line.Tokenize(" ");
              for (Int_t iMezzanine=0;iMezzanine<16;iMezzanine++){
                if(16*iGroup+iMezzanine<fNROMezzanines){
                  fROMezzaninesT0[16*iGroup+iMezzanine] = static_cast<TObjString*>(l->At(iMezzanine+1))->GetString().Atof();
                  //std::cout << fReco->GetName() << " ROMezzanine T0 Offset " << fROMezzaninesT0[16*iGroup+iMezzanine] << " per Readout Mezzanine " << 16*iGroup+iMezzanine << std::endl;
                }
              }
              delete l;
            }
          }
        }
      }
      NA62ConditionsService::GetInstance()->Close(fReco->GetCoarseT0FileName());
    }
  }
}

NA62VRawDecoder::~NA62VRawDecoder(){
  if(fDecoder){ //avoid multi deletion due to fDecoder being of the class NA62VRawDecoder
    std::cout << "Deleting " << fName << " RawDecoder" << std::endl;
    if(fDigiEvent){
      delete fDigiEvent;
      fDigiEvent=0;
    }
    if(fSpecialTriggerEvent) {
      delete fSpecialTriggerEvent;
      fSpecialTriggerEvent=0;
    }
    delete fDecoder;
    fDecoder=0;
  }
  if(fChannelRemap){
    delete [] fChannelRemap;
    fChannelRemap=0;
  }
  if(fChannelRO){
    delete [] fChannelRO;
    fChannelRO=0;
  }
  if(fNSlots){
    delete [] fNSlots;
    fNSlots=0;
  }
  if(fLastSlotID){
    delete [] fLastSlotID;
    fLastSlotID=0;
  }
  if(fROMezzanineMasksPerBoard){
    delete [] fROMezzanineMasksPerBoard;
    fROMezzanineMasksPerBoard=0;
  }
  if(fROMezzaninesT0){
    delete [] fROMezzaninesT0;
    fROMezzaninesT0=0;
  }
  if(fNCriticalErrors){
    delete [] fNCriticalErrors;
    fNCriticalErrors=0;
  }
  if(fNQualityWarnings){
    delete [] fNQualityWarnings;
    fNQualityWarnings=0;
  }
  if(fNWrongSlots){
    delete [] fNWrongSlots;
    fNWrongSlots=0;
  }
  if(fNTotalSlots){
    delete [] fNTotalSlots;
    fNTotalSlots=0;
  }
  if(fNHitsFromMaskedChannels){
    delete [] fNHitsFromMaskedChannels;
    fNHitsFromMaskedChannels=0;
  }
  if(fHDecoderErrors){
    delete fHDecoderErrors;
    fHDecoderErrors=0;
  }
  if(fHDigiTimeRaw){
    delete fHDigiTimeRaw;
    fHDigiTimeRaw=0;
  }
  if(fHDigiTimeRawFine){
    delete fHDigiTimeRawFine;
    fHDigiTimeRawFine=0;
  }
  if(fHDigiTimeRawFineVsROChannel){
    delete fHDigiTimeRawFineVsROChannel;
    fHDigiTimeRawFineVsROChannel=0;
  }
}

void NA62VRawDecoder::Init() { //called for RawData only

  TFile *histoFile=fReco->GetHistoFile();
  if(!histoFile) return;
  // switch to a subdirectory in the output file to avoid histogram name clash from decoders
  // base class is instatiated first

  TDirectory * monitorDir =histoFile->GetDirectory(fReco->GetName()+"Monitor");
  if (!monitorDir) histoFile->mkdir(fReco->GetName()+"Monitor")->cd();
  else monitorDir->cd();

  // Book histograms to evaluate StationsT0, NSlots, LastSlotID (data only)
  if(fNROMezzanines){
    Int_t FineTimePrecision = 256;
    Int_t ChannelGrouping = 1;
    if (fReco->GetName()=="Spectrometer") {
      FineTimePrecision = 32;
    }
    else if (fReco->GetName()=="GigaTracker") {
      ChannelGrouping = 40;
    }
    Double_t DigiTimeWidth     = 1.; //ns
    Int_t    DigiTimeNBins     = (Int_t)((fNSlotsMax+3)*ClockPeriod);
    Double_t DigiTimeMax       = (fLastSlotIDMax+2.)*ClockPeriod-0.5*DigiTimeWidth;
    Double_t DigiTimeMin       = DigiTimeMax-((Double_t)DigiTimeNBins)*DigiTimeWidth;
    Double_t DigiTimeFineWidth = ClockPeriod/((Double_t)FineTimePrecision);
    Int_t    DigiTimeFineNBins = (fNSlotsMax+3)*FineTimePrecision; // even number of bins, for rebinning at a later stage
    Double_t DigiTimeFineMax   = (fLastSlotIDMax+2.)*ClockPeriod-0.5*DigiTimeFineWidth;
    Double_t DigiTimeFineMin   = DigiTimeFineMax-((Double_t)DigiTimeFineNBins)*DigiTimeFineWidth;

    fHDigiTimeRaw = new TH2F
      ("DigiTimeRaw", fReco->GetName()+" DigiTimeRaw Vs MezzanineID",fNROMezzanines,-0.5,fNROMezzanines-0.5,
       DigiTimeNBins, DigiTimeMin, DigiTimeMax);
    fHDigiTimeRaw->GetXaxis()->SetTitle("Mezzanine ID");
    fHDigiTimeRaw->GetYaxis()->SetTitle("Raw leading time (ns)");
    fHDigiTimeRaw->SetOption("COLZ");
    fHDigiTimeRawFine = new TH2F
      ("DigiTimeRawFine", fReco->GetName()+" DigiTimeRawFine Vs MezzanineID",fNROMezzanines,-0.5,fNROMezzanines-0.5,
       DigiTimeFineNBins, DigiTimeFineMin, DigiTimeFineMax);
    fHDigiTimeRawFine->GetXaxis()->SetTitle("Mezzanine ID");
    fHDigiTimeRawFine->GetYaxis()->SetTitle("Raw leading time (ns)");
    fHDigiTimeRawFine->SetOption("COLZ");
    if (fNROChannels) {
      fHDigiTimeRawFineVsROChannel = new TH2F
        ("DigiTimeRawFineVsROChannel", fReco->GetName()+" DigiTimeRawFine Vs ROChannel",
         fNROChannels/ChannelGrouping, -0.5, fNROChannels-0.5,
         DigiTimeFineNBins, DigiTimeFineMin, DigiTimeFineMax);
      fHDigiTimeRawFineVsROChannel->GetYaxis()->SetTitle("Raw leading time (ns)");
      fHDigiTimeRawFineVsROChannel->GetXaxis()->SetTitle("Readout Channel");
      fHDigiTimeRawFineVsROChannel->SetOption("COLZ");
    }
    else std::cerr << "[NA62VRawDecoder] WARNING: fNROChannels = 0! [SubDet: " << fReco->GetName() << "]" << std::endl;
  }
  else std::cerr << "[NA62VRawDecoder] WARNING: fNROMezzanines = 0! [SubDet: " << fReco->GetName() << "]" << std::endl;
}

///////////////////////////////////////////////////////////////
// ParseRawDecoderSettingsFile
////////////////////////////////////////////////////////////////

void NA62VRawDecoder::ParseRawDecoderSettingsFile(TString RawDecFileName){
  Int_t nItems=0;
  Int_t MaxChannelID = -1;
  TString Line;
  if(NA62ConditionsService::GetInstance()->Open(RawDecFileName)!=kSuccess) return;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(RawDecFileName))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if(Line.BeginsWith("NROBoards=")){
      TObjArray * l = Line.Tokenize(" ");
      fNROBoards = static_cast<TObjString*>(l->At(1))->GetString().Atoi();
      fNROMezzanines = fNROBoards*fNROMezzaninesPerFullBoard;
      delete l;
      continue;
    }
    else if(Line.BeginsWith("NROChannels=")){
      TObjArray * l = Line.Tokenize(" ");
      fNROChannels = static_cast<TObjString*>(l->At(1))->GetString().Atoi();
      if(fNROChannels) {
        fChannelRemap = new Int_t[fNROChannels];
        for(Int_t iCh=0;iCh<fNROChannels;iCh++) fChannelRemap[iCh] = -1; //reset array
      }
      nItems++;
      delete l;
      continue;
    }
    else if(Line.BeginsWith("ChRemap_")){
      for(Int_t iCh=0;iCh<fNROChannels/16;iCh++){
        if(Line.BeginsWith(Form("ChRemap_%04d=",iCh))){
          TObjArray * l = Line.Tokenize(" ");
          for (Int_t jCh=0;jCh<16;jCh++){
            fChannelRemap[16*iCh+jCh] = static_cast<TObjString*>(l->At(jCh+1))->GetString().Atoi();
            if(fChannelRemap[16*iCh+jCh]>MaxChannelID) MaxChannelID = fChannelRemap[16*iCh+jCh];
          }
          nItems++;
          delete l;
        }
      }
    }
    else if (Line.BeginsWith("NSlots")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries()-1;
      if(NEntries!=fNROMezzanines) {
        std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: " << NEntries << " NSlots for " << fNROMezzanines << " mezzanines!";
        if(NEntries>fNROMezzanines) {
          std::cerr << " Ignoring last " << NEntries-fNROMezzanines << " occurrences in NSlots.." << std::endl;
        }
        else {
          std::cerr << " Using '" << static_cast<TObjString*>(l->At(1))->GetString() << "' for last " << fNROMezzanines-NEntries << " mezzanines.." << std::endl;
          for(Int_t i=0;i<fNROMezzanines-NEntries;i++) {
            TObject *obj = l->At(1)->Clone();
            l->Add(obj);
          }
        }
        NEntries = fNROMezzanines;
      }
      if(NEntries) fNSlots = new Int_t[NEntries];
      else std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: fNROMezzanines = 0!" << std::endl;
      for(Int_t i = 0; i < NEntries; i++) {
        fNSlots[i] = static_cast<TObjString*>(l->At(i+1))->GetString().Atoi();
        if(fNSlotsMax<fNSlots[i]) fNSlotsMax = fNSlots[i];
      }
      delete l;
      continue;
    }
    else if (Line.BeginsWith("LastSlotID")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries()-1;
      if(NEntries!=fNROMezzanines) {
        std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: " << NEntries << " LastSlotID for " << fNROMezzanines << " mezzanines!";
        if(NEntries>fNROMezzanines) {
          std::cerr << " Ignoring last " << NEntries-fNROMezzanines << " occurrences in LastSlotID.." << std::endl;
        }
        else {
          std::cerr << " Using '" << static_cast<TObjString*>(l->At(1))->GetString() << "' for last " << fNROMezzanines-NEntries << " mezzanines.." << std::endl;
          for(Int_t i=0;i<fNROMezzanines-NEntries;i++) {
            TObject *obj = l->At(1)->Clone();
            l->Add(obj);
          }
        }
        NEntries = fNROMezzanines;
      }
      if(NEntries) fLastSlotID = new Int_t[NEntries];
      else std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: fNROMezzanines = 0!" << std::endl;
      fLastSlotIDMax = -1000;
      for(Int_t i = 0; i < NEntries; i++) {
        fLastSlotID[i] = static_cast<TObjString*>(l->At(i+1))->GetString().Atoi();
        if(fLastSlotIDMax<fLastSlotID[i]) fLastSlotIDMax = fLastSlotID[i];
      }
      delete l;
      continue;
    }
    else if (Line.BeginsWith("ROMezzanineMasksPerBoard")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries()-1;
      if(NEntries!=fNROBoards) {
        std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: " << NEntries << " ROMezzanineMasksPerBoards for " << fNROBoards << " boards!";
        if(NEntries>fNROBoards) {
          std::cerr << " Ignoring last " << NEntries-fNROBoards << " occurrences in ROMezzanineMasksPerBoards.." << std::endl;
        }
        else {
          std::cerr << " Using '" << static_cast<TObjString*>(l->At(1))->GetString() << "' for last " << fNROBoards-NEntries << " boards.." << std::endl;
          for(Int_t i=0;i<fNROBoards-NEntries;i++) {
            TObject *obj = l->At(1)->Clone();
            l->Add(obj);
          }
        }
        NEntries = fNROBoards;
      }
      if(NEntries) fROMezzanineMasksPerBoard = new UInt_t[NEntries];
      else std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: fNROBoards = 0!" << std::endl;
      for(Int_t i = 0; i < NEntries; i++) {
        TString subLine = static_cast<TObjString*>(l->At(i+1))->GetString();
        fROMezzanineMasksPerBoard[i] = strtol(TString(subLine(TRegexp("[0-9a-fA-F]+"))),NULL,16);
      }
      delete l;
      continue;
    }
    else if (Line.BeginsWith("NROBoardsPerStation")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries()-1;
      fNROBoardsPerStation.clear();
      for(Int_t i = 0; i < NEntries; i++) {
        fNROBoardsPerStation.push_back(static_cast<TObjString*>(l->At(i+1))->GetString().Atoi());
      }
      delete l;
      continue;
    }
    else if (Line.BeginsWith("WarningsLevel")) {
      fWarningsLevel = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(RawDecFileName);

  //initialising fChannelRO
  if(MaxChannelID>=0) {
    fChannelROSize = MaxChannelID+1;
    fChannelRO = new Int_t[fChannelROSize];
    for(Int_t iCh=0;iCh<fChannelROSize;iCh++) fChannelRO[iCh] = -1; //reset array
  }
  for(Int_t iROCh=0;iROCh<fNROChannels;iROCh++) {
    if(0<=fChannelRemap[iROCh] && fChannelRemap[iROCh]<fChannelROSize) fChannelRO[fChannelRemap[iROCh]]=iROCh;
  }
}

void NA62VRawDecoder::FillDigiTimes(Double_t ReferenceFineTime) {
  // This function is called at the end of ProcessEvent(),
  // so the L0TPData information is available (unlike in DecodeNextEvent)

  if(!fDigiEvent) return;
  if((NA62RecoManager::GetInstance()->GetEventHeader()->GetTriggerType()&0xf0)==0x30) return; //calibration or periodics

  //Common histos to be filled here
  TClonesArray& Digis = (*(fDigiEvent->GetDigis()));
  for (Int_t iDigi=0; iDigi<fDigiEvent->GetNDigis(); iDigi++) {
    TVDigi *Digi = static_cast<TVDigi*>(Digis[iDigi]);
    if(fDecoder) {
      Int_t ROChannelID   = fDecoder->GetChannelRO(Digi->GetChannelID());
      Int_t ROMezzanineID = fDecoder->GetROMezzanine(ROChannelID);
      TH2F *hDigiTimeRaw     = fDecoder->GetHDigiTimeRaw();
      TH2F *hDigiTimeRawFine = fDecoder->GetHDigiTimeRawFine();
      TH2F *hDigiTimeRawFineVsROChannel = fDecoder->GetHDigiTimeRawFineVsROChannel();

      // Discard digis from high threshold channels (LAV,IRC,CHOD,MUV0,SAC),
      // mean-timer channels (NewCHOD) and dead channels (LKr)
      Bool_t GoodDigi = true;
      if(Digi->IsA()==TCHANTIDigi::Class() && static_cast<TCHANTIDigi*>(Digi)->GetThresholdType())  GoodDigi = false;
      else if(Digi->IsA()==TLAVDigi::Class() && static_cast<TLAVDigi*>(Digi)->GetThresholdType())   GoodDigi = false;
      else if(Digi->IsA()==TIRCDigi::Class() && static_cast<TIRCDigi*>(Digi)->GetThresholdType())   GoodDigi = false;
      else if(Digi->IsA()==TCHODDigi::Class() && static_cast<TCHODDigi*>(Digi)->GetThresholdType()) GoodDigi = false;
      else if(Digi->IsA()==TMUV0Digi::Class() && static_cast<TMUV0Digi*>(Digi)->GetThresholdType()) GoodDigi = false;
      else if(Digi->IsA()==TSACDigi::Class() && static_cast<TSACDigi*>(Digi)->GetThresholdType())   GoodDigi = false;
      else if(Digi->IsA()==THACDigi::Class() && static_cast<THACDigi*>(Digi)->GetThresholdType())   GoodDigi = false;
      else if(Digi->IsA()==TNewCHODDigi::Class() && static_cast<TNewCHODDigi*>(Digi)->GetChannelID()<100) GoodDigi = false;
      else if(Digi->IsA()==TLKrDigi::Class() &&
          LKrParameters::GetInstance()->IsDeadCell(static_cast<TLKrDigi*>(Digi)->GetXCellID(),static_cast<TLKrDigi*>(Digi)->GetYCellID())) GoodDigi = false;
      //else if(Digi->IsA()==TSpectrometerDigi::Class() &&
      //    NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetTriggerFlags()&0x2) GoodDigi = false); //no mask 1 for Spectrometer
      if(GoodDigi) {
        if(hDigiTimeRaw)     hDigiTimeRaw->Fill(ROMezzanineID,Digi->GetTime());
        if(hDigiTimeRawFine) hDigiTimeRawFine->Fill(ROMezzanineID,Digi->GetTime()-ReferenceFineTime);
      }
      if(hDigiTimeRawFineVsROChannel) hDigiTimeRawFineVsROChannel->Fill(ROChannelID,Digi->GetTime()-ReferenceFineTime); // all channels
    }
  }
}

void NA62VRawDecoder::EndProcessing(){
  if(fHDigiTimeRaw)                fHDigiTimeRaw->Write();
  if(fHDigiTimeRawFine)            fHDigiTimeRawFine->Write();
  if(fHDigiTimeRawFineVsROChannel) fHDigiTimeRawFineVsROChannel->Write();
  if(fHDecoderErrors)              fHDecoderErrors->Write();
}

///////////////////////////////////////////////////////////////
// Reset
////////////////////////////////////////////////////////////////

void NA62VRawDecoder::Reset(){
  fSpecialTriggerEvent->Clear("C");
  fDigiEvent->Clear("C");
  for(Int_t iMezzanine=0;iMezzanine<fNROMezzanines;iMezzanine++){
    fNCriticalErrors[iMezzanine]=0;         //for debug
    fNQualityWarnings[iMezzanine]=0;        //for debug
    fNWrongSlots[iMezzanine]=0;             //for debug
    fNTotalSlots[iMezzanine]=0;             //for debug
    fNHitsFromMaskedChannels[iMezzanine]=0; //for debug
  }
}

