#include "NA62VReconstruction.hh"
#include "NA62Reconstruction.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"

#include "Riostream.h"
#include "TRegexp.h"
#include "TF1.h"

#define DBGTHIS "NA62VReco"
#include "Debug.h"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

NA62VReconstruction::NA62VReconstruction(TFile* HistoFile, TString Name, TString ConfigFileName) :
  NA62VNamedModule(Name),
  fHistoFile(HistoFile),
  fMainReco(nullptr),
  fRawDecoder(nullptr),
  fStationsMCToF(nullptr),
  fStationsT0(nullptr),
  fNTimeClusters(0),
  fiCurrentTimeCluster(0),
  fRecoEvent(nullptr),
  fSlimRecoEvent(nullptr),
  fChannels(nullptr),
  fConfigFileName(ConfigFileName),
  fT0FileName(""),
  fCoarseT0FileName(""),
  fT0ReferenceDetector(""),
  fT0NHitsMin(0),
  fHeaderFileName(""),
  fBinaryFileName(""),

  fNStations(0),
  fNChannels(0),
  fDetID(0),
  fEnableT0(kFALSE),
  fEnableROMezzaninesT0(kFALSE),
  fEnableStationsT0(kFALSE),
  fEnableTriggerDriftT0(kFALSE),
  fChannelHistograms  (kFALSE),
  fOnlineMonitorAccumulation(0),

  // Reset histos
  fHistosLevel(0),
  fT0HistoMask(0),
  fHRecoHitTimeWrtReferenceVsROChannel(0),
  fHRecoHitTimeWrtReferenceVsROChannelNoT0(0),
  fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim(0)
{
  // Array of histograms
  fHistoArray = new TObjArray();

  //----------- Parse config file for common parameters ----------//

  ParseConfFile(ConfigFileName);
}

NA62VReconstruction::NA62VReconstruction(TString Name) :
  NA62VNamedModule(Name),
  fHistoFile(nullptr),
  fMainReco(nullptr),
  fRawDecoder(nullptr),
  fStationsMCToF(nullptr),
  fStationsT0(nullptr),
  fNTimeClusters(0),
  fiCurrentTimeCluster(0),
  fRecoEvent(nullptr),
  fSlimRecoEvent(nullptr),
  fChannels(nullptr),
  fT0FileName(""),
  fHeaderFileName(""),
  fBinaryFileName(""),

  fNStations(0),
  fNChannels(0),
  fDetID(0),
  fEnableT0(kTRUE),
  fEnableROMezzaninesT0(kTRUE),
  fEnableStationsT0(kTRUE),
  fEnableTriggerDriftT0(kTRUE),
  fChannelHistograms  (kFALSE),
  fOnlineMonitorAccumulation(0),

  // Reset histos
  fHistoArray(nullptr),
  fHistosLevel(0),
  fT0HistoMask(0),
  fHRecoHitTimeWrtReferenceVsROChannel(0),
  fHRecoHitTimeWrtReferenceVsROChannelNoT0(0),
  fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim(0){}

NA62VReconstruction::~NA62VReconstruction(){
  //std::cout << "Deleting " << fName << " reconstruction" << std::endl;
  if(fHRecoHitTimeWrtReferenceVsROChannel){
    delete fHRecoHitTimeWrtReferenceVsROChannel;
    fHRecoHitTimeWrtReferenceVsROChannel=0;
  }
  if(fHRecoHitTimeWrtReferenceVsROChannelNoT0){
    delete fHRecoHitTimeWrtReferenceVsROChannelNoT0;
    fHRecoHitTimeWrtReferenceVsROChannelNoT0=0;
  }
  if(fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim){
    delete fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim;
    fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim=0;
  }
  if(fRecoEvent){
    delete fRecoEvent;
    fRecoEvent=0;
  }
  if(fSlimRecoEvent){
    delete fSlimRecoEvent;
    fSlimRecoEvent=0;
  }
  if(fStationsT0){
    delete [] fStationsT0;
    fStationsT0=0;
  }
  if(fStationsMCToF){
    delete [] fStationsMCToF;
    fStationsMCToF=0;
  }
  if (fChannels) {
    for (Int_t ich=0; ich<fNChannels; ich++) {
      if (fChannels[ich]) delete fChannels[ich];
      fChannels[ich] = 0;
    }
    delete [] fChannels;
    fChannels = 0;
  }
  if (fHistoArray) delete fHistoArray;
  fHistoArray = 0;
}

void NA62VReconstruction::Exception(TString Message, Int_t ErrorCode){
  std::cerr << std::endl << "********************************************************************************" << std::endl;
  std::cerr << "Exception:" << std::endl << "        " << Message.Data() << std::endl;
  std::cerr << std::endl << "********************************************************************************" << std::endl;
  _Exit(ErrorCode); // unlike exit() does not execute atexit functions
}

void NA62VReconstruction::Init(NA62VReconstruction* MainReco) {
  fMainReco = MainReco;
  //read Station T0s
  ReadStationT0s();

  //------------------ initialise common histos ------------------//
  // switch to a subdirectory in the output file to avoid histogram name clash from decoders
  // base class is instatiated first

  GetOrMakeDir(fHistoFile,fName+"Monitor")->cd(); //go to the SubDet directory

  // Histos to evaluate T0 corrections
  if (fHistosLevel>0) {
    Double_t DeltaTime = 40.;
    Int_t nTimeBins = ((Int_t)DeltaTime)*10; // 200 ps resolution
    if (fName=="Cedar" || fName=="RICH") nTimeBins = ((Int_t)DeltaTime)*40; // 50 ps resolution
    else if (fName=="GigaTracker") {
      DeltaTime = 10.;
      nTimeBins = ((Int_t)DeltaTime)*20; // 100 ps resolution
    }
    else if (fName=="LKr") nTimeBins = ((Int_t)DeltaTime)*5; // 400 ps resolution
    else if (fName=="Spectrometer") return;

    if (fRawDecoder && fRawDecoder->GetDecoder()) {
      Int_t nROCh = fRawDecoder->GetDecoder()->GetNROChannels();
      if(fT0HistoMask&(1<<kHistoT0)){
        AddHisto(fHRecoHitTimeWrtReferenceVsROChannel = new TH2F
            ("RecoHitTimeWrtReferenceVsReadoutChannel",
             "RecoHit-"+fT0ReferenceDetector+" Time vs RO Channel", nROCh, -0.5, nROCh-0.5, nTimeBins, -DeltaTime, DeltaTime));
        fHRecoHitTimeWrtReferenceVsROChannel->GetXaxis()->SetTitle("RO channel number");
        fHRecoHitTimeWrtReferenceVsROChannel->GetYaxis()->SetTitle("Reconstructed hit time wrt "+fT0ReferenceDetector+" (ns)");
      }

      if(fT0HistoMask&(1<<kHistoNoT0)){
        AddHisto(fHRecoHitTimeWrtReferenceVsROChannelNoT0 = new TH2F
            ("RecoHitTimeWrtReferenceVsReadoutChannelNoT0",
             "Uncorrected RecoHit-"+fT0ReferenceDetector+" Time vs RO Channel", nROCh, -0.5, nROCh-0.5, nTimeBins, -DeltaTime, DeltaTime));
        fHRecoHitTimeWrtReferenceVsROChannelNoT0->GetXaxis()->SetTitle("RO channel number");
        fHRecoHitTimeWrtReferenceVsROChannelNoT0->GetYaxis()->SetTitle("Uncorrected hit time wrt "+fT0ReferenceDetector+" (ns)");
      }

      if(fT0HistoMask&(1<<kHistoNoT0Prim)){
        AddHisto(fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim = new TH2F
            ("RecoHitTimeWrtReferenceVsReadoutChannelNoT0Prim",
             "Uncorrected RecoHit-"+fT0ReferenceDetector+" Time vs RO Channel [for Primitives]", nROCh, -0.5, nROCh-0.5, nTimeBins, -DeltaTime, DeltaTime));
        fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->GetXaxis()->SetTitle("RO channel number");
        fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->GetYaxis()->SetTitle("Uncorrected hit time wrt "+fT0ReferenceDetector+" (ns)");
      }
    }
  }
}

TTimeCluster* NA62VReconstruction::TimeClustering(TDetectorVEvent * tEvent, Double_t TimeWindow){
  TDigiVEvent* DigiEvent = static_cast<TDigiVEvent*>(tEvent);
  debug_cout(1,"NA62VReconstruction::TimeClustering: NDigis = " << DigiEvent->GetNDigis() << " NTimeClusters = " << fNTimeClusters << " iCurrentTimeCluster = " << fiCurrentTimeCluster);

  if (!DigiEvent->GetNDigis()) return 0;

  if(fNTimeClusters > 0){ // Polling clusters buffer
    if(fiCurrentTimeCluster < fNTimeClusters)
      return static_cast<TTimeCluster*>(DigiEvent->GetCandidate(fiCurrentTimeCluster++));
    else{
      fNTimeClusters = 0;
      return 0;
    }
  }
  TClonesArray &TimeClusters = *(DigiEvent->GetCandidates());
  TimeClusters.Clear("C");
  DigiEvent->SetNCandidates(0);

  Int_t NDigis = DigiEvent->GetNDigis();
  TClonesArray& Digis = *(DigiEvent->GetDigis());
  Digis.Sort();

  TTimeCluster * Cluster = static_cast<TTimeCluster*>(DigiEvent->AddCandidate());
  Double_t ClusterTime = 0;
  Int_t NClusterDigis = 0;
  for(Int_t iDigi = 0; iDigi < NDigis; iDigi++){
    Cluster->AddDigi(iDigi);
    Double_t DigiTime = static_cast<TVDigi*>(Digis[iDigi])->GetTime();
    ClusterTime += DigiTime;
    NClusterDigis++;
    Cluster->SetAverage(ClusterTime/NClusterDigis);
    debug_cout(2,"NA62VReconstruction::TimeClustering: iDigi = " << iDigi << " -> iCluster = " << TimeClusters.GetEntries() - 1 << " Cluster =  " << TimeClusters[TimeClusters.GetEntries() - 1]);
    if(iDigi < NDigis - 1 && static_cast<TVDigi*>(Digis[iDigi + 1])->GetTime() - static_cast<TVDigi*>(Digis[iDigi])->GetTime() > TimeWindow){
      ClusterTime = 0;
      NClusterDigis = 0;
      Cluster = static_cast<TTimeCluster*>(DigiEvent->AddCandidate());
    }
  }
  for(Int_t iCluster = 0; iCluster < TimeClusters.GetEntries(); iCluster++){
    Cluster = static_cast<TTimeCluster*>(TimeClusters[iCluster]);
    NClusterDigis = Cluster->GetNHits();
    ClusterTime = Cluster->GetAverage();
    Double_t ClusterRMS2 = 0;
    for(Int_t iDigi = 0; iDigi < NClusterDigis; iDigi++){
      Double_t DigiTime = static_cast<TVDigi*>(Cluster->GetDigi(iDigi))->GetTime();
      ClusterRMS2 += (DigiTime - ClusterTime) * (DigiTime - ClusterTime);
    }
    Cluster->SetRMS(NClusterDigis > 1 ? TMath::Sqrt(ClusterRMS2/(NClusterDigis - 1)) : 0);
    debug_cout(1,"NA62VReconstruction::TimeClustering: iCluster = " << iCluster << " NDigis = " << NClusterDigis << " Cluster Time =  " << Cluster->GetAverage() << " Cluster RMS =  " << Cluster->GetRMS());
  }

  TimeClusters.Compress();
  TimeClusters.Sort();
  DigiEvent->SetNCandidates(TimeClusters.GetEntries());
  fiCurrentTimeCluster = 0;
  fNTimeClusters = DigiEvent->GetNCandidates();
  TTimeCluster * CurrentCluster = static_cast<TTimeCluster*>(DigiEvent->GetCandidate(fiCurrentTimeCluster++));
  debug_cout(1,"NA62VReconstruction::TimeClustering: End: NTimeClusters = " << fNTimeClusters << " iCurrentTimeCluster = " << fiCurrentTimeCluster << " CurrentTimeCluster = " << CurrentCluster);
  return CurrentCluster;
}

TRecoVEvent* NA62VReconstruction::ProcessEvent (TDetectorVEvent* tEvent, Event* /*tGenEvent*/) {
  // to be called from the derived classes

  (*(TVEvent*)fRecoEvent) = (*(TVEvent*)tEvent);
  fRecoEvent->Clear("C");
  fRecoEvent->SetDigiEvent(static_cast<TDigiVEvent*>(tEvent));
  fRecoEvent->SetErrorMask(fRecoEvent->GetDigiEvent()->GetErrorMask());

  return fRecoEvent;
}

void NA62VReconstruction::EndProcessing(){
  // to be called from the derived classes

  if(!fHistoFile) return;
  if(!fHistoFile->cd(fName+"Monitor")){
    std::cerr << "["+fName+"Reconstruction] WARNING: Failed to find directory '" << fName+"Monitor' in the output file, this should not happen" << std::endl;
    return;
  }
  if(fHRecoHitTimeWrtReferenceVsROChannel) fHRecoHitTimeWrtReferenceVsROChannel->Write();
  if(fHRecoHitTimeWrtReferenceVsROChannelNoT0) fHRecoHitTimeWrtReferenceVsROChannelNoT0->Write();
  if(fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Write();
  if(fRawDecoder && fRawDecoder->GetDecoder()) fRawDecoder->GetDecoder()->EndProcessing();
}

void NA62VReconstruction::FillTimes(Double_t /*ReferenceTime*/){}

Double_t NA62VReconstruction::StrawDrift(Double_t * x, Double_t * par){
  if(x[0]<=par[1])
    return par[0]*TMath::Gaus(x[0],par[1],par[2]) + par[7];
  else if(x[0]<=par[1]+3*par[3])
    return par[0]*(par[4]*TMath::Gaus(x[0],par[1],par[3]) + (1 - par[4]) )+ par[7];
  else
    return par[0]*(1-par[4])*(1-TMath::Erf((x[0]-(par[1] + par[5]))/par[6]))*0.5 + par[7];
}

TDirectory * NA62VReconstruction::GetOrMakeDir(TDirectory *inDir,TString dirName){
  if( !inDir ){
    std::cerr << "*** can not make a directory in a non-existent file ***" << std::endl;
    return 0;
  }
  TDirectory * newDir = inDir->GetDirectory(dirName);
  if(newDir) return newDir;
  return inDir->mkdir(dirName);
}

void NA62VReconstruction::ParseConfFile(TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }

  fT0FileName = fCoarseT0FileName = "";

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("NStations")) {
      fNStations = TString(Line(TRegexp("[0-9]+"))).Atoi();
      if(fNStations>0){
        fStationsT0 = new Double_t[fNStations];
        for(Int_t i = 0; i < fNStations; i++) fStationsT0[i] = 0.;
      }
      continue;
    }
    else if (Line.BeginsWith("NChannels")) {
      fNChannels = TString(Line(TRegexp("[0-9]+"))).Atoi();
      if(fNChannels) fChannels = new NA62VChannel*[fNChannels];
      for (Int_t ich=0; ich<fNChannels; ich++) fChannels[ich] = 0;
      continue;
    }
    else if (Line.BeginsWith("HeaderFileName")) {
      TObjArray * l = Line.Tokenize(" ");
      fHeaderFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("BinaryFileName")) {
      TObjArray * l = Line.Tokenize(" ");
      fBinaryFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("DetID")) {
      fDetID = strtol(TString(Line(TRegexp("0[xX][0-9a-fA-F]+"))),NULL,16);
      continue;
    }
    else if (Line.BeginsWith("StationsMCToF")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries()-1;
      if(NEntries!=fNStations) {
        std::cerr << "["<<fName<<"Reconstruction] WARNING: " << NEntries << " StationsMCToF for " << fNStations << " stations!";
        if(NEntries>fNStations) {
          std::cerr << " Ignoring last " << NEntries-fNStations << " occurrences in StationsMCToF.." << std::endl;
        }
        else {
          std::cerr << " Using '" << static_cast<TObjString*>(l->At(1))->GetString() << "' for last " << fNStations-NEntries << " stations.." << std::endl;
          for(Int_t i=0;i<fNStations-NEntries;i++) {
            TObject* obj = l->At(1)->Clone();
            l->Add(obj);
          }
        }
        NEntries = fNStations;
      }
      fStationsMCToF = new Double_t[NEntries];
      for(Int_t i = 0; i < NEntries; i++) {
        fStationsMCToF[i] = static_cast<TObjString*>(l->At(i+1))->GetString().Atof()*ns;
      }
      delete l;
      continue;
    }
    else if (Line.BeginsWith("T0FileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fT0FileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("ROMezzaninesT0FileInput")) { //should be changed to CoarseT0FileInput in the future..
      TObjArray *l = Line.Tokenize(" ");
      fCoarseT0FileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("EnableT0")) {
      Line.Remove(7,1);
      fEnableT0 = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnableROMezzaninesT0")) {
      Line.Remove(19,1);
      fEnableROMezzaninesT0 = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnableStationsT0")) {
      Line.Remove(15,1);
      fEnableStationsT0 = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnableTriggerDriftT0")) {
      Line.Remove(19,1);
      fEnableTriggerDriftT0 = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("OnlineMonitorAccumulation")) {
      fOnlineMonitorAccumulation = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnableChannelHistograms")) {
      fChannelHistograms = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if(Line.BeginsWith("TimeClusteringWindow=")){
      fTimeClusteringWindow = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("T0HistoMask")) {
      Line.Remove(1,1);
      fT0HistoMask = TString(Line(TRegexp("[0-7]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("RawDecoderSettingsFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fRawDecoderSettingsFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
  }
  confFile.close();

  if(fName=="NA62") return; //main reco instance

  if (fNStations<=0) {
    std::cerr << "["<<fName<<"Reconstruction] ERROR: invalid number of stations specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fNChannels<=0) {
    std::cerr << "["<<fName<<"Reconstruction] ERROR: invalid number of channels specified"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (!fT0FileName.Length()) {
    std::cerr << "["<<fName<<"Reconstruction] ERROR: T0 file not defined"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (!fCoarseT0FileName.Length()) {
    std::cerr << "["<<fName<<"Reconstruction] ERROR: CoarseT0 file not defined"<< std::endl;
    exit(kWrongConfiguration);
  }
}

Double_t NA62VReconstruction::GetT0Correction(Int_t ChannelID, Int_t iStation){
  Int_t ROchannel         = fRawDecoder->GetDecoder()->GetChannelRO(ChannelID);
  Int_t ROMezzanineID     = fRawDecoder->GetDecoder()->GetROMezzanine(ROchannel);
  if (fName=="LAV") iStation--; // LAV StationIDs start from 1
  Double_t StationT0      = (fEnableStationsT0) ? fStationsT0[iStation] : 0.0;
  Double_t ROMezzanineT0  = (fEnableROMezzaninesT0) ? fRawDecoder->GetDecoder()->GetROMezzanineT0(ROMezzanineID) : 0.0;
  Double_t TriggerDriftT0 = (fEnableTriggerDriftT0) ? static_cast<NA62Reconstruction *>(fMainReco)->GetTriggerDriftT0() : 0.0;
  if((NA62RecoManager::GetInstance()->GetEventHeader()->GetTriggerType()&0xff)==0x10) { // Subtract it for "pure" CTRL events (=0x10)
    TriggerDriftT0 -= static_cast<NA62Reconstruction *>(fMainReco)->GetTriggerDriftT0();
  }
  Double_t T0Correction   = StationT0+ROMezzanineT0+TriggerDriftT0;
  return T0Correction;
}

Double_t NA62VReconstruction::GetT0Correction(TVDigi * Digi){
  Int_t PositionID       = Digi->GetChannelID();
  Int_t iStation         = Digi->GetStationID();
  return GetT0Correction(PositionID, iStation);
}

//////////////////////////////
// Read the station T0 offsets

void NA62VReconstruction::ReadStationT0s() {

  if(!fEnableStationsT0) return;
  if(NA62ConditionsService::GetInstance()->Open(fCoarseT0FileName)!=kSuccess) return;

  TString Line;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fCoarseT0FileName))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if(Line.BeginsWith("StationsT0")){
      TObjArray * l = Line.Tokenize(" ");
      for (Int_t iStation=0;iStation<fNStations;iStation++){
        fStationsT0[iStation] = static_cast<TObjString*>(l->At(iStation+1))->GetString().Atof();
        //std::cout << fName << " Station T0 Offset " << fStationsT0[iStation] << " per Readout Station " << iStation << std::endl;
      }
      delete l;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fCoarseT0FileName);
}

//////////////////////////////
// Read the channel T0 offsets

void NA62VReconstruction::ReadT0s () {

  if (!fEnableT0) return;
  if (NA62ConditionsService::GetInstance()->Open(fT0FileName)!=kSuccess) return;
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fT0FileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t     ch = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
    Double_t  t0 = static_cast<TObjString*>(l->At(2))->GetString().Atof();
    if(fabs(t0)>999) t0 = 0.0; // -999.999: masked channel, +999.999: failed to compute T0
    if(ch>=fNChannels) {
      std::cerr << "["+fName+"Reconstruction] WARNING: Skipping unexpected line while reading T0s! [Channel: "<<ch<<"/"<<fNChannels<<"]" << std::endl;
      continue;
    }
    fChannels[ch]->SetT0(t0);
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fT0FileName);
  //PrintT0s();
}

/////////////////////////////////////////////////////////////////////////
// Print the T0 constants used in a format readable by the reconstruction

void NA62VReconstruction::PrintT0s() {
  std::cout <<"# " << fName << " T0 constants used:" << std::endl;
  for (Int_t ich=0; ich<fNChannels; ich++) {
    if (fChannels[ich]->GetGeoChannelID()>=0) {
      std::cout << Form("%4d %4d %8.3f\n", ich, fChannels[ich]->GetGeoChannelID(), fChannels[ich]->GetT0());
    }
  }
}

void NA62VReconstruction::ResetT0s() {
  for (Int_t ich=0; ich<fNChannels; ich++) fChannels[ich]->SetT0(0.0);
}

void NA62VReconstruction::AddHisto(TObject* Object){;
  if(!Object) {
    std::cerr << "["+fName+"Reconstruction] WARNING: AddHisto with invalid pointer!" << std::endl;
    return;
  }
  fHistoArray->Add(Object);
}

void NA62VReconstruction::StartOfBurst() {
}

void NA62VReconstruction::EndOfBurst() {
}
