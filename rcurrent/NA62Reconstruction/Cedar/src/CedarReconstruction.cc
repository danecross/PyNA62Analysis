// ---------------------------------------------------------------
// History:
//
// 2014 modifications added by Karim Massri (karim.massri@cern.ch) 2014-09-30
// A major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2013-02-14
// RecoHit added by Karim Massri (karim.massri@cern.ch) 2012-11-08
// Technical Run modifications added by Karim Massri (karim.massri@cern.ch) 2012-11-07
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------
/// \class CedarReconstruction
/// \Brief
/// Main Cedar reconstruction: starts from Cedar Hits and collects them in Time Clusters (group of hits near in time).
/// Digi hits come from single PMs, RecoHits are created from digi hits fulfilling a user-defined condition (e.g. leading+trailing edges).
/// RecoHit times are corrected with T0 and Slewing corrections.
/// Two iterations are done to improve the clustering. Each Time Cluster is a Cedar Candidate.
/// The minimum number of hits of a Cedar Candidate is 1 (i.e. it is not necessarily a Kaon Candidate!)
/// \EndBrief
///
/// \Detailed
/// It makes use of base class NA62VReconstruction and of classes in NA62MC/Cedar/Persistency (TRecoCedarEvent, TRecoCedarCandidate, TRecoCedarHit).
/// The Cedar reconstruction basic parameters are set in NA62Reconstruction/config/Cedar.conf
/// \n
/// =========
/// \n
/// Cedar.conf
/// \n
/// =========
/// \n
/// Description of parameters can be found in Cedar.conf, here the parameters you may want to change to perform your own reconstruction:
/// \n
/// EdgeRequirement= 3 --> Edge requirement for the hit reconstruction:
///                           1 = at least one leading,  2 = at least one trailing, 3 = leading+trailing (default)
///                           0 = every possible hit (at least one leading or at least one trailing)
///                          -1 = leading only,         -2 = trailing only
/// \n
/// TimeWindow=5       --> used to build TimeCluster for hits inside TimeWindow width (+/- 0.5*TimeWindow)
/// \n
/// StationsT0 (ns) --> used to center Leading Time of TDC around 0.
/// \n
/// =======================================================================================================================================================
/// \n
/// Cedar Reconstruction
/// \n
///
/// TimeCandidate
/// \n
/// Iterations of the time candidate clustering procedure (NCandidateClusteringIterations= 1 or 2)
/// \n
/// A cluster is defined collecting hits in 0.5*TimeWindow
/// \n
///   - 1: a new cluster is defined if a hit is more than 0.5*TimeWindow away from the closest existing cluster
///   - 2: clusters closer than TimeWindow to a larger cluster are checked to possibly merge their hits in the larger cluster
///   .
/// CandidateTime is the average time of hits belonging to the same Candidate
/// \n
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include "Riostream.h"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "NA62ConditionsService.hh"
#include "CedarReconstruction.hh"
#include "CedarChannel.hh"
#include "TRecoCedarEvent.hh"
#include "TRecoCedarHit.hh"
#include "TCedarDigi.hh"
#include "NA62Buffer.hh"
#include "NA62BufferProto.hh"
#include "TTDCBSpecialTrigger.hh"
#include "TString.h"
#include "TRegexp.h"

CedarReconstruction::CedarReconstruction (TFile* HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "Cedar", ConfigFileName) {

  fRawDecoder          = 0;
  fRecoEvent           = new TRecoCedarEvent();
  fGeometry            = new CedarGeometry();
  fNSectors            = fGeometry->GetNSectors();
  fMinSectors          = -999;
  fMinPMTs             = -999;
  fTimeWindow          = -999;
  fNRecoHitsIntegrated = new Int_t[fNSectors]; // hit sectors integrated over burst
  fNCoincidencesIntegrated = new Int_t[fNSectors]; // number of coincidences integrated over burst
  fSectorIsEnabled     = new Bool_t[fNSectors];
  fSectorOccupancy     = new Int_t[fNSectors];
  fEnableSlewingCorr   = kTRUE;
  fEvaluateSlewingCorr = kFALSE;
  fEdgeRequirement     = 3;
  fNCandidateClusteringIterations = 1;
  fAlignEnabled        = kTRUE;
  fPMTTime_Response    = "";
  fPMTTime_min         = -999;
  fPMTTime_max         = -999;
  fPMTWidth_mean       = -999;
  fPMTWidth_sigma      = -999;
  fPMT_Efficiency      = -999;
  fPMT_MergeThreshold  = -999;
  fDIMInfo.Clear();
  fNSelectedCandidatesInTimeSlots.clear();

  ResetHistograms();

  fResetOMAlignmentNEvents = 10000; //default

  fAlign = new CedarAlign();
  ParseConfFile(ConfigFileName);
  fNSectorsEnabled = 0;
  for (Int_t i=0; i<fNSectors; i++) if (fSectorIsEnabled[i]) fNSectorsEnabled++;
}

CedarReconstruction::CedarReconstruction():
  NA62VReconstruction("Cedar") {
  
  fRawDecoder          = 0;
  fRecoEvent           = new TRecoCedarEvent();
  fGeometry            = new CedarGeometry();
  fNSectors            = fGeometry->GetNSectors();
  fMinSectors          = -999;
  fMinPMTs             = -999;
  fTimeWindow          = -999;
  fNRecoHitsIntegrated = new Int_t[fNSectors]; // hit sectors integrated over burst
  fNCoincidencesIntegrated = new Int_t[fNSectors]; // number of coincidences integrated over burst
  fSectorIsEnabled     = new Bool_t[fNSectors];
  fSectorOccupancy     = new Int_t[fNSectors];
  fEnableSlewingCorr   = kTRUE;
  fEvaluateSlewingCorr = kFALSE;
  fEdgeRequirement     = 3;
  fNCandidateClusteringIterations = 1;
  fAlignEnabled        = kFALSE;
  fPMTTime_Response    = "";
  fPMTTime_min         = -999;
  fPMTTime_max         = -999;
  fPMTWidth_mean       = -999;
  fPMTWidth_sigma      = -999;
  fPMT_Efficiency      = -999;
  fPMT_MergeThreshold  = -999;  
  fDIMInfo.Clear();
  fNSelectedCandidatesInTimeSlots.clear();

  ResetHistograms();

  fResetOMAlignmentNEvents = 10000; //default

  fAlign = nullptr;
  
  fNSectorsEnabled = 0;
  for (Int_t i=0; i<fNSectors; i++) {
    fSectorIsEnabled[i] = true; // by default enabled all sectors
    if (fSectorIsEnabled[i]) fNSectorsEnabled++;
  }
}

void CedarReconstruction::Init(NA62VReconstruction* MainReco) {

  NA62VReconstruction::Init(MainReco); // common part for all the subdetectors
  if (fAlignEnabled) fAlign->Init();
  for (Int_t ich=0; ich<fNChannels; ich++) {
    Int_t PositionID = fRawDecoder->GetDecoder()->GetChannelRemap(ich);
    fChannels[ich] = new CedarChannel(PositionID, ich, fChannelHistograms);
    static_cast<CedarChannel*>(fChannels[ich])->SetNewMinWidth(fNewMinWidth);
    static_cast<CedarChannel*>(fChannels[ich])->SetNewMaxWidth(fNewMaxWidth);
  }
  Double_t NSlotsMax = 0.;
  for(UInt_t iROMezzanine=0;iROMezzanine<fRawDecoder->GetDecoder()->GetNROMezzanines();iROMezzanine++){
    if(NSlotsMax<fRawDecoder->GetDecoder()->GetNSlots(iROMezzanine)) NSlotsMax = fRawDecoder->GetDecoder()->GetNSlots(iROMezzanine);
  }
  fNSelectedCandidatesInTimeSlots.resize(NSlotsMax);
  ReadT0s();
  ReadSlewingCorrections();
  InitHistograms();
}

CedarReconstruction::~CedarReconstruction() {
  if(fGeometry)            delete fGeometry;
  if(fNRecoHitsIntegrated) delete [] fNRecoHitsIntegrated;
  if(fNCoincidencesIntegrated) delete [] fNCoincidencesIntegrated;
  if(fSectorIsEnabled)     delete [] fSectorIsEnabled;
  if(fSectorOccupancy)     delete [] fSectorOccupancy;
  if(fAlign)               delete fAlign;
  DeleteHistograms();
}

// Read CEDAR reconstruction parameters from a configuration file
void CedarReconstruction::ParseConfFile(TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }
  else std::cout << "[CedarReconstruction] Reading config file: " << ConfFileName << std::endl;

  fSlewingCorrFileName = "";
  Bool_t printCedarParameters = kFALSE;

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("MinSectors")) { // minimal number of sectors in coincidence
      fMinSectors = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("MinPMTs")) { // minimal number of PMTs in coincidence
      fMinPMTs = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("TimeWindow")) {
      fTimeWindow = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
    else if (Line.BeginsWith("EdgeRequirement")) {
      fEdgeRequirement = TString(Line(TRegexp(".[0-9]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("OctantsEnabled")) {
      TObjArray *l = Line.Tokenize(" ");
      for (Int_t i=0; i<fNSectors; i++)
        fSectorIsEnabled[i] = static_cast<TObjString*>(l->At(i+1))->GetString().Atoi();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("EnableSlewingCorrection")) {
      fEnableSlewingCorr = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EvaluateSlewingCorrection")) {
      fEvaluateSlewingCorr = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("NCandidateClusteringIterations")) {
      fNCandidateClusteringIterations = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("SlewingCorrectionFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fSlewingCorrFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("NewMinWidth")) {
      fNewMinWidth = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof()*ns;
      continue;
    }
    else if (Line.BeginsWith("NewMaxWidth")) {
      fNewMaxWidth = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof()*ns;
      continue;
    }
    else if (Line.BeginsWith("PrintCedarParameters")) { // print the interpreted contents?
      printCedarParameters = (Bool_t)TString(Line(TRegexp("[01]"))).Atoi();
    }
    else if ( Line.BeginsWith("PMTTime_Response") ){
      // note this is an arbiratry string
      Int_t eqPos = Line.Index("=")+1;
      // skip spaces after first "="
      while( Line[eqPos] == ' ' ) ++eqPos;
      Int_t len = Line.Length() - eqPos;
      // check if trailing '\' i.e. continue on next line
      if( Line[Line.Length()-1] == '\\' ) --len;
      fPMTTime_Response = Line(eqPos,len);
      while(Line[Line.Length()-1] == '\\'){
        Line.ReadLine(confFile);
        //strip space to first character
        Int_t firstPos = 0;
        while( TString(Line[firstPos]).IsWhitespace() ) ++firstPos;
        len = Line.Length() - firstPos;
        if( Line[Line.Length()-1] == '\\' ) --len;
        fPMTTime_Response.Append(Line(firstPos,len));
      }
    }
    else if ( Line.BeginsWith("PMTTime_min") ){
      fPMTTime_min = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if ( Line.BeginsWith("PMTTime_max") ){
      fPMTTime_max = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if ( Line.BeginsWith("PMTWidth_mean") ){
      fPMTWidth_mean = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if ( Line.BeginsWith("PMTWidth_sigma") ){
      fPMTWidth_sigma = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if ( Line.BeginsWith("PMT_Efficiency") ){
      fPMT_Efficiency = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if ( Line.BeginsWith("PMT_MergeThreshold") ){
      fPMT_MergeThreshold = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    }
    else if ( Line.BeginsWith("AlignmentEnabled") ){
      fAlignEnabled = TString(Line(TRegexp("[01]"))).Atoi();
    }
    else if ( Line.BeginsWith( "AlignmentAllTemplate") ){
      TObjArray * align_array = Line.Tokenize( " " );
      //double weight = static_cast<TObjString*>(align_array->At(1))->GetString().Atof();
      TString filename = static_cast<TObjString*>(align_array->At(2))->GetString();
      fAlign->SetAllTemplateFile(filename.Data());
      delete align_array;
    }
    else if ( Line.BeginsWith( "AlignmentSelectedTemplate") ){
      TObjArray * align_array = Line.Tokenize( " " );
      //double weight = static_cast<TObjString*>(align_array->At(1))->GetString().Atof();
      TString filename = static_cast<TObjString*>(align_array->At(2))->GetString();
      fAlign->SetSelectedTemplateFile(filename.Data());
      delete align_array;
    }
    else if ( Line.BeginsWith( "AlignmentFolder") ){
      TObjArray * align_array = Line.Tokenize( " " );
      //double weight = static_cast<TObjString*>(align_array->At(1))->GetString().Atof();
      TString folder = static_cast<TObjString*>(align_array->At(2))->GetString();
      delete align_array;
      //Ignored at the moment
    }
    else if ( Line.BeginsWith( "PMTPositions") ){
      TObjArray * line_array = Line.Tokenize( " " );
      TString PMTPosFile = static_cast<TObjString*>( line_array->At(1))->GetString();
      fAlign->SetPMTPositionMap(PMTPosFile.Data());
      delete line_array;
    }
    else if ( Line.BeginsWith( "PMTEffs") ){
      TObjArray * line_array = Line.Tokenize( " " );
      TString PMTEffsFile = static_cast<TObjString*>( line_array->At(1))->GetString();
      fAlign->SetPMTEffMap(PMTEffsFile.Data());
      delete line_array;
    }
    else if (Line.BeginsWith("ResetOMAlignmentNEvents")) {
      fResetOMAlignmentNEvents = TString(Line(TRegexp("[0-9]+"))).Atoi();
    }
  }
  confFile.close();

  if (fEvaluateSlewingCorr) {
    fChannelHistograms = kTRUE;
    fEnableSlewingCorr = kFALSE; // disable existing slewing correction
  }
  if (!fSlewingCorrFileName.Length()) {
    std::cerr << "[CedarReconstruction] ERROR: slewing correction file not defined"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fTimeWindow<=0) {
    std::cerr << "[CedarReconstruction] ERROR: time window <= 0"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (abs(fEdgeRequirement)>3) {
    std::cerr << "[CedarReconstruction] ERROR: invalid edge requirement"<< std::endl;
    exit(kWrongConfiguration);
  }
  if (fPMTTime_Response == "" ||
      fPMTTime_min == -999. ||
      fPMTTime_max == -999. ||
      fPMTWidth_mean == -999. ||
      fPMTWidth_sigma == -999. ||
      fPMT_Efficiency == -999. ||
      fPMT_MergeThreshold == -999.) {
    std::cerr << "[CedarReconstruction] ERROR: digitisation parameters not fully set" << std::endl;
    PrintCedarDigitizerParameters();
    exit(kWrongConfiguration);
  }
  if (fPMT_Efficiency<0.0 || fPMT_Efficiency>1.0) {
    std::cerr << "[CedarReconstruction] ERROR: global PMT efficiency outside the range [0.0-1.0]" << std::endl;
    exit(kWrongConfiguration);
  }
  if (printCedarParameters) PrintCedarDigitizerParameters();

  if (!fAlignEnabled && fAlign) { //delete alignment module
    delete fAlign;
    fAlign=0;
  }
}

void CedarReconstruction::PrintCedarDigitizerParameters() {
  std::cout << "#CedarDigitizer parameters are\n"
    << "PMTTime_Response = " << fPMTTime_Response << std::endl
    << "PMTTime_min = " << fPMTTime_min << std::endl
    << "PMTTime_max = " << fPMTTime_max << std::endl
    << "PMTWidth_mean = " << fPMTWidth_mean << std::endl
    << "PMTWidth_sigma = " << fPMTWidth_sigma << std::endl
    << "PMT_Efficiency = " << fPMT_Efficiency << std::endl
    << "PMT_MergeThreshold = " << fPMT_MergeThreshold << std::endl;
}

///////////////////////////////////////
// Read the channel slewing corrections

void CedarReconstruction::ReadSlewingCorrections() {
  NA62ConditionsService::GetInstance()->Open(fSlewingCorrFileName);
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fSlewingCorrFileName))) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("MinWidth")) {
      Double_t MinWidth = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof()*ns;
      for (Int_t iCh=0;iCh<fNChannels;iCh++) static_cast<CedarChannel*>(fChannels[iCh])->SetMinWidth(MinWidth);
    }
    else if (Line.BeginsWith("MaxWidth")) {
      Double_t MaxWidth = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof()*ns;
      for (Int_t iCh=0;iCh<fNChannels;iCh++) static_cast<CedarChannel*>(fChannels[iCh])->SetMaxWidth(MaxWidth);
    }
    else if (Line.BeginsWith("SlewingCorr")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t ch = static_cast<TObjString*>(l->At(1))->GetString().Atoi();
      if (ch<fNChannels) {
        Double_t sc_wmin = static_cast<TObjString*>(l->At(2))->GetString().Atof();
        Double_t sc_wmax = static_cast<TObjString*>(l->At(3))->GetString().Atof();
        static_cast<CedarChannel*>(fChannels[ch])->SetSlewingCorrectionParameters(sc_wmin, sc_wmax);
      }
      delete l;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fSlewingCorrFileName);
}

//////////////////////////////////////////////////////////

TDetectorVEvent* CedarReconstruction::Trigger (TDetectorVEvent* tEvent, Event* /*tGenEvent*/) {
  return tEvent;
}

void CedarReconstruction::StartOfBurst() {
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  for (Int_t i=0; i<fNSectors; i++) {
    fNRecoHitsIntegrated[i] = 0;
    fNCoincidencesIntegrated[i] = 0;
  }
  fDIMInfo.Clear();
  for(UInt_t iSlot=0;iSlot<fNSelectedCandidatesInTimeSlots.size();iSlot++) fNSelectedCandidatesInTimeSlots[iSlot]=0;
}

void CedarReconstruction::EndOfBurst() {
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors

  //cout << "//---------- Cedar EOB Info: ---------//" << std::endl;
  //cout << "EOBTime:          " << fDIMInfo.GetEOBTime()           << std::endl;
  //cout << "TempRear:         " << fDIMInfo.GetTempRear()          << std::endl;
  //cout << "TempFront:        " << fDIMInfo.GetTempFront()         << std::endl;
  //cout << "TempDiaph:        " << fDIMInfo.GetTempDiaph()         << std::endl;
  //cout << "MotorPosX:        " << fDIMInfo.GetMotorPosX()         << std::endl;
  //cout << "MotorPosY:        " << fDIMInfo.GetMotorPosY()         << std::endl;
  //cout << "Diaphragm:        " << fDIMInfo.GetDiaphragmAperture() << std::endl;
  //cout << "PressureState:    " << fDIMInfo.GetPressureState()     << std::endl;
  //cout << "Pressure:         " << fDIMInfo.GetPressure()          << std::endl;
  //cout << "PressureSetPoint: " << fDIMInfo.GetPressureSetPoint()  << std::endl;
  //cout << "HVStatus:         " << fDIMInfo.GetHVStatus()          << std::endl;
  //cout << "FEStatus:         " << fDIMInfo.GetFEStatus()          << std::endl;
  //cout << "CedarStatus:      " << fDIMInfo.GetCedarStatus()       << std::endl;
  //cout << "AlignStatus:      " << fDIMInfo.GetAlignStatus()       << std::endl;
  //cout << "KTAGEnvStatus:    " << fDIMInfo.GetKTAGEnvStatus()     << std::endl;
  //cout << "KTAGStatus:       " << fDIMInfo.GetKTAGStatus()        << std::endl;
  //cout << "WienerStatus:     " << fDIMInfo.GetWienerStatus()      << std::endl;
  //cout << "//------------------------------------//" << std::endl;

  //Pass Motor information to Alignment
  if (fAlignEnabled) fAlign->SetMotorPosXY(fDIMInfo.GetMotorPosX(), fDIMInfo.GetMotorPosY());

  // Kaon rate evaluation
  Double_t TotalTimePerSlot = static_cast<NA62Reconstruction*>(fMainReco)->GetNProcessedEventsInFile()*1.e-3*ClockPeriod; //us

  UInt_t NSlotsUsed = 0;
  Double_t NSelectedCandidatesInTimeSlotsMean  = 0.;
  Int_t LastSlotID = fRawDecoder->GetDecoder()->GetLastSlotID(0);
  for(UInt_t iSlot=1; iSlot<fNSelectedCandidatesInTimeSlots.size()-1; iSlot++){ // Skip first and last slot
    if(iSlot==fNSelectedCandidatesInTimeSlots.size()-LastSlotID-1) continue; // Skip trigger slot
    NSelectedCandidatesInTimeSlotsMean  += fNSelectedCandidatesInTimeSlots[iSlot];
    NSlotsUsed++;
  }
  NSelectedCandidatesInTimeSlotsMean /= NSlotsUsed;

  Double_t NSelectedCandidatesInTimeSlotsSigma = 0.;
  for(UInt_t iSlot=1; iSlot<fNSelectedCandidatesInTimeSlots.size()-1; iSlot++){ // Skip first and last slot
    if(iSlot==fNSelectedCandidatesInTimeSlots.size()-LastSlotID-1) continue; // Skip trigger slot
    NSelectedCandidatesInTimeSlotsSigma += pow(fNSelectedCandidatesInTimeSlots[iSlot]-NSelectedCandidatesInTimeSlotsMean,2.);
  }
  NSelectedCandidatesInTimeSlotsSigma = sqrt(NSelectedCandidatesInTimeSlotsSigma)/(NSlotsUsed*(NSlotsUsed-1));

  Double_t KaonRateMean  = NSelectedCandidatesInTimeSlotsMean/TotalTimePerSlot; //MHz
  Double_t KaonRateSigma = NSelectedCandidatesInTimeSlotsSigma/TotalTimePerSlot; //MHz
  std::cout << "[CedarReconstruction] Mean Kaon Rate: (" << KaonRateMean << "+/-" << KaonRateSigma << ") MHz" << std::endl;
  static_cast<NA62Reconstruction*>(fMainReco)->SetKaonRate(KaonRateMean);
  static_cast<NA62Reconstruction*>(fMainReco)->SetKaonRateError(KaonRateSigma);
}

/////////////////////////////////////////////////////////

TRecoVEvent* CedarReconstruction::ProcessEvent (TDetectorVEvent* tEvent, Event* tGenEvent) {

  if (tEvent->IsA()->InheritsFrom("TSpecialTriggerEvent")){
    for(Int_t iSpecTrig=0; iSpecTrig<tEvent->GetNHits(); iSpecTrig++){
      if(!isL0EOB(tEvent->GetTriggerType())) continue; //skip SOB
      fDIMInfo = static_cast<TCedarSpecialTriggerEvent*>(tEvent)->GetDIMInfo(); // copy DIM Info
      TTDCBSpecialTrigger * SpecTrig = reinterpret_cast<TTDCBSpecialTrigger *>(tEvent->GetHit(iSpecTrig));
      if(!SpecTrig) continue;
      if(isL0EOB(SpecTrig->GetTriggerType())){ //get TEL62 EOB information
        PrimCounter* ChannelCounts = SpecTrig->GetCounter("CHANNEL_COUNT_L");
        if(ChannelCounts){
          CedarChannelID* CedarCh = new CedarChannelID();
          for(UInt_t iEntry=0;iEntry<ChannelCounts->GetNEntries();iEntry++){
            Int_t ChannelID = ChannelCounts->GetChannelID(iEntry);
            Int_t NCounts   = ChannelCounts->GetValue(iEntry);
            if(ChannelID<0) continue; // Masked channel
            //cout << "Cedar EOB counts: " << ChannelID << " " << NCounts << std::endl;
            CedarCh->DecodeChannelID(ChannelID);
            Int_t iSector   = CedarCh->GetSectorID();
            Int_t iRow      = CedarCh->GetRowID();
            Double_t xSec   = sin(0.25*TMath::Pi()*(iSector-0.5));
            Double_t ySec   = cos(0.25*TMath::Pi()*(iSector-0.5));
            fHNRecoHitsInSector[1]->Fill(iSector,NCounts);
            fHNRecoHitsInSectorVis[1]->Fill(xSec,ySec,NCounts);
            fHNRecoHitsInRow[1]->Fill(iSector*10+iRow,NCounts);
            fHNRecoHitsChannelProfile[1]->Fill(ChannelID,NCounts);
	    Int_t iCone = CedarCh->GetConeID();
            fHChannelProfileInSectorVis[iSector-1][1]->Fill(iCone-0.5*(iRow%2),-iRow,NCounts);
	    /*
            fHChannelProfileInSector[iSector-1][1]->Fill(iRow*10+iCone,NCounts);
            fHRowProfileInSector[iSector-1][1]->Fill(iRow,NCounts);
	    */
          }
          delete CedarCh;
        }
      }
    }
    return 0;
  }
  for (Int_t ich=0; ich<fNChannels; ich++) fChannels[ich]->Reset();

  //common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  TDCEvent* TdcEvent = static_cast<TDCEvent*>(tEvent);
  TDCEventMonitor(TdcEvent);

  Int_t NDigis = TdcEvent->GetNHits();
  for (Int_t iDigi=0; iDigi<NDigis; iDigi++) {
    Int_t ChannelID = TdcEvent->GetHit(iDigi)->GetChannelID();
    Int_t ROID      = fRawDecoder->GetDecoder()->GetChannelRO(ChannelID);
    fChannels[ROID]->AddHit();
  }
  TClonesArray& Digis = (*(TdcEvent->GetHits()));

  /**************************************/
  /* Reconstruction of Cedar Candidates */
  /**************************************/

  // Reconstruct PMT hit times: Digis --> RecoHits

  for (Int_t iDigi=0; iDigi<NDigis; iDigi++) {

    TCedarDigi *Digi = static_cast<TCedarDigi*>(Digis[iDigi]);

    if (!fSectorIsEnabled[Digi->GetSectorID()-1]) continue; // reject hits in disabled sectors

    // Edge Requirement for the hit reconstruction:
    //  1 = at least one leading,  2 = at least one trailing, 3 = leading+trailing (default)
    //  0 = every possible hit (at least one leading or at least one trailing)
    // -1 = leading only,         -2 = trailing only
    Bool_t EdgeRequirement = (Digi->GetDetectedEdge()==3 || Digi->GetDetectedEdge()==fEdgeRequirement);
    if      (fEdgeRequirement<0) EdgeRequirement = (Digi->GetDetectedEdge()==abs(fEdgeRequirement));
    else if (!fEdgeRequirement)  EdgeRequirement = kTRUE;

    if (!EdgeRequirement) continue; //hits not matching required edge condition are not reconstructed

    Int_t ROChannelID = fRawDecoder->GetDecoder()->GetChannelRO(Digi->GetChannelID());

    Double_t RecoTime = GetRecoTime(Digi);
    fHRecoHitTime->Fill(RecoTime);

    TRecoCedarHit* RecoHit = static_cast<TRecoCedarHit*>(fRecoEvent->AddHit(Digi));
    RecoHit->DecodeChannelID(); // @@ should become redundant
    RecoHit->SetTime(RecoTime);
    RecoHit->SetWidth(Digi->GetTrailingEdge() - Digi->GetLeadingEdge());
    RecoHit->SetROChannelID(ROChannelID);

    fNRecoHitsIntegrated[Digi->GetSectorID()-1]++;
  }

  fHNRecoHits->Fill(fRecoEvent->GetNHits());

  TClonesArray& Hits = (*(fRecoEvent->GetHits()));

  fNSectorsOccupancy = 0;
  for (Int_t i=0; i<fNSectors; i++) fSectorOccupancy[i] = 0;
  for (Int_t iHit=0; iHit<fRecoEvent->GetNHits(); iHit++) {
    TRecoCedarHit *Hit = static_cast<TRecoCedarHit*>(Hits[iHit]);
    Int_t iSector    = Hit->GetSectorID();
    fSectorOccupancy[iSector-1]++;
  }
  for (Int_t i=0; i<fNSectors; i++){
    if(fSectorOccupancy[i]) fNSectorsOccupancy++;
  }
  fHNSectors->Fill(fNSectorsOccupancy);

  // RecoHits time clusterisation
  ReconstructCandidates(fRecoEvent);

  /********************************************************/
  /* Fill monitoring histograms related to the candidates */
  /********************************************************/

  Int_t NSelectedCandidates = 0;
  for (Int_t iCand=0; iCand<fRecoEvent->GetNCandidates(); iCand++) {

    TRecoCedarCandidate* Candidate = static_cast<TRecoCedarCandidate*>(fRecoEvent->GetCandidate(iCand));
    Double_t CandidateTime = Candidate->GetTime();
    Double_t TriggerTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256;

    for (Int_t iHit=0; iHit<Candidate->GetNHits(); iHit++) {
      TRecoCedarHit *Hit = static_cast<TRecoCedarHit*>(Candidate->GetHit(iHit));
      Int_t iSector      = Hit->GetSectorID();
      Int_t iRow         = Hit->GetRowID();
      Double_t xSec      = sin(0.25*TMath::Pi()*(iSector-0.5));
      Double_t ySec      = cos(0.25*TMath::Pi()*(iSector-0.5));
      //Int_t ROChannelID = fRawDecoder->GetDecoder()->GetChannelRO(Hit->GetChannelID());
      //Int_t ROBoardID   = ROChannelID/512;
      //Int_t NROBoards   = fRawDecoder->GetDecoder()->GetNROBoards();
      //Int_t FPGAID      = (ROChannelID-ROBoardID*512)/128;
      //Int_t NROMezzaninesPerFullBoard = fRawDecoder->GetDecoder()->GetNROMezzaninesPerFullBoard();

      fHNRecoHitsInSector[0]->Fill(iSector);
      fHNRecoHitsInSectorVis[0]->Fill(xSec,ySec);
      fHNRecoHitsInRow[0]->Fill(iSector*10+iRow);
      Int_t iCone = Hit->GetConeID();
      fHChannelProfileInSectorVis[iSector-1][0]->Fill(iCone-0.5*(iRow%2),-iRow);
      /*
      fHChannelProfileInSector[iSector-1][0]->Fill(iRow*10+iCone);
      fHRowProfileInSector[iSector-1][0]->Fill(iRow);
      */
      fHNRecoHitsChannelProfile[0]->Fill(Hit->GetChannelID());
      // "Selected" = matching minimum number of sectors and hits requirements
      if (Candidate->GetIsSelected()) {
        fHRecoHitTimeWrtCandidate->Fill(Hit->GetTime()-CandidateTime);
        fHRecoHitTimeWrtCandidateVsWidth->Fill(Hit->GetWidth(), Hit->GetTime()-CandidateTime);
        fHRecoHitTimeWrtCandidateVsNHitsPerCandidate->Fill(Candidate->GetNHits(), Hit->GetTime()-CandidateTime);
        fHRecoHitTimeWrtTriggerVsNHitsPerCandidate->Fill(Candidate->GetNHits(), Hit->GetTime()-TriggerTime);
        //fHRecoHitTimeWrtCandidateInSector[iSector-1]->Fill(Hit->GetTime()-CandidateTime);
	/*
        if(ROBoardID<NROBoards && FPGAID<NROMezzaninesPerFullBoard){
          fHRecoHitTimeWrtCandidateInFPGA[ROBoardID][FPGAID]->Fill(Hit->GetTime()-CandidateTime);
          fHRecoHitTimeWrtCandidateInFPGA[ROBoardID][NROMezzaninesPerFullBoard]->Fill(Hit->GetTime()-CandidateTime); //all the PPs
          fHRecoHitTimeWrtCandidateInFPGA[NROBoards][FPGAID]->Fill(Hit->GetTime()-CandidateTime); //all the TEL62s
          fHRecoHitTimeWrtCandidateInFPGA[NROBoards][NROMezzaninesPerFullBoard]->Fill(Hit->GetTime()-CandidateTime); //all the TEL62s, all the PPs
        }
        else{
          std::cerr << "[CedarReconstruction] WARNING: overflow in ";
          if((UInt_t)ROBoardID>=fRawDecoder->GetDecoder()->GetNROBoards()) std::cerr << "ROBoardID: ROBoardID=" << ROBoardID << ", fNROBoards=" << fRawDecoder->GetDecoder()->GetNROBoards();
          else                      std::cerr << "FPGAID: FPGAID=" << FPGAID << ", NROMezzaninesPerFullBoard=" << NROMezzaninesPerFullBoard;
          std::cerr << std::endl;
        }
	*/
      }
    }

    fHNRecoHitsInCandidate->Fill(Candidate->GetNHits());
    fHNSectorsInCandidate->Fill(Candidate->GetNSectors());
    if (Candidate->GetIsSelected()) {
      fHNRecoHitsInSelectedCandidate->Fill(Candidate->GetNHits());
      fHNSectorsInSelectedCandidate->Fill(Candidate->GetNSectors());
      fHCandidateTimeWrtTriggerVsNHitsPerCandidate->Fill(Candidate->GetNHits(), CandidateTime-TriggerTime);
      Int_t NSlots = fNSelectedCandidatesInTimeSlots.size();
      Int_t LastSlotID = fRawDecoder->GetDecoder()->GetLastSlotID(0);
      for(Int_t iSlot=LastSlotID-NSlots+1; iSlot<=LastSlotID; iSlot++){
        if (iSlot*ClockPeriod<Candidate->GetTime() && Candidate->GetTime()<=(iSlot+1.)*ClockPeriod) {
          fNSelectedCandidatesInTimeSlots[NSlots-1+iSlot-LastSlotID]++;
        }
      }
      NSelectedCandidates++;
    }
    /*
    for (Int_t iSec=0; iSec<fNSectors; iSec++) if (fSectorIsEnabled[iSec]) {
      fHNRecoHitsInCandidateInSector[iSec]->Fill(NHitsInSectorPerCandidate[iCand][iSec]);
    }
    */
    for (Int_t iCoinc=0; iCoinc<Candidate->GetNSectors(); iCoinc++) {
      fNCoincidencesIntegrated[iCoinc]++;
    }
  }
  fHNCandidates->Fill(fRecoEvent->GetNCandidates());
  fHNSelectedCandidates->Fill(NSelectedCandidates);

  //Fill alignment info
  if(fAlignEnabled) fAlign->Fill(*static_cast<TRecoCedarEvent*>(fRecoEvent));

  /****************************************************************/

  return fRecoEvent;
}

// Compute the reconstructed hit time
Double_t CedarReconstruction::GetRecoTime (TCedarDigi *Digi) {
  Int_t PositionID       = Digi->GetChannelID();
  Int_t ROchannel        = fRawDecoder->GetDecoder()->GetChannelRO(PositionID);
  Double_t T0            = (fEnableT0) ? fChannels[ROchannel]->GetT0() : 0.0;
  Double_t LeadingEdge   = Digi->GetLeadingEdge();
  Double_t TrailingEdge  = Digi->GetTrailingEdge();
  Double_t SlewCorr      = (fEnableSlewingCorr) ? static_cast<CedarChannel*>(fChannels[ROchannel])->GetSlewingCorrection(TrailingEdge-LeadingEdge) : 0.0;
  Double_t time          = LeadingEdge-GetT0Correction(Digi)-T0-SlewCorr; //T0 channel-by-channel to be included soon
  return time;
}

///////////////////////////////////////////////////////////////
// TDC event monitoring
///////////////////////////////////////////////////////////////

void CedarReconstruction::TDCEventMonitor(TDCEvent* TdcEvent) {

  Int_t NDigis = TdcEvent->GetNHits();
  TClonesArray& Digis = (*(TdcEvent->GetHits()));

  for (Int_t iDigi=0; iDigi<NDigis; iDigi++) {
    TCedarDigi *Digi = static_cast<TCedarDigi*>( Digis[iDigi]);

    Int_t    Status          = Digi->GetDetectedEdge(); // 1=leading, 2=trailing, 3=both
    Double_t LeadingTimeRaw  = Digi->GetLeadingEdge();
    Double_t TrailingTimeRaw = Digi->GetTrailingEdge();

    fHHitStatus->Fill(Status);
    if (Status==3) {
      fHTimeWidth->Fill(TrailingTimeRaw-LeadingTimeRaw);
    }
  }
}

///////////////////////////////////////////////////////////////////////////

void CedarReconstruction::EndProcessing() {
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  if (fEvaluateSlewingCorr) EvaluateSlewingCorrections();
  SaveHistograms();
}

void CedarReconstruction::FillTimes(Double_t ReferenceTime) {

  //common part for all the subdetectors
  NA62VReconstruction::FillTimes(ReferenceTime);

  if (fT0ReferenceDetector=="Cedar") {
    // Cedar is its own reference detector: find the closest candidate to ReferenceTime
    Double_t TimeWrtReference = 1.e28;
    Int_t ClosestCandidateID = -1;
    for (Int_t iCand=0; iCand<fRecoEvent->GetNCandidates(); iCand++) {
      TRecoCedarCandidate* Candidate = static_cast<TRecoCedarCandidate*>(fRecoEvent->GetCandidate(iCand));
      if (fabs(Candidate->GetTime()-ReferenceTime)<TimeWrtReference) {
        TimeWrtReference = fabs(Candidate->GetTime()-ReferenceTime);
        ClosestCandidateID = iCand;
      }
    }

    if (ClosestCandidateID == -1) return; // no candidates found

    // Use the closest candidate only
    TRecoCedarCandidate* Candidate = static_cast<TRecoCedarCandidate*>(fRecoEvent->GetCandidate(ClosestCandidateID));
    for (Int_t ihit=0; ihit<Candidate->GetNHits(); ihit++) {
      TRecoCedarHit *RecoHit = static_cast<TRecoCedarHit*>(Candidate->GetHit(ihit));
      Int_t    iROCh = RecoHit->GetROChannelID();
      Double_t Time  = RecoHit->GetTime();
      if (fHRecoHitTimeWrtReferenceVsROChannel)         fHRecoHitTimeWrtReferenceVsROChannel->Fill(iROCh, Time-ReferenceTime);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0)     fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(iROCh, Time+fChannels[iROCh]->GetT0()-ReferenceTime);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill(iROCh, RecoHit->GetDigi()->GetTime()-ReferenceTime);
      if (fChannelHistograms) fChannels[iROCh]->FillTime(RecoHit->GetDigi()->GetTime(), RecoHit->GetWidth(), ReferenceTime); // no slew!
    }
  }
  else { // Another detector is the Cedar reference detector: use of all RecoHits
    for (Int_t iHit=0; iHit<fRecoEvent->GetNHits(); iHit++) {
      TRecoCedarHit *RecoHit = static_cast<TRecoCedarHit*>(fRecoEvent->GetHit(iHit));
      Int_t    iROCh = RecoHit->GetROChannelID();
      Double_t Time  = RecoHit->GetTime();
      if (fHRecoHitTimeWrtReferenceVsROChannel)         fHRecoHitTimeWrtReferenceVsROChannel->Fill(iROCh, Time-ReferenceTime);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0)     fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(iROCh, Time+fChannels[iROCh]->GetT0()-ReferenceTime);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill(iROCh, RecoHit->GetDigi()->GetTime()-ReferenceTime);
      if (fChannelHistograms) fChannels[iROCh]->FillTime(RecoHit->GetDigi()->GetTime(), RecoHit->GetWidth(), ReferenceTime); // no slew!
    }
  }
}

void CedarReconstruction::EvaluateSlewingCorrections() {
  ofstream outfile ("Cedar-SlewingCorr.new");
  outfile << "### Cedar slewing constants" << std::endl << endl;
  outfile << "# The slewing correction is a linear function of the signal width in"<< std::endl;
  outfile << "# the width interval of (MinWidth, MaxWidth) and constant outside this interval."<< std::endl;
  outfile << "# Data format: RO channel, corrections at width=MinWidth and width=MaxWidth."<< std::endl;
  outfile << "# The values of MinWidth and MaxWidth are indicated below."<< std::endl<<endl;
  outfile << "#\n# Generated on "<< TimeString();
  outfile << "#"<< std::endl;

  outfile << "MinWidth " << fNewMinWidth << std::endl;
  outfile << "MaxWidth " << fNewMaxWidth << std::endl;
  for (Int_t ich=0; ich<fNChannels; ich++) {
    if (fChannels[ich]->GetGeoChannelID()>0) {
      static_cast<CedarChannel*>(fChannels[ich])->EvaluateSlewingCorrection();
      outfile << Form("SlewingCorr %04d %6.4f %6.4f\n", ich,
          static_cast<CedarChannel*>(fChannels[ich])->GetNewSlewingCorr_MinWidth(),
          static_cast<CedarChannel*>(fChannels[ich])->GetNewSlewingCorr_MaxWidth());
    }
  }
  outfile.close();
}

//////////////////////// Histograms handling //////////////////////////////

void CedarReconstruction::InitHistograms() {

  TDirectory *CedarDir = GetOrMakeDir(fHistoFile,"CedarMonitor");
  CedarDir->mkdir("CedarSectors");
  CedarDir->mkdir("CedarCoincidences");
  CedarDir->mkdir("CedarAlignment");
  CedarDir->mkdir("CedarTDAQ");
  if (fChannelHistograms) CedarDir->mkdir("CedarChannels");

  fHChannelProfileInSectorVis       = new TH2I**[fNSectors];
  /*
     fHChannelProfileInSector          = new TH1D**[fNSectors];
     fHRowProfileInSector              = new TH1D**[fNSectors];
     fHNRecoHitsInCandidateInSector    = new TH1D*[fNSectors];
     fHRecoHitTimeWrtCandidateInSector = new TH1D*[fNSectors];
     */

  fHistoFile->cd("CedarMonitor");

  const Int_t NTrigs = 2;
  TString TrigSuffix[NTrigs] = {
    "",
    "_EOB"
  };

  for (Int_t iSector=0; iSector<fNSectors; iSector++) {
    fHChannelProfileInSectorVis[iSector] = new TH2I*[NTrigs];
    for(Int_t iTrig=0; iTrig<NTrigs;iTrig++){
      fHChannelProfileInSectorVis[iSector][iTrig] = new TH2I(Form("ChannelProfileInSectorVis%d%s", iSector+1,TrigSuffix[iTrig].Data()), Form("2D Channel Profile In Sector %d %s", iSector+1,TrigSuffix[iTrig].Data()), 20, 0, 10, 18, -9, 0);
      fHChannelProfileInSectorVis[iSector][iTrig]->GetXaxis()->SetTitle("Cone");
      fHChannelProfileInSectorVis[iSector][iTrig]->GetYaxis()->SetTitle("Row");
    }
  }
  /*
     for (Int_t iSector=0; iSector<fNSectors; iSector++) {
     fHChannelProfileInSector[iSector]    = new TH1D*[NTrigs];
     fHRowProfileInSector[iSector]        = new TH1D*[NTrigs];
     for(Int_t iTrig=0; iTrig<NTrigs;iTrig++){
     fHChannelProfileInSector[iSector][iTrig] = new TH1D(Form("ChannelProfileInSector%d%s",iSector+1,TrigSuffix[iTrig].Data()), Form("Channel Profile In Sector %d %s", iSector+1,TrigSuffix[iTrig].Data()), 90, 0.5, 90.5);
     fHChannelProfileInSector[iSector][iTrig]->GetXaxis()->SetTitle("Channel position ID");


     fHRowProfileInSector[iSector][iTrig] = new TH1D(Form("RowProfileInSector%d%s", iSector+1,TrigSuffix[iTrig].Data()), Form("Row Profile In Sector %d %s", iSector+1,TrigSuffix[iTrig].Data()), 8, 0.5, 8.5);
     fHRowProfileInSector[iSector][iTrig]->GetXaxis()->SetTitle("Row ID");
     }
     fHNRecoHitsInCandidateInSector[iSector] = new TH1D(Form("NRecoHitsInCandidateInSector%d", iSector+1), Form("RecoHits Per Candidate In Sector %d", iSector+1), 15, -0.5, 14.5);
     fHNRecoHitsInCandidateInSector[iSector]->GetXaxis()->SetTitle("Number of reconstructed hits");

     fHRecoHitTimeWrtCandidateInSector[iSector] = new TH1D(Form("RecoHitTimeWrtCandidateInSector%d", iSector+1), Form("RecoHit-Candidate Time In Sector %d", iSector+1), 100*fTimeWindow, -fTimeWindow, fTimeWindow);
     fHRecoHitTimeWrtCandidateInSector[iSector]->GetXaxis()->SetTitle("Reconstructed hit time (ns)");
     }
     */

  fHNRecoHits = new TH1D("NRecoHits", "Number of RecoHits", 80, -0.5, 79.5);
  fHNRecoHits->GetXaxis()->SetTitle("Reconstructed hits");

  fHNSectors = new TH1D("NSectors", "Number of Sectors", 9, -0.5, 8.5);
  fHNSectors->GetXaxis()->SetTitle("Sectors");

  fHNCandidates = new TH1D("NCandidates", "Number of Candidates", 21, -0.5, 20.5);
  fHNCandidates->GetXaxis()->SetTitle("Candidates");

  fHNSelectedCandidates = new TH1D("NSelectedCandidates", "Number of Selected Candidates", 21, -0.5, 20.5);
  fHNSelectedCandidates->GetXaxis()->SetTitle("Selected Candidates");

  fHNRecoHitsInCandidate =  new TH1D("NRecoHitsInCandidate", "Number of RecoHits Per Candidate", 80, -0.5, 79.5);
  fHNRecoHitsInCandidate->GetXaxis()->SetTitle("Reconstructed hits");

  fHNRecoHitsInSelectedCandidate = new TH1D("NRecoHitsInSelectedCandidate", "Number of RecoHits Per Selected Candidate", 80, -0.5, 79.5);
  fHNRecoHitsInSelectedCandidate->GetXaxis()->SetTitle("Reconstructed hits");

  fHNSectorsInCandidate = new TH1D("NSectorsInCandidate", "Number of Sectors Per Candidate", 9, -0.5, 8.5);
  fHNSectorsInCandidate->GetXaxis()->SetTitle("Sectors");

  fHNSectorsInSelectedCandidate = new TH1D("NSectorsInSelectedCandidate", "Number of Sectors Per Selected Candidate", 9, -0.5, 8.5);
  fHNSectorsInCandidate->GetXaxis()->SetTitle("Sectors");

  fHNRecoHitsInSector       = new TH1D*[NTrigs];
  fHNRecoHitsInSectorVis    = new TH2I*[NTrigs];
  fHNRecoHitsInRow          = new TH1D*[NTrigs];
  fHNRecoHitsChannelProfile = new TH1D*[NTrigs];
  for(Int_t iTrig=0; iTrig<NTrigs;iTrig++){
    fHNRecoHitsInSector[iTrig] = new TH1D(Form("NRecoHitsInSector%s",TrigSuffix[iTrig].Data()), Form("Number of RecoHits Per Sector %s",TrigSuffix[iTrig].Data()), 8, 0.5, 8.5);
    fHNRecoHitsInSector[iTrig]->GetXaxis()->SetTitle("Sector ID");

    fHNRecoHitsInSectorVis[iTrig] = new TH2I(Form("NRecoHitsInSectorVis%s",TrigSuffix[iTrig].Data()), Form("Number of RecoHits Per Sector %s",TrigSuffix[iTrig].Data()), 12, -1.2, 1.2, 12, -1.2, 1.2);
    fHNRecoHitsInSectorVis[iTrig]->GetXaxis()->SetTitle("Jura - Saleve");
    fHNRecoHitsInSectorVis[iTrig]->GetYaxis()->SetTitle("Bottom - Top");

    fHNRecoHitsInRow[iTrig] = new TH1D(Form("NRecoHitsInRow%s",TrigSuffix[iTrig].Data()), Form("Number of RecoHits Per Row %s",TrigSuffix[iTrig].Data()), 80, 9.5, 89.5);
    fHNRecoHitsInRow[iTrig]->GetXaxis()->SetTitle("(Sector x10) + Row");

    fHNRecoHitsChannelProfile[iTrig] = new TH1D(Form("NRecoHitsChannelProfile%s",TrigSuffix[iTrig].Data()), Form("Hits by PositionID %s",TrigSuffix[iTrig].Data()), 800, 99.5, 899.5);
    fHNRecoHitsChannelProfile[iTrig]->GetXaxis()->SetTitle("PositionID");
    fHNRecoHitsChannelProfile[iTrig]->GetXaxis()->SetTitle("Events");
  }

  fHHitStatus = new TH1D("HitStatus", "Hit Status", 3, 0.5, 3.5);
  fHHitStatus->GetXaxis()->SetTitle("HitStatus: 1=leading edge only, 2=trailing edge only, 3=both");

  fHRecoHitTime = new TH1D("RecoHitTime", "RecoHit Time", 100, -50, 50);
  fHRecoHitTime->GetXaxis()->SetTitle("Reconstructed hit time (ns)");

  fHRecoHitTimeWrtCandidate = new TH1D("RecoHitTimeWrtCandidate", "RecoHit-Candidate Time", 100*fTimeWindow, -fTimeWindow, fTimeWindow);
  fHRecoHitTimeWrtCandidate->GetXaxis()->SetTitle("Reconstructed hit time - candidate time (ns)");

  fHRecoHitTimeWrtCandidateVsWidth = new TH2F("RecoHitTimeWrtCandidateVsWidth", "RecoHit-Candidate Time Vs Signal Width",
      300, 0.5*TdcCalib, 300.5*TdcCalib, 100*fTimeWindow, -fTimeWindow, fTimeWindow);
  fHRecoHitTimeWrtCandidateVsWidth->GetXaxis()->SetTitle("Time width (ns)");
  fHRecoHitTimeWrtCandidateVsWidth->GetYaxis()->SetTitle("Reconstructed hit time - candidate time (ns)");

  fHRecoHitTimeWrtCandidateVsNHitsPerCandidate = new TH2F("RecoHitTimeWrtCandidateVsNHitsPerCandidate",
      "RecoHit-Candidate Time Vs NHitsPerCandidate", 80, -0.5,79.5, 100*fTimeWindow, -fTimeWindow, fTimeWindow);
  fHRecoHitTimeWrtCandidateVsNHitsPerCandidate->GetXaxis()->SetTitle("Hits per candidate");
  fHRecoHitTimeWrtCandidateVsNHitsPerCandidate->GetYaxis()->SetTitle("Reconstructed hit time - candidate time (ns)");

  fHRecoHitTimeWrtTriggerVsNHitsPerCandidate = new TH2F("RecoHitTimeWrtTriggerVsNHitsPerCandidate",
      "RecoHit-Trigger Time Vs NHitsPerCandidate", 80, -0.5,79.5, 100*fTimeWindow, -fTimeWindow, fTimeWindow);
  fHRecoHitTimeWrtTriggerVsNHitsPerCandidate->GetXaxis()->SetTitle("Hits per candidate");
  fHRecoHitTimeWrtTriggerVsNHitsPerCandidate->GetYaxis()->SetTitle("Reconstructed hit time - trigger time (ns)");

  fHCandidateTimeWrtTriggerVsNHitsPerCandidate = new TH2F("CandidateTimeWrtTriggerVsNHitsPerCandidate",
      "Candidate-Trigger Time Vs NHitsPerCandidate", 80, -0.5,79.5, 100*fTimeWindow, -fTimeWindow, fTimeWindow);
  fHCandidateTimeWrtTriggerVsNHitsPerCandidate->GetXaxis()->SetTitle("Hits per candidate");
  fHCandidateTimeWrtTriggerVsNHitsPerCandidate->GetYaxis()->SetTitle("Candidate time - trigger time (ns)");

  /*
     TString hname_tel62;
     TString hname_tel62_pp;
     fHRecoHitTimeWrtCandidateInFPGA = new TH1D**[fRawDecoder->GetDecoder()->GetNROBoards()+1];
     Int_t NROMezzaninesPerFullBoard = fRawDecoder->GetDecoder()->GetNROMezzaninesPerFullBoard();
     for(UInt_t iROBoard=0; iROBoard<=fRawDecoder->GetDecoder()->GetNROBoards(); iROBoard++){ //0-n-1: ith Tel62, n: all
     if(iROBoard<fRawDecoder->GetDecoder()->GetNROBoards()) hname_tel62 = Form("ktag%d",iROBoard);
     else hname_tel62 = "all";
     fHRecoHitTimeWrtCandidateInFPGA[iROBoard] = new TH1D*[NROMezzaninesPerFullBoard+1];
     for(Int_t iFPGA=0;iFPGA<=NROMezzaninesPerFullBoard;iFPGA++){ //0-n-1: ith PP, n: all
     if(iFPGA<NROMezzaninesPerFullBoard) hname_tel62_pp = Form("%s_pp%d",hname_tel62.Data(),iFPGA);
     else             hname_tel62_pp = hname_tel62;
     fHRecoHitTimeWrtCandidateInFPGA[iROBoard][iFPGA] = new TH1D("RecoHitTimeWrtCandidateInFPGA_"+hname_tel62_pp, "RecoHit-Candidate Time In ("+hname_tel62_pp+")", 100*fTimeWindow, -fTimeWindow, fTimeWindow);
     fHRecoHitTimeWrtCandidateInFPGA[iROBoard][iFPGA]->GetXaxis()->SetTitle("Reconstructed hit time (ns)");
     }
     }
     */

  fHTimeWidth = new TH1D("TimeWidth", "Signal Width", 300, 0.5*TdcCalib, 300.5*TdcCalib);
  fHTimeWidth->GetXaxis()->SetTitle("Time width (ns)");

  fHAsymUpDown   = new TH1D("AsymmetryUpDown",     "UP-DOWN asymmetry",     100, -1, 1);
  fHAsymSalvJura = new TH1D("AsymmetrySaleveJura", "SALEVE-JURA asymmetry", 100, -1, 1);
  fHAsymUpDown->GetXaxis()->SetTitle("(Up-Down)/(Up+Down)");
  fHAsymUpDown->GetYaxis()->SetTitle("Bursts");
  fHAsymSalvJura->GetXaxis()->SetTitle("(Saleve-Jura)/(Saleve+Jura)");
  fHAsymSalvJura->GetYaxis()->SetTitle("Bursts");
}

void CedarReconstruction::SaveHistograms() {

  const Int_t NTrigs = 2;
  fHistoFile->cd("CedarMonitor");

  fHHitStatus->Write();
  fHRecoHitTime->Write();
  fHRecoHitTimeWrtCandidate->Write();
  fHRecoHitTimeWrtCandidateVsWidth->Write();
  fHRecoHitTimeWrtCandidateVsNHitsPerCandidate->Write();
  fHRecoHitTimeWrtTriggerVsNHitsPerCandidate->Write();
  fHCandidateTimeWrtTriggerVsNHitsPerCandidate->Write();
  fHTimeWidth->Write();

  fHNCandidates->Write();
  fHNSelectedCandidates->Write();
  fHNRecoHitsInSelectedCandidate->Write();
  fHNSectorsInSelectedCandidate->Write();
  fHNRecoHits->Write();
  fHNSectors->Write();
  fHNRecoHitsInCandidate->Write();
  fHNSectorsInCandidate->Write();
  fHNRecoHitsInSelectedCandidate->Write();
  fHNSectorsInSelectedCandidate->Write();
  for(UInt_t iTrig=0;iTrig<NTrigs;iTrig++){
    fHNRecoHitsChannelProfile[iTrig]->Write();
    fHNRecoHitsInSector[iTrig]->Write();
    fHNRecoHitsInSectorVis[iTrig]->Write();
    fHNRecoHitsInRow[iTrig]->Write();
  }

  fHistoFile->cd("CedarMonitor/CedarCoincidences");

  /*
     fHistoFile->cd("CedarMonitor/CedarSectors");
     for (Int_t iSector=0; iSector<fNSectors; iSector++) {
     if (fSectorIsEnabled[iSector]) {
     for(UInt_t iTrig=0;iTrig<NTrigs;iTrig++){
     fHChannelProfileInSector[iSector][iTrig]->Write();
     fHChannelProfileInSectorVis[iSector][iTrig]->Write();
     fHRowProfileInSector[iSector][iTrig]->Write();
     }
     fHNRecoHitsInCandidateInSector[iSector]->Write();
     fHRecoHitTimeWrtCandidateInSector[iSector]->Write();
     }
     }
     */

  if (fChannelHistograms) {
    for (Int_t iCh=0; iCh<fNChannels; iCh++) fChannels[iCh]->Write(fHistoFile);
  }

  fHistoFile->cd("CedarMonitor/CedarAlignment");
  if (fAlignEnabled) {
    fAlign->ComputeAlignment();
    fAlign->ResetHits();
  }

  /*
     fHistoFile->cd("CedarMonitor/CedarTDAQ");
     Int_t NROMezzaninesPerFullBoard = fRawDecoder->GetDecoder()->GetNROMezzaninesPerFullBoard();
     for (UInt_t iROBoard=0; iROBoard<=fRawDecoder->GetDecoder()->GetNROBoards(); iROBoard++){
     for (int iFPGA=0; iFPGA<=NROMezzaninesPerFullBoard; iFPGA++){
     fHRecoHitTimeWrtCandidateInFPGA[iROBoard][iFPGA]->Write();
     }
     }
     */
  fHistoFile->cd("/");
}

void CedarReconstruction::ResetHistograms() {

  fHChannelProfileInSector = 0;
  fHChannelProfileInSectorVis = 0;
  fHRowProfileInSector = 0;
  fHNRecoHitsInCandidateInSector = 0;
  fHRecoHitTimeWrtCandidateInSector = 0;

  fHNRecoHits = 0;
  fHNSectors = 0;
  fHNCandidates = 0;
  fHNSelectedCandidates = 0;
  fHNRecoHitsInCandidate = 0;
  fHNRecoHitsInSelectedCandidate = 0;
  fHNSectorsInCandidate = 0;
  fHNSectorsInSelectedCandidate = 0;
  fHNRecoHitsInSector = 0;
  fHNRecoHitsInSectorVis = 0;
  fHNRecoHitsInRow = 0;
  fHHitStatus = 0;
  fHRecoHitTime = 0;
  fHRecoHitTimeWrtCandidate = 0;
  fHRecoHitTimeWrtCandidateVsWidth = 0;
  fHRecoHitTimeWrtCandidateVsNHitsPerCandidate = 0;
  fHRecoHitTimeWrtTriggerVsNHitsPerCandidate = 0;
  fHCandidateTimeWrtTriggerVsNHitsPerCandidate = 0;
  fHRecoHitTimeWrtCandidateInFPGA = 0;
  fHTimeWidth = 0;
  fHAsymUpDown = 0;
  fHAsymSalvJura = 0;
  fHNRecoHitsChannelProfile = 0;
}

void CedarReconstruction::DeleteHistograms() {

  for (Int_t iSector=0; iSector<fNSectors; iSector++) {
    if(fHChannelProfileInSector && fHChannelProfileInSector[iSector])                   delete [] fHChannelProfileInSector[iSector];
    if(fHChannelProfileInSectorVis && fHChannelProfileInSectorVis[iSector])             delete [] fHChannelProfileInSectorVis[iSector];
    if(fHRowProfileInSector && fHRowProfileInSector[iSector])                           delete [] fHRowProfileInSector[iSector];
    if(fHNRecoHitsInCandidateInSector && fHNRecoHitsInCandidateInSector[iSector])       delete fHNRecoHitsInCandidateInSector[iSector];
    if(fHRecoHitTimeWrtCandidateInSector && fHRecoHitTimeWrtCandidateInSector[iSector]) delete fHRecoHitTimeWrtCandidateInSector[iSector];
  }

  if(fHChannelProfileInSector)          delete [] fHChannelProfileInSector;
  if(fHChannelProfileInSectorVis)       delete [] fHChannelProfileInSectorVis;
  if(fHRowProfileInSector)              delete [] fHRowProfileInSector;
  if(fHNRecoHitsInCandidateInSector)    delete [] fHNRecoHitsInCandidateInSector;
  if(fHRecoHitTimeWrtCandidateInSector) delete [] fHRecoHitTimeWrtCandidateInSector;

  if(fHNRecoHits)                              delete fHNRecoHits;
  if(fHNSectors)                               delete fHNSectors;
  if(fHNCandidates)                            delete fHNCandidates;
  if(fHNSelectedCandidates)                    delete fHNSelectedCandidates;
  if(fHNRecoHitsInCandidate)                   delete fHNRecoHitsInCandidate;
  if(fHNRecoHitsInSelectedCandidate)           delete fHNRecoHitsInSelectedCandidate;
  if(fHNSectorsInCandidate)                    delete fHNSectorsInCandidate;
  if(fHNSectorsInSelectedCandidate)            delete fHNSectorsInSelectedCandidate;
  if(fHNRecoHitsInSector)                      delete [] fHNRecoHitsInSector;    //NTrigs array
  if(fHNRecoHitsInSectorVis)                   delete [] fHNRecoHitsInSectorVis; //NTrigs array
  if(fHNRecoHitsInRow)                         delete [] fHNRecoHitsInRow;       //NTrigs array
  if(fHHitStatus)                              delete fHHitStatus;
  if(fHRecoHitTime)                            delete fHRecoHitTime;
  if(fHRecoHitTimeWrtCandidate)                delete fHRecoHitTimeWrtCandidate;
  if(fHRecoHitTimeWrtCandidateVsWidth)         delete fHRecoHitTimeWrtCandidateVsWidth;
  if(fHRecoHitTimeWrtCandidateVsNHitsPerCandidate) delete fHRecoHitTimeWrtCandidateVsNHitsPerCandidate;
  if(fHRecoHitTimeWrtTriggerVsNHitsPerCandidate)   delete fHRecoHitTimeWrtTriggerVsNHitsPerCandidate;
  if(fHCandidateTimeWrtTriggerVsNHitsPerCandidate) delete fHCandidateTimeWrtTriggerVsNHitsPerCandidate;

  if(fRawDecoder && fRawDecoder->GetDecoder()){
    Int_t NROMezzaninesPerFullBoard = fRawDecoder->GetDecoder()->GetNROMezzaninesPerFullBoard();
    for(UInt_t iROBoard=0; iROBoard<=fRawDecoder->GetDecoder()->GetNROBoards(); iROBoard++){ //0-n-1: ith Tel62, n: all
      for(Int_t iFPGA=0;iFPGA<=NROMezzaninesPerFullBoard;iFPGA++){ //0-n-1: ith PP, n: all
        if(fHRecoHitTimeWrtCandidateInFPGA && fHRecoHitTimeWrtCandidateInFPGA[iROBoard] && fHRecoHitTimeWrtCandidateInFPGA[iROBoard][iFPGA])
          delete fHRecoHitTimeWrtCandidateInFPGA[iROBoard][iFPGA];
      }
      if(fHRecoHitTimeWrtCandidateInFPGA && fHRecoHitTimeWrtCandidateInFPGA[iROBoard]) delete [] fHRecoHitTimeWrtCandidateInFPGA[iROBoard];
    }
  }
  if(fHRecoHitTimeWrtCandidateInFPGA) delete [] fHRecoHitTimeWrtCandidateInFPGA;

  if(fHTimeWidth)               delete fHTimeWidth;
  if(fHAsymUpDown)              delete fHAsymUpDown;
  if(fHAsymSalvJura)            delete fHAsymSalvJura;
  if(fHNRecoHitsChannelProfile) delete [] fHNRecoHitsChannelProfile;
  ResetHistograms();
}

void CedarReconstruction::ReconstructCandidates(TRecoVEvent* RecoEvent){

  std::vector<int> NHitsInSectorPerEachCandidate;
  std::vector< vector<int> > NHitsInSectorPerCandidate;
  std::vector<int> NHitsInPMTPerEachCandidate;

  TRecoCedarCandidate* CurrentCandidate = nullptr;

  TClonesArray& Hits = (*(RecoEvent->GetHits()));

  for (Int_t iIter=0; iIter<fNCandidateClusteringIterations; iIter++) {

    // One or two iterations:
    // 1) define a new cluster if a hit away more than 0.5*fTimeWindow is found from the existing clusters
    // 2) remove the "bad" clusters and try to include them in existing "good" clusters

    for (Int_t iHit=0; iHit<RecoEvent->GetNHits(); iHit++) {
      TRecoCedarHit *Hit = static_cast<TRecoCedarHit*>(Hits[iHit]);
      Bool_t HitFromNewCluster = kTRUE;
      Double_t HitTime = Hit->GetTime();
      Int_t iSector    = Hit->GetSectorID();

      if (fSectorIsEnabled[iSector-1]) {

        // find the candidate closest in time to the hit
        Double_t HitCandidateTime = 1e28*ns;
        Int_t CandidateID = -1;
        for (Int_t iCand=0; iCand<RecoEvent->GetNCandidates(); iCand++) {
          TRecoCedarCandidate* Candidate = static_cast<TRecoCedarCandidate*>(RecoEvent->GetCandidate(iCand));
          if (fabs(Candidate->GetTime()-HitTime)<HitCandidateTime) {
            HitCandidateTime = fabs(Candidate->GetTime()-HitTime);
            CandidateID = iCand;
          }
        }

        if (CandidateID>=0) {
          CurrentCandidate = static_cast<TRecoCedarCandidate*>(RecoEvent->GetCandidate(CandidateID));
          if (fabs(CurrentCandidate->GetTime()-HitTime)<0.5*fTimeWindow)
            HitFromNewCluster = kFALSE;
        }

        if (HitFromNewCluster) { //hit not consistent with previous candidates: create a new candidate
          NHitsInSectorPerEachCandidate.clear();
          NHitsInSectorPerEachCandidate.resize(fNSectors);
          NHitsInSectorPerCandidate.push_back(NHitsInSectorPerEachCandidate);
          NHitsInPMTPerEachCandidate.clear();
          NHitsInPMTPerEachCandidate.resize(fNChannels);
          CandidateID = RecoEvent->GetNCandidates();
          CurrentCandidate = static_cast<TRecoCedarCandidate*>(RecoEvent->AddCandidate());
        }

        Bool_t MissingHit = kTRUE;
        if (iIter>0) { //second iteration: check to see if current hit is already in a candidate
          for (Int_t i=0; i<CurrentCandidate->GetNHits(); i++) {
            if (CurrentCandidate->GetHitsIndexes()[i]==iHit) MissingHit = kFALSE;
          }
        }

        if (MissingHit) { //missing hit: add it to the current candidate (closest or new)
          CurrentCandidate->AddHit(iHit);
          CurrentCandidate->UpdateTime(HitTime);
          if (!NHitsInSectorPerCandidate[CandidateID][iSector-1]) CurrentCandidate->SetNSectors(CurrentCandidate->GetNSectors()+1);
          NHitsInSectorPerCandidate[CandidateID][iSector-1]++;
        }
      }
    }

    // End of iteration: calculate the candidate info
    // (DeltaTimeClosestCandidate, NHitsClosestCandidate, IsSelected)
    for (Int_t kCand=0; kCand<RecoEvent->GetNCandidates(); kCand++) {
      CurrentCandidate = static_cast<TRecoCedarCandidate*>(RecoEvent->GetCandidate(kCand));
      Double_t CandidateTime = CurrentCandidate->GetTime();

      // labelling selected candidates
      if (CurrentCandidate->GetNSectors()>=fMinSectors && CurrentCandidate->GetNHits()>=fMinPMTs)
        CurrentCandidate->SetIsSelected(kTRUE);
      Double_t DeltaTimeClosestCandidate = 1.e28; //ns
      Int_t NHitsClosestCandidate = 0;
      //Int_t iClosestCandidate = -1;

      // check the minimum time distance from any other candidate
      for (Int_t jCand=0; jCand<RecoEvent->GetNCandidates(); jCand++) {
        TRecoCedarCandidate *OtherCandidate = static_cast<TRecoCedarCandidate*>(RecoEvent->GetCandidate(jCand));
        if (kCand!=jCand && fabs(OtherCandidate->GetTime()-CandidateTime)<fabs(DeltaTimeClosestCandidate)) {
          DeltaTimeClosestCandidate = OtherCandidate->GetTime() - CandidateTime;
          NHitsClosestCandidate = OtherCandidate->GetNHits();
          //iClosestCandidate = jCand;
        }
      }
      CurrentCandidate->SetDeltaTimeClosestCandidate(DeltaTimeClosestCandidate);
      CurrentCandidate->SetNHitsClosestCandidate(NHitsClosestCandidate);
    }

    //Remove all bad candidates and hits (if not last iteration)
    if (iIter<fNCandidateClusteringIterations-1) {
      for (Int_t kCand=0; kCand<RecoEvent->GetNCandidates(); kCand++) {
        CurrentCandidate = static_cast<TRecoCedarCandidate*>(RecoEvent->GetCandidate(kCand));
        if (!CurrentCandidate->GetIsSelected() ||
            (CurrentCandidate->GetNHits()<CurrentCandidate->GetNHitsClosestCandidate() &&
             CurrentCandidate->GetDeltaTimeClosestCandidate()<fTimeWindow)) { // remove bad candidates
          RecoEvent->RemoveCandidate(kCand);
          NHitsInSectorPerCandidate.erase(NHitsInSectorPerCandidate.begin()+kCand);
          kCand--;
        }
        else { // remove bad hits
          for (Int_t iHitCand=0; iHitCand<CurrentCandidate->GetNHits(); iHitCand++) {
            TRecoCedarHit *Hit = static_cast<TRecoCedarHit *>(CurrentCandidate->GetHit(iHitCand));
            Double_t HitTime   = Hit->GetTime();
            Double_t HitSector = Hit->GetSectorID();
            if (fabs(HitTime-CurrentCandidate->GetTime())>0.5*fTimeWindow) {
              CurrentCandidate->RemoveHit(iHitCand);
              CurrentCandidate->UpdateTime();
              NHitsInSectorPerCandidate[kCand][HitSector-1]--;
              if (!NHitsInSectorPerCandidate[kCand][HitSector-1]) CurrentCandidate->SetNSectors(CurrentCandidate->GetNSectors()-1);
              iHitCand--;
            }
          }
        }
      }
    }
  }
}
