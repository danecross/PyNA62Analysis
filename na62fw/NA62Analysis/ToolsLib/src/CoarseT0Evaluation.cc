// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"
#include "NA62ConditionsService.hh"
#include "TRegexp.h"
#include "TGraphErrors.h"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class CoarseT0Evaluation
/// \Brief
/// A generic tool for computation of the coarse T0 constants.
/// \EndBrief
///
/// \Detailed
/// A generic tool for computation of the coarse T0 constants for each subdetector.
/// Daughter classes for each subdetector are located in Analyzers/CalibrationTools.
/// The input for coarse T0 computations is a 2-dimensional histogram of DigiTimeRawFine vs (RO Mezzanine ID).
/// XML-like T0s are evaluated using the 2-dimensional histogram of DigiTimeRawFine vs (RO Channel ID).
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

CoarseT0Evaluation::CoarseT0Evaluation(Core::BaseAnalysis *ba, TString DetectorName) : Analyzer(ba,string(DetectorName+"CoarseT0")), fROMezzanineMasksPerBoard(0), fStationsT0(0), fStationsDeltaT0(0), fStationsSigmaT0(0), fStationsDeltaSigmaT0(0), fROMezzaninesT0(0), fROMezzaninesDeltaT0(0), fROMezzaninesSigmaT0(0), fROMezzaninesDeltaSigmaT0(0), fXMLT0(0), fXMLDeltaT0(0), fXMLSigmaT0(0), fXMLDeltaSigmaT0(0),fHDigiTimeRawFine(0), fHDigiTimeRawFineVsROChannel(0), fHNEventsProcessedPerBurst(0){

  // Defaults for the user-defined parameters
  fDetectorName = DetectorName;
  fNStations = 0;
  fNROMezzanines = 0;
  fNROChannels = 0;
  fNROMezzaninesPerFullBoard = 0;
  fNROBoardsPerStation.clear();
  fRawDecoderSettingsFileName = fDetectorName+"-RawDecoderSettings.dat";
  fGenerateOutputPDF = true;
  fOutPDFFileName = "./" + fDetectorName + "-CoarseT0.pdf";
  fHistoTimeLowerLimit = -1000.;
  fHistoTimeUpperLimit =  1000.;
  fHDigiTimeRawFinePerMezzanine.clear();
  fHDigiTimeRawFinePerStation.clear();
  fHDigiTimeRawFineXML.clear();
}

CoarseT0Evaluation::~CoarseT0Evaluation(){
  if(fROMezzanineMasksPerBoard) delete [] fROMezzanineMasksPerBoard;
  if(fStationsT0              ) delete [] fStationsT0;
  if(fStationsDeltaT0         ) delete [] fStationsDeltaT0;
  if(fStationsSigmaT0         ) delete [] fStationsSigmaT0;
  if(fStationsDeltaSigmaT0    ) delete [] fStationsDeltaSigmaT0;
  if(fStationsFitStatus       ) delete [] fStationsFitStatus;
  if(fROMezzaninesT0          ) delete [] fROMezzaninesT0;
  if(fROMezzaninesDeltaT0     ) delete [] fROMezzaninesDeltaT0;
  if(fROMezzaninesSigmaT0     ) delete [] fROMezzaninesSigmaT0;
  if(fROMezzaninesDeltaSigmaT0) delete [] fROMezzaninesDeltaSigmaT0;
  if(fROMezzaninesFitStatus   ) delete [] fROMezzaninesFitStatus;
  if(fXMLT0                   ) delete [] fXMLT0;
  if(fXMLDeltaT0              ) delete [] fXMLDeltaT0;
  if(fXMLSigmaT0              ) delete [] fXMLSigmaT0;
  if(fXMLDeltaSigmaT0         ) delete [] fXMLDeltaSigmaT0;
  if(fXMLFitStatus            ) delete [] fXMLFitStatus;
  for(UInt_t iMezzanine=0;iMezzanine<fHDigiTimeRawFinePerMezzanine.size();iMezzanine++){
    delete fHDigiTimeRawFinePerMezzanine[iMezzanine];
  }
  for(UInt_t iStation=0;iStation<fHDigiTimeRawFinePerStation.size();iStation++){
    delete fHDigiTimeRawFinePerStation[iStation];
  }
  for(UInt_t iHisto=0;iHisto<fHDigiTimeRawFineXML.size();iHisto++){
    delete fHDigiTimeRawFineXML[iHisto];
  }
}

void CoarseT0Evaluation::InitHist() {

  if (!GetIsHisto() || GetIsTree()) {
    cout << user_normal() << "ERROR: CoarseT0Evaluation-based analyzers must be run in the --histo mode" << endl;
    exit(kWrongConfiguration);
  }

  fHNEventsProcessedPerBurst = (TH1F*)RequestHistogram("/", "EventsPerBurst", true);  // accumulated
  fHDigiTimeRawFine = (TH2F*)RequestHistogram(fDetectorName+"Monitor", "DigiTimeRawFine", true);  // accumulated
  fHDigiTimeRawFineVsROChannel = (TH2F*)RequestHistogram(fDetectorName+"Monitor", "DigiTimeRawFineVsROChannel", true);  // accumulated
  if (!fHNEventsProcessedPerBurst) cerr << user_normal() << "WARNING: Histogram 'EventsPerBurst' not found" << endl;
  if (!fHDigiTimeRawFine) cerr << user_normal() << "WARNING: Histogram '" << fDetectorName << "Monitor/DigiTimeRawFine' not found" << endl;
  if (!fHDigiTimeRawFineVsROChannel) cerr << user_normal() << "WARNING: Histogram '" << fDetectorName << "Monitor/DigiTimeRawFineVsROChannel' not found" << endl;

  // Book monitoring histograms to be saved into the output
  BookHisto("StationsCoarseT0",               new TGraphErrors());
  BookHisto("StationsCoarseT0Resolution",     new TGraphErrors());
  BookHisto("ROMezzaninesCoarseT0",           new TGraphErrors());
  BookHisto("ROMezzaninesCoarseT0Resolution", new TGraphErrors());
  BookHisto("XMLT0",                          new TGraphErrors());
  BookHisto("XMLT0Resolution",                new TGraphErrors());
  fHisto.GetTGraph("StationsCoarseT0"              )->SetName("StationsCoarseT0"              );
  fHisto.GetTGraph("StationsCoarseT0Resolution"    )->SetName("StationsCoarseT0Resolution"    );
  fHisto.GetTGraph("ROMezzaninesCoarseT0"          )->SetName("ROMezzaninesCoarseT0"          );
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->SetName("ROMezzaninesCoarseT0Resolution");
  fHisto.GetTGraph("XMLT0"                         )->SetName("XMLT0"                         );
  fHisto.GetTGraph("XMLT0Resolution"               )->SetName("XMLT0Resolution"               );
  fHisto.GetTGraph("StationsCoarseT0"              )->SetTitle("StationsCoarseT0;StationID;CoarseT0 and its error [ns]"                    );
  fHisto.GetTGraph("StationsCoarseT0Resolution"    )->SetTitle("StationsCoarseT0Resolution;StationID;Peak width and its error [ns]"        );
  fHisto.GetTGraph("ROMezzaninesCoarseT0"          )->SetTitle("ROMezzaninesCoarseT0;ROMezzanineID;CoarseT0 and its error [ns]"            );
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->SetTitle("ROMezzaninesCoarseT0Resolution;ROMezzanineID;Peak width and its error [ns]");
  fHisto.GetTGraph("XMLT0"                         )->SetTitle("XMLT0;ROChannel;XML T0 and its error [ns]"                                 );
  fHisto.GetTGraph("XMLT0Resolution"               )->SetTitle("XMLT0Resolution;ROChannel;Peak width and its error [ns]"                   );
}

void CoarseT0Evaluation::StartOfRunUser() {
  // Find and read the detector-specific parameters (run-dependent!)
  ParseRawDecoderSettingsFile();
}

/////////////////////////////////////////////////////////////
// Read the detector-specific parameters from the RawDecoderSettings file

void CoarseT0Evaluation::ParseRawDecoderSettingsFile() {

  if (fNROMezzaninesPerFullBoard<=0) {
    cout << user_normal() << "ERROR: invalid number of ROMezzaninesPerFullBoard specified [="<<fNROMezzaninesPerFullBoard<<"]"<<endl;
    exit(kWrongConfiguration);
  }

  Int_t MaxChannelID = -1;
  TString Line;
  NA62ConditionsService::GetInstance()->Open(fRawDecoderSettingsFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fRawDecoderSettingsFileName))) {
    if (Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("NROBoards=")){
      TObjArray * l = Line.Tokenize(" ");
      fNROBoards = ((TObjString*)(l->At(1)))->GetString().Atoi();
      fNROMezzanines = fNROBoards*fNROMezzaninesPerFullBoard;
      if(fNROMezzanines) {
        fROMezzaninesT0 = new Double_t[fNROMezzanines];
        fROMezzaninesDeltaT0 = new Double_t[fNROMezzanines];
        fROMezzaninesSigmaT0 = new Double_t[fNROMezzanines];
        fROMezzaninesDeltaSigmaT0 = new Double_t[fNROMezzanines];
        fROMezzaninesFitStatus = new Int_t[fNROMezzanines];
        for(Int_t iROMezzanine=0;iROMezzanine<fNROMezzanines;iROMezzanine++){
          fROMezzaninesT0[iROMezzanine] = 0.;
          fROMezzaninesDeltaT0[iROMezzanine] = 0.;
          fROMezzaninesSigmaT0[iROMezzanine] = 0.;
          fROMezzaninesDeltaSigmaT0[iROMezzanine] = 0.;
          fROMezzaninesFitStatus[iROMezzanine] = 0;
        }
      }
      delete l;
      continue;
    }
    else if (Line.BeginsWith("NROChannels")) {
      fNROChannels = TString(Line(TRegexp("[0-9]+"))).Atoi();
      if(fNROChannels) {
        fXMLT0 = new Double_t[fNROChannels];
        fXMLDeltaT0 = new Double_t[fNROChannels];
        fXMLSigmaT0 = new Double_t[fNROChannels];
        fXMLDeltaSigmaT0 = new Double_t[fNROChannels];
        fXMLFitStatus = new Int_t[fNROChannels];
        fChannelRemap = new Int_t[fNROChannels];
        for(Int_t iROChannel=0;iROChannel<fNROChannels;iROChannel++){
          fXMLT0[iROChannel] = 0.;
          fXMLDeltaT0[iROChannel] = 0.;
          fXMLSigmaT0[iROChannel] = 0.;
          fXMLDeltaSigmaT0[iROChannel] = 0.;
          fXMLFitStatus[iROChannel] = 0;
          fChannelRemap[iROChannel] = -1; //reset array
        }
      }
      continue;
    }
    else if (Line.BeginsWith("NROBoardsPerStation")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries()-1;
      fNROBoardsPerStation.clear();
      for(Int_t i = 0; i < NEntries; i++) {
        fNROBoardsPerStation.push_back(((TObjString*)(l->At(i+1)))->GetString().Atoi());
      }
      fNStations = fNROBoardsPerStation.size();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("ROMezzanineMasksPerBoard")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries()-1;
      if(NEntries!=fNROBoards) {
        cerr << user_normal() << "WARNING: " << NEntries << " ROMezzanineMasksPerBoards for " << fNROBoards << " boards!";
        if(NEntries>fNROBoards) {
          cout << user_normal() << " Ignoring last " << NEntries-fNROBoards << " occurrences in ROMezzanineMasksPerBoards" << endl;
        }
        else {
          cout << user_normal() << " Using '" << ((TObjString*)l->At(1))->GetString() << "' for last " << fNROBoards-NEntries << " boards" << endl;
          for(Int_t i=0;i<fNROBoards-NEntries;i++) {
            TObject * obj = l->At(1)->Clone();
            l->Add(obj);
          }
        }
        NEntries = fNROBoards;
      }
      if(NEntries) fROMezzanineMasksPerBoard = new UInt_t[NEntries];
      else cerr << user_normal() << "WARNING: fNROBoards = 0!" << endl;
      for(Int_t i = 0; i < NEntries; i++) {
        Line = ((TObjString*)(l->At(i+1)))->GetString();
        fROMezzanineMasksPerBoard[i] = strtol(TString(Line(TRegexp("[0-9a-fA-F]+"))),NULL,16);
      }
      delete l;
      continue;
    }
    else if(Line.BeginsWith("ChRemap_")){
      for(Int_t iCh=0;iCh<fNROChannels/16;iCh++){
        if(Line.BeginsWith(Form("ChRemap_%04d=",iCh))){
          TObjArray * l = Line.Tokenize(" ");
          for (Int_t jCh=0;jCh<16;jCh++){
            fChannelRemap[16*iCh+jCh] = ((TObjString*)(l->At(jCh+1)))->GetString().Atoi();
            if(fChannelRemap[16*iCh+jCh]>MaxChannelID) MaxChannelID = fChannelRemap[16*iCh+jCh];
          }
          delete l;
        }
      }
    }
  }
  NA62ConditionsService::GetInstance()->Close(fRawDecoderSettingsFileName);

  if (fNStations<=0) {
    cout << user_normal() << "ERROR: invalid number of stations specified [="<<fNStations<<"]"<<endl;
    exit(kWrongConfiguration);
  }
  if (fNROBoards<=0) {
    cout << user_normal() << "ERROR: invalid number of ROBoards specified [="<<fNROBoards<<"]"<<endl;
    exit(kWrongConfiguration);
  }
  if (fNROMezzanines<=0) {
    cout << user_normal() << "ERROR: invalid number of ROMezzanines specified [="<<fNROMezzanines<<"]"<<endl;
    exit(kWrongConfiguration);
  }
  if (fNROChannels<=0) {
    cout << user_normal() << "ERROR: invalid number of ROChannels specified [="<<fNROChannels<<"]"<<endl;
    exit(kWrongConfiguration);
  }
  if(fNStations) {
    fStationsT0 = new Double_t[fNStations];
    fStationsDeltaT0 = new Double_t[fNStations];
    fStationsSigmaT0 = new Double_t[fNStations];
    fStationsDeltaSigmaT0 = new Double_t[fNStations];
    fStationsFitStatus = new Int_t[fNStations];
    for(Int_t iStation=0;iStation<fNStations;iStation++){
      fStationsT0[iStation] = 0.;
      fStationsDeltaT0[iStation] = 0.;
      fStationsSigmaT0[iStation] = 0.;
      fStationsDeltaSigmaT0[iStation] = 0.;
      fStationsFitStatus[iStation] = 0;
    }
  }

  //initialising fChannelRO
  if(MaxChannelID>=0) {
    fChannelRO = new Int_t[MaxChannelID+1];
    for(Int_t iCh=0;iCh<MaxChannelID+1;iCh++) fChannelRO[iCh] = -1; //reset array
  }
  for(Int_t iROCh=0;iROCh<fNROChannels;iROCh++) {
    if(0<=fChannelRemap[iROCh] && fChannelRemap[iROCh]<MaxChannelID+1) fChannelRO[fChannelRemap[iROCh]]=iROCh;
  }
}

void CoarseT0Evaluation::EndOfJobUser() {
  if (!fHDigiTimeRawFine) return; // Subdetector not enabled
  // The numbers of bursts and events processed
  const Int_t NMinEventsProcessed = 10000;
  Int_t NBurstsProcessed = 0, NEventsProcessed = 0;
  if (fHNEventsProcessedPerBurst) {
    for (Int_t i=1; i<=fHNEventsProcessedPerBurst->GetNbinsX(); i++) {
      Int_t Nevents = fHNEventsProcessedPerBurst->GetBinContent(i);
      if (Nevents>0) {
        NBurstsProcessed++;
        NEventsProcessed += Nevents;
      }
    }
  }
  else cout << user_normal() << "ERROR: Histogram 'EventsPerBurst' not found!" << endl;
  if(NEventsProcessed>=NMinEventsProcessed){
    FindHistoTimeRange();
    EvaluateT0Offsets("Stations");
    EvaluateT0Offsets("ROMezzanines");
    EvaluateT0Offsets("XML");
    GeneratePDFReport(NBurstsProcessed,NEventsProcessed);
    SaveAllPlots();
  }
  else cout << user_normal() << "ERROR: Insufficient number of processed events! [" << NEventsProcessed << " instead of >= " << NMinEventsProcessed << "]" << endl;
}

void CoarseT0Evaluation::FindHistoTimeRange(){

  if (fHDigiTimeRawFine) {
    TH1F * hDigiTimeRawFine1D = (TH1F*)fHDigiTimeRawFine->ProjectionY();
    Int_t iBeginOfROWin = 1;
    Int_t iEndOfROWin = hDigiTimeRawFine1D->GetNbinsX();
    if(hDigiTimeRawFine1D->Integral()){//non empty histo
      for (Int_t iBin = hDigiTimeRawFine1D->GetMaximumBin(); iBin > 0; iBin--) {
        if (hDigiTimeRawFine1D->GetBinContent(iBin) == 0){
          iBeginOfROWin = iBin;
          break;
        }
      }
      for (Int_t iBin = hDigiTimeRawFine1D->GetMaximumBin(); iBin < hDigiTimeRawFine1D->GetNbinsX(); iBin++) {
        if (hDigiTimeRawFine1D->GetBinContent(iBin) == 0){
          iEndOfROWin = iBin;
          break;
        }
      }
    }
    fHistoTimeLowerLimit = hDigiTimeRawFine1D->GetBinCenter(iBeginOfROWin)-50.;
    fHistoTimeUpperLimit = hDigiTimeRawFine1D->GetBinCenter(iEndOfROWin)+50.;
  }
}

void CoarseT0Evaluation::EvaluateT0Offsets(TString Option) {

  /// Option == "Stations": Evaluate Stations T0 by extracting the mean of the digit time distribution
  ///                       T0 values are stored for each run in SubDetector-StationsT0.dat
  ///                       The new Stations T0 are written in SubDetector-StationsT0.new (old method)
  ///                       The new Stations T0 are written in SubDetector-CoarseT0.dat
  /// Option == "ROMezzanines": Evaluate ROMezzanines T0 by extracting the mean of the digit time distribution
  ///                       T0 values are stored for each run in SubDetector-ROMezzanines.dat
  ///                       The new ROMezzanines T0 are written in SubDetector-ROMezzanines.new (old method)
  ///                       The new ROMezzanines T0 are written in SubDetector-CoarseT0.dat
  /// Option == "XML":      Evaluate XML T0 by extracting the mean of the digit time distribution
  ///                       The new XML T0 are written in SubDetector-XMLT0.dat

  if(Option=="Stations" && !fNStations){
    cerr << user_normal() << "WARNING: NStations = 0! EvaluateT0Offsets is terminating" << endl;
    return;
  }
  if(Option=="XML" && !fNROChannels){
    cerr << user_normal() << "WARNING: NROChannels = 0! EvaluateT0Offsets is terminating" << endl;
    return;
  }
  if(!fNROMezzanines){
    cerr << user_normal() << "WARNING: NROMezzanines = 0! EvaluateT0Offsets is terminating" << endl;
    return;
  }

  TH2F* InputHisto2D = fHDigiTimeRawFine;
  if(Option=="XML") InputHisto2D = fHDigiTimeRawFineVsROChannel;

  // Compute the T0Offsets
  std::vector<Double_t> NewT0Offsets;
  std::vector<Double_t> DeltaNewT0Offsets;
  std::vector<Double_t> SigmaNewT0Offsets;
  std::vector<Double_t> DeltaSigmaNewT0Offsets;
  std::vector<Int_t   > FitStatusNewT0Offsets;
  Int_t NOffsets = fNROMezzanines;
  if(Option=="Stations") NOffsets = fNStations;
  if(Option=="XML")      {
    NOffsets = InputHisto2D->GetNbinsX();
    if(InputHisto2D->GetNbinsX()!=fNROChannels){
      cerr << user_normal() << "WARNING: Nbins in " << InputHisto2D->GetName() << "!= NROChannels [=" << fNROChannels << "]" << endl;
    }
  }
  NewT0Offsets.resize(NOffsets);
  DeltaNewT0Offsets.resize(NOffsets);
  SigmaNewT0Offsets.resize(NOffsets);
  DeltaSigmaNewT0Offsets.resize(NOffsets);
  FitStatusNewT0Offsets.resize(NOffsets);
  Int_t iCounter = 0;
  Int_t iStation = 0;
  for(Int_t iOffset=0; iOffset<NOffsets; iOffset++) {
    if(InputHisto2D){
      NewT0Offsets[iOffset]           = 0;
      DeltaNewT0Offsets[iOffset]      = 0;
      SigmaNewT0Offsets[iOffset]      = 0;
      DeltaSigmaNewT0Offsets[iOffset] = 0;
      FitStatusNewT0Offsets[iOffset]  = 0;
      Int_t iFirstBin = iOffset*InputHisto2D->GetNbinsX()/NOffsets+1;
      Int_t iLastBin  = (iOffset+1)*InputHisto2D->GetNbinsX()/NOffsets;
      if(Option=="Stations"){
        iFirstBin = iCounter*InputHisto2D->GetNbinsX()/fNROBoards+1;
        iCounter += fNROBoardsPerStation[iOffset];
        iLastBin  = iCounter*InputHisto2D->GetNbinsX()/fNROBoards;
      }
      TH1F * hDigiTimeRawFine1D = (TH1F*)InputHisto2D->ProjectionY(Form("%s%s%s%03d",fDetectorName.Data(),InputHisto2D->GetName(),Option.Data(),iOffset),iFirstBin,iLastBin);
      hDigiTimeRawFine1D->SetTitle(hDigiTimeRawFine1D->GetName());
      if(Option!="XML" && !hDigiTimeRawFine1D->GetEntries()){
        cerr << user_normal() << "WARNING: " << InputHisto2D->GetName() << " used for the evaluation of T0Offsets for " << Option << " " << iOffset <<" is empty!" << endl;
      }

      if(Option=="ROMezzanines")  fHDigiTimeRawFinePerMezzanine.push_back(hDigiTimeRawFine1D);
      else if(Option=="Stations") fHDigiTimeRawFinePerStation.push_back(hDigiTimeRawFine1D);
      //XML T0s plots saved afterwards (for pathologic channels only)

      if(hDigiTimeRawFine1D->Integral()){ //non empty histo

        if(fDetectorName == "Spectrometer")     hDigiTimeRawFine1D->Rebin(2);
        else if(fDetectorName == "GigaTracker") hDigiTimeRawFine1D->Rebin(4);

        Int_t NBins = hDigiTimeRawFine1D->GetNbinsX();
        Int_t iBeginOfROWin = 1;
        Int_t iEndOfROWin = NBins;
        for (Int_t iBin = hDigiTimeRawFine1D->GetMaximumBin(); iBin > 0; iBin--) {
          if (hDigiTimeRawFine1D->GetBinContent(iBin) == 0){
            iBeginOfROWin = iBin;
            break;
          }
        }
        for (Int_t iBin = hDigiTimeRawFine1D->GetMaximumBin(); iBin < hDigiTimeRawFine1D->GetNbinsX(); iBin++) {
          if (hDigiTimeRawFine1D->GetBinContent(iBin) == 0){
            iEndOfROWin = iBin;
            break;
          }
        }
        TF1 * fAccidentals = 0;
        if(fDetectorName == "GigaTracker")
          fAccidentals = new TF1("fAccidentals","pol0",hDigiTimeRawFine1D->GetBinCenter(hDigiTimeRawFine1D->GetMaximumBin()) + 3, hDigiTimeRawFine1D->GetBinCenter(hDigiTimeRawFine1D->GetMaximumBin()) + 4);
        else
          fAccidentals = new TF1("fAccidentals","pol0",hDigiTimeRawFine1D->GetBinCenter(iEndOfROWin) - 40,hDigiTimeRawFine1D->GetBinCenter(iEndOfROWin) - 20);
        hDigiTimeRawFine1D->Fit("fAccidentals","RQ");
        Double_t Baseline = fAccidentals->GetParameter(0);
        Double_t maxcontent = hDigiTimeRawFine1D->GetBinContent(hDigiTimeRawFine1D->GetMaximumBin()) - Baseline;
        Int_t iBeginOfPeak=1;
        Double_t PeakFraction = 0.70;
        for (Int_t i=hDigiTimeRawFine1D->GetMaximumBin(); i>=0; i--) {
          if (hDigiTimeRawFine1D->GetBinContent(i) - Baseline<PeakFraction*maxcontent) {
            iBeginOfPeak = i;
            break;
          }
        }
        Int_t iEndOfPeak=hDigiTimeRawFine1D->GetNbinsX();
        for (Int_t i=hDigiTimeRawFine1D->GetMaximumBin(); i<=hDigiTimeRawFine1D->GetNbinsX()+1; i++) {
          if (hDigiTimeRawFine1D->GetBinContent(i) - Baseline<PeakFraction*maxcontent) {
            iEndOfPeak = i;
            break;
          }
        }
        if(fDetectorName == "Spectrometer" && TString(hDigiTimeRawFine1D->GetName()).Contains("Mezzanine")){
          TF1 * fStraw = new TF1("fStraw",CoarseT0Evaluation::StrawDrift,hDigiTimeRawFine1D->GetBinCenter(iBeginOfROWin) + 25, hDigiTimeRawFine1D->GetBinCenter(iEndOfROWin) - 25,8);
          fStraw->SetParameters(maxcontent,hDigiTimeRawFine1D->GetBinCenter(hDigiTimeRawFine1D->GetMaximumBin()),7,20,0.65,130,24,Baseline);
          fStraw->SetParLimits(2,5,10);
          fStraw->SetParLimits(3,15,30);
          fStraw->SetParLimits(4,0.5,0.8);
          fStraw->SetParLimits(5,120,140);
          fStraw->SetParLimits(6,15,30);
          Int_t FitStatus = hDigiTimeRawFine1D->Fit("fStraw","RQ");
          NewT0Offsets[iOffset] = fStraw->GetParameter(1); //mean
          DeltaNewT0Offsets[iOffset] = fStraw->GetParError(1); //delta mean
          SigmaNewT0Offsets[iOffset] = fStraw->GetParameter(2); //sigma
          DeltaSigmaNewT0Offsets[iOffset] = fStraw->GetParError(2); //delta sigma
          FitStatusNewT0Offsets[iOffset]  = FitStatus; //0: success; any other values: failure
          delete fStraw;
        }
        else{
          TF1 * fGauss = new TF1("fGauss","gaus",hDigiTimeRawFine1D->GetBinCenter(iBeginOfPeak),hDigiTimeRawFine1D->GetBinCenter(iEndOfPeak));
          Int_t FitStatus = hDigiTimeRawFine1D->Fit("fGauss","RQ+");
          NewT0Offsets[iOffset] = fGauss->GetParameter(1); //mean
          DeltaNewT0Offsets[iOffset] = fGauss->GetParError(1); //delta mean
          SigmaNewT0Offsets[iOffset] = fGauss->GetParameter(2); //sigma
          DeltaSigmaNewT0Offsets[iOffset] = fGauss->GetParError(2); //delta sigma
          FitStatusNewT0Offsets[iOffset]  = FitStatus; //0: success; any other values: failure
          delete fGauss;
        }
        delete fAccidentals;
        if(Option=="ROMezzanines"){ 
          Int_t iROBoard = iOffset/fNROMezzaninesPerFullBoard;
          Int_t iMezzanine = iOffset%fNROMezzaninesPerFullBoard;
          if(!(fROMezzanineMasksPerBoard[iROBoard]&(1<<iMezzanine))) continue;
          while(iStation<fNStations-1 && iROBoard>=fNROBoardsPerStation[iStation]+iCounter){
            iCounter+=fNROBoardsPerStation[iStation];
            iStation++;
          }
          NewT0Offsets[iOffset] -= fStationsT0[iStation];
          fROMezzaninesT0[iOffset] = NewT0Offsets[iOffset];
          fROMezzaninesDeltaT0[iOffset] = DeltaNewT0Offsets[iOffset];
          fROMezzaninesSigmaT0[iOffset] = SigmaNewT0Offsets[iOffset];
          fROMezzaninesDeltaSigmaT0[iOffset] = DeltaSigmaNewT0Offsets[iOffset];
          fROMezzaninesFitStatus[iOffset] = FitStatusNewT0Offsets[iOffset];
          // Store ROMezzanine-related information
          if(hDigiTimeRawFine1D->GetEntries()){
            TGraphErrors* gROMezzaninesCoarseT0 = (TGraphErrors*) fHisto.GetTGraph("ROMezzaninesCoarseT0");
            gROMezzaninesCoarseT0->Set(gROMezzaninesCoarseT0->GetN()+1);
            gROMezzaninesCoarseT0->SetPoint(gROMezzaninesCoarseT0->GetN()-1,iOffset,fROMezzaninesT0[iOffset]);
            gROMezzaninesCoarseT0->SetPointError(gROMezzaninesCoarseT0->GetN()-1,0.5,fROMezzaninesDeltaT0[iOffset]);
            TGraphErrors* gROMezzaninesCoarseT0Resolution = (TGraphErrors*) fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution");
            gROMezzaninesCoarseT0Resolution->Set(gROMezzaninesCoarseT0Resolution->GetN()+1);
            gROMezzaninesCoarseT0Resolution->SetPoint(gROMezzaninesCoarseT0Resolution->GetN()-1,iOffset,fROMezzaninesSigmaT0[iOffset]);
            gROMezzaninesCoarseT0Resolution->SetPointError(gROMezzaninesCoarseT0Resolution->GetN()-1,0.5,fROMezzaninesDeltaSigmaT0[iOffset]);
          }
        }
        else if(Option=="Stations") {
          fStationsT0[iOffset] = NewT0Offsets[iOffset];
          fStationsDeltaT0[iOffset] = DeltaNewT0Offsets[iOffset];
          fStationsSigmaT0[iOffset] = SigmaNewT0Offsets[iOffset];
          fStationsDeltaSigmaT0[iOffset] = DeltaSigmaNewT0Offsets[iOffset];
          fStationsFitStatus[iOffset] = FitStatusNewT0Offsets[iOffset];
          // Store Station-related information
          if(hDigiTimeRawFine1D->GetEntries()){
            TGraphErrors* gStationsCoarseT0 = (TGraphErrors*) fHisto.GetTGraph("StationsCoarseT0");
            gStationsCoarseT0->Set(gStationsCoarseT0->GetN()+1);
            gStationsCoarseT0->SetPoint(gStationsCoarseT0->GetN()-1,iOffset,fStationsT0[iOffset]);
            gStationsCoarseT0->SetPointError(gStationsCoarseT0->GetN()-1,0.5,fStationsDeltaT0[iOffset]);
            TGraphErrors* gStationsCoarseT0Resolution = (TGraphErrors*) fHisto.GetTGraph("StationsCoarseT0Resolution");
            gStationsCoarseT0Resolution->Set(gStationsCoarseT0Resolution->GetN()+1);
            gStationsCoarseT0Resolution->SetPoint(gStationsCoarseT0Resolution->GetN()-1,iOffset,fStationsSigmaT0[iOffset]);
            gStationsCoarseT0Resolution->SetPointError(gStationsCoarseT0Resolution->GetN()-1,0.5,fStationsDeltaSigmaT0[iOffset]);
          }
        }
        else if(Option=="XML") {
          fXMLT0[iOffset] = NewT0Offsets[iOffset];
          fXMLDeltaT0[iOffset] = DeltaNewT0Offsets[iOffset];
          fXMLSigmaT0[iOffset] = SigmaNewT0Offsets[iOffset];
          fXMLDeltaSigmaT0[iOffset] = DeltaSigmaNewT0Offsets[iOffset];
          fXMLFitStatus[iOffset] = FitStatusNewT0Offsets[iOffset];
          // Store Station-related information
          if(hDigiTimeRawFine1D->GetEntries()){
            TGraphErrors* gXMLT0 = (TGraphErrors*) fHisto.GetTGraph("XMLT0");
            gXMLT0->Set(gXMLT0->GetN()+1);
            gXMLT0->SetPoint(gXMLT0->GetN()-1,iOffset,fXMLT0[iOffset]);
            gXMLT0->SetPointError(gXMLT0->GetN()-1,0.5,fXMLDeltaT0[iOffset]);
            TGraphErrors* gXMLT0Resolution = (TGraphErrors*) fHisto.GetTGraph("XMLT0Resolution");
            gXMLT0Resolution->Set(gXMLT0Resolution->GetN()+1);
            gXMLT0Resolution->SetPoint(gXMLT0Resolution->GetN()-1,iOffset,fXMLSigmaT0[iOffset]);
            gXMLT0Resolution->SetPointError(gXMLT0Resolution->GetN()-1,0.5,fXMLDeltaSigmaT0[iOffset]);
          }
          if(hDigiTimeRawFine1D->Integral()<1000){ //less than 1000 entries
            NewT0Offsets[iOffset] = 999.9;
          }
          else if (FitStatusNewT0Offsets[iOffset]>0) { //fit failed
            NewT0Offsets[iOffset] = -999.9;
          }
          if(fDetectorName!="Spectrometer" && fDetectorName!="GigaTracker" && fDetectorName!="LKr" && fDetectorName!="MUV1" && fDetectorName!="MUV2" && fabs(NewT0Offsets[iOffset])>1.) fHDigiTimeRawFineXML.push_back(hDigiTimeRawFine1D); //save plot for pathologic channels
        }
      }
    }
  }

  if(Option=="Stations")     PrintStationsT0(NewT0Offsets);
  if(Option=="ROMezzanines") PrintROMezzaninesT0(NewT0Offsets);
  if(Option=="XML")          PrintXMLT0(NewT0Offsets);
}

Double_t CoarseT0Evaluation::StrawDrift(Double_t * x, Double_t * par){
  if(x[0]<=par[1])
    return par[0]*TMath::Gaus(x[0],par[1],par[2]) + par[7];
  else if(x[0]<=par[1]+3*par[3])
    return par[0]*(par[4]*TMath::Gaus(x[0],par[1],par[3]) + (1 - par[4]) )+ par[7];
  else
    return par[0]*(1-par[4])*(1-TMath::Erf((x[0]-(par[1] + par[5]))/par[6]))*0.5 + par[7];
}

void CoarseT0Evaluation::PrintStationsT0(vector<Double_t> NewT0Offsets){
  // Format of the SubDetector-T0Offsets file :  T0Offsets

  time_t now = time(0);

  //--- CoarseT0 single file (StationT0s + MezzanineT0s)
  TString CoarseT0FileName = "./"+fDetectorName+"-CoarseT0.dat";
  ofstream CoarseT0File (CoarseT0FileName);
  CoarseT0File << "### "+fDetectorName+" Coarse T0 file" << endl;
  CoarseT0File << "#"<<endl;
  CoarseT0File << "# "+fDetectorName+" Stations T0 corrections generated on "<<asctime(localtime(&now));

  //write the current run values
  CoarseT0File << "StationsT0=";
  for (UInt_t iOffset=0; iOffset<NewT0Offsets.size(); iOffset++) CoarseT0File << Form(" %4.3f", NewT0Offsets[iOffset]);
  CoarseT0File << endl;
  CoarseT0File.close();

}

void CoarseT0Evaluation::PrintROMezzaninesT0(vector<Double_t> NewT0Offsets){
  // Format of the SubDetector-T0Offsets file :  T0Offsets

  time_t now = time(0);

  //--- CoarseT0 single file (StationT0s + MezzanineT0s)
  TString CoarseT0FileName = "./"+fDetectorName+"-CoarseT0.dat";
  ofstream CoarseT0File (CoarseT0FileName, ios::out | ios::app );
  CoarseT0File << "#"<<endl;
  CoarseT0File << "# "+fDetectorName+" ROMezzanines T0 corrections generated on "<<asctime(localtime(&now));
  CoarseT0File << "# Format: Starting from readout ID 0, grouped by 16" << endl;

  UInt_t NGroups = NewT0Offsets.size()/16;
  if(NewT0Offsets.size()%16) NGroups++;
  for (UInt_t iGroup=0; iGroup<NGroups;iGroup++) {
    CoarseT0File << Form("MezzaninesT0_%02d=",iGroup);
    for (UInt_t iOffset=0; iOffset<16;iOffset++) {
      if (iGroup*16+iOffset<(UInt_t)(fNROMezzanines))
        CoarseT0File << Form(" %4.3f", NewT0Offsets[iGroup*16+iOffset]);
      else
        CoarseT0File << Form(" %4.3f", 0.0);
    }
    CoarseT0File << endl;
  }
  CoarseT0File.close();
}

void CoarseT0Evaluation::PrintXMLT0(vector<Double_t> NewT0Offsets){
  // Format of the SubDetector-T0Offsets file :  T0Offsets

  time_t now = time(0);
  TString T0OffsetsFileNewName = "./"+fDetectorName+"-XMLT0.dat";
  ofstream T0OffsetsFileNew (T0OffsetsFileNewName);
  T0OffsetsFileNew << "### "+fDetectorName+" XML T0 file" << endl;
  T0OffsetsFileNew << "# "+fDetectorName+" XML T0 corrections" << endl;
  T0OffsetsFileNew << "#\n# Generated on "<<asctime(localtime(&now));
  T0OffsetsFileNew << "#"<<endl;

  Bool_t LAVFEE=false;
  if(fDetectorName=="LAV")         LAVFEE = true;
  else if(fDetectorName=="IRC")    LAVFEE = true;
  else if(fDetectorName=="SAC")    LAVFEE = true;
  else if(fDetectorName=="CHOD")   LAVFEE = true;
  else if(fDetectorName=="CHANTI") LAVFEE = true;
  else if(fDetectorName=="HAC")    LAVFEE = true;
  else if(fDetectorName=="MUV0")   LAVFEE = true;

  //write the current run values
  for (UInt_t iOffset=0; iOffset<NewT0Offsets.size(); iOffset++) {
    Int_t T0Offset = iOffset;
    if(LAVFEE) T0Offset = 2*(iOffset/2); //Force to use Low threshold also for High Threshold
    T0OffsetsFileNew << Form("%5d %4.3f", iOffset,NewT0Offsets[T0Offset]) << endl;
  }
  T0OffsetsFileNew.close();

  if(LAVFEE){ //check alignment of High and Low thresholds (HighThrT0>=LowThrT0!)
    TString HighLowDeltaT0FileName = "./"+fDetectorName+"-HighLowDeltaT0.dat";
    ofstream HighLowDeltaT0File (HighLowDeltaT0FileName);
    HighLowDeltaT0File << "### "+fDetectorName+" HighLowDeltaT0 file" << endl;
    HighLowDeltaT0File << "# "+fDetectorName+" High-Low threshold T0s monitor" << endl;
    HighLowDeltaT0File << "#\n# Generated on "<<asctime(localtime(&now));
    HighLowDeltaT0File << "#"<<endl;
    for (UInt_t iLowThr=0; iLowThr<NewT0Offsets.size()/2; iLowThr++) {
      HighLowDeltaT0File << Form("%5d %5d %4.3f", 2*iLowThr+1,2*iLowThr,NewT0Offsets[2*iLowThr+1]-NewT0Offsets[2*iLowThr]) << endl;
    }
    HighLowDeltaT0File.close();
  }
}

/////////////////////
// Build a PDF report

void CoarseT0Evaluation::GeneratePDFReport(Int_t NBurstsProcessed, Int_t NEventsProcessed) {

  if (!fGenerateOutputPDF || !fOutPDFFileName.Length()) {
    cout << user_normal() << "Report generation is not required" << endl;
    return;
  }

  cout << user_normal() << "Generating report "<< fOutPDFFileName << endl;

  gStyle->SetOptStat("ei"); // print the number of entries and integral within drawing limits
  gStyle->SetOptFit();
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  TCanvas* FrontCanvas = new TCanvas("FrontCanvas");
  FrontCanvas->Divide(2,2);
  for (int i=1; i<=4; i++) {
    FrontCanvas->GetPad(i)->SetLeftMargin(0.12);
    FrontCanvas->GetPad(i)->SetRightMargin(0.01);
    FrontCanvas->GetPad(i)->SetTopMargin(0.01);
    FrontCanvas->GetPad(i)->SetBottomMargin(0.10);
  }

  TCanvas* XMLFrontCanvas = new TCanvas("XMLFrontCanvas");
  XMLFrontCanvas->Divide(1,2);
  for (int i=1; i<=2; i++) {
    XMLFrontCanvas->GetPad(i)->SetLeftMargin(0.07);
    XMLFrontCanvas->GetPad(i)->SetRightMargin(0.01);
    XMLFrontCanvas->GetPad(i)->SetTopMargin(0.01);
    XMLFrontCanvas->GetPad(i)->SetBottomMargin(0.06);
  }

  TCanvas* StationCanvas = new TCanvas("StationCanvas");
  StationCanvas->Divide(2,2);
  for (int i=1; i<=4; i++) {
    StationCanvas->GetPad(i)->SetLeftMargin(0.07);
    StationCanvas->GetPad(i)->SetRightMargin(0.01);
    StationCanvas->GetPad(i)->SetTopMargin(0.01);
    StationCanvas->GetPad(i)->SetBottomMargin(0.06);
  }

  TCanvas* MezzanineCanvas = new TCanvas("MezzanineCanvas");
  MezzanineCanvas->Divide(4,4);
  for (int i=1; i<=16; i++) {
    MezzanineCanvas->GetPad(i)->SetLeftMargin(0.07);
    MezzanineCanvas->GetPad(i)->SetRightMargin(0.01);
    MezzanineCanvas->GetPad(i)->SetTopMargin(0.01);
    MezzanineCanvas->GetPad(i)->SetBottomMargin(0.06);
  }

  TCanvas* XMLCanvas = new TCanvas("XMLCanvas");
  XMLCanvas->Divide(4,4);
  for (int i=1; i<=16; i++) {
    XMLCanvas->GetPad(i)->SetLeftMargin(0.07);
    XMLCanvas->GetPad(i)->SetRightMargin(0.01);
    XMLCanvas->GetPad(i)->SetTopMargin(0.01);
    XMLCanvas->GetPad(i)->SetBottomMargin(0.06);
  }

  TText* aText = new TText();
  //TLine* l = new TLine();

  ///////////////////////////
  // Summary plots (one page)

  FrontCanvas->cd(1);
  fHisto.GetTGraph("StationsCoarseT0")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("StationsCoarseT0")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("StationsCoarseT0")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("StationsCoarseT0")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("StationsCoarseT0")->GetYaxis()->SetTitleOffset(1.2);
  fHisto.GetTGraph("StationsCoarseT0")->SetLineColor(kBlue);
  fHisto.GetTGraph("StationsCoarseT0")->SetMarkerColor(kBlue);
  fHisto.GetTGraph("StationsCoarseT0")->Draw();

  if (fHNEventsProcessedPerBurst) {
    aText->SetTextSize(0.07);
    aText->SetTextColor(kBlack);
    aText->SetTextAlign(kHAlignLeft+kVAlignTop);
    aText->DrawTextNDC(0.2, 0.90, Form("Bursts processed: %d", NBurstsProcessed));
    aText->DrawTextNDC(0.2, 0.80, Form("Events processed: %d", NEventsProcessed));
  }

  FrontCanvas->cd(2);
  fHisto.GetTGraph("ROMezzaninesCoarseT0")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("ROMezzaninesCoarseT0")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("ROMezzaninesCoarseT0")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("ROMezzaninesCoarseT0")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("ROMezzaninesCoarseT0")->GetYaxis()->SetTitleOffset(1.2);
  fHisto.GetTGraph("ROMezzaninesCoarseT0")->SetLineColor(kBlue);
  fHisto.GetTGraph("ROMezzaninesCoarseT0")->SetMarkerColor(kBlue);
  fHisto.GetTGraph("ROMezzaninesCoarseT0")->Draw();

  FrontCanvas->cd(3);
  fHisto.GetTGraph("StationsCoarseT0Resolution")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("StationsCoarseT0Resolution")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("StationsCoarseT0Resolution")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("StationsCoarseT0Resolution")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("StationsCoarseT0Resolution")->GetYaxis()->SetTitleOffset(1.2);
  fHisto.GetTGraph("StationsCoarseT0Resolution")->SetLineColor(kBlue);
  fHisto.GetTGraph("StationsCoarseT0Resolution")->SetMarkerColor(kBlue);
  fHisto.GetTGraph("StationsCoarseT0Resolution")->Draw();

  FrontCanvas->cd(4);
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->GetYaxis()->SetTitleOffset(1.2);
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->SetLineColor(kBlue);
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->SetMarkerColor(kBlue);
  fHisto.GetTGraph("ROMezzaninesCoarseT0Resolution")->Draw();

  FrontCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the front canvas

  ///////////////////////////
  // XML T0s plots (one page)

  XMLFrontCanvas->cd(1);
  fHisto.GetTGraph("XMLT0")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("XMLT0")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("XMLT0")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("XMLT0")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("XMLT0")->GetYaxis()->SetTitleOffset(1.2);
  fHisto.GetTGraph("XMLT0")->SetLineColor(kBlue);
  fHisto.GetTGraph("XMLT0")->SetMarkerColor(kBlue);
  fHisto.GetTGraph("XMLT0")->Draw();

  XMLFrontCanvas->cd(2);
  fHisto.GetTGraph("XMLT0Resolution")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("XMLT0Resolution")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetTGraph("XMLT0Resolution")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("XMLT0Resolution")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetTGraph("XMLT0Resolution")->GetYaxis()->SetTitleOffset(1.2);
  fHisto.GetTGraph("XMLT0Resolution")->SetLineColor(kBlue);
  fHisto.GetTGraph("XMLT0Resolution")->SetMarkerColor(kBlue);
  fHisto.GetTGraph("XMLT0Resolution")->Draw();

  XMLFrontCanvas->Print(fOutPDFFileName, "pdf");

  ///////////////////////////////////////////
  // Time distribution plots (multiple pages)

  aText->SetTextAlign(kHAlignCenter+kVAlignCenter);
  aText->SetTextSize(0.15);
  aText->SetTextColor(kGreen+2);

  gStyle->SetOptStat("e"); // print the number of entries

  // Stations T0 plots
  int NPages = fNStations/4;
  if (fNStations%4) NPages++;
  for (int iPage=0; iPage<NPages; iPage++) {
    for (int iPlot=0; iPlot<4; iPlot++) {
      StationCanvas->GetPad(iPlot+1)->Clear();
      int iStation = iPage*4 + iPlot;
      if (iStation>=fNStations) continue;
      if (!fHDigiTimeRawFinePerStation.size()) continue;
      if (!fHDigiTimeRawFinePerStation[iStation]) continue;
      StationCanvas->cd(iPlot+1);
      Int_t Integral = fHDigiTimeRawFinePerStation[iStation]->Integral(); // the integral is later changed by SetRangeUser()
      Int_t maxbin   = fHDigiTimeRawFinePerStation[iStation]->GetMaximumBin();
      Double_t c0    = fHDigiTimeRawFinePerStation[iStation]->GetBinContent(maxbin);
      if (c0<1) c0 = 1;
      fHDigiTimeRawFinePerStation[iStation]->SetMaximum(1.1*c0);
      fHDigiTimeRawFinePerStation[iStation]->GetXaxis()->SetRangeUser(fHistoTimeLowerLimit, fHistoTimeUpperLimit);
      fHDigiTimeRawFinePerStation[iStation]->GetXaxis()->SetLabelSize(0.07);
      fHDigiTimeRawFinePerStation[iStation]->GetYaxis()->SetLabelSize(0.055);
      fHDigiTimeRawFinePerStation[iStation]->GetXaxis()->SetTitle("");
      fHDigiTimeRawFinePerStation[iStation]->SetLineWidth(1);
      fHDigiTimeRawFinePerStation[iStation]->Draw();

      // Warning cases
      if (Integral==0) aText->DrawTextNDC(0.5, 0.8, "EMPTY");
      else if (Integral<1000) { //less than 1000 entries
        aText->SetTextColor(kRed);
        aText->DrawTextNDC(0.5, 0.8, "FEW ENTRIES");
        aText->SetTextColor(kGreen+2);
      }
      else if (fStationsFitStatus[iStation]>0) {
        aText->SetTextColor(kRed);
        aText->DrawTextNDC(0.5, 0.8, "FIT FAILED");
        aText->SetTextColor(kGreen+2);
      }
    }
    StationCanvas->Print(fOutPDFFileName, "pdf");
  }

  // ROMezzanineT0s plots
  NPages = fNROMezzanines/16;
  if (fNROMezzanines%16) NPages++;
  for (int iPage=0; iPage<NPages; iPage++) {
    for (int iPlot=0; iPlot<16; iPlot++) {
      MezzanineCanvas->GetPad(iPlot+1)->Clear();
      int iMezzanine = iPage*16 + iPlot;
      if (iMezzanine>=fNROMezzanines) continue;
      if (!fHDigiTimeRawFinePerMezzanine.size()) continue;
      if (!fHDigiTimeRawFinePerMezzanine[iMezzanine]) continue;
      MezzanineCanvas->cd(iPlot+1);
      Int_t Integral = fHDigiTimeRawFinePerMezzanine[iMezzanine]->Integral(); // the integral is later changed by SetRangeUser()
      Int_t maxbin   = fHDigiTimeRawFinePerMezzanine[iMezzanine]->GetMaximumBin();
      Double_t c0    = fHDigiTimeRawFinePerMezzanine[iMezzanine]->GetBinContent(maxbin);
      if (c0<1) c0 = 1;
      fHDigiTimeRawFinePerMezzanine[iMezzanine]->SetMaximum(1.1*c0);
      fHDigiTimeRawFinePerMezzanine[iMezzanine]->GetXaxis()->SetRangeUser(fHistoTimeLowerLimit, fHistoTimeUpperLimit);
      fHDigiTimeRawFinePerMezzanine[iMezzanine]->GetXaxis()->SetLabelSize(0.07);
      fHDigiTimeRawFinePerMezzanine[iMezzanine]->GetYaxis()->SetLabelSize(0.055);
      fHDigiTimeRawFinePerMezzanine[iMezzanine]->GetXaxis()->SetTitle("");
      fHDigiTimeRawFinePerMezzanine[iMezzanine]->SetLineWidth(1);
      fHDigiTimeRawFinePerMezzanine[iMezzanine]->Draw();

      // Warning cases
      if (Integral==0) aText->DrawTextNDC(0.5, 0.8, "EMPTY");
      else if (Integral<1000) { //less than 1000 entries
        aText->SetTextColor(kRed);
        aText->DrawTextNDC(0.5, 0.8, "FEW ENTRIES");
        aText->SetTextColor(kGreen+2);
      }
      else if (fROMezzaninesFitStatus[iMezzanine]>0) {
        aText->SetTextColor(kRed);
        aText->DrawTextNDC(0.5, 0.8, "FIT FAILED");
        aText->SetTextColor(kGreen+2);
      }
    }
    MezzanineCanvas->Print(fOutPDFFileName, "pdf");
  }

  // XMLs T0 plots
  NPages = fHDigiTimeRawFineXML.size()/16;
  if (fHDigiTimeRawFineXML.size()%16) NPages++;
  for (int iPage=0; iPage<NPages; iPage++) {
    for (int iPlot=0; iPlot<16; iPlot++) {
      XMLCanvas->GetPad(iPlot+1)->Clear();
      UInt_t iHisto = iPage*16 + iPlot;
      if (!fHDigiTimeRawFineXML.size()) continue;
      if (iHisto>=fHDigiTimeRawFineXML.size()) continue;
      if (!fHDigiTimeRawFineXML[iHisto]) continue;
      XMLCanvas->cd(iPlot+1);
      TString HistoName = fHDigiTimeRawFineXML[iHisto]->GetName();
      UInt_t ROChannelID = TString(HistoName(TRegexp("[0-9]+"))).Atoi();
      Int_t Integral = fHDigiTimeRawFineXML[iHisto]->Integral(); // the integral is later changed by SetRangeUser()
      Int_t maxbin   = fHDigiTimeRawFineXML[iHisto]->GetMaximumBin();
      Double_t c0    = fHDigiTimeRawFineXML[iHisto]->GetBinContent(maxbin);
      if (c0<1) c0 = 1;
      fHDigiTimeRawFineXML[iHisto]->SetMaximum(1.1*c0);
      //fHDigiTimeRawFineXML[iHisto]->GetXaxis()->SetRangeUser(fHistoTimeLowerLimit, fHistoTimeUpperLimit);
      fHDigiTimeRawFineXML[iHisto]->GetXaxis()->SetRangeUser(-25.,25.);
      fHDigiTimeRawFineXML[iHisto]->GetXaxis()->SetLabelSize(0.07);
      fHDigiTimeRawFineXML[iHisto]->GetYaxis()->SetLabelSize(0.055);
      fHDigiTimeRawFineXML[iHisto]->GetXaxis()->SetTitle("");
      fHDigiTimeRawFineXML[iHisto]->SetLineWidth(1);
      fHDigiTimeRawFineXML[iHisto]->Draw();

      // Warning cases
      if (Integral==0) aText->DrawTextNDC(0.5, 0.8, "EMPTY");
      else if (Integral<1000) { //less than 1000 entries
        aText->SetTextColor(kRed);
        aText->DrawTextNDC(0.5, 0.8, "FEW ENTRIES");
        aText->SetTextColor(kGreen+2);
      }
      else if (fXMLFitStatus[ROChannelID]>0) {
        aText->SetTextColor(kRed);
        aText->DrawTextNDC(0.5, 0.8, "FIT FAILED");
        aText->SetTextColor(kGreen+2);
      }
    }
    XMLCanvas->Print(fOutPDFFileName, "pdf");
  }
  XMLCanvas->Print(Form(fOutPDFFileName + "]"), "pdf"); // close file

  delete FrontCanvas;
  delete StationCanvas;
  delete MezzanineCanvas;
  gErrorIgnoreLevel = -1; // restore the default
}
