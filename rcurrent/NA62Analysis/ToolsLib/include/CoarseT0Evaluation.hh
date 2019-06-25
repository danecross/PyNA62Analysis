// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#ifndef COARSET0EVALUATION_HH
#define COARSET0EVALUATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <TStyle.h>

#include "TH1F.h"
#include "TProfile.h"
#include "TF1.h"
#include "TText.h"
#include "TLine.h"
#include "TLegend.h"

class CoarseT0Evaluation : public NA62Analysis::Analyzer {

  public:
    CoarseT0Evaluation(NA62Analysis::Core::BaseAnalysis *ba, TString DetectorName);
    ~CoarseT0Evaluation();

    // Standard methods
    void InitOutput() { AddParam("generateoutputpdf", &fGenerateOutputPDF, 1); }
    void DefineMCSimple() {}
    void InitHist();
    void StartOfRunUser();
    void Process(int) {}
    void PostProcess() {}
    void EndOfRunUser() {}
    void EndOfJobUser();
    void DrawPlot() {}

    // CoarseT0Evaluation-specific methods
    void ParseRawDecoderSettingsFile();
    virtual void EvaluateT0Offsets(TString Option);
    static Double_t StrawDrift(Double_t * x, Double_t * par);
    void FindHistoTimeRange();
    void PrintStationsT0(std::vector<Double_t>);
    void PrintROMezzaninesT0(std::vector<Double_t>);
    void PrintXMLT0(std::vector<Double_t>);
    void GeneratePDFReport(Int_t,Int_t);

  private:

  protected:

    TString   fDetectorName;
    TString   fRawDecoderSettingsFileName;
    Int_t     fNStations;
    Int_t     fNROBoards;
    Int_t     fNROMezzanines;
    Int_t     fNROChannels;
    Int_t     fNROMezzaninesPerFullBoard;
    UInt_t*   fROMezzanineMasksPerBoard;
    Double_t* fStationsT0;
    Double_t* fStationsDeltaT0;
    Double_t* fStationsSigmaT0;
    Double_t* fStationsDeltaSigmaT0;
    Int_t*    fStationsFitStatus;
    Double_t* fROMezzaninesT0;
    Double_t* fROMezzaninesDeltaT0;
    Double_t* fROMezzaninesSigmaT0;
    Double_t* fROMezzaninesDeltaSigmaT0;
    Int_t*    fROMezzaninesFitStatus;
    std::vector<Int_t> fNROBoardsPerStation;
    Double_t* fXMLT0;
    Double_t* fXMLDeltaT0;
    Double_t* fXMLSigmaT0;
    Double_t* fXMLDeltaSigmaT0;
    Int_t*    fXMLFitStatus;
    TH2F*     fHDigiTimeRawFine;
    TH2F*     fHDigiTimeRawFineVsROChannel;
    TH1F*     fHNEventsProcessedPerBurst;
    std::vector<TH1F*> fHDigiTimeRawFinePerMezzanine,fHDigiTimeRawFinePerStation,fHDigiTimeRawFineXML;
    Int_t     fGenerateOutputPDF;
    TString   fOutPDFFileName;
    Double_t  fHistoTimeLowerLimit,fHistoTimeUpperLimit;
    Int_t *   fChannelRemap;
    Int_t *   fChannelRO;

};

#endif
