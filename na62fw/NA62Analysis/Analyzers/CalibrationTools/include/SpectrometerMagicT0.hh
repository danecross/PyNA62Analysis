// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-08-09
//
// ---------------------------------------------------------

#ifndef SPECTROMETERMAGICT0_HH
#define SPECTROMETERMAGICT0_HH

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

class SpectrometerMagicT0 : public NA62Analysis::Analyzer {

  public:
    explicit SpectrometerMagicT0(NA62Analysis::Core::BaseAnalysis *ba);
    ~SpectrometerMagicT0();

    // Standard methods
    void InitOutput() {}
    void DefineMCSimple() {}
    void InitHist();
    void StartOfRunUser() {}
    void StartOfBurstUser() {}
    void Process(int) {}
    void EndOfBurstUser(){};
    void EndOfRunUser() {}
    void EndOfJobUser();
    void PostProcess() {}
    void DrawPlot() {}

    // SpectrometerMagicT0-specific methods
    void EvaluateRecoWires(TH2F*, Int_t);
    void EvaluateRecoWire(Int_t);
    virtual Bool_t FitMagicT0(Int_t, Double_t, Double_t, Double_t, Double_t);
    void GenerateMagicT0TextFile();
    void GeneratePDFReport();
    virtual void RequestUserHistograms() {}
    virtual void GenerateUserPDFReport() {}
    virtual void Clear();

  protected:

    Int_t    fNHypotheses;
    Int_t    *fBurstCounter;
    Double_t fBinWidth;
    TH2F     *fH2, *fH2_Integrated;
    TH1F     *fHNEventsProcessedPerBurst;
    TH1D     **fHRecoWireSum;
    TF1      **fFChannelFit;
    Double_t *fMagicT0, *fRecoWire, *fDeltaRecoWire, *fRecoWireSigma, *fDeltaRecoWireSigma;
    Bool_t   fGenerateOutputPDF;     ///< Generate a PDF report? Controllable from command line

    TString  fTH2Name;               ///< Name of the input histogram for MagicT0 evaluation (controllable from command line)
    TString  fOutTextFileName;       ///< Name of the output file with the computed MagicT0 constant
    TString  fOutPDFFileName;        ///< Name of the output PDF report file (optional, if report is required)
    TString  fIODirName;             ///< Name of input & output directory (input conf files, output dat and pdf files)
    Int_t    fNFilesToAccumulate;    ///< Unit of time for stability checks
    Double_t fMinIntegral;           ///< Minimal number of entries (excl. underflows, overflows) to attempt fit
    Double_t fMinContentMaxBin;      ///< Minimal content of most populated bin to attempt fit
    Double_t fInitialRecoWireSigma;  ///< Initial RecoWireSigma
    Double_t fFittingRange;          ///< Half-width of the fitting RecoWire range
    Double_t fMaxDeltaRecoWire;      ///< Maximum statistical error on RecoWire to consider the fit successful
    Double_t fMaxDeltaRecoWireSigma; ///< Maximum statistical error on RecoWireSigma to consider the fit successful

    TCanvas *fCanvas, *fFrontCanvas;
    TText   *fText;
};

#endif
