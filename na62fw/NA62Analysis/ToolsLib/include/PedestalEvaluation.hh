// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#ifndef PEDESTALEVALUATION_HH
#define PEDESTALEVALUATION_HH

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

class PedestalEvaluation : public NA62Analysis::Analyzer {

  public:
    PedestalEvaluation(NA62Analysis::Core::BaseAnalysis *ba, std::string DetectorName);

    // Standard methods
    void InitOutput() {}
    void DefineMCSimple() {}
    void InitHist();
    void StartOfRunUser();
    void StartOfBurstUser() {}
    void Process(int) {}
    void EndOfBurstUser(){};
    void EndOfRunUser() {}
    void EndOfJobUser();
    void PostProcess() {}
    void DrawPlot() {}

    // PedestalEvaluation-specific methods
    void ParseRawDecoderSettingsFile();
    void EvaluatePedestals(TH2F*, Int_t, Bool_t);
    void EvaluateChannelPedestal(Int_t, Bool_t);
    virtual Bool_t FitChannel(Int_t, Double_t, Double_t, Double_t, Double_t);
    void GeneratePedestalTextFile();
    void GeneratePDFReport();
    virtual void RequestUserHistograms() {}
    virtual void GenerateUserPDFReport() {}
    virtual void Clear();

  protected:

    Bool_t   fEvaluatePedestals;
    Int_t    fNChannels, fNChannelsActive, *fActiveChannelMap;
    Int_t    *fBurstCounter;
    Double_t fBinWidth;
    Bool_t   *fIsActive;
    TH2F     *fH2, *fH2_Integrated;
    TH1F     *fHNEventsProcessedPerBurst;
    TH1D     **fHPedestal;
    TF1      **fFChannelFit;
    Int_t    *fChannelID, *fStatus;
    Double_t *fPedestal, *fDeltaPedestal, *fPedestalSigma, *fDeltaPedestalSigma;
    Double_t *fSecondPeakPos;
    Bool_t   *fWarning;
    Bool_t   fUseChannelMap, fIssueWarnings;
    Bool_t   fGenerateOutputPDF; ///< Generate a PDF report? Controllable from command line

    TString  fDetectorName;          ///< Name of the detector
    TString  fDirName;               ///< Name of directory in the input file
    TString  fTH2Name;               ///< Name of the input histogram for Pedestal evaluation (controllable from command line)
    TString  fRawDecoderSettingsFileName; ///< RawDecoderSettings file name (optional, if one wants channel map in the printout)
    TString  fOutTextFileName;       ///< Name of the output file with the computed Pedestal constants
    TString  fOutPDFFileName;        ///< Name of the output PDF report file (optional, if report is required)
    TString  fIODirName;             ///< Name of input & output directory (input conf files, output dat and pdf files)
    Int_t    fNFilesToAccumulate;    ///< Unit of time for stability checks
    Double_t fMinIntegral;           ///< Minimal number of entries (excl. underflows, overflows) to attempt fit
    Double_t fMinContentMaxBin;      ///< Minimal content of most populated bin to attempt fit
    Double_t fFittingRange;          ///< Half-width of the fitting Pedestal range
    Double_t fEqualisationValue;     ///< Pedestal value used for the equalisation (LKr = 400., MUV1/2 = 1000.)
    Double_t fHistoPedestalLimit;    ///< Half-size of the X axis span for the PDF report [ADC counts]
    Double_t fSignalPeakWidth;       ///< Exclusion region half-width when looking for anomalous shape
    Double_t fInitialPedestalSigma;  ///< Initial value of the time resolution parameter for the fit
    Double_t fMaxPedestalSigma;      ///< Maximum pedestal resolution to consider the fit successful
    Double_t fMaxDeltaPedestal;      ///< Maximum statistical error on pedestal to consider the fit successful
    Double_t fMaxDeltaPedestalSigma; ///< Maximum statistical error on pedestal resolution to consider the fit successful
    Int_t    fPage1MinChannelID;     ///< Minimum channel ID in page 1 plots (useful for detectors sharing a TEL62)
    Int_t    fPage1MaxChannelID;     ///< Maximum channel ID in page 1 plots (useful for detectors sharing a TEL62)

    TCanvas *fCanvas, *fFrontCanvas;
    TText   *fText;
};

#endif
