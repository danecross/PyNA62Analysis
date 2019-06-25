// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-04-10
//
// ---------------------------------------------------------------

#ifndef T0EVALUATION_HH
#define T0EVALUATION_HH

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

class T0Evaluation : public NA62Analysis::Analyzer {

public:
  T0Evaluation(NA62Analysis::Core::BaseAnalysis *ba, std::string DetectorName);

  // Standard methods
  void InitOutput() {}
  void DefineMCSimple() {}
  void InitHist();
  void StartOfRunUser();
  void StartOfBurstUser() {}
  void Process(int) {}
  void EndOfBurstUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

  // T0Evaluation-specific methods
  void ParseRawDecoderSettingsFile();
  void EvaluateT0s(TH2F*, Int_t, Bool_t);
  void EvaluateChannelT0(Int_t, Bool_t);
  virtual Bool_t FitChannel(Int_t, Double_t, Double_t, Double_t, Double_t);
  void GenerateT0TextFile();
  void GeneratePDFReport();
  virtual void RequestUserHistograms() {}
  virtual void GenerateUserPDFReport() {}
  virtual void Clear();

private:
  void Publish();

protected:

  Bool_t   fEvaluateT0s;
  Int_t    fNChannels, fNChannelsActive, ActiveChannelMap[20000];
  Int_t    fBurstCounter[20000], fTimeBinsCounter[20000];
  Double_t fBinWidth;
  Bool_t   fIsActive[20000];
  TH2F     *fH2, *fH2_Partial, *fH2_Integrated;
  TH1F     *fHNEventsProcessedPerBurst, *fHRawTime, *fHTime[20000], *fHT0VsTime[20000];
  TF1      *fFChannelFit[20000], *fFChannelStability[20000];
  Int_t    fChannelID[20000]; ///< Geometric channel ID versus RO channel ID
  Double_t fT0[20000], fDeltaT0[20000], fResol[20000], fDeltaResol[20000];
  Double_t fSecondPeakPos[20000];
  Bool_t   fWarning[20000];
  Bool_t   fUseChannelMap, fPlotChannelTimes, fPlotTimeDependences, fIssueWarnings;
  Bool_t   fPrim; ///< Process standard or "Prim" histogram? Controllable from command line
  Bool_t   fCT; ///< Process standard or "CT" histogram? Controllable from command line
  Bool_t   fGenerateOutputPDF; ///< Generate a PDF report? Controllable from command line

  TString  fDetectorName;       ///< Name of the detector
  TString  fDirName;            ///< Name of directory in the input file
  TString  fTH2Name;            ///< Name of the input histogram for T0 evaluation (controllable from command line)
  TString  fRawDecoderSettingsFileName; ///< RawDecoderSettingsiguration file name (optional, if one wants channel map in the printout)
  TString  fOutTextFileName;    ///< Name of the output file with the computed T0 constants
  TString  fOutPDFFileName;     ///< Name of the output PDF report file (optional, if report is required)
  TString  fIODirName;          ///< Name of input & output directory (input conf files, output dat and pdf files)
  Int_t    fNFilesToAccumulate; ///< Unit of time for stability checks
  Double_t fMinIntegral;        ///< Minimal number of entries (excl. underflows, overflows) to attempt fit
  Double_t fMinContentMaxBin;   ///< Minimal content of most populated bin to attempt fit
  Double_t fFittingRange;       ///< Half-width of the fitting T0 range
  Double_t fHistoTimeLimit;     ///< Half-size of the X axis span for the PDF report [ns]
  Double_t fPeakBkgThreshold;   ///< Second highest peak/Bkg threshold
  Double_t fSignalPeakWidth;    ///< Exclusion region half-width when looking for anomalous shape
  Double_t fInitialResol;       ///< Initial value of the time resolution parameter for the fit
  Double_t fMaxResol;           ///< Maximum time resolution to consider the fit successful
  Double_t fMaxDeltaT0;         ///< Maximum statistical error on T0 to consider the fit successful
  Double_t fMaxDeltaResol;      ///< Maximum statistical error on time resolution to consider the fit successful
  Int_t    fPage1MinChannelID;  ///< Minimum channel ID in page 1 plots (useful for detectors sharing a TEL62)
  Int_t    fPage1MaxChannelID;  ///< Maximum channel ID in page 1 plots (useful for detectors sharing a TEL62)

  TCanvas *fCanvas, *fFrontCanvas;
  TText   *fText;
};

#endif
