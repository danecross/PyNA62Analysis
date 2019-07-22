// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-04-10
//
// ---------------------------------------------------------------

#include "T0Evaluation.hh"
#include "NA62ConditionsService.hh"
#include <TRegexp.h>

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class T0Evaluation
/// \Brief
/// A generic tool for computation of the T0 constants.
/// \EndBrief
/// \Detailed
/// A generic tool for computation of the T0 constants for each subdetector.
/// Daughter classes for each subdetector are located in Analyzers/CalibrationTools.
/// The input for T0 computations is a 2-dimensional histogram of
/// (Uncorrected time wrt reference time) vs (Readout channel ID).
/// The only correction to be made to the channel time is the subtraction of the global T0.
/// The standard histogram name is "RecoHitTimeWrtReferenceVsReadoutChannelNoT0".
/// A command option "Prim" appends "Prim" to the name, allowing for the computation of the
/// T0s for primitives from the corresponding input histogram.
/// A command option "CT" appends "CT" to the name, allowing for the computation of the
/// T0s for events using the control trigger only.
/// The "Prim" and "CT" options may be used together.
/// The recommended time bin width is 0.2ns.
/// A Gaussian fit is performed for each channel in the input histogram to evaluate the T0s.
/// If it fails, a second fit with a constant+Gaussian function is attempted.
/// Basic checks of the time distribution shape are performed,
/// and warnings are issued if requested by user.
/// The output is a text file with a table of T0 constants
/// (which is NA62Reco-readable for subdetectors that conform to the standard),
/// an output root file with channel T0s and resolutions, and a detailed PDF report containing plots.
/// A number of parameters (for the daughter classes!) are controllable from command line,
/// see the example below and the source code.
/// \code
/// ./MyApp -l <list> -p "MUV3T0:IODirName=/tmp;Prim=1&RICHT0:GeneratePDF=1" ...
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

T0Evaluation::T0Evaluation(Core::BaseAnalysis *ba, std::string DetectorName) :
  Analyzer(ba),
  fBinWidth(0.2),
  fH2(nullptr), fH2_Partial(nullptr), fH2_Integrated(nullptr),
  fHNEventsProcessedPerBurst(nullptr), fHRawTime(nullptr), fHTime{nullptr},
  fHT0VsTime{nullptr}, fFChannelFit{nullptr}, fFChannelStability{nullptr},
  fGenerateOutputPDF(false), fPeakBkgThreshold(0),
  fCanvas(nullptr), fFrontCanvas(nullptr), fText(nullptr)
{
  ///////////////////////////
  // Read external parameters

  // Input & output directory (input conf files, output dat and pdf files)
  AddParam("IODirName", &fIODirName, ".");
  // Name of the input histogram: not conforming to the standard is shameful
  AddParam("InputHistoName", &fTH2Name, "RecoHitTimeWrtReferenceVsReadoutChannelNoT0");
  // Process the standard or "Prim" histogram?
  AddParam("Prim", &fPrim, false);
  // Process the standard or "CT" histogram?
  AddParam("CT", &fCT, false);
  // Generate the output PDF file?
  AddParam("GeneratePDF", &fGenerateOutputPDF, true);

  fDetectorName        = DetectorName;
  fAnalyzerName        = fDetectorName + "T0";
  fDirName             = fDetectorName + "Monitor";

  fMinIntegral         = 100;   // minimal number of entries (excluding underflows, overflows) for fit attempt
  fMinContentMaxBin    = 10.0;  // minimal content of most populated bin for fit attempt
  fFittingRange        = 0.9;   // fitting range = [-0.9ns:+0.9ns], i.e. 9 bins of 0.2ns width
  fNFilesToAccumulate  = 20;    // for the T0 stability plots
  fHistoTimeLimit      = 30.0;  // time half-span of plotted histograms [ns]
  fSignalPeakWidth     = 1.0;   // exclusion region half-width for the spectrum shape check [ns]
  fInitialResol        = 1.0;   // initial value of the time resolution parameter for the fit [ns]
  fMaxResol            = 2.0;   // max time resolution to consider the fit successful [ns]
  fMaxDeltaT0          = 0.5;   // max statistical error on T0 to consider the fit successful [ns]
  fMaxDeltaResol       = 0.5;   // max statistical error on time resolution to consider the fit successful [ns]
  fIssueWarnings       = false; // check if the spectrum shape is OK?
  fPlotChannelTimes    = true;  // plot times in each channel?
  fPlotTimeDependences = false; // check and plot the time stability of the T0 constants?
  fPage1MinChannelID   = -1;    // adjust channel range in page 1 plots (default: negative = no adjustment)
  fPage1MaxChannelID   = -1;    // adjust channel range in page 1 plots (default: negative = no adjustment)

  // Initialization of the analyzer
  fEvaluateT0s = fUseChannelMap = true;
  fNChannels = fNChannelsActive = 0;
}

void T0Evaluation::Clear() {
  if (fH2_Partial) delete fH2_Partial;
  if (fHRawTime)   delete fHRawTime;
  fH2_Partial = nullptr;
  fHRawTime = nullptr;
  for (Int_t ich=0; ich<fNChannels; ich++) {
    if (fHT0VsTime[ich])         delete fHT0VsTime[ich];
    if (fFChannelStability[ich]) delete fFChannelStability[ich];
    if (fHTime[ich])             delete fHTime[ich];
    if (fFChannelFit[ich])       delete fFChannelFit[ich];
    fHT0VsTime[ich]= nullptr;
    fFChannelStability[ich]= nullptr;
    fHTime[ich]= nullptr;
    fFChannelFit[ich]= nullptr;
  }
}

void T0Evaluation::InitHist() {

  if (fIODirName!=".") {
    cout << extended() << "Directory for input & output files set to " << fIODirName << endl;
  }
  if (!GetIsHisto() || GetIsTree()) {
    cout << user_normal() << "Error: T0Evaluation-based analyzers must be run in the --histo mode" << endl;
    exit(kWrongConfiguration);
  }

  fRawDecoderSettingsFileName = fDetectorName+"-RawDecoderSettings.dat";
  if (!fPrim && !fCT) { // T0s for offline reconstruction
    fOutTextFileName = fIODirName + "/" + fDetectorName + "-T0.dat";
    fOutPDFFileName  = fIODirName + "/" + fDetectorName + "-T0.pdf";
  }
  else if (fPrim && !fCT) { // T0s for trigger primitives
    fOutTextFileName = fIODirName + "/" + fDetectorName + "-T0-Prim.dat";
    fOutPDFFileName  = fIODirName + "/" + fDetectorName + "-T0-Prim.pdf";
  }
  else if (!fPrim && fCT) { // T0s for offline reconstruction, control triggers
    fOutTextFileName = fIODirName + "/" + fDetectorName + "-T0-CT.dat";
    fOutPDFFileName  = fIODirName + "/" + fDetectorName + "-T0-CT.pdf";
  }
  else { // T0s for trigger primitives, control triggers
    fOutTextFileName = fIODirName + "/" + fDetectorName + "-T0-Prim-CT.dat";
    fOutPDFFileName  = fIODirName + "/" + fDetectorName + "-T0-Prim-CT.pdf";
  }

  fHNEventsProcessedPerBurst = (TH1F*)RequestHistogram("/", "EventsPerBurst", true);

  if (fEvaluateT0s) {
    TString HistoName = fTH2Name;
    if (fPrim) HistoName +="Prim";
    if (fCT) HistoName +="CT";
    fH2            = (TH2F*)RequestHistogram(fDirName, HistoName, false); // reset for each input file
    fH2_Integrated = (TH2F*)RequestHistogram(fDirName, HistoName, true);  // accumulated
    if (!fH2) {
      fEvaluateT0s = false;
      cout << user_normal() << "Input histogram " << fDirName << "/" << HistoName << " not found" << endl;
    }
    else {
      cout << user_normal() << "Processing input histogram " << fDirName << "/" << HistoName << endl;
    }
  }

  RequestUserHistograms();

  if (fEvaluateT0s) {
    fH2_Partial = new TH2F(*fH2); // for T0 stability vs time
    fH2_Partial->Reset();
    fNChannels = fH2->GetNbinsX();
    fBinWidth  = fH2->GetYaxis()->GetBinWidth(1);
  }

  for (Int_t i=0; i<fNChannels; i++) {
    fHTime[i]           = nullptr;
    fBurstCounter[i]    = 0;
    fTimeBinsCounter[i] = 0;
    fChannelID[i]       = -99;
    fT0[i]              = -999.999;
    fWarning[i]         = false;
  }

  // Book monitoring histograms to be saved into the output
  if (fEvaluateT0s) {
    BookHisto (new TH1F
	       ("T0", "T0;RO channel ID;T0 and its error",
		fNChannels, -0.5, fNChannels-0.5));
    BookHisto (new TH1F
	       ("T0Resolution",	"T0Resolution;RO channel ID;T0 resolution and its error",
		fNChannels, -0.5, fNChannels-0.5));
  }
}

void T0Evaluation::StartOfRunUser() {
  // Find and read the channel map
  if (fUseChannelMap) {
    ParseRawDecoderSettingsFile();
  }
  else {
    cout << user_normal() << "RawDecoderSettings file ignored on user request" << endl;
  }

  // Check which the RO channels are active
  fNChannelsActive = 0;
  if (!fUseChannelMap) std::fill(std::begin(fChannelID), std::end(fChannelID), 0); // all channels are active if no config file
  for (Int_t ich=0; ich<fNChannels; ich++) {
    fIsActive[ich] = (fChannelID[ich]>=0);
    if (fIsActive[ich]) {
      ActiveChannelMap[fNChannelsActive++] = ich;
      TString Name = (fUseChannelMap) ?
	Form("%s RO %04d GeomID %04d vs time",fDetectorName.Data(), ich, fChannelID[ich]) :
	Form("%s RO %04d vs time",fDetectorName.Data(), ich);
      fHT0VsTime[ich] = new TH1F(Name, Name, 5000, -0.5, 4999.5);
    }
  }
}

///////////////////////////////////////////////////
// Read the channel map from the configuration file

void T0Evaluation::ParseRawDecoderSettingsFile() {
  if(NA62ConditionsService::GetInstance()->Open(fRawDecoderSettingsFileName)!=kSuccess){
    fUseChannelMap = false;
    return;
  }
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fRawDecoderSettingsFileName))) {
    if (!Line.BeginsWith("ChRemap_")) continue;
    TString chNumber = TString(Line("ChRemap_[0-9]+"))(8, 4);
    if (chNumber.IsNull()) continue;
    Int_t iCh = chNumber.Atoi();
    TObjArray *l = Line.Tokenize(" ");
    for (Int_t jCh = 0; jCh < 16; jCh++) {
      fChannelID[16*iCh+jCh] = ((TObjString*)(l->At(jCh+1)))->GetString().Atoi();
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fRawDecoderSettingsFileName);
}

////////////////////////////////////
// Build the T0 time stability plots

void T0Evaluation::EndOfBurstUser() {
  if (!fEvaluateT0s) return;
  if (!fPlotTimeDependences) return;
  fH2_Partial->Add(fH2);

  for (Int_t ich=0; ich<fNChannels; ich++) {
    fBurstCounter[ich]++;
    if (fBurstCounter[ich] < fNFilesToAccumulate) continue;
    Double_t Integral = fH2_Partial->Integral(ich+1, ich+1, 1, fH2_Partial->GetYaxis()->GetNbins());
    if (Integral<fMinIntegral) continue;
    EvaluateT0s(fH2_Partial, ich, false);
    if (fabs(fT0[ich])>999.0) continue;

    // A successful T0 fit is obtained: fill the time dependence, clean histogram
    fHT0VsTime[ich]->SetBinContent(fTimeBinsCounter[ich]+1, fT0[ich]);
    fHT0VsTime[ich]->SetBinError  (fTimeBinsCounter[ich]+1, fDeltaT0[ich]);
    for (Int_t i=1; i<=fH2_Partial->GetNbinsY(); i++) {
      fH2_Partial->SetBinContent(ich+1, i, 0);
      fH2_Partial->SetBinError  (ich+1, i, 0);
    }
    fBurstCounter[ich] = 0;
    fTimeBinsCounter[ich]++;
  }
}

void T0Evaluation::EndOfJobUser() {

  // Time stability of the T0s
  for (Int_t ich=0; ich<fNChannels; ich++) {
    if (fIsActive[ich]) {
      fFChannelStability[ich] = new TF1("pol0", "pol0", -0.5, fTimeBinsCounter[ich]-0.5);
      if (fHT0VsTime[ich]->GetEntries()) {
	fHT0VsTime[ich]->Fit(fFChannelStability[ich], "0Q", "", -0.5, fTimeBinsCounter[ich]-0.5);
      }
    }
  }

  // Evaluate the T0s and global offset with the full data sample
  if (fEvaluateT0s) {
    EvaluateT0s(fH2_Integrated, -1, fIssueWarnings); // -1: all channels
    // Generate and save the output
    if (fEvaluateT0s) {
      Int_t NSuccessfulFits = 0;
      for (Int_t i=0; i<fNChannels; i++) {
        if (fIsActive[i] && fabs(fT0[i])<999.0) NSuccessfulFits++;
      }
      if (NSuccessfulFits) {
        GenerateT0TextFile();
        GeneratePDFReport();
      }
      else {
        cout << user_normal() << "All T0 fits failed, no DAT and PDF output produced" << endl;
      }
    }
  }

  SaveAllPlots(); // Save the T0 and T0 resolution vs channel ID plots
  Clear();
}

void T0Evaluation::EvaluateT0s(TH2F *h2, Int_t ChannelID, Bool_t IssueWarnings) {
  // Evaluate T0 for all channels or a single channel?
  Int_t ch1 = 0;
  Int_t ch2 = fNChannels-1;
  if (ChannelID>=0) ch1 = ch2 = ChannelID;

  for (Int_t ich=ch1; ich<=ch2; ich++) {
    if (fIsActive[ich]) {
      TString Name = (fUseChannelMap) ?
	Form("%s RO %04d GeomID %04d", fDetectorName.Data(), ich, fChannelID[ich]) :
	Form("%s RO %04d", fDetectorName.Data(), ich);
      if (fHTime[ich]) delete fHTime[ich];
      fHTime[ich] = (TH1F*)h2->ProjectionY(Name, ich+1, ich+1);
      fHTime[ich]->SetTitle(Name);
      fHTime[ich]->SetDirectory(0); //needed to detach the histo from root file (otherwise it cannot be deleted!)
      EvaluateChannelT0(ich, IssueWarnings);
    }
  }
}

///////////////////////////
// Evaluate T0 in a channel

void T0Evaluation::EvaluateChannelT0 (Int_t ich, Bool_t IssueWarning) {

  // Check if there are enough entries for the fit to converge
  if (fHTime[ich]->Integral()==0) return;
  fT0[ich] = +999.999;
  if (fHTime[ich]->Integral()<fMinIntegral) return;

  // Remove contents outside (-fHistoTimeLimit,fHistoTimeLimit)
  Int_t LowerBoundaryBin = fHTime[ich]->FindBin(-fHistoTimeLimit);
  Int_t UpperBoundaryBin = fHTime[ich]->FindBin(+fHistoTimeLimit);
  for(Int_t iBin=1;iBin<LowerBoundaryBin;iBin++) fHTime[ich]->SetBinContent(iBin,0.);
  for(Int_t iBin=UpperBoundaryBin+1;iBin<=fHTime[ich]->GetNbinsX();iBin++) fHTime[ich]->SetBinContent(iBin,0.);

  // Fitting interval: around the bin with max content
  Int_t maxbin  = fHTime[ich]->GetMaximumBin();
  Double_t c0   = fHTime[ich]->GetBinCenter(maxbin);
  Double_t cmin = c0 - fFittingRange;
  Double_t cmax = c0 + fFittingRange;
  Double_t maxc = fHTime[ich]->GetBinContent(maxbin);
  if (maxc < fMinContentMaxBin) return;

  if (!FitChannel(ich, c0, cmin, cmax, maxc)) return;

  fHisto.GetHisto("T0")->          SetBinContent(ich+1, fT0[ich]);
  fHisto.GetHisto("T0")->          SetBinError  (ich+1, fDeltaT0[ich]);
  fHisto.GetHisto("T0Resolution")->SetBinContent(ich+1, fResol[ich]);
  fHisto.GetHisto("T0Resolution")->SetBinError  (ich+1, fDeltaResol[ich]);

  /////////////////////////////////////////////////
  // Issue a warning in case there is a second peak

  if (IssueWarning && maxc>20.0) {
    Int_t    nsidebandbins = 0;
    Double_t background    = 0.0;
    Int_t    highestbin    = 0;
    Double_t highestcont   = 0.0;
    for (Int_t ibin=1; ibin<=fHTime[ich]->GetNbinsX(); ibin++) {
      Double_t t    = fHTime[ich]->GetBinCenter(ibin);
      Double_t cont = fHTime[ich]->GetBinContent(ibin);
      if (fabs(t-fT0[ich]) < fSignalPeakWidth) continue;
      nsidebandbins++;
      background += cont;
      if (cont > highestcont) {
	highestbin  = ibin;
	highestcont = cont;
      }
    }
    if (nsidebandbins) background /= nsidebandbins;

    if (highestcont-background > 0.05*maxc && highestcont > 10.0*sqrt(background)) {
      fWarning[ich]       = true;
      fSecondPeakPos[ich] = fHTime[ich]->GetBinCenter(highestbin);
    }
  }
}

//////////////////////////////////////////////////////////////////////////
// The fitting routine in a channel, can be overloaded in daughter classes
// Parameters:
//   ich: channel number;
//   c0: centre of the most populated bin of the time distribution;
//   cmin, cmax: fitting range;
//   cmax: content of the most populated bin

Bool_t T0Evaluation::FitChannel(Int_t ich, Double_t c0, Double_t cmin, Double_t cmax, Double_t maxc) {

  fFChannelFit[ich] = new TF1("gaus", "gaus", cmin, cmax);

  // Initial parameters of the fitting function: Gaussian amplitude, mean value (=T0) and RMS (=time resolution)
  fFChannelFit[ich]->SetParameters(maxc, c0, fInitialResol);
  fHTime[ich]->Fit(fFChannelFit[ich], "R0Q");
  Double_t T0         = fFChannelFit[ich]->GetParameter(1);
  Double_t Resol      = fFChannelFit[ich]->GetParameter(2);
  Double_t DeltaT0    = fFChannelFit[ich]->GetParError(1);
  Double_t DeltaResol = fFChannelFit[ich]->GetParError(2);

  //////////////////////////////////////////////////////////////////////////////
  // If the first fit fails, make a second attempt with more degrees of freedom.
  // The fit success checks are performed on:
  //   time resolution (fMaxResol);
  //   statistical error on T0 (fMaxDeltaT0);
  //   statistical error on time resolution (fMaxDeltaResol).

  if (Resol>fMaxResol || DeltaT0>fMaxDeltaT0 || DeltaResol>fMaxDeltaResol) {

    delete fFChannelFit[ich];
    fFChannelFit[ich] = new TF1("GausPol", "gaus(0)+pol0(3)", cmin, cmax);

    // Initial parameters of the fitting function:
    // Gaussian amplitude, mean value (=T0) and RMS (=time resolution), flat background rate
    fFChannelFit[ich]->SetParameters(maxc, c0, fInitialResol, 0.0);
    fHTime[ich]->Fit(fFChannelFit[ich], "R0Q");
    T0         = fFChannelFit[ich]->GetParameter(1);
    Resol      = fFChannelFit[ich]->GetParameter(2);
    DeltaT0    = fFChannelFit[ich]->GetParError(1);
    DeltaResol = fFChannelFit[ich]->GetParError(2);

    // Check if the second fit is successful
    if (Resol>fMaxResol || DeltaT0>fMaxDeltaT0 || DeltaResol>fMaxDeltaResol) return false;
  }

  fT0[ich]         = T0;
  fDeltaT0[ich]    = DeltaT0;
  fResol[ich]      = Resol;
  fDeltaResol[ich] = DeltaResol;
  return true;
}

/////////////////////
// Build a PDF report

void T0Evaluation::GeneratePDFReport() {

  if (!fGenerateOutputPDF || !fOutPDFFileName.Length()) {
    cout << user_normal() << "PDF report generation is not required" << endl;
    return;
  }

  cout << user_normal() << "Generating PDF report "<< fOutPDFFileName << endl;

  // The numbers of bursts and events processed
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

  gStyle->SetOptStat("ei"); // print the number of entries and integral within drawing limits
  gStyle->SetOptFit();
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  fFrontCanvas = new TCanvas("FrontCanvas");
  fFrontCanvas->Divide(1,2);
  for (Int_t i=1; i<=2; i++) {
    fFrontCanvas->GetPad(i)->SetLeftMargin(0.05);
    fFrontCanvas->GetPad(i)->SetRightMargin(0.01);
    fFrontCanvas->GetPad(i)->SetTopMargin(0.01);
    fFrontCanvas->GetPad(i)->SetBottomMargin(0.10);
  }

  fCanvas = new TCanvas("Canvas");
  fCanvas->Divide(4,4);
  for (Int_t i=1; i<=16; i++) {
    fCanvas->GetPad(i)->SetLeftMargin(0.07);
    fCanvas->GetPad(i)->SetRightMargin(0.01);
    fCanvas->GetPad(i)->SetTopMargin(0.01);
    fCanvas->GetPad(i)->SetBottomMargin(0.06);
  }

  fText = new TText();
  TLine *l = new TLine();

  ///////////////////////////
  // Summary plots (one page)

  // Adjust axis range in page 1 plots (convenient for detectors sharing a TEL62 board)
  if (fPage1MinChannelID>0 || fPage1MaxChannelID>0) {
    Double_t xmin = (fPage1MinChannelID>0) ? fPage1MinChannelID-0.5 : -0.5;
    Double_t xmax = (fPage1MaxChannelID>0) ? fPage1MaxChannelID+0.5 : fNChannels-0.5;
    fHisto.GetHisto("T0")->GetXaxis()->SetRangeUser(xmin, xmax);
    fHisto.GetHisto("T0Resolution")->GetXaxis()->SetRangeUser(xmin, xmax);
  }

  fFrontCanvas->cd(1);
  fHisto.GetHisto("T0")->SetStats(0);
  fHisto.GetHisto("T0")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("T0")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("T0")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("T0")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("T0")->GetYaxis()->SetTitleOffset(0.5);
  fHisto.GetHisto("T0")->GetXaxis()->SetTitle("Readout channel ID");
  fHisto.GetHisto("T0")->GetYaxis()->SetTitle("T0 and its error [ns]");
  fHisto.GetHisto("T0")->SetLineColor(kBlue);
  fHisto.GetHisto("T0")->SetMarkerColor(kBlue);
  fHisto.GetHisto("T0")->Draw();

  if (fHNEventsProcessedPerBurst) {
    fText->SetTextSize(0.07);
    fText->SetTextColor(kBlack);
    fText->SetTextAlign(kHAlignLeft+kVAlignTop);
    fText->DrawText(10, fHisto.GetHisto("T0")->GetMaximum(), Form("Bursts processed: %d", NBurstsProcessed));
    fText->DrawText(10, 0.85*fHisto.GetHisto("T0")->GetMaximum(), Form("Events processed: %d", NEventsProcessed));
  }

  fFrontCanvas->cd(2);
  fHisto.GetHisto("T0Resolution")->SetStats(0);
  fHisto.GetHisto("T0Resolution")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("T0Resolution")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("T0Resolution")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("T0Resolution")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("T0Resolution")->GetYaxis()->SetTitleOffset(0.5);
  fHisto.GetHisto("T0Resolution")->GetXaxis()->SetTitle("Readout channel ID");
  fHisto.GetHisto("T0Resolution")->GetYaxis()->SetTitle("Peak width and its error [ns]");
  fHisto.GetHisto("T0Resolution")->SetLineColor(kBlue);
  fHisto.GetHisto("T0Resolution")->SetMarkerColor(kBlue);
  fHisto.GetHisto("T0Resolution")->Draw();

  fFrontCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the front canvas

  ///////////////////////////////////////////
  // Time distribution plots (multiple pages)

  Int_t Npages = fNChannelsActive/16;
  if (fNChannelsActive%16) Npages++;

  fText->SetTextAlign(kHAlignCenter+kVAlignCenter);
  fText->SetTextSize(0.15);
  fText->SetTextColor(kGreen+2);

  if (fPlotChannelTimes) {

    gStyle->SetOptStat("e"); // print the number of entries
    for (Int_t ipage=0; ipage<Npages; ipage++) {
      for (Int_t i=0; i<16; i++) {
	fCanvas->GetPad(i+1)->Clear();
	Int_t ichActive = ipage*16 + i;
	if (ichActive>=fNChannelsActive) continue;
	Int_t ich = ActiveChannelMap[ichActive];
	fCanvas->cd(i+1);
	Int_t Integral = fHTime[ich]->Integral(); // the integral is later changed by SetRangeUser()
	Int_t maxbin   = fHTime[ich]->GetMaximumBin();
	Double_t c0    = fHTime[ich]->GetBinContent(maxbin);
	if (c0<1) c0 = 1;
	fHTime[ich]->SetMaximum(1.1*c0);
	fHTime[ich]->GetXaxis()->SetRangeUser(-fHistoTimeLimit, +fHistoTimeLimit);
	fHTime[ich]->GetXaxis()->SetLabelSize(0.07);
	fHTime[ich]->GetYaxis()->SetLabelSize(0.055);
	fHTime[ich]->GetXaxis()->SetTitle("");
	fHTime[ich]->SetLineWidth(1);
	fHTime[ich]->Draw();

	if (fFChannelFit[ich]) {
	  fFChannelFit[ich]->SetLineWidth(1);
	  fFChannelFit[ich]->Draw("same");
	}

	// Draw line at the position of the second highest peak
	if (fWarning[ich]) {
	  l->SetLineColor(kGreen+2);
	  l->SetLineWidth(1);
	  l->DrawLine(fSecondPeakPos[ich], 0., fSecondPeakPos[ich], fHTime[ich]->GetMaximum());
	  fText->DrawText(0, 0.55*c0, "WARNING");
	}
	else if (Integral==0) {
	  fText->DrawText(0, 0.55*c0, "EMPTY");
	}
	else if (Integral<fMinIntegral) {
	  fText->SetTextColor(kRed);
	  fText->DrawText(0, 0.55*c0, "FEW ENTRIES");
	  fText->SetTextColor(kGreen+2);
	}
	else if (fT0[ich]>999.0) {
	  fText->SetTextColor(kRed);
	  fText->DrawText(0, 0.55*c0, "FIT FAILED");
	  fText->SetTextColor(kGreen+2);
	}
      }
      fCanvas->Print(fOutPDFFileName, "pdf");
    }
  }
  else {
    cout << user_normal() << "Channel time plots not requested" << endl;
  }

  ////////////////////////////////////////
  // Time stability plots (multiple pages)

  if (fPlotTimeDependences) {

    for (Int_t ipage=0; ipage<Npages; ipage++) {
      for (Int_t i=0; i<16; i++) {
	fCanvas->GetPad(i+1)->Clear();
	Int_t ichActive = ipage*16 + i;
	if (ichActive>=fNChannelsActive) continue;
	Int_t ich = ActiveChannelMap[ichActive];
	fCanvas->cd(i+1);

	Double_t ymin = 999, ymax = -999;
	for (Int_t iBin=1; iBin<=fHT0VsTime[ich]->GetNbinsX(); iBin++) {
	  Double_t c = fHT0VsTime[ich]->GetBinContent(iBin);
	  Double_t e = fHT0VsTime[ich]->GetBinError(iBin);
	  if (c) {
	    if (ymin > c-e) ymin = c-e;
	    if (ymax < c+e) ymax = c+e;
	  }
	}
	if (ymin>ymax) { ymin = -1; ymax = +1; }

	fHT0VsTime[ich]->GetXaxis()->SetRangeUser(-0.5, fTimeBinsCounter[ich]-0.5);
	fHT0VsTime[ich]->SetMinimum(ymin);
	fHT0VsTime[ich]->SetMaximum(ymax);
	fHT0VsTime[ich]->SetLineWidth(1);
	fHT0VsTime[ich]->SetLineColor(kBlue);
	fHT0VsTime[ich]->SetMarkerColor(kBlue);
	fFChannelStability[ich]->SetLineColor(kRed);
	fFChannelStability[ich]->SetLineWidth(1);
	fHT0VsTime[ich]->Draw();
	if (fHT0VsTime[ich]->GetEntries()) fFChannelStability[ich]->Draw("same");
      }
      fCanvas->Print(fOutPDFFileName, "pdf");
    }
  }
  else if (!fPlotTimeDependences) {
    cout << user_normal() << "T0 stability plots not requested" << endl;
  }
  else {
    cout << user_normal() << "T0 stability plots not produced (too few input files)" << endl;
  }

  GenerateUserPDFReport(); // add subdetector-specific pages

  fCanvas->Print(Form(fOutPDFFileName + "]"), "pdf"); // close file
  delete fFrontCanvas;
  delete fCanvas;
  delete fText;
  delete l;
  gErrorIgnoreLevel = -1; // restore the default
}

////////////////////////////////
// Build an output file with T0s

void T0Evaluation::GenerateT0TextFile() {
  time_t now = time(0);
  ofstream outfile (fOutTextFileName);
  outfile << "# "<<fDetectorName<<" T0 constants. Format: RO channel; geometric ID; T0 offset (ns)."<<endl;
  outfile << "# These T0 offsets should be subtracted from the raw times."<<endl;
  outfile << "# Special values: -999.999 for masked channels or low statistics, +999.999 for failed T0 fits."<<endl;
  outfile << "# An offset T0 must be ignored by the reconstruction in case |T0|>999ns."<<endl;
  outfile << "#\n# Generated by the T0Evaluation tool on "<<asctime(localtime(&now));
  outfile << "#"<<endl;
  for (Int_t i=0; i<fNChannels; i++) {
    if (fIsActive[i]) outfile << Form("%4d %4d %8.3f\n", i, fChannelID[i], fT0[i]);
  }
  outfile.close();
}
