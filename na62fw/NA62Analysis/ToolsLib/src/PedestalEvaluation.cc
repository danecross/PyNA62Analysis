// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#include "PedestalEvaluation.hh"
#include "NA62ConditionsService.hh"
#include <TRegexp.h>

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class PedestalEvaluation
/// \Brief
/// A generic tool for computation of the Pedestal constants.
/// \EndBrief
/// \Detailed
/// A generic tool for computation of the Pedestal constants for each subdetector.
/// Daughter classes for each subdetector are located in Analyzers/CalibrationTools.
/// The input for Pedestal computations is a 2-dimensional histogram of Pedestal vs (Readout channel ID).
/// The standard histogram name is "PedestalVsROChannel".
/// A Gaussian fit is performed for each channel in the input histogram to evaluate the pedestals.
/// If it fails, a second fit with a constant+Gaussian function is attempted.
/// Basic checks of the distribution shape are performed, and warnings are issued if requested by user.
/// The output is a text file with a table of Pedestal constants
/// (which is NA62Reco-readable for subdetectors that conform to the standard),
/// an output root file with channel Pedestals and resolutions, and a detailed PDF report containing plots.
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

PedestalEvaluation::PedestalEvaluation(Core::BaseAnalysis *ba, std::string DetectorName) : Analyzer(ba), fActiveChannelMap(nullptr),
  fBurstCounter(nullptr), fBinWidth(0.2), fIsActive(nullptr), fH2(nullptr), fH2_Integrated(nullptr), fHNEventsProcessedPerBurst(nullptr),
  fHPedestal(nullptr), fFChannelFit(nullptr), fChannelID(nullptr), fStatus(nullptr), fPedestal(nullptr), fDeltaPedestal(nullptr),
  fPedestalSigma(nullptr), fDeltaPedestalSigma(nullptr), fSecondPeakPos(nullptr), fWarning(nullptr), fGenerateOutputPDF(false),
  fCanvas(nullptr), fFrontCanvas(nullptr), fText(nullptr){

    ///////////////////////////
    // Read external parameters

    // Input & output directory (input conf files, output dat and pdf files)
    AddParam("IODirName", &fIODirName, ".");
    // Name of the input histogram
    AddParam("InputHistoName", &fTH2Name, "PedestalVsROChannel");
    // Generate the output PDF file?
    AddParam("GeneratePDF", &fGenerateOutputPDF, true);

    fDetectorName        = DetectorName;
    fAnalyzerName        = fDetectorName + "Pedestal";
    fDirName             = fDetectorName + "Monitor";

    fMinIntegral            = 100;   ///< Minimal number of entries (excl. underflows, overflows) to attempt fit
    fMinContentMaxBin       = 10.0;  ///< Minimal content of most populated bin to attempt fit
    fFittingRange           = 5.;    ///< Half-width of the fitting Pedestal range
    fEqualisationValue      = 400.;  ///< Pedestal value used for the equalisation (LKr = 400., MUV1/2 = 1000.)
    fHistoPedestalLimit     = 30.0;  ///< Half-size of the X axis span for the PDF report [ADC counts]
    fSignalPeakWidth        = 5.0;   ///< Exclusion region half-width when looking for anomalous shape [ADC counts]
    fInitialPedestalSigma   = 3.0;   ///< Initial value of the time resolution parameter for the fit [ADC counts]
    fMaxPedestalSigma       = 5.0;   ///< Maximum pedestal resolution to consider the fit successful [ADC counts]
    fMaxDeltaPedestal       = 1.0;   ///< Maximum statistical error on pedestal to consider the fit successful [ADC counts]
    fMaxDeltaPedestalSigma  = 1.0;   ///< Maximum statistical error on pedestal resolution to consider the fit successful [ADC counts]
    fIssueWarnings          = true;  // check if the spectrum shape is OK?
    fPage1MinChannelID      = -1;    // adjust channel range in page 1 plots (default: negative = no adjustment)
    fPage1MaxChannelID      = -1;    // adjust channel range in page 1 plots (default: negative = no adjustment)

    // Initialization of the analyzer
    fEvaluatePedestals = fUseChannelMap = true;
    fNChannels = fNChannelsActive = 0;
  }

void PedestalEvaluation::Clear() {
  for (Int_t ich=0; ich<fNChannels; ich++) {
    if (fHPedestal[ich])   delete fHPedestal[ich];
    if (fFChannelFit[ich]) delete fFChannelFit[ich];
  }
  if(fHPedestal)          delete [] fHPedestal;
  if(fBurstCounter)       delete [] fBurstCounter;
  if(fChannelID)          delete [] fChannelID;
  if(fStatus)             delete [] fStatus;
  if(fPedestal)           delete [] fPedestal;
  if(fWarning)            delete [] fWarning;
  if(fIsActive)           delete [] fIsActive;
  if(fActiveChannelMap)   delete [] fActiveChannelMap;
  if(fFChannelFit)        delete [] fFChannelFit;
  if(fDeltaPedestal)      delete [] fDeltaPedestal;
  if(fPedestalSigma)      delete [] fPedestalSigma;
  if(fDeltaPedestalSigma) delete [] fDeltaPedestalSigma;
  if(fSecondPeakPos)      delete [] fSecondPeakPos;
  fHPedestal = nullptr;
  fBurstCounter = nullptr;
  fChannelID = nullptr;
  fStatus = nullptr;
  fPedestal = nullptr;
  fWarning = nullptr;
  fIsActive = nullptr;
  fActiveChannelMap = nullptr;
  fFChannelFit = nullptr;
  fDeltaPedestal = nullptr;
  fPedestalSigma = nullptr;
  fDeltaPedestalSigma = nullptr;
  fSecondPeakPos = nullptr;
}

void PedestalEvaluation::InitHist() {

  if (fIODirName!=".") {
    cout << user_normal() << "Directory for input & output files set to " << fIODirName << endl;
  }
  if (!GetIsHisto() || GetIsTree()) {
    cout << user_normal() << "Error: PedestalEvaluation-based analyzers must be run in the --histo mode" << endl;
    exit(kWrongConfiguration);
  }

  fRawDecoderSettingsFileName = fDetectorName+"-RawDecoderSettings.dat";
  fOutTextFileName = fIODirName + "/" + fDetectorName + "-Pedestal.dat";
  fOutPDFFileName  = fIODirName + "/" + fDetectorName + "-Pedestal.pdf";

  fHNEventsProcessedPerBurst = (TH1F*)RequestHistogram("/", "EventsPerBurst", true);

  if (fEvaluatePedestals) {
    fH2            = (TH2F*)RequestHistogram(fDirName, fTH2Name, false); // reset for each input file
    fH2_Integrated = (TH2F*)RequestHistogram(fDirName, fTH2Name, true);  // accumulated
    if (!fH2) {
      fEvaluatePedestals = false;
      cout << user_normal() << "Input histogram " << fDirName << "/" << fTH2Name << " not found" << endl;
    }
    else {
      cout << user_normal() << "Processing input histogram " << fDirName << "/" << fTH2Name << endl;
    }
  }

  RequestUserHistograms();

  if (fEvaluatePedestals) {
    fNChannels = fH2->GetNbinsX();
    fBinWidth  = fH2->GetYaxis()->GetBinWidth(1);
  }

  if(fNChannels){
    fHPedestal          = new TH1D*   [fNChannels];
    fBurstCounter       = new Int_t   [fNChannels];
    fChannelID          = new Int_t   [fNChannels];
    fStatus             = new Int_t   [fNChannels];
    fPedestal           = new Double_t[fNChannels];
    fWarning            = new Bool_t  [fNChannels];
    fIsActive           = new Bool_t  [fNChannels];
    fActiveChannelMap   = new Int_t   [fNChannels];
    fFChannelFit        = new TF1*    [fNChannels];
    fDeltaPedestal      = new Double_t[fNChannels];
    fPedestalSigma      = new Double_t[fNChannels];
    fDeltaPedestalSigma = new Double_t[fNChannels];
    fSecondPeakPos      = new Double_t[fNChannels];
  }

  for (Int_t i=0; i<fNChannels; i++) {
    fHPedestal[i]          = nullptr;
    fBurstCounter[i]       = 0;
    fChannelID[i]          = -99;
    fStatus[i]             = 99;
    fPedestal[i]           = -999.999;
    fWarning[i]            = false;
    fIsActive[i]           = true; // all channels are active by default
    fActiveChannelMap[i]   = -1;
    fFChannelFit[i]        = nullptr;
    fDeltaPedestal[i]      = 0.;
    fPedestalSigma[i]      = 0.;
    fDeltaPedestalSigma[i] = 0.;
    fSecondPeakPos[i]      = 0.;
  }

  // Book monitoring histograms to be saved into the output
  if (fEvaluatePedestals) {
    BookHisto (new TH1F
        ("Pedestal", "Pedestal;RO channel ID;Pedestal and its error",
         fNChannels, -0.5, fNChannels-0.5));
    BookHisto (new TH1F
        ("PedestalSigma",	"PedestalSigma;RO channel ID;Pedestal resolution and its error",
         fNChannels, -0.5, fNChannels-0.5));
  }
}

void PedestalEvaluation::StartOfRunUser() {
  // Find and read the channel map
  if (fUseChannelMap) {
    ParseRawDecoderSettingsFile();
  }
  else {
    cout << user_normal() << "RawDecoderSettings file ignored on user request" << endl;
  }

  // Check which the RO channels are active
  fNChannelsActive = 0;
  for (Int_t ich=0; ich<fNChannels; ich++) {
    if (fUseChannelMap) fIsActive[ich] = (fChannelID[ich]>=0);
    if (fIsActive[ich]) {
      fActiveChannelMap[fNChannelsActive++] = ich;
    }
  }

}

///////////////////////////////////////////////////
// Read the channel map from the configuration file

void PedestalEvaluation::ParseRawDecoderSettingsFile() {
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
      if(16*iCh+jCh<fNChannels) fChannelID[16*iCh+jCh] = ((TObjString*)(l->At(jCh+1)))->GetString().Atoi();
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fRawDecoderSettingsFileName);
}

void PedestalEvaluation::EndOfJobUser() {

  // Evaluate the Pedestals and global offset with the full data sample
  if (fEvaluatePedestals) {
    EvaluatePedestals(fH2_Integrated, -1, fIssueWarnings); // -1: all channels

    // Generate and save the output
    if (fEvaluatePedestals) {
      Int_t NSuccessfulFits = 0;
      for (Int_t ich=0; ich<fNChannels; ich++) {
        if (!fStatus[ich]) NSuccessfulFits++;
      }
      if (NSuccessfulFits) {
        GeneratePedestalTextFile();
        GeneratePDFReport();
      }
      else {
        cout << user_normal() << "All Pedestal fits failed, no DAT and PDF output produced" << endl;
      }
    }
  }

  SaveAllPlots(); // Save the Pedestal and Pedestal resolution vs channel ID plots
  Clear();
}

void PedestalEvaluation::EvaluatePedestals(TH2F *h2, Int_t ChannelID, Bool_t IssueWarnings) {
  // Evaluate Pedestal for all channels or a single channel?
  Int_t ch1 = 0;
  Int_t ch2 = fNChannels-1;
  if (ChannelID>=0) ch1 = ch2 = ChannelID;

  for (Int_t ich=ch1; ich<=ch2; ich++) {
    if (fIsActive[ich]) {
      TString Name = (fUseChannelMap) ?
        Form("%sRO%04dGeomID%04d", fDetectorName.Data(), ich, fChannelID[ich]) :
        Form("%sRO%04d", fDetectorName.Data(), ich);
      if (fHPedestal[ich]) delete fHPedestal[ich];
      fHPedestal[ich] = h2->ProjectionY(Name, ich+1, ich+1);
      fHPedestal[ich]->SetTitle(Name);
      fHPedestal[ich]->SetDirectory(0); //needed to detach the histo from root file (otherwise it cannot be deleted!)
      EvaluateChannelPedestal(ich, IssueWarnings);
    }
  }
}

///////////////////////////
// Evaluate Pedestal in a channel

void PedestalEvaluation::EvaluateChannelPedestal (Int_t ich, Bool_t IssueWarning) {

  // Check if there are enough entries for the fit to converge
  fStatus[ich] = 9; //active but dead
  if (fHPedestal[ich]->Integral()==0) return;
  fStatus[ich] = 1; //active but fit failed
  if (fHPedestal[ich]->Integral()<fMinIntegral) return;

  // Fitting interval: around the bin with max content
  Int_t maxbin  = fHPedestal[ich]->GetMaximumBin();
  Double_t c0   = fHPedestal[ich]->GetBinCenter(maxbin);
  Double_t cmin = c0 - fFittingRange;
  Double_t cmax = c0 + fFittingRange;
  Double_t maxc = fHPedestal[ich]->GetBinContent(maxbin);
  if (maxc < fMinContentMaxBin) return;

  if (!FitChannel(ich, c0, cmin, cmax, maxc)) return;

  fHisto.GetHisto("Pedestal")->SetBinContent(ich+1, fPedestal[ich]);
  fHisto.GetHisto("Pedestal")->SetBinError  (ich+1, fDeltaPedestal[ich]);
  fHisto.GetHisto("PedestalSigma")->SetBinContent(ich+1, fPedestalSigma[ich]);
  fHisto.GetHisto("PedestalSigma")->SetBinError  (ich+1, fDeltaPedestalSigma[ich]);

  /////////////////////////////////////////////////
  // Issue a warning in case there is a second peak

  if (IssueWarning && maxc>20.0) {
    Int_t    nsidebandbins = 0;
    Double_t background    = 0.0;
    Int_t    highestbin    = 0;
    Double_t highestcont   = 0.0;
    for (Int_t ibin=1; ibin<=fHPedestal[ich]->GetNbinsX(); ibin++) {
      Double_t t    = fHPedestal[ich]->GetBinCenter(ibin);
      Double_t cont = fHPedestal[ich]->GetBinContent(ibin);
      if (fabs(t-fPedestal[ich]) < fSignalPeakWidth) continue;
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
      fSecondPeakPos[ich] = fHPedestal[ich]->GetBinCenter(highestbin);
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

Bool_t PedestalEvaluation::FitChannel(Int_t ich, Double_t c0, Double_t cmin, Double_t cmax, Double_t maxc) {

  fFChannelFit[ich] = new TF1("gaus", "gaus", cmin, cmax);

  // Initial parameters of the fitting function: Gaussian amplitude, mean value (=Pedestal) and RMS (=time resolution)
  fFChannelFit[ich]->SetParameters(maxc, c0, fInitialPedestalSigma);
  fHPedestal[ich]->Fit(fFChannelFit[ich], "R0Q");
  Double_t Pedestal   = fFChannelFit[ich]->GetParameter(1);
  Double_t PedestalSigma      = fFChannelFit[ich]->GetParameter(2);
  Double_t DeltaPedestal = fFChannelFit[ich]->GetParError(1);
  Double_t DeltaPedestalSigma = fFChannelFit[ich]->GetParError(2);

  //////////////////////////////////////////////////////////////////////////////
  // If the first fit fails, make a second attempt with more degrees of freedom.
  // The fit success checks are performed on:
  //   time resolution (fMaxPedestalSigma);
  //   statistical error on Pedestal (fMaxDeltaPedestal);
  //   statistical error on time resolution (fMaxDeltaPedestalSigma).

  if (PedestalSigma>fMaxPedestalSigma || DeltaPedestal>fMaxDeltaPedestal || DeltaPedestalSigma>fMaxDeltaPedestalSigma) {

    delete fFChannelFit[ich];
    fFChannelFit[ich] = new TF1("GausPol", "gaus(0)+pol0(3)", cmin, cmax);

    // Initial parameters of the fitting function:
    // Gaussian amplitude, mean value (=Pedestal) and RMS (=time resolution), flat background rate
    fFChannelFit[ich]->SetParameters(maxc, c0, fInitialPedestalSigma, 0.0);
    fHPedestal[ich]->Fit(fFChannelFit[ich], "R0Q");
    Pedestal         = fFChannelFit[ich]->GetParameter(1);
    PedestalSigma      = fFChannelFit[ich]->GetParameter(2);
    DeltaPedestal    = fFChannelFit[ich]->GetParError(1);
    DeltaPedestalSigma = fFChannelFit[ich]->GetParError(2);

    // Check if the second fit is successful
    if (PedestalSigma>fMaxPedestalSigma || DeltaPedestal>fMaxDeltaPedestal || DeltaPedestalSigma>fMaxDeltaPedestalSigma) return false;
  }

  fPedestal[ich]      = Pedestal;
  fDeltaPedestal[ich] = DeltaPedestal;
  fPedestalSigma[ich] = PedestalSigma;
  fDeltaPedestalSigma[ich] = DeltaPedestalSigma;
  fStatus[ich] = 0; //success
  return true;
}

/////////////////////
// Build a PDF report

void PedestalEvaluation::GeneratePDFReport() {

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

  fFrontCanvas->cd(1);
  fHisto.GetHisto("Pedestal")->SetStats(0);
  fHisto.GetHisto("Pedestal")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("Pedestal")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("Pedestal")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("Pedestal")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("Pedestal")->GetYaxis()->SetTitleOffset(0.5);
  fHisto.GetHisto("Pedestal")->GetXaxis()->SetTitle("Readout channel ID");
  fHisto.GetHisto("Pedestal")->GetYaxis()->SetTitle("Pedestal and its error [ns]");
  fHisto.GetHisto("Pedestal")->SetLineColor(kBlue);
  fHisto.GetHisto("Pedestal")->SetMarkerColor(kBlue);
  fHisto.GetHisto("Pedestal")->GetYaxis()->SetRangeUser(fEqualisationValue-fHistoPedestalLimit, fEqualisationValue+fHistoPedestalLimit);
  fHisto.GetHisto("Pedestal")->Draw();

  if (fHNEventsProcessedPerBurst) {
    fText->SetTextSize(0.07);
    fText->SetTextColor(kBlack);
    fText->SetTextAlign(kHAlignLeft+kVAlignTop);
    fText->DrawText(10, fHisto.GetHisto("Pedestal")->GetMaximum(), Form("Bursts processed: %d", NBurstsProcessed));
    fText->DrawText(10, 0.99*fHisto.GetHisto("Pedestal")->GetMaximum(), Form("Events processed: %d", NEventsProcessed));
  }

  fFrontCanvas->cd(2);
  fHisto.GetHisto("PedestalSigma")->SetStats(0);
  fHisto.GetHisto("PedestalSigma")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("PedestalSigma")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("PedestalSigma")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("PedestalSigma")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("PedestalSigma")->GetYaxis()->SetTitleOffset(0.5);
  fHisto.GetHisto("PedestalSigma")->GetXaxis()->SetTitle("Readout channel ID");
  fHisto.GetHisto("PedestalSigma")->GetYaxis()->SetTitle("Peak width and its error [ns]");
  fHisto.GetHisto("PedestalSigma")->SetLineColor(kBlue);
  fHisto.GetHisto("PedestalSigma")->SetMarkerColor(kBlue);
  fHisto.GetHisto("PedestalSigma")->Draw();

  fFrontCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the front canvas

  ///////////////////////////////////////////
  // Pedestal distribution plots (multiple pages)

  Int_t Npages = fNChannelsActive/16;
  if (fNChannelsActive%16) Npages++;

  fText->SetTextAlign(kHAlignCenter+kVAlignCenter);
  fText->SetTextSize(0.15);
  fText->SetTextColor(kGreen+2);

  gStyle->SetOptStat("e"); // print the number of entries
  for (Int_t ipage=0; ipage<Npages; ipage++) {
    for (Int_t i=0; i<16; i++) {
      fCanvas->GetPad(i+1)->Clear();
      Int_t ichActive = ipage*16 + i;
      if (ichActive>=fNChannelsActive) continue;
      Int_t ich = fActiveChannelMap[ichActive];
      fCanvas->cd(i+1);
      Int_t Integral = fHPedestal[ich]->Integral(); // the integral is later changed by SetRangeUser()
      Int_t maxbin   = fHPedestal[ich]->GetMaximumBin();
      Double_t c0    = fHPedestal[ich]->GetBinContent(maxbin);
      if (c0<1) c0 = 1;
      fHPedestal[ich]->SetMaximum(1.1*c0);
      fHPedestal[ich]->GetXaxis()->SetRangeUser(fEqualisationValue-fHistoPedestalLimit, fEqualisationValue+fHistoPedestalLimit);
      fHPedestal[ich]->GetXaxis()->SetLabelSize(0.07);
      fHPedestal[ich]->GetYaxis()->SetLabelSize(0.055);
      fHPedestal[ich]->GetXaxis()->SetTitle("");
      fHPedestal[ich]->SetLineWidth(1);
      fHPedestal[ich]->Draw();

      if (fFChannelFit[ich]) {
        fFChannelFit[ich]->SetLineWidth(1);
        fFChannelFit[ich]->Draw("same");
      }

      // Draw line at the position of the second highest peak
      if (fWarning[ich]) {
        l->SetLineColor(kGreen+2);
        l->SetLineWidth(1);
        l->DrawLine(fSecondPeakPos[ich], 0., fSecondPeakPos[ich], fHPedestal[ich]->GetMaximum());
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
      else if (fStatus[ich]==1) {
        fText->SetTextColor(kRed);
        fText->DrawText(0, 0.55*c0, "FIT FAILED");
        fText->SetTextColor(kGreen+2);
      }
    }
    fCanvas->Print(fOutPDFFileName, "pdf");
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
// Build an output file with Pedestals

void PedestalEvaluation::GeneratePedestalTextFile() {
  time_t now = time(0);
  ofstream outfile (fOutTextFileName);
  outfile << "# "<<fDetectorName<<" Pedestal constants. Format: RO channel; geometric ID; Status; Pedestal value; Pedestal sigma."<<endl;
  outfile << "# These pedestals should be subtracted from the ADC counts."<<endl;
  outfile << "# Special value: -999.999 for masked channels, low statistics or for failed Pedestal fits."<<endl;
  outfile << "# Status: 0 = OK; 99 = not instrumented; 9 = dead cell; 1 = fit failed." << endl;
  outfile << "#\n# Generated by the PedestalEvaluation tool on "<<asctime(localtime(&now));
  outfile << "#"<<endl;
  for (Int_t ich=0; ich<fNChannels; ich++) {
    outfile << Form("%5d %6d %3d %8.3f %8.3f\n", ich, fChannelID[ich], fStatus[ich], fPedestal[ich], fPedestalSigma[ich]);
  }
  outfile.close();
}
