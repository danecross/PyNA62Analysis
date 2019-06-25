// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-08-09
//
// ---------------------------------------------------------

#include "SpectrometerMagicT0.hh"
#include <TRegexp.h>

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class SpectrometerMagicT0
/// \Brief
/// A tool for computation of the Spectrometer MagicT0.
/// \EndBrief
/// \Detailed
/// A tool for computation of the Spectrometer MagicT0.
/// The input for the MagicT0 computation is a 2-dimensional histogram of RecoWire vs (Magic T0 hypothesis).
/// The standard histogram name is "RecoHitWireSum2VsMagicT0".
/// A Gaussian fit is performed for each hypothesis in the input histogram to find the one with reco wire = 4.4.
/// The output is a text file with the MagicT0 corresponding to a reco wire = 4.4
/// and a detailed PDF report containing plots.
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

SpectrometerMagicT0::SpectrometerMagicT0(Core::BaseAnalysis *ba) : Analyzer(ba,"SpectrometerMagicT0"),
  fBurstCounter(nullptr), fBinWidth(0.2), fH2(nullptr), fH2_Integrated(nullptr), fHNEventsProcessedPerBurst(nullptr), fHRecoWireSum(nullptr),
  fFChannelFit(nullptr), fMagicT0(nullptr), fRecoWire(nullptr), fDeltaRecoWire(nullptr), fRecoWireSigma(nullptr), fDeltaRecoWireSigma(nullptr),
  fGenerateOutputPDF(false), fCanvas(nullptr), fFrontCanvas(nullptr), fText(nullptr){

    ///////////////////////////
    // Read external parameters

    // Input & output directory (input conf files, output dat and pdf files)
    AddParam("IODirName", &fIODirName, ".");
    // Name of the input histogram
    AddParam("InputHistoName", &fTH2Name, "RecoHitWireSum2VsMagicT0");
    // Generate the output PDF file?
    AddParam("GeneratePDF", &fGenerateOutputPDF, true);

    fMinIntegral            = 100;   ///< Minimal number of entries (excl. underflows, overflows) to attempt fit
    fMinContentMaxBin       = 10.0;  ///< Minimal content of most populated bin to attempt fit
    fInitialRecoWireSigma   = 0.4;   ///< Initial RecoWireSigma
    fFittingRange           = 0.7;   ///< Half-width of the fitting RecoWire range
    fMaxDeltaRecoWire       = 1.0;   ///< Maximum statistical error on RecoWire to consider the fit successful
    fMaxDeltaRecoWireSigma  = 1.0;   ///< Maximum statistical error on RecoWireSigma to consider the fit successful

    // Initialization of the analyzer
    fNHypotheses = 0;
  }

SpectrometerMagicT0::~SpectrometerMagicT0(){}

void SpectrometerMagicT0::Clear() {
  for (Int_t iHp=0; iHp<fNHypotheses; iHp++) {
    if (fHRecoWireSum[iHp])  delete fHRecoWireSum[iHp];
    if (fFChannelFit[iHp])   delete fFChannelFit[iHp];
  }
  if(fHRecoWireSum)          delete [] fHRecoWireSum;
  if(fBurstCounter)          delete [] fBurstCounter;
  if(fMagicT0)               delete [] fMagicT0;
  if(fRecoWire)              delete [] fRecoWire;
  if(fFChannelFit)           delete [] fFChannelFit;
  if(fDeltaRecoWire)         delete [] fDeltaRecoWire;
  if(fRecoWireSigma)         delete [] fRecoWireSigma;
  if(fDeltaRecoWireSigma)    delete [] fDeltaRecoWireSigma;
  fHRecoWireSum = nullptr;
  fBurstCounter = nullptr;
  fMagicT0 = nullptr;
  fRecoWire = nullptr;
  fFChannelFit = nullptr;
  fDeltaRecoWire = nullptr;
  fRecoWireSigma = nullptr;
  fDeltaRecoWireSigma = nullptr;
}

void SpectrometerMagicT0::InitHist() {

  if (fIODirName!=".") {
    cout << user_normal() << "Directory for input & output files set to " << fIODirName << endl;
  }
  if (!GetIsHisto() || GetIsTree()) {
    cout << user_normal() << "Error: SpectrometerMagicT0-based analyzers must be run in the --histo mode" << endl;
    exit(kWrongConfiguration);
  }

  fOutTextFileName = fIODirName + "/Spectrometer-MagicT0.dat";
  fOutPDFFileName  = fIODirName + "/Spectrometer-MagicT0.pdf";

  fHNEventsProcessedPerBurst = static_cast<TH1F*>(RequestHistogram("/", "EventsPerBurst", true));

  fH2            = static_cast<TH2F*>(RequestHistogram("SpectrometerMonitor", fTH2Name, false)); // reset for each input file
  fH2_Integrated = static_cast<TH2F*>(RequestHistogram("SpectrometerMonitor", fTH2Name, true));  // accumulated
  if (!fH2) {
    cout << user_normal() << "Input histogram SpectrometerMonitor/" << fTH2Name << " not found" << endl;
  }
  else {
    cout << user_normal() << "Processing input histogram SpectrometerMonitor/" << fTH2Name << endl;
  }

  RequestUserHistograms();

  if (fH2) {
    fNHypotheses = fH2->GetNbinsX();
    fBinWidth  = fH2->GetYaxis()->GetBinWidth(1);
    // Book monitoring histograms to be saved into the output
    BookHisto(new TH1F("RecoWire", "RecoWire;RO channel ID;RecoWire and its error",
          fNHypotheses, fH2->GetXaxis()->GetXmin(), fH2->GetXaxis()->GetXmax()));
    BookHisto(new TH1F("RecoWireSigma","RecoWireSigma;RO channel ID;RecoWire resolution and its error",
          fNHypotheses, fH2->GetXaxis()->GetXmin(), fH2->GetXaxis()->GetXmax()));
  }

  if(fNHypotheses){
    fHRecoWireSum       = new TH1D*   [fNHypotheses];
    fBurstCounter       = new Int_t   [fNHypotheses];
    fMagicT0            = new Double_t[fNHypotheses];
    fRecoWire           = new Double_t[fNHypotheses];
    fFChannelFit        = new TF1*    [fNHypotheses];
    fDeltaRecoWire      = new Double_t[fNHypotheses];
    fRecoWireSigma      = new Double_t[fNHypotheses];
    fDeltaRecoWireSigma = new Double_t[fNHypotheses];
  }

  for (Int_t i=0; i<fNHypotheses; i++) {
    fHRecoWireSum[i]       = nullptr;
    fBurstCounter[i]       = 0;
    fMagicT0[i]            = -999.999;
    fRecoWire[i]           = -999.999;
    fFChannelFit[i]        = nullptr;
    fDeltaRecoWire[i]      = 0.;
    fRecoWireSigma[i]      = 0.;
    fDeltaRecoWireSigma[i] = 0.;
  }
}

void SpectrometerMagicT0::EndOfJobUser() {

  // Evaluate the MagicT0s and global offset with the full data sample
  EvaluateRecoWires(fH2_Integrated, -1); // -1: all hypotheses

  // Generate and save the output
  GenerateMagicT0TextFile();
  GeneratePDFReport();

  SaveAllPlots(); // Save the RecoWire and RecoWire resolution vs MagicT0 hypothesis plots
  Clear();
}

void SpectrometerMagicT0::EvaluateRecoWires(TH2F *h2, Int_t Hypothesis) {
  // Evaluate RecoWire for all hypotheses or a single hypothesis
  Int_t Hp1 = 0;
  Int_t Hp2 = fNHypotheses-1;
  if (Hypothesis>=0) Hp1 = Hp2 = Hypothesis;

  if (h2->Integral()<fMinIntegral) {
    std::cerr << "[SpectrometerMagicT0] WARNING: less than " << fMinIntegral << " entries in the " << fTH2Name << " histo!" << std::endl;
  }

  for (Int_t iHp=Hp1; iHp<=Hp2; iHp++) {
    fMagicT0[iHp] = h2->GetXaxis()->GetBinCenter(iHp+1);
    TString Name = Form("MagicT0_%04d", (Int_t)(fMagicT0[iHp]*1000.));
    if (fHRecoWireSum[iHp]) delete fHRecoWireSum[iHp];
    fHRecoWireSum[iHp] = h2->ProjectionY(Name, iHp+1, iHp+1);
    fHRecoWireSum[iHp]->SetTitle(Name);
    fHRecoWireSum[iHp]->SetDirectory(0); //needed to detach the histo from root file (otherwise it cannot be deleted!)
    EvaluateRecoWire(iHp);
  }
}

/////////////////////////////////////////////
// Evaluate RecoWire for a MagicT0 hypothesis

void SpectrometerMagicT0::EvaluateRecoWire(Int_t iHp) {

  // Check if there are enough entries for the fit to converge
  if (fHRecoWireSum[iHp]->Integral()<fMinIntegral) return;

  // Fitting interval: around the bin with max content
  Int_t maxbin  = fHRecoWireSum[iHp]->GetMaximumBin();
  Double_t c0   = fHRecoWireSum[iHp]->GetBinCenter(maxbin);
  Double_t cmin = c0 - fFittingRange;
  Double_t cmax = c0 + fFittingRange;
  Double_t maxc = fHRecoWireSum[iHp]->GetBinContent(maxbin);
  if (maxc < fMinContentMaxBin) return;

  if (!FitMagicT0(iHp, c0, cmin, cmax, maxc)) return;

  fHisto.GetHisto("RecoWire")->SetBinContent(iHp+1, fRecoWire[iHp]);
  fHisto.GetHisto("RecoWire")->SetBinError  (iHp+1, fDeltaRecoWire[iHp]);
  fHisto.GetHisto("RecoWireSigma")->SetBinContent(iHp+1, fRecoWireSigma[iHp]);
  fHisto.GetHisto("RecoWireSigma")->SetBinError  (iHp+1, fDeltaRecoWireSigma[iHp]);

}

//////////////////////////////////////////////////////////////////////////
// The fitting routine in a channel, can be overloaded in daughter classes
// Parameters:
//   iHp: channel number;
//   c0: centre of the most populated bin of the time distribution;
//   cmin, cmax: fitting range;
//   cmax: content of the most populated bin

Bool_t SpectrometerMagicT0::FitMagicT0(Int_t iHp, Double_t c0, Double_t cmin, Double_t cmax, Double_t maxc) {

  fFChannelFit[iHp] = new TF1("gaus", "gaus", cmin, cmax);

  // Initial parameters of the fitting function: Gaussian amplitude, mean value (=RecoWire) and RMS (=RecoWire resolution)
  fFChannelFit[iHp]->SetParameters(maxc, c0, fInitialRecoWireSigma);
  fHRecoWireSum[iHp]->Fit(fFChannelFit[iHp], "R0Q");
  Double_t RecoWire   = fFChannelFit[iHp]->GetParameter(1);
  Double_t RecoWireSigma      = fFChannelFit[iHp]->GetParameter(2);
  Double_t DeltaRecoWire = fFChannelFit[iHp]->GetParError(1);
  Double_t DeltaRecoWireSigma = fFChannelFit[iHp]->GetParError(2);

  // Check if the fit is successful
  if (DeltaRecoWire>fMaxDeltaRecoWire || DeltaRecoWireSigma>fMaxDeltaRecoWireSigma) return false;

  fRecoWire[iHp]      = RecoWire;
  fDeltaRecoWire[iHp] = DeltaRecoWire;
  fRecoWireSigma[iHp] = RecoWireSigma;
  fDeltaRecoWireSigma[iHp] = DeltaRecoWireSigma;
  return true;
}

/////////////////////
// Build a PDF report

void SpectrometerMagicT0::GeneratePDFReport() {

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
  fHisto.GetHisto("RecoWire")->SetStats(0);
  fHisto.GetHisto("RecoWire")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("RecoWire")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("RecoWire")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("RecoWire")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("RecoWire")->GetYaxis()->SetTitleOffset(0.5);
  fHisto.GetHisto("RecoWire")->GetXaxis()->SetTitle("Readout channel ID");
  fHisto.GetHisto("RecoWire")->GetYaxis()->SetTitle("RecoWire and its error [ns]");
  fHisto.GetHisto("RecoWire")->SetLineColor(kBlue);
  fHisto.GetHisto("RecoWire")->SetMarkerColor(kBlue);
  fHisto.GetHisto("RecoWire")->GetYaxis()->SetRangeUser(0,10);
  fHisto.GetHisto("RecoWire")->Draw();

  if (fHNEventsProcessedPerBurst) {
    fText->SetTextSize(0.07);
    fText->SetTextColor(kBlack);
    fText->SetTextAlign(kHAlignLeft+kVAlignTop);
    fText->DrawText(10, fHisto.GetHisto("RecoWire")->GetMaximum(), Form("Bursts processed: %d", NBurstsProcessed));
    fText->DrawText(10, 0.80*fHisto.GetHisto("RecoWire")->GetMaximum(), Form("Events processed: %d", NEventsProcessed));
  }

  fFrontCanvas->cd(2);
  fHisto.GetHisto("RecoWireSigma")->SetStats(0);
  fHisto.GetHisto("RecoWireSigma")->GetXaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("RecoWireSigma")->GetYaxis()->SetTitleSize(0.05);
  fHisto.GetHisto("RecoWireSigma")->GetXaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("RecoWireSigma")->GetYaxis()->SetLabelSize(0.05);
  fHisto.GetHisto("RecoWireSigma")->GetYaxis()->SetTitleOffset(0.5);
  fHisto.GetHisto("RecoWireSigma")->GetXaxis()->SetTitle("Readout channel ID");
  fHisto.GetHisto("RecoWireSigma")->GetYaxis()->SetTitle("Peak width and its error [ns]");
  fHisto.GetHisto("RecoWireSigma")->SetLineColor(kBlue);
  fHisto.GetHisto("RecoWireSigma")->SetMarkerColor(kBlue);
  fHisto.GetHisto("RecoWireSigma")->Draw();

  fFrontCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the front canvas

  ///////////////////////////////////////////
  // RecoWire distribution plots (multiple pages)

  Int_t Npages = fNHypotheses/16;
  if (fNHypotheses%16) Npages++;

  fText->SetTextAlign(kHAlignCenter+kVAlignCenter);
  fText->SetTextSize(0.15);
  fText->SetTextColor(kGreen+2);

  gStyle->SetOptStat("e"); // print the number of entries
  for (Int_t ipage=0; ipage<Npages; ipage++) {
    for (Int_t i=0; i<16; i++) {
      fCanvas->GetPad(i+1)->Clear();
      Int_t iHp = ipage*16 + i;
      if (iHp>=fNHypotheses) continue;
      fCanvas->cd(i+1);
      Int_t Integral = fHRecoWireSum[iHp]->Integral(); // the integral is later changed by SetRangeUser()
      Int_t maxbin   = fHRecoWireSum[iHp]->GetMaximumBin();
      Double_t c0    = fHRecoWireSum[iHp]->GetBinContent(maxbin);
      if (c0<1) c0 = 1;
      fHRecoWireSum[iHp]->SetMaximum(1.1*c0);
      fHRecoWireSum[iHp]->GetXaxis()->SetRangeUser(0,10);
      fHRecoWireSum[iHp]->GetXaxis()->SetLabelSize(0.07);
      fHRecoWireSum[iHp]->GetYaxis()->SetLabelSize(0.055);
      fHRecoWireSum[iHp]->GetXaxis()->SetTitle("");
      fHRecoWireSum[iHp]->SetLineWidth(1);
      fHRecoWireSum[iHp]->Draw();

      if (fFChannelFit[iHp]) {
        fFChannelFit[iHp]->SetLineWidth(1);
        fFChannelFit[iHp]->Draw("same");
      }

      // Draw line at the position of the second highest peak
      if (Integral==0) {
        fText->DrawText(0, 0.55*c0, "EMPTY");
      }
      else if (Integral<fMinIntegral) {
        fText->SetTextColor(kRed);
        fText->DrawText(0, 0.55*c0, "FEW ENTRIES");
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
// Build an output file with RecoWires

void SpectrometerMagicT0::GenerateMagicT0TextFile() {
  Double_t MinDeltaWire=1000.;
  Int_t BestHp = -1;
  for (Int_t iHp=0; iHp<fNHypotheses; iHp++) {
    if(fabs(fRecoWire[iHp]-4.4)<MinDeltaWire) {
      MinDeltaWire = fabs(fRecoWire[iHp]-4.4);
      BestHp = iHp;
    }
  }
  if(MinDeltaWire>0.1) return;

  ofstream outfile (fOutTextFileName);
  outfile << "# SpectrometerMagicT0 file. Format: MagicT0; RecoWire value; RecoWire sigma."<<endl;
  outfile << "#\n# Generated by the SpectrometerMagicT0 tool on "<<TimeString();
  outfile << "#"<<endl;
  outfile << Form("%2.3f %2.3f %2.3f\n", fMagicT0[BestHp], fRecoWire[BestHp], fRecoWireSigma[BestHp]);
  outfile.close();
}
