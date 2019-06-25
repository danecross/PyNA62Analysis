// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-27
//
// ---------------------------------------------------------------

/// \class NewCHODDataQualityPlotter
/// \Brief
/// Build a PDF report with using the standard NewCHOD monitoring histograms
/// \EndBrief

#include "NewCHODDataQualityPlotter.hh"

using namespace std;

NewCHODDataQualityPlotter::NewCHODDataQualityPlotter(std::vector<TH1*> Input, TString OutputPDFFileName) :
  fHTileAsymmetry(nullptr),
  fHTileAsymmetryEOB(nullptr),
  fHTotalPrimitiveCountsEOB(nullptr),
  fHErrorCountsEOB(nullptr)
{
  fGeo = NewCHODGeometry::GetInstance();
  fOutPDFFileName = OutputPDFFileName;
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetPalette(1);

  if (!Input.size()) return;

  //////////////////
  // Read histograms

  UInt_t Index = 0;
  fHNEventsProcessedPerBurst            = (TH1D*)Input[Index++];
  fHChannelProfile                      = (TH1D*)Input[Index++];
  fHChannelProfileEOB                   = (TH1D*)Input[Index++];
  fHTileOR                              = (TH1D*)Input[Index++];
  fHTileAND                             = (TH1D*)Input[Index++];
  fHNRecoHitsVsL0TriggerBit             = (TH2F*)Input[Index++];
  fHNRecoHitsVsNoL0TriggerBit           = (TH2F*)Input[Index++];
  fHTightRecoHitProfileVsL0TriggerBit   = (TH2F*)Input[Index++];
  fHTightRecoHitProfileVsNoL0TriggerBit = (TH2F*)Input[Index++];
  fHChannelProfileVsBurst               = (TH2F*)Input[Index++];
  fHChannelProfileVsBurstEOB            = (TH2F*)Input[Index++];
  fTimeWrtReferenceVsReadoutChannelNoT0 = (TH2F*)Input[Index++];
  fTimeWrtReferenceVsReadoutChannel     = (TH2F*)Input[Index++];
  fHTightRecoHitTimeWrtReferenceVsTile  = (TH2F*)Input[Index++];
  fHLooseRecoHitTimeWrtReferenceVsTile  = (TH2F*)Input[Index++];
  fHTightRecoHitTimeWrtReference        = (TH1D*)Input[Index++];
  fHLooseRecoHitTimeWrtReference        = (TH1D*)Input[Index++];
  fHList.resize(9);
  for (Int_t i=0; i<9; i++) fHList[i] = (TH2F*)Input[Index++];
}

void NewCHODDataQualityPlotter::BuildPDF() {

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

  // Build tile asymmetries: same binning as channel profiles
  TString Name = "NewCHODTileAsymmetry";
  fHTileAsymmetry = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHTileAsymmetry->GetXaxis()->SetTitle("NewCHOD tile");
  fHTileAsymmetry->GetYaxis()->SetTitle("Asymmetry N(hits): (PM1-PM0)/(PM1+PM0)");

  Name = "NewCHODTileAsymmetryEOB";
  fHTileAsymmetryEOB = new TH1D(Name, Name, 350, 100.5, 450.5);
  fHTileAsymmetryEOB->GetXaxis()->SetTitle("NewCHOD tile");
  fHTileAsymmetryEOB->GetYaxis()->SetTitle("Asymmetry N(hits): (PM1-PM0)/(PM1+PM0)");

  for (Int_t i=1; i<=fHTileAsymmetry->GetNbinsX(); i++) {
    fHTileAsymmetry->SetBinContent(i, -999);
    fHTileAsymmetryEOB->SetBinContent(i, -999);
  }

  for (Int_t iq=1; iq<=4; iq++) {
    for (Int_t ic=1; ic<=38; ic++) {
      Int_t ich1    = iq*100 + ic;
      Int_t ich2    = ich1 + 50;
      Double_t x    = fHChannelProfile->GetBinContent(ich2-100); // axis starts at 100.5
      Double_t y    = fHChannelProfile->GetBinContent(ich1-100);

      if (x+y<1) continue;
      Double_t dx   = sqrt(x);
      Double_t dy   = sqrt(y);
      Double_t f    = (x-y)/(x+y);
      Double_t dfdx = 2.0*y/(x+y)/(x+y);
      Double_t dfdy = 2.0*x/(x+y)/(x+y);
      Double_t df   = sqrt((dfdx*dx)*(dfdx*dx)+(dfdy*dy)*(dfdy*dy));
      fHTileAsymmetry->SetBinContent(ich1-100, f);
      fHTileAsymmetry->SetBinError(ich1-100, df);
    }
  }

  for (Int_t iq=1; iq<=4; iq++) {
    for (Int_t ic=1; ic<=38; ic++) {
      Int_t ich1    = iq*100 + ic;
      Int_t ich2    = ich1 + 50;
      Double_t x    = fHChannelProfileEOB->GetBinContent(ich2-100); // axis starts at 100.5
      Double_t y    = fHChannelProfileEOB->GetBinContent(ich1-100);
      if (x+y<1) continue;
      Double_t dx   = sqrt(x);
      Double_t dy   = sqrt(y);
      Double_t f    = (x-y)/(x+y);
      Double_t dfdx = 2.0*y/(x+y)/(x+y);
      Double_t dfdy = 2.0*x/(x+y)/(x+y);
      Double_t df   = sqrt((dfdx*dx)*(dfdx*dx)+(dfdy*dy)*(dfdy*dy));
      fHTileAsymmetryEOB->SetBinContent(ich1-100, f);
      fHTileAsymmetryEOB->SetBinError(ich1-100, df);
    }
  }

  TCanvas *Canvas = new TCanvas("NewCHODCanvas0");
  Canvas->Divide(1,3);
  gStyle->SetOptStat(0);

  TText *Text = new TText();

  //////////////////////////////////////////////////
  // Channel profile and numbers of OR, AND in tiles

  Canvas->Print(Form(fOutPDFFileName + "["), "pdf");
  if (!(fHChannelProfile && fHTileOR && fHTileAND)) {
    std::cout << "WARNING: either 'fHChannelProfile', 'fHTileOR', or 'fHTileAND' histogram not found" << std::endl;
  }
  else {
    for (Int_t i=1; i<=3; i++) {
      Canvas->GetPad(i)->SetLeftMargin(0.04);
      Canvas->GetPad(i)->SetRightMargin(0.01);
      Canvas->GetPad(i)->SetTopMargin(0.06);
      Canvas->GetPad(i)->SetBottomMargin(0.10);
    }

    Canvas->cd(1);
    fHChannelProfile->SetTitle("NewCHOD channel profile");
    fHChannelProfile->GetXaxis()->SetTitleSize(0.055);
    fHChannelProfile->GetXaxis()->SetLabelSize(0.055);
    fHChannelProfile->GetYaxis()->SetTitleSize(0.055);
    fHChannelProfile->GetYaxis()->SetLabelSize(0.055);
    fHChannelProfile->SetStats(0);
    fHChannelProfile->SetLineWidth(1);
    fHChannelProfile->SetLineColor(kBlue);
    fHChannelProfile->SetFillColor(kYellow);
    fHChannelProfile->Draw();

    if (fHNEventsProcessedPerBurst) {
      Canvas->Update();
      Text->SetTextSize(0.07);
      Text->SetTextColor(kBlack);
      Text->SetTextAlign(kHAlignLeft+kVAlignTop);
      Text->DrawText(110, 0.92*gPad->GetUymax(), Form("Bursts processed: %d", NBurstsProcessed));
      Text->DrawText(110, 0.80*gPad->GetUymax(), Form("Events processed: %d", NEventsProcessed));
      Text->DrawText(110, 0.68*gPad->GetUymax(), Form("Digis: %d", (int)fHChannelProfile->Integral()));
    }

    Canvas->cd(2);

    ////////////////////////////////////////////

    Double_t ymax = fHTileOR->GetMaximum();
    fHTileOR->SetTitle("NewCHOD: OR, AND of signals in tiles");
    fHTileOR->GetXaxis()->SetTitleSize(0.055);
    fHTileOR->GetXaxis()->SetLabelSize(0.055);
    fHTileOR->GetYaxis()->SetTitleSize(0.055);
    fHTileOR->GetYaxis()->SetLabelSize(0.055);
    fHTileOR->GetYaxis()->SetRangeUser(0, 1.02*ymax);
    fHTileOR->SetStats(0);
    fHTileOR->SetLineWidth(1);
    fHTileOR->SetLineColor(kBlue);
    fHTileAND->SetStats(0);
    fHTileAND->SetLineWidth(1);
    fHTileAND->SetLineColor(kRed);

    fHTileOR->Draw("hist");
    fHTileAND->Draw("hist same");

    // Mark the non-existing tiles
    TBox *b0 = new TBox();
    b0->SetFillColor(kGreen-7);
    b0->SetLineColor(kGreen-7);
    b0->DrawBox(64.6, 0.01*fHTileOR->GetMaximum(), 66.4, 1.01*ymax);
    b0->DrawBox(76.6, 0.01*fHTileOR->GetMaximum(), 78.4, 1.01*ymax);

    TLegend* ANDORLegend = new TLegend(0.18,0.75,0.30,0.88);
    ANDORLegend->SetFillColor(kWhite);
    ANDORLegend->SetTextSize(0.05);
    ANDORLegend->AddEntry(fHTileOR,  "OR", "l");
    ANDORLegend->AddEntry(fHTileAND, "AND", "l");
    ANDORLegend->Draw();

    Canvas->cd(3);
    fHTileAsymmetry->SetTitle("NewCHOD: Asymmetries in tiles, A = (N1#minusN0) / (N1+N0)");
    fHTileAsymmetry->GetXaxis()->SetTitleSize(0.055);
    fHTileAsymmetry->GetXaxis()->SetLabelSize(0.055);
    fHTileAsymmetry->GetYaxis()->SetTitleSize(0.055);
    fHTileAsymmetry->GetYaxis()->SetLabelSize(0.055);
    fHTileAsymmetry->SetMinimum(-1.05);
    fHTileAsymmetry->SetMaximum(+1.05);
    fHTileAsymmetry->SetLineColor(kRed);
    fHTileAsymmetry->SetMarkerColor(kRed);
    fHTileAsymmetry->SetMarkerStyle(21);
    fHTileAsymmetry->SetMarkerSize(0.32);
    fHTileAsymmetryEOB->SetLineColor(kBlue);
    fHTileAsymmetryEOB->SetMarkerColor(kBlue);
    fHTileAsymmetryEOB->SetMarkerStyle(20);
    fHTileAsymmetryEOB->SetMarkerSize(0.32);

    fHTileAsymmetry->Draw("p");
    fHTileAsymmetryEOB->Draw("p same");
    TLegend* Legend = new TLegend(0.18,0.75,0.30,0.88);
    Legend->SetFillColor(kWhite);
    Legend->SetTextSize(0.05);
    Legend->AddEntry(fHTileAsymmetry,    "Readout",      "l");
    Legend->AddEntry(fHTileAsymmetryEOB, "End of burst", "l");
    Legend->Draw();

    Canvas->Print(fOutPDFFileName);
  }
  Text->SetTextAngle(0);

  ///////////////////////////////////////
  // Number of RecoHits vs L0 trigger bit

  TCanvas *CanvasT = new TCanvas("MUV3CanvasT");
  CanvasT->Divide(1,3);
  for (Int_t i=1; i<=3; i++) {
    CanvasT->GetPad(i)->SetLeftMargin(0.04);
    CanvasT->GetPad(i)->SetRightMargin(0.08);
    CanvasT->GetPad(i)->SetTopMargin(0.06);
    CanvasT->GetPad(i)->SetBottomMargin(0.12);
  }

  TH2F *sum = (TH2F*)fHNRecoHitsVsL0TriggerBit->Clone();
  TH2F *rat = (TH2F*)fHNRecoHitsVsL0TriggerBit->Clone();
  sum->Reset();
  rat->Reset();
  sum->Add(fHNRecoHitsVsL0TriggerBit, fHNRecoHitsVsNoL0TriggerBit);
  rat->Divide(fHNRecoHitsVsL0TriggerBit, sum);

  CanvasT->cd(1);
  fHNRecoHitsVsL0TriggerBit->SetTitle("L0 trigger bit vs number of RecoHits: L0 bit ON");
  fHNRecoHitsVsL0TriggerBit->GetXaxis()->SetTitleSize(0.055);
  fHNRecoHitsVsL0TriggerBit->GetYaxis()->SetTitleSize(0.055);
  fHNRecoHitsVsL0TriggerBit->GetXaxis()->SetLabelSize(0.06);
  fHNRecoHitsVsL0TriggerBit->GetYaxis()->SetLabelSize(0.06);
  fHNRecoHitsVsL0TriggerBit->Draw("colz");

  CanvasT->cd(2);
  fHNRecoHitsVsNoL0TriggerBit->SetTitle("L0 trigger bit vs number of RecoHits: L0 bit OFF");
  fHNRecoHitsVsNoL0TriggerBit->GetXaxis()->SetTitleSize(0.055);
  fHNRecoHitsVsNoL0TriggerBit->GetYaxis()->SetTitleSize(0.055);
  fHNRecoHitsVsNoL0TriggerBit->GetXaxis()->SetLabelSize(0.06);
  fHNRecoHitsVsNoL0TriggerBit->GetYaxis()->SetLabelSize(0.06);
  fHNRecoHitsVsNoL0TriggerBit->Draw("colz");

  CanvasT->cd(3);
  rat->SetTitle("Probability of L0 bit being ON vs number of RecoHits (not the trigger efficiency!)");
  rat->SetMinimum(0.0);
  rat->SetMaximum(1.0);
  rat->GetXaxis()->SetTitleSize(0.055);
  rat->GetYaxis()->SetTitleSize(0.055);
  rat->GetXaxis()->SetLabelSize(0.06);
  rat->GetYaxis()->SetLabelSize(0.06);
  rat->Draw("colz");

  CanvasT->Print(fOutPDFFileName, "pdf");

  //////////////////////////////////////////
  // Tight RecoHit tile ID vs L0 trigger bit

  CanvasT->cd(1);
  fHTightRecoHitProfileVsL0TriggerBit->SetTitle("L0 trigger bit vs tight RecoHit tile ID: L0 bit ON");
  fHTightRecoHitProfileVsL0TriggerBit->GetXaxis()->SetTitleSize(0.055);
  fHTightRecoHitProfileVsL0TriggerBit->GetYaxis()->SetTitleSize(0.055);
  fHTightRecoHitProfileVsL0TriggerBit->GetXaxis()->SetLabelSize(0.06);
  fHTightRecoHitProfileVsL0TriggerBit->GetYaxis()->SetLabelSize(0.06);
  fHTightRecoHitProfileVsL0TriggerBit->Draw("colz");

  CanvasT->cd(2);
  fHTightRecoHitProfileVsNoL0TriggerBit->SetTitle("L0 trigger bit vs tight RecoHit tile ID: L0 bit OFF");
  fHTightRecoHitProfileVsNoL0TriggerBit->GetXaxis()->SetTitleSize(0.055);
  fHTightRecoHitProfileVsNoL0TriggerBit->GetYaxis()->SetTitleSize(0.055);
  fHTightRecoHitProfileVsNoL0TriggerBit->GetXaxis()->SetLabelSize(0.06);
  fHTightRecoHitProfileVsNoL0TriggerBit->GetYaxis()->SetLabelSize(0.06);
  fHTightRecoHitProfileVsNoL0TriggerBit->Draw("colz");

  TH2F *sumc = (TH2F*)fHTightRecoHitProfileVsL0TriggerBit->Clone();
  TH2F *ratc = (TH2F*)fHTightRecoHitProfileVsL0TriggerBit->Clone();
  sumc->Reset();
  ratc->Reset();
  sumc->Add(fHTightRecoHitProfileVsL0TriggerBit, fHTightRecoHitProfileVsNoL0TriggerBit);
  ratc->Divide(fHTightRecoHitProfileVsL0TriggerBit, sumc);

  CanvasT->cd(3);
  ratc->SetTitle("Probability of L0 bit being ON vs tight RecoHit tile ID (not the trigger efficiency!)");
  ratc->SetMinimum(0.0);
  ratc->SetMaximum(1.0);
  ratc->GetXaxis()->SetTitleSize(0.055);
  ratc->GetYaxis()->SetTitleSize(0.055);
  ratc->GetXaxis()->SetLabelSize(0.06);
  ratc->GetYaxis()->SetLabelSize(0.06);
  ratc->Draw("colz");

  CanvasT->Print(fOutPDFFileName, "pdf");
  
  ///////////////////////
  // Digi timing plots

  TCanvas *CanvasA = new TCanvas("NewCHODCanvasA");
  CanvasA->Divide(1,2);
  CanvasA->cd(1);
  fTimeWrtReferenceVsReadoutChannelNoT0->SetTitle
    ("NewCHOD Digi #minus Cedar time vs RO channel (no T0 corrections)");
  fTimeWrtReferenceVsReadoutChannelNoT0->GetXaxis()->SetRangeUser(191.5, 511.5);
  fTimeWrtReferenceVsReadoutChannelNoT0->Draw("colz");

  CanvasA->cd(2);
  fTimeWrtReferenceVsReadoutChannel->SetTitle
    ("NewCHOD Digi #minus Cedar time vs RO channel (T0-corrected)");
  fTimeWrtReferenceVsReadoutChannel->GetXaxis()->SetRangeUser(191.5, 511.5);
  fTimeWrtReferenceVsReadoutChannel->Draw("colz");
  CanvasA->Print(fOutPDFFileName, "pdf");

  /////////////////////////////////////////////////
  // RecoHit (main output of the Reco) timing plots

  TCanvas *CanvasAA = new TCanvas("NewCHODCanvasAA");
  CanvasAA->Divide(2,2);
  CanvasAA->cd(1);
  fHTightRecoHitTimeWrtReferenceVsTile->SetTitle("Tight RecoHit #minus Cedar time vs Tile ID");
  fHTightRecoHitTimeWrtReferenceVsTile->Draw("colz");
  CanvasAA->cd(2);
  fHLooseRecoHitTimeWrtReferenceVsTile->SetTitle("Loose RecoHit #minus Cedar time vs Tile ID");
  fHLooseRecoHitTimeWrtReferenceVsTile->Draw("colz");
  CanvasAA->cd(3);
  fHTightRecoHitTimeWrtReference->SetTitle("Tight RecoHit #minus Cedar time");
  fHTightRecoHitTimeWrtReference->Draw("");
  CanvasAA->cd(4);
  fHLooseRecoHitTimeWrtReference->SetTitle("Loose RecoHit #minus Cedar time");
  fHLooseRecoHitTimeWrtReference->Draw("");

  CanvasAA->Print(fOutPDFFileName, "pdf");

  ////////////////////////////
  // 2D colour occupancy plots

  // Set the colour scale of the 2D channel occupancy plots
  Bool_t UniqueColourScale = true; // this can be changed; should be false for the NIM paper plot
  Double_t max0 = fHList[0]->GetBinContent(fHList[0]->GetMaximumBin());
  Double_t max1 = fHList[1]->GetBinContent(fHList[1]->GetMaximumBin());
  if (UniqueColourScale) {
    Double_t max = TMath::Max(max0, max1);
    max0 = max;
    max1 = max;
    fHList[0]->SetMaximum(max);
    fHList[1]->SetMaximum(max);
  }
  else {
    fHList[0]->SetMaximum(max0);
    fHList[1]->SetMaximum(max1);
  }

  TCanvas *CanvasB = new TCanvas("NewCHODCanvasB");
  CanvasB->Divide(2,1);
  for (Int_t i=1; i<=2; i++) {
    CanvasB->GetPad(i)->SetLeftMargin(0.065);
    CanvasB->GetPad(i)->SetRightMargin(0.005);
    CanvasB->GetPad(i)->SetTopMargin(0.167);
    CanvasB->GetPad(i)->SetBottomMargin(0.167);
  }

  CanvasB->cd(1);
  fHList[0]->SetTitle("Digis: Channel map (low channel)");
  fHList[0]->GetXaxis()->SetTitle("X [m]");
  fHList[0]->GetYaxis()->SetTitle("Y [m]");
  fHList[0]->GetXaxis()->SetTitleOffset(0.85);
  fHList[0]->GetYaxis()->SetTitleOffset(0.85);
  fHList[0]->Draw("col");
  DrawBoundaries(1);
  PrintBinContents(fHChannelProfile, 0, 1.0, 999, max0); // print bin contents on top of 2D plot

  CanvasB->cd(2);
  fHList[1]->SetTitle("Digis: Channel map (high channel)");
  fHList[1]->GetXaxis()->SetTitle("X [m]");
  fHList[1]->GetYaxis()->SetTitle("Y [m]");
  fHList[1]->GetXaxis()->SetTitleOffset(0.85);
  fHList[1]->GetYaxis()->SetTitleOffset(0.85);
  fHList[1]->Draw("col");
  DrawBoundaries(1);
  PrintBinContents(fHChannelProfile, 1, 1.0, 999, max1); // print bin contents on top of 2D plot

  CanvasB->Print(fOutPDFFileName, "pdf");

  /////////////////////////////////////////////
  // Readout and EOB channel 2D asymmetry plots

  for (Int_t i=1; i<=2; i++) {
    CanvasB->GetPad(i)->SetLeftMargin(0.07);
    CanvasB->GetPad(i)->SetRightMargin(0.11);
    CanvasB->GetPad(i)->SetTopMargin(0.15);
    CanvasB->GetPad(i)->SetBottomMargin(0.15);
  }

  Name = "Digis: channel asymmetries";
  TH2F *hAsym = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
  ConvertProfileToTwoDimensionalMap(fHTileAsymmetry, hAsym, 0);
  hAsym->GetXaxis()->SetTitle("X [m]");
  hAsym->GetYaxis()->SetTitle("Y [m]");
  hAsym->SetMinimum(-1.0);
  hAsym->SetMaximum(+1.0);

  Name = "EOB: channel asymmetries";
  TH2F *hAsymEOB = new TH2F(Name, Name, 16, -1.072, 1.072, 20, -1.070, 1.070);
  ConvertProfileToTwoDimensionalMap(fHTileAsymmetryEOB, hAsymEOB, 0);
  hAsymEOB->GetXaxis()->SetTitle("X [m]");
  hAsymEOB->GetYaxis()->SetTitle("Y [m]");
  hAsymEOB->SetMinimum(-1.0);
  hAsymEOB->SetMaximum(+1.0);

  CanvasB->cd(1);
  hAsym->Draw("colz");
  DrawBoundaries(1);
  CanvasB->cd(2);
  hAsymEOB->Draw("colz");
  DrawBoundaries(1);

  CanvasB->Print(fOutPDFFileName, "pdf");

  /////////////////////////
  // 2D count-maps, textual

  TCanvas *Canvas2 = new TCanvas("NewCHODCanvas2");
  Canvas2->SetLeftMargin(0.05);
  Canvas2->SetRightMargin(0.01);
  Canvas2->SetTopMargin(0.07);
  Canvas2->SetBottomMargin(0.05);

  TH2F *hBlank = new TH2F("Blank", "Blank", 16, -1.072, 1.072, 20, -1.070, 1.070);

  hBlank->SetTitle("Digis: channel map, low channel [unit=1000]");
  hBlank->Draw("");
  DrawBoundaries(1);
  PrintBinContents(fHChannelProfile, 0, 1e-3, kBlack); // scale factor = 0.001
  Canvas2->Print(fOutPDFFileName, "pdf");

  hBlank->SetTitle("Digis: channel map, high channel [unit=1000]");
  hBlank->Draw("");
  DrawBoundaries(1);
  PrintBinContents(fHChannelProfile, 1, 1e-3, kBlack);
  Canvas2->Print(fOutPDFFileName, "pdf");

  hBlank->SetTitle("EOB channel map, low channel [unit=1000]");
  hBlank->Draw("");
  DrawBoundaries(1);
  PrintBinContents(fHChannelProfileEOB, 0, 1e-3, kBlack); // scale factor = 0.001
  Canvas2->Print(fOutPDFFileName, "pdf");

  hBlank->SetTitle("EOB channel map, high channel [unit=1000]");
  hBlank->Draw("");
  DrawBoundaries(1);
  PrintBinContents(fHChannelProfileEOB, 1, 1e-3, kBlack);
  Canvas2->Print(fOutPDFFileName, "pdf");

  /////////////////////
  // Close the PDF file

  CanvasB->Print(Form(fOutPDFFileName + "]"), "pdf");

  gErrorIgnoreLevel = -1; // restore the default
}

void NewCHODDataQualityPlotter::ConvertProfileToTwoDimensionalMap (TH1D *h1, TH2F *h2, Int_t HighChannel) {
  h2->Reset();
  for (Int_t iq=1; iq<=4; iq++) {
    for (Int_t ic=1; ic<=38; ic++) {
      Int_t Tile = 100*iq+ic;
      Double_t val = h1->GetBinContent(Tile+50*HighChannel-100);
      for (Int_t i=0; i<100; i++) {
	if (fGeo->GetScintMap(i)==Tile%100) {
	  Int_t BrickID = 100*(Tile/100)+i;
	  Double_t x = fGeo->GetBrickCentreX(BrickID);
	  Double_t y = fGeo->GetBrickCentreY(BrickID);
	  h2->Fill(1e-3*x, 1e-3*y, val); // [mm] --> [m]
	}
      }
    }
  }
}

////////////////////////////////////////////////////
// Draw the boundaries of the counters in the 2D map

void NewCHODDataQualityPlotter::DrawBoundaries(Int_t LineWidth) {
  TLine *l = new TLine();
  l->SetLineColor(kBlack); l->SetLineWidth(LineWidth);

  // counter boundaries: 134*107 mm^2 blocks
  for (Int_t iv=-1; iv<=1; iv+=2) {
    for (Int_t i=0; i<6; i++) {
      l->DrawLine(-1.072, iv*i*0.107, 1.072, iv*i*0.107);
    }
    l->DrawLine(-6*0.134, iv*6*0.107, 6*0.134, iv*6*0.107);
    l->DrawLine(  -1.072, iv*7*0.107,   1.072, iv*7*0.107);
    l->DrawLine(-4*0.134, iv*8*0.107, 4*0.134, iv*8*0.107);
    l->DrawLine(-2*0.134, iv*9*0.107, 2*0.134, iv*9*0.107);
  }
  for (Int_t i=-3; i<=3; i++) {
    l->DrawLine(2*i*0.134, -1.072, 2*i*0.134, 1.072);
  }
  for (Int_t ih=-1; ih<=1; ih+=2) {
    l->DrawLine(  ih*0.134,    0.107,   ih*0.134,  4*0.107);
    l->DrawLine(  ih*0.134,   -0.107,   ih*0.134, -4*0.107);
    l->DrawLine(3*ih*0.134, -2*0.107, 3*ih*0.134,  2*0.107);
  }

  // Fill the space outside the outer radius
  Double_t x[12], y[12];
  for (Int_t ih=-1; ih<=1; ih+=2) {
    for (Int_t iv=-1; iv<=1; iv+=2) {
      for (Int_t i=0; i<=10; i++) {
	x[i] = ih*1.070*cos(0.05*i*TMath::Pi());
	y[i] = iv*1.070*sin(0.05*i*TMath::Pi());
      }
      x[11] = 1.070*ih; y[11] = 1.070*iv;
      TGraph *g = new TGraph(12, x, y);
      g->SetFillColor(kWhite);
      g->Draw("f");
    }
  }

  TArc *c = new TArc(0, 0, 0.140);
  c->SetFillColor(kWhite);
  c->SetLineWidth(LineWidth);
  TArc *c1 = new TArc(0, 0, 1.070);
  c1->SetFillStyle(0);
  c1->SetLineWidth(LineWidth);
  c->Draw();
  c1->Draw();
}

//////////////////////////////////////////////////////////////////////////
// Parameters: the colour parameter is either colour ID (kBlack, etc),
// special value (999) meaning "white on blue, black otherwise".
// For this colour scheme, the max limit of the colour scheme is required.

void NewCHODDataQualityPlotter::PrintBinContents
(TH1D *h, Int_t HighChannel, Double_t ScaleFactor, Color_t colour, Double_t max) {

  TText *Text = new TText();
  Text->SetTextAlign(kHAlignCenter+kVAlignCenter);
  Text->SetTextSize(0.02); // 0.02 is standard; 0.025 for the NIM paper
  Text->SetTextColor(colour);
  char s[100];
  for (Int_t iq=1; iq<=4; iq++) {
    for (Int_t ic=1; ic<=38; ic++) {
      Int_t Tile = 100*iq+ic;
      if (colour==999) { // the special label colour scheme
	Double_t val = ScaleFactor * h->GetBinContent(Tile + 50*HighChannel - 100);
	Color_t colour_here = (val < 0.25*ScaleFactor*max) ? kWhite : kBlack;
	Text->SetTextColor(colour_here);
      }
      Double_t val = ScaleFactor * h->GetBinContent(Tile + 50*HighChannel - 100);
      sprintf(s, "%4.2f", val);
      Text->DrawText(1e-3*fGeo->GetTileCentreX(Tile), 1e-3*fGeo->GetTileCentreY(Tile), s);
    }
  }
  delete Text;
}

void NewCHODDataQualityPlotter::FillOccupancyPlotWithChannelIDs(TH1D *h) {
  for (Int_t i=0; i<=h->GetNbinsX(); i++) h->SetBinContent(i, h->GetBinCenter(i));
}
