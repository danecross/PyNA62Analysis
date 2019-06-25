// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-26
//
// ---------------------------------------------------------------

/// \class MUV3DataQualityPlotter
/// \Brief
/// Build a PDF report with MUV3 monitoring histograms
/// \EndBrief
/// \Detailed
/// Called by NA62Analysis/MUV3DataQualityPlotter. A possibility of being called by
/// MUV3OnlineMonitor is foreseen, to produce PDF reports online.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "MUV3DataQualityPlotter.hh"

using namespace std;

MUV3DataQualityPlotter::MUV3DataQualityPlotter(std::vector<TH1*> Input, TString OutputPDFFileName) :
  fOutPDFFileName(OutputPDFFileName),
  fHTileAsymmetry(nullptr),
  fHTileAsymmetryEOB(nullptr),
  fHAsym2(nullptr),
  fHAsym2Inner(nullptr),
  fHAsym2EOB(nullptr),
  fHAsym2InnerEOB(nullptr)
{
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetPalette(1);

  if (!Input.size()) return;

  //////////////////
  // Read histograms

  UInt_t Index = 0;
  fHNEventsProcessedPerBurst                    = (TH1D*)Input[Index++];
  fHChannelProfile                              = (TH1D*)Input[Index++];
  fHChannelProfileEOB                           = (TH1D*)Input[Index++];
  fHTileOR                                      = (TH1D*)Input[Index++];
  fHTileAND                                     = (TH1D*)Input[Index++];
  fHNCandidatesVsL0TriggerBit                   = (TH2F*)Input[Index++];
  fHNCandidatesVsNoL0TriggerBit                 = (TH2F*)Input[Index++];
  fHTightCandidateProfileVsL0TriggerBit         = (TH2F*)Input[Index++];
  fHTightCandidateProfileVsNoL0TriggerBit       = (TH2F*)Input[Index++];
  fHCandidateTimeWrtReferenceNoTileT0           = (TH1D*)Input[Index++];
  fHCandidateTimeWrtReference                   = (TH1D*)Input[Index++];
  fHCandidateAvgTimeWrtReference                = (TH1D*)Input[Index++];
  fHTotalPrimitiveCountsEOB                     = (TH1D*)Input[Index++];
  fHErrorCountsEOB                              = (TH1D*)Input[Index++];
  fHChannelProfileVsBurst                       = (TH2F*)Input[Index++];
  fHChannelProfileVsBurstEOB                    = (TH2F*)Input[Index++];
  fHRecoHitTimeWrtReferenceVsReadoutChannelNoT0 = (TH2F*)Input[Index++];
  fHRecoHitTimeWrtReferenceVsReadoutChannel     = (TH2F*)Input[Index++];
  fHCandidateTimeWrtReferenceNoTileT0VsTile     = (TH2F*)Input[Index++];
  fHCandidateTimeWrtReferenceVsTile             = (TH2F*)Input[Index++];
  fHCandidateAvgTimeWrtReferenceVsTile          = (TH2F*)Input[Index++];
  fHList.resize(18);
  for (Int_t i=0; i<18; i++) fHList[i] = (TH2F*)Input[Index++];

  fHCandidateAvgTimeWrtReferenceVsTile->SetTitle
    ("Tight MUV3 Candidate #minus Cedar time vs Tile ID: average hit time");
  fHCandidateTimeWrtReferenceNoTileT0VsTile->SetTitle
    ("Tight MUV3 Candidate #minus Cedar time vs Tile ID: latest hit time");
  fHCandidateTimeWrtReferenceVsTile->SetTitle
    ("Tight MUV3 Candidate #minus Cedar time vs Tile ID: latest, tile T0 corrected");
  fHCandidateAvgTimeWrtReference->SetTitle
    ("Tight MUV3 candidate time #minus Cedar time");
}

void MUV3DataQualityPlotter::BuildPDF() {

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

  // Build tile asymmetries
  TString Name = "MUV3TileAsymmetry";
  fHTileAsymmetry = new TH1D(Name, Name, 152, -0.5, 151.5);
  Name = "MUV3TileAsymmetryEOB";
  fHTileAsymmetryEOB = new TH1D(Name, Name, 152, -0.5, 151.5);

  GenerateTileAsymmetry(fHChannelProfile, fHTileAsymmetry);
  GenerateTileAsymmetry(fHChannelProfileEOB, fHTileAsymmetryEOB);

  fHAsym2         = (TH2F*)fHList[0]->Clone();
  fHAsym2Inner    = (TH2F*)fHList[1]->Clone();
  fHAsym2EOB      = (TH2F*)fHList[0]->Clone();
  fHAsym2InnerEOB = (TH2F*)fHList[1]->Clone();

  ConvertProfileToTwoDimensionalMap(fHTileAsymmetry, fHAsym2, fHAsym2Inner);
  ConvertProfileToTwoDimensionalMap(fHTileAsymmetryEOB, fHAsym2EOB, fHAsym2InnerEOB);

  gStyle->SetOptStat(0);

  //////////////////////////////////////////////////
  // Channel profile and numbers of OR, AND in tiles

  TCanvas *Canvas = new TCanvas("MUV3Canvas1");
  Canvas->Divide(1,3);
  TLatex *Text = new TLatex();

  for (Int_t i=1; i<=3; i++) {
    Canvas->GetPad(i)->SetLeftMargin(0.04);
    Canvas->GetPad(i)->SetRightMargin(0.01);
    Canvas->GetPad(i)->SetTopMargin(0.06);
    Canvas->GetPad(i)->SetBottomMargin(0.10);
  }

  Canvas->cd(1);
  fHChannelProfile->SetTitle("MUV3 channel profile");
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
    Text->DrawText(10, 0.92*gPad->GetUymax(), Form("Bursts processed: %d", NBurstsProcessed));
    Text->DrawText(10, 0.80*gPad->GetUymax(), Form("Events processed: %d", NEventsProcessed));
    Text->DrawText(10, 0.68*gPad->GetUymax(), Form("Digis: %d", (Int_t)fHChannelProfile->Integral()));
  }

  Canvas->cd(2);
  Double_t ymax = fHTileOR->GetMaximum();
  fHTileOR->SetTitle("MUV3: OR, AND of signals in tiles");
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

  TLegend* ANDORLegend = new TLegend(0.20,0.75,0.35,0.88);
  ANDORLegend->SetFillColor(kWhite);
  ANDORLegend->SetTextSize(0.05);
  ANDORLegend->AddEntry(fHTileOR,  "OR", "l");
  ANDORLegend->AddEntry(fHTileAND, "AND", "l");
  ANDORLegend->Draw();

  Canvas->cd(3);
  fHTileAsymmetry->SetTitle("MUV3: Asymmetries in tiles, A = (N1#minusN0) / (N1+N0). Red=Readout, Blue=EoB.");
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

  // Mark the non-existing tiles
  b0->DrawBox(64.6, -1.03, 66.4, 1.03);
  b0->DrawBox(76.6, -1.03, 78.4, 1.03);

  Text->SetTextSize(0.05);
  Text->SetTextAngle(90);
  Text->SetTextColor(kGreen+2);
  Text->SetTextAlign(kHAlignCenter+kVAlignCenter);
  for (Int_t i=1; i<=152; i++) {
    Int_t iTile = fHTileAsymmetry->GetBinCenter(i);
    Double_t Asym = fHTileAsymmetry->GetBinContent(i);
    if (fabs(Asym)>0.6 &&
	iTile!=65 && iTile!=66 && iTile!=77 && iTile!=78) { // these tiles do not exist
      Double_t y = (Asym>0) ? 0.5 : -0.5;
      Text->DrawText(iTile, y, Form("%d", iTile));
    }
  }
  Canvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open file and print
  Text->SetTextAngle(0);

  /////////////////////////////////////////
  // Number of candidates vs L0 trigger bit

  TCanvas *CanvasT = new TCanvas("MUV3CanvasT");
  CanvasT->Divide(1,3);
  for (Int_t i=1; i<=3; i++) {
    CanvasT->GetPad(i)->SetLeftMargin(0.04);
    CanvasT->GetPad(i)->SetRightMargin(0.08);
    CanvasT->GetPad(i)->SetTopMargin(0.06);
    CanvasT->GetPad(i)->SetBottomMargin(0.12);
  }

  TH2F *sum = (TH2F*)fHNCandidatesVsL0TriggerBit->Clone();
  TH2F *rat = (TH2F*)fHNCandidatesVsL0TriggerBit->Clone();
  sum->Reset();
  rat->Reset();
  sum->Add(fHNCandidatesVsL0TriggerBit, fHNCandidatesVsNoL0TriggerBit);
  rat->Divide(fHNCandidatesVsL0TriggerBit, sum);

  CanvasT->cd(1);
  fHNCandidatesVsL0TriggerBit->SetTitle("L0 trigger bit vs number of candidates: L0 bit ON");
  fHNCandidatesVsL0TriggerBit->GetXaxis()->SetTitleSize(0.055);
  fHNCandidatesVsL0TriggerBit->GetYaxis()->SetTitleSize(0.055);
  fHNCandidatesVsL0TriggerBit->GetXaxis()->SetLabelSize(0.06);
  fHNCandidatesVsL0TriggerBit->GetYaxis()->SetLabelSize(0.06);
  fHNCandidatesVsL0TriggerBit->Draw("colz");

  CanvasT->cd(2);
  fHNCandidatesVsNoL0TriggerBit->SetTitle("L0 trigger bit vs number of candidates: L0 bit OFF");
  fHNCandidatesVsNoL0TriggerBit->GetXaxis()->SetTitleSize(0.055);
  fHNCandidatesVsNoL0TriggerBit->GetYaxis()->SetTitleSize(0.055);
  fHNCandidatesVsNoL0TriggerBit->GetXaxis()->SetLabelSize(0.06);
  fHNCandidatesVsNoL0TriggerBit->GetYaxis()->SetLabelSize(0.06);
  fHNCandidatesVsNoL0TriggerBit->Draw("colz");

  CanvasT->cd(3);
  rat->SetTitle("Probability of L0 bit being ON vs number of candidates (not the trigger efficiency!)");
  rat->SetMinimum(0.0);
  rat->SetMaximum(1.0);
  rat->GetXaxis()->SetTitleSize(0.055);
  rat->GetYaxis()->SetTitleSize(0.055);
  rat->GetXaxis()->SetLabelSize(0.06);
  rat->GetYaxis()->SetLabelSize(0.06);
  rat->Draw("colz");

  CanvasT->Print(fOutPDFFileName, "pdf");

  ////////////////////////////////////////////
  // Tight candidate tile ID vs L0 trigger bit

  CanvasT->cd(1);
  fHTightCandidateProfileVsL0TriggerBit->SetTitle("L0 trigger bit vs tight candidate tile ID: L0 bit ON");
  fHTightCandidateProfileVsL0TriggerBit->GetXaxis()->SetTitleSize(0.055);
  fHTightCandidateProfileVsL0TriggerBit->GetYaxis()->SetTitleSize(0.055);
  fHTightCandidateProfileVsL0TriggerBit->GetXaxis()->SetLabelSize(0.06);
  fHTightCandidateProfileVsL0TriggerBit->GetYaxis()->SetLabelSize(0.06);
  fHTightCandidateProfileVsL0TriggerBit->Draw("colz");

  CanvasT->cd(2);
  fHTightCandidateProfileVsNoL0TriggerBit->SetTitle("L0 trigger bit vs tight candidate tile ID: L0 bit OFF");
  fHTightCandidateProfileVsNoL0TriggerBit->GetXaxis()->SetTitleSize(0.055);
  fHTightCandidateProfileVsNoL0TriggerBit->GetYaxis()->SetTitleSize(0.055);
  fHTightCandidateProfileVsNoL0TriggerBit->GetXaxis()->SetLabelSize(0.06);
  fHTightCandidateProfileVsNoL0TriggerBit->GetYaxis()->SetLabelSize(0.06);
  fHTightCandidateProfileVsNoL0TriggerBit->Draw("colz");

  TH2F *sumc = (TH2F*)fHTightCandidateProfileVsL0TriggerBit->Clone();
  TH2F *ratc = (TH2F*)fHTightCandidateProfileVsL0TriggerBit->Clone();
  sumc->Reset();
  ratc->Reset();
  sumc->Add(fHTightCandidateProfileVsL0TriggerBit, fHTightCandidateProfileVsNoL0TriggerBit);
  ratc->Divide(fHTightCandidateProfileVsL0TriggerBit, sumc);

  CanvasT->cd(3);
  ratc->SetTitle("Probability of L0 bit being ON vs tight candidate tile ID (not the trigger efficiency!)");
  ratc->SetMinimum(0.0);
  ratc->SetMaximum(1.0);
  ratc->GetXaxis()->SetTitleSize(0.055);
  ratc->GetYaxis()->SetTitleSize(0.055);
  ratc->GetXaxis()->SetLabelSize(0.06);
  ratc->GetYaxis()->SetLabelSize(0.06);
  ratc->Draw("colz");

  CanvasT->Print(fOutPDFFileName, "pdf");

  ///////////////////////
  // Channel timing plots

  TCanvas *CanvasA = new TCanvas("MUV3CanvasA");
  CanvasA->Divide(1,2);
  CanvasA->cd(1);
  fHRecoHitTimeWrtReferenceVsReadoutChannelNoT0->Draw("colz");
  CanvasA->cd(2);
  fHRecoHitTimeWrtReferenceVsReadoutChannel->Draw("colz");
  CanvasA->Print(fOutPDFFileName, "pdf");

  /////////////////////////
  // Candidate timing plots

  TCanvas *CanvasB = new TCanvas("MUV3CanvasB");
  CanvasB->Divide(2,2,0.001,0.001);
  for (Int_t i=1; i<=4; i++) {
    CanvasB->GetPad(i)->SetLeftMargin(0.04);
    CanvasB->GetPad(i)->SetRightMargin(0.10);
    CanvasB->GetPad(i)->SetTopMargin(0.06);
    CanvasB->GetPad(i)->SetBottomMargin(0.10);
  }
  CanvasB->GetPad(4)->SetLeftMargin(0.06);
  CanvasB->GetPad(4)->SetRightMargin(0.04);

  CanvasB->cd(1);
  fHCandidateAvgTimeWrtReferenceVsTile->Draw("colz");
  CanvasB->cd(2);
  fHCandidateTimeWrtReferenceNoTileT0VsTile->Draw("colz");
  CanvasB->cd(3);
  fHCandidateTimeWrtReferenceVsTile->Draw("colz");

  CanvasB->cd(4);
  gPad->SetGridx();
  gPad->SetGridy();
  fHCandidateTimeWrtReference->SetLineColor(kBlue);
  fHCandidateAvgTimeWrtReference->SetLineColor(kRed);
  fHCandidateTimeWrtReferenceNoTileT0->SetLineColor(kGreen+2);
  fHCandidateAvgTimeWrtReference->SetMarkerColor(kRed);
  fHCandidateTimeWrtReference->SetMarkerColor(kBlue);
  fHCandidateTimeWrtReferenceNoTileT0->SetMarkerColor(kGreen+2);

  Double_t max0 =
    TMath::Max(fHCandidateTimeWrtReference->GetBinContent(fHCandidateTimeWrtReference->GetMaximumBin()),
	       fHCandidateAvgTimeWrtReference->GetBinContent(fHCandidateAvgTimeWrtReference->GetMaximumBin()));
  Double_t max1 =
    TMath::Max(fHCandidateTimeWrtReference->GetBinContent(fHCandidateTimeWrtReference->GetMaximumBin()),
	       fHCandidateTimeWrtReferenceNoTileT0->GetBinContent(fHCandidateTimeWrtReferenceNoTileT0->GetMaximumBin()));
  Double_t max = TMath::Max(max0, max1);

  fHCandidateTimeWrtReference->GetXaxis()->SetRangeUser(-3,3);
  fHCandidateTimeWrtReference->SetMaximum(1.05*max);
  fHCandidateTimeWrtReference->SetMinimum(0.0);
  fHCandidateTimeWrtReference->Draw();
  fHCandidateAvgTimeWrtReference->Draw("same");
  fHCandidateTimeWrtReferenceNoTileT0->Draw("same");
  TLegend *Legend = new TLegend(0.64,0.77,0.94,0.90);
  Legend->AddEntry(fHCandidateAvgTimeWrtReference,      "Average hit time", "l");
  Legend->AddEntry(fHCandidateTimeWrtReferenceNoTileT0, "Latest hit time", "l");
  Legend->AddEntry(fHCandidateTimeWrtReference,         "Latest (tile T0 corrected)", "l");
  Legend->Draw();

  CanvasB->Print(fOutPDFFileName, "pdf");

  ////////////////////////////
  // 2D colour occupancy plots

  // Set the colour scale of the 2D channel occupancy plots
  max0 = 0.5 *
    TMath::Max(fHList[0]->GetBinContent(fHList[0]->GetMaximumBin()),
	       fHList[1]->GetBinContent(fHList[1]->GetMaximumBin()));
  max1 = 0.5 *
    TMath::Max(fHList[2]->GetBinContent(fHList[2]->GetMaximumBin()),
	       fHList[3]->GetBinContent(fHList[3]->GetMaximumBin()));
  max = TMath::Max(max0, max1);
  fHList[0]->SetMaximum(max);
  fHList[1]->SetMaximum(max);
  fHList[2]->SetMaximum(max);
  fHList[3]->SetMaximum(max);

  TCanvas *CanvasC = new TCanvas("MUV3CanvasC");
  CanvasC->Divide(2,1);
  for (Int_t i=1; i<=2; i++) {
    CanvasC->GetPad(i)->SetLeftMargin(0.06);
    CanvasC->GetPad(i)->SetRightMargin(0.11);
    CanvasC->GetPad(i)->SetTopMargin(0.17);
    CanvasC->GetPad(i)->SetBottomMargin(0.17);
  }

  CanvasC->cd(1);
  fHList[0]->SetTitle("Digis: Channel map (low channel)");
  fHList[0]->GetXaxis()->SetTitle("");
  fHList[0]->GetYaxis()->SetTitle("");
  fHList[0]->Draw("col");
  fHList[1]->Draw("col same");
  DrawBoundaries(1);

  Text->SetTextSize(0.035);
  Text->SetTextColor(kWhite);
  Text->SetTextAlign(kHAlignCenter+kVAlignCenter);
  for (Int_t ix=1; ix<=12; ix++) {
    for (Int_t iy=1; iy<=12; iy++) {
      if (fHAsym2->GetBinContent(ix, iy)>0.5) {
	Double_t xc = fHList[0]->GetXaxis()->GetBinCenter(ix);
	Double_t yc = fHList[0]->GetYaxis()->GetBinCenter(iy);
	Text->DrawLatex(xc, yc, "#Psi");
      }
    }
  }

  CanvasC->cd(2);
  fHList[2]->SetTitle("Digis: Channel map (high channel)");
  fHList[2]->GetXaxis()->SetTitle("");
  fHList[2]->GetYaxis()->SetTitle("");
  fHList[2]->Draw("col");
  fHList[3]->Draw("col same");
  DrawBoundaries(1);

  for (Int_t ix=1; ix<=12; ix++) {
    for (Int_t iy=1; iy<=12; iy++) {
      if (fHAsym2->GetBinContent(ix, iy)<-0.5) {
	Double_t xc = fHList[0]->GetXaxis()->GetBinCenter(ix);
	Double_t yc = fHList[0]->GetYaxis()->GetBinCenter(iy);
	Text->DrawLatex(xc, yc, "#Psi");
      }
    }
  }

  CanvasC->Print(fOutPDFFileName, "pdf");

  /////////////////////////////////////////////
  // Readout and EOB channel hit asymmetry plots

  CanvasC->cd(1);
  fHAsym2->SetTitle("Digis: channel asymmetries");
  fHAsym2->SetMinimum(-1.0);
  fHAsym2->SetMaximum(+1.0);
  fHAsym2Inner->SetMinimum(-1.0);
  fHAsym2Inner->SetMaximum(+1.0);
  fHAsym2->Draw("colz");
  fHAsym2Inner->Draw("col same");
  DrawBoundaries(1);

  CanvasC->cd(2);
  fHAsym2EOB->SetTitle("EOB: channel asymmetries");
  fHAsym2EOB->SetMinimum(-1.0);
  fHAsym2EOB->SetMaximum(+1.0);
  fHAsym2InnerEOB->SetMinimum(-1.0);
  fHAsym2InnerEOB->SetMaximum(+1.0);
  fHAsym2EOB->Draw("colz");
  fHAsym2InnerEOB->Draw("col same");
  DrawBoundaries(1);

  CanvasC->Print(fOutPDFFileName, "pdf");

  /////////////////////////
  // 2D count-maps, textual

  TCanvas *Canvas2 = new TCanvas("MUV3Canvas2");
  Canvas2->SetLeftMargin(0.05);
  Canvas2->SetRightMargin(0.01);
  Canvas2->SetTopMargin(0.07);
  Canvas2->SetBottomMargin(0.05);

  for (UInt_t i=0; i<fHList.size(); i++) {
    if (!fHList[i]) {
      std::cout << " WARNING: histo number " << i << " not found." << std::endl;
      std::cout << " WARNING: 2D count maps will not be produced." << std::endl;
      return;
    }
  }

  fHList [4]->Add(fHList [2], fHList [0], 1, -1);
  fHList [5]->Add(fHList [3], fHList [1], 1, -1);
  fHList[14]->Add(fHList[12], fHList[10], 1, -1);
  fHList[15]->Add(fHList[13], fHList[11], 1, -1);

  vector <TString> titles(9, TString());
  titles [0] = "N(Digis, low PMT)";
  titles [1] = "N(Digis, high PMT)";
  titles [2] = "N(Digis), High#minusLow and A=(High#minusLow)/(High+Low)";
  titles [3] = "N(Candidates)";
  titles [4] = "void"; // tight candidates are not plotted separately
  titles [5] = "EOB: N(hits, low PMT)";
  titles [6] = "EOB: N(hits, high PMT)";
  titles [7] = "EOB: N(hits), High#minusLow and A=(High#minusLow)/(High+Low)";
  titles [8] = "N(EOB L0FW hits)";

  TBox *b = new TBox();
  b->SetFillStyle(0);
  b->SetLineColor(kBlack);
  gStyle->SetPaintTextFormat(".0f");

  for (UInt_t i=0; i<fHList.size(); i++) {
    if (i==8 || i==9) continue; // do not plot tight candidates separately
    fHList[i]->SetMarkerSize(0.8);
    fHList[i]->GetXaxis()->SetTitle("");
    fHList[i]->GetYaxis()->SetTitle("");

    if (i%2==0) { // Outer tiles

      // Display integrals in title
      if (i==0 || i==2 || i==6) {
	titles[i/2] += Form(". Inner: %d, outer: %d",
			    (int)fHList[i+1]->Integral(), (int)fHList[i]->Integral());
      }
      if (i==10 || i==12 || i==16) {
	titles[i/2] += Form(". Inner: %4.2fM, outer: %4.2fM",
			    1e-6*fHList[i+1]->Integral(), 1e-6*fHList[i]->Integral());
      }

      Canvas2->Clear();
      fHList[i]->SetTitle(titles[i/2]);
      fHList[i]->GetXaxis()->SetTickLength(0);
      fHList[i]->GetYaxis()->SetTickLength(0);
      fHList[i]->Draw("TEXT20");

      // Label the outer tiles
      for (Int_t it=1; it<=12; it++) {
	Double_t xc = fHList[0]->GetXaxis()->GetBinCenter(it);
	for (Int_t jt=1; jt<=12; jt++) {
	  Double_t yc = fHList[0]->GetYaxis()->GetBinCenter(jt);
	  Int_t TileID = it-1+(jt-1)*12;
	  if (TileID==65 || TileID==66 || TileID==77 || TileID==78) continue;
	  b->SetLineColor(kBlack);
	  b->DrawBox(xc-0.11, yc-0.11, xc+0.11, yc+0.11);
	  Text->SetTextSize(0.02);
	  Text->SetTextColor(kGreen+2);
	  Text->SetTextAlign(kHAlignLeft+kVAlignTop);
	  Text->DrawText(xc-0.11, yc+0.11, Form("%d", TileID));
	}
      }

      if (i==4 || i==14) { // Print out asymmetries
	for (Int_t it=1; it<=12; it++) {
	  Double_t xc = fHList[i]->GetXaxis()->GetBinCenter(it);
	  for (Int_t jt=1; jt<=12; jt++) {
	    Double_t yc = fHList[i]->GetYaxis()->GetBinCenter(jt);
	    Double_t Nhigh = fHList[i-2]->GetBinContent(fHList[i-2]->GetBin(it,jt));
	    Double_t Nlow  = fHList[i-4]->GetBinContent(fHList[i-4]->GetBin(it,jt));
	    if (!(Nhigh+Nlow)) continue;
	    Double_t Asym = (Nhigh-Nlow) / (Nhigh+Nlow);
	    Color_t color = kBlack;
	    if (Asym<-0.5) color = kBlue;
	    if (Asym>+0.5) color = kRed;
	    Text->SetTextSize(0.018);
	    Text->SetTextColor(color);
	    Text->SetTextAlign(kHAlignRight+kVAlignBottom);
	    Text->DrawText(xc+0.10, yc-0.10, Form("A=%5.3f", Asym));
	  }
	}
      }

      if (i==6) { // Label tiles with loose candidates
	for (Int_t it=1; it<=12; it++) {
	  Double_t xc = fHList[6]->GetXaxis()->GetBinCenter(it);
	  for (Int_t jt=1; jt<=12; jt++) {
	    Double_t yc = fHList[6]->GetYaxis()->GetBinCenter(jt);
	    Double_t Nall   = fHList[6]->GetBinContent(fHList[6]->GetBin(it,jt));
	    Double_t Ntight = fHList[8]->GetBinContent(fHList[8]->GetBin(it,jt));
	    if (Nall>0 && !Ntight) { // loose candidates are generated
	      Text->SetTextSize(0.020);
	      Text->SetTextColor(kRed);
	      Text->SetTextAlign(kHAlignRight+kVAlignBottom);
	      Text->DrawText(xc+0.10, yc-0.10, "LOOSE");
	    }
	  }
	}
      }
    }

    else { // Inner tiles
      fHList[i]->Draw("TEXT20SAME");

      // Label the inner tiles
      for (Int_t it=1; it<=3; it++) {
	Double_t xc = fHList[1]->GetXaxis()->GetBinCenter(it);
	for (Int_t jt=1; jt<=3; jt++) {
	  Double_t yc = fHList[1]->GetYaxis()->GetBinCenter(jt);
	  if (it==2 && jt==2) continue;
	  Int_t TileID = 144+(it-1)+(jt-1)*3;
	  if (TileID>=149) TileID--;
	  b->SetLineColor(kBlack);
	  b->DrawBox(xc-0.22/3.0, yc-0.22/3.0, xc+0.22/3.0, yc+0.22/3.0);
	  Text->SetTextSize(0.018);
	  Text->SetTextColor(kGreen+2);
	  Text->SetTextAlign(kHAlignLeft+kVAlignTop);
	  Text->DrawText(xc-0.22/3.0, yc+0.22/3.0, Form("%d", TileID));
	}
      }

      if (i==5 || i==15) { // PrInt_t out asymmetries
	for (Int_t it=1; it<=3; it++) {
	  Double_t xc = fHList[5]->GetXaxis()->GetBinCenter(it);
	  for (Int_t jt=1; jt<=3; jt++) {
	    if (it==2 && jt==2) continue;
	    Double_t yc = fHList[5]->GetYaxis()->GetBinCenter(jt);
	    Double_t Nhigh = fHList[i-2]->GetBinContent(fHList[i-2]->GetBin(it,jt));
	    Double_t Nlow  = fHList[i-4]->GetBinContent(fHList[i-4]->GetBin(it,jt));
	    if (!(Nhigh+Nlow)) continue;
	    Double_t Asym = (Nhigh-Nlow) / (Nhigh+Nlow);
	    Color_t color = kBlack;
	    if (Asym<-0.5) color = kBlue;
	    if (Asym>+0.5) color = kRed;
	    Text->SetTextSize(0.015);
	    Text->SetTextColor(color);
	    Text->SetTextAlign(kHAlignRight+kVAlignBottom);
	    Text->DrawText(xc+0.07, yc-0.07, Form("A=%5.3f", Asym));
	  }
	}
      }

      if (i==7) { // Label tiles with loose candidates
	for (Int_t it=1; it<=3; it++) {
	  Double_t xc = fHList[7]->GetXaxis()->GetBinCenter(it);
	  for (Int_t jt=1; jt<=3; jt++) {
	    if (it==2 && jt==2) continue;
	    Double_t yc = fHList[7]->GetYaxis()->GetBinCenter(jt);
	    Double_t Nall   = fHList[7]->GetBinContent(fHList[7]->GetBin(it,jt));
	    Double_t Ntight = fHList[9]->GetBinContent(fHList[9]->GetBin(it,jt));
	    if (Nall>0 && !Ntight) { // loose candidates are generated
	      Text->SetTextSize(0.017);
	      Text->SetTextColor(kRed);
	      Text->SetTextAlign(kHAlignRight+kVAlignBottom);
	      Text->DrawText(xc+0.07, yc-0.07, "LOOSE");
	    }
	  }
	}
      }
      Canvas2->Print(fOutPDFFileName, "pdf");
    }
  }

  //////////////////////////////////////
  // EOB primitive type and error counts

  if (fHTotalPrimitiveCountsEOB && fHErrorCountsEOB) {
    TCanvas *Canvas3 = new TCanvas("MUV3Canvas3");
    Canvas3->Divide(2,1);
    Canvas3->SetLeftMargin(0.01);
    Canvas3->SetRightMargin(0.01);
    Canvas3->SetTopMargin(0.01);
    Canvas3->SetBottomMargin(0.01);
    for (Int_t i=1; i<=2; i++) {
      Canvas3->GetPad(i)->SetLeftMargin(0.08);
      Canvas3->GetPad(i)->SetRightMargin(0.01);
      Canvas3->GetPad(i)->SetTopMargin(0.06);
      Canvas3->GetPad(i)->SetBottomMargin(0.05);
    }

    Canvas3->cd(1);
    fHTotalPrimitiveCountsEOB->SetTitle("EOB primitive counts by type [mln]");
    fHTotalPrimitiveCountsEOB->GetXaxis()->SetTitle("Primitive type");
    fHTotalPrimitiveCountsEOB->GetXaxis()->SetLabelSize(0); // suppress labels
    fHTotalPrimitiveCountsEOB->GetXaxis()->SetTitleOffset(0.4);
    fHTotalPrimitiveCountsEOB->SetLineWidth(1);
    fHTotalPrimitiveCountsEOB->SetLineColor(kBlue);
    fHTotalPrimitiveCountsEOB->SetFillColor(kYellow);
    fHTotalPrimitiveCountsEOB->Draw();

    Text->SetTextSize(0.04);
    Text->SetTextAngle(90);
    Text->SetTextAlign(kHAlignCenter+kVAlignCenter);
    Text->SetTextColor(kGreen+2);
    for (Int_t i=0; i<16; i++) {
      Text->DrawText(i, 0.5*fHTotalPrimitiveCountsEOB->GetMaximum(),
		     fHTotalPrimitiveCountsEOB->GetXaxis()->GetBinLabel(i+1));
    }
    TLine *l = new TLine();
    l->SetLineColor(kRed);
    Canvas3->Update();
    l->DrawLine( 9.5, 0,  9.5, gPad->GetUymax());
    l->DrawLine(13.5, 0, 13.5, gPad->GetUymax());

    Canvas3->cd(2);
    fHErrorCountsEOB->SetTitle("SL EOB error counts by type");
    fHErrorCountsEOB->GetXaxis()->SetTitle("Error type");
    fHErrorCountsEOB->GetXaxis()->SetLabelSize(0); // suppress labels
    fHErrorCountsEOB->GetXaxis()->SetTitleOffset(0.4);
    fHErrorCountsEOB->SetLineWidth(1);
    fHErrorCountsEOB->SetLineColor(kBlue);
    fHErrorCountsEOB->SetFillColor(kYellow);
    fHErrorCountsEOB->Draw();
    for (Int_t i=0; i<12; i++) {
      Text->DrawText(i, 0.5*fHErrorCountsEOB->GetMaximum(),
		     fHErrorCountsEOB->GetXaxis()->GetBinLabel(i+1));
    }
    Text->SetTextAngle(0);

    Canvas3->Print(fOutPDFFileName, "pdf");
  }

  /////////////////////////////////////////////////
  // Tile asymmetry vs burst ID plots for each tile
  GenerateTileAsymmetryVsBurstIDPlots();

  Canvas->Print(Form(fOutPDFFileName + "]"), "pdf"); // close file
  gErrorIgnoreLevel = -1; // restore the default
}

void MUV3DataQualityPlotter::GenerateTileAsymmetry(TH1D *hIn, TH1D* hOut) {
  for (Int_t i=1; i<=152; i++) {
    Double_t x    = hIn->GetBinContent(i+200);
    Double_t y    = hIn->GetBinContent(i);
    if (x+y<1) continue;
    Double_t dx   = sqrt(x);
    Double_t dy   = sqrt(y);
    Double_t f    = (x-y)/(x+y);
    Double_t dfdx = 2.0*y/(x+y)/(x+y);
    Double_t dfdy = 2.0*x/(x+y)/(x+y);
    Double_t df   = sqrt((dfdx*dx)*(dfdx*dx)+(dfdy*dy)*(dfdy*dy));
    hOut->SetBinContent(i, f);
    hOut->SetBinError(i, df);
  }
}

void MUV3DataQualityPlotter::GenerateTileAsymmetryVsBurstIDPlots() {

  if (!fHChannelProfileVsBurst->Integral()) return;

  Int_t FirstBurst = 0;
  while (!fHChannelProfileVsBurst->Integral(FirstBurst+1, FirstBurst+1)) FirstBurst++;
  Int_t LastBurst = fHChannelProfileVsBurst->GetNbinsX();
  while (!fHChannelProfileVsBurst->Integral(LastBurst+1, LastBurst+1)) LastBurst--;

  Int_t Ntiles = 152;
  Int_t Npages = Ntiles/16;
  if (Ntiles%16) Npages++;

  TCanvas* Canvas = new TCanvas("MUV3Canvas");
  Canvas->Divide(4,4);
  for (Int_t i=1; i<=16; i++) {
    Canvas->GetPad(i)->SetLeftMargin(0.04);
    Canvas->GetPad(i)->SetRightMargin(0.0);
    Canvas->GetPad(i)->SetTopMargin(0.0);
    Canvas->GetPad(i)->SetBottomMargin(0.03);
  }

  std::vector<TH1F*> hAsym1(Ntiles), hAsym2(Ntiles);

  for (Int_t ipage=0; ipage<Npages; ipage++) {
    for (Int_t ipad=0; ipad<16; ipad++) {
      Canvas->GetPad(ipad+1)->Clear();
      Int_t itile = ipage*16 + ipad;
      if (itile>=Ntiles) continue;
      Canvas->cd(ipad+1);

      // TDC event monitor
      TString Name = Form("Tile %03d asymmetry vs burst ID", itile);
      hAsym1[itile] = new TH1F(Name, Name, 3000, -0.5, 2999.5);

      for (Int_t i=FirstBurst; i<=LastBurst; i++) {
	Double_t x = fHChannelProfileVsBurst->GetBinContent(i+1, itile+201);
	Double_t y = fHChannelProfileVsBurst->GetBinContent(i+1, itile+1);
	if (x+y<1) {
	  hAsym1[itile]->SetBinContent(i+1, 999);
	  hAsym1[itile]->SetBinError(i+1, 0);
	  continue;
	}
	Double_t dx   = sqrt(x);
	Double_t dy   = sqrt(y);
	Double_t f    = (x-y)/(x+y);
	Double_t dfdx = 2.0*y/(x+y)/(x+y);
	Double_t dfdy = 2.0*x/(x+y)/(x+y);
	Double_t df   = sqrt((dfdx*dx)*(dfdx*dx)+(dfdy*dy)*(dfdy*dy));
	if (df==0) df = 1e-3;
	hAsym1[itile]->SetBinContent(i+1, f);
	hAsym1[itile]->SetBinError(i+1, df);
      }

      hAsym1[itile]->GetXaxis()->SetRangeUser(FirstBurst-0.5, LastBurst+0.5);
      hAsym1[itile]->GetXaxis()->SetLabelSize(0.05);
      hAsym1[itile]->GetYaxis()->SetLabelSize(0.05);
      hAsym1[itile]->SetTitle(Name);
      hAsym1[itile]->SetMinimum(-1.1);
      hAsym1[itile]->SetMaximum(1.1);
      hAsym1[itile]->SetMarkerStyle(21);
      hAsym1[itile]->SetMarkerColor(kRed);
      hAsym1[itile]->SetMarkerSize(.15);
      hAsym1[itile]->SetLineColor(kRed);
      hAsym1[itile]->Draw();

      // EOB counters
      Name = Form("Tile %03d asymmetry(EOB) vs burst ID", itile);
      hAsym2[itile] = new TH1F(Name, Name, 3000, -0.5, 2999.5);

      for (Int_t i=FirstBurst; i<=LastBurst; i++) {
	Double_t x = fHChannelProfileVsBurstEOB->GetBinContent(i+1, itile+201);
	Double_t y = fHChannelProfileVsBurstEOB->GetBinContent(i+1, itile+1);
	if (x+y<1) {
	  hAsym2[itile]->SetBinContent(i+1, 999);
	  hAsym2[itile]->SetBinError(i+1, 0);
	  continue;
	}
	Double_t dx   = sqrt(x);
	Double_t dy   = sqrt(y);
	Double_t f    = (x-y)/(x+y);
	Double_t dfdx = 2.0*y/(x+y)/(x+y);
	Double_t dfdy = 2.0*x/(x+y)/(x+y);
	Double_t df   = sqrt((dfdx*dx)*(dfdx*dx)+(dfdy*dy)*(dfdy*dy));
	if (df==0) df = 1e-3;
	hAsym2[itile]->SetBinContent(i+1, f);
	hAsym2[itile]->SetBinError(i+1, df);
      }

      hAsym2[itile]->SetMarkerStyle(20);
      hAsym2[itile]->SetMarkerColor(kBlue);
      hAsym2[itile]->SetMarkerSize(.15);
      hAsym2[itile]->SetLineColor(kBlue);
      hAsym2[itile]->Draw("same");

      if (!ipad) { // a simple legend
	TText *Text = new TText();
	Text->SetTextSize(0.07);
	Text->SetTextColor(kRed);
	Text->SetTextAlign(kHAlignLeft+kVAlignBottom);
	Text->DrawText(FirstBurst+0.03*(LastBurst-FirstBurst), -0.8, "Readout");
	Text->SetTextColor(kBlue);
	Text->DrawText(FirstBurst+0.03*(LastBurst-FirstBurst), -1.0, "EOB");
      }
    }
    Canvas->Print(fOutPDFFileName, "pdf");
  }

  hAsym1.clear();
  hAsym2.clear();
  delete Canvas;
}

void MUV3DataQualityPlotter::ConvertProfileToTwoDimensionalMap (TH1D *h1, TH2F *h2a, TH2F *h2b) {
  h2a->Reset();
  h2b->Reset();
  for (Int_t iy=1; iy<=12; iy++) {
    for (Int_t ix=1; ix<=12; ix++) {
      h2a->SetBinContent(ix, iy, h1->GetBinContent(ix+(iy-1)*12));
    }
  }
  h2b->SetBinContent(1, 1, h1->GetBinContent(146));
  h2b->SetBinContent(1, 2, h1->GetBinContent(147));
  h2b->SetBinContent(1, 3, h1->GetBinContent(148));
  h2b->SetBinContent(2, 1, h1->GetBinContent(149));
  h2b->SetBinContent(2, 3, h1->GetBinContent(150));
  h2b->SetBinContent(3, 1, h1->GetBinContent(151));
  h2b->SetBinContent(3, 2, h1->GetBinContent(152));
  h2b->SetBinContent(3, 3, h1->GetBinContent(153));
}

void MUV3DataQualityPlotter::DrawBoundaries(Int_t width) {

  TLine *l = new TLine();
  l->SetLineColor(kBlack); l->SetLineWidth(width);
  TArc *c = new TArc(0, 0, 0.103);
  c->SetFillColor(kWhite);
  c->SetLineWidth(width);

  // outer counter boundaries
  for (Int_t i=1; i<=5; i++) {
    l->DrawLine(-0.22*i, -1.32, -0.22*i, +1.32);
    l->DrawLine(+0.22*i, -1.32, +0.22*i, +1.32);
    l->DrawLine(-1.32, -0.22*i, +1.32, -0.22*i);
    l->DrawLine(-1.32, +0.22*i, +1.32, +0.22*i);
  }
  l->DrawLine(-1.32, 0, -0.22, 0);
  l->DrawLine(+1.32, 0, +0.22, 0);
  l->DrawLine(0, +0.22, 0, +1.32);
  l->DrawLine(0, -0.22, 0, -1.32);

  // inner counter boundaries
  l->DrawLine(-0.22, +0.073333, +0.22, +0.073333);
  l->DrawLine(-0.22, -0.073333, +0.22, -0.073333);
  l->DrawLine(-0.073333, -0.22, -0.073333, +0.22);
  l->DrawLine(+0.073333, -0.22, +0.073333, +0.22);
  c->Draw();
}
