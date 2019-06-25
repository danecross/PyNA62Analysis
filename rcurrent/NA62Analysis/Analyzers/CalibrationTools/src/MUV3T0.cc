#include "MUV3T0.hh"

/// \class MUV3T0
/// \Brief
/// Evaluation of the T0 constants for MUV3
/// \EndBrief
/// \Detailed
/// A detailed PDF report is generated, including channel times (wrt reference times) and their time stabilities
/// (standard feature of the T0Evaluation tool), correlations and differences of the times (wrt reference times)
/// of two channels in each tile (MUV3 specific feature).
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

MUV3T0::MUV3T0(Core::BaseAnalysis *ba) : T0Evaluation(ba, "MUV3") {

  // Optional parameters
  fMinIntegral         = 100;  // minimal number of entries (excluding underflows, overflows) for fit attempt
  fFittingRange        = 0.9;  // fitting range = [-0.9ns:+0.9ns], i.e. 9 bins of 0.2ns width
  fNFilesToAccumulate  = 20;   // for the T0 stability plots
  fHistoTimeLimit      = 25.0; // time half-span of plotted histograms [ns]
  fSignalPeakWidth     = 3.0;  // exclusion region half-width when looking for anomalous shape [ns]
  fInitialResol        = 0.6;  // initial value of the time resolution parameter for the fit [ns]
  fIssueWarnings       = true; // check if the spectrum shape is OK?
  fPlotChannelTimes    = true; // plot times in each channel?
  fPlotTimeDependences = false; // check and plot the time stability of the T0 constants?
}

///////////////////////////////////////////////////////////////////
// Read the "tile time" histograms to produce the additional output

void MUV3T0::RequestUserHistograms() {
  fAllTileHistogramsFound = true;
  fHDigiTimeRaw = static_cast<TH2F*>(RequestHistogram("MUV3Monitor", "DigiTimeRawFineVsROChannel", true));
  for (Int_t iTile=0; iTile<152; iTile++) {
    TString Name1 = Form("Tile %03d T1,T2", iTile);
    fHTileT1T2[iTile] = static_cast<TH2F*>(RequestHistogram("MUV3Monitor/MUV3Tiles/Time2D", Name1, true));
    if (!fHTileT1T2[iTile]) fAllTileHistogramsFound = false;
    TString Name2 = Form("Tile %03d dT", iTile);
    fHTileDeltaTime[iTile] = static_cast<TH1F*>(RequestHistogram("MUV3Monitor/MUV3Tiles/dT", Name2, true));
    if (!fHTileDeltaTime[iTile]) fAllTileHistogramsFound = false;
  }
}

void MUV3T0::GenerateUserPDFReport() {

  Int_t Npages = 10;
  TCanvas *Canvas = new TCanvas("Canvas0");
  Canvas->Divide(4,4);
  for (int i=1; i<=16; i++) {
    Canvas->GetPad(i)->SetLeftMargin(0.07);
    Canvas->GetPad(i)->SetRightMargin(0.01);
    Canvas->GetPad(i)->SetTopMargin(0.01);
    Canvas->GetPad(i)->SetBottomMargin(0.06);
  }
  gStyle->SetPalette(1);

  /////////////////////////////
  // Digi times in each channel

  if (fHDigiTimeRaw) {
    TH1D* h[512];
    Int_t iPad = 1;
    for (Int_t i=1; i<=fHDigiTimeRaw->GetNbinsX(); i++) {
      Canvas->GetPad(iPad)->Clear();
      Canvas->cd(iPad);
      TString HistoName = Form("RO channel %03d Digi Time [ns]", i-1);
      h[i-1] = fHDigiTimeRaw->ProjectionY(HistoName, i, i);
      if (h[i-1]->Integral()<0.5) continue;
      h[i-1]->GetXaxis()->SetRangeUser(-50, 50);
      h[i-1]->SetTitle(HistoName);
      h[i-1]->GetXaxis()->SetLabelSize(0.055);
      h[i-1]->GetYaxis()->SetLabelSize(0.055);
      h[i-1]->GetXaxis()->SetTitleSize(0.055);
      h[i-1]->GetYaxis()->SetTitleSize(0.055);
      h[i-1]->Draw();
      iPad++;
      if (iPad==17) {
	Canvas->Print(fOutPDFFileName, "pdf");
	iPad = 1;
      }
    }
  }

  if (!fAllTileHistogramsFound) {
    std::cout<<user_normal() << "Tile T1,T2 or DeltaT histograms not found" << std::endl;
    std::cout<<user_normal() << "Need 'EnableChannelHistograms=1' in NA62Reco MUV3.conf to produce additional monitoring plots" << std::endl;
    return;
  }

  //////////////////////////////////////
  // Time correlation plots in each tile

  for (Int_t ipage=0; ipage<Npages; ipage++) {
    for (Int_t i=0; i<16; i++) {
      Canvas->GetPad(i+1)->Clear();
      Canvas->cd(i+1);
      Int_t iTile = ipage*16 + i;
      if (iTile>=152) continue;

      // Find out the RO channel IDs for this tile
      Int_t roch1 = -999, roch2 = -999;
      for (Int_t ich=0; ich<fNChannels; ich++) { // loop over readout channels
	if (fChannelID[ich]==iTile)     roch1 = ich;
	if (fChannelID[ich]==iTile+200) roch2 = ich;
      }
      TString HistoName = Form("Tile %03d times [ns]", iTile);
      fHTileT1T2[iTile]->SetTitle(HistoName);
      fHTileT1T2[iTile]->GetXaxis()->SetTitle("");
      fHTileT1T2[iTile]->GetYaxis()->SetTitle("");
      fHTileT1T2[iTile]->GetXaxis()->SetLabelSize(0.07);
      fHTileT1T2[iTile]->GetYaxis()->SetLabelSize(0.07);
      fHTileT1T2[iTile]->SetTitleSize(0.07);
      fHTileT1T2[iTile]->Draw("col");

      Bool_t not_existing     = (iTile==65 || iTile==66 || iTile==77 || iTile==78);
      Bool_t not_instrumented = (fUseChannelMap && (roch1<0 || roch2<0));

      fText->SetTextAlign(kHAlignCenter+kVAlignCenter);
      fText->SetTextSize(0.15);
      fText->SetTextColor(kGreen+2);
      if (not_existing) fText->DrawText(0, 0, "DOES NOT EXIST");
      else if (not_instrumented) fText->DrawText(0, 0, "N/A");
      if (not_existing || not_instrumented) continue;

      TString XAxisName = Form("Geom %03d, RO %03d", iTile,     roch1);
      TString YAxisName = Form("Geom %03d, RO %03d", iTile+200, roch2);
      fText->SetTextAlign(kHAlignCenter+kVAlignCenter);
      fText->SetTextSize(0.072);
      fText->SetTextColor(kBlack);
      fText->DrawText(10, -17, XAxisName);
      fText->SetTextAngle(90);
      fText->DrawText(-18, 5, YAxisName);
      fText->SetTextAngle(0);
    }
    Canvas->Print(fOutPDFFileName, "pdf");
  }

  /////////////////////////////////////
  // Time difference plots in each tile

  fText->SetTextAlign(kHAlignCenter+kVAlignCenter);
  fText->SetTextSize(0.15);
  fText->SetTextColor(kGreen+2);
  for (Int_t ipage=0; ipage<Npages; ipage++) {
    for (Int_t i=0; i<16; i++) {
      Canvas->GetPad(i+1)->Clear();
      Canvas->cd(i+1);
      Int_t iTile = ipage*16 + i;
      if (iTile>=152) continue;

      // Find out the RO channel IDs for this tile
      Int_t roch1 = -999, roch2 = -999;
      for (Int_t ich=0; ich<fNChannels; ich++) {
	if (fChannelID[ich]==iTile)     roch1 = ich;
	if (fChannelID[ich]==iTile+200) roch2 = ich;
      }
      TString HistoName = Form("Tile %03d: T(ch%03d)#minusT(ch%03d) [ns]", iTile, iTile+200, iTile);
      fHTileDeltaTime[iTile]->SetTitle(HistoName);
      fHTileDeltaTime[iTile]->GetXaxis()->SetTitle("");
      fHTileDeltaTime[iTile]->GetYaxis()->SetTitle("");
      fHTileDeltaTime[iTile]->GetXaxis()->SetLabelSize(0.07);
      fHTileDeltaTime[iTile]->GetYaxis()->SetLabelSize(0.07);
      fHTileDeltaTime[iTile]->GetXaxis()->SetRangeUser(-10.0, +10.0);
      fHTileDeltaTime[iTile]->SetFillColor(kYellow);
      fHTileDeltaTime[iTile]->Draw();

      Bool_t not_existing     = (iTile==65 || iTile==66 || iTile==77 || iTile==78);
      Bool_t not_instrumented = (fUseChannelMap && (roch1<0 || roch2<0));
      if (not_existing) fText->DrawText(0, 0.5, "DOES NOT EXIST");
      else if (not_instrumented) fText->DrawText(0, 0.5, "N/A");
    }
    Canvas->Print(fOutPDFFileName, "pdf");
  }
}
