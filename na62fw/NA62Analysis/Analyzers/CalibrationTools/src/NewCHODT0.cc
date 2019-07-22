#include "NewCHODT0.hh"

NewCHODT0::NewCHODT0(Core::BaseAnalysis *ba) : T0Evaluation(ba, "NewCHOD") {
  // Optional parameters
  fMinIntegral         = 100;  // minimal number of entries (excluding underflows, overflows) for fit attempt
  fFittingRange        = 3.0;  // fitting range = [-3.0ns:+3.0ns], i.e. 30 bins of 0.2ns width
  fNFilesToAccumulate  = 20;   // for the T0 stability plots
  fHistoTimeLimit      = 25.0; // time half-span of plotted histograms [ns]
  fSignalPeakWidth     = 10.0; // exclusion region half-width when looking for anomalous shape [ns]
  fInitialResol        = 2.0;  // initial value of the time resolution parameter for the fit [ns]
  fMaxResol            = 5.0;  // max time resolution to consider the fit successful [ns]
  fIssueWarnings       = false; // check if the spectrum shape is OK?
  fPlotChannelTimes    = true;  // plot times in each channel?
  fPlotTimeDependences = false; // check and plot the time stability of the T0 constants?
  fPage1MinChannelID   = 160;  // adjust the lower limit for plotting in page 1 of the PDF report
}

///////////////////////////////////////////////////////////////////
// Read the "tile time" histograms to produce the additional output

void NewCHODT0::RequestUserHistograms() {
  // Adjust fitting range for 2017 data (new CFDs: better resolution)
  if (GetRunID()>=6942) {
    fFittingRange = 2.0;
    fInitialResol = 1.2;
  }
  fAllTileHistogramsFound = true;
  Int_t fNTiles = 152;
  for (Int_t i=0; i<fNTiles; i++) {
    Int_t Quadrant  = 1 + i/38;
    Int_t Position = Quadrant*100 + i%38 + 1;
    TString Name1 = Form("Tile %03d T1,T2", Position);
    fHTileT1T2[i] = static_cast<TH2F*>(RequestHistogram("NewCHODMonitor/NewCHODTiles/Time2D", Name1, true));
    if (!fHTileT1T2[i]) fAllTileHistogramsFound = false;
    TString Name2 = Form("Tile %03d dT", Position);
    fHTileDeltaTime[i] = static_cast<TH1F*>(RequestHistogram("NewCHODMonitor/NewCHODTiles/dT", Name2, true));
    if (!fHTileDeltaTime[i]) fAllTileHistogramsFound = false;
  }
  fHTileTight = static_cast<TH2F*>(RequestHistogram("NewCHODMonitor","TightRecoHitTimeWrtReferenceVsTile", true));
  if (!fHTileTight) fAllTileHistogramsFound = false;
}

void NewCHODT0::GenerateUserPDFReport() {
  if (!fAllTileHistogramsFound) {
    std::cout << user_normal() << "Tile T1,T2 or DeltaT histograms or TightRecoHitTimeWrtReferenceVsTile not found" << std::endl;
    std::cout << user_normal() << "Need 'EnableChannelHistograms=1' in NA62Reco NewCHOD.conf to produce additional monitoring plots" << std::endl;
    return;
  }

  fHTileTight->Write();

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

  const Int_t fNTiles = 152;
  Int_t Quadrant, Position;
  Int_t iTile[fNTiles];
  for (Int_t i=0; i<fNTiles; i++) {
    Quadrant  = 1 + i/38;
    Position = Quadrant*100 + i%38 + 1;
    iTile[i] = Position;
  }

  //////////////////////////////////////
  // Time correlation plots in each tile

  for (Int_t ipage=0; ipage<Npages; ipage++) {
    for (Int_t i=0; i<16; i++) {
      Canvas->GetPad(i+1)->Clear();
      Canvas->cd(i+1);
      Int_t Tile = ipage*16 + i;
      if (Tile>=fNTiles) continue;

      // Find out the RO channel IDs for this tile
      Int_t roch1 = -999, roch2 = -999;
      for (Int_t ich=0; ich<fNChannels; ich++) {
	if (fChannelID[ich]==iTile[Tile])     roch1 = ich;
	if (fChannelID[ich]==iTile[Tile]+50) roch2 = ich;
      }

      TString HistoName = Form("Tile %d times [ns]", iTile[Tile]);
      fHTileT1T2[Tile]->SetTitle(HistoName);
      fHTileT1T2[Tile]->GetXaxis()->SetTitle("");
      fHTileT1T2[Tile]->GetYaxis()->SetTitle("");
      fHTileT1T2[Tile]->GetXaxis()->SetLabelSize(0.07);
      fHTileT1T2[Tile]->GetYaxis()->SetLabelSize(0.07);
      fHTileT1T2[Tile]->SetTitleSize(0.07);
      fHTileT1T2[Tile]->Draw("col");

      //Bool_t not_existing     = (iTile==65 || iTile==66 || iTile==77 || iTile==78);
      Bool_t not_instrumented = (fUseChannelMap && (roch1<0 || roch2<0));

      fText->SetTextAlign(kHAlignCenter+kVAlignCenter);
      fText->SetTextSize(0.15);
      fText->SetTextColor(kGreen+2);
      //if (not_existing) fText->DrawText(0, 0, "DOES NOT EXIST");
      if (not_instrumented) {
          fText->DrawText(0, 0, "N/A");
          continue;
      }

      TString XAxisName = Form("Geom %d, RO %d", iTile[Tile],     roch1);
      TString YAxisName = Form("Geom %d, RO %d", iTile[Tile]+50, roch2);
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

      Int_t Tile = ipage*16 + i;
      if (Tile>=fNTiles) continue;

      // Find out the RO channel IDs for this tile
      Int_t roch1 = -999, roch2 = -999;
      for (Int_t ich=0; ich<fNChannels; ich++) {
	if (fChannelID[ich]==iTile[Tile])     roch1 = ich;
	if (fChannelID[ich]==iTile[Tile]+50) roch2 = ich;
      }

      TString HistoName = Form("Tile %d: T(ch%d)#minusT(ch%d) [ns]", iTile[Tile], iTile[Tile]+50, iTile[Tile]);
      fHTileDeltaTime[Tile]->SetTitle(HistoName);
      fHTileDeltaTime[Tile]->GetXaxis()->SetTitle("");
      fHTileDeltaTime[Tile]->GetYaxis()->SetTitle("");
      fHTileDeltaTime[Tile]->GetXaxis()->SetLabelSize(0.07);
      fHTileDeltaTime[Tile]->GetYaxis()->SetLabelSize(0.07);
      fHTileDeltaTime[Tile]->GetXaxis()->SetRangeUser(-10.0, +10.0);
      fHTileDeltaTime[Tile]->SetFillColor(kYellow);
      fHTileDeltaTime[Tile]->Draw();

      //Bool_t not_existing     = (iTile==65 || iTile==66 || iTile==77 || iTile==78);
      Bool_t not_instrumented = (fUseChannelMap && (roch1<0 || roch2<0));
      //if (not_existing) fText->DrawText(0, 0.5, "DOES NOT EXIST");
      if (not_instrumented) fText->DrawText(0, 0.5, "N/A");
    }
    Canvas->Print(fOutPDFFileName, "pdf");
  }

  /////////////////////////////////////
  // Time difference of tight reco hit and reference time in each tile

  fText->SetTextAlign(kHAlignCenter+kVAlignCenter);
  fText->SetTextSize(0.15);
  fText->SetTextColor(kGreen+2);

  for (Int_t ipage=0; ipage<Npages; ipage++) {
    for (Int_t i=0; i<16; i++) {
      Canvas->GetPad(i+1)->Clear();
      Canvas->cd(i+1);

      Int_t Tile = ipage*16 + i;
      if (Tile>=fNTiles) continue;

      fHTileTightRecoHit[Tile] = fHTileTight->ProjectionY(Form("Tile %d: TightRecoHit time wrt CEDAR", iTile[Tile]),fHTileTight->GetXaxis()->FindBin(iTile[Tile]),fHTileTight->GetXaxis()->FindBin(iTile[Tile]));
      fHTileTightRecoHit[Tile]->SetTitle(Form("Tile %d: TightRecoHit time wrt CEDAR", iTile[Tile]));
      fHTileTightRecoHit[Tile]->GetXaxis()->SetTitle("");
      fHTileTightRecoHit[Tile]->GetYaxis()->SetTitle("");
      fHTileTightRecoHit[Tile]->GetXaxis()->SetLabelSize(0.07);
      fHTileTightRecoHit[Tile]->GetYaxis()->SetLabelSize(0.07);
      fHTileTightRecoHit[Tile]->GetXaxis()->SetRangeUser(-20.0, +20.0);
      fHTileTightRecoHit[Tile]->SetFillColor(kYellow);
      fHTileTightRecoHit[Tile]->Draw();
    }
    Canvas->Print(fOutPDFFileName, "pdf");
  }
}

