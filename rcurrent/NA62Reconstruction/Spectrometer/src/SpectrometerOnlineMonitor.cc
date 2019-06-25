// ---------------------------------------------------------------
// History:
//
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2016-08-17
// Updated by Michal Koval (michal.koval@cern.ch) 2015-07-10
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TStyle.h"
#include "TF1.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TFitResult.h"
#include "TText.h"
#include "TPolyLine.h"
#include "TLine.h"
#include "TPaletteAxis.h"
#include "TGraphErrors.h"
#include "NA62Global.hh"

#include "SpectrometerOnlineMonitor.hh"
#include "SpectrometerReconstruction.hh"
#include "SpectrometerDigiManager.hh"

using namespace ChannelActivityBorders;

SpectrometerOnlineMonitor::SpectrometerOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco,"Spectrometer"), fHChannelActivity(nullptr), fHSRBLeadings(nullptr), fHCoverLeadings(nullptr), fHAllLeadings(nullptr), fHMmissFit(nullptr), fHMomentumFit(nullptr), fHCh1Illumination(nullptr), fHCh4Illumination(nullptr), fHPlaneRadius(nullptr), fHMissMassVStime(nullptr), fHMomFitBurst(nullptr), fHMissFitBurst(nullptr), fHNleadingsBurst(nullptr){

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void SpectrometerOnlineMonitor::CreateShifterModeTabs(){
  // 1. Tab Readout activity tab
  NA62VOnlineMonitorCanvas *Activity = AddCanvasTab("ChannelActivity");

  Activity->Divide(2,1);

  Activity->cd(1);
  gPad->SetLogz(0);
  gPad->SetLeftMargin(0.02);
  gPad->SetRightMargin(0.02);
  gPad->SetTopMargin(0.06);
  gPad->SetBottomMargin(0.1);

  fHChannelActivity = static_cast<SpectrometerReconstruction*>(fReco)->GetDigiManager()->GetHChannelActivity();
  fHChannelActivity->SetStats(kFALSE);
  Activity->GetCurrentFrame()->DrawHisto(fHChannelActivity,"LEGO 0");
  fHChannelActivity->GetXaxis()->SetTitleSize(0.05);

  Activity->cd(2);
  gPad->SetLogz(1);
  gPad->SetLeftMargin(0.10);
  gPad->SetRightMargin(0.2);
  gPad->SetTopMargin(0.06);
  gPad->SetBottomMargin(0.1);

  Activity->GetCurrentFrame()->DrawHisto(fHChannelActivity,"colz");    

  fHChannelActivity->GetXaxis()->SetTitleSize(0.05);
  gPad->Update();
  gPad->Modified();
  gPad->Update();
  TPolyLine* poly[32] = {new TPolyLine(9, (double*)ch1uLx, (double*)ch1uy), 
    new TPolyLine(9, (double*)ch1uRx, (double*)ch1uy),
    new TPolyLine(9, (double*)ch1vLx, (double*)ch1vy), 
    new TPolyLine(9, (double*)ch1vRx, (double*)ch1vy),
    new TPolyLine(9, (double*)ch1xLx, (double*)ch1xy), 
    new TPolyLine(9, (double*)ch1xRx, (double*)ch1xy),
    new TPolyLine(9, (double*)ch1yLx, (double*)ch1yy), 
    new TPolyLine(9, (double*)ch1yRx, (double*)ch1yy),
    new TPolyLine(5, (double*)ch2uLx, (double*)ch2uy), 
    new TPolyLine(5, (double*)ch2uRx, (double*)ch2uy),
    new TPolyLine(5, (double*)ch2vLx, (double*)ch2vy), 
    new TPolyLine(5, (double*)ch2vRx, (double*)ch2vy),
    new TPolyLine(5, (double*)ch2xLx, (double*)ch2xy), 
    new TPolyLine(5, (double*)ch2xRx, (double*)ch2xy),
    new TPolyLine(9, (double*)ch2yLx, (double*)ch2yy), 
    new TPolyLine(9, (double*)ch2yRx, (double*)ch2yy),
    new TPolyLine(9, (double*)ch3uLx, (double*)ch3uy), 
    new TPolyLine(9, (double*)ch3uRx, (double*)ch3uy),
    new TPolyLine(9, (double*)ch3vLx, (double*)ch3vy), 
    new TPolyLine(9, (double*)ch3vRx, (double*)ch3vy),
    new TPolyLine(7, (double*)ch3xLx, (double*)ch3xy), 
    new TPolyLine(7, (double*)ch3xRx, (double*)ch3xy),
    new TPolyLine(9, (double*)ch3yLx, (double*)ch3yy), 
    new TPolyLine(9, (double*)ch3yRx, (double*)ch3yy),
    new TPolyLine(7, (double*)ch4uLx, (double*)ch4uy), 
    new TPolyLine(7, (double*)ch4uRx, (double*)ch4uy),
    new TPolyLine(7, (double*)ch4vLx, (double*)ch4vy), 
    new TPolyLine(7, (double*)ch4vRx, (double*)ch4vy),
    new TPolyLine(9, (double*)ch4xLx, (double*)ch4xy), 
    new TPolyLine(9, (double*)ch4xRx, (double*)ch4xy),
    new TPolyLine(9, (double*)ch4yLx, (double*)ch4yy), 
    new TPolyLine(9, (double*)ch4yRx, (double*)ch4yy)
  };

  for(int t=0; t<32; t++){
    poly[t]->SetLineWidth(2.2);
    poly[t]->Draw("same");
  };

  TText *t = new TText();
  t->SetTextSize(0.03);
  TText *tt = new TText();
  tt->SetTextSize(0.03);
  tt->SetTextAngle(-90);
  for (int i = 0; i < 4; i++) { 
    const double sep = 0.21;
    t->DrawTextNDC(0.02, 0.12 + sep*i, "U");
    t->DrawTextNDC(0.02, 0.17 + sep*i, "V");
    t->DrawTextNDC(0.02, 0.22 + sep*i, "X");
    t->DrawTextNDC(0.02, 0.27 + sep*i, "Y");
    tt->DrawTextNDC(0.93, 0.285 + sep*i, Form("Chamber %i", i+1));
  }

  // 2. Timing tab
  NA62VOnlineMonitorCanvas *Timing = AddCanvasTab("Timing");

  Timing->Divide(2,1);
  Timing->cd(1);
  gPad->SetLeftMargin(0.12);
  gPad->SetRightMargin(0.15);
  gPad->SetBottomMargin(0.1);
  gPad->SetLogz(kTRUE);

  fHSRBLeadings = static_cast<SpectrometerReconstruction*>(fReco)->GetDigiManager()->GetHAllLeading_srb();
  fHSRBLeadings->SetTitle("Leading time distribution per SRB");
  fHSRBLeadings->GetXaxis()->SetTitle("T_{leading} - T_{reference} - T_{0} [ns]");
  fHSRBLeadings->GetXaxis()->SetTitleSize(0.04);
  fHSRBLeadings->GetYaxis()->SetTitle("SRB ID");
  fHSRBLeadings->GetYaxis()->SetTitleOffset(1.5);
  fHSRBLeadings->GetYaxis()->SetTitleSize(0.04);
  fHSRBLeadings->SetMinimum(5);
  Timing->GetCurrentFrame()->DrawHisto(fHSRBLeadings,"colz");

  Timing->cd(2);
  Timing->GetCurrentFrame()->Divide(1,2); //this canvas is split further
  Timing->GetCurrentFrame()->cd(1);
  gPad->SetBottomMargin(0.12);
  // Total leading time distribution
  fHAllLeadings = static_cast<SpectrometerReconstruction*>(fReco)->GetDigiManager()->GetHLeadingTimeTotal();
  Timing->GetCurrentFrame()->GetCurrentFrame()->DrawHisto(fHAllLeadings, "hist"); // 2 levels of GetCurrentFrame()!
  fHAllLeadings->GetXaxis()->SetTitle("T_{leading} - T_{reference} - T_{0} [ns]");
  fHAllLeadings->GetXaxis()->SetRangeUser(-40, 240);
  fHAllLeadings->GetXaxis()->SetTitleSize(0.05);

  Timing->GetCurrentFrame()->cd(2);
  fHPlaneRadius = static_cast<SpectrometerReconstruction*>(fReco)->GetHRecoHitWireSum2();
  fHPlaneRadius->SetTitle("Sum of the reco radius - two consecutive planes (staggering)");
  Timing->GetCurrentFrame()->GetCurrentFrame()->DrawHisto(fHPlaneRadius,"hist"); // 2 levels of GetCurrentFrame()!
  fHPlaneRadius->GetXaxis()->SetTitle("R [mm]");
  fHPlaneRadius->GetXaxis()->SetTitleSize(0.05);
  fHPlaneRadius->GetXaxis()->SetRangeUser(0., 10.);

  // 3. Tracks
  NA62VOnlineMonitorCanvas *Tracks = AddCanvasTab("ReconstructedTracks");

  Tracks->Divide(2,2);

  Tracks->cd(1);
  gPad->SetBottomMargin(0.12);
  fHMmissFit = static_cast<SpectrometerReconstruction*>(fReco)->GetHPic_mmiss_fit_4Chambers();
  Tracks->GetCurrentFrame()->DrawHisto(fHMmissFit,"hist");
  fHMmissFit->SetStats(1);
  fHMmissFit->SetTitle("Missing mass squared (nominal beam momentum) - 4 chamber track fit");
  fHMmissFit->GetXaxis()->SetTitle("M_{miss}^{2}(#pi) [GeV^{2}/c^{4}]");
  fHMmissFit->GetXaxis()->SetTitleSize(0.05);

  Tracks->cd(2);
  gPad->SetBottomMargin(0.12);
  fHMomentumFit = static_cast<SpectrometerReconstruction*>(fReco)->GetHPic_momentum_fit();
  Tracks->GetCurrentFrame()->DrawHisto(fHMomentumFit,"hist");
  fHMomentumFit->SetTitle("Reconstructed momentum");
  fHMomentumFit->GetXaxis()->SetRangeUser(0.0, 80.0);
  fHMomentumFit->GetXaxis()->SetTitle("p [GeV/c]");
  fHMomentumFit->GetXaxis()->SetTitleSize(0.05);

  Tracks->cd(3);
  gPad->SetRightMargin(0.15);
  fHCh1Illumination = static_cast<SpectrometerReconstruction*>(fReco)->GetHIllum4Chambers_ch1();
  Tracks->GetCurrentFrame()->DrawHisto(fHCh1Illumination,"colz");
  fHCh1Illumination->SetTitle("1. chamber illumination: reconstructed tracks - 4 chamber track fit");
  fHCh1Illumination->GetXaxis()->SetTitle("x [mm]");
  fHCh1Illumination->GetXaxis()->SetTitleSize(0.05);
  fHCh1Illumination->GetYaxis()->SetTitle("y [mm]");
  fHCh1Illumination->GetYaxis()->SetTitleSize(0.05);
  fHCh1Illumination->Rebin2D(2);

  Tracks->cd(4);
  gPad->SetRightMargin(0.15);
  fHCh4Illumination = static_cast<SpectrometerReconstruction*>(fReco)->GetHIllum4Chambers_ch4();
  Tracks->GetCurrentFrame()->DrawHisto(fHCh4Illumination,"colz");
  fHCh4Illumination->SetTitle("4. chamber illumination: reconstructed tracks - 4 chamber track fit");
  fHCh4Illumination->GetXaxis()->SetTitle("x [mm]");
  fHCh4Illumination->GetXaxis()->SetTitleSize(0.05);
  fHCh4Illumination->GetYaxis()->SetTitle("y [mm]");
  fHCh4Illumination->GetYaxis()->SetTitleSize(0.05);
  fHCh4Illumination->Rebin2D(2);
}

void SpectrometerOnlineMonitor::CreateExpertModeTabs(){
  // 1. Tab Readout activity tab
  NA62VOnlineMonitorCanvas *Activity = AddCanvasTab("ChannelActivity");

  Activity->Divide(2,1);

  Activity->cd(1);
  gPad->SetLogz(0);
  gPad->SetLeftMargin(0.02);
  gPad->SetRightMargin(0.02);
  gPad->SetTopMargin(0.06);
  gPad->SetBottomMargin(0.1);

  fHChannelActivity = static_cast<SpectrometerReconstruction*>(fReco)->GetDigiManager()->GetHChannelActivity();
  fHChannelActivity->SetStats(kFALSE);
  Activity->GetCurrentFrame()->DrawHisto(fHChannelActivity,"LEGO 0");
  fHChannelActivity->GetXaxis()->SetTitleSize(0.05);

  Activity->cd(2);
  gPad->SetLogz(1);
  gPad->SetLeftMargin(0.10);
  gPad->SetRightMargin(0.2);
  gPad->SetTopMargin(0.06);
  gPad->SetBottomMargin(0.1);

  Activity->GetCurrentFrame()->DrawHisto(fHChannelActivity,"colz");    

  fHChannelActivity->GetXaxis()->SetTitleSize(0.05);
  gPad->Update();
  gPad->Modified();
  gPad->Update();
  TPolyLine* poly[32] = {new TPolyLine(9, (double*)ch1uLx, (double*)ch1uy), 
    new TPolyLine(9, (double*)ch1uRx, (double*)ch1uy),
    new TPolyLine(9, (double*)ch1vLx, (double*)ch1vy), 
    new TPolyLine(9, (double*)ch1vRx, (double*)ch1vy),
    new TPolyLine(9, (double*)ch1xLx, (double*)ch1xy), 
    new TPolyLine(9, (double*)ch1xRx, (double*)ch1xy),
    new TPolyLine(9, (double*)ch1yLx, (double*)ch1yy), 
    new TPolyLine(9, (double*)ch1yRx, (double*)ch1yy),
    new TPolyLine(5, (double*)ch2uLx, (double*)ch2uy), 
    new TPolyLine(5, (double*)ch2uRx, (double*)ch2uy),
    new TPolyLine(5, (double*)ch2vLx, (double*)ch2vy), 
    new TPolyLine(5, (double*)ch2vRx, (double*)ch2vy),
    new TPolyLine(5, (double*)ch2xLx, (double*)ch2xy), 
    new TPolyLine(5, (double*)ch2xRx, (double*)ch2xy),
    new TPolyLine(9, (double*)ch2yLx, (double*)ch2yy), 
    new TPolyLine(9, (double*)ch2yRx, (double*)ch2yy),
    new TPolyLine(9, (double*)ch3uLx, (double*)ch3uy), 
    new TPolyLine(9, (double*)ch3uRx, (double*)ch3uy),
    new TPolyLine(9, (double*)ch3vLx, (double*)ch3vy), 
    new TPolyLine(9, (double*)ch3vRx, (double*)ch3vy),
    new TPolyLine(7, (double*)ch3xLx, (double*)ch3xy), 
    new TPolyLine(7, (double*)ch3xRx, (double*)ch3xy),
    new TPolyLine(9, (double*)ch3yLx, (double*)ch3yy), 
    new TPolyLine(9, (double*)ch3yRx, (double*)ch3yy),
    new TPolyLine(7, (double*)ch4uLx, (double*)ch4uy), 
    new TPolyLine(7, (double*)ch4uRx, (double*)ch4uy),
    new TPolyLine(7, (double*)ch4vLx, (double*)ch4vy), 
    new TPolyLine(7, (double*)ch4vRx, (double*)ch4vy),
    new TPolyLine(9, (double*)ch4xLx, (double*)ch4xy), 
    new TPolyLine(9, (double*)ch4xRx, (double*)ch4xy),
    new TPolyLine(9, (double*)ch4yLx, (double*)ch4yy), 
    new TPolyLine(9, (double*)ch4yRx, (double*)ch4yy)
  };

  for(int t=0; t<32; t++){
    poly[t]->SetLineWidth(2.2);
    poly[t]->Draw("same");
  };

  TText *t = new TText();
  t->SetTextSize(0.03);
  TText *tt = new TText();
  tt->SetTextSize(0.03);
  tt->SetTextAngle(-90);
  for (int i = 0; i < 4; i++) { 
    const double sep = 0.21;
    t->DrawTextNDC(0.02, 0.12 + sep*i, "U");
    t->DrawTextNDC(0.02, 0.17 + sep*i, "V");
    t->DrawTextNDC(0.02, 0.22 + sep*i, "X");
    t->DrawTextNDC(0.02, 0.27 + sep*i, "Y");
    tt->DrawTextNDC(0.93, 0.285 + sep*i, Form("Chamber %i", i+1));
  }

  // 2. Timing tab
  NA62VOnlineMonitorCanvas *Timing = AddCanvasTab("Timing");

  Timing->Divide(2,1);
  Timing->cd(1);
  gPad->SetLeftMargin(0.12);
  gPad->SetRightMargin(0.15);
  gPad->SetBottomMargin(0.1);
  gPad->SetLogz(kTRUE);

  fHSRBLeadings = static_cast<SpectrometerReconstruction*>(fReco)->GetDigiManager()->GetHAllLeading_srb();
  fHSRBLeadings->SetTitle("Leading time distribution per SRB");
  fHSRBLeadings->GetXaxis()->SetTitle("T_{leading} - T_{reference} - T_{0} [ns]");
  fHSRBLeadings->GetXaxis()->SetTitleSize(0.04);
  fHSRBLeadings->GetYaxis()->SetTitle("SRB ID");
  fHSRBLeadings->GetYaxis()->SetTitleOffset(1.5);
  fHSRBLeadings->GetYaxis()->SetTitleSize(0.04);
  fHSRBLeadings->SetMinimum(5);
  Timing->GetCurrentFrame()->DrawHisto(fHSRBLeadings,"colz");

  Timing->cd(2);
  Timing->GetCurrentFrame()->Divide(1,2); //this canvas is split further
  Timing->GetCurrentFrame()->cd(1);
  gPad->SetBottomMargin(0.12);
  // Total leading time distribution
  fHAllLeadings = static_cast<SpectrometerReconstruction*>(fReco)->GetDigiManager()->GetHLeadingTimeTotal();
  Timing->GetCurrentFrame()->GetCurrentFrame()->DrawHisto(fHAllLeadings, "hist"); // 2 levels of GetCurrentFrame()!
  fHAllLeadings->GetXaxis()->SetTitle("T_{leading} - T_{reference} - T_{0} [ns]");
  fHAllLeadings->GetXaxis()->SetRangeUser(-40, 240);
  fHAllLeadings->GetXaxis()->SetTitleSize(0.05);

  Timing->GetCurrentFrame()->cd(2);
  fHPlaneRadius = static_cast<SpectrometerReconstruction*>(fReco)->GetHRecoHitWireSum2();
  fHPlaneRadius->SetTitle("Sum of the reco radius - two consecutive planes (staggering)");
  Timing->GetCurrentFrame()->GetCurrentFrame()->DrawHisto(fHPlaneRadius,"hist"); // 2 levels of GetCurrentFrame()!
  fHPlaneRadius->GetXaxis()->SetTitle("R [mm]");
  fHPlaneRadius->GetXaxis()->SetTitleSize(0.05);
  fHPlaneRadius->GetXaxis()->SetRangeUser(0., 10.);

  //3. Covers
  NA62VOnlineMonitorCanvas *Covers = AddCanvasTab("Covers");
  Covers->cd(1);

  fHCoverLeadings = static_cast<SpectrometerReconstruction*>(fReco)->GetDigiManager()->GetHAllLeading_cover();
  fHCoverLeadings->SetTitle("Leading time distribution per Cover");
  fHCoverLeadings->GetXaxis()->SetTitle("T_{leading} - T_{reference} - T_{0} [ns]");
  fHCoverLeadings->GetXaxis()->SetTitleSize(0.04);
  fHCoverLeadings->GetXaxis()->SetRangeUser(-200, 450);
  fHCoverLeadings->GetYaxis()->SetTitle("Cover ID");
  fHCoverLeadings->GetYaxis()->SetTitleSize(0.04);
  Covers->GetCurrentFrame()->DrawHisto(fHCoverLeadings,"colz");
  fHCoverLeadings->RebinX(2);

  // 4. Tracks
  NA62VOnlineMonitorCanvas *Tracks = AddCanvasTab("ReconstructedTracks");

  Tracks->Divide(2,2);

  Tracks->cd(1);
  gPad->SetBottomMargin(0.12);
  fHMmissFit = static_cast<SpectrometerReconstruction*>(fReco)->GetHPic_mmiss_fit_4Chambers();
  Tracks->GetCurrentFrame()->DrawHisto(fHMmissFit,"hist");
  fHMmissFit->SetStats(1);
  fHMmissFit->SetTitle("Missing mass squared (nominal beam momentum) - 4 chamber track fit");
  fHMmissFit->GetXaxis()->SetTitle("M_{miss}^{2}(#pi) [GeV^{2}/c^{4}]");
  fHMmissFit->GetXaxis()->SetTitleSize(0.05);

  Tracks->cd(2);
  gPad->SetBottomMargin(0.12);
  fHMomentumFit = static_cast<SpectrometerReconstruction*>(fReco)->GetHPic_momentum_fit();
  Tracks->GetCurrentFrame()->DrawHisto(fHMomentumFit,"hist");
  fHMomentumFit->SetTitle("Reconstructed momentum");
  fHMomentumFit->GetXaxis()->SetRangeUser(0.0, 80.0);
  fHMomentumFit->GetXaxis()->SetTitle("p [GeV/c]");
  fHMomentumFit->GetXaxis()->SetTitleSize(0.05);

  Tracks->cd(3);
  gPad->SetRightMargin(0.15);
  fHCh1Illumination = static_cast<SpectrometerReconstruction*>(fReco)->GetHIllum4Chambers_ch1();
  Tracks->GetCurrentFrame()->DrawHisto(fHCh1Illumination,"colz");
  fHCh1Illumination->SetTitle("1. chamber illumination: reconstructed tracks - 4 chamber track fit");
  fHCh1Illumination->GetXaxis()->SetTitle("x [mm]");
  fHCh1Illumination->GetXaxis()->SetTitleSize(0.05);
  fHCh1Illumination->GetYaxis()->SetTitle("y [mm]");
  fHCh1Illumination->GetYaxis()->SetTitleSize(0.05);
  fHCh1Illumination->Rebin2D(2);

  Tracks->cd(4);
  gPad->SetRightMargin(0.15);
  fHCh4Illumination = static_cast<SpectrometerReconstruction*>(fReco)->GetHIllum4Chambers_ch4();
  Tracks->GetCurrentFrame()->DrawHisto(fHCh4Illumination,"colz");
  fHCh4Illumination->SetTitle("4. chamber illumination: reconstructed tracks - 4 chamber track fit");
  fHCh4Illumination->GetXaxis()->SetTitle("x [mm]");
  fHCh4Illumination->GetXaxis()->SetTitleSize(0.05);
  fHCh4Illumination->GetYaxis()->SetTitle("y [mm]");
  fHCh4Illumination->GetYaxis()->SetTitleSize(0.05);
  fHCh4Illumination->Rebin2D(2);

  // 5. Stability
  NA62VOnlineMonitorCanvas *Stability = AddCanvasTab("Stability");
  Stability->Divide(2,2);

  Stability->cd(1);
  fHMissFitBurst = new TGraphErrors(1);
  fHMissFitBurst->SetPoint(0, 1., -0.1);
  fHMissFitBurst->SetTitle("Gaussian fit of #pi^{0} mass peak; Burst ID; M_{miss}^{2}(#pi) [GeV^{2}/c^{4}]");   
  fHMissFitBurst->SetMarkerStyle(8);
  fHMissFitBurst->SetLineColor(kBlue);
  fHMissFitBurst->SetMarkerColor(kBlack);
  fHMissFitBurst->SetMinimum(0.0175);
  fHMissFitBurst->SetMaximum(0.019);
  Stability->GetCurrentFrame()->DrawHisto(fHMissFitBurst,"PA");
  TF1 *f1 = new TF1("f1", "0.018225", 0.5, 5000.5);
  f1->Draw("same");

  Stability->cd(2);
  fHMomFitBurst = new TGraphErrors(1);
  fHMomFitBurst->SetPoint(0, 1., -0.1);
  fHMomFitBurst->SetTitle("Gaussian fit of beam momentum peak; Burst ID; p [GeV/c]");   
  fHMomFitBurst->SetMarkerStyle(8);
  fHMomFitBurst->SetLineColor(kBlue);
  fHMomFitBurst->SetMarkerColor(kBlack);
  fHMomFitBurst->SetMinimum(71.);
  fHMomFitBurst->SetMaximum(77.);
  Stability->GetCurrentFrame()->DrawHisto(fHMomFitBurst,"PA");

  Stability->cd(3);
  gPad->SetRightMargin(0.15);
  fHMissMassVStime = static_cast<SpectrometerReconstruction*>(fReco)->GetHMissMassTime();
  Stability->GetCurrentFrame()->DrawHisto(fHMissMassVStime, "colz");
  fHMissMassVStime->GetXaxis()->SetTitleSize(0.05);
  fHMissMassVStime->GetYaxis()->SetTitleSize(0.05);
  fHMissMassVStime->GetXaxis()->SetTitleOffset(0.8);
  fHMissMassVStime->GetYaxis()->SetTitleOffset(0.8);

  Stability->cd(4);
  gPad->SetGridx(1);
  gPad->SetGridy(1);
  fHNleadingsBurst = new TGraphErrors(1);
  fHNleadingsBurst->SetPoint(0, 1., -0.1);
  fHNleadingsBurst->SetTitle("N_events(>10 leadings in-time wrt reference) / N_events(all); Burst ID");   
  fHNleadingsBurst->SetMarkerStyle(8);
  fHNleadingsBurst->SetLineColor(kBlue);
  fHNleadingsBurst->SetMarkerColor(kBlack);
  fHNleadingsBurst->SetMinimum(0.);
  fHNleadingsBurst->SetMaximum(1.1);
  Stability->GetCurrentFrame()->DrawHisto(fHNleadingsBurst,"PA");
}

SpectrometerOnlineMonitor::~SpectrometerOnlineMonitor() {}

void SpectrometerOnlineMonitor::Update(Int_t BurstID){

  // number of leadings per burst
  TH1I* hNleadingsBurst = static_cast<SpectrometerReconstruction*>(fReco)->GetDigiManager()->GetHLeadingsInTime();
  double LastValueX3=-1.;
  double LastValueY3=0.;
  Double_t val = hNleadingsBurst->Integral(11, -1)/hNleadingsBurst->Integral(1,-1); 
  if(fHNleadingsBurst){
    if(fHNleadingsBurst->GetN()>0){
      fHNleadingsBurst->GetPoint(fHNleadingsBurst->GetN()-1, LastValueX3, LastValueY3);
      if(LastValueX3==BurstID || LastValueY3<0){
        fHNleadingsBurst->RemovePoint(fHNleadingsBurst->GetN()-1);
      }
    }
    fHNleadingsBurst->Set(fHNleadingsBurst->GetN()+1);
    fHNleadingsBurst->SetPoint(fHNleadingsBurst->GetN()-1, (double)BurstID, val);    
    fHNleadingsBurst->GetXaxis()->SetTitleSize(0.05);
    fHNleadingsBurst->GetYaxis()->SetTitleSize(0.05);
  }

  // fit pi0 missing mass squared
  if(fHMissFitBurst){
    Int_t fitStatus1 = -1;
    TF1 fmiss("fmiss", "gaus", 0.01, 0.03);
    if(fHMmissFit && fHMmissFit->GetEntries() > 100) {
      fitStatus1 = fHMmissFit->Fit("fmiss","NRQ");
    }
    double LastValueX1=-1.;
    double LastValueY1=0.;
    if(fitStatus1 == 0) {
      Double_t mean1  = fmiss.GetParameter(1);
      Double_t meanError1 = fmiss.GetParError(1);
      if(fHMissFitBurst->GetN()>0){
        fHMissFitBurst->GetPoint(fHMissFitBurst->GetN()-1, LastValueX1, LastValueY1);
        if(LastValueX1==BurstID || LastValueY1<0){
          fHMissFitBurst->RemovePoint(fHMissFitBurst->GetN()-1);
        }
      }
      fHMissFitBurst->Set(fHMissFitBurst->GetN()+1);
      fHMissFitBurst->SetPointError(fHMissFitBurst->GetN()-1, 0., meanError1);
      fHMissFitBurst->SetPoint(fHMissFitBurst->GetN()-1, (double)BurstID, mean1);    
      fHMissFitBurst->GetXaxis()->SetTitleSize(0.05);
      fHMissFitBurst->GetYaxis()->SetTitleSize(0.05);
    }
  }

  // fit beam momentum component
  if(fHMomFitBurst){
    Int_t fitStatus2 = -1;
    TF1 fmom("fmom", "gaus", 73., 77.);
    if(fHMomentumFit && fHMomentumFit->GetEntries() > 100) {
      fitStatus2 = fHMomentumFit->Fit("fmom","NRQ");
    }
    double LastValueX2=-1.;
    double LastValueY2=0.;
    if(fitStatus2 == 0) {
      Double_t mean2  = fmom.GetParameter(1);
      Double_t meanError2 = fmom.GetParError(1);
      if(fHMomFitBurst->GetN()>0){
        fHMomFitBurst->GetPoint(fHMomFitBurst->GetN()-1, LastValueX2, LastValueY2);
        if(LastValueX2==BurstID || LastValueY2<0){
          fHMomFitBurst->RemovePoint(fHMomFitBurst->GetN()-1);
        }
      }
      fHMomFitBurst->Set(fHMomFitBurst->GetN()+1);
      fHMomFitBurst->SetPointError(fHMomFitBurst->GetN()-1, 0., meanError2);
      fHMomFitBurst->SetPoint(fHMomFitBurst->GetN()-1, (double)BurstID, mean2);    
      fHMomFitBurst->GetXaxis()->SetTitleSize(0.05);
      fHMomFitBurst->GetYaxis()->SetTitleSize(0.05);
    }
  }

  NA62VOnlineMonitor::Update(BurstID);
}
