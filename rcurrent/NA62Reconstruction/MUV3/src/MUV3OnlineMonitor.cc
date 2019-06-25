// ---------------------------------------------------------------
// History:
//
// Updated by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-07
// Refurbished by Karim Massri (karim.massri@cern.ch)   2014-02-03
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)   2012-07-16
//
// ---------------------------------------------------------------

/// \class MUV3OnlineMonitor
/// \Brief
/// Online monitoring for the MUV3 reconstruction
/// \EndBrief

#include "Riostream.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TStyle.h"
#include "TLine.h"
#include "TArc.h"
#include "TText.h"
#include "NA62Global.hh"

#include "NA62Reconstruction.hh"
#include "MUV3OnlineMonitor.hh"

MUV3OnlineMonitor::MUV3OnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow, Reco, "MUV3"), fDataQualityPlotter(nullptr), fHNEventsProcessedPerBurst(nullptr), fHNRecoHitsPerBurst(nullptr), fHNCandidatesPerBurst(nullptr), fHTileAsymmetry(nullptr), fHTileAsymmetryEOB(nullptr), fHAsym2(nullptr), fHAsym2Inner(nullptr), fHAsym2EOB(nullptr), fHAsym2InnerEOB(nullptr){

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void MUV3OnlineMonitor::CreateShifterModeTabs(){

  // Provision for the interface to the plotter
  std::vector<TH1*> PlotterInput;
  fDataQualityPlotter = new MUV3DataQualityPlotter(PlotterInput, "./MUV3OnlineMonitor.pdf");

  //////////////////////
  // Channel profile tab

  NA62VOnlineMonitorCanvas* ChannelProfileTab = AddCanvasTab("ChannelProfile");
  ChannelProfileTab->Divide(3,2);
  for (Int_t i=1; i<=6; i++) {
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetRightMargin(0.10);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.10);
  }

  // MUV3 rates (2D): PM0
  ChannelProfileTab->cd(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->SetTitle("Channel map (low channel)");
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->GetYaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->SetLineColor(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM0()->SetLineColor(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->SetLineWidth(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM0()->SetLineWidth(2);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0(),"box");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM0(),"box same");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM0(),"colz same");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM0(),"col same");
  fDataQualityPlotter->DrawBoundaries(1);

  // MUV3 rates (2D): PM1
  ChannelProfileTab->cd(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->SetTitle("Channel map (high channel)");
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->GetYaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->SetLineColor(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM1()->SetLineColor(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->SetLineWidth(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM1()->SetLineWidth(2);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1(),"box");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM1(),"box same");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM1(),"colz same");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM1(),"col same");
  fDataQualityPlotter->DrawBoundaries(1);

  // MUV3 candidates per tile
  ChannelProfileTab->cd(3);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->SetTitle("Number of candidates per tile");
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile(),"hist");

  // MUV3 rates (1D)
  ChannelProfileTab->cd(4);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->SetTitle("Channel profile");
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile());

  // MUV3 rates (1D) from EOB monitor
  ChannelProfileTab->cd(5);
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->SetTitle("Readout channel profile");
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile());

  // AND/OR of signals in tiles
  ChannelProfileTab->cd(6);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->SetTitle("OR, AND of signals in tiles");
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR(), "hist");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHTileAND(),"hist same");

  TLegend* ANDORLegend = new TLegend(0.20,0.75,0.45,0.88);
  ANDORLegend->SetFillColor(kWhite);
  ANDORLegend->SetTextSize(0.05);
  ANDORLegend->AddEntry(static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR(),  "OR", "l");
  ANDORLegend->AddEntry(static_cast<MUV3Reconstruction*>(fReco)->GetHTileAND(), "AND", "l");
  ANDORLegend->Draw();
}

void MUV3OnlineMonitor::CreateExpertModeTabs(){

  // Provision for the interface to the plotter
  std::vector<TH1*> PlotterInput;
  fDataQualityPlotter = new MUV3DataQualityPlotter(PlotterInput, "./MUV3OnlineMonitor.pdf");

  //////////////////////
  // Channel profile tab

  NA62VOnlineMonitorCanvas* ChannelProfileTab = AddCanvasTab("ChannelProfile");
  ChannelProfileTab->Divide(3,2);
  for (Int_t i=1; i<=6; i++) {
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetRightMargin(0.10);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.10);
  }

  // MUV3 rates (2D): PM0
  ChannelProfileTab->cd(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->SetTitle("Channel map (low channel)");
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->GetYaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->SetLineColor(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM0()->SetLineColor(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0()->SetLineWidth(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM0()->SetLineWidth(2);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM0(),"box");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM0(),"box same");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM0(),"colz same");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM0(),"col same");
  fDataQualityPlotter->DrawBoundaries(1);

  // MUV3 rates (2D): PM1
  ChannelProfileTab->cd(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->SetTitle("Channel map (high channel)");
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->GetYaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->SetLineColor(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM1()->SetLineColor(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1()->SetLineWidth(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM1()->SetLineWidth(2);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2D_PM1(),"box");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHMaskedProfile2DInner_PM1(),"box same");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM1(),"colz same");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM1(),"col same");
  fDataQualityPlotter->DrawBoundaries(1);

  // MUV3 candidates per tile
  ChannelProfileTab->cd(3);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->SetTitle("Number of candidates per tile");
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateProfile(),"hist");

  // MUV3 rates (1D)
  ChannelProfileTab->cd(4);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->SetTitle("Channel profile");
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile());

  // MUV3 rates (1D) from EOB monitor
  ChannelProfileTab->cd(5);
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->SetTitle("Readout channel profile");
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHROChannelProfile());

  // AND/OR of signals in tiles
  ChannelProfileTab->cd(6);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->SetTitle("OR, AND of signals in tiles");
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR(), "hist");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHTileAND(),"hist same");

  TLegend* ANDORLegend = new TLegend(0.20,0.75,0.45,0.88);
  ANDORLegend->SetFillColor(kWhite);
  ANDORLegend->SetTextSize(0.05);
  ANDORLegend->AddEntry(static_cast<MUV3Reconstruction*>(fReco)->GetHTileOR(),  "OR", "l");
  ANDORLegend->AddEntry(static_cast<MUV3Reconstruction*>(fReco)->GetHTileAND(), "AND", "l");
  ANDORLegend->Draw();

  /////////////////////
  // Tile asymmetry tab

  NA62VOnlineMonitorCanvas* AsymmetryTab = AddCanvasTab("Asymmetries");
  AsymmetryTab->Divide(2,2);

  // 1D asymmetries
  AsymmetryTab->cd(1);
  fHTileAsymmetry = new TH1D("Asymmetry", "Tile asymmetry", 152, -0.5, 151.5);
  fHTileAsymmetry->SetMinimum(-1.05);
  fHTileAsymmetry->SetMaximum(+1.05);
  fHTileAsymmetry->SetLineColor(kRed);
  fHTileAsymmetry->SetMarkerColor(kRed);
  fHTileAsymmetry->SetMarkerStyle(21);
  fHTileAsymmetry->SetMarkerSize(0.32);
  AsymmetryTab->GetCurrentFrame()->DrawHisto(fHTileAsymmetry,"p");

  AsymmetryTab->cd(2);
  fHTileAsymmetryEOB = new TH1D("AsymmetryEOB", "EOB tile asymmetry", 152, -0.5, 151.5);
  fHTileAsymmetryEOB->SetMinimum(-1.05);
  fHTileAsymmetryEOB->SetMaximum(+1.05);
  fHTileAsymmetryEOB->SetLineColor(kBlue);
  fHTileAsymmetryEOB->SetMarkerColor(kBlue);
  fHTileAsymmetryEOB->SetMarkerStyle(20);
  fHTileAsymmetryEOB->SetMarkerSize(0.32);
  AsymmetryTab->GetCurrentFrame()->DrawHisto(fHTileAsymmetryEOB,"p");

  // 2D asymmetries
  AsymmetryTab->cd(3);
  fHAsym2      = new TH2F("Asym2",  "Tile asymmetry",  12, -1.32, 1.32, 12, -1.32, 1.32);
  fHAsym2Inner = new TH2F("Asym2I", "Asym2I",           3, -0.22, 0.22,  3, -0.22, 0.22);
  fHAsym2->SetMinimum(-1.0);
  fHAsym2->SetMaximum(+1.0);
  fHAsym2Inner->SetMinimum(-1.0);
  fHAsym2Inner->SetMaximum(+1.0);
  AsymmetryTab->GetCurrentFrame()->DrawHisto(fHAsym2,"colz");
  AsymmetryTab->GetCurrentFrame()->DrawHisto(fHAsym2Inner,"col same");
  fDataQualityPlotter->DrawBoundaries(1);

  AsymmetryTab->cd(4);
  fHAsym2EOB      = new TH2F("Asym2eob",  "EOB tile asymmetry",  12, -1.32, 1.32, 12, -1.32, 1.32);
  fHAsym2InnerEOB = new TH2F("Asym2Ieob", "Asym2Ieob",            3, -0.22, 0.22,  3, -0.22, 0.22);
  fHAsym2EOB->SetMinimum(-1.0);
  fHAsym2EOB->SetMaximum(+1.0);
  fHAsym2InnerEOB->SetMinimum(-1.0);
  fHAsym2InnerEOB->SetMaximum(+1.0);
  AsymmetryTab->GetCurrentFrame()->DrawHisto(fHAsym2EOB,"colz");
  AsymmetryTab->GetCurrentFrame()->DrawHisto(fHAsym2InnerEOB,"col same");
  fDataQualityPlotter->DrawBoundaries(1);

  //////////////////
  // EOB scalers tab

  NA62VOnlineMonitorCanvas* EOBscalersTab = AddCanvasTab("EOBscalers");
  EOBscalersTab->Divide(3,2);

  for (Int_t i=1; i<=6; i++) {
    EOBscalersTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    EOBscalersTab->GetCanvas()->GetPad(i)->SetRightMargin(0.03);
    EOBscalersTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    EOBscalersTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.06);
  }

  // Column 1: hit and tight primitive profiles
  EOBscalersTab->cd(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfileEOB()->SetTitle("EOB channel hit profile");
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfileEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfileEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfileEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfileEOB()->GetYaxis()->SetLabelSize(0.05);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfileEOB());

  EOBscalersTab->cd(4);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->SetTitle("Tight primitives in tiles");
  static_cast<MUV3Reconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->GetYaxis()->SetLabelSize(0.05);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHTightPrimitiveProfileEOB());

  // Column 2: hit and tight primitive numbers per burst
  EOBscalersTab->cd(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNHitsPerBurstEOB()->SetTitle("EOB hits vs burst [mln]");
  static_cast<MUV3Reconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetYaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetXaxis()->SetRangeUser(-0.5,99.5);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHNHitsPerBurstEOB());

  EOBscalersTab->cd(5);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNTightMuonsPerBurstEOB()->SetTitle("Tight muon primitives vs burst [mln]");
  static_cast<MUV3Reconstruction*>(fReco)->GetHNTightMuonsPerBurstEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNTightMuonsPerBurstEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNTightMuonsPerBurstEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNTightMuonsPerBurstEOB()->GetYaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNTightMuonsPerBurstEOB()->GetXaxis()->SetRangeUser(-0.5,99.5);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHNTightMuonsPerBurstEOB());

  // Column 3: primitive types and firmware error counts
  EOBscalersTab->cd(3);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTotalPrimitiveCountsEOB()->SetTitle("MUV3 primitives by type [mln]");
  static_cast<MUV3Reconstruction*>(fReco)->GetHTotalPrimitiveCountsEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHTotalPrimitiveCountsEOB()->GetYaxis()->SetLabelSize(0.05);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHTotalPrimitiveCountsEOB());

  EOBscalersTab->cd(6);
  static_cast<MUV3Reconstruction*>(fReco)->GetHErrorCountsEOB()->SetTitle("MUV3 error counts");
  static_cast<MUV3Reconstruction*>(fReco)->GetHErrorCountsEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHErrorCountsEOB()->GetYaxis()->SetLabelSize(0.05);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHErrorCountsEOB());

  /////////////
  // Timing tab

  NA62VOnlineMonitorCanvas* TimingTab = AddCanvasTab("Timing");
  TimingTab->Divide(3,2);

  for (Int_t i=1; i<=6; i++) {
    TimingTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    TimingTab->GetCanvas()->GetPad(i)->SetRightMargin(0.03);
    TimingTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    TimingTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.06);
  }

  // Left top: T0-corrected Digi leading time
  TimingTab->cd(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTime()->SetTitle("T0-corrected digi leading time [ns]");
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTime()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTime()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTime()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTime()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTime());

  // Left bottom: T0-corrected Digi leading time vs RO channel ID
  TimingTab->cd(4);
  gPad->SetLogz();
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTimeVsROChannel()->SetTitle("T0-corrected digi leading time vs RO channel");
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTimeVsROChannel()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTimeVsROChannel()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTimeVsROChannel()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTimeVsROChannel()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHLeadingTimeVsROChannel(),"colz");

  // Middle top: Delta time in tiles (PM1-PM0), T0-corrected
  TimingTab->cd(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTime()->SetTitle("PM1-PM0 time difference [ns]");
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTime()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTime()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTime()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTime()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTime());

  // Bottom middle: Delta time vs tile ID, T0-corrected
  TimingTab->cd(5);
  gPad->SetLogz();
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTimeVsTile()->SetTitle("PM1-PM0 time difference vs Tile ID");
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTimeVsTile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTimeVsTile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTimeVsTile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTimeVsTile()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHDeltaTimeVsTile(),"colz");

  // Right top: RecoHit & Candidate times wrt Reference (Cedar), T0-corrected
  TimingTab->cd(3);
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReference()->SetTitle("RecoHit & candidate times wrt CEDAR [ns]");
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReference()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReference()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReference()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReference()->GetYaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReference()->GetXaxis()->SetRangeUser(-10, 10);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReference());
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReference(),"same");

  TLegend* TimeLegend = new TLegend(0.60,0.82,0.99,0.95);
  TimeLegend->SetFillColor(kWhite);
  TimeLegend->SetTextSize(0.05);
  TimeLegend->AddEntry(static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReference(),   "RecoHits", "l");
  TimeLegend->AddEntry(static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReference(), "Candidates", "l");
  TimeLegend->Draw();

  // Right bottom: RecoHit time wrt Reference vs RO channel ID, T0-corrected
  TimingTab->cd(6);
  gPad->SetLogz();
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->SetTitle("RecoHit time wrt CEDAR vs RO channel");
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel(),"colz");

  ////////////////
  // Stability tab

  NA62VOnlineMonitorCanvas* StabilityTab = AddCanvasTab("Stability");
  StabilityTab->Divide(2,1);

  for (Int_t i=1; i<=2; i++) {
    StabilityTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    StabilityTab->GetCanvas()->GetPad(i)->SetRightMargin(0.03);
    StabilityTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    StabilityTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.06);
  }

  // Events, hits and candidates per burst
  StabilityTab->cd(1);
  TString Name = "NEventsProcessedPerBurst";
  fHNEventsProcessedPerBurst = new TH1D(Name, Name, 3000, -0.5, 2999.5);
  fHNEventsProcessedPerBurst->SetTitle("Events, RecoHits, Candidates per burst [thousands]");
  fHNEventsProcessedPerBurst->SetTitleSize(0.05);
  fHNEventsProcessedPerBurst->GetXaxis()->SetTitle("Burst ID");
  fHNEventsProcessedPerBurst->GetXaxis()->SetTitleSize(0.05);
  fHNEventsProcessedPerBurst->GetXaxis()->SetLabelSize(0.05);
  fHNEventsProcessedPerBurst->GetYaxis()->SetTitleSize(0.05);
  fHNEventsProcessedPerBurst->GetYaxis()->SetLabelSize(0.05);
  fHNEventsProcessedPerBurst->GetXaxis()->SetRangeUser(-0.5,99.5);
  StabilityTab->GetCurrentFrame()->DrawHisto(fHNEventsProcessedPerBurst);

  Name = "NRecoHitsPerBurst";
  fHNRecoHitsPerBurst = new TH1D(Name, Name, 3000, -0.5, 2999.5);
  fHNRecoHitsPerBurst->GetXaxis()->SetRangeUser(-0.5,99.5);
  StabilityTab->GetCurrentFrame()->DrawHisto(fHNRecoHitsPerBurst,"same");

  Name = "NCandidatesPerBurst";
  fHNCandidatesPerBurst = new TH1D(Name, Name, 3000, -0.5, 2999.5);
  fHNCandidatesPerBurst->GetXaxis()->SetRangeUser(-0.5,99.5);
  StabilityTab->GetCurrentFrame()->DrawHisto(fHNCandidatesPerBurst,"same");

  TLegend* StabilityLegend = new TLegend(0.59,0.82,0.99,0.95);
  StabilityLegend->SetFillColor(kWhite);
  StabilityLegend->SetTextSize(0.04);
  StabilityLegend->AddEntry(fHNEventsProcessedPerBurst, "Events/burst",     "l");
  StabilityLegend->AddEntry(fHNRecoHitsPerBurst,        "RecoHits/burst",   "l");
  StabilityLegend->AddEntry(fHNCandidatesPerBurst,      "Candidates/burst", "l");
  StabilityLegend->Draw();

  // Candidate time vs burst
  StabilityTab->cd(2);
  gPad->SetLogz();
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetYaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->SetRangeUser(-0.5,99.5);
  StabilityTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst(),"col");

  //////////////////////
  // Signal quality tab

  NA62VOnlineMonitorCanvas* SignalQualityTab = AddCanvasTab("SignalQuality");
  SignalQualityTab->Divide(3,2);

  for (Int_t i=1; i<=6; i++) {
    SignalQualityTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    SignalQualityTab->GetCanvas()->GetPad(i)->SetRightMargin(0.03);
    SignalQualityTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    SignalQualityTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.06);
  }

  SignalQualityTab->cd(1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNDigis()->SetTitle("Number of Digis in event");
  static_cast<MUV3Reconstruction*>(fReco)->GetHNDigis()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNDigis()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNDigis()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNDigis()->GetYaxis()->SetLabelSize(0.05);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHNDigis());

  SignalQualityTab->cd(2);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNCandidates()->SetTitle("Number of candidates in event");
  static_cast<MUV3Reconstruction*>(fReco)->GetHNCandidates()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNCandidates()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNCandidates()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNCandidates()->GetYaxis()->SetLabelSize(0.05);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHNCandidates());

  SignalQualityTab->cd(4);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatus()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatus()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatus()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatus()->GetYaxis()->SetLabelSize(0.05);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatus());

  SignalQualityTab->cd(5);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatusVsChannel()->GetXaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatusVsChannel()->GetXaxis()->SetLabelSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatusVsChannel()->GetYaxis()->SetTitleSize(0.05);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatusVsChannel()->GetYaxis()->SetLabelSize(0.05);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHHitStatusVsChannel(),"colz");

  TText *t2 = new TText();
  t2->SetTextSize(0.06);
  t2->SetTextColor(kGreen+2);
  t2->SetTextAlign(12);
  t2->DrawText(15, 1, "Leading only");
  t2->DrawText(15, 2, "Trailing only");

  // Right-hand column
  SignalQualityTab->cd(3);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTimeBits());

  SignalQualityTab->cd(6);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTime256());
}

void MUV3OnlineMonitor::Update (Int_t BurstID) {

  // Set the time period to be displayed for the stability plots
  Int_t MinBurst = (BurstID<100) ?  0 : BurstID-99;
  Int_t MaxBurst = (BurstID<100) ? 99 : BurstID;
  if(fHNEventsProcessedPerBurst) fHNEventsProcessedPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  if(fHNRecoHitsPerBurst)        fHNRecoHitsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  if(fHNCandidatesPerBurst)      fHNCandidatesPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetXaxis()->
    SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  static_cast<MUV3Reconstruction*>(fReco)->GetHNTightMuonsPerBurstEOB()->GetXaxis()->
    SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  static_cast<MUV3Reconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->
    SetRangeUser(MinBurst-0.5, MaxBurst+0.5);

  // Rescaling fine time bit plot 1
  Int_t MaxBin  = static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTimeBits()->GetMaximumBin();
  Int_t MinBin  = static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTimeBits()->GetMinimumBin();
  Int_t MaxCont = static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTimeBits()->GetBinContent(MaxBin);
  Int_t MinCont = static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTimeBits()->GetBinContent(MinBin);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTimeBits()->GetYaxis()->SetRangeUser(0.8*MinCont, 1.2*MaxCont);

  // Rescaling fine time bit plot 2
  MaxBin  = static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTime256()->GetMaximumBin();
  MinBin  = static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTime256()->GetMinimumBin();
  MaxCont = static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTime256()->GetBinContent(MaxBin);
  MinCont = static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTime256()->GetBinContent(MinBin);
  static_cast<MUV3Reconstruction*>(fReco)->GetHHitFineTime256()->GetYaxis()->SetRangeUser(0.8*MinCont, 1.2*MaxCont);

  // Set the colour scale of the 2D channel occupancy plots
  Double_t max0 = 0.5 *
    TMath::Max(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM0()->
	       GetBinContent(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM0()->GetMaximumBin()),
	       static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM0()->
	       GetBinContent(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM0()->GetMaximumBin()));
  Double_t max1 = 0.5 *
    TMath::Max(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM1()->
	       GetBinContent(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM1()->GetMaximumBin()),
	       static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM1()->
	       GetBinContent(static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM1()->GetMaximumBin()));

  Double_t max = TMath::Max(max0, max1);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM0()->SetMaximum(max);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM0()->SetMaximum(max);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2D_PM1()->SetMaximum(max);
  static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile2DInner_PM1()->SetMaximum(max);

  // Set event, hit, candidate numbers per burst

  if(fHNEventsProcessedPerBurst) fHNEventsProcessedPerBurst->SetBinContent
    (fHNEventsProcessedPerBurst->FindBin(BurstID),
     1e-3*static_cast<NA62Reconstruction*>(static_cast<MUV3Reconstruction*>(fReco)->GetMainReco())->GetNProcessedEventsInFile());
  if(fHNRecoHitsPerBurst)        fHNRecoHitsPerBurst->SetBinContent
    (fHNRecoHitsPerBurst->FindBin(BurstID), 1e-3*static_cast<MUV3Reconstruction*>(fReco)->GetNRecoHitsPerBurst());
  if(fHNCandidatesPerBurst)      fHNCandidatesPerBurst->SetBinContent
    (fHNCandidatesPerBurst->FindBin(BurstID), 1e-3*static_cast<MUV3Reconstruction*>(fReco)->GetNCandidatesPerBurst());

  // Set the maxima for the above histograms
  Double_t ymax = 0.; 
  if(fHNEventsProcessedPerBurst) ymax = TMath::Max(fHNEventsProcessedPerBurst->GetBinContent(fHNEventsProcessedPerBurst->GetMaximumBin()),ymax);
  if(fHNRecoHitsPerBurst)        ymax = TMath::Max(fHNRecoHitsPerBurst->GetBinContent(fHNRecoHitsPerBurst->GetMaximumBin()),ymax);
  if(fHNCandidatesPerBurst)      ymax = TMath::Max(fHNCandidatesPerBurst->GetBinContent(fHNCandidatesPerBurst->GetMaximumBin()), ymax);
  if(fHNEventsProcessedPerBurst) fHNEventsProcessedPerBurst->SetMaximum(1.15*ymax);
  if(fHNRecoHitsPerBurst)        fHNRecoHitsPerBurst->SetMaximum(1.15*ymax);
  if(fHNCandidatesPerBurst)      fHNCandidatesPerBurst->SetMaximum(1.15*ymax);

  // Update tile asymmetries
  if(fDataQualityPlotter){
    if(fHTileAsymmetry)    fDataQualityPlotter->GenerateTileAsymmetry
      (static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfile(), fHTileAsymmetry);
    if(fHTileAsymmetryEOB) fDataQualityPlotter->GenerateTileAsymmetry
      (static_cast<MUV3Reconstruction*>(fReco)->GetHChannelProfileEOB(), fHTileAsymmetryEOB);
    if(fHTileAsymmetry)    fDataQualityPlotter->ConvertProfileToTwoDimensionalMap(fHTileAsymmetry, fHAsym2, fHAsym2Inner);
    if(fHTileAsymmetryEOB) fDataQualityPlotter->ConvertProfileToTwoDimensionalMap(fHTileAsymmetryEOB, fHAsym2EOB, fHAsym2InnerEOB);
  }

  NA62VOnlineMonitor::Update(BurstID);
}
