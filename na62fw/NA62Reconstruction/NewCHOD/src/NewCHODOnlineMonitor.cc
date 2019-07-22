// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

/// \class NewCHODOnlineMonitor
/// \Brief
/// Online monitoring for the NewCHOD reconstruction
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
#include "NewCHODOnlineMonitor.hh"
#include "NewCHODReconstruction.hh"

NewCHODOnlineMonitor::NewCHODOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow, Reco, "NewCHOD"), fDataQualityPlotter(nullptr), fHNEventsProcessedPerBurst(nullptr), fHNRecoHitsPerBurst(nullptr){

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void NewCHODOnlineMonitor::CreateShifterModeTabs(){

  // Provision for the interface to the plotter
  std::vector<TH1*> PlotterInput;
  fDataQualityPlotter = new NewCHODDataQualityPlotter(PlotterInput, "./NewCHODOnlineMonitor.pdf");

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

  // NewCHOD rates (2D)

  TLine *l = new TLine();
  l->SetLineColor(kBlack); l->SetLineWidth(2);
  TArc *c = new TArc(0, 0, 0.103);
  c->SetFillColor(kWhite);
  c->SetLineWidth(2);

  // ... PM 0
  ChannelProfileTab->cd(1);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM0(),"COLZ");
  fDataQualityPlotter->DrawBoundaries(3) ;

  // ... PM 1
  ChannelProfileTab->cd(2);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM1(),"COLZ");
  fDataQualityPlotter->DrawBoundaries(3) ;

  // NewCHOD candidates per tile
  ChannelProfileTab->cd(3);

  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->SetTitle("Number of candidates per tile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile(),"hist");

  // NewCHOD rates (1D)
  ChannelProfileTab->cd(4);

  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->SetTitle("Channel profile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile(), "hist");

  // NewCHOD rates (1D)
  ChannelProfileTab->cd(5);

  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->SetTitle("Readout channel profile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile(), "hist");

  // AND/OR of signals in tiles
  ChannelProfileTab->cd(6);

  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->SetTitle("OR, AND of signals in tiles");
  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->GetYaxis()->SetLabelSize(0.05);

  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR(), "hist");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHTileAND(),"hist same");

  TLegend* ANDORLegend = new TLegend(0.20,0.75,0.45,0.88);
  ANDORLegend->SetFillColor(kWhite);
  ANDORLegend->SetTextSize(0.05);
  ANDORLegend->AddEntry(static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR(),  "OR", "l");
  ANDORLegend->AddEntry(static_cast<NewCHODReconstruction*>(fReco)->GetHTileAND(), "AND", "l");
  ANDORLegend->Draw();
}

void NewCHODOnlineMonitor::CreateExpertModeTabs(){

  // Provision for the interface to the plotter
  std::vector<TH1*> PlotterInput;
  fDataQualityPlotter = new NewCHODDataQualityPlotter(PlotterInput, "./NewCHODOnlineMonitor.pdf");

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

  // NewCHOD rates (2D)

  TLine *l = new TLine();
  l->SetLineColor(kBlack); l->SetLineWidth(2);
  TArc *c = new TArc(0, 0, 0.103);
  c->SetFillColor(kWhite);
  c->SetLineWidth(2);

  // ... PM 0
  ChannelProfileTab->cd(1);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM0(),"COLZ");
  fDataQualityPlotter->DrawBoundaries(3) ;

  // ... PM 1
  ChannelProfileTab->cd(2);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM1(),"COLZ");
  fDataQualityPlotter->DrawBoundaries(3) ;

  // NewCHOD candidates per tile
  ChannelProfileTab->cd(3);

  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->SetTitle("Number of candidates per tile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfile(),"hist");

  // NewCHOD rates (1D)
  ChannelProfileTab->cd(4);

  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->SetTitle("Channel profile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile(),"hist");

  // NewCHOD rates (1D)
  ChannelProfileTab->cd(5);

  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->SetTitle("Readout channel profile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfile(),"hist");

  // AND/OR of signals in tiles
  ChannelProfileTab->cd(6);

  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->SetTitle("OR, AND of signals in tiles");
  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR()->GetYaxis()->SetLabelSize(0.05);

  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR(), "hist");
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHTileAND(),"hist same");

  TLegend* ANDORLegend = new TLegend(0.20,0.75,0.45,0.88);
  ANDORLegend->SetFillColor(kWhite);
  ANDORLegend->SetTextSize(0.05);
  ANDORLegend->AddEntry(static_cast<NewCHODReconstruction*>(fReco)->GetHTileOR(),  "OR", "l");
  ANDORLegend->AddEntry(static_cast<NewCHODReconstruction*>(fReco)->GetHTileAND(), "AND", "l");
  ANDORLegend->Draw();

  //////////////////////
  // Channel profile tab (EOB)

  NA62VOnlineMonitorCanvas* ChannelProfileEOBTab = AddCanvasTab("ChannelProfileEOB");
  ChannelProfileEOBTab->Divide(3,2);
  for (Int_t i=1; i<=6; i++) {
    ChannelProfileEOBTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    ChannelProfileEOBTab->GetCanvas()->GetPad(i)->SetRightMargin(0.03);
    ChannelProfileEOBTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    ChannelProfileEOBTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.06);
  }

  // NewCHOD rates (2D)

  TLine *l_eob = new TLine();
  l_eob->SetLineColor(kBlack); l_eob->SetLineWidth(2);
  TArc *c_eob = new TArc(0, 0, 0.103);
  c_eob->SetFillColor(kWhite);
  c_eob->SetLineWidth(2);

  // ... PM 0
  ChannelProfileEOBTab->cd(1);
  ChannelProfileEOBTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_EOB_PM0(),"COLZ");
  fDataQualityPlotter->DrawBoundaries(3) ;

  // ... PM 1
  ChannelProfileEOBTab->cd(2);
  ChannelProfileEOBTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_EOB_PM1(),"COLZ");
  fDataQualityPlotter->DrawBoundaries(3) ;

  // NewCHOD rates (1D)
  ChannelProfileEOBTab->cd(4);

  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->SetTitle("Channel profile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileEOBTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB(),"hist");

  // NewCHOD rates (1D) from EOB monitor
  ChannelProfileEOBTab->cd(5);

  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfileEOB()->SetTitle("Readout channel profile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfileEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfileEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfileEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfileEOB()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileEOBTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHROChannelProfileEOB(),"hist");

  //////////////////
  // EOB scalers tab
  /*
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
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->SetTitle("EOB channel hit profile");
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB()->GetYaxis()->SetLabelSize(0.05);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfileEOB());

  EOBscalersTab->cd(4);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->SetTitle("Tight primitives in tiles");
  static_cast<NewCHODReconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTightPrimitiveProfileEOB()->GetYaxis()->SetLabelSize(0.05);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHTightPrimitiveProfileEOB());

  // Column 2: hit and tight primitive numbers per burst
  EOBscalersTab->cd(2);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNHitsPerBurstEOB()->SetTitle("EOB hits vs burst [mln]");
  static_cast<NewCHODReconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetYaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetXaxis()->SetRangeUser(-0.5,99.5);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHNHitsPerBurstEOB());

  EOBscalersTab->cd(5);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNTightPrimitivesPerBurstEOB()->SetTitle("Tight muon primitives vs burst [mln]");
  static_cast<NewCHODReconstruction*>(fReco)->GetHNTightPrimitivesPerBurstEOB()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNTightPrimitivesPerBurstEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNTightPrimitivesPerBurstEOB()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNTightPrimitivesPerBurstEOB()->GetYaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNTightPrimitivesPerBurstEOB()->GetXaxis()->SetRangeUser(-0.5,99.5);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHNTightPrimitivesPerBurstEOB());

  // Column 3: primitive types and firmware error counts
  EOBscalersTab->cd(3);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTotalPrimitiveCountsEOB()->SetTitle("NewCHOD primitives by type [mln]");
  static_cast<NewCHODReconstruction*>(fReco)->GetHTotalPrimitiveCountsEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTotalPrimitiveCountsEOB()->GetYaxis()->SetLabelSize(0.05);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHTotalPrimitiveCountsEOB());

  EOBscalersTab->cd(6);
  static_cast<NewCHODReconstruction*>(fReco)->GetHErrorCountsEOB()->SetTitle("NewCHOD error counts");
  static_cast<NewCHODReconstruction*>(fReco)->GetHErrorCountsEOB()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHErrorCountsEOB()->GetYaxis()->SetLabelSize(0.05);
  EOBscalersTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHErrorCountsEOB());
  */

  /////////////
  // Timing tab

  NA62VOnlineMonitorCanvas* TimingTab = AddCanvasTab("Timing");
  TimingTab->Divide(3, 2);

  for (Int_t i=1; i<=6; i++) {
    TimingTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    TimingTab->GetCanvas()->GetPad(i)->SetRightMargin(0.03);
    TimingTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    TimingTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.06);
  }

  // Left top: T0-corrected Digi leading time
  TimingTab->cd(1);
  static_cast<NewCHODReconstruction*>(fReco)->GetHLeadingTime()->SetTitle("T0-corrected digi leading time [ns]");
  static_cast<NewCHODReconstruction*>(fReco)->GetHLeadingTime()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHLeadingTime()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHLeadingTime()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHLeadingTime()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHLeadingTime());

  // Left bottom: T0-corrected Digi leading time vs RO channel ID
  TimingTab->cd(4);
  gPad->SetLogz();
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->
    SetTitle("T0-corrected digi leading time vs RO channel");
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->
					  GetHRecoHitTimeWrtReferenceVsROChannel(),"COLZ");

  // Middle top: Delta time in tiles (PM1-PM0), T0-corrected
  TimingTab->cd(2);
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTime()->SetTitle("PM1-PM0 time difference [ns]");
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTime()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTime()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTime()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTime()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTime());

  // Bottom middle: Delta time vs tile ID, T0-corrected
  TimingTab->cd(5);
  gPad->SetLogz();
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTimeVsTile()->SetTitle("PM1-PM0 time difference vs Tile ID");
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTimeVsTile()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTimeVsTile()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTimeVsTile()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTimeVsTile()->GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHDeltaTimeVsTile(),"COLZ");

  // Right top: RecoHit times wrt Reference (Cedar), T0-corrected
  TimingTab->cd(3);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReference()->SetTitle("Digi times wrt CEDAR [ns]");
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReference()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReference()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReference()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReference()->GetYaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReference()->GetXaxis()->SetRangeUser(-10, 10);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReference());

  // Right bottom: Time wrt Reference (Cedar) vs RO channel ID, T0-corrected
  // This is a histogram from TVReconstruction, it's called RecoHit... but contains Digi info
  TimingTab->cd(6);
  gPad->SetLogz();
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->
    SetTitle("Digi time wrt CEDAR vs RO channel");
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->
    GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->
    GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->
    GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel()->
    GetYaxis()->SetLabelSize(0.05);
  TimingTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->
					  GetHRecoHitTimeWrtReferenceVsROChannel(),"COLZ");

  ////////////////
  // Stability tab
 
  NA62VOnlineMonitorCanvas* StabilityTab = AddCanvasTab("Stability");
  StabilityTab->Divide(2, 1);

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

  TLegend* StabilityLegend = new TLegend(0.59,0.82,0.99,0.95);
  StabilityLegend->SetFillColor(kWhite);
  StabilityLegend->SetTextSize(0.04);
  StabilityLegend->AddEntry(fHNEventsProcessedPerBurst, "Events/burst",     "l");
  StabilityLegend->AddEntry(fHNRecoHitsPerBurst,        "RecoHits/burst",   "l");
  StabilityLegend->Draw();

  // Candidate time vs burst
  StabilityTab->cd(2);
  gPad->SetLogz();
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReferenceVsBurst()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReferenceVsBurst()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReferenceVsBurst()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReferenceVsBurst()->GetYaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReferenceVsBurst()->GetXaxis()->SetRangeUser(-0.5,99.5);
  StabilityTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReferenceVsBurst(),"COLZ");

  //////////////////////
  // Signal quality tab

  NA62VOnlineMonitorCanvas* SignalQualityTab = AddCanvasTab("SignalQuality");
  SignalQualityTab->Divide(2, 2);

  for (Int_t i=1; i<=4; i++) {
    SignalQualityTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    SignalQualityTab->GetCanvas()->GetPad(i)->SetRightMargin(0.03);
    SignalQualityTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    SignalQualityTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.06);
  }

  SignalQualityTab->cd(1);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNDigis()->SetTitle("Number of Digis in event");
  static_cast<NewCHODReconstruction*>(fReco)->GetHNDigis()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNDigis()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNDigis()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNDigis()->GetYaxis()->SetLabelSize(0.05);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHNDigis());

  SignalQualityTab->cd(2);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNRecoHits()->SetTitle("Number of RecoHits in event");
  static_cast<NewCHODReconstruction*>(fReco)->GetHNRecoHits()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNRecoHits()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNRecoHits()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNRecoHits()->GetYaxis()->SetLabelSize(0.05);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHNRecoHits());

  SignalQualityTab->cd(3);
  static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatus()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatus()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatus()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatus()->GetYaxis()->SetLabelSize(0.05);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatus());

  SignalQualityTab->cd(4);
  static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatusVsChannel()->GetXaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatusVsChannel()->GetXaxis()->SetLabelSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatusVsChannel()->GetYaxis()->SetTitleSize(0.05);
  static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatusVsChannel()->GetYaxis()->SetLabelSize(0.05);
  SignalQualityTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHHitStatusVsChannel(),"COLZ");

  TText *t2 = new TText();
  t2->SetTextSize(0.06);
  t2->SetTextColor(kGreen+2);
  t2->SetTextAlign(12);
  t2->DrawText(15, 1, "Leading only");
  t2->DrawText(15, 2, "Trailing only");

  ///////////////////////////
  // Fine time monitoring tab
  /*  
  NA62VOnlineMonitorCanvas* FineTimeMon = AddCanvasTab("FineTime");
  FineTimeMon->Divide(2,1);
  FineTimeMon->cd(1);
  FineTimeMon->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHHitFineTimeBits());
  FineTimeMon->cd(2);
  FineTimeMon->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHHitFineTime256());
  */

  NA62VOnlineMonitorCanvas* MotherboardTab = AddCanvasTab("MotherboardTab");
  MotherboardTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfileMb1(),"hist");
  MotherboardTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfileMb2(),"histsame");
  MotherboardTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfileMb3(),"histsame");
  MotherboardTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfileMb4(),"histsame");
  MotherboardTab->GetCurrentFrame()->DrawHisto(static_cast<NewCHODReconstruction*>(fReco)->GetHRecoHitProfileMb5(),"histsame");
}

void NewCHODOnlineMonitor::Update (Int_t BurstID) {

  // Set the time period to be displayed for the stability plots
  Int_t MinBurst = (BurstID<100) ?  0 : BurstID-99;
  Int_t MaxBurst = (BurstID<100) ? 99 : BurstID;
  if(fHNEventsProcessedPerBurst) fHNEventsProcessedPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  if(fHNRecoHitsPerBurst)        fHNRecoHitsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNHitsPerBurstEOB()->GetXaxis()->
    SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  static_cast<NewCHODReconstruction*>(fReco)->GetHNTightPrimitivesPerBurstEOB()->GetXaxis()->
    SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  static_cast<NewCHODReconstruction*>(fReco)->GetHTimeWrtReferenceVsBurst()->GetXaxis()->
    SetRangeUser(MinBurst-0.5, MaxBurst+0.5);

  // Set the colour scale of the 2D channel occupancy plots
  Double_t max0 = static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM0()->
    GetBinContent(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM0()->GetMaximumBin());
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM0()->SetMaximum(max0);

  Double_t max1 = static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM1()->
  GetBinContent(static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM1()->GetMaximumBin());
  static_cast<NewCHODReconstruction*>(fReco)->GetHChannelProfile2D_PM1()->SetMaximum(max1);

  // Set event, hit, candidate numbers per burst
  if(fHNEventsProcessedPerBurst) fHNEventsProcessedPerBurst->SetBinContent
    (fHNEventsProcessedPerBurst->FindBin(BurstID),
     1e-3*static_cast<NA62Reconstruction*>(static_cast<NewCHODReconstruction*>(fReco)->GetMainReco())->GetNProcessedEventsInFile());
  if(fHNRecoHitsPerBurst)        fHNRecoHitsPerBurst->SetBinContent
    (fHNRecoHitsPerBurst->FindBin(BurstID), 1e-3*static_cast<NewCHODReconstruction*>(fReco)->GetNRecoHitsPerBurst());

  // Set the maximum for the above histograms
  Double_t ymax = 0.; 
  if(fHNEventsProcessedPerBurst) ymax = TMath::Max(fHNEventsProcessedPerBurst->GetBinContent(fHNEventsProcessedPerBurst->GetMaximumBin()),ymax);
  if(fHNRecoHitsPerBurst)        ymax = TMath::Max(fHNRecoHitsPerBurst->GetBinContent(fHNRecoHitsPerBurst->GetMaximumBin()),ymax);
  if(fHNEventsProcessedPerBurst) fHNEventsProcessedPerBurst->SetMaximum(1.15*ymax);
  if(fHNRecoHitsPerBurst)        fHNRecoHitsPerBurst->SetMaximum(1.15*ymax);

  NA62VOnlineMonitor::Update(BurstID);
}
