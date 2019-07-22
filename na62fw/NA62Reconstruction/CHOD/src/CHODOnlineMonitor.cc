// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "NA62Global.hh"

#include "NA62Reconstruction.hh"
#include "CHODOnlineMonitor.hh"
#include "CHODReconstruction.hh"

CHODOnlineMonitor::CHODOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco,"CHOD"), fHNEventsProcessedPerBurst(nullptr), fHNRecoHitsPerBurst(nullptr), fHNCandidatesPerBurst(nullptr){

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void CHODOnlineMonitor::CreateShifterModeTabs(){
  NA62VOnlineMonitorCanvas * CHOD = AddCanvasTab("General");
  CHOD->Divide(2,2);
  CHOD->cd(1);
  gPad->SetLogy();
  static_cast<CHODReconstruction*>(fReco)->GetHNHitsPerEventLow()->SetAxisRange(0., 50., "X");
  CHOD->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHNHitsPerEventLow());
  CHOD->cd(2);
  CHOD->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsIntersectionIDNoT0(),"COLZ");
  CHOD->cd(3);
  CHOD->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHChannelOccupancyLow());
  CHOD->cd(4);
  CHOD->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHChannelIllumination(),"COLZ");
}

void CHODOnlineMonitor::CreateExpertModeTabs(){
  NA62VOnlineMonitorCanvas * CHOD = AddCanvasTab("General");
  CHOD->Divide(2,2);
  CHOD->cd(1);
  gPad->SetLogy();
  static_cast<CHODReconstruction*>(fReco)->GetHNHitsPerEventLow()->SetAxisRange(0., 50., "X");
  CHOD->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHNHitsPerEventLow());
  CHOD->cd(2);
  CHOD->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsIntersectionIDNoT0(),"COLZ");
  CHOD->cd(3);
  CHOD->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHChannelOccupancyLow());
  CHOD->cd(4);
  CHOD->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHChannelIllumination(),"COLZ");

  NA62VOnlineMonitorCanvas * CHODTime = AddCanvasTab("Timing");
  CHODTime->Divide(2,2);
  CHODTime->cd(1);
  CHODTime->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHDtAllCorr());
  CHODTime->cd(2);
  static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReference()->SetAxisRange(-5., 5., "X");
  CHODTime->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReference());
  CHODTime->cd(3);
  CHODTime->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHHitWidthLow());
  CHODTime->cd(4);
  CHODTime->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHNDetectedEdge());

  NA62VOnlineMonitorCanvas * CHODStability = AddCanvasTab("Stability");
  CHODStability->Divide(2, 1);
  // Hits and candidates per burst
  CHODStability->cd(1);

  fHNEventsProcessedPerBurst = new TH1D("NEventsProcessedPerBurst", "NEventsProcessedPerBurst", 3000, -0.5, 2999.5);
  fHNEventsProcessedPerBurst->SetTitle("Events, Candidates per burst");
  fHNEventsProcessedPerBurst->GetXaxis()->SetTitle("Burst ID");
  fHNEventsProcessedPerBurst->GetXaxis()->SetTitleSize(0.05);
  fHNEventsProcessedPerBurst->GetXaxis()->SetLabelSize(0.05);
  fHNEventsProcessedPerBurst->GetYaxis()->SetTitleSize(0.05);
  fHNEventsProcessedPerBurst->GetYaxis()->SetLabelSize(0.05);
  fHNEventsProcessedPerBurst->GetXaxis()->SetRangeUser(-0.5,99.5);
  CHODStability->GetCurrentFrame()->DrawHisto(fHNEventsProcessedPerBurst);
  
  fHNCandidatesPerBurst = new TH1D("NCandidatesPerBurst", "NCandidatesPerBurst", 3000, -0.5, 2999.5);
  fHNCandidatesPerBurst->GetXaxis()->SetTitle("Burst ID");
  fHNCandidatesPerBurst->GetXaxis()->SetTitleSize(0.05);
  fHNCandidatesPerBurst->GetXaxis()->SetLabelSize(0.05);
  fHNCandidatesPerBurst->GetYaxis()->SetTitleSize(0.05);
  fHNCandidatesPerBurst->GetYaxis()->SetLabelSize(0.05);
  fHNCandidatesPerBurst->GetXaxis()->SetRangeUser(-0.5,99.5);
  CHODStability->GetCurrentFrame()->DrawHisto(fHNCandidatesPerBurst,"same");

  TLegend* StabilityLegend = new TLegend(0.55,0.80,0.99,0.95);
  StabilityLegend->SetFillColor(kWhite);
  StabilityLegend->SetTextSize(0.045);
  StabilityLegend->AddEntry(fHNEventsProcessedPerBurst, "Events/burst", "l");
  StabilityLegend->AddEntry(fHNCandidatesPerBurst,      "Candidates/burst", "l");
  StabilityLegend->Draw();

  // Candidate time vs burst
  CHODStability->cd(2);
  gPad->SetLogz();
  static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->SetTitleSize(0.05);
  static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->SetLabelSize(0.05);
  static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetYaxis()->SetTitleSize(0.05);
  static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetYaxis()->SetLabelSize(0.05);
  static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->SetRangeUser(-0.5,99.5);
  CHODStability->GetCurrentFrame()->DrawHisto(static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst(),"COLZ");
}

CHODOnlineMonitor::~CHODOnlineMonitor() {}

void CHODOnlineMonitor::Update (Int_t BurstID) {

  // Set the time period to be displayed for the stability plots
  Int_t MinBurst = (BurstID<100) ?  0 : BurstID-99;
  Int_t MaxBurst = (BurstID<100) ? 99 : BurstID;
  if(fHNEventsProcessedPerBurst) fHNEventsProcessedPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  if(fHNCandidatesPerBurst)      fHNCandidatesPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  static_cast<CHODReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);

  // Set entries per burst
  if(fHNEventsProcessedPerBurst) fHNEventsProcessedPerBurst->SetBinContent(fHNEventsProcessedPerBurst->FindBin(BurstID),
      static_cast<NA62Reconstruction*>(static_cast<CHODReconstruction*>(fReco)->GetMainReco())->GetNProcessedEventsInFile());
  if(fHNCandidatesPerBurst)      fHNCandidatesPerBurst->SetBinContent(fHNCandidatesPerBurst->FindBin(BurstID), static_cast<CHODReconstruction*>(fReco)->GetNCandidatesPerBurst());

  NA62VOnlineMonitor::Update(BurstID);
}
