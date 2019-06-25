// ---------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "NA62Global.hh"

#include "SAVOnlineMonitor.hh"
#include "SAVReconstruction.hh"

SAVOnlineMonitor::SAVOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco, "SAV") {

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void SAVOnlineMonitor::CreateShifterModeTabs(){

  TObjArray *HistoArray = ((SAVReconstruction*)fReco)->GetHistos();

  TH1 * h1D = nullptr;
  TH2 * h2D = nullptr;

  NA62VOnlineMonitorCanvas *ChannelMonitor = AddCanvasTab("ChannelMonitor");
  ChannelMonitor->Divide(2,1);
  ChannelMonitor->cd(1);
  h1D = ((TH1*)HistoArray->FindObject("ChannelsOccupancy"));
  if (h1D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h1D);
  h1D = nullptr;
  ChannelMonitor->cd(2);
  h2D = ((TH2*)HistoArray->FindObject("CREAMFlags"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  ChannelMonitor->cd(3);
  h2D = ((TH2*)HistoArray->FindObject("HitTime"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
}

void SAVOnlineMonitor::CreateExpertModeTabs(){

  TObjArray *HistoArray = ((SAVReconstruction*)fReco)->GetHistos();

  TH1 * h1D = nullptr;
  TH2 * h2D = nullptr;

  NA62VOnlineMonitorCanvas *ChannelMonitor = AddCanvasTab("ChannelMonitor");
  ChannelMonitor->Divide(2,1);
  ChannelMonitor->cd(1);
  h1D = ((TH1*)HistoArray->FindObject("ChannelsOccupancy"));
  if (h1D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h1D);
  h1D = nullptr;
  ChannelMonitor->cd(2);
  h2D = ((TH2*)HistoArray->FindObject("CREAMFlags"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  ChannelMonitor->cd(3);
  h2D = ((TH2*)HistoArray->FindObject("HitTime"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  
  
  NA62VOnlineMonitorCanvas *Stability = AddCanvasTab("Stability");
  Stability->Divide(2,1);
  Stability->cd(1);
  h2D = ((TH2*)HistoArray->FindObject("NHitsVSBurst"));
  if (h2D) Stability->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  Stability->cd(2);
  h2D = ((TH2*)HistoArray->FindObject("HitTimeVSBurst"));
  if (h2D) Stability->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  
  
  NA62VOnlineMonitorCanvas *HitMonitor = AddCanvasTab("HitMonitor");
  HitMonitor->Divide(2,2);
  HitMonitor->cd(1);
  h2D = ((TH2F*)HistoArray->FindObject("HitTimeVSChannel"));
  if (h2D) HitMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  HitMonitor->cd(2);
  h2D = ((TH2*)HistoArray->FindObject("BaselinesVSChannel"));
  if (h2D) HitMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  HitMonitor->cd(3);
  h2D = ((TH2*)HistoArray->FindObject("AmplitudeVSChannel"));
  if (h2D) HitMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  HitMonitor->cd(4);
  h2D = ((TH2*)HistoArray->FindObject("EnergyVSChannel"));
  if (h2D) HitMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  
  
  NA62VOnlineMonitorCanvas *ClusterMonitor = AddCanvasTab("ClusterMonitor");
  ClusterMonitor->Divide(2,2);
  ClusterMonitor->cd(1);
  h2D = ((TH2*)HistoArray->FindObject("CandidateMap_dID0"));
  if (h2D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  ClusterMonitor->cd(2);
  h2D = ((TH2*)HistoArray->FindObject("CandidateMap_dID1"));
  if (h2D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  ClusterMonitor->cd(3);
  h2D = ((TH2*)HistoArray->FindObject("CandidateEnergy_dID0"));
  if (h2D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
  ClusterMonitor->cd(4);
  h2D = ((TH2*)HistoArray->FindObject("CandidateEnergy_dID1"));
  if (h2D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
}

SAVOnlineMonitor::~SAVOnlineMonitor() {}
