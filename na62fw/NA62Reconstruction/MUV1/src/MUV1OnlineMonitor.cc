// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "NA62Global.hh"

#include "MUV1OnlineMonitor.hh"
#include "MUV1Reconstruction.hh"

MUV1OnlineMonitor::MUV1OnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco, "MUV1") {

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void MUV1OnlineMonitor::CreateShifterModeTabs(){

  TObjArray *HistoArray = static_cast<MUV1Reconstruction*>(fReco)->GetHistos();

  TH1 * h1D = nullptr;
  TH2 * h2D = nullptr;

  NA62VOnlineMonitorCanvas *ChannelMonitor = AddCanvasTab("ChannelMonitor");
  ChannelMonitor->Divide(2,2);

  ChannelMonitor->cd(1);
  h1D = static_cast<TH1*>(HistoArray->FindObject("ChannelsOccupancy"));
  if (h1D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h1D);
  h1D = nullptr;

  ChannelMonitor->cd(2);
  //  h2D = static_cast<TH2*>(HistoArray->FindObject("ChannelsHitMap"));
  h2D = static_cast<TH2*>(HistoArray->FindObject("HitMap"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  ChannelMonitor->cd(3);
  h2D = static_cast<TH2*>(HistoArray->FindObject("CREAMFlags"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  ChannelMonitor->cd(4);
  h2D = static_cast<TH2*>(HistoArray->FindObject("HitTime"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
}

void MUV1OnlineMonitor::CreateExpertModeTabs(){

  TObjArray *HistoArray = static_cast<MUV1Reconstruction*>(fReco)->GetHistos();

  TH1 * h1D = nullptr;
  TH2 * h2D = nullptr;

  NA62VOnlineMonitorCanvas *ChannelMonitor = AddCanvasTab("ChannelMonitor");
  ChannelMonitor->Divide(2,2);

  ChannelMonitor->cd(1);
  h1D = static_cast<TH1*>(HistoArray->FindObject("ChannelsOccupancy"));
  if (h1D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h1D);
  h1D = nullptr;

  ChannelMonitor->cd(2);
  h2D = static_cast<TH2*>(HistoArray->FindObject("ChannelsHitMap"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  ChannelMonitor->cd(3);
  h2D = static_cast<TH2*>(HistoArray->FindObject("CREAMFlags"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  ChannelMonitor->cd(4);
  h2D = static_cast<TH2*>(HistoArray->FindObject("HitTime"));
  if (h2D) ChannelMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;


  NA62VOnlineMonitorCanvas *Stability = AddCanvasTab("Stability");
  Stability->Divide(2,1);

  Stability->cd(1);
  h2D = static_cast<TH2*>(HistoArray->FindObject("NHitsVSBurst"));
  if (h2D) Stability->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  Stability->cd(2);
  h2D = static_cast<TH2*>(HistoArray->FindObject("HitTimeVSBurst"));
  if (h2D) Stability->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;


  NA62VOnlineMonitorCanvas *HitMonitor = AddCanvasTab("HitMonitor");
  HitMonitor->Divide(2,2);

  HitMonitor->cd(1);
  h2D = static_cast<TH2F*>(HistoArray->FindObject("HitTimeVSChannel"));
  if (h2D) HitMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  HitMonitor->cd(2);
  h2D = static_cast<TH2*>(HistoArray->FindObject("QVSChannel"));
  if (h2D) HitMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  HitMonitor->cd(3)->SetLogz();
  h2D = static_cast<TH2*>(HistoArray->FindObject("AmplitudeVSChannel"));
  if (h2D){
    HitMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
    h2D->GetYaxis()->SetRangeUser(0,1000);
  }
  h2D = nullptr;

  HitMonitor->cd(4);
  h2D = static_cast<TH2*>(HistoArray->FindObject("SigmaVSChannel"));
  if (h2D) HitMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;


  NA62VOnlineMonitorCanvas *ClusterMonitor = AddCanvasTab("ClusterMonitor");
  ClusterMonitor->Divide(2,2);

  ClusterMonitor->cd(1);
  h2D = static_cast<TH2*>(HistoArray->FindObject("HitMap"));
  if (h2D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  //  ClusterMonitor->cd(2)->SetLogy();
  //  h1D = static_cast<TH1*>(HistoArray->FindObject("ShowerWidth"));
  //  if (h1D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h1D);
  //  h1D = nullptr;
  
  ClusterMonitor->cd(2);
  h2D = static_cast<TH2*>(HistoArray->FindObject("ClusterSeedChargeVsChannel"));
  if (h2D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;

  ClusterMonitor->cd(3);
  h1D = static_cast<TH1*>(HistoArray->FindObject("ClusterLayerTimeDiff"));
  if (h1D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h1D);
  h1D = nullptr;

  ClusterMonitor->cd(4);
  h2D = static_cast<TH2*>(HistoArray->FindObject("SeedVSNHits"));
  if (h2D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;


  NA62VOnlineMonitorCanvas *EnergyMonitor = AddCanvasTab("TotalEnergy");
  EnergyMonitor->cd(0);
  h1D = static_cast<TH1*>(HistoArray->FindObject("TotalEnergy"));
  if (h1D) h1D->Draw();
  h1D = nullptr;


  NA62VOnlineMonitorCanvas *BaselinesMonitor = AddCanvasTab("BaselinesMonitor");
  BaselinesMonitor->cd(0);
  h2D = static_cast<TH2*>(HistoArray->FindObject("BaselineVSChannel"));
  if (h2D) ClusterMonitor->GetCurrentFrame()->DrawHisto(h2D,"COLZ");
  h2D = nullptr;
}

MUV1OnlineMonitor::~MUV1OnlineMonitor() {}
