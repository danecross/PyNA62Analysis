// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TLine.h"
#include "NA62Global.hh"

#include "LKrOnlineMonitor.hh"
#include "LKrReconstruction.hh"
#include "NA62Reconstruction.hh"
#include "CREAMRawDecoder.hh"

LKrOnlineMonitor::LKrOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco, "LKr") {

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void LKrOnlineMonitor::CreateShifterModeTabs(){

  NA62VOnlineMonitorCanvas * LowLevelMap = AddCanvasTab("LowLevel");
  LowLevelMap->Divide(3,2);
  LowLevelMap->cd(1);
  gPad->SetLogz();
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHZSCounterOOB(),"COLZ");
  LowLevelMap->cd(2);
  gPad->SetLogz(0);
  static_cast<LKrReconstruction *>(fReco)->GetHPedestalXY()->GetZaxis()->SetRangeUser(390,410);
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHPedestalXY(),"COLZ");
  LowLevelMap->cd(3);
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHCellTotalEnergyVsPrimBits(),"COLZ");
  LowLevelMap->cd(4);
  static_cast<LKrReconstruction *>(fReco)->GetHSigma()->GetZaxis()->SetRangeUser(2,12);
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHSigma(),"COLZ");
  LowLevelMap->cd(5);
  static_cast<LKrReconstruction *>(fReco)->GetHMaxSampleVsL0Mask()->GetZaxis()->SetRangeUser(300,600);
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHMaxSampleVsL0Mask(),"COLZ");
  LowLevelMap->cd(6);
  TH2F * hMaxSampleXYCalib = static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHMaxSampleXYCalib();
  LowLevelMap->GetCurrentFrame()->DrawHisto(hMaxSampleXYCalib,"COLZ");
  
  NA62VOnlineMonitorCanvas * HitMap = AddCanvasTab("HitMap");
  HitMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMap(),"COLZ");

  NA62VOnlineMonitorCanvas * HitMapEn = AddCanvasTab("HitMapEnergy");
  gPad->SetLogz();
  HitMapEn->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapEnergy(),"COLZ");
  static_cast<LKrReconstruction *>(fReco)->GetHHitMapEnergy()->SetMinimum(0.1);

  NA62VOnlineMonitorCanvas * Clusters = AddCanvasTab("Clusters");
  Clusters->Divide(2,2);
  Clusters->cd(1);
  Clusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHClusterXY(),"COLZ");
  Clusters->cd(2);
  Clusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHClusterEnergy());
  Clusters->cd(3);
  Clusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHNClusters());
  Clusters->cd(4);
  Clusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHMaxSample());

  NA62VOnlineMonitorCanvas * Energy = AddCanvasTab("Energy");
  Energy->Divide(3,2);
  Energy->cd(1);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHTotalEnergyVsL0Mask(),"COLZ");
  Energy->cd(2);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHClusterTimeVsEnergy(),"COLZ");
  Energy->cd(3);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHTotalEnergyVsPrimBits(),"COLZ");
  Energy->cd(4);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHCellTotalEnergy(),"COLZ");
  Energy->cd(5);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHCellTimeVsEnergy(),"COLZ");
  Energy->cd(6);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHCellTotalEnergyVsPrimBits(),"COLZ");

  NA62VOnlineMonitorCanvas * HitMapPrimBitsCells = AddCanvasTab("HitMapPrimBitsCells");
  HitMapPrimBitsCells->Divide(4,2);
  HitMapPrimBitsCells->cd(1);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsCellsEff(8),"COLZ");
  HitMapPrimBitsCells->cd(2);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsCellsEff(12),"COLZ");
  HitMapPrimBitsCells->cd(3);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsCellsEff(13),"COLZ");
  HitMapPrimBitsCells->cd(4);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsCellsEff(14),"COLZ");
  HitMapPrimBitsCells->cd(5);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsCellsEff(8));
  HitMapPrimBitsCells->cd(6);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsCellsEff(12));
  HitMapPrimBitsCells->cd(7);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsCellsEff(13));
  HitMapPrimBitsCells->cd(8);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsCellsEff(14));

  NA62VOnlineMonitorCanvas * CSInfo = AddCanvasTab("CrateSlotInfo");
  CSInfo->Divide(2,2);
  CSInfo->cd(1);
  CSInfo->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapCrateSlot(),"COLZ");
  DrawCrateSlotBoundaries();
  CSInfo->cd(2);
  TH2F * hMaxSampleCSCalib = static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHMaxSampleCrateSlotCalib();
  CSInfo->GetCurrentFrame()->DrawHisto(hMaxSampleCSCalib,"COLZ");
  DrawCrateSlotBoundaries();
  CSInfo->cd(3);
  TH2F * hNCriticalErrorsCS = static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHNCriticalErrorsCrateSlot();
  CSInfo->GetCurrentFrame()->DrawHisto(hNCriticalErrorsCS,"COLZ");
  DrawCrateSlotBoundaries();
  CSInfo->cd(4);
  TH2F * hNQualityWarningsCS = static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHNQualityWarningsCrateSlot();
  CSInfo->GetCurrentFrame()->DrawHisto(hNQualityWarningsCS,"COLZ");
  DrawCrateSlotBoundaries();
}

void LKrOnlineMonitor::CreateExpertModeTabs(){

  NA62VOnlineMonitorCanvas * LowLevelMap = AddCanvasTab("LowLevel");
  LowLevelMap->Divide(3,2);
  LowLevelMap->cd(1);
  gPad->SetLogz();
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHZSCounterOOB(),"COLZ");
  LowLevelMap->cd(2);
  gPad->SetLogz(0);
  static_cast<LKrReconstruction *>(fReco)->GetHPedestalXY()->GetZaxis()->SetRangeUser(390,410);
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHPedestalXY(),"COLZ");
  LowLevelMap->cd(3);
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHNHits());
  LowLevelMap->cd(4);
  static_cast<LKrReconstruction *>(fReco)->GetHSigma()->GetZaxis()->SetRangeUser(2,12);
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHSigma(),"COLZ");
  LowLevelMap->cd(5);
  static_cast<LKrReconstruction *>(fReco)->GetHMaxSampleVsL0Mask()->GetZaxis()->SetRangeUser(300,600);
  LowLevelMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHMaxSampleVsL0Mask(),"COLZ");
  LowLevelMap->cd(6);
  TH2F * hMaxSampleXYCalib = static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHMaxSampleXYCalib();
  LowLevelMap->GetCurrentFrame()->DrawHisto(hMaxSampleXYCalib,"COLZ");
  
  NA62VOnlineMonitorCanvas * HitMap = AddCanvasTab("HitMap");
  HitMap->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMap(),"COLZ");

  NA62VOnlineMonitorCanvas * HitMapEn = AddCanvasTab("HitMapEnergy");
  gPad->SetLogz();
  HitMapEn->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapEnergy(),"COLZ");
  static_cast<LKrReconstruction *>(fReco)->GetHHitMapEnergy()->SetMinimum(0.1);

  NA62VOnlineMonitorCanvas * Clusters = AddCanvasTab("Clusters");
  Clusters->Divide(2,2);
  Clusters->cd(1);
  Clusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHClusterXY(),"COLZ");
  Clusters->cd(2);
  Clusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHClusterEnergy());
  Clusters->cd(3);
  Clusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHNClusters());
  Clusters->cd(4);
  Clusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHMaxSample());

  NA62VOnlineMonitorCanvas * Energy = AddCanvasTab("Energy");
  Energy->Divide(3,2);
  Energy->cd(1);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHTotalEnergyVsL0Mask(),"COLZ");
  Energy->cd(2);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHClusterTimeVsEnergy(),"COLZ");
  Energy->cd(3);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHTotalEnergyVsPrimBits(),"COLZ");
  Energy->cd(4);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHCellTotalEnergy(),"COLZ");
  Energy->cd(5);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHCellTimeVsEnergy(),"COLZ");
  Energy->cd(6);
  Energy->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHCellTotalEnergyVsPrimBits(),"COLZ");

  NA62VOnlineMonitorCanvas * HitMapPrimBitsClusters = AddCanvasTab("HitMapPrimBitsClusters");
  HitMapPrimBitsClusters->Divide(4,2);
  HitMapPrimBitsClusters->cd(1);
  HitMapPrimBitsClusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsClustersEff(8),"COLZ");
  HitMapPrimBitsClusters->cd(2);
  HitMapPrimBitsClusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsClustersEff(12),"COLZ");
  HitMapPrimBitsClusters->cd(3);
  HitMapPrimBitsClusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsClustersEff(13),"COLZ");
  HitMapPrimBitsClusters->cd(4);
  HitMapPrimBitsClusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsClustersEff(14),"COLZ");
  HitMapPrimBitsClusters->cd(5);
  HitMapPrimBitsClusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsClustersEff(8));
  HitMapPrimBitsClusters->cd(6);
  HitMapPrimBitsClusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsClustersEff(12));
  HitMapPrimBitsClusters->cd(7);
  HitMapPrimBitsClusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsClustersEff(13));
  HitMapPrimBitsClusters->cd(8);
  HitMapPrimBitsClusters->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsClustersEff(14));

  NA62VOnlineMonitorCanvas * HitMapPrimBitsCells = AddCanvasTab("HitMapPrimBitsCells");
  HitMapPrimBitsCells->Divide(4,2);
  HitMapPrimBitsCells->cd(1);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsCellsEff(8),"COLZ");
  HitMapPrimBitsCells->cd(2);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsCellsEff(12),"COLZ");
  HitMapPrimBitsCells->cd(3);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsCellsEff(13),"COLZ");
  HitMapPrimBitsCells->cd(4);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapPrimBitsCellsEff(14),"COLZ");
  HitMapPrimBitsCells->cd(5);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsCellsEff(8));
  HitMapPrimBitsCells->cd(6);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsCellsEff(12));
  HitMapPrimBitsCells->cd(7);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsCellsEff(13));
  HitMapPrimBitsCells->cd(8);
  HitMapPrimBitsCells->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHEnergyPrimBitsCellsEff(14));

  NA62VOnlineMonitorCanvas * CSInfo = AddCanvasTab("CrateSlotInfo");
  CSInfo->Divide(2,2);
  CSInfo->cd(1);
  CSInfo->GetCurrentFrame()->DrawHisto(static_cast<LKrReconstruction *>(fReco)->GetHHitMapCrateSlot(),"COLZ");
  DrawCrateSlotBoundaries();
  CSInfo->cd(2);
  TH2F * hMaxSampleCSCalib = static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHMaxSampleCrateSlotCalib();
  CSInfo->GetCurrentFrame()->DrawHisto(hMaxSampleCSCalib,"COLZ");
  DrawCrateSlotBoundaries();
  CSInfo->cd(3);
  TH2F * hNCriticalErrorsCS = static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHNCriticalErrorsCrateSlot();
  CSInfo->GetCurrentFrame()->DrawHisto(hNCriticalErrorsCS,"COLZ");
  DrawCrateSlotBoundaries();
  CSInfo->cd(4);
  TH2F * hNQualityWarningsCS = static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHNQualityWarningsCrateSlot();
  CSInfo->GetCurrentFrame()->DrawHisto(hNQualityWarningsCS,"COLZ");
  DrawCrateSlotBoundaries();
}

LKrOnlineMonitor::~LKrOnlineMonitor() {}

void LKrOnlineMonitor::Update(Int_t BurstID){

  static_cast<LKrReconstruction *>(fReco)->GetHPedestalXY()->GetZaxis()->SetRangeUser(390,410);
  static_cast<LKrReconstruction *>(fReco)->GetHSigma()->GetZaxis()->SetRangeUser(2,12);
  static_cast<LKrReconstruction *>(fReco)->GetHMaxSample()->GetZaxis()->SetRangeUser(300,600);

  Double_t NQWThr = 1.e-3*32*static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetNProcessedEventsInFile();
  static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHNQualityWarningsCrateSlot()->SetMinimum(NQWThr);

  // Update PrimBits plots
  static_cast<LKrReconstruction *>(fReco)->UpdateLKrMonitorHisto();

  NA62VOnlineMonitor::Update(BurstID);
}

void LKrOnlineMonitor::BurstReset(Int_t BurstID){

  // Standard reset function
  NA62VOnlineMonitor::BurstReset(BurstID);

  if(static_cast<LKrReconstruction*>(fReco)->GetOnlineMonitorAccumulation()<=0) return; //histo are never reset
  if (BurstID % static_cast<LKrReconstruction*>(fReco)->GetOnlineMonitorAccumulation()) return;

  //Plots not present in the OM, but used for other histos
  static_cast<LKrReconstruction *>(fReco)->GetHZSCounter()->Reset("M");
  static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHZSCounterXY()->Reset("M");
  static_cast<CREAMRawDecoder*>(static_cast<LKrReconstruction *>(fReco)->GetRawDecoder()->GetDecoder())->GetHZSCounterXYCalib()->Reset("M");
}

void LKrOnlineMonitor::DrawCrateSlotBoundaries(){

  //draw lines for Crate/Slot plots
  TLine *LineCS = new TLine();
  LineCS->SetLineColor(kBlack);
  LineCS->SetLineWidth(2);

  for (Int_t i=1; i<=31; i++) {
    Double_t y1_dw = 2.5, y2_dw = 10.5;
    Double_t y1_up = 12.5, y2_up = 20.5;
    if(i==4) {
      LineCS->DrawLine(i-0.5,  8.5, i-0.5, y2_dw);
      y2_dw = 4.5;
    }
    if(i==28) {
      LineCS->DrawLine(i-0.5, 18.5, i-0.5, y2_up);
      y2_up = 14.5;
    }
    LineCS->DrawLine(i-0.5, y1_dw, i-0.5, y2_dw);
    LineCS->DrawLine(i-0.5, y1_up, i-0.5, y2_up);
  }
  for (Int_t j=0; j<2;j++){
    for (Int_t i=3+10*j; i<=11+10*j; i++) {
      Double_t x1 = 3.5,  x2 = 27.5;
      if( 6<=i&&i<= 8) x1 =  4.5;
      if(16<=i&&i<=18) x2 = 26.5;
      if(!j && (i<5 || i>9)){
        LineCS->DrawLine(x1, i-0.5, 23.5, i-0.5);
        x1 = 24.5;
      }
      if(j && (i<15 || i>19)){
        LineCS->DrawLine(x1, i-0.5, 6.5, i-0.5);
        x1 = 7.5;
      }
      LineCS->DrawLine( 0.5, i-0.5,  2.5, i-0.5);
      LineCS->DrawLine(x1,   i-0.5,   x2, i-0.5);
      LineCS->DrawLine(28.5, i-0.5, 30.5, i-0.5);
    }
  }
}
