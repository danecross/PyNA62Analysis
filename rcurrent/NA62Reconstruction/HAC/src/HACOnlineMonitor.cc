// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "NA62Global.hh"

#include "HACOnlineMonitor.hh"
#include "HACReconstruction.hh"

HACOnlineMonitor::HACOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco, "HAC") {

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void HACOnlineMonitor::CreateShifterModeTabs(){

  HACReconstruction * HACReco = static_cast<HACReconstruction*>( fReco );
  NA62VOnlineMonitorCanvas * HACGeneral = AddCanvasTab("General");
  HACGeneral->Divide(2,2);
  HACGeneral->cd(1);
  gPad->SetLogz(1);
  Double_t TimeWindow = 40;
  HACGeneral->GetCurrentFrame()->DrawHisto(HACReco->GetHRecoHitTimeWrtReferenceVsROChannel(),"COLZ");
  HACReco->GetHRecoHitTimeWrtReferenceVsROChannel()->GetYaxis()->SetRangeUser(-TimeWindow,TimeWindow);
  HACGeneral->cd(2);
  HACGeneral->GetCurrentFrame()->DrawHisto(HACReco->GetHChID());
  HACGeneral->cd(3);
  HACGeneral->GetCurrentFrame()->DrawHisto(HACReco->GetHModulevsSection(),"COLZ");
  HACGeneral->cd(4);
  HACGeneral->GetCurrentFrame()->DrawHisto(HACReco->GetHModuleID());
}

void HACOnlineMonitor::CreateExpertModeTabs(){

  HACReconstruction * HACReco = static_cast<HACReconstruction*>( fReco );
  NA62VOnlineMonitorCanvas * HACPattern = AddCanvasTab("HitPattern");
  HACPattern->GetCurrentFrame()->DrawHisto(HACReco->GetHModulevsSection(),"COLZ");

  NA62VOnlineMonitorCanvas * HACGeneral = AddCanvasTab("General");
  HACGeneral->Divide(2,2);
  HACGeneral->cd(1);
  gPad->SetLogz(1);
  Double_t TimeWindow = 40;
  HACGeneral->GetCurrentFrame()->DrawHisto(HACReco->GetHRecoHitTimeWrtReferenceVsROChannel(),"COLZ");
  HACReco->GetHRecoHitTimeWrtReferenceVsROChannel()->GetYaxis()->SetRangeUser(-TimeWindow,TimeWindow);
  HACGeneral->cd(2);
  HACGeneral->GetCurrentFrame()->DrawHisto(HACReco->GetHChID());
  HACGeneral->cd(3);
  HACGeneral->GetCurrentFrame()->DrawHisto(HACReco->GetHEdgeFlagvsChID(),"COLZ");
  HACGeneral->cd(4);
  HACGeneral->GetCurrentFrame()->DrawHisto(HACReco->GetHModuleID());

  NA62VOnlineMonitorCanvas * HACTimeCharge = AddCanvasTab("TimeAndCharge");
  HACTimeCharge->Divide(1,2);
  HACTimeCharge->cd(1);
  HACTimeCharge->GetCurrentFrame()->DrawHisto(HACReco->GetHTimevsSiPMid(),"COLZ");
  HACTimeCharge->cd(2);
  HACTimeCharge->GetCurrentFrame()->DrawHisto(HACReco->GetHChargevsSiPMid(),"COLZ");
}

HACOnlineMonitor::~HACOnlineMonitor() {}
