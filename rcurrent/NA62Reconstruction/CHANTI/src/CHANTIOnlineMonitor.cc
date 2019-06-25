// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "NA62Global.hh"

#include "TDCBRawDecoder.hh"
#include "CHANTIOnlineMonitor.hh"
#include "CHANTIReconstruction.hh"

CHANTIOnlineMonitor::CHANTIOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco,"CHANTI") {

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void CHANTIOnlineMonitor::CreateShifterModeTabs(){

  Int_t  NPlanes = static_cast<CHANTIReconstruction*>(fReco)->GetNPlanes();

  gStyle->SetPaintTextFormat("4.0f");

  NA62VOnlineMonitorCanvas * ChannelOccupancyPlaneX = AddCanvasTab("X_view");
  ChannelOccupancyPlaneX->Divide(4,3);
  for(int iPlane = 0; iPlane < NPlanes; iPlane++){
    ChannelOccupancyPlaneX->cd(2*iPlane+1);
    gPad->SetLeftMargin(0.20);
    gPad->SetRightMargin(0.1);
    gPad->SetTopMargin(0.12);
    gPad->SetBottomMargin(0.12);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0)->SetMarkerSize(3);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0)->GetXaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0)->GetXaxis()->SetLabelSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0)->GetYaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0)->GetYaxis()->SetLabelSize(0.06);
    ChannelOccupancyPlaneX->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0),"COLZ");
    ChannelOccupancyPlaneX->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0),"TEXT90SAME");
    ChannelOccupancyPlaneX->cd(2*iPlane+2);
    gPad->SetLeftMargin(0.20);
    gPad->SetRightMargin(0.1);
    gPad->SetTopMargin(0.12);
    gPad->SetBottomMargin(0.12);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0)->SetMarkerSize(3);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0)->GetXaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0)->GetXaxis()->SetLabelSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0)->GetYaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0)->GetYaxis()->SetLabelSize(0.06);
    ChannelOccupancyPlaneX->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0),"COLZ");
    ChannelOccupancyPlaneX->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0),"TEXT90SAME");
  }

  NA62VOnlineMonitorCanvas * ChannelOccupancyPlaneY = AddCanvasTab("Y_view");
  ChannelOccupancyPlaneY->Divide(4,3);
  for(int iPlane = 0; iPlane < NPlanes; iPlane++){
    ChannelOccupancyPlaneY->cd(2*iPlane+1);
    gPad->SetLeftMargin(0.20);
    gPad->SetRightMargin(0.1);
    gPad->SetTopMargin(0.12);
    gPad->SetBottomMargin(0.12);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0)->SetMarkerSize(3);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0)->GetXaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0)->GetXaxis()->SetLabelSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0)->GetYaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0)->GetYaxis()->SetLabelSize(0.06);
    ChannelOccupancyPlaneY->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0),"COLZ");
    ChannelOccupancyPlaneY->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0),"TEXTSAME");
    ChannelOccupancyPlaneY->cd(2*iPlane+2);
    gPad->SetLeftMargin(0.20);
    gPad->SetRightMargin(0.1);
    gPad->SetTopMargin(0.12);
    gPad->SetBottomMargin(0.12);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0)->SetMarkerSize(3);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0)->GetXaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0)->GetXaxis()->SetLabelSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0)->GetYaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0)->GetYaxis()->SetLabelSize(0.06);
    ChannelOccupancyPlaneY->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0),"COLZ");
    ChannelOccupancyPlaneY->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0),"TEXTSAME");
  }

  NA62VOnlineMonitorCanvas * ChannelOccupancyTrack = AddCanvasTab("Hits/Track");

  ChannelOccupancyTrack->Divide(2,2);
  ChannelOccupancyTrack->cd(1);
  gPad->SetLeftMargin(0.20);
  gPad->SetRightMargin(0.1);
  gPad->SetTopMargin(0.12);
  gPad->SetBottomMargin(0.12);
  static_cast<CHANTIReconstruction*>(fReco)->GetHXHit()->GetXaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHXHit()->GetXaxis()->SetLabelSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHXHit()->GetYaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHXHit()->GetYaxis()->SetLabelSize(0.06);
  ChannelOccupancyTrack->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHXHit());
  ChannelOccupancyTrack->cd(2);
  gPad->SetLeftMargin(0.20);
  gPad->SetRightMargin(0.1);
  gPad->SetTopMargin(0.12);
  gPad->SetBottomMargin(0.12);
  static_cast<CHANTIReconstruction*>(fReco)->GetHYHit()->GetXaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHYHit()->GetXaxis()->SetLabelSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHYHit()->GetYaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHYHit()->GetYaxis()->SetLabelSize(0.06);
  ChannelOccupancyTrack->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHYHit());
  ChannelOccupancyTrack->cd(3);
  gPad->SetLeftMargin(0.20);
  gPad->SetRightMargin(0.1);
  gPad->SetTopMargin(0.12);
  gPad->SetBottomMargin(0.12);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX()->GetXaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX()->GetXaxis()->SetLabelSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX()->GetYaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX()->GetYaxis()->SetLabelSize(0.06);
  ChannelOccupancyTrack->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX());
  ChannelOccupancyTrack->cd(4);
  gPad->SetLeftMargin(0.20);
  gPad->SetRightMargin(0.1);
  gPad->SetTopMargin(0.12);
  gPad->SetBottomMargin(0.12);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY()->GetXaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY()->GetXaxis()->SetLabelSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY()->GetYaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY()->GetYaxis()->SetLabelSize(0.06);
  ChannelOccupancyTrack->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY());

}

void CHANTIOnlineMonitor::CreateExpertModeTabs(){

  Int_t  NPlanes = static_cast<CHANTIReconstruction*>(fReco)->GetNPlanes();

  gStyle->SetPaintTextFormat("4.0f");

  const Int_t NTrigs = 2;
  TString TrigSuffix[NTrigs] = {
    "",
    "_EOB"
  };


  for(int iTrig=0; iTrig<NTrigs; iTrig++){
    NA62VOnlineMonitorCanvas * ChannelOccupancyPlaneX = AddCanvasTab(Form("X_view%s",TrigSuffix[iTrig].Data()));
    ChannelOccupancyPlaneX->Divide(4,3);
    for(int iPlane = 0; iPlane < NPlanes; iPlane++){
      ChannelOccupancyPlaneX->cd(2*iPlane+1);
      gPad->SetLeftMargin(0.20);
      gPad->SetRightMargin(0.1);
      gPad->SetTopMargin(0.12);
      gPad->SetBottomMargin(0.12);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,iTrig)->SetMarkerSize(3);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,iTrig)->GetXaxis()->SetTitleSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,iTrig)->GetXaxis()->SetLabelSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,iTrig)->GetYaxis()->SetTitleSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,iTrig)->GetYaxis()->SetLabelSize(0.06);
      ChannelOccupancyPlaneX->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,iTrig),"COLZ");
      ChannelOccupancyPlaneX->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,iTrig),"TEXT90SAME");
      ChannelOccupancyPlaneX->cd(2*iPlane+2);
      gPad->SetLeftMargin(0.20);
      gPad->SetRightMargin(0.1);
      gPad->SetTopMargin(0.12);
      gPad->SetBottomMargin(0.12);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,iTrig)->SetMarkerSize(3);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,iTrig)->GetXaxis()->SetTitleSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,iTrig)->GetXaxis()->SetLabelSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,iTrig)->GetYaxis()->SetTitleSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,iTrig)->GetYaxis()->SetLabelSize(0.06);
      ChannelOccupancyPlaneX->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,iTrig),"COLZ");
      ChannelOccupancyPlaneX->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,iTrig),"TEXT90SAME");
    }

    NA62VOnlineMonitorCanvas * ChannelOccupancyPlaneY = AddCanvasTab(Form("Y_view%s",TrigSuffix[iTrig].Data()));
    ChannelOccupancyPlaneY->Divide(4,3);
    for(int iPlane = 0; iPlane < NPlanes; iPlane++){
      ChannelOccupancyPlaneY->cd(2*iPlane+1);
      gPad->SetLeftMargin(0.20);
      gPad->SetRightMargin(0.1);
      gPad->SetTopMargin(0.12);
      gPad->SetBottomMargin(0.12);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,iTrig)->SetMarkerSize(3);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,iTrig)->GetXaxis()->SetTitleSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,iTrig)->GetXaxis()->SetLabelSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,iTrig)->GetYaxis()->SetTitleSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,iTrig)->GetYaxis()->SetLabelSize(0.06);
      ChannelOccupancyPlaneY->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,iTrig),"COLZ");
      ChannelOccupancyPlaneY->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,iTrig),"TEXTSAME");
      ChannelOccupancyPlaneY->cd(2*iPlane+2);
      gPad->SetLeftMargin(0.20);
      gPad->SetRightMargin(0.1);
      gPad->SetTopMargin(0.12);
      gPad->SetBottomMargin(0.12);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,iTrig)->SetMarkerSize(3);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,iTrig)->GetXaxis()->SetTitleSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,iTrig)->GetXaxis()->SetLabelSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,iTrig)->GetYaxis()->SetTitleSize(0.06);
      static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,iTrig)->GetYaxis()->SetLabelSize(0.06);
      ChannelOccupancyPlaneY->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,iTrig),"COLZ");
      ChannelOccupancyPlaneY->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,iTrig),"TEXTSAME");
    }
  }

  NA62VOnlineMonitorCanvas * ChannelOccupancyTrack = AddCanvasTab("Hits/Track");

  ChannelOccupancyTrack->Divide(2,2);
  ChannelOccupancyTrack->cd(1);
  gPad->SetLeftMargin(0.20);
  gPad->SetRightMargin(0.1);
  gPad->SetTopMargin(0.12);
  gPad->SetBottomMargin(0.12);
  static_cast<CHANTIReconstruction*>(fReco)->GetHXHit()->GetXaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHXHit()->GetXaxis()->SetLabelSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHXHit()->GetYaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHXHit()->GetYaxis()->SetLabelSize(0.06);
  ChannelOccupancyTrack->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHXHit());
  ChannelOccupancyTrack->cd(2);
  gPad->SetLeftMargin(0.20);
  gPad->SetRightMargin(0.1);
  gPad->SetTopMargin(0.12);
  gPad->SetBottomMargin(0.12);
  static_cast<CHANTIReconstruction*>(fReco)->GetHYHit()->GetXaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHYHit()->GetXaxis()->SetLabelSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHYHit()->GetYaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHYHit()->GetYaxis()->SetLabelSize(0.06);
  ChannelOccupancyTrack->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHYHit());
  ChannelOccupancyTrack->cd(3);
  gPad->SetLeftMargin(0.20);
  gPad->SetRightMargin(0.1);
  gPad->SetTopMargin(0.12);
  gPad->SetBottomMargin(0.12);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX()->GetXaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX()->GetXaxis()->SetLabelSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX()->GetYaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX()->GetYaxis()->SetLabelSize(0.06);
  ChannelOccupancyTrack->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotX());
  ChannelOccupancyTrack->cd(4);
  gPad->SetLeftMargin(0.20);
  gPad->SetRightMargin(0.1);
  gPad->SetTopMargin(0.12);
  gPad->SetBottomMargin(0.12);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY()->GetXaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY()->GetXaxis()->SetLabelSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY()->GetYaxis()->SetTitleSize(0.06);
  static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY()->GetYaxis()->SetLabelSize(0.06);
  ChannelOccupancyTrack->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHDispPlotY());

  //NA62VOnlineMonitorCanvas * TimeAlignment = AddCanvasTab("Timing");
  //TimeAlignment->Divide(3,4);
  //TimeAlignment->cd(1);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReference());
  //TimeAlignment->cd(2);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceNoT0());
  //TimeAlignment->cd(3);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHCandidateTimeWrtReference());
  //TimeAlignment->cd(4);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsBurst(),"COLZ");
  //TimeAlignment->cd(5);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsBurstNoT0(),"COLZ");
  //TimeAlignment->cd(6);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst(),"COLZ");
  //TimeAlignment->cd(7);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannel(),"COLZ");
  //TimeAlignment->cd(9);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsROChannelNoT0(),"COLZ");
  //TimeAlignment->cd(10);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsWidth(),"COLZ");
  //TimeAlignment->cd(12);
  //gPad->SetLeftMargin(0.20);
  //gPad->SetRightMargin(0.1);
  //gPad->SetTopMargin(0.12);
  //gPad->SetBottomMargin(0.12);
  //TimeAlignment->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsWidthNoT0(),"COLZ");

  NA62VOnlineMonitorCanvas * BeamProfile = AddCanvasTab("Beam_Profile");
  BeamProfile->Divide(2,3);
  for(int iPlane = 0; iPlane < NPlanes; iPlane++){
    BeamProfile->cd(iPlane+1);
    gPad->SetLeftMargin(0.10);
    gPad->SetRightMargin(0.05);
    gPad->SetTopMargin(0.06);
    gPad->SetBottomMargin(0.06);
    gPad->SetLeftMargin(0.20);
    gPad->SetRightMargin(0.1);
    gPad->SetTopMargin(0.12);
    gPad->SetBottomMargin(0.12);
    static_cast<CHANTIReconstruction*>(fReco)->GetHXYClusterPerPlane(iPlane)->GetXaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHXYClusterPerPlane(iPlane)->GetXaxis()->SetLabelSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHXYClusterPerPlane(iPlane)->GetYaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHXYClusterPerPlane(iPlane)->GetYaxis()->SetLabelSize(0.06);
    BeamProfile->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHXYClusterPerPlane(iPlane),"COLZ");
  }

  NA62VOnlineMonitorCanvas * StabilityPerPlane = AddCanvasTab("Stability/Plane");
  StabilityPerPlane->Divide(2,3);
  for(int iPlane = 0; iPlane < NPlanes; iPlane++){
    StabilityPerPlane->cd(iPlane+1);
    gPad->SetLeftMargin(0.20);
    gPad->SetRightMargin(0.1);
    gPad->SetTopMargin(0.12);
    gPad->SetBottomMargin(0.12);
    gPad->SetLogy();
    static_cast<CHANTIReconstruction*>(fReco)->GetHNHitsForPlaneVSBurst(iPlane)->GetXaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHNHitsForPlaneVSBurst(iPlane)->GetXaxis()->SetLabelSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHNHitsForPlaneVSBurst(iPlane)->GetYaxis()->SetTitleSize(0.06);
    static_cast<CHANTIReconstruction*>(fReco)->GetHNHitsForPlaneVSBurst(iPlane)->GetYaxis()->SetLabelSize(0.06);
    StabilityPerPlane->GetCurrentFrame()->DrawHisto(static_cast<CHANTIReconstruction*>(fReco)->GetHNHitsForPlaneVSBurst(iPlane));
  }
}

CHANTIOnlineMonitor::~CHANTIOnlineMonitor() {}

void CHANTIOnlineMonitor::Update(Int_t BurstID) {

  Int_t  NPlanes = static_cast<CHANTIReconstruction*>(fReco)->GetNPlanes();
  Int_t  NSlots = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetNSlots(0);
  Double_t FreqNorm = NSlots*(static_cast<CHANTIReconstruction*>(fReco)->GetNTriggersPerBurst()*ClockPeriod);
  Double_t EobNorm = 1e3*(static_cast<CHANTIReconstruction*>(fReco)->GetBurstLength());

  for(int iPlane = 0; iPlane < NPlanes; iPlane++){
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0)->Scale(1e6/FreqNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0)->Scale(1e6/FreqNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0)->Scale(1e6/FreqNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0)->Scale(1e6/FreqNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,1)->Scale(1./EobNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,1)->Scale(1./EobNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,1)->Scale(1./EobNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,1)->Scale(1./EobNorm);
  }

  const Int_t NStoredBursts = 90;
  Int_t MinBurstID = 0;

  if (BurstID >= MinBurstID+NStoredBursts) MinBurstID = BurstID - NStoredBursts;
  Int_t MaxBurstID = MinBurstID + NStoredBursts + 10;
  static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsBurst()->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
  static_cast<CHANTIReconstruction*>(fReco)->GetHRecoHitTimeWrtReferenceVsBurstNoT0()->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
  static_cast<CHANTIReconstruction*>(fReco)->GetHCandidateTimeWrtReferenceVsBurst()->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
  for(int iPlane=0;iPlane<NPlanes;iPlane++){
    static_cast<CHANTIReconstruction*>(fReco)->GetHNHitsForRingVSBurst(2*iPlane)->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
    static_cast<CHANTIReconstruction*>(fReco)->GetHNHitsForRingVSBurst(2*iPlane+1)->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
    static_cast<CHANTIReconstruction*>(fReco)->GetHNHitsForPlaneVSBurst(iPlane)->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
  }

  NA62VOnlineMonitor::Update(BurstID);

  //restore initial scaling
  for(int iPlane = 0; iPlane < NPlanes; iPlane++){
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,0)->Scale(FreqNorm/1e6);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,0)->Scale(FreqNorm/1e6);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,0)->Scale(FreqNorm/1e6);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,0)->Scale(FreqNorm/1e6);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX1THRL(iPlane,1)->Scale(EobNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewX2THRL(iPlane,1)->Scale(EobNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY1THRL(iPlane,1)->Scale(EobNorm);
    static_cast<CHANTIReconstruction*>(fReco)->GetHViewY2THRL(iPlane,1)->Scale(EobNorm);
  }
}
