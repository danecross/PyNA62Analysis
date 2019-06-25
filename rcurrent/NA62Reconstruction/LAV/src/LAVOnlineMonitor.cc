// ---------------------------------------------------------------
// History:
//
// Created by M. Raggi 16/10/2012
// Modified by S. Martellotti 06/06/2016
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TStyle.h"
#include "NA62Global.hh"

#include "LAVOnlineMonitor.hh"
#include "LAVReconstruction.hh"

LAVOnlineMonitor::LAVOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco, "LAV"), fPhiOnPhysics(nullptr), fPhiOffPhysics(nullptr), fHTotEOBRate(nullptr), fHTotEOBRateAllLAVs(nullptr){

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void LAVOnlineMonitor::CreateShifterModeTabs(){

    // ADD a tab to the monitor
    NA62VOnlineMonitorCanvas * HitNumberLAV = AddCanvasTab("OccupancyMap");
    HitNumberLAV->Divide(4,3);
    for (Int_t iStation = 1; iStation <= 12; iStation++){
      HitNumberLAV->cd(iStation);
      HitNumberLAV->GetCurrentFrame()->DrawHisto(static_cast<LAVReconstruction*>(fReco)->GetHNLAVHit(iStation));
      gPad->SetLogy(1);
    }
}

void LAVOnlineMonitor::CreateExpertModeTabs(){

    // ADD a tab to the monitor
    NA62VOnlineMonitorCanvas * HitNumberLAV = AddCanvasTab("OccupancyMap");
    HitNumberLAV->Divide(4,3);
    for (Int_t iStation = 1; iStation <= 12; iStation++){
      HitNumberLAV->cd(iStation);
      HitNumberLAV->GetCurrentFrame()->DrawHisto(static_cast<LAVReconstruction*>(fReco)->GetHNLAVHit(iStation));
      gPad->SetLogy(1);
    }

    NA62VOnlineMonitorCanvas * GoodHitNumberLAV = AddCanvasTab("GoodHitMap");
    GoodHitNumberLAV->Divide(4,3);
    for (Int_t iStation = 1; iStation <= 12; iStation++){
      GoodHitNumberLAV->cd(iStation);
      GoodHitNumberLAV->GetCurrentFrame()->DrawHisto(static_cast<LAVReconstruction*>(fReco)->GetHNLAVGoodHit(iStation));
      gPad->SetLogy(1);
    }

    NA62VOnlineMonitorCanvas * NGoodHitPerLAV = AddCanvasTab("NGoodHit");
    NGoodHitPerLAV->Divide(4,3);
    for (Int_t iStation = 1; iStation <= 12; iStation++){
      NGoodHitPerLAV->cd(iStation);
      NGoodHitPerLAV->GetCurrentFrame()->DrawHisto(static_cast<LAVReconstruction*>(fReco)->GetHNRecoGoodHit(iStation));
      gPad->SetLogy(1);
    }

    NA62VOnlineMonitorCanvas * EOBOccupancyLAV = AddCanvasTab("EOBOccupancy");
    EOBOccupancyLAV->Divide(4,3);
    for(Int_t iStation = 1; iStation <= 12; iStation++){
      EOBOccupancyLAV->cd(iStation);
      EOBOccupancyLAV->GetCurrentFrame()->DrawHisto(static_cast<LAVReconstruction*>(fReco)->GetEOBLAVOccupancy(iStation));
      gPad->SetLogy(1);
    }

    NA62VOnlineMonitorCanvas * PhiDistributionPlotOnPhys = AddCanvasTab("PhiDistributionOnPhys");
    fPhiOnPhysics = static_cast<LAVReconstruction*>(fReco)->GetPhiDistributionOnPhys();
    fPhiOnPhysics->GetXaxis()->SetTitle("LAV stations");
    fPhiOnPhysics->GetYaxis()->SetTitle("#phi[degrees]");
    fPhiOnPhysics->GetXaxis()->SetTitleOffset(1.5);
    fPhiOnPhysics->GetYaxis()->SetTitleOffset(1.7);
    fPhiOnPhysics->GetXaxis()->CenterTitle();
    fPhiOnPhysics->GetYaxis()->CenterTitle();
    fPhiOnPhysics->GetYaxis()->SetNdivisions(-6);
    gStyle->SetOptStat(0);
    gPad->SetLogz(1);
    PhiDistributionPlotOnPhys->GetCurrentFrame()->DrawHisto(fPhiOnPhysics, "LEGO2");

    NA62VOnlineMonitorCanvas * PhiDistributionPlotOffPhys = AddCanvasTab("PhiDistributionOffPhys");
    fPhiOffPhysics = static_cast<LAVReconstruction*>(fReco)->GetPhiDistributionOffPhys();
    fPhiOffPhysics->GetXaxis()->SetTitle("LAV stations");
    fPhiOffPhysics->GetYaxis()->SetTitle("#phi[degrees]");
    fPhiOffPhysics->GetXaxis()->SetTitleOffset(1.5);
    fPhiOffPhysics->GetYaxis()->SetTitleOffset(1.7);
    fPhiOffPhysics->GetXaxis()->CenterTitle();
    fPhiOffPhysics->GetYaxis()->CenterTitle();
    fPhiOffPhysics->GetYaxis()->SetNdivisions(-6);
    gStyle->SetOptStat(0);
    gPad->SetLogz(1);
    PhiDistributionPlotOffPhys->GetCurrentFrame()->DrawHisto(fPhiOffPhysics, "LEGO2");

    //NA62VOnlineMonitorCanvas * EOBTotRateLAV = AddCanvasTab("EOBTotRate");
    TLegend * EOBTotRateLAVLegend = new TLegend(0.85,0.60,0.98,0.95);
    EOBTotRateLAVLegend->SetFillColor(kWhite);
    fHTotEOBRate = new TGraph*[12];
    fHTotEOBRateAllLAVs = new TMultiGraph();
    Int_t PlotColor[12] = {1, 2, 3, 4, 5, 6, 11, 13, 40, 7, 8, 9};
    Int_t PlotStyle[12] = {20, 21, 22, 23, 20, 21, 22, 23, 20, 21, 22, 23};

    for(Int_t iStation = 1; iStation <= 12; iStation++) fHTotEOBRate[iStation-1] = 0;
    for(Int_t iStation = 1; iStation <= 12; iStation++){
      fHTotEOBRate[iStation-1] = new TGraph();
      fHTotEOBRate[iStation-1]->Set(1);
      fHTotEOBRate[iStation-1]->SetPoint(0,-1.,0.);
      fHTotEOBRate[iStation-1]->SetMarkerStyle(PlotStyle[iStation-1]);
      fHTotEOBRate[iStation-1]->SetMarkerSize(0.9);
      fHTotEOBRate[iStation-1]->SetMarkerColor(PlotColor[iStation-1]);
      fHTotEOBRate[iStation-1]->SetLineColor(PlotColor[iStation-1]);
      fHTotEOBRate[iStation-1]->SetLineWidth(5);
      EOBTotRateLAVLegend->AddEntry(fHTotEOBRate[iStation-1],Form("LAV %d", iStation), "P");
      fHTotEOBRateAllLAVs->Add(fHTotEOBRate[iStation-1]);  
    }
    fHTotEOBRateAllLAVs->Draw("AP");
    EOBTotRateLAVLegend->Draw();
}

LAVOnlineMonitor::~LAVOnlineMonitor() {}

void LAVOnlineMonitor::Update(Int_t BurstID) {
  
  if(fHTotEOBRateAllLAVs){
    Double_t LastValueX=-1., LastValueY=0.;
    Double_t EOBRateSingleLAV = 0.;
    TList *TGraphList = fHTotEOBRateAllLAVs->GetListOfGraphs();
    TIter itr(TGraphList);
    TObject *obj(0);
    Int_t iStation = 1;
    while((obj = itr())){
      TGraph *gLAV = dynamic_cast<TGraph*>(obj);
      if(gLAV){
        if(gLAV->GetN()>0){
          gLAV->GetPoint(gLAV->GetN()-1, LastValueX, LastValueY);
          if(LastValueX==BurstID || LastValueX<0) gLAV->RemovePoint(gLAV->GetN()-1);
        }
        gLAV->Set(gLAV->GetN()+1);
        EOBRateSingleLAV = (Double_t)(static_cast<LAVReconstruction*>(fReco)->GetEOBRateVsLAV()->GetBinContent(iStation));
        gLAV->SetPoint(gLAV->GetN()+1,BurstID,EOBRateSingleLAV);
        iStation++;
      }                                                                                                                              
    }
  }

  NA62VOnlineMonitor::Update(BurstID);

}
