// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "NA62Global.hh"

#include "SACOnlineMonitor.hh"
#include "SACReconstruction.hh"

SACOnlineMonitor::SACOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco, "SAC") {

  for(int iChan=0;iChan<4; iChan ++) {
    fGHitRateChOnline[iChan] = nullptr;
  }

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void SACOnlineMonitor::CreateShifterModeTabs(){

  for(int iChan=0;iChan<4; iChan ++) {
    fGHitRateChOnline [iChan] = new TGraphErrors();
    fGHitRateChOnline [iChan]->SetName(Form("HitRateOnline_ch_%d",iChan));
    fGHitRateChOnline [iChan]->SetTitle(Form("RecoHitRate"));
    fGHitRateChOnline [iChan]->SetMarkerColor(iChan+1);
    fGHitRateChOnline [iChan]->SetMarkerStyle(26);
    fGHitRateChOnline [iChan]->SetMinimum(1e-9);
  }

  SACReconstruction* const recoSAC=static_cast<SACReconstruction*>(fReco);

  TGraph **fGRateChLow  = recoSAC->GetGRateChLow ();
  TGraph **fGRateChHigh = recoSAC->GetGRateChHigh();
  TGraph **fGRateChLowNorm  = recoSAC->GetGRateChLowNorm ();
  TGraph **fGRateChHighNorm = recoSAC->GetGRateChHighNorm();

  NA62VOnlineMonitorCanvas * ChannelOccupancy = AddCanvasTab("ChannelOccupancy");
  ChannelOccupancy->Divide(2,2);
  ChannelOccupancy->cd(1);
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGRateChLow[0],"AP");
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGRateChHigh[0],"P same");
  for(int iCh=1;iCh<4;iCh++) {
    ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGRateChLow[iCh],"P same");
    ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGRateChHigh[iCh],"P same");
  }
  ChannelOccupancy->cd(2);
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(recoSAC->GetHRateBurst());
  ChannelOccupancy->cd(3);
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGRateChLowNorm[0],"AP");
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGRateChHighNorm[0],"P same");
  for(int iCh=1;iCh<4;iCh++) {
    ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGRateChLowNorm[iCh],"P same");
    ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGRateChHighNorm[iCh],"P same");
  }
  ChannelOccupancy->cd(4);
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGHitRateChOnline[0],"AP");
  //  ChannelOccupancy->GetCurrentFrame()->SetName("Normalized hit rate");
  for(int iCh=1;iCh<4;iCh++) {
    ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGHitRateChOnline[iCh],"P same");
  }
}

void SACOnlineMonitor::CreateExpertModeTabs(){

  for(int iChan=0;iChan<4; iChan ++) {
    fGHitRateChOnline [iChan] = new TGraphErrors();
    fGHitRateChOnline [iChan]->SetName(Form("HitRateOnline_ch_%d",iChan));
    fGHitRateChOnline [iChan]->SetTitle(Form("RecoHitRate"));
    fGHitRateChOnline [iChan]->SetMarkerColor(iChan+1);
    fGHitRateChOnline [iChan]->SetMarkerStyle(26);
    fGHitRateChOnline [iChan]->SetMinimum(1e-9);
  }

  SACReconstruction* const recoSAC=static_cast<SACReconstruction*>(fReco);

  NA62VOnlineMonitorCanvas * ChannelOccupancy = AddCanvasTab("ChannelOccupancy");
  ChannelOccupancy->Divide(2,2);
  ChannelOccupancy->cd(1);
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(recoSAC->GetHRateRun(),"COLZ");
  ChannelOccupancy->cd(2);
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(recoSAC->GetHRateBurst());
  ChannelOccupancy->cd(3);
  TGraphErrors **fGHitRateChNorm  = recoSAC->GetGHitRateChNorm();
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGHitRateChNorm[0],"AP");
  for(int iCh=1;iCh<4;iCh++) {
    ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGHitRateChNorm[iCh],"P same");
  }

  ChannelOccupancy->cd(4);
  ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGHitRateChOnline[0],"AP");
  //  ChannelOccupancy->GetCurrentFrame()->SetName("Normalized hit rate");
  for(int iCh=1;iCh<4;iCh++) {
    ChannelOccupancy->GetCurrentFrame()->DrawHisto(fGHitRateChOnline[iCh],"P same");
  }

  NA62VOnlineMonitorCanvas * Scalers= AddCanvasTab("Scalers");
  Scalers->Divide(2,2);

  TGraph **fGRateChLow  = recoSAC->GetGRateChLow ();
  TGraph **fGRateChHigh = recoSAC->GetGRateChHigh();

  Scalers->cd(1);
  Scalers->GetCurrentFrame()->DrawHisto(fGRateChLow[0],"AP");
  for(int iCh=1;iCh<4;iCh++) {
    Scalers->GetCurrentFrame()->DrawHisto(fGRateChLow[iCh],"P same");
  }
  Scalers->cd(2);
  Scalers->GetCurrentFrame()->DrawHisto(fGRateChHigh[0],"AP");
  for(int iCh=1;iCh<4;iCh++) {
    Scalers->GetCurrentFrame()->DrawHisto(fGRateChHigh[iCh],"P same");
  }

  TGraph **fGRateChLowNorm  = recoSAC->GetGRateChLowNorm ();
  TGraph **fGRateChHighNorm = recoSAC->GetGRateChHighNorm();

  Scalers->cd(3);
  Scalers->GetCurrentFrame()->DrawHisto(fGRateChLowNorm[0],"AP");
  for(int iCh=1;iCh<4;iCh++) {
    Scalers->GetCurrentFrame()->DrawHisto(fGRateChLowNorm[iCh],"P same");
  }
  Scalers->cd(4);
  Scalers->GetCurrentFrame()->DrawHisto(fGRateChHighNorm[0],"AP");
  for(int iCh=1;iCh<4;iCh++) {
    Scalers->GetCurrentFrame()->DrawHisto(fGRateChHighNorm[iCh],"P same");
  }

  NA62VOnlineMonitorCanvas * digiToT = AddCanvasTab("digiToT");
  digiToT->GetCurrentFrame()->DrawHisto(recoSAC->GetHDigiToTVsROChannel(),"COLZ");

  NA62VOnlineMonitorCanvas * hitToT = AddCanvasTab("HitToT");
  hitToT->Divide(3,2);
  hitToT->cd(1);
  hitToT->GetCurrentFrame()->DrawHisto(recoSAC->GetHHitToT(),"COLZ");
  hitToT->cd(2);
  hitToT->GetCurrentFrame()->DrawHisto(recoSAC->GetHHitToTAll(),"COLZ");
  hitToT->cd(3);
  hitToT->GetCurrentFrame()->DrawHisto(recoSAC->GetHHitToTOrdered(),"COLZ");
  hitToT->cd(4);
  hitToT->GetCurrentFrame()->DrawHisto(recoSAC->GetHHitToTLowThr(),"COLZ");
  hitToT->cd(5);
  hitToT->GetCurrentFrame()->DrawHisto(recoSAC->GetHHitToTHighThr(),"COLZ");
  hitToT->cd(6);
  hitToT->GetCurrentFrame()->DrawHisto(recoSAC->GetHHitToTLowThrIfHighThrExists(),"COLZ");

  NA62VOnlineMonitorCanvas * tmptab = AddCanvasTab("tmptab");
  tmptab->Divide(3,2);
  tmptab->cd(1);
  tmptab->GetCurrentFrame()->DrawHisto(recoSAC->GetHRecoHitTimeWrtReferenceVsROChannelNoT0(),"COLZ");

  //NA62VOnlineMonitorCanvas * leadingTimes = AddCanvasTab("LeadingTimes");
  //leadingTimes->Divide(2,2);
  //leadingTimes->cd(1);
  //leadingTimes->GetCurrentFrame()->DrawHisto(recoSAC->GetHETime(2));
  //leadingTimes->cd(2);
  //leadingTimes->GetCurrentFrame()->DrawHisto(recoSAC->GetHETime(1));
  //leadingTimes->cd(3);
  //leadingTimes->GetCurrentFrame()->DrawHisto(recoSAC->GetHETime(3));
  //leadingTimes->cd(4);
  //leadingTimes->GetCurrentFrame()->DrawHisto(recoSAC->GetHETime(4));

  //// Per event
  //NA62VOnlineMonitorCanvas * totlo = AddCanvasTab("ETOTLoThr");
  //NA62VOnlineMonitorCanvas * tothi = AddCanvasTab("ETOTHiThr");
  //totlo->Divide(2,2);
  //tothi->Divide(2,2);
  //totlo->cd(1); totlo->GetCurrentFrame()->DrawHisto(recoSAC->GetHEToT(2,0));
  //totlo->cd(2); totlo->GetCurrentFrame()->DrawHisto(recoSAC->GetHEToT(1,0));
  //totlo->cd(3); totlo->GetCurrentFrame()->DrawHisto(recoSAC->GetHEToT(3,0));
  //totlo->cd(4); totlo->GetCurrentFrame()->DrawHisto(recoSAC->GetHEToT(4,0));
  //tothi->cd(1); tothi->GetCurrentFrame()->DrawHisto(recoSAC->GetHEToT(2,1));
  //tothi->cd(2); tothi->GetCurrentFrame()->DrawHisto(recoSAC->GetHEToT(1,1));
  //tothi->cd(3); tothi->GetCurrentFrame()->DrawHisto(recoSAC->GetHEToT(3,1));
  //tothi->cd(4); tothi->GetCurrentFrame()->DrawHisto(recoSAC->GetHEToT(4,1));

  //// Per Burst
  //NA62VOnlineMonitorCanvas * btotlo = AddCanvasTab("BTOTLoThr");
  //NA62VOnlineMonitorCanvas * btothi = AddCanvasTab("BTOTHiThr");
  //btotlo->Divide(2,2);
  //btothi->Divide(2,2);
  //btotlo->cd(1); btotlo->GetCurrentFrame()->DrawHisto(recoSAC->GetHBToT(2,0),"COLZ");
  //btotlo->cd(2); btotlo->GetCurrentFrame()->DrawHisto(recoSAC->GetHBToT(1,0),"COLZ");
  //btotlo->cd(3); btotlo->GetCurrentFrame()->DrawHisto(recoSAC->GetHBToT(3,0),"COLZ");
  //btotlo->cd(4); btotlo->GetCurrentFrame()->DrawHisto(recoSAC->GetHBToT(4,0),"COLZ");
  //btothi->cd(1); btothi->GetCurrentFrame()->DrawHisto(recoSAC->GetHBToT(2,1),"COLZ");
  //btothi->cd(2); btothi->GetCurrentFrame()->DrawHisto(recoSAC->GetHBToT(1,1),"COLZ");
  //btothi->cd(3); btothi->GetCurrentFrame()->DrawHisto(recoSAC->GetHBToT(3,1),"COLZ");
  //btothi->cd(4); btothi->GetCurrentFrame()->DrawHisto(recoSAC->GetHBToT(4,1),"COLZ");

  //NA62VOnlineMonitorCanvas * AllDigislo = AddCanvasTab("BHitsLoThr");
  //NA62VOnlineMonitorCanvas * AllDigishi = AddCanvasTab("BHitsHiThr");
  //AllDigislo->Divide(2,2);
  //AllDigishi->Divide(2,2);
  //AllDigislo->cd(1); AllDigislo->GetCurrentFrame()->DrawHisto(recoSAC->GetGBAllHits(2,0));
  //AllDigislo->cd(2); AllDigislo->GetCurrentFrame()->DrawHisto(recoSAC->GetGBAllHits(1,0));
  //AllDigislo->cd(3); AllDigislo->GetCurrentFrame()->DrawHisto(recoSAC->GetGBAllHits(3,0));
  //AllDigislo->cd(4); AllDigislo->GetCurrentFrame()->DrawHisto(recoSAC->GetGBAllHits(4,0));
  //AllDigishi->cd(1); AllDigishi->GetCurrentFrame()->DrawHisto(recoSAC->GetGBAllHits(2,1));
  //AllDigishi->cd(2); AllDigishi->GetCurrentFrame()->DrawHisto(recoSAC->GetGBAllHits(1,1));
  //AllDigishi->cd(3); AllDigishi->GetCurrentFrame()->DrawHisto(recoSAC->GetGBAllHits(3,1));
  //AllDigishi->cd(4); AllDigishi->GetCurrentFrame()->DrawHisto(recoSAC->GetGBAllHits(4,1));

  //NA62VOnlineMonitorCanvas * nNotPairedRatio = AddCanvasTab("NotPairedDigis");
  //nNotPairedRatio->Divide(2,2);
  //nNotPairedRatio->cd(1); nNotPairedRatio->GetCurrentFrame()->DrawHisto(recoSAC->GetGBNotPaired(2,0));
  //nNotPairedRatio->cd(2); nNotPairedRatio->GetCurrentFrame()->DrawHisto(recoSAC->GetGBNotPaired(1,0));
  //nNotPairedRatio->cd(3); nNotPairedRatio->GetCurrentFrame()->DrawHisto(recoSAC->GetGBNotPaired(3,0));
  //nNotPairedRatio->cd(4); nNotPairedRatio->GetCurrentFrame()->DrawHisto(recoSAC->GetGBNotPaired(4,0));

  //NA62VOnlineMonitorCanvas * timeCorrection = AddCanvasTab("ChannelOccupancy");
  //ChannelOccupancy->Divide(4,4);
  //ChannelOccupancy->cd(1);
  //recoSAC->GetHistET0[chanId]
  //NA62VOnlineMonitorCanvas * ChannelOccupancy = AddCanvasTab("ChannelOccupancy");
  //ChannelOccupancy->Divide(4,4);
  //ChannelOccupancy->cd(1);
  //recoSAC->GetHECorrTime[chanId]
}

SACOnlineMonitor::~SACOnlineMonitor() {
  for(int ich=0;ich<4;ich++) {
    if(fGHitRateChOnline[ich]) delete fGHitRateChOnline[ich];
  }
}

Double_t SACOnlineMonitor::GetMaxGraphArray(TGraph **graphArray,Int_t nArr) {
  if(nArr < 1) return 0.;
  Int_t nPoints = graphArray[0]->GetN();
  Double_t *Rates[4];
  Double_t max = 0.;
  for(int iCh=0;iCh<nArr;iCh++) {
    Rates[iCh] = graphArray[iCh]->GetY();
    for(int ip = 0;ip<nPoints;ip++) {
      if(max < Rates[iCh][ip]) max =  Rates[iCh][ip];
    }
  }
  return max;
}


Double_t SACOnlineMonitor::GetMinGraphArray(TGraph **graphArray,Int_t nArr) {
  if(nArr < 1) return 0.;
  Int_t nPoints = graphArray[0]->GetN();
  Double_t *Rates[4];
  Double_t min = nPoints > 0 ?  (graphArray[0]->GetY())[0] : 0;
  for(int iCh=0;iCh<nArr;iCh++) {
    Rates[iCh] = graphArray[iCh]->GetY();
    for(int ip = 0;ip<nPoints;ip++) {
      if(min > Rates[iCh][ip]) min =  Rates[iCh][ip];
    }
  }
  return min;
}
Double_t SACOnlineMonitor::GetMaxGraphArray(TGraphErrors **graphArray,Int_t nArr) {
  if(nArr < 1) return 0.;
  Int_t nPoints = graphArray[0]->GetN();
  Double_t *Rates[4];
  Double_t max = 0.;
  for(int iCh=0;iCh<nArr;iCh++) {
    Rates[iCh] = graphArray[iCh]->GetY();
    for(int ip = 0;ip<nPoints;ip++) {
      if(max < Rates[iCh][ip]) max =  Rates[iCh][ip];
    }
  }
  return max;
}


Double_t SACOnlineMonitor::GetMinGraphArray(TGraphErrors **graphArray,Int_t nArr) {
  if(nArr < 1) return 0.;
  Int_t nPoints = graphArray[0]->GetN();
  Double_t *Rates[4];
  Double_t min = nPoints > 0 ?  (graphArray[0]->GetY())[0] : 0;
  for(int iCh=0;iCh<nArr;iCh++) {
    Rates[iCh] = graphArray[iCh]->GetY();
    for(int ip = 0;ip<nPoints;ip++) {
      if(min > Rates[iCh][ip]) min =  Rates[iCh][ip];
    }
  }
  return min;
}
void SACOnlineMonitor::Update(Int_t BurstID) {

  for(int iChan=0;iChan<4; iChan ++) {
    fGHitRateChOnline [iChan]->SetPoint(fGHitRateChOnline [iChan]->GetN(),BurstID,0);
  }

  SACReconstruction* const recoSAC=static_cast<SACReconstruction*>(fReco);
  Double_t maxScale = 1.1;
  Double_t minScale = 0.8;
  Int_t nBurstsToShow=200;
  const Int_t firstAboveZero=recoSAC->GetHRateRun()->FindFirstBinAbove()-1;
  const Int_t showBinEnd=BurstID+10;
  Int_t showBinBegin=(
      BurstID-nBurstsToShow>firstAboveZero?
      BurstID-nBurstsToShow-1:
      firstAboveZero);
  if(showBinBegin<0)showBinBegin=0;
  recoSAC->GetHRateRun()->GetXaxis()->
    SetRangeUser(showBinBegin,showBinEnd);

  TGraph **fGRateChLow  = recoSAC->GetGRateChLow ();
  TGraph **fGRateChHigh = recoSAC->GetGRateChHigh();

  Double_t max = GetMaxGraphArray(fGRateChLow,4);
  Double_t min = GetMinGraphArray(fGRateChHigh,4);

  fGRateChLow[0]->SetMaximum(max*maxScale);
  fGRateChLow[0]->SetMinimum(min*minScale);
  fGRateChHigh[0]->SetMaximum(max*maxScale);
  fGRateChHigh[0]->SetMinimum(min*minScale);

  TGraphErrors **fGHitRateChNorm  = recoSAC->GetGHitRateChNorm();
  max = GetMaxGraphArray(fGHitRateChNorm,4);
  min = GetMinGraphArray(fGHitRateChNorm,4);
  fGHitRateChNorm[0]->SetMaximum(max*maxScale);
  fGHitRateChNorm[0]->SetMinimum(min*minScale);

  
  TGraph **fGRateChLowNorm  = recoSAC->GetGRateChLowNorm ();
  TGraph **fGRateChHighNorm = recoSAC->GetGRateChHighNorm();
  max = GetMaxGraphArray(fGRateChLowNorm,4);
  min = GetMinGraphArray(fGRateChHighNorm,4);
  fGRateChLowNorm[0]->SetMaximum(max*maxScale);
  fGRateChLowNorm[0]->SetMinimum(min*minScale);
  fGRateChHighNorm[0]->SetMaximum(max*maxScale);
  fGRateChHighNorm[0]->SetMinimum(min*minScale);

  Int_t *nHits = recoSAC->GetNHitsBurst();
  for(int iChan=0;iChan<4; iChan ++) {
    fGHitRateChOnline [iChan]->SetPoint(fGHitRateChOnline [iChan]->GetN()-1,BurstID,nHits[iChan]);
    fGHitRateChOnline [iChan]->SetPointError(fGHitRateChOnline [iChan]->GetN()-1,0,sqrt(nHits[iChan]));
  }
  max = GetMaxGraphArray(fGHitRateChOnline,4);
  min = GetMinGraphArray(fGHitRateChOnline,4);
  fGHitRateChOnline[0]->SetMaximum(max*maxScale);
  fGHitRateChOnline[0]->SetMinimum(min*minScale);
    
  NA62VOnlineMonitor::Update(BurstID);
}