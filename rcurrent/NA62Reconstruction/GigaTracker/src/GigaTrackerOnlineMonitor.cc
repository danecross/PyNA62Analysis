// ---------------------------------------------------------------
// History:
//
// Modified by Alina KLeimenova (alina.kleimenova@cern.ch) Sep. 2017
// - add table to the OM
//
// Modified by Bob Velghe (bob.velghe@cern.ch) Aug. 2014
//  - First layout of the online monitor. To be refined.
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "NA62Global.hh"

#include "GigaTrackerOnlineMonitor.hh"
#include "GigaTrackerReconstruction.hh"

GigaTrackerOnlineMonitor::GigaTrackerOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco, "GigaTracker") {
  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void GigaTrackerOnlineMonitor::CreateShifterModeTabs(){

  //ToT
  for(int gtk(0); gtk<3;gtk++){
    NA62VOnlineMonitorCanvas * c = AddCanvasTab(Form("GTK%d",gtk+1));
    c->Divide(3,2);
    
    c->cd(1);
    gPad->SetLogz(1);
    static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMap(gtk)->SetMaximum(100);
    c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMap(gtk),"COLZ");
    c->cd(2);
    c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetAbsTimeProfile(gtk));
    c->cd(3);
    gPad->SetLogz(1);
    c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetDtOverburst(gtk),"COLZ");
    
    for(int i = 10; i < 11; i++) {

      c->cd(4);
      if(i==0) c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetToT(gtk,i));
      else     c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetToT(gtk,i),"SAME");
      
      c->cd(5);
      if(i==0) c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetTime(gtk,i));
      else     c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetTime(gtk,i),"SAME");

      c->cd(6);
      if(i==0) c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetNHitsPerTrigger(gtk,i));
      else     c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetNHitsPerTrigger(gtk,i),"SAME");
      
    }  

    NA62VOnlineMonitorCanvas * cn = AddCanvasTab(Form("Noise_GTK%d",gtk+1));
    cn->Divide(3,2);
    
    cn->cd(1);
    cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMapOOB(gtk),"COLZ");
    cn->cd(2);
    DrawTable(gtk);
    //cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetAbsTimeProfileOOB(gtk));
    cn->cd(3);
    cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetAbsTimeProfileZoomOOB(gtk));
    
    
    for(int i = 10; i < 11; i++) {
      cn->cd(4);
      if(i==0) cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetToTOOB(gtk,i));
      else     cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetToTOOB(gtk,i),"SAME");
      
      cn->cd(5);
      if(i==0) cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetTimeOOB(gtk,i));
      else     cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetTimeOOB(gtk,i),"SAME");

      cn->cd(6);
      if(i==0) cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetNHitsPerTriggerOOB(gtk,i));
      else     cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetNHitsPerTriggerOOB(gtk,i),"SAME");
      
    }  

  }
}

void GigaTrackerOnlineMonitor::CreateExpertModeTabs(){

  //ToT
  for(int gtk(0); gtk<3;gtk++){
    NA62VOnlineMonitorCanvas * c = AddCanvasTab(Form("GTK%d",gtk+1));
    c->Divide(3,2);
    
    c->cd(1);
    gPad->SetLogz(1);
    static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMap(gtk)->SetMaximum(100);
    c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMap(gtk),"COLZ");
    c->cd(2);
    c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetAbsTimeProfile(gtk));
    c->cd(3);
    gPad->SetLogz(1);
    c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetDtOverburst(gtk),"COLZ");
    
    for(int i = 10; i < 11; i++) {

      c->cd(4);
      if(i==0) c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetToT(gtk,i));
      else     c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetToT(gtk,i),"SAME");
      
      c->cd(5);
      if(i==0) c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetTime(gtk,i));
      else     c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetTime(gtk,i),"SAME");

      c->cd(6);
      if(i==0) c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetNHitsPerTrigger(gtk,i));
      else     c->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetNHitsPerTrigger(gtk,i),"SAME");
      
    }  

    NA62VOnlineMonitorCanvas * cn = AddCanvasTab(Form("Noise_GTK%d",gtk+1));
    cn->Divide(3,2);
    
    cn->cd(1);
    cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMapOOB(gtk),"COLZ");
    cn->cd(2);
    DrawTable(gtk);
    //cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetAbsTimeProfileOOB(gtk));
    cn->cd(3);
    cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetAbsTimeProfileZoomOOB(gtk));
    
    
    for(int i = 10; i < 11; i++) {
      cn->cd(4);
      if(i==0) cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetToTOOB(gtk,i));
      else     cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetToTOOB(gtk,i),"SAME");
      
      cn->cd(5);
      if(i==0) cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetTimeOOB(gtk,i));
      else     cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetTimeOOB(gtk,i),"SAME");

      cn->cd(6);
      if(i==0) cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetNHitsPerTriggerOOB(gtk,i));
      else     cn->GetCurrentFrame()->DrawHisto(static_cast<GigaTrackerReconstruction*>(fReco)->GetNHitsPerTriggerOOB(gtk,i),"SAME");
      
    }  

  }
}

void GigaTrackerOnlineMonitor::Update(Int_t BurstID){
  /// Setting the axis scale of GTK1 and GTK2 hit map
  /// based on GTK3 maximum value
  double max = static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMap(2)->GetMaximum();
  static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMap(1)->SetMaximum(max);
  static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMap(0)->SetMaximum(max);
  /// Threshold for noisy pixels:
  Double_t NMaxHits = static_cast<GigaTrackerReconstruction*>(fReco)->GetNoiseThreshold();
  /// Cleaning the container with noisy pixels info:
  for (int i=0;i<3;i++) fContainer[i].clear();
  
  //Filling Container
  for (int iS=0; iS<3; ++iS){
    for(int i=0; i < static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMapOOB(iS)->GetNbinsX(); i++){
      for(int j=0; j < static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMapOOB(iS)->GetNbinsY(); j++){
        int bin = static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMapOOB(iS)->GetBin(i,j);
        if (static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMapOOB(iS)->GetBinContent(bin) <= 0 ) continue;
        else {
	  std::vector<int> v;
	  v.push_back(static_cast<GigaTrackerReconstruction*>(fReco)->GetHitMapOOB(iS)->GetBinContent(bin));
	  v.push_back(i);
	  v.push_back(j);
	  fContainer[iS].push_back(v);
	}
      }
    }
    sort (fContainer[iS].begin(), fContainer[iS].end(), SortByFirstCol());                       
  }
                        
  for (int gtk(0);gtk<3;gtk++){
    for (int i=0; i<5; i++){
      for(int j=0; j<6; j++){
        fTableCont[gtk][i][j]->Clear();
        switch (j){
        case 0:
          {
            fTableCont[gtk][i][j]->AddText(Form("(%d,%d)",
                                                (GetPixelInfo(gtk,i,1)-1),
                                                ( GetPixelInfo(gtk,i,2)-1)));
            if (GetPixelInfo(gtk,i,0)>NMaxHits){
              fTableCont[gtk][i][j]->SetTextColor(kRed);
            }
            break;
          }
        case 1:
          {
            fTableCont[gtk][i][j]->AddText(Form("%d",(GetPixelInfo(gtk,i,0))));
            if (GetPixelInfo(gtk,i,0)>NMaxHits){
              fTableCont[gtk][i][j]->SetTextColor(kRed);
            }
            break;
          }
        case 2:
          {
            fTableCont[gtk][i][j]->AddText(Form("%d",
                                                GetPixelID(1,
                                                           GetPixelInfo(gtk,i,1),
                                                           GetPixelInfo(gtk,i,2))));
            if (GetPixelInfo(gtk,i,0)>NMaxHits){
              fTableCont[gtk][i][j]->SetTextColor(kRed);
            }
            break;
          }
        case 3:
          {
            fTableCont[gtk][i][j]->AddText(Form("%d",
                                                GetPixelID(2,
                                                           GetPixelInfo(gtk,i,1),
                                                           GetPixelInfo(gtk,i,2))));
            if (GetPixelInfo(gtk,i,0)>NMaxHits){
              fTableCont[gtk][i][j]->SetTextColor(kRed);
            }
            break;
          }
        case 4:
          {
            fTableCont[gtk][i][j]->AddText(Form("%d",
                                                GetPixelID(3,
                                                           GetPixelInfo(gtk,i,1),
                                                           GetPixelInfo(gtk,i,2))));
            if (GetPixelInfo(gtk,i,0)>NMaxHits){
              fTableCont[gtk][i][j]->SetTextColor(kRed);
            }
            break;
          }
        case 5:
          {
            fTableCont[gtk][i][j]->AddText(Form("%d",
                                                GetPixelID(4,
                                                           GetPixelInfo(gtk,i,1),
                                                           GetPixelInfo(gtk,i,2))));
            if (GetPixelInfo(gtk,i,0)>NMaxHits){
              fTableCont[gtk][i][j]->SetTextColor(kRed);
            }
            break;
          }
        }
      }
    }
  }

  NA62VOnlineMonitor::Update(BurstID) ;
}

GigaTrackerOnlineMonitor::~GigaTrackerOnlineMonitor() {}

int GigaTrackerOnlineMonitor::GetPixelInfo(int station, int i, int j){
  if(station <0 || station >2) return 0;
  if (fContainer[station].empty()) return 0;
  UInt_t k = (i+1);
  if (fContainer[station].size()< k) return 0;
  return fContainer[station][i][j];
}

void GigaTrackerOnlineMonitor::DrawTable(int gtk){
  TPaveText *fTable = new TPaveText(0.0,0.0,1.,1.,"NB,NDC");
  fTable->SetFillColor(kWhite);
    
  fTable->AddLine(fX1,fY2,fX2,fY2);
  fTable->AddLine(fX1,fY1,fX2,fY1);
  fTable->AddLine(fX2,fY1,fX2,fY2);
  fTable->AddLine(fX1,fY1,fX1,fY2);

  double a=fX1;
  double b=fY1;
  for(int i=0;i<6;i++){
    a+=(fX2-fX1)/6.;
    fTable->AddLine(a,fY1,a,fY2);
    b+=(fY2-fY1)/6.;
    fTable->AddLine(fX1,b,fX2,b);
  }
  fTable->Draw("");

  TPaveText *fCoordinatesCol = new TPaveText(0.08,0.65,0.17,0.74,"NB,NDC");
  fCoordinatesCol->SetFillColor(kWhite);
  fCoordinatesCol->AddText("(x,y)");
  fCoordinatesCol->Draw("");

  TPaveText *fNHitsCol = new TPaveText(0.24,0.65,0.31,0.74,"NB,NDC");
  fNHitsCol->SetFillColor(kWhite);
  fNHitsCol->AddText("Hits");
  fNHitsCol->Draw("");
    
  TPaveText *fChipIDCol = new TPaveText(0.36,0.65,0.49,0.74,"NB,NDC");
  fChipIDCol->SetFillColor(kWhite);
  fChipIDCol->AddText("Chip ID");
  fChipIDCol->Draw("");

  TPaveText *fColPairCol = new TPaveText(0.51,0.65,0.64,0.74,"NB,NDC");
  fColPairCol->SetFillColor(kWhite);
  fColPairCol->AddText("Col pair");
  fColPairCol->Draw("");

  TPaveText *fSubColCol = new TPaveText(0.66,0.65,0.79,0.74,"NB,NDC");
  fSubColCol->SetFillColor(kWhite);
  fSubColCol->AddText("Sub col");
  fSubColCol->Draw("");

  TPaveText *fRowCol = new TPaveText(0.84,0.65,0.91,0.74,"NB,NDC");
  fRowCol->SetFillColor(kWhite);
  fRowCol->AddText("Row");
  fRowCol->Draw("");
    
  b=fY2-(fY2-fY1)/3.;
  a=fX1;
  double d=fY2-(fY2-fY1)/6.;
  double e=fX1+(fX2-fX1)/6.;
    
  for (int i=0; i<5; i++){
    for(int j=0; j<6; j++){
      fTableCont[gtk][i][j] = new TPaveText(a+0.01,b+0.01,e-0.01,d-0.01,"NB,NDC");
      fTableCont[gtk][i][j]->SetFillColor(kWhite);
      fTableCont[gtk][i][j]->Draw("");            
      a+=(fX2-fX1)/6.;
      e+=(fX2-fX1)/6.;
    }
    a=fX1;
    e=fX1+(fX2-fX1)/6.;
    d-=(fY2-fY1)/6.;
    b-=(fY2-fY1)/6.;
  }
    
}

int GigaTrackerOnlineMonitor::GetPixelID(int ID,int x,int y){
  x--;
  y--;
  switch (ID){
  case 1://Chip ID
    {
      if (y<45) return x/40;
      else return (5+x/40);
      break;
    }
  case 2://colp
    {
      if (y<45) return ((x - x/40 * 40)/2);
      else return (19 - (x - x/40 * 40)/2);       
      break;
    }
  case 3://subcol
    {
      int mod = (x - x/40 * 40);
      if (y<45) return (mod - mod/2 * 2);
      else return (1 - (mod - mod/2 * 2));       
      break;
    }
  case 4://row
    {
      if (y<45) return y;
      else return (89 - y);      
      break;
    }
  }
  return 0;
}
