#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "GigaTrackerPixelT0ChipTW.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"

#include "TF1.h"
#include <TStyle.h>
#include "TROOT.h"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class GigaTrackerPixelT0ChipTW
/// \Brief
/// GTK time correction (pixel t0 and chip time-walk)
/// \EndBrief
/// \Detailed
/// Analyser produce GTK-KTAG time difference vs ToT for every pixel pixel.
/// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)
/// \EndDetailed


GigaTrackerPixelT0ChipTW::GigaTrackerPixelT0ChipTW(Core::BaseAnalysis *ba) : Analyzer(ba, "GigaTrackerPixelT0ChipTW")
{

  fGigaTrackerEvt = new TRecoGigaTrackerEvent;
  RequestTree("GigaTracker",fGigaTrackerEvt);

  fCedarEvt = new TRecoCedarEvent;
  RequestTree("Cedar",fCedarEvt);

  AddParam("Chip", &fChip, -1);  // chip id or -1 to treat all chips at once
  AddParam("Station", &fStation, -1);  // chip id or -1 to treat all chips at once

  fDirName = "GigaTrackerPixelT0ChipTW";

}

void GigaTrackerPixelT0ChipTW::InitOutput(){
}

void GigaTrackerPixelT0ChipTW::InitHist(){
  Bool_t all = true;

  if (GetIsTree()){
    int nBins = 410;
    double *bins = new double[nBins+1];
    for(int i(0);i<(nBins+1);i++) {
      bins[i]= i*ClockPeriod/256.;
      if (i > 299) bins[i] = i*ClockPeriod/128. - 299.*ClockPeriod/256.;
      if (i == 410) bins[i] = 205.0;
    }

    double *dtbins = new double[261];
    for(int i(0);i<261;i++) dtbins[i] = -130.*ClockPeriod/256. + i*ClockPeriod/256.;//-5.+i*(2*5.)/200.;

    double *pixbins = new double[1801];
    for(int i(0);i<1801;i++)  pixbins[i] = i;

    BookHisto("h_dt_cedar", new TH1F("h_dt_cedar","h_dt_cedar", 500, -5., 5.));
    for(int s(0);s<3;s++){
      for(int c(0);c<10;c++){
        if( (c!=fChip) && (fChip != -1) ) continue;
        if ((s!=fStation) && (fStation !=-1)) continue;
        BookHisto(Form("h_ktag_gtk%dc%d_ToTdtPix",s+1,c), new TH3F(Form("h_ktag_gtk%dc%d_dtToTPix",s+1,c),Form("h_gtk%dc%d_dtToTPix",s+1,c), nBins, bins, 260, dtbins,1800,pixbins));
      }
    }
    delete[] bins;
    delete[] dtbins;
    delete[] pixbins;
    return;
  }

  fHdtKTAG = static_cast<TH1F*>(RequestHistogram(fDirName, "h_dt_cedar", all));

  for(int s(0);s<3;s++){
    for(int c(0);c<10;c++){
      if( (c!=fChip) && (fChip != -1) ) continue;
      if ((s!=fStation) && (fStation !=-1)) continue;
      fHdtKTAG_ToT_Pix[s][c] = static_cast<TH3F*>(RequestHistogram(fDirName, Form("h_ktag_gtk%dc%d_dtToTPix",s+1,c), all));
    }
  }
}

void GigaTrackerPixelT0ChipTW::DefineMCSimple(){
}

void GigaTrackerPixelT0ChipTW::StartOfRunUser(){
}

void GigaTrackerPixelT0ChipTW::StartOfBurstUser(){
}

void GigaTrackerPixelT0ChipTW::ProcessSpecialTriggerUser(int, unsigned int){
}

void GigaTrackerPixelT0ChipTW::Process(Int_t){
  if (!GetIsTree()) return;

  fGigaTrackerEvt = GetEvent<TRecoGigaTrackerEvent>();
  fCedarEvt  = GetEvent<TRecoCedarEvent>();
  TRecoGigaTrackerHit* GigaTrackerHit;
  TRecoCedarCandidate* CedarCand;
  EventHeader* rawHeader = GetEventHeader();
  double triggertime = rawHeader->GetFineTime()*ClockPeriod/256.;

  // build time difference using ktag
  for(int iK(0); iK<fCedarEvt->GetNCandidates();iK++){
    CedarCand = static_cast<TRecoCedarCandidate*>(fCedarEvt->GetCandidate(iK));
    if(CedarCand->GetNSectors()<4 || CedarCand->GetNHits()>30) continue;
    double tref = CedarCand->GetTime();
    FillHisto("h_dt_cedar",tref-triggertime);

    int nHits =fGigaTrackerEvt->GetNHits();
    for (int iH(0); iH<nHits; iH++){
      GigaTrackerHit = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvt->GetHit(iH));
      if (GigaTrackerHit->GetIsPileUpHit()) continue;
      int s = GigaTrackerHit->GetStationNo();
      int c = GigaTrackerHit->GetChipID();
      int p = (GigaTrackerHit->GetColumn()%40)*45 + GigaTrackerHit->GetRow()%45;
      float rawDt = GigaTrackerHit->GetRawTime() - tref;
      float corDt = GigaTrackerHit->GetTime() - tref;
      float tot =  GigaTrackerHit->GetToT();
      if( (c!=fChip) && (fChip != -1) ) continue;
      if( abs(corDt)>4) continue; //use corrected gtk time to make selection to reduce background
      FillHisto(Form("h_ktag_gtk%dc%d_ToTdtPix",s+1,c),tot,rawDt,p);
    }

  }

}

void GigaTrackerPixelT0ChipTW::PostProcess(){

}

void GigaTrackerPixelT0ChipTW::EndOfBurstUser(){
}

void GigaTrackerPixelT0ChipTW::EndOfRunUser(){

}

void GigaTrackerPixelT0ChipTW::EndOfJobUser(){

  if (GetIsTree()) {
    SaveAllPlots();
    return;
  }
  gErrorIgnoreLevel = kFatal;
  gStyle->SetPaintTextFormat("3.1f");

  for (int gtk(0); gtk<3; ++gtk){
    for (int chip(0); chip<10; ++chip){
      if( (chip!=fChip) && (fChip != -1) ) continue;
      if ((gtk!=fStation) && (fStation !=-1)) continue;
      if (!fHdtKTAG_ToT_Pix[gtk][chip]) continue;
      Int_t nBins = fHdtKTAG_ToT_Pix[gtk][chip]->GetXaxis()->GetNbins();
      int nStep(2); // need to be 2

      int nX = 40;
      int nY = 45;
      int x0 = (chip%5) * 40;
      int y0 = (chip/5) * 45;

      float *tw = new float[nBins];
      float *t0 = new float[nBins];
      for (int i(0); i<nBins; i++){
        tw[i]=0;
        t0[i]=0;
      }
      float vt0[40][45];
      bool masks[40][45];
      for(int i(0); i<40;i++){
        for(int j(0); j<45;j++){
          vt0[i][j]=0;
          masks[i][j] = 0;
        }
      }

      double  totT0 = 16;
      TH2D* htw1 = NULL;

      std::cout<< user_normal() << "Identifying Noisy and Masked Pixels"<<std::endl;
      CheckPixels(fHdtKTAG_ToT_Pix[gtk][chip],gtk,chip,masks);

      for(int step(0); step<nStep;step++){
        int iH = 0;
        TH2D* ht0  = new TH2D (Form("ht0-%d", step),
                               Form("T0 evaluated at ToT = %f ns;X [pixel];Y [pixel]",totT0),40,x0,x0+40,45,y0,y0+45);
        TH2D* ht0s = new TH2D (Form("ht0s-%d", step),
                               Form("T0 evaluated at ToT = %f ns - Smoothed;X [pixel];Y [pixel]",totT0),40,x0,x0+40,45,y0,y0+45);

        cout<< user_normal() << "Extracting t0 step "<<step<<endl;
        double mean(0);
        cout<< user_normal() << "\r  Evaluate t0\r"<<endl;
        for(int iX(0);iX<nX;iX++ ){
          for(int iY(0);iY<nY;iY++ ){
            if(masks[iX][iY] == 1) {
              cout<< user_normal() << "    Skip Pixel "<<iX<<" - "<<iY<<endl;
              if(iH!=0) iH++;
              continue;
            }
            cout<< user_normal() << "   > Pixel "<<iH+1<<" / "<< nX*nY <<" \r"; std::cout.flush();

            fHdtKTAG_ToT_Pix[gtk][chip]->GetZaxis()->SetRange(iX*45+iY+1, iX*45+iY+1);
            TH2D* hh = (TH2D*) fHdtKTAG_ToT_Pix[gtk][chip]->Project3D("yx");
            hh->SetName(Form("h_dt_tot_x%d_y%d",iX,iY));
            TH2D* h = (TH2D*) hh->Clone();

            if(step > 0 )  {
              ApplyTW(h,tw);
              totT0 = 0;
              ht0->SetTitle("T0 evaluated with all ToT");
            }

            if(step==0) GetTZero(h,mean,totT0, string(Form("t0-gtk%d-ch%d-%d/pix", gtk, chip, step)));//f->cd();
            else        GetTZero(h,mean,-1, string(Form("t0-gtk%d-ch%d-%d/pix", gtk, chip, step)));//f->cd();

            vt0[iX][iY] = mean;
            for(int i(0);i<nBins;i++) t0[i]=mean;
            ApplyTW(hh,t0);
            if (iH==0) {
              htw1=(TH2D*)hh->Clone();
              htw1->SetTitle(Form("GTK %d Chip %d| All Pixels Pixel T0 + Global TW",gtk,chip));
              htw1->SetName(Form("dt_tot_all_%d",step));
            }
            else htw1->Add(hh);

            ht0->Fill(iX+x0,iY+y0,vt0[iX][iY]);
            iH++;
            hh->Delete();
            h->Delete();
          }
        }
        cout<< user_normal() << "  Smooth t0"<<endl;
        SmoothT0(vt0,masks,string(Form("t0-gtk%d-ch%d-%d/check",gtk, chip, step)),string(Form("t0-gtk%d-ch%d-%d/pix",gtk, chip, step)));
        //SmoothT0(vt0,masks);
        for(int iX(0);iX<nX;iX++ ){
          for(int iY(0);iY<nY;iY++ ){
            ht0s->Fill(iX+x0,iY+y0,vt0[iX][iY]);
          }
        }
        gDirectory->cd(Form("t0-gtk%d-ch%d-%d",gtk, chip, step));
        ht0->Write();
        ht0s->Write();
        ht0->Delete();
        ht0s->Delete();
        gDirectory->cd("../");

        cout<< user_normal() << "--> done!"<<endl;
        cout<< user_normal() << "Extracting time walk"<<endl;
        //SmoothTWHisto(htw1);
        GetTW(htw1, tw,TMath::Max(int(htw1->GetEntries()/1e4),5000),string(Form("tw-gtk%d-ch%d-%d",gtk, chip, step)),1);
        //ApplyTW(htw1,tw);
        cout<< user_normal() << "--> done!"<<endl;
        //cout<<"Extracting Time Resolution"<<endl;
        //GetTResoMean(htw1,reso,mean,fout,"reso");//f->cd();
        //fprintf(fReso,"Indicative Time Reso (Pixel T0 + Global TW): %f\n",reso);
        //cout<<"--> "<<  reso*1000 <<" ps"<<endl;
        //cout<<"--> done!"<<endl;

        if(step < (nStep-1) ) htw1->Delete();
      }
      // t0 and tw standardisation
      // tw at ToT=16ns is 0
      // average pixel t0 is 0
      cout<< user_normal() << "Normalised Correction"<<endl;
      int bin0 = htw1->GetXaxis()->FindBin(16.)-1;
      double tw0 = tw[bin0];
      cout<< user_normal() << " TW at 16 set to 0: offset by "<<tw0<<endl;
      float twc[410];
      float tw0s[410];
      for(int i(1); i<nBins;i++) {
        tw[i]=tw[i]-tw0;
        tw0s[i]=tw0;
        twc[i] = htw1->GetXaxis()->GetBinCenter(i+1);
      }
      ApplyTW(htw1,tw0s);

      TGraph * gr = new TGraph (nBins,twc,tw);
      if(gDirectory->GetDirectory(Form("tw-gtk%d-ch%d-final_gr", gtk, chip)) == NULL) {
        gDirectory->mkdir(Form("tw-gtk%d-ch%d-final_gr", gtk, chip));
      }
      gDirectory->cd(Form("tw-gtk%d-ch%d-final_gr", gtk, chip));
      gr->Write();
      gDirectory->cd("../");

      if(gDirectory->GetDirectory(Form("tw-gtk%d-ch%d-final", gtk, chip)) == NULL) {
        gDirectory->mkdir(Form("tw-gtk%d-ch%d-final", gtk, chip));
      }
      gDirectory->cd(Form("tw-gtk%d-ch%d-final", gtk, chip));
      htw1->Write();
      gDirectory->cd("../");
      //htw1->Delete();

      double avT0(0);
      for(int i(0); i<40;i++){
        for(int j(0); j<45;j++){
          avT0 += vt0[i][j];
        }
      }
      avT0 /= (40*45);
      TH2D* ht0final = new TH2D(Form("t0-gtk%d-chip%dfinal", gtk, chip),"T0 Normalised;X [pixel];Y [pixel]",40,x0,x0+40,45,y0,y0+45);
      for(int i(0); i<40;i++){
        for(int j(0); j<45;j++){
          vt0[i][j] -= avT0;
          ht0final->Fill(i+x0,j+y0,vt0[i][j]);
        }
      }
      ht0final->Write();
      ht0final->Delete();
      //Save t0 and tw
      ofstream TWBinning;
      ofstream TimeWalk;
      ofstream T0Corr;
      TWBinning.open(Form("GigaTracker-time-walk_binning_%06d.dat",GetRunID()));
      TimeWalk.open(Form("GigaTracker-time-walk_gtk%d-chip%d_%06d.dat", gtk,chip,GetRunID()));
      T0Corr.open(Form("GigaTracker-t-zero_gtk%d-chip%d_%06d.dat",gtk,chip,GetRunID()));

      TWBinning << Form("%5.5f", htw1->GetXaxis()->GetBinLowEdge(1)) << std::endl;
      TimeWalk << Form("%5.5f", tw[0]) << std::endl;
      for(int i(1); i<nBins;i++) {
        //if( fabs(tw[i-1]-tw[i])<0.005 ) continue;
        TWBinning << Form("%5.5f", htw1->GetXaxis()->GetBinUpEdge(i)) << std::endl;
        TimeWalk << Form("%5.5f", tw[i]) << std::endl;
      }
      TWBinning << Form("%5.5f", htw1->GetXaxis()->GetBinUpEdge(nBins)) << std::endl;
      for(int iX(x0);iX<(x0+nX);iX++ ){
        for(int iY(y0);iY<y0+nY;iY++ ){
          int uid = iX + iY*200; //uid goes from 0->17999
          T0Corr << Form("%d %5.6f",uid, vt0[iX-x0][iY-y0]) << std::endl;
        }
      }
      TWBinning.close();
      TimeWalk.close();
      T0Corr.close();
      delete [] t0;
      delete [] tw;
    }
  }
  return;
}

void GigaTrackerPixelT0ChipTW::DrawPlot(){
}

GigaTrackerPixelT0ChipTW::~GigaTrackerPixelT0ChipTW(){
}

int GigaTrackerPixelT0ChipTW::GetUID(int ix, int iy, int x0, int y0){
  return (iy-y0)*40+(ix-x0)%40;
}


void GigaTrackerPixelT0ChipTW::CheckPixels(TH3F* h, int s, int c, bool mask[40][45]){
  vector<double> PixCounts;
  PixCounts.reserve(1800);
  TH2F* hMap = new TH2F(Form("MapGTK%dChip%d",s, c), Form("Hit Map GTK%d Chip%d; X [pixel]; y [pixel]",s,c),40,0,40,45,0,45);
  TH2F* hMapNoise = new TH2F(Form("MapNoiseGTK%dChip%d",s, c), Form("Hit Map GTK%d Chip%d; X [pixel]; y [pixel]",s,c),40,0,40,45,0,45);

  for(int iX(0);iX<40;iX++ ){
    for(int iY(0);iY<45;iY++ ){
      double NCounts = h->Integral(0,-1,0,-1, iX*45+iY+1, iX*45+iY+1 );
      hMap->Fill(iX,iY,NCounts);
      PixCounts.push_back(NCounts);
    }
  }

  std::sort(PixCounts.begin(), PixCounts.end());
  // cppcheck-suppress containerOutOfBoundsIndexExpression
  double UpThr = PixCounts[0.5*PixCounts.size()];
  // cppcheck-suppress containerOutOfBoundsIndexExpression
  double LowThr = PixCounts[0.1*PixCounts.size()];
  for(int iX(0);iX<40;iX++ ){
    for(int iY(0);iY<45;iY++ ){
      if(hMap->GetBinContent(iX+1,iY+1) < 10*UpThr &&
         hMap->GetBinContent(iX+1,iY+1) >0.1*LowThr) continue;
      mask[iX][iY]=1;
      hMapNoise->Fill(iX,iY);
      if(hMap->GetBinContent(iX+1,iY+1) > 10*UpThr)   cout<< user_normal() << "    pixel "<<iX<<" - "<<iY<<" is noisy"<<endl;
      if(hMap->GetBinContent(iX+1,iY+1) < 0.1*LowThr)  cout<< user_normal() << "    pixel "<<iX<<" - "<<iY<<" is disconnected"<<endl;
    }
  }

  if(gDirectory->GetDirectory("HitMaps") == NULL) {
    gDirectory->mkdir("HitMaps");
  }
  gDirectory->cd("HitMaps");
  hMap->Write();
  hMapNoise->Write();
  hMap->Delete();
  hMapNoise->Delete();
  gDirectory->cd("../");
  cout<< user_normal() << "--> done!"<<endl;
  return;
}

int GigaTrackerPixelT0ChipTW::GetTZero(TH2D* h, double& t0, double tot, string repo){
  TH1D* hProj;
  if(tot>0){
    int ToTBin = h->GetXaxis()->FindBin(tot);
    hProj = h->ProjectionY(Form("t0_%s",h->GetName()),ToTBin,ToTBin+1);
  }
  else {
    int ToTBin0 = h->GetXaxis()->FindBin(5); // why?
    int ToTBin1 = h->GetXaxis()->FindBin(19); // why?
    hProj = h->ProjectionY(Form("t0_%s",h->GetName()),ToTBin0,ToTBin1);
  }
  //SmoothT0Histo(hProj);
  double center = hProj->GetXaxis()->GetBinCenter(hProj->GetMaximumBin());
  TF1 * fFt0 = new TF1("fFt0","gaus",-20,20);

  fFt0->SetParameter(1,center);
  fFt0->SetParameter(2,0.240);
  hProj->Fit(fFt0,"Q","",center-0.4,center+0.4);
  t0 =  fFt0->GetParameter(1);

  hProj->SetName(Form("t0_%s", h->GetName()));
  hProj->SetTitle(Form("%s center %2.3f",hProj->GetTitle(),center));

  if(gDirectory->GetDirectory(repo.c_str()) == NULL) {
    gDirectory->mkdir(repo.c_str());
  }
  gDirectory->cd(repo.c_str());
  hProj->Write();
  hProj->Delete();
  fFt0->Delete();

  std::string directory="../";
  const char * pch;
  pch=strchr(repo.c_str(),'/');
  while (pch!=NULL){
    directory = directory+"../";
    pch=strchr(pch+1,'/');
  }
  gDirectory->cd(directory.c_str());
  return 1;
}

void GigaTrackerPixelT0ChipTW::ApplyTW(TH2D* h, float* tw){
  TH2D* hh= (TH2D*) h->Clone();
  h->Reset();
  for (int iX(1); iX<hh->GetXaxis()->GetNbins()+1; iX++){
    for (int iY(1); iY<hh->GetYaxis()->GetNbins()+1; iY++){
      float x = hh->GetXaxis()->GetBinCenter(iX);
      float y = hh->GetYaxis()->GetBinCenter(iY) - tw[iX-1];
      double s = hh->GetBinContent(iX,iY);
      h->Fill(x,y,s);
    }
  }
  hh->Delete();
  return;
}

void GigaTrackerPixelT0ChipTW::SmoothT0(float vt0[40][45], bool masks[40][45], string repo1, string repo2 ){
  int nX = 40;
  int nY = 45;
  for(int iX(0); iX<nX;iX++){
    for(int iY(0); iY<nY;iY++){
      //compute average
      double average = 0;
      double npoint = 0;
      for(int dx(-3);dx<=3;dx++){
        for(int dy(-1);dy<=1;dy++){
          if(dx==0 && dy==0 ) continue;
          if( iX+dx >= nX || iX+dx<0    ) continue;
          if( iY+dy >= nY || iY+dy<0    ) continue;
          if(masks[iX+dx][iY+dy]       ) continue;
          average += vt0[iX+dx][iY+dy];
          npoint += 1;
        }
      }
      if(npoint == 0 ) continue;
      average /= npoint;
      if(masks[iX][iY] == 1){
        printf("    T0 of Masked Pixel %d-%d set to %f\n",iX,iY,average);
        vt0[iX][iY] = average;
      }
      if(fabs(vt0[iX][iY] - average) > 2 ){
        printf("    T0 of Pixel %d-%d reset from %f to %f, please check\n",iX,iY,vt0[iX][iY],average);
        vt0[iX][iY] = average;
        TH1D* h = (TH1D*) gDirectory->Get(Form("%s/t0_h_dt_tot_x%d_y%d", repo2.c_str(), iX,iY));
        if(gDirectory->GetDirectory(repo1.c_str()) == NULL) {
          gDirectory->mkdir(repo1.c_str());
        }
        if(h==NULL) {
          cout<< user_normal() << "Histo "<< Form("%s/t0_h_dt_tot_x%d_y%d",repo2.c_str(),iX,iY)<<" is missing"<<endl;
          continue;
        }
        gDirectory->cd(repo1.c_str());
        h->Write();
        h->Delete();

        std::string directory="../";
        const char * pch;
        pch=strchr(repo1.c_str(),'/');
        while (pch!=NULL){
          directory = directory+"../";
          pch=strchr(pch+1,'/');
        }
        gDirectory->cd(directory.c_str());
      }
    }
  }
  return;
}

void GigaTrackerPixelT0ChipTW::GetTW(TH2D* h, float* tw,  int min , string repo, bool saveHisto ){

  int nBins = h->GetXaxis()->GetNbins();
  int nSlice(0);
  int xM,yM,zM;
  h->GetBinXYZ(h->GetMaximumBin(),xM,yM,zM);
  int start = xM;
  int stop = start+1;
  TF1 * f1 = new TF1 ("f1","gaus",-10,10);
  TF1 * f2 = new TF1 ("f2","gaus",-10,10);

  ProcessSlice(h,start, stop, tw, f1,nSlice, 1, min);

  for(int p(0);p<3;p++)  f2->SetParameter(p, f1->GetParameter(p));

  for(stop++; stop<= nBins; stop++){
    if( ! ProcessSlice(h,start,stop,tw,f1,nSlice, 1, min) ) continue;
    if( fabs(stop-start) > 30 ) {
      break;
    }
  }

  start = xM-1;
  stop = start+1;
  for(; start>0; start--){
    if( ! ProcessSlice(h,start,stop,tw,f2,nSlice, -1, min) ) continue;
    if( fabs(stop-start) > 30 ) {
      break;
    }
  }
  f1->Delete();
  f2->Delete();

  int i(0);
  while(i<nBins-1 && tw[i]==0) i++;
  while(i>0 && tw[i-1]==0){
    tw[i-1] = tw[i];
    i--;
  }

  i = nBins-1;
  while(tw[i]==0 && i>0) i--;
  while(i<(nBins-1) && tw[i+1]==0){
    tw[i+1] = tw[i];
    i++;
  }

  float twc[410];
  for(i=0;i<nBins;i++){
    twc[i] = h->GetXaxis()->GetBinCenter(i+1);
  }
  TGraph * gr = new TGraph (nBins,twc,tw);
  gr->SetName(Form("%s_gr",h->GetName()));
  gr->SetTitle(h->GetTitle());
  if(gDirectory->GetDirectory(repo.c_str()) == NULL) gDirectory->mkdir(repo.c_str());
  gDirectory->cd(repo.c_str());
  if(saveHisto) h->Write();
  gr->Write();
  gr->Delete();

  std::string directory="../";
  const char * pch;
  pch=strchr(repo.c_str(),'/');
  while (pch!=NULL){
    directory = directory+"../";
    pch=strchr(pch+1,'/');
  }
  gDirectory->cd(directory.c_str());
}

bool GigaTrackerPixelT0ChipTW::ProcessSlice(TH2D* h, int& start,int& stop, float* tw, TF1* f1, int& nSlice, int dir , int min ){
  char sliceName[100];
  double sliceLow = h->GetXaxis()->GetBinLowEdge(start);
  double sliceUp = h->GetXaxis()->GetBinLowEdge(stop);
  sprintf(sliceName,"sl_%d_%.1f-to-%.1f",nSlice,sliceLow, sliceUp);
  TH1D* slice =  h->ProjectionY(sliceName,start,stop);
  slice->SetTitle(sliceName);
  if(!isGoodSlice(slice, min)) return 0;
  tw[stop-1] = FitSlice(slice,f1,nSlice);
  for(int i(start);i<=stop-1;i++) {
    tw[i]=tw[stop-1];
  }

  if(dir>0) start++;
  if(dir<0) stop--;
  nSlice++;
  slice->Delete();
  return 1;
}

double GigaTrackerPixelT0ChipTW::FitSlice(TH1D* h, TF1* f1,int nSlice){
  double center(0);
  if (nSlice == 0 ) center = h->GetXaxis()->GetBinCenter(h->GetMaximumBin());
  else center = f1->GetParameter(1);
  h->GetXaxis()->SetRangeUser(center-0.5,center+0.5);
  h->Fit(f1,"Q","",center-0.4,center+0.4);

  center = f1->GetParameter(1);
  f1->SetParLimits(1,center-0.3,center+0.3); //was 0.4

  double width = f1->GetParameter(2);
  f1->SetParLimits(2,0,width+0.5);
  return f1->GetParameter(1);
}

bool GigaTrackerPixelT0ChipTW::isGoodSlice(TH1D* h , int min ){
  double inte = h->Integral();
  if( inte < min ) return 0;
  //double peakSig = h->GetMaximum()*h->GetNbinsX()/inte;
  return 1;
}
