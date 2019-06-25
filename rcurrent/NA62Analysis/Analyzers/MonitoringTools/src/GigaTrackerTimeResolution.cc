#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "GigaTrackerTimeResolution.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "DownstreamTrack.hh"
#include "Particle.hh"
#include "Algorithm.hh"
#include "VertexLSF.hh"
#include "Utils.hh"
#include "GeometricAcceptance.hh"
#include <bitset>
#include "TF1.h"
#include "TLegend.h"
#include "RooRealVar.h"
#include "RooGaussian.h"
#include "RooFormulaVar.h"
#include "RooCategory.h"
#include "RooSimultaneous.h"
#include "RooDataHist.h"
#include "RooPlot.h"
#include "RooFitResult.h"
#include "TMultiGraph.h"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

GigaTrackerTimeResolution::GigaTrackerTimeResolution(Core::BaseAnalysis *ba) : Analyzer(ba, "GigaTrackerTimeResolution") {
  fReadingData = true;
  fThreePiAlgo = new ThreePiAssociationAlgo(ba,this,"ThreePiAlgo");
  fCHODEvt = new TRecoCHODEvent;
  RequestTree("CHOD",fCHODEvt);
  fCedarEvt = new TRecoCedarEvent;
  RequestTree("Cedar",fCedarEvt);
  fGigaTrackerEvt = new TRecoGigaTrackerEvent;
  RequestTree("GigaTracker",fGigaTrackerEvt);
}

void GigaTrackerTimeResolution::InitOutput(){}

void GigaTrackerTimeResolution::InitHist(){

  fReadingData = GetIsTree();

  if(fReadingData){
    
    for (int i(0);i<3;i ++) fHDtKTAGN[i] = new TH2D(Form("hDt%dKTAGN",i+1),Form(";t_{KTAG} - t_{GTK%d} [ns]; NHits KTAG",i+1),1400,-3.5,3.5,70,0,70);
    fHDtKTAGN[3] = new TH2D("hDtKTAGN",";t_{KTAG} - t_{GTK} [ns]; NHits KTAG",1400,-3.5,3.5,70,0,70);
    for (int i(0);i<4;i++) BookHisto(fHDtKTAGN[i]->GetName(), fHDtKTAGN[i],0,"KTAG");
    
    for (int i(0);i<3;i++){
      fHDtKTAGToT[i] = new TH2D(Form("hDt%dKTAGToT",i+1),Form(";t_{GTK%d} - t_{KTAG} [ns]; ToT [ns]",i+1),1400,-3.5,3.5,200,0,40);
      BookHisto(fHDtKTAGToT[i]->GetName(), fHDtKTAGToT[i],0,"KTAG");
    }

    fHDtKTAG[0] = new TH1D("hDt1t3KTAG",";t_{GTK3} - t_{GTK1} [ns]; count",1400,-3.5,3.5);
    fHDtKTAG[1] = new TH1D("hDt2t3KTAG",";t_{GTK3} - t_{GTK2} [ns]; count",1400,-3.5,3.5);
    fHDtKTAG[2] = new TH1D("hDt1t2KTAG",";t_{GTK1} - t_{GTK2} [ns]; count",1400,-3.5,3.5);
    for (int i(0);i<3;i++)    BookHisto(fHDtKTAG[i]->GetName(), fHDtKTAG[i],0,"KTAG");


    for (int i(0);i<3;i ++) fHDtRICHN[i] = new TH2D(Form("hDt%dRICHN",i+1),Form(";t_{RICH} - t_{GTK%d} [ns]; NHits RICH",i+1),1400,-3.5,3.5,70,0,70);
    fHDtRICHN[3] = new TH2D("hDtRICHN",";t_{RICH} - t_{GTK} [ns]; NHits RICH",1400,-3.5,3.5,70,0,70);
    for (int i(0);i<4;i++) BookHisto(fHDtRICHN[i]->GetName(), fHDtRICHN[i],0,"RICH");
    
    for (int i(0);i<3;i++){
      fHDtRICHToT[i] = new TH2D(Form("hDt%dRICHToT",i+1),Form(";t_{GTK%d} - t_{RICH} [ns]; ToT [ns]",i+1),1400,-3.5,3.5,200,0,40);
      BookHisto(fHDtRICHToT[i]->GetName(), fHDtRICHToT[i],0,"RICH");
    }

    fHDtRICH[0] = new TH1D("hDt1t3RICH",";t_{GTK3} - t_{GTK1} [ns]; count",1400,-3.5,3.5);
    fHDtRICH[1] = new TH1D("hDt2t3RICH",";t_{GTK3} - t_{GTK2} [ns]; count",1400,-3.5,3.5);
    fHDtRICH[2] = new TH1D("hDt1t2RICH",";t_{GTK1} - t_{GTK2} [ns]; count",1400,-3.5,3.5);
    for (int i(0);i<3;i++)    BookHisto(fHDtRICH[i]->GetName(), fHDtRICH[i],0,"RICH");



    
	
    BookHisto("hDt_GTKKTAGHits_N",new TH2D("hDt_GTKKTAGHits_N",";t^{hits}_{KTAG} - t_{GTK}; NHits",1400,-3.5,3.5,70,0,70),0,"KTAG");
    BookHisto("hDt_RICHHits_N"   ,new TH2D("hDt_RICHHits_N",";t^{hits}_{RICH} - t^{cand}_{RICH}; NHits",1400,-3.5,3.5,70,0,70),0,"RICH");
    BookHisto("hDt_GTKRICHHits_N",new TH2D("hDt_GTKRICHHits_N",";t^{hits}_{RICH} - t_{GTK}; NHits",1400,-3.5,3.5,70,0,70),0,"RICH");
    
    fHDtCedarRICH = new TH1D("hDtCedarRICH_s",";t_{KTAG} - t_{RICH} [ns]",1400,-3.5,3.5);
    BookHisto("hDtCedarRICH_s"   ,fHDtCedarRICH);

    fHDtNN = new TH3D("hDtCedarRICHNN_s",";t_{KTAG} - t_{RICH} [ns]; NHits KTAG; NHits RICH",1400,-3.5,3.5,70,0,70,70,0,70);
    BookHisto(fHDtNN->GetName(),fHDtNN);
  }

  else{

    fHDtCedarRICH= (TH1D*) RequestHistogram("GigaTrackerTimeResolution","hDtCedarRICH_s",true);

    fHDtKTAG[0] = (TH1D*) RequestHistogram("GigaTrackerTimeResolution/KTAG","hDt1t3KTAG",true);
    fHDtKTAG[1] = (TH1D*) RequestHistogram("GigaTrackerTimeResolution/KTAG","hDt2t3KTAG",true);
    fHDtKTAG[2] = (TH1D*) RequestHistogram("GigaTrackerTimeResolution/KTAG","hDt1t2KTAG",true);

    fHDtRICH[0] = (TH1D*) RequestHistogram("GigaTrackerTimeResolution/RICH","hDt1t3RICH",true);
    fHDtRICH[1] = (TH1D*) RequestHistogram("GigaTrackerTimeResolution/RICH","hDt2t3RICH",true);
    fHDtRICH[2] = (TH1D*) RequestHistogram("GigaTrackerTimeResolution/RICH","hDt1t2RICH",true);

    for (int i(0);i<3;i ++){
      fHDtKTAGN[i] = (TH2D*) RequestHistogram("GigaTrackerTimeResolution/KTAG",Form("hDt%dKTAGN",i+1),true);
      //cout<<Form("Check hDt%dKTAGN :",i+1)<<fHDtKTAGN[i]<<endl;
      if(!fHDtKTAGN[i]) continue;
      fHDtKTAG[3+i] = (TH1D*) fHDtKTAGN[i]->ProjectionX();
    }
    fHDtKTAGN[3] = (TH2D*) RequestHistogram("GigaTrackerTimeResolution/KTAG","hDtKTAGN",true);
    //cout<<Form("Check hDt%dKTAGN :",4)<<fHDtKTAGN[3]<<endl;
    if(fHDtKTAGN[3]) fHDtKTAG[6] = (TH1D*) fHDtKTAGN[3]->ProjectionX();

    for (int i(0);i<3;i ++){
      fHDtRICHN[i] = (TH2D*) RequestHistogram("GigaTrackerTimeResolution/RICH",Form("hDt%dRICHN",i+1),true);
      //cout<<Form("Check hDt%dRICHN :",i+1)<<fHDtRICHN[i]<<endl;
      if(!fHDtRICHN[i]) continue;
      fHDtRICH[3+i] = (TH1D*) fHDtRICHN[i]->ProjectionX();
    }
    fHDtRICHN[3] = (TH2D*) RequestHistogram("GigaTrackerTimeResolution/RICH","hDtRICHN",true);
    //cout<<Form("Check hDt%dRICHN :",4)<<fHDtRICHN[3]<<endl;
    if(fHDtRICHN[3]) fHDtRICH[6] = (TH1D*) fHDtRICHN[3]->ProjectionX();
    
    fHDtNN = (TH3D*) RequestHistogram("GigaTrackerTimeResolution","hDtCedarRICHNN_s",true);
    


    //some tgraphs
    for(int i(0); i<3; i++){
      //fGNWKTAG[i] = new TGraph();
      //BookHisto(Form("GNWKTAG",i+1), fGNWKTAG[i]);

      //fGNWRICH[i] = new TGraph();
      //BookHisto(Form("GNWRICH",i+1), fGNWRICH[i]);

      fGNMKTAG[i] = new TGraph();
      BookHisto(Form("GNMKTAG%d",i+1), fGNMKTAG[i]);

      fGNMRICH[i] = new TGraph();
      BookHisto(Form("GNMRICH%d",i+1), fGNMRICH[i]);
    }
  }
}

void GigaTrackerTimeResolution::DefineMCSimple(){
}

void GigaTrackerTimeResolution::StartOfRunUser(){
}

void GigaTrackerTimeResolution::StartOfBurstUser(){
}

void GigaTrackerTimeResolution::Process(int /*iEvent*/){
  if(!fReadingData) return;

  fCHODEvt = GetEvent<TRecoCHODEvent>();
  fCedarEvt  = GetEvent<TRecoCedarEvent>();
  fGigaTrackerEvt = GetEvent<TRecoGigaTrackerEvent>();
  
  //skip shower like event in CHOD
  if(fCHODEvt->GetNQuadrants()==4) return;

  //build downstream tracks
  std::vector<DownstreamTrack> dTracks = *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");

  //filter for pions
  std::vector<Particle> parts;

  for (auto it = dTracks.begin(); it!=dTracks.end(); it++){
    int nCHODCand = (it)->GetNCHODAssociationRecords();
    if( nCHODCand == 0) continue;

    //Check no good MUV3 candidate is matched
    if ((it)->MUV3AssociationExists() &&  (it)->GetMUV3Candidate(0) != NULL) {
      double dt = (it)->GetMUV3Candidate(0)->GetTime() - (it)->GetCHODTime();
      if(abs(dt)<1.5) continue; //NoMUV3
    }

    Particle part;
    part.SetMomentum( (it)->GetMomentumBeforeMagnet());
    part.SetPosition((it)->GetPositionBeforeMagnet());
    part.SetTime((it)->GetCHODTime());
    part.SetMass(139.57018);
    part.SetProto(&(*it));
    parts.push_back(part);
  }

  //build K3pi
  std::vector<Particle> vK3Pi = fThreePiAlgo->Associate(parts);

  //associate K3Pi with GTK (kinematics based)
  fThreePiAlgo->Associate(vK3Pi, fGigaTrackerEvt);

  //associate K3Pi with KTAG
  if(fCedarEvt!=NULL)   fThreePiAlgo->Associate(vK3Pi, fCedarEvt);
  
  //time differnce with KTAG
  for(auto it = vK3Pi.begin(); it!=vK3Pi.end(); it++){
    if(it->GetProto() == NULL ||
       it->GetProto()->GetCandidate(0,ProtoParticle::kTRecoCedarCandidate) ==NULL ||
       it->GetProto()->GetCandidate(0,ProtoParticle::kTRecoGigaTrackerCandidate) ==NULL) continue;
    TRecoCedarCandidate* cedarCand =static_cast<TRecoCedarCandidate*>(it->GetProto()->GetCandidate(0,ProtoParticle::kTRecoCedarCandidate));
    TRecoGigaTrackerCandidate* gigatrackerCand =static_cast<TRecoGigaTrackerCandidate*>(it->GetProto()->GetCandidate(0,ProtoParticle::kTRecoGigaTrackerCandidate));
    //fill histos
    double tRef = cedarCand->GetTime();
    int nHits = cedarCand->GetNHits();
    FillHisto("hDtKTAGN",tRef-gigatrackerCand->GetTime(),nHits);
    FillHisto("hDt1KTAGN",tRef-gigatrackerCand->GetTimeStation(0),nHits);
    FillHisto("hDt2KTAGN",tRef-gigatrackerCand->GetTimeStation(1),nHits);
    FillHisto("hDt3KTAGN",tRef-gigatrackerCand->GetTimeStation(2),nHits);

    FillHisto("hDt1t3KTAG",gigatrackerCand->GetTimeStation(2)-gigatrackerCand->GetTimeStation(0));
    FillHisto("hDt2t3KTAG",gigatrackerCand->GetTimeStation(2)-gigatrackerCand->GetTimeStation(1));
    FillHisto("hDt1t2KTAG",gigatrackerCand->GetTimeStation(0)-gigatrackerCand->GetTimeStation(1));

    int* hitIndexes = gigatrackerCand->GetHitsIndexes();
    for(int iH(0);iH< gigatrackerCand->GetNHits();iH++){
      TRecoGigaTrackerHit* hit = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvt->GetHit(hitIndexes[iH]));
      int station = hit->GetStationNo();
      FillHisto(Form("hDt%dKTAGToT",station+1),tRef-gigatrackerCand->GetTimeStation(station),hit->GetToT());
    }

    int* hitIndexes2 = cedarCand->GetHitsIndexes();
    for(int iH(0);iH< cedarCand->GetNHits();iH++){
      TRecoCedarHit* hit = static_cast<TRecoCedarHit*>(fCedarEvt->GetHit(hitIndexes2[iH]));
      FillHisto("hDt_GTKKTAGHits_N",hit->GetTime()-gigatrackerCand->GetTime(),cedarCand->GetNHits());
    }
  }

  //time differnce with RICH
  for(auto it = vK3Pi.begin(); it!=vK3Pi.end(); it++){
    if(it->GetProto() == NULL || it->GetProto()->GetCandidate(0,ProtoParticle::kTRecoGigaTrackerCandidate) ==NULL) continue;
    TRecoGigaTrackerCandidate* gigatrackerCand =static_cast<TRecoGigaTrackerCandidate*>(it->GetProto()->GetCandidate(0,ProtoParticle::kTRecoGigaTrackerCandidate));

    //compute the rich time
    vector<Particle> vPions = it->GetDaughters();
    int nHits(0);
    double tRef(0);
    for(int iPi(0);iPi<3;iPi++){
      DownstreamTrack* piDST = static_cast<DownstreamTrack*>(vPions[iPi].GetProto());
      if(tRef < -998 ) continue;
      tRef += piDST->GetRICHRingTime(3)*piDST->GetRICHRingNHits(3);
      nHits += piDST->GetRICHRingNHits(3);

      std::vector<TRecoRICHHit*> richHits =  piDST->GetRICHAssignedHits(3);
      if ((int)richHits.size()!=piDST->GetRICHRingNHits(3)) cout<< user_normal() << "WE HAVE A PROBLEM!! "<<richHits.size()<<" vs "<< piDST->GetRICHRingNHits(3)<<endl;
      for (auto itt = richHits.begin(); itt!=richHits.end();itt++){
	FillHisto("hDt_RICHHits_N",(*itt)->GetTime()-piDST->GetRICHRingTime(3), piDST->GetRICHRingNHits(3));
	FillHisto("hDt_GTKRICHHits_N",(*itt)->GetTime()-gigatrackerCand->GetTime(), piDST->GetRICHRingNHits(3));
      }
    }
    if(nHits==0) continue;
    tRef /= nHits;    

    //fill histos
    FillHisto("hDtRICHN",tRef-gigatrackerCand->GetTime(),nHits);
    FillHisto("hDt1RICHN",tRef-gigatrackerCand->GetTimeStation(0),nHits);
    FillHisto("hDt2RICHN",tRef-gigatrackerCand->GetTimeStation(1),nHits);
    FillHisto("hDt3RICHN",tRef-gigatrackerCand->GetTimeStation(2),nHits);

    FillHisto("hDt1t3RICH",gigatrackerCand->GetTimeStation(2)-gigatrackerCand->GetTimeStation(0));
    FillHisto("hDt2t3RICH",gigatrackerCand->GetTimeStation(2)-gigatrackerCand->GetTimeStation(1));

    FillHisto("hDt1t2RICH",gigatrackerCand->GetTimeStation(0)-gigatrackerCand->GetTimeStation(1));

    int* hitIndexes = gigatrackerCand->GetHitsIndexes();
    for(int iH(0);iH< gigatrackerCand->GetNHits();iH++){
      TRecoGigaTrackerHit* hit = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvt->GetHit(hitIndexes[iH]));
      int station = hit->GetStationNo();
      FillHisto(Form("hDt%dRICHToT",station+1),tRef-gigatrackerCand->GetTimeStation(station),hit->GetToT());
    }

    //cross check time diff cedar/rich
    if(it->GetProto() == NULL ||  it->GetProto()->GetCandidate(0,ProtoParticle::kTRecoCedarCandidate) ==NULL) continue;
    TRecoCedarCandidate* cedarCand =static_cast<TRecoCedarCandidate*>(it->GetProto()->GetCandidate(0,ProtoParticle::kTRecoCedarCandidate));
    FillHisto("hDtCedarRICH_s"    , cedarCand->GetTime()-tRef);	
    FillHisto("hDtCedarRICHNN_s"    , cedarCand->GetTime()-tRef,cedarCand->GetNHits(),nHits);	
  }
}

void GigaTrackerTimeResolution::PostProcess(){
  for( vector<DownstreamTrack*>::iterator i=fDSTs.begin(); i!=fDSTs.end();++i) delete (*i);
}

void GigaTrackerTimeResolution::EndOfBurstUser(){
}

void GigaTrackerTimeResolution::EndOfRunUser(){

}

void GigaTrackerTimeResolution::EndOfJobUser(){

  if(!fReadingData){

    //Make plts
    std::cout<< user_normal() << "Saving Summary Plots in: "<<Form("Run%4dGTKTimeReso.pdf",GetRunID())<<std::endl;
    fCv = new TCanvas("cv","",2970,2100);
    fCv->SetTitle(Form("Run%4dGTKTimeReso",GetRunID()));
    fCv->Print(Form("%s.pdf[", fCv->GetTitle()));
    
    if(fHDtKTAGN[3]) fHDtKTAGN[3]->Draw("COLZ");
    fCv->Print(Form("%s.pdf", fCv->GetTitle()));

    if(fHDtRICHN[3]) fHDtRICHN[3]->Draw("COLZ");
    fCv->Print(Form("%s.pdf", fCv->GetTitle()));

    FILE* ftxt;
    char buffer[50];
    sprintf(buffer,"GigaTrackerTimeResolutionRun%d_StandAlone.txt",GetRunID());
    ftxt = fopen(buffer, "w");
    
    //Resolution Standalone
    Standalone(ftxt,"KTAG");
    for(int i(0); i<7; i++){
      if(!fHDtKTAG[i]) continue;
      fHDtKTAG[i]->Draw();
      fCv->Print(Form("%s.pdf", fCv->GetTitle()));
    }

    Standalone(ftxt,"RICH");
    for(int i(0); i<7; i++){
      if(!fHDtRICH[i]) continue;
      fHDtRICH[i]->Draw();
      fCv->Print(Form("%s.pdf", fCv->GetTitle()));
    }
    fclose(ftxt);

    //Resolution Simultaneous
    Simultaneous();

    fCv->Print(Form("%s.pdf]", fCv->GetTitle()));
  }
  
  SaveAllPlots();
  fThreePiAlgo->SaveAllPlots();
}

void GigaTrackerTimeResolution::Standalone(FILE* ftxt, TString ref){
  //Extract TimeReso Standalone
  double fitWidth = 0.6;


  TH1D* fHDt[8];
  for(int i(0);i<7;i++){
    if(ref.CompareTo("KTAG")==0) fHDt[i] = fHDtKTAG[i];
    else if(ref.CompareTo("RICH")==0) fHDt[i] = fHDtRICH[i];
  }
  fHDt[7] = fHDtCedarRICH;

  TF1* gauses[8]={0};
  double reso[8]={0};
  double mean[8]={0};
  TCanvas cv;

  for(int i(0); i<8; i++){
    gauses[i] = new TF1(Form("gaus-%d",i),"gaus",-3,3);
    fHDt[i]->Fit(gauses[i],"Q","",-fitWidth,fitWidth);
    reso[i] = gauses[i]->GetParameter(2)*1e3;
    mean[i] = gauses[i]->GetParameter(1)*1e3;
  }
  printf(" GTK13 %f %f\n",reso[0],reso[0]/sqrt(2));
  printf(" GTK23 %f %f\n",reso[1],reso[1]/sqrt(2));
  printf(" GTK12 %f %f\n",reso[2],reso[2]/sqrt(2));
  printf(" GTK1 %f GTK2 %f GTK3 %f\n", 
	 sqrt((reso[0]*reso[0]+reso[2]*reso[2]-reso[1]*reso[1])/2),
	 sqrt((reso[1]*reso[1]+reso[2]*reso[2]-reso[0]*reso[0])/2),
	 sqrt((reso[0]*reso[0]+reso[1]*reso[1]-reso[2]*reso[2])/2) );
  printf(" GTK1-%s   %f (m) %f (r)\n",ref.Data(),mean[3],reso[3]);
  printf(" GTK2-%s   %f (m) %f (r)\n",ref.Data(),mean[4],reso[4]);
  printf(" GTK3-%s   %f (m) %f (r)\n",ref.Data(),mean[5],reso[5]);
  printf(" GTK-%s    %f (m) %f (r)\n",ref.Data(),mean[6],reso[6]);
  printf(" RICH-KTAG %f (m) %f (r)\n",mean[7],reso[7]);


  fprintf(ftxt,"reso1 %f\n",sqrt((reso[0]*reso[0]+reso[2]*reso[2]-reso[1]*reso[1])/2));
  fprintf(ftxt,"reso2 %f\n",sqrt((reso[1]*reso[1]+reso[2]*reso[2]-reso[0]*reso[0])/2));
  fprintf(ftxt,"reso3 %f\n",sqrt((reso[0]*reso[0]+reso[1]*reso[1]-reso[2]*reso[2])/2));
  fprintf(ftxt,"GTK13 %f %f\n",reso[0],reso[0]/sqrt(2));
  fprintf(ftxt,"GTK23 %f %f\n",reso[1],reso[1]/sqrt(2));
  fprintf(ftxt,"GTK12 %f %f\n",reso[2],reso[2]/sqrt(2));
  fprintf(ftxt,"GTK1-%s   %f (m) %f (r)\n",ref.Data(),mean[3],reso[3]);
  fprintf(ftxt,"GTK2-%s   %f (m) %f (r)\n",ref.Data(),mean[4],reso[4]);
  fprintf(ftxt,"GTK3-%s   %f (m) %f (r)\n",ref.Data(),mean[5],reso[5]);
  fprintf(ftxt,"GTK-%s    %f (m) %f (r)\n",ref.Data(),mean[6],reso[6]);
  fprintf(ftxt,"RICH-KTAG %f (m) %f (r)\n",mean[7],reso[7]);
    
  for(int i(0);i<8;i++) delete gauses[i]; //cleaning

}


void GigaTrackerTimeResolution::Simultaneous(){

  double fitWidth = 0.6;
  int nRmin = 10;
  int nRmax = 29;
  int nKmin = 10;
  int nKmax = 29; //23
  int nEvtMin = 20;

  TH2D* hDtN[2][3];
  for( int g(0);g<3;g++){
    hDtN[0][g] = fHDtKTAGN[g];
    hDtN[1][g] = fHDtRICHN[g];
  }
  
  TString out = Form("run%d",GetRunID());
  FILE* ftxt; FILE* ftxtKG; FILE* ftxtRG;
  ftxt   = fopen(Form("%s-reso-simu.txt",out.Data()), "w");
  ftxtKG = fopen(Form("%s-KG-reso-simu.txt",out.Data()), "w");
  ftxtRG = fopen(Form("%s-RG-reso-simu.txt",out.Data()), "w");

  int nNhitsKTAG = 0;
  int nNhitsRICH = 0;
  if(hDtN[0][0]) nNhitsKTAG = hDtN[0][0]->GetYaxis()->GetNbins();
  if(hDtN[1][0]) nNhitsRICH = hDtN[1][0]->GetYaxis()->GetNbins();
  cout<< user_normal() << "Nb of Bins are: "<<nNhitsKTAG<<"   "<<nNhitsRICH<<endl;
  
  TH1D* histosKR[70][70] = {NULL};
  TH1D* histosKG[70][3] = {NULL};
  TH1D* histosRG[70][3] = {NULL};
  RooRealVar dt("dt","dt",-3,3);
  dt.setRange(-fitWidth,fitWidth);

  RooRealVar sKTAG("sKTAG","sKTAG",0.01,0.3);
  RooRealVar sRICH("sRICH","sRICH",0.01,0.3);

  RooRealVar* sGTK[3] = {NULL};

  RooRealVar sPMTKTAG("sPMTKTAG","sPMTKTAG",0.1,0.5);
  RooRealVar sPMTRICH("sPMTRICH","sPMTRICH",0.1,0.5);

  RooCategory samples("samples","samples") ;
  std::map< std::string, TH1 * > histMap;
  RooGaussian* gKR[70][70] = {NULL};
  RooGaussian* gKG[70][3] = {NULL};
  RooGaussian* gRG[70][3] = {NULL};

  RooFormulaVar* sCandKR[70][70] = {NULL};
  RooFormulaVar* sCandKG[70][3] = {NULL};
  RooFormulaVar* sCandRG[70][3] = {NULL};

  RooRealVar* meanKR[70][70] = {NULL};
  RooRealVar* meanKG[70][3] = {NULL};
  RooRealVar* meanRG[70][3] = {NULL};

  RooSimultaneous simPdf("simPdf","simultaneous pdf",samples);

  for(int g(0); g<3;g++){
    sGTK[g] = new RooRealVar(Form("sGTK%d",g+1),Form("sGTK%d",g+1),0.01,0.3);

    //KTAG vs GTK
    for (int i(0); i<nNhitsKTAG; i++){
      
      TString cat = Form("KG-%d-%d",i+1,g+1);
      if(hDtN[0][g]) histosKG[i][g] = hDtN[0][g]->ProjectionX(Form("_KG-%d-%d",i+1,g+1),i+1,i+2);
      
      if((histosKG[i][g] && histosKG[i][g]->Integral()<nEvtMin) || i>nKmax || i<nKmin) continue;
      meanKG[i][g]  = new RooRealVar(Form("meanKG%d-%d",i+1,g),Form("meanKG%d%d",i+1,g),-0.5,0.5);
      sCandKG[i][g] = new RooFormulaVar(Form("sCandKG-%d-%d",i+1,g),Form("reso Cand %d %d hits",i+1,g),Form("sqrt(sGTK%d*sGTK%d + sKTAG*sKTAG + sPMTKTAG*sPMTKTAG/%d)",g+1,g+1,i+1),RooArgList(*sGTK[g], sKTAG,sPMTKTAG) );
      gKG[i][g] = new RooGaussian(Form("gKG-%d-%d",i+1,g+1),Form("gKG-%d-%d",i+1,g+1),dt,*meanKG[i][g],*sCandKG[i][g]) ;
      samples.defineType(cat);
      histMap.insert(pair< std::string, TH1D * >( string(cat.Data()),histosKG[i][g]));
      simPdf.addPdf(*gKG[i][g],cat) ;
    }

    //RICH vs GTK
    for (int i(0); i<nNhitsRICH; i++){
      
      TString cat = Form("RG-%d-%d",i+1,g+1);
      if(hDtN[1][g]) histosRG[i][g] = hDtN[1][g]->ProjectionX(Form("_RG-%d-%d",i+1,g+1),i+1,i+2);
      
      if((histosRG[i][g] && histosRG[i][g]->Integral()<nEvtMin) || i>nRmax || i<nRmin ) continue;
      meanRG[i][g]  = new RooRealVar(Form("meanRG%d-%d",i+1,g),Form("meanRG%d%d",i+1,g),-0.5,0.5);
      sCandRG[i][g] = new RooFormulaVar(Form("sCandRG-%d-%d",i+1,g),Form("reso Cand %d %d hits",i+1,g),Form("sqrt(sGTK%d*sGTK%d + sRICH*sRICH + sPMTRICH*sPMTRICH/%d)",g+1,g+1,i+1),RooArgList(*sGTK[g], sRICH,sPMTRICH) );
      gRG[i][g] = new RooGaussian(Form("gRG-%d-%d",i+1,g+1),Form("gRG-%d-%d",i+1,g+1),dt,*meanRG[i][g],*sCandRG[i][g]) ;
      samples.defineType(cat);
      histMap.insert(pair< std::string, TH1D * >( string(cat.Data()),histosRG[i][g]));
      simPdf.addPdf(*gRG[i][g],cat) ;
    }

  }
  // KTAG vs RICH
  for (int iK(0); iK<nNhitsKTAG; iK++){
    for (int iR(0); iR<nNhitsRICH; iR++){
      TString cat = Form("KR-%d-%d",iK+1,iR+1);
      if(fHDtNN) histosKR[iK][iR] = fHDtNN->ProjectionX(Form("_KR-%d-%d",iK+1,iR+1),iK+1,iK+2,iR+1,iR+2);

      if((histosKR[iK][iR] && histosKR[iK][iR]->Integral()<nEvtMin) || iR>nRmax || iR<nRmin || iK>nKmax || iK<nKmin) continue;
      meanKR[iK][iR]  = new RooRealVar(Form("meanKR%d-%d",iK+1,iR+1),Form("meanKR%d%d",iK+1,iR+1),-0.5,0.5);
      sCandKR[iK][iR] = new RooFormulaVar(Form("sCandKR-%d-%d",iK+1,iR+1),Form("reso Cand %d %d hits",iK+1,iR+1),
					  Form("sqrt(sKTAG*sKTAG  + sPMTKTAG*sPMTKTAG/%d + sRICH*sRICH + sPMTRICH*sPMTRICH/%d)",iK+1,iR+1),
					  RooArgList(sKTAG, sRICH, sPMTRICH, sPMTKTAG) );

      gKR[iK][iR] = new RooGaussian(Form("gKR-%d-%d",iK+1,iR+1+1),Form("gKR-%d-%d",iK+1,iR+1+1),dt,*meanKR[iK][iR],*sCandKR[iK][iR]) ;
      samples.defineType(cat);
      histMap.insert(pair< std::string, TH1D * >( string(cat.Data()),histosKR[iK][iR]));
      simPdf.addPdf(*gKR[iK][iR],cat) ;
    }
  }
  if(histMap.size()>0){
    RooDataHist combData("data","data",dt,samples,histMap);
  /*RooFitResult* r = */simPdf.fitTo(combData, RooFit::PrintLevel(-1)) ;
  }
 
  printf("KTAG   %3.2f +/- %3.2f\n",1e3*sKTAG.getValV(),1e3*sKTAG.getError());
  printf("PMT-K  %3.2f +/- %3.2f\n",1e3*sPMTKTAG.getValV(),1e3*sPMTKTAG.getError());
  printf("RICH   %3.2f +/- %3.2f\n",1e3*sRICH.getValV(),1e3*sRICH.getError());
  printf("PMT-R  %3.2f +/- %3.2f\n",1e3*sPMTRICH.getValV(),1e3*sPMTRICH.getError());

  fprintf(ftxt,"KTAG %3.2f  %3.2f\n",1e3*sKTAG.getValV(),1e3*sKTAG.getError());
  fprintf(ftxt," PMT %3.2f  %3.2f\n",1e3*sPMTKTAG.getValV(),1e3*sPMTKTAG.getError());
  fprintf(ftxt,"RICH %3.2f  %3.2f\n",1e3*sRICH.getValV(),1e3*sRICH.getError());
  fprintf(ftxt," PMT %3.2f  %3.2f\n",1e3*sPMTRICH.getValV(),1e3*sPMTRICH.getError());
  
  for(int g(0); g<3;g++){
    printf("GTK%d   %3.2f +/- %3.2f\n",g+1,1e3*sGTK[g]->getValV(),1e3*sGTK[g]->getError());
    fprintf(ftxt,"GTK%d %3.2f %3.2f\n",g+1,1e3*sGTK[g]->getValV(),1e3*sGTK[g]->getError());
  }

  int ii(0);
  for (int i(0); i<nNhitsKTAG; i++){
    if (meanKG[i][0] == NULL || meanKG[i][1] == NULL || meanKG[i][2] == NULL) continue;
    fGNMKTAG[0]->SetPoint(ii,i,1e3*meanKG[i][0]->getValV());
    fGNMKTAG[1]->SetPoint(ii,i,1e3*meanKG[i][1]->getValV());
    fGNMKTAG[2]->SetPoint(ii,i,1e3*meanKG[i][2]->getValV());
    ii++;
    
    printf("KTAG%d   %3.2f +/- %3.2f | %3.2f +/- %3.2f | %3.2f +/- %3.2f\n",i+1,
    	   1e3*meanKG[i][0]->getValV(),1e3*meanKG[i][0]->getError(),
    	   1e3*meanKG[i][1]->getValV(),1e3*meanKG[i][1]->getError(),
    	   1e3*meanKG[i][2]->getValV(),1e3*meanKG[i][2]->getError());
    fprintf(ftxtKG,"%d %3.2f %3.2f %3.2f %3.2f %3.2f %3.2f\n",i+1,
	    1e3*meanKG[i][0]->getValV(),1e3*meanKG[i][1]->getValV(),1e3*meanKG[i][2]->getValV(),
	    1e3*meanKG[i][0]->getError(),1e3*meanKG[i][1]->getError(),1e3*meanKG[i][2]->getError());

  }
  
  ii = 0;
  for (int i(0); i<nNhitsRICH; i++){
    if (meanRG[i][0] == NULL || meanRG[i][1] == NULL || meanRG[i][2] == NULL) continue;
    fGNMRICH[0]->SetPoint(ii,i,1e3*meanRG[i][0]->getValV());
    fGNMRICH[1]->SetPoint(ii,i,1e3*meanRG[i][1]->getValV());
    fGNMRICH[2]->SetPoint(ii,i,1e3*meanRG[i][2]->getValV());
    ii++;
    
    printf("RICH%d   %3.2f +/- %3.2f | %3.2f +/- %3.2f | %3.2f +/- %3.2f\n",i+1,
	   1e3*meanRG[i][0]->getValV(),1e3*meanRG[i][0]->getError(),
	   1e3*meanRG[i][1]->getValV(),1e3*meanRG[i][1]->getError(),
	   1e3*meanRG[i][2]->getValV(),1e3*meanRG[i][2]->getError());

    fprintf(ftxtRG,"%d %3.2f %3.2f %3.2f %3.2f %3.2f %3.2f\n",i+1,
	    1e3*meanRG[i][0]->getValV(),1e3*meanRG[i][1]->getValV(),1e3*meanRG[i][2]->getValV(),
	    1e3*meanRG[i][0]->getError(),1e3*meanRG[i][1]->getError(),1e3*meanRG[i][2]->getError());

  }
  TLegend* leg = new TLegend(0.6, 0.75, 0.9, 0.9);
  leg->SetNColumns(2);

  TMultiGraph* mg = new TMultiGraph();
  for (int i(0);i<3;i++) {
    fGNMKTAG[i]->SetLineColor(i+2);
    fGNMKTAG[i]->SetMarkerColor(i+2);
    fGNMKTAG[i]->SetMarkerStyle(22);
    fGNMKTAG[i]->SetMarkerSize(2);
    leg->AddEntry(fGNMKTAG[i],Form("KTAG-GTK%d",i+1),"pl");
    mg->Add(fGNMKTAG[i]);
  }

  for (int i(0);i<3;i++) {
    fGNMRICH[i]->SetLineColor(i+2);
    fGNMRICH[i]->SetMarkerColor(i+2);
    fGNMRICH[i]->SetMarkerStyle(23);
    fGNMRICH[i]->SetMarkerSize(2);
    leg->AddEntry(fGNMRICH[i],Form("RICH-GTK%d",i+1),"pl");
    mg->Add(fGNMRICH[i]);
  }
  mg->SetTitle(";Nb Of Hits; Mean Time Diff [ns]");
  mg->Draw("ALP");
  leg->Draw();
  fCv->Print(Form("%s.pdf", fCv->GetTitle()));

  //cleaning
  for(int g(0); g<3;g++){
    delete sGTK[g];
    for (int i(0); i<nNhitsKTAG; i++){
      if(meanKG[i][g]==NULL) continue;
      delete meanKG[i][g];
      delete sCandKG[i][g];
      delete gKG[i][g];
    }
    for (int i(0); i<nNhitsRICH; i++){
      if(meanRG[i][g]==NULL) continue;
      delete meanRG[i][g];
      delete sCandRG[i][g];
      delete gRG[i][g];
    }
  }
  for (int iK(0); iK<nNhitsKTAG; iK++){
    for (int iR(0); iR<nNhitsRICH; iR++){
      if(meanKR[iK][iR]==NULL) continue;
      delete meanKR[iK][iR];
      delete sCandKR[iK][iR];
      delete gKR[iK][iR];
    }
  }

  fclose(ftxt);
  fclose(ftxtKG);
  fclose(ftxtRG);
  return;
}

void GigaTrackerTimeResolution::DrawPlot(){
}

GigaTrackerTimeResolution::~GigaTrackerTimeResolution(){
}
