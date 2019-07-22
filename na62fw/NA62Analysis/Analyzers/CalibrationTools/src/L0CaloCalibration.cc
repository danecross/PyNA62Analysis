// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-05-08
// With help from Andrea Salamon and Nico De Simone
// ---------------------------------------------------------------

/// \class L0CaloCalibration
/// \Brief
/// An analyzer to compute the time resolution of the L0Calo trigger.
/// \EndBrief
/// \Detailed
/// Events that contain exactly one cluster of more than 20GeV are selected.
/// The time resolution is plotted as a function of energy in each LKr supercell.
/// Extra plots can be saved in eps in pdf format using the ExtraOutput flag.
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "L0CaloCalibration.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"

#include "TF1.h"
#include "TText.h"
#include "TStyle.h"
#include "TLine.h"

L0CaloCalibration::L0CaloCalibration(Core::BaseAnalysis *ba) : Analyzer(ba, "L0CaloCalibration") {
  RequestL0Data();
  RequestTree("LKr", new TRecoLKrEvent, "Reco");
  fHandle = L0PrimitiveHandler::GetInstance();
  fReadingData=false;
  AddParam("ExtraOutput", &fExtraOutput, false);
}

void L0CaloCalibration::InitOutput(){
}

void L0CaloCalibration::InitHist(){

  fReadingData = GetIsTree();

  if(fReadingData){
    TString name = "L0Calo_ClusterEnergy";
    BookHisto(new TH1F(name, "Cluster energy;Energy [GeV]", 200, 0, 100));
    name = "L0Calo_ClusterTime";
    BookHisto(new TH1F(name, "Cluster time;Time wrt trigger time [ns]", 200, -50, 50));
    name = "L0Calo_PrimTime";
    BookHisto(new TH1F(name, "Primitive time;Time wrt trigger time [ns]", 201, -100.5*TdcCalib, 100.5*TdcCalib));
    name = "L0Calo_PrimTimeE20";
    BookHisto(new TH1F(name, "Primitive time E20;Time wrt trigger time [ns]", 201, -100.5*TdcCalib, 100.5*TdcCalib));

    name = "L0Calo_ClusterTimeVsEnergy";
    BookHistoArray(new TH2F(name, "Cluster time vs energy;Cluster energy [GeV];Time wrt trigger time [ns]", 70, 20, 90, 201, -100.5*TdcCalib, 100.5*TdcCalib), 34*34);

    name = "L0Calo_ClusterTimeVsEnergyCell";
    BookHisto(new TH2F(name, "Cluster time vs energy;Cluster energy [GeV];Time wrt trigger time [ns]", 70, 20, 90, 201, -100.5*TdcCalib, 100.5*TdcCalib));

    name = "L0Calo_ClusterXY";
    BookHisto(new TH2F(name, "Cluster coordinates (E>30 GeV);x [m];y [m]", 110, -1.1, 1.1, 110, -1.1, 1.1));
    name = "L0Calo_ClusterXYC";
    BookHisto(new TH2F(name, "Cluster coordinates (E>30 GeV);x [m];y [m]", 128, -8*0.16, 8*0.16, 128, -8*0.16, 8*0.16));
    name = "L0Calo_ClusterXYSC";
    BookHisto(new TH2F(name, "Cluster coordinates (E>30 GeV);x [m];y [m]", 32, -8*0.16, 8*0.16, 32, -8*0.16, 8*0.16));
    name = "L0Calo_ClusterXYSC2";
    BookHisto(new TH2F(name, "Cluster coordinates (E>30 GeV);x [m];y [m]", 16, -8*0.16, 8*0.16, 16, -8*0.16, 8*0.16));
  }
  else {
    fHist1.resize(5,NULL);
    fHist1[0] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "L0Calo_ClusterTimeVsEnergyCell", true));
    fHist1[1] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "L0Calo_ClusterXYC", true));
    fHist1[2] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "L0Calo_ClusterXYSC", true));
    fHist1[3] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "L0Calo_ClusterXYSC2", true));

    int tt = 34*34;
    fHist2.resize(tt,NULL);
    for(int i=0; i<tt; ++i){
      if(i<35 || i>1120)      continue;
      if(i%34==0 || i%34==33) continue;
      fHist2[i] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("L0Calo_ClusterTimeVsEnergy%i",i), true));
    }
  }// end fReading
}

void L0CaloCalibration::DefineMCSimple(){}

void L0CaloCalibration::StartOfRunUser(){}

void L0CaloCalibration::StartOfBurstUser(){}

void L0CaloCalibration::ProcessSpecialTriggerUser(int , unsigned int ){}

void L0CaloCalibration::ProcessEOBEvent(){
  // fHandle->SetData(GetL0Data(), GetEventHeader()->GetRunID());
}

void L0CaloCalibration::Process(int ){
  if(!fReadingData) return;

  fHandle->SetData(GetL0Data(), GetEventHeader()->GetRunID());

  Bool_t Control = GetL0Data()->GetDataType() & 0x10;
  if(!Control) return ;

  Bool_t hasRICH = fHandle->CheckCondition(kL0RICH);
  if(!hasRICH) return;

  Int_t    richtime  = fHandle->GetTriggerTime(kL0RICH);
  Double_t fRefTime  = richtime * TdcCalib;

  TRecoLKrEvent* LKRevent = GetEvent<TRecoLKrEvent>();
  Int_t nClus = 0;
  TRecoLKrCandidate* Lcand = NULL;
  Double_t Ltime=0.0;
  for(int i=0; i<LKRevent->GetNCandidates(); ++i){
    TRecoLKrCandidate* cand = static_cast<TRecoLKrCandidate*>(LKRevent->GetCandidate(i));
    Double_t time = cand->GetTime()-fRefTime;
    if(fabs(time)>37.5) continue;
    Lcand = cand;
    nClus++;
    Ltime = time ;
  }
  // require exactly 1 cluster within +/-37.5ns
  if(nClus!=1) return ;

  PrimInfo CaloPrim;
  Bool_t HasPrim = fHandle->GetClosestPrimitiveInfo(richtime, kL0Calo, CaloPrim);
  if(!HasPrim) return;
  Double_t value = CaloPrim[2]*TdcCalib;

  Double_t e    = 1e-3*Lcand->GetClusterEnergy(); // [GeV]
  Double_t xcl  = 1e-3*Lcand->GetClusterX(); // [mm]
  Double_t ycl  = 1e-3*Lcand->GetClusterY(); // [mm]
  FillHisto("L0Calo_ClusterEnergy", e);
  FillHisto("L0Calo_PrimTime",value);
  if(e>=20.0){
    FillHisto("L0Calo_ClusterTime", Ltime);
    FillHisto("L0Calo_ClusterXY", xcl, ycl);
    FillHisto("L0Calo_ClusterXYC", xcl, ycl);
    FillHisto("L0Calo_ClusterXYSC", xcl, ycl);
    FillHisto("L0Calo_ClusterXYSC2", xcl, ycl);
    FillHisto("L0Calo_PrimTimeE20",value);

    TH2F* h = static_cast<TH2F*>(fHisto.GetTH2("L0Calo_ClusterXYSC"));
    Int_t scbin = h->FindBin(xcl,ycl);
    FillHistoArray("L0Calo_ClusterTimeVsEnergy",scbin, e, value);

    TH2F* ch = static_cast<TH2F*>(fHisto.GetTH2("L0Calo_ClusterXYC"));
    Int_t cbin = ch->FindBin(xcl,ycl);
    Int_t tbin = ch->FindBin(0.15,0.0);
    if(cbin==tbin) FillHisto("L0Calo_ClusterTimeVsEnergyCell",e,value);

  } // e>20
}

void L0CaloCalibration::PostProcess(){
}

void L0CaloCalibration::EndOfBurstUser(){
}

void L0CaloCalibration::EndOfRunUser(){

  if(!fReadingData){
    gStyle->SetOptStat(0);
    gStyle->SetOptFit(0);
    gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

    ofstream ofile;
    ofile.open("./L0CaloCalibration.dat");
    ofile << "### This file contains T0 offsets for the L0Calo system" << std::endl;
    ofile << "### The T0 values are the offset of the L0Calo primitives with respect to the RICH primitives." << std::endl;
    ofile << "### Therefore these T0 values must be SUBTRACTED in order to apply the proper correction." << std::endl;
    ofile << "### If there are more than 5 events, a Gaussian fit is made to the time offsets." << std::endl;
    ofile << "### If there are 5 events or less, the T0 is simply reported as zero." << std::endl;
    ofile << "### Each line in the file corresponds to one LKr supercell." << std::endl;
    ofile << "### The first entry in this file corresponds to the bottom cell on the Saleve side." << std::endl;
    ofile << "### The first 32 entries corresponds to the bottom row of supercells, going from Saleve to Jura." << std::endl;
    ofile << "### Then each row is handled sequentially, from the bottom row to the top row." << std::endl;
    ofile << "### This means last entry corresponds to the top cell on the Jura side." << std::endl;
    ofile << "### Note that these T0s need to be changed if the RICH T0s are modified." << std::endl;
    ofile << "### " << std::endl;
    ofile << "### | T0 Value | T0 Error | N events | " << std::endl;

    TString ofname = "L0CaloCalibration.pdf";
    TCanvas* can = new TCanvas();
    can->SetCanvasSize(700,500);
    can->SaveAs(ofname+"[");

    TF1* fit = new TF1("fit", "pol1");
    TF1* ggg = new TF1("ggg", "gaus");

    TH2F* mean = static_cast<TH2F*>(fHist1[2]->Clone("CaloT0s"));
    mean->Reset();
    mean->SetTitle("Calo T0 Map;x [m];y [m]");

    TH2F* map = static_cast<TH2F*>(fHist1[2]->Clone("CaloMap"));
    map->Reset();
    map->SetTitle("Calo Binning Map;x [m];y [m]");
    for(int i=1; i<33; i++){
      for(int j=1; j<33; j++){
	Double_t gbin = map->GetBin(i,j);
	map->SetBinContent(i,j,gbin);
      }
    }
    map->SetMarkerSize(0.8);

    fHist1[0]->Draw("box");
    can->SaveAs(ofname);
    can->Clear();

    can->SetLogz(1);
    fHist1[1]->Draw("colz");
    can->SaveAs(ofname);
    can->Clear();

    fHist1[2]->Draw("colz");
    map->Draw("textsame");
    can->SaveAs(ofname);
    can->Clear();

    fHist1[3]->Draw("colz");
    can->SaveAs(ofname);
    can->Clear();

    can->SetCanvasSize(1250,600);
    can->SetLogz(0);
    TText* title = new TText(0.7, 0.7, "");
    title->SetTextSize(0.05) ;
    TText* title2 = new TText(0.7, 0.65, "");
    title2->SetTextSize(0.05) ;

    TText* title3 = new TText(0.7, 0.7, "");
    title3->SetTextSize(0.05) ;
    TText* title4 = new TText(0.7, 0.65, "");
    title4->SetTextSize(0.05) ;

    TText* title5 = new TText(0.7, 0.7, "");
    title5->SetTextSize(0.05) ;
    TText* title6 = new TText(0.7, 0.65, "");
    title6->SetTextSize(0.05) ;

    TH2F* hist= NULL ;
    TH1D* proj= NULL ;

    Bool_t first=true;
    TH1F* tot= NULL ;
    TH1F* totCorr= NULL ;
    TH1F* totCorrG= NULL ;

    int tt = 34*34 ;
    for(int i=0; i<tt; ++i){
      if(i<35 || i>1120)      continue;
      if(i%34==0 || i%34==33) continue;

      can->Clear();
      can->Divide(2,1);
      can->cd(1);

      hist = fHist2[i];
      if(hist->GetEntries()<1000) hist->Draw("box");
      else hist->Draw("colz");
      if(hist->GetEntries()>10){
	hist->Fit("fit","Q");
	double a = fit->GetParameter(0);
	double b = fit->GetParError(0);
	double c = fit->GetParameter(1);
	double d = fit->GetParError(1);
	title->DrawTextNDC(0.45, 0.85, Form("p0= %.2f +/- %.2f", a, b) );
	title2->DrawTextNDC(0.45, 0.80, Form("p1= %.2f +/- %.2f", c, d) );
      }
      else{
	title->DrawTextNDC(0.45, 0.8, "Not enough events");
      }

      can->cd(2);
      TString nn = hist->GetTitle();
      nn = nn.ReplaceAll("Cluster time vs energy", "Primitive time resolution ");

      proj = hist->ProjectionY(Form("proj%i",i));

      if(first){
	tot = (TH1F*)proj->Clone("Total");
	tot->SetTitle("Time Offset for all supercells");
	totCorr = (TH1F*)proj->Clone("TotalCorrected");
	totCorr->SetTitle("Time Offset for all supercells, corrected");
	totCorr->Reset();

	totCorrG = (TH1F*)proj->Clone("TotalCorrected");
	totCorrG->SetTitle("Time Offset for all supercells, good corrected");
	totCorrG->Reset();
	first = false;
      }
      else{
	tot->Add(proj);
      }

      proj->SetTitle(nn);
      proj->Draw();
      proj->GetYaxis()->SetRangeUser(0.0, proj->GetMaximum()*1.2);

      double t0=0.0;
      double t0err=0.0;
      if(hist->GetEntries()>5){
	double xr = proj->GetMean();
	ggg->SetParameter(1, xr);
	ggg->SetParLimits(1, -10.0, 10.0);
	ggg->SetParameter(2, 2.0);
	ggg->SetParLimits(2, 0.0, 5.0);
	ggg->SetRange(xr-2.5, xr+2.5); // not sure this is helpful really.
	proj->Fit("ggg","QLR");
	double a = ggg->GetParameter(1);
	double b = ggg->GetParError(1);
	double c = ggg->GetParameter(2);
	double d = ggg->GetParError(2);
	// std::cout << " index " << i << " mean " << a << " +/- " << b
	// 	  << " sigma " << c << " +/- " << d << std::endl;
	title->DrawTextNDC(0.45, 0.85, Form("mean= %.2f +/- %.2f", a, b) );
	title2->DrawTextNDC(0.45, 0.80, Form("sigma= %.2f +/- %.2f", c, d) );
	mean->SetBinContent(i, a);
	t0    = a ;
	t0err = b ;
      }
      else{
	title->DrawTextNDC(0.45, 0.8, "Not enough events");
	mean->SetBinContent(i, 0);
	t0    = 0.0;
	t0err = 0.0;
      }
      ofile.width(6);
      ofile << Form("%.2f",t0);
      ofile.width(6);
      ofile << Form("%.2f", t0err);
      ofile << "  " << int(hist->GetEntries()) << std::endl;

      if(hist->GetEntries()>5){ // only for filled histograms
	for(Int_t j=1; j<proj->GetNbinsX()+1; ++j){
	  totCorr->Fill(proj->GetBinCenter(j)-t0, proj->GetBinContent(j));
	}

	if(fabs(t0)>t0err){
	  for(Int_t j=1; j<proj->GetNbinsX()+1; ++j){
	    totCorrG->Fill(proj->GetBinCenter(j)-t0, proj->GetBinContent(j));
	  }

	}
	else{
	  totCorrG->Add(proj);
	}
      }
      else{
	totCorr->Add(proj);
      }

      can->SaveAs(ofname);
      hist = NULL;
    }

    ofile.close();

    can->Clear();
    can->SetCanvasSize(700,500);
    gStyle->SetPaintTextFormat(".1f");
    const Int_t NCont = 255;
    const Int_t NRGBs = 5;
    Int_t MyPalette[NCont] ;
    Double_t stops[NRGBs] = { 0.00, 0.25, 0.50, 0.75, 1.00 };
    Double_t red[NRGBs]   = { 0.00, 0.00, 1.00, 1.00, 0.51 };
    Double_t green[NRGBs] = { 0.00, 0.81, 1.00, 0.20, 0.00 };
    Double_t blue[NRGBs]  = { 0.51, 1.00, 1.00, 0.00, 0.00 };
    Int_t FI = TColor::CreateGradientColorTable(NRGBs, stops, red, green, blue, NCont);
    for (Int_t i=0;i<NCont;i++) MyPalette[i] = FI+i;
    gStyle->SetPalette(NCont, MyPalette);
    gStyle->SetNumberContours(NCont);

    Double_t levels[256] = {0};
    for(int i=0; i<256; ++i){
      levels[i] = -10.0 + (20.0/256)*i;
    }
    mean->SetContour((sizeof(levels)/sizeof(Double_t)), levels);
    mean->Draw("colztext");
    mean->SetMinimum(-10.00001);
    can->SaveAs(ofname);
    if(fExtraOutput) {
        can->SaveAs("T0Map.eps");
        can->SaveAs("T0Map.pdf");
    }

    totCorr->SetLineWidth(2);
    totCorr->SetLineColor(kBlue); // blue
    totCorr->SetLineStyle(1); // solid

    totCorrG->SetLineWidth(2);
    totCorrG->SetLineColor(1); // black
    totCorrG->SetLineStyle(1); // solid

    tot->SetFillStyle(1001);
    tot->SetFillColor(16);
    tot->SetLineWidth(1);
    tot->SetLineColor(14);

    TF1* f1 = new TF1("f1", "gaus");
    f1->SetLineWidth(3);
    f1->SetLineColor(12);
    f1->SetLineStyle(2);
    f1->SetRange(tot->GetMean()-2.0, tot->GetMean()+2.0);
    f1->SetParameter(1,tot->GetMean());
    f1->SetParameter(2,2.0);
    tot->Fit("f1","R");

    TF1* f2 = new TF1("f2", "gaus");
    f2->SetLineWidth(3);
    f2->SetLineColor(kRed); // red
    f2->SetLineStyle(7); // long dash
    f2->SetRange(totCorrG->GetMean()-2.0, totCorrG->GetMean()+2.0);
    totCorrG->Fit("f2","R");

    TLine a(0.0,0.0,0.0,totCorr->GetMaximum()*1.05);
    a.SetLineColor(kBlack);
    a.SetLineStyle(3);
    a.SetLineWidth(1.0);

    can->Clear();
    totCorr->GetXaxis()->SetRangeUser(-10.0, 10.0);
    totCorr->Draw("hist");
    tot->Draw("same");
    totCorr->Draw("axissame");
    a.Draw("same");
    totCorr->Draw("histsame");
    totCorrG->Draw("histsame");
    f2->Draw("same");

    title->DrawTextNDC( 0.13, 0.85, "Uncorrected");
    title2->DrawTextNDC(0.13, 0.80, Form("mean= %.2f +/- %.2f",  f1->GetParameter(1), f1->GetParError(1)) );
    title3->DrawTextNDC(0.13, 0.75, Form("sigma= %.2f +/- %.2f", f1->GetParameter(2), f1->GetParError(2)) );

    title4->DrawTextNDC(0.58, 0.85, "T0 corrected");
    title5->DrawTextNDC(0.58, 0.80, Form("mean= %.2f +/- %.2f",  f2->GetParameter(1), f2->GetParError(1)) );
    title6->DrawTextNDC(0.58, 0.75, Form("sigma= %.2f +/- %.2f", f2->GetParameter(2), f2->GetParError(2)) );

    can->SaveAs(ofname);
    can->SaveAs(ofname+"]");

    if(fExtraOutput){
      can->SaveAs("Comparison.eps");
      can->SaveAs("Comparison.pdf");

      can->Clear();
      tot->Draw();
      tot->GetXaxis()->SetRangeUser(-10.0, 10.0);
      tot->GetYaxis()->SetRangeUser(0.0,totCorr->GetMaximum()*1.05);
      a.Draw("same");
      title->DrawTextNDC( 0.13, 0.85, "Uncorrected");
      title2->DrawTextNDC(0.13, 0.80, Form("mean= %.2f +/- %.2f",  f1->GetParameter(1), f1->GetParError(1)) );
      title3->DrawTextNDC(0.13, 0.75, Form("sigma= %.2f +/- %.2f", f1->GetParameter(2), f1->GetParError(2)) );

      can->SaveAs("Uncorrected.eps");
      can->SaveAs("Uncorrected.pdf");

      can->Clear();
      totCorrG->Draw("hist");
      totCorrG->GetXaxis()->SetRangeUser(-10.0, 10.0);
      a.Draw("same");
      title4->DrawTextNDC(0.58, 0.85, "T0 corrected");
      title5->DrawTextNDC(0.58, 0.80, Form("mean= %.2f +/- %.2f",  f2->GetParameter(1), f2->GetParError(1)) );
      title6->DrawTextNDC(0.58, 0.75, Form("sigma= %.2f +/- %.2f", f2->GetParameter(2), f2->GetParError(2)) );
      f2->Draw("same");

      can->SaveAs("Corrected.eps");
      can->SaveAs("Corrected.pdf");
    }
  } // fReadingData

  SaveAllPlots();
}

void L0CaloCalibration::EndOfJobUser(){
}

void L0CaloCalibration::DrawPlot(){
}

L0CaloCalibration::~L0CaloCalibration(){
}
