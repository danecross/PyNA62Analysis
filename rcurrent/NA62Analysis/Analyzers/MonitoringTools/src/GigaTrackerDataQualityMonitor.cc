#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "GigaTrackerDataQualityMonitor.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "BaseAnalysis.hh"
#include "TLine.h"
#include "TStyle.h"
#include <TMultiGraph.h>
#include "ConfigSettings.hh"
#include <TError.h>

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class GigaTrackerDataQualityMonitor
/// \Brief
/// GTK data monitor
/// \EndBrief
///
/// \Detailed
/// Monitor is working in two modes:
/// 1. Reads reconstructed data (with --no-check-badevent-all option)
/// 2. Reads its own output in --histo mode and produces pdf report and bad burst list
///
/// The efficiency is evaluated using k3pi and k2pi decays. Analyzer is simply calculating number of GTK candidates
/// in 4ns with k3pi/k2pi candidates.
/// \author Alina Kleimenova (alina.kleimenova@cern.ch)
/// \EndDetailed

GigaTrackerDataQualityMonitor::GigaTrackerDataQualityMonitor(Core::BaseAnalysis *ba) : Analyzer(ba, "GigaTrackerDataQualityMonitor") {
  Configuration::ConfigSettings::SetNoSkipBadBurst(true); // do not skip bad bursts

  RequestBeamSpecialTrigger();
  RequestL0SpecialTrigger();
  RequestL0Data();
  fTriggerConditions = TriggerConditions::GetInstance();
  fGigaTrackerEvent = new TRecoGigaTrackerEvent;
  RequestTree("GigaTracker", fGigaTrackerEvent,"Reco");

  fDirName = "GigaTrackerMonitor";
  AddParam("EffThreshold",   &fEffThreshold, 0.77);
  AddParam("ArgonionCountsMin", &fArgonionCountsMin, 1.e5);
  AddParam("NControlTriggersMin", &fNControlTriggersMin, 1.e3);
}
void GigaTrackerDataQualityMonitor::InitOutput(){}

void GigaTrackerDataQualityMonitor::InitHist(){
  Bool_t all = true;
  fReadingData = GetIsTree();
  TString type[3] = {"K2pi", "K3pi", ""};
  TString fHistoName;

  if (fReadingData) {
    std::cout << user_normal() << "Reading reco" << std::endl;

    BookHisto("hK3piGTKCandTimeDiff",
              new TH1F("hK3piGTKCandTimeDiff",
                       "Time difference between K3Pi and GTK candidate (Control); Time_{GTK}-Time_{K3Pi} [ns]", 400, -20, 20));

    BookHisto("hK2piGTKCandTimeDiff",
              new TH1F("hK2piGTKCandTimeDiff",
                       "Time difference between K2Pi and GTK candidate (Control); Time_{GTK}-Time_{K2Pi} [ns]", 400, -20, 20));

    BookHisto("hArgonionCountsVsBurstID", new TGraphErrors());
    fHArgonionCountsVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->SetName("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->Set(0);

    BookHisto("hGTKCriticalEventsVsBurstID", new TGraphErrors());
    fHGTKCriticalEventsVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hGTKCriticalEventsVsBurstID");
    fHGTKCriticalEventsVsBurstID->SetName("hGTKCriticalEventsVsBurstID");
    fHGTKCriticalEventsVsBurstID->Set(0);

    BookHisto("hControlTriggersVsBurstID", new TGraphErrors());
    fHControlTriggersVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hControlTriggersVsBurstID");
    fHControlTriggersVsBurstID->SetName("hControlTriggersVsBurstID");
    fHControlTriggersVsBurstID->Set(0);

    for (int i(0); i<3; i++){
      fHistoName = "hN" + type[i] + "EventsVsBurstID";
      BookHisto(Form(fHistoName), new TGraphErrors());
      fHNExpectedEventsVsBurstID[i] = (TGraphErrors*)fHisto.GetTGraph(Form(fHistoName));
      fHNExpectedEventsVsBurstID[i]->SetName(Form(fHistoName));
      fHNExpectedEventsVsBurstID[i]->Set(0);

      fHistoName = "hGTKEfficiency" + type[i] + "VsBurstID";
      BookHisto(Form(fHistoName), new TGraphErrors());
      fHEfficiencyVsBurstID[i] = (TGraphErrors*)fHisto.GetTGraph(Form(fHistoName));
      fHEfficiencyVsBurstID[i]->SetName(Form(fHistoName));
      fHEfficiencyVsBurstID[i]->Set(0);
    }
  }
  else {
    std::cout << user_normal() << "Reading my own output" << std::endl;
    fDirName = "GigaTrackerDataQualityMonitor";

    fHArgonionCountsVsBurstID = (TGraphErrors*)RequestHistogram(fDirName, "hArgonionCountsVsBurstID", all);
    fHControlTriggersVsBurstID  = (TGraphErrors*)RequestHistogram(fDirName, "hControlTriggersVsBurstID", all);
    fHGTKCriticalEventsVsBurstID    = (TGraphErrors*)RequestHistogram(fDirName, "hGTKCriticalEventsVsBurstID", all);

    for (int i(0); i<3; i++){
      if (i<2) {
        fHistoName = "h" + type[i] + "GTKCandTimeDiff";
        fHGTKCandidateTimeDiff[i] = static_cast<TH1F*>(RequestHistogram(fDirName, Form(fHistoName), all));
      }
      fHistoName = "hN" + type[i] + "EventsVsBurstID";
      fHNExpectedEventsVsBurstID[i] = (TGraphErrors*)RequestHistogram(fDirName, Form(fHistoName), all);

      fHistoName = "hGTKEfficiency" + type[i] + "VsBurstID";
      fHEfficiencyVsBurstID[i]      = (TGraphErrors*)RequestHistogram(fDirName, Form(fHistoName), all);
    }
  }
  /*
    ==================================================================================
    ============================= Request reco histograms ============================
    ==================================================================================
  */

  for(int iS(0); iS<3; iS++){
    //All bursts info
    fHHitMap_all[iS]    = static_cast<TH2D*>(RequestHistogram(fDirName+"/Reco", Form("HitMap_%d",iS+1), all,"Reco"));
    fHAbsTimeProfile_all[iS]  = static_cast<TH1F*>(RequestHistogram(fDirName+"/Reco", Form("AbsTimeProfile_%d",iS+1), all, "Reco"));

    fHToT_all[iS]     = static_cast<TH1F*>(RequestHistogram(fDirName+"/Reco", Form("ToT_%d_all",iS+1),  all, "Reco"));
    fHTime_all[iS]      = static_cast<TH1F*>(RequestHistogram(fDirName+"/Reco",Form("Time_%d_all",iS+1),  all, "Reco"));
    fHTimeProfile_all[iS]   = static_cast<TH2D*>(RequestHistogram(fDirName+"/Reco",Form("TimeProfile_%d_all",iS+1),  all, "Reco"));

  }

  fHDecoderErrors     = static_cast<TH2D*>(RequestHistogram(fDirName, "fHDecoderErrors", all));
  fHDecoderErrors_all     = static_cast<TH2D*>(RequestHistogram(fDirName, "DecoderErrors", all));
  fHDigiTimeRawFine_all   = static_cast<TH2D*>(RequestHistogram(fDirName, "DigiTimeRawFine", all));
  fHDigiTimeRawFineVsROChannel_all  = static_cast<TH2F*>(RequestHistogram(fDirName, "DigiTimeRawFineVsROChannel", all));

}

void GigaTrackerDataQualityMonitor::DefineMCSimple(){}

void GigaTrackerDataQualityMonitor::ProcessEOBEvent(){
  if(!fReadingData) return;
  fArgonionCounts = GetBeamSpecialTrigger()->GetARGONION().GetCounts()/1.e9;
}

void GigaTrackerDataQualityMonitor::StartOfBurstUser(){
  fBurstID = GetBurstID();
  fNGTKBadEvents = 0;
  fArgonionCounts = 0.;
  fNControl = 0;
  for (int i(0); i<3; i++){
    fNSelected[i] = 0.;
    fNExpected[i] = 0.;
  }
}

void GigaTrackerDataQualityMonitor::Process(Int_t){
  if (fReadingData){
    fNExpected[2]++;

    if ((GetEventHeader()->GetEventQualityMask()&(1<<kGigaTracker)))
      fNGTKBadEvents++;

    else fNSelected[2]++;

    GetL0Data();
    Bool_t ControlTrigger = false;
    ControlTrigger = fTriggerConditions->IsControlTrigger(GetL0Data());
    if (ControlTrigger) fNControl++;
    TRecoGigaTrackerEvent* Event = GetEvent<TRecoGigaTrackerEvent>();
    Int_t fNCandidates = Event->GetNCandidates();
    Bool_t Selected[2]={0,0};
    Selected[0] = *(Bool_t*)  GetOutput("K2piSelection.EventSelected");
    Selected[1] = *(Bool_t*)  GetOutput("K3piSelection.EventSelected");
    Double_t EventTime[2]={0.,0.};
    EventTime[0] = *(Double_t*)GetOutput("K2piSelection.K2piTime");
    EventTime[1] = *(Double_t*)GetOutput("K3piSelection.K3piTime");
    for (int i(0); i<2; i++){
      if (Selected[i] && ControlTrigger) {
        Double_t BestTimeDiff = 999.9;
        if (!(GetEventHeader()->GetEventQualityMask()&(1<<kGigaTracker))){
          fNExpected[i]++;
          for (int iCand = 0; iCand < fNCandidates; iCand++){
            TRecoGigaTrackerCandidate * Candidate = static_cast<TRecoGigaTrackerCandidate *>(Event->GetCandidate(iCand));
            Double_t TimeDiff = Candidate->GetTime() - EventTime[i];
            if (fabs(TimeDiff) < fabs (BestTimeDiff)){
              BestTimeDiff = TimeDiff;
            }
          }
          FillHisto(Form("hK%dpiGTKCandTimeDiff",i+2), BestTimeDiff);
          if (fabs(BestTimeDiff)<4.) fNSelected[i]++;
        }
      }
    }
  }
}

void GigaTrackerDataQualityMonitor::PostProcess(){}

void GigaTrackerDataQualityMonitor::EndOfBurstUser(){
  if (fReadingData){
    fHArgonionCountsVsBurstID->Set(fHArgonionCountsVsBurstID->GetN()+1);
    fHArgonionCountsVsBurstID->SetPoint(fHArgonionCountsVsBurstID->GetN()-1,fBurstID, fArgonionCounts);
    fHArgonionCountsVsBurstID->SetPointError(fHArgonionCountsVsBurstID->GetN()-1,0,0);
    fHControlTriggersVsBurstID->Set(fHControlTriggersVsBurstID->GetN()+1);
    fHControlTriggersVsBurstID->SetPoint(fHControlTriggersVsBurstID->GetN()-1,fBurstID, fNControl);
    fHControlTriggersVsBurstID->SetPointError(fHControlTriggersVsBurstID->GetN()-1,0,0);

    fHGTKCriticalEventsVsBurstID->Set(fHGTKCriticalEventsVsBurstID->GetN()+1);
    fHGTKCriticalEventsVsBurstID->SetPoint(fHGTKCriticalEventsVsBurstID->GetN()-1,fBurstID, fNGTKBadEvents);
    fHGTKCriticalEventsVsBurstID->SetPointError(fHGTKCriticalEventsVsBurstID->GetN()-1,0,0);

    for (int i(0); i<3; i++){
      Double_t eff = 0.;
      Double_t err = 0.;
      if (fNExpected[i] != 0.) {
        eff = fNSelected[i]/(1.0 * fNExpected[i]);
        err = sqrt(eff * (1. - eff) / (1.0 * fNExpected[i]));
      }
      fHEfficiencyVsBurstID[i]->Set(fHEfficiencyVsBurstID[i]->GetN()+1);
      fHEfficiencyVsBurstID[i]->SetPoint(fHEfficiencyVsBurstID[i]->GetN()-1,fBurstID, eff);
      fHEfficiencyVsBurstID[i]->SetPointError(fHEfficiencyVsBurstID[i]->GetN()-1,0, err);

      fHNExpectedEventsVsBurstID[i]->Set(fHNExpectedEventsVsBurstID[i]->GetN()+1);
      fHNExpectedEventsVsBurstID[i]->SetPoint(fHNExpectedEventsVsBurstID[i]->GetN()-1,fBurstID, fNExpected[i]);
      fHNExpectedEventsVsBurstID[i]->SetPointError(fHNExpectedEventsVsBurstID[i]->GetN()-1,0,0);
    }
  }
}

void GigaTrackerDataQualityMonitor::EndOfJobUser(){
  SaveAllPlots();

  if(!fReadingData){
    Bool_t kHistoCheck = true;
    kHistoCheck = !fHArgonionCountsVsBurstID || !fHControlTriggersVsBurstID;
    for (int i=0; i<3; i++)
      kHistoCheck = kHistoCheck || !fHNExpectedEventsVsBurstID[i] || !fHEfficiencyVsBurstID[i];

    if (kHistoCheck) {
      std::cout <<
        user_normal() << "Can't find needed histograms! Check if the input file is my own output!"
                << std::endl;
      return;
    }
    /*
      ==================================================================================
      ============================== print bad burst list ==============================
      ==================================================================================
    */
    ofstream BadBurstList;
    BadBurstList.open("GigaTrackerDataQualityMonitor_BadBurstList.dat");

    Int_t fNBadBursts = 0;
    Int_t fNBadBurstsLowEff = 0;
    Int_t fNBadBurstsLowStatK3Pi = 0;
    Int_t fNBadBurstsLowStatK2Pi = 0;
    Int_t fNEmptyBursts = 0;
    Int_t fMaxNBursts = fHEfficiencyVsBurstID[2]->GetN();

    for (int iPoint(0);iPoint<fHEfficiencyVsBurstID[2]->GetN(); iPoint++){
      Double_t BurstID = 0., NArgon = 0., NControl = 0.;
      fHArgonionCountsVsBurstID->GetPoint(iPoint, BurstID, NArgon);
      fHControlTriggersVsBurstID->GetPoint(iPoint, BurstID, NControl);
      for(int i(0); i<3; i++){
        fHNExpectedEventsVsBurstID[i]->GetPoint(iPoint, BurstID, fNExpected[i]);
        fHEfficiencyVsBurstID[i]->GetPoint(iPoint, BurstID, fNSelected[i]);
      }
      if (NControl < fNControlTriggersMin){
        if ((NArgon == 0) && (fNExpected[2]==0))
          BadBurstList <<
            Form("BadBurst %06d %04d EMPTYBURST", GetRunID(), (Int_t) BurstID) <<std::endl;
        else if (NArgon == 0)
          BadBurstList <<
            Form("BadBurst %06d %04d NOARGON", GetRunID(), (Int_t) BurstID) <<std::endl;
        else if (NArgon*1e9 < fArgonionCountsMin)
          BadBurstList <<
            Form("BadBurst %06d %04d LOWARGON ", GetRunID(), (Int_t) BurstID) << NArgon*1e9 <<std::endl;
        else
          BadBurstList <<
            Form("BadBurst %06d %04d LOWTRIGGER ", GetRunID(), (Int_t) BurstID)<< NControl <<std::endl;
        fNBadBursts++;
        fNEmptyBursts++;
      }
      else if (fNExpected[0]<30){
        if (fNSelected[2]<0.90)
          BadBurstList <<
            Form("BadBurst %06d %04d BADGTKDAQ Nk2pi = ", GetRunID(), (Int_t) BurstID) << fNExpected[0]
                                                                                        << "; Nk3pi = " << fNExpected[1] << "; GTKDAQEff = " << fNSelected[2] <<std::endl;
        else BadBurstList <<
               Form("BadBurst %06d %04d LOWSTATK2PI Nk2pi =  ", GetRunID(), (Int_t) BurstID) << fNExpected[0] << "; Nk3pi = " << fNExpected[1] <<std::endl;
        fNBadBurstsLowStatK2Pi++;
        fNBadBursts++;
      }
      else if (fNExpected[1]<15){
        if (fNSelected[2]<0.90)
          BadBurstList <<
            Form("BadBurst %06d %04d BADGTKDAQ Nk2pi = ", GetRunID(), (Int_t) BurstID) << fNExpected[0]
                                                                                        << "; Nk3pi = " << fNExpected[1] << "; GTKDAQEff = " << fNSelected[2] <<std::endl;
        else BadBurstList <<
               Form("BadBurst %06d %04d LOWSTATK3PI Nk3pi = ", GetRunID(), (Int_t) BurstID) << fNExpected[1] << "; Nk2pi = " << fNExpected[0] << std::endl;
        fNBadBurstsLowStatK3Pi++;
        fNBadBursts++;
      }
      else if ((fNSelected[0]<fEffThreshold) && (fNSelected[1]<fEffThreshold)){
        BadBurstList <<
          Form("BadBurst %06d %04d LOWEFFGTK", GetRunID(), (Int_t) BurstID) <<std::endl;
        fNBadBurstsLowEff++;
        fNBadBursts++;
      }
    }

    BadBurstList.close();
    std::cout << user_standard() << "------------------------------------------------------" << std::endl;
    std::cout << user_standard() << "                   Bad burst summary" << std::endl;
    std::cout << user_standard() << "------------------------------------------------------" << std::endl;
    std::cout << user_standard() << "Number of bursts                                "
              << Form("%04d",fMaxNBursts) << std::endl;
    std::cout << user_standard() << "Total number of bad bursts                      "
              << Form("%04d",fNBadBursts) << std::endl;
    std::cout << user_standard() << "Number of empty bursts                          "
              << Form("%04d",fNEmptyBursts) << std::endl;
    std::cout << user_standard() << "Number of bad bursts due to low K2Pi statistics "
              << Form("%04d",fNBadBurstsLowStatK2Pi) << std::endl;
    std::cout << user_standard() << "Number of bad bursts due to low K3Pi statistics "
              << Form("%04d",fNBadBurstsLowStatK3Pi) << std::endl;
    std::cout << user_standard() << "Number of bad bursts due to low GTK efficiency  "
              << Form("%04d",fNBadBurstsLowEff) << std::endl;
    std::cout << user_standard() << "------------------------------------------------------" << std::endl;
    /*
      ==================================================================================
      ================================ build PDF report ================================
      ==================================================================================
    */
    gErrorIgnoreLevel = 5000;
    TCanvas *Canvas;
    Canvas = new TCanvas("GeneralPlots");
    TString fOutputName = "GigaTrackerDataQualityMonitor.pdf";
    for (int iPoint(0);iPoint<fHEfficiencyVsBurstID[2]->GetN(); iPoint++){
      Double_t BurstID=0., NControl=0.;
      fHControlTriggersVsBurstID->GetPoint(iPoint, BurstID, NControl);
      if (NControl < fNControlTriggersMin){
        fHControlTriggersVsBurstID->RemovePoint(iPoint);
        for (int i(0);i<3;i++) fHEfficiencyVsBurstID[i]->RemovePoint(iPoint);
        iPoint--;
      }
    }

    kHistoCheck = !fHDigiTimeRawFine_all || (!fHDecoderErrors_all && !fHDecoderErrors);
    for (int i=0; i<3; i++)
      kHistoCheck = kHistoCheck || !fHHitMap_all[i] || !fHAbsTimeProfile_all[i] || 
                    !fHTime_all[i] || !fHTimeProfile_all[i] || !fHToT_all[i];

    Canvas->cd();
    TString type[3] = {"K2pi", "K3pi", "Good events"};
    EColor Color[3] = {kViolet, kOrange, kAzure};
    TMultiGraph *MultiGraph = new TMultiGraph();
    MultiGraph->SetTitle(Form("GTK efficiency vs BurstID for run %d",GetRunID()));
    TLegend* Legend = new TLegend(0.8,0.15,0.99,0.35);//(0.1,0.7,0.48,0.9);
    Legend->SetHeader("From:");
    for (int i=0; i<3; i++){
      fHEfficiencyVsBurstID[i]->SetTitle(Form("GTK efficiency vs BurstID for run %d", GetRunID()));
      fHEfficiencyVsBurstID[i]->SetMarkerStyle(23-i);
      fHEfficiencyVsBurstID[i]->SetMarkerSize(0.4);
      fHEfficiencyVsBurstID[i]->SetMarkerColor(-2*i + Color[i]);
      fHEfficiencyVsBurstID[i]->SetLineColor(-2*i + Color[i]);
      MultiGraph->Add(fHEfficiencyVsBurstID[i],"P");
      Legend->AddEntry(fHEfficiencyVsBurstID[i], Form(type[i]),"lep");
    }

    MultiGraph->Draw("AP");
    MultiGraph->GetXaxis()->SetTitle("Burst ID");
    MultiGraph->GetYaxis()->SetTitle("Efficiency");
    MultiGraph->GetYaxis()->SetRangeUser(-0.05, 1.05);
    TLine *ThresholdLine =
      new TLine(MultiGraph->GetXaxis()->GetXmin(),fEffThreshold,
                MultiGraph->GetXaxis()->GetXmax(),fEffThreshold);
    ThresholdLine->SetLineColor(kTeal+4);
    ThresholdLine->Draw();
    Legend->AddEntry(ThresholdLine,Form("Threshold=%.02f",fEffThreshold),"l");
    Legend->Draw();
    Canvas->Print(Form(fOutputName+"("), "pdf");

    // I do it just in case someone will run it on the output of previous version.
    if (!(!fHGTKCandidateTimeDiff[0] || !fHGTKCandidateTimeDiff[1])){
      Canvas->Clear();
      Canvas->Divide(2,1);
      for (int i(0); i<2; i++){
        Canvas->cd(i+1);
        fHGTKCandidateTimeDiff[i]->GetXaxis()->SetRangeUser(-10.0, 10.0);
        fHGTKCandidateTimeDiff[i]->SetLineColor(kBlack);
        fHGTKCandidateTimeDiff[i]->Draw();
        gStyle->SetOptStat("emr");
      }
      Canvas->Print(fOutputName, "pdf");
    }
    if (kHistoCheck) {
      Canvas->Print(Form(fOutputName+"]"), "pdf");
      std::cout <<
        user_normal() << "Can't find Reco histograms!"
                << std::endl;
      delete Legend;
      delete Canvas;
      return;
    }

    else{
      Canvas->Clear();
      Canvas->Divide(1,2);
      Canvas->cd(1);
      if (!fHDecoderErrors_all) {
        fHDecoderErrors->Draw("colz");
        fHDecoderErrors->SetMinimum(1);
      }
      if (!fHDecoderErrors) {
        fHDecoderErrors_all->Draw("colz");
        fHDecoderErrors_all->SetMinimum(1);
      }
      gPad->SetLogz();
      gStyle->SetOptStat("e");

      Canvas->cd(2);
      fHDigiTimeRawFine_all->Draw("colz");
      fHDigiTimeRawFine_all->GetYaxis()->SetRangeUser(-70.,70.);
      fHDigiTimeRawFine_all->SetMinimum(1);
      gPad->SetLogz();
      gStyle->SetOptStat("e");

      Canvas->Print(fOutputName, "pdf");
      Canvas->Clear();

      for (Int_t iS(0); iS<3; iS++){
        Canvas->Divide(2,2);
        Canvas->cd(1);
        fHHitMap_all[iS]->Draw("colz");
        fHHitMap_all[iS]->SetMinimum(1);
        gPad->SetLogz();
        gStyle->SetOptStat("e");

        Canvas->cd(2);
        fHToT_all[iS]->Draw();
        fHToT_all[iS]->SetMinimum(0);
        gStyle->SetOptStat("emr");

        Canvas->cd(3);
        fHTimeProfile_all[iS]->Draw("colz");
        fHTimeProfile_all[iS]->SetMinimum(1);
        gPad->SetLogz();
        gStyle->SetOptStat("e");

        Canvas->cd(4);
        fHTime_all[iS]->Draw();
        fHTime_all[iS]->SetMinimum(0);
        gStyle->SetOptStat("e");

        Canvas->Print(fOutputName, "pdf");
        Canvas->Clear();
      }
      Canvas->Print(Form(fOutputName+"]"), "pdf");
      delete Legend;
      delete Canvas;
    }
  }
}

void GigaTrackerDataQualityMonitor::DrawPlot(){

}

GigaTrackerDataQualityMonitor::~GigaTrackerDataQualityMonitor(){}
