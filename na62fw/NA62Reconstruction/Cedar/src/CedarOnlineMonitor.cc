// ---------------------------------------------------------------
// History:
//
// 2014 Online Monitor - Karim Massri (karim.massri@cern.ch) 2014-09-30
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)        2012-07-16
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TStyle.h"
#include "NA62Global.hh"

#include "NA62Reconstruction.hh"
#include "CedarOnlineMonitor.hh"
#include "CedarReconstruction.hh"

CedarOnlineMonitor::CedarOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco,"Cedar"), fPadMap(nullptr), fCedarAlignmentCanvas(nullptr), fCedarAlignmentTrendsCanvas(nullptr), fCedarAlignmentText(nullptr), fCedarAsymText(nullptr), fHNCoincidencesPerBurst(nullptr), fHNCoincidencesPerPressure(nullptr), fHNHitsInCandidatePerBurst(nullptr), fHPressurePerBurst(nullptr){

#ifdef DIM
  fAlignSugXService = new DimService("NA62/OM/KTAG/MOTOR_SUG_X",fAlignSugX);
  fAlignSugYService = new DimService("NA62/OM/KTAG/MOTOR_SUG_Y",fAlignSugY);
#endif

  //mapping between Pads and KTAG Sector IDs
  Int_t PadMap[16] = {0,8,1,0,7,0,0,2,6,0,0,3,0,5,4,0};
  fPadMap = new Int_t[16];
  for(int i=0;i<16;i++) fPadMap[i] = PadMap[i]; //initialize fPadMap

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
} 

CedarOnlineMonitor::~CedarOnlineMonitor() {
#ifdef DIM
  if(fAlignSugXService) delete fAlignSugXService;
  if(fAlignSugYService) delete fAlignSugYService;
#endif
}

void CedarOnlineMonitor::CreateShifterModeTabs(){

  CedarReconstruction * CedarReco = static_cast<CedarReconstruction*>( fReco );

  NA62VOnlineMonitorCanvas * ChannelProfile = AddCanvasTab("ChannelProfile");
  ChannelProfile->Divide(4,4);
  for(int iPad=0;iPad<16;iPad++){
    if(!fPadMap[iPad]) continue;

    ChannelProfile->cd(iPad+1);
    gPad->SetLeftMargin(0.10);
    gPad->SetRightMargin(0.05);
    gPad->SetTopMargin(0.06);
    gPad->SetBottomMargin(0.06);

    CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],0)->GetXaxis()->SetTitleSize(0.06);
    CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],0)->GetXaxis()->SetLabelSize(0.06);
    CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],0)->GetYaxis()->SetTitleSize(0.06);
    CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],0)->GetYaxis()->SetLabelSize(0.06);
    ChannelProfile->GetCurrentFrame()->DrawHisto(CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],0),"COLZ");
  }

  NA62VOnlineMonitorCanvas * Summary = AddCanvasTab("Summary");
  Double_t x1 = 0.65;
  Double_t y1 = 0.65;
  Double_t x2 = 0.98;
  Double_t y2 = 0.85;
  TLegend * NCandidatesLegend = new TLegend(x1,y1,x2,y2);
  NCandidatesLegend->SetFillColor(kWhite);
  Summary->Divide(2,2);

  Summary->cd(1);
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNSectorsInCandidate());
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNSectorsInSelectedCandidate(),"same");
  NCandidatesLegend->AddEntry(CedarReco->GetHNSectorsInCandidate(),"Candidates", "L");
  NCandidatesLegend->AddEntry(CedarReco->GetHNSectorsInSelectedCandidate(),"Selected Candidates", "L");
  NCandidatesLegend->Draw();

  Summary->cd(2);
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNRecoHitsInCandidate());
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNRecoHitsInSelectedCandidate(),"same");
  NCandidatesLegend->Draw();

  Summary->cd(3);
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHRecoHitTimeWrtCandidate());

  Summary->cd(4);
  gPad->SetLogz(1);
  Double_t TimeWindow = CedarReco->GetTimeWindow();
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHRecoHitTimeWrtReferenceVsROChannel(),"COLZ");
  CedarReco->GetHRecoHitTimeWrtReferenceVsROChannel()->GetYaxis()->SetRangeUser(-TimeWindow,TimeWindow);

}

void CedarOnlineMonitor::CreateExpertModeTabs(){

  CedarReconstruction * CedarReco = static_cast<CedarReconstruction*>( fReco );

  const Int_t NTrigs = 2;
  TString TrigSuffix[NTrigs] = {
    "",
    "_EOB"
  };

  for(int iTrig=0; iTrig<NTrigs; iTrig++){
    NA62VOnlineMonitorCanvas * ChannelProfile = AddCanvasTab(Form("ChannelProfile%s",TrigSuffix[iTrig].Data()));
    ChannelProfile->Divide(4,4);
    for(int iPad=0;iPad<16;iPad++){
      if(!fPadMap[iPad]) continue;

      ChannelProfile->cd(iPad+1);
      gPad->SetLeftMargin(0.10);
      gPad->SetRightMargin(0.05);
      gPad->SetTopMargin(0.06);
      gPad->SetBottomMargin(0.06);

      CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],iTrig)->GetXaxis()->SetTitleSize(0.06);
      CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],iTrig)->GetXaxis()->SetLabelSize(0.06);
      CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],iTrig)->GetYaxis()->SetTitleSize(0.06);
      CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],iTrig)->GetYaxis()->SetLabelSize(0.06);
      ChannelProfile->GetCurrentFrame()->DrawHisto(CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],iTrig),"COLZ");
    }
  }

  NA62VOnlineMonitorCanvas * SignalQuality = AddCanvasTab("SignalQuality");
  SignalQuality->Divide(2,2);

  SignalQuality->cd(1);
  SignalQuality->GetCurrentFrame()->DrawHisto(CedarReco->GetHHitStatus());

  SignalQuality->cd(2);
  SignalQuality->GetCurrentFrame()->DrawHisto(CedarReco->GetHTimeWidth());

  SignalQuality->cd(3);
  gPad->SetLogz(1);
  Double_t TimeWindow = CedarReco->GetTimeWindow();
  SignalQuality->GetCurrentFrame()->DrawHisto(CedarReco->GetHRecoHitTimeWrtReferenceVsROChannel(),"COLZ");
  CedarReco->GetHRecoHitTimeWrtReferenceVsROChannel()->GetYaxis()->SetRangeUser(-TimeWindow,TimeWindow);

  SignalQuality->cd(4);
  gPad->SetLogz(1);
  SignalQuality->GetCurrentFrame()->DrawHisto(CedarReco->GetHRecoHitTimeWrtCandidateVsWidth(),"COLZ");

  //ALIGNMENT
  if(CedarReco->GetAlignEnabled()){
    fCedarAlignmentCanvas = AddCanvasTab("CedarAlignment");
    fCedarAlignmentCanvas->Divide(4,2);

    fCedarAlignmentCanvas->cd(1);
    fCedarAlignmentCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentSectorsVis(),"COLZ");
    fCedarAsymText = new TPaveText( 0.3, 0.4, 0.7, 0.6, "NB,NDC" );
    fCedarAsymText->SetBorderSize(0);
    fCedarAsymText->Draw();

    fCedarAlignmentCanvas->cd(2);
    fCedarAlignmentCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentOctantsVis(),"COLZ");

    fCedarAlignmentCanvas->cd(3);
    fCedarAlignmentCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentSixthsVis(),"COLZ");

    fCedarAlignmentCanvas->cd(4);
    fCedarAlignmentCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentCoarseVis(),"COLZ");

    fCedarAlignmentCanvas->cd(5);
    fCedarAlignmentCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentPMTsVis(),"COLZ");

    fCedarAlignmentCanvas->cd(6);
    fCedarAlignmentCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentSixthsFitVis(),"COLZ");

    fCedarAlignmentCanvas->cd(7);
    fCedarAlignmentCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentOctantsFitVis(),"COLZ");

    fCedarAlignmentCanvas->cd(8);
    fCedarAlignmentText = new TPaveText(0.05, 0.05, 0.95, 0.95, "NB");
    fCedarAlignmentText->SetBorderSize(0);
    fCedarAlignmentText->Draw();

    fCedarAlignmentTrendsCanvas = AddCanvasTab("CedarAlignmentTrends");
    fCedarAlignmentTrendsCanvas->Divide( 2, 2 );

    fCedarAlignmentTrendsCanvas->cd(1);
    fCedarAlignmentTrendsCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentAsymUD());

    fCedarAlignmentTrendsCanvas->cd(2);
    fCedarAlignmentTrendsCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetHAlignmentAsymSJ());

    fCedarAlignmentTrendsCanvas->cd(3);
    TGraph * gAlignmentSugXYTrend = CedarReco->GetGAlignmentSugXYTrend();
    gAlignmentSugXYTrend->SetMarkerStyle( 21 );
    gAlignmentSugXYTrend->SetMarkerSize( 0.4 );
    fCedarAlignmentTrendsCanvas->GetCurrentFrame()->DrawHisto(CedarReco->GetGAlignmentSugXYTrend(),"AP");
  }
  //--------------------------------------------------

  NA62VOnlineMonitorCanvas * CedarStability = AddCanvasTab("Stability");
  CedarStability->Divide(2,2);
  Int_t NMinCoincidences = 4;
  Int_t NMaxCoincidences = 8;
  if (fReco->GetConfigFileName().Contains("2012")) { //2012 settings
    NMinCoincidences = 2;
    NMaxCoincidences = 4;
  }
  CedarStability->cd(1);
  gPad->SetLogy(1);
  TString DrawOption = "ALP";
  TLegend * NCoincidencesPerBurstLegend = new TLegend(0.85,0.75,0.98,0.95);
  NCoincidencesPerBurstLegend->SetFillColor(kWhite);
  fHNCoincidencesPerBurst = new TGraph*[8];
  for(int iCoinc=0;iCoinc<8;iCoinc++) fHNCoincidencesPerBurst[iCoinc] = 0;
  for(int iCoinc=NMinCoincidences;iCoinc<=NMaxCoincidences;iCoinc++){
    if(iCoinc>NMinCoincidences) DrawOption = "LP";
    fHNCoincidencesPerBurst[iCoinc-1] = new TGraph();
    fHNCoincidencesPerBurst[iCoinc-1]->Set(1);
    fHNCoincidencesPerBurst[iCoinc-1]->SetPoint(0,-1,0);
    fHNCoincidencesPerBurst[iCoinc-1]->SetTitle("NCoincidences VS Burst number;BurstID;Coincidences/Triggers");
    fHNCoincidencesPerBurst[iCoinc-1]->SetMarkerStyle(20);
    fHNCoincidencesPerBurst[iCoinc-1]->SetMarkerSize(0.7);
    fHNCoincidencesPerBurst[iCoinc-1]->SetMarkerColor(iCoinc+1);
    fHNCoincidencesPerBurst[iCoinc-1]->SetLineColor(iCoinc+1);
    fHNCoincidencesPerBurst[iCoinc-1]->SetLineWidth(3);
    CedarStability->GetCurrentFrame()->DrawHisto(fHNCoincidencesPerBurst[iCoinc-1],DrawOption);
    NCoincidencesPerBurstLegend->AddEntry(fHNCoincidencesPerBurst[iCoinc-1],Form("%d-fold", iCoinc), "LP");
  }
  NCoincidencesPerBurstLegend->Draw();
  CedarStability->cd(2);
  fHNHitsInCandidatePerBurst = new TGraph();
  fHNHitsInCandidatePerBurst->Set(1);
  fHNHitsInCandidatePerBurst->SetPoint(0,-1,0);
  fHNHitsInCandidatePerBurst->SetTitle("NHitsInCandidate VS Burst number;BurstID;NHitsInCandidate");
  fHNHitsInCandidatePerBurst->SetMarkerStyle(20);
  fHNHitsInCandidatePerBurst->SetMarkerSize(0.7);
  fHNHitsInCandidatePerBurst->SetLineWidth(3);
  CedarStability->GetCurrentFrame()->DrawHisto(fHNHitsInCandidatePerBurst,"ALP");

  CedarStability->cd(3);
  gPad->SetLogy(1);
  DrawOption = "ALP";
  TLegend * NCoincidencesPerPressureLegend = new TLegend(0.85,0.75,0.98,0.95);
  NCoincidencesPerPressureLegend->SetFillColor(kWhite);
  fHNCoincidencesPerPressure = new TGraph*[8];
  for(int iCoinc=0;iCoinc<8;iCoinc++) fHNCoincidencesPerPressure[iCoinc] = 0;
  for(int iCoinc=NMinCoincidences;iCoinc<=NMaxCoincidences;iCoinc++){
    if(iCoinc>NMinCoincidences) DrawOption = "LP";
    fHNCoincidencesPerPressure[iCoinc-1] = new TGraph();
    fHNCoincidencesPerPressure[iCoinc-1]->Set(1);
    fHNCoincidencesPerPressure[iCoinc-1]->SetPoint(0,-1,0);
    fHNCoincidencesPerPressure[iCoinc-1]->SetTitle("NCoincidences VS Pressure;Pressure [bar];Coincidences/Triggers");
    fHNCoincidencesPerPressure[iCoinc-1]->SetMarkerStyle(20);
    fHNCoincidencesPerPressure[iCoinc-1]->SetMarkerSize(0.7);
    fHNCoincidencesPerPressure[iCoinc-1]->SetMarkerColor(iCoinc+1);
    fHNCoincidencesPerPressure[iCoinc-1]->SetLineColor(iCoinc+1);
    fHNCoincidencesPerPressure[iCoinc-1]->SetLineWidth(3);
    CedarStability->GetCurrentFrame()->DrawHisto(fHNCoincidencesPerPressure[iCoinc-1],DrawOption);
    NCoincidencesPerPressureLegend->AddEntry(fHNCoincidencesPerPressure[iCoinc-1],Form("%d-fold", iCoinc), "LP");
  }
  NCoincidencesPerPressureLegend->Draw();
  CedarStability->cd(4);
  fHPressurePerBurst = new TGraph();
  fHPressurePerBurst->Set(1);
  fHPressurePerBurst->SetPoint(0,-1,0);
  fHPressurePerBurst->SetTitle("Pressure VS Burst;BurstID;Pressure [bar]");
  fHPressurePerBurst->SetMarkerStyle(20);
  fHPressurePerBurst->SetMarkerSize(0.7);
  fHPressurePerBurst->SetMarkerColor(1);
  fHPressurePerBurst->SetLineColor(1);
  fHPressurePerBurst->SetLineWidth(3);
  CedarStability->GetCurrentFrame()->DrawHisto(fHPressurePerBurst,"ALP");

  NA62VOnlineMonitorCanvas * Summary = AddCanvasTab("Summary");
  Double_t x1 = 0.65;
  Double_t y1 = 0.65;
  Double_t x2 = 0.98;
  Double_t y2 = 0.85;
  TLegend * NCandidatesLegend = new TLegend(x1,y1,x2,y2);
  NCandidatesLegend->SetFillColor(kWhite);
  Summary->Divide(2,2);

  Summary->cd(4);
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNCandidates());
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNSelectedCandidates(),"same");
  NCandidatesLegend->AddEntry(CedarReco->GetHNCandidates(),"Candidates", "L");
  NCandidatesLegend->AddEntry(CedarReco->GetHNSelectedCandidates(),"Selected Candidates", "L");
  NCandidatesLegend->Draw();

  Summary->cd(1);
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNSectorsInCandidate());
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNSectorsInSelectedCandidate(),"same");
  NCandidatesLegend->Draw();

  Summary->cd(2);
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNRecoHitsInCandidate());
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHNRecoHitsInSelectedCandidate(),"same");
  NCandidatesLegend->Draw();

  Summary->cd(3);
  Summary->GetCurrentFrame()->DrawHisto(CedarReco->GetHRecoHitTimeWrtCandidate());
}



void CedarOnlineMonitor::Update(Int_t BurstID) {
  CedarReconstruction * CedarReco = static_cast<CedarReconstruction*>( fReco );

  //--- update ChannelProfile tabs
  const Int_t NTrigs = 2;
  if(CedarReco->GetHChannelProfileInSectorVis()){
    for(int iTrig=0; iTrig<NTrigs; iTrig++){
      Double_t MaxChannelProfile = 0.;
      for(int iPad=0;iPad<16;iPad++){
        if(!fPadMap[iPad]) continue;
        TH2I * hChannelProfile = CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],iTrig);
        if(hChannelProfile->GetBinContent(hChannelProfile->GetMaximumBin())>MaxChannelProfile)
          MaxChannelProfile = hChannelProfile->GetBinContent(hChannelProfile->GetMaximumBin());
      }
      MaxChannelProfile*=1.1;
      for(int iPad=0;iPad<16;iPad++){
        if(!fPadMap[iPad]) continue;
        CedarReco->GetHChannelProfileInSectorVis(fPadMap[iPad],iTrig)->SetMaximum(MaxChannelProfile);
      }
    }
  }

  //--- update NRecoHitsInSector tab
  if(CedarReco->GetHNRecoHitsInCandidateInSector()){
    Double_t MaxNRecoHitsInSector = 0.;
    for(int iPad=0;iPad<16;iPad++){
      if(!fPadMap[iPad]) continue;
      TH1D * hNRecoHitsInSector = CedarReco->GetHNRecoHitsInCandidateInSector(fPadMap[iPad]);
      if(hNRecoHitsInSector->GetBinContent(hNRecoHitsInSector->GetMaximumBin())>MaxNRecoHitsInSector)
        MaxNRecoHitsInSector = hNRecoHitsInSector->GetBinContent(hNRecoHitsInSector->GetMaximumBin());
    }
    MaxNRecoHitsInSector*=1.1;
    for(int iPad=0;iPad<16;iPad++){
      if(!fPadMap[iPad]) continue;
      CedarReco->GetHNRecoHitsInCandidateInSector(fPadMap[iPad])->SetMaximum(MaxNRecoHitsInSector);
    }
  }

  //--- update HitTimePerSector tab
  if(CedarReco->GetHRecoHitTimeWrtCandidateInSector()){
    Double_t MaxHitTimePerSector = 0.;
    for(int iPad=0;iPad<16;iPad++){
      if(!fPadMap[iPad]) continue;
      TH1D * hHitTimePerSector = CedarReco->GetHRecoHitTimeWrtCandidateInSector(fPadMap[iPad]);
      if(hHitTimePerSector->GetBinContent(hHitTimePerSector->GetMaximumBin())>MaxHitTimePerSector)
        MaxHitTimePerSector = hHitTimePerSector->GetBinContent(hHitTimePerSector->GetMaximumBin());
    }
    MaxHitTimePerSector*=1.1;
    for(int iPad=0;iPad<16;iPad++){
      if(!fPadMap[iPad]) continue;
      CedarReco->GetHRecoHitTimeWrtCandidateInSector(fPadMap[iPad])->SetMaximum(MaxHitTimePerSector);
    }
  }

  //--- update CedarSummary tab
  Double_t ymaxCand = CedarReco->GetHNCandidates()->GetBinContent(CedarReco->GetHNCandidates()->GetMaximumBin());
  Double_t ymaxSelectedCand = CedarReco->GetHNSelectedCandidates()->GetBinContent(CedarReco->GetHNSelectedCandidates()->GetMaximumBin());
  if (ymaxCand<ymaxSelectedCand) ymaxCand = ymaxSelectedCand;
  ymaxCand *= 1.1;
  CedarReco->GetHNCandidates()->SetMaximum(ymaxCand);

  //--- --- update CedarStability tab
  Double_t LastValueX=-1., LastValueY=0.;
  // CoincidencesPerBurst
  if(fHNCoincidencesPerBurst) {
    for(int iCoinc=1;iCoinc<=8;iCoinc++){
      if(!fHNCoincidencesPerBurst[iCoinc-1]) continue;
      if(fHNCoincidencesPerBurst[iCoinc-1]->GetN()>0) {
        fHNCoincidencesPerBurst[iCoinc-1]->GetPoint(fHNCoincidencesPerBurst[iCoinc-1]->GetN()-1,LastValueX,LastValueY);
        if(LastValueX==BurstID || LastValueX<0) fHNCoincidencesPerBurst[iCoinc-1]->RemovePoint(fHNCoincidencesPerBurst[iCoinc-1]->GetN()-1); //remove last point
      }
      fHNCoincidencesPerBurst[iCoinc-1]->Set(fHNCoincidencesPerBurst[iCoinc-1]->GetN()+1);
      Double_t CoincidencesPerTrigger = (Double_t)CedarReco->GetNCoincidencesIntegrated(iCoinc-1)/static_cast<NA62Reconstruction*>(CedarReco->GetMainReco())->GetNProcessedEventsInFile();
      fHNCoincidencesPerBurst[iCoinc-1]->SetPoint(fHNCoincidencesPerBurst[iCoinc-1]->GetN()-1,BurstID,CoincidencesPerTrigger);
    }
    // Find maxima and minima
    Double_t yMax = 0; 
    Double_t yMin = 0.8e-6; //default
    for(Int_t iCoinc=1;iCoinc<=8;iCoinc++){
      if(!fHNCoincidencesPerBurst[iCoinc-1]) continue;
      Double_t yMax_i = TMath::MaxElement(fHNCoincidencesPerBurst[iCoinc-1]->GetN(),fHNCoincidencesPerBurst[iCoinc-1]->GetY());
      Double_t yMin_i = TMath::MinElement(fHNCoincidencesPerBurst[iCoinc-1]->GetN(),fHNCoincidencesPerBurst[iCoinc-1]->GetY());
      if(yMax<yMax_i) yMax = yMax_i;
      if(yMin>yMin_i) yMin = yMin_i;
    }
    yMax*=1.1;
    yMin/=1.1;
    for(int iCoinc=1;iCoinc<=8;iCoinc++){
      if(!fHNCoincidencesPerBurst[iCoinc-1]) continue;
      fHNCoincidencesPerBurst[iCoinc-1]->SetMinimum(yMin);
      fHNCoincidencesPerBurst[iCoinc-1]->SetMaximum(yMax);
    }
  }
  // NHitsInCandidatePerBurst
  if(fHNHitsInCandidatePerBurst){
    if(fHNHitsInCandidatePerBurst->GetN()>0) {
      fHNHitsInCandidatePerBurst->GetPoint(fHNHitsInCandidatePerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHNHitsInCandidatePerBurst->RemovePoint(fHNHitsInCandidatePerBurst->GetN()-1); //remove last point
    }
    fHNHitsInCandidatePerBurst->Set(fHNHitsInCandidatePerBurst->GetN()+1);
    Double_t NMeanHitsInCandidate = CedarReco->GetHNRecoHitsInSelectedCandidate()->GetMean();
    fHNHitsInCandidatePerBurst->SetPoint(fHNHitsInCandidatePerBurst->GetN()-1,BurstID,NMeanHitsInCandidate);
  }
  // CoincidencesPerPressure
  Double_t CurrentPressure = CedarReco->GetDIMInfo().GetPressure();
  Double_t DeltaPressure = 0.001; //1 mbar approximation
  if(CurrentPressure>0){
    if(fHNCoincidencesPerPressure) {
      for(int iCoinc=1;iCoinc<=8;iCoinc++){
        if(!fHNCoincidencesPerPressure[iCoinc-1]) continue;
        if(fHNCoincidencesPerPressure[iCoinc-1]->GetN()>0) {
          fHNCoincidencesPerPressure[iCoinc-1]->GetPoint(fHNCoincidencesPerPressure[iCoinc-1]->GetN()-1,LastValueX,LastValueY);
          if(abs(LastValueX-CurrentPressure)<=DeltaPressure || LastValueX<=0) fHNCoincidencesPerPressure[iCoinc-1]->RemovePoint(fHNCoincidencesPerPressure[iCoinc-1]->GetN()-1); //remove last point
        }
        fHNCoincidencesPerPressure[iCoinc-1]->Set(fHNCoincidencesPerPressure[iCoinc-1]->GetN()+1);
        Double_t CoincidencesPerTrigger = (Double_t)CedarReco->GetNCoincidencesIntegrated(iCoinc-1)/static_cast<NA62Reconstruction*>(CedarReco->GetMainReco())->GetNProcessedEventsInFile();
        fHNCoincidencesPerPressure[iCoinc-1]->SetPoint(fHNCoincidencesPerPressure[iCoinc-1]->GetN()-1,CurrentPressure,CoincidencesPerTrigger);
      }
      // Find maxima and minima
      Double_t xMax = 0.;
      Double_t xMin = 0.;
      Double_t yMax = 0; 
      Double_t yMin = 0.8e-8; //default
      for(Int_t iCoinc=1;iCoinc<=8;iCoinc++){
        if(!fHNCoincidencesPerPressure[iCoinc-1]) continue;
        Double_t xMax_i = TMath::MaxElement(fHNCoincidencesPerPressure[iCoinc-1]->GetN(),fHNCoincidencesPerPressure[iCoinc-1]->GetX());
        Double_t xMin_i = TMath::MinElement(fHNCoincidencesPerPressure[iCoinc-1]->GetN(),fHNCoincidencesPerPressure[iCoinc-1]->GetX());
        Double_t yMax_i = TMath::MaxElement(fHNCoincidencesPerPressure[iCoinc-1]->GetN(),fHNCoincidencesPerPressure[iCoinc-1]->GetY());
        Double_t yMin_i = TMath::MinElement(fHNCoincidencesPerPressure[iCoinc-1]->GetN(),fHNCoincidencesPerPressure[iCoinc-1]->GetY());
        if(xMax<xMax_i) xMax = xMax_i;
        if(xMin>xMin_i) xMin = xMin_i;
        if(yMax<yMax_i) yMax = yMax_i;
        if(yMin>yMin_i) yMin = yMin_i;
      }
      xMax*=1.1;
      xMin/=1.1;
      yMax*=2; //to have enough gap with logarithmic scale
      yMin/=2;
      for(int iCoinc=1;iCoinc<=8;iCoinc++){
        if(!fHNCoincidencesPerPressure[iCoinc-1]) continue;
        fHNCoincidencesPerPressure[iCoinc-1]->GetXaxis()->SetRangeUser(xMin,xMax);
        fHNCoincidencesPerPressure[iCoinc-1]->SetMinimum(yMin);
        fHNCoincidencesPerPressure[iCoinc-1]->SetMaximum(yMax);
      }
    }
    // PressurePerBurst
    if(fHPressurePerBurst){
      if(fHPressurePerBurst->GetN()>0) {
        fHPressurePerBurst->GetPoint(fHPressurePerBurst->GetN()-1,LastValueX,LastValueY);
        if(LastValueX==BurstID || LastValueX<0 || LastValueY==0.) fHPressurePerBurst->RemovePoint(fHPressurePerBurst->GetN()-1); //remove last point
      }
      fHPressurePerBurst->Set(fHPressurePerBurst->GetN()+1);
      fHPressurePerBurst->SetPoint(fHPressurePerBurst->GetN()-1,BurstID,CedarReco->GetDIMInfo().GetPressure());
      //fHPressurePerBurst->SetMinimum(0.8);
      //fHPressurePerBurst->SetMaximum(2.5);
    }
  }

  //---------------------------- cedar alignment ----------------------------//
  CedarReco->ComputeAlignment();

  CedarReco->PrintAlignment( fCedarAlignmentText );
  CedarReco->PrintAsym( fCedarAsymText );

  //--- update all alignment pads
  if(fCedarAlignmentCanvas){
    for ( UInt_t iFrame = 0 ; iFrame != fCedarAlignmentCanvas->GetNFrames(); ++iFrame ){
      fCedarAlignmentCanvas->GetFrame(iFrame)->GetCanvas()->Modified();
    }
    fCedarAlignmentCanvas->GetCanvas()->Modified();
  }
  if(fCedarAlignmentTrendsCanvas){
    for ( UInt_t iFrame = 0 ; iFrame != fCedarAlignmentTrendsCanvas->GetNFrames(); ++iFrame ){
      fCedarAlignmentTrendsCanvas->GetFrame(iFrame)->GetCanvas()->Modified();
    }
    fCedarAlignmentTrendsCanvas->GetCanvas()->Modified();
  }

#ifdef DIM
  fAlignSugX = - CedarReco->GetAlignmentBestX();
  fAlignSugXService->updateService();
  fAlignSugY = - CedarReco->GetAlignmentBestY();
  fAlignSugYService->updateService();
#endif
  //-------------------------------------------------------------------------//

  //--- update range of stability plots
  const Int_t NStoredBursts = 190;
  Int_t MinBurstID = 0;
  if (BurstID >= MinBurstID+NStoredBursts) MinBurstID = BurstID - NStoredBursts;
  Int_t MaxBurstID = MinBurstID + NStoredBursts + 10;

  if(fHNCoincidencesPerBurst) {
    for(int iCoinc=1;iCoinc<=8;iCoinc++){
      if(!fHNCoincidencesPerBurst[iCoinc-1]) continue;
      fHNCoincidencesPerBurst[iCoinc-1]->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
    }
  }

  NA62VOnlineMonitor::Update(BurstID);
}
