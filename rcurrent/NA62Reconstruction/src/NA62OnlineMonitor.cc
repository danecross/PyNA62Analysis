// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-06-30
//
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TLegendEntry.h"
#include "TStyle.h"

#include "NA62OnlineMonitor.hh"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"

NA62OnlineMonitor::NA62OnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco,"NA62"), fCurrentBurstInfo(nullptr), fChokeONTimeInfo(nullptr), fHNProcessedEventsInFile(nullptr), fHT10IntensityPerBurst(nullptr), fHNCountsArgonionPerBurst(nullptr), fHL1TPRejectionPerBurst(nullptr), fHL1TPInputControlPerBurst(nullptr), fHL1TPInputPeriodicsPerBurst(nullptr), fHL1TPOutputControlPerBurst(nullptr), fHL1TPOutputPeriodicsPerBurst(nullptr), fHL1TPRejectionPerMaskPerBurst(nullptr), fHL1TPInputPhysicsPerMaskPerBurst(nullptr), fHL1TPOutputPhysicsPerMaskPerBurst(nullptr), fOMROBoardLinesRaw(nullptr), fOMROBoardLinesRawFine(nullptr), fOMROBoardLinesErrors(nullptr){

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::CompleteTab();
} 

void NA62OnlineMonitor::CreateShifterModeTabs(){

  NA62VOnlineMonitorCanvas *EventInfo = AddCanvasTab("EventInfo");
  EventInfo->Divide(2,2);
  EventInfo->cd(1);
  fHNProcessedEventsInFile = new TGraph();
  fHNProcessedEventsInFile->Set(1);
  fHNProcessedEventsInFile->SetPoint(0,-1,0);
  fHNProcessedEventsInFile->SetTitle("Processed events Vs Burst;BurstID;Processed events");
  fHNProcessedEventsInFile->SetMarkerStyle(20);
  fHNProcessedEventsInFile->SetMarkerSize(0.7);
  fHNProcessedEventsInFile->SetMarkerColor(1);
  fHNProcessedEventsInFile->SetLineColor(1);
  fHNProcessedEventsInFile->SetLineWidth(3);
  EventInfo->GetCurrentFrame()->DrawHisto(fHNProcessedEventsInFile,"ALP");
  // add current Run and Burst information
  fCurrentBurstInfo = new TPaveText( 0.15, 0.78, 0.35, 0.88,"NB,NDC");
  fCurrentBurstInfo->SetFillColor(kWhite);
  fCurrentBurstInfo->AddText(Form("Current Run:   %5d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID()));
  fCurrentBurstInfo->AddText(Form("Current Burst: %5d",NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()));
  fCurrentBurstInfo->Draw();
  EventInfo->cd(2);
  gPad->SetBottomMargin(0.12);
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStamp());
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHSkippedEventTimeStamp(),"same");
  // add ChokeON time information
  fChokeONTimeInfo = new TPaveText( 0.55, 0.68, 0.88, 0.88,"NB,NDC");
  fChokeONTimeInfo->SetFillColor(kWhite);
  fChokeONTimeInfo->AddText("Choke ON Time: -");
  fChokeONTimeInfo->Draw();
  EventInfo->cd(3);
  fHNCountsArgonionPerBurst = new TGraph();
  fHNCountsArgonionPerBurst->Set(1);
  fHNCountsArgonionPerBurst->SetPoint(0,-1,0);
  fHNCountsArgonionPerBurst->SetTitle("Argonion counts Vs Burst;BurstID;Argonion counts");
  fHNCountsArgonionPerBurst->SetMarkerStyle(20);
  fHNCountsArgonionPerBurst->SetMarkerSize(0.7);
  fHNCountsArgonionPerBurst->SetMarkerColor(1);
  fHNCountsArgonionPerBurst->SetLineColor(1);
  fHNCountsArgonionPerBurst->SetLineWidth(3);
  EventInfo->GetCurrentFrame()->DrawHisto(fHNCountsArgonionPerBurst,"ALP");
  EventInfo->cd(4);
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHNEventsWithQualityWarningsPerDetector(),"HISTO");
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHNCriticalEventsPerDetector(),"HISTOsame");

  NA62VOnlineMonitorCanvas *L0L1TPInfo = AddCanvasTab("TriggerInfo");
  L0L1TPInfo->Divide(2,2);
  L0L1TPInfo->cd(1);
  L0L1TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags(),"COLZ");
  L0L1TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags(),"TEXTsame");
  static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags()->GetXaxis()->SetRangeUser(0,8);
  static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags()->GetYaxis()->SetRangeUser(0,18);
  static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags()->SetMarkerSize(2); //change the text size
  L0L1TPInfo->cd(2);
  static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags()->SetStats(0);
  L0L1TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags(),"COLZ");
  L0L1TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags(),"TEXTsame");
  static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags()->GetXaxis()->SetRangeUser(0,8);
  static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags()->GetYaxis()->SetRangeUser(0,8);
  static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags()->SetMarkerSize(2); //change the text size

  TLegend *legL1TP = new TLegend(0.9,0.50,0.999,0.92);
  legL1TP->SetBorderSize(0);
  legL1TP->SetLineColor(0);
  legL1TP->SetLineStyle(1);
  legL1TP->SetLineWidth(1);
  legL1TP->SetFillColor(0);
  legL1TP->SetFillStyle(1001);

  L0L1TPInfo->cd(3);

  TLegend *legL1In = new TLegend(0.9,0.50,0.999,0.92);
  legL1In->SetBorderSize(0);
  legL1In->SetLineColor(0);
  legL1In->SetLineStyle(1);
  legL1In->SetLineWidth(1);
  legL1In->SetFillColor(0);
  legL1In->SetFillStyle(1001);

  fHL1TPInputControlPerBurst = new TGraph();
  fHL1TPInputControlPerBurst->Set(1);
  fHL1TPInputControlPerBurst->SetPoint(0,-1,0);
  fHL1TPInputControlPerBurst->SetTitle("L1 Input Occupancy Vs Burst;BurstID; Fraction Wrt Total");
  fHL1TPInputControlPerBurst->SetMarkerStyle(25);
  fHL1TPInputControlPerBurst->SetMarkerSize(1);
  fHL1TPInputControlPerBurst->SetMarkerColor(1);
  fHL1TPInputControlPerBurst->SetLineColor(1);
  fHL1TPInputControlPerBurst->SetLineWidth(3);
  fHL1TPInputControlPerBurst->SetMinimum(0);
  fHL1TPInputControlPerBurst->SetMaximum(1);
  TLegendEntry *entry2=legL1In->AddEntry("fHL1TPInputControlPerBurst","Control","lep");
  entry2->SetLineColor(1);
  entry2->SetLineStyle(1);
  entry2->SetLineWidth(3);
  entry2->SetMarkerColor(1);
  entry2->SetMarkerStyle(25);
  entry2->SetMarkerSize(1);
  L0L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPInputControlPerBurst,"ALP");

  fHL1TPInputPeriodicsPerBurst = new TGraph();
  fHL1TPInputPeriodicsPerBurst->Set(1);
  fHL1TPInputPeriodicsPerBurst->SetPoint(0,-1,0);
  fHL1TPInputPeriodicsPerBurst->SetMarkerStyle(24);
  fHL1TPInputPeriodicsPerBurst->SetMarkerSize(1);
  fHL1TPInputPeriodicsPerBurst->SetMarkerColor(1);
  fHL1TPInputPeriodicsPerBurst->SetLineColor(1);
  fHL1TPInputPeriodicsPerBurst->SetLineWidth(3);
  TLegendEntry *entry3=legL1In->AddEntry("fHL1TPInputPeriodicsPerBurst","Periodics","lep");
  entry3->SetLineColor(1);
  entry3->SetLineStyle(1);
  entry3->SetLineWidth(3);
  entry3->SetMarkerColor(1);
  entry3->SetMarkerStyle(24);
  entry3->SetMarkerSize(1);
  L0L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPInputPeriodicsPerBurst,"PLsame");

  fHL1TPInputPhysicsPerMaskPerBurst = new TGraph*[NL1Masks];
  for(UInt_t iMask=0; iMask<NL1Masks; iMask++){
    UInt_t iColor = 0;
    if(iMask<8) iColor = 2+iMask;
    else iColor = 4+iMask;
    fHL1TPInputPhysicsPerMaskPerBurst[iMask] = new TGraph();
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->Set(1);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetPoint(0,-1,0);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetMarkerStyle(20);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetMarkerSize(1);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetMarkerColor(iColor);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetLineColor(iColor);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetLineWidth(3);
    TLegendEntry *entry4;
    if(iMask!=1) entry4=legL1In->AddEntry("fHL1TPInputPhysicsPerMaskPerBurst[iMask]",Form("L0 Mask %i",iMask),"lep");
    else entry4=legL1In->AddEntry("fHL1TPInputPhysicsPerMaskPerBurst[iMask]","L0 PNN","lep");
    entry4->SetLineColor(iColor);
    entry4->SetLineStyle(1);
    entry4->SetLineWidth(3);
    entry4->SetMarkerColor(iColor);
    entry4->SetMarkerStyle(20);
    entry4->SetMarkerSize(1);
    L0L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPInputPhysicsPerMaskPerBurst[iMask],"PLsame");
  }
  legL1In->Draw();

  L0L1TPInfo->cd(4);

  TLegend *legL1Out = new TLegend(0.9,0.50,0.999,0.92);
  legL1Out->SetBorderSize(0);
  legL1Out->SetLineColor(0);
  legL1Out->SetLineStyle(1);
  legL1Out->SetLineWidth(1);
  legL1Out->SetFillColor(0);
  legL1Out->SetFillStyle(1001);

  fHL1TPOutputControlPerBurst = new TGraph();
  fHL1TPOutputControlPerBurst->Set(1);
  fHL1TPOutputControlPerBurst->SetPoint(0,-1,0);
  fHL1TPOutputControlPerBurst->SetTitle("L1 Output Occupancy Vs Burst;BurstID; Fraction Wrt Total");
  fHL1TPOutputControlPerBurst->SetMarkerStyle(25);
  fHL1TPOutputControlPerBurst->SetMarkerSize(1);
  fHL1TPOutputControlPerBurst->SetMarkerColor(1);
  fHL1TPOutputControlPerBurst->SetLineColor(1);
  fHL1TPOutputControlPerBurst->SetLineWidth(3);
  fHL1TPOutputControlPerBurst->SetMinimum(0);
  fHL1TPOutputControlPerBurst->SetMaximum(1);
  TLegendEntry *entry5=legL1Out->AddEntry("fHL1TPOutputControlPerBurst","Control","lep");
  entry5->SetLineColor(1);
  entry5->SetLineStyle(1);
  entry5->SetLineWidth(3);
  entry5->SetMarkerColor(1);
  entry5->SetMarkerStyle(25);
  entry5->SetMarkerSize(1);
  L0L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPOutputControlPerBurst,"ALP");

  fHL1TPOutputPeriodicsPerBurst = new TGraph();
  fHL1TPOutputPeriodicsPerBurst->Set(1);
  fHL1TPOutputPeriodicsPerBurst->SetPoint(0,-1,0);
  fHL1TPOutputPeriodicsPerBurst->SetMarkerStyle(24);
  fHL1TPOutputPeriodicsPerBurst->SetMarkerSize(1);
  fHL1TPOutputPeriodicsPerBurst->SetMarkerColor(1);
  fHL1TPOutputPeriodicsPerBurst->SetLineColor(1);
  fHL1TPOutputPeriodicsPerBurst->SetLineWidth(3);
  TLegendEntry *entry6=legL1Out->AddEntry("fHL1TPOutputPeriodicsPerBurst","Periodics","lep");
  entry6->SetLineColor(1);
  entry6->SetLineStyle(1);
  entry6->SetLineWidth(3);
  entry6->SetMarkerColor(1);
  entry6->SetMarkerStyle(24);
  entry6->SetMarkerSize(1);
  L0L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPOutputPeriodicsPerBurst,"PLsame");

  fHL1TPOutputPhysicsPerMaskPerBurst = new TGraph*[NL1Masks];
  for(UInt_t iMask=0; iMask<NL1Masks; iMask++){
    UInt_t iColor = 0;
    if(iMask<8) iColor = 2+iMask;
    else iColor = 4+iMask;
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask] = new TGraph();
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->Set(1);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetPoint(0,-1,0);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetMarkerStyle(20);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetMarkerSize(1);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetMarkerColor(iColor);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetLineColor(iColor);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetLineWidth(3);
    TLegendEntry *entry7;
    if(iMask!=1) entry7=legL1Out->AddEntry("fHL1TPOutputPhysicsPerMaskPerBurst[iMask]",Form("L0 Mask %i",iMask),"lep");
    else entry7=legL1Out->AddEntry("fHL1TPOutputPhysicsPerMaskPerBurst[iMask]","L0 PNN","lep");
    entry7->SetLineColor(iColor);
    entry7->SetLineStyle(1);
    entry7->SetLineWidth(3);
    entry7->SetMarkerColor(iColor);
    entry7->SetMarkerStyle(20);
    entry7->SetMarkerSize(1);
    L0L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPOutputPhysicsPerMaskPerBurst[iMask],"PLsame");
  }
  legL1Out->Draw();

  Int_t NReconstructions = static_cast<NA62Reconstruction*>(fReco)->GetNReconstructions();

  //find right NxReconstructions and NyReconstructions to accomodate all the plots
  Int_t NxReconstructions = 0;
  Int_t NyReconstructions = 0;
  while(NxReconstructions*NyReconstructions<NReconstructions){
    if(3*NyReconstructions<2*NxReconstructions) NyReconstructions++;
    if(NxReconstructions*NyReconstructions<NReconstructions) NxReconstructions++;
  }

  NA62VOnlineMonitorCanvas *DigiTimeRawFine = AddCanvasTab("DigiTimeRawFine");
  DigiTimeRawFine->Divide(NxReconstructions,NyReconstructions);
  fOMROBoardLinesRawFine = new TLine**[NReconstructions];
  for (Int_t iReco=0; iReco<NReconstructions; iReco++) {
    DigiTimeRawFine->cd(iReco+1) ;
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()) {
      TH2F * hDigiTimeRawFine = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine();
      hDigiTimeRawFine->SetStats(0);
      DigiTimeRawFine->GetCurrentFrame()->DrawHisto(hDigiTimeRawFine,"COLZ");
      UInt_t nROBoards = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
      UInt_t nROMezzaninesPerFullBoard = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard();
      if(nROBoards>1){
        fOMROBoardLinesRawFine[iReco] = new TLine*[nROBoards-1];
        for(UInt_t iROBoard=0;iROBoard<nROBoards-1;iROBoard++){
          fOMROBoardLinesRawFine[iReco][iROBoard] = new TLine((iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDigiTimeRawFine->GetYaxis()->GetXmin(),
              (iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDigiTimeRawFine->GetYaxis()->GetXmax());
          fOMROBoardLinesRawFine[iReco][iROBoard]->SetLineStyle(2);
          fOMROBoardLinesRawFine[iReco][iROBoard]->Draw();
        }
      }
      gPad->SetGrid(kFALSE,kTRUE);
      gPad->SetLogz(1);
    }
  }

  NA62VOnlineMonitorCanvas *DecoderErrors = AddCanvasTab("DecoderErrors");
  DecoderErrors->Divide(NxReconstructions,NyReconstructions);
  fOMROBoardLinesErrors = new TLine**[NReconstructions];
  for(Int_t iReco=0;iReco<NReconstructions;iReco++){
    DecoderErrors->cd(iReco+1) ;
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()) {
      TH2F * hDecoderErrors = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors();
      hDecoderErrors->SetStats(0);
      DecoderErrors->GetCurrentFrame()->DrawHisto(hDecoderErrors,"COLZ");
      UInt_t nROBoards = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
      UInt_t nROMezzaninesPerFullBoard = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard();
      if(nROBoards>1){
        fOMROBoardLinesErrors[iReco] = new TLine*[nROBoards-1];
        for(UInt_t iROBoard=0;iROBoard<nROBoards-1;iROBoard++){
          fOMROBoardLinesErrors[iReco][iROBoard] = new TLine((iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDecoderErrors->GetYaxis()->GetXmin(),
              (iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDecoderErrors->GetYaxis()->GetXmax());
          fOMROBoardLinesErrors[iReco][iROBoard]->SetLineStyle(2);
          fOMROBoardLinesErrors[iReco][iROBoard]->Draw();
        }
      }
      gPad->SetGrid(kFALSE,kTRUE);
      gPad->SetLeftMargin(0.12);
      gPad->SetLogz(1);
    }
  }
} 

void NA62OnlineMonitor::CreateExpertModeTabs(){

  NA62VOnlineMonitorCanvas *EventInfo = AddCanvasTab("EventInfo");
  EventInfo->Divide(2,2);
  EventInfo->cd(1);
  fHNProcessedEventsInFile = new TGraph();
  fHNProcessedEventsInFile->Set(1);
  fHNProcessedEventsInFile->SetPoint(0,-1,0);
  fHNProcessedEventsInFile->SetTitle("Processed events Vs Burst;BurstID;Processed events");
  fHNProcessedEventsInFile->SetMarkerStyle(20);
  fHNProcessedEventsInFile->SetMarkerSize(0.7);
  fHNProcessedEventsInFile->SetMarkerColor(1);
  fHNProcessedEventsInFile->SetLineColor(1);
  fHNProcessedEventsInFile->SetLineWidth(3);
  EventInfo->GetCurrentFrame()->DrawHisto(fHNProcessedEventsInFile,"ALP");
  // add current Run and Burst information
  fCurrentBurstInfo = new TPaveText( 0.15, 0.78, 0.35, 0.88,"NB,NDC");
  fCurrentBurstInfo->SetFillColor(kWhite);
  fCurrentBurstInfo->AddText(Form("Current Run:   %5d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID()));
  fCurrentBurstInfo->AddText(Form("Current Burst: %5d",NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()));
  fCurrentBurstInfo->Draw();
  EventInfo->cd(3);
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetGVirtMem());
  EventInfo->cd(2);
  gPad->SetBottomMargin(0.12);
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStamp());
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHSkippedEventTimeStamp(),"same");
  // add ChokeON time information
  fChokeONTimeInfo = new TPaveText( 0.55, 0.68, 0.88, 0.88,"NB,NDC");
  fChokeONTimeInfo->SetFillColor(kWhite);
  fChokeONTimeInfo->AddText("Choke ON Time: -");
  fChokeONTimeInfo->Draw();
  EventInfo->cd(4);
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHNEventsWithQualityWarningsPerDetector(),"HISTO");
  EventInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHNCriticalEventsPerDetector(),"HISTOsame");

  NA62VOnlineMonitorCanvas *L0TPInfo = AddCanvasTab("L0TPInfo");
  L0TPInfo->Divide(2,2);
  L0TPInfo->cd(1);
  L0TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHEventSize());
  static_cast<NA62Reconstruction*>(fReco)->GetHEventSize()->GetXaxis()->SetRangeUser(0,40.); //KB
  L0TPInfo->cd(2);
  L0TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHDeltaTimeStamp());
  L0TPInfo->cd(3);
  L0TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags(),"COLZ");
  L0TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags(),"TEXTsame");
  static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags()->GetXaxis()->SetRangeUser(0,8);
  static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags()->GetYaxis()->SetRangeUser(0,18);
  static_cast<NA62Reconstruction*>(fReco)->GetHL0TriggerFlags()->SetMarkerSize(2); //change the text size
  L0TPInfo->cd(4);
  L0TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHDeltaTimeStampStored());

  NA62VOnlineMonitorCanvas *L1TPInfo = AddCanvasTab("L1TPInfo");
  L1TPInfo->Divide(2,2);
  L1TPInfo->cd(1);

  TLegend *legL1TP = new TLegend(0.9,0.50,0.999,0.92);
  legL1TP->SetBorderSize(0);
  legL1TP->SetLineColor(0);
  legL1TP->SetLineStyle(1);
  legL1TP->SetLineWidth(1);
  legL1TP->SetFillColor(0);
  legL1TP->SetFillStyle(1001);

  fHL1TPRejectionPerBurst = new TGraph();
  fHL1TPRejectionPerBurst->Set(1);
  fHL1TPRejectionPerBurst->SetPoint(0,-1,0);
  fHL1TPRejectionPerBurst->SetTitle("L1 Passed/Total Vs Burst;BurstID; Passed/Total");
  fHL1TPRejectionPerBurst->SetMarkerStyle(25);
  fHL1TPRejectionPerBurst->SetMarkerSize(1);
  fHL1TPRejectionPerBurst->SetMarkerColor(1);
  fHL1TPRejectionPerBurst->SetLineColor(1);
  fHL1TPRejectionPerBurst->SetLineWidth(3);
  fHL1TPRejectionPerBurst->SetMinimum(0);
  fHL1TPRejectionPerBurst->SetMaximum(1);
  TLegendEntry *entry=legL1TP->AddEntry("fHL1TPRejectionPerBurst","All Data","lep");
  entry->SetLineColor(1);
  entry->SetLineStyle(1);
  entry->SetLineWidth(3);
  entry->SetMarkerColor(1);
  entry->SetMarkerStyle(25);
  entry->SetMarkerSize(1);
  L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPRejectionPerBurst,"ALP");

  fHL1TPRejectionPerMaskPerBurst = new TGraph*[NL1Masks];
  for(UInt_t iMask=0; iMask<NL1Masks; iMask++){
    UInt_t iColor = 0;
    if(iMask<8) iColor = 2+iMask;
    else iColor = 4+iMask;
    fHL1TPRejectionPerMaskPerBurst[iMask] = new TGraph();
    fHL1TPRejectionPerMaskPerBurst[iMask]->Set(1);
    fHL1TPRejectionPerMaskPerBurst[iMask]->SetPoint(0,-1,0);
    fHL1TPRejectionPerMaskPerBurst[iMask]->SetMarkerStyle(20);
    fHL1TPRejectionPerMaskPerBurst[iMask]->SetMarkerSize(1);
    fHL1TPRejectionPerMaskPerBurst[iMask]->SetMarkerColor(iColor);
    fHL1TPRejectionPerMaskPerBurst[iMask]->SetLineColor(iColor);
    fHL1TPRejectionPerMaskPerBurst[iMask]->SetLineWidth(3);
    TLegendEntry *entry_mask;
    if(iMask!=1) entry_mask=legL1TP->AddEntry("fHL1TPRejectionPerBurst[iMask]",Form("L0 Mask %i",iMask),"lep");
    else entry_mask=legL1TP->AddEntry("fHL1TPRejectionPerBurst[iMask]","L0 PNN","lep");
    entry_mask->SetLineColor(iColor);
    entry_mask->SetLineStyle(1);
    entry_mask->SetLineWidth(3);
    entry_mask->SetMarkerColor(iColor);
    entry_mask->SetMarkerStyle(20);
    entry_mask->SetMarkerSize(1);
    L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPRejectionPerMaskPerBurst[iMask],"PLsame");
  }
  legL1TP->Draw();

  L1TPInfo->cd(2);
  static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags()->SetStats(0);
  L1TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags(),"COLZ");
  L1TPInfo->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags(),"TEXTsame");
  static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags()->GetXaxis()->SetRangeUser(0,8);
  static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags()->GetYaxis()->SetRangeUser(0,8);
  static_cast<NA62Reconstruction*>(fReco)->GetHL1TriggerFlags()->SetMarkerSize(2); //change the text size

  L1TPInfo->cd(3);

  TLegend *legL1In = new TLegend(0.9,0.50,0.999,0.92);
  legL1In->SetBorderSize(0);
  legL1In->SetLineColor(0);
  legL1In->SetLineStyle(1);
  legL1In->SetLineWidth(1);
  legL1In->SetFillColor(0);
  legL1In->SetFillStyle(1001);

  fHL1TPInputControlPerBurst = new TGraph();
  fHL1TPInputControlPerBurst->Set(1);
  fHL1TPInputControlPerBurst->SetPoint(0,-1,0);
  fHL1TPInputControlPerBurst->SetTitle("L1 Input Occupancy Vs Burst;BurstID; Fraction Wrt Total");
  fHL1TPInputControlPerBurst->SetMarkerStyle(25);
  fHL1TPInputControlPerBurst->SetMarkerSize(1);
  fHL1TPInputControlPerBurst->SetMarkerColor(1);
  fHL1TPInputControlPerBurst->SetLineColor(1);
  fHL1TPInputControlPerBurst->SetLineWidth(3);
  fHL1TPInputControlPerBurst->SetMinimum(0);
  fHL1TPInputControlPerBurst->SetMaximum(1);
  TLegendEntry *entry2=legL1In->AddEntry("fHL1TPInputControlPerBurst","Control","lep");
  entry2->SetLineColor(1);
  entry2->SetLineStyle(1);
  entry2->SetLineWidth(3);
  entry2->SetMarkerColor(1);
  entry2->SetMarkerStyle(25);
  entry2->SetMarkerSize(1);
  L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPInputControlPerBurst,"ALP");

  fHL1TPInputPeriodicsPerBurst = new TGraph();
  fHL1TPInputPeriodicsPerBurst->Set(1);
  fHL1TPInputPeriodicsPerBurst->SetPoint(0,-1,0);
  fHL1TPInputPeriodicsPerBurst->SetMarkerStyle(24);
  fHL1TPInputPeriodicsPerBurst->SetMarkerSize(1);
  fHL1TPInputPeriodicsPerBurst->SetMarkerColor(1);
  fHL1TPInputPeriodicsPerBurst->SetLineColor(1);
  fHL1TPInputPeriodicsPerBurst->SetLineWidth(3);
  TLegendEntry *entry3=legL1In->AddEntry("fHL1TPInputPeriodicsPerBurst","Periodics","lep");
  entry3->SetLineColor(1);
  entry3->SetLineStyle(1);
  entry3->SetLineWidth(3);
  entry3->SetMarkerColor(1);
  entry3->SetMarkerStyle(24);
  entry3->SetMarkerSize(1);
  L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPInputPeriodicsPerBurst,"PLsame");

  fHL1TPInputPhysicsPerMaskPerBurst = new TGraph*[NL1Masks];
  for(UInt_t iMask=0; iMask<NL1Masks; iMask++){
    UInt_t iColor = 0;
    if(iMask<8) iColor = 2+iMask;
    else iColor = 4+iMask;
    fHL1TPInputPhysicsPerMaskPerBurst[iMask] = new TGraph();
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->Set(1);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetPoint(0,-1,0);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetMarkerStyle(20);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetMarkerSize(1);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetMarkerColor(iColor);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetLineColor(iColor);
    fHL1TPInputPhysicsPerMaskPerBurst[iMask]->SetLineWidth(3);
    TLegendEntry *entry4;
    if(iMask!=1) entry4=legL1In->AddEntry("fHL1TPInputPhysicsPerMaskPerBurst[iMask]",Form("L0 Mask %i",iMask),"lep");
    else entry4=legL1In->AddEntry("fHL1TPInputPhysicsPerMaskPerBurst[iMask]","L0 PNN","lep");
    entry4->SetLineColor(iColor);
    entry4->SetLineStyle(1);
    entry4->SetLineWidth(3);
    entry4->SetMarkerColor(iColor);
    entry4->SetMarkerStyle(20);
    entry4->SetMarkerSize(1);
    L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPInputPhysicsPerMaskPerBurst[iMask],"PLsame");
  }
  legL1In->Draw();

  L1TPInfo->cd(4);

  TLegend *legL1Out = new TLegend(0.9,0.50,0.999,0.92);
  legL1Out->SetBorderSize(0);
  legL1Out->SetLineColor(0);
  legL1Out->SetLineStyle(1);
  legL1Out->SetLineWidth(1);
  legL1Out->SetFillColor(0);
  legL1Out->SetFillStyle(1001);

  fHL1TPOutputControlPerBurst = new TGraph();
  fHL1TPOutputControlPerBurst->Set(1);
  fHL1TPOutputControlPerBurst->SetPoint(0,-1,0);
  fHL1TPOutputControlPerBurst->SetTitle("L1 Output Occupancy Vs Burst;BurstID; Fraction Wrt Total");
  fHL1TPOutputControlPerBurst->SetMarkerStyle(25);
  fHL1TPOutputControlPerBurst->SetMarkerSize(1);
  fHL1TPOutputControlPerBurst->SetMarkerColor(1);
  fHL1TPOutputControlPerBurst->SetLineColor(1);
  fHL1TPOutputControlPerBurst->SetLineWidth(3);
  fHL1TPOutputControlPerBurst->SetMinimum(0);
  fHL1TPOutputControlPerBurst->SetMaximum(1);
  TLegendEntry *entry5=legL1Out->AddEntry("fHL1TPOutputControlPerBurst","Control","lep");
  entry5->SetLineColor(1);
  entry5->SetLineStyle(1);
  entry5->SetLineWidth(3);
  entry5->SetMarkerColor(1);
  entry5->SetMarkerStyle(25);
  entry5->SetMarkerSize(1);
  L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPOutputControlPerBurst,"ALP");

  fHL1TPOutputPeriodicsPerBurst = new TGraph();
  fHL1TPOutputPeriodicsPerBurst->Set(1);
  fHL1TPOutputPeriodicsPerBurst->SetPoint(0,-1,0);
  fHL1TPOutputPeriodicsPerBurst->SetMarkerStyle(24);
  fHL1TPOutputPeriodicsPerBurst->SetMarkerSize(1);
  fHL1TPOutputPeriodicsPerBurst->SetMarkerColor(1);
  fHL1TPOutputPeriodicsPerBurst->SetLineColor(1);
  fHL1TPOutputPeriodicsPerBurst->SetLineWidth(3);
  TLegendEntry *entry6=legL1Out->AddEntry("fHL1TPOutputPeriodicsPerBurst","Periodics","lep");
  entry6->SetLineColor(1);
  entry6->SetLineStyle(1);
  entry6->SetLineWidth(3);
  entry6->SetMarkerColor(1);
  entry6->SetMarkerStyle(24);
  entry6->SetMarkerSize(1);
  L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPOutputPeriodicsPerBurst,"PLsame");

  fHL1TPOutputPhysicsPerMaskPerBurst = new TGraph*[NL1Masks];
  for(UInt_t iMask=0; iMask<NL1Masks; iMask++){
    UInt_t iColor = 0;
    if(iMask<8) iColor = 2+iMask;
    else iColor = 4+iMask;
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask] = new TGraph();
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->Set(1);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetPoint(0,-1,0);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetMarkerStyle(20);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetMarkerSize(1);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetMarkerColor(iColor);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetLineColor(iColor);
    fHL1TPOutputPhysicsPerMaskPerBurst[iMask]->SetLineWidth(3);
    TLegendEntry *entry7;
    if(iMask!=1) entry7=legL1Out->AddEntry("fHL1TPOutputPhysicsPerMaskPerBurst[iMask]",Form("L0 Mask %i",iMask),"lep");
    else entry7=legL1Out->AddEntry("fHL1TPOutputPhysicsPerMaskPerBurst[iMask]","L0 PNN","lep");
    entry7->SetLineColor(iColor);
    entry7->SetLineStyle(1);
    entry7->SetLineWidth(3);
    entry7->SetMarkerColor(iColor);
    entry7->SetMarkerStyle(20);
    entry7->SetMarkerSize(1);
    L1TPInfo->GetCurrentFrame()->DrawHisto(fHL1TPOutputPhysicsPerMaskPerBurst[iMask],"PLsame");
  }
  legL1Out->Draw();

  //NA62VOnlineMonitorCanvas *ReconstructionMoni = AddCanvasTab("RecoTiming");
  //ReconstructionMoni->Divide(1,2);
  //ReconstructionMoni->cd(1)->SetBottomMargin(0.25);
  //ReconstructionMoni->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHTimingProfile());
  //ReconstructionMoni->cd(2)->SetBottomMargin(0.25);
  //ReconstructionMoni->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHTiming2D(),"COLZ");

  //NA62VOnlineMonitorCanvas *TimeStampMon = AddCanvasTab("TimeStampMon");
  //TimeStampMon->Divide(2,2);
  //TimeStampMon->cd(1);
  //(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStampBits()->SetStats(0));
  //TimeStampMon->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStampBits());
  //TimeStampMon->cd(2);
  //(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStamp16()->SetStats(0));
  //TimeStampMon->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStamp16());
  //TimeStampMon->cd(3);
  //(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStamp128()->SetStats(0));
  //TimeStampMon->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStamp128());
  //TimeStampMon->cd(4);
  //(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStamp1024()->SetStats(0));
  //TimeStampMon->GetCurrentFrame()->DrawHisto(static_cast<NA62Reconstruction*>(fReco)->GetHEventTimeStamp1024());

  NA62VOnlineMonitorCanvas *BeamInfo = AddCanvasTab("BeamInfo");
  BeamInfo->Divide(2,1);

  BeamInfo->cd(1);
  fHT10IntensityPerBurst = new TGraph();
  fHT10IntensityPerBurst->Set(1);
  fHT10IntensityPerBurst->SetPoint(0,-1,0);
  fHT10IntensityPerBurst->SetTitle("T10 intensity Vs Burst;BurstID;T10 intensity [#times 10^{11}]");
  fHT10IntensityPerBurst->SetMarkerStyle(20);
  fHT10IntensityPerBurst->SetMarkerSize(0.7);
  fHT10IntensityPerBurst->SetMarkerColor(1);
  fHT10IntensityPerBurst->SetLineColor(1);
  fHT10IntensityPerBurst->SetLineWidth(3);
  BeamInfo->GetCurrentFrame()->DrawHisto(fHT10IntensityPerBurst,"ALP");
  BeamInfo->cd(2);
  fHNCountsArgonionPerBurst = new TGraph();
  fHNCountsArgonionPerBurst->Set(1);
  fHNCountsArgonionPerBurst->SetPoint(0,-1,0);
  fHNCountsArgonionPerBurst->SetTitle("Argonion counts Vs Burst;BurstID;Argonion counts");
  fHNCountsArgonionPerBurst->SetMarkerStyle(20);
  fHNCountsArgonionPerBurst->SetMarkerSize(0.7);
  fHNCountsArgonionPerBurst->SetMarkerColor(1);
  fHNCountsArgonionPerBurst->SetLineColor(1);
  fHNCountsArgonionPerBurst->SetLineWidth(3);
  BeamInfo->GetCurrentFrame()->DrawHisto(fHNCountsArgonionPerBurst,"ALP");

  Int_t NReconstructions = static_cast<NA62Reconstruction*>(fReco)->GetNReconstructions();

  //find right NxReconstructions and NyReconstructions to accomodate all the plots
  Int_t NxReconstructions = 0;
  Int_t NyReconstructions = 0;
  while(NxReconstructions*NyReconstructions<NReconstructions){
    if(3*NyReconstructions<2*NxReconstructions) NyReconstructions++;
    if(NxReconstructions*NyReconstructions<NReconstructions) NxReconstructions++;
  }

  NA62VOnlineMonitorCanvas *DigiTimeRaw = AddCanvasTab("DigiTimeRaw");
  DigiTimeRaw->Divide(NxReconstructions,NyReconstructions);
  fOMROBoardLinesRaw = new TLine**[NReconstructions];
  for(Int_t iReco=0;iReco<NReconstructions;iReco++){
    DigiTimeRaw->cd(iReco+1) ;
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw()) {
      TH2F * hDigiTimeRaw = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw();
      hDigiTimeRaw->SetStats(0);
      DigiTimeRaw->GetCurrentFrame()->DrawHisto(hDigiTimeRaw,"COLZ");
      UInt_t nROBoards = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
      UInt_t nROMezzaninesPerFullBoard = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard();
      if(nROBoards>1){
        fOMROBoardLinesRaw[iReco] = new TLine*[nROBoards-1];
        for(UInt_t iROBoard=0;iROBoard<nROBoards-1;iROBoard++){
          fOMROBoardLinesRaw[iReco][iROBoard] = new TLine((iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDigiTimeRaw->GetYaxis()->GetXmin(),
              (iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDigiTimeRaw->GetYaxis()->GetXmax());
          fOMROBoardLinesRaw[iReco][iROBoard]->SetLineStyle(2);
          fOMROBoardLinesRaw[iReco][iROBoard]->Draw();
        }
      }
      gPad->SetGrid(kFALSE,kTRUE);
      gPad->SetLogz(1);
    }
  }

  NA62VOnlineMonitorCanvas *DigiTimeRawFine = AddCanvasTab("DigiTimeRawFine");
  DigiTimeRawFine->Divide(NxReconstructions,NyReconstructions);
  fOMROBoardLinesRawFine = new TLine**[NReconstructions];
  for (Int_t iReco=0; iReco<NReconstructions; iReco++) {
    DigiTimeRawFine->cd(iReco+1) ;
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()) {
      TH2F * hDigiTimeRawFine = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine();
      hDigiTimeRawFine->SetStats(0);
      DigiTimeRawFine->GetCurrentFrame()->DrawHisto(hDigiTimeRawFine,"COLZ");
      UInt_t nROBoards = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
      UInt_t nROMezzaninesPerFullBoard = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard();
      if(nROBoards>1){
        fOMROBoardLinesRawFine[iReco] = new TLine*[nROBoards-1];
        for(UInt_t iROBoard=0;iROBoard<nROBoards-1;iROBoard++){
          fOMROBoardLinesRawFine[iReco][iROBoard] = new TLine((iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDigiTimeRawFine->GetYaxis()->GetXmin(),
              (iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDigiTimeRawFine->GetYaxis()->GetXmax());
          fOMROBoardLinesRawFine[iReco][iROBoard]->SetLineStyle(2);
          fOMROBoardLinesRawFine[iReco][iROBoard]->Draw();
        }
      }
      gPad->SetGrid(kFALSE,kTRUE);
      gPad->SetLogz(1);
    }
  }

  NA62VOnlineMonitorCanvas *DecoderErrors = AddCanvasTab("DecoderErrors");
  DecoderErrors->Divide(NxReconstructions,NyReconstructions);
  fOMROBoardLinesErrors = new TLine**[NReconstructions];
  for(Int_t iReco=0;iReco<NReconstructions;iReco++){
    DecoderErrors->cd(iReco+1) ;
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()) {
      TH2F * hDecoderErrors = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors();
      hDecoderErrors->SetStats(0);
      DecoderErrors->GetCurrentFrame()->DrawHisto(hDecoderErrors,"COLZ");
      UInt_t nROBoards = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
      UInt_t nROMezzaninesPerFullBoard = SubDetReco->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard();
      if(nROBoards>1){
        fOMROBoardLinesErrors[iReco] = new TLine*[nROBoards-1];
        for(UInt_t iROBoard=0;iROBoard<nROBoards-1;iROBoard++){
          fOMROBoardLinesErrors[iReco][iROBoard] = new TLine((iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDecoderErrors->GetYaxis()->GetXmin(),
              (iROBoard+1)*nROMezzaninesPerFullBoard-0.5,hDecoderErrors->GetYaxis()->GetXmax());
          fOMROBoardLinesErrors[iReco][iROBoard]->SetLineStyle(2);
          fOMROBoardLinesErrors[iReco][iROBoard]->Draw();
        }
      }
      gPad->SetGrid(kFALSE,kTRUE);
      gPad->SetLeftMargin(0.12);
      gPad->SetLogz(1);
    }
  }
} 


NA62OnlineMonitor::~NA62OnlineMonitor() {}

void NA62OnlineMonitor::Update(Int_t BurstID){
  
  if(fCurrentBurstInfo){
    fCurrentBurstInfo->Clear();
    fCurrentBurstInfo->AddText(Form("Current Run:   %5d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID()));
    fCurrentBurstInfo->AddText(Form("Current Burst: %5d",NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()));
  }
  if(fChokeONTimeInfo){
    fChokeONTimeInfo->Clear();
    Double_t ChokeONTime = static_cast<NA62Reconstruction*>(fReco)->GetChokeONTimeInFile();
    TString ChokeONTimeString = Form("#color[%d]{%.1f ns}",kGreen+2,ChokeONTime); //green
    if(ChokeONTime > 1.e3) ChokeONTimeString = Form("#color[%d]{%.2f #mu s}",kGreen+2,ChokeONTime/1.e3); //green
    if(ChokeONTime > 1.e6) ChokeONTimeString = Form("#color[%d]{%.2f ms}",kOrange+2,ChokeONTime/1.e6); //>1ms: orange
    if(ChokeONTime > 1.e8) ChokeONTimeString = Form("#color[%d]{%.2f s}",kRed,ChokeONTime/1.e9);  //>100ms: red
    fChokeONTimeInfo->AddText(Form("Choke ON Time: %s",ChokeONTimeString.Data()));
  }

  Double_t LastValueX=-1., LastValueY=0.;

  // NProcessedEventsInFile
  if(fHNProcessedEventsInFile){
    if(fHNProcessedEventsInFile->GetN()>0) {
      fHNProcessedEventsInFile->GetPoint(fHNProcessedEventsInFile->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHNProcessedEventsInFile->RemovePoint(fHNProcessedEventsInFile->GetN()-1); //remove last point
    }
    fHNProcessedEventsInFile->Set(fHNProcessedEventsInFile->GetN()+1);
    fHNProcessedEventsInFile->SetPoint(fHNProcessedEventsInFile->GetN()-1,BurstID,static_cast<NA62Reconstruction*>(fReco)->GetNProcessedEventsInFile());
  }

  // L1TPRejectionPerBurst
  if(fHL1TPRejectionPerBurst){
    if(fHL1TPRejectionPerBurst->GetN()>0) {
      fHL1TPRejectionPerBurst->GetPoint(fHL1TPRejectionPerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHL1TPRejectionPerBurst->RemovePoint(fHL1TPRejectionPerBurst->GetN()-1); //remove last point
    }
    fHL1TPRejectionPerBurst->Set(fHL1TPRejectionPerBurst->GetN()+1);
  }

  // L1TPInputControlPerBurst
  if(fHL1TPInputControlPerBurst){
    if(fHL1TPInputControlPerBurst->GetN()>0) {
      fHL1TPInputControlPerBurst->GetPoint(fHL1TPInputControlPerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHL1TPInputControlPerBurst->RemovePoint(fHL1TPInputControlPerBurst->GetN()-1); //remove last point
    }
    fHL1TPInputControlPerBurst->Set(fHL1TPInputControlPerBurst->GetN()+1);
  }

  // L1TPInputPeriodicsPerBurst
  if(fHL1TPInputPeriodicsPerBurst){
    if(fHL1TPInputPeriodicsPerBurst->GetN()>0) {
      fHL1TPInputPeriodicsPerBurst->GetPoint(fHL1TPInputPeriodicsPerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHL1TPInputPeriodicsPerBurst->RemovePoint(fHL1TPInputPeriodicsPerBurst->GetN()-1); //remove last point
    }
    fHL1TPInputPeriodicsPerBurst->Set(fHL1TPInputPeriodicsPerBurst->GetN()+1);
  }

  // L1TPOutputControlPerBurst
  if(fHL1TPOutputControlPerBurst->GetN()>0) {
    fHL1TPOutputControlPerBurst->GetPoint(fHL1TPOutputControlPerBurst->GetN()-1,LastValueX,LastValueY);
    if(LastValueX==BurstID || LastValueX<0) fHL1TPOutputControlPerBurst->RemovePoint(fHL1TPOutputControlPerBurst->GetN()-1); //remove last point
  }
  fHL1TPOutputControlPerBurst->Set(fHL1TPOutputControlPerBurst->GetN()+1);

  // L1TPOutputPeriodicsPerBurst
  if(fHL1TPOutputPeriodicsPerBurst) {
    if(fHL1TPOutputPeriodicsPerBurst->GetN()>0) {
      fHL1TPOutputPeriodicsPerBurst->GetPoint(fHL1TPOutputPeriodicsPerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHL1TPOutputPeriodicsPerBurst->RemovePoint(fHL1TPOutputPeriodicsPerBurst->GetN()-1); //remove last point
    }
    fHL1TPOutputPeriodicsPerBurst->Set(fHL1TPOutputPeriodicsPerBurst->GetN()+1);
  }

  std::vector<L1MaskBlock> L1Infos = NA62RecoManager::GetInstance()->GetEventHeader()->GetL1TPData()->GetL0Masks();
  UInt_t nL0MasksOn = L1Infos.size();
  // Loop on the active masks of L0
  for (UInt_t iMask=0; iMask<nL0MasksOn; iMask++) {
    Int_t l0MaskID = (Int_t) L1Infos.at(iMask).GetL0MaskID();
    if(l0MaskID>=NL1Masks) {
      std::cerr << "[NA62OnlineMonitor] WARNING: Invalid L0MaskID (= " << l0MaskID << ")!" << std::endl;
      continue;
    }

    // L1TPRejectionPerMaskPerBurst
    if(fHL1TPRejectionPerMaskPerBurst){
      if(fHL1TPRejectionPerMaskPerBurst[l0MaskID]->GetN()>0) {
        fHL1TPRejectionPerMaskPerBurst[l0MaskID]->GetPoint(fHL1TPRejectionPerMaskPerBurst[l0MaskID]->GetN()-1,LastValueX,LastValueY);
        if(LastValueX==BurstID || LastValueX<0) fHL1TPRejectionPerMaskPerBurst[l0MaskID]->RemovePoint(fHL1TPRejectionPerMaskPerBurst[l0MaskID]->GetN()-1); //remove last point
      }
      fHL1TPRejectionPerMaskPerBurst[l0MaskID]->Set(fHL1TPRejectionPerMaskPerBurst[l0MaskID]->GetN()+1);
    }

    // L1TPInputPhysicsPerMaskPerBurst
    if(fHL1TPInputPhysicsPerMaskPerBurst){
      if(fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->GetN()>0) {
        fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->GetPoint(fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->GetN()-1,LastValueX,LastValueY);
        if(LastValueX==BurstID || LastValueX<0) fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->RemovePoint(fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->GetN()-1); //remove last point
      }
      fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->Set(fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->GetN()+1);
    }

    //L1TPOutputPhysicsPerMaskPerBurst
    if(fHL1TPOutputPhysicsPerMaskPerBurst) {
      if(fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->GetN()>0) {
        fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->GetPoint(fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->GetN()-1,LastValueX,LastValueY);
        if(LastValueX==BurstID || LastValueX<0) fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->RemovePoint(fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->GetN()-1); //remove last point
      }
      fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->Set(fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->GetN()+1);
    }
  }

  Double_t L1Rejection = 0.;
  if (static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APPhysics)){
    L1Rejection = (Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APPhysicsPassed)/(Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APPhysics);
  }
  for (UInt_t iMask=0; iMask<nL0MasksOn; iMask++) {
    Int_t l0MaskID = (Int_t) L1Infos.at(iMask).GetL0MaskID();
    if(l0MaskID>=NL1Masks) {
      std::cerr << "[NA62OnlineMonitor] WARNING: Invalid L0MaskID (= " << l0MaskID << ")!" << std::endl;
      continue;
    }
    Double_t L1RejectionPerMask = 0;
    if (static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APPhysicsPerMask+l0MaskID)){
      L1RejectionPerMask = (Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APPhysicsPassedPerMask+l0MaskID)/(Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APPhysicsPerMask+l0MaskID);
    }
    if(fHL1TPRejectionPerMaskPerBurst) fHL1TPRejectionPerMaskPerBurst[l0MaskID]->SetPoint(fHL1TPRejectionPerMaskPerBurst[l0MaskID]->GetN()-1, BurstID, L1RejectionPerMask);
  }
  if(fHL1TPRejectionPerBurst) fHL1TPRejectionPerBurst->SetPoint(fHL1TPRejectionPerBurst->GetN()-1, BurstID, L1Rejection);

  Double_t L1InFractionControl = 0.;
  Double_t L1InFractionPeriodics = 0.;
  if (static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APTotal)){
    L1InFractionControl = (Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APControl)/(Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APTotal);
    L1InFractionPeriodics = (Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APPeriodics)/(Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APTotal);
  }
  for (UInt_t iMask=0; iMask<nL0MasksOn; iMask++) {
    Int_t l0MaskID = (Int_t) L1Infos.at(iMask).GetL0MaskID();
    if(l0MaskID>=NL1Masks) {
      std::cerr << "[NA62OnlineMonitor] WARNING: Invalid L0MaskID (= " << l0MaskID << ")!" << std::endl;
      continue;
    }
    Double_t L1InFractionPerMask = 0;
    if (static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APTotal)){
      L1InFractionPerMask = (Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APPhysicsPerMask+l0MaskID)/(Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1APTotal);
    }
    if(fHL1TPInputPhysicsPerMaskPerBurst) fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->SetPoint(fHL1TPInputPhysicsPerMaskPerBurst[l0MaskID]->GetN()-1, BurstID, L1InFractionPerMask);
  }
  if(fHL1TPInputControlPerBurst)   fHL1TPInputControlPerBurst->SetPoint(fHL1TPInputControlPerBurst->GetN()-1, BurstID, L1InFractionControl);
  if(fHL1TPInputPeriodicsPerBurst) fHL1TPInputPeriodicsPerBurst->SetPoint(fHL1TPInputPeriodicsPerBurst->GetN()-1, BurstID, L1InFractionPeriodics);

  Double_t L1OutFractionControl = 0.;
  Double_t L1OutFractionPeriodics = 0.;
  if (static_cast<NA62Reconstruction*>(fReco)->GetNReadEventsInFile()){
    L1OutFractionControl = (Double_t)static_cast<NA62Reconstruction*>(fReco)->GetNReadControlTriggerEventsInFile()/(Double_t)static_cast<NA62Reconstruction*>(fReco)->GetNReadEventsInFile();
    L1OutFractionPeriodics = (Double_t)static_cast<NA62Reconstruction*>(fReco)->GetNReadPeriodicTriggerEventsInFile()/(Double_t)static_cast<NA62Reconstruction*>(fReco)->GetNReadEventsInFile();
    for (UInt_t iMask=0; iMask<nL0MasksOn; iMask++) {
      Int_t l0MaskID = (Int_t) L1Infos.at(iMask).GetL0MaskID();
      if(l0MaskID>=NL1Masks) {
        std::cerr << "[NA62OnlineMonitor] WARNING: Invalid L0MaskID (= " << l0MaskID << ")!" << std::endl;
        continue;
      }
      Double_t L1OutFractionPerMask = 0;
      Int_t TotalNEvents = static_cast<NA62Reconstruction*>(fReco)->GetNReadEventsInFile()-static_cast<NA62Reconstruction*>(fReco)->GetNSkippedDownscaledEventsInFile(); //downscaling might affect physics masks (unlike control and periodics, due to the reading of L0TP block)
      if (TotalNEvents){
        L1OutFractionPerMask = (Double_t)static_cast<NA62Reconstruction*>(fReco)->GetL1Counter(kL1PhysicsPassedPerMask+l0MaskID)/(Double_t)TotalNEvents;
      }
      if(fHL1TPOutputPhysicsPerMaskPerBurst) fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->SetPoint(fHL1TPOutputPhysicsPerMaskPerBurst[l0MaskID]->GetN()-1, BurstID, L1OutFractionPerMask);
    }
  }
  if(fHL1TPOutputControlPerBurst)   fHL1TPOutputControlPerBurst->SetPoint(fHL1TPOutputControlPerBurst->GetN()-1, BurstID, L1OutFractionControl);
  if(fHL1TPOutputPeriodicsPerBurst) fHL1TPOutputPeriodicsPerBurst->SetPoint(fHL1TPOutputPeriodicsPerBurst->GetN()-1, BurstID, L1OutFractionPeriodics);

  // T10IntensityPerBurst
  if(fHT10IntensityPerBurst){
    if(fHT10IntensityPerBurst->GetN()>0) {
      fHT10IntensityPerBurst->GetPoint(fHT10IntensityPerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHT10IntensityPerBurst->RemovePoint(fHT10IntensityPerBurst->GetN()-1); //remove last point
    }
    fHT10IntensityPerBurst->Set(fHT10IntensityPerBurst->GetN()+1);
    fHT10IntensityPerBurst->SetPoint(fHT10IntensityPerBurst->GetN()-1,BurstID,NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetIntensityT10());
  }

  // NCountsArgonionPerBurst
  if(fHNCountsArgonionPerBurst) {
    if(fHNCountsArgonionPerBurst->GetN()>0) {
      fHNCountsArgonionPerBurst->GetPoint(fHNCountsArgonionPerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHNCountsArgonionPerBurst->RemovePoint(fHNCountsArgonionPerBurst->GetN()-1); //remove last point
    }
    fHNCountsArgonionPerBurst->Set(fHNCountsArgonionPerBurst->GetN()+1);
    fHNCountsArgonionPerBurst->SetPoint(fHNCountsArgonionPerBurst->GetN()-1,BurstID,NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsARGONION());
  }

  //--- update range of stability plots
  const Int_t NStoredBursts = 190;
  Int_t MinBurstID = 0;
  if (BurstID >= MinBurstID+NStoredBursts) MinBurstID = BurstID - NStoredBursts;
  Int_t MaxBurstID = MinBurstID + NStoredBursts + 10;

  if(fHNProcessedEventsInFile)  fHNProcessedEventsInFile->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
  if(fHT10IntensityPerBurst)    fHT10IntensityPerBurst->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);
  if(fHNCountsArgonionPerBurst) fHNCountsArgonionPerBurst->GetXaxis()->SetRangeUser(MinBurstID,MaxBurstID);

  Int_t NReconstructions = static_cast<NA62Reconstruction*>(fReco)->GetNReconstructions();

  // set all the DigiTimeRaw histos to the same scale
  Double_t MaxDigiTimeRawAll = 0.;
  Double_t MinDigiTimeRawAll = 1e28;
  for(Int_t iReco=0;iReco<NReconstructions;iReco++){
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw()) {
      Double_t MaxDigiTimeRaw = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw()->GetMaximum();
      Double_t MinDigiTimeRaw = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw()->GetMinimum();
      if(MaxDigiTimeRawAll<MaxDigiTimeRaw) MaxDigiTimeRawAll = MaxDigiTimeRaw;
      if(MinDigiTimeRawAll>MinDigiTimeRaw) MinDigiTimeRawAll = MinDigiTimeRaw;
    }
  }
  MaxDigiTimeRawAll*=1.1;
  //for(Int_t iReco=0;iReco<NReconstructions;iReco++){
  //  NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(((NA62Reconstruction*)fReco)->GetRecoSequence()[iReco]);
  //  if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw()) {
  //    SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw()->SetMaximum(MaxDigiTimeRawAll);
  //    SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRaw()->SetMinimum(MinDigiTimeRawAll);
  //  }
  //}

  // set all the DigiTimeRawFine histos to the same scale
  Double_t MaxDigiTimeRawFineAll = 0.;
  Double_t MinDigiTimeRawFineAll = 1e28;
  for(Int_t iReco=0;iReco<NReconstructions;iReco++){
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()) {
      Double_t MaxDigiTimeRawFine = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()->GetMaximum();
      Double_t MinDigiTimeRawFine = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()->GetMinimum();
      if(MaxDigiTimeRawFineAll<MaxDigiTimeRawFine) MaxDigiTimeRawFineAll = MaxDigiTimeRawFine;
      if(MinDigiTimeRawFineAll>MinDigiTimeRawFine) MinDigiTimeRawFineAll = MinDigiTimeRawFine;
    }
  }
  MaxDigiTimeRawFineAll*=1.1;
  //for(Int_t iReco=0;iReco<NReconstructions;iReco++){
  //  NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(((NA62Reconstruction*)fReco)->GetRecoSequence()[iReco]);
  //  if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()) {
  //    SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()->SetMaximum(MaxDigiTimeRawFineAll);
  //    SubDetReco->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine()->SetMinimum(MinDigiTimeRawFineAll);
  //  }
  //}

  // set all the DecoderErrors histos to the same scale
  Double_t MaxDecoderErrorsAll = 0.;
  Double_t MinDecoderErrorsAll = 1e28;
  for(Int_t iReco=0;iReco<NReconstructions;iReco++){
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()) {
      Double_t MaxDecoderErrors = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()->GetMaximum();
      Double_t MinDecoderErrors = SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()->GetMinimum();
      if(MaxDecoderErrorsAll<MaxDecoderErrors) MaxDecoderErrorsAll = MaxDecoderErrors;
      if(MinDecoderErrorsAll>MinDecoderErrors) MinDecoderErrorsAll = MinDecoderErrors;
    }
  }
  MaxDecoderErrorsAll*=1.1;
  for(Int_t iReco=0;iReco<NReconstructions;iReco++){
    NA62VReconstruction * SubDetReco = static_cast<NA62Reconstruction*>(fReco)->FindReco(static_cast<NA62Reconstruction*>(fReco)->GetRecoSequence()[iReco]);
    if (SubDetReco && SubDetReco->GetRawDecoder() && SubDetReco->GetRawDecoder()->GetDecoder() && SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()) {
      SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()->SetMaximum(MaxDecoderErrorsAll);
      SubDetReco->GetRawDecoder()->GetDecoder()->GetHDecoderErrors()->SetMinimum(MinDecoderErrorsAll);
    }
  }

  NA62VOnlineMonitor::Update(BurstID);
}
