// ---------------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-12-24
//
// ---------------------------------------------------------------

/// \class CedarEfficiency
/// \Brief
/// Evaluate the Cedar efficiency
/// \EndBrief
///
/// \Detailed
/// The Cedar efficiency is evaluated using two different kaon samples, from K2pi and K3pi decays,
/// as a function of the requirement on the number of sectors in coincidence.
/// Only control triggers are used, in order to avoid bias due to KTAG at L1.
/// The analyzer can be run in two modes:
/// 1) Reading reconstructed data (without using the --histo command line option);
/// 2) In the HISTO mode (using the --histo command line option), it reads its own
/// output and produces final report in the form of a PDF file.
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TLatex.h>
#include <TLegend.h>
#include <TColor.h>
#include <TBox.h>
#include <TLine.h>
#include <TMultiGraph.h>
#include "BaseAnalysis.hh"
#include "CedarEfficiency.hh"
#include "ConfigSettings.hh"

#include "TRecoCedarEvent.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

CedarEfficiency::CedarEfficiency(Core::BaseAnalysis *ba): Analyzer(ba, "CedarEfficiency") {

  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts
  RequestL0Data();
  RequestL0SpecialTrigger();
  RequestBeamSpecialTrigger();
  RequestTree("Cedar", new TRecoCedarEvent);

  fOutPDFFileName = fAnalyzerName + ".pdf";

  AddParam("MaxCedarDecayDeltaT",   &fMaxCedarDecayDeltaT, 3.); // Max time difference (ns) for the decay-CedarCandidate association
  AddParam("EfficiencyThreshold",   &fEfficiencyThreshold, 0.97);
  AddParam("NSectorsForEfficiency", &fNSectorsForEfficiency, 5);
  AddParam("ArgonionCountsMin",     &fArgonionCountsMin, 1.e5);
  AddParam("NSelectedTriggersMin",  &fNSelectedTriggersMin, 1.e3);

  fSelectionLabels = new TString[NSELECTIONS];
  fSelectionLabels[K2pi] = "K2pi";
  fSelectionLabels[K3pi] = "K3pi";

  fCanvas = nullptr;
}

CedarEfficiency::~CedarEfficiency() {
  delete [] fSelectionLabels;
}

void CedarEfficiency::InitHist() {

  fReadingData = GetIsTree();

  // Allocate memory for histo pointers
  fHMatched    = new TH1F*[NSELECTIONS];
  fHExpected   = new TH1F*[NSELECTIONS];
  fHEfficiency = new TH1F*[NSELECTIONS];
  fHMatchedVsDecayTime         = new TH1F**[NSELECTIONS];
  fHExpectedVsDecayTime        = new TH1F* [NSELECTIONS];
  fHEfficiencyVsDecayTime      = new TH1F**[NSELECTIONS];
  fHMatchedVsEventTimeStamp    = new TH1F**[NSELECTIONS];
  fHExpectedVsEventTimeStamp   = new TH1F* [NSELECTIONS];
  fHEfficiencyVsEventTimeStamp = new TH1F**[NSELECTIONS];
  fNMatchedPerBurst            = new Double_t*[NSELECTIONS];
  fNExpectedPerBurst           = new Double_t [NSELECTIONS];
  fHEfficiencyVsBurstID        = new TGraphErrors**[NSELECTIONS];
  fHNExpectedVsBurstID         = new TGraphErrors*[NSELECTIONS];
  fHNExpectedNormVsBurstID     = new TGraphErrors*[NSELECTIONS];
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    fHMatchedVsDecayTime[iSelection]         = new TH1F*[8];
    fHEfficiencyVsDecayTime[iSelection]      = new TH1F*[8];
    fHMatchedVsEventTimeStamp[iSelection]    = new TH1F*[8];
    fHEfficiencyVsEventTimeStamp[iSelection] = new TH1F*[8];
    fNMatchedPerBurst[iSelection]            = new Double_t[8];
    fHEfficiencyVsBurstID[iSelection]        = new TGraphErrors*[8];
  }
  fHCedarDecayDeltaT = new TH2F*[NSELECTIONS];
  fHCedarNHits       = new TH1F*[NSELECTIONS];
  fHCedarNSectors    = new TH1F*[NSELECTIONS];

  if (fReadingData) {
    std::cout << user_normal() << "Reading reconstructed data" << std::endl;

    Int_t NBinsDecayTime = 200;
    Int_t NBinsEventTimeStamp = 200;
    for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
      BookHisto(new TH1F(Form("hMatched_%s",   fSelectionLabels[iSelection].Data()),
            Form("hMatched_%s",   fSelectionLabels[iSelection].Data()), 8, 0.5, 8.5));
      BookHisto(new TH1F(Form("hExpected_%s",  fSelectionLabels[iSelection].Data()),
            Form("hExpected_%s",  fSelectionLabels[iSelection].Data()), 8, 0.5, 8.5));
      BookHisto(new TH1F(Form("hExpectedVsDecayTime_%s",fSelectionLabels[iSelection].Data()),
            Form("hExpectedVsDecayTime_%s",fSelectionLabels[iSelection].Data()), NBinsDecayTime, -60., 60.));
      BookHisto(new TH1F(Form("hExpectedVsEventTimeStamp_%s",fSelectionLabels[iSelection].Data()),
            Form("hExpectedVsEventTimeStamp_%s",fSelectionLabels[iSelection].Data()), NBinsEventTimeStamp, 0., 6.5));
      for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
        BookHisto(new TH1F(Form("hMatchedVsDecayTime_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence),
              Form("hMatchedVsDecayTime_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), NBinsDecayTime, -60., 60.));
        BookHisto(new TH1F(Form("hMatchedVsEventTimeStamp_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence),
              Form("hMatchedVsEventTimeStamp_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), NBinsEventTimeStamp, 0., 6.5));
        // The only efficiency evaluated at the step 1 should be the one Vs BurstID! (for OM purposes)
        BookHisto(Form("hEfficiencyVsBurstID_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), fHEfficiencyVsBurstID[iSelection][iCoincidence-1] = new TGraphErrors());
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->SetName(Form("hEfficiencyVsBurstID_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence));
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->Set(0);
      }
      BookHisto(Form("hNExpectedVsBurstID_%s",fSelectionLabels[iSelection].Data()), new TGraphErrors());
      fHNExpectedVsBurstID[iSelection] = (TGraphErrors*)fHisto.GetTGraph(Form("hNExpectedVsBurstID_%s",fSelectionLabels[iSelection].Data()));
      fHNExpectedVsBurstID[iSelection]->SetName(Form("hNExpectedVsBurstID_%s",fSelectionLabels[iSelection].Data()));
      fHNExpectedVsBurstID[iSelection]->Set(0);
      BookHisto(Form("hNExpectedNormVsBurstID_%s",fSelectionLabels[iSelection].Data()), new TGraphErrors());
      fHNExpectedNormVsBurstID[iSelection] = (TGraphErrors*)fHisto.GetTGraph(Form("hNExpectedNormVsBurstID_%s",fSelectionLabels[iSelection].Data()));
      fHNExpectedNormVsBurstID[iSelection]->SetName(Form("hNExpectedNormVsBurstID_%s",fSelectionLabels[iSelection].Data()));
      fHNExpectedNormVsBurstID[iSelection]->Set(0);
      // Response histos
      BookHisto(new TH2F(Form("hCedarDecayDeltaT_%s",fSelectionLabels[iSelection].Data()),
            Form("hCedarDecayDeltaT_%s",fSelectionLabels[iSelection].Data()), 81,-0.5,80.5,100,-10.,10.));
      BookHisto(new TH1F(Form("hCedarNHits_%s",fSelectionLabels[iSelection].Data()),
            Form("hCedarNHits_%s",fSelectionLabels[iSelection].Data()), 81,-0.5,80.5));
      BookHisto(new TH1F(Form("hCedarNSectors_%s",fSelectionLabels[iSelection].Data()),
            Form("hCedarNSectors_%s",fSelectionLabels[iSelection].Data()), 9,-0.5,8.5));
    }
    BookHisto("hArgonionCountsVsBurstID", new TGraphErrors());
    fHArgonionCountsVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->SetName("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->Set(0);
    BookHisto("hNTriggersVsBurstID", new TGraphErrors());
    fHNTriggersVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNTriggersVsBurstID");
    fHNTriggersVsBurstID->SetName("hNTriggersVsBurstID");
    fHNTriggersVsBurstID->Set(0);
    BookHisto("hNSelectedTriggersVsBurstID", new TGraphErrors());
    fHNSelectedTriggersVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->SetName("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->Set(0);

    // Set up the online monitor
    CreateCanvas("CedarEfficiencyCanvas");
    for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
      for(UInt_t iCoincidence=4;iCoincidence<=8;iCoincidence++){
        PlacePlotOnCanvas(Form("hEfficiencyVsBurstID_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), "CedarEfficiencyCanvas",1,1);
      }
    }
    SetUpdateInterval(50000);
  }
  else {
    std::cout << user_normal() << "Reading my own output" << std::endl;
    for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
      fHExpected[iSelection]   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, Form("hExpected_%s",  fSelectionLabels[iSelection].Data()), true));
      fHExpected[iSelection]->Sumw2();
      fHExpectedVsDecayTime[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,
          Form("hExpectedVsDecayTime_%s",fSelectionLabels[iSelection].Data()), true));
      fHExpectedVsDecayTime[iSelection]->Sumw2();
      fHExpectedVsEventTimeStamp[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,
          Form("hExpectedVsEventTimeStamp_%s",fSelectionLabels[iSelection].Data()), true));
      fHExpectedVsEventTimeStamp[iSelection]->Sumw2();
      fHMatched[iSelection]    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, Form("hMatched_%s",   fSelectionLabels[iSelection].Data()), true));
      fHMatched[iSelection]->Sumw2();
      BookHisto(new TH1F(Form("hEfficiency_%s",fSelectionLabels[iSelection].Data()),
            Form("hEfficiency_%s",fSelectionLabels[iSelection].Data()), 8, 0.5, 8.5));
      fHEfficiency[iSelection] = static_cast<TH1F*>(fHisto.GetTH1(Form("hEfficiency_%s",fSelectionLabels[iSelection].Data())));
      for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
        fHMatchedVsDecayTime[iSelection][iCoincidence-1]    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,
            Form("hMatchedVsDecayTime_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), true));
        fHMatchedVsDecayTime[iSelection][iCoincidence-1]->Sumw2();
        BookHisto(new TH1F(Form("hEfficiencyVsDecayTime_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence),
              Form("hEfficiencyVsDecayTime_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), fHExpectedVsDecayTime[iSelection]->GetNbinsX(), -60.,60.));
        fHEfficiencyVsDecayTime[iSelection][iCoincidence-1] =
          static_cast<TH1F*>(fHisto.GetTH1(Form("hEfficiencyVsDecayTime_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence)));
        fHMatchedVsEventTimeStamp[iSelection][iCoincidence-1]    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,
            Form("hMatchedVsEventTimeStamp_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), true));
        fHMatchedVsEventTimeStamp[iSelection][iCoincidence-1]->Sumw2();
        BookHisto(new TH1F(Form("hEfficiencyVsEventTimeStamp_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence),
              Form("hEfficiencyVsEventTimeStamp_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), fHExpectedVsEventTimeStamp[iSelection]->GetNbinsX(), 0., 6.5));
        fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1] =
          static_cast<TH1F*>(fHisto.GetTH1(Form("hEfficiencyVsEventTimeStamp_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence)));
        // Retrieve fHEfficiencyVsBurstID evaluated at the step 1
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1] = reinterpret_cast<TGraphErrors*>(RequestHistogram(fAnalyzerName,
            Form("hEfficiencyVsBurstID_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), true));
      }
      fHNExpectedVsBurstID[iSelection] = reinterpret_cast<TGraphErrors*>(RequestHistogram(fAnalyzerName, Form("hNExpectedVsBurstID_%s",fSelectionLabels[iSelection].Data()), true));
      fHNExpectedNormVsBurstID[iSelection] = reinterpret_cast<TGraphErrors*>(RequestHistogram(fAnalyzerName, Form("hNExpectedNormVsBurstID_%s",fSelectionLabels[iSelection].Data()), true));
      // Response histos
      fHCedarDecayDeltaT[iSelection] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,
          Form("hCedarDecayDeltaT_%s",fSelectionLabels[iSelection].Data()), true));
      fHCedarNHits[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,
          Form("hCedarNHits_%s",fSelectionLabels[iSelection].Data()), true));
      fHCedarNSectors[iSelection] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,
          Form("hCedarNSectors_%s",fSelectionLabels[iSelection].Data()), true));
    }
    fHArgonionCountsVsBurstID    = reinterpret_cast<TGraphErrors*>(RequestHistogram(fAnalyzerName, "hArgonionCountsVsBurstID",    true));
    fHNTriggersVsBurstID         = reinterpret_cast<TGraphErrors*>(RequestHistogram(fAnalyzerName, "hNTriggersVsBurstID",         true));
    fHNSelectedTriggersVsBurstID = reinterpret_cast<TGraphErrors*>(RequestHistogram(fAnalyzerName, "hNSelectedTriggersVsBurstID", true));
  }
}

void CedarEfficiency::ProcessSpecialTriggerUser(int, unsigned int triggerType){
  if(triggerType!=0x23) return; // only EOB
  fArgonionCounts = GetBeamSpecialTrigger()->GetCountsARGONION()/1.e9;
}

void CedarEfficiency::Process(Int_t) {

  if (!fReadingData) return;

  fBurstID        = GetEventHeader()->GetBurstID();
  fDecayTime      = GetEventHeader()->GetFineTime()*ClockPeriod/256.; //default event time
  fEventTimeStamp = GetEventHeader()->GetTimeStamp()*ClockPeriod/1.e9; // [s]

  fNTriggers++;

  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t PhysicsData   = L0DataType & 0x1;
  Bool_t CTRLTrigger   = L0DataType & 0x10;
  //Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0x1)) || CTRLTrigger; //MASK0 only + CTRL
  Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0xFF)) || CTRLTrigger; //ALL MASKS + CTRL
  if (!TriggerOK) return; // process control triggers and selected MASKS only

  fNSelectedTriggers++;

  TRecoCedarEvent *CedarEvent = GetEvent<TRecoCedarEvent>();

  //Bool_t   CedarLimiterON = kFALSE;
  UInt_t   SelectionMask  = 0;

  Bool_t K2piSelected = *GetOutput<Bool_t>("K2piSelection.EventSelected");
  Bool_t K3piSelected = *GetOutput<Bool_t>("K3piSelection.EventSelected");
  if(K2piSelected) {
    SelectionMask = (SelectionMask|(1<<K2pi));
    fDecayTime = *GetOutput<Double_t>("K2piSelection.K2piTime");
  }
  if(K3piSelected) {
    SelectionMask = (SelectionMask|(1<<K3pi));
    fDecayTime = *GetOutput<Double_t>("K3piSelection.K3piTime");
  }

  if(!SelectionMask) return;

  // Find best-match within Cedar Candidates wrt DecayTime
  TRecoCedarCandidate* BestCedarCandidate[8];
  for(Int_t iCoincidence=1;iCoincidence<=8;iCoincidence++) {
    BestCedarCandidate[iCoincidence-1] = nullptr;
    Double_t CedarDecayDeltaT = 1.e19;
    for(Int_t iCedarCand=0; iCedarCand<CedarEvent->GetNCandidates(); iCedarCand++){
      TRecoCedarCandidate *CedarCandidate = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCedarCand));
      if(CedarCandidate->GetNSectors()<iCoincidence) continue; // ignore candidates with less than N sectors
      if(fabs(CedarCandidate->GetTime()-fDecayTime)<fabs(CedarDecayDeltaT)) {
        CedarDecayDeltaT = CedarCandidate->GetTime()-fDecayTime;
        BestCedarCandidate[iCoincidence-1] = CedarCandidate;
      }
    }
  }
  //CedarLimiterON = ((CedarEvent->GetErrorMask()&(1<<28))>>28);

  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    if(!(SelectionMask&(1<<iSelection))) continue;
    // Fill the "expected hits" histograms
    for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++) FillHisto(Form("hExpected_%s",fSelectionLabels[iSelection].Data()),iCoincidence);
    FillHisto(Form("hExpectedVsDecayTime_%s",fSelectionLabels[iSelection].Data()), fDecayTime);
    FillHisto(Form("hExpectedVsEventTimeStamp_%s",fSelectionLabels[iSelection].Data()), fEventTimeStamp);
    fNExpectedPerBurst[iSelection]++;

    // Fill the "matched hits" histograms
    for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
      if(!BestCedarCandidate[iCoincidence-1]) continue;
      if(fabs(BestCedarCandidate[iCoincidence-1]->GetTime()-fDecayTime)<fMaxCedarDecayDeltaT) {
        FillHisto(Form("hMatched_%s",fSelectionLabels[iSelection].Data()), iCoincidence);
        FillHisto(Form("hMatchedVsDecayTime_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), fDecayTime);
        FillHisto(Form("hMatchedVsEventTimeStamp_%s_NCoinc%u",fSelectionLabels[iSelection].Data(),iCoincidence), fEventTimeStamp);
        fNMatchedPerBurst[iSelection][iCoincidence-1]++;
      }
    }
    // Fill the response histos
    if(BestCedarCandidate[fNSectorsForEfficiency-1]) {
      FillHisto(Form("hCedarDecayDeltaT_%s",fSelectionLabels[iSelection].Data()), BestCedarCandidate[fNSectorsForEfficiency-1]->GetNHits(),BestCedarCandidate[fNSectorsForEfficiency-1]->GetTime()-fDecayTime);
      FillHisto(Form("hCedarNHits_%s",fSelectionLabels[iSelection].Data()), BestCedarCandidate[fNSectorsForEfficiency-1]->GetNHits());
    FillHisto(Form("hCedarNSectors_%s",fSelectionLabels[iSelection].Data()), BestCedarCandidate[fNSectorsForEfficiency-1]->GetNSectors());
    }
  }
}

void CedarEfficiency::StartOfRunUser() {
  if(6560<=GetRunID() && GetRunID()<7000){ //KTAG4 was out
    fEfficiencyThreshold = 0.95;
  }
}

void CedarEfficiency::EndOfJobUser() {

  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  /////////////
  // HISTO mode

  if (!fReadingData) {
    if (!fHMatched) {
      std::cout << user_normal() << "Asked to read my own output but cannot found it" << std::endl;
      return;
    }

    /////////////////////////
    // Create efficiency histos

    for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
      fHEfficiency[iSelection]->Divide(fHMatched[iSelection], fHExpected[iSelection], 1., 1., "B");
      for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
        fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->Divide(fHMatchedVsDecayTime[iSelection][iCoincidence-1],
            fHExpectedVsDecayTime[iSelection], 1., 1., "B");
        fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->Divide(fHMatchedVsEventTimeStamp[iSelection][iCoincidence-1],
            fHExpectedVsEventTimeStamp[iSelection], 1., 1., "B");
      }
    }

    /////////////////////////
    // Create the BadBurst list

    CreateBadBurstList();

    // Remove empty bursts from the TGraphs
    for(Int_t iPoint=0;iPoint<fHEfficiencyVsBurstID[0][0]->GetN();iPoint++){
      double  BurstID=0., NSelectedTriggers=0.;
      fHNSelectedTriggersVsBurstID->GetPoint(iPoint,BurstID,NSelectedTriggers);
      if(NSelectedTriggers<fNSelectedTriggersMin){
        for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
          for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
            fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->RemovePoint(iPoint);
          }
          fHNExpectedVsBurstID[iSelection]->RemovePoint(iPoint);
          fHNExpectedNormVsBurstID[iSelection]->RemovePoint(iPoint);
        }
        fHArgonionCountsVsBurstID->RemovePoint(iPoint);
        fHNTriggersVsBurstID->RemovePoint(iPoint);
        fHNSelectedTriggersVsBurstID->RemovePoint(iPoint);
        iPoint--;
      }
    }

    /////////////////////////
    // Produce the PDF output

    BuildPDFReport();

  }

  SaveAllPlots();
  gErrorIgnoreLevel = -1; // restore the default
}

void CedarEfficiency::StartOfBurstUser() {
  fBurstID = GetBurstID();
  fArgonionCounts = 0.;
  fNTriggers = 0.;
  fNSelectedTriggers = 0.;
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
      fNMatchedPerBurst[iSelection][iCoincidence-1] = 0;
    }
    fNExpectedPerBurst[iSelection] = 0;
  }
}

void CedarEfficiency::EndOfBurstUser() {
  if (fReadingData) {
    for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
      fHNExpectedVsBurstID[iSelection]->Set(fHNExpectedVsBurstID[iSelection]->GetN()+1);
      fHNExpectedVsBurstID[iSelection]->SetPoint(fHNExpectedVsBurstID[iSelection]->GetN()-1,fBurstID,fNExpectedPerBurst[iSelection]);
      fHNExpectedVsBurstID[iSelection]->SetPointError(fHNExpectedVsBurstID[iSelection]->GetN()-1,0,sqrt(fNExpectedPerBurst[iSelection]));
      double NExpectedNorm=0., eNExpectedNorm=0.;
      if(fNSelectedTriggers){
        NExpectedNorm  = fNExpectedPerBurst[iSelection]/fNSelectedTriggers;
        eNExpectedNorm = sqrt(NExpectedNorm*(1.-NExpectedNorm)/fNSelectedTriggers);
      }
      fHNExpectedNormVsBurstID[iSelection]->Set(fHNExpectedNormVsBurstID[iSelection]->GetN()+1);
      fHNExpectedNormVsBurstID[iSelection]->SetPoint(fHNExpectedNormVsBurstID[iSelection]->GetN()-1,fBurstID,NExpectedNorm);
      fHNExpectedNormVsBurstID[iSelection]->SetPointError(fHNExpectedNormVsBurstID[iSelection]->GetN()-1,0,eNExpectedNorm);
      for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
        double Efficiency=0., eEfficiency=0.;
        if(fNExpectedPerBurst[iSelection]){
          Efficiency  = fNMatchedPerBurst[iSelection][iCoincidence-1]/fNExpectedPerBurst[iSelection];
          eEfficiency = sqrt(Efficiency*(1.-Efficiency)/fNExpectedPerBurst[iSelection]);
        }
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->Set(fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->GetN()+1);
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->SetPoint(fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->GetN()-1,fBurstID,Efficiency);
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->SetPointError(fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->GetN()-1,0,eEfficiency);
      }
    }
    fHArgonionCountsVsBurstID->Set(fHArgonionCountsVsBurstID->GetN()+1);
    fHArgonionCountsVsBurstID->SetPoint(fHArgonionCountsVsBurstID->GetN()-1,fBurstID, fArgonionCounts);
    fHArgonionCountsVsBurstID->SetPointError(fHArgonionCountsVsBurstID->GetN()-1,0,0);
    fHNTriggersVsBurstID->Set(fHNTriggersVsBurstID->GetN()+1);
    fHNTriggersVsBurstID->SetPoint(fHNTriggersVsBurstID->GetN()-1,fBurstID, fNTriggers);
    fHNTriggersVsBurstID->SetPointError(fHNTriggersVsBurstID->GetN()-1,0,0);
    fHNSelectedTriggersVsBurstID->Set(fHNSelectedTriggersVsBurstID->GetN()+1);
    fHNSelectedTriggersVsBurstID->SetPoint(fHNSelectedTriggersVsBurstID->GetN()-1,fBurstID, fNSelectedTriggers);
    fHNSelectedTriggersVsBurstID->SetPointError(fHNSelectedTriggersVsBurstID->GetN()-1,0,0);
  }
}

void CedarEfficiency::BuildPDFReport() {
  if(fCanvas) delete fCanvas;
  fCanvas = new TCanvas("Canvas");

  // Efficiency vs NCoincidences
  TString DrawOption = "";
  TLegend* Legend = new TLegend(0.13,0.13,0.3,0.25);
  Legend->SetFillColor(kWhite);
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    if(iSelection) DrawOption = "same";
    fHEfficiency[iSelection]->SetTitle(Form("Cedar Efficiency vs NCoincidences for run %d",GetRunID()));
    fHEfficiency[iSelection]->SetStats(0);
    fHEfficiency[iSelection]->SetLineColor(kBlack);
    fHEfficiency[iSelection]->SetMarkerColor(kBlack);
    fHEfficiency[iSelection]->SetMarkerStyle(20+iSelection*4);
    fHEfficiency[iSelection]->SetMarkerSize(0.8);
    fHEfficiency[iSelection]->Draw(DrawOption);
    fHEfficiency[iSelection]->GetXaxis()->SetTitle("NCoincidences");
    fHEfficiency[iSelection]->GetYaxis()->SetTitle("Efficiency");
    Legend->AddEntry(fHEfficiency[iSelection],fSelectionLabels[iSelection]);
  }
  Legend->Draw();
  fCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the canvas
  delete Legend;

  // Efficiency vs DecayTime
  DrawOption = "";
  Legend = new TLegend(0.13,0.13,0.25,0.4);
  Legend->SetFillColor(kWhite);
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    if(iSelection) DrawOption = "same";
    for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
      if(iCoincidence-1) DrawOption = "same";
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->SetTitle(Form("Cedar Efficiency vs DecayTime for run %d",GetRunID()));
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->SetStats(0);
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->SetLineColor(iCoincidence);
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->SetMarkerColor(iCoincidence);
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->SetMarkerStyle(20+iSelection*4);
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->SetMarkerSize(0.8);
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->Draw(DrawOption);
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->GetXaxis()->SetTitle("DecayTime [ns]");
      fHEfficiencyVsDecayTime[iSelection][iCoincidence-1]->GetYaxis()->SetTitle("Efficiency");
      if(!iSelection) Legend->AddEntry(fHEfficiencyVsDecayTime[iSelection][iCoincidence-1],Form("#geq %d-fold",iCoincidence));
    }
  }
  Legend->Draw();
  fCanvas->Print(fOutPDFFileName, "pdf");
  delete Legend;

  // Efficiency vs EventTimeStamp
  DrawOption = "";
  Legend = new TLegend(0.13,0.13,0.25,0.4);
  Legend->SetFillColor(kWhite);
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    if(iSelection) DrawOption = "same";
    for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
      if(iCoincidence-1) DrawOption = "same";
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->SetTitle(Form("Cedar Efficiency vs EventTimeStamp for run %d",GetRunID()));
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->SetStats(0);
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->SetLineColor(iCoincidence);
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->SetMarkerColor(iCoincidence);
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->SetMarkerStyle(20+iSelection*4);
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->SetMarkerSize(0.8);
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->Draw(DrawOption);
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->GetXaxis()->SetTitle("EventTimeStamp [s]");
      fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1]->GetYaxis()->SetTitle("Efficiency");
      if(!iSelection) Legend->AddEntry(fHEfficiencyVsEventTimeStamp[iSelection][iCoincidence-1],Form("#geq %d-fold",iCoincidence));
    }
  }
  Legend->Draw();
  fCanvas->Print(fOutPDFFileName, "pdf");
  delete Legend;

  // Efficiency vs BurstID
  if(fHEfficiencyVsBurstID[0][0]->GetN()){
    for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
      Legend = new TLegend(0.13,0.13,0.25,0.4);
      Legend->SetFillColor(kWhite);
      TMultiGraph * mGraph = new TMultiGraph();
      mGraph->SetTitle(Form("Cedar Efficiency (%s selection) vs BurstID for run %d",fSelectionLabels[iSelection].Data(),GetRunID()));
      for(UInt_t iCoincidence=1;iCoincidence<=8;iCoincidence++){
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->SetTitle(Form("Cedar Efficiency vs BurstID for run %d",GetRunID()));
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->SetLineColor(iCoincidence);
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->SetMarkerColor(iCoincidence);
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->SetMarkerStyle(20+iSelection*4);
        fHEfficiencyVsBurstID[iSelection][iCoincidence-1]->SetMarkerSize(0.3);
        mGraph->Add(fHEfficiencyVsBurstID[iSelection][iCoincidence-1],"P");
        Legend->AddEntry(fHEfficiencyVsBurstID[iSelection][iCoincidence-1],Form("#geq %d-fold",iCoincidence),"pl");
      }
      mGraph->Draw("AP");
      mGraph->GetXaxis()->SetTitle("BurstID");
      mGraph->GetYaxis()->SetTitle("Efficiency");
      mGraph->GetYaxis()->SetRangeUser(0.,1.05);
      Legend->Draw();
      TLine* EfficiencyThrBurst = new TLine(mGraph->GetXaxis()->GetXmin(),fEfficiencyThreshold,mGraph->GetXaxis()->GetXmax(),fEfficiencyThreshold);
      EfficiencyThrBurst->SetLineColor(fNSectorsForEfficiency);
      EfficiencyThrBurst->Draw();
      fCanvas->Print(fOutPDFFileName, "pdf");

      // Zoom
      mGraph->GetYaxis()->SetRangeUser(0.75,1.05);
      mGraph->Draw("AP");
      EfficiencyThrBurst->Draw();
      fCanvas->Print(fOutPDFFileName, "pdf");

      delete Legend;
      delete EfficiencyThrBurst;
    }
  }

  // NHits and NSectors for the selected samples
  fCanvas->Clear();
  fCanvas->Divide(2,1);
  fCanvas->cd(1);
  DrawOption = "";
  Bool_t AlreadyDrawn = false;
  TLegend* LegendHits = new TLegend(0.76,0.80,0.89,0.89);
  LegendHits->SetFillColor(kWhite);
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    if(AlreadyDrawn) {
      DrawOption = "HISTOsame";
      fHCedarNHits[iSelection]->Scale(fHCedarNHits[0]->GetEntries()/fHCedarNHits[iSelection]->GetEntries()); //scale
    }
    fHCedarNHits[iSelection]->SetTitle(Form("Cedar NHits distribution for run %d",GetRunID()));
    fHCedarNHits[iSelection]->SetStats(0);
    fHCedarNHits[iSelection]->SetLineColor(iSelection+1);
    if(fHCedarNHits[iSelection]->GetEntries()){
      fHCedarNHits[iSelection]->Draw(DrawOption);
      AlreadyDrawn = true;
    }
    fHCedarNHits[iSelection]->GetXaxis()->SetTitle("Cedar NHits");
    fHCedarNHits[iSelection]->GetYaxis()->SetTitle("Arbitrary Units");
    LegendHits->AddEntry(fHCedarNHits[iSelection],fSelectionLabels[iSelection]);
  }
  LegendHits->Draw();
  fCanvas->cd(2);
  DrawOption = "";
  AlreadyDrawn = false;
  TLegend* LegendSectors = new TLegend(0.12,0.80,0.25,0.89);
  LegendSectors->SetFillColor(kWhite);
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    if(AlreadyDrawn) {
      DrawOption = "HISTOsame";
      fHCedarNSectors[iSelection]->Scale(fHCedarNSectors[0]->GetEntries()/fHCedarNSectors[iSelection]->GetEntries()); //scale
    }
    fHCedarNSectors[iSelection]->SetTitle(Form("Cedar NSectors distribution for run %d",GetRunID()));
    fHCedarNSectors[iSelection]->SetStats(0);
    fHCedarNSectors[iSelection]->SetLineColor(iSelection+1);
    if(fHCedarNSectors[iSelection]->GetEntries()){
      fHCedarNSectors[iSelection]->Draw(DrawOption);
      AlreadyDrawn = true;
    }
    fHCedarNSectors[iSelection]->GetXaxis()->SetTitle("Cedar NSectors");
    fHCedarNSectors[iSelection]->GetYaxis()->SetTitle("Arbitrary Units");
    LegendSectors->AddEntry(fHCedarNSectors[iSelection],fSelectionLabels[iSelection]);
  }
  LegendSectors->Draw();
  fCanvas->Print(fOutPDFFileName, "pdf");
  delete LegendHits;
  delete LegendSectors;

  // DeltaT for the selected samples
  fCanvas->Clear();
  DrawOption = "";
  AlreadyDrawn = false;
  Legend = new TLegend(0.76,0.80,0.89,0.89);
  Legend->SetFillColor(kWhite);
  double yMax = 0.;
  TH1D ** hCedarDecayDeltaT1D = new TH1D*[NSELECTIONS];
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    hCedarDecayDeltaT1D[iSelection] = fHCedarDecayDeltaT[iSelection]->ProjectionY(Form("hCedarDecayDeltaT1D_%s",fSelectionLabels[iSelection].Data()));
    if(AlreadyDrawn) {
      DrawOption = "HISTOsame";
      hCedarDecayDeltaT1D[iSelection]->Scale(hCedarDecayDeltaT1D[0]->GetEntries()/hCedarDecayDeltaT1D[iSelection]->GetEntries()); //scale
    }
    hCedarDecayDeltaT1D[iSelection]->SetTitle(Form("CedarTime wrt Decay Time for run %d",GetRunID()));
    hCedarDecayDeltaT1D[iSelection]->SetStats(0);
    hCedarDecayDeltaT1D[iSelection]->SetLineColor(iSelection+1);
    if(hCedarDecayDeltaT1D[iSelection]->GetEntries()){
      hCedarDecayDeltaT1D[iSelection]->Draw(DrawOption);
      AlreadyDrawn = true;
    }
    if(yMax<hCedarDecayDeltaT1D[iSelection]->GetBinContent(hCedarDecayDeltaT1D[iSelection]->GetMaximumBin())){
      yMax = hCedarDecayDeltaT1D[iSelection]->GetBinContent(hCedarDecayDeltaT1D[iSelection]->GetMaximumBin());
    }
    hCedarDecayDeltaT1D[iSelection]->GetXaxis()->SetTitle("CedarTime-DecayTime [ns]");
    hCedarDecayDeltaT1D[iSelection]->GetYaxis()->SetTitle("Arbitrary Units");
    Legend->AddEntry(hCedarDecayDeltaT1D[iSelection],fSelectionLabels[iSelection]);
  }
  hCedarDecayDeltaT1D[0]->SetMaximum(1.1*yMax);
  Legend->Draw();
  fCanvas->Print(fOutPDFFileName, "pdf");
  delete Legend;
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++) delete hCedarDecayDeltaT1D[iSelection];
  delete [] hCedarDecayDeltaT1D;

  // DeltaT vs Number of Hits
  fCanvas->Clear();
  DrawOption = "";
  Legend = new TLegend(0.76,0.80,0.89,0.89);
  Legend->SetFillColor(kWhite);
  TMultiGraph* mGraph = new TMultiGraph();
  mGraph->SetTitle(Form("CedarTime-DecayTime RMS vs CedarNHits for run %d",GetRunID()));
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    TH1D* hCedarDecayDeltaT = 0;
    TGraphErrors* grSigmaDeltaT = new TGraphErrors();
    grSigmaDeltaT->Set(0);
    grSigmaDeltaT->SetLineColor(iSelection+1);
    grSigmaDeltaT->SetMarkerColor(iSelection+1);
    grSigmaDeltaT->SetMarkerStyle(20);
    grSigmaDeltaT->SetMarkerSize(0.7);
    for(Int_t iHitBin=1;iHitBin<=fHCedarDecayDeltaT[iSelection]->GetNbinsX();iHitBin++){
      Int_t NHits = fHCedarDecayDeltaT[iSelection]->GetXaxis()->GetBinCenter(iHitBin);
      hCedarDecayDeltaT = fHCedarDecayDeltaT[iSelection]->ProjectionY(Form("hCedarDecayDeltaTSlice_%s",fSelectionLabels[iSelection].Data()),iHitBin,iHitBin);
      grSigmaDeltaT->Set(grSigmaDeltaT->GetN()+1);
      grSigmaDeltaT->SetPoint(grSigmaDeltaT->GetN()-1,NHits,hCedarDecayDeltaT->GetRMS());
      grSigmaDeltaT->SetPointError(grSigmaDeltaT->GetN()-1,0,hCedarDecayDeltaT->GetRMSError());
    }
    mGraph->Add(grSigmaDeltaT,"LP");
    Legend->AddEntry(grSigmaDeltaT,fSelectionLabels[iSelection],"lp");
    delete hCedarDecayDeltaT;
  }
  mGraph->Draw("A");
  mGraph->GetXaxis()->SetTitle("Cedar NHits");
  mGraph->GetYaxis()->SetTitle("CedarTime-DecayTime RMS [ns]");
  mGraph->GetYaxis()->SetRangeUser(0.,1.5);
  Legend->Draw();
  fCanvas->Print(fOutPDFFileName, "pdf");
  delete Legend;
  delete mGraph;

  fCanvas->Print(Form(fOutPDFFileName + "]"), "pdf");
}

void CedarEfficiency::CreateBadBurstList() {

  Int_t NEmptyBursts=0, NBadLowStat=0, NBadEfficiency=0;
  std::ofstream BadBurstList;
  BadBurstList.open(Form("CedarEfficiency.BadBursts.thr%.3f.dat",fEfficiencyThreshold));
  TGraphErrors** hEfficiency = new TGraphErrors*[NSELECTIONS];
  Double_t* Efficiency       = new Double_t[NSELECTIONS];
  Double_t* eEfficiency      = new Double_t[NSELECTIONS];
  Double_t* NExpected        = new Double_t[NSELECTIONS];
  Double_t* NExpectedNorm    = new Double_t[NSELECTIONS];
  for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
    hEfficiency[iSelection] = fHEfficiencyVsBurstID[iSelection][fNSectorsForEfficiency-1];
  }
  for(Int_t iPoint=0;iPoint<hEfficiency[K2pi]->GetN();iPoint++){
    double BurstID=0., Argonion=0., NTriggers=0., NSelectedTriggers=0.;
    for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
      hEfficiency[iSelection]->GetPoint(iPoint,BurstID,Efficiency[iSelection]);
      eEfficiency[iSelection] = hEfficiency[iSelection]->GetErrorY(iPoint);
      fHNExpectedVsBurstID[iSelection]->GetPoint(iPoint,BurstID,NExpected[iSelection]);
      fHNExpectedNormVsBurstID[iSelection]->GetPoint(iPoint,BurstID,NExpectedNorm[iSelection]);
    }
    fHArgonionCountsVsBurstID->GetPoint(iPoint,BurstID,Argonion);
    fHNTriggersVsBurstID->GetPoint(iPoint,BurstID,NTriggers);
    fHNSelectedTriggersVsBurstID->GetPoint(iPoint,BurstID,NSelectedTriggers);
    if(NSelectedTriggers<fNSelectedTriggersMin){
      if(NTriggers==0 && Argonion==0){ // Corrupted file
        BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " BADFILE " << std::endl;
      }
      else if(Argonion==0){ // No Argonion info
        BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " NO_ARGN " << std::endl;
      }
      else if(Argonion*1.e9<fArgonionCountsMin){ // Low number of triggers due to no beam
        BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " LOWARGN " << Argonion*1.e9 << std::endl;
      }
      else{
        BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " LOWTRIG " << NSelectedTriggers << std::endl;
      }
      NEmptyBursts++;
    }
    else if(NExpected[K2pi]<100){ // Low stat (K2pi)
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " LOWK2PI " << NExpected[K2pi] << " [NExpectedNorm: " << NExpectedNorm[K2pi] << ", SelectedTriggers: " << NSelectedTriggers << "]" <<  std::endl;
      NBadLowStat++;
    }
    else if(NExpected[K3pi]<100){ // Low stat (K3pi)
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " LOWK3PI " << NExpected[K3pi] << " [NExpectedNorm: " << NExpectedNorm[K3pi] << ", SelectedTriggers: " << NSelectedTriggers << "]" <<  std::endl;
      NBadLowStat++;
    }
    else if(Efficiency[K2pi]<fEfficiencyThreshold && Efficiency[K3pi]<fEfficiencyThreshold){ // bad Cedar efficiency
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " EFFI>="<<fNSectorsForEfficiency;
      for(UInt_t iSelection=0;iSelection<NSELECTIONS;iSelection++){
        BadBurstList << " eff("<<fSelectionLabels[iSelection]<<"): " << Efficiency[iSelection] << " +/- " << eEfficiency[iSelection];
      }
      BadBurstList << std::endl;
      NBadEfficiency++;
    }
  }
  BadBurstList.close();
  std::cout << user_standard() << "***** BADBURST SUMMARY ***** " << std::endl;
  std::cout << user_standard() << "Bad(LowStat):  " << NBadLowStat << std::endl;
  std::cout << user_standard() << "Bad(EffN>="<<fNSectorsForEfficiency<<"):  " << NBadEfficiency << std::endl;
  std::cout << user_standard() << "Bad(CedarTotal): " << NBadLowStat+NBadEfficiency;
  std::cout << user_standard() << " over " << hEfficiency[K2pi]->GetN()-NEmptyBursts << " non-empty bursts (NEmptyBursts: " << NEmptyBursts<< ")"<< std::endl;
  std::cout << user_standard() << "**************************** " << std::endl;
  delete [] hEfficiency;
  delete [] Efficiency;
  delete [] eEfficiency;
  delete [] NExpected;
  delete [] NExpectedNorm;
}
