#include <stdlib.h>
#include <iostream>
#include <vector>
#include <TChain.h>
#include <TStyle.h>
#include "LevelOnePerformance.hh"
#include "functions.hh"
#include "EventHeader.hh"
#include "BeamParameters.hh"
#include "TLegend.h"
#include "TLegendEntry.h"
#include "TPaveText.h"
#include "BaseAnalysis.hh"
#include "ConfigSettings.hh"

#include "Event.hh"
#include "Persistency.hh"
#include <bitset>

using namespace std;
using namespace NA62Constants;

/// \class LevelOnePerformance
/// \Brief
/// Analyzer to assess the performance of the L1 trigger for each L0 mask, to be used solely on data.
/// \EndBrief
/// \Detailed
/// It can be run in three modes:
/// 1) [DEFAULT] read the reconstructed data and outputs histograms with numerators and denominators for each L1 algorithm and for each L0 mask,
/// for the evaluation of the efficiency and fraction of rejected events of each algorithm.
/// 2) [via --histo in command line] read its own output to evaluate efficiency and rejection.
/// 3) [via -p "LevelOnePerformance:AllSteps" in command line] performs both steps 1 and 2.
/// Only physics L0 data type is considered and only autopass L1 events are considered for the evaluation.
/// The maximum binning for stability plots given by the maximum number of bursts is by default 3000. It can be modified via, e.g.:
/// \code
/// ./MyApplication ... -p "LevelOnePerformance:MaxNBursts=2000"
/// \endcode
/// Other parameters that can be modified from the command line:
/// BurstChunk: number of bursts averaged for a single efficiency/rejection point (default 5)
/// VerboseFlag: = 0 for minimum print out; !=0 for verbose printout (default 0)
/// NTriggers:  number of level-zero masks to be considered (default 16)
/// BuildPDFReport:  if !=0, will produce a pdf report named LevelOnePerformance.pdf (default 1)
/// EventsForUpdate: if >0, will Fill and Evaluate performances every EventsForUpdate events (default 0)
/// \author Tommaso Spadaro (tommaso.spadaro@lnf.infn.it)
/// \EndDetailed

LevelOnePerformance::LevelOnePerformance(BaseAnalysis *ba) :
	Analyzer(ba, "LevelOnePerformance"),
	fHistogramInputWasRetrieve(false),
	fL1TriggerInfoVsL0MaskIndex(nullptr),
	fMaxBurstBin(0),
	fiTags(0),
	fEventTime(0),
	fNSpectrometerCandidateTracks(0),
	fSpectrometerCondition(0),
	fNSpectrometerBeamTracks(0),
	fPiPi0Track(0),
	fPi0lavFlag(0),
	fPi0lkrFlag(0),
	fPi0lavlkrFlag(0),
	fKM2Flag(0),
	fKM2Track(0),
	fTrkClusInTime(0),
	fNCHODMaxSlabs(0),
	fNCHODSlabs(0),
	fNTotalLAVHits(0),
	fLevelZeroWord(0),
	fNTotalMUV3Pads(0),
	fDistaLKrMin(0)
{
// Parameters

  AddParam("MaxNBursts",  &fMaxNBursts,  3000);      // max number of bins in stability histograms
  AddParam("BurstChunk",  &fBurstChunk,  20);        // number of bursts contributing to a single efficiency/rejection evaluation
  AddParam("VerboseFlag", &fVerboseFlag, 0);         // if != 0 --> maximum printout
  AddParam("AllSteps",    &fAllSteps,  0);           // if != 0 --> Perform both 1 and 2 steps
  AddParam("NTriggers",   &fNTriggers, 16);          // number of level-zero masks to be considered
  AddParam("BuildPDFReport", &fBuildPDFReport, 1);   // if == 0 --> the PDF Report is NOT generated
  AddParam("EventsForUpdate", &fEventsForUpdate, 0); // if != 0 --> update efficiency/rejection plots every fEventsForUpdate events
  if (fNTriggers<=0) {
    cout << user_normal() << "ParameterReading Wrong number of trigger in input " << fNTriggers << endl;
    return;
  }
  if (fMaxNBursts<=0) {
    cout << user_normal() << "ParameterReading Wrong number of MaxNBursts in input " << fMaxNBursts << endl;
    return;
  }
  if (fBurstChunk<=0) {
    cout << user_normal() << "ParameterReading Wrong number of BurstChunk in input " << fBurstChunk << endl;
    return;
  }

  // Default values of other private variables

  fGlobalFillCounter = 0; // Fill counter for plot update

  fReadingData = kTRUE; // reading reconstructed data, by default
  fHistogramInputWasRetrieved = kFALSE; // by default, needed when running the --histo

  fFlagOrCutAlgoMode[0] = "Cutting";
  fFlagOrCutAlgoMode[1] = "Flagging";
  for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) {
    fCountersAreInitialized[i] = kFALSE;
    fNAlgosCuttingOrFlagging[i][0] = 0;
    fNAlgosCuttingOrFlagging[i][1] = 0;
    fOldInCuttingOrFlagging[i][0] = 0;
    fOldInCuttingOrFlagging[i][1] = 0;
    fL1TriggerNumToMaskID[i] = -1;
    fhE[i][0] = nullptr;
    fhE[i][1] = nullptr;
    fhR[i][0] = nullptr;
    fhR[i][1] = nullptr;
    fhNInCutOrFlag[i][0] = nullptr;
    fhNInCutOrFlag[i][1] = nullptr;
    for (Int_t j=0; j<NUMBER_OF_L1_ALGOS; j++) {
      fhEff[i][j] = NULL;
      fhRej[i][j] = NULL;
    }
  }

  fThetaTrim5 = 1.2E-3;
  fpKNominalMod = 75000.;
  fpKNominal.SetPxPyPzE(fThetaTrim5*fpKNominalMod,0.,fpKNominalMod,TMath::Sqrt(fpKNominalMod*fpKNominalMod*(1.+fThetaTrim5*fThetaTrim5) + NA62Constants::SQMKCH));

// Require reconstruction trees

  RequestL0Data();
  RequestL1Data();

  RequestTree("LAV",new TRecoLAVEvent);
  RequestTree("LKr",new TRecoLKrEvent);
  RequestTree("Spectrometer",new TRecoSpectrometerEvent);
  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts

// Event classification tags

  fNEventTags = 9;
  fTagsEvent = new TString[fNEventTags]; fControlSampleLabels = new const char*[fNEventTags];
  fTagsEvent[0] = ""            ;     fControlSampleLabels[0] = "all events";
  fTagsEvent[1] = "BeamTracks"  ;     fControlSampleLabels[1] = "beam tracks";
  fTagsEvent[2] = "MultiTracks" ;     fControlSampleLabels[2] = "multi tracks";
  fTagsEvent[3] = "Pi0Tracks"   ;     fControlSampleLabels[3] = "pipi0";
  fTagsEvent[4] = "Pi0gg"       ;     fControlSampleLabels[4] = "pipi0 2g LKr";
  fTagsEvent[5] = "Pi0LAV"      ;     fControlSampleLabels[5] = "pipi0 2g LAV";
  fTagsEvent[6] = "KMu2"        ;     fControlSampleLabels[6] = "Km2";
  fTagsEvent[7] = "Pi0gLAVgLKr" ;     fControlSampleLabels[7] = "pipi0 1g LKr 1g LAV";
  fTagsEvent[8] = "K3Pi"        ;     fControlSampleLabels[8] = "K to pi+pi+pi-";
}

LevelOnePerformance::~LevelOnePerformance() {
  delete[] fTagsEvent;
  delete[] fControlSampleLabels;
}

void LevelOnePerformance::InitOutput(){}

void LevelOnePerformance::InitHist(){
  fReadingData = GetIsTree();
  if (fAllSteps && !fReadingData) {
    cerr << user_normal() << ">> Input internal inconsistency - Ask 2 steps but apparently is not ReadingData" << endl;
    return;
  }

  if (fReadingData) {

    //cout << user_normal() << "Booking histograms" << endl;

// General L0 and L1 diagnostics plots

    BookHisto(new TH2F("L1AutopassVsL0Mask", "L1 autopassed vs bitL0Mask", 1200, -0.5, 1199.5, 2,-0.5,1.5));
    BookHisto(new TH1F("L0triggerType", "L0 Trigger Type word", 256, -0.5, 255.5));
    BookHisto(new TH1F("L0triggerFlags", "L0 Trigger Flags", 1200, -0.5, 1199.5));
    BookHisto(new TH1F("L0MasksOn", "L0 Trigger Masks On", NUMBER_OF_L0_MASKS, -0.5, NUMBER_OF_L0_MASKS-0.5));
    BookHisto(new TH1F("L1BadFlags", "L1TimeOut(bit0), L1AlgosDisabled(bit1) Flags", 4, -0.5, 3.5));
    BookHisto(new TH1F("L1AlgoQualityBadFlags", "NotProcessed(bit0) EmptyPacket(bit1) Corrupted(bit2)", 8, -0.5, 7.5));
    BookHisto("L1TriggerInfoVsL0MaskIndex", fL1TriggerInfoVsL0MaskIndex = new TH2F("L1TriggerInfoVsL0MaskIndex", "L1 Trigger Info Index vs L0 Trigger Mask Index", NUMBER_OF_L0_MASKS, -0.5, NUMBER_OF_L0_MASKS-0.5, NUMBER_OF_L0_MASKS, -0.5, NUMBER_OF_L0_MASKS-0.5));

// population of event classification algorithms

    for (Int_t i=0; i<fNTriggers; i++) {
      TH1F* histoTmp = new TH1F(Form("ControlSamplesPop%d",i)," test ",fNEventTags,0,fNEventTags);
      for (int j=0; j<fNEventTags; j++) histoTmp->GetXaxis()->SetBinLabel(j+1,fControlSampleLabels[j]);
      BookHisto(histoTmp);
    }

// local plots from tracking analysis

    BookHisto(new TH2F("MomentumVsZ","Momentum vs zVertex",200,0,250000.,200,0,100000.));
    BookHisto(new TH2F("MomentumVsPCA","Momentum vs PCA",200,0,1000.,200,0,100000.));
    BookHisto(new TH2F("SquareMissingMassMuVsMomentum","MmissMu2 vs Ptrack",200,15000.,65000.,250,-100000.,150000.));
    BookHisto(new TH2F("mmissVsMomentum","Mmiss2 vs mom",200,0,100000.,250,-100000.,150000.));
    BookHisto(new TH2F("BeamTracksVsFVTracks","Number of Beam vs FV Tracks",10,0,10,10,0,10));

// local plots from pp0 analysis

    BookHisto(new TH2F("LKrdtVsdrPi","LKr tracks dt clu-hit vs dr clu-hit",100,0,4000.,100,-200.,200.));
    BookHisto(new TH2F("LAVLKrggDTheVsDPhi","DThe vs DPhi from gg LKr-LAV",100,-1.,1.,100,-1.,1.));
    BookHisto(new TH2F("SquareMissingMassVsZvertex","Mmiss2 vs zVertex from gg",200,0,250000.,250,-100000.,150000.));
    for (Int_t i = 0; i<fNTriggers; i++){
      BookHisto(new TH2F(Form("LAVvsLKrNClusters_Type%d",i),"Number of LAV vs LKr clusters pp0 events",10,0,10,10,0,10));
      BookHisto(new TH2F(Form("LAVvsLKrEClusters_11_Type%d",i),"energy of LAV clus vs LKr pp0 events",100,0,100000.,100,0,20.));
      BookHisto(new TH2F(Form("LKrClusters_20_Type%d",i),"energy of 2 clus pp0 events",100,0,100000.,100,0,100000.));
      BookHisto(new TH2F(Form("LAVClusters_02_Type%d",i),"energy of 2 clus pp0 events",100,0,20,100,0,20));
    }

    BookHisto(new TH2F("LKrdtVsdr","LKrgammas dt clu-hit vs dr clu-hit",100,0,4000.,100,-200.,200.));
    BookHisto(new TH1F("LAVdt","LAV dt clu-expected",100,-50,50.));
    BookHisto(new TH2F("NLKrVsNLAV","NLKr vs NLAV",10,0,10,10,0,10));
    BookHisto(new TH2F("LKrdtVsdrPiCell","LKr tracks dt clu-hit vs dr clu-hit",100,0,4000.,100,-200.,200.));
    BookHisto(new TH1F("MomentumForLAVLKr11","Pion momentum",90,0,90000.));
    BookHisto(new TH1F("MomentumForLAVLKr20","Pion momentum",90,0,90000.));
    BookHisto(new TH1F("MomentumForLAVLKr02","Pion momentum",90,0,90000.));

// book numerator and denominator histograms + special histogram for storing number of algos in cutting and nunmber of algos in flagging

    for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) {
      for (Int_t j=0; j<2; j++)
	BookHisto(Form("NCutVsBurstForL0Mask%d%d",i,j),
		  fhNInCutOrFlag[i][j] = new TH1F(Form("NL1AlgosInCutOrFlagForL0Mask%d%d",i,j), "L1 Algos in Cut/Flag vs BurstID",fMaxNBursts, -0.5, fMaxNBursts-0.5));

      BookHisto(Form("EffDenL1AlgoVsBurstL0Mask%d",i),
		fhE[i][0] = new TH2F(Form("EfficiencyDenominatorL1AlgoVsBurstL0Mask%d",i), "L1AlgoVs;Burst ID",
				     fMaxNBursts, -0.5, fMaxNBursts-0.5,NUMBER_OF_L1_ALGOS+2,0,NUMBER_OF_L1_ALGOS+2));
      BookHisto(Form("EffNumL1AlgoVsBurstL0Mask%d",i),
		fhE[i][1] = new TH2F(Form("EfficiencyNumeratorL1AlgoVsBurstL0Mask%d",i), "L1AlgoVs;Burst ID",
				     fMaxNBursts, -0.5, fMaxNBursts-0.5,NUMBER_OF_L1_ALGOS+2,0,NUMBER_OF_L1_ALGOS+2));
      BookHisto(Form("RejDenL1AlgoVsBurstL0Mask%d",i),
		fhR[i][0] = new TH2F(Form("RejectionDenominatorL1AlgoVsBurstL0Mask%d",i), "L1AlgoVs;Burst ID",
				     fMaxNBursts, -0.5, fMaxNBursts-0.5,NUMBER_OF_L1_ALGOS+2,0,NUMBER_OF_L1_ALGOS+2));
      BookHisto(Form("RejNumL1AlgoVsBurstL0Mask%d",i),
		fhR[i][1] = new TH2F(Form("RejectionNumeratorL1AlgoVsBurstL0Mask%d",i), "L1AlgoVs;Burst ID",
				     fMaxNBursts, -0.5, fMaxNBursts-0.5,NUMBER_OF_L1_ALGOS+2,0,NUMBER_OF_L1_ALGOS+2));

    }
  }
  else { // Not Reading data: in any case has to retrieve histograms from input
    cout << user_normal() << "Reading output from previous execution" << endl;

    fL1TriggerInfoVsL0MaskIndex = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"L1TriggerInfoVsL0MaskIndex", true));

    for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) {

      for (Int_t j=0; j<2; j++)
      fhNInCutOrFlag[i][j] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, Form("NL1AlgosInCutOrFlagForL0Mask%d%d",i,j), true));

      if (fhNInCutOrFlag[i][0] == NULL || fhNInCutOrFlag[i][1] == NULL) {
	cerr << user_normal() << ">> Mandatory histograms not retrieved - internal inconsistency - ConfigHistogram L0Mask=" << i << " " << fhNInCutOrFlag[i][0] << " " << fhNInCutOrFlag[i][1] << endl;
	return;
      }
      fhE[i][0] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("EfficiencyDenominatorL1AlgoVsBurstL0Mask%d",i), true));
      fhE[i][1] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("EfficiencyNumeratorL1AlgoVsBurstL0Mask%d",i), true));
      fhR[i][0] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("RejectionDenominatorL1AlgoVsBurstL0Mask%d",i), true));
      fhR[i][1] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("RejectionNumeratorL1AlgoVsBurstL0Mask%d",i), true));
      if (fhE[i][0] == NULL || fhE[i][1] == NULL || fhR[i][0] == NULL || fhR[i][1] == NULL) {
	cerr << user_normal() << ">> Mandatory histograms not retrieved - internal inconsistency -  L0Mask = " << i << " Histos "
	     << fhE[i][0] << " " << fhE[i][1] << " " << fhR[i][0] << " " << fhR[i][1] << endl;
	return;
      }
    }
    fHistogramInputWasRetrieved = kTRUE;
  }

  for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) CreateCanvas(Form("L1PerformanceForL0Mask%d",i));
}

void LevelOnePerformance::StartOfRunUser(){
	/// \MemberDescr
	/// This method is called at the beginning of the processing (corresponding to a start of run in the normal NA62 data taking)\n
	/// Do here your start of run processing if any
	/// \EndMemberDescr

  fpKNominal.SetVectM(BeamParameters::GetInstance()->GetBeamThreeMomentum(), MKCH);
}

void LevelOnePerformance::StartOfBurstUser(){
	/// \MemberDescr
	/// This method is called when a new file is opened in the ROOT TChain (corresponding to a start/end of burst in the normal NA62 data taking) + at the beginning of the first file\n
	/// Do here your start/end of burst processing if any
	/// \EndMemberDescr

  for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) {
    fNumeratorRejectionOverallCut[i] = 0;
    fNumeratorRejectionOverall[i] = 0;

    for (Int_t j=0; j<NUMBER_OF_L1_ALGOS; j++) {
      fNumeratorEfficiency[i][j] = 0;
      fDenominatorEfficiency[i][j] = 0;
      fNumeratorRejection[i][j] = 0;
      fDenominatorRejection[i][j] = 0;

    }
  }

//	for (Int_t i=0; i<fNTriggers; i++) {
//	  fTimePrevious[i] = -1;
//	  fTimeDiffPrevious[i] = -1;
//	}
//	fPreviousEventNumber = -1;
}

void LevelOnePerformance::EndOfBurstUser(){
  if (fReadingData) FillPerformance();
}

void LevelOnePerformance::FillPerformance(){
  Int_t ib = GetEventHeader()->GetBurstID();
  for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) {

    if (
         (fOldInCuttingOrFlagging[i][0]>0 && fOldInCuttingOrFlagging[i][0] != fNAlgosCuttingOrFlagging[i][0]) ||
         (fOldInCuttingOrFlagging[i][1]>0 && fOldInCuttingOrFlagging[i][1] != fNAlgosCuttingOrFlagging[i][1])) {
      cerr << user_normal() << ">> Internal inconsistency OR algorithm change during Run,"
           << " From " << fOldInCuttingOrFlagging[i][0] << " to " << fNAlgosCuttingOrFlagging[i][0]
           << " From " << fOldInCuttingOrFlagging[i][1] << " to " << fNAlgosCuttingOrFlagging[i][1]
           << " Mask " << i << endl;
      return;
    }

    for (int j=0; j<2; j++) fhNInCutOrFlag[i][j]->SetBinContent(ib+1,fNAlgosCuttingOrFlagging[i][j]);
    Int_t NTotAlgos = fNAlgosCuttingOrFlagging[i][0]+fNAlgosCuttingOrFlagging[i][1];
    if (NTotAlgos == 0) continue;


    if (fOldInCuttingOrFlagging[i][0]==0 && fOldInCuttingOrFlagging[i][1]==0) { // first burst

      for (Int_t k=0; k<2; k++) {
        for (Int_t j=0; j<NTotAlgos; j++) {
          fhE[i][k]->GetYaxis()->SetBinLabel(j+1,fConditionName[i][j].Data());
          fhR[i][k]->GetYaxis()->SetBinLabel(j+1,fConditionName[i][j].Data());
        }
        fhE[i][k]->GetYaxis()->SetBinLabel(NTotAlgos+1,"AllCutting");
        fhR[i][k]->GetYaxis()->SetBinLabel(NTotAlgos+1,"AllCutting");
        fhE[i][k]->GetYaxis()->SetBinLabel(NTotAlgos+2,"All");
        fhR[i][k]->GetYaxis()->SetBinLabel(NTotAlgos+2,"All");
        for (Int_t j=NTotAlgos+2; j<NUMBER_OF_L1_ALGOS; j++) {
          fhE[i][k]->GetYaxis()->SetBinLabel(j+1,Form("NotUsed%d",j-NTotAlgos-1));
          fhR[i][k]->GetYaxis()->SetBinLabel(j+1,Form("NotUsed%d",j-NTotAlgos-1));
        }
      }
    }

    for (Int_t j=0; j<2; j++) fOldInCuttingOrFlagging[i][j] = fNAlgosCuttingOrFlagging[i][j];

    for (Int_t j=0; j<NTotAlgos; j++) {
      if (fDenominatorEfficiency[i][j]) {
        fhE[i][0]->SetBinContent(ib+1,j+1,fhE[i][0]->GetBinContent(ib+1,j+1)+fDenominatorEfficiency[i][j]); // sumup denominator
        fhE[i][1]->SetBinContent(ib+1,j+1,fhE[i][1]->GetBinContent(ib+1,j+1)+fNumeratorEfficiency[i][j]); // sumup numerator
      }

      if (fDenominatorRejection[i][j]) {
        fhR[i][0]->SetBinContent(ib+1,j+1,fhR[i][0]->GetBinContent(ib+1,j+1)+fDenominatorRejection[i][j]); // sumup denominator
        fhR[i][1]->SetBinContent(ib+1,j+1,fhR[i][1]->GetBinContent(ib+1,j+1)+fNumeratorRejection[i][j]); // sumup numerator
      }
    }
    if (fDenominatorRejection[i][0]) {
      fhR[i][0]->SetBinContent(ib+1,NTotAlgos+1,fhR[i][0]->GetBinContent(ib+1,NTotAlgos+1)+fDenominatorRejection[i][0]); // sumup denominator
      fhR[i][1]->SetBinContent(ib+1,NTotAlgos+1,fhR[i][1]->GetBinContent(ib+1,NTotAlgos+1)+fNumeratorRejectionOverallCut[i]); // sumup numerator
    }
    if (fDenominatorRejection[i][0]) {
      fhR[i][0]->SetBinContent(ib+1,NTotAlgos+2,fhR[i][0]->GetBinContent(ib+1,NTotAlgos+2)+fDenominatorRejection[i][0]); // sumup denominator
      fhR[i][1]->SetBinContent(ib+1,NTotAlgos+2,fhR[i][1]->GetBinContent(ib+1,NTotAlgos+2)+fNumeratorRejectionOverall[i]); // sumup numerator
    }
  }
}

void LevelOnePerformance::EvaluatePerformance() {
  cout << user_normal() << "Evaluating performance and producing bad burst list" << endl;

  Int_t RunNumber = GetRunID();
  Int_t numberOfBurstBins = fhE[0][0]->GetXaxis()->GetNbins();
  Int_t rebinnedBurstBins = Double_t(numberOfBurstBins)/fBurstChunk;
  Double_t binMin = fhE[0][0]->GetXaxis()->GetXmin();
  Double_t binMax = binMin + fBurstChunk*rebinnedBurstBins;

  fMaxBurstBin = 0;
  ofstream BadBurstFile;
  BadBurstFile.open("./LevelOnePerformance_BadBursts.dat");

  for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) {

    for(Int_t ibinX=0; ibinX<fL1TriggerInfoVsL0MaskIndex->GetNbinsX(); ibinX++){
      if(fL1TriggerInfoVsL0MaskIndex->GetBinContent(ibinX+1,i+1))
        fL1TriggerNumToMaskID[i] = ibinX;
    }
    // retrieve number of algorithms in cutting and flagging for this level zero mask

    for (Int_t j=0; j<2; j++) {
      fNAlgosCuttingOrFlagging[i][j] = 0;
      for (Int_t ib=0; ib<numberOfBurstBins; ib++) {
        Int_t content = fhNInCutOrFlag[i][j]->GetBinContent(ib+1);
        if (content) {
          fNAlgosCuttingOrFlagging[i][j] = content;
          break;
        }
      }
    }

    Int_t NTotAlgos = fNAlgosCuttingOrFlagging[i][0] + fNAlgosCuttingOrFlagging[i][1];
    if (NTotAlgos == 0) continue; // L0 Mask was not enabled OR no L1 for that L0Mask

    for (Int_t j=0; j<NTotAlgos+2; j++) {
      TString conditionName = fhE[i][0]->GetYaxis()->GetBinLabel(j+1);
      if (fhEff[i][j] == NULL && fhRej[i][j] == NULL) {
	BookHisto(Form("EfficiencyVsBurstForL0Mask%dL1Algo%d",i,j), fhEff[i][j] = new TH1F(Form("EfficiencyVsBurstL0Mask%dL1Algo%d",i,j), "L1AlgoEffiVs;Burst ID",rebinnedBurstBins,binMin, binMax));
	fhEff[i][j]->SetName(conditionName.Data());
	fhEff[i][j]->SetMaximum(1.1); fhEff[i][j]->SetMinimum(0.);
	fhEff[i][j]->GetXaxis()->SetTitle("Burst ID");
	fhEff[i][j]->GetYaxis()->SetTitle("Algo Efficiency");
	PlacePlotOnCanvas(Form("EfficiencyVsBurstForL0Mask%dL1Algo%d",i,j), Form("L1PerformanceForL0Mask%d",i),1,1);

	BookHisto(Form("RejectionVsBurstForL0Mask%dL1Algo%d",i,j),  fhRej[i][j] = new TH1F(Form("RejectionVsBurstL0Mask%dL1Algo%d",i,j), "L1AlgoRejectionVs;Burst ID",rebinnedBurstBins,binMin, binMax));
	fhRej[i][j]->SetName(conditionName.Data());
	fhRej[i][j]->SetMaximum(1.1); fhRej[i][j]->SetMinimum(0.);
	fhRej[i][j]->GetXaxis()->SetTitle("Burst ID");
	fhRej[i][j]->GetYaxis()->SetTitle("Algo Rejection");
	PlacePlotOnCanvas(Form("RejectionVsBurstForL0Mask%dL1Algo%d",i,j), Form("L1PerformanceForL0Mask%d",i),2,1);
      }
    }

    BadBurstFile << "Summary for L0 Mask " << fL1TriggerNumToMaskID[i] << " Number of algos in cutting or flagging = " << fNAlgosCuttingOrFlagging[i][0] << " " << fNAlgosCuttingOrFlagging[i][1] << endl;

    // prepare variables for bad burst lists
    Int_t NBursts = 0;
    Int_t NBadBursts = 0;
    Bool_t BadBurstFlag = 0;

    for (Int_t ibb = 0; ibb<rebinnedBurstBins; ibb++) {

      Array1D<bool> BadBurstForAlgo = createArray1D<bool>(NTotAlgos+2, false);

      // evaluate numerators and denominators after burst rebinning

      Int_t rebinnedDenominators[NUMBER_OF_L1_ALGOS+2][2]={0}; // index 0 -> efficiency; index 1 -> rejection
      Int_t rebinnedNumerators[NUMBER_OF_L1_ALGOS+2][2]={0};   // index 0 -> efficiency; index 1 -> rejection
      for (Int_t k=0; k<2; k++) {
	TH2F** histoTmp = fhE[i]; // evaluate efficiency
	TH1F** histoOutTmp = fhEff[i]; // store efficiency
	if (k) {
	  histoTmp = fhR[i]; // evaluate rejection
	  histoOutTmp = fhRej[i]; // store rejection
	}
 	for (Int_t j=0; j<NTotAlgos+2; j++) {
	  for (Int_t ib = 0; ib<fBurstChunk; ib++) {
	    rebinnedDenominators[j][k] += histoTmp[0]->GetBinContent(ibb*fBurstChunk+ib+1,j+1);
	    rebinnedNumerators[j][k]   += histoTmp[1]->GetBinContent(ibb*fBurstChunk+ib+1,j+1);
	    if (rebinnedDenominators[j][k]) fMaxBurstBin = binMin + (ibb+1)*fBurstChunk; // maximum burst occupied
	  }
	}
	if (rebinnedDenominators[0][1]>0) NBursts++;

	// evaluate single algorithm ratios

	Double_t ratioPerAlgo;        // single algorithm ratio
	Double_t ratioErrorPerAlgo;   // single algorithm ratio

	Double_t ratioTot[2]={1,1};     // total for cutting/cutting+flagging mode
	Double_t ratioErrorTot[2]={0,0}; //error on total ratios

	for (Int_t j=0; j<NTotAlgos; j++) {

	  //if(rebinnedDenominators[j][k]) cout << "Den " << rebinnedDenominators[j][k] << " Num " << rebinnedNumerators[j][k] << endl;
	  CalculateEfficiency(rebinnedDenominators[j][k],rebinnedNumerators[j][k],&ratioPerAlgo,&ratioErrorPerAlgo);
	  //if(rebinnedDenominators[j][k]) cout << "Ratio Per Algo " << ratioPerAlgo << " +- " << ratioErrorPerAlgo << endl;
	  histoOutTmp[j]->SetBinContent(ibb+1,ratioPerAlgo);
	  histoOutTmp[j]->SetBinError(ibb+1,ratioErrorPerAlgo);
	  if(!k && ratioPerAlgo>0 && ratioPerAlgo<0.8) BadBurstForAlgo[j] = 1;

	  // efficiency evaluate as product of single algorithm efficiencies

	  if (j < fNAlgosCuttingOrFlagging[i][0]){
	    ratioTot[0] *= ratioPerAlgo;
	    ratioErrorTot[0] += (ratioErrorPerAlgo*ratioErrorPerAlgo);
	  }
	  ratioTot[1] *= ratioPerAlgo;
	  ratioErrorTot[1] += (ratioErrorPerAlgo*ratioErrorPerAlgo);

	  //if(rebinnedDenominators[j][k]){
	  //  cout << "Ratio ToT " << ratioTot[0] << " +- " << ratioErrorTot[0] << endl;
	  //  cout << "Ratio ToT " << ratioTot[1] << " +- " << ratioErrorTot[1] << endl;
	  //}
	}
	if (k==1) { // for rejection, do not evaluate total ratios as product of single, but directly from the last step
	  for (Int_t j=NTotAlgos; j<NTotAlgos+2; j++) {
	    //if(rebinnedDenominators[0][k]) cout << "Den " << rebinnedDenominators[0][k] << " Num " << rebinnedNumerators[j][k] << endl;
	    CalculateEfficiency(rebinnedDenominators[0][k],rebinnedNumerators[j][k],&ratioTot[j-NTotAlgos],&ratioErrorTot[j-NTotAlgos]);
	  }
	}

	if (fNAlgosCuttingOrFlagging[i][0]==0){ //only Algorithms in flagging
	  ratioTot[0] = 0;
	  ratioErrorTot[0] = 0;
	}

	for (Int_t j=NTotAlgos; j<NTotAlgos+2; j++) {
	  //if(rebinnedDenominators[j-NTotAlgos][k]){
	  //  if(k==1) cout << "Ratio Tot " << ratioTot[j-NTotAlgos] << " +- " << ratioErrorTot[j-NTotAlgos] << endl;
	  //  else cout << "Ratio Tot " << ratioTot[j-NTotAlgos] << " +- " << sqrt(ratioErrorTot[j-NTotAlgos]) << endl;
	  //}

	  histoOutTmp[j]->SetBinContent(ibb+1,ratioTot[j-NTotAlgos]);
	  if(k==1) histoOutTmp[j]->SetBinError(ibb+1,ratioErrorTot[j-NTotAlgos]);
	  else histoOutTmp[j]->SetBinError(ibb+1,sqrt(ratioErrorTot[j-NTotAlgos]));
	  if(!k && ratioTot[j-NTotAlgos]>0 && ratioTot[j-NTotAlgos]<0.8) BadBurstForAlgo[j] = 1;
	}
      }

      for (Int_t j=0; j<NTotAlgos+2; j++) {
	if (BadBurstForAlgo[j]) {
	  BadBurstFlag = 1;
	  TString s = Form("BadBurst %06d %04d L0 Mask %d for L1 Algo ", RunNumber, (Int_t)(binMin+(ibb+0.5)*fBurstChunk),i);
	  TString conditionName = fhE[i][0]->GetYaxis()->GetBinLabel(j+1);
	  s += conditionName;
	  BadBurstFile << s << endl;
	}
      }
      if (BadBurstFlag) NBadBursts++;
      BadBurstFlag = 0;

      if (fVerboseFlag) {
	if (rebinnedDenominators[0][1] == 0) continue; // no events in the denominator for total rejection --> skip burst

	cout << user_standard() << endl;
	cout << user_normal() << "Summary for L0 Mask " << fL1TriggerNumToMaskID[i] << " Burst " << binMin + (ibb+0.5)*fBurstChunk << endl;

	for (Int_t j=0; j<NTotAlgos; j++) {
	  TString conditionName = fhE[i][0]->GetYaxis()->GetBinLabel(j+1);

	  cout  << user_normal() << "Efficiency for " << conditionName.Data()
		<< " = " << rebinnedNumerators[j][0] << "/" << rebinnedDenominators[j][0]
		<< " = " << fhEff[i][j]->GetBinContent(ibb+1)
		<< " +- " <<fhEff[i][j]->GetBinError(ibb+1) << endl;
	}
	cout << user_normal() << "Efficiency for AllCutting"
	     << " = " << fhEff[i][NTotAlgos]->GetBinContent(ibb+1)
	     << " +- " << fhEff[i][NTotAlgos]->GetBinError(ibb+1) << endl;

	cout << user_normal() << "Efficiency for All"
	     << " = " << fhEff[i][NTotAlgos+1]->GetBinContent(ibb+1)
	     << " +- " << fhEff[i][NTotAlgos+1]->GetBinError(ibb+1) << endl;

	for (Int_t j=0; j<NTotAlgos; j++) {
	  TString conditionName = fhE[i][0]->GetYaxis()->GetBinLabel(j+1);

	  cout << user_normal() << "Rejection  for " << conditionName.Data()
	       << " = " << rebinnedNumerators[j][1] << "/" << rebinnedDenominators[j][1]
	       << " = " << fhRej[i][j]->GetBinContent(ibb+1)
	       << " +- "<< fhRej[i][j]->GetBinError(ibb+1) << endl;
	}
	cout << user_normal() << "Rejection  for AllCutting"
	     << " = " << fhRej[i][NTotAlgos]->GetBinContent(ibb+1)
	     << " +- "<< fhRej[i][NTotAlgos]->GetBinError(ibb+1) << endl;

	cout << user_normal() << "Rejection  for All"
	     << " = " << fhRej[i][NTotAlgos+1]->GetBinContent(ibb+1)
	     << " +- "<< fhRej[i][NTotAlgos+1]->GetBinError(ibb+1) << endl;
      }
    }
    BadBurstFile << "L0 Mask " << i << " RunID " << RunNumber << " Total = good + bad (rebin x" << fBurstChunk << ") bursts: " << NBursts << " = " << NBursts-NBadBursts << " + " <<NBadBursts << endl;
    BadBurstFile << "L0 Mask " << i << " RunID " << RunNumber << " Fraction = bad/total bursts: " << (Double_t)NBadBursts/NBursts << endl;
    UpdateCanvas(Form("L1PerformanceForL0Mask%d",i));
  }
  BadBurstFile.close();
}

Int_t LevelOnePerformance::CalculateEfficiency(Int_t denominator, Int_t numerator, Double_t* efficiency, Double_t* error){
  if (denominator) {
    *efficiency = Double_t(numerator)/denominator;
    *error = TMath::Sqrt(*efficiency*(1.-*efficiency)/denominator);
    return 1;
  }
  else{
    *efficiency = 0.;
    *error = 0.;
    return 0;
  }
}


void LevelOnePerformance::BuildPDFReport() {

  //Double_t MinBurst = 0;
  //Double_t MaxBurst = fhE[0][0]->GetXaxis()->GetXmax();

  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  cout << user_normal() << "Building PDF Report in file " << OutputPDFFileName.Data() << endl;

  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetOptStat(11);
  gStyle->SetPalette(1);

  TCanvas *Canvas = new TCanvas("L1Canvas");

  Canvas->Print(Form(OutputPDFFileName + "["), "pdf"); // open file
  Canvas->SetLeftMargin(0.07);
  Canvas->SetRightMargin(0.01);

  // count number of active L0 masks and determine canvas divisions

  Int_t L0ActiveMasks = 0;
  for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) {
    Int_t NTotAlgos = fNAlgosCuttingOrFlagging[i][0] + fNAlgosCuttingOrFlagging[i][1];
    if (NTotAlgos) L0ActiveMasks++;
  }

  cout << user_normal() << "Will plot " << L0ActiveMasks << " masks " << endl;

  Canvas->Divide(2,2);

  cout << user_normal() << "Canvas divided - will plot in the range " << fMaxBurstBin << endl;

  for (Int_t i=0; i<NUMBER_OF_L0_MASKS; i++) {
    Int_t NTotAlgos = fNAlgosCuttingOrFlagging[i][0] + fNAlgosCuttingOrFlagging[i][1];
    if (NTotAlgos == 0) continue;

    cout << user_normal() << "Plot NAlgos " << NTotAlgos  << " for L0 Mask " << fL1TriggerNumToMaskID[i] << endl;

    TLine *l = new TLine();
    l->SetLineWidth(1);
    TPaveText *L0MaskText = new TPaveText(0.05,0.91,0.45,1.00,"brNDC");
    L0MaskText->SetFillStyle(4000);
    L0MaskText->SetLineColor(0);
    L0MaskText->SetBorderSize(0);
    L0MaskText->SetTextFont(132);
    L0MaskText->SetTextSize(0.05);
    L0MaskText->AddText(Form("L0 Trigger Mask %d",fL1TriggerNumToMaskID[i]));

    TPaveText *pt = new TPaveText(0.7,-0.05,0.89,0.1,"brNDC");
    pt->SetFillStyle(4000);
    pt->SetLineColor(0);
    pt->SetBorderSize(0);
    pt->SetTextFont(132);
    pt->SetTextSize(0.05);
    pt->AddText("Burst Number");

    TPaveText *pt2 = new TPaveText(0.02,0.7,0.06,0.9,"brNDC");
    pt2->SetFillStyle(4000);
    pt2->SetLineColor(0);
    pt2->SetBorderSize(0);
    pt2->SetTextFont(132);
    pt2->SetTextSize(0.05);
    TText *text2 = pt2->AddText("Efficiency");
    text2->SetTextAngle(90);

    TPaveText *pt3 = new TPaveText(0.02,0.6,0.06,0.9,"brNDC");
    pt3->SetFillStyle(4000);
    pt3->SetLineColor(0);
    pt3->SetBorderSize(0);
    pt3->SetTextFont(132);
    pt3->SetTextSize(0.05);
    TText *text3 = pt3->AddText("Rejection Factor");
    text3->SetTextAngle(90);

    for (Int_t k=0; k<2; k++) { // effi, rejection
      TH1F** histo;
      if (k==0) histo = fhEff[i];
      else histo = fhRej[i];
      Canvas->GetPad(1+2*k)->Clear();

      TVirtualPad* vp = 0;
      if (!(vp = Canvas->cd(1+2*k))) {
	cout << user_normal() << "Null pad retrieved " << vp << " index is 1 " << k << endl;
	continue;
      }
      vp->DrawFrame(-0.5,0.,fMaxBurstBin,1.1);

      TLegend *leg = new TLegend(0.6,0.91,0.9,1.00);
      leg->SetBorderSize(0);
      leg->SetLineColor(0);
      leg->SetLineStyle(1);
      leg->SetLineWidth(1);
      leg->SetFillColor(0);
      leg->SetFillStyle(1001);

      for (Int_t j=0; j<NTotAlgos; j++) {
	histo[j]->SetMarkerSize(0.2);
	histo[j]->SetMarkerStyle(20+j);
	histo[j]->SetMarkerColor(1+j);
	histo[j]->SetLineColor(j+1);
	TLegendEntry *entry=leg->AddEntry("histo[j]",histo[j]->GetName(),"lep");
	entry->SetLineColor(j+1);
	entry->SetLineStyle(1);
	entry->SetLineWidth(1);
	entry->SetMarkerColor(j+1);
	entry->SetMarkerStyle(20+j);
	entry->SetMarkerSize(0.2);
	histo[j]->SetTitle("Number of events");
	histo[j]->Draw("Psame");
	l->SetLineColor(j+1);
     }
      leg->Draw();
      L0MaskText->Draw();
      pt->Draw();
      if(k==0) pt2->Draw();
      else pt3->Draw();

      Canvas->GetPad(2+2*k)->Clear();
      if (!(vp = Canvas->cd(2+2*k))) {
	cout << user_normal() << "Null pad retrieved " << vp << " index is 2 " << k << endl;
	continue;
      }
      vp->DrawFrame(-0.5,0.,fMaxBurstBin,1.1);

      TLegend *legTotAlgos = new TLegend(0.6,0.91,0.9,1.00);
      legTotAlgos->SetBorderSize(0);
      legTotAlgos->SetLineColor(0);
      legTotAlgos->SetLineStyle(1);
      legTotAlgos->SetLineWidth(1);
      legTotAlgos->SetFillColor(0);
      legTotAlgos->SetFillStyle(1001);

      histo[NTotAlgos]->SetMarkerSize(0.2);
      histo[NTotAlgos]->SetMarkerStyle(28);
      histo[NTotAlgos]->SetMarkerColor(kBlue);
      histo[NTotAlgos]->SetLineColor(kBlue);
      TLegendEntry *entryTot=legTotAlgos->AddEntry("histo[NTotAlgos]",histo[NTotAlgos]->GetName(),"lep");
      //TLegendEntry *entryTot=legTotAlgos->AddEntry("histo[NTotAlgos]","All Cutting Algos","lep");
      entryTot->SetLineColor(kBlue);
      entryTot->SetLineStyle(1);
      entryTot->SetLineWidth(1);
      entryTot->SetMarkerColor(kBlue);
      entryTot->SetMarkerStyle(28);
      entryTot->SetMarkerSize(0.2);
      entryTot->SetTextSize(0.05);
      histo[NTotAlgos]->Draw("Psame");
      l->SetLineColor(kBlue);
      if(k==0) l->DrawLine(-0.5, 0.8, fMaxBurstBin, 0.8);

      //histo[NTotAlgos+1]->SetMarkerSize(0.2);
      //histo[NTotAlgos+1]->SetMarkerStyle(20);
      //histo[NTotAlgos+1]->SetMarkerColor(22);
      //histo[NTotAlgos+1]->SetLineColor(22);
      //TLegendEntry *entryTotPlus=legTotAlgos->AddEntry("histo[NTotAlgos+1]",histo[NTotAlgos+1]->GetName(),"lep");
      ////TLegendEntry *entryTotPlus=legTotAlgos->AddEntry("histo[NTotAlgos+1]","All Algos","lep");
      //entryTotPlus->SetLineColor(22);
      //entryTotPlus->SetLineStyle(1);
      //entryTotPlus->SetLineWidth(1);
      //entryTotPlus->SetMarkerColor(22);
      //entryTotPlus->SetMarkerStyle(20);
      //entryTotPlus->SetMarkerSize(0.2);
      //entryTotPlus->SetTextSize(0.05);
      //histo[NTotAlgos+1]->Draw("Psame");
      legTotAlgos->Draw();
      L0MaskText->Draw();
      pt->Draw();
      if(k==0) pt2->Draw();
      else pt3->Draw();
    }
    Canvas->Print(OutputPDFFileName, "pdf");
  }

  Canvas->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file
  delete Canvas;
  gErrorIgnoreLevel = -1; // restore the default
}


void LevelOnePerformance::Process(Int_t) {

  if (!fReadingData) return; // no action if reading its own output in --histo mode
  if (GetWithMC()) return;

  // Retrieve event trees
  TRecoLAVEvent *LAVEvent = GetEvent<TRecoLAVEvent>();
  TRecoLKrEvent *LKrEvent = GetEvent<TRecoLKrEvent>();

  // Retrieve downstreamTrack package
  std::vector<DownstreamTrack> Tracks = *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");

  // Retrieve trigger information
  EventHeader* rhe = GetEventHeader();

  // L0 Information
  L0TPData *L0TPDataEv = GetL0Data();
  UChar_t L0DT = L0TPDataEv->GetDataType();
  UInt_t L0TriggerFlags = L0TPDataEv->GetTriggerFlags();
  fLevelZeroWord = L0TriggerFlags&0x0000FF;
  fHisto.GetTH1("L0triggerType")->Fill(fLevelZeroWord);
  Bool_t L0PhysicsTrigger = (L0DT & 0x1) != 0;

  if (!L0PhysicsTrigger) return; // skip non-physics triggers

  //  L1 Information
  Int_t level1Word = (rhe->GetTriggerType()&0x00FF00)>>8;  //  Int_t level1Word = (L0TPDataEv->GetTriggerType()&0x00FF00)>>8;
// This is a global, event-related 8 bit word which contains information about L1 (autopass, flagging, bypass, physics verdict, etc)
// bit 0 = OR of all L1 physics verdict
// bit 3 = bad data flag (if set to 1 the L1 trigger configuration was not set properly)
// bit 4 = the event is selected by a L0 mask with no L1 trigger applied on it
// bit 5 = bypassed events (special, periodics and control triggers)
// bit 6 = the event is selected by a L0 mask with a L1 trigger applied in flagging (rather than cutting) mode
// bit 7 = globally flagged (all data processed at L1) or global autopass data (portion of the L1 global bandwidth to be processed at L1 and sent to storage for trigger efficiency studies)

  Int_t L1AutoPassed = 0;
  if (level1Word & (1<<7)) L1AutoPassed = 1;
  fHisto.GetTH2("L1AutopassVsL0Mask")->Fill(L0TriggerFlags,L1AutoPassed);

  if (L1AutoPassed == 0) return; // Keep for the moment only the events with global autopass flag set

  L1TPData *L1TPDataEv = GetL1Data(); //  L1TPDataEv->GetL1FlagMode() // =1 se in flagging mode globale

  if (L1TPDataEv == NULL) return;     // data generally related to the L1 packet of the event can be found at this stage; for documentation, see https://na62-sw.web.cern.ch/sites/na62-sw.web.cern.ch/files/doxygen/da/db2/classL1TPData.html

  std::vector<L1MaskBlock> L1Infos = L1TPDataEv->GetL0Masks(); // retrieving vector of L1 masks; data related to the mask packet can be found here; for documentation, see https://na62-sw.web.cern.ch/sites/na62-sw.web.cern.ch/files/doxygen/dc/da6/classL1MaskBlock.html
  fHisto.GetTH1("L0triggerFlags")->Fill(L0TriggerFlags);
  Int_t nL0masksOn = L1Infos.size();
  fHisto.GetTH1("L0MasksOn")->Fill(nL0masksOn);

  // event classification
  fEventTime = rhe->GetFineTime()*TdcCalib;
  if (fVerboseFlag) {
    cout << user_normal() << "Trigger words: " << rhe->GetTriggerType() << " L0 " << fLevelZeroWord << " L1 " << level1Word << "; Event timestamp " << rhe->GetTimeStamp() << endl;
    cout << user_normal() << "Trigger flags: " << L0TriggerFlags << " Data type " << (Int_t)L0DT << endl;
  }

  fNSpectrometerCandidateTracks = 0;
  fSpectrometerCondition = 0;
  fNSpectrometerBeamTracks = 0;

// zero array which will be filled by pp0analysis

  for(int i=0; i<2; i++) {
    flkrgammas[i]=-1;
    flavgammas[i]=-1;
  }
  for(int i=0; i<1000; i++) {
    flkrcellgammas[i]=-1;
    flavcellgammas[i]=-1;
  }

  fPiPi0Track = -1;
  fPi0lavFlag = -1;
  fPi0lkrFlag = -1;
  fPi0lavlkrFlag = -1;
  fKM2Flag = -1;
  fKM2Track = -1;
  fTrkClusInTime = -1;

  trackingAnalysis(Tracks);
  fHisto.GetTH2("BeamTracksVsFVTracks")->Fill(fNSpectrometerCandidateTracks,fNSpectrometerBeamTracks);

// pi0->gg analysis

  Double_t refTime = fEventTime;
  pp0Analysis(LKrEvent,LAVEvent,Tracks,refTime);
  fiTags = 1;                                               // bit 0 corresponding to all events
  if (fNSpectrometerBeamTracks) fiTags += 2;                // bit 1, beam tracks
  else if (fNSpectrometerCandidateTracks>1) fiTags += 4;    // bit 2, multi-tracks
  else {
    if (fPiPi0Track>=0) fiTags += 8;                        // bit 3, pipi0 (no cut on the photon pair)
    if (fPiPi0Track>=0 && fPi0lkrFlag == 1) fiTags+= 16;    // bit 4, pipi0, 2-gamma in the LKr
    if (fPiPi0Track>=0 && fPi0lavFlag == 1) fiTags+= 32;    // bit 5, pipi0, 2-gamma in the LAV
    if (fPiPi0Track == -1 && fKM2Flag ==1) fiTags+= 64;     // bit 6, Km2
    if (fPiPi0Track>=0 && fPi0lavlkrFlag == 1) fiTags+= 128;// bit 7, pipi0, 1-gamma in the LAV, 1 gamma in the LKr
  }
  NA62Analysis::UserMethods::OutputState state_3pi;
  Bool_t K3PiSelected = *(Bool_t*)GetOutput("K3piSelection.EventSelected",state_3pi);
  if (K3PiSelected) fiTags += 256; // bit 8, K-->3pi

// Loop on the active masks of L0

  for (Int_t i=0; i<nL0masksOn; i++) {
    Int_t maskID = (Int_t) L1Infos.at(i).GetL0MaskID();
    fL1TriggerInfoVsL0MaskIndex->Fill(maskID,i);
    fL1TriggerNumToMaskID[i] = maskID;
    if ((L0TriggerFlags & (1<<maskID)) > 0) { // L0Mask #i is fired
      for (int j=0;j<fNEventTags;j++) {
	if (fiTags & (1<<j)) fHisto.GetTH1(Form("ControlSamplesPop%d",maskID))->Fill(j);
      }

      if (fVerboseFlag) {
	    cout << user_normal() << "Dumping all info for i= "<< i << " mask " << maskID << endl;
	    dumpAllL1Info(L1Infos.at(i));
	    cout << user_standard() << endl;
      }

// check if the algorithms or their order is changed for this L0 mask

      std::vector<L1AlgoBlock> l1algos = L1Infos.at(i).GetL1Algorithms(); // retrieving vector of L1 algorithms; a vector is associated to each mask;  each algo has its own ID (eg. CHOD = 0, RICH = 1, etc.); every data related to the algo packet can be found here; for documentation, see https://na62-sw.web.cern.ch/sites/na62-sw.web.cern.ch/files/doxygen/d2/d22/classL1AlgoBlock.html

      Bool_t changedConfiguration = kFALSE;
      AlgoBlockCollection tmpOrderAlgos;
      tmpOrderAlgos.clear();
      for (Int_t j=0; j<(int) l1algos.size(); j++){

	// retrieving algo flags for each algo; this contains the following information:
	// bit 0 = if 1 the algo is downscaled
	// bit 2 = if 1 the algo is configured with positive logic (0 for veto)
	// bit 4 = if 1 the algo is used in flagging (0 for cutting) mode
	// bit 6 = if 1 the algo is enabled
	if (l1algos.at(j).GetL1AlgoFlags() & (1<<6)) // consider only enabled algos
	  tmpOrderAlgos.push_back(l1algos.at(j));
      }
      SortAlgoBlocks(tmpOrderAlgos); // sort according to ProcessID

      if (fCountersAreInitialized[i]) {
	if (tmpOrderAlgos.size() != fOrderAlgos[i].size()) {
	  cerr << user_normal() << ">> Algorithms added or taken out " << tmpOrderAlgos.size() << " " << fOrderAlgos[i].size() << endl;
	  changedConfiguration = kTRUE;
	  fCountersAreInitialized[i]= kFALSE;
	}
	else {
	  for (Int_t j=0; j<(int) tmpOrderAlgos.size(); j++) {
	    if (AlgorithmConfigurationDiffer(tmpOrderAlgos.at(j),fOrderAlgos[i].at(j))) {
	      cerr << user_normal() << ">> Algorithms new and old differ " << j << " " << fOrderAlgos[i].size() << endl;
	      fCountersAreInitialized[i]= kFALSE;
	      changedConfiguration = kTRUE;
	      break;
	    }
	  }
	}
      }
      if (changedConfiguration) cerr << user_normal() << ">> Configuration Changed for L1 for L0 trigger mask " << i << endl;

      if (!fCountersAreInitialized[i]) { // initialize the counters and the condition names for mask #i

// Fill and sort the private array of l1algos

	fOrderAlgos[i].clear();
	for (Int_t j=0; j<(int) tmpOrderAlgos.size(); j++) fOrderAlgos[i].push_back(tmpOrderAlgos.at(j)); // copy tmp into private (probably can do that better)

	fNAlgosCuttingOrFlagging[i][0] = 0; // number of consecutive algos in cut  mode starting from the first
	fNAlgosCuttingOrFlagging[i][1] = 0; // number of consecutive algos in flag mode
	Bool_t inconsistentConfig = kFALSE;
	for (Int_t j=0; j<(int) fOrderAlgos[i].size(); j++) {
	  L1AlgoBlock algoBlock = fOrderAlgos[i].at(j);

	  Int_t flaggingAlgo = 0; // default cutting
	  if (algoBlock.GetL1AlgoFlags() & (1<<4)) flaggingAlgo = 1; // flagging

	  Bool_t algoLogicTmp = algoBlock.GetL1AlgoFlags() & (1<<2);
	  fConditionName[i][j] = algoIdToTriggerLabel(algoBlock.GetL1AlgoID(),algoLogicTmp);
	  fConditionName[i][j].Append(fFlagOrCutAlgoMode[flaggingAlgo].Data());

	  if (j) {
	    L1AlgoBlock algoBlockOld = fOrderAlgos[i].at(j-1);
	    Bool_t algoOldLogicTmp = algoBlockOld.GetL1AlgoFlags() & (1<<2);
	    fConditionName[i][j].Append("Given");
	    fConditionName[i][j].Append(algoIdToTriggerLabel(algoBlockOld.GetL1AlgoID(),algoOldLogicTmp));
	  }

	  if (flaggingAlgo == 0) { //cutting
	    fNAlgosCuttingOrFlagging[i][0]++;
	    if (fNAlgosCuttingOrFlagging[i][1] != 0) cerr << user_normal() << ">> Possible inconsistent configuration: cut/flag/cut on L0 Trigger Mask " << fL1TriggerNumToMaskID[i] << endl;
	  }
	  else fNAlgosCuttingOrFlagging[i][1]++;
	}

	if (fNAlgosCuttingOrFlagging[i][0]+fNAlgosCuttingOrFlagging[i][1] != (int) fOrderAlgos[i].size()) {
	  cerr << user_normal() << ">> Inconsistent configuration in the total number of algorithms enabled: Cut "
	       << fNAlgosCuttingOrFlagging[i][0] << " Flag " << fNAlgosCuttingOrFlagging[i][1] << " Total " << fOrderAlgos[i].size()
	       << " L0 Mask " << fL1TriggerNumToMaskID[i] << endl;
	  inconsistentConfig = kTRUE;
	}
	if (inconsistentConfig) continue; // try another L0 mask

	// Zero all of the relevant counters, assign the control samples to be used

	fNumeratorRejectionOverallCut[i] = 0;
	fNumeratorRejectionOverall[i] = 0;
	for (Int_t j=0; j<(int) fOrderAlgos[i].size(); j++) {
	  fNumeratorEfficiency[i][j] = 0;   // last is for the total L1 efficiency
	  fDenominatorEfficiency[i][j] = 0; // last is for the total L1 efficiency
	  fNumeratorRejection[i][j] = 0;    // last is for the total L1 rejection
	  fDenominatorRejection[i][j] = 0;  // last is for the total L1 rejection

	  if (fConditionName[i][j].BeginsWith("UNKNOWN")) {
	    cerr << user_normal() << ">> Unknown algorithm in L0Mask = " << i << " L1Algo = " << j << endl;
	    return;
	  }
	  L1AlgoBlock algoBlock = fOrderAlgos[i].at(j);
	  controlSampleToBeUsed[i][j] = algoIdToControlSampleID(algoBlock.GetL1AlgoID(),maskID);
	  if (controlSampleToBeUsed[i][j]==-1) {
	    cerr << user_normal() << ">> Wrong Control sample retrieved - internal inconsistency -  L0Mask = " << i << " L1Algo = " << j << endl;
	    return;
	  }
	}
	fCountersAreInitialized[i] = kTRUE;
      }

// Reject for computation events with timeout flags or with all of the algos disabled

      // Retrieve L1 flags for each mask; this contains the following information:
      // bit 0 (New - introduced in 2018)= if 1 the event has a STRAW variation enabled on the specific L1 mask
      // bit 2 = if 1 this generally indicates that something went wrong (timeout) in the configuration of L1 trigger mask (this info propagates to bit 3 of global level1Word)
      // bit 4 = if 1 the mask has no L1 triggers enabled on it (this info propagates to bit 4 of global level1Word)
      // bit 6 = if 1 the mask has at least a L1 trigger used in flagging mode (this info propagates to bit 6 of global level1Word)
      Int_t disableFlag = (L1Infos.at(i).GetL1Flags() & (1<<4))>0;
      Int_t timeOutFlag = (L1Infos.at(i).GetL1Flags() & (1<<2))>0;
      //Int_t flagAlgoFlag = (L1Infos.at(i).GetL1Flags() & (1<<6))>0;
      //Int_t newStrawFlag = (L1Infos.at(i).GetL1Flags() & (1<<0))>0;
      fHisto.GetTH1("L1BadFlags")->Fill(timeOutFlag+2*disableFlag);
      if (disableFlag || timeOutFlag) continue;

// Increment denominators and numerators for single algorithm efficiencies
      for (Int_t j=0; j<(int) tmpOrderAlgos.size(); j++) {
	L1AlgoBlock algoBlock = tmpOrderAlgos.at(j);

	// Retrieve algo quality flags for each algo; this contains the following information:
	// bit 0 = if 1 the algo was successful
	// bit 2 = if 1 the data was corrupted
	// bit 4 = if 1 the data packet received at L1 was empty (no hits)
	// bit 6 = if 1 the algo was processed
	Int_t algoQuality = algoBlock.GetL1QualityFlags();
	Int_t algoNotProcessedFlag = (algoQuality & (1<<6))==0;
	Int_t algoEmptyPacketFlag  = (algoQuality & (1<<4))>0;
	Int_t algoCorruptedFlag    = (algoQuality & (1<<2))>0;
	Int_t badAlgo = algoNotProcessedFlag+2*algoEmptyPacketFlag+4*algoCorruptedFlag;
	fHisto.GetTH1("L1AlgoQualityBadFlags")->Fill(badAlgo);
	if (badAlgo) continue;  // algo was not indeed processed

	fDenominatorRejection[i][j]++;
	if (fiTags & (1<<controlSampleToBeUsed[i][j])) fDenominatorEfficiency[i][j]++;

	//Patch to retrieve correct L1 KTAG verdict for Run Number < 7969
	Bool_t algoLogic = algoBlock.GetL1AlgoFlags() & (1<<kL1KTAG);
	std::vector<UInt_t> l1data = algoBlock.GetL1DataWords();
	Bool_t firedAlgo=0;
	if((GetRunID()<7969) && (algoBlock.GetL1AlgoID()==kL1KTAG)){
	  if (algoLogic) firedAlgo=((UInt_t)l1data[0]>4);
	  else firedAlgo=!((UInt_t)l1data[0]>4);
	}
	else firedAlgo=(algoQuality & 1);
	//if((algoQuality & 1) != firedAlgo) cout << "@@@@@ Found Mismatch for Algo " << algoIdToTriggerLabel(algoBlock.GetL1AlgoID(),algoLogic) << endl;

	if (firedAlgo) { // algo was successful
	  fNumeratorRejection[i][j]++;
	  if (fiTags & (1<<controlSampleToBeUsed[i][j])) fNumeratorEfficiency[i][j]++;
	}
	else break;

	if (j == fNAlgosCuttingOrFlagging[i][0]-1) fNumeratorRejectionOverallCut[i]++;
	if (j == fNAlgosCuttingOrFlagging[i][0]+fNAlgosCuttingOrFlagging[i][1]-1) fNumeratorRejectionOverall[i]++;
      }
    }
  }

  if (fEventsForUpdate>0) {
    fGlobalFillCounter++;
    if (fGlobalFillCounter%fEventsForUpdate == 0) {
      FillPerformance();
      EvaluatePerformance();
    }
  }
}

void LevelOnePerformance::dumpAllL1Info(L1MaskBlock maskBlock){
  cout << user_normal() << "Dump of L1MaskBlock for L0MaskID " << (Int_t) maskBlock.GetL0MaskID() << endl;
  Int_t disableFlag = (maskBlock.GetL1Flags() & (1<<4))>0;
  Int_t timeOutFlag = (maskBlock.GetL1Flags() & (1<<2))>0;
  cout << user_normal() << "DisableFlag " << disableFlag << " timeOutFlag " << timeOutFlag << endl;
  std::vector<L1AlgoBlock> algos = maskBlock.GetL1Algorithms();
  cout << user_normal() << "Enabled algos " << (Int_t)maskBlock.GetL1NEnabledAlgos() << " check: " << algos.size() << endl;
  for (int i=0; i<(Int_t) algos.size(); i++) {
    cout << user_normal() << "Algo #" << i << " ID " << (Int_t)algos.at(i).GetL1AlgoID() << " order of processing " << (Int_t) algos.at(i).GetL1ProcessID() << endl;
    int algoQuality =  algos.at(i).GetL1QualityFlags();
    Int_t algoNotProcessedFlag = (algoQuality & (1<<6))==0;
    Int_t algoEmptyPacketFlag  = (algoQuality & (1<<4))>0;
    Int_t algoCorruptedFlag    = (algoQuality & (1<<2))>0;
    Int_t badAlgo = algoNotProcessedFlag+2*algoEmptyPacketFlag+4*algoCorruptedFlag;
    Int_t firedAlgo = (algoQuality & 1)>0;
    cout << user_normal() << "BadAlgoFlag " << badAlgo << " firedAlgoFlag= " << firedAlgo << endl;
  }
}

Bool_t LevelOnePerformance::AlgorithmConfigurationDiffer(L1AlgoBlock algoNew, L1AlgoBlock algoOld){
  bool algoLogicNew = algoNew.GetL1AlgoFlags() & (1<<2);
  bool algoLogicOld = algoOld.GetL1AlgoFlags() & (1<<2);
  if ( !(algoLogicNew && algoLogicOld) && (algoLogicNew || algoLogicOld)) return kTRUE;  // change in the logic flag
  if (algoNew.GetL1AlgoID() != algoOld.GetL1AlgoID()) return kTRUE;  // change in the ID --> order changed
  if (algoNew.GetL1ProcessID() != algoOld.GetL1ProcessID()) return kTRUE;  // change in the processID --> order changed
  if (algoNew.GetL1TimeWindow() != algoOld.GetL1TimeWindow()) return kTRUE;  // change in the time window
  return kFALSE;
}

string LevelOnePerformance::algoIdToTriggerLabel(uint algoID, uint algoLogic) {

  string triggerLabel = "";
  if(!algoLogic) triggerLabel += "!";

  switch (algoID) {
  case 0:
    triggerLabel += "CHOD";
    break;
  case 1:
    triggerLabel += "RICH";
    break;
  case 2:
    triggerLabel += "KTAG";
    break;
  case 3:
    triggerLabel += "LAV";
    break;
  case 4:
    triggerLabel += "IRCSAC";
    break;
  case 5:
    triggerLabel += "STRAW";
    break;
  case 6:
    triggerLabel += "MUV3";
    break;
  case 7:
    triggerLabel += "NEWCHOD";
    break;
  default:
    triggerLabel = "UNKNOWN ALGO ID!";
  }
  return triggerLabel;
}

int LevelOnePerformance::algoIdToControlSampleID(uint algoID, int maskID) {
//
// return controlSampleID from 1 to .. (0 refers to all data)
//
  if (maskID == 1) {
    switch (algoID) {
    case 2:
      //return 4; // pp0 gg on LKR mimic for algo "KTAG"
      return 5; // pipi0, gg on LAV mimics for algo "KTAG";
      //return 6; // Km2 (track momentum 15-35 GeV/c) for algo "KTAG"
      //return 8; // use K->3pi for algo "KTAG"
    case 3:
      return 4; // pp0 gg on LKR mimics for algo "LAV";
    case 5:
      //return 4; // pipi0, gg on LKR mimics for algo "STRAW";
      return 6; // Km2 (track momentum 15-35 GeV/c) for algo "STRAW"
    default:
      return -1; // "UNKNOWN ALGO ID!"; <--- 0 means all data!!
    }
  }
  else{
    switch (algoID) {
    case 0:
      return 5; // pp0 gg on LAV mimics for algo "CHOD"
    case 1:
      return 1; // single beam-track mimics for algo "RICH";
    case 2:
      return 4; // pp0 gg on LKR mimic for algo "KTAG"
    case 3:
      return 4; // pp0 gg on LKR mimics for algo "LAV";
    case 4:
      return 5; // pipi0, gg on LAV mimics for algo "IRCSAC";
    case 5:
      if (maskID == 0 || maskID == 10) return 4; // pipi0, gg on LKR mimics for algo "STRAW";
      else return 8; // use K->3pi for algo "STRAW exotics"
    case 6:
      return 4; // pipi0, gg on LKR mimics for algo "MUV3";
    case 7:
      return 5; // pp0 gg on LAV mimics for algo "NEWCHOD";
    default:
      return -1; // "UNKNOWN ALGO ID!"; <--- 0 means all data!!
    }
  }
}


void LevelOnePerformance::pp0Analysis(TRecoLKrEvent* LKrEvent, TRecoLAVEvent* LAVEvent, std::vector<DownstreamTrack> Tracks, Double_t refTime){

  fPi0lavlkrFlag = 0;
  fPi0lkrFlag = 0;
  fPi0lavFlag = 0;
  fKM2Flag = 0;
  fTrkClusInTime = -1;
  fPosAtLKr.SetXYZT(0,0,0,0);

  if (fPiPi0Track < 0 && fKM2Track < 0) return;
  if (fPiPi0Track >= 0 && fKM2Track >= 0) return;
  Int_t itrk = fPiPi0Track;
  if (fKM2Track >=0) itrk = fKM2Track;
  TRecoSpectrometerCandidate* spectrometerCandi = Tracks[itrk].GetSpectrometerCandidate();
  Double_t Ptrack = Tracks[itrk].GetMomentum(); // spectrometer calibration included

  /////////////////////////////////////////
  // Vertex: track wrt the beam axis
  TVector3 PosAtVtx(Tracks[itrk].GetBeamAxisVertex().X(),Tracks[itrk].GetBeamAxisVertex().Y(),Tracks[itrk].GetBeamAxisVertex().Z());

  TLorentzVector pPiPlus;
  pPiPlus.SetVectM(spectrometerCandi->GetThreeMomentumBeforeMagnet(), MPI);
  TLorentzVector pPi0 = fpKNominal - pPiPlus;

  if (!GeometricAcceptance::GetInstance()->InAcceptance(spectrometerCandi, kLKr)) return;

  // select LKr clusters in time away from the pion track
  Int_t NLKrCandidates = LKrEvent->GetNCandidates();
  Int_t NLKrClusInTime = 0;
  Int_t iLKrClusInTime[10];
  Int_t NLKrClusInTimeNoCut = 0;
  Int_t iLKrClusInTimeNoCut[10];
  Double_t energyTrkClusInTime = 0;
  for (Int_t i = 0; i<NLKrCandidates; i++) {
    TRecoLKrCandidate* LKrCandi = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(i));
    Double_t energy = LKrCandi->GetClusterEnergy();
    Double_t dt = LKrCandi->GetTime() - refTime;
    Double_t dx = LKrCandi->GetClusterX() - Tracks[itrk].xAt(GeometricAcceptance::GetInstance()->GetZLKr());
    Double_t dy = LKrCandi->GetClusterY() - Tracks[itrk].yAt(GeometricAcceptance::GetInstance()->GetZLKr());
    Double_t distance = sqrt(dx*dx+dy*dy);
    fHisto.GetTH2("LKrdtVsdrPi")->Fill(distance,dt);

    if (TMath::Abs(dt) < 10 && distance < 100.) {
      fTrkClusInTime = i;
      energyTrkClusInTime = energy;
      continue;
    }

    if (NLKrClusInTimeNoCut < 10) iLKrClusInTimeNoCut[NLKrClusInTimeNoCut] = i;
    NLKrClusInTimeNoCut++;
    if (energy < 4000.) continue;
    if (NLKrClusInTime < 10) iLKrClusInTime[NLKrClusInTime] = i;
    NLKrClusInTime++;
  }

  // count number of LAV clusters in time

  Int_t nLAVClus = LAVEvent->GetNCandidates();
  Int_t nLAVClusInTime = 0;
  Int_t iLAVClusInTime[100];
  TClonesArray& clusArray = (* (LAVEvent->GetCandidates()));
  for (Int_t i=0; i< nLAVClus; i++){
    TRecoLAVCandidate* lavCandi = static_cast<TRecoLAVCandidate*>(clusArray[i]);
    if (lavCandi->GetAlgorithm() != 1) continue;

    Double_t dt = lavCandi->GetTime()  - refTime;
    fHisto.GetTH1("LAVdt")->Fill(dt);
    if (TMath::Abs(dt) > 10) continue;

    if (nLAVClusInTime < 100) {
      iLAVClusInTime[nLAVClusInTime] = i;
      nLAVClusInTime++;
    }
  }

  // analysis of gg kinematics
  Int_t ggKinematicsLAVLKr = 0;
  Int_t ggKinematicsLKrLKr = 0;
  if (fPiPi0Track >=0) fHisto.GetTH2("NLKrVsNLAV")->Fill(nLAVClusInTime,NLKrClusInTime);

  if (fPiPi0Track >=0 && NLKrClusInTime == 1 && nLAVClusInTime == 1) {
    TRecoLKrCandidate* LKrClus = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(iLKrClusInTime[0]));
    Double_t energy = LKrClus->GetClusterEnergy();
    TVector3 gammaMom(LKrClus->GetClusterX(),LKrClus->GetClusterY(),GeometricAcceptance::GetInstance()->GetZLKr());
    gammaMom -= PosAtVtx;
    gammaMom *= (energy/gammaMom.Mag());
    TVector3 otherGammaMom = pPi0.Vect() - gammaMom;

    TRecoLAVCandidate* LAVClus = static_cast<TRecoLAVCandidate*>(LAVEvent->GetCandidate(iLAVClusInTime[0]));
    double dphi = LAVClus->GetPosition().Phi() - otherGammaMom.Phi();
    if (dphi<-TMath::Pi()) dphi += TMath::Pi()*2;
    else if (dphi>TMath::Pi()) dphi -= TMath::Pi()*2;

    double dthe = LAVClus->GetPosition().Perp()/(LAVClus->GetPosition().Z()-PosAtVtx.Z()) - otherGammaMom.Perp()/otherGammaMom.Z();

    fHisto.GetTH2("LAVLKrggDTheVsDPhi")->Fill(dphi,dthe);
    if (fabs(dphi) < 0.4 && fabs(dthe) < 0.125) ggKinematicsLAVLKr = 1; //fPi0lavlkrFlag = 1;
  }

  Double_t mass2 = -99999;
  Double_t zVertex = -99999;

  if (fPiPi0Track >=0 && NLKrClusInTime == 2) { // && nLAVClusInTime == 0

    TRecoLKrCandidate* LKrClus[2];
    LKrClus[0] = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(iLKrClusInTime[0]));
    LKrClus[1] = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(iLKrClusInTime[1]));
    Double_t energy[2];
    Double_t positions[2][2];
    for (int i=0; i<2; i++) {
      energy[i] =  LKrClus[i]->GetClusterEnergy();
      positions[i][0] = LKrClus[i]->GetClusterX();
      positions[i][1] = LKrClus[i]->GetClusterY();
    }
    Double_t mutualDistance = TMath::Sqrt(
					  (positions[0][0]-positions[1][0])*(positions[0][0]-positions[1][0]) +
					  (positions[0][1]-positions[1][1])*(positions[0][1]-positions[1][1]) );

    zVertex = GeometricAcceptance::GetInstance()->GetZLKr() - mutualDistance*TMath::Sqrt(energy[0]*energy[1])/NA62Constants::MPI0;
    Double_t xVertex = fpKNominal.X()/fpKNominal.Z()*(zVertex - GeometricAcceptance::GetInstance()->GetZGTK3());
    Double_t yVertex = fpKNominal.Y()/fpKNominal.Z()*(zVertex - GeometricAcceptance::GetInstance()->GetZGTK3());
    TLorentzVector vertex(xVertex,yVertex,zVertex,0);

    TVector3 rGamma[2];
    TLorentzVector pGamma[2];

    for (Int_t ig=0;ig<2;ig++) {
      rGamma[ig].SetXYZ(positions[ig][0],positions[ig][1],GeometricAcceptance::GetInstance()->GetZLKr());
      rGamma[ig] = rGamma[ig] - vertex.Vect();
      pGamma[ig].SetVect(rGamma[ig]*(energy[ig]/rGamma[ig].Mag()));
      pGamma[ig].SetE(energy[ig]);
    }
    TLorentzVector pPiZero = pGamma[0]+pGamma[1];
    TLorentzVector pPiPlusFromPi0 = fpKNominal - pPiZero;

    mass2 = pPiPlusFromPi0.Mag2();
    fHisto.GetTH2("SquareMissingMassVsZvertex")->Fill(zVertex,mass2);
    if (zVertex>115000. && zVertex < 165000.) {
      if (mass2 < 25000. && mass2 > 10000.) ggKinematicsLKrLKr = 1;
    }
  }

  // Assign event identification flag

  if (NLKrClusInTime == 1 && nLAVClusInTime == 1 && ggKinematicsLAVLKr == 1) {
    flkrgammas[0] = iLKrClusInTimeNoCut[0];
    flavgammas[0] = iLAVClusInTime[0];
    fHisto.GetTH1("MomentumForLAVLKr11")->Fill(Ptrack);
    if (Ptrack<35000. && fPiPi0Track >= 0) fPi0lavlkrFlag = 1;
  }
  else if (NLKrClusInTime == 2 && nLAVClusInTime == 0 && ggKinematicsLKrLKr == 1) {
    flkrgammas[0] = iLKrClusInTimeNoCut[0];
    flkrgammas[1] = iLKrClusInTimeNoCut[1];
    fHisto.GetTH1("MomentumForLAVLKr02")->Fill(Ptrack);
    if (Ptrack<35000. && fPiPi0Track >= 0) fPi0lkrFlag = 1;
  }
  else if (NLKrClusInTime == 0 && nLAVClusInTime == 2) {
    flavgammas[0]  = iLAVClusInTime[0];
    flavgammas[1]  = iLAVClusInTime[1];
    fHisto.GetTH1("MomentumForLAVLKr20")->Fill(Ptrack);
    if (Ptrack<65000. && fPiPi0Track >= 0) fPi0lavFlag = 1;
  }
  else if (NLKrClusInTime == 0 && nLAVClusInTime == 0 && energyTrkClusInTime > 0 && energyTrkClusInTime < 1000.) {
    //if (fPiPi0Track < 0) fKM2Flag = 1;
    // modification 05/08/2018
    if (Ptrack<35000. && fPiPi0Track < 0) fKM2Flag = 1;   //forcing Ptrk < 35GeV/c to use Km2 as control sample for PNN
  }

  // plot energy correlations

  for (Int_t j=0; j<fNTriggers; j++) {
    if( fLevelZeroWord & (int) TMath::Power(2,j) ) { //fLevelZeroWord  == (int) TMath::Power(2,j+1)-1) {

      fHisto.GetTH2(Form("LAVvsLKrNClusters_Type%d",j))->Fill(NLKrClusInTimeNoCut,nLAVClusInTime);

      if (fPi0lavlkrFlag == 1) {
	fHisto.GetTH2(Form("LAVvsLKrEClusters_11_Type%d",j))->Fill(
								   static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(flkrgammas[0]))->GetClusterEnergy(),
								   TMath::Log(static_cast<TRecoLAVCandidate*>(LAVEvent->GetCandidate(flavgammas[0]))->GetEnergy())
								   );
      }
      else if (fPi0lkrFlag == 1) {
	fHisto.GetTH2(Form("LKrClusters_20_Type%d",j))->Fill(
							     static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(flkrgammas[0]))->GetClusterEnergy(),
							     static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(flkrgammas[1]))->GetClusterEnergy()
							     );
      }
      else if (fPi0lavFlag == 1) {
	fHisto.GetTH2(Form("LAVClusters_02_Type%d",j))->Fill(
							    TMath::Log(static_cast<TRecoLAVCandidate*>(LAVEvent->GetCandidate(flavgammas[0]))->GetEnergy()),
							    TMath::Log(static_cast<TRecoLAVCandidate*>(LAVEvent->GetCandidate(flavgammas[1]))->GetEnergy())
							     );
      }
    }
  }

  // store candidates and recohits belonging to the pipi0 gammas, to be vetoed

  Int_t nLKrRecoHits = LKrEvent->GetNHits();
  TClonesArray& hitLKrArray = (* (LKrEvent->GetHits()));

  for (Int_t j=0; j<nLKrRecoHits; j++) {
    TRecoLKrHit* hit = static_cast<TRecoLKrHit*>(hitLKrArray[j]);

    for (Int_t i = 0; i<2; i++) {
      if (flkrgammas[i] == -1) continue;
      TRecoLKrCandidate* LKrCandi = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(flkrgammas[i]));
      Double_t dx = hit->GetPosition().X()-LKrCandi->GetClusterX();
      Double_t dy = hit->GetPosition().Y()-LKrCandi->GetClusterY();
      Double_t dr = TMath::Sqrt(dx*dx+dy*dy);
      Double_t dt = hit->GetTime()-LKrCandi->GetTime();
      fHisto.GetTH2("LKrdtVsdr")->Fill(dr,dt);
      if (j<1000 && dr < 150. && fabs(dt) < 25.) {
	flkrcellgammas[j] = 1;
      }
    }

    if (flkrcellgammas[j]==1) continue;

    Double_t dx = hit->GetPosition().X() - Tracks[itrk].xAt(GeometricAcceptance::GetInstance()->GetZLKr());
    Double_t dy = hit->GetPosition().Y() - Tracks[itrk].yAt(GeometricAcceptance::GetInstance()->GetZLKr());
    Double_t dr = TMath::Sqrt(dx*dx+dy*dy);
    Double_t dt = hit->GetTime() - refTime;
    fHisto.GetTH2("LKrdtVsdrPiCell")->Fill(dr,dt);
    if (j<1000 && dr < 150. && fabs(dt) < 25.) flkrcellgammas[j] = 2;
  }

  for (Int_t i = 0; i<2; i++) {
    if (flavgammas[i] == -1) continue;
    TRecoLAVCandidate* LAVCandi = static_cast<TRecoLAVCandidate*>(LAVEvent->GetCandidate(flavgammas[i]));
    Int_t* hitIndexArrayi = LAVCandi->GetHitsIndexes();
    for (Int_t j=0; j<LAVCandi->GetNHits(); j++) {
      if (hitIndexArrayi[j]<0 || hitIndexArrayi[j]>=1000) continue;
      if (hitIndexArrayi[j]<1000) flavcellgammas[hitIndexArrayi[j]] = 1;
    }
  }
  return;
}

void LevelOnePerformance::trackingAnalysis(std::vector<DownstreamTrack> Tracks) {

  for (UInt_t i = 0; i<Tracks.size(); i++){

    TRecoSpectrometerCandidate* spectrometerCandi = Tracks[i].GetSpectrometerCandidate();
    Int_t    Q            = Tracks[i].GetCharge();
    Double_t Ptrack       = Tracks[i].GetMomentum(); // spectrometer calibration included
    Double_t Chi2track    = Tracks[i].GetChi2();

    if (!GeometricAcceptance::GetInstance()->InAcceptance(spectrometerCandi, kCHOD)) continue;
    //if (!GeometricAcceptance::GetInstance()->InAcceptance(spectrometerCandi, kSpectrometer, 0)) return;
    //if (!GeometricAcceptance::GetInstance()->InAcceptance(spectrometerCandi, kSpectrometer, 1)) return;
    //if (!GeometricAcceptance::GetInstance()->InAcceptance(spectrometerCandi, kSpectrometer, 2)) return;
    //if (!GeometricAcceptance::GetInstance()->InAcceptance(spectrometerCandi, kSpectrometer, 3)) return;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(spectrometerCandi, kLKr)) continue;

    /////////////////////////////////////////
    // Zvertex & CDA: track wrt the beam axis

    Double_t cda  = Tracks[i].GetBeamAxisCDA();
    Double_t Zvtx = Tracks[i].GetBeamAxisVertex().Z();

    TLorentzVector pPiPlus;
    pPiPlus.SetVectM(spectrometerCandi->GetThreeMomentumBeforeMagnet(), MPI);
    TLorentzVector pMuPlus;
    pMuPlus.SetVectM(spectrometerCandi->GetThreeMomentumBeforeMagnet(), MMU);
    TLorentzVector pPi0 = fpKNominal - pPiPlus;
    TLorentzVector pNu = fpKNominal - pMuPlus;

    // add cut on chi^2 < 3 and ask 4chambers or 3chambers in the 013 configuration

    bool qualityCondition = (Chi2track >= 0) && (Chi2track < 10) && (spectrometerCandi->GetNChambers() == 4);

    if (!qualityCondition) continue;

    fHisto.GetTH2("MomentumVsZ")->Fill(Zvtx,Ptrack);
    fHisto.GetTH2("MomentumVsPCA")->Fill(cda,Ptrack);

    Bool_t fvCondition = (Zvtx > 105000.) && (Zvtx < 165000.) && (Ptrack > 15000.) && (Ptrack < 65000.) && (cda < 20.);

    if (fvCondition) {
      fNSpectrometerCandidateTracks++;
      if (Q>0) {
	fSpectrometerCondition = 1;
	Double_t mMissMu = pNu.Mag2();
	fHisto.GetTH2("SquareMissingMassMuVsMomentum")->Fill(Ptrack,mMissMu);
	if (mMissMu < 1000.0 && mMissMu > -10000. && Ptrack > 15000.) fKM2Track = i;

	fHisto.GetTH2("mmissVsMomentum")->Fill(Ptrack,pPi0.Mag2());
	if (pPi0.Mag2() < 24000. && pPi0.Mag2() > 5000. && Ptrack > 15000. && fPiPi0Track==-1) fPiPi0Track = i;
      }
    }

    Bool_t beamTrackCondition = (Zvtx < 110000.) && (cda < 50.) && (Ptrack > 72000.) && (Ptrack < 77000.);
    if (beamTrackCondition) fNSpectrometerBeamTracks++;
  }

  if (fSpectrometerCondition == 1 && fNSpectrometerCandidateTracks > 1) fSpectrometerCondition = 2;
  return;
}

void LevelOnePerformance::PostProcess(){}

void LevelOnePerformance::EndOfRunUser(){}

void LevelOnePerformance::EndOfJobUser(){
  if (fAllSteps) { // both steps to be done
    EvaluatePerformance();
    if (fBuildPDFReport) BuildPDFReport();
    SaveAllPlots();
    return;
  }
  else { // a single step to be done
    if (fReadingData) { // Data mode: save output
      SaveAllPlots();
      return;
    }
    else {
      if (!fHistogramInputWasRetrieved) {
	cout << user_normal() << "Problem in retrieving input histograms - exiting " << endl;
	return;
      }
      EvaluatePerformance();
      if (fBuildPDFReport) BuildPDFReport();
      SaveAllPlots();
    }
  }
}

void LevelOnePerformance::ExportPlot(){
  SaveAllPlots();
}

void LevelOnePerformance::DrawPlot(){}
