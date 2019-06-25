#ifndef LEVELONEPERFORMANCE_HH
#define LEVELONEPERFORMANCE_HH

#define NEWUNITSOFMEASURE_HH 1

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include "Persistency.hh"
#include <TCanvas.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

#define MAXEDGES 1000
#define NUMBER_OF_L0_MASKS 16
#define NUMBER_OF_L1_ALGOS 10

using namespace std;

struct myAlgoBlockComparison{
  Bool_t operator() ( L1AlgoBlock algo1, L1AlgoBlock algo2) {return ((int) algo1.GetL1ProcessID() < (int) algo2.GetL1ProcessID());};
};

class LevelOnePerformance : public Analyzer {

public:
  explicit LevelOnePerformance(BaseAnalysis *ba);
  ~LevelOnePerformance();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void PostProcess();
  void ExportPlot();
  void DrawPlot();

private:

// Configuration variables

  Int_t fMaxNBursts ;
  Int_t fBurstChunk ;
  Int_t fVerboseFlag;
  Int_t fBuildPDFReport;
  Int_t fAllSteps;
  Int_t fNTriggers  ; // number of trigger L0 masks to be considered
  Int_t fEventsForUpdate; // number of events for update of efficiency/rejection plots  

// Running modes: reading data/reading its own output

  Bool_t fReadingData;
  Bool_t fHistogramInputWasRetrieved;
  TString fFlagOrCutAlgoMode[2]; //"Cutting", "Flagging"
  Bool_t fCountersAreInitialized[NUMBER_OF_L0_MASKS];
  Int_t fNAlgosCuttingOrFlagging[NUMBER_OF_L0_MASKS][2]; // for each L0 mask, consecutiveAlgos in cutting/flagging mode [0]/[1]
  Int_t fL1TriggerNumToMaskID[NUMBER_OF_L0_MASKS];

// Global fill counter for canvas update in continuous mode

  Int_t fGlobalFillCounter;

// Nominal beam parameters

  Double_t fThetaTrim5;
  Double_t fpKNominalMod;
  TLorentzVector fpKNominal;

// Event classification tags and corresponding histogram labels

  TString* fTagsEvent;
  const char **fControlSampleLabels;
  Bool_t fHistogramInputWasRetrieve;

// Histogram to retrieve the correct L0 Mask Index for PDF report
  TH2F* fL1TriggerInfoVsL0MaskIndex;

// Histograms filled on a burst by burst basis

  TH2F* fhE[NUMBER_OF_L0_MASKS][2]; // Efficiency denominator ([0]) and numerator([1]) for each L0 mask, for each enabled algorithm + all algos in cutting mode + all algos
  TH2F* fhR[NUMBER_OF_L0_MASKS][2]; // Rejection denominator([0]) and numerator([1]) for each L0 mask, for each enabled algorithm + all algos in cutting mode + all algos
  TH1F* fhNInCutOrFlag[NUMBER_OF_L0_MASKS][2]; // Number of algorithms in cutting/flagging mode ([0/1]) for each L0 mask vs BurstID

  TH1F* fhEff[NUMBER_OF_L0_MASKS][NUMBER_OF_L1_ALGOS+2];
  TH1F* fhRej[NUMBER_OF_L0_MASKS][NUMBER_OF_L1_ALGOS+2];
  Int_t fMaxBurstBin; // maximum bin occupied in the stability histograms

  void dumpAllL1Info(L1MaskBlock);
  void FillPerformance();     // Fill numerators and denominators adding on top of previous counters (if present)
  void EvaluatePerformance(); // Evaluate performance
  Int_t CalculateEfficiency(Int_t,Int_t,Double_t*, Double_t*);
  void BuildPDFReport();      // build report in output pdf

  string algoIdToTriggerLabel(uint algoID, uint algoLogic); // return algorithm label
  int algoIdToControlSampleID(uint algoID, int maskID); // return algorithm name given the algoID and the L0MaskID

  TString fConditionName[NUMBER_OF_L0_MASKS][NUMBER_OF_L1_ALGOS];

  Int_t controlSampleToBeUsed[NUMBER_OF_L0_MASKS][NUMBER_OF_L1_ALGOS]; // index of the control sample mimicking the signal for efficiency purposes, the last is given by the product
  Int_t fNumeratorEfficiency[NUMBER_OF_L0_MASKS][NUMBER_OF_L1_ALGOS];   
  Int_t fDenominatorEfficiency[NUMBER_OF_L0_MASKS][NUMBER_OF_L1_ALGOS]; 
  Int_t fNumeratorRejection[NUMBER_OF_L0_MASKS][NUMBER_OF_L1_ALGOS];    
  Int_t fDenominatorRejection[NUMBER_OF_L0_MASKS][NUMBER_OF_L1_ALGOS];  
  Int_t fNumeratorRejectionOverallCut[NUMBER_OF_L0_MASKS];    // for the total L1 rejection from cutting algos
  Int_t fNumeratorRejectionOverall[NUMBER_OF_L0_MASKS];    // for the total L1 rejection from cutting+flagging algos

  Int_t fOldInCuttingOrFlagging[NUMBER_OF_L0_MASKS][2];  // Number of Algorithm in cutting in the previous burst analyzed

  void Publish(); ///< Deprecated


  // classification methods

  void trackingAnalysis(std::vector<DownstreamTrack>); // beam-tracks, multitracks, pipi0 and km2 tracks
  void pp0Analysis(TRecoLKrEvent*, TRecoLAVEvent*, std::vector<DownstreamTrack>, Double_t); // cluster finding for pipi0
  Int_t fiTags;

  // Future L1 algorithms to be tested

  Bool_t MUV3LevelOne(TRecoMUV3Event*, Double_t refTime);  

// event classification
  Double_t fEventTime;
  Int_t fNSpectrometerCandidateTracks;
  Int_t fSpectrometerCondition;
  Int_t fNSpectrometerBeamTracks;
  Int_t fPiPi0Track; // = index of piPlus track giving a pi0 missing mass
  Int_t fPi0lavFlag; // = 1 if 2 gamma on LAV and p<65 GeV
  Int_t fPi0lkrFlag; // = 1 if 2 gamma on LKr and p<35 GeV
  Int_t fPi0lavlkrFlag; // = 1 if 1 gamma on LAV and one gamma on LKr and p<35 GeV
  Int_t fKM2Flag;    // = 1 if track is kinematically compatible with Km2 decay (low-momentum region only)
  Int_t fKM2Track;   // index of selected straw candidate as KM2
  TLorentzVector fPosAtLKr; // position of track at LKr
  Int_t fTrkClusInTime; // LKr candidate associated to track    
  Int_t flkrgammas[2]; // lkr gamma candidates
  Int_t flavgammas[2]; // lav gamma candidates
  Int_t flkrcellgammas[1000]; // lkr gamma recohits 
  Int_t flavcellgammas[1000]; // lav gamma recohits

  // L0 mask configuration

  string fTriggerTYPE;
  Int_t fNCHODMaxSlabs;

  Int_t fNCHODSlabs;
  Double_t fCHODCentralPos[2];
  Int_t fNCHODSlabsPos[2];

  Int_t fNHitsPerStation[12];
  Int_t fNTotalLAVHits;
  Int_t fLevelZeroWord;
  Int_t fNTotalMUV3Pads;

  Int_t fNEventTags;
  TVector3 fPosLKr;
  Double_t fDistaLKrMin;

protected:
  Bool_t AlgorithmConfigurationDiffer(L1AlgoBlock, L1AlgoBlock);

  typedef std::vector<L1AlgoBlock> AlgoBlockCollection;
  AlgoBlockCollection fOrderAlgos[NUMBER_OF_L0_MASKS];

  myAlgoBlockComparison  AlgoBlockComparison;
  void SortAlgoBlocks(AlgoBlockCollection collection){std::sort(collection.begin(), collection.end(), AlgoBlockComparison);}

};
#endif
