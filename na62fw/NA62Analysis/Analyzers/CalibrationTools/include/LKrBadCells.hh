// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-02-05
//
// ---------------------------------------------------------

#ifndef LKRBADCELLS_HH
#define LKRBADCELLS_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
//#include <TCanvas.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

struct LKrCell{
  UInt_t x;
  UInt_t y;
};

class LKrBadCells : public NA62Analysis::Analyzer{
  public:
    explicit LKrBadCells(NA62Analysis::Core::BaseAnalysis *ba);
    ~LKrBadCells();
    void InitHist();
    void InitOutput();
    void DefineMCSimple(){};
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(int iEvent);
    void PostProcess();
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();

    std::vector<LKrCell> FindHotCellsFromHitMap(TH2F*);
    std::vector<LKrCell> FindHotCellsFromQualityWarnings(TH2F*);
    std::vector<LKrCell> FindHotCellsFromPedestals(TH2F*);
    std::vector<LKrCell> FindDeadCellsFromHitMap(TH2F*,Bool_t AllDeadCells=false);
    Int_t FindDeadClusters(std::vector<LKrCell> &DeadCells, Int_t iDeadCell);
    Int_t FindMaxNCellsInDeadCluster(Bool_t IgnorePeripheral);
    void PrintBadCellsFile();
    void GeneratePDFReport();

    enum LKrBadCellsMaskBits{
      kDeadHitMapBit=0,
      kHotHitMapBit=1,
      kHotQualityWarningsBit=2,
      kHotPedestalsBit=3
    };

  protected:

    Double_t             fHitMapScaleFactor;

    Int_t                fNProcessedBursts;
    Int_t                fNProcessedEvents;
    Int_t                fBadCellsMask;
    std::vector<LKrCell> fHotCellsFromHitMap;
    std::vector<LKrCell> fHotCellsFromQualityWarnings;
    std::vector<LKrCell> fHotCellsFromPedestals;
    std::vector<LKrCell> fDeadCellsFromHitMap;
    std::vector<LKrCell> fDeadCellsFromHitMapInTotal;  // includes also known dead cells
    Int_t                fMaxNCellsInDeadCluster;
    Int_t                fMaxNCellsInDeadClusterInnerCircle;  // Check N innermost circles of cells (for dead cell detection)
    Int_t                fInnerCircleRadiusForDeadCells;  // Radius in units of number of cells (for dead cell detection)
    Double_t             fNSigmaForBadCellSearch;
    Double_t             fPedLowerLimit;
    Double_t             fPedUpperLimit;

    Int_t                fGenerateOutputPDF;
    TString              fOutPDFFileName;
    TString              fOutTxtFileName;

    TH1F*                fHProcessingInfo;
    TH2F*                fHHitMapEnergyAboveThr;
    TH2F*                fHNQualityWarningsXY;
    TH2F*                fHPedestalsXY;
    TH2F*                fHHitMapEnergyAboveThrPerBurst;
    TH2F*                fHNQualityWarningsXYPerBurst;
    TH2F*                fHPedestalsXYPerBurst;
    TH2F*                fHHitMapEnergyAboveThrPreviousBurst;
    TH2F*                fHNQualityWarningsXYPreviousBurst;
    TH2F*                fHPedestalsXYPreviousBurst;
    TH1F*                fHPedestals; //obtained from fHPedestalsXY

};
#endif
