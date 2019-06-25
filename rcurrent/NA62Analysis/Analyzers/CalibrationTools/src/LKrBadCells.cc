// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-02-05
//
// ---------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "LKrBadCells.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include <TStyle.h>
#include <TEllipse.h>
#include <TLine.h>
#include "NA62ConditionsService.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class LKrBadCells
/// \Brief
/// Looks for LKr bad cells and writes them in LKr-badchannels.dat
/// \EndBrief
///
/// \Detailed
/// Looks for LKr cells which have (depending on how the parameter BadCellsMask is set):
/// 1) a pedestal more than NSigma away from the mean pedestal value
/// 2) an anomalous rate for energy deposition above a certain threshold (default in the reconstruction: 250 MeV)
/// and writes them in LKr-BadCells.dat
/// The input for bad cells evaluation are 2-dimensional histograms (HitMap, NQualityWarnings and Pedestals) for each cell (x:y).
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

LKrBadCells::LKrBadCells(Core::BaseAnalysis *ba) : Analyzer(ba, "LKrBadCells"){

  fHotCellsFromHitMap.clear();
  fHotCellsFromQualityWarnings.clear();
  fHotCellsFromPedestals.clear();
  fPedLowerLimit     = 300.;
  fPedUpperLimit     = 500.;
  fGenerateOutputPDF = true;
  fOutPDFFileName    = "./LKr-BadCells.pdf";
  fOutTxtFileName    = "./LKr-BadCells.dat";

  AddParam("BadCellsMask",           &fBadCellsMask, 7);             // BadCellsMask (bit 0: dead, bit 1: hot(hitmap), bit 2: hot(quality warnings), bit 3: hot(pedestal))
  AddParam("HitMapScaleFactor",      &fHitMapScaleFactor, 10.);      // ScaleFactor for defining HitMap hot cells
  AddParam("NSigmaForBadCellSearch", &fNSigmaForBadCellSearch, 10.); // NSigma for defining the good/bad cell range
  AddParam("InnerCircleRadiusForDeadCells", &fInnerCircleRadiusForDeadCells, 56); // Radius in units of number of cells (for dead cell detection)

  fNProcessedBursts=0;
  fNProcessedEvents=0;

  fHProcessingInfo = nullptr;
  fHHitMapEnergyAboveThr = nullptr;
  fHNQualityWarningsXY = nullptr;
  fHPedestalsXY = nullptr;
  fHHitMapEnergyAboveThrPerBurst = nullptr;
  fHNQualityWarningsXYPerBurst = nullptr;
  fHPedestalsXYPerBurst = nullptr;
  fHHitMapEnergyAboveThrPreviousBurst = nullptr;
  fHNQualityWarningsXYPreviousBurst   = nullptr;
  fHPedestalsXYPreviousBurst          = nullptr;
  fHPedestals = nullptr;
}

void LKrBadCells::InitOutput(){
  RegisterOutput("BadCellsMask",                &fBadCellsMask               );
  RegisterOutput("HotCellsFromHitMap",          &fHotCellsFromHitMap         );
  RegisterOutput("HotCellsFromQualityWarnings", &fHotCellsFromQualityWarnings);
  RegisterOutput("HotCellsFromPedestals",       &fHotCellsFromPedestals      );
  RegisterOutput("DeadCellsFromHitMap",         &fDeadCellsFromHitMap        );
  RegisterOutput("DeadCellsFromHitMapInTotal",  &fDeadCellsFromHitMapInTotal );
  RegisterOutput("MaxNCellsInDeadCluster",      &fMaxNCellsInDeadCluster);
  RegisterOutput("MaxNCellsInDeadClusterInnerCircle", &fMaxNCellsInDeadClusterInnerCircle);
}

void LKrBadCells::InitHist(){

  // Looking for histos in LKrMonitor dir (for Step0)
  fHHitMapEnergyAboveThr = static_cast<TH2F*>(RequestHistogram("LKrMonitor", "HitMapEnergyAboveThr", true)); // accumulated
  fHNQualityWarningsXY   = static_cast<TH2F*>(RequestHistogram("LKrMonitor", "NQualityWarningsXY",   true)); // accumulated
  fHPedestalsXY          = static_cast<TH2F*>(RequestHistogram("LKrMonitor", "PedestalsXY",          true)); // accumulated
  if (GetIsTree()) {
    if(fHHitMapEnergyAboveThr) fHHitMapEnergyAboveThrPerBurst = static_cast<TH2F*>(fHHitMapEnergyAboveThr->Clone("HitMapEnergyAboveThrPerBurst"));
    if(fHNQualityWarningsXY)   fHNQualityWarningsXYPerBurst   = static_cast<TH2F*>(fHNQualityWarningsXY->Clone("NQualityWarningsXYPerBurst"));
    if(fHPedestalsXY       )   fHPedestalsXYPerBurst          = static_cast<TH2F*>(fHPedestalsXY->Clone("PedestalsXYPerBurst"));
  }
  else {
    // Looking for histos in LKrBadCells dir (for DQ)
    if(!fHHitMapEnergyAboveThr) fHHitMapEnergyAboveThr = static_cast<TH2F*>(RequestHistogram("LKrBadCells", "HitMapEnergyAboveThr", true)); // accumulated
    if(!fHNQualityWarningsXY)   fHNQualityWarningsXY   = static_cast<TH2F*>(RequestHistogram("LKrBadCells", "NQualityWarningsXY",   true)); // accumulated
    if(!fHPedestalsXY       )   fHPedestalsXY          = static_cast<TH2F*>(RequestHistogram("LKrBadCells", "PedestalsXY",          true)); // accumulated
    fHProcessingInfo = static_cast<TH1F*>(RequestHistogram("LKrBadCells", "ProcessingInfo",       true)); // accumulated
  }
  if (!fHHitMapEnergyAboveThr) cout << user_normal() << "WARNING: Histogram 'HitMapEnergyAboveThr' not found" << endl;
  if (!fHNQualityWarningsXY)   cout << user_normal() << "WARNING: Histogram 'NQualityWarningsXY' not found" << endl;
  if (!fHPedestalsXY)          cout << user_normal() << "WARNING: Histogram 'PedestalsXY' not found" << endl;

  BookHisto("Pedestals", new TH1F("Pedestals","Mean pedestal distribution for each LKr active cell", 200, 0, 1000));
  fHPedestals   = static_cast<TH1F*>(fHisto.GetTH1("Pedestals"));
  if(!fHProcessingInfo) BookHisto("ProcessingInfo", new TH1F("ProcessingInfo","Processing info", 2, 0.5, 2.5));
}

void LKrBadCells::StartOfRunUser() {}

void LKrBadCells::StartOfBurstUser() {

  TH1F* hProcessingInfo = fHProcessingInfo;
  if(!hProcessingInfo){
    FillHisto("ProcessingInfo",1); 
    FillHisto("ProcessingInfo",2,GetRecoInfo()->GetNReadEvents()); //histos filled ad the raw-decoder level!
    hProcessingInfo = (TH1F*)fHisto.GetTH1("ProcessingInfo");
  }
  fNProcessedBursts = hProcessingInfo->GetBinContent(1);
  fNProcessedEvents = hProcessingInfo->GetBinContent(2);
  if(!GetIsTree()) return; //no per-burst checks in histo mode
  if(fHHitMapEnergyAboveThrPreviousBurst) fHHitMapEnergyAboveThrPerBurst->Add(fHHitMapEnergyAboveThr,fHHitMapEnergyAboveThrPreviousBurst,1,-1);
  if(fHNQualityWarningsXYPreviousBurst)   fHNQualityWarningsXYPerBurst->Add(fHNQualityWarningsXY,fHNQualityWarningsXYPreviousBurst,1,-1);
  if(fHPedestalsXYPreviousBurst)          fHPedestalsXYPerBurst->Add(fHPedestalsXY,fHPedestalsXYPreviousBurst,1,-1);
  fHotCellsFromHitMap           = FindHotCellsFromHitMap(fHHitMapEnergyAboveThrPerBurst);
  fHotCellsFromQualityWarnings  = FindHotCellsFromQualityWarnings(fHNQualityWarningsXYPerBurst);
  fHotCellsFromPedestals        = FindHotCellsFromPedestals(fHPedestalsXYPerBurst);
  fDeadCellsFromHitMap          = FindDeadCellsFromHitMap(fHHitMapEnergyAboveThrPerBurst);
  fDeadCellsFromHitMapInTotal   = FindDeadCellsFromHitMap(fHHitMapEnergyAboveThrPerBurst,true);
  fMaxNCellsInDeadCluster       = FindMaxNCellsInDeadCluster(false);
  fMaxNCellsInDeadClusterInnerCircle = FindMaxNCellsInDeadCluster(true);

  if(fHHitMapEnergyAboveThrPreviousBurst) delete fHHitMapEnergyAboveThrPreviousBurst;
  if(fHNQualityWarningsXYPreviousBurst  ) delete fHNQualityWarningsXYPreviousBurst;
  if(fHPedestalsXYPreviousBurst         ) delete fHPedestalsXYPreviousBurst;
  if(fHHitMapEnergyAboveThr) fHHitMapEnergyAboveThrPreviousBurst = static_cast<TH2F*>(fHHitMapEnergyAboveThr->Clone("HitMapEnergyAboveThrPreviousBurst"));
  if(fHNQualityWarningsXY)   fHNQualityWarningsXYPreviousBurst   = static_cast<TH2F*>(fHNQualityWarningsXY->Clone("NQualityWarningsXYPreviousBurst"));
  if(fHPedestalsXY       )   fHPedestalsXYPreviousBurst          = static_cast<TH2F*>(fHPedestalsXY->Clone("PedestalsXYPreviousBurst"));
}

void LKrBadCells::ProcessSpecialTriggerUser(int, unsigned int) {}

void LKrBadCells::Process(int) {}

void LKrBadCells::PostProcess() {}

void LKrBadCells::EndOfBurstUser(){}

void LKrBadCells::EndOfRunUser() {}

void LKrBadCells::EndOfJobUser(){
  fHotCellsFromHitMap           = FindHotCellsFromHitMap(fHHitMapEnergyAboveThr);
  fHotCellsFromQualityWarnings  = FindHotCellsFromQualityWarnings(fHNQualityWarningsXY);
  fHotCellsFromPedestals        = FindHotCellsFromPedestals(fHPedestalsXY);
  fDeadCellsFromHitMap          = FindDeadCellsFromHitMap(fHHitMapEnergyAboveThr);
  fDeadCellsFromHitMapInTotal   = FindDeadCellsFromHitMap(fHHitMapEnergyAboveThr,true);
  fMaxNCellsInDeadCluster       = FindMaxNCellsInDeadCluster(false);
  fMaxNCellsInDeadClusterInnerCircle = FindMaxNCellsInDeadCluster(true);

  PrintBadCellsFile();
  GeneratePDFReport();
  SaveAllPlots();
}

void LKrBadCells::DrawPlot() {}

LKrBadCells::~LKrBadCells() {}

///////////////////////
// Find LKr HotCells from hitmap

std::vector<LKrCell> LKrBadCells::FindHotCellsFromHitMap(TH2F* hHitMap){

  vector<LKrCell> HotCells;
  HotCells.clear();

  if (!hHitMap) return HotCells;

  // Look for hot cells
  for(Int_t xBin=1;xBin<=hHitMap->GetNbinsX();xBin++){
    for(Int_t yBin=1;yBin<=hHitMap->GetNbinsY();yBin++){
      if(hHitMap->GetBinContent(xBin,yBin)<100.) continue; //ignore cells with less than 100 hits
      LKrCell cell;
      cell.x = hHitMap->GetXaxis()->GetBinCenter(xBin);
      cell.y = hHitMap->GetYaxis()->GetBinCenter(yBin);
      // Evaluate mean occupancy of the neighbours
      std::vector<Int_t> OccupancyBins;
      std::vector<Int_t> OccupancyBinsEntries;
      OccupancyBins.clear();
      OccupancyBinsEntries.clear();
      for(Int_t iNeighbourX=-1;iNeighbourX<=1;iNeighbourX++){
        for(Int_t iNeighbourY=-1;iNeighbourY<=1;iNeighbourY++){
          if(!iNeighbourX && !iNeighbourY) continue; //itself
          Int_t NeighbourOccupancy = hHitMap->GetBinContent(xBin+iNeighbourX,yBin+iNeighbourY);
          if(!NeighbourOccupancy) continue; //empty neighbour
          // Bin the neighbour population (to vote on the occupancy)
          Bool_t BinFound = false;
          for(UInt_t iBin=0;iBin<OccupancyBins.size();iBin++){
            if(fabs(log(NeighbourOccupancy)-log(OccupancyBins[iBin]))<log(2.)) { // bin together values between (Occupancy/2, 2*Occupancy)
              OccupancyBinsEntries[iBin]++;
              BinFound=true;
            }
          }
          if(!BinFound){ //create new bin
            OccupancyBins.push_back(NeighbourOccupancy);
            OccupancyBinsEntries.push_back(1);
          }
        }
      }
      // Find the most populated bin
      Int_t MostVotedBinID = 0; // start with first bin
      for(UInt_t iBin=1;iBin<OccupancyBins.size();iBin++){
        if(OccupancyBinsEntries[iBin]>OccupancyBinsEntries[MostVotedBinID]) MostVotedBinID = iBin;
      }
      // If the cell has an occupancy >fHitMapScaleFactor times higher than neighbours -> noisy cell
      if(OccupancyBins.size()>0 && hHitMap->GetBinContent(xBin,yBin)>fHitMapScaleFactor*OccupancyBins[MostVotedBinID]) HotCells.push_back(cell);
    }
  }
  return HotCells;
}

///////////////////////
// Find LKr HotCells from QualityWarnings

std::vector<LKrCell> LKrBadCells::FindHotCellsFromQualityWarnings(TH2F* hNQualityWarningsXY){

  vector<LKrCell> HotCells;
  HotCells.clear();

  if(!hNQualityWarningsXY) return HotCells;

  TH2F* NBitFlipsXY = static_cast<TH2F*>(hNQualityWarningsXY->Clone("NBitFlipsXY")); // To manipulate histo without changing the original
  Int_t xBin=0, yBin=0, zBin=0;

  //Look for bit flips
  while(NBitFlipsXY->GetBinContent(NBitFlipsXY->GetMaximumBin())>1.e-3*fNProcessedEvents){
    NBitFlipsXY->GetBinXYZ(NBitFlipsXY->GetMaximumBin(),xBin,yBin,zBin);
    LKrCell cell;
    cell.x = NBitFlipsXY->GetXaxis()->GetBinCenter(xBin);
    cell.y = NBitFlipsXY->GetYaxis()->GetBinCenter(yBin);
    HotCells.push_back(cell);
    NBitFlipsXY->SetBinContent(NBitFlipsXY->GetMaximumBin(),0.);
  }
  delete NBitFlipsXY;
  return HotCells;
}

///////////////////////
// Find LKr HotCells from pedestal

std::vector<LKrCell> LKrBadCells::FindHotCellsFromPedestals(TH2F* hPedestalsXY){

  vector<LKrCell> HotCells;
  HotCells.clear();

  if (!hPedestalsXY) return HotCells; // Subdetector not enabled

  // Fill fHPedestals for active cells
  fHPedestals->Reset("M");
  for(Int_t iBinX=1; iBinX<=hPedestalsXY->GetNbinsX();iBinX++){
    for(Int_t iBinY=1; iBinY<=hPedestalsXY->GetNbinsY();iBinY++){
      Double_t PedValue = hPedestalsXY->GetBinContent(iBinX,iBinY)/fNProcessedBursts;
      if(!PedValue) continue; //Skip empty cells
      fHPedestals->Fill(PedValue);
    }
  }

  // Define pedestal range for good cells 
  fPedLowerLimit = fHPedestals->GetMean()-fNSigmaForBadCellSearch*fHPedestals->GetRMS();
  fPedUpperLimit = fHPedestals->GetMean()+fNSigmaForBadCellSearch*fHPedestals->GetRMS();

  TH2F* PedXY = static_cast<TH2F*>(hPedestalsXY->Clone("PedXY")); // To manipulate histo without changing the original
  Int_t xBin=0, yBin=0, zBin=0;

  // Look for hot cells
  while(PedXY->GetBinContent(PedXY->GetMaximumBin())/fNProcessedBursts>fPedUpperLimit){
    PedXY->GetBinXYZ(PedXY->GetMaximumBin(),xBin,yBin,zBin);
    LKrCell cell;
    cell.x = PedXY->GetXaxis()->GetBinCenter(xBin);
    cell.y = PedXY->GetYaxis()->GetBinCenter(yBin);
    HotCells.push_back(cell);
    PedXY->SetBinContent(PedXY->GetMaximumBin(),0.);
  }

  // Look for dead cells
  while(PedXY->GetBinContent(PedXY->GetMinimumBin())/fNProcessedBursts<fPedLowerLimit){
    PedXY->GetBinXYZ(PedXY->GetMinimumBin(),xBin,yBin,zBin);
    if(PedXY->GetBinContent(PedXY->GetMinimumBin())){ // Ignore empty cells
      LKrCell cell;
      cell.x = PedXY->GetXaxis()->GetBinCenter(xBin);
      cell.y = PedXY->GetYaxis()->GetBinCenter(yBin);
      HotCells.push_back(cell);
    }
    PedXY->SetBinContent(PedXY->GetMinimumBin(),99999.*fNProcessedBursts);
  }
  delete PedXY;
  return HotCells;
}

///////////////////////
// Find LKr DeadCells from hitmap

std::vector<LKrCell> LKrBadCells::FindDeadCellsFromHitMap(TH2F* hHitMap, Bool_t AllDeadCells){

  vector<LKrCell> DeadCells;
  DeadCells.clear();

  if (!hHitMap) return DeadCells;

  // Reset cell status info
  Int_t CellStatus[128][128]; // 0: good, 1: bad, 99: non-instrumented
  for(Int_t ixCell=0;ixCell<128;ixCell++){
    for(Int_t iyCell=0;iyCell<128;iyCell++){
      CellStatus[ixCell][iyCell] = 0;
    }
  }

  // Load masked cells info
  TString ChapedesFile = "LKr-chapedes.dat";
  NA62ConditionsService::GetInstance()->Open(ChapedesFile);
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(ChapedesFile))) {
    if (Line.BeginsWith("#")) continue;
    if (Line.BeginsWith("*")) continue;
    Int_t ix = -1, iy = -1, Status = -1;
    Double_t PedRef = -1., PedSigma = -1.;
    stringstream ss;
    ss << Line;
    ss >> ix >> iy >> Status >> PedRef >> PedSigma; 
    if(Status!=0 && 0<=ix && ix<128 && 0<=iy && iy<128){ // bad cell
      if(Status==99) CellStatus[ix][iy] = 99; // status = 99: not instrumented cells
      else CellStatus[ix][iy] = 1; 
    }
  }
  NA62ConditionsService::GetInstance()->Close(ChapedesFile);

  TString ChaslopeFile = "LKr-chaslope.dat";
  NA62ConditionsService::GetInstance()->Open(ChaslopeFile);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(ChaslopeFile))) {
    if (Line.BeginsWith("#")) continue;
    if (Line.BeginsWith("*")) continue;
    Int_t ix = -1, iy = -1, Status = -1;
    Double_t CalSteig = -1.;
    stringstream ss;
    ss << Line;
    ss >> ix >> iy >> Status >> CalSteig; 
    if(Status>9 && Status<1000 && 0<=ix && ix<128 && 0<=iy && iy<128){ // bad cell
      if(Status==99) CellStatus[ix][iy] = 99; // status = 99: not instrumented cells
      else CellStatus[ix][iy] = 1; 
    }
  }
  NA62ConditionsService::GetInstance()->Close(ChaslopeFile);

  Int_t NMaskedCells=0;
  for(Int_t ixCell=0;ixCell<128;ixCell++){
    for(Int_t iyCell=0;iyCell<128;iyCell++){
      if(CellStatus[ixCell][iyCell]!=0 && CellStatus[ixCell][iyCell]!=99) NMaskedCells++;
    }
  }
  std::cout << user_normal() << "Found " << NMaskedCells << " masked cells!" << std::endl;

  // Look for unexpected dead cells
  for(Int_t xBin=1;xBin<=hHitMap->GetNbinsX();xBin++){
    for(Int_t yBin=1;yBin<=hHitMap->GetNbinsY();yBin++){
      if(hHitMap->GetBinContent(xBin,yBin)>0.) continue; // ignore non-empty cells
      LKrCell cell;
      cell.x = hHitMap->GetXaxis()->GetBinCenter(xBin);
      cell.y = hHitMap->GetYaxis()->GetBinCenter(yBin);
      if(CellStatus[cell.x][cell.y]==99) continue; // Always skip not instrumented cells
      if(!AllDeadCells && CellStatus[cell.x][cell.y]>0) continue; // ignore masked cells (= known dead cells + masked hot cells)
      DeadCells.push_back(cell);
    }
  }
  return DeadCells;
}

///////////////////////
// Find biggest cluster of LKr dead cells

Int_t LKrBadCells::FindMaxNCellsInDeadCluster(Bool_t IgnorePeripheral){

  Int_t NMaxDeadCellsInCluster = 0;
  vector<LKrCell> DeadCells(fDeadCellsFromHitMapInTotal);

  // Don't check "known" cases
  for(UInt_t iDeadCell=0;iDeadCell<DeadCells.size();iDeadCell++){
    Bool_t IgnoreCell=false;
    if(DeadCells[iDeadCell].y==127) IgnoreCell=true;  // Don't check the top row
    if(IgnorePeripheral && sqrt(pow(DeadCells[iDeadCell].x-64,2)+pow(DeadCells[iDeadCell].y-64,2))>=fInnerCircleRadiusForDeadCells) {
      IgnoreCell=true;  // Don't check the outer cells = sqrt((ix-64)^2+(iy-64)^2)>=fInnerCircleRadiusForDeadCells
    }
    if(IgnoreCell) {
      DeadCells.erase(DeadCells.begin()+iDeadCell);
      iDeadCell--;
    }
  }

  // Look for dead cells clusters
  for(UInt_t iDeadCell=0;iDeadCell<DeadCells.size();iDeadCell++){
    Int_t NDeadCellsInCluster = FindDeadClusters(DeadCells,iDeadCell);
    if(NDeadCellsInCluster>NMaxDeadCellsInCluster) NMaxDeadCellsInCluster = NDeadCellsInCluster;
    iDeadCell--;
  }

  return NMaxDeadCellsInCluster;
}

Int_t LKrBadCells::FindDeadClusters(std::vector<LKrCell> &DeadCells, Int_t iDeadCell){
  Int_t NDeadCellsInCluster=0;
  LKrCell DeadCell = DeadCells[iDeadCell];
  DeadCells.erase(DeadCells.begin()+iDeadCell);
  NDeadCellsInCluster++;
  for(Int_t iNeighbourX=-1;iNeighbourX<=1;iNeighbourX++){
    for(Int_t iNeighbourY=-1;iNeighbourY<=1;iNeighbourY++){
      if(!iNeighbourX && !iNeighbourY) continue; //itself
      LKrCell NeighbourCell;
      NeighbourCell.x = DeadCell.x+iNeighbourX;
      NeighbourCell.y = DeadCell.y+iNeighbourY;
      Int_t iNeighbour = -1;
      for(UInt_t jDeadCell=0;jDeadCell<DeadCells.size();jDeadCell++){
        if(NeighbourCell.x==DeadCells[jDeadCell].x && NeighbourCell.y==DeadCells[jDeadCell].y) { // found
          iNeighbour = jDeadCell;
          break;
        }
      }
      if(iNeighbour>=0) NDeadCellsInCluster+=FindDeadClusters(DeadCells,iNeighbour);
    }
  }
  return NDeadCellsInCluster;
}

///////////////////////
// Print BadCells file

void LKrBadCells::PrintBadCellsFile(){

  const int PedFlag   =  2; // bad cell flag in chapdes file
  const int SlopeFlag = 11; // bad cell flag in chaslope file

  vector<LKrCell> BadCells;
  BadCells.clear();

  if(fBadCellsMask&(1<<kDeadHitMapBit)) {
    for(UInt_t iCell=0;iCell<fDeadCellsFromHitMap.size();iCell++){
      Bool_t Found = false;
      for(UInt_t jCell=0;jCell<BadCells.size();jCell++){
        if(BadCells[jCell].x==fDeadCellsFromHitMap[iCell].x && BadCells[jCell].y==fDeadCellsFromHitMap[iCell].y) {
          Found = true;
          break;
        }
      }
      if(!Found) BadCells.push_back(fDeadCellsFromHitMap[iCell]); //add bad cell
    }
  }
  if(fBadCellsMask&(1<<kHotHitMapBit)) {
    for(UInt_t iCell=0;iCell<fHotCellsFromHitMap.size();iCell++){
      Bool_t Found = false;
      for(UInt_t jCell=0;jCell<BadCells.size();jCell++){
        if(BadCells[jCell].x==fHotCellsFromHitMap[iCell].x && BadCells[jCell].y==fHotCellsFromHitMap[iCell].y) {
          Found = true;
          break;
        }
      }
      if(!Found) BadCells.push_back(fHotCellsFromHitMap[iCell]); //add bad cell
    }
  }
  if(fBadCellsMask&(1<<kHotQualityWarningsBit)) {
    for(UInt_t iCell=0;iCell<fHotCellsFromQualityWarnings.size();iCell++){
      Bool_t Found = false;
      for(UInt_t jCell=0;jCell<BadCells.size();jCell++){
        if(BadCells[jCell].x==fHotCellsFromQualityWarnings[iCell].x && BadCells[jCell].y==fHotCellsFromQualityWarnings[iCell].y) {
          Found = true;
          break;
        }
      }
      if(!Found) BadCells.push_back(fHotCellsFromQualityWarnings[iCell]); //add bad cell
    }
  }
  if(fBadCellsMask&(1<<kHotPedestalsBit)) {
    for(UInt_t iCell=0;iCell<fHotCellsFromPedestals.size();iCell++){
      Bool_t Found = false;
      for(UInt_t jCell=0;jCell<BadCells.size();jCell++){
        if(BadCells[jCell].x==fHotCellsFromPedestals[iCell].x && BadCells[jCell].y==fHotCellsFromPedestals[iCell].y) {
          Found = true;
          break;
        }
      }
      if(!Found) BadCells.push_back(fHotCellsFromPedestals[iCell]); //add bad cell
    }
  }

  ofstream BadCellsFile;
  BadCellsFile.open(fOutTxtFileName);
  BadCellsFile << GetRunID() << " " << BadCells.size();
  for(UInt_t iCell=0; iCell<BadCells.size();iCell++){
    BadCellsFile << " " << BadCells.at(iCell).x << " " << BadCells.at(iCell).y << " " << PedFlag << " " << SlopeFlag;
  }
  BadCellsFile << endl;
  BadCellsFile.close();
}

///////////////////////
// Build a PDF report

void LKrBadCells::GeneratePDFReport() {

  if (!fGenerateOutputPDF || !fOutPDFFileName.Length()) {
    cout << user_normal() << "Report generation is not required" << endl;
    return;
  }

  if (!fHHitMapEnergyAboveThr) return;
  if (!fHNQualityWarningsXY)   return;
  if (!fHPedestalsXY)          return;

  cout << user_normal() << "Generating report "<< fOutPDFFileName << endl;

  TCanvas* Canvas = new TCanvas("Canvas");

  fHHitMapEnergyAboveThr->SetTitle(Form("Total energy deposited per cell for run %d",GetRunID()));
  fHHitMapEnergyAboveThr->Draw("COLZ");
  // Show the hot cells
  TEllipse ** CirclesHotHM = 0;
  if(fHotCellsFromHitMap.size()){
    CirclesHotHM = new TEllipse*[fHotCellsFromHitMap.size()];
    for(UInt_t iCell=0; iCell<fHotCellsFromHitMap.size();iCell++){
      CirclesHotHM[iCell] = new TEllipse(fHotCellsFromHitMap.at(iCell).x,fHotCellsFromHitMap.at(iCell).y,1.);
      CirclesHotHM[iCell]->SetFillStyle(0);
      CirclesHotHM[iCell]->Draw();
    }
  }
  // Show the dead cells
  TEllipse ** CirclesDeadHM = 0;
  if(fDeadCellsFromHitMap.size()){
    CirclesDeadHM = new TEllipse*[fDeadCellsFromHitMap.size()];
    for(UInt_t iCell=0; iCell<fDeadCellsFromHitMap.size();iCell++){
      CirclesDeadHM[iCell] = new TEllipse(fDeadCellsFromHitMap.at(iCell).x,fDeadCellsFromHitMap.at(iCell).y,1.);
      CirclesDeadHM[iCell]->SetFillStyle(0);
      CirclesDeadHM[iCell]->SetLineColor(kRed); // dead cells are red
      CirclesDeadHM[iCell]->Draw();
    }
  }
  gPad->SetLogz(1);
  Canvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the canvas
  gPad->SetLogz(0);

  fHNQualityWarningsXY->SetTitle(Form("Quality warnings per cell for run %d",GetRunID()));
  fHNQualityWarningsXY->Draw("COLZ");
  // Show the bad cells
  TEllipse ** CirclesHotQW = 0;
  if(fHotCellsFromQualityWarnings.size()){
    CirclesHotQW = new TEllipse*[fHotCellsFromQualityWarnings.size()];
    for(UInt_t iCell=0; iCell<fHotCellsFromQualityWarnings.size();iCell++){
      CirclesHotQW[iCell] = new TEllipse(fHotCellsFromQualityWarnings.at(iCell).x,fHotCellsFromQualityWarnings.at(iCell).y,1.);
      CirclesHotQW[iCell]->SetFillStyle(0);
      CirclesHotQW[iCell]->Draw();
    }
  }
  gPad->SetLogz(1);
  Canvas->Print(fOutPDFFileName, "pdf");
  gPad->SetLogz(0);


  fHPedestalsXY->SetTitle(Form("Pedestal values per cell for run %d",GetRunID()));
  fHPedestalsXY->Draw("COLZ");
  // Show the bad cells
  TEllipse ** CirclesHotPed = 0;
  if(fHotCellsFromPedestals.size()){
    CirclesHotPed = new TEllipse*[fHotCellsFromPedestals.size()];
    for(UInt_t iCell=0; iCell<fHotCellsFromPedestals.size();iCell++){
      CirclesHotPed[iCell] = new TEllipse(fHotCellsFromPedestals.at(iCell).x,fHotCellsFromPedestals.at(iCell).y,1.);
      CirclesHotPed[iCell]->SetFillStyle(0);
      CirclesHotPed[iCell]->Draw();
    }
  }
  Canvas->Print(fOutPDFFileName, "pdf");

  fHPedestals->SetTitle(Form("Pedestal values per cell for run %d",GetRunID()));
  fHPedestals->Draw();
  gPad->SetLogy(1);
  TLine * LLimit = new TLine(fPedLowerLimit,0.,fPedLowerLimit,1000);
  TLine * ULimit = new TLine(fPedUpperLimit,0.,fPedUpperLimit,1000);
  LLimit->Draw();
  ULimit->Draw();
  Canvas->Print(Form(fOutPDFFileName + ")"), "pdf"); // print and close the file
  gPad->SetLogy(0);

  // Delete objects
  delete Canvas;
  for(UInt_t iCell=0; iCell<fHotCellsFromHitMap.size();iCell++){
    delete CirclesHotHM[iCell];
  }
  if(fHotCellsFromHitMap.size()) delete [] CirclesHotHM;
  for(UInt_t iCell=0; iCell<fHotCellsFromQualityWarnings.size();iCell++){
    delete CirclesHotQW[iCell];
  }
  if(fHotCellsFromQualityWarnings.size()) delete [] CirclesHotQW;
  for(UInt_t iCell=0; iCell<fHotCellsFromPedestals.size();iCell++){
    delete CirclesHotPed[iCell];
  }
  if(fHotCellsFromPedestals.size()) delete [] CirclesHotPed;
  for(UInt_t iCell=0; iCell<fDeadCellsFromHitMap.size();iCell++){
    delete CirclesDeadHM[iCell];
  }
  if(fDeadCellsFromHitMap.size()) delete [] CirclesDeadHM;
  delete LLimit;
  delete ULimit;

}
