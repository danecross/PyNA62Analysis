#include <iostream>
#include <fstream>
#include <sstream>
#include "TLKrDigi.hh"
#include "LKrL0Emulator.hh"

LKrL0Emulator::LKrL0Emulator() : 
  fFADCEvent(nullptr), fL0Data(nullptr), fUseOffsets(false), fDebug(false), fRefFT(-1),
  fNoPedCounts(), p1(false), p2(false), m1(false),
  can(nullptr), pol(nullptr), lin(nullptr), vline(nullptr), hline(nullptr),
  eventinfile(0)
{
  InitHistograms();

  // In case T0s are applied to the trigger sums
  if(fUseOffsets) ReadL0CaloCalibration();
  
  // initialize pieces used to plot the trigger sum fits
  can = new TCanvas();
  //can->SaveAs("TestPDF.pdf[");
  pol = new TF1("pol", "[0]*x*x + [1]*x + [2]",0,9);
  pol->SetLineColor(kRed);
  lin = new TF1("lin", "[0]*x + [1]", 0,9);
  lin->SetLineColor(kBlue);
  vline = new TLine();
  vline->SetLineColor(kGreen+2);
  vline->SetLineWidth(2);
  hline = new TLine();
  hline->SetLineColor(kMagenta+2);
  hline->SetLineWidth(2);
}

// Destructor. Nothing to do here.
LKrL0Emulator::~LKrL0Emulator(){
  delete can; 
  delete pol;
  delete lin;
  delete vline;
  delete hline;
  // should delete histograms? Or did ownership change?
}

void LKrL0Emulator::InitHistograms(){

  //8 sample: 200 ns => 32 bin @ 6.25 ns
  SChisto = new TH1F * [1024]; // what is this? array of histograms?
  for(int i=0; i<1024;i++){ 
    SChisto[i] = new TH1F(Form("h%d",i),Form("H%d",i),9,0,9); // Istogramma rozzo. In sample da 25 ns.
    SChisto[i]->GetXaxis()->SetNdivisions(9,4,0,kFALSE);
  }
  SCfinetime = new TH1F("finetime","finetime",20,-7.5,12.5); //in ns

  fCellEnergy    = new TH1F("CellEnergy", "CellEnergy", 51, -0.5, 50.5);
  fCellEnergyNZS = new TH1F("CellEnergyNZS", "CellEnergyNZS", 51, -0.5, 50.5);
  fCellEnergyNP    = new TH1F("CellEnergyNP", "CellEnergyNP", 501, -0.5, 500.5);
  fCellEnergyNZSNP = new TH1F("CellEnergyNZSNP", "CellEnergyNZSNP", 501, -0.5, 500.5);
  fCellMaxEnergy = new TH1F("CellMaxEnergy", "CellMaxEnergy", 101, -0.5, 100.5);
  fCellMaxEnergyNZS = new TH1F("CellMaxEnergyNZS", "CellMaxEnergyNZS", 101, -0.5, 100.5);
  fCellMaxEnergyNP = new TH1F("CellMaxEnergyNP", "CellMaxEnergyNP", 501, -0.5, 500.5);
  fCellMaxEnergyNZSNP = new TH1F("CellMaxEnergyNZSNP", "CellMaxEnergyNZSNP", 501, -0.5, 500.5);
  fSuperCellEnergy    = new TH1F("SuperCellEnergy", "SuperCellEnergy", 201, -0.5, 200.5);
  fSuperCellEnergyNZS = new TH1F("SuperCellEnergyNZS", "SuperCellEnergyNZS", 201, -0.5, 200.5);
  fSuperCellEnergyNP    = new TH1F("SuperCellEnergyNP", "SuperCellEnergyNP", 201, -0.5, 2009.5);
  fSuperCellEnergyNZSNP = new TH1F("SuperCellEnergyNZSNP", "SuperCellEnergyNZSNP", 401, -0.5, 10024.5);
  fPrimTimeDiff = new TH1F("PrimTimeDiff","PrimTimeDiff",101, -50.5, 50.5);
  fPrimTimeDiffHighE = new TH1F("PrimTimeDiffHighE","PrimTimeDiffHighE",101, -50.5, 50.5);
  fPrimTimeDiffL = new TH1F("PrimTimeDiffL","PrimTimeDiffL",2001, -1000.5, 1000.5);
  fPrimTimeDiffRICH = new TH1F("PrimTimeDiffRICH","PrimTimeDiffRICH",201, -100.5, 100.5);

  fPrimTimeCorr = new TH2F("PrimTimeCorr","PrimTimeCorr",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);

  fPrimTimeCorrAll = new TH2F("PrimTimeCorrAll","PrimTimeCorrAll",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);

  fPrimTimeCorrP = new TH2F("PrimTimeCorrP","PrimTimeCorrP",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);
  fPrimTimeCorrT = new TH2F("PrimTimeCorrT","PrimTimeCorrT",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);
  fPrimTimeCorrN = new TH2F("PrimTimeCorrN","PrimTimeCorrN",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);

  fPrimTimeCorr1 = new TH2F("PrimTimeCorr1","PrimTimeCorr1",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);
  fPrimTimeCorr2 = new TH2F("PrimTimeCorr2","PrimTimeCorr2",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);
  fPrimTimeCorrHighE = new TH2F("PrimTimeCorrHighE","PrimTimeCorrHighE",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);
  fPrimTimeCorrShifted = new TH2F("PrimTimeCorrShifted","PrimTimeCorrShifted",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);
  fPrimTimeCorrHighEShifted = new TH2F("PrimTimeCorrHighEShifted","PrimTimeCorrHighEShifted",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);
  // fPrimTimeCorrShiftedUp = new TH2F("PrimTimeCorrShiftedUp","PrimTimeCorrShiftedUp",
  // 			   256+128, -128, 256, 256+128+128, -64-128, 256+64);
  // fPrimTimeCorrHighEShiftedUp = new TH2F("PrimTimeCorrHighEShiftedUp","PrimTimeCorrHighEShiftedUp",
  // 			   256+128, -128, 256, 256+128+128, -64-128, 256+64);
  fPrimTimeCorrDiff = new TH2F("PrimTimeCorrDiff","PrimTimeCorrDiff",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);
  fPrimTimeCorrRICH = new TH2F("PrimTimeCorrRICH","PrimTimeCorrRICH",
			   256+128+64, -128, 256+64, 256+128+128+64+64, -64-128, 256+64+64+64);

  fPrimEnergyDiff = new TH1F("PrimEnergyDiff","PrimEnergyDiff", 99, -(49.5*448), (49.5*448));
  fPrimEnergyCorr = new TH2F("PrimEnergyCorr","PrimEnergyCorr", 55, 25000, 80000, 65, 20000, 85000);
  fPrimTimeEnergyDiff = new TH2F("PrimTimeEnergyDiff","PrimTimeEnergyDiff",101, -50.5, 50.5,99, -(49.5*448), (49.5*448));


  fPrimTimeDiffSC   = new TH2F("PrimTimeDiffSC", "PrimTimeDiffSC", 1024, -0.5, 1023.5, 50, -25.0,25.0);
  fPrimEnergyDiffSC = new TH2F("PrimEnergyDiffSC","PrimEnergyDiffSC", 1024, -0.5, 1023.5, 49, -(24.5*448), (24.5*448));
  fSuperCellCount = new TH2F("SuperCellCount", "SuperCellCount", 1024, -0.5, 1023.5, 21, -0.5, 20.5);
  fSuperCellCountNZS = new TH2F("SuperCellCountNZS", "SuperCellCountNZS", 1024, -0.5, 1023.5, 21, -0.5, 20.5);

  fPeakXY = new TH2F("PeakXY","PeakXY", 32, -0.5,31.5, 32, -0.5, 31.5);
  fPeakXYEnergy = new TH2F("PeakXYEnergy","PeakXYEnergy", 32, -0.5,31.5, 32, -0.5, 31.5);
  fPrimXY = new TH2F("PrimXY","PrimXY", 32, -0.5,31.5, 32, -0.5, 31.5);
  fPrimXYEnergyDiff = new TH2F("PrimXYEnergyDiff","PrimXYEnergyDiff", 32, -0.5,31.5, 32, -0.5, 31.5);
  fPrimXYTimeDiff = new TH2F("PrimXYTimeDiff","PrimXYTimeDiff", 32, -0.5,31.5, 32, -0.5, 31.5);
  fPrimXYAbsTimeDiff = new TH2F("PrimXYAbsTimeDiff","PrimXYAbsTimeDiff", 32, -0.5,31.5, 32, -0.5, 31.5);

  fEmulPeakFineTime = new TH1F("EmulPeakFineTime", "EmulPeakFineTime", 256, -0.5, 255.5);
  fEmulPeakFineTime2 = new TH1F("EmulPeakFineTime2", "EmulPeakFineTime2", 256, -0.5, 255.5);

  fRealPrimFineTime = new TH1F("RealPrimFineTime", "RealPrimFineTime", 256, -0.5, 255.5);
  fEmulPrimFineTime = new TH1F("EmulPrimFineTime", "EmulPrimFineTime", 256, -0.5, 255.5);
  fEmulPrimFineTime2 = new TH1F("EmulPrimFineTime2", "EmulPrimFineTime2", 256, -0.5, 255.5);

  fRealPrimFineTimeShifted = new TH1F("RealPrimFineTimeShifted", "RealPrimFineTimeShifted", 256, -0.5-69, 255.5-69);
  fEmulPrimFineTimeShifted = new TH1F("EmulPrimFineTimeShifted", "EmulPrimFineTimeShifted", 256, -0.5-69, 255.5-69);
  // fRealPrimFineTimeShiftedUp = new TH1F("RealPrimFineTimeShiftedUp",
  // 					"RealPrimFineTimeShiftedUp",256, -0.5+59, 255.5+59);
  // fEmulPrimFineTimeShiftedUp = new TH1F("EmulPrimFineTimeShiftedUp", 
  // 					"EmulPrimFineTimeShiftedUp", 256, -0.5+59, 255.5+59);
  
  fEmulatorEfficiency = new TH1F("EmulatorEfficiency","EmulatorEfficiency", 2, -0.5, 1.5);

  fPrimTimeDiffEnergy = new TH2F("PrimTimeDiffEnergy","PrimTimeDiffEnergy",70, 10000,80000, 101, -50.5, 50.5 );
  fPrimTimeDiffPrimEnergy = new TH2F("PrimTimeDiffPrimEnergy","PrimTimeDiffPrimEnergy",70, 10000,80000, 101, -50.5, 50.5 );

  fPrimEnergyDiffvsN = new TH2F("PrimEnergyDiffvsN","PrimEnergyDiffvsN", 21, -0.5, 20.5, 99, -(49.5*448), (49.5*448)); 
  fSuperCellMap = new TH2F("SuperCellMap","SuperCellMap", 32, -0.5,31.5, 32, -0.5, 31.5);
  fSuperCellMapM = new TH2F("SuperCellMapM","SuperCellMapM", 32, -16*0.08, 16*0.08, 32, -16*0.08, 16*0.08);
  for(int i=0;i<1024;++i){
    fSuperCellMap->Fill( SuperCellX(i),SuperCellY(i), i);
    fSuperCellMapM->Fill( -15.5*0.08 + 0.08*SuperCellX(i), -15.5*0.08 + 0.08*SuperCellY(i), i);
  }

  fPeakEnergyDiff1   = new TH1F("PeakEnergyDiff1", "PeakEnergyDiff1", 201, -0.5, 200.5); // 200 counts = 11 GeV
  fPeakEnergyDiff2   = new TH1F("PeakEnergyDiff2", "PeakEnergyDiff2", 201, -0.5, 200.5); // 200 counts = 11 GeV
  // fPeakEnergyCorr1   = new TH2F("PeakEnergyCorr1", "PeakEnergyCorr1", 
  // 				1001, -0.5, 1000.5, 1001, -0.5, 1000.5); // 200 counts = 11 GeV
  // fPeakEnergyCorr2   = new TH2F("PeakEnergyCorr2", "PeakEnergyCorr2",
  // 				1001, -0.5, 1000.5, 1001, -0.5, 1000.5); // 200 counts = 11 GeV

  fCircEnergyDiff1   = new TH1F("CircEnergyDiff1", "CircEnergyDiff1", 201, -0.5, 200.5); // 200 counts = 11 GeV
  fCircEnergyDiff2   = new TH1F("CircEnergyDiff2", "CircEnergyDiff2", 201, -0.5, 200.5); // 200 counts = 11 GeV
  // fCircEnergyCorr1   = new TH2F("CircEnergyCorr1", "CircEnergyCorr1",
  // 				1001, -0.5, 1000.5, 1001, -0.5, 1000.5); // 200 counts = 11 GeV
  // fCircEnergyCorr2   = new TH2F("CircEnergyCorr2", "CircEnergyCorr2",
  // 				1001, -0.5, 1000.5, 1001, -0.5, 1000.5); // 200 counts = 11 GeV
 
  fRecoEnergyDiff    = new TH1F("RecoEnergyDiff","RecoEnergyDiff",99, -(49.5*448), (49.5*448));
  fRecoTotEnergyDiff = new TH1F("RecoTotEnergyDiff","RecoTotEnergyDiff",99, -(49.5*448), (49.5*448));
  fRecoEnergyCorr    = new TH2F("RecoEnergyCorr","RecoEnergyCorr", 100, 0, 80000, 100, 0, 80000);
  fRecoTotEnergyCorr = new TH2F("RecoTotEnergyCorr","RecoTotEnergyCorr", 100, 0, 80000, 100, 0, 80000);

  fPrimTimeCorr->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorr->GetYaxis()->SetNdivisions(10,2,0,kFALSE);

  fPrimTimeCorrAll->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrAll->GetYaxis()->SetNdivisions(10,2,0,kFALSE);

  fPrimTimeCorrP->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrP->GetYaxis()->SetNdivisions(10,2,0,kFALSE);
  fPrimTimeCorrT->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrT->GetYaxis()->SetNdivisions(10,2,0,kFALSE);
  fPrimTimeCorrN->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrN->GetYaxis()->SetNdivisions(10,2,0,kFALSE);

  fPrimTimeCorrHighE->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrHighE->GetYaxis()->SetNdivisions(10,2,0,kFALSE);
  fPrimTimeCorrShifted->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrShifted->GetYaxis()->SetNdivisions(10,2,0,kFALSE);
  fPrimTimeCorrHighEShifted->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrHighEShifted->GetYaxis()->SetNdivisions(10,2,0,kFALSE);
  // fPrimTimeCorrShiftedUp->GetXaxis()->SetNdivisions(6,2,0,kFALSE);
  // fPrimTimeCorrShiftedUp->GetYaxis()->SetNdivisions(8,2,0,kFALSE);
  // fPrimTimeCorrHighEShiftedUp->GetXaxis()->SetNdivisions(6,2,0,kFALSE);
  // fPrimTimeCorrHighEShiftedUp->GetYaxis()->SetNdivisions(8,2,0,kFALSE);
  fPrimTimeCorrDiff->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrDiff->GetYaxis()->SetNdivisions(10,2,0,kFALSE);
  fPrimTimeCorrRICH->GetXaxis()->SetNdivisions(7,2,0,kFALSE);
  fPrimTimeCorrRICH->GetYaxis()->SetNdivisions(10,2,0,kFALSE);
}

void LKrL0Emulator::WriteHistograms(){
  fCellEnergy->Write();
  fCellEnergyNZS->Write();
  fCellEnergyNP->Write();
  fCellEnergyNZSNP->Write();
  fCellMaxEnergy->Write();
  fCellMaxEnergyNZS->Write();
  fCellMaxEnergyNP->Write();
  fCellMaxEnergyNZSNP->Write();
  fSuperCellEnergy->Write();
  fSuperCellEnergyNZS->Write();
  fSuperCellEnergyNP->Write();
  fSuperCellEnergyNZSNP->Write();

  fPrimTimeDiff->Write();
  fPrimTimeDiffHighE->Write();
  fPrimTimeDiffL->Write();
  fPrimTimeDiffRICH->Write();

  fPrimTimeCorr->Write();

  fPrimTimeCorrAll->Write();

  fPrimTimeCorrP->Write();
  fPrimTimeCorrT->Write();
  fPrimTimeCorrN->Write();

  fPrimTimeCorr1->Write();
  fPrimTimeCorr2->Write();
  fPrimTimeCorrHighE->Write();
  fPrimTimeCorrShifted->Write();
  fPrimTimeCorrHighEShifted->Write();
  // fPrimTimeCorrShiftedUp->Write();
  // fPrimTimeCorrHighEShiftedUp->Write();
  fPrimTimeCorrDiff->Write();
  fPrimTimeCorrRICH->Write();

  fPrimEnergyDiff->Write();
  fPrimEnergyCorr->Write();
  fPrimTimeEnergyDiff->Write();
  fPeakXY->Write();
  fPeakXYEnergy->Write();
  fPrimXY->Write();
  fPrimXYTimeDiff->Write();
  fPrimXYAbsTimeDiff->Write();
  fPrimXYEnergyDiff->Write();

  fEmulPeakFineTime->Write();
  fEmulPeakFineTime2->Write();

  fRealPrimFineTime->Write();
  fEmulPrimFineTime->Write();
  fEmulPrimFineTime2->Write();

  fRealPrimFineTimeShifted->Write();
  fEmulPrimFineTimeShifted->Write();
  // fRealPrimFineTimeShiftedUp->Write();
  // fEmulPrimFineTimeShiftedUp->Write();

  fPrimTimeDiffSC->Write();
  fPrimEnergyDiffSC->Write();
  fSuperCellCount->Write();
  fSuperCellCountNZS->Write();

  fEmulatorEfficiency->Write();

  fPrimTimeDiffEnergy->Write();
  fPrimTimeDiffPrimEnergy->Write();
  fPrimEnergyDiffvsN->Write();

  fSuperCellMap->Write();
  fSuperCellMapM->Write();
  
  fPeakEnergyDiff1->Write();
  fPeakEnergyDiff2->Write();
  // fPeakEnergyCorr1->Write();
  // fPeakEnergyCorr2->Write();

  fCircEnergyDiff1->Write();
  fCircEnergyDiff2->Write();
  // fCircEnergyCorr1->Write();
  // fCircEnergyCorr2->Write();

  fRecoEnergyDiff->Write();
  fRecoTotEnergyDiff->Write();
  fRecoEnergyCorr->Write();
  fRecoTotEnergyCorr->Write();

  // can->SaveAs("TestPDF.pdf]");
}

// This function is to read the L0Calo T0 offset file generated as part of the 
// post-processing. It is here as a placeholder since the offsets have never been used.
void LKrL0Emulator::ReadL0CaloCalibration(){
  std::ifstream f;
  f.open("L0CaloCalibration.dat"); // placeholder
  if(!f) std::cout << " FILE NOT OPEN " << std::endl;
  std::string str;
  int c=0;

  for(unsigned i=0; i<1024; ++i) OffsetsSC[i]=0;

  while (std::getline(f, str)){
    std::istringstream s(str);
    std::string ts;
    s >> ts; // first word 

    if(ts.at(0)=='#')  continue; // comments
    if(ts.length()==0) continue; // empty string
    double temp = std::stod(ts); // convert string to double
    OffsetsSC[c] = (temp/TdcCalib); // units
    c++;
  }
}

int LKrL0Emulator::FromCellToSuperCell(int CellX, int CellY){
  return (CellX/4)*32 + (CellY/4);  
}

int LKrL0Emulator::SuperCellX(int SuperCell){ 
  return SuperCell/32;
}

int LKrL0Emulator::SuperCellY(int SuperCell){ 
  return SuperCell%32;
}

int LKrL0Emulator::CalibrationCellXY(int CellX, int CellY){ 
  return CellY*34 + CellX; // as above, but x and y are inverted
}

int LKrL0Emulator::CalibrationCellID(int CellID){ 
  int x = SuperCellX(CellID); // decode X from CellID
  int y = SuperCellY(CellID); // decode Y from CellID
  return CalibrationCellXY(x,y); // get calibration CellID using X,Y
}

void LKrL0Emulator::Process(FADCEvent* event, L0TPData* L0TPData){
  
  fFADCEvent = event;
  fL0Data = L0TPData;

  // bool ControlData = (fL0Data->GetDataType() & 0x10 );
  // bool PhysicsData = (fL0Data->GetDataType() & 0x1  );
  // bool OnlyMask1   = (fL0Data->GetTriggerFlags() == 0x2);
  // if( (PhysicsData&&OnlyMask1) && !ControlData) return; // do not run if only mask1 fired.  
  
  // std::cout << " ============= New Event " << eventinfile << " =================== " << std::endl;
  eventinfile++;
  
  if(fDebug){
    // Build peaks from reconstructed hits
    BuildRecoPeaks();

    // Sort the reco peaks in time.
    // This is not part of the L0Calo algorithm, it's just for easier debugging.
    // The L0Calo algorithm is not sensitive to the time-order of the peaks so no results are affected.
    std::sort( fRecoPeaks.begin(), fRecoPeaks.end(), 
	       [](const PeakOrCluster& a, const PeakOrCluster& b)
	       { return (a.T()<b.T());} );
    PrintPeakOrCluster(fRecoPeaks, "RECOPEAK");
  }

  // Prepare trigger sums
  // These are histograms of the ADC counts, in 8 samples, for each supercell.
  // Supercells are 4x4 groups of cells, corresponding to one CREAM board.
  // Each sample corresponds to 25ns.
  BuildTriggerSums();

  // Build peaks (in time) from the trigger sums
  // The energy of the peak is determined by a parabolic fit to the largest
  // sample and the two adjacent samples in each supercell.
  // The fine time of the peak is defined at the point where the peak reaches
  // half of its maximum value (i.e. a constant fraction).
  // The fine time is obtained by a linear interpolation between the (two)
  // samples that are above and below the half-maximum energy.
  fTimePeaks.clear();
  for(int sc=0;sc<1024;sc++){
    BuildPeaks(sc);
  }
  std::sort( fTimePeaks.begin(), fTimePeaks.end(), 
  	     [](const PeakOrCluster& a, const PeakOrCluster& b)
  	     { return (a.T()<b.T());} );
  if(fDebug){
    PrintPeakOrCluster(fTimePeaks, "PEAK");
    // Compare real primitives with emulated primitives (for debugging)
    ComparePeaks();
  }
  
  // Fill circular buffer with time peaks
  BuildCircularBuffer();

  // Build primitives from circular buffer.
  BuildPrimitives();

  // Compare real primitives with emulated primitives (for debugging)
  if(fDebug) ComparePrimitives();

  // put the primitives in L0TPData, for MC only
  StorePrimitives();
}

// This function prepares the trigger sums, which are used 
// to find energy peaks in each supercell
void LKrL0Emulator::BuildTriggerSums(){

  // reset SC histograms for this event.
  // all histograms should be replaced with fixed arrays ?
  SCfinetime->Reset("m");
  for (int i=0; i<1024;i++) {
    SChisto[i]->Reset("m"); // all bin contents set to zero.
    for(unsigned j=0; j<9; ++j) fNoPedCounts[i][j] = 0;
  }

  // get ADC counts in each cell
  TClonesArray& cellsADC    = (*(fFADCEvent->GetHits()));

  int SCcount[1024] = {0};

  // loop over all cells (with non-zero energy??)
  Int_t nHits = fFADCEvent->GetNHits();
  // std::cout << " Number of cells " << nHits << std::endl;
  for (Int_t icell=0; icell<nHits; icell++){

    // get ADC samples for this cell
    TLKrDigi*    CellADC = static_cast<TLKrDigi *>(cellsADC[icell]);
    Double_t* ADCSamples = CellADC->GetAllSamples();

    // TLKrDigi*    CellEnergy = static_cast<TLKrDigi *>(cellsEnergy[icell]);
    // Double_t* EnergySamples = CellEnergy->GetAllSamples();

    // get estimate of the pedestal for this cell
    Int_t CPPedestal = 400; // 405 for best timing

    // get cell position
    Int_t ix = CellADC->GetXCellID();
    Int_t iy = CellADC->GetYCellID();

    // convert cell "position ID" to supercell ID
    int SuperCell = FromCellToSuperCell(ix,iy);
    if(SuperCell>1023){
      std::cout<<"************ERROR!!!!!!!!!!!!!!!!**** Supercell Overflow"<< std::endl; 
      continue;
    }

    SCcount[SuperCell]++;

    // get "position ID" of the supercell

    // Fill SChisto with ADC counts for this cell, with pedestal removed.
    int vmax = -1;
    for (Int_t j=0; j<8; j++) {
      fNoPedCounts[SuperCell][j] += ADCSamples[j]; // store "no pedestal" energies

      Int_t val = ADCSamples[j] - CPPedestal;
      if(val<0) val=0;
      SChisto[SuperCell]->AddBinContent(j+1, val); // first sample into first bin (not underflow) 
      if(val>vmax){
	vmax = val;
      }
    }

    // now for plots of the peak times.
    int max = -1;
    int maxNP = -1;
    for (Int_t j=0; j<8; j++) {
      Int_t val = ADCSamples[j];
      if(p1) fCellEnergyNP->Fill(val);
      if(p2) fCellEnergyNZSNP->Fill(val);
      if(val>maxNP) maxNP=val;
      val -= CPPedestal;
      if(val<0) val = 0; // ADC count cannot be negative.
      if(val>max) max = val;
      if(p1) fCellEnergy->Fill(val);
      if(p2) fCellEnergyNZS->Fill(val);
    }
    if(p1) fCellMaxEnergy->Fill(max);
    if(p2) fCellMaxEnergyNZS->Fill(max);
    if(p1) fCellMaxEnergyNP->Fill(maxNP);
    if(p2) fCellMaxEnergyNZSNP->Fill(maxNP);
  }// end loop on cells   

  // Convert SChisto from CREAM energy precision (3.5MeV)
  // to L0Calo energy precision (56MeV). 4 bits truncated; 2^4 = 16
  // Note that the trigger sums are made using the 3.5MeV precision,
  // then are truncated when sent to the L0Calo firmware.

  for(unsigned i=0; i<1024; ++i){
    if(p1) fSuperCellCount->Fill(i, SCcount[i]);
    if(p2) fSuperCellCountNZS->Fill(i, SCcount[i]);

    for(unsigned j=1; j<10; ++j){
      
      if(p1) fSuperCellEnergyNP->Fill(fNoPedCounts[i][j-1]);
      if(p2) fSuperCellEnergyNZSNP->Fill(fNoPedCounts[i][j-1]);
      
      int v = SChisto[i]->GetBinContent(j); 
      if(v>65535) std::cout << " Energy overflow in SC " << i << std::endl; // 2^16 = 65536
      v /= 16; // drop 4 LSB. 2^4 = 16
      if(p2) fSuperCellEnergyNZS->Fill(v);
      if(p1) fSuperCellEnergy->Fill(v);
      SChisto[i]->SetBinContent(j, v);
    }
  }

  return;
}

// Algorithm to find time peaks in each supercell using the maximum bin as the seed. 
// This is not the same as the L0Calo system, which analyses each sample as it comes.
// But it's the best available proxy, and the same as the hit reconstruction in LKrRawDecoder.
void LKrL0Emulator::BuildPeaks(int nSC){

  // get the relevant histogram
  TH1F* SC = SChisto[nSC];

  // Energy cut to define a time peak. Units of ADC counts
  // 7 counts is the best guess, based on energy resolution in real and emulated primitives.
  Double_t EnergyCut=7; // g.t.7 ADC counts == 448 in one supercell.

  // Find maximum bin.
  int tbin = SC->GetMaximumBin();
  // double imax = tbin;
  
  // build arrays for parabolic fit. Choice of x coordinate (its arbitrary?)
  //Double_t x[3] = {imax-1,imax,imax+1};
  Double_t x[3] = {SC->GetBinLowEdge(tbin-1),SC->GetBinLowEdge(tbin),SC->GetBinLowEdge(tbin+1)};
  Double_t y[3] = {SC->GetBinContent(tbin-1), SC->GetBinContent(tbin), SC->GetBinContent(tbin+1)};

  bool haspeak = (y[1]>EnergyCut);
  if(!haspeak) return;

  // std::cout << " => => => tbin " << tbin << " imax " << imax << std::endl;
  // for(unsigned i=0; i<3; ++i) std::cout << " x[i] " << x[i] << " y[i] " << y[i] << std::endl;

  int scnd = std::max(y[0],y[2]);
  int thrd = std::min(y[0],y[2]);
  fPeakEnergyDiff1->Fill(y[1]-scnd);
  fPeakEnergyDiff2->Fill(y[1]-thrd);
  // fPeakEnergyCorr1->Fill(y[1],y[1]-scnd);
  // fPeakEnergyCorr2->Fill(y[1],y[1]-thrd);
  
  bool goodpeak = (y[0]<y[1] && y[2]<=y[1]); // see event 27193, burst 24, run 8252
  if(!goodpeak) return;

  // allow peaks in bins [2..8]. LinearInterpolation might not work in bin 2.
  bool edgepeak = (tbin<2 || tbin>8);
  if(edgepeak) return;

  // make the parabolic fit to the energy peak
  // returns a double value, but fitted at 56MeV precision
  Double_t peakmax = MakeParabolicFit(x,y);
  
  // make a linear extrapolation to the fine time at half-maximum energy
  // this function is passed the maximum peak energy
  // returns an integer in TDC units (98ps) from the start of the first sample in the event
  Int_t peakFT = MakeLinearInterpolation(SC, tbin, peakmax);  
  if(peakFT<0){
    // std::cout << " linear interpolation failed, SC = " << nSC 
    // 	      << " tbin = " << tbin << " peakmax " << peakmax << std::endl;
    return;
  }

  fEmulPeakFineTime->Fill(peakFT%256);
  peakFT += 69; // for some reason
  // peakFT += 59; // for some reason.
  fEmulPeakFineTime2->Fill(peakFT%256);

  // apply T0 Offsets to peaks (never used in data)
  // peakFT -= OffsetsSC[CalibrationCellID(nSC)];
  
  if(peakFT<0){
    std::cout << " Peak has negative time!" << std::endl;
  }
  
  // energy of the peak is the max value returned from the parabolic fit
  int peakEn  = peakmax;
  int peakX   = SuperCellX(nSC);
  int peakY   = SuperCellY(nSC);
  
  // add this peak to container of peaks
  PeakOrCluster a(peakFT, peakEn, peakX, peakY);
  a.SetSC(nSC);
  fTimePeaks.push_back(a);

  fPeakXY->Fill(peakX, peakY);
  fPeakXYEnergy->Fill(peakX, peakY, peakEn);
  
  can->Clear();
  SC->Draw();
  SC->SetLineColor(1);
  pol->Draw("same");
  lin->Draw("same");
  vline->Draw("same");
  hline->Draw("same");

  // to save peaks to a pdf file, for detailed debugging
  // can->SaveAs("TestPDF.pdf");
}

// This function makes a parabolic fit to three energy samples.
// It is identical to the one made in the LKrRawDecoder, except that here it is made on each supercell.
// The function returns the height (ymax) and the time (xmax) of the peak
// By setting x[0] = -1, x[1]=0, x[2]=+1 the equations simplify dramatically,
// it's likely this 'trick' was used in the firmware to simplify the computation.
Double_t LKrL0Emulator::MakeParabolicFit(Double_t* x, Double_t* y){

  Double_t p2 = (y[2]-y[0])/((x[2]-x[0])*(x[2]-x[1])) - (y[1]-y[0])/((x[1]-x[0])*(x[2]-x[1]));
  Double_t p1 = (y[1]-y[0])/(x[1]-x[0]) -p2*(x[1]+x[0]);
  Double_t p0 = y[0] -p2*x[0]*x[0] -p1*x[0];                                                   

  // for plotting the parabola
  pol->SetParameters(p2,p1,p0);

  Double_t xmax = -p1/(2*p2); // time relative to first sample
  Double_t ymax = p2*xmax*xmax + p1*xmax + p0; // maximum energy in the peak

  // for plotting the half max
  hline->SetX1(0.0);
  hline->SetX2(xmax);
  hline->SetY1(ymax/2.0);
  hline->SetY2(ymax/2.0);

  return ymax;
}

// This function finds the fine-time (98ps) at which the energy became higher than half of the maximum.
// This time is the one used by the L0Calo to define the fine time of the peaks, and therefore of the primitives.
Int_t LKrL0Emulator::MakeLinearInterpolation(TH1F* SC, unsigned tbin, Double_t peakmax){
  
  // Double_t halfmax = peakmax/2.0 ; // FP division
  // Double_t halfmax = int(peakmax/2) ; // integer division
  Double_t halfmax = int(peakmax) ; // CHANGE

  // find first bin larger than halfmax:
  // no! go backwards from current position and find first bin lower than halfmax!
  int lowbin = -1;
  for(unsigned i=tbin; i>0; i--){ // bins 1-9
    Double_t v = SC->GetBinContent(i)*2; // CHANGE
    if(v<=halfmax){
      lowbin = i;
      break;
    }
    // Note: when v==halfmax the bin is chosen.
    // but the linear fit gives the minimum possible value, FT==0
    // this causes the spike seen in the primitive times.
    // This is much more likely to happen for low-energy peaks.
    // Other spikes appear due to the halfmax being expressed as an integer,
    // so values such as halfmax=5.5 become 5.0
  }
      
  if(lowbin<0){
    // std::cout << " warning: lowbin is not set. could not find peak for this SC. " << std::endl;
    return -1;
  }
  
  int uppbin = lowbin+1;
  if(uppbin>9){ // overflow bin! impossible?
    // std::cout << " warning: uppbin is not set. could not find peak for this SC. " << std::endl;
    return -1;
  }

  Double_t x1 = SC->GetBinLowEdge(lowbin);
  Double_t y1 = SC->GetBinContent(lowbin)*2; // CHANGE
  Double_t x2 = SC->GetBinLowEdge(uppbin);
  Double_t y2 = SC->GetBinContent(uppbin)*2; // CHANGE

  // linear interpolation from lowbin to uppbin:
  Double_t m = (y2-y1)/(x2-x1);
  Double_t c = y1-(x1*m);
  lin->SetParameters(m,c);

  // the finetime of the peak
  Double_t peakft = (halfmax-c)/m; // from maths
  peakft *= 256;

  // most likely VHDL implementation ?
  // fraction gives the fractional distance of halfmax in the bin.
  // all values in the computation are bit-shifted by 8 (*256)
  // except for peakmax (which gives half peakmax = halfmax)
  // then the fraction is simply the fine time in the bin
  // Double_t fraction = (int(128*peakmax)-256*y1)/(y2-y1); // 128*peakmax == 256*halfmax
  // Double_t peakft   = 256*(x1)+fraction;
  // std::cout << " peakftm " << peakftm<< " peakft " << peakft << " halfmax " << halfmax << " y2 " << y2 << " y1 " << y1 << " fraction " << fraction << std::endl;

  // for plotting fit result
  vline->SetX1(peakft);
  vline->SetX2(peakft);
  vline->SetY1(0.0);
  vline->SetY2(halfmax);
  
  // convert into TDC units (each sample is 256 TDC units)
  peakft = int(peakft); // integer!
  
  return peakft;
}

// Algorithm to take time peaks and add them to a "circular buffer".
// Each slot in the circular buffer is 6.25ns wide.
// When peaks are added to the buffer they are converted to 'clusters' by 
// summing their energy and taking the fine-time of the highest energy peak.
// Each time a peak is added the peak count (N) is increased.
// If the position of the incoming peak is not consistent with the first peak
// added to the cluster, the 'cluster count' (C) is increased.
void LKrL0Emulator::BuildCircularBuffer(){

  int w=64; // 32
  int n=(9*256)/w; // 9 samples per SC (sort of), but now in units of 64. 9*4 = 36

  fCircularBuffer.clear();
  fCircularBuffer.resize(n); 

  // add each time peak into the circular buffer.
  for(unsigned i=0; i<fTimePeaks.size(); ++i){
    // if(fTimePeaks[i].SC()==420 || fTimePeaks[i].SC()==389){
    //   // these two SC are noisy
    //   std::cout << " SC " << fTimePeaks[i].SC() << " E " << fTimePeaks[i].E()
    //   		<< " T " << fTimePeaks[i].T() << std::endl;
    // }

    // time in bins of 64 tdc units.
    // find slot in the circular buffer.
    int time = fTimePeaks[i].T();
    if(time<0) continue; // don't fill negative times
    int CB = time/w;
    if(CB<0 || CB>=n){
      std::cout << "WARNING! Negative time given to circular buffer" << std::endl;
      exit(1);
    }
    
    // add time peak to circular buffer
    MergePeaks(fCircularBuffer[CB], fTimePeaks[i]);
  }
  
  // clean up time peaks
  fTimePeaks.clear();

  if(fDebug) PrintPeakOrCluster(fCircularBuffer, "CLUSTER");
}

// Algorithm to merge peaks in the circular buffer.
// If there is nothing in the buffer (a.N()==0) then simply add the peak
// in the buffer. Otherwise, set the time in the buffer as the time of
// the largest peak, sum the energy of the two peaks. Compare their
// positions to see if they are consistent with a single peak.
void LKrL0Emulator::MergePeaks(PeakOrCluster& a, PeakOrCluster& b){

  if(a.N()==0){ // copy 
    a = b;
  }
  else{ // merge
    // set values to those of largest peak ever seen by this cluster
    if(a.BE()<b.E()){
      a.SetT(b.T());
      a.SetBE(b.E());
      a.SetBX(b.X());
      a.SetBY(b.Y());
      a.SetSC(b.SC());
    }
    // sum energy of peaks
    a.AddE(b.E());
    // check for more than one peak
    if(a.N()==1){
      int dx = abs(a.X()-b.X());
      int dy = abs(a.Y()-b.Y());
      int d  = dx+dy;
      if(d>2) a.SetN(2);
    }
    a.AddC(b.C());
  }
}

// Algorithm to build L0Calo primitives from clusters.
// Scan through the circular buffer (aka through time) looking for clusters.
// Merge close clusters and produce the respective primitive.
// The energy of clusters is summed, while the primitive time is that of the most energetic cluster.
// Since the cluster time is that of the most energetic peak it contains, ultimately the primitive 
// time is simply defined by the (single) most energetic peak.
void LKrL0Emulator::BuildPrimitives(){

  fPrimitives.clear();
  // int EThreshold1  =  17; // roughly  1GeV in 56MeV ADC counts
  // int EThreshold5  =  89; // roughly  5GeV in 56MeV ADC counts
  int EThreshold10 = 178; // roughly 10GeV in 56MeV ADC counts
  // int EThreshold20 = 357; // roughly 20GeV in 56MeV ADC counts
  // int EThreshold30 = 538; // roughly 30GeV in 56MeV ADC counts
  // int EThreshold40 = 714; // roughly 40GeV in 56MeV ADC counts
  
  int maxmid=-1;
  int maxlow=-1;
  int maxupp=-1;
  for(unsigned i=1; i<fCircularBuffer.size()-1;++i){
    // step through energy peaks, build primitives
    // not testing first or last bin of 6.25ns

    int mid = fCircularBuffer[i].E();
    int low = fCircularBuffer[i-1].E();
    int upp = fCircularBuffer[i+1].E();
    
    if(mid>maxmid){
      maxmid=mid;
      maxlow=low;
      maxupp=upp;
    }
    
    if(low>=mid) continue; // not the peak
    if(upp>=mid) continue; // not the peak

    // primitive energy
    int pE = mid+low+upp;

    // check energy of primitive against lowest 'kaon mode' threshold.
    if(pE<EThreshold10) continue; // not enough energy to build primitive

    // primitive time and 'N' clusters, as used by the L0Calo hardware
    int pT = fCircularBuffer[i].T();
    int pN = fCircularBuffer[i].N();

    // record fine time of all emulated primitives.
    fEmulPrimFineTime->Fill(pT%256);
    fEmulPrimFineTimeShifted->Fill((pT%256)-69);
    //fEmulPrimFineTimeShiftedUp->Fill((pT%256)+59); // -69+128

    PeakOrCluster a(pT, pE, pN);
    a.SetX( fCircularBuffer[i].BX()); // set X of biggest energy peak in cluster
    a.SetY( fCircularBuffer[i].BY()); // set Y of biggest energy peak in cluster
    a.SetC( fCircularBuffer[i].C()); // set number of peaks merged into this primitive
    a.SetSC( fCircularBuffer[i].SC()); // set SuperCell ID of biggest peak in the primitive
    fPrimitives.push_back(a);

    // remove used clusters from circular buffer:
    fCircularBuffer[i-1].SetE(0.0);
    fCircularBuffer[i].SetE(0.0);
    fCircularBuffer[i+1].SetE(0.0);
  }

  if(maxmid>7){ // make sure cluster has at least 560MeV
    int scnd = std::max(maxupp,maxlow);
    int thrd = std::min(maxupp,maxlow);
    fCircEnergyDiff1->Fill(maxmid-scnd);
    fCircEnergyDiff2->Fill(maxmid-thrd);
  //   fCircEnergyCorr1->Fill(maxmid,maxmid-scnd);
  //   fCircEnergyCorr2->Fill(maxmid,maxmid-thrd);
  }

  fCircularBuffer.clear(); // done with these now.

  if(fDebug) PrintPeakOrCluster(fPrimitives, "PRIMITIVE");
}

// Algorithm to store emulated primitives in the L0TPData packet.
// For MC only! In data this will overwrite the real primitives!
void LKrL0Emulator::StorePrimitives(){

  // check for fake trigger flags and data type to identify MC events
  if(fL0Data->GetTriggerFlags()!=0xFFFF) return;

  // get the vector of primitives (avoid changing L0TPData class)
  std::vector<L0Primitive> L0Primitives;
  for(unsigned i=0; i<3; ++i){
    for(unsigned j=0; j<7; ++j){
      L0Primitives.push_back( fL0Data->GetPrimitive(i,j) );
    }
  }
  
  // remove the fake primitive added in NA62Reconstruction
  L0Primitives[kL0Calo].SetPrimitiveID(0);
  L0Primitives[kL0Calo].SetFineTime(0);

  // get the trigger fine time
  int ReferenceFineTime = fL0Data->GetReferenceFineTime();
  
  // loop through emulated primitives
  std::vector<PeakOrCluster>::iterator it;
  it = fPrimitives.begin();
  // int count =0; 
  int TOffset = 4;
  for(; it != fPrimitives.end() ;++it){
    
    int time     = it->T() - TOffset*256 - ReferenceFineTime ; // account for trigger time offset.
    int finetime = (it->T()%256);
    int energy   = it->E() * 56.0;
    int clusters = it->N();

    // std::cout << " Primitive " << count << " energy " << energy << " clusters " << clusters << " time " << time << " ref FT " << ReferenceFineTime << std::endl;
    // count++;

    // Compute primitive ID. Stop if the condition is not passed
    unsigned primitiveid=0;
    if( (clusters>1) || (energy>30000.0) ) primitiveid = 0x4000;
    else                                   continue;    

    // Compute slot from primitive time.
    unsigned iL0Slot = 99;
    if(time < -64)      continue;
    else if(time <   0) iL0Slot =  0; // previous 
    else if(time <  64) iL0Slot =  1; // trigger slot
    else if(time < 128) iL0Slot =  2; // next slot
    else                continue;
    
    if(iL0Slot==99){
      std::cout << " ERROR! This iL0Slot " << iL0Slot << " is not possible!" << std::endl;
      continue;
    }

    // build new L0Primitive
    L0Primitive a;
    a.SetFineTime(finetime);
    a.SetPrimitiveID( primitiveid );

    // std::cout << " ---> adding primitive: FT= " << finetime << " ID " << primitiveid << std::endl;

    // store new L0Primitive locally
    L0Primitives[iL0Slot*L0NMAXDETECTORS+kL0Calo] = a;
  }
  
  // put the new primitives into the L0TP packet
  fL0Data->SetPrimitives(L0Primitives);
}

void LKrL0Emulator::ComparePrimitive(int Slot, int Detector){  
  Bool_t PrimitiveH = (fL0Data->GetPrimitive(Slot,Detector).GetPrimitiveID()>0);

  // require real primitive to make comparison
  if(!PrimitiveH) return;
  
  Int_t  PrimitiveE =  448*(fL0Data->GetPrimitive(Slot,Detector).GetPrimitiveID()&0x00ff);
  // Int_t  PrimitiveT = fL0Data->GetPrimitive(Slot,Detector).GetFineTime();
  Int_t PrimitiveT = fL0Data->GetPrimitive(Slot,Detector).GetCorrectedFineTime(Slot, fRefFT, 2);
  
  int TOffset = 3;
  // // primitive in next slot but fine time is smaller: must be in the next TS
  // if(Slot==kL0NextSlot     && PrimitiveT<fRefFT) TOffset=4;
  // // primitive in previous slot but fine time is bigger: must be in the previous TS
  // if(Slot==kL0PreviousSlot && PrimitiveT>fRefFT) TOffset=2;
      
  if(Detector==kL0Calo){
    fRealPrimFineTime->Fill(PrimitiveT);
    fRealPrimFineTimeShifted->Fill(PrimitiveT-69);
    // fRealPrimFineTimeShiftedUp->Fill(PrimitiveT+59);
  }

  // loop through emulated primitives to find the one that matches best
  std::vector<PeakOrCluster>::iterator it;
  it = fPrimitives.begin();

  bool hasMatch=false;
  int smalltime = 999999;
  int ftime     = 999999;
  int ctime     = 999999;
  int cenergy   = 999999;
  int cx = 999;
  int cy = 999;
  int cc = 999;
  int csc = 999;
  // int cn = 999;

  for(; it != fPrimitives.end() ;++it){
    int time = it->T() - TOffset*256; // account for trigger time offset.
    // std::cout << " primitive time " << time << std::endl;
    int energy = it->E();
    int tdiff = abs(PrimitiveT-time);

    fPrimTimeCorrAll->Fill(PrimitiveT, time);
    
    // if this is the closest emulated primitive to the real one.
    if(tdiff < smalltime){
      hasMatch=true;
      smalltime = tdiff;
      ctime = time;
      ftime = (it->T()%256);
      cenergy = energy;
      cx = it->X();
      cy = it->Y();
      cc = it->C();
      csc = it->SC();
      // cn = it->N();
    }
  }

  if(Detector==kL0RICH){
    
    if(!hasMatch) return; // no matching primitive.

    fPrimTimeDiffRICH->Fill(ctime - PrimitiveT);
    fPrimTimeCorrRICH->Fill(PrimitiveT, ctime);
    
    // no more plots
    return;
  }

  // record fine time of matched emulated primitive
  fEmulPrimFineTime2->Fill(ftime);

  // if there is a primitive within 64 TDC units, 
  // consider the emulator to be efficient
  int eff = (smalltime<64);
  fEmulatorEfficiency->Fill(eff);
  // if(!eff) std::cout << " ==INEFFICIENT== " << smalltime << std::endl;

  if(!hasMatch) return; // no matching primitive (no emulated primitives at all)

  // std::cout << " tdiff " << smalltime << " sc " << csc << std::endl;

  // compute energy difference. Putting emulated energy into units of 448MeV
  int ediff = 448*(cenergy/8) - PrimitiveE;

  // fill general plots
  fPrimTimeDiff->Fill(ctime - PrimitiveT);
  fPrimTimeDiffL->Fill(ctime - PrimitiveT);

  fPrimTimeCorr->Fill(PrimitiveT,  ctime);
  if(Slot==kL0PreviousSlot) fPrimTimeCorrP->Fill(PrimitiveT,  ctime);
  if(Slot==kL0TriggerSlot)  fPrimTimeCorrT->Fill(PrimitiveT,  ctime);
  if(Slot==kL0NextSlot)     fPrimTimeCorrN->Fill(PrimitiveT,  ctime);

  fPrimTimeCorrShifted->Fill(PrimitiveT-69,  ctime-69);
  // fPrimTimeCorrShiftedUp->Fill(PrimitiveT+59,  ctime+59);
  if(cc<3) fPrimTimeCorr1->Fill(PrimitiveT,  ctime);
  else     fPrimTimeCorr2->Fill(PrimitiveT,  ctime);
  fPrimTimeCorrDiff->Fill(PrimitiveT,  ctime-PrimitiveT);

  fPrimTimeDiffSC->Fill(csc, ctime-PrimitiveT);

  fPrimXY->Fill(cx, cy);
  fPrimXYTimeDiff->Fill(cx, cy, ctime - PrimitiveT);
  fPrimXYAbsTimeDiff->Fill(cx, cy, abs(ctime - PrimitiveT));

  fPrimTimeDiffEnergy->Fill(cenergy*56, ctime - PrimitiveT);
  fPrimTimeDiffPrimEnergy->Fill(PrimitiveE, ctime - PrimitiveT);
  
  // fill plots with high energy primitives
  if(PrimitiveE>1){ // primitives with less than 30GeV do not have their energy recorded
    fPrimTimeCorrHighE->Fill(PrimitiveT,  ctime);
    fPrimTimeCorrHighEShifted->Fill(PrimitiveT-69,  ctime-69);
    // fPrimTimeCorrHighEShiftedUp->Fill(PrimitiveT+59,  ctime+59);
    // if(smalltime>10) std::cout << " ==TIMING== " << smalltime << std::endl; 
    fPrimTimeDiffHighE->Fill(ctime - PrimitiveT);
    fPrimEnergyDiff->Fill(ediff);
    fPrimEnergyCorr->Fill(PrimitiveE, 448*(cenergy/8));
    fPrimTimeEnergyDiff->Fill(ctime-PrimitiveT, ediff);
    fPrimXYEnergyDiff->Fill(cx, cy, abs(ediff));
    fPrimEnergyDiffvsN->Fill(cc, ediff);
    fPrimEnergyDiffSC->Fill(csc, ediff);
  }

}

void LKrL0Emulator::ComparePrimitives(){

  fRefFT=-1;
  Bool_t PrimitiveC = (fL0Data->GetPrimitive(kL0TriggerSlot,kL0CHOD).GetPrimitiveID()>0);
  if(PrimitiveC) fRefFT = fL0Data->GetPrimitive(kL0TriggerSlot,kL0CHOD).GetFineTime();
  Bool_t PrimitiveR = (fL0Data->GetPrimitive(kL0TriggerSlot,kL0RICH).GetPrimitiveID()>0);
  if(PrimitiveR) fRefFT = fL0Data->GetPrimitive(kL0TriggerSlot,kL0RICH).GetFineTime();

  if(!PrimitiveC && !PrimitiveR){
    // std::cout << " WARNING: Nothing to compare, not physics or control trigger! " << std::endl;
    return;
  }
  
  // To compare primitives in the correct order: kL0PreviousSlot, kL0TriggerSlot, kL0NextSlot
  // Only comparing to existing Calo primitives for now.
  ComparePrimitive(kL0PreviousSlot, kL0Calo);
  ComparePrimitive(kL0TriggerSlot,  kL0Calo);
  ComparePrimitive(kL0NextSlot,     kL0Calo);

  ComparePrimitive(kL0PreviousSlot, kL0RICH);
  ComparePrimitive(kL0TriggerSlot,  kL0RICH);
  ComparePrimitive(kL0NextSlot,     kL0RICH);

  // Compare LKr primitives to RICH primitives
  
}

void LKrL0Emulator::PrintPeakOrCluster(std::vector<PeakOrCluster>& vec, TString type){
  std::vector<PeakOrCluster>::iterator it;
  it = vec.begin();
  int totalenergy=0;
  int c=0;
  std::cout << "Number of " << type << " " << vec.size() << std::endl;
  for(; it != vec.end() ;++it){
    if(it->N()>0){
      int time = it->T();
      int ts = time/256;
      int energy = it->E();
      totalenergy += energy;
      std::cout << type
		<< " " << c // counter
		<< " time " << time 
  		<< " timestamp "    << ts 
  		<< " fine time "    << time - (256*ts)
  		<< " FC "           << (time - (256*ts))/64
  		<< " energy "       << energy 
  		<< " ("             << energy*56.0 << " MeV)"
  		<< " ("             << (energy/8)*448.0 << " MeV)"
  		<< " n "            << it->N()
  		<< " x "            << it->X()
  		<< " y "            << it->Y()
  		<< " c "            << it->C()
		<< " sc "           << it->SC()
		<< " calsc "        << CalibrationCellID(it->SC())
		<< std::endl;
    }
    c++;
  }
  std::cout << type << " total energy " << totalenergy << "(" << totalenergy*56.0 << " MeV)" << std::endl;
}

// These are energy peaks in the supercells created using the *reconstructed* hits. 
// The time of the peaks is the weighted sum of the energy in each hit.
void LKrL0Emulator::BuildRecoPeaks(){
  SCfinetime->Reset("m");
  for (int i=0; i<1024;i++) {
    SChisto[i]->Reset("m"); // all bin contents set to zero.
  }

  fRecoPeaks.clear();

  int CPPedestal = 400;
  int EnergyCut=7;

  TClonesArray &cellsADC = (*(fFADCEvent->GetHits()));
  Int_t nHits = fFADCEvent->GetNHits();
  //std::cout << " Number of cells " << nHits << std::endl;
  for (Int_t icell=0; icell<nHits; icell++){

    // get ADC samples for this cell
    TLKrDigi*    CellADC = static_cast<TLKrDigi *>(cellsADC[icell]);
    
    // get cell position
    Int_t ix = CellADC->GetXCellID();
    Int_t iy = CellADC->GetYCellID();

    // convert cell "position ID" to supercell ID
    int SuperCell = FromCellToSuperCell(ix,iy);
    if(SuperCell>1023){
      std::cout<<"************ERROR!!!!!!!!!!!!!!!!**** Supercell Overflow"<< std::endl; 
      continue;
    }
   
    double energy = CellADC->GetADCPeakEnergy()-CPPedestal; // RecoPeaks!
    double time   = CellADC->GetADCPeakTime(); // max of signal (int)

    // need a cut on time here! otherwise will add out-of-time energy into the summation?
    if(time<3 || time>5) continue; // loose cut

    SChisto[SuperCell]->AddBinContent(1, energy);
    SChisto[SuperCell]->AddBinContent(2, time*energy); // weighted
  }
  
  for(int i=0; i<1024; ++i){

    double e = SChisto[i]->GetBinContent(1);
    int e2 = e/16; // units of 56 MeV

    if(e2<=EnergyCut) continue;// no 1GeV peak here
    
    double t = SChisto[i]->GetBinContent(2);
    t /= e; // weighted average of the time (sample time)
    int t2=t*256;
    if(t2<10)   continue; // cutting on time of peak. Reco can build peaks in first sample!

    PeakOrCluster a(t2, e2, SuperCellX(i), SuperCellY(i));
    a.SetSC(i);
    fRecoPeaks.push_back(a);
  }
}

// a function to compare reconstructed and emulated peaks
void LKrL0Emulator::ComparePeaks(){

  std::vector<PeakOrCluster>::iterator it1 = fTimePeaks.begin();
  std::vector<PeakOrCluster>::iterator it2;
  
  int tot1=0;
  for(; it1!= fTimePeaks.end(); ++it1){
    int e1 = 56*it1->E();

    int se2 = 99999; 
    int sde = 99999;
    it2 = fRecoPeaks.begin();
    for(; it2!=fRecoPeaks.end(); ++it2){
      int e2 = 56*it2->E();
      int de = abs(e1-e2);
      if(de<sde){
    	sde = de;
    	// se2 = e2;
      }
    }

    // std::cout << " e1 " << e1 << " se2 " << se2 << std::endl;
    fRecoEnergyDiff->Fill(se2-e1); // MeV
    fRecoEnergyCorr->Fill(se2,e1);
    tot1+=e1;
  }

  int tot2=0;
  it2 = fRecoPeaks.begin();
  for(; it2!=fRecoPeaks.end(); ++it2){
    tot2 += 56*it2->E();
  }
  fRecoTotEnergyDiff->Fill(tot2-tot1);
  fRecoTotEnergyCorr->Fill(tot2,tot1);
}

