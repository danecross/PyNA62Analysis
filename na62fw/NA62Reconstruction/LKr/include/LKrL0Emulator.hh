#ifndef LKRL0EMULATOR_H
#define LKRL0EMULATOR_H 1

#include "FADCEvent.hh"
#include "L0TPData.hh"
#include "TLine.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TH1F.h"
#include "TH2F.h"

class LKrL0Emulator{

public:

  LKrL0Emulator();
  ~LKrL0Emulator();

  // Used by LKrReconstruction to run the emulator
  void Process(FADCEvent* event, L0TPData* L0TPData);
  // Used by LKrReconstruction to write the emulator control plots
  void WriteHistograms();
  
private: 

  // forward-declaration of this nested class
  class PeakOrCluster;

  //////////////////////////////////////////////
  // The main event (and L0 info)
  //////////////////////////////////////////////
  FADCEvent* fFADCEvent;
  L0TPData* fL0Data;

  //////////////////////////////////////////////
  // Main algorithms of the L0Calo emulator
  //////////////////////////////////////////////

  // Step 1. Build the trigger sums
  void BuildTriggerSums();

  // Step 2. Build peaks from the trigger sums
  void BuildPeaks(int);

  // Step 2b. Make parabolic then linear fit to determine peak fine time
  Double_t MakeParabolicFit(Double_t*, Double_t*);
  Int_t    MakeLinearInterpolation(TH1F*, unsigned, Double_t);
  
  // Step 3. Merge peaks into the circular buffer
  void BuildCircularBuffer();
  void MergePeaks(PeakOrCluster& a, PeakOrCluster& b);

  // Step 4. Build primitives from the circular buffer
  void BuildPrimitives();
  
  // Step 5. Store primitives in L0Data (MC only)
  void StorePrimitives();

  //////////////////////////////////////////////
  // Helper functions
  //////////////////////////////////////////////
  
  // For reading T0 offsets (never used)
  bool fUseOffsets;
  int OffsetsSC[1024];
  void ReadL0CaloCalibration();

  // To convert to/from cell/supercell ID and (X,Y) position
  int FromCellToSuperCell(int CellX, int CellY);
  int SuperCellX(int SuperCell);
  int SuperCellY(int SuperCell);

  // To convert from supercell ID in this code to the one used by L0CaloCalibration
  // Used to access the proper T0 correction
  int CalibrationCellXY(int CellX, int CellY);
  int CalibrationCellID(int CellID);

  // Build peaks from reconstructed hits
  void BuildRecoPeaks();
  // Compare reconstructed and emulated peaks
  void ComparePeaks();
  // Compare real and emulated primitives
  void ComparePrimitive(int,int);  // just one
  void ComparePrimitives(); // calls above function several times
  // Print a container of PeakOrCluster
  void PrintPeakOrCluster(std::vector<PeakOrCluster>& vec, TString type);

  //////////////////////////////////////////////
  // Member variables
  //////////////////////////////////////////////

  Bool_t fDebug; 
  Int_t fRefFT;

  // containers for the algorithm outputs
  std::vector< PeakOrCluster > fTimePeaks;
  std::vector< PeakOrCluster > fRecoPeaks;
  std::vector< PeakOrCluster > fCircularBuffer;
  std::vector< PeakOrCluster > fPrimitives;  

  // to contain counts without pedestal subtraction
  int fNoPedCounts[1024][9];

  //////////////////////////////////////////////
  // Histograms
  //////////////////////////////////////////////

  void InitHistograms();

  // histograms for the 1024 possible supercells
  TH1F** SChisto;
  TH1F*  SCfinetime; // Change this name!

  TH1F* fCellEnergy;
  TH1F* fCellEnergyNZS;
  TH1F* fCellEnergyNP;
  TH1F* fCellEnergyNZSNP;
  TH1F* fCellMaxEnergy;
  TH1F* fCellMaxEnergyNZS;
  TH1F* fCellMaxEnergyNP;
  TH1F* fCellMaxEnergyNZSNP;
  TH1F* fSuperCellEnergy;
  TH1F* fSuperCellEnergyNZS;

  TH1F* fPrimTimeDiff;
  TH1F* fPrimTimeDiffHighE;
  TH1F* fPrimTimeDiffL;
  TH1F* fPrimTimeDiffRICH;

  TH2F* fPrimTimeCorr;

  TH2F* fPrimTimeCorrAll;

  TH2F* fPrimTimeCorrP;
  TH2F* fPrimTimeCorrT;
  TH2F* fPrimTimeCorrN;

  TH2F* fPrimTimeCorr1;
  TH2F* fPrimTimeCorr2;
  TH2F* fPrimTimeCorrHighE;
  TH2F* fPrimTimeCorrShifted;
  TH2F* fPrimTimeCorrHighEShifted;
  // TH2F* fPrimTimeCorrShiftedUp;
  // TH2F* fPrimTimeCorrHighEShiftedUp;
  TH2F* fPrimTimeCorrDiff;
  TH2F* fPrimTimeCorrRICH;

  TH1F* fPrimEnergyDiff;
  TH2F* fPrimEnergyCorr;
  TH2F* fPrimTimeEnergyDiff;
  TH2F* fPrimXY;
  TH2F* fPrimXYEnergyDiff;
  TH2F* fPrimXYTimeDiff;
  TH2F* fPrimXYAbsTimeDiff;

  TH1F* fEmulPeakFineTime;
  TH1F* fEmulPeakFineTime2;

  TH1F* fRealPrimFineTime;
  TH1F* fEmulPrimFineTime;
  TH1F* fEmulPrimFineTime2;

  TH1F* fRealPrimFineTimeShifted;
  TH1F* fEmulPrimFineTimeShifted;
  // TH1F* fRealPrimFineTimeShiftedUp;
  // TH1F* fEmulPrimFineTimeShiftedUp;

  TH1F* fEmulatorEfficiency;

  TH2F* fPeakXY;
  TH2F* fPeakXYEnergy;

  TH2F* fPrimTimeDiffEnergy;
  TH2F* fPrimTimeDiffPrimEnergy;
  TH2F* fPrimEnergyDiffvsN;

  TH1F* fRecoEnergyDiff;
  TH1F* fRecoTotEnergyDiff;
  TH2F* fRecoEnergyCorr;
  TH2F* fRecoTotEnergyCorr;

  TH2F* fSuperCellMap;
  TH2F* fSuperCellMapM;

  TH1F* fPeakEnergyDiff1;
  TH1F* fPeakEnergyDiff2;
  // TH2F* fPeakEnergyCorr1;
  // TH2F* fPeakEnergyCorr2;

  TH1F* fCircEnergyDiff1;
  TH1F* fCircEnergyDiff2;
  // TH2F* fCircEnergyCorr1;
  // TH2F* fCircEnergyCorr2;

  TH2F* fPrimTimeDiffSC;
  TH2F* fPrimEnergyDiffSC;
  
  TH1F* fSuperCellEnergyNP;
  TH1F* fSuperCellEnergyNZSNP;

  TH2F* fSuperCellCount;
  TH2F* fSuperCellCountNZS;

  // event selection
  bool p1;
  bool p2;
  bool m1;

  // for plotting peak fits to trigger sums
  TCanvas* can;
  TF1* pol;
  TF1* lin;
  TLine* vline;
  TLine* hline;

  int eventinfile;

  // Helper class
  class PeakOrCluster{
  public:
    
    PeakOrCluster()                            
      : fT(0), fE(0), fX(0), fY(0), fN(0), fC(0), fSC(0), fBE(0), fBX(0), fBY(0){}
    PeakOrCluster(int T, int E, int X, int Y) 
      : fT(T), fE(E), fX(X), fY(Y), fN(1), fC(1), fSC(0), fBE(E), fBX(X), fBY(Y){}
    PeakOrCluster(int T, int E, int N)        
      : fT(T), fE(E), fX(0), fY(0), fN(N), fC(0), fSC(0), fBE(E), fBX(0), fBY(0){}
    
    //get set methods
    int E() const {return fE;}
    int T() const {return fT;}
    int X() const {return fX;}
    int Y() const {return fY;}
    int N() const {return fN;}
    int C() const {return fC;}
    int SC() const {return fSC;}
    int BE() const {return fBE;}
    int BX() const {return fBX;}
    int BY() const {return fBY;}
    
    void SetE(int E){fE = E;}
    void SetT(int T){fT = T;}
    void SetX(int X){fX = X;}
    void SetY(int Y){fY = Y;}
    void SetN(int N){fN = N;}
    void SetC(int C){fC = C;}
    void SetSC(int SC){fSC = SC;}
    void SetBE(int BE){fBE = BE;}
    void SetBX(int BX){fBX = BX;}
    void SetBY(int BY){fBY = BY;}

    void AddE(int E){fE +=E;}
    void AddC(int C){fC +=C;}
    
  private:
    int fT; // time, 98ps
    int fE; // energy
    int fX; // x position of first merged peak
    int fY; // y position of first merged peak
    int fN; // number of peaks
    int fC; // number of peaks merged
    int fSC; // supercell ID
    int fBE; // energy of biggest peak merged here.
    int fBX; // X position of biggest peak merged here.
    int fBY; // Y position of biggest peak merged here.
  };
};

#endif
