// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-05-10
//
// ---------------------------------------------------------------

/// \class LKrFineCalibration
/// \Brief
/// Interface to the burst-by-burst positron mean E/p database (data)
/// \EndBrief
/// \Detailed
/// Used by the LKrClusterCorrections pre-analyzer to access the constants (a,b)
/// of the fine calibration for cluster energies, for data only.
/// The fine calibration factor is defined as f(E) = a+b/E, where the parameters
/// are technically burst-dependent (and in practice, run-dependent).
/// The fine calibration can be switched off (as required e.g. for
/// the computation of the corrections by LKrEopMonitor)
/// by reconfiguring LKrClusterCorrections.
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "LKrFineCalibration.hh"
#include "NA62ConditionsService.hh"
#include "NA62Global.hh"

using namespace std;

static LKrFineCalibration* fInstance = 0;

LKrFineCalibration* LKrFineCalibration::GetInstance() {
  if (!fInstance) fInstance = new LKrFineCalibration();
  return fInstance;
}

LKrFineCalibration::LKrFineCalibration() : fFileName("LKr-FineCalibration.dat"), fRunID(-1) {}

Double_t LKrFineCalibration::GetEnergyScaling(Int_t RunID, Int_t BurstID) {
  if (RunID != fRunID) { // new run: read a set of calibration constants
    fRunID = RunID;
    ParseInputFile();
  }
  return fEnergyScaling[BurstID];
}

Double_t LKrFineCalibration::GetEnergyOffset(Int_t RunID, Int_t BurstID) {
  if (RunID != fRunID) { // new run: read a set of calibration constants
    fRunID = RunID;
    ParseInputFile();
  }
  return fEnergyOffset[BurstID];
}

////////////////////////////////
// Read the LKr calibration file

void LKrFineCalibration::ParseInputFile() {

  for (Int_t i=0; i<NBURMAX_LKRFINECALIB; i++) {
    fEnergyScaling[i] = 1.0;
    fEnergyOffset[i] = 0.0;
  }

  if (NA62ConditionsService::GetInstance()->Open(fFileName)!=kSuccess) {
    cout << "[LKrFineCalibration] Error opening input file, fine corrections not applied" << endl;
    return;
  }

  for (Int_t i=0; i<NBURMAX_LKRFINECALIB; i++) fEnergyScaling[i] = fEnergyOffset[i] = -999.0;

  TString Line;
  Int_t NTotalEntriesRead = 0, NEntriesRead = 0;
  Int_t MinBurst = NBURMAX_LKRFINECALIB-1, MaxBurst = 0;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t    Run   = ((TObjString*)(l->At(0)))->GetString().Atoi();
    Int_t    Burst = ((TObjString*)(l->At(1)))->GetString().Atoi();
    Double_t EoP   = ((TObjString*)(l->At(2)))->GetString().Atof();
    Double_t dEoP  = ((TObjString*)(l->At(3)))->GetString().Atof();
    Double_t p0    = ((TObjString*)(l->At(4)))->GetString().Atof();
    //Double_t dp0   = ((TObjString*)(l->At(5)))->GetString().Atof();
    Double_t p1    = ((TObjString*)(l->At(6)))->GetString().Atof();
    //Double_t dp1   = ((TObjString*)(l->At(7)))->GetString().Atof();
    delete l;
    if (Run!=fRunID) continue;
    NTotalEntriesRead++;
    if (Burst<0 || Burst>=NBURMAX_LKRFINECALIB) continue;
    if (EoP<0.95 || EoP>1.05) continue; // something is wrong
    if (dEoP>0.005) continue; // poor accuracy on E/p measurement
    fEnergyScaling[Burst] = p0;
    fEnergyOffset[Burst] = p1;
    NEntriesRead++;
    if (Burst<MinBurst) MinBurst = Burst;
    if (Burst>MaxBurst) MaxBurst = Burst;
  }
  NA62ConditionsService::GetInstance()->Close(fFileName);
  cout << "[LKrFineCalibration] " <<
    (NEntriesRead?Form("Found %d",NEntriesRead):"No") << " valid " <<
    ((NEntriesRead==1)?"entry":"entries") << " for run " << fRunID << endl;

  if (NEntriesRead) {
    // Missing calibrations for bursts out of range
    for (Int_t i=0; i<MinBurst; i++) {
      fEnergyScaling[i] = fEnergyScaling[MinBurst];
      fEnergyOffset[i] = fEnergyOffset[MinBurst];
    }
    for (Int_t i=MaxBurst+1; i<NBURMAX_LKRFINECALIB; i++) {
      fEnergyScaling[i] = fEnergyScaling[MaxBurst];
      fEnergyOffset[i] = fEnergyOffset[MaxBurst];
    }
    // Missing calibrations for bursts inside the range
    for (Int_t i=MinBurst+1; i<=MaxBurst-1; i++) {
      if (fEnergyScaling[i]<0.0) {
	fEnergyScaling[i] = fEnergyScaling[i-1];
	fEnergyOffset[i] = fEnergyOffset[i-1];
      }
    }
  }
  else { // no input found: the default calibration
    for (Int_t i=0; i<NBURMAX_LKRFINECALIB; i++) {
      fEnergyScaling[i] = 1.0;
      fEnergyOffset[i] = 0.0;
    }
  }
}

void LKrFineCalibration::Print() {
  if (fRunID<0) {
    cout << "[LKrFineCalibration::Print] No calibrations loaded" << endl;
    return;
  }
  cout << "[LKrFineCalibration] Constants are loaded for run " << fRunID << endl;
  for (Int_t i=0; i<NBURMAX_LKRFINECALIB; i++) {
    cout << "[LKrFineCalibration] Burst " << i << " EnergyScaling= " << fEnergyScaling[i] << " EnergyOffset= " << fEnergyOffset[i] << endl;
  }
}
