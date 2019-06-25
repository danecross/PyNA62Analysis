// --------------------------------------------------------------
// History:
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - added the old (pre-2012) PMTs
//
// 2011-08-09 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - hit definition
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef CedarSD_h
#define CedarSD_h 1

#include "G4VSensitiveDetector.hh"
#include "CedarHit.hh"
#include "Rtypes.h"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class CedarSD : public G4VSensitiveDetector {

public:
  CedarSD (G4String name, G4String colName);
  ~CedarSD();
  
  void Initialize (G4HCofThisEvent*HCE);
  G4bool ProcessHits (G4Step*aStep,G4TouchableHistory*);
  void EndOfEvent (G4HCofThisEvent*HCE);
  void clear();
  void DrawAll();
  void PrintAll();

  // Quantum efficiencies of various PMTs
  Double_t QE                 (Double_t wavelength, Int_t PMType);
  Double_t QE_R7400U_03       (Double_t wavelength);
  Double_t QE_R9880U_110      (Double_t wavelength);
  Double_t QE_R9880U_210      (Double_t wavelength);
  Double_t QE_EMI_9820_QB_Lau (Double_t wavelength);
  Double_t QE_EMI_9820_QB     (Double_t wavelength);

private:
  CedarHitsCollection *fCollection;
  G4int iLGType;
  G4int nHits;
  int HCID;
};

#endif
