//
// --------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (Tommaso.Spadaro@cern.ch) 2017-01-18
//
// --------------------------------------------------------------
#ifndef ExoticProductionTable_H
#define ExoticProductionTable_H 1


#include "globals.hh"
#include "ExoticProductionMode.hh"

class ExoticProductionTable {
public:

  virtual ~ExoticProductionTable() {}

  static ExoticProductionTable* GetInstance();

private:

  static ExoticProductionTable* fInstance;

protected:

  ExoticProductionTable();

public:
  G4int GetNAvailableProductionProcesses(){return fAllProductionModes.size();} ///< Get number of possible production processes for all exotic particles
  ExoticProductionMode* GetProductionProcess(G4int); ///< Get i-th production process existing
  void SetCoupling(G4int coupling){fCoupling =coupling;};
private:
  
  G4int kAxion = 0;
  G4int kDarkPhoton = 2;
  G4int kHeavyNeutrino = 1;
  std::vector<ExoticProductionMode*> fAllProductionModes; 
  std::vector<G4int> fNProductionProcesses;
  G4double fCoupling;
 
};
#endif // ExoticProductionTable_H
