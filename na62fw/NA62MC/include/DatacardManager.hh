#ifndef DatacardManager_H
#define DatacardManager_H 1

#include "MCTruthManager.hh"
#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
#include "G4ThreeVector.hh"
#include "globals.hh"
#include <list>

class DatacardMessenger;

class DatacardManager {

public:
  virtual ~DatacardManager();
  static DatacardManager* GetInstance();

private:
  static DatacardManager* fInstance;
  DatacardMessenger *datacardMessenger;

protected:
  DatacardManager();

public:
  void     SetMessenger();
  G4String GetOutputFileName()                    { return fOutputFileName;       }
  void     SetOutputFileName(G4String val)        { fOutputFileName = val;        }
  TString  GetTurtleDatacardFileName();
  G4bool   GetDecayForce()                        { return fDecayForce;           }
  void     SetDecayForce(G4bool val)              { fDecayForce = val;            }
  G4bool   GetMuonDecayForce()                    { return fMuonDecayForce;       }
  void     SetMuonDecayForce(G4bool val)          { fMuonDecayForce = val;        }
  G4int    GetPionDecayForce()                    { return fPionDecayForce;       }
  void     SetPionDecayForce(G4int val)           { fPionDecayForce = val;        }
  G4int    GetDecayType()                         { return fDecayType;            }
  void     SetDecayType(G4int val)                { fDecayType = val;             }
  G4int    GetDecayRadcor()                       { return fDecayRadcor;          }
  void     SetDecayRadcor(G4int val)              { fDecayRadcor = val;           }

  G4int    GetDecayPizeroDecay()
  { return fDecayPizeroDecay[0] + 100*fDecayPizeroDecay[1] + 10000*fDecayPizeroDecay[2]; }
  G4ThreeVector GetDecayPizeroDecayVector()
  { return fDecayPizeroDecay; }
  void     SetDecayPizeroDecay(G4ThreeVector val)    { fDecayPizeroDecay = val;          }

  G4double GetTwoPhotonsMaxAngle()                   { return fTwoPhotonsMaxAngle;       }
  void     SetTwoPhotonsMaxAngle(G4double val)       { fTwoPhotonsMaxAngle = val;        }
  G4double GetRadiativePhotonMinEnergy()             { return fRadiativePhotonMinEnergy; }
  void     SetRadiativePhotonMinEnergy(G4double val) { fRadiativePhotonMinEnergy = val;  }
  G4double GetRadiativePhotonMaxEnergy()             { return fRadiativePhotonMaxEnergy; }
  void     SetRadiativePhotonMaxEnergy(G4double val) { fRadiativePhotonMaxEnergy = val;  }
  G4double GetLeptonPhotonMinAngle()                 { return fLeptonPhotonMinAngle;     }
  void     SetLeptonPhotonMinAngle(G4double val)     { fLeptonPhotonMinAngle = val;      }
  G4double GetMinTracksMomentum()                    { return fMinTracksMomentum;        }
  void     SetMinTracksMomentum(G4double val)        { fMinTracksMomentum = val;         }

  G4double GetDecayZmin()                            { return fDecayZMin;            }
  void     SetDecayZmin(G4double val)                { fDecayZMin = val;             }
  G4double GetDecayZmax()                            { return fDecayZMax;            }
  void     SetDecayZmax(G4double val)                { fDecayZMax = val;             }
  G4double GetDecayPath()                            { return fDecayZMax-fDecayZMin; }
  G4int    GetDecayVerbose()                         { return fDecayVerbose;         }
  void     SetDecayVerbose(G4int val)                { fDecayVerbose = val;          }
  G4int    GetRandDecaySeed()                        { return fRandDecaySeed;        }
  void     SetRandDecaySeed(G4int val)               { fRandDecaySeed = val;         }
  G4int    GetRunNumber()                            { return fRunNumber;            }
  void     SetRunNumber(G4int val);
  G4int    GetRunBeamOn()                            { return fRunBeamOn;            }
  void     SetRunBeamOn(G4int val)                   { fRunBeamOn = val;             }

  // Run-dependent beam tuning parameters
  Double_t GetBeamAlignmentX();
  Double_t GetBeamAlignmentY();
  Double_t GetBeamMomentumScaleFactor();
  Double_t GetBend6FieldScaleFactor();
  Double_t GetTrim5FieldScaleFactor();
  Double_t GetQuad09FieldScaleFactor();
  Double_t GetQuad10FieldScaleFactor();

  void SetSaveTracksForRegeneration(G4int val)
  { MCTruthManager::GetInstance()->GetConfig()->SetSaveTracksForRegeneration(val); }
  void SetSaveAllTracks(G4int val)
  { MCTruthManager::GetInstance()->GetConfig()->SetSaveAllTracks(val); }
  void SetSaveAllSteps(G4int val)
  { MCTruthManager::GetInstance()->GetConfig()->SetSaveAllSteps(val); }
  void AddBeamParticleEndProcessToReject(G4String val)
  { MCTruthManager::GetInstance()->GetConfig()->AddBeamParticleEndProcessToReject(val); }
  void SetKineZMin(G4double val)
  { MCTruthManager::GetInstance()->GetConfig()->SetZMin(val); }
  void SetKineZMax(G4double val)
  { MCTruthManager::GetInstance()->GetConfig()->SetZMax(val); }
  void AddParticleTypeToSave(G4String val)
  { MCTruthManager::GetInstance()->GetConfig()->AddParticleTypeToSave(val); }
  void AddCreatorProcessToSave(G4String val)
  { MCTruthManager::GetInstance()->GetConfig()->AddParticleCreatorProcessToSave(val); }
  void AddEndProcessToSave(G4String val)
  { MCTruthManager::GetInstance()->GetConfig()->AddParticleEndProcessToSave(val); }
  void AddParticleTypeToReject(G4String val)
  { MCTruthManager::GetInstance()->GetConfig()->AddParticleTypeToReject(val); }
  void AddCreatorProcessToReject(G4String val)
  { MCTruthManager::GetInstance()->GetConfig()->AddParticleCreatorProcessToReject(val); }
  void AddEndProcessToReject(G4String val)
  { MCTruthManager::GetInstance()->GetConfig()->AddParticleEndProcessToReject(val); }
  void SetKineEMin(G4double val)
  { MCTruthManager::GetInstance()->GetConfig()->SetEMin(val); }
  void SetMaxIntLevel(G4int val)
  { MCTruthManager::GetInstance()->GetConfig()->SetMaxInteractionLevel(val); }
  void SetSaveVerbose(G4int val)
  { MCTruthManager::GetInstance()->GetConfig()->SetSaveTrackVerbose(val); }

  G4int    GetFastSimuSchemeStep()  { return fFastSimuSchemeStep; }
  void     SetFastSimuSchemeStep(G4int val);
  G4String GetFastSimuSchemeInput() { return fFastSimuSchemeFileName; }
  void     SetFastSimuSchemeInput(G4String val);
  G4int    GetFastSimulationMode()  { return fFastSimulationMode; }
  void     SetFastSimulationMode(G4int val);

private:
  G4String      fOutputFileName;
  G4String      fTurtleDatacardFileName;
  G4bool        fDecayForce;
  G4bool        fMuonDecayForce;
  G4int         fPionDecayForce;
  G4int         fDecayType;
  G4int         fDecayRadcor;
  G4ThreeVector fDecayPizeroDecay;
  G4double      fTwoPhotonsMaxAngle; ///< Maximum angle between photon pairs for biased MC
  G4double      fRadiativePhotonMinEnergy;
  G4double      fRadiativePhotonMaxEnergy;
  G4double      fLeptonPhotonMinAngle; ///< Minimum lepton-photon angle (for biased MC)
  G4double      fMinTracksMomentum;    ///< Minimum sum of momenta of all charged tracks (for biased MC) [MeV]
  G4double      fDecayZMin; ///< [mm]
  G4double      fDecayZMax; ///< [mm]
  G4int         fDecayVerbose;
  G4int         fRandDecaySeed;
  G4int         fRunNumber; ///< Run number defines MC tuning: 0 means default MC parameters
  G4int         fRunBeamOn; ///< Number of events in the single job (defined in the macro by the command /run/beamOn)
  G4int         fFastSimuSchemeStep;
  G4String      fFastSimuSchemeFileName;
  G4int         fFastSimulationMode;
};

#endif
