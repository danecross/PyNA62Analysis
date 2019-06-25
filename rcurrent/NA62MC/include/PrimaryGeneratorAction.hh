//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// Modified by Sergey Podolsky 2011-01-21
// Modified by Giuseppe Ruggiero 2012-01-30
// Modified by Sergey Podolsky 2013-10-25
// Modified by Nicolas Lurkin 2016-07-25
// Modified by Cari Cesarotti 2017-02-05
// --------------------------------------------------------------

#ifndef PrimaryGeneratorAction_h
#define PrimaryGeneratorAction_h 1

#include "globals.hh"
#include "G4LorentzVector.hh"
#include "G4VUserPrimaryGeneratorAction.hh"
#include "G4Event.hh"
#include "EventAction.hh"
#include "MatrixGun.hh"
#include "TH2D.h"
#include "TF2.h"
#include "ExoticParticleGun.hh"

struct ExternalParticlesArray {
  vector<int> pdg_code;
  vector<double> x;
  vector<double> y;
  vector<double> z;
  vector<double> px;
  vector<double> py;
  vector<double> pz;
};

class DetectorConstruction;
class G4ParticleTable;
class G4GeneralParticleSource;
class G4ParticleGun;
class FastBeam;
class PrimaryGeneratorMessenger;
class G4CMCDecayer;

class PrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction {

public:

  PrimaryGeneratorAction(DetectorConstruction*, EventAction& );
  ~PrimaryGeneratorAction();
  void GeneratePrimaries(G4Event* anEvent);
  void ReadCharmSpectrum();
  G4double BranchingRatioMesonToHN(G4double*, G4double*);
  void InitializeExternalParticlesArray();
  ExternalParticlesArray fExternalParticlesArray;
  G4bool fExternalParticlesInitialized;

  G4int    GetBeamType() const              { return fBeamType;      }
  void     SetBeamType(G4int value)         { fBeamType = value;     }
  G4String GetParticleName() const          { return fParticleName;  }
  void     SetParticleName(G4String value)  { fParticleName = value; }
  G4String GetDatacardName()                { return fDatacardName;  }
  void     SetDatacardName(G4String value)  { fDatacardName = value; }
  void     SetInputFileName(G4String value); ///< for the replay mode
  void     SetExoticProdMode(G4int value)   { fExoticProdMode = value; }
  // external particle beam mode
  G4String GetExternalParticlesFileName()                    { return fExternalParticlesFileName;    }
  void     SetInputExternalParticlesFileName(G4String value) { fExternalParticlesFileName = value;   }
  
  void     SetG4BeamLineInputFileName(G4String  value);       
  void     SetG4BeamStartZPosition(G4String value)           { fG4BeamStartZPosition = value;        }
private:

  G4ParticleTable* fParticleTable;
  G4GeneralParticleSource* fParticleGun;
  PrimaryGeneratorMessenger* fMessenger;

  G4int    fBeamType;
  G4String fParticleName; ///< Turtle particle name (using Geant4 convension)
  G4String fDatacardName; ///< Turtle datacard name

  G4int    fRunNumber;  ///< Run number: beamline/detector alignment depends on it
  G4double fAlignmentX; ///< Beamlime/detector x alignment
  G4double fAlignmentY; ///< Beamlime/detector y alignment
  G4double fMomentumScaleFactor; ///< Scaling of beam particle momentum at Turtle/G4 handover

  G4int fExoticProdMode; //Number of exotic production mode

  DetectorConstruction* myDetector;
  EventAction* fMyEventAct;
  FastBeam* fTurtleBeam; ///< Turtle charged particle beam generator
  G4ParticleDefinition *fParticleDef; ///< Used for Turtle generation
  G4LorentzVector f4Momentum;
  G4double ProperTime(G4double, G4double);
  G4CMCDecayer* fDecay;

  // Variables for external particle beam generation
  ifstream       fExternalParticlesFile;
  G4String       fExternalParticlesFileName;
  
  // G4BeamLine related variables
  G4String           fG4BeamStartZPosition; ///< Beam starting point
  G4String           fG4FileName;           ///< Input file name
  TFile*             fG4BeamLineOut;        ///< Input file
  TTree*             fG4Beam;               ///< Input TTree

  MatrixGun*     fMGun;     ///< Matrix gun generator
  G4ParticleGun* fAxionGun; ///< Axion-on-target generator
  G4ParticleGun* fExoticDaughterGun1; ///< HNL two-body-decay daughters generator
  G4ParticleGun* fExoticDaughterGun2;
  ExoticParticleGun* fExoticPartGun;
  G4int fExoticCounter;

  TFile *inputEventFile;
  G4int fCurrentInputEvent;
  G4int fMaxInputEvent;
  TBranch* fNKinePartsBr;
  TBranch* fKinePartsBr;
  TClonesArray* fKinePartsInput;

  //TH2D fHAxionPtPl;
  TH2D fHDPlusPtPl;
  TH2D fHDMinusPtPl;
  TH2D fHDSPlusPtPl;
  TH2D fHDSMinusPtPl;
  TH2D fHD0PtPl;
  TH2D fHD0BarPtPl;

  // variables for the generation of K3pi with 2 forced pion decays
  G4double fZmax;
  G4double fPionTau;

  // Some quantities for the HNL mode

  // Masses
  G4double fMe;
  G4double fMmu;
  G4double fMtau;
  G4double fMpi;
  G4double fMpi0;
  G4double fMrho;
  G4double fMrho0;
  G4double fMeta;
  G4double fMetaprime;
  G4double fMD;
  G4double fMDS;
  G4double fMD0;
  G4double fMK;
  G4double fMK0;
  G4double fMp;
  G4double fMKStar;
  G4double fMK0Star;

  // Lifetimes
  G4double fDlife;
  G4double fDSlife;
  G4double fD0life;
  G4double ftaulife;

  // Constants
  G4double fhc;
  G4double fGF;
  G4double fPi;
  G4double fRho;
  G4double fD;
  G4double fDS;
  G4double fK;
  G4double fEta;
  G4double fEtaprime;
  G4double fsigmacc;

  // CKM
  G4double fVcs;
  G4double fVcd;
  G4double fVud;
  G4double fVus;

  // Form factors, pseudoscalar and vector mesons
  G4double fDK0;
  G4double fDpi0;
  G4double fD0K;
  G4double fD0pi;
  G4double fgDK0;
  G4double fgDpi0;
  G4double fgD0K;
  G4double fgD0pi;
  G4double fA0D;
  G4double fA1D;
  G4double fA2D;
  G4double fVD;
  G4double fA0D0;
  G4double fA1D0;
  G4double fA2D0;
  G4double fVD0;

  // Fragmentation fractions
  G4double ffD;
  G4double ffD0;
  G4double ffDS;

  // NA62 parameters
  G4double fpMom;
  G4double fBeA;
  G4double fBeDensity;
  G4double fpBeLambda;
  G4double ftargetLength;
  G4double fCuA;
  G4double fCuDensity;
  G4double fpCuLambda;
  G4double fTAXLength;
  G4double fTAXDistance;
  G4double fbeamLength;

  // Other parameters
  static constexpr G4int fNSpecies = 6;
  G4double fPTotal[fNSpecies];
  G4double fmesonMass[fNSpecies];
  G4double fmesonTau[fNSpecies];
  G4bool fReadCharmSpectrum;

  void InitializeExoticDaughter(TRandom3* RandomDecay);
  //void ReadAxionSpectrum();
  void GenerateKLMomentum();
  G4LorentzVector GenerateKLPositionTime();

  G4double PhaseSpace(G4double, G4double, G4double);
  G4double PhaseSpaceFactor(G4double, G4double, G4double);
  G4double TwoBodyBR(G4double, G4double, G4double, G4int);
  G4double ThreeBodyBR(G4double, G4double, G4double, G4double, G4bool, G4bool, G4double, G4double, G4int);
  std::string ThreeBodyFunction(G4double, G4double);
  static void minFunctionStatic(int&, double*, double&, double*, int);
  G4double lambda(G4double, G4double, G4double);
  void GenerateHeavyNeutrino(G4Event* anEvent, TRandom3* RandomDecay);
  G4double MaxDecayProb(double p, G4double m, G4double tau); 
  void ForcedDecay(G4PrimaryParticle *G4BeamParticle, G4ThreeVector Pos);
};

#endif
