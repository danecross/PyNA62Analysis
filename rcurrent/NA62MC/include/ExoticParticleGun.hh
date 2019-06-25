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
// Created by Tommaso Spadaro (Tommaso.Spadaro@cern.ch) 2017-01-18
//
// Edited by Cari Cesarotti Feb 2017
// --------------------------------------------------------------
#ifndef ExoticParticleGun_h
#define ExoticParticleGun_h 1

#include "KinePart.hh"
#include "G4ParticleGun.hh"
#include "ExoticProductionMode.hh"
#include "globals.hh"
#include "G4PhysicalConstants.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "G4VUserPrimaryGeneratorAction.hh"
#include "TH2D.h"
#include "ExoticProductionTable.hh"
#include "RandomGenerator.hh"
#include "TRandom3.h"
#include "RootIOManager.hh"
#include "TLorentzVector.h"
#include "ExoticParticleGunMessenger.hh"
#include "G4LorentzVector.hh"


class G4ParticleGun;

class ExoticParticleGun : public G4ParticleGun
{

public:

  ExoticParticleGun();
  ~ExoticParticleGun();
  void     GeneratePrimaries(G4Event*, KinePart*);
  void     AddExoticProductionMode(G4String);
  G4double GetDecayZMin()                        {return fDecayZmin;}
  void     SetDecayZMin(G4double val)            {fDecayZmin = val;}
  G4double GetDecayZMax()                        {return fDecayZmax;}
  void     SetDecayZMax(G4double val)            {fDecayZmax = val;}
  void     SetCouplingE(G4double coup)           {fCouplingE=coup;}
  void     SetCouplingM(G4double coup)           {fCouplingM=coup;}
  void     SetCouplingU(G4double coup)           {fCouplingU=coup;}
  void     TaxOrTarget(G4String&);
  void     PrintProdModes(); 
  void     AddDaughter(G4String);
  G4String GetDaughter(G4int j)                  {return daughters.at(j);}
  void     PropExotic(G4String);
  G4bool   GetMesonDaughter()                    {return fMesonDaughter;}
  void     SetMesonDaughter(G4bool val)          {fMesonDaughter = val;}
  
  G4bool SaveKineParts()                         {return fSaveKine;}
  
  TString EndProcName();
  TLorentzVector GetExoticMom()                  {return fExoticMom;}
  TString GetExoticName()                        {return fExoticName;}
  TLorentzVector GetExoticProdPos()              {return fExoticProdPos;}
  TLorentzVector GetExoticEndPos()               {return fExoticEndPos;}

  std::vector<G4ParticleDefinition*> daughtersList;
private:
  std::vector<G4String> fProductionModes;
  std::vector<ExoticProductionMode*> fExoticProdModes;
  void ReadAxionSpectrum(G4double mass);
  void ReadCharmedSpectrum();
  //G4double ProperTime(G4double, G4double, G4double); 
  G4double ProperTime(G4LorentzVector, G4double, G4double);
  G4bool DaughtersInAcceptance(G4ThreeVector, G4ThreeVector, G4ThreeVector);


private:
  
  G4PrimaryVertex* fVertex;

  G4double fTotalCrossSec;
  std::vector<G4String> daughters;

  G4bool fPropExotic;

  G4bool fSaveKine;
  
  TLorentzVector fExoticMom;
  TLorentzVector fExoticProdPos; 
  TLorentzVector fExoticEndPos;
  TString fExoticName;
  
  ExoticProductionTable* fExoticProdTable;
  G4ParticleTable* fParticleTable;

  G4double fCouplingE; //HNL to e
  G4double fCouplingM; //HNL to mu
  G4double fCouplingU; //A' to photon

  G4double fDecayZmin; 
  G4double fDecayZmax;

  G4bool fUseTarget; // true for generation at target, false for TAX
  G4bool fMesonDaughter; // 0 - no muon from parent meson, 1 - add muon to G4

  ExoticParticleGunMessenger *fMessenger;

  //For checking daughter acceptances, in mm
  G4double fZmagnet; 
  G4double fZfinal;
  G4double fRhoMin; 
  G4double fRhoMax;
  G4double fpKick; 

  G4ParticleDefinition* fPart1;
  G4ParticleDefinition* fPart2;

};

#endif
