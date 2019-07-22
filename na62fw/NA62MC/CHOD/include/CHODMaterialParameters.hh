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
// --------------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#ifndef CHODMaterialParameters_H
#define CHODMaterialParameters_H 1

#include "globals.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"
#include "TObjArray.h"

class CHODMaterialParameters {

public:

  ~CHODMaterialParameters() {}
  static CHODMaterialParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static CHODMaterialParameters* fInstance;

protected:

  CHODMaterialParameters();
  void ReadPMsData(G4int);
  // Method to define materials used for detector
  void DefineMaterials();
  void SetMaterialProperties();

public:

  G4int GetMaterialPropertiesNEntries()            { return fMaterialPropertiesNEntries;  }
  void  SetMaterialPropertiesNEntries(G4int value) { fMaterialPropertiesNEntries = value; }

private:

  G4int fMaterialPropertiesNEntries;

};
#endif