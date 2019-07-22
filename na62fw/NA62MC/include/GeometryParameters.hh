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
// 
// --------------------------------------------------------------
#ifndef GeometryParameters_H
#define GeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"

#include "NA62VGeometryParameters.hh"

class GeometryParameters {

public:

  ~GeometryParameters() {}
  static GeometryParameters* GetInstance();
  G4bool Check();
  TObjArray GetHashTable();
  void Print();

private:

  static GeometryParameters* fInstance;

protected:

  GeometryParameters();

public:

  G4double             GetWorldZLength()                                  { return fWorldZLength;           }
  void                 SetWorldZLength(G4double value)                    { fWorldZLength = value;          }
  G4double             GetWorldXLength()                                  { return fWorldXLength;           }
  void                 SetWorldXLength(G4double value)                    { fWorldXLength = value;          }
  G4double             GetWorldYLength()                                  { return fWorldYLength;           }
  void                 SetWorldYLength(G4double value)                    { fWorldYLength = value;          }

  std::vector<NA62VGeometryParameters*> 
                       GetSubDetectorsParameters()                        { return fSubDetectorsParameters; }
  void                 SetSubDetectorsParameters(std::vector<NA62VGeometryParameters*> &value)
  { fSubDetectorsParameters = value;      }

private:
  G4double fWorldZLength;
  G4double fWorldXLength;
  G4double fWorldYLength;
  std::vector<NA62VGeometryParameters*> fSubDetectorsParameters;
};
#endif
