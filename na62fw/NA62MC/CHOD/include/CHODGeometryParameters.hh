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
// 2015-10-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - NewCHOD moved into a separate detector
//
// 2014-03-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - NewCHOD simulation added
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-02
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#ifndef CHODGeometryParameters_H
#define CHODGeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"
#include "NA62VGeometryParameters.hh"

class CHODGeometryParameters : public NA62VGeometryParameters {

public:
  ~CHODGeometryParameters();
  static CHODGeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print() {}

private:
  static CHODGeometryParameters* fInstance;

protected:
  CHODGeometryParameters();

public:

  G4double GetRespRegionZStart () { return fRespRegionZStart;  }
  G4double GetRespRegionZEnd   () { return fRespRegionZEnd;    }
  G4double GetRespRegionZCentre() { return fRespRegionZCentre; }
  G4double GetRespRegionXLength() { return fRespRegionXLength; }
  G4double GetRespRegionYLength() { return fRespRegionYLength; }
  G4double GetRespRegionZLength() { return fRespRegionZLength; }

  G4int    GetNPlanes()                          { return  fNPlanes;               }
  void     SetNPlanes(G4int val)                 { fNPlanes = val;                 }
  G4int    GetNQuadrants()                       { return  fNQuadrants;            }
  void     SetNQuadrants(G4int val)              { fNQuadrants = val;              }
  G4double GetTransverseSize()                   { return fTransverseSize;         }
  void     SetTransverseSize(G4double val)       { fTransverseSize = val;          }

  G4double GetDetectorZPositionVer()             { return fDetectorZPositionVer;   }
  void     SetDetectorZPositionVer(G4double val) { fDetectorZPositionVer = val;    }
  G4double GetDetectorZPositionHor()             { return fDetectorZPositionHor;   }
  void     SetDetectorZPositionHor(G4double val) { fDetectorZPositionHor = val;    }
  G4double GetDetectorZRotationVer()             { return fDetectorZRotationVer;   }
  void     SetDetectorZRotationVer(G4double val) { fDetectorZRotationVer = val;    }
  G4double GetDetectorZRotationHor()             { return fDetectorZRotationHor;   }
  void     SetDetectorZRotationHor(G4double val) { fDetectorZRotationHor = val;    }

  G4double GetInnerRadius()                      { return fInnerRadius;            }
  void     SetInnerRadius(G4double val)          { fInnerRadius = val;             }
  G4double GetOuterRadius()                      { return fOuterRadius;            }
  void     SetOuterRadius(G4double val)          { fOuterRadius = val;             }
  G4double GetScintThickness()                   { return fScintThickness;         }
  void     SetScintThickness(G4double val)       { fScintThickness = val;          }

  G4int    GetNCounters()                        { return fNCounters;              }
  void     SetNCounters(G4int val)               { fNCounters = val;               }
  G4ThreeVector GetScintSize(G4int i)            { return fScintSize[i];           }
  G4ThreeVector GetScintPosition(G4int i)        { return fScintPosition[i];       }

private:

  G4double fRespRegionZStart;
  G4double fRespRegionZEnd;
  G4double fRespRegionZCentre;
  G4double fRespRegionXLength;
  G4double fRespRegionYLength;
  G4double fRespRegionZLength;

  G4int    fNPlanes;
  G4int    fNQuadrants;
  G4double fTransverseSize;
  G4double fDetectorZPositionVer;
  G4double fDetectorZPositionHor;
  G4double fDetectorZRotationVer;
  G4double fDetectorZRotationHor;

  G4double fInnerRadius;
  G4double fOuterRadius;
  G4double fScintThickness;

  G4int fNCounters;
  G4ThreeVector fScintSize[100];
  G4ThreeVector fScintPosition[100];
};

#endif
