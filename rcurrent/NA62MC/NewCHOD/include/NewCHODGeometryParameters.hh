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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// --------------------------------------------------------------------
#ifndef NewCHODGeometryParameters_H
#define NewCHODGeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"
#include "NA62VGeometryParameters.hh"

class NewCHODGeometryParameters : public NA62VGeometryParameters {

public:

  ~NewCHODGeometryParameters() {}
  static NewCHODGeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static NewCHODGeometryParameters* fInstance;

protected:

  NewCHODGeometryParameters();

public:

  G4double GetRespRegionZStart () { return fRespRegionZStart;  }
  G4double GetRespRegionZEnd   () { return fRespRegionZEnd;    }
  G4double GetRespRegionZCentre() { return fRespRegionZCentre; }
  G4double GetRespRegionXLength() { return fRespRegionXLength; }
  G4double GetRespRegionYLength() { return fRespRegionYLength; }
  G4double GetRespRegionZLength() { return fRespRegionZLength; } 

  G4double GetFiberglassThickness()                { return fFiberglassThickness;    }
  void     SetFiberglassThickness(G4double val)    { fFiberglassThickness = val;     }
  G4double GetScintThickness()                     { return fScintThickness;         }
  void     SetScintThickness(G4double val)         { fScintThickness = val;          }
  G4double GetAlWindowThickness()                  { return fAlWindowThickness;      }
  void     SetAlWindowThickness(G4double val)      { fAlWindowThickness = val;       }
  G4double GetHoneycombThickness()                 { return fHoneycombThickness;     }
  void     SetHoneycombThickness(G4double val)     { fHoneycombThickness = val;      }
  G4double GetHoneycombSkinThickness()             { return fHoneycombSkinThickness; }
  void     SetHoneycombSkinThickness(G4double val) { fHoneycombSkinThickness = val;  }
  G4double GetHoneycombAlThickness()               { return fHoneycombAlThickness;   }
  void     SetHoneycombAlThickness(G4double val)   { fHoneycombAlThickness = val;    }

  G4double GetInnerRadius()                       { return fInnerRadius;           }
  void     SetInnerRadius(G4double val)           { fInnerRadius = val;            }
  G4double GetOuterRadius()                       { return fOuterRadius;           }
  void     SetOuterRadius(G4double val)           { fOuterRadius = val;            }
  G4double GetHoneycombInnerRadius()              { return fHoneycombInnerRadius;  }
  void     SetHoneycombInnerRadius(G4double val)  { fHoneycombInnerRadius = val;   }
  G4double GetHoneycombOuterRadius()              { return fHoneycombOuterRadius;  }
  void     SetHoneycombOuterRadius(G4double val)  { fHoneycombOuterRadius = val;   }
  G4double GetFiberglassOuterRadius()             { return fFiberglassOuterRadius; }
  void     SetFiberglassOuterRadius(G4double val) { fFiberglassOuterRadius = val;  }
  G4double GetZPosition()                         { return fZPosition;             }
  void     SetZPosition(G4double val)             { fZPosition = val;              }
  G4double GetScintZPosition1()                   { return fScintZPosition1;       }
  void     SetScintZPosition1(G4double val)       { fScintZPosition1 = val;        }
  G4double GetScintZPosition2()                   { return fScintZPosition2;       }
  void     SetScintZPosition2(G4double val)       { fScintZPosition2 = val;        }
  G4double GetAlWindowZPosition1()                { return fAlWindowZPosition1;    }
  void     SetAlWindowZPosition1(G4double val)    { fAlWindowZPosition1 = val;     }
  G4double GetAlWindowZPosition2()                { return fAlWindowZPosition2;    }
  void     SetAlWindowZPosition2(G4double val)    { fAlWindowZPosition2 = val;     }
  G4double GetHoneycombZPosition()                { return fHoneycombZPosition;    }
  void     SetHoneycombZPosition(G4double val)    { fHoneycombZPosition = val;     }

  G4int         GetNCounters()            { return fNCounters;        }
  void          SetNCounters(G4int val)   { fNCounters = val;         }
  G4ThreeVector GetScintSize()            { return fScintSize;        }
  G4ThreeVector GetScintPosition(G4int i) { return fScintPosition[i]; }
  G4int         GetScintMap(G4int i)      { return fScintMap[i];      }

private:

  G4double fRespRegionZStart;
  G4double fRespRegionZEnd;
  G4double fRespRegionZCentre;
  G4double fRespRegionXLength;
  G4double fRespRegionYLength;
  G4double fRespRegionZLength;

  G4double fFiberglassThickness;
  G4double fScintThickness;
  G4double fAlWindowThickness;
  G4double fHoneycombThickness;
  G4double fHoneycombSkinThickness;
  G4double fHoneycombAlThickness;

  G4double fInnerRadius;
  G4double fOuterRadius;
  G4double fHoneycombInnerRadius;
  G4double fHoneycombOuterRadius;  ///< apothem of a regular octagon
  G4double fFiberglassOuterRadius; ///< apothem of a regular octagon

  G4double fZPosition;
  G4double fScintZPosition1;
  G4double fScintZPosition2;
  G4double fAlWindowZPosition1;
  G4double fAlWindowZPosition2;
  G4double fHoneycombZPosition;

  G4int         fNCounters;
  G4ThreeVector fScintSize;
  G4ThreeVector fScintPosition[100];
  G4int         fScintMap[100];
};

#endif
