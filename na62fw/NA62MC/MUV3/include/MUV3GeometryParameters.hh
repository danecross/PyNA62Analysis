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
// Created by:       Antonino Sergi (Antonino.Sergi@cern.ch) 
//                   Spasimir Balev (Spasimir.Balev@cern.ch)
//
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) March 2014
//
// --------------------------------------------------------------------
#ifndef MUV3GeometryParameters_H
#define MUV3GeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"

#include "NA62VGeometryParameters.hh"

class MUV3GeometryParameters : public NA62VGeometryParameters {

public:

  ~MUV3GeometryParameters() {}
  static MUV3GeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static MUV3GeometryParameters* fInstance;

protected:

  MUV3GeometryParameters();

public:

  G4double GetResponsibilityRegionXLength()             { return fResponsibilityRegionXLength; }
  void     SetResponsibilityRegionXLength(G4double val) { fResponsibilityRegionXLength = val;  }
  G4double GetResponsibilityRegionYLength()             { return fResponsibilityRegionYLength; }
  void     SetResponsibilityRegionYLength(G4double val) { fResponsibilityRegionYLength = val;  }
  G4double GetResponsibilityRegionZStart()              { return fResponsibilityRegionZStart;  }
  void     SetResponsibilityRegionZStart(G4double val)  { fResponsibilityRegionZStart = val;   }
  G4double GetResponsibilityRegionZEnd()                { return fResponsibilityRegionZEnd;    }
  void     SetResponsibilityRegionZEnd(G4double val)    { fResponsibilityRegionZEnd = val;     }

  G4double GetFiscZStart()            { return fFiscZStart;          }
  G4double GetFeWallZStart()          { return fFeWallZStart;        }
  G4double GetFrontPlateZStart()      { return fFrontPlateZStart;    }
  G4double GetScintillatorZStart()    { return fScintillatorZStart;  }
  G4double GetPMTZStart()             { return fPMTZStart;           }
  G4double GetBackPlateZStart()       { return fBackPlateZStart;     }

  G4double GetFrontPlateThickness()   { return fFrontPlateThickness; }
  G4double GetScintillatorZLength()   { return fScintillatorZLength; }
  G4double GetModuleZLength()         { return fModuleZLength;       }
  G4double GetBackPlateThickness()    { return fBackPlateThickness;  }
  G4double GetPMTWindowThickness()    { return fPMTWindowThickness;  } 
  G4double GetPMTWindowRadius()       { return fPMTWindowRadius;     }

  G4double GetActiveInnerRadius()     { return fActiveInnerRadius;   }
  G4double GetPassiveInnerRadius()    { return fPassiveInnerRadius;  }

  G4double GetFeWallThickness()       { return fFeWallThickness;     }
  G4double GetFeWallXSize()           { return fFeWallXSize;         }
  G4double GetFeWallYSize()           { return fFeWallYSize;         }
  G4double GetFeWallInnerRadius()     { return fFeWallInnerRadius;   }

  G4double GetGapWidth()              { return fGapWidth;            }

  G4int    GetNModulesX()             { return fNModulesX;           }
  G4int    GetNModulesY()             { return fNModulesY;           }
  G4double GetLargeModuleSize()       { return fLargeModuleSize;     }
  G4double GetSmallModuleSize()       { return fSmallModuleSize;     }

  G4double GetPMT1LargeX()            { return fPMT1LargeX;          }
  G4double GetPMT1LargeY()            { return fPMT1LargeY;          }
  G4double GetPMT2LargeX()            { return fPMT2LargeX;          }
  G4double GetPMT2LargeY()            { return fPMT2LargeY;          }
  G4double GetPMT1CornerX()           { return fPMT1CornerX;         }
  G4double GetPMT1CornerY()           { return fPMT1CornerY;         }
  G4double GetPMT2CornerX()           { return fPMT2CornerX;         }
  G4double GetPMT2CornerY()           { return fPMT2CornerY;         }
  G4double GetPMTSideX()              { return fPMTSideX;            }
  G4double GetPMTSideY()              { return fPMTSideY;            }

private:

  G4double fResponsibilityRegionXLength;
  G4double fResponsibilityRegionYLength;
  G4double fResponsibilityRegionZStart;
  G4double fResponsibilityRegionZEnd;

  G4double fFiscZStart;       ///< FISC 5 counter front Z position
  G4double fFeWallZStart;     ///< Iron wall fron Z position
  G4double fFrontPlateZStart;
  G4double fScintillatorZStart;
  G4double fPMTZStart;
  G4double fBackPlateZStart;

  G4double fFrontPlateThickness;
  G4double fScintillatorZLength;
  G4double fModuleZLength;
  G4double fBackPlateThickness;
  G4double fPMTWindowThickness;
  G4double fPMTWindowRadius;

  G4double fActiveInnerRadius;
  G4double fPassiveInnerRadius;

  G4double fFeWallThickness;
  G4double fFeWallXSize;
  G4double fFeWallYSize;
  G4double fFeWallInnerRadius;

  G4double fGapWidth;

  G4int    fNModulesX;
  G4int    fNModulesY;
  G4double fLargeModuleSize;
  G4double fSmallModuleSize;

  G4double fPMT1LargeX;
  G4double fPMT1LargeY;
  G4double fPMT2LargeX;
  G4double fPMT2LargeY;
  G4double fPMT1CornerX;
  G4double fPMT1CornerY;
  G4double fPMT2CornerX;
  G4double fPMT2CornerY;
  G4double fPMTSideX;
  G4double fPMTSideY;
};
#endif
