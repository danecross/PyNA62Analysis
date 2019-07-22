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
// Modified by Francesca Bucci (francesca.bucci@cern.ch) 2011-03-11 
//    to include methods to access BeamPipe members.
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef NA62VGeometryParameters_H
#define NA62VGeometryParameters_H 1

#include "globals.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"
#include <vector>
#include "ResponsibilityRegion.hh"
#include "NA62VNamed.hh"

class NA62VGeometryParameters : public NA62VNamed {

public:
  
  explicit NA62VGeometryParameters(G4String);
  ~NA62VGeometryParameters(){};

  std::vector<ResponsibilityRegion*> 
                       GetResponsibilityRegion()                          { return fResponsibilityRegion;         }
  void                 SetResponsibilityRegion(std::vector<ResponsibilityRegion*> &value)
                                                                          { fResponsibilityRegion = value;        }
 
  G4double             GetBeamPipeZPosition(G4int value)                  { return fBeamPipeZPosition[value];     }

  G4double             GetBeamPipeZLength(G4int value)                    { return fBeamPipeZLength[value];       }
  G4double             GetBeamPipeInnerRadius(G4int value)                { return fBeamPipeInnerRadius[value];   }
  G4double             GetBeamPipeOuterRadius(G4int value)                { return fBeamPipeOuterRadius[value];   }
  
  G4double             GetBeamPipeInputDisplacementWRTBeam(G4int value)           { return fBeamPipeInputDisplacementWRTBeam[value];        }
  G4double             GetBeamPipeOutputDisplacementWRTBeam(G4int value)          { return fBeamPipeOutputDisplacementWRTBeam[value];       }
  
  G4double             GetBeamPipeFinZLength(G4int value)                 { return fBeamPipeFinZLength[value];              }

  G4double             GetBeamPipeFinOuterRadius(G4int value)             { return fBeamPipeFinOuterRadius[value];          }

  G4double             GetBeamPipeFinSpacing(G4int value)                 { return fBeamPipeFinSpacing[value];              }

  G4double             GetBeamPipeZLengthWFins(G4int value)               { return fBeamPipeZLengthWFins[value];            }

protected:

  std::vector<ResponsibilityRegion*> fResponsibilityRegion;

  G4double fBeamPipeZPosition[15];
  G4double fBeamPipeZLength[15];
  G4double fBeamPipeInnerRadius[15];
  G4double fBeamPipeOuterRadius[15];

  G4double fBeamPipeInputDisplacementWRTBeam[15];
  G4double fBeamPipeOutputDisplacementWRTBeam[15];

  G4double fBeamPipeFinZLength[15];
  G4double fBeamPipeFinOuterRadius[15];
  G4double fBeamPipeFinSpacing[15];
  G4double fBeamPipeZLengthWFins[15];
};

#endif
