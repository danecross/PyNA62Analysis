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
// Created by Francesca Bucci (francesca.bucci@cern.ch) 2011-03-11
//
// --------------------------------------------------------------
#ifndef BeamTubeFins_H
#define BeamTubeFins_H 1

#include "NA62VComponent.hh"
#include "NA62VGeometryParameters.hh"
#include "globals.hh"

#include "G4OpticalSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class BeamTubeFins : public NA62VComponent
{

public:
  
  BeamTubeFins(G4int,G4Material*, NA62VGeometryParameters*, G4LogicalVolume*);
  ~BeamTubeFins();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  void DefineOpticalSurface();

public:

  G4OpticalSurface * GetOpticalSurface(){return fOpticalSurface;}
  void SetOpticalSurface(G4OpticalSurface * value){fOpticalSurface=value;}

  G4int                GetSegmentIndex()                                  { return fSegmentIndex;                    };
  void                 SetSegmentIndex(G4int value)                       { fSegmentIndex = value;                   };

  G4int                GetBeamPipeZPosition()                             { return fZPosition;                    };
  void                 SetBeamPipeZPosition(G4int value)                  { fZPosition = value;                   };

  G4double             GetZLength()                                       { return fZLength;                      };
  void                 SetZLength(G4double value)                         { fZLength = value;                     };
  G4double             GetInnerRadius()                                   { return fInnerRadius;                  };
  void                 SetInnerRadius(G4double value)                     { fInnerRadius = value;                 };
  G4double             GetOuterRadius()                                   { return fOuterRadius;                  };
  void                 SetOuterRadius(G4double value)                     { fOuterRadius = value;                 };
  G4double             GetSpacing()                                       { return fSpacing;                      };
  void                 SetSpacing(G4double value)                         { fSpacing = value;                     };
  G4double             GetBeamPipeZLengthWFins()                          { return fBeamPipeZLengthWFins;                      };
  void                 SetBeamPipeZLengthWFins(G4double value)            { fBeamPipeZLengthWFins = value;                     };

  NA62VGeometryParameters *  GetGeoPars() { return fGeoPars;};
  void  SetGeoPars(NA62VGeometryParameters * value) { fGeoPars = value;};

  
private:

  G4int     fSegmentIndex;

  G4double   fZPosition; 
  G4double   fZLength; 
  G4double   fInnerRadius;
  G4double   fOuterRadius;
  G4double   fSpacing;
  G4double   fBeamPipeZLengthWFins; 

  NA62VGeometryParameters* fGeoPars;

  G4OpticalSurface* fOpticalSurface;
};

#endif

