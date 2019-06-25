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
// 2011-03-23 Created by Spasimir Balev
//
// --------------------------------------------------------------------
#ifndef IRCBeamPipe_H
#define IRCBeamPipe_H 1

#include "NA62VComponent.hh"
#include "IRCGeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class IRCBeamPipe : public NA62VComponent
{

public:
  
  ~IRCBeamPipe();
  IRCBeamPipe(G4Material*, G4LogicalVolume*, G4Transform3D, G4double, G4double, G4double, G4double, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4int                GetiCopy()                                         { return fiCopy;                        };
  void                 SetiCopy(G4int value)                              { fiCopy = value;                       };
  G4Transform3D        GetTransform3D()                                   { return fTransform3D;                  };
  void                 SetTransform3D(G4Transform3D value)                { fTransform3D = value;                 };

  G4double             GetZLength()                                       { return fZLength;                      };
  void                 SetZLength(G4double value)                         { fZLength = value;                     };
  G4double             GetInnerRadius()                                   { return fInnerRadius;                  };
  void                 SetInnerRadius(G4double value)                     { fInnerRadius = value;                 };
  G4double             GetOuterRadius()                                   { return fOuterRadius;                  };
  void                 SetOuterRadius(G4double value)                     { fOuterRadius = value;                 };
  G4double             GetDisplacementX()                                 { return fDisplacementX;                };
  void                 SetDisplacementX(G4double value)                   { fDisplacementX = value;               };

private:

  G4int fiCopy;
  G4Transform3D fTransform3D;

  G4double fZLength;
  G4double fInnerRadius;
  G4double fOuterRadius;
  G4double fDisplacementX;
};

#endif
