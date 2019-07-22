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
// Created by   Antonino Sergi (Antonino.Sergi@cern.ch) 
//              Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------
#ifndef SACDetector_H
#define SACDetector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "SACGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class SACPlane;
class SACGeometryParameter;

class SACDetector : public NA62VComponent, public NA62VNamed
{

public:

  ~SACDetector() {}
  SACDetector(G4Material*, G4LogicalVolume*);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4int                GetSACSimulationMode()                              {return fSACSimulationMode;                  }
  void                 SetSACSimulationMode(G4int value)                   {fSACSimulationMode = value;                 }
  G4double             GetSACResponsibilityRegionXLength()                 {return fSACResponsibilityRegionXLength;     }
  void                 SetSACResponsibilityRegionXLength(G4double value)   {fSACResponsibilityRegionXLength = value;    }
  G4double             GetSACResponsibilityRegionYLength()                 {return fSACResponsibilityRegionYLength;     }
  void                 SetSACResponsibilityRegionYLength(G4double value)   {fSACResponsibilityRegionYLength = value;    }
  G4double             GetSACResponsibilityRegionZBeginning()              {return fSACResponsibilityRegionZBeginning;  }
  void                 SetSACResponsibilityRegionZBeginning(G4double value){fSACResponsibilityRegionZBeginning = value; }
  G4double             GetSACResponsibilityRegionZEnd()                    {return fSACResponsibilityRegionZEnd;        }
  void                 SetSACResponsibilityRegionZEnd(G4double value)      {fSACResponsibilityRegionZEnd = value;       }

  G4double             GetSACDetectorFrontZPosition()                      {return fSACDetectorFrontZPosition;          }
  void                 SetSACDetectorFrontZPosition(G4double value)        {fSACDetectorFrontZPosition = value;         }
  G4double             GetSACDetectorXLength()                             {return fSACDetectorXLength;                 }
  void                 SetSACDetectorXLength(G4double value)               {fSACDetectorXLength = value;                }
  G4double             GetSACDetectorYLength()                             {return fSACDetectorYLength;                 }
  void                 SetSACDetectorYLength(G4double value)               {fSACDetectorYLength = value;                }
  G4double             GetSACDetectorZLength()                             {return fSACDetectorZLength;                 }
  void                 SetSACDetectorZLength(G4double value)               {fSACDetectorZLength = value;                }

  G4double             GetSACDetectorYRotation()                           {return fSACDetectorYRotation;               }
  void                 SetSACDetectorYRotation(G4double value)             {fSACDetectorYRotation = value;              }

  G4double             GetAbsorberLayerXLength()                           {return fAbsorberLayerXLength;               }
  void                 SetAbsorberLayerXLength(G4double value)             {fAbsorberLayerXLength = value;              }
  G4double             GetAbsorberLayerYLength()                           {return fAbsorberLayerYLength;               }
  void                 SetAbsorberLayerYLength(G4double value)             {fAbsorberLayerYLength = value;              }
  G4double             GetAbsorberLayerZLength()                           {return fAbsorberLayerZLength;               }
  void                 SetAbsorberLayerZLength(G4double value)             {fAbsorberLayerZLength = value;              }

  G4double             GetScintillatorLayerXLength()                       {return fScintillatorLayerXLength;           }
  void                 SetScintillatorLayerXLength(G4double value)         {fScintillatorLayerXLength = value;          }
  G4double             GetScintillatorLayerYLength()                       {return fScintillatorLayerYLength;           }
  void                 SetScintillatorLayerYLength(G4double value)         {fScintillatorLayerYLength = value;          }
  G4double             GetScintillatorLayerZLength()                       {return fScintillatorLayerZLength;           }
  void                 SetScintillatorLayerZLength(G4double value)         {fScintillatorLayerZLength = value;          }

  G4int                GetNLayers()                                        {return fNLayers;                            }
  void                 SetNLayers(G4int value)                             {fNLayers = value;                           }

  G4double             GetAluminiumLayerXLength()                          {return fAluminiumLayerXLength;              }
  void                 SetAluminiumLayerXLength(G4double value)            {fAluminiumLayerXLength = value;             }
  G4double             GetAluminiumLayerYLength()                          {return fAluminiumLayerYLength;              }
  void                 SetAluminiumLayerYLength(G4double value)            {fAluminiumLayerYLength = value;             }
  G4double             GetAluminiumLayerZLength()                          {return fAluminiumLayerZLength;              }
  void                 SetAluminiumLayerZLength(G4double value)            {fAluminiumLayerZLength = value;             }

  G4double             GetFiberDiameter()                                  {return fFiberDiameter;                      }
  void                 SetFiberDiameter(G4double value)                    {fFiberDiameter = value;                     }
  G4double             GetFiberSpacing()                                   {return fFiberSpacing;                       }
  void                 SetFiberSpacing(G4double value)                     {fFiberSpacing = value;                      }
  G4int                GetNFibers()                                        {return fNFibers;                            }
  void                 SetNFibers(G4int value)                             {fNFibers = value;                           }

private:

  G4int    fSACSimulationMode;
  G4double fSACResponsibilityRegionXLength;
  G4double fSACResponsibilityRegionYLength;
  G4double fSACResponsibilityRegionZBeginning;
  G4double fSACResponsibilityRegionZEnd;

  G4double fSACDetectorFrontZPosition;
  G4double fSACDetectorXLength;
  G4double fSACDetectorYLength;
  G4double fSACDetectorZLength;

  G4double fSACDetectorYRotation;

  G4double fAbsorberLayerXLength;
  G4double fAbsorberLayerYLength;
  G4double fAbsorberLayerZLength;

  G4double fScintillatorLayerXLength;
  G4double fScintillatorLayerYLength;
  G4double fScintillatorLayerZLength;

  G4int    fNLayers;

  G4double fAluminiumLayerXLength;
  G4double fAluminiumLayerYLength;
  G4double fAluminiumLayerZLength;

  G4double fFiberDiameter;
  G4double fFiberSpacing;
  G4int    fNFibers;

  G4double fSACFlangeZLength;
  G4double fSACFlangeZPosition;
  G4double fSACFlangeInnerRadius;
  G4double fSACFlangeOuterRadius;
  G4double fSACFlangeDisplacement;
  
  SACGeometryParameters* GeoPars;
};

#endif
