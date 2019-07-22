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
#ifndef SpectrometerGeometryParameters_H
#define SpectrometerGeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"

#include "NA62VGeometryParameters.hh"

class SpectrometerGeometryParameters : public NA62VGeometryParameters {

public:

  ~SpectrometerGeometryParameters() {}
  static SpectrometerGeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static SpectrometerGeometryParameters* fInstance;

protected:

  SpectrometerGeometryParameters();

public:

  // Return info about the responsibility regions

  G4double GetResponsibilityRegionZofFrontFace(G4int i) { return fRR_ZofFrontFace[i];      }
  G4double GetResponsibilityRegionZofBackFace(G4int i)  { return fRR_ZofBackFace[i];       }
  G4double GetResponsibilityRegionRadius(G4int i)       { return fRR_Radius[i];            }

  // Straw geometry

  G4double GetStrawInnerRadius()                        { return fStrawInnerRadius;        }
  void     SetStrawInnerRadius(G4double value)          { fStrawInnerRadius = value;       }
  G4double GetCopperThickness()                         { return fCopperThickness;         }
  void     SetCopperThickness(G4double value)           { fCopperThickness = value;        }
  G4double GetMylarThickness()                          { return fMylarThickness;          }
  void     SetMylarThickness(G4double value)            { fMylarThickness = value;         }
  G4double GetGoldThickness()                           { return fGoldThickness;           }
  void     SetGoldThickness(G4double value)             { fGoldThickness = value;          }
  G4double GetStrawRadius()                             { return fStrawRadius;             }
  void     SetStrawRadius(G4double value)               { fStrawRadius = value;            }
  G4double GetStrawDiameter()                           { return fStrawDiameter;           }
  void     SetStrawDiameter(G4double value)             { fStrawDiameter = value;          }
  G4double GetStrawLength()                             { return fStrawLength;             }
  void     SetStrawLength(G4double value)               { fStrawLength = value;            }
  G4double GetWireRadius()                              { return fWireRadius;              }
  void     SetWireRadius(G4double value)                { fWireRadius = value;             }

  // View geometry

  G4int    GetNStraws()                                 { return fNStraws;                 }
  void     SetNStraws(G4int value)                      { fNStraws = value;                }
  G4double GetViewSize()                                { return fViewSize;                }
  void     SetViewSize(G4double value)                  { fViewSize = value;               }
  G4double GetStrawSpacing()                            { return fStrawSpacing;            }
  void     SetStrawSpacing(G4double value)              { fStrawSpacing = value;           }
  G4double GetLayerSpacing()                            { return fLayerSpacing;            }
  void     SetLayerSpacing(G4double value)              { fLayerSpacing = value;           }
  G4double GetLayerDisplacement()                       { return fLayerDisplacement;       }
  void     SetLayerDisplacement(G4double value)         { fLayerDisplacement = value;      }

  G4double GetHalfViewSpacing()                         { return fHalfViewSpacing;         }
  void     SetHalfViewSpacing(G4double value)           { fHalfViewSpacing = value;        }
  G4double GetHalfViewDisplacement()                    { return fHalfViewDisplacement;    }
  void     SetHalfViewDisplacement(G4double value)      { fHalfViewDisplacement = value;   }
  G4double GetHalfViewZLength()                         { return fHalfViewZLength;         }
  void     SetHalfViewZLength(G4double value)           { fHalfViewZLength = value;        }
  G4double GetHalfViewXLength()                         { return fHalfViewXLength;         }
  void     SetHalfViewXLength(G4double value)           { fHalfViewXLength = value;        }
  G4double GetHalfViewYLength()                         { return fHalfViewYLength;         }
  void     SetHalfViewYLength(G4double value)           { fHalfViewYLength = value;        }

  G4double GetViewZLength()                             { return fViewZLength;             }
  void     SetViewZLength(G4double value)               { fViewZLength = value;            }
  G4double GetViewXLength()                             { return fViewXLength;             }
  void     SetViewXLength(G4double value)               { fViewXLength = value;            }
  G4double GetViewYLength()                             { return fViewYLength;             }
  void     SetViewYLength(G4double value)               { fViewYLength = value;            }
  G4double GetViewSpacing1()                            { return fViewSpacing1;            }
  void     SetViewSpacing1(G4double value)              { fViewSpacing1 = value;           }
  G4double GetViewSpacing2()                            { return fViewSpacing2;            }
  void     SetViewSpacing2(G4double value)              { fViewSpacing2 = value;           }

  // Chamber geometry

  G4double GetChamberRadius()                           { return fChamberRadius;           }
  void     SetChamberRadius(G4double value)             { fChamberRadius = value;          }
  G4double GetChamberZLength()                          { return fChamberZLength;          }
  void     SetChamberZLength(G4double value)            { fChamberZLength = value;         }
  G4double GetChamberZCenter(G4int i)                   { return fChamberZCenter[i];       }
  G4double GetChamberZCenterOffset(G4int i)             { return fChamberZCenterOffset[i]; }

  G4double GetChambernXRminY()                          { return fChambernXRminY;          }
  G4double GetChambernXORminY()                         { return fChambernXORminY;         }
  void     SetChambernXRminY(G4double value)            { fChambernXRminY = value;         }
  void     SetChambernXORminY(G4double value)           { fChambernXORminY = value;        }
  G4double GetChambernXRmaxY()                          { return fChambernXRmaxY;          }
  G4double GetChambernXORmaxY()                         { return fChambernXORmaxY;         }
  void     SetChambernXRmaxY(G4double value)            { fChambernXRmaxY = value;         }
  void     SetChambernXORmaxY(G4double value)           { fChambernXORmaxY = value;        }
  G4double GetChamberXDisplacement(G4int i)             { return fChamberXDisplacement[i]; }
  G4double GetChamberXRminU(G4int i)                    { return fChamberXRminU[i];        }
  G4double GetChamberXRmaxU(G4int i)                    { return fChamberXRmaxU[i];        }
  G4double GetChamberXRminX(G4int i)                    { return fChamberXRminX[i];        }
  G4double GetChamberXRmaxX(G4int i)                    { return fChamberXRmaxX[i];        }
  G4double GetChamberXORminU(G4int i)                   { return fChamberXORminU[i];       }
  G4double GetChamberXORmaxU(G4int i)                   { return fChamberXORmaxU[i];       }
  G4double GetChamberXORminX(G4int i)                   { return fChamberXORminX[i];       }
  G4double GetChamberXORmaxX(G4int i)                   { return fChamberXORmaxX[i];       }

  // Magnet geometry

  G4double GetMagnetZLength()                           { return fMagnetZLength;           }
  void     SetMagnetZLength(G4double value)             { fMagnetZLength = value;          }
  G4double GetMagnetXLength()                           { return fMagnetXLength;           }
  void     SetMagnetXLength(G4double value)             { fMagnetXLength = value;          }
  G4double GetMagnetYLength()                           { return fMagnetYLength;           }
  void     SetMagnetYLength(G4double value)             { fMagnetYLength = value;          }
  G4double GetMagnetZPosition()                         { return fMagnetZPosition;         }
  void     SetMagnetZPosition(G4double value)           { fMagnetZPosition = value;        }
  G4double GetMagnetFieldStrength()                     { return fMagnetFieldStrength;     }
  void     SetMagnetFieldStrength(G4double value)       { fMagnetFieldStrength = value;    }

private:

  // Responsibility regions

  G4double fRR_ZofFrontFace[4];
  G4double fRR_ZofBackFace[4];
  G4double fRR_Radius[4];

  // Straw geometry

  G4double fStrawInnerRadius;
  G4double fCopperThickness;
  G4double fMylarThickness;
  G4double fGoldThickness;
  G4double fStrawRadius;
  G4double fStrawDiameter; 
  G4double fStrawLength;
  G4double fWireRadius;

  // View geometry

  G4int    fNStraws;
  G4double fViewSize; 
  G4double fStrawSpacing;
  G4double fLayerSpacing;
  G4double fLayerDisplacement;

  G4double fHalfViewSpacing;
  G4double fHalfViewDisplacement;
  G4double fHalfViewZLength;
  G4double fHalfViewXLength; 
  G4double fHalfViewYLength;

  G4double fViewZLength;
  G4double fViewXLength; 
  G4double fViewYLength;
  G4double fViewSpacing1;
  G4double fViewSpacing2;

  // Chamber geometry

  G4double fChamberRadius;
  G4double fChamberZLength;
  G4double fChamberZCenter[4];
  G4double fChamberZCenterOffset[4];

  G4double fChambernXRminY;
  G4double fChambernXRmaxY;
  G4double fChamberXDisplacement[4];
  G4double fChamberXRminU[4];
  G4double fChamberXRmaxU[4];
  G4double fChamberXRminX[4];
  G4double fChamberXRmaxX[4];
  G4double fChamberXORminU[4];
  G4double fChamberXORmaxU[4];
  G4double fChamberXORminX[4];
  G4double fChamberXORmaxX[4]; 
  G4double fChambernXORminY;
  G4double fChambernXORmaxY;

  // Magnetic fields

  G4double fMagnetZLength;
  G4double fMagnetXLength;
  G4double fMagnetYLength;
  G4double fMagnetZFront;    ///< Z position of MNP33 front Z plane
  G4double fMagnetZPosition; ///< Z position of MNP33 magnet centre
  
  G4double fMagnetFieldStrength; ///< Vertical magnetic field component for uniform field simulation [T]
  G4int    fFringeField;
  G4int    fMNP33Field;
  


};
#endif
