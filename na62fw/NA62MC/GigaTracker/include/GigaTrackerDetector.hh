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
// 2014-03-01 Bob Velghe (bob.velghe@cern.ch)
// - Add MDX (TRIM5) and MCB (Achromats) magnets
//
// 2008-04-22 S.Bifani (Simone.Bifani@cern.ch)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------
//
#ifndef GigaTrackerDetector_H
#define GigaTrackerDetector_H 1

#include "globals.hh"
#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerStation.hh"
#include "GigaTrackerMDXMagnet.hh"
#include "GigaTrackerMCBMagnet.hh"
#include "GigaTrackerScraperMagnet.hh"
#include "GigaTrackerCollimator.hh"

#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class GigaTrackerDetector : public NA62VComponent, public NA62VNamed {
  
public:
  GigaTrackerDetector(G4Material*, G4LogicalVolume*);
  ~GigaTrackerDetector() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  
public:
  
  G4String             GetGigaTrackerSensitiveDetectorName()              { return fGigaTrackerSensitiveDetectorName; };
  void                 SetGigaTrackerSensitiveDetectorName(G4String value)
  { fGigaTrackerSensitiveDetectorName = value; };
  G4String             GetGigaTrackerCollectionName()                     { return fGigaTrackerCollectionName;    };
  void                 SetGigaTrackerCollectionName(G4String value)       { fGigaTrackerCollectionName = value;   };
  
  G4double             GetXLength()                                       { return fXLength;                      };
  void                 SetXLength(G4double value)                         { fXLength = value;                     };
  G4double             GetYLength()                                       { return fYLength;                      };
  void                 SetYLength(G4double value)                         { fYLength = value;                     };
  G4double             GetZLength()                                       { return fZLength;                      };
  void                 SetZLength(G4double value)                         { fZLength = value;                     };
  
  G4double             GetZPosition()                                     { return fZPosition;                    };
  void                 SetZPosition(G4double value)                       { fZPosition = value;                   };
  
  GigaTrackerStation *
                       GetStation1()                                      { return fStation1;                     };
  void                 SetStation1(GigaTrackerStation * value)            { fStation1 = value;                    };
  GigaTrackerStation *
                       GetStation2()                                      { return fStation2;                     };
  void                 SetStation2(GigaTrackerStation * value)            { fStation2 = value;                    };
  GigaTrackerStation *
                       GetStation3()                                      { return fStation3;                     };
  void                 SetStation3(GigaTrackerStation * value)            { fStation3 = value;                    };
  
  G4ThreeVector        GetStation1Position()                              { return fStation1Position;             };
  void                 SetStation1Position(G4ThreeVector value)           { fStation1Position = value;            };
  G4ThreeVector        GetStation2Position()                              { return fStation2Position;             };
  void                 SetStation2Position(G4ThreeVector value)           { fStation2Position = value;            };
  G4ThreeVector        GetStation3Position()                              { return fStation3Position;             };
  void                 SetStation3Position(G4ThreeVector value)           { fStation3Position = value;            };
  
  GigaTrackerCollimator * GetCollimator()                                 { return fCollimator;                   };
  void                    SetCollimator(GigaTrackerCollimator * value)    { fCollimator = value;                  };
  
  G4ThreeVector        GetCollimatorPosition()                            { return fCollimatorPosition;           };
  void                 SetCollimatorPosition(G4ThreeVector value)         { fCollimatorPosition = value;          };
  
private:
  
  G4String fGigaTrackerSensitiveDetectorName;
  G4String fGigaTrackerCollectionName;
  
  G4double fXLength;
  G4double fYLength;
  G4double fZLength;
  
  G4double fZPosition;
  
  GigaTrackerStation * fStation1;
  GigaTrackerStation * fStation2;
  GigaTrackerStation * fStation3;
  
  G4ThreeVector fStation1Position;
  G4ThreeVector fStation2Position;
  G4ThreeVector fStation3Position;

  GigaTrackerMCBMagnet * fMagnet1;
  GigaTrackerMCBMagnet * fMagnet2;
  GigaTrackerMCBMagnet * fMagnet3;
  GigaTrackerMCBMagnet * fMagnet4;
  GigaTrackerMDXMagnet * fMagnet5;
  GigaTrackerScraperMagnet * fScraper;
  
  G4ThreeVector fMagnet1Position;
  G4ThreeVector fMagnet2Position;
  G4ThreeVector fMagnet3Position;
  G4ThreeVector fMagnet4Position;
  G4ThreeVector fMagnet5Position;
  G4ThreeVector fScraperPosition;

  GigaTrackerCollimator *fCollimator;
  
  GigaTrackerGeometryParameters * fGeoPars;
  G4ThreeVector fCollimatorPosition;
  
};

#endif
