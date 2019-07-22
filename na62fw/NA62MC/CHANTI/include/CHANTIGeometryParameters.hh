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
#ifndef CHANTIGeometryParameters_H
#define CHANTIGeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4String.hh"

#include "NA62VGeometryParameters.hh"

class CHANTIGeometryParameters : public NA62VGeometryParameters
{

public:

  ~CHANTIGeometryParameters();
  static CHANTIGeometryParameters* GetInstance();
  TObjArray GetHashTable();
 

  // My methods
  G4double GetCHANTIFiberRadius()                    { return fFiberRadius;  };
  void     SetCHANTIFiberRadius(G4double value)      { fFiberRadius = value; };
  
  G4double GetCHANTITriangleAltitude()               { return fTriangleAltitude;  };
  void     SetCHANTITriangleAltitude(G4double value) { fTriangleAltitude = value; };
  G4double GetCHANTITriangleBase()                   { return fTriangleBase;      };
  void     SetCHANTITriangleBase(G4double value)     { fTriangleBase = value;     };

  G4double             GetCHANTIRingThickness()                             { return fRingThickness;          };
  void                 SetCHANTIRingThickness(G4double value)               { fRingThickness = value;         };

  G4double             GetCHANTIXInnerHoleLength()                             { return fXInnerHoleLength;          };
  void                 SetCHANTIXInnerHoleLength(G4double value)               { fXInnerHoleLength = value;         };
  G4double             GetCHANTIYInnerHoleLength()                             { return fYInnerHoleLength;          };
  void                 SetCHANTIYInnerHoleLength(G4double value)               { fYInnerHoleLength = value;         };

  void SetCHANTISquareLength(G4double value)                                   { fSquareLength = value; }
  G4double GetCHANTISquareLength()                                             { return fSquareLength; }
    
  // rings positions
  
  G4double GetZPos_Ring(int index) { return fZPos_Ring[index];}; 

  void SetZPos_Ring(int index, G4double value ) { fZPos_Ring[index] = value;}; 

  // sensitive detector attribute
  G4String GetCHANTISensitiveDetectorName()                            { return fCHANTISensitiveDetectorName;};
  void  SetCHANTISensitiveDetectorName(G4String value)                 { fCHANTISensitiveDetectorName = value; };

  G4String GetCHANTICollectionName()                                   { return fCHANTICollectionName; };
  void  SetCHANTICollectionName(G4String value)                        { fCHANTICollectionName = value; };

  G4double GetCHANTIStripLength()                                      { return fStripLength; };
  void  SetCHANTIStripLength(G4double value)                           { fStripLength = value; };

  void Print();

private:

  static CHANTIGeometryParameters* fInstance;
  
  
protected:

  CHANTIGeometryParameters();

public:


  G4double             GetWorldZLength()                                  { return fWorldZLength;                 };
  void                 SetWorldZLength(G4double value)                    { fWorldZLength = value;                };
  G4double             GetWorldXLength()                                  { return fWorldXLength;                 };
  void                 SetWorldXLength(G4double value)                    { fWorldXLength = value;                };
  G4double             GetWorldYLength()                                  { return fWorldYLength;                 };
  void                 SetWorldYLength(G4double value)                    { fWorldYLength = value;                };

  G4double             GetCHANTIDetectorZPosition()                         { return fCHANTIDetectorZPosition;        };
  void                 SetCHANTIDetectorZPosition(G4double value)           { fCHANTIDetectorZPosition = value;       };

  G4double             GetCHANTIDetectorZLength()                           { return fCHANTIDetectorZLength;          };
  void                 SetCHANTIDetectorZLength(G4double value)             { fCHANTIDetectorZLength = value;         };
  G4double             GetCHANTIDetectorXLength()                           { return fCHANTIDetectorXLength;          };
  void                 SetCHANTIDetectorXLength(G4double value)             { fCHANTIDetectorXLength = value;         };
  G4double             GetCHANTIDetectorYLength()                           { return fCHANTIDetectorYLength;          };
  void                 SetCHANTIDetectorYLength(G4double value)             { fCHANTIDetectorYLength = value;         };

  G4int GetNofHalfStrip_Vertical_Front()   { return fNofHalfStrip_Vertical_Front;   };
  void SetNofHalfStrip_Vertical_Front(Int_t value)    { fNofHalfStrip_Vertical_Front = value; };
  G4int GetNofHalfStrip_Vertical_Back()    { return fNofHalfStrip_Vertical_Back ;   };
  void SetNofHalfStrip_Vertical_Back(Int_t value)    { fNofHalfStrip_Vertical_Back = value;   };
  G4int GetNofHalfStrip_Horizzontal_Front() { return fNofHalfStrip_Horizzontal_Front; };
  void SetNofHalfStrip_Horizzontal_Front(Int_t value) { fNofHalfStrip_Horizzontal_Front= value; };
  G4int GetNofHalfStrip_Horizzontal_Back()  { return fNofHalfStrip_Horizzontal_Back ; };
  void SetNofHalfStrip_Horizzontal_Back(Int_t value)  { fNofHalfStrip_Horizzontal_Back = value; };

  //FRAME
  G4double             GetCHANTIXFrameLength()                           { return fXFrameLength;          };
  void                 SetCHANTIXFrameLength(G4double value)             { fXFrameLength = value;         };
  G4double             GetCHANTIYFrameLength()                           { return fYFrameLength;          };
  void                 SetCHANTIYFrameLength(G4double value)             { fYFrameLength = value;         };
  G4double             GetCHANTIZFrameLength()                           { return fZFrameLength;          };
  void                 SetCHANTIZFrameLength(G4double value)             { fZFrameLength = value;         };
  G4double             GetCHANTIFrameThickness()                         { return fFrameThickness;        };
  void                 SetCHANTIFrameThickness(G4double value)           { fFrameThickness = value;       };
  G4double             GetCHANTIXFrame()                                 { return fXFrame;                };
  void                 SetCHANTIXFrame(G4double value)                   { fXFrame = value;               };
  G4double             GetCHANTIYFrame()                                 { return fYFrame;                };
  void                 SetCHANTIYFrame(G4double value)                   { fYFrame = value;               };
  G4double             GetCHANTISupportAlaX()                            { return fSupportAlaX;           };
  void                 SetCHANTISupportAlaX(G4double value)              { fSupportAlaX = value;          };
  G4double             GetCHANTISupportAlaY()                            { return fSupportAlaY;           };
  void                 SetCHANTISupportAlaY(G4double value)              { fSupportAlaY = value;          };
  G4double             GetCHANTISupportAlaZ()                            { return fSupportAlaZ;           };
  void                 SetCHANTISupportAlaZ(G4double value)              { fSupportAlaZ = value;          };
  G4double             GetCHANTIDistXHole()                              { return fDistXHole;             };
  void                 SetCHANTIDistXHole(G4double value)                {  fDistXHole = value;           };
  G4double             GetCHANTIDistYHole()                              { return fDistYHole;             };
  void                 SetCHANTIDistYHole(G4double value)                {  fDistYHole = value;           };

  //SILICON RING
  G4double             GetCHANTISilThickness()                           { return fSilThickness;          };
  void                 SetCHANTISilThickness(G4double value)             {  fSilThickness = value;        };

  //VESSEL
  G4double             GetCHANTIVesselWallThickness()                    { return fVesselWallThickness;   };
  void                 SetCHANTIVesselWallThickness(G4double value)      {  fVesselWallThickness = value; };
  G4double             GetCHANTIVesselWallHeight()                       { return fVesselWallHeight;      };
  void                 SetCHANTIVesselWallHeight(G4double value)         {  fVesselWallHeight = value;    };
  G4double             GetCHANTIVesselWall1()                            { return fZVesselWall1;          };
  void                 SetCHANTIVesselWall1(G4double value)              {  fZVesselWall1 = value;        };
  G4double             GetCHANTIVesselWall2()                            { return fZVesselWall2;          };
  void                 SetCHANTIVesselWall2(G4double value)              {  fZVesselWall2 = value;        };
  G4double             GetCHANTIVesselWall3()                            { return fZVesselWall3;          };
  void                 SetCHANTIVesselWall3(G4double value)              {  fZVesselWall3 = value;        };
  G4double             GetCHANTIVesselPatchLength()                      { return fVesselPatchLength;     };
  void                 SetCHANTIVesselPatchLength(G4double value)        {  fVesselPatchLength = value;   };
  G4double             GetCHANTIVesselWidth()                            { return fVesselWidth;           };
  void                 SetCHANTIVesselWidth(G4double value)              {  fVesselWidth = value;         };
  G4int                GetCHANTINumberOfStation()                        { return fNumberOfStation;       };
  void                 SetCHANTINumberOfStation(G4int value)             {  fNumberOfStation = value;     };


private:

  G4double fRingThickness;
  G4double fRingXsideLength;
  G4double fRingYsideLength;

  // MAIN parameters 
  G4double fWorldZLength;
  G4double fWorldXLength;
  G4double fWorldYLength;

  G4String fCHANTISensitiveDetectorName;
  G4String fCHANTICollectionName;

  G4double fCHANTIDetectorZPosition;

  G4double fCHANTIDetectorZLength;
  G4double fCHANTIDetectorXLength;
  G4double fCHANTIDetectorYLength;

  // my parameters
  G4double fFiberRadius;
  
  G4double fStripLength;

  G4double fTriangleAltitude;
  G4double fTriangleBase;

  G4double fXInnerHoleLength;
  G4double fYInnerHoleLength;

  G4double fInnerRadius;
  G4double fOuterRadius;

  G4double fSquareLength;

  G4double fXInnerHalfLength;
  G4double fYInnerHalfLength;
	    
  G4int fNofHalfStrip_Vertical_Front;
  G4int fNofHalfStrip_Vertical_Back ;
  G4int fNofHalfStrip_Horizzontal_Front;
  G4int fNofHalfStrip_Horizzontal_Back ;

  // DERIVED parameters
  G4double fZPos_Ring[6]; 

  //FRAME
  G4double fXFrameLength;
  G4double fYFrameLength;
  G4double fZFrameLength;
  G4double fFrameThickness;
  G4double fXFrame;
  G4double fYFrame;
  G4double fFrameSubZ;
  G4double fDistXHole;
  G4double fDistYHole;
  G4double fSupportAlaX;
  G4double fSupportAlaY;
  G4double fSupportAlaZ;

  //SILICON RING
  G4double fSilThickness;

  //VESSEL
  G4double fVesselWallThickness;
  G4double fVesselWallHeight;
  G4double fZVesselWall1;
  G4double fZVesselWall2;
  G4double fZVesselWall3;
  G4double fVesselPatchLength;
  G4double fVesselWidth;
  G4int    fNumberOfStation;
};
#endif
