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
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------
#ifndef LKrSD_h
#define LKrSD_h 1

#include "G4VSensitiveDetector.hh"
#include "LKrHit.hh"
#include "LKrGeometryParameters.hh"

#include "G4Step.hh"
#include "G4HCofThisEvent.hh"
#include "G4TouchableHistory.hh"
#include "G4Trap.hh"

class LKrSD : public G4VSensitiveDetector
{

    public:
        LKrSD(G4String name, G4String colName);
        ~LKrSD();

        void Initialize(G4HCofThisEvent*HCE);
        G4bool ProcessHits(G4Step*aStep,G4TouchableHistory*);
        G4ThreeVector ComputeDimensions (const G4Trap* TouchedTrapezoid, G4Trap* ParamTrapezoid) const;

        void EndOfEvent(G4HCofThisEvent*HCE);
        void clear();
        void DrawAll();
        void PrintAll();


    public:

        G4double             GetDistanceFrontPlateBackPlate()                    { return fDistanceFrontPlateBackPlate;      };
        void                 SetDistanceFrontPlateBackPlate(G4double value)      { fDistanceFrontPlateBackPlate = value;     };

        G4double             GetSpaceToNextPlate()                               { return fSpaceToNextPlate;                 };
        void                 SetSpaceToNextPlate(G4double value)                 { fSpaceToNextPlate = value;                };

        G4double             GetHalfSizeFrontWallSegmentZ()                      { return fHalfSizeFrontWallSegmentZ;        };
        void                 SetHalfSizeFrontWallSegmentZ(G4double value)        { fHalfSizeFrontWallSegmentZ = value;       };

        G4double             GetHalfCellSizeAtFrontWall()                        { return fHalfCellSizeAtFrontWall;          };
        void                 SetHalfCellSizeAtFrontWall(G4double value)          { fHalfCellSizeAtFrontWall = value;         };

        G4double             GetXfrontReferenceCell()                            { return fXfrontReferenceCell;              };
        void                 SetXfrontReferenceCell(G4double value)              { fXfrontReferenceCell = value;             };

        G4double              GetLKrCellLength()                                 { return fLKrCellLength;                    };
        void                 SetLKrCellLength(G4double value)                    { fLKrCellLength = value;                   };

        G4double             GetTopRightCornerX()                                { return fTopRightCornerX;                  };
        void                 SetTopRightCornerX(G4double value)                  { fTopRightCornerX = value;                 };

        G4double             GetTopRightCornerY()                                { return fTopRightCornerY;                  };
        void                 SetTopRightCornerY(G4double value)                  { fTopRightCornerY = value;                 };

        G4double             GetLKrCell0X()                                      { return fLKrCell0X;                        };
        void                 SetLKrCell0X(G4double value)                        { fLKrCell0X = value;                       };

        G4double             GetLKrCell0Y()                                      { return fLKrCell0Y;                        };
        void                 SetLKrCell0Y(G4double value)                        { fLKrCell0Y = value;                       };


    private:
        LKrHitsCollection *Collection;
        LKrHit * HitMap[16384]; //Number of cells in the calorimeter

        G4int nHits;
        int HCID;

        G4double fZtr;

        G4double fXbackReferenceCell;

        G4double fYbackReferenceCell;

        G4double fXfrontReferenceCell;

        G4double fYfrontReferenceCell;


        G4double fProjectivityPointPositionZ;

        G4double fProjectivityAxisProjectionZ; 


        G4double fDistanceToNextElectrodeFrontX;

        G4double fDistanceToNextElectrodeBackX; 

        G4double fDistanceToNextElectrodeFrontY;

        G4double fDistanceToNextElectrodeBackY; 


        G4int fHalfNElectrodesX; 

        G4int fHalfNElectrodesY; 

        G4int fNRowsFullNumberElectrodes;


        G4double fRHoleX; 

        G4double fRHoleY; 
        G4double fHalfZWidth  ;

        G4double fHalfYWidthF ;

        G4double fHalfXWidthF;

        G4double fHalfYWidthB ;
        G4double fHalfXWidthB ;
        G4double fBackWallPositionZ;				    



        G4double fDistanceFrontPlateBackPlate;
        G4double fSpaceToNextPlate;

        G4double fHalfSizeFrontWallSegmentZ ;
        G4double fHalfCellSizeAtFrontWall;

        G4double fTopRightCornerX;
        G4double fTopRightCornerY;

        G4double fLKrCellLength;

        G4double fLKrCell0X;
        G4double fLKrCell0Y;

        G4Trap * fParametrizedTrapezoid;

        G4double fIncr;
        G4double SpaceToNextPlate;

        G4double fGevtoCurr1[5000];
        G4double fGevtoCurr2[5000];
        G4double fGevtoCurr3[5000];

};




#endif

