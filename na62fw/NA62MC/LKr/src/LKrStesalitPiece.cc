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
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//            Evelina Marinova(Evelina.Marinova@cern.ch) 
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4Para.hh"
#include "G4Sphere.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "LKrGeometryParameters.hh"
#include "LKrMaterialParameters.hh"
#include "LKrElectrodes.hh"
#include "LKrStesalitPiece.hh"
#include "G4SDManager.hh"
#include "LKrLKrVolume.hh"
#include "LKrFrontBackPlate.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

/// \class LKrStesalitPiece 
/// \Brief
/// Stesalit pieces between the electrode cells and the beam pipe of the LKr
/// \EndBrief
/// \Detailed
/// This class creates a stesalit piece to fill the gap between the electrode cells and the beam pipe.
/// \EndDetailed

LKrStesalitPiece::LKrStesalitPiece( G4Material * Material, G4LogicalVolume * MotherVolume, G4double IndexLine, G4double nSpacerPlane, G4double NCellsInLine,G4int Sign):
  NA62VComponent(Material,MotherVolume) {
  ReadGeometryParameters();
  fIndexLine = IndexLine;
  fnSpacerPlane = nSpacerPlane;
  fNCellsInLine = NCellsInLine;
  fSign = Sign;

  // Mandatory here to Find or Build the needed materials
  LKrMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

LKrStesalitPiece::~LKrStesalitPiece(){}

void LKrStesalitPiece::ReadGeometryParameters() {
  // Read all the geometrical parameters and copy them to private members
  LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();

  fXfrontReferenceCell = GeoPars->GetXfrontReferenceCell();
  fYfrontReferenceCell = GeoPars->GetYfrontReferenceCell();
  fRadiusHoleSpacerPlate = GeoPars->GetRadiusHoleSpacerPlate();
  fHalfSizeCentralGap = GeoPars->GetHalfSizeCentralGap();  
  fDistanceFrontPlateBackPlate = GeoPars->GetDistanceFrontPlateBackPlate();
  fLongitudinalLengthLKrVolume = GeoPars->GetLongitudinalLengthLKrVolume();
  fLengthOfColdWindowPlate = GeoPars->GetLengthOfColdWindowPlate();

  fRadiusHoleSpacerPlate = GeoPars->GetRadiusHoleSpacerPlate();
  fHalfCellSizeAtFrontWall = GeoPars->GetHalfCellSizeAtFrontWall();
  fIncr = GeoPars->GetIncr();
  fHalfSizeSpacerWallSegmentZ = GeoPars->GetHalfSizeSpacerWallSegmentZ();
  fRadiusHoleSpacerPlate = GeoPars->GetRadiusHoleSpacerPlate();
  fHalfIrregularCellSizeAtFrontWallY = GeoPars->GetHalfIrregularCellSizeAtFrontWallY();
  fPassiveLKrInsideOctagon = GeoPars->GetPassiveLKrInsideOctagon(); 
  fHalfLengthOfFrontPlateOctagon = GeoPars->GetHalfLengthOfFrontPlateOctagon();
  fSpaceToNextPlate = GeoPars->GetSpaceToNextPlate();
}

void LKrStesalitPiece::CreateGeometry() {

  G4double pSPhi = 0*deg;
  G4double pDPhi = 360*deg;  
  G4double NCellsInRow = 19. - 4 * fIndexLine;

  // which plane and how many cells?
  // how many vertical lines

  G4double pDx = 0.5 * fHalfCellSizeAtFrontWall * pow(fIncr, fnSpacerPlane) * NCellsInRow;

  G4double pDy = (fNCellsInLine * fHalfCellSizeAtFrontWall - 0.5 * fHalfIrregularCellSizeAtFrontWallY) * pow(fIncr,fnSpacerPlane);
  G4double pDz = fHalfSizeSpacerWallSegmentZ; 
  G4double Xtr = fXfrontReferenceCell * pow(fIncr, fnSpacerPlane);
  G4double Ytr = 0;

  if (fIndexLine == 0) {
    Ytr =  3 * fHalfCellSizeAtFrontWall* pow(fIncr, fnSpacerPlane) - 0.5*fHalfIrregularCellSizeAtFrontWallY;
    pDy = (fNCellsInLine * fHalfCellSizeAtFrontWall - 0.5 * fHalfIrregularCellSizeAtFrontWallY) * pow(fIncr,fnSpacerPlane);}
  if (fIndexLine == 1) {
    Ytr =  7 * fHalfCellSizeAtFrontWall* pow(fIncr, fnSpacerPlane)- fHalfIrregularCellSizeAtFrontWallY;
    pDy = (fNCellsInLine * fHalfCellSizeAtFrontWall ) * pow(fIncr,fnSpacerPlane);}
  if (fIndexLine == 2) {
    Ytr =  9 * fHalfCellSizeAtFrontWall* pow(fIncr, fnSpacerPlane)- fHalfIrregularCellSizeAtFrontWallY;
    pDy = (fNCellsInLine * fHalfCellSizeAtFrontWall ) * pow(fIncr,fnSpacerPlane);}

  Ytr = fSign * (Ytr + fHalfSizeCentralGap);

  G4double Ztr = - fLongitudinalLengthLKrVolume/ 2 + fHalfLengthOfFrontPlateOctagon * 2 
    + fPassiveLKrInsideOctagon + fHalfSizeSpacerWallSegmentZ + fLengthOfColdWindowPlate + fnSpacerPlane * fSpaceToNextPlate;

  G4Box* fSolidBox = new G4Box("StesalitPiece", pDx, pDy, pDz);

  G4Tubs* VirtualTube = new G4Tubs
    ("virtual tube",                        
     0. ,
     fRadiusHoleSpacerPlate,
     fDistanceFrontPlateBackPlate,  //can be random
     pSPhi, pDPhi);

 G4SubtractionSolid* fSolidVolume = new
   G4SubtractionSolid("IntersectedSolid", new
		      G4DisplacedSolid("intersected",fSolidBox,0,G4ThreeVector(Xtr,Ytr,0)),
		      VirtualTube);

  fLogicalVolume = new G4LogicalVolume(fSolidVolume,      // solid
				       fMaterial,         // material
				       "StesalitPiece",   // name
				       0,                 // field manager
				       0,                 // sensitive detector
				       0);                // user limits

  G4RotationMatrix Ra =  G4RotationMatrix(G4ThreeVector(0., 0. ,0.),0*deg);
  G4ThreeVector Ta = G4ThreeVector(0.,0,Ztr);

  fPhysicalVolume = new G4PVPlacement(G4Transform3D(Ra,Ta),
				      fLogicalVolume,  // its logical volume
				      "StesalitPiece", // its name
				      fMotherVolume,   // its mother  volume
				      false,           // no boolean operations
				      0);              // copy number
}

void LKrStesalitPiece::SetProperties() {
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
