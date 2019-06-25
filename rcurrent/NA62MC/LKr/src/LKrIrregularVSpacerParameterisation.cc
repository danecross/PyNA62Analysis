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
//
//  03-09-2012 Sergey Podolsky (siarhei.padolski@cern.ch)      
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4Para.hh"

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

#include "LKrIrregularVSpacerParameterisation.hh"
#include "G4SDManager.hh"

/// \class LKrIrregularVSpacerParameterisation
/// \Brief
/// IrregularLKrVSpacerParameterisation class.
/// \EndBrief
///
/// \Detailed
/// This class stores and provides the information about the geometry and position of the content of each single "irregular half electrode" cell -
///electrode segments, front, back and spacer wall segmants. Parts of the front and back wall are built separately in 
///LKrFrontBackWall.cc.
/// \EndDetailed


LKrIrregularVSpacerParameterisation::LKrIrregularVSpacerParameterisation (G4VPhysicalVolume* physVol){

  ReadGeometryParameters();

  // Mandatory here to Find or Build the needed materials
  LKrMaterialParameters::GetInstance();

  LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();
  fPositionOfWallZ0 = GeoPars->GetPositionOfWallZ0();                              
  fSpaceToNextPlate = GeoPars->GetSpaceToNextPlate();                              
  fSpaceFromWallToElectrodeAtWallX = GeoPars->GetSpaceFromWallToElectrodeAtWallX() ;              
  fSizeHoleX = GeoPars->GetSizeHoleX();                                     
  fSizeHoleY = GeoPars->GetSizeHoleY();                                     
  fIncr = GeoPars->GetIncr();                                          
  fHalfSizeIrregularWallVSegmentY = GeoPars->GetHalfSizeIrregularWallVSegmentY();
  fHalfCellSizeAtFrontWall = GeoPars->GetHalfCellSizeAtFrontWall();
  fHalfSizeSpacerWallSegmentZ = GeoPars->GetHalfSizeSpacerWallSegmentZ();                    
  fIrrZigHalfSizeX = GeoPars->GetIrrZigHalfSizeX(); 
  fIrrZigHalfSizeY = GeoPars->GetIrrZigHalfSizeY();
  fIrrZigHalfSizeZ = GeoPars->GetIrrZigHalfSizeZ();

  fHalfIrregularCellSizeAtFrontWallY = GeoPars->GetHalfIrregularCellSizeAtFrontWallY(); 
  fHalfIrregularCellSizeAtBackWallY = GeoPars->GetHalfIrregularCellSizeAtBackWallY();                    
  fSizeHoleYHalfCell = GeoPars->GetSizeHoleYHalfCell();

  G4Trap * ElectrodeCell = (G4Trap*) physVol->GetMotherLogical()->GetSolid();
  G4double Theta = ElectrodeCell->GetSymAxis().theta();
  G4double Phi = ElectrodeCell->GetSymAxis().phi();

  for (int iSpacer =0; iSpacer < 34; iSpacer++){

    // Calculate the position of the Wall segments
    // place the blocks  -- initialize outside the loop for front plate, add the next each 21 cm
    G4double PositionOfWallZ =  fPositionOfWallZ0 + (G4int)(iSpacer%7) * fSpaceToNextPlate;

    // Calculate the X and Y position of the vertical block wrt trapezoid centre
    // take into account the projectivity
    G4double HSegmentPositionX = PositionOfWallZ * cos(Phi) * sin(Theta) / cos(Theta);
    G4double VSegmentPositionY = PositionOfWallZ * sin(Phi) * sin(Theta) / cos(Theta);

    // Increase the size of the cell for each successive plate due to projectivity
    G4double HalfCellSize= fHalfCellSizeAtFrontWall*pow(fIncr,(G4int)(iSpacer%7));
    G4double HalfIrregularCellSize =  fHalfIrregularCellSizeAtFrontWallY*pow(fIncr,(G4int)(iSpacer%7));
    G4double HalfSizeWallVSegmentX =  0.5*(HalfCellSize-fSizeHoleX);
    G4double VSegmentPositionX =  HSegmentPositionX + pow(-1, iSpacer%7) * (0.5 * HalfCellSize - HalfSizeWallVSegmentX );

    // Define the X size of a horizontal segment of the spacer plate -parameterized.
    G4double HalfSizeWallHSegmentX = 0.5*HalfCellSize;

    // Calculate the Y size for the two horizontal segments 
    G4double HalfSizeWallHSegmentY = 0.5*0.5*(HalfIrregularCellSize-fSizeHoleYHalfCell);

    G4double HSegmentPositionY_UP   = fHalfSizeIrregularWallVSegmentY  + HalfSizeWallHSegmentY + VSegmentPositionY;
    G4double HSegmentPositionY_DOWN = -fHalfSizeIrregularWallVSegmentY  - HalfSizeWallHSegmentY + VSegmentPositionY;

    //calculate position of the zigzag segment passing trough the spacer plate hole
    G4double ZigzagShortSegmentPosition = HSegmentPositionX - pow(-1, iSpacer%7) * (0.5 * HalfCellSize - 2 * fIrrZigHalfSizeX );

    G4ThreeVector origin; 


    if(iSpacer == 0){      // 0: vertical segments of the front wall
      origin = G4ThreeVector(HSegmentPositionX + 1.5 * fIrrZigHalfSizeX, VSegmentPositionY, PositionOfWallZ);
    }
    else if(iSpacer <= 5){ // 1-5: vertical segments of the spacer plates
      origin = G4ThreeVector(VSegmentPositionX, VSegmentPositionY, PositionOfWallZ);
    }
    else if(iSpacer == 6){ // 6: vertical segments of the back wall
      origin = G4ThreeVector(HSegmentPositionX + 1.5 * fIrrZigHalfSizeX, VSegmentPositionY, PositionOfWallZ);
    }
    else if(iSpacer <= 13){ // 7-13: upper horizontal segments of the spacer plates
      origin = G4ThreeVector(HSegmentPositionX, HSegmentPositionY_UP, PositionOfWallZ);
    }
    else if(iSpacer <= 20){ // 14-20: lower horizontal segments of the spacer plates
      origin = G4ThreeVector(HSegmentPositionX, HSegmentPositionY_DOWN, PositionOfWallZ);
    }
    else if(iSpacer <= 27){ // 21-27: zigzags segments passing trough the holes
      origin = G4ThreeVector(ZigzagShortSegmentPosition, VSegmentPositionY, PositionOfWallZ);
    }
    else if(iSpacer <= 33){ // 28-33: real zigzags
      // Calculate X and Y positions of the zig zags.
      G4double Vxpos_el = (PositionOfWallZ + fSpaceToNextPlate/2) * cos(Phi) * sin(Theta) / cos(Theta);
      G4double Vypos_el = (PositionOfWallZ + fSpaceToNextPlate/2) * sin(Phi) * sin(Theta) / cos(Theta);
      origin = G4ThreeVector(Vxpos_el,Vypos_el, PositionOfWallZ + fSpaceToNextPlate/2);
    }

    fPosition[iSpacer][0] = origin.x();
    fPosition[iSpacer][1] = origin.y();
    fPosition[iSpacer][2] = origin.z();

    if(iSpacer == 0 || iSpacer == 6){ // 0,6 : vertical walls (front/back)
      fDimensions[iSpacer][0] = fHalfSizeSpacerWallSegmentZ;
      fDimensions[iSpacer][1] = Theta;
      fDimensions[iSpacer][2] = Phi;
      fDimensions[iSpacer][3] = fHalfSizeIrregularWallVSegmentY;
      fDimensions[iSpacer][4] = HalfSizeWallHSegmentX - 2.5 * fIrrZigHalfSizeX;
      fDimensions[iSpacer][5] = HalfSizeWallHSegmentX - 2.5 * fIrrZigHalfSizeX;
      fDimensions[iSpacer][6] = 0;
      fDimensions[iSpacer][7] = fHalfSizeIrregularWallVSegmentY;
      fDimensions[iSpacer][8] = HalfSizeWallHSegmentX - 2.5 * fIrrZigHalfSizeX;
      fDimensions[iSpacer][9] = HalfSizeWallHSegmentX - 2.5 * fIrrZigHalfSizeX;
      fDimensions[iSpacer][10] = 0;
    }
    else if(iSpacer <= 5){ // 1-5: vertical wall 2-6
      fDimensions[iSpacer][0] = fHalfSizeSpacerWallSegmentZ;
      fDimensions[iSpacer][1] = Theta;
      fDimensions[iSpacer][2] = Phi;
      fDimensions[iSpacer][3] = fHalfSizeIrregularWallVSegmentY;
      fDimensions[iSpacer][4] = HalfSizeWallVSegmentX;
      fDimensions[iSpacer][5] = HalfSizeWallVSegmentX;
      fDimensions[iSpacer][6] = 0;
      fDimensions[iSpacer][7] = fHalfSizeIrregularWallVSegmentY;
      fDimensions[iSpacer][8] = HalfSizeWallVSegmentX;
      fDimensions[iSpacer][9] = HalfSizeWallVSegmentX;
      fDimensions[iSpacer][10] = 0;
    }
    else if(iSpacer <= 20){ // 7-20: up and down - horizontal walls
      G4double Tolerance =  0.001 * mm;
      fDimensions[iSpacer][0] = fHalfSizeSpacerWallSegmentZ;
      fDimensions[iSpacer][1] = Theta;
      fDimensions[iSpacer][2] = Phi;
      fDimensions[iSpacer][3] = HalfSizeWallHSegmentY-Tolerance;
      fDimensions[iSpacer][4] = HalfSizeWallHSegmentX;
      fDimensions[iSpacer][5] = HalfSizeWallHSegmentX;
      fDimensions[iSpacer][6] = 0;
      fDimensions[iSpacer][7] = HalfSizeWallHSegmentY-Tolerance;
      fDimensions[iSpacer][8] = HalfSizeWallHSegmentX;
      fDimensions[iSpacer][9] = HalfSizeWallHSegmentX;
      fDimensions[iSpacer][10] = 0;
    }
    else if(iSpacer <= 27){ // 21-27: electrode passing trough the wall
      fDimensions[iSpacer][0] = fHalfSizeSpacerWallSegmentZ;
      fDimensions[iSpacer][1] = 0;
      fDimensions[iSpacer][2] = 0;
      fDimensions[iSpacer][3] = fIrrZigHalfSizeY;
      fDimensions[iSpacer][4] = fIrrZigHalfSizeX;
      fDimensions[iSpacer][5] = fIrrZigHalfSizeX;
      fDimensions[iSpacer][6] = 0;
      fDimensions[iSpacer][7] = fIrrZigHalfSizeY;
      fDimensions[iSpacer][8] = fIrrZigHalfSizeX;
      fDimensions[iSpacer][9] = fIrrZigHalfSizeX;
      fDimensions[iSpacer][10] = 0;
    }
    else if(iSpacer <= 33){ // 28-33: zigzags
      // Calculate the position of the Wall segments
      //place the blocks  -- iniialize outside the loop for front plate, add the next each 21 cm
      G4double PositionOfWallZ =  fPositionOfWallZ0 + (G4int)(iSpacer%7) * fSpaceToNextPlate;
      // Calculate the X and Y position of the vertical block wrt trapezoid centre
      // take into account the projectivity
      G4double VSegmentPositionX = PositionOfWallZ * cos(Phi) * sin(Theta) / cos(Theta);
      G4double VSegmentPositionY = PositionOfWallZ * sin(Phi) * sin(Theta) / cos(Theta);

      //leave 1. mm for safety reasons 
      //the electrode is a ribbon i.e. touches the walls of the plates

      //define two points of two successive plates:

      //electrode passes next to the wall of the hole

      G4double ZigZagPositionFrontEnd = VSegmentPositionY;
      G4double ZigZagPositionBackEnd = (PositionOfWallZ + fSpaceToNextPlate) * sin(Phi) * sin(Theta)/ cos(Theta);

      G4double ShiftZigZagX = fSpaceFromWallToElectrodeAtWallX - ElectrodeCell->GetSymAxis().x()/fabs(ElectrodeCell->GetSymAxis().x())*pow(-1,iSpacer%7)* 
        (fabs((PositionOfWallZ+fSpaceToNextPlate)*cos(Phi)*sin(Theta)/cos(Theta)-VSegmentPositionX));

      G4double ShiftZigZagY = ZigZagPositionBackEnd - ZigZagPositionFrontEnd;

      //define angle of deformation of the parallelepiped

      G4double theta_el = -pow(-1,iSpacer%7)*acos((fSpaceToNextPlate-2*fHalfSizeSpacerWallSegmentZ)/sqrt(ShiftZigZagX*ShiftZigZagX + 
            ShiftZigZagY*ShiftZigZagY + (fSpaceToNextPlate-2*fHalfSizeSpacerWallSegmentZ)*(fSpaceToNextPlate-2*fHalfSizeSpacerWallSegmentZ)));

      G4double phi_el = -pow(-1,iSpacer%7)*atan(ShiftZigZagY/ShiftZigZagX);


      fDimensions[iSpacer][0] = fIrrZigHalfSizeZ;
      fDimensions[iSpacer][1] = theta_el;
      fDimensions[iSpacer][2] = phi_el;
      fDimensions[iSpacer][3] = fIrrZigHalfSizeY;
      fDimensions[iSpacer][4] = fIrrZigHalfSizeX;
      fDimensions[iSpacer][5] = fIrrZigHalfSizeX;
      fDimensions[iSpacer][6] = 0;
      fDimensions[iSpacer][7] = fIrrZigHalfSizeY;
      fDimensions[iSpacer][8] = fIrrZigHalfSizeX;
      fDimensions[iSpacer][9] = fIrrZigHalfSizeX;
      fDimensions[iSpacer][10] = 0;
    }
  }
}

LKrIrregularVSpacerParameterisation::~LKrIrregularVSpacerParameterisation(){}

void LKrIrregularVSpacerParameterisation::ReadGeometryParameters(){}


void LKrIrregularVSpacerParameterisation::ComputeTransformation (const G4int iSpacer, G4VPhysicalVolume* physVol) const {


  G4ThreeVector origin; 
  origin = G4ThreeVector(fPosition[iSpacer][0],fPosition[iSpacer][1],fPosition[iSpacer][2]);

  physVol->SetTranslation(origin);
  physVol->SetRotation(0);
}

void  LKrIrregularVSpacerParameterisation::ComputeDimensions(G4Trap& parameterisedSpacer , const G4int iSpacer, const G4VPhysicalVolume* /*physVol*/) const{

  parameterisedSpacer.SetAllParameters(
      fDimensions[iSpacer][0],   //pDz:    Half z length
      fDimensions[iSpacer][1],   //pTheta: Polar angle of the line joining the centres of the faces at -/+pDz
      fDimensions[iSpacer][2],   //pPhi:   Azimuthal angle of the line joining the centres of the faces at -/+pDz
      fDimensions[iSpacer][3],   //pDy1:   Half y length at -pDz
      fDimensions[iSpacer][4],   //pDx1:   Half x length at -pDz, y=-pDy1
      fDimensions[iSpacer][5],   //pDx2:   Half x length at -pDz, y=+pDy1
      fDimensions[iSpacer][6],   //pAlp1:  Angle with respect to the y axis from the centre of the side (lower endcap)
      fDimensions[iSpacer][7],   //pDy2:   Half y length at +pDz
      fDimensions[iSpacer][8],   //pDx3:   Half x length at +pDz, y=-pDy2
      fDimensions[iSpacer][9],   //pDx4:   Half x length at +pDz, y=+pDy2
      fDimensions[iSpacer][10]   //pAlp2:  Angle with respect to the y axis from the centre of the side (upper endcap)
      );
}

  G4Material* LKrIrregularVSpacerParameterisation::ComputeMaterial(G4VPhysicalVolume* physVol, const G4int iSpacer, const G4VTouchable *){
  G4Material * Material;

  if(iSpacer < 21){
    Material = G4Material::GetMaterial("LKr_Epoxy");
  }
  else{
    Material = G4Material::GetMaterial("LKr_Ribbon");
  }

  physVol->GetLogicalVolume()->SetMaterial(Material);

  return Material; 
}

G4Material* LKrIrregularVSpacerParameterisation::GetMaterial(G4int iMaterial) const {
  G4Material* Material=0;
  if(iMaterial < 1)
    Material = G4Material::GetMaterial("LKr_Epoxy");
  else
    Material = G4Material::GetMaterial("LKr_Ribbon");

  return Material; 
}