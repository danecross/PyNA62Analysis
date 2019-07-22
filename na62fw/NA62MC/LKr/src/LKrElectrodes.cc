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
//  03-11-2017 Karim Massri (karim.massri@cern.ch)
//      - LKr(Irregular)ElectrodeParameterisation removed
//      - Major geometry overlaps fixed
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4Para.hh"
#include "G4LogicalVolume.hh"
#include "G4PVParameterised.hh"
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
#include "LKrVSpacerParameterisation.hh"
#include "LKrIrregularVSpacerParameterisation.hh"
#include "LKrElectrodes.hh"
#include "G4SDManager.hh"
#include "G4RegionStore.hh"
#include "G4ProductionCuts.hh"

/// \class LKrElectrodes 
/// \Brief
/// LKrElectrodes class.
/// \EndBrief   
/// \Detailed
/// This class stores and provides the information about the geometry and position of each horizontal row of electrode cells. Each row 
/// is built of parameterised electrode cells. The so called "irregular half cells" (around the beam pipe) are added here (to be 
/// finished).
/// \EndDetailed

LKrElectrodes::LKrElectrodes (G4Material * Material, G4LogicalVolume * MotherVolume) :
  NA62VComponent(Material,MotherVolume) {

  ReadGeometryParameters();

  // Mandatory here to Find or Build the needed materials
  LKrMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

LKrElectrodes::~LKrElectrodes(){}

void LKrElectrodes::ReadGeometryParameters(){

  // Read all the geometrical parameters and copy them to private members
  LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();
  fZtr = GeoPars->GetZtr();
  fXbackReferenceCell = GeoPars->GetXbackReferenceCell();
  fYbackReferenceCell = GeoPars->GetYbackReferenceCell();
  fXfrontReferenceCell = GeoPars->GetXfrontReferenceCell();
  fYfrontReferenceCell = GeoPars->GetYfrontReferenceCell();
  fXbackProjectivityReferenceCell = GeoPars->GetXbackProjectivityReferenceCell();
  fYbackProjectivityReferenceCell = GeoPars->GetYbackProjectivityReferenceCell();
  fProjectivityPointPositionZ = GeoPars->GetProjectivityPointPositionZ();
  fProjectivityAxisProjectionZ = GeoPars->GetProjectivityAxisProjectionZ();
  fDistanceToNextElectrodeFrontX = GeoPars->GetDistanceToNextElectrodeFrontX();
  fDistanceToNextElectrodeBackX = GeoPars->GetDistanceToNextElectrodeBackX();
  fDistanceToNextElectrodeFrontY = GeoPars->GetDistanceToNextElectrodeFrontY();
  fDistanceToNextElectrodeBackY = GeoPars->GetDistanceToNextElectrodeBackY();
  fHalfNElectrodesX = GeoPars->GetHalfNElectrodesX();
  fHalfNElectrodesY  = GeoPars->GetHalfNElectrodesY();
  fNRowsFullNumberElectrodes = GeoPars->GetNRowsFullNumberElectrodes();
  fRHoleX = GeoPars->GetRHoleX();
  fRHoleY  = GeoPars->GetRHoleY();
  fHalfZWidth   = GeoPars->GetHalfZWidth();
  fHalfYWidthF = GeoPars->GetHalfYWidthF();
  fHalfXWidthF = GeoPars->GetHalfXWidthF();
  fHalfYWidthB = GeoPars->GetHalfYWidthB();
  fHalfXWidthB = GeoPars->GetHalfXWidthB();

  fBackWallPositionZ = GeoPars->GetBackWallPositionZ();                              
  fHalfSizeCentralGap = GeoPars->GetHalfSizeCentralGap();

  fHalfLengthOfFrontPlateOctagon =  GeoPars->GetHalfLengthOfFrontPlateOctagon();
  fLongitudinalLengthInnerCryo =  GeoPars->GetLongitudinalLengthInnerCryo();

  fDistanceFrontPlateBackPlate = GeoPars->GetDistanceFrontPlateBackPlate();
  fLongitudinalLengthLKrVolume = GeoPars->GetLongitudinalLengthLKrVolume();
  fLengthOfColdWindowPlate =  GeoPars->GetLengthOfColdWindowPlate();

  //irregular electrodes
  fHalfIrregularCellSizeAtBackWallY = GeoPars->GetHalfIrregularCellSizeAtBackWallY();
  fHalfIrregularCellSizeAtFrontWallY = GeoPars->GetHalfIrregularCellSizeAtFrontWallY();
  fHalfYWidthIrregularB  = GeoPars->GetHalfYWidthIrregularB();
  fHalfYWidthIrregularF  = GeoPars->GetHalfYWidthIrregularF();
  fPassiveLKrInsideOctagon = GeoPars->GetPassiveLKrInsideOctagon();
  fLengthOfColdWindowPlate = GeoPars->GetLengthOfColdWindowPlate();
}

void LKrElectrodes::CreateGeometry(){

  G4double pDz = 0.;
  G4double pTheta = 0.; 
  G4double pPhi = 0.;
  G4double pDy1 = 0.; 
  G4double pDx1 = 0.; 
  G4double pDx2 = 0.; 
  G4double pAlp1 = 0.;
  G4double pDy2 = 0.; 
  G4double pDx3 = 0.; 
  G4double pDx4 = 0.; 
  G4double pAlp2 = 0.;

  G4double Xback = 0.;  
  G4double Yback = 0.;  

  G4double Theta = 0.;
  G4double Phi = 0.;
  G4double Xtr = 0.; 
  G4double Ytr = 0.;
  G4double Ztr = 0.;

  G4double YBackLongIrregularLine = 0.; 

  G4double YBackShortFirstIrregularLine = 0.;
  G4double XBackShortFirstLineLeft = 0.;   
  G4double XBackShortFirstLineRight = 0.;   

  G4double YBackShortSecondIrregularLine = 0.;
  G4double XBackShortSecondLineLeft = 0.; 
  G4double XBackShortSecondLineRight = 0.;   

  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  G4String LKrSensitiveDetectorName = "/LKr";
  G4VSensitiveDetector * lkrSD = SDman->FindSensitiveDetector(LKrSensitiveDetectorName);
  caloRegion = new G4Region("EMCalo_parameterization_region");

  for(G4int iLines = 0; iLines < 128; iLines ++){
    for(G4int iColumn = 0; iColumn < 1 + (iLines > 58 && iLines < 69); iColumn++){ //check column!
      G4int ParameterisedNumberElectrodes = 0;
      if(iLines < 37)      ParameterisedNumberElectrodes = 105 + iLines * 4;
      else{
        if(iLines > 26+64) ParameterisedNumberElectrodes  = 126 + 127 - (iLines - 26 - 64) * 4;
        else               ParameterisedNumberElectrodes  = 126 + 127;
      }
      if(iLines > 58 && iLines < 69){
        if(iLines > 60 && iLines < 67) ParameterisedNumberElectrodes = 117;
        else{
          if(iLines == 60 || iLines == 67) ParameterisedNumberElectrodes = 119;
          else                             ParameterisedNumberElectrodes = 121;
        }
        Xback = pow(-1.,iColumn)*((fHalfXWidthB*2*(126.5-ParameterisedNumberElectrodes/2))-fXbackReferenceCell
            - fHalfXWidthB * ((iColumn%2) ==1) + 3*fHalfXWidthB * ((iColumn%2) ==0)); 
      }
      else Xback = 0.;

      Yback =  fYbackReferenceCell + (63 - iLines) * fDistanceToNextElectrodeBackY;

      // define positions for the irregular cells here ..	  
      if (iLines == 66){
        YBackShortFirstIrregularLine = - Yback;
        if(iColumn ==  1)  
          XBackShortFirstLineLeft = Xback;   
        if(iColumn ==  0) 
          XBackShortFirstLineRight = Xback;   
      }
      if (iLines == 67){
        YBackShortSecondIrregularLine = - Yback;
        if(iColumn ==  1)    
          XBackShortSecondLineLeft = Xback; 
        if(iColumn ==  0) 
          XBackShortSecondLineRight = Xback;   
      }
      if (iLines == 68) YBackLongIrregularLine = - Yback;

      // end of definition ....	  

      Theta =  acos((fProjectivityAxisProjectionZ) /
          sqrt(Xback*Xback+Yback*Yback+fProjectivityAxisProjectionZ*fProjectivityAxisProjectionZ));

      // Azimutal angle of the projectivity axis
      Phi = atan2(Yback, Xback);

      pDx1 = fHalfXWidthF*ParameterisedNumberElectrodes; 
      pDx2 = fHalfXWidthF*ParameterisedNumberElectrodes; 
      pDy1 = fHalfYWidthF;
      pDx3 = fHalfXWidthB*ParameterisedNumberElectrodes;
      pDx4 = fHalfXWidthB*ParameterisedNumberElectrodes;
      pDy2 = fHalfYWidthB;
      pDz  = fHalfZWidth; 

      pTheta = Theta;
      pPhi   = Phi; 
      pAlp1  = 0*deg; 
      pAlp2  = 0*deg;

      Xtr = (fZtr-fProjectivityPointPositionZ)*sin(Theta)*cos(Phi)/cos(Theta);
      Ytr = (fZtr-fProjectivityPointPositionZ)*sin(Theta)*sin(Phi)/cos(Theta);
      //add the gap between the 2 halves
      Ytr = Ytr + Ytr/fabs(Ytr)*fHalfSizeCentralGap;
      Ztr = fZtr;

      std::ostringstream logname0;
      logname0 <<"LINESline"<<iLines <<"-"<< iColumn;
      solidLINES= new G4Trap(logname0.str(),
          pDz,  pTheta,
          pPhi, pDy1,
          pDx1, pDx2,
          pAlp1, pDy2,
          pDx3,  pDx4,
          pAlp2);

      logicLINES = new G4LogicalVolume(solidLINES , G4Material::GetMaterial("G4_lKr"), logname0.str(),0,0,0);

      physiLINES = new G4PVPlacement(0, // no rotation
          G4ThreeVector(Xtr,Ytr,Ztr),   // at (x,y,z)
          logicLINES,                   // its logical volume                      
          logname0.str(),               // its name
          fMotherVolume,                // its mother  volume
          false,                        // boolean operation 
          iLines);                      // copy number

      G4VisAttributes* TrapVisAtt= new G4VisAttributes(G4Colour(1.,0.2,0.2));
      //  TrapVisAtt->SetForceSolid(true);
      logicLINES->SetVisAttributes(TrapVisAtt);

      logicLINES->SetRegion(caloRegion);
      caloRegion->AddRootLogicalVolume(logicLINES);

      // dummy value : kZAxis -- modified by parameterised volume

      G4Trap *NonParameterisedElectrode;
      G4LogicalVolume* NonParameterisedElectrodeLogic;
      G4VPhysicalVolume* NonParameterisedElectrodePlacement;
      G4Trap * solidParameterisedVSpacer;
      G4LogicalVolume* logicParameterisedVSpacer;

      //
      //    DECIDE WHETHER YOU WANT NHOD INSIDE THE LKr - the two parts of the code are used alternatively , 
      //                             use make MDEFINES="LKrNHOD=1" for turning the NHOD on
      //

      //********************************************************************************************
      //      IF NOT DEFINED NHOD, 34 virtual trapezoids in total form the walls and the zigzags
      //


      for (int ll = 0; ll < ParameterisedNumberElectrodes; ll++){

        std::ostringstream logname;
        logname <<"LKrline"<<iLines<<"-"<<iColumn<<"-"<<ll;

        //********************************************************************************************************
        // Recalculate the X position of the backof the line. Needed for deformation calculation.
        // PAY attention how you calculate the angles of deformation. They must be calculated wrt the global volume.

        G4double XbackElGlobal = Xback+solidLINES->GetXHalfLength4()-(ll+0.5)*2.*fHalfXWidthB;
        G4double YbackElGlobal = Yback;

        G4double ThetaEl =  acos((fProjectivityAxisProjectionZ) /
            sqrt(XbackElGlobal*XbackElGlobal+YbackElGlobal*YbackElGlobal+fProjectivityAxisProjectionZ*fProjectivityAxisProjectionZ));

        // Azimutal angle of the projectivity axis
        G4double PhiEl = atan2(YbackElGlobal, XbackElGlobal);

        // Electrode Placement (wrt solidLINES)
        G4double XtrEl = (fZtr-fProjectivityPointPositionZ)*sin(ThetaEl)*cos(PhiEl)/cos(ThetaEl)-Xtr;
        G4double YtrEl = 0;
        G4double ZtrEl = fZtr - Ztr;

        NonParameterisedElectrode = new G4Trap(logname.str(),
            fHalfZWidth,
            ThetaEl,
            PhiEl,
            fHalfYWidthF,
            fHalfXWidthF,
            fHalfXWidthF, 
            0,
            fHalfYWidthB,
            fHalfXWidthB,
            fHalfXWidthB,
            0);

        NonParameterisedElectrodeLogic = new G4LogicalVolume(NonParameterisedElectrode,G4Material::GetMaterial("G4_lKr"), logname.str(),0,0,0);
        NonParameterisedElectrodeLogic->SetSensitiveDetector(lkrSD);
        NonParameterisedElectrodePlacement = new G4PVPlacement(0, // no rotation
            G4ThreeVector(XtrEl,YtrEl,ZtrEl), // at (x,y,z)
            NonParameterisedElectrodeLogic,   // its logical volume                      
            logname.str(),                    // its name
            logicLINES,                       // its mother  volume
            false,                            // boolean operation 
            0);                               // copy number
        G4VisAttributes* TrapVisAtt1= new G4VisAttributes(G4Colour(0.6,0.0,0.0));
        TrapVisAtt1->SetForceWireframe(true);

        NonParameterisedElectrodeLogic->SetVisAttributes(TrapVisAtt1);
#ifndef LKrNHOD	    
        VSpacerParameterisation = new LKrVSpacerParameterisation(NonParameterisedElectrodePlacement);     
        std::ostringstream logname1;
        logname1 << "VSpacerline"<<iLines<<"-"<<iColumn<<"-"<<ll;
        // define trapezoid with random initial size, it'll be changed by the parameterisation.
        solidParameterisedVSpacer = new G4Trap(logname1.str(),1*mm, 0, 0, 1*mm, 1*mm,1*mm, 0, 1*mm, 1*mm,1*mm, 0);
        logicParameterisedVSpacer = new G4LogicalVolume(solidParameterisedVSpacer,G4Material::GetMaterial("LKr_Epoxy"),logname1.str(),0,0,0);
        physiParameterisedVSpacer = new G4PVParameterised(
            logname1.str(),                 // their name
            logicParameterisedVSpacer,      // their logical volume
            NonParameterisedElectrodeLogic, // Mother logical volume 
            kUndefined,                     // Are placed along this axis 
            21,                             // Number of chambers
            VSpacerParameterisation);       // The parametrisation
        logicParameterisedVSpacer->SetVisAttributes(TrapVisAtt1);
#endif
      }
    }   
  }


  //************************************************************************88
  //
  //                     Irregular electrodes
  //


  G4Trap* NonParameterisedIrregularElectrode;
  G4LogicalVolume * logicNonParameterisedIrregularElectrode;
  G4VPhysicalVolume * physiNonParameterisedIrregularElectrode;

  for(G4int iIrregularLines = 0; iIrregularLines < 10; iIrregularLines ++){

    // Total number in one half of the LKr: 19 electrodes (= one long line, 4 short)
    // - short irregular lines ParameterisedNumberElectrodes = 2;
    // - long irregular lines ParameterisedNumberElectrodes = 11;

    //
    // long lines
    //
    if(iIrregularLines == 0){ // long line, down
      Xback =  fXbackReferenceCell;   //for long irregular lines
      Yback = -(YBackLongIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 11;
    }
    if(iIrregularLines == 1){ //long line up
      Xback =  fXbackReferenceCell;   //for long irregular lines
      Yback = YBackLongIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB;
      ParameterisedNumberElectrodes = 11;
    }
    //
    // first lines, closer to the center
    //
    if(iIrregularLines == 2){ // short line, down left, first irregular line (counting from the center)  
      Xback = XBackShortFirstLineLeft + (117 + 2) * fHalfXWidthB;    //for long irregular lines
      Yback = -( YBackShortFirstIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 2;
    }
    if(iIrregularLines == 3){ // short line, down right, first irregular line (counting from the center)  
      Xback = XBackShortFirstLineRight - (117 + 2) * fHalfXWidthB;    
      Yback = -( YBackShortFirstIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 2;
    }
    if(iIrregularLines == 4){ // short line, up left, first irregular line (counting from the center)  
      Xback = XBackShortFirstLineLeft + (117 + 2) * fHalfXWidthB;    
      Yback = ( YBackShortFirstIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 2;
    }
    if(iIrregularLines == 5){ // short line, up right, first irregular line (counting from the center)  
      Xback = XBackShortFirstLineRight - (117 + 2) * fHalfXWidthB;   
      Yback = ( YBackShortFirstIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 2;
    }

    //
    // second line
    //

    if(iIrregularLines == 6){ // short line, down left, first irregular line (counting from the center)  
      Xback = XBackShortSecondLineLeft +  (119 + 2) * fHalfXWidthB;    //for long irregular lines
      Yback = -( YBackShortSecondIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 2;
    }
    if(iIrregularLines == 7){ // short line, down right, first irregular line (counting from the center)  
      Xback = XBackShortSecondLineRight - (119 + 2) * fHalfXWidthB;   
      Yback = -( YBackShortSecondIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 2;
    }
    if(iIrregularLines == 8){ // short line, up left, first irregular line (counting from the center)  
      Xback = XBackShortSecondLineLeft + (119 + 2) * fHalfXWidthB;   
      Yback = ( YBackShortSecondIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 2;
    }
    if(iIrregularLines == 9){ // short line, up right, first irregular line (counting from the center)  
      Xback = XBackShortSecondLineRight - (119 + 2) * fHalfXWidthB;   
      Yback = ( YBackShortSecondIrregularLine + fHalfYWidthB - fHalfYWidthIrregularB);
      ParameterisedNumberElectrodes = 2;
    }

    Theta =  acos((fProjectivityAxisProjectionZ) / 
        sqrt(Xback* Xback + Yback * Yback + fProjectivityAxisProjectionZ * fProjectivityAxisProjectionZ));

    // Azimutal angle of the projectivity axis
    Phi = atan2(Yback, Xback);
    pDx1 = fHalfXWidthF*ParameterisedNumberElectrodes; 
    pDx2 = fHalfXWidthF*ParameterisedNumberElectrodes; 
    pDy1 = fHalfYWidthIrregularF;
    pDx3 = fHalfXWidthB*ParameterisedNumberElectrodes;
    pDx4 = fHalfXWidthB*ParameterisedNumberElectrodes;
    pDy2 = fHalfYWidthIrregularB;
    pDz = fHalfZWidth; 

    pTheta = Theta;
    pPhi = Phi; 
    pAlp1 = 0*deg; 
    pAlp2 = 0*deg;

    Xtr = (fZtr - fProjectivityPointPositionZ) * Xback / fProjectivityAxisProjectionZ;
    Ytr = (fZtr - fProjectivityPointPositionZ) * Yback / fProjectivityAxisProjectionZ; 
    //add the gap between the 2 halves
    Ytr = Ytr + Ytr/fabs(Ytr)*fHalfSizeCentralGap;
    Ztr = fZtr;

    std::ostringstream logname0;
    logname0 <<"IrrLINESline"<<iIrregularLines;
    solidIrregularLINES= new G4Trap(logname0.str(),
        pDz,  pTheta,
        pPhi, pDy1,
        pDx1, pDx2,
        pAlp1, pDy2,
        pDx3,  pDx4,
        pAlp2);

    logicIrregularLINES = new G4LogicalVolume(solidIrregularLINES , G4Material::GetMaterial("G4_lKr"), "logicIrregularLINES",0,0,0);

    physiIrregularLINES = new G4PVPlacement(0,  // no rotation
        G4ThreeVector(Xtr,Ytr,Ztr), // at (x,y,z)
        logicIrregularLINES,        // its logical volume                      
        "IrregularLines",           // its name
        fMotherVolume,              // its mother  volume
        false,                      //
        iIrregularLines             // copy number
        );

    logicIrregularLINES->SetRegion(caloRegion);
    caloRegion->AddRootLogicalVolume(logicIrregularLINES);

    for (int ll = 0; ll < ParameterisedNumberElectrodes; ll++){

      std::ostringstream logname;
      logname <<"LKrIrrline"<<iIrregularLines<<"-"<<ll;

      //********************************************************************************************************
      // Recalculate the X position of the backof the line. Needed for deformation calculation.
      // PAY attention how you calculate the angles of deformation. They must be calculated wrt the global volume.

      G4double XbackElGlobal = Xback+solidIrregularLINES->GetXHalfLength4()-(ll+0.5)*2.*fHalfXWidthB;
      G4double YbackElGlobal = Yback;

      G4double ThetaEl =  acos((fProjectivityAxisProjectionZ) /
          sqrt(XbackElGlobal*XbackElGlobal+YbackElGlobal*YbackElGlobal+fProjectivityAxisProjectionZ*fProjectivityAxisProjectionZ));

      // Azimutal angle of the projectivity axis
      G4double PhiEl = atan2(YbackElGlobal, XbackElGlobal);

      // Electrode Placement (wrt solidIrregularLINES)
      G4double XtrEl = (fZtr-fProjectivityPointPositionZ)*sin(ThetaEl)*cos(PhiEl)/cos(ThetaEl)-Xtr;
      G4double YtrEl = 0;
      G4double ZtrEl = fZtr - Ztr;

      NonParameterisedIrregularElectrode = new G4Trap(logname.str(),
          fHalfZWidth,
          ThetaEl,
          PhiEl,
          fHalfYWidthIrregularF,
          fHalfXWidthF,
          fHalfXWidthF,
          0,
          fHalfYWidthIrregularB ,
          fHalfXWidthB,
          fHalfXWidthB,
          0);

      logicNonParameterisedIrregularElectrode = new G4LogicalVolume(NonParameterisedIrregularElectrode, G4Material::GetMaterial("G4_lKr"),
          "Parameterised_IrregularElectrode",0,0,0);

      // dummy value : kZAxis -- modified by parameterised volume

      physiNonParameterisedIrregularElectrode = new G4PVPlacement(0,              // no rotation
          G4ThreeVector(XtrEl,YtrEl,ZtrEl), // at (x,y,z)
          logicNonParameterisedIrregularElectrode,    // its logical volume                      
          logname.str(),               // its name
          logicIrregularLINES,         // its mother  volume
          false,                       // boolean operation 
          0);                          // copy number

      G4VisAttributes* TrapVisAtt= new G4VisAttributes(G4Colour(0.,0.,0.9));
      //  TrapVisAtt->SetForceSolid(true);
      logicNonParameterisedIrregularElectrode->SetVisAttributes(TrapVisAtt);
      logicNonParameterisedIrregularElectrode->SetSensitiveDetector(lkrSD);

      IrregularVSpacerParameterisation = new LKrIrregularVSpacerParameterisation( physiNonParameterisedIrregularElectrode );     
      std::ostringstream logname1;
      logname1 << "VSpacerIrrline"<<iIrregularLines<<"-"<<ll;
      // define trapezoid with random initial size, it'll be changed by the parameterisation.
      solidParameterisedIrregularVSpacer = new G4Trap(logname.str(),1*mm, 0, 0, 1*mm, 1*mm,1*mm, 0, 1*mm, 1*mm,1*mm, 0);
      logicParameterisedIrregularVSpacer = new G4LogicalVolume(solidParameterisedIrregularVSpacer,G4Material::GetMaterial("LKr_Epoxy"),
          logname1.str(),0,0,0);
      physiParameterisedIrregularVSpacer = new G4PVParameterised(
          logname1.str(),                          // their name
          logicParameterisedIrregularVSpacer,      // their logical volume
          logicNonParameterisedIrregularElectrode, // Mother logical volume 
          kUndefined,                              // Are placed along this axis 
          34,                                      // Number of chambers
          IrregularVSpacerParameterisation);       // The parametrisation

      TrapVisAtt= new G4VisAttributes(G4Colour(0.,0.6,0.3));
      //  TrapVisAtt->SetForceSolid(true);
      logicParameterisedIrregularVSpacer->SetVisAttributes(TrapVisAtt);
    }
  }
  //fShowers = new LKrEMShowers("fastShowerSimu", caloRegion);
}

void LKrElectrodes::SetProperties(){
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  //fLogicalVolume ->SetVisAttributes(fVisAtt);
}
