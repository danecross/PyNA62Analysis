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
#include "LKrCryostat.hh"
#include "G4SDManager.hh"
#include "LKrLKrVolume.hh"
#include "LKrFrontBackPlate.hh"

/// \class LKrCryostat 
/// \Brief
/// LKrCryostat class.
/// \EndBrief   
///
/// \Detailed
/// This class stores and provides the information about the geometry and position of the inner and the outer cryostat (without the front and back windows).
/// \EndDetailed



LKrCryostat::LKrCryostat (G4Material * Material, G4LogicalVolume * MotherVolume, 
        G4Transform3D  CryostatTransform, G4String PathToLKrShowersDb):
    NA62VComponent(Material,MotherVolume), fPathToLKrShowersDb(PathToLKrShowersDb)
{
    ReadGeometryParameters();
    fCryostatTransform = CryostatTransform;

    // Mandatory here to Find or Build the needed materials
    LKrMaterialParameters::GetInstance();
    CreateGeometry();
    SetProperties();
}

LKrCryostat::~LKrCryostat(){}

void LKrCryostat::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();
  
  fInnerCryostatMinRadius = GeoPars->GetInnerCryostatMinRadius();
  fThicknessOfInnerCryo = GeoPars->GetThicknessOfInnerCryo();
  fLongitudinalLengthInnerCryo = GeoPars->GetLongitudinalLengthInnerCryo();
  
  fOuterCryostatMinRadius = GeoPars->GetOuterCryostatMinRadius();
  fThicknessOfOuterCryo = GeoPars->GetThicknessOfOuterCryo(); 
  fLongitudinalLengthOuterCryo = GeoPars->GetLongitudinalLengthOuterCryo();
  
  fInnerDistanceFrontBackWarmWindow = GeoPars->GetInnerDistanceFrontBackWarmWindow();
  fDistanceFrontWarmWindowLKr = GeoPars->GetDistanceFrontWarmWindowLKr();
  
  fLongitudinalLengthLKrVolume  = GeoPars->GetLongitudinalLengthLKrVolume();
  fLengthOfColdWindowPlate = GeoPars->GetLengthOfColdWindowPlate();
  
  fBEATCHPositionFrontWarmWindowEdge = GeoPars->GetBEATCHPositionFrontWarmWindowEdge();
  fBEATCHPositionFrontEdgeOuterVessel = GeoPars->GetBEATCHPositionFrontEdgeOuterVessel();
  fBEATCHPositionBackEdgeOuterVessel = GeoPars->GetBEATCHPositionBackEdgeOuterVessel();
  fBEATCHPositionBackWarmWindowEdge = GeoPars->GetBEATCHPositionBackWarmWindowEdge();

  fRadiusHoleSpacerPlate = GeoPars->GetRadiusHoleSpacerPlate();


  fElectronicsBackG10HalfLength = GeoPars->GetElectronicsBackG10HalfLength();
  fElectronicsBackCuHalfLength = GeoPars->GetElectronicsBackCuHalfLength();
  fElectronicsBackBrassHalfLength = GeoPars->GetElectronicsBackBrassHalfLength();
  fElectronicsBackTeflonHalfLength = GeoPars->GetElectronicsBackTeflonHalfLength();
}

void LKrCryostat::CreateGeometry()
{
  G4double  pSPhi = 0*deg;
  G4double  pDPhi = 360*deg;
  
  // Outer cryostat
  G4double  pRMin1 = fOuterCryostatMinRadius;
  G4double  pRMax1 = fOuterCryostatMinRadius + fThicknessOfOuterCryo ; 
  G4double  pDz1 = fLongitudinalLengthOuterCryo / 2; 
  
  solidOuterCryostat = new G4Tubs("solid_OuterCryostat",                        
				  pRMin1,
				  pRMax1,
				  pDz1,
				  pSPhi,
				  pDPhi);
  
  ///material --> Aluminium
  logicOuterCryostat = new G4LogicalVolume(solidOuterCryostat ,G4Material::GetMaterial("G4_Al") , "outer",0,0,0);
  physiOuterCryostat = new G4PVPlacement(fCryostatTransform,              // no rotation
					 logicOuterCryostat,    // its logical volume
					 "outer",       // its name
					 fMotherVolume,      // its mother  volume
					 false,           // no boolean operations
					 0);              // copy number
  
  
  //G4cout << " Outer cryo " << G4endl;
  //**********************************************************************************
  //***************************************      INNER CRYO INSIDE LKrBox 
  
  
  
  G4double  pRMin = fInnerCryostatMinRadius;
  G4double  pRMax = fInnerCryostatMinRadius + fThicknessOfInnerCryo ; 
  
  G4double  pDz =  fLongitudinalLengthInnerCryo / 2;  
  
  solidInnerCryostat = new G4Tubs("solid_InnerCryostat",                        
				  pRMin,
				  pRMax,
				  pDz,
				  pSPhi,
				  pDPhi);
      
  logicInnerCryostat = new G4LogicalVolume(solidInnerCryostat , fMaterial, "inner",0,0,0);
  
  G4RotationMatrix Ra;
  
  G4double OffsetLKrVolume =  ( fLongitudinalLengthOuterCryo + (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge) 
				+ ( fBEATCHPositionBackWarmWindowEdge -  fBEATCHPositionBackEdgeOuterVessel) 
				- fInnerDistanceFrontBackWarmWindow )/2 +  fDistanceFrontWarmWindowLKr;
  
  G4ThreeVector Ta = G4ThreeVector(0,0, - fLongitudinalLengthOuterCryo/2 
				   - (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge)
				   + fLongitudinalLengthInnerCryo/2 + OffsetLKrVolume);
  
  physiInnerCryostat = new G4PVPlacement(G4Transform3D(Ra,Ta),              // no rotation
					 logicInnerCryostat,    // its logical volume
					 "inner",       // its name
					 fMotherVolume,      // its mother  volume
					 false,           // no boolean operations
					 0);              // copy number

  //**********************************************************
  //****************************************    LKR VOLUME  
  
  Ta = G4ThreeVector(0,0, - fLongitudinalLengthInnerCryo/2 +  fLongitudinalLengthLKrVolume/2 + Ta.z());
  
  //G4cout << " call lkr volume " << G4endl;
  fLKrVolume = new LKrLKrVolume(G4Material::GetMaterial("G4_lKr"),
				fMotherVolume,
				G4Transform3D(Ra,Ta), fPathToLKrShowersDb);


  //****************************************************************
  //*************************** electronics in the back - naive model

  //material --> Stesalit
  
  solidElectronics = new G4Tubs("solid_Electronics",                        
				fRadiusHoleSpacerPlate,
				fInnerCryostatMinRadius,
				fElectronicsBackG10HalfLength,
				pSPhi,
				pDPhi);
  
  logicElectronics = new G4LogicalVolume(solidElectronics , G4Material::GetMaterial("LKr_Epoxy"), "electroniscs",0,0,0);
  
  Ta = G4ThreeVector(0,0, - fLongitudinalLengthOuterCryo/2 
		     - (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge)
		     + fLongitudinalLengthInnerCryo/2 + OffsetLKrVolume 
		     - fElectronicsBackG10HalfLength + fLongitudinalLengthLKrVolume/ 2);
  
  physiElectronics = new G4PVPlacement(G4Transform3D(Ra,Ta),              // no rotation
				       logicElectronics,    // its logical volume
				       "electronics",       // its name
				       fMotherVolume,      // its mother  volume
				       false,           // no boolean operations
				       0);              // copy number
 
  //material --> Cu 
  
  solidElectronicsCu = new G4Tubs("solid_ElectronicsCu",                        
				  fRadiusHoleSpacerPlate,
				  fInnerCryostatMinRadius,
				  fElectronicsBackCuHalfLength,
				  pSPhi,
				  pDPhi);
  
  logicElectronicsCu = new G4LogicalVolume(solidElectronicsCu , G4Material::GetMaterial("G4_Cu"), "electroniscsCu",0,0,0);
  
  Ta = G4ThreeVector(0,0, Ta.z()+ fElectronicsBackG10HalfLength+fElectronicsBackCuHalfLength);
  
  physiElectronicsCu = new G4PVPlacement(G4Transform3D(Ra,Ta),              // no rotation
					 logicElectronicsCu,    // its logical volume
					 "electronicsCu",       // its name
					 fMotherVolume,      // its mother  volume
					 false,           // no boolean operations
					 0);              // copy number

  //material --> Teflon 

 solidElectronicsTeflon = new G4Tubs("solid_ElectronicsTeflon",                        
				fRadiusHoleSpacerPlate,
				fInnerCryostatMinRadius,
				fElectronicsBackTeflonHalfLength,
				pSPhi,
				pDPhi);
      
  logicElectronicsTeflon = new G4LogicalVolume(solidElectronicsTeflon, G4Material::GetMaterial("G4_TEFLON"), "electroniscsTeflon",0,0,0);
  
  Ta = G4ThreeVector(0,0, Ta.z()+ fElectronicsBackTeflonHalfLength + fElectronicsBackCuHalfLength);
  
  physiElectronicsTeflon = new G4PVPlacement(G4Transform3D(Ra,Ta),              // no rotation
				       logicElectronicsTeflon,    // its logical volume
				       "electronicsTeflon",       // its name
				       fMotherVolume,      // its mother  volume
				       false,           // no boolean operations
				       0);              // copy number


 //material --> Brass 

 solidElectronicsBrass = new G4Tubs("solid_ElectronicsBrass",                        
				fRadiusHoleSpacerPlate,
				fInnerCryostatMinRadius,
				fElectronicsBackBrassHalfLength,
				pSPhi,
				pDPhi);
    
  logicElectronicsBrass = new G4LogicalVolume(solidElectronicsTeflon, G4Material::GetMaterial("LKr_Brass"), "electroniscsBrass",0,0,0);
  
  Ta = G4ThreeVector(0,0, Ta.z()+ fElectronicsBackTeflonHalfLength+fElectronicsBackBrassHalfLength);
  
  physiElectronicsBrass = new G4PVPlacement(G4Transform3D(Ra,Ta),              // no rotation
				       logicElectronicsBrass,    // its logical volume
				       "electronicsBrass",       // its name
				       fMotherVolume,      // its mother  volume
				       false,           // no boolean operations
				       0);              // copy number


 
}

void LKrCryostat::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  logicOuterCryostat ->SetVisAttributes(fVisAtt);
}
