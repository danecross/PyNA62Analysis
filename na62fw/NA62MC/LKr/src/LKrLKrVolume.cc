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

#include "LKrElectrodes.hh"
#include "LKrCryostat.hh"

#include "LKrLKrVolume.hh"
#include "LKrFrontBackPlate.hh"
#include "LKrStesalitPiece.hh"

#include "G4SDManager.hh"




#include "G4Region.hh"
#include "G4RegionStore.hh"

/// \class LKrLKrVolume 
/// \Brief
/// LKrLKrVolume class.
/// \EndBrief   
///
/// \Detailed
/// This class stores and provides the information about the geometry and position of the liquid krypton volume inside the inner 
/// cryostat.
/// \EndDetailed


LKrLKrVolume::LKrLKrVolume (G4Material * Material, G4LogicalVolume * MotherVolume, 
			    G4Transform3D  LKrVolumeTransform) :
  NA62VComponent(Material,MotherVolume) {
  ReadGeometryParameters();
  fLKrVolumeTransform = LKrVolumeTransform;

  // Mandatory here to Find or Build the needed materials
  LKrMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

LKrLKrVolume::~LKrLKrVolume(){}

void LKrLKrVolume::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();

    fPositionSteelBars= GeoPars->GetPositionSteelBars();  
    fInnerCryostatMinRadius = GeoPars->GetInnerCryostatMinRadius();
    fOuterBeamPipeRadius = GeoPars->GetOuterBeamPipeRadius(); 

    fLongitudinalLengthInnerCryo = GeoPars->GetLongitudinalLengthInnerCryo();
    fLongitudinalLengthLKrVolume = GeoPars->GetLongitudinalLengthLKrVolume();
    fRadiusSteelBars = GeoPars->GetRadiusSteelBars(); 
}

void LKrLKrVolume::CreateGeometry()
{

    G4double  pRMin = fOuterBeamPipeRadius;
    G4double  pRMax = fInnerCryostatMinRadius; 
    G4double  pDz =  fLongitudinalLengthLKrVolume/ 2; 
    

    G4double  pSPhi = 0*deg;
    G4double  pDPhi = 360*deg;

    solidLKrVolume = new G4Tubs("solid_LKrVolume",                        
            pRMin,
            pRMax,
            pDz,
            pSPhi,
            pDPhi);

    logicLKrVolume = new G4LogicalVolume(solidLKrVolume , fMaterial, "lkrvolume",0,0,0);
    physiLKrVolume = new G4PVPlacement(fLKrVolumeTransform,              // no rotation
            logicLKrVolume,    // its logical volume
            "lkrvolume",       // its name
            fMotherVolume,      // its mother  volume
            false,           // no boolean operations
            0);              // copy number

    fElectrodes = new LKrElectrodes(G4Material::GetMaterial("G4_lKr"),
				    logicLKrVolume);

    fFrontPlate = new LKrFrontBackPlate(G4Material::GetMaterial("LKr_Epoxy"),
            logicLKrVolume); 
    
    // -------------------------------Stainless Steel bars on the outer part of the Octagon
    ///// add Stainless Steel Bars

    G4double pRMin1 = 0 *cm;
    G4double pRMax1 = fRadiusSteelBars;

    G4Tubs* solidSteelBar = new G4Tubs("solid_Bar",                        
            pRMin1,
            pRMax1,
            pDz,
            pSPhi,
            pDPhi);

    G4LogicalVolume* logicSteelBar = new G4LogicalVolume(solidSteelBar,G4Material::GetMaterial("LKr_StainlessSteel"),"ParameterisedBar",0,0,0);
    G4RotationMatrix Ra;
    for(G4int iBar=0; iBar<30;iBar++ ) {
      physiSteelBar[iBar]= new G4PVPlacement(  G4Transform3D (Ra, fPositionSteelBars[iBar]),              // no rotation
					       logicSteelBar,    // its logical volume
					       "SteelBar",       // its name
					       logicLKrVolume,      // its mother  volume
					       false,           // no boolean operations
					       0);              // copy number
    }
    


    //*******************************************************
    //**************   stesalit around the beam pipe
    //*******************************************************


    for(G4int NSpacerPlate = 0;NSpacerPlate <7; NSpacerPlate ++ ){
      for (G4int IndexLine = 0; IndexLine < 3; IndexLine ++){
        
        G4int NCellsInLine;
        if (IndexLine == 0) NCellsInLine = 3;
      else NCellsInLine = 1;
    
        fStesalitPieces = new LKrStesalitPiece(G4Material::GetMaterial("LKr_Epoxy"),
        				       logicLKrVolume, IndexLine, NSpacerPlate,NCellsInLine,1);
        fStesalitPieces = new LKrStesalitPiece(G4Material::GetMaterial("LKr_Epoxy"),
        				       logicLKrVolume, IndexLine, NSpacerPlate,NCellsInLine,-1);
      } 
    }
    
    
}

void LKrLKrVolume::SetProperties()
{
    // Set visualization properties
    fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
    fVisAtt -> SetVisibility(true);
    logicLKrVolume ->SetVisAttributes(fVisAtt);
}
