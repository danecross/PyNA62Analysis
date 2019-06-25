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
#include "LKrFlange.hh"
#include "G4SDManager.hh"
#include "LKrLKrVolume.hh"
#include "LKrFrontBackPlate.hh"

#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"


/// \class LKrFlange 
/// \Brief
/// Single flange (used in LKr).
/// \EndBrief   
///
/// \Detailed
/// This class creates a general flange.
/// \EndDetailed



LKrFlange::LKrFlange (G4String Name,G4Material * Material, G4LogicalVolume * MotherVolume, G4double MinRadius, G4double Hight, G4double Length, G4ThreeVector Position):
    NA62VComponent(Material,MotherVolume)
{
    ReadGeometryParameters();

    fMinRadius = MinRadius;
    fLength = Length;
    fHight = Hight;
    fPosition = Position;
    fName = Name;


    // Mandatory here to Find or Build the needed materials
    LKrMaterialParameters::GetInstance();
    CreateGeometry();
    SetProperties();
}

LKrFlange::~LKrFlange(){}

void LKrFlange::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    //LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();

}

void LKrFlange::CreateGeometry()
{

  G4double  pSPhi = 0*deg;
  G4double  pDPhi = 360*deg;
      
    //************  general flange

 
  
  fSolidVolume = new G4Tubs("solid_Flange",                        
			    fMinRadius,
			    fMinRadius + fHight,
			    fLength/ 2,
			    pSPhi,
			    pDPhi);
  
  
  fLogicalVolume = new G4LogicalVolume(fSolidVolume,        // solid
				       fMaterial,             // material
				       fName,           // name
				       0,                    // field manager
				       0,                    // sensitive detector
				       0);                   // user limits
  
  
    
  
  G4RotationMatrix Ra =  G4RotationMatrix(G4ThreeVector(0., 0. ,0.),0*deg);
  
  
  
  
  
  fPhysicalVolume = new G4PVPlacement(G4Transform3D(Ra,fPosition),
				      fLogicalVolume,             // its logical volume
				      fName,                // its name
				      fMotherVolume,              // its mother  volume
				      false,                     // no boolean operations
				      0);                        // copy number
  


    
}

void LKrFlange::SetProperties()
{
    // Set visualization properties
    fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
    fVisAtt -> SetVisibility(true);
    fLogicalVolume ->SetVisAttributes(fVisAtt);
}
