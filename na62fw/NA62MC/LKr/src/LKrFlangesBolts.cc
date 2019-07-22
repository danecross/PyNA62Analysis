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
#include "LKrFlangesBolts.hh"
#include "G4SDManager.hh"
#include "LKrLKrVolume.hh"
#include "LKrFrontBackPlate.hh"

#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"


/// \class LKrFlangesBolts 
/// \Brief
/// Single bolt at a flange of the LKr.
/// \EndBrief   
///
/// \Detailed
/// This class constructs the bolts of the flanges, or, if vaciim material used, some hole bedore the head of the bolts (again in the flanges).
/// \EndDetailed



LKrFlangesBolts::LKrFlangesBolts (G4String Name, G4Material * Material, G4LogicalVolume * MotherVolume, G4double Radius, G4double Length, G4double  RPosition, G4double ThetaPosition, G4double ZOfHolesBolts):
    NA62VComponent(Material,MotherVolume)
{

    ReadGeometryParameters();
    fRadius = Radius;
    fLength = Length;
    fRPosition = RPosition;
    fThetaPosition = ThetaPosition;
    fZOfHolesBolts = ZOfHolesBolts;
    fName =Name;

    // Mandatory here to Find or Build the needed materials
    LKrMaterialParameters::GetInstance();
    CreateGeometry();
    SetProperties();
}

LKrFlangesBolts::~LKrFlangesBolts(){}

void LKrFlangesBolts::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    //LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();

}

void LKrFlangesBolts::CreateGeometry()
{

  // create solid beam pipe so this volume can later be subtracted of the windows and the filler

  G4double  pSPhi = 0*deg;
  G4double  pDPhi = 360*deg;
      
    // bolts in part one of the flange at the front window around the beam pipe

 
    fSolidVolume = new G4Tubs("solid_Bolts",                        
			      0,
			      fRadius,
			      fLength/ 2,
			      pSPhi,
			      pDPhi);
    
    fLogicalVolume = new G4LogicalVolume(fSolidVolume,        // solid
					 fMaterial,             // material
					 fName,           // name
					 0,                    // field manager
					 0,                    // sensitive detector
					 0);                   // user limits
    
    
    G4double XOfHolesBolts = fRPosition * cos(fThetaPosition);
    G4double YOfHolesBolts = fRPosition * sin(fThetaPosition);
    
    G4RotationMatrix Ra =  G4RotationMatrix(G4ThreeVector(0., 0. ,0.),0*deg);
    
    
    G4ThreeVector Ta = G4ThreeVector(XOfHolesBolts,YOfHolesBolts,fZOfHolesBolts);
    
    fPhysicalVolume = new G4PVPlacement(G4Transform3D(Ra,Ta),
					fLogicalVolume,             // its logical volume
					fName,                // its name
					fMotherVolume,              // its mother  volume
					false,                     // no boolean operations
					0);                        // copy number
    
    
    
    
}

void LKrFlangesBolts::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
