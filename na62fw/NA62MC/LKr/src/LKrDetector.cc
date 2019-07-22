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
//            Evelina Marinova(Evelina.Marinova@cern.ch)
//
//  03-09-2012 Sergey Podolsky (siarhei.padolski@cern.ch)      
//
// --------------------------------------------------------------
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4VPhysicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "LKrGeometryParameters.hh"
#include "LKrMaterialParameters.hh"

#include "LKrDetector.hh"
#include "LKrDetectorMessenger.hh"

#include "LKrSD.hh"
#include "G4SDManager.hh"

#include "LKrElectrodes.hh"
#include "LKrCryostat.hh"
#include "LKrBeamPipe.hh"
#include "LKrLKrVolume.hh"
#include "LKrFrontBackPlate.hh"
#include "LKrColdWindow.hh"
#include "LKrWarmWindow.hh"


/// \class LKrDetector 
/// \Brief
/// LKrDetector class.
/// \EndBrief   
///
/// \Detailed
/// This class stores and provides the information about the geometry and position of the LKr Detector responsible region.
/// \EndDetailed


LKrDetector::LKrDetector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
    NA62VComponent(Material,MotherVolume), NA62VNamed("LKr"),
    fCryostat(nullptr),
    fBeamPipe(nullptr),
    fLKrColdWindow(nullptr),
    fLKrWarmWindow(nullptr),
    fXLength(.0),
    fYLength(.0),
    fZLength(.0),
    fZPosition(.0)
{

    // Mandatory here to Find or Build the needed materials
    LKrMaterialParameters::GetInstance();
  
   // Connect to LKrDetectorMessenger to enable datacard configuration
   fLKrMessenger = new LKrDetectorMessenger(this);

}

LKrDetector::~LKrDetector()
{
  delete fLKrMessenger;
}

void LKrDetector::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();

    fXLength = GeoPars->GetLKrDetectorXLength();
    fYLength = GeoPars->GetLKrDetectorYLength();
    fZLength = GeoPars->GetLKrDetectorZLength();

    fZPosition = GeoPars->GetLKrDetectorZPosition();
}

void LKrDetector::CreateGeometry()
{
    ReadGeometryParameters();

    // Sensitive detector must be instantiated and registered here
    // because each lkr cell will search it to attach it to the lkr

    G4SDManager* SDman = G4SDManager::GetSDMpointer();
    G4String LKrSensitiveDetectorName = "/LKr";
    G4String LKrCollectionName= "LKrCollection";
    LKrSD * lkrSD = static_cast<LKrSD*>(SDman->FindSensitiveDetector(LKrSensitiveDetectorName));
    if(!lkrSD){
        lkrSD = new LKrSD(LKrSensitiveDetectorName, LKrCollectionName);
        SDman->AddNewDetector(lkrSD);
    }

    // Build one or more boxes that will contain all the 
    // detector sections, up to fill the responsibility region

    G4double HalfZLength = 0.5*fZLength;
    G4double HalfXLength = 0.5*fXLength;
    G4double HalfYLength = 0.5*fYLength;

    fSolidVolume= new G4Box("LKr",HalfXLength,HalfYLength,HalfZLength);

    fLogicalVolume= new G4LogicalVolume(fSolidVolume,                        // solid
            fMaterial, // material
            "LKr",                           // name
            0,                                 // field manager 
            0,                                 // sensitive detector
            0);                                // user limits

    fPhysicalVolume = new G4PVPlacement(0,
            G4ThreeVector(0.,0.,fZPosition),
            fLogicalVolume,      // its logical volume
            "LKr",         // its name
            fMotherVolume,               // its mother  volume
            false,           // no boolean operations
            0);              // copy number


    G4RotationMatrix Ra;
    G4ThreeVector Ta = G4ThreeVector(0,0,0);

    fCryostat = new LKrCryostat(G4Material::GetMaterial("LKr_StainlessSteel"),
            fLogicalVolume, G4Transform3D(Ra,Ta));
    
    fBeamPipe = new LKrBeamPipe(G4Material::GetMaterial("LKr_StainlessSteel"),
            fLogicalVolume, G4Transform3D(Ra,Ta));
    
    fLKrColdWindow = new LKrColdWindow(G4Material::GetMaterial("LKr_StainlessSteel"),
				       fLogicalVolume);
     
    fLKrWarmWindow = new LKrWarmWindow(G4Material::GetMaterial("G4_Al"),
				       fLogicalVolume);
    
    SetProperties();
}

void LKrDetector::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}


