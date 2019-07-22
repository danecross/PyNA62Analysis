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
// 2014-03-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - NewCHOD simulation added
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-02
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"
#include "G4Transform3D.hh"
#include "G4SDManager.hh"

#include "BeamPipe.hh"
#include "CHODGeometryParameters.hh"
#include "CHODMaterialParameters.hh"
#include "CHODDetector.hh"
#include "CHODPlane.hh"
#include "CHODScintillatorCounter.hh"
#include "CHODSD.hh"

CHODDetector::CHODDetector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume), NA62VNamed("CHOD") {
  // Mandatory here to find or build the needed materials
  CHODMaterialParameters::GetInstance();
}

void CHODDetector::ReadGeometryParameters() {

  CHODGeometryParameters* GeoPars = CHODGeometryParameters::GetInstance();

  fRespRegionZStart  = GeoPars->GetRespRegionZStart();
  fRespRegionZEnd    = GeoPars->GetRespRegionZEnd();
  fRespRegionZCentre = GeoPars->GetRespRegionZCentre();
  fRespRegionXLength = GeoPars->GetRespRegionXLength();
  fRespRegionYLength = GeoPars->GetRespRegionYLength();
  fRespRegionZLength = GeoPars->GetRespRegionZLength();

  fNPlanes        = GeoPars->GetNPlanes();
  fNCounters      = GeoPars->GetNCounters();
  fZPositionVer   = GeoPars->GetDetectorZPositionVer();
  fZPositionHor   = GeoPars->GetDetectorZPositionHor();
  fPlanePosZ[0]   = fZPositionVer - fRespRegionZCentre;
  fPlanePosZ[1]   = fZPositionHor - fRespRegionZCentre;
  fPlaneRotZ[0]   = GeoPars->GetDetectorZRotationVer();
  fPlaneRotZ[1]   = GeoPars->GetDetectorZRotationHor();
  fScintThickness = GeoPars->GetScintThickness();
  fInnerRadius    = GeoPars->GetInnerRadius();
  fOuterRadius    = GeoPars->GetOuterRadius();
  for (G4int i=0; i<fNCounters; i++) {
    fScintSize[i]     = GeoPars->GetScintSize(i);
    fScintPosition[i] = GeoPars->GetScintPosition(i);
  }
}

void CHODDetector::CreateGeometry() {
  ReadGeometryParameters();

  // Example of sensitive detector; in general it would be
  // associated to smaller volume/s inside the global box/es
  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  G4String CHODSensitiveDetectorName = "/CHOD";
  G4String CHODCollectionName= "CHODCollection";
  CHODSD * ChodSD = static_cast<CHODSD*>(SDman->FindSensitiveDetector(CHODSensitiveDetectorName));
  if (!ChodSD) {
    ChodSD = new CHODSD(CHODSensitiveDetectorName,CHODCollectionName);
    SDman->AddNewDetector(ChodSD);
  }

  // Create the responsibility region
  G4double z1 = fRespRegionZStart;
  G4double z2 = fRespRegionZEnd;
  G4double x  = fRespRegionXLength;
  G4double y  = fRespRegionYLength;

  G4Box* rrSolid = new G4Box("CHOD", 0.5*x, 0.5*y, 0.5*(z2-z1));
  G4String name = "CHODRespReg";
  fLogicalVolume = new G4LogicalVolume(rrSolid, fMaterial, name, 0, 0, 0);
  fLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);

  new G4PVPlacement
    (0, G4ThreeVector(0.0,0.0,0.5*(z1+z2)), fLogicalVolume,
     name, fMotherVolume, false, 0);

  new BeamPipe(0, G4Material::GetMaterial("G4_Galactic"),
	       CHODGeometryParameters::GetInstance(), fLogicalVolume);

  ////////////////////////////
  // Place the two CHOD planes

  for (G4int iPlane = 0; iPlane<fNPlanes; iPlane++) {
    G4RotationMatrix PlaneRotation; 
    PlaneRotation.rotateZ(fPlaneRotZ[iPlane]);
    G4ThreeVector PlanePosition = G4ThreeVector(0.,0.,fPlanePosZ[iPlane]);
    G4Transform3D PlaneTransform = G4Transform3D(PlaneRotation, PlanePosition);
    new CHODPlane (fMaterial, fLogicalVolume, PlaneTransform, iPlane);
  }

  SetProperties();
}

void CHODDetector::SetProperties() {
  // Set visualization properties
  fVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
