// --------------------------------------------------------------
// History:
//
// 2014-04-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Cedar misalignment simulation introduced
//
// 2011-08-09 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - sensitive detector added
//
// 2011-06-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - vessel optics
//
// 2009-11-16 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - basic passive Cedar geometry + two quadrupoles (QFS077,079)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------

/// \class CedarDetector
/// \Brief
/// Steering class building the CEDAR detector geometry
/// \EndBrief

#include "G4Box.hh"
#include "G4Orb.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "DatacardManager.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "CedarDetector.hh"
#include "CedarSD.hh"
#include "G4SDManager.hh"

CedarDetector::CedarDetector(G4Material *Material, G4LogicalVolume *MotherVolume) : 
  NA62VComponent(Material,MotherVolume), NA62VNamed("Cedar"),
  fXLength(0), fYLength(0), fZLength(0), fZPosition(0),
  fRotBoxXLength(0), fRotBoxYLength(0), fRotBoxZLength(0), fRotAngleX(0), fRotAngleY(0),
  fQFSMagnet077NominalGradient(0), fQFSMagnet079NominalGradient(0),
  fVessel(nullptr), fManginMirror(nullptr), fChromaticCorrector(nullptr),
  fDiaphragm(nullptr), fCondenser(nullptr), fOctant(nullptr),
  fQFSMagnet077(nullptr), fQFSMagnet079(nullptr), fCherenkovEffect(true) {
#ifdef CEDARRAYTRACING
  G4cerr << "================================================================================" << G4endl;
  G4cerr << "                                RAY TRACING\n";
  G4cerr << "CEDARRAYTRACING has been defined so the following components now point to CedarSD:\n"
            "CedarDiaphragm, CedarQuartzWindow, CedarSphericalMirror, and CedarLightGuide\n";
  G4cerr << "================================================================================" << G4endl;
#endif

  // Messenger: enables detector configuration with datacards
  fCedarMessenger = new CedarDetectorMessenger(this);

  // Find or build the required materials
  CedarMaterialParameters::GetInstance();
}

CedarDetector::~CedarDetector() {
  delete fCedarMessenger;
}

void CedarDetector::ReadGeometryParameters() {

  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();

  fXLength   = GeoPars->GetCedarRegionXLength();
  fYLength   = GeoPars->GetCedarRegionYLength();
  fZLength   = GeoPars->GetCedarRegionZLength();
  fZPosition = GeoPars->GetCedarRegionZPositionLab();

  fRotBoxXLength  = GeoPars->GetRotBoxXLength();
  fRotBoxYLength  = GeoPars->GetRotBoxYLength();
  fRotBoxZLength  = GeoPars->GetRotBoxZLength();
  fRotBoxPosition = GeoPars->GetRotBoxPosition();

  fRotCentrePosition = GeoPars->GetRotCentrePosition();
  fRotAngleX = GeoPars->GetRotAngleX();
  fRotAngleY = GeoPars->GetRotAngleY();

  fQFSMagnet077Position = GeoPars->GetQFSMagnet077Position();
  fQFSMagnet079Position = GeoPars->GetQFSMagnet079Position();
  fQFSMagnet077NominalGradient = GeoPars->GetQFSMagnet077NominalGradient();
  fQFSMagnet079NominalGradient = GeoPars->GetQFSMagnet079NominalGradient();
}

void CedarDetector::CreateGeometry() {

  if (!fCherenkovEffect) {
    CedarMaterialParameters::GetInstance()->RemoveOpticalProperties();
  }

  ReadGeometryParameters();
  const G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();

  ////////////////////////////////////////////////////////////////////////
  // Register the sensitive detector
  // (PMT photocathodes are associated to it during detector construction)

  G4SDManager* fSDmanager = G4SDManager::GetSDMpointer();
  G4String fCedarSensitiveDetectorName = "/Cedar";
  G4String fCedarCollectionName = "CedarCollection";
  CedarSD *fCedarSD =
    static_cast<CedarSD*>(fSDmanager->FindSensitiveDetector(fCedarSensitiveDetectorName));
  if (!fCedarSD) {
    fCedarSD = new CedarSD(fCedarSensitiveDetectorName, fCedarCollectionName);
    fSDmanager->AddNewDetector(fCedarSD);
  }

  /*** Create the CEDAR responsibility region ***/

  fSolidVolume = new G4Box("CedarRespReg", 0.5*fXLength, 0.5*fYLength, 0.5*fZLength);

  fLogicalVolume = new G4LogicalVolume
    (fSolidVolume, fMaterial, "CedarRespReg", 0, 0, 0);

  fPhysicalVolume = new G4PVPlacement
    (0, G4ThreeVector(0.0, 0.0, fZPosition), fLogicalVolume,
     "CedarRespReg", fMotherVolume, false, 0);

  /*** Place the CEDAR+KTAG box that can be rotated ***/

  G4Box *fRotBox = new G4Box
    ("CedarRotBox", 0.5*fRotBoxXLength, 0.5*fRotBoxYLength, 0.5*fRotBoxZLength);

  G4LogicalVolume *fRotBoxLogicalVolume = new G4LogicalVolume
    (fRotBox, G4Material::GetMaterial("G4_Galactic"), "CedarRotBox", 0, 0, 0);

  // Define the rotation matrix
  G4RotationMatrix Rotation;
  Rotation.rotateY(fRotAngleX);
  Rotation.rotateX(fRotAngleY);

  // Define the translation (required becaue the rotation is not about the box centre)
  G4ThreeVector NewRotCentrePosition = Rotation * fRotCentrePosition;
  G4ThreeVector RotCentreMovement    = NewRotCentrePosition - fRotCentrePosition;
  G4ThreeVector Trans                = fRotBoxPosition - RotCentreMovement;

  new G4PVPlacement
    (G4Transform3D(Rotation, Trans),
     fRotBoxLogicalVolume, "CedarRotBox", fLogicalVolume, false, 0);

  /*** Place objects inside the rotating box ***/

  // Stainless steel vessel + front pipe filled with radiator gas
  fVessel = new CedarVessel
    (G4Material::GetMaterial("Cedar_StainlessSteel"), fRotBoxLogicalVolume);

  // Mangin mirror (inside vessel)
  fManginMirror = new CedarManginMirror
    (G4Material::GetMaterial("Cedar_Quartz"),
     fVessel->GetVesselRadiatorGasLogicalVolume());

  // Chromatic corrector (inside vessel)
  fChromaticCorrector = new CedarChromaticCorrector
    (G4Material::GetMaterial("Cedar_Quartz"),
     fVessel->GetVesselRadiatorGasLogicalVolume());

  // Diaphragm (inside vessel)
  fDiaphragm = new CedarDiaphragm
    (G4Material::GetMaterial("Cedar_StainlessSteel"),
     fVessel->GetVesselRadiatorGasLogicalVolume());

  // Consenser lenses (inside vessel)
  fCondenser = new CedarCondenser*[fNSectors];
  for (G4int iSector=0; iSector<fNSectors; iSector++) {
    fCondenser[iSector] = new CedarCondenser
      (G4Material::GetMaterial("Cedar_Quartz"),
       fVessel->GetVesselRadiatorGasLogicalVolume(), iSector);
  }

  // Octants containing spherical mirrors, PMTs, etc.
  fOctant = new CedarOctant*[fNSectors];
  for (G4int iSector=0; iSector<fNSectors; iSector++) {
    fOctant[iSector] = new CedarOctant
      (G4Material::GetMaterial("Cedar_Nitrogen"), fRotBoxLogicalVolume, iSector);
  }

  // A marker at the centre of rotation: useful for testing
  G4Orb *fMarker = new G4Orb("CedarRotCentre", 10*mm);
  G4LogicalVolume *fMarkerLogical = new G4LogicalVolume
    (fMarker, G4Material::GetMaterial("Cedar_StainlessSteel"), "CedarRotCentre");
  new G4PVPlacement
    (0, fRotCentrePosition, fMarkerLogical, "CedarRotCentre", fRotBoxLogicalVolume, false, 0);

  ///////////////////////////////////////////////////////////////////////////
  // Place objects at fixed positions in lab frame (outside the rotating box)

  /////////////////
  // Exit beam pipe

  new CedarExitPipe(G4Material::GetMaterial("Cedar_StainlessSteel"), fLogicalVolume);

  /////////////////////////////////////////////////////////////////////////
  // QFS quadrupole magnets (Quads 9,10) with run-dependent field gradients

  G4double sf077 = DatacardManager::GetInstance()->GetQuad09FieldScaleFactor();
  G4double sf079 = DatacardManager::GetInstance()->GetQuad10FieldScaleFactor();
  G4cout << "[CedarDetector] Quad9,10 field scale factors = " <<
    Form("%5.3f", sf077) << " " << Form("%5.3f", sf079) << G4endl;

  fQFSMagnet077 = new CedarQFSMagnet
    (G4Material::GetMaterial("G4_Galactic"), fLogicalVolume,
     fQFSMagnet077Position, sf077*fQFSMagnet077NominalGradient, 0);
  fQFSMagnet079 = new CedarQFSMagnet
    (G4Material::GetMaterial("G4_Galactic"), fLogicalVolume,
     fQFSMagnet079Position, sf079*fQFSMagnet079NominalGradient, 1);

  ///////////////////////////////////////////////////////////////////////
  // Mangin mirror optical surfaces: cannot be defined in ManginMirror.cc

  new G4LogicalBorderSurface
    ("CedarManginMirrorRefractingSurface1",
     fVessel->GetVesselRadiatorGasPhysicalVolume(),
     fManginMirror->GetLensPhysicalVolume(),
     fManginMirror->GetRefractingOpticalSurface());
  new G4LogicalBorderSurface
    ("CedarManginMirrorRefractingSurface2",
     fManginMirror->GetLensPhysicalVolume(),
     fVessel->GetVesselRadiatorGasPhysicalVolume(),
     fManginMirror->GetRefractingOpticalSurface());
  new G4LogicalBorderSurface
    ("CedarManginMirrorReflectingSurface",
     fManginMirror->GetLensPhysicalVolume(),
     fManginMirror->GetCoatingPhysicalVolume(),
     fManginMirror->GetReflectingOpticalSurface());

  SetProperties();
}

void CedarDetector::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
