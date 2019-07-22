// LAVDetector.cc
// --------------------------------------------------------------
// History:
//
// 2019-06-11 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added (optional) verification of volume overlaps
// 2015-05-01 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Build blue tube structure
// 2015-03-19 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Using Air instead of vacuum in A12 
//   - Now using inner and outer radii of vessel instead of outer radius and thickness
// 2011-01-24 Domenico Di Filippo (difilippo@na.infn.it)
//   - Move the SensitiveDetector construction to subdetectors (leadglas,photomultiplier etc)
//   - Added construction of the accurate block
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added methods to en-/dis-able stations, vessels, layers, bananas
//   - Added code to define concrete instances of LAVVPbGlBlock
//   - Added code to handle the new LAVBanana class
// 2010-11-10 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Add use of LAVDetectorMessenger class
//   - Added code to handle new responsibility regions
//   - Allow enable/disable of single LAV station
// 2010-11-03 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Updated LAV stations positions
//   - Blocks geometry is now correct
//   - Added (part of) block support structure
// 2009-03-02 Emanuele Leonardi (Emanuele.Leonardi@roma1.infn.it)
//   - First implementation of LAV geometry
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------

#include "LAVDetector.hh"
#include "LAVDetectorMessenger.hh"

#include "LAVPbGlBlock.hh"
#include "LAVAccurateBlock.hh"
#include "LAVBanana.hh"

#include "G4Tubs.hh"
#include "G4ExtrudedSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "BeamPipe.hh"

#include "LAVGeometryParameters.hh"
#include "LAVMaterialParameters.hh"

#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"

#include "G4FieldManager.hh"
#include "G4PropagatorInField.hh"
#include "G4MagIntegratorStepper.hh"
#include "G4ChordFinder.hh"
#include "G4MagneticField.hh"
#include "G4ElectroMagneticField.hh"
#include "G4Mag_EqRhs.hh"
#include "G4Mag_SpinEqRhs.hh"
#include "G4EqMagElectricField.hh"
#include "G4EqEMFieldWithSpin.hh"
#include "G4ClassicalRK4.hh"
#include "G4MagIntegratorDriver.hh"

LAVDetector::LAVDetector(G4Material* Material, G4LogicalVolume* MotherVolume): 
NA62VComponent(Material,MotherVolume), NA62VNamed("LAV") {

  // Connect to LAVDetectorMessenger to enable datacard configuration
  fLAVMessenger = new LAVDetectorMessenger(this);
  fFieldManager = 0;

  // All stations/layers/bananas/vessels are enabled by default
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) {
    fStationEnabled[iS] = 1;
    fVesselEnabled[iS] = 1;
    for (G4int iL=0;iL<NUMBER_OF_RINGS;iL++) {
      fLayerEnabled[iS][iL] = 1;
      for (G4int iB=0;iB<NUMBER_OF_BANANAS;iB++) {
	fBananaEnabled[iS][iL][iB] = 1;
      }
    }
  }

  // Default block simulation is "standard"
  fBlockSimulation = "standard";

  // Mandatory here to Find or Build the needed materials
  LAVMaterialParameters::GetInstance();
}

LAVDetector::~LAVDetector() {
  delete fLAVMessenger;
  if (fFieldManager) delete fFieldManager;
}

void LAVDetector::ReadGeometryParameters() {}

void LAVDetector::CreateGeometry() {

  //ReadGeometryParameters();

  // Set to true to check for overlapping volumes
  G4bool checkOverlaps = false;

  // Get access to geometry parameters
  LAVGeometryParameters* geoParams = LAVGeometryParameters::GetInstance();

  // Create responsibility regions for LAV detector and position them inside mother volume
  G4LogicalVolume* rrLogicalVolumes[NUMBER_OF_RESPONSIBILITY_REGIONS];
  G4int nRR = geoParams->GetNumberOfResponsibilityRegions();
  for(G4int iRR=0;iRR<nRR;iRR++) {
    G4double rrFF = geoParams->GetResponsibilityRegionZofFrontFace(iRR);
    G4double rrBF = geoParams->GetResponsibilityRegionZofBackFace(iRR);
    G4double rrR  = geoParams->GetResponsibilityRegionRadius(iRR);
    G4double rrHalfZLen = 0.5*(rrBF-rrFF);
    G4Tubs* rrSolid = new G4Tubs("LAV",0,rrR,rrHalfZLen,0.*M_PI*rad,2.*M_PI*rad);

    rrLogicalVolumes[iRR] = new G4LogicalVolume(rrSolid,fMaterial,"LAV_RR",0,0,0);
    rrLogicalVolumes[iRR]->SetVisAttributes(G4VisAttributes::Invisible);
    new G4PVPlacement(0,G4ThreeVector(0.,0.,rrFF+rrHalfZLen),rrLogicalVolumes[iRR],
		      Form("LAV_RR%d", iRR),fMotherVolume,false,iRR,checkOverlaps);
  }

  G4Mag_SpinEqRhs *equation = new G4Mag_SpinEqRhs(MagneticField::GetInstance());
  G4MagIntegratorStepper *stepper = new G4ClassicalRK4(equation, 12); // 12: mag field
  G4MagInt_Driver* driver = new G4MagInt_Driver(1.0e-3*mm, stepper,
                                                stepper->GetNumberOfVariables());
  G4ChordFinder* finder = new G4ChordFinder(driver);
  fFieldManager = new G4FieldManager(MagneticField::GetInstance());
  fFieldManager->SetDeltaOneStep(1e-10); // improved tracking precision
  fFieldManager->SetMinimumEpsilonStep(1e-10);
  fFieldManager->SetMaximumEpsilonStep(1e-10);
  fFieldManager->SetDetectorField(MagneticField::GetInstance());
  fFieldManager->SetChordFinder(finder);

  ////////////////////////////////////////////////////////////////////////////
  // Blue tube magnetic field present in RR0 (LAV1-8, 104.458m < z < 181.775m)
  // Fringe Field is present in          RR1 (LAV9,   183.704m < z < 193.371m)
  // MNP33 & Fringe field present in     RR2 (LAV10,  198.070m < z < 203.764m)
  // Fringe field is present in          RR3 (LAV11,  204.656m < z < 218.190m)

  if (fabs(MagneticField::GetInstance()->GetBlueTubeFieldScale())>0.001) {
    rrLogicalVolumes[0]->SetFieldManager(fFieldManager, true);
  }
  if (MagneticField::GetInstance()->GetMNP33FieldMode() && // detailed field required
      fabs(MagneticField::GetInstance()->GetMNP33FieldScale())>0.001) {
    rrLogicalVolumes[1]->SetFieldManager(fFieldManager, true); // Fringe
    rrLogicalVolumes[2]->SetFieldManager(fFieldManager, true); // MNP33 + Fringe
    rrLogicalVolumes[3]->SetFieldManager(fFieldManager, true); // Fringe
  }

  // Get total number of blue tube sections
  G4int nBTsect = geoParams->GetNumberOfBlueTubeZones();
  for(G4int iBT=0;iBT<nBTsect;iBT++) {

    // Get id and dimensions of RR where this blue tube section is positioned
    G4int btRR = geoParams->GetBlueTubeResponsibilityRegion(iBT);
    G4double btRRZFrontFace = geoParams->GetResponsibilityRegionZofFrontFace(btRR);
    G4double btRRZBackFace = geoParams->GetResponsibilityRegionZofBackFace(btRR);
    G4double btRRHalfZLen = 0.5*(btRRZBackFace-btRRZFrontFace);

    // Get dimensions of this blue tube section
    G4double btInR  = geoParams->GetBlueTubeInnerRadius(iBT);
    G4double btOutR = geoParams->GetBlueTubeOuterRadius(iBT);
    G4double btZfront = geoParams->GetBlueTubeZofFrontFace(iBT);
    G4double btZback  = geoParams->GetBlueTubeZofBackFace(iBT);
    G4double btHalfZLen = 0.5*(btZback-btZfront);

    // Create blue tube cylinder
    G4Tubs* btSolid = new G4Tubs("BlueTube",btInR,btOutR,btHalfZLen,0.*M_PI*rad,2.*M_PI*rad);
    G4LogicalVolume* btLV = new G4LogicalVolume(btSolid,G4Material::GetMaterial("G4_Fe"),"BlueTube",0,0,0);
    G4VisAttributes* blueVisAtt = new G4VisAttributes(G4Colour(0.0,0.0,1.0));
    btLV->SetVisAttributes(blueVisAtt);

    // Position blue tube cylinder inside its responsibility region
    G4double btZPos = -btRRHalfZLen+(btZfront-btRRZFrontFace)+btHalfZLen;
    new G4PVPlacement(0,G4ThreeVector(0.,0.,btZPos),btLV,"BlueTube",rrLogicalVolumes[btRR],false,iBT,checkOverlaps);

    // Add rails (if needed)
    if ( geoParams->BlueTubeHasRails(iBT) ) {
      G4int nR = geoParams->GetNumberOfBTRails();
      for(G4int iR=0;iR<nR;iR++) {

	// Create BT rail with given XY vertices and same length of current BT section
	G4int nV = geoParams->GetNumberOfBTRailVertices(iR);
	std::vector<G4TwoVector> v(nV);
        std::vector<G4ExtrudedSolid::ZSection> sections {
          G4ExtrudedSolid::ZSection(-btHalfZLen, G4TwoVector(0.,0.), 1.),
          G4ExtrudedSolid::ZSection(btHalfZLen, G4TwoVector(0.,0.), 1.) };
	for(G4int iV=0;iV<nV;iV++) {
	  v[iV] = geoParams->GetBTRailVertex(iR,iV);
	  //G4cout << "Rail " << iR << " vertex " << iV << " point " << geoParams->GetBTRailVertex(iR,iV) << G4endl;
	}
	G4ExtrudedSolid* btRailSolid = new G4ExtrudedSolid("btRail",v,sections);
	// Create BT rail logical volume and position it inside the responsibility region
	G4LogicalVolume* btRailLV = new G4LogicalVolume(btRailSolid,G4Material::GetMaterial("G4_Fe"),"BlueTubeRail",0,0,0);
	btRailLV->SetVisAttributes(blueVisAtt);
	new G4PVPlacement(0,G4ThreeVector(0.,0.,btZPos),btRailLV,"BlueTubeRail",rrLogicalVolumes[btRR],false,10*iBT+iR,checkOverlaps);

      }
    }

  }

  // Get total number of LAV stations
  G4int nLAV = geoParams->GetTotalNumberOfVetoStations();

  // For each LAV station create a volume and position it inside its responsibility region
  for(G4int iLAV=0;iLAV<nLAV;iLAV++) {

    // See if this station is enabled
    if ( fStationEnabled[iLAV] == 0 ) continue;

    // Get id of RR where this station is positioned
    G4int lRR = geoParams->GetStationResponsibilityRegion(iLAV);
    G4double lRRZFrontFace = geoParams->GetResponsibilityRegionZofFrontFace(lRR);
    G4double lRRZBackFace = geoParams->GetResponsibilityRegionZofBackFace(lRR);
    G4double lRRHalfZLen = 0.5*(lRRZBackFace-lRRZFrontFace);

    // Get position and dimensions of station
    G4double lZFrontFace = geoParams->GetStationZofFrontFace(iLAV);
    G4double lHalfZLen = 0.5*geoParams->GetStationZLength(iLAV);
    G4double lRadius = geoParams->GetStationOuterRadius(iLAV);

    // Create station logical volume and position it inside its RR
    G4Tubs* lStation = new G4Tubs("LAVStation",geoParams->GetBeamPipeFinOuterRadius(iLAV),lRadius,lHalfZLen,0*M_PI*rad,2.*M_PI*rad);
    G4Material* stationMaterial = fMaterial;
    if (iLAV==11) stationMaterial = G4Material::GetMaterial("G4_AIR"); // A12 is in air
    G4LogicalVolume* lStationLV = new G4LogicalVolume(lStation,stationMaterial,"LAVStation",0,0,0);
    lStationLV->SetVisAttributes(G4VisAttributes::Invisible);
    G4double lStationZPosition = -lRRHalfZLen+(lZFrontFace-lRRZFrontFace)+lHalfZLen;
    G4String name = Form("LAV_Station_%02d", iLAV+1);
    new G4PVPlacement(0,G4ThreeVector(0.,0.,lStationZPosition),lStationLV,name,rrLogicalVolumes[lRR],false,iLAV,checkOverlaps);

      // Create steel vessel
    if ( fVesselEnabled[iLAV] == 1 ) {

      // Get vessel's information
      G4double lVesselInnerRadius = geoParams->GetVesselInnerRadius(iLAV);
      G4double lVesselOuterRadius = geoParams->GetVesselOuterRadius(iLAV);
      G4double lVesselHalfZLen = 0.5*geoParams->GetStationZLength(iLAV); // Same as station

      // Create solid and logical volume for vessel
      // The current model of the steel vessel is an empty cylinder. Flanges will be added in future.
      G4Tubs* lVessel = new G4Tubs("LAVVessel",lVesselInnerRadius,lVesselOuterRadius,lVesselHalfZLen,0*M_PI*rad,2.*M_PI*rad);

      G4LogicalVolume* lVesselLV;
      if (iLAV==11) { // A12 vessel is empty (support structure will be added in future)
	lVesselLV = new G4LogicalVolume(lVessel,G4Material::GetMaterial("G4_AIR"),"LAV",0,0,0);
	lVesselLV->SetVisAttributes(G4VisAttributes::Invisible);
      } else {
	lVesselLV = new G4LogicalVolume(lVessel,G4Material::GetMaterial("G4_Fe"),"LAV",0,0,0);
	G4VisAttributes* SteelVisAtt = new G4VisAttributes(G4Colour(0.0,1.0,0.0));
	lVesselLV->SetVisAttributes(SteelVisAtt);
      }

      // Position steel vessel inside station volume
      new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),lVesselLV,"LAV_Vessel",lStationLV,false,0,checkOverlaps);

    }

    // Get parameters to generate this LAV station
    G4int nRings = geoParams->GetStationNRings(iLAV);                   // Number of rings
    G4int nBananasPerRing = geoParams->GetStationNBananasPerRing(iLAV); // Number of bananas in each ring
    G4int bananaType = geoParams->GetStationBananaType(iLAV);           // Banana type for this station

    G4double ringGap = geoParams->GetStationRingGap(iLAV);              // Z gap between adjacent rings
    G4double zPosFirstRing = geoParams->GetStationFirstRingZPos(iLAV);  // Z position of first ring wrt front face of vessel

    G4double phiRefRing = geoParams->GetStationPhiReference(iLAV);      // Phi reference point for center of banana 0 of layer 0
    G4double phiRotRing = geoParams->GetStationPhiRotationBetweenLayers(iLAV); // Phi rotation between consecutive rings

    for(G4int iRing=0;iRing<nRings;iRing++) {

      // See if this layer is enabled
      if ( fLayerEnabled[iLAV][iRing] == 0 ) continue;

      // Get OPAL type of crystal used for this ring and create corresponding block
      G4int blockType = geoParams->GetTypeOfCrystal(iLAV,iRing);
      LAVVPbGlBlock* pbglBlock;
      if ( fBlockSimulation == "standard" ) {
         pbglBlock = new LAVPbGlBlock(geoParams->GetBlockIdFromOpalId(blockType));
      }	else if ( fBlockSimulation == "optical" ) {
         pbglBlock = new LAVAccurateBlock(geoParams->GetBlockIdFromOpalId(blockType));
      } else {
         G4cout << "LAVDetector - PbGlBlock simulation set to unknown value: " << fBlockSimulation
         << " - using standard simulation" << G4endl;
         pbglBlock = new LAVPbGlBlock(geoParams->GetBlockIdFromOpalId(blockType));
      }
      
      // Create banana of type/block used in this ring and get its dimensions
      //G4cout << "LAVDetector - Creating banana type " << bananaType << " Block type " << blockType << G4endl;
      LAVBanana* banana = new LAVBanana(bananaType,pbglBlock);
      G4LogicalVolume* bananaLV = banana->GetLogicalVolume();
      G4double bananaThickness = banana->GetBananaThickness();

      // Compute layer Z displacement wrt front face of vessel
      G4double zRing = -lHalfZLen+zPosFirstRing+iRing*(bananaThickness+ringGap)+0.5*bananaThickness;

      // Total phi displacement of current ring wrt to first ring
      G4double phiRing = iRing*phiRotRing;

      for(G4int iBanana=0;iBanana<nBananasPerRing;iBanana++) {

	// See if this banana is enabled
	if ( fBananaEnabled[iLAV][iRing][iBanana] == 0 ) continue;

	//G4cout << "LAVDetector - Position: Station " << iLAV << " Ring " << iRing << " Banana " << iBanana << G4endl;

	// Compute banana rotation matrix and translation vector
	// Take into account that bananas are created with center at 90deg
	G4double phiBanana = phiRefRing+phiRing+iBanana*(360.*deg/nBananasPerRing)-90.*deg;
	G4RotationMatrix* rotationBanana = new G4RotationMatrix;
	rotationBanana->rotateZ(-phiBanana);
	G4ThreeVector translationBanana = G4ThreeVector(0.,0.,zRing);

	// Position banana inside station
	new G4PVPlacement(rotationBanana,translationBanana,bananaLV,"LAV_Banana",
			  lStationLV,false,1000*iLAV+100*iRing+iBanana,checkOverlaps);

      }
    }

    // Put beam pipe inside A12
    if (iLAV==11) new BeamPipe(11,G4Material::GetMaterial("G4_AIR"),geoParams,rrLogicalVolumes[lRR]);
  }

}

void LAVDetector::SetProperties() {}

void LAVDetector::EnableAllStations() {
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) fStationEnabled[iS] = 1;
}

void LAVDetector::DisableAllStations() {
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) fStationEnabled[iS] = 0;
}

void LAVDetector::EnableAllLayers() {
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) {
    for (G4int iL=0;iL<NUMBER_OF_RINGS;iL++) fLayerEnabled[iS][iL] = 1;
  }
}

void LAVDetector::DisableAllLayers() {
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) {
    for (G4int iL=0;iL<NUMBER_OF_RINGS;iL++) fLayerEnabled[iS][iL] = 0;
  }
}

void LAVDetector::EnableAllBananas() {
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) {
    for (G4int iL=0;iL<NUMBER_OF_RINGS;iL++) {
      for (G4int iB=0;iB<NUMBER_OF_BANANAS;iB++) fBananaEnabled[iS][iL][iB] = 1;
    }
  }
}

void LAVDetector::DisableAllBananas() {
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) {
    for (G4int iL=0;iL<NUMBER_OF_RINGS;iL++) {
      for (G4int iB=0;iB<NUMBER_OF_BANANAS;iB++) fBananaEnabled[iS][iL][iB] = 0;
    }
  }
}

void LAVDetector::EnableAllVessels() {
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) fVesselEnabled[iS] = 1;
}

void LAVDetector::DisableAllVessels() {
  for (G4int iS=0;iS<NUMBER_OF_VETOES;iS++) fVesselEnabled[iS] = 0;
}
