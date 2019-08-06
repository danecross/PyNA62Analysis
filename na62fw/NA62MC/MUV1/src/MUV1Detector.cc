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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-10
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Modified (MUV -> MUV1) Rainer Wanke              2010-11-26
// Modified  Mario Vormstein (mario.vormstein@cern.ch)  2011-01-19
// Code reashaping Antonino Sergi                      2011-03-08 
// Modified by Mario Vormstein (mario.vormstein@cern.ch) 2011-07-14
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch)  2017-11-10
//     major updates: moved from 24 to 23 scintillator layer geometry
//                    thickness of iron and rubber layers updated
//                    transportation tube is added
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"
#include "G4Transform3D.hh"

#include "MUV1GeometryParameters.hh"
#include "MUV1MaterialParameters.hh"
#include "MUV1Detector.hh"
#include "MUV1DetectorMessenger.hh"
#include "MUV1SD.hh"
#include "MUV1PMTSD.hh"
#include "MUV1FiberSD.hh"
#include "G4SDManager.hh"

#include "MUV1IronPlate.hh"
#include "MUV1ScintillatorLayer.hh"
#include "MUV1RubberPlate.hh"
#include "MUV1Bolt.hh"
#include "MUV1BeamPipe.hh"
#include "MUV1TransportationTube.hh"

/// \class MUV1Detector
/// \Brief
/// MUV1Detector class.
/// \EndBrief
///
/// \Detailed
/// In this class die different substructures of the MUV1 are called.
///	This are :
///	-   The first iron plate
/// -	12 horizontal and 11 vertical scintillator layers, 23 rubber and iron layers
/// -	The last iron plate
///	-	4 Iron bolts, holding MUV1 together
/// -   Transportation tube for stability of MUV1
/// - 	The beampipe
/// \EndDetailed


MUV1Detector::MUV1Detector(G4Material * Material,
		G4LogicalVolume * MotherVolume) :
	NA62VComponent(Material, MotherVolume), NA62VNamed("MUV1") {

	// Connect to MUV1DetectorMessenger
	fMUV1Messenger = new MUV1DetectorMessenger(this);

	G4cout << "MUV1 Fast Simulation is: " << fFastSimulation << G4endl;

	// Fast simulation by default
	fFastSimulation = "true";

	ReadGeometryParameters();

	// Mandatory here to Find or Build the needed materials
	MUV1MaterialParameters::GetInstance();
	CreateGeometry();
	SetProperties();
}


MUV1Detector::~MUV1Detector() {
	delete fMUV1Messenger;
}


void MUV1Detector::ReadGeometryParameters() {
	// Read all the geometrical parameters and copy them to private members
	MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();

	fMUV1ResponsibilityRegionZBeginning
			= GeoPars->GetMUV1ResponsibilityRegionZBeginning();
	fMUV1ResponsibilityRegionZEnd = GeoPars->GetMUV1ResponsibilityRegionZEnd();
	fMUV1ResponsibilityRegionXLength
			= GeoPars->GetMUV1ResponsibilityRegionXLength();
	fMUV1ResponsibilityRegionYLength
			= GeoPars->GetMUV1ResponsibilityRegionYLength();
	fXLength = 0.5 * GeoPars->GetMUV1XSize(); ///<  half transverse X size of all MUV1
	fYLength = 0.5 * GeoPars->GetMUV1YSize(); ///<  half transverse Y size of all MUV1
	fZLength = 0.5 * GeoPars->GetMUV1Length(); ///<  half length of all MUV1

	fZPosition = GeoPars->GetMUV1DetectorFrontZPosition();

	// thickness of each scintillator (zsci)

	fBareScintillatorThickness = GeoPars->GetBareScintillatorThickness();
	fScintLayerThickness = GeoPars->GetScintLayerThickness();
	fRubberRealThickness = GeoPars->GetRubberRealThickness();
	fAirGapWidth = GeoPars->GetAirGapWidth();

	// thickness of each iron plate (zfe)
	fIronThickness = GeoPars->GetIronThickness();
	fRubberThickness = GeoPars->GetRubberThickness();

	//Iron parameters
	fNIronPlate = GeoPars->GetNIronPlate();
	fIronPlateSizeX = GeoPars->GetIronPlateSizeX();
	fIronPlateSizeY = GeoPars->GetIronPlateSizeY();
	fIronPlateOuterSizeX = GeoPars->GetIronPlateOuterSizeX();
	fIronPlateOuterSizeY = GeoPars->GetIronPlateOuterSizeY();
	fBoltPositionX = GeoPars->GetBoltPositionX();
	fBoltPositionY = GeoPars->GetBoltPositionY();

	// Define some more useful variables
	MUV1ResponsibilityRegionZLength = fMUV1ResponsibilityRegionZEnd
			- fMUV1ResponsibilityRegionZBeginning;
	MUV1ResponsibilityRegionZCenter = fMUV1ResponsibilityRegionZBeginning
			+ MUV1ResponsibilityRegionZLength / 2.;
}


void MUV1Detector::CreateGeometry() {
	// Example of sensitive detector; in general it would be
	// associated to smaller volume/s inside the global boxes
	G4SDManager* SDman = G4SDManager::GetSDMpointer();

	G4String MUV1SensitiveDetectorName = "/MUV1";
	G4String MUV1PMTSensitiveDetectorName = "/MUV1/PMT";
	G4String MUV1FiberSensitiveDetectorName = "/MUV1/Fiber";

	G4String MUV1CollectionName = "MUV1Collection";
	G4String MUV1PMTCollectionName = "MUV1PMTCollection";
	//G4String MUV1FiberCollectionName = "MUV1FiberCollection";

	MUV1SD *Muv1SD = static_cast<MUV1SD*>( SDman->FindSensitiveDetector( MUV1SensitiveDetectorName ));
	MUV1PMTSD *Muv1PMTSD = static_cast<MUV1PMTSD*>( SDman->FindSensitiveDetector( MUV1PMTSensitiveDetectorName ));
	MUV1FiberSD *Muv1FiberSD = static_cast<MUV1FiberSD*>( SDman->FindSensitiveDetector( MUV1FiberSensitiveDetectorName ));

	if (!Muv1SD) {
		Muv1SD = new MUV1SD(MUV1SensitiveDetectorName, MUV1CollectionName);
		SDman->AddNewDetector(Muv1SD);

		Muv1FiberSD = new MUV1FiberSD(MUV1FiberSensitiveDetectorName);
		SDman->AddNewDetector(Muv1FiberSD);

		Muv1PMTSD = new MUV1PMTSD(MUV1PMTSensitiveDetectorName,	MUV1PMTCollectionName);
		SDman->AddNewDetector(Muv1PMTSD);
	}

	// Build one or more boxes that will contain all the
	// detector sections, up to fill the responsibility region

	fSolidVolume = new G4Box("MUV1", fMUV1ResponsibilityRegionXLength / 2.,
			fMUV1ResponsibilityRegionYLength / 2.,
			MUV1ResponsibilityRegionZLength / 2.+0.01*mm); //+0.01*cm against roundoff overlaps

	fLogicalVolume = new G4LogicalVolume(fSolidVolume, // solid
			fMaterial, // material
			"MUV1", // name
			0, // field manager
			0, // sensitive detector
			0); // user limits

	fPhysicalVolume = new G4PVPlacement(0, G4ThreeVector(0., 0.,
			MUV1ResponsibilityRegionZCenter), // position
			fLogicalVolume, // its logical volume
			"MUV1", // its name
			fMotherVolume, // its mother  volume
			false, // no boolean operations
			0); // copy number


	// MUV1 horizontal scintillator layer rotation matrix
	G4RotationMatrix* g4rot = new G4RotationMatrix(	CLHEP::HepRotationZ(90.*deg) );
	*g4rot = g4rot->inverse();


	//****************************************************
	//*                Iron                              *
	//*                Rubber                            *
	//*            Scintillators                         *
	//****************************************************

	// center position of the first upstream iron plate
	G4double IronPlatePosZ = fZPosition - MUV1ResponsibilityRegionZCenter + 0.5	* fIronThickness;

	//	G4double muv1startz =0.;
	//	G4double muv1endz =0.;
	//	G4cout <<"fZPosition                      = "<< fZPosition << G4endl;
	//	G4cout <<"MUV1ResponsibilityRegionZCenter = "<< MUV1ResponsibilityRegionZCenter << G4endl;
	//	G4cout <<"First: IronPlatePosZ                = " << IronPlatePosZ<< G4endl;
	//	muv1startz = IronPlatePosZ - fIronThickness/2. ;

	// Creating the first outer iron plate
	// size of the plate
	G4ThreeVector IronPlateSize = G4ThreeVector(0.5 * fIronPlateOuterSizeX, 0.5 * fIronPlateOuterSizeY, 0.5 * fIronThickness);

	// position of the plate in the frame of the mother volume (MUV1)
	G4ThreeVector IronPlatePosition = G4ThreeVector(0. * cm, 0. * cm, IronPlatePosZ);

	// * IRON * FIRST PLATE *
	fIronPlate = new MUV1IronPlate(G4Material::GetMaterial("G4_Fe"), fLogicalVolume, IronPlateSize, IronPlatePosition, 0);


	// Z position of the next iron plate in the frame of the mother volume (MUV1)
	IronPlatePosZ += (fBareScintillatorThickness + fIronThickness	+ fRubberThickness);

	// iron plate size for inner layers (23 in total)
	IronPlateSize = G4ThreeVector(0.5 * fIronPlateSizeX, 0.5 * fIronPlateSizeY,	0.5 * fIronThickness);

	// rubber plate size
	//	G4ThreeVector RubberPlateSize = G4ThreeVector(0.5 * fIronPlateSizeX, 0.5 * fIronPlateSizeY, 0.5 * fRubberThickness);
	G4ThreeVector RubberPlateSize = G4ThreeVector(0.5 * fIronPlateSizeX, 0.5 * fIronPlateSizeY, 0.5 * fRubberRealThickness);

	// Z position of the first upstream rubber layer
	G4double RubberPlatePosZ = (fZPosition - MUV1ResponsibilityRegionZCenter) + fIronThickness
								+ fAirGapWidth
								+ fRubberRealThickness/2.;

	//	G4cout <<"0: RubberPlatePosZ         = " << RubberPlatePosZ         <<"   Distance from iron plate surface            "<< RubberPlatePosZ - ( (IronPlatePosZ -(fBareScintillatorThickness + fIronThickness	+ fRubberThickness)) + fIronThickness/2. ) << G4endl;


	// Z position of the first upstream scintillator layer
	G4double ScintPosZ = (fZPosition - MUV1ResponsibilityRegionZCenter) + fIronThickness
						 + fAirGapWidth
						 + fRubberRealThickness
						 //The half value of fScintLayerThickness already includes 2 x (1/3*fAirGapWidth) extra air thickness :
						 //one 1/3*fAirGapWidth is an extra thickness of scintillator layer logical volume and another 1/3*fAirGapWidth is an extra thickness of scintillator mother logical volume.
						 //Therefore, adding here only 1/3*fAirGapWidth extra distance means 3 x (1./3. * fAirGapWidth) distance between the rubber surface and the surface of the scintillator wrapper.
						 + 1./3. * fAirGapWidth
						 + 0.5 * fScintLayerThickness;

	//	G4cout <<"0: fRubberRealThickness/2. = " << fRubberRealThickness/2. <<"   Distance from scint mother layer surface    "<<  (ScintPosZ - 0.5 * fScintLayerThickness) - (RubberPlatePosZ + fRubberRealThickness/2.) << G4endl;
	//	G4cout <<"0: ScintPosZ               = " << ScintPosZ<<"   Distance from next iron plate surface "<< (IronPlatePosZ - fIronThickness/2.) - (ScintPosZ + 0.5 * fScintLayerThickness) << G4endl;


	MUV1ScintillatorLayer * Layer = NULL;

	// loop over the 12 layers
	for (G4int iScintLayer = 1; iScintLayer < 12 + 1; iScintLayer++) {

		// * RUBBER *

		//		G4cout <<iScintLayer<<": RubberPlatePosZ =        " << RubberPlatePosZ<< G4endl;
		G4ThreeVector RubberPlatePosition = G4ThreeVector(0., 0.,	RubberPlatePosZ);
		fRubberPlate = new MUV1RubberPlate(G4Material::GetMaterial(	"G4_RUBBER_NATURAL"), fLogicalVolume, RubberPlateSize, RubberPlatePosition, iScintLayer);

		RubberPlatePosZ += (fBareScintillatorThickness + fRubberThickness + fIronThickness);

		// * SCINTILLATOR * HORIZONTAL LAYER *
		//		G4cout <<iScintLayer<<": ScintPosZ =        " << ScintPosZ<< G4endl;
		// iScintLayer --> 2*iScintLayer-2   but will be restored in MUV1ScintillatorLayer::CreateGeometry()
		Layer = new MUV1ScintillatorLayer(G4Material::GetMaterial("G4_AIR"), fLogicalVolume, G4ThreeVector(0, 0, ScintPosZ), 2*iScintLayer-2, 50, fFastSimulation);
		Layer->GetPhysicalVolume()->SetRotation(g4rot);

		ScintPosZ += (fBareScintillatorThickness + fRubberThickness + fIronThickness );

		if (iScintLayer < 12) {

			// * IRON *

			//			G4cout <<iScintLayer<<": IronPlatePosZ =        " << IronPlatePosZ<< G4endl;
			G4ThreeVector IronPlatePosition = G4ThreeVector(0., 0., IronPlatePosZ);
			fIronPlate = new MUV1IronPlate(G4Material::GetMaterial("G4_Fe"), fLogicalVolume, IronPlateSize, IronPlatePosition, iScintLayer);

			IronPlatePosZ += (fBareScintillatorThickness + fRubberThickness + fIronThickness);



			// * RUBBER *

			//			G4cout <<iScintLayer<<": RubberPlatePosZ =        " << RubberPlatePosZ<< G4endl;
			RubberPlatePosition = G4ThreeVector(0., 0., RubberPlatePosZ);
			fRubberPlate = new MUV1RubberPlate(G4Material::GetMaterial("G4_RUBBER_NATURAL"), fLogicalVolume, RubberPlateSize, RubberPlatePosition, iScintLayer);

			RubberPlatePosZ += (fBareScintillatorThickness	+ fRubberThickness + fIronThickness);

			// * SCINTILLATOR * VERTICAL LAYER *

			//			G4cout <<iScintLayer<<": ScintPosZ =        " << ScintPosZ<< G4endl;
			// iScintLayer --> 2*iScintLayer-1   but will be restored in MUV1ScintillatorLayer::CreateGeometry()
			Layer =	new MUV1ScintillatorLayer(G4Material::GetMaterial("G4_AIR"), fLogicalVolume, G4ThreeVector(0, 0, ScintPosZ), 2*iScintLayer-1, 0, fFastSimulation);

			ScintPosZ += (fBareScintillatorThickness + fRubberThickness + fIronThickness);



			// * IRON *

			//			G4cout <<iScintLayer<<": IronPlatePosZ =        " << IronPlatePosZ<< G4endl;
			IronPlatePosition = G4ThreeVector(0., 0., IronPlatePosZ);
			fIronPlate = new MUV1IronPlate(G4Material::GetMaterial("G4_Fe"), fLogicalVolume, IronPlateSize, IronPlatePosition, iScintLayer);

			IronPlatePosZ += (fBareScintillatorThickness + fRubberThickness + fIronThickness);
		}

	}


	// * IRON * LAST PLATE *
	//	G4cout <<"Last: IronPlatePosZ                = " << IronPlatePosZ<< G4endl;
	IronPlateSize     = G4ThreeVector(0.5 * fIronPlateOuterSizeX, 0.5 * fIronPlateOuterSizeY, 0.5 * fIronThickness);
	IronPlatePosition = G4ThreeVector(0., 0., IronPlatePosZ);
	fIronPlate = new MUV1IronPlate(G4Material::GetMaterial("G4_Fe"), fLogicalVolume, IronPlateSize, IronPlatePosition, 24);


	//           ***         CONNECTION BOLTS           ***

	// -X, +Y
	fBoltLeftUp
	= new MUV1Bolt(G4Material::GetMaterial("G4_Fe"), fLogicalVolume,
			G4ThreeVector(-fBoltPositionX, fBoltPositionY, fZPosition
					- MUV1ResponsibilityRegionZCenter + fZLength), 0);
	// +X, +Y
	fBoltRightUp
	= new MUV1Bolt(G4Material::GetMaterial("G4_Fe"), fLogicalVolume,
			G4ThreeVector(fBoltPositionX, fBoltPositionY, fZPosition
					- MUV1ResponsibilityRegionZCenter + fZLength), 0);

	// +X, -Y
	fBoltRightDown
	= new MUV1Bolt(G4Material::GetMaterial("G4_Fe"), fLogicalVolume,
			G4ThreeVector(fBoltPositionX, -fBoltPositionY, fZPosition
					- MUV1ResponsibilityRegionZCenter + fZLength), 0);

	// -X, -Y
	fBoltLeftDown
	= new MUV1Bolt(G4Material::GetMaterial("G4_Fe"), fLogicalVolume,
			G4ThreeVector(-fBoltPositionX, -fBoltPositionY, fZPosition
					- MUV1ResponsibilityRegionZCenter + fZLength), 0);



	//****************************************************
	//*                                                  *
	//*            transportation tube                   *
	//*                                                  *
	//****************************************************

	fTransportationTube = new MUV1TransportationTube(G4Material::GetMaterial("G4_Fe"),
													fLogicalVolume,
													2. * fZLength,
													G4ThreeVector(0, 0, fZPosition - MUV1ResponsibilityRegionZCenter + fZLength),
													0);

	//****************************************************
	//*                                                  *
	//*            beam pipe                             *
	//*                                                  *
	//****************************************************

	G4RotationMatrix Ra;
	G4ThreeVector Ta = G4ThreeVector(0, 0, 0);

	fBeamPipe = new MUV1BeamPipe(
			G4Material::GetMaterial("G4_Al"), fLogicalVolume,
			MUV1ResponsibilityRegionZLength, G4Transform3D(Ra, Ta));
}

void MUV1Detector::SetProperties() {
  // Set visualization properties
  fVisAtt = new G4VisAttributes(G4Colour(0.0, 1.0, 0.0));
  fVisAtt -> SetVisibility(false);
  fVisAtt->SetDaughtersInvisible(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
