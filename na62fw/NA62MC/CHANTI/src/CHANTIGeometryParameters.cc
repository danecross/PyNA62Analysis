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
// Created by Antonino Sergi 
// Modified by Vito Palladino  
// --------------------------------------------------------------

#include "TVector.h"
#include "CHANTIGeometryParameters.hh"
#include "DetectorParameter.hh"

using namespace std;

CHANTIGeometryParameters* CHANTIGeometryParameters::fInstance = 0;

CHANTIGeometryParameters::CHANTIGeometryParameters() : NA62VGeometryParameters(G4String("CHANTI")) 
{
  // Define all the geometrical parameters and build the
  // responsibility region accordingly

  // Unused World Parameters for stand-alone configurations and Detectors geometrical definition
  
  
  //         y ^    
  //           |  ^z
  //           | /
  //     x<____|/

  // MAIN parameters

  fWorldXLength = 10.*m;
  fWorldYLength = 10.*m;
  fWorldZLength = 10.*m;	

  fCHANTIDetectorXLength = 6000.*mm;
  fCHANTIDetectorYLength = 6000.*mm;

  G4double CHANTIStart   = 102.420*m;
  G4double CHANTIEnd     = 104.458*m;
  fCHANTIDetectorZLength = (CHANTIEnd - CHANTIStart);
  fCHANTIDetectorZPosition = 0.5*(CHANTIStart + CHANTIEnd);

  fCHANTISensitiveDetectorName = "/CHANTI";
  fCHANTICollectionName = "CHANTICollection";
  fNumberOfStation = 6;

  fRingXsideLength = 200.*mm;
  fRingYsideLength = 200.*mm;
  fTriangleAltitude = 16.5*mm;
  fTriangleBase = 33.*mm;
  fSquareLength = 300.*mm;
  fXInnerHoleLength = 95.*mm;
  fYInnerHoleLength = 65.*mm;
  fFiberRadius = 2.*mm;
  fOuterRadius = 150.*mm;

  //FRAME
  fXFrameLength = 1*mm;
  fYFrameLength = 1*mm;
  fZFrameLength = 1*mm;
  fFrameThickness = 1.0*mm;

  fXFrame = 23.5*mm;
  fYFrame = 10*mm;
  fDistXHole = 125.*mm;
  fDistYHole = 104*mm;

  fSupportAlaX = 104.*mm;
  fSupportAlaY = 1.*mm;
  fSupportAlaZ = 10.5*mm;

  //SILICON
  fSilThickness = 20*mm;

  //VESSEL
  fVesselWallThickness = 5.*mm; 
  fVesselWallHeight = 540.*mm; 
  fZVesselWall1 = 2033.*mm; 
  fZVesselWall2 = 2016.*mm;
  fZVesselWall3 = 17*mm;
  //fZVesselWall3 = fZVesselWall1 - fZVesselWall2 ;
  fVesselPatchLength  = 110*mm;
  fVesselWidth = 550 *mm;

  // DERIVED parameters

  fNofHalfStrip_Vertical_Front   =  ( 1 + (G4int)( 2*fXInnerHalfLength + fTriangleBase )/fTriangleBase );
  fNofHalfStrip_Vertical_Back    =  2*( (G4int)( fXInnerHalfLength/fTriangleBase + 1) );
  fNofHalfStrip_Horizzontal_Front =  ( 1 + (G4int)( 2*fYInnerHalfLength + fTriangleBase )/fTriangleBase );
  fNofHalfStrip_Horizzontal_Back  =  2*( (G4int)( fYInnerHalfLength/fTriangleBase + 1) );                  

  //int NofFullStripVerticalFront   =  ( 1 + (G4int)( 2*fOuterRadius + fTriangleBase )/fTriangleBase ) - fNofHalfStrip_Vertical_Front      ;
  //int NofFullStripVerticalBack    =  2*( (G4int)( fOuterRadius/fTriangleBase + 1) )                  - fNofHalfStrip_Vertical_Back       ;
  //int NofFullStripHorizontalFront =  ( 1 + (G4int)( 2*fOuterRadius + fTriangleBase )/fTriangleBase ) - fNofHalfStrip_Horizzontal_Front    ;
  //int NofFullStriphorizontalBack  =  2*( (G4int)( fOuterRadius/fTriangleBase + 1) )                  - fNofHalfStrip_Horizzontal_Back     ;

  fInnerRadius = fYInnerHalfLength; 
  fStripLength = fRingXsideLength;
  fRingThickness = fTriangleAltitude;

  /*
  G4double ZAbsolutePos_Ring0 =  102.427*m; // positions from target 
  G4double ZAbsolutePos_Ring1 =  102.477*m; // .
  G4double ZAbsolutePos_Ring2 =  102.577*m; // .
  G4double ZAbsolutePos_Ring3 =  102.777*m; // .
  G4double ZAbsolutePos_Ring4 =  103.177*m; // .
  G4double ZAbsolutePos_Ring5 =  103.977*m; // .
  */
  G4double ZAbsolutePos_Ring0 =  102.427*m; // positions from target 
  G4double ZAbsolutePos_Ring1 =  102.485*m; // .
  G4double ZAbsolutePos_Ring2 =  102.600*m; // .
  G4double ZAbsolutePos_Ring3 =  102.830*m; // .
  G4double ZAbsolutePos_Ring4 =  103.290*m; // .
  G4double ZAbsolutePos_Ring5 =  104.210*m; // .

  //G4double ZAbsolutePosSpectrometer = 183.678*m; // .

  fZPos_Ring[0] = (ZAbsolutePos_Ring0 - fCHANTIDetectorZPosition); 
  fZPos_Ring[1] = (ZAbsolutePos_Ring1 - fCHANTIDetectorZPosition);
  fZPos_Ring[2] = (ZAbsolutePos_Ring2 - fCHANTIDetectorZPosition);
  fZPos_Ring[3] = (ZAbsolutePos_Ring3 - fCHANTIDetectorZPosition);
  fZPos_Ring[4] = (ZAbsolutePos_Ring4 - fCHANTIDetectorZPosition);
  fZPos_Ring[5] = (ZAbsolutePos_Ring5 - fCHANTIDetectorZPosition);

  fResponsibilityRegion.push_back(new ResponsibilityRegion
				  (fCHANTIDetectorZPosition-0.5*fCHANTIDetectorZLength,
				   fCHANTIDetectorZPosition+0.5*fCHANTIDetectorZLength));

  //Double_t RingsPos[] = {fZPos_Ring0, fZPos_Ring1, fZPos_Ring2, fZPos_Ring3, fZPos_Ring4, fZPos_Ring5 };
}

CHANTIGeometryParameters::~CHANTIGeometryParameters(){}

CHANTIGeometryParameters* CHANTIGeometryParameters::GetInstance() {
  if (fInstance == 0) { fInstance = new CHANTIGeometryParameters(); }
  return fInstance;
}

TObjArray CHANTIGeometryParameters::GetHashTable() {
  TObjArray CHANTIGeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fWorldZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldZLength));
  CHANTIGeometryParameters.Add(new DetectorParameter("fWorldZLength",Value.Data(),
					       "World Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldXLength));
  CHANTIGeometryParameters.Add(new DetectorParameter("fWorldXLength",Value.Data(),
					       "World X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldYLength));
  CHANTIGeometryParameters.Add(new DetectorParameter("fWorldYLength",Value.Data(),
					       "World Y Length", ParameterData));
  ParameterData.Clear();

  Buffer << fCHANTIDetectorZPosition;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fCHANTIDetectorZPosition));
  CHANTIGeometryParameters.Add(new DetectorParameter("fCHANTIDetectorZPosition",Value.Data(),
					       "CHANTI Detector Z Position", ParameterData));
  ParameterData.Clear();

  Buffer << fCHANTIDetectorZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fCHANTIDetectorZLength));
  CHANTIGeometryParameters.Add(new DetectorParameter("fCHANTIDetectorZLength",Value.Data(),
					       "CHANTI Detector Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fCHANTIDetectorXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fCHANTIDetectorXLength));
  CHANTIGeometryParameters.Add(new DetectorParameter("fCHANTIDetectorXLength",Value.Data(),
					       "CHANTI Detector X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fCHANTIDetectorYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fCHANTIDetectorYLength));
  CHANTIGeometryParameters.Add(new DetectorParameter("fCHANTIDetectorYLength",Value.Data(),
					       "CHANTI Detector Y Length", ParameterData));

  return CHANTIGeometryParameters;
}

void CHANTIGeometryParameters::Print(){
  /*  G4cout << "fWorldXLength= "<< fWorldXLength << G4endl
	 << "fWorldYLength= "<< fWorldYLength << G4endl
	 << "fCHANTIDetectorZPosition= "<< fCHANTIDetectorZPosition << G4endl
	 << "fCHANTIDetectorXLength= "<< fCHANTIDetectorXLength << G4endl
	 << "fCHANTIDetectorYLength= "<< fCHANTIDetectorYLength << G4endl
	 << " fCHANTISensitiveDetectorName" << fCHANTISensitiveDetectorName
	 << "fCHANTICollectionName= "  << fCHANTICollectionName << G4endl
	 << "fNofTiles= "  << fNofTiles << G4endl
	 << "fSuperpositionGap= "  << fSuperpositionGap << G4endl
	 << "fInnerRadius= "  << fInnerRadius << G4endl
	 << "fOuterRadius= "  << fOuterRadius << G4endl
	 << "fCHANTISensitiveDetectorName= "  << fCHANTISensitiveDetectorName << G4endl
	 << "fCHANTICollectionName= "  << fCHANTICollectionName << G4endl
	 << "fTileThickness= "  << fTileThickness << G4endl
	 << "fLayersDistance= "  << fLayersDistance << G4endl
	 << " --------DERIVED parameters------"  << G4endl
	 << "fTheta= "  << fTheta << G4endl
	 << "fTileSmallSideLength= "  << fTileSmallSideLength << G4endl
	 << "fTileBigSideLength= "  << fTileBigSideLength << G4endl
	 << "fTileHeight= "  << fTileHeight << G4endl
	 << "fFirstTilesLayerPos= "  << fFirstTilesLayerPos << G4endl
	 << "fSecondTilesLayerPos= "  << fSecondTilesLayerPos << G4endl
	 << "fRingThickness= "  << fRingThickness << G4endl           
	 << "fZPos_Ring0= "  << fZPos_Ring0 << G4endl
	 << "fZPos_Ring1= "  << fZPos_Ring1 << G4endl
	 << "fZPos_Ring2= "  << fZPos_Ring2 << G4endl
	 << "fZPos_Ring3= "  << fZPos_Ring3 << G4endl
	 << "fZPos_Ring4= "  << fZPos_Ring4 << G4endl
	 << "fZPos_Ring5= "  << fZPos_Ring5 << G4endl
	 << "fCHANTIDetectorZLength= "<< fCHANTIDetectorZLength << G4endl
	 << "fWorldZLength= "<< fWorldZLength << G4endl
	 << G4endl;
  

  */



}
