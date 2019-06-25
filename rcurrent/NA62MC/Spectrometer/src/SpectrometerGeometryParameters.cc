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
// Modified by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-04-24
// - Fixed geometry conflicts
//
// Modified by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-02-07
// - Removed extra copper layer from the Straw radius
//
// Modified by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2016-01-15
// -Corrections to longitudinal coordinates of the STRAW chambers
//
// Modified by Emanuele Leonardi (emanuele.leonardi@roma1.infn.it) 01-05-2015
// - Reduced responsibility regions to allow construction of blue tube
//
// Modified by Giuseppe Ruggiero (giuseppe.ruggiero@cern.ch) 30-01-2012
// - Geometry changed according to the Left-Right inversion of the Layout
// 
// Modified by Giuseppe Ruggiero (giuseppe.ruggiero@cern.ch) 22-09-2010 
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#include "TVector.h"

#include "SpectrometerGeometryParameters.hh"
#include "DetectorParameter.hh"

/// \class SpectrometerGeometryParameters 
/// \Brief
/// Class containing the parameters of the straw spectrometer
/// \EndBrief
/// 
/// \Detailed
/// The parameters describe the straw spectrometer according to the BEATCH file 11/06/2010 and
/// to the TD.
/// \EndDetailed

SpectrometerGeometryParameters* SpectrometerGeometryParameters::fInstance = 0;

SpectrometerGeometryParameters::SpectrometerGeometryParameters() :
  NA62VGeometryParameters(G4String("Spectrometer")) {

  /////////////////////////////////////////////////
  // Define and register the responsibility regions

  // preceded by LAV RR0 (LAVs 1-8)
  fRR_ZofFrontFace[0] = 183.310*m;
  fRR_ZofBackFace[0] = 183.705*m; // RR0: Chamber 1 
  // interspaced by LAV RR1 (LAV 9)
  fRR_ZofFrontFace[1] = 193.371*m;
  fRR_ZofBackFace[1] = 198.070*m; // RR1: Chamber 2 + MNP33 magnet
  // interspaced by LAV RR2 (LAV 10)
  fRR_ZofFrontFace[2] = 203.764*m; 
  fRR_ZofBackFace[2] = 204.656*m; // RR2: Chamber 3
  // interspaced by LAV RR3 (LAV 11)
  fRR_ZofFrontFace[3] = 218.190*m; 
  fRR_ZofBackFace[3] = 219.545*m; // RR3: Chamber 4
  // followed by the RICH RR (starting at 219.546 m)

  for (G4int iRR=0; iRR<4; iRR++) {
    fRR_Radius[iRR]    = 2200*mm; // to accommodate the chambers and the magnet
    fResponsibilityRegion.push_back
      (new ResponsibilityRegion(fRR_ZofFrontFace[iRR],fRR_ZofBackFace[iRR]));
  }

  // One straw (to be updated according to the TD)
  fStrawInnerRadius = 0.4875*cm;
  fCopperThickness  = 0.05*um;
  fMylarThickness   = 36*um;
  fGoldThickness    = 0.02*um;
  fStrawLength      = 210.*cm;
  fWireRadius       = 15.*um;
  fStrawRadius      = fStrawInnerRadius+fGoldThickness+fCopperThickness+fMylarThickness;
  fStrawDiameter    = 2*fStrawRadius; 

  // One view (TD)
  fNStraws              = 122; // number of straws per layer (4 layers = one view)
  fViewSize             = fStrawLength; 
  fStrawSpacing         = 17.6*mm;                      // spacing between straws along the view direction
  fLayerSpacing         = 11.0*mm;                      // distance in z between layers 1-2 and between layers 3-4 
  fLayerDisplacement    = fStrawSpacing/2.;             // displacement of layers 1-2 and layers 3-4 along the view direction
  fHalfViewSpacing      = fLayerSpacing+15.00*mm;       // distance in z between layer 1 and layer 3 
  fHalfViewDisplacement = fStrawSpacing/4.;             // displacement of layers 2-3 along the view direction
  fHalfViewZLength      = 2*fStrawRadius+fLayerSpacing; // thickness in z of one half-view
  fHalfViewXLength      = (fNStraws-1)*fStrawSpacing + fStrawDiameter;               
  fHalfViewYLength      = fStrawLength;
  fViewZLength          = fHalfViewSpacing+fLayerSpacing+fStrawDiameter; // thickness in z of one view
  fViewXLength          = fHalfViewXLength+fHalfViewDisplacement; //fHalfViewDisplacement added due to shift of halfviews wrt view center
  fViewYLength          = fStrawLength;
  fViewSpacing1         = 57.0*mm;   // distance between views u-v and views x-y in one chamber 
  fViewSpacing2         = 233.0*mm;  // distance between views v-x in one chamber

  // Chamber (beatch file 23/02/2011) 
  fChamberRadius            = fViewXLength/sqrt(2.0);
  fChamberZLength           = 39.382214*cm; // originally 39.38cm, but true length = fViewZLength/2 + fViewSpacing1 + fViewSpacing2 + fViewSpacing1 + fViewZLength/2 = 39.382214cm 
  fChamberZCenterOffset[0]  = -0.0001*m;
  fChamberZCenterOffset[1]  = -0.0031*m;
  fChamberZCenterOffset[2]  = -0.0091*m;
  fChamberZCenterOffset[3]  = -0.0093*m;
  fChamberZCenter[0]        = 183.508*m+fChamberZCenterOffset[0]; //same offsets as in data (from config file in reconstruction)
  fChamberZCenter[1]        = 194.066*m+fChamberZCenterOffset[1];
  fChamberZCenter[2]        = 204.459*m+fChamberZCenterOffset[2];
  fChamberZCenter[3]        = 218.885*m+fChamberZCenterOffset[3];
  fChambernXRminY           = -63.8*mm;
  fChambernXRmaxY           = +63.8*mm;
  fChambernXORminY          = -1054.5*mm;
  fChambernXORmaxY          = 1037.5*mm;

  fChamberXDisplacement[0]  = 101.2*mm; // average x coordinate of the center of the hole in chamber 1
  fChamberXRmaxU[0]         = 134.2*mm;
  fChamberXRminU[0]         = 6.7*mm;
  fChamberXRmaxX[0]         = 165.0*mm;
  fChamberXRminX[0]         = 37.4*mm;
  fChamberXORminU[0]        = -1054.1*mm; //Zuzana: outer radius, to avoid the outermost straws 
  fChamberXORmaxU[0]        = 1037*mm;
  fChamberXORminX[0]        = -1054.5*mm;
  fChamberXORmaxX[0]        = 1037*mm;  

  fChamberXDisplacement[1]  = 114.4*mm; // average x coordinate of the center of the hole in chamber 2
  fChamberXRmaxU[1]         = 143.0*mm;
  fChamberXRminU[1]         = 15.5*mm;
  fChamberXRmaxX[1]         = 178.2*mm;
  fChamberXRminX[1]         = 50.6*mm;
  fChamberXORminU[1]        = -1054.5*mm;
  fChamberXORmaxU[1]        = 1037.5*mm;
  fChamberXORminX[1]        = -1054.5*mm;
  fChamberXORmaxX[1]        = 1037.5*mm;
  
  fChamberXDisplacement[2]  = 92.4*mm;  // average x coordinate of the center of the hole in chamber 3
  fChamberXRmaxU[2]         = 129.8*mm;
  fChamberXRminU[2]         = 2.2*mm;
  fChamberXRmaxX[2]         = 156.2*mm;
  fChamberXRminX[2]         = 28.7*mm;
  fChamberXORminU[2]        = -1054.5*mm;
  fChamberXORmaxU[2]        = 1037.5*mm;
  fChamberXORminX[2]        = -1054.5*mm;
  fChamberXORmaxX[2]        = 1037.5*mm;

  fChamberXDisplacement[3]  = 52.8*mm;  // average x coordinate of the center of the hole in chamber 4
  fChamberXRmaxU[3]         = 103.4*mm;
  fChamberXRminU[3]         = -24.2*mm;
  fChamberXRmaxX[3]         = 116.6*mm;
  fChamberXRminX[3]         = -11.0*mm;
  fChamberXORminU[3]        = -1054.5*mm;
  fChamberXORmaxU[3]        = 1037.5*mm;
  fChamberXORminX[3]        = -1054.5*mm;
  fChamberXORmaxX[3]        = 1037.5*mm;

  // MNP33 magnet
  fMagnetXLength       = 2.0*fChamberRadius;
  fMagnetYLength       = 2.0*fChamberRadius;
  fMagnetZLength       = 1.3*m;
  fMagnetZFront        = 196.345*m;
  fMagnetZPosition     = fMagnetZFront + 0.5*fMagnetZLength;
  fMagnetFieldStrength = 0.6928*tesla;
}

SpectrometerGeometryParameters* SpectrometerGeometryParameters::GetInstance() {
  if (fInstance == 0) fInstance = new SpectrometerGeometryParameters();
  return fInstance;
}

TObjArray SpectrometerGeometryParameters::GetHashTable() {
  TObjArray SpectrometerGeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;
  ParameterData.Clear();

  Buffer << fStrawInnerRadius;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fStrawInnerRadius));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fStrawInnerRadius",Value.Data(),
							   "Straw Inner Radius", ParameterData));
  ParameterData.Clear();

  Buffer << fCopperThickness;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fCopperThickness));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fCopperThickness",Value.Data(),
							   "Copper Thickness", ParameterData));
  ParameterData.Clear();

  Buffer << fMylarThickness;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMylarThickness));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fMylarThickness",Value.Data(),
							   "Mylar Thickness", ParameterData));
  ParameterData.Clear();

  Buffer << fGoldThickness;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fGoldThickness));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fGoldThickness",Value.Data(),
							   "Gold Thickness", ParameterData));
  ParameterData.Clear();

  Buffer << fStrawRadius;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fStrawRadius));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fStrawRadius",Value.Data(),
							   "Straw Radius", ParameterData));
  ParameterData.Clear();

  Buffer << fStrawDiameter;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fStrawDiameter));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fStrawDiameter",Value.Data(),
							   "Straw Diameter", ParameterData));
  ParameterData.Clear();

  Buffer << fStrawLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fStrawLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fStrawLength",Value.Data(),
							   "Straw Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWireRadius;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWireRadius));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fWireRadius",Value.Data(),
							   "Wire Radius", ParameterData));
  ParameterData.Clear();

  Buffer << fViewSize;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fViewSize));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fViewSize",Value.Data(),
							   "View Size", ParameterData));
  ParameterData.Clear();

  Buffer << fStrawSpacing;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fStrawSpacing));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fStrawSpacing",Value.Data(),
							   "Straw Spacing", ParameterData));
  ParameterData.Clear();

  Buffer << fLayerSpacing;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLayerSpacing));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fLayerSpacing",Value.Data(),
							   "Layer Spacing", ParameterData));
  ParameterData.Clear();

  Buffer << fLayerDisplacement;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLayerDisplacement));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fLayerDisplacement",Value.Data(),
							   "Layer Displacement", ParameterData));
  ParameterData.Clear();

  Buffer << fHalfViewSpacing;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fHalfViewSpacing));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fHalfViewSpacing",Value.Data(),
							   "Half View Spacing", ParameterData));
  ParameterData.Clear();

  Buffer << fHalfViewDisplacement;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fHalfViewDisplacement));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fHalfViewDisplacement",Value.Data(),
							   "Half View Displacement", ParameterData));
  ParameterData.Clear();

  Buffer << fHalfViewZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fHalfViewZLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fHalfViewZLength",Value.Data(),
							   "Half View Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fHalfViewXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fHalfViewXLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fHalfViewXLength",Value.Data(),
							   "Half View X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fHalfViewYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fHalfViewYLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fHalfViewYLength",Value.Data(),
							   "Half View Y Length", ParameterData));
  ParameterData.Clear();

  Buffer << fViewZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fViewZLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fViewZLength",Value.Data(),
							   "View Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fViewXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fViewXLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fViewXLength",Value.Data(),
							   "View X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fViewYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fViewYLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fViewYLength",Value.Data(),
							   "View Y Length", ParameterData));
  ParameterData.Clear();

  Buffer << fViewSpacing1;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fViewSpacing1));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fViewSpacing1",Value.Data(),
							   "View Spacing", ParameterData));
  ParameterData.Clear();

  Buffer << fViewSpacing2;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fViewSpacing2));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fViewSpacing2",Value.Data(),
							   "View Spacing", ParameterData));
  ParameterData.Clear();

  Buffer << fChamberRadius;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fChamberRadius));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fChamberRadius",Value.Data(),
							   "Chamber Radius", ParameterData));
  ParameterData.Clear();

  Buffer << fChamberZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fChamberZLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fChamberZLength",Value.Data(),
							   "Chamber Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fMagnetZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMagnetZLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fMagnetZLength",Value.Data(),
							   "Magnet Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fMagnetXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMagnetXLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fMagnetXLength",Value.Data(),
							   "Magnet X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fMagnetYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMagnetYLength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fMagnetYLength",Value.Data(),
							   "Magnet Y Length", ParameterData));
  ParameterData.Clear();

  Buffer << fMagnetZPosition;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMagnetZPosition));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fMagnetZPosition",Value.Data(),
							   "Magnet Z Position", ParameterData));
  ParameterData.Clear();

  Buffer << fMagnetFieldStrength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMagnetFieldStrength));
  SpectrometerGeometryParameters.Add(new DetectorParameter("fMagnetFieldStrength",Value.Data(),
							   "Magnet Field Strength", ParameterData));
  ParameterData.Clear();

  return SpectrometerGeometryParameters;
}

void SpectrometerGeometryParameters::Print() {
  G4cout << "fStrawInnerRadius= "<< fStrawInnerRadius << G4endl
	 << "fCopperThickness= "<< fCopperThickness << G4endl
	 << "fMylarThickness= "<< fMylarThickness << G4endl
	 << "fGoldThickness= "<< fGoldThickness << G4endl
	 << "fStrawRadius= "<< fStrawRadius << G4endl
	 << "fStrawDiameter= "<< fStrawDiameter << G4endl
	 << "fStrawLength= "<< fStrawLength << G4endl
	 << "fWireRadius= "<< fWireRadius << G4endl
	 << "fNStraws= "<< fNStraws << G4endl
	 << "fViewSize= "<< fViewSize << G4endl
	 << "fStrawSpacing= "<< fStrawSpacing << G4endl
	 << "fLayerSpacing= "<< fLayerSpacing << G4endl
	 << "fLayerDisplacement= "<< fLayerDisplacement << G4endl
	 << "fHalfViewSpacing= "<< fHalfViewSpacing << G4endl
	 << "fHalfViewDisplacement= "<< fHalfViewDisplacement << G4endl
	 << "fHalfViewZLength= "<< fHalfViewZLength << G4endl
	 << "fHalfViewXLength= "<< fHalfViewXLength << G4endl
	 << "fHalfViewYLength= "<< fHalfViewYLength << G4endl
	 << "fViewZLength= "<< fViewZLength << G4endl
	 << "fViewXLength= "<< fViewXLength << G4endl
	 << "fViewYLength= "<< fViewYLength << G4endl
	 << "fViewSpacing1= "<< fViewSpacing1 << G4endl
	 << "fViewSpacing2= "<< fViewSpacing2 << G4endl
	 << "fChamberRadius= "<< fChamberRadius << G4endl
	 << "fChamberZLength= "<< fChamberZLength << G4endl
	 << "fMagnetZLength= "<< fMagnetZLength << G4endl
	 << "fMagnetXLength= "<< fMagnetXLength << G4endl
	 << "fMagnetYLength= "<< fMagnetYLength << G4endl
	 << "fMagnetZPosition= "<< fMagnetZPosition << G4endl
	 << "fMagnetFieldStrength= "<< fMagnetFieldStrength << G4endl;
}
