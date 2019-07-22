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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-02
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
//
// Copied from Harish/MUV/MUVGeometryParameters.cc
// Changes to MUV1 by ykohl in March 2010
// Changes to MUV1 by Mario Vormstein since 2010-12-01
// Changes to MUV1 by Gia Khoriauli since 2017-11-10
//
// --------------------------------------------------------------------
#include "TVector.h"

#include "MUV1GeometryParameters.hh"
#include "DetectorParameter.hh"

MUV1GeometryParameters* MUV1GeometryParameters::fInstance = 0;

MUV1GeometryParameters::MUV1GeometryParameters() : NA62VGeometryParameters(G4String("MUV1")) 
{
  // Define all the geometrical parameters and build the
  // responsibility region accordingly

  // Unused World Parameters for stand-alone configurations
  fWorldZLength = 22.*m;
  fWorldXLength = 10.*m;
  fWorldYLength = 10.*m;

  // Responsibility region (from BEATCH file 29/11/10)
  fMUV1ResponsibilityRegionXLength    =   5.000*m;
  fMUV1ResponsibilityRegionYLength    =   5.000*m;


  fMUV1ResponsibilityRegionZBeginning = 243.224*m;
  fMUV1ResponsibilityRegionZEnd       = 244.385*m;
  fMUV1DetectorFrontZPosition         = 243.418*m;  // = fMUV1ResponsibilityRegionZBeginning

  // thickness of each scintillator (zsci)
  fBareScintillatorThickness  =  0.9*cm;
  // thickness of each iron plate (zfe)
  fIronThickness  =  2.68*cm;
  // Thickness of the rubber plate
  fRubberThickness=  0.3*cm;
 

  // number of iron planes in the MUV1 (anfe)
  fNIronPlate = 24;
  //Size of iron plates
  fIronPlateSizeX = 2730*mm;
  fIronPlateSizeY = 2630*mm;
  fIronPlateOuterSizeX = 3100*mm;
  fIronPlateOuterSizeY = 3000*mm;
  // number of rubber plates
  fNRubberPlate = 23;
  // Radius for connection holes
  fConnectionHoleRadius  = 25*mm;
  fOuterSpacerOuterRadius = 45*mm;

  // COUNTER VARIABLES

  // Number of Standard Counters in one MUV1 top-plane(nsvv)
  fNMUV1CounterStandard = 16;
  // Number of Middle Counters Standard in one MUV1 quadrant-plane
  fNMUV1CounterMiddleStd = 1;
  // Number of Middle Counters  Outer in one MUV1 Quadrant Plane
  fNMUV1CounterMiddleOuter = 1;
  // Total Number of Middle Counters in one MUV1 quadrant-plane
  fNMUV1CounterMiddle = fNMUV1CounterMiddleStd +fNMUV1CounterMiddleOuter ;
  // Number of Outer Counters in one MUV1 quadrant-plane
  fNMUV1CounterOuter = 4;


  // PMT

  fPMTSize=G4ThreeVector(1.*cm,1.*cm,1*cm);
  // Gap between fibers and PMT
  fPMTAirGap = 0.1*mm;
  // Thickness of glass window of PMT
  fPMTGlassGap = 1*mm;


  // FIBER VARIABLES

  // Number of fibers in a scintillator
  fNFiber = 2; //todo: is it needed?
  // Radius of Each Fiber
  fFiberRadius = 0.6*mm;
  // Sepretation between the centers of the two fibers
  fFiberSeperation = 2.9*cm; //TODO: is it needed?
  // First position of fiber w.r.t to Scintillator
  fFirstFiberXPosition = -1.45*cm;
  // Depth of box cut in scintillator
  fCutDepth = 22*mm; //TODO: outdated parameter
  //Shortest cut depth of fiber in trapezoidal scintillator
  fCutDepthShort = 2*mm;
  //Longest cut depth of fiber in trapezoidal scintillator
  fCutDepthLong  = 32*mm;

  fCutWidth = 27*mm;
  // height of the cut in the middle outer counter Logical Volume ----->??
  fmOCounterCutHeight = 2.2*cm; //todo: is it needed?
  // Length of the outer fibers (between scintillator and PMT)
  //In standard length or half size scintillators
  fFiberLengthOut = 90*cm;
  // Cut of photon energy
  fFiberPhotonEnergyCut = 2*eV;


  // Groove

  fGrooveWidth = 1.6*mm;
  fGrooveDepth = 1.6*mm;
  fGroovePositionInStandardScintillator = 1.5*cm;



  // SCINTILLATOR VARIABLES

  // number of scintillators for each vertical counter in the MUV1 (0.5*strf)
  fNScintVertical = 11;
  // number of scintillators for each horizontal counter in the MUV1 (0.5*strf)
  fNScintHorizontal = 12;

  // Scintillator width standard (xnvv)
  fBareScintWidthStandard = 6.*cm;
  // Scintillator width middle (xmvv)
  fBareScintWidthMiddle = 5.4*cm;
  // Scintillator width outer (Back) (xsvv)
  fBareScintWidthOuter = 6.*cm;

  // Scintillator length standard (ynvv)
  fBareScintLengthStandard = 261.6*cm;
  // Scintillator length middle (ymvv)
  fBareScintLengthMiddleStd = 120.*cm;
  // Trapezoidal middle scintillator length
  fBareScintLengthMiddleOuter = 125.4*cm;

  // Scintillator wrapper width
  fSkinWidth   = 0.01*mm;
  // Air gap as a width between wrapper and scintillator and as a position tolerance parameter
  fAirGapWidth = 0.001*mm;

  // Scintillator thickness wrapped
  fScintillatorThickness  =  fBareScintillatorThickness + 2 * (fSkinWidth + fAirGapWidth);

  // Scintillator width standard wrapped (xnvv)
  fScintWidthStandard = fBareScintWidthStandard + 2 * (fSkinWidth + fAirGapWidth);
  // Scintillator width middle wrapped (xmvv)
  fScintWidthMiddle = fBareScintWidthMiddle + 2 * (fSkinWidth + fAirGapWidth);
  // Scintillator width outer wrapped (Back) (xsvv)
  fScintWidthOuter = fBareScintWidthOuter + 2 * (fSkinWidth + fAirGapWidth);

  // Scintillator length standard wrapped (ynvv)
  fScintLengthStandard = fBareScintLengthStandard + 2 * (fSkinWidth + fAirGapWidth);
  // Scintillator length middle wrapped (ymvv)
  fScintLengthMiddleStd = fBareScintLengthMiddleStd + 2 * (fSkinWidth + fAirGapWidth);
  // Trapezoidal middle scintillator length wrapped
  fScintLengthMiddleOuter = fBareScintLengthMiddleOuter + 2 * (fSkinWidth + fAirGapWidth);

  // Scintillator length outer (Back) (ysvv)
  fScintLengthOuter = new G4double[fNMUV1CounterOuter];
  fScintLengthOuter[0] = 213.6*cm + 2 * (fSkinWidth + fAirGapWidth);
  fScintLengthOuter[1] = 225.6*cm + 2 * (fSkinWidth + fAirGapWidth);
  fScintLengthOuter[2] = 237.6*cm + 2 * (fSkinWidth + fAirGapWidth);
  fScintLengthOuter[3] = 249.6*cm + 2 * (fSkinWidth + fAirGapWidth);

  // Scintillator mother box width
  fScintMotherWidth = fScintWidthStandard + 2 * (fAirGapWidth/3.);

  // Scintillator mother box lenght
  fScintMotherLenght = fScintLengthStandard + 2 * (fFiberLengthOut + 2 * fPMTSize.y()) + 2 * (fAirGapWidth/3.) ;

  // Scintillator mother box thickness
  fScintMotherThickness  =  fScintillatorThickness + 2 * (fAirGapWidth/3.);

  // Scintillator layer box thickness
  fScintLayerThickness  =  fScintMotherThickness + 2 * (fAirGapWidth/3.);

  // Rubber thickness is smaller than the nominal value because  fScintLayerThickness > fBareScintillatorThickness and there have to be some gaps between iron, scintillator mother and rubber
  fRubberRealThickness =  0.7 * mm ;



  // Scintillator Birks Contant

   fBirksConstant = 0.151; // mm/MeV

   // HitContainer.cc

   fHitContainerTimeLimit = 20.*ns;
   fHitContainerScintillatorSegmentation = 100; // If you change this value, please change the array size of std::vector<MUV1Hit*> fHitArray in MUV1HitContainer.hh accordingly.
   fHitContainerDimension = 28;

   // POSITION VARIABLES

  //  overall transverse dimensions (xall,yall)
  fMUV1XSize = 273.0*cm; 
  fMUV1YSize = 263.0*cm;

  // MUV1 length
  fMUV1Length = (fNScintVertical + fNScintHorizontal)*fScintillatorThickness + 
                    fNIronPlate*fIronThickness + fNRubberPlate*fRubberThickness;
  // = 25*2.50+24*0.9+24*0.3 = 91.30 cm / OLD DESIGN
  // = 24*2.68+23*0.9+23*0.3 = 91.92 cm / NEW DESIGN -> AS BUILT
  
  // MUV1 position in z
  // fMUV1ZPosition = 0*cm; // set to zero for testing, so you can rotate it properly
  fMUV1ZPosition = fMUV1DetectorFrontZPosition + 0.5 * fMUV1Length;  // beatch file 
  // = 243.418*m + 0.5*0.9130*m = 243.8745*m / OLD DESIGN
  // = 243.418*m + 0.5*0.9192*m = 243.8771*m / NEW DESIGN -> AS BUILT

  // MUV1 hole diameter (dchol)
  fHoleDiameter = 21.6*cm;

  // Scintillator Spacer
  fScintillatorSpacerSize = G4ThreeVector((216./2)*mm, (216./2)*mm, (12./2)*mm);
  fCutBoxOuterSize        = G4ThreeVector((54./2)*mm,  (14./2)*mm,  (12./2)*mm);
  fCutBoxMiddleOuterSize  = G4ThreeVector((72./2)*mm,  (72./2)*mm,  (12./2)*mm);

  // Transportation tube
  fHoleInnerRadius = 105*mm;
  fTubeInnerRadius = 103*mm;

  //Bolt
  fBoltPositionX = 1280*mm;
  fBoltPositionY = 1250*mm;

  // Beam pipe inner and outer radius
  fInnerBeamPipeRadius = 92.5*mm;
  fOuterBeamPipeRadius = 98.5*mm;

  fLongitudinalLengthBeamPipe =  fMUV1ResponsibilityRegionZEnd - fMUV1ResponsibilityRegionZBeginning;


  fResponsibilityRegion.push_back(new ResponsibilityRegion(fMUV1ResponsibilityRegionZBeginning,
							   fMUV1ResponsibilityRegionZEnd));
}

MUV1GeometryParameters::~MUV1GeometryParameters(){}

MUV1GeometryParameters* MUV1GeometryParameters::GetInstance()
{
  if ( fInstance == 0 ) { fInstance = new MUV1GeometryParameters(); }
  return fInstance;
}

TObjArray MUV1GeometryParameters::GetHashTable()
{
  TObjArray MUV1GeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fWorldZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldZLength));
  MUV1GeometryParameters.Add(new DetectorParameter("fWorldZLength",Value.Data(),
						   "World Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldXLength));
  MUV1GeometryParameters.Add(new DetectorParameter("fWorldXLength",Value.Data(),
						   "World X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldYLength));
  MUV1GeometryParameters.Add(new DetectorParameter("fWorldYLength",Value.Data(),
						   "World Y Length", ParameterData));
  ParameterData.Clear();

  Buffer << fMUV1ZPosition;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMUV1ZPosition));
  MUV1GeometryParameters.Add(new DetectorParameter("fMUV1ZPosition",Value.Data(),
						   "MUV1 Z Position", ParameterData));
  ParameterData.Clear();

  Buffer << fMUV1XSize;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMUV1XSize));
  MUV1GeometryParameters.Add(new DetectorParameter("fMUV1XSize",Value.Data(),
						   "MUV1 Detector Transverse Size", ParameterData));

  ParameterData.Clear();

  Buffer << fMUV1YSize;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMUV1YSize));
  MUV1GeometryParameters.Add(new DetectorParameter("fMUV1YSize",Value.Data(),
						   "MUV1 Detector LOngitudinal Size", ParameterData));

  ParameterData.Clear();
 

  Buffer << fMUV1Length;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMUV1Length));
  MUV1GeometryParameters.Add(new DetectorParameter("fMUV1Length",Value.Data(),
						   "MUV1 Detector Length", ParameterData));

  return MUV1GeometryParameters;
}

void MUV1GeometryParameters::Print(){

  G4cout << "fWorldZLength= "  << fWorldZLength  << G4endl
	 << "fWorldXLength= "  << fWorldXLength  << G4endl
	 << "fWorldYLength= "  << fWorldYLength  << G4endl
	 << "fMUV1ZPosition= " << fMUV1ZPosition << G4endl
	 << "fMUV1XSize= "     << fMUV1XSize     << G4endl
         << "fMUV1YSize= "     << fMUV1YSize     << G4endl
	 << "fMUV1Length= "    << fMUV1Length    << G4endl;
}

G4RotationMatrix MUV1GeometryParameters::stringToRotationMatrix(G4String rotation) {
  // We apply successive rotations OF THE OBJECT around the FIXED
  // axes of the parent's local coordinates; rotations are applied
  // left-to-right (rotation="r1,r2,r3" => r1 then r2 then r3).

  G4RotationMatrix rot;

  int place = 0;

  while (place+1 < (int)rotation.size()) {

    char *p=0;
    G4double angle = strtod(rotation.substr(place+1).c_str(), &p) * deg;
    unsigned int pIndex = p-rotation.substr(place+1).c_str();
    if(!p || (pIndex<rotation.substr(place+1).size() && *p != (char) '\0' && *p != (char) ',')) {
      G4cerr << "Invalid rotation specification: " << rotation.c_str() << G4endl;
      return rot;
    }

    G4RotationMatrix thisRotation;

    switch (rotation.substr(place, 1).c_str()[0]) {
      case 'X':
      case 'x':
        thisRotation = G4RotationMatrix(CLHEP::HepRotationX(angle));
        break;
      case 'Y':
      case 'y':
        thisRotation = G4RotationMatrix(CLHEP::HepRotationY(angle));
        break;
      case 'Z':
      case 'z':
        thisRotation = G4RotationMatrix(CLHEP::HepRotationZ(angle));
        break;
      default:
        G4cerr << " Invalid rotation specification: " << rotation << G4endl;
        return rot;
    }

    rot = thisRotation * rot;
    place = rotation.find(',', place);
    if (place ==-1)
      break;
    ++place;
  }

  return rot;

}
