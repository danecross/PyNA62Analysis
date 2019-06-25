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
// Copied from MUVGeometryParameters
// Changes to MUV2 and commented out by ykohl in March 2010
//
//
// Modified Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-17
// --------------------------------------------------------------------
#include "TVector.h"

#include "MUV2GeometryParameters.hh"
#include "DetectorParameter.hh"

MUV2GeometryParameters* MUV2GeometryParameters::fInstance = 0;

MUV2GeometryParameters::MUV2GeometryParameters() : NA62VGeometryParameters(G4String("MUV2"))
{
    // Define all the geometrical parameters and build the
    // responsibility region accordingly
    
    // Unused World Parameters for stand-alone configurations
    fWorldZLength = 22.*m;
    fWorldXLength = 10.*m;
    fWorldYLength = 10.*m;
    
    // Responsibility region
    fMUV2ResponsibilityRegionXLength    =   5.000*m;
    fMUV2ResponsibilityRegionYLength    =   5.000*m;
    fMUV2ResponsibilityRegionZBeginning = 244.385*m;
    fMUV2ResponsibilityRegionZEnd       = 245.350*m;

    fMUV2DetectorFrontZPosition = 244.435*m;

    // thickness of each scintillator (zsci)
    fScintillatorThickness  =  4.3*mm;
    // thickness of each iron plate (zfe)
    fIronThickness  =  25*mm;
    // thickness of the gap
    fGapThickness   =  7.7*mm;
    
    // thickness of aluminum wrapper foil
    fSkinAluminumWidth = 0.10*mm;
    // thickness of tape on top of aluminum wrapper foil
    fSkinTapeWidth = 0.05*mm;
    // Air gap parameter, tolerance.
    fAirGapWidth= 0.025*mm;

    // number of iron planes in the MUV2 (anfe)
    fNIronPlate = 24;
    
    // Scintillator width standard (xnvv)
    fScintWidthStandard = 119*mm;
    // Scintillator width middle (xmvv)
    fScintWidthMiddle = 108*mm;
    // Scintillator width outer (Back) (xsvv)
    fScintWidthOuter = 108*mm;
    // Scintillator length standard (ynvv)
    fScintLengthStandard = 1300*mm;
    // Scintillator length middle (ymvv)
    fScintLengthMiddle = 1242*mm;
    // Scintillator length outer (Back) (ysvv)
    fScintLengthOuter = 1150*mm;

    // Depth of box cut in scintillator
    fBoxCutDepth = (18+32)*mm;
    // height of the cut in the middle outer counter Logical Volume
    fmOScintillatorCutHeight = 50*mm;
    fmOScintillatorCutHeight2 = 32*mm;

    fScintillatorCutWidth = 58*mm;
    fScintillatorCutWidth2 = 32*mm;

    // Scintillator Birks Contant
    
    fBirksConstant = 0.151; // mm/MeV
    
    // HitContainer.cc
    
    fHitContainerTimeLimit = 20.*ns;
    fHitContainerScintillatorSegmentation = 100; // If you change this value, please change the array size of std::vector<MUV2Hit*> fHitArray in MUV2HitContainer.hh accordingly.
    fHitContainerDimension = 28;
    
    
    // Number of Standard Counters in one MUV2 quadrant-plane(nsvv)
    fNMUV2CounterStandard = 10;
    // Number of Middle Counters in one MUV2 quadrant-plane(0.5*nsmv)
    fNMUV2CounterMiddle = 1;
    // Number of Outer Counters in one MUV2 quadrant-plane
    fNMUV2CounterOuter = 0;
    
    // number of scintillators for each vertical counter in the MUV2 (0.5*strf)
    fNScintVertical = 12;
    // number of scintillators for each horizontal counter in the MUV2  (0.5*strf)
    fNScintHorizontal = 12;
    
    //  overall transverse dimensions (xall,yall)
    fMUV2Size = 2700*mm;
    // MUV2 length
    fMUV2Length = (fNScintVertical + fNScintHorizontal)*fScintillatorThickness + fNIronPlate*fIronThickness + (fNIronPlate-1)*fGapThickness;

    // MUV2 position in z
    fMUV2ZPosition = fMUV2DetectorFrontZPosition + 0.5 * fMUV2Length; // beatch file

    // MUV2 hole diameter
    fHoleDiameter = 212*mm;
    // MUV2 inner tube inner diameter
    fInnerTubeDiameter = 202*mm;

    // Beam pipe inner and outer radius
    fInnerBeamPipeRadius = 92.5*mm;
    fOuterBeamPipeRadius = 98.5*mm;

    fLongitudinalLengthBeamPipe =  fMUV2ResponsibilityRegionZEnd - fMUV2ResponsibilityRegionZBeginning;

    fResponsibilityRegion.push_back(new ResponsibilityRegion(fMUV2ResponsibilityRegionZBeginning,
                                                             fMUV2ResponsibilityRegionZEnd));
}

MUV2GeometryParameters::~MUV2GeometryParameters(){}
MUV2GeometryParameters* MUV2GeometryParameters::GetInstance()
{
    if ( fInstance == 0 ) { fInstance = new MUV2GeometryParameters(); }
    return fInstance;
}
TObjArray MUV2GeometryParameters::GetHashTable()
{
    TObjArray MUV2GeometryParameters;
    std::ostringstream Buffer;
    TString Value;
    TObjArray ParameterData;
    
    Buffer << fWorldZLength;
    Value = Buffer.str();
    Buffer.str("");
    ParameterData.Add(new TVectorT<G4double>(1, &fWorldZLength));
    MUV2GeometryParameters.Add(new DetectorParameter("fWorldZLength",Value.Data(),
                                                     "World Z Length", ParameterData));
    ParameterData.Clear();
    
    Buffer << fWorldXLength;
    Value = Buffer.str();
    Buffer.str("");
    ParameterData.Add(new TVectorT<G4double>(1, &fWorldXLength));
    MUV2GeometryParameters.Add(new DetectorParameter("fWorldXLength",Value.Data(),
                                                     "World X Length", ParameterData));
    ParameterData.Clear();
    
    Buffer << fWorldYLength;
    Value = Buffer.str();
    Buffer.str("");
    ParameterData.Add(new TVectorT<G4double>(1, &fWorldYLength));
    MUV2GeometryParameters.Add(new DetectorParameter("fWorldYLength",Value.Data(),
                                                     "World Y Length", ParameterData));
    ParameterData.Clear();
    
    Buffer << fMUV2ZPosition;
    Value = Buffer.str();
    Buffer.str("");
    ParameterData.Add(new TVectorT<G4double>(1, &fMUV2ZPosition));
    MUV2GeometryParameters.Add(new DetectorParameter("fMUV2ZPosition",Value.Data(),
                                                     "MUV2 Z Position", ParameterData));
    ParameterData.Clear();
    
    Buffer << fMUV2Size;
    Value = Buffer.str();
    Buffer.str("");
    ParameterData.Add(new TVectorT<G4double>(1, &fMUV2Size));
    MUV2GeometryParameters.Add(new DetectorParameter("fMUV2Size",Value.Data(),
                                                     "MUV2 Detector Transverse Size", ParameterData));
    ParameterData.Clear();
    
    Buffer << fMUV2Length;
    Value = Buffer.str();
    Buffer.str("");
    ParameterData.Add(new TVectorT<G4double>(1, &fMUV2Length));
    MUV2GeometryParameters.Add(new DetectorParameter("fMUV2Length",Value.Data(),
                                                     "MUV2 Detector Length", ParameterData));
    
    return MUV2GeometryParameters;
}
void MUV2GeometryParameters::Print(){
    
    G4cout << "fWorldZLength= "  << fWorldZLength  << G4endl
    << "fWorldXLength= "  << fWorldXLength  << G4endl
    << "fWorldYLength= "  << fWorldYLength  << G4endl
    << "fMUV2ZPosition= " << fMUV2ZPosition << G4endl
    << "fMUV2Size= "      << fMUV2Size      << G4endl
    << "fMUV2Length= "    << fMUV2Length    << G4endl;
}
G4RotationMatrix MUV2GeometryParameters::stringToRotationMatrix(G4String rotation) {
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
