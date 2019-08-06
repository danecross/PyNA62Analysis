#include "TVector.h"

#include "HACGeometryParameters.hh"
#include "DetectorParameter.hh"
//
// --------------------------------------------------------------------
// History:
//
// Created by Giuseppe Ruggiero 04-09-2012
//
// 2013-02-08 Spasimir Balev - update of HAC magnet and basic simulation
//                             of the detector
//
// --------------------------------------------------------------------

HACGeometryParameters* HACGeometryParameters::fInstance = 0;

HACGeometryParameters::HACGeometryParameters() :
  NA62VGeometryParameters(G4String("HAC")),
  // Responsibility region
  fHACRespRegionZStart (247.800*m),
  fHACRespRegionZEnd   (255.059*m),
  fHACRespRegionZCenter((fHACRespRegionZStart+fHACRespRegionZEnd)/2.),
  fHACRespRegionXLength(5.*m),
  fHACRespRegionYLength(5.*m),
  fHACRespRegionZLength((fHACRespRegionZEnd-fHACRespRegionZStart)),
  // Magnet
  fHACMagnetZPosition    (249.200*m),
  fHACMagnetZLength      (2.0*m),
  fHACMagnetFieldStrength(1.7012*tesla), // Ptkick = -1021 MeV/c
  fHACMagnetXLength      (420*mm),
  fHACMagnetYLength      (200*mm),
  // Absorber
  fAbsorberLayerXLength(100*mm),
  fAbsorberLayerYLength(100*mm),
  fAbsorberLayerZLength(16*mm),
  // Scintillator
  fScintillatorLayerXLength(100*mm),
  fScintillatorLayerYLength(100*mm),
  fScintillatorLayerZLength(4*mm),
  // Scintillator Layers (absorber layers fNLayers-1)
  fNLayers(60),
  // Module
  fHACModuleXLength(100*mm),
  fHACModuleYLength(100*mm),
  fHACModuleZLength(170*cm),
  // Detector
  fHACDetectorZPosition(253.350*m), // this leaves 170.9 mm to the end of ECN3 for services
  fHACDetectorZLength  (170*cm)
{
  // Beam pipes
  fBeamPipeZLength[0] = 0.4*m;
  fBeamPipeZPosition[0] = 248.0*m - fHACRespRegionZCenter;
  fBeamPipeInnerRadius[0] = 92.5*mm;
  fBeamPipeOuterRadius[0] = 98.5*mm;
  fBeamPipeFinZLength[0] = 1.0*mm;
  fBeamPipeFinOuterRadius[0] = 99.5*mm;
  fBeamPipeFinSpacing[0] = 5.0*m;
  fBeamPipeInputDisplacementWRTBeam[0]=0;
  fBeamPipeOutputDisplacementWRTBeam[0]=0;
  fBeamPipeZLengthWFins[0] = 0;

  fBeamPipeZLength[1] = 4.315*m - 1.75*mm;
  fBeamPipeZPosition[1] = 252.9015*m - fHACRespRegionZCenter-1.75*mm;
  fBeamPipeInnerRadius[1] = 159.0*mm;
  fBeamPipeOuterRadius[1] = 162.0*mm;
  fBeamPipeFinZLength[1] = 1.0*mm;
  fBeamPipeFinOuterRadius[1] = 163.0*mm;
  fBeamPipeFinSpacing[1] = 5.0*m;
  fBeamPipeInputDisplacementWRTBeam[1]=40*mm;
  fBeamPipeOutputDisplacementWRTBeam[1]=-40*mm;
  fBeamPipeZLengthWFins[1] = 0;

  fBeamPipeZLength[2] = 0.534*m;
  fBeamPipeZPosition[2] = 250.467*m - fHACRespRegionZCenter;
  fBeamPipeInnerRadius[2] = 92.5*mm;
  fBeamPipeOuterRadius[2] = 98.5*mm;
  fBeamPipeFinZLength[2] = 1.0*mm;
  fBeamPipeFinOuterRadius[2] = 99.5*mm;
  fBeamPipeFinSpacing[2] = 5.0*m;
  fBeamPipeInputDisplacementWRTBeam[2]=0;
  fBeamPipeOutputDisplacementWRTBeam[2]=0;
  fBeamPipeZLengthWFins[2] = 0;
  //cppcheck-suppress useInitializationList
  fHACDetectorYRotation = -(fBeamPipeInputDisplacementWRTBeam[1] - fBeamPipeOutputDisplacementWRTBeam[1])/fBeamPipeZLength[1]*rad;

  // Coordinates of the centers of the 9 modules
  // Module numbering (temprorary)
  // <-- x axis
  //      3 4 5
  // pipe 0 1 2
  //      6 7 8

  fHACModuleXPosition[0] = -1.2*mm + fBeamPipeInputDisplacementWRTBeam[1] +
    ((fBeamPipeOutputDisplacementWRTBeam[1] - fBeamPipeInputDisplacementWRTBeam[1]))*
    ((fHACDetectorZPosition+fHACDetectorZLength/2.) - (fBeamPipeZPosition[1]-fBeamPipeZLength[1]/2.+fHACRespRegionZCenter))/
    fBeamPipeZLength[1]-
    fBeamPipeOuterRadius[1]-
    fHACModuleXLength/2.*(1./sqr(1-fHACDetectorYRotation*fHACDetectorYRotation));
  fHACModuleXPosition[1] = fHACModuleXPosition[0] - fHACModuleXLength*(1./sqr(1-fHACDetectorYRotation*fHACDetectorYRotation));
  fHACModuleXPosition[2] = fHACModuleXPosition[1] - fHACModuleXLength*(1./sqr(1-fHACDetectorYRotation*fHACDetectorYRotation));
  fHACModuleXPosition[3] = fHACModuleXPosition[0] +
    fBeamPipeOuterRadius[1] - sqrt(fBeamPipeOuterRadius[1]*fBeamPipeOuterRadius[1]-fHACModuleXLength*(1./sqr(1-fHACDetectorYRotation*fHACDetectorYRotation))*fHACModuleXLength/4.);
  fHACModuleXPosition[4] = fHACModuleXPosition[3] - fHACModuleXLength*(1./sqr(1-fHACDetectorYRotation*fHACDetectorYRotation));
  fHACModuleXPosition[5] = fHACModuleXPosition[4] - fHACModuleXLength*(1./sqr(1-fHACDetectorYRotation*fHACDetectorYRotation));

  fHACModuleXPosition[6] = fHACModuleXPosition[3];
  fHACModuleXPosition[7] = fHACModuleXPosition[4];
  fHACModuleXPosition[8] = fHACModuleXPosition[5];

  fHACModuleYPosition[0] = 0;
  fHACModuleYPosition[1] = 0;
  fHACModuleYPosition[2] = 0;
  fHACModuleYPosition[3] = fHACModuleYLength;
  fHACModuleYPosition[4] = fHACModuleYLength;
  fHACModuleYPosition[5] = fHACModuleYLength;
  fHACModuleYPosition[6] = -fHACModuleYLength;
  fHACModuleYPosition[7] = -fHACModuleYLength;
  fHACModuleYPosition[8] = -fHACModuleYLength;

  fResponsibilityRegion.push_back(new ResponsibilityRegion(fHACRespRegionZStart,fHACRespRegionZEnd));
}

HACGeometryParameters::~HACGeometryParameters(){}

HACGeometryParameters* HACGeometryParameters::GetInstance()
{
  if ( fInstance == 0 ) { fInstance = new HACGeometryParameters(); }
  return fInstance;
}

TObjArray HACGeometryParameters::GetHashTable()
{
  TObjArray HACGeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fHACDetectorZPosition;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fHACDetectorZPosition));
  HACGeometryParameters.Add(new DetectorParameter("fHACDetectorZPosition",Value.Data(),
					       "HAC V Detector Z Position", ParameterData));
  ParameterData.Clear();

  Buffer << fHACDetectorZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fHACDetectorZLength));
  HACGeometryParameters.Add(new DetectorParameter("fHACDetectorSize",Value.Data(),
					       "HAC Detector Half Size", ParameterData));
  return HACGeometryParameters;
}

void HACGeometryParameters::Print(){
}
