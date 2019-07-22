// --------------------------------------------------------------------
// History:
//
// 2014-03-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Updated to be consistent with the NewCHOD simulation
//
// Created by Giuseppe Ruggiero 04-09-2012 
//
// --------------------------------------------------------------------

#ifndef MUV0Detector_H
#define MUV0Detector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "MUV0GeometryParameters.hh"
#include "MUV0MaterialParameters.hh"
#include "MUV0ScintillatorCounter.hh"
#include "MUV0SD.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class MUV0Detector : public NA62VComponent, public NA62VNamed {

public:

  MUV0Detector(G4Material*, G4LogicalVolume*);
  ~MUV0Detector() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:

  G4double fRespRegionXCentre;
  G4double fRespRegionZCentre;
  G4double fRespRegionXLength;
  G4double fRespRegionYLength;
  G4double fRespRegionZLength;
  G4int    fNCounters;
  G4double fZPosition;
  G4double fScintillatorThickness;
  G4double fFrameInnerSize; ///< Half-size of the inner size of the square frame
  G4double fFrameThickness; ///< Side of the frame square cross-section
  G4double fCoverThickness; ///< Aluminium cover sheet thickness along the beam

  G4ThreeVector fScintillatorSize[9];
  G4ThreeVector fScintillatorPosition[9];
};

#endif
