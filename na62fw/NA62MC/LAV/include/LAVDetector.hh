// LAVDetector.hh
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - First implementation of LAV geometry
// 2010-11-03 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Updated LAV stations positions
//   - Blocks geometry is now correct
//   - Added (part of) block support structure
// 2010-11-10 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added use of LAVDetectorMessenger class
//   - Allow enable/disable of single LAV station
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Allow en-/dis-able of layer, banana, vessel
//   - Allow selection of full optical block
//
// --------------------------------------------------------------

#ifndef LAVDetector_H
#define LAVDetector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "globals.hh"

#include "LAVGeometryParameters.hh"
#include "G4FieldManager.hh"
#include "MagneticField.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class LAVDetectorMessenger;

class LAVDetector : public NA62VComponent, public NA62VNamed {

public:

  ~LAVDetector();
  LAVDetector(G4Material*, G4LogicalVolume*);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

  void EnableStation(const G4int is)  { fStationEnabled[is]=1; };
  void DisableStation(const G4int is) { fStationEnabled[is]=0; };

  void EnableLayer(const G4int is, const G4int il)  { fLayerEnabled[is][il]=1; };
  void DisableLayer(const G4int is, const G4int il) { fLayerEnabled[is][il]=0; };

  void EnableBanana(const G4int is, const G4int il, const G4int ib)  { fBananaEnabled[is][il][ib]=1; };
  void DisableBanana(const G4int is, const G4int il, const G4int ib) { fBananaEnabled[is][il][ib]=0; };

  void EnableVessel(const G4int iv)  { fVesselEnabled[iv]=1; };
  void DisableVessel(const G4int iv) { fVesselEnabled[iv]=0; };

  void EnableAllStations();
  void DisableAllStations();
  void EnableAllLayers();
  void DisableAllLayers();
  void EnableAllBananas();
  void DisableAllBananas();
  void EnableAllVessels();
  void DisableAllVessels();

  void SetBlockSimulation(const G4String type) { fBlockSimulation = type; };

public:

  G4double GetXLength()                 { return fXLength;    };
  void     SetXLength(G4double value)   { fXLength = value;   };
  G4double GetYLength()                 { return fYLength;    };
  void     SetYLength(G4double value)   { fYLength = value;   };
  G4double GetZLength()                 { return fZLength;    };
  void     SetZLength(G4double value)   { fZLength = value;   };
  G4double GetZPosition()               { return fZPosition;  };
  void     SetZPosition(G4double value) { fZPosition = value; };

private:

  LAVDetectorMessenger* fLAVMessenger;

  G4double fXLength;
  G4double fYLength;
  G4double fZLength;
  G4double fZPosition;

  G4int fStationEnabled[NUMBER_OF_VETOES];
  G4int fLayerEnabled[NUMBER_OF_VETOES][NUMBER_OF_RINGS];
  G4int fBananaEnabled[NUMBER_OF_VETOES][NUMBER_OF_RINGS][NUMBER_OF_BANANAS];
  G4int fVesselEnabled[NUMBER_OF_VETOES];

  G4String fBlockSimulation;

  G4FieldManager* fFieldManager;
};

#endif
