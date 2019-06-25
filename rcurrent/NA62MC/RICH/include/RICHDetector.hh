// 2011-01-18 Monica Pepe
//   - Added use of RICHDetectorMessenger class
//   - Allow enable/disable of RICH Fast Simulation
//
// --------------------------------------------------------------
#ifndef RICHDetector_H
#define RICHDetector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "RICHGeometryParameters.hh"
#include "globals.hh"

#include "RICHMirrorWindow.hh"

#include "RICHVessel.hh"
#include "RICHRadiator.hh"
#include "RICHMirror.hh"
#include "RICHMirrorSupports.hh"
#include "RICHPMTsWindow.hh"
#include "RICHBeamWindow.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class RICHDetectorMessenger;

class RICHDetector : public NA62VComponent, public NA62VNamed
{

public:
  
  ~RICHDetector();
  RICHDetector(G4Material*, G4LogicalVolume*);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();



 private:


public:


  G4double             GetXLength()                                       { return fXLength;                      };
  void                 SetXLength(G4double value)                         { fXLength = value;                     };
  G4double             GetYLength()                                       { return fYLength;                      };
  void                 SetYLength(G4double value)                         { fYLength = value;                     };
  G4double             GetZLength()                                       { return fZLength;                      };
  void                 SetZLength(G4double value)                         { fZLength = value;                     };

  G4double             GetInputDisplacementWRTXaxis()                      { return fInputDisplacementWRTXaxis;     };
  void                 SetInputDisplacementWRTXaxis(G4double value)        { fInputDisplacementWRTXaxis = value;    };
  G4double             GetOutputDisplacementWRTXaxis()                     { return fOutputDisplacementWRTXaxis;    };
  void                 SetOutputDisplacementWRTXaxis(G4double value)       { fOutputDisplacementWRTXaxis = value;   };

  G4double             GetZPosition()                                     { return fZPosition;                    };
  void                 SetZPosition(G4double value)                       { fZPosition = value;                   };

  G4double             GetAngleWRTXaxis()                                  { return fAngleWRTXaxis;                 };
  void                 SetAngleWRTXaxis(G4double value)                    { fAngleWRTXaxis = value;                };

  G4int                GetNPMs()                                          { return fNPMs;                         };
  void                 SetNPMs(G4int value)                               { fNPMs = value;                        };
  G4int                GetNMirrors()                                      { return fNMirrors;                     };
  void                 SetNMirrors(G4int value)                           { fNMirrors = value;                    };

  RICHVessel *         GetVessel()                                        { return fVessel;                       };
  void                 SetVessel(RICHVessel * value)                      { fVessel = value;                      };

  RICHRadiator *       GetRadiator()                                      { return fRadiator;                     };
  void                 SetRadiator(RICHRadiator * value)                  { fRadiator = value;                    };

  RICHMirror *         GetMirror()                                        { return fMirror;                       };
  void                 SetMirror(RICHMirror * value)                      { fMirror = value;                      };

  RICHMirrorSupports*  GetMirrorSupports()                                { return fMirrorSupports;               };
  void                 SetMirrorSupports(RICHMirrorSupports * value)      { fMirrorSupports = value;              };

  RICHMirrorWindow *   GetMirrorWindow()                                  { return fMirrorWindow;                 };
  void                 SetMirrorWindow(RICHMirrorWindow * value)          { fMirrorWindow = value;                };

  RICHPMTsWindow *     GetPMTsWindow()                                    { return fPMTsWindow;                   };
  void                 SetPMTsWindow(RICHPMTsWindow * value)              { fPMTsWindow = value;                  };
 
  RICHBeamWindow *     GetBeamWindow()                                    { return fBeamWindow;                   };
  void                 SetBeamWindow(RICHBeamWindow * value)              { fBeamWindow = value;                  };

  void ResetNeonRefractiveIndex();
 
private:

  G4double fXLength;
  G4double fYLength;
  G4double fZLength;

  G4double  fInputDisplacementWRTXaxis;
  G4double  fOutputDisplacementWRTXaxis;

  G4double  fZPosition;

  G4double  fAngleWRTXaxis;

  G4int fNPMs;
  G4int fNMirrors;

  RICHVessel* fVessel;

  RICHRadiator* fRadiator;
  RICHMirror*   fMirror;
  RICHMirrorSupports * fMirrorSupports;
  RICHMirrorWindow* fMirrorWindow;
  RICHPMTsWindow*    fPMTsWindow;
  RICHBeamWindow*   fBeamWindow;

  RICHDetectorMessenger* fRICHMessenger;
  G4String fFastSimulation;

};

#endif
