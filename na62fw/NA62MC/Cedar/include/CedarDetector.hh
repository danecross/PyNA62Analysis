// --------------------------------------------------------------
// History:
//
// 2011-08-09 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - sensitive detector added
//
// 2011-06-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - vessel optics
//
// 2009-11-16 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - basic passive Cedar geometry + two quadrupoles (QFS077,079)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// ---------------------------------------------------------------------

#ifndef CedarDetector_H
#define CedarDetector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "CedarGeometryParameters.hh"
#include "CedarVessel.hh"
#include "CedarManginMirror.hh"
#include "CedarChromaticCorrector.hh"
#include "CedarDiaphragm.hh"
#include "CedarCondenser.hh"
#include "CedarOctant.hh"
#include "CedarExitPipe.hh"
#include "CedarQFSMagnet.hh"
#include "CedarDetectorMessenger.hh"
#include "G4LogicalBorderSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class CedarDetectorMessenger;

class CedarDetector : public NA62VComponent, public NA62VNamed {

public:

  CedarDetector(G4Material*, G4LogicalVolume*);
  ~CedarDetector();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4double          GetXLength()               { return fXLength;   }
  void              SetXLength(G4double val)   { fXLength = val;    }
  G4double          GetYLength()               { return fYLength;   }
  void              SetYLength(G4double val)   { fYLength = val;    }
  G4double          GetZLength()               { return fZLength;   }
  void              SetZLength(G4double val)   { fZLength = val;    }
  G4double          GetZPosition()             { return fZPosition; }
  void              SetZPosition(G4double val) { fZPosition = val;  }

  G4ThreeVector     GetQFSMagnet077Position()                    { return fQFSMagnet077Position;        }
  void              SetQFSMagnet077Position(G4ThreeVector val)   { fQFSMagnet077Position = val;         }
  G4ThreeVector     GetQFSMagnet079Position()                    { return fQFSMagnet079Position;        }
  void              SetQFSMagnet079Position(G4ThreeVector val)   { fQFSMagnet079Position = val;         }
  G4double          GetQFSMagnet077NominalGradient()             { return fQFSMagnet077NominalGradient; }
  void              SetQFSMagnet077NominalGradient(G4double val) { fQFSMagnet077NominalGradient = val;  }
  G4double          GetQFSMagnet079NominalGradient()             { return fQFSMagnet079NominalGradient; }
  void              SetQFSMagnet079NominalGradient(G4double val) { fQFSMagnet079NominalGradient = val;  }

  CedarVessel*      GetVessel()                                  { return fVessel;                      }
  void              SetVessel(CedarVessel *val)                  { fVessel = val;                       }
  CedarQFSMagnet*   GetQFSMagnet077()                            { return fQFSMagnet077;                }
  void              SetQFSMagnet077(CedarQFSMagnet *val)         { fQFSMagnet077 = val;                 }
  CedarQFSMagnet*   GetQFSMagnet079()                            { return fQFSMagnet079;                }
  void              SetQFSMagnet079(CedarQFSMagnet *val)         { fQFSMagnet079 = val;                 }
  G4bool            GetCherenkovEffect()                         { return fCherenkovEffect;             }
  void              SetCherenkovEffect(const G4bool val)         { fCherenkovEffect = val;              }

private:

  G4double fXLength;
  G4double fYLength;
  G4double fZLength;
  G4double fZPosition;

  G4double fRotBoxXLength;
  G4double fRotBoxYLength;
  G4double fRotBoxZLength;
  G4ThreeVector fRotBoxPosition;
  G4ThreeVector fRotCentrePosition;
  G4double fRotAngleX;
  G4double fRotAngleY;

  G4ThreeVector fQFSMagnet077Position;
  G4ThreeVector fQFSMagnet079Position;
  G4double      fQFSMagnet077NominalGradient; ///< Nominal quad field gradient, corrected by a run-dependent factor
  G4double      fQFSMagnet079NominalGradient; ///< Nominal quad field gradient, corrected by a run-dependent factor

  CedarVessel             *fVessel;
  CedarManginMirror       *fManginMirror;
  CedarChromaticCorrector *fChromaticCorrector;
  CedarDiaphragm          *fDiaphragm;
  CedarCondenser          **fCondenser;
  CedarOctant             **fOctant;

  CedarQFSMagnet          *fQFSMagnet077;
  CedarQFSMagnet          *fQFSMagnet079;

  CedarDetectorMessenger  *fCedarMessenger;
  G4bool                  fCherenkovEffect;
};

#endif
