// --------------------------------------------------------------------
// History:
//
// 2014-03-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Updated to be consistent with the NewCHOD simulation
//
// Created by Giuseppe Ruggiero 04-09-2012
//
// --------------------------------------------------------------------

#ifndef MUV0GeometryParameters_H
#define MUV0GeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"
#include "NA62VGeometryParameters.hh"

class MUV0GeometryParameters : public NA62VGeometryParameters {

public:

  ~MUV0GeometryParameters() {}
  static MUV0GeometryParameters* GetInstance();
  TObjArray GetHashTable();

private:

  static MUV0GeometryParameters* fInstance;

protected:

  MUV0GeometryParameters();

public:

  G4double GetRespRegionZStart()                  { return fRespRegionZStart;      }
  G4double GetRespRegionZEnd()                    { return fRespRegionZEnd;        }
  G4double GetRespRegionZCentre()                 { return fRespRegionZCentre;     }
  G4double GetRespRegionZLength()                 { return fRespRegionZLength;     }
  G4double GetRespRegionXCentre()                 { return fRespRegionXCentre;     }
  G4double GetRespRegionXLength()                 { return fRespRegionXLength;     }
  G4double GetRespRegionYLength()                 { return fRespRegionYLength;     }

  G4double GetDetectorXPosition()                 { return fDetectorXPosition;     }
  void     SetDetectorXPosition(G4double val)     { fDetectorXPosition = val;      }
  G4double GetDetectorYPosition()                 { return fDetectorYPosition;     }
  void     SetDetectorYPosition(G4double val)     { fDetectorYPosition = val;      }
  G4double GetDetectorZPosition()                 { return fDetectorZPosition;     }
  void     SetDetectorZPosition(G4double val)     { fDetectorZPosition = val;      }
  G4int    GetNCounters()                         { return fNCounters;             }
  void     SetNCounters(G4int val)                { fNCounters = val;              }
  G4double GetScintillatorThickness()             { return fScintillatorThickness; }
  void     SetScintillatorThickness(G4double val) { fScintillatorThickness = val;  }
  G4double GetFrameInnerSize()                    { return fFrameInnerSize;        }
  void     SetFrameInnerSize(G4double val)        { fFrameInnerSize = val;         }
  G4double GetFrameThickness()                    { return fFrameThickness;        }
  void     SetFrameThickness(G4double val)        { fFrameThickness = val;         }
  G4double GetCoverThickness()                    { return fCoverThickness;        }
  void     SetCoverThickness(G4double val)        { fCoverThickness = val;         }

  G4ThreeVector GetScintillatorSize(G4int i)                        { return fScintillatorSize[i];     }
  void          SetScintillatorSize(G4int i, G4ThreeVector val)     { fScintillatorSize[i] = val;      }
  G4ThreeVector GetScintillatorPosition(G4int i)                    { return fScintillatorPosition[i]; }
  void          SetScintillatorPosition(G4int i, G4ThreeVector val) { fScintillatorPosition[i] = val;  }

private:

  G4double fRespRegionZStart;
  G4double fRespRegionZEnd;
  G4double fRespRegionZCentre;
  G4double fRespRegionZLength;
  G4double fRespRegionXCentre;
  G4double fRespRegionXLength;
  G4double fRespRegionYLength;

  G4double fScintillatorThickness;
  G4double fDetectorXPosition; ///< MUV0 centre x position in lab frame
  G4double fDetectorYPosition; ///< MUV0 centre y position in lab frame
  G4double fDetectorZPosition; ///< MUV0 centre z position in lab frame
  G4int    fNCounters;
  G4double fFrameInnerSize; ///< Inner size of the square frame
  G4double fFrameThickness; ///< Side of the frame square cross-section
  G4double fCoverThickness; ///< Aluminium cover sheet thickness along the beam

  G4ThreeVector fScintillatorPosition[9];
  G4ThreeVector fScintillatorSize[9];
};
#endif
