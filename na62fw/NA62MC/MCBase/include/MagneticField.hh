// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2015-10-13
//
// - Added fringe field map (Andrew Sturgess, axs@hep.ph.bham.ac.uk)
//                                                         2015-12-01
// ------------------------------------------------------------------

#ifndef MagneticField_H
#define MagneticField_H 1

#include "G4MagneticField.hh"
#include "BlueTubeMagneticFieldMap.hh"
#include "FringeMagneticFieldMap.hh"
#include "MNP33MagneticFieldMap.hh"

class MagneticField : public G4MagneticField {

public:
  static   MagneticField* GetInstance();
  G4double GetBlueTubeFieldScale()             { return fBlueTubeFieldScale; }
  G4bool   GetMNP33FieldMode()                 { return fMNP33FieldMode;     }
  G4double GetMNP33FieldScale()                { return fMNP33FieldScale;    }
  void     SetBlueTubeFieldScale(G4double val) { fBlueTubeFieldScale = val;  }
  void     SetMNP33FieldMode(G4bool val)       { fMNP33FieldMode = val;      }
  void     SetMNP33FieldScale(G4double val)    { fMNP33FieldScale = val;     }

protected:
  MagneticField();
  ~MagneticField();
  void GetFieldValue(const G4double Point[4], G4double *Bfield) const;

private:
  G4double fBlueTubeFieldZmin, fBlueTubeFieldZmax;
  G4double fFringeFieldZminR1, fFringeFieldZmaxR1;
  G4double fMNP33FieldZmin, fMNP33FieldZCentre, fMNP33FieldZmax;
  G4double fFringeFieldZminR2, fFringeFieldZmaxR2;

  G4bool   fMNP33FieldMode; ///< True = detailed map, false = simple map
  G4double fBlueTubeFieldScale, fMNP33FieldScale;

  BlueTubeMagneticFieldMap *fBlueTubeMap;
  FringeMagneticFieldMap   *fFringeMap;
  MNP33MagneticFieldMap    *fMNP33Map;
};

#endif
