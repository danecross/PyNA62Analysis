// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2015-10-13
//
// ------------------------------------------------------------------

/// \class MagneticField
/// \Brief
/// Magnetic fields: blue tube, fringe and MNP33 fields
/// \EndBrief
/// \Detailed
/// This class is a link between Geant4 tracking and the field maps implemented in NA62Tools/include/*Field.hh.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "MagneticField.hh"
#include "G4SystemOfUnits.hh"
#include "TVector3.h"

static MagneticField* fInstance = nullptr;

///////////////////////////////////////////////
// Initialization of the field parameterization

MagneticField::MagneticField() {

  /////////////////////////
  // Blue tube field region

  fBlueTubeFieldZmin = 104.458*m; // start of LAV RR0
  fBlueTubeFieldZmax = 183.311*m; // end of LAV RR0

  /////////////////////////////////////////////////////////
  // Fringe field: first region spanning STRAW RR0, LAV RR1

  fFringeFieldZminR1 = 183.311*m;  // end of LAV RR0
  fFringeFieldZmaxR1 = 193.083*m;  // first plane where MNP33 field is measured

  //////////////////////////////////////////////////////////////
  // MNP33 magnetic field region: symmetric wrt the MNP33 centre

  fMNP33FieldZmin    = 193.083*m; // first plane where MNP33 field is measured
  fMNP33FieldZCentre = 196.995*m; // centre of the magnet
  fMNP33FieldZmax    = 200.907*m; // one-but-last plane where MNP33 field is measured

  ////////////////////////////////////////////////////////////////
  // Fringe field: second region spanning STRAW RR2, LAV RR2 & RR3

  fFringeFieldZminR2 = 200.907*m; // one-but-last plane where MNP33 field is measured
  fFringeFieldZmaxR2 = 212.000*m; // end of fringe field region 2

  // Fringe field map needs MNP33 map to estimate field in changeover buffer region
  fBlueTubeMap = new BlueTubeMagneticFieldMap(fBlueTubeFieldZmin/mm, fBlueTubeFieldZmax/mm);
  fMNP33Map    = new MNP33MagneticFieldMap();
  fFringeMap   = new FringeMagneticFieldMap(fFringeFieldZminR1/mm, fFringeFieldZmaxR2/mm, fMNP33Map);

  ///////////////////////////////////////////////////////////////////
  // Default parameter values: they are adjustable via the macro file

  fMNP33FieldMode     = kTRUE;
  fBlueTubeFieldScale = 1.0;
  fMNP33FieldScale    = 1.0;

  ////////////////////////////////////////////////////
  // Fringe field scale is set by the MNP33 scale
  // BUT, map features a const term, set by blue-tube

  fFringeMap->SetFringeFieldScale(fMNP33FieldScale);
  fFringeMap->SetBlueTubeScale(fBlueTubeFieldScale);
}

MagneticField::~MagneticField() {
  delete fBlueTubeMap;
  delete fFringeMap;
  delete fMNP33Map;
}

MagneticField* MagneticField::GetInstance() {
  if (!fInstance) fInstance = new MagneticField();
  return fInstance;
}

/////////////////////////////////////////////////////////////
// Evaluation of the field value: used by Geant4 for tracking

void MagneticField::GetFieldValue(const G4double Point[4], G4double *B) const {
  if (Point[2] >= fBlueTubeFieldZmin && Point[2] < fBlueTubeFieldZmax) { // Blue tube field
    TVector3 Field = fBlueTubeMap->GetField(Point[0]/mm, Point[1]/mm, Point[2]/mm);
    B[0] = fBlueTubeFieldScale * 1e-6*Field[0]*tesla; // [mkT] --> [T]
    B[1] = fBlueTubeFieldScale * 1e-6*Field[1]*tesla;
    B[2] = fBlueTubeFieldScale * 1e-6*Field[2]*tesla;
  }
  else if (Point[2] >= fFringeFieldZminR1 && Point[2] < fFringeFieldZmaxR1) { // Fringe field
    TVector3 Field = fFringeMap->GetField(Point[0]/mm, Point[1]/mm, Point[2]/mm);
    B[0] = 1e-6*Field[0]*tesla; // [mkT] --> [T]
    B[1] = 1e-6*Field[1]*tesla;
    B[2] = 1e-6*Field[2]*tesla;
  }
  else if (Point[2] >= fMNP33FieldZmin && Point[2] < fMNP33FieldZmax) { // MNP33 field
    TVector3 Field = fMNP33Map->GetField(Point[0]/mm, Point[1]/mm, Point[2]/mm);
    B[0] = fMNP33FieldScale*Field[0]*tesla;
    B[1] = fMNP33FieldScale*Field[1]*tesla;
    B[2] = fMNP33FieldScale*Field[2]*tesla;
  }
  else if (Point[2] >= fFringeFieldZminR2 && Point[2] < fFringeFieldZmaxR2) { // Fringe field
    TVector3 Field = fFringeMap->GetField(Point[0]/mm, Point[1]/mm, Point[2]/mm);
    B[0] = 1e-6*Field[0]*tesla; // [mkT] --> [T]
    B[1] = 1e-6*Field[1]*tesla;
    B[2] = 1e-6*Field[2]*tesla;
  }
  else {
    B[0] = B[1] = B[2] = 0.0;
  }
}
