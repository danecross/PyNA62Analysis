// ------------------------------------------------------------------
// History:
//
// Created by Simone Schuchmann 2019-07-03
//
// Class to read G4beamline magnet field maps. Use G4BLMagnetField
// class to get the properly transformed info for given magnet.
//
// ------------------------------------------------------------------

#ifndef G4BLMagneticFieldMap_h
#define G4BLMagneticFieldMap_h 1

#include "TVector3.h"
#include "TString.h"

// all default double in mm
class G4BLMagneticFieldMap {
  int  fnX = 2;
  int  fnY = 2;
  int  fnZ = 2;
  double  fdX = 10.0;
  double  fdY = 10.0;
  double  fdZ = 10.0;
  double fX0 = 0.0;
  double  fY0 = 0.0;
  double fZ0 = 0.0;
  double ftolerance = 0.01;
  float *fmapBx = 0;
  float *fmapBy = 0;
  float *fmapBz = 0;
  bool fextendX = false;
  bool fextendY = false;
  bool fextendZ = false;
  int fextendXbits = 0;
  int fextendYbits = 0;
  int fextendZbits = 0;

  double fcurrent = 1.0;
  double fnormB = 1.0;
  double ftime = 0;
  
  TString fname;

public:
  explicit G4BLMagneticFieldMap(TString Filename);
  virtual ~G4BLMagneticFieldMap();
  TVector3 GetFieldValue(double local[4],double _current=0.0);
  bool ReadFieldMap();
  virtual bool HasB() { return fmapBx!=0 || fmapBy!=0 || fmapBz!=0; }
  int Bits(TString s);
  TString Bits2str(int v);
  bool SetField(double X, double Y, double Z, double Bx,
                double By, double Bz,  int linenumber);
  TString GetFieldMapName() {return fname;}  
};

#endif
