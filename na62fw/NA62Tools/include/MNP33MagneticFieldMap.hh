// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2015-10-13
// Modified by Viacheslav Duk (Viacheslav.Duk@cern.ch)     07.05.2018
//
// ------------------------------------------------------------------

#ifndef MNP33MagneticFieldMap_h
#define MNP33MagneticFieldMap_h 1

#include "TObject.h"
#include "TH3D.h"

class TVector3;

class MNP33MagneticFieldMap {

public:

  MNP33MagneticFieldMap();
  ~MNP33MagneticFieldMap() {}

  TVector3 GetField(Double_t, Double_t, Double_t);
  Double_t GetIntegral(Int_t, Int_t, Int_t); ///< Compute integral(Bi*dz) for a fixed (x,y) in a bin centre

  void PrintFieldValue(Double_t, Double_t, Double_t);
  void PrintFieldValue(TVector3);
  void PrintFieldIntegrals(); ///< Print Bx, By and Bz integrals for a range of (x,y)

private:

  Int_t    fNPointsX, fNPointsY, fNPointsZ;
  Double_t fZmin, fZmax, fXmin, fXmax, fYmin, fYmax;
  Double_t fZref;
  Double_t fXstep, fYstep, fZstep;
  Double_t fEmpiricZ; // empiric parameter to optimize Z-bin search during the interpolation

  Double_t fXBinCentre[26], fYBinCentre[26];
  Double_t fZBinCentre[169], fZEdgeLow[169], fZEdgeHigh[169];
  Double_t fXEdge[27], fYEdge[27], fZEdge[170]; // bin boundaries used to book the 3D histograms
  Double_t fBx[26][26][169], fBy[26][26][169], fBz[26][26][169];
  Bool_t   fMeasurementExists[26][26];

  TH3D *fHBx, *fHBy, *fHBz;

  // Coefficients for the trilinear interpolation
  Double_t fRealZBinCentre[169];
  Double_t coeff0_x[26][26][169]; Double_t coeff0_y[26][26][169]; Double_t coeff0_z[26][26][169];
  Double_t coeff1_x[26][26][169]; Double_t coeff1_y[26][26][169]; Double_t coeff1_z[26][26][169];
  Double_t coeff2_x[26][26][169]; Double_t coeff2_y[26][26][169]; Double_t coeff2_z[26][26][169];
  Double_t coeff3_x[26][26][169]; Double_t coeff3_y[26][26][169]; Double_t coeff3_z[26][26][169];
  Double_t coeff4_x[26][26][169]; Double_t coeff4_y[26][26][169]; Double_t coeff4_z[26][26][169];
  Double_t coeff5_x[26][26][169]; Double_t coeff5_y[26][26][169]; Double_t coeff5_z[26][26][169];
  Double_t coeff6_x[26][26][169]; Double_t coeff6_y[26][26][169]; Double_t coeff6_z[26][26][169];
  Double_t coeff7_x[26][26][169]; Double_t coeff7_y[26][26][169]; Double_t coeff7_z[26][26][169];
};

#endif
