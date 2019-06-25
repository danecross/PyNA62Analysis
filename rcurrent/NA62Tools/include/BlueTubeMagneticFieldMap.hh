// ------------------------------------------------------------------
// History:
//
// Created by Anne Chappuis (anne.chappuis9@gmail.com) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2015-05-18
//
// ------------------------------------------------------------------

#ifndef BlueTubeMagneticFieldMap_h
#define BlueTubeMagneticFieldMap_h 1

#include "TObject.h"
class TVector3;

class BlueTubeMagneticFieldMap {

public:

  BlueTubeMagneticFieldMap(Double_t Zmin, Double_t Zmax);
  ~BlueTubeMagneticFieldMap() {}

  TVector3 GetField(Double_t, Double_t, Double_t);
  void PrintFieldValue(Double_t, Double_t, Double_t);
  void PrintFieldValue(TVector3);
  void PrintFieldIntegrals();

  Int_t    GetNPlanes()                         { return fNPlanes;                 }
  Double_t GetZmin()                            { return fZmin;                    }
  Double_t GetZmax()                            { return fZmax;                    }
  Double_t GetZ(Int_t i)                        { return fZ[i];                    }
  Double_t GetBxParam(Int_t i, Int_t j)         { return fBx_param[i][j];          }
  Double_t GetByParam(Int_t i, Int_t j)         { return fBy_param[i][j];          }
  Double_t GetBxParamGradient(Int_t i, Int_t j) { return fBx_param_gradient[i][j]; }
  Double_t GetByParamGradient(Int_t i, Int_t j) { return fBy_param_gradient[i][j]; }
  Double_t GetBxParamIntegral(Int_t i, Int_t j) { return fBx_param_integral[i][j]; }
  Double_t GetByParamIntegral(Int_t i, Int_t j) { return fBy_param_integral[i][j]; }

private:

  Int_t    fNPlanes;
  Double_t fZmin, fZmax;
  Double_t fZ[40], fBx_param[6][40], fBy_param[4][40], fBz_param0[40], fBz_param[2];
  Double_t fBx_param_gradient[6][40], fBy_param_gradient[4][40], fBz_param0_gradient[40];
  Double_t fBx_param_integral[6][40], fBy_param_integral[4][40], fBz_param0_integral[40];
};

#endif
