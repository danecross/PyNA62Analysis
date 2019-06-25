//-----------------------------------------------------------
// History:
//
// Created by Andrew Sturgess (axs@hep.ph.bham.ac.uk) 
//        
//-----------------------------------------------------------

#ifndef FringeMagneticFieldMap_h
#define FringeMagneticFieldMap_h 1

#include "TVector3.h"
#include "MNP33MagneticFieldMap.hh"

class FringeMagneticFieldMap {

public:

  FringeMagneticFieldMap(Double_t Zmin, Double_t Zmax, MNP33MagneticFieldMap *fMNP33);
  ~FringeMagneticFieldMap() {}
  Double_t GetIntegral(Int_t component, Int_t x, Int_t y, Bool_t UpOrDown);
  void ParameterArray(Double_t zz);
  TVector3 MNP33FirstBound(Double_t x, Double_t y);
  TVector3 MNP33SecondBound(Double_t x, Double_t y);
  TVector3 GetField(Double_t, Double_t, Double_t);
  void PrintFieldValue(Double_t, Double_t, Double_t);
  void PrintFieldValue(TVector3);
  void PrintFieldIntegrals();

  Int_t    GetNPlanes()                        { return fNPlanes;   }
  Double_t GetZmin()                           { return fZmin;      }
  Double_t GetZmax()                           { return fZmax;      }
  Double_t GetZ(Int_t i)                       { return fZ[i];      }
  void     SetFringeFieldScale(Double_t scale) { fSF = scale*0.897; }
  void     SetBlueTubeScale(Double_t scale)    { fBT = scale;       }
  Double_t GetFringeFieldScale()               { return fSF;        }
  Double_t GetBlueFieldScale()                 { return fBT;        }

private:

  Double_t fZmin, fZmax;
  Double_t fSF, fBT;
  Int_t    fNPlanes,fNGridPos;
  Double_t fZ[16];
  Double_t fPar[6][3];
  MNP33MagneticFieldMap* fMNP33;
  Double_t fZStartFringe, fZReflection;
  Double_t fZStartBuffer1, fZEndBuffer1;
  Double_t fZStartBuffer2, fZEndBuffer2;
  Double_t fZStartBuffer3, fZEndBuffer3;
  Double_t fBufferSize1, fBufferSize2, fBufferSize3;
};

#endif
