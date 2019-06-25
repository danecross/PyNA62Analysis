#ifndef LKrGeometry_H
#define LKrGeometry_H 1

#include "TVector3.h"
#include "TROOT.h"
#include "Riostream.h"
#include <iostream>
#include <string> 

class LKrCommon;

class LKrGeometry
{

public:

  LKrGeometry();
  static LKrGeometry* GetInstance();

private:

  static LKrGeometry* fInstance;

private:

  void CreateGeometry();

public:
    Double_t GetXfrontReferenceCell() { return fXfrontReferenceCell; };
    Double_t GetThermalContractionConstantG10() { return fThermalContractionConstantG10; };
    Double_t GetHalfCellSizeAtFrontWall() { return fHalfCellSizeAtFrontWall; };
    Double_t GetLKrCellLength() { return fLKrCellLength; };

private:
    Double_t fXfrontReferenceCell;
    Double_t fThermalContractionConstantG10;
    Double_t fHalfCellSizeAtFrontWall;
    Double_t fLKrCellLength;
    LKrCommon* fCom;

};
#endif
