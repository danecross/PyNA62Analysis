// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-04-16
//
// ---------------------------------------------------------------

#ifndef TwoLinesCDA_H
#define TwoLinesCDA_H

#include "TVector3.h"
#include <iostream>

class TwoLinesCDA {

public:

  TwoLinesCDA();
  ~TwoLinesCDA() {}

  void ComputeVertexCDA();
  void Print();

  void SetLine1Point1Point2(TVector3 val1, TVector3 val2)
  { fL1p1 = val1; fL1p2 = val2; fv1 = fL1p2-fL1p1; }
  void SetLine2Point1Point2(TVector3 val1, TVector3 val2)
  { fL2p1 = val1; fL2p2 = val2; fv2 = fL2p2-fL2p1; }

  void SetLine1PointDir(TVector3 val1, TVector3 val2)
  { fL1p1 = val1; fv1 = val2; fL1p2 = fL1p1 + fv1; }
  void SetLine2PointDir(TVector3 val1, TVector3 val2)
  { fL2p1 = val1; fv2 = val2; fL2p2 = fL2p1 + fv2; }

  TVector3 GetLine1Point1() { return fL1p1; }
  TVector3 GetLine1Point2() { return fL1p2; }
  TVector3 GetLine2Point1() { return fL2p1; }
  TVector3 GetLine2Point2() { return fL2p2; }
  TVector3 GetDir1()        { return fv1;   }
  TVector3 GetDir2()        { return fv2;   }

  TVector3 GetVertex()             { return fVertex; }
  void     SetVertex(TVector3 val) { fVertex = val;  }
  Double_t GetCDA()                { return fCDA;    }
  void     SetCDA(Double_t val)    { fCDA = val;     }

private:

  TVector3 fL1p1, fL1p2, fL2p1, fL2p2, fv1, fv2;
  TVector3 fVertex;
  Double_t fCDA;
};

#endif
