// ---------------------------------------------------------------
// History:
//
// Created by Lorenza Iacobuzio (lorenza.iacobuzio@cern.ch) 2016-10-03
//
// ---------------------------------------------------------------

#ifndef PointLineDistance_H
#define PointLineDistance_H

#include "TVector3.h"
#include <iostream>

/// \class PointLineDistance
/// \Brief
/// Computation of distance between a line and a point.
/// \EndBrief
/// \Detailed
/// The line can be defined either by two points or by a point and a direction.
/// The distance between the line and the point is computed by the ComputeDistance() method.
/// \EndDetailed

class PointLineDistance {

public:
  
  PointLineDistance();
  ~PointLineDistance() {}

  void ComputeDistance();
  void Print();
  
  void SetLinePoint1 (TVector3 val) { fp1 = val; }
  void SetLinePoint2 (TVector3 val) { fp2 = val; }
  void SetPoint      (TVector3 val) { fP = val;  }
  void SetLineDir    (TVector3 val) { fv = val;  }

  void SetLinePoint1 (Double_t x, Double_t y, Double_t z);
  void SetLinePoint2 (Double_t x, Double_t y, Double_t z);
  void SetPoint      (Double_t x, Double_t y, Double_t z);
  void SetLineDir    (Double_t x, Double_t y, Double_t z);

  TVector3 GetLinePoint1() { return fp1; }
  TVector3 GetLinePoint2() { return fp2; }
  TVector3 GetLineDir()    { return fv;  }
  TVector3 GetPoint  ()    { return fP;  }

  Double_t GetDistance()             { return fDist; }
  void     SetDistance(Double_t val) { fDist = val;  }

private:

  TVector3 fp1, fp2, fv, fP; // fp1, fp2 are points to parametrise the line. fv is the direction to parametrise the line. fP is the external point wrt which to compute the distance.
  Double_t fDist;
};

#endif
