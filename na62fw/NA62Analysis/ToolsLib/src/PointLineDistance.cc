// ---------------------------------------------------------------
// History:
//
// Created by Lorenza Iacobuzio (lorenza.iacobuzio@cern.ch) 2016-10-03
//
// ---------------------------------------------------------------

#include "PointLineDistance.hh"
#include <cmath>

using namespace std;

PointLineDistance::PointLineDistance() :
  fp1   (999.999,999.999,999.999),
  fp2   (999.999,999.999,999.999),
  fv    (999.999,999.999,999.999),
  fP    (999.999,999.999,999.999),
  fDist (999.999)
{
}

void PointLineDistance::SetLinePoint1 (Double_t x, Double_t y, Double_t z) {
  fp1.SetX(x);
  fp1.SetY(y);
  fp1.SetZ(z);
}

void PointLineDistance::SetLinePoint2 (Double_t x, Double_t y, Double_t z) {
  fp2.SetX(x);
  fp2.SetY(y);
  fp2.SetZ(z);
}

void PointLineDistance::SetLineDir (Double_t x, Double_t y, Double_t z) {
  fv.SetX(x);
  fv.SetY(y);
  fv.SetZ(z);
}

void PointLineDistance::SetPoint (Double_t x, Double_t y, Double_t z) {
  fP.SetX(x);
  fP.SetY(y);
  fP.SetZ(z);
}

void PointLineDistance::ComputeDistance() {
  if (fv.X() == 999.999 && fv.Y() == 999.999 && fv.Z() == 999.999) {}
  else
    SetLinePoint2(fv+fp1);
  TVector3 A = fP-fp1;
  TVector3 B = fp2-fp1;
  Double_t C = A*B;
  Double_t D = B*B;
  Double_t E = C/D;
  TVector3 F = E*B;
  TVector3 G = A-F;
  fDist = sqrt(G*G);
}

void PointLineDistance::Print() {
  cout << "Line point 1   = "<<fp1.X()<<" "<<fp1.Y()<<" "<<fp1.Z()<<endl;
  if (fv.X() == 999.999 && fv.Y() == 999.999 && fv.Z() == 999.999)
    cout << "Line point 2   = "<<fp2.X()<<" "<<fp2.Y()<<" "<<fp2.Z()<<endl;
  else
    cout << "Line 1 direction = "<<fv.X()<<" "<<fv.Y()<<" "<<fv.Z()<<endl;
  cout << "Point          = "<<fP.X()<<" "<<fP.Y()<<" "<<fP.Z()<<endl;
  cout << "Distance       = "<<fDist<<endl;
}
