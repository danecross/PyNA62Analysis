// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-04-16
//
// ---------------------------------------------------------------

/// \class TwoLinesCDA
/// \Brief
/// Computation of distance between two lines (useful for vertexing)
/// \EndBrief
/// \Detailed
/// Each of the two lines can be defined either by two points,
/// or by a point and a direction vector.
/// The direction vector can be of any (non-zero) magnitude.
/// The distance between the two lines and the point of closest approach are computed
/// by the ComputeVertexCDA() method.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "TwoLinesCDA.hh"

using namespace std;

TwoLinesCDA::TwoLinesCDA() :
  fL1p1(0.0,0.0,0.0), fL1p2(1.0,0.0,0.0), fL2p1(0.0,0.0,0.0), fL2p2(0.0,1.0,0.0),
  fv1(1.0,0.0,0.0), fv2(0.0,1.0,0.0), fVertex(0.0,0.0,0.0), fCDA(0.0) {}

void TwoLinesCDA::ComputeVertexCDA() {
  double A   = fv1*fv1;
  double B   = fv2*fv2;
  double C   = fv1*fv2;
  double det = C*C - A*B;
  if (det==0.0) { // the two lines are parallel
    fVertex = TVector3(0,0,0);
    fCDA = -999.0;
    return;
  }
  TVector3 R12 = fL1p1 - fL2p1;
  double D     = R12*fv1;
  double E     = R12*fv2;
  double T1    = (B*D - C*E) / det;
  double T2    = (C*D - A*E) / det;
  TVector3 Q1  = fL1p1 + T1*fv1;
  TVector3 Q2  = fL2p1 + T2*fv2;
  fVertex      = 0.5*(Q1+Q2);
  fCDA         = (Q1-Q2).Mag();
}

void TwoLinesCDA::Print() {
  cout << "Line 1 point 1   = "<<fL1p1.X()<<" "<<fL1p1.Y()<<" "<<fL1p1.Z()<<endl;
  cout << "Line 1 point 2   = "<<fL1p2.X()<<" "<<fL1p2.Y()<<" "<<fL1p2.Z()<<endl;
  cout << "Line 2 point 1   = "<<fL2p1.X()<<" "<<fL2p1.Y()<<" "<<fL2p1.Z()<<endl;
  cout << "Line 2 point 2   = "<<fL2p2.X()<<" "<<fL2p2.Y()<<" "<<fL2p2.Z()<<endl;
  cout << "Line 1 direction = "<<fv1.X()<<" "<<fv1.Y()<<" "<<fv1.Z()<<endl;
  cout << "Line 2 direction = "<<fv2.X()<<" "<<fv2.Y()<<" "<<fv2.Z()<<endl;
  cout << "Vertex (CDA)     = "<<fVertex.X()<<" "<<fVertex.Y()<<" "<<fVertex.Z()<<" ("<<fCDA<<")"<<endl;
}
