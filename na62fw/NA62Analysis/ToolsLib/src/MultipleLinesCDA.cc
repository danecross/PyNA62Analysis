// ---------------------------------------------------------------
// History:
//
// Created by Plamen Petrov (plpetrov@cern.ch) 2015-04-20
//  
// ---------------------------------------------------------------

#include "MultipleLinesCDA.hh"
#include "TwoLinesCDA.hh"
#include "TMath.h"
#include <iostream>

using namespace std;

MultipleLinesCDA::MultipleLinesCDA() {
  fLineP1.SetClass("TClonesArray", 10);
  fDir.SetClass("TClonesArray", 10);
  fVertex = TVector3(0,0,0);
  fNLines = 0;
}

MultipleLinesCDA::~MultipleLinesCDA() {
  fLineP1.Clear("C");
  fDir.Clear("C");
}

void MultipleLinesCDA::Reset() {
  fLineP1.Clear();
  fDir.Clear();
  fNLines = 0;
}

void MultipleLinesCDA::AddLinePoint1Dir (TVector3 p1, TVector3 dir) {
  new(fLineP1[fNLines]) TVector3(p1);
  new(fDir[fNLines]) TVector3(dir);  
  fNLines++;
}

void MultipleLinesCDA::RemoveLine(Int_t n) {
  if (n<fNLines && fNLines>0) {
    fLineP1.RemoveAt(n);
    fDir.RemoveAt(n);
    fLineP1.Compress();
    fDir.Compress();
    fNLines = fNLines-1;
  } else return;
}

void MultipleLinesCDA::SetLinePoint1 (Int_t n, Double_t x, Double_t y, Double_t z) {
  if (n < fNLines) {
    TVector3& p1 = *(TVector3*)fLineP1[n];
    p1.SetXYZ(x, y, z);
  } 
}

void MultipleLinesCDA::SetLineDir (Int_t n, Double_t x, Double_t y, Double_t z) {
  if (n < fNLines) {
    TVector3& dir = *(TVector3*)fDir[n];
    dir.SetXYZ(x, y, z);
  }
}

TVector3 MultipleLinesCDA::GetLinePoint1 (Int_t n) {
  TVector3 p1;
  if (n < fNLines) {
    p1 = *(TVector3*)fLineP1[n];
  }
  return p1;
}

TVector3 MultipleLinesCDA::GetLineDir (Int_t n) {
  TVector3 dir;
  if (n < fNLines) {
    dir = *(TVector3*)fDir[n];
  }
  return dir;
}

Double_t MultipleLinesCDA::GetCDA(Int_t n1, Int_t n2) {
  TwoLinesCDA cda;
  if (n1 < fNLines) {
    TVector3* p1 = (TVector3*)fLineP1[n1];
    TVector3* dir = (TVector3*)fDir[n1];
    cda.SetLine1PointDir(*p1, *dir);
  } else return -999;
  if (n2 < fNLines) {
    TVector3* p1 = (TVector3*)fLineP1[n2];
    TVector3* dir = (TVector3*)fDir[n2];
    cda.SetLine2PointDir(*p1, *dir);
  } else return -999;
  cda.ComputeVertexCDA();
  return cda.GetCDA();
}

void MultipleLinesCDA::ComputeVertex() {
  // Based on TwoLinesCDA
  Double_t c = 0.5 * TMath::Factorial(fNLines) / TMath::Factorial(fNLines-2);
  Double_t x=0.0, y=0.0, z=0.0;
  TwoLinesCDA twolines;
  for (Int_t i=0; i<fNLines; i++) {
    for (Int_t j=i+1; j<fNLines; j++) {
      TVector3* l1p1  = (TVector3*)fLineP1[i];
      TVector3* l1dir = (TVector3*)fDir[i];
      TVector3* l2p1  = (TVector3*)fLineP1[j];
      TVector3* l2dir = (TVector3*)fDir[j];
      twolines.SetLine1PointDir(*l1p1, *l1dir);
      twolines.SetLine2PointDir(*l2p1, *l2dir);
      twolines.ComputeVertexCDA();
      TVector3 vertex = twolines.GetVertex();
      x += vertex.X();
      y += vertex.Y();
      z += vertex.Z();
    }
  }
  fVertex.SetXYZ(x/c, y/c, z/c);
}

void MultipleLinesCDA::Print() {
  if (!fNLines) return;
  for (Int_t i=0; i<fNLines; i++) {
    TVector3* p1 = (TVector3*)fLineP1[i];
    TVector3* dir = (TVector3*)fDir[i];
    cout << "Line " << i <<"  Point 1   = "<<p1->X()<<" "<<p1->Y()<<" "<<p1->Z()<<endl;
    cout << "Line " << i <<"  Direction = "<<dir->X()<<" "<<dir->Y()<<" "<<dir->Z()<<endl;
  }
  cout << "Vertex     = "<<fVertex.X()<<" "<<fVertex.Y()<<" "<<fVertex.Z()<<endl;
}
