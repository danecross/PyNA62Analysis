// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2018-01-25
// ---------------------------------------------------------------
#ifndef COULOMBCORRECTION_H
#define COULOMBCORRECTION_H

#include "Event.hh"
#include "GenePart.hh"

class CoulombCorrection {
  
public:
  static CoulombCorrection* GetInstance();

  Double_t GetCorrection(Event* event);
  Double_t GetCorrection(GenePart** p);
  Double_t GetCorrection(std::vector<GenePart*> p);
  Double_t GetCorrection(GenePart* p1, GenePart* p2, GenePart* p3);
  Double_t GetCorrection(TLorentzVector p1, TLorentzVector p2, TLorentzVector p3, Int_t q1, Int_t q2, Int_t q3);

  Double_t GetRelativeCorrection(Event* event);
  Double_t GetRelativeCorrection(GenePart** p);
  Double_t GetRelativeCorrection(std::vector<GenePart*> p);
  Double_t GetRelativeCorrection(GenePart* p1, GenePart* p2, GenePart* p3);
  Double_t GetRelativeCorrection(TLorentzVector p1, TLorentzVector p2, TLorentzVector p3, Int_t q1, Int_t q2, Int_t q3);

private:
  CoulombCorrection();
  ~CoulombCorrection();

  Double_t ComputeCorrection(TLorentzVector v1, TLorentzVector v2, TLorentzVector v3, Int_t q1, Int_t q2, Int_t q3, Bool_t mode);
  Double_t Aij(Double_t qi, Double_t qj, Double_t alpha, Double_t Bij);
  Double_t Bij(Double_t msij, Double_t msi, Double_t msj);
  Double_t Cij(Double_t aij);

  Double_t fFSC; /// Fine structure constant
  Double_t fALP; /// Value of alpha in the kch2pill generator
};

#endif
