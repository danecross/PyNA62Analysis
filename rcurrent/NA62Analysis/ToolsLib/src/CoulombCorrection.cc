// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2018-01-25
// ---------------------------------------------------------------

/// \class CoulombCorrection
/// \Brief
/// Computation of coulomb corrections of Kaon decays to three particles.
/// \EndBrief
/// \Detailed
/// This tool computes coulomb corrections of Kaon decays to three particles.
/// The input to the tool is three GenePart pointers.
/// As an incorrect correction was applied in the kch2pill generator, see
/// NARKD-670, the ratio of the correct and incorrect values
/// can also be computed.
///
/// The tool can be used with individual GenePart pointers, a vector of
/// GenePart pointers, or an array of GenePart pointers. If the MC Event
/// is passed to the tool, GeneParts {0,1,2} in the event are used.
///
/// If a null pointer is passed to the tool the correction will be -1.
/// Users should check that the (relative) correction is greater than zero.
///
/// Example usage:
/// \code
/// #include "CoulombCorrection.hh"
/// ... 
/// CoulombCorrection* Corrector = CoulombCorrection::GetInstance();
/// Double_t CC = Corrector->GetCorrection(GetMCEvent());
/// Double_t CC = Corrector->GetCorrection(p1, p2, p3);
/// Double_t RelativeCC = Corrector->GetRelativeCorrection(p1, p2, p3);
/// \endcode 
/// 
/// \EndDetailed

#include <iostream>
#include "CoulombCorrection.hh"

static CoulombCorrection* fInstance = nullptr;

// function to return pointer to instance.
CoulombCorrection* CoulombCorrection::GetInstance(){
  if(fInstance==nullptr) fInstance = new CoulombCorrection();
  return fInstance;
}

// Default (private) constructor
CoulombCorrection::CoulombCorrection() : 
  fFSC(0.0072973525664), fALP(-20.6e-8)
{
}

// Absolute correction using event
Double_t CoulombCorrection::GetCorrection(Event* event){
  
  return GetCorrection(event->GetGenePart(0),
			   event->GetGenePart(1),
			   event->GetGenePart(2));
}

// Absolute correction using array
Double_t CoulombCorrection::GetCorrection(GenePart** p){
  if(!p[0]) return -1.0;
  if(!p[1]) return -1.0;
  if(!p[2]) return -1.0;

  return ComputeCorrection(p[0]->GetInitial4Momentum(), p[1]->GetInitial4Momentum(), p[2]->GetInitial4Momentum(), p[0]->GetCharge(), p[1]->GetCharge(), p[2]->GetCharge(), true);
}

// Absolute correction using vector
Double_t CoulombCorrection::GetCorrection(std::vector<GenePart*> p){
  if(p.size()!=3) return -1.0;
  return ComputeCorrection(p[0]->GetInitial4Momentum(), p[1]->GetInitial4Momentum(), p[2]->GetInitial4Momentum(), p[0]->GetCharge(), p[1]->GetCharge(), p[2]->GetCharge(), true);
}

// Absolute correction using three pointers
Double_t CoulombCorrection::GetCorrection(GenePart* p1, GenePart* p2, GenePart* p3){
  return ComputeCorrection(p1->GetInitial4Momentum(), p2->GetInitial4Momentum(), p3->GetInitial4Momentum(), p1->GetCharge(), p2->GetCharge(), p3->GetCharge(), true);
}

// Absolute correction using thre vectors and charges
Double_t CoulombCorrection::GetCorrection(TLorentzVector p1, TLorentzVector p2, TLorentzVector p3, Int_t q1, Int_t q2, Int_t q3){
  return ComputeCorrection(p1, p2, p3, q1, q2, q3, true);
}

// Absolute correction using event
Double_t CoulombCorrection::GetRelativeCorrection(Event* event){
  
  return GetRelativeCorrection(event->GetGenePart(0),
			   event->GetGenePart(1),
			   event->GetGenePart(2));
}

// Relative correction using array
Double_t CoulombCorrection::GetRelativeCorrection(GenePart** p){
  if(!p[0]) return -1.0;
  if(!p[1]) return -1.0;
  if(!p[2]) return -1.0;

  return ComputeCorrection(p[0]->GetInitial4Momentum(), p[1]->GetInitial4Momentum(), p[2]->GetInitial4Momentum(), p[0]->GetCharge(), p[1]->GetCharge(), p[2]->GetCharge(), false);
}

// Relative correction using vector
Double_t CoulombCorrection::GetRelativeCorrection(std::vector<GenePart*> p){
  if(p.size()!=3) return -1.0;
  return ComputeCorrection(p[0]->GetInitial4Momentum(), p[1]->GetInitial4Momentum(), p[2]->GetInitial4Momentum(), p[0]->GetCharge(), p[1]->GetCharge(), p[2]->GetCharge(), false);
}

// Relative correction using three pointers
Double_t CoulombCorrection::GetRelativeCorrection(GenePart* p1, GenePart* p2, GenePart* p3){
  return ComputeCorrection(p1->GetInitial4Momentum(), p2->GetInitial4Momentum(), p3->GetInitial4Momentum(), p1->GetCharge(), p2->GetCharge(), p3->GetCharge(), false);
}

// Relative correction using thre vectors and charges
Double_t CoulombCorrection::GetRelativeCorrection(TLorentzVector p1, TLorentzVector p2, TLorentzVector p3, Int_t q1, Int_t q2, Int_t q3){
  return ComputeCorrection(p1, p2, p3, q1, q2, q3, false);
}


//Computation of the (relative) coulomb correction
Double_t CoulombCorrection::ComputeCorrection(TLorentzVector v1, TLorentzVector v2, TLorentzVector v3, Int_t q1, Int_t q2, Int_t q3, Bool_t mode){
  // mass squared of the three particles
  Double_t ms1 = v1.M2();
  Double_t ms2 = v2.M2();
  Double_t ms3 = v3.M2();

  // mass squared of the three two-particle pairs
  Double_t ms12 = (v1+v2).M2();
  Double_t ms13 = (v1+v3).M2();
  Double_t ms23 = (v2+v3).M2();

  // Bij terms
  Double_t B12 = Bij(ms12, ms1, ms2);
  Double_t B13 = Bij(ms13, ms1, ms3);
  Double_t B23 = Bij(ms23, ms2, ms3);

  // Aij terms with proper value of Alpha
  Double_t A12 = Aij(q1, q2, fFSC, B12);
  Double_t A13 = Aij(q1, q3, fFSC, B13);
  Double_t A23 = Aij(q2, q3, fFSC, B23);

  // Compute proper correct value
  Double_t CC = Cij(A12)*Cij(A13)*Cij(A23);

  // In default mode, return proper correction
  if(mode) return CC;

  // Aij terms with wrong value of Alpha as used 
  // in kch2pill generator until NARKD-670
  Double_t W12 = Aij(q1, q2, fALP, B12);
  Double_t W13 = Aij(q1, q3, fALP, B13);
  Double_t W23 = Aij(q2, q3, fALP, B23);

  // Compute correction using wrong value of alpha
  Double_t WW = Cij(W12)*Cij(W13)*Cij(W23);

  // In this case, return relative correction
  // to the one already applied in the generator
  return CC/WW;
}

Double_t CoulombCorrection::Aij(Double_t qi, Double_t qj, Double_t alpha, Double_t bij){
  return (2.0*M_PI*alpha*qi*qj)/bij;
}

Double_t CoulombCorrection::Bij(Double_t msij, Double_t msi, Double_t msj){
  Double_t a = 4.0*msi*msj;
  Double_t b = msij - msi - msj;
  Double_t c = b*b;
  return sqrt(1.0-(a/c));
}

Double_t CoulombCorrection::Cij(Double_t aij){
  return aij/(expm1(aij));
}
