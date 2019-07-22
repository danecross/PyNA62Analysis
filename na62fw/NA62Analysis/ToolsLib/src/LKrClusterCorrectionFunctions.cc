/*
 * LKrClusterCorrectionFunctions.cc
 *
 *  Created on: 9 Dec 2016
 *      Author: ncl
 */

#include "LKrClusterCorrectionFunctions.hh"
#include "TRecoLKrCandidate.hh"

namespace NA62Analysis {
namespace LKrClusterCorrectionFunctions {

// The run-independent large corrections to LKr cluster energies

Double_t CorrectedEnergy(const TRecoLKrCandidate* Lcand, Bool_t isMC) {

  /////////////////////////////////////////////
  // Energy scale and non-linearity corrections

  Double_t Energy = isMC ?
    CorrectedEnergyMC  (Lcand->GetNCells(), Lcand->GetClusterEnergy()) :
    CorrectedEnergyData(Lcand->GetNCells(), Lcand->GetClusterEnergy());

  /////////////////////////////////////////////////////////////////////////////////////////
  // Energy loss in the hole (common correction for data and MC).
  // This correction comes from the NA48 code:
  // /afs/cern.ch/na48/offline2/compact/compact-7.3/compact/rlib/anasrc/fuser_lkrcalcor98.F
  // See function RadCorr(E,r)

  Double_t Radius = sqrt(Lcand->GetClusterX()*Lcand->GetClusterX()+
			 Lcand->GetClusterY()*Lcand->GetClusterY());
  if (Radius>=140.0 && Radius<=185.0) Energy = Energy / (0.97249+0.00014692*Radius) * 0.9999;
  Double_t rcorr2 = 0.0;
  if      (Radius>=140.0 && Radius<180.0) rcorr2 =  0.00420 - 3.7e-5*Radius;
  else if (Radius>=180.0 && Radius<200.0) rcorr2 = -0.00211;
  else if (Radius>=200.0 && Radius<220.0) rcorr2 = -0.01694 + 7.769e-5*Radius;
  return Energy*(1.0-rcorr2);
}

Double_t CorrectedEnergyData(Int_t NCells, Double_t E0) {

  Double_t Energy = E0;

  ///////////////////////////
  // Non-linearity correction


  if (NCells>4) {
    Double_t correctedEnergy;

    // The baseline correction
    if      (E0<22000.0) correctedEnergy = E0 / (0.7666  +0.0573489*log(1e-3*E0));
    else if (E0<65000.0) correctedEnergy = E0 / (0.828962+0.0369797*log(1e-3*E0));
    else                 correctedEnergy = E0 / (0.828962+0.0369797*log(65.0));

    // Further run-dependent corrections
    Double_t e = 0.001*correctedEnergy; // MeV --> GeV
    if (e< 6.0) e =  6.0; // no energy-dependence below 6 GeV
    if (e>45.0) e = 45.0; // no energy-dependence above 45 GeV
    correctedEnergy *= (0.99 / (0.94 + 0.0037*e - 9.4e-05*e*e + 8.9e-07*e*e*e));
    Energy = correctedEnergy;
  }

  // Energy loss at low energy
  if (Energy<15000.0) Energy = (Energy+15.0)/15015.0*15000.0;

  // Global scale factor
  Energy *= 1.03;

  return Energy;
}

/////////////////////////////////////////////////////////////////////
// MC cluster energy correction evaluated from a
// simulation of GPS photons in LKr with energy in the range 1-80 GeV
// A.Shaikhiev (shaykhiev@inr.ru), Nov 2016

Double_t CorrectedEnergyMC(Int_t NCells, Double_t E0) {
  Double_t Energy = E0;
  if (NCells>4) {
    Double_t Scale = 24.12/(17.05 + sqrt(E0)) + 0.926;
    Energy = E0*Scale;
  }
  return Energy;
}

TRecoLKrCandidate* CreateCorrectedCandidate(const TRecoLKrCandidate *LCand, Bool_t isMC) {
  TRecoLKrCandidate* newCand = new TRecoLKrCandidate(*LCand);
  Double_t Energy = CorrectedEnergy(newCand, isMC);
  newCand->SetClusterEnergy(Energy);
  return newCand;
}

} /* namespace LKrClusterCorrectionFunctions */
} /* namespace NA62Analysis */
