// LAVRecoHit.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - added method for residual slewing correction
// - added doxygen compliant documentation
// Created by T. Spadaro and E. Leonardi (tommaso.spadaro@cern.ch, emanuele.leonardi@cern.ch) 2015-01-22
// --------------------------------------------------------------
/// \class LAVRecoHit
/// \Brief
/// Class for LAV Reconstructed hits (i.e., the reconstructed hit information soft class living in the context of the reconstruction code)
/// \EndBrief
#include "TMath.h"
#include "LAVRecoHit.hh"

LAVRecoHit::LAVRecoHit(Int_t blockID) :
  fLowThreshold(5.E-3),
  fSigmaLowThreshold(0.2E-3),
  fHighThreshold(15.E-3),
  fSigmaHighThreshold(0.2E-3),
  fHysteresis(1.5E-3),
  fSigmaHysteresis(0.4*fHysteresis),
//  fRiseTime(6.7), // ns
  fRiseTime(6.1), // ns
  fSigmaRiseTime(0.2), // ns
  fTau(157e-3*50), // ns
  fSigmaTau(0.05*fTau),
  fSigmaTLow(0.1), // ns
  fSigmaTHigh(0.1), // ns

  fBlockID(blockID),
  fEdgeMask(0),

  fDigiLeadingEdgeLow(nullptr),
  fDigiTrailingEdgeLow(nullptr),
  fDigiLeadingEdgeHigh(nullptr),
  fDigiTrailingEdgeHigh(nullptr),

  fLeadingEdgeLow(.0),
  fTrailingEdgeLow(.0),
  fLeadingEdgeHigh(.0),
  fTrailingEdgeHigh(.0)
{
  fTStartTOTLow[0] = -1;
  fTStartTOTHigh[0] = -1;
  fTStartSlope[0] = -1;

  fTMaxTOTLow[0] = -1;
  fTMaxTOTHigh[0] = -1;
  fTMaxSlope[0] = -1;

  fVMaxTOTLow[0] = -1;
  fVMaxTOTHigh[0] = -1;
  fVMaxSlope[0] = -1;
  fVMaxSlopeTrailingLow[0] = -1;
  fVMaxSlopeTrailingHigh[0] = -1;

  fChargeTOTLow[0] = -1;
  fChargeTOTHigh[0] = -1;
  fChargeSlope[0] = -1;
  fChargeSlopeTrailingLow[0] = -1;
  fChargeSlopeTrailingHigh[0] = -1;

}

Double_t* LAVRecoHit::GetTStartResults(){
  fTStartResults[0] = fTStartTOTLow[0];
  fTStartResults[1] = fTStartTOTLow[1];
  fTStartResults[2] = fTStartTOTHigh[0];
  fTStartResults[3] = fTStartTOTHigh[1];
  fTStartResults[4] = fTStartSlope[0];
  fTStartResults[5] = fTStartSlope[1];
  return fTStartResults;
}
Double_t* LAVRecoHit::GetTMaxResults(){
  fTMaxResults[0] = fTMaxTOTLow[0];
  fTMaxResults[1] = fTMaxTOTLow[1];
  fTMaxResults[2] = fTMaxTOTHigh[0];
  fTMaxResults[3] = fTMaxTOTHigh[1];
  fTMaxResults[4] = fTMaxSlope[0];
  fTMaxResults[5] = fTMaxSlope[1];
  return fTMaxResults;
}
Double_t* LAVRecoHit::GetVMaxResults(){
  fVMaxResults[0] = fVMaxTOTLow[0];
  fVMaxResults[1] = fVMaxTOTLow[1];
  fVMaxResults[2] = fVMaxTOTHigh[0];
  fVMaxResults[3] = fVMaxTOTHigh[1];
  fVMaxResults[4] = fVMaxSlope[0];
  fVMaxResults[5] = fVMaxSlope[1];
  fVMaxResults[6] = fVMaxSlopeTrailingLow[0];
  fVMaxResults[7] = fVMaxSlopeTrailingLow[1];
  fVMaxResults[8] = fVMaxSlopeTrailingHigh[0];
  fVMaxResults[9] = fVMaxSlopeTrailingHigh[1];
  return fVMaxResults;
}
Double_t* LAVRecoHit::GetChargeResults(){
  fChargeResults[0] = fChargeTOTLow[0];
  fChargeResults[1] = fChargeTOTLow[1];
  fChargeResults[2] = fChargeTOTHigh[0];
  fChargeResults[3] = fChargeTOTHigh[1];
  fChargeResults[4] = fChargeSlope[0];
  fChargeResults[5] = fChargeSlope[1];
  fChargeResults[6] = fChargeSlopeTrailingLow[0];
  fChargeResults[7] = fChargeSlopeTrailingLow[1];
  fChargeResults[8] = fChargeSlopeTrailingHigh[0];
  fChargeResults[9] = fChargeSlopeTrailingHigh[1];
  return fChargeResults;
}

Double_t* LAVRecoHit::VMaxFromTOT(Double_t leadingTime, Double_t trailingTime, Double_t threshold){

  Double_t* vmax = new Double_t[2];

  Double_t argLamb = fTau/fRiseTime*(threshold-fHysteresis)/threshold*TMath::Exp((fRiseTime-(trailingTime-leadingTime))/fTau);
  Double_t Lamb = Lambertian0(argLamb);

  vmax[0] = fRiseTime/fTau*threshold/Lamb;
  Double_t dArgdHyst = argLamb/(threshold-fHysteresis);
  vmax[1] = vmax[0]*dArgdHyst/(argLamb*(1.+Lamb))*fSigmaHysteresis; // error due to the hysteresis
  return vmax;
}

Double_t* LAVRecoHit::TMaxFromTOT(Double_t leadingTime, Double_t threshold, Double_t vmax, Double_t evmax ){
  Double_t* tmax = new Double_t[2];

  tmax[0] = leadingTime + fRiseTime*(1.-threshold/vmax); 
  tmax[1] = fRiseTime*threshold*evmax/(vmax*vmax); 
  return tmax;
}

Double_t* LAVRecoHit::ChargeFromVMax(Double_t vmax, Double_t evmax ){
  Double_t* charge = new Double_t[2];
  Double_t timeEff = fRiseTime*0.5 + fTau;
  Double_t etimeEff = TMath::Sqrt(0.25*fSigmaRiseTime*fSigmaRiseTime + fSigmaTau*fSigmaTau);
  charge[0] = vmax*timeEff/50.*1000.; // expressed in pC
  Double_t relError = evmax*evmax/vmax/vmax + etimeEff*etimeEff/timeEff/timeEff;
  charge[1] = charge[0]*TMath::Sqrt(relError);
//				    TMath::Power(evmax/vmax,2) +
//				    TMath::Power(etimeEff/timeEff,2)
//				    ); 
  return charge;
}

void LAVRecoHit::HitReconstruct(){

  if ((fEdgeMask & 1) && (fEdgeMask & 8)) { // TOT Low
    Double_t * vmax = LAVRecoHit::VMaxFromTOT(fLeadingEdgeLow,fTrailingEdgeLow,fLowThreshold);
    fVMaxTOTLow[0] = vmax[0];
    fVMaxTOTLow[1] = vmax[1];
    delete[] vmax;

    Double_t* tmax = LAVRecoHit::TMaxFromTOT(fLeadingEdgeLow,fLowThreshold,fVMaxTOTLow[0],fVMaxTOTLow[1]);
    fTMaxTOTLow[0] = tmax[0];
    fTMaxTOTLow[1] = tmax[1];
    fTStartTOTLow[0] = fTMaxTOTLow[0] - fRiseTime;
    fTStartTOTLow[1] = fTMaxTOTLow[1];
    delete[] tmax;
    
    Double_t* charge = LAVRecoHit::ChargeFromVMax(fVMaxTOTLow[0],fVMaxTOTLow[1]);
    fChargeTOTLow[0] = charge[0];
    fChargeTOTLow[1] = charge[1];
    delete[] charge;

  }

  if ((fEdgeMask & 2) && (fEdgeMask & 4)) { // TOT High
    Double_t* vmax = LAVRecoHit::VMaxFromTOT(fLeadingEdgeHigh,fTrailingEdgeHigh,fHighThreshold);
    fVMaxTOTHigh[0] = vmax[0];
    fVMaxTOTHigh[1] = vmax[1];
    delete[] vmax;

    Double_t* tmax = LAVRecoHit::TMaxFromTOT(fLeadingEdgeHigh,fHighThreshold,fVMaxTOTHigh[0],fVMaxTOTHigh[1]);
    fTMaxTOTHigh[0] = tmax[0];
    fTMaxTOTHigh[1] = tmax[1];
    delete[] tmax;

    fTStartTOTHigh[0] = fTMaxTOTHigh[0] - fRiseTime;
    fTStartTOTHigh[1] = fTMaxTOTHigh[1];

    Double_t* charge = LAVRecoHit::ChargeFromVMax(fVMaxTOTHigh[0],fVMaxTOTHigh[1]);
    fChargeTOTHigh[0] = charge[0];
    fChargeTOTHigh[1] = charge[1];
    delete[] charge;

  }

  if ((fEdgeMask & 1) && (fEdgeMask & 2)) { // Slope
    Double_t dTLead = fLeadingEdgeHigh-fLeadingEdgeLow;
    if (dTLead > 0.2) {
      Double_t dVLead = fHighThreshold-fLowThreshold;
      fTStartSlope[0] = fLeadingEdgeLow - fLowThreshold/dVLead*dTLead;
      fTStartSlope[1] = TMath::Sqrt(
	TMath::Power(fHighThreshold/dVLead*fSigmaTLow,2) +
	TMath::Power(fLowThreshold/dVLead*fSigmaTHigh,2) +
	TMath::Power(fHighThreshold*dTLead/(dVLead*dVLead)*fSigmaHighThreshold,2) +
	TMath::Power(fLowThreshold*dTLead/(dVLead*dVLead)*fSigmaLowThreshold,2));
      fTMaxSlope[0] = fTStartSlope[0] + fRiseTime;
      fTMaxSlope[1] = fTStartSlope[1];
      fVMaxSlope[0] = dVLead/dTLead*fRiseTime;
      Double_t sigmadv = TMath::Sqrt(fSigmaHighThreshold*fSigmaHighThreshold + fSigmaLowThreshold*fSigmaLowThreshold);
      Double_t sigmadt = TMath::Sqrt(fSigmaTHigh*fSigmaTHigh + fSigmaTLow*fSigmaTLow);
      fVMaxSlope[1] = fVMaxSlope[0]*TMath::Sqrt(
				  (sigmadv/dVLead)*(sigmadv/dVLead) +
				  (sigmadt/dTLead)*(sigmadt/dTLead) +
				  (fSigmaRiseTime/fRiseTime)*(fSigmaRiseTime/fRiseTime)
				  );
      Double_t* charge = LAVRecoHit::ChargeFromVMax(fVMaxSlope[0],fVMaxSlope[1]);
      fChargeSlope[0] = charge[0];
      fChargeSlope[1] = charge[1];
      delete[] charge;

    }
    else {
      fTStartSlope[0] = 0.5*(fLeadingEdgeLow+fLeadingEdgeHigh);
      fTStartSlope[1] = 0.5*fabs(fLeadingEdgeLow-fLeadingEdgeHigh);
      fTMaxSlope[0] = fTStartSlope[0] + fRiseTime;
      fTMaxSlope[1] = TMath::Sqrt(fTStartSlope[1]*fTStartSlope[1] + fSigmaRiseTime*fSigmaRiseTime);
    }

    if (fEdgeMask & 8) {
      fVMaxSlopeTrailingLow[0] = (fLowThreshold - fHysteresis)*TMath::Exp((fTrailingEdgeLow-fTMaxSlope[0])/fTau);
      fVMaxSlopeTrailingLow[1] = TMath::Sqrt(
					     TMath::Power(fVMaxSlopeTrailingLow[0]*fSigmaHysteresis/(fLeadingEdgeLow - fHysteresis),2) +
					     TMath::Power(fVMaxSlopeTrailingLow[0]*fTMaxSlope[1]/fTau,2));
      Double_t* charge = LAVRecoHit::ChargeFromVMax(fVMaxSlopeTrailingLow[0],fVMaxSlopeTrailingLow[1]);
      fChargeSlopeTrailingLow[0] = charge[0];
      fChargeSlopeTrailingLow[1] = charge[1];
      delete[] charge;

    }
    if (fEdgeMask & 4) {
      fVMaxSlopeTrailingHigh[0] = (fHighThreshold - fHysteresis)*TMath::Exp((fTrailingEdgeHigh-fTMaxSlope[0])/fTau);
      fVMaxSlopeTrailingHigh[1] = TMath::Sqrt(
					      TMath::Power(fVMaxSlopeTrailingHigh[0]*fSigmaHysteresis/(fLeadingEdgeHigh - fHysteresis),2) +
					      TMath::Power(fVMaxSlopeTrailingHigh[0]*fTMaxSlope[1]/fTau,2));

      Double_t* charge = LAVRecoHit::ChargeFromVMax(fVMaxSlopeTrailingHigh[0],fVMaxSlopeTrailingHigh[1]);
      fChargeSlopeTrailingHigh[0] = charge[0];
      fChargeSlopeTrailingHigh[1] = charge[1];
      delete[] charge;

    }
  }

}

Double_t LAVRecoHit::Lambertian0(Double_t x){
  if (x < 0) {
    std::cout << "LAVRecoHit >> Lambertian0 wrong input " << x << std::endl;
    return -1;
  }
  if (x == 0) return 0;
  // zeroeth order approximation N.G. de Bruijn, Asymptotic Methods in Analysis, New York, Dover (1981) 27.

  Double_t wn = WAsymptotic(x);  

  // Fritch's iteration F.N. Fritsch, R.E. Shafer, and W.P. Crowley, Algorithm 443: solution of the transcendental equation w ew = x, Commun. ACM 16 (1973) 123.

  Double_t zn = TMath::Log(x/wn) - wn;
  Double_t qn = 2*(1. + wn)*(1+wn + 2./3.*zn);
  Double_t epsilon = zn/(1. + wn)*(qn-zn)/(qn-2*zn);
  Double_t wres = wn*(1.+epsilon);
  return wres;
}

Double_t LAVRecoHit::WAsymptotic(Double_t x){
  if (x <= 0) {
    std::cout << "LAVRecoHit >> WAsymptotic wrong input " << x << std::endl;
    return -1;
  }
  Double_t asympt;

  Double_t coeffA[5];
  coeffA[0] = 1;
  coeffA[1] = 2.445053070726557;
  coeffA[2] = 1.343664225958226;
  coeffA[3] = 0.148440055397592;
  coeffA[4] = 0.000804750172913;
  Double_t coeffB[5];
  coeffB[0] = 1;
  coeffB[1] = 3.444708986486002;
  coeffB[2] = 3.292489857371952;
  coeffB[3] = 0.916460018803122;
  coeffB[4] = 0.0530686404483322;
  Double_t nume = 0;
  Double_t deno = 0;
  for (Int_t i=0; i<5; i++) {
    nume += coeffA[i]*TMath::Power(x,i);
    deno += coeffB[i]*TMath::Power(x,i);
  }
  asympt = x*nume/deno;

  return asympt;
}

Double_t LAVRecoHit::GetResidualSlewingCorrection(Int_t method, Double_t* slewParsLow, Int_t nSlewParsLow, Double_t* slewParsHigh, Int_t nSlewParsHigh){

// method 0 is the default: use low and high leading if possible; else, use low leading and trailing.
// method 1: use low and high leading if possible, return 0 in other cases;
// method 2: use low leading and trailing if possible, return 0 in other cases;

  if (method == 0) {
    if (fEdgeMask & 1 && fEdgeMask & 2) {
      Double_t dTLead = fLeadingEdgeHigh - fLeadingEdgeLow;
      Double_t dTLeadEff = dTLead;
      if (dTLead < 0.2) dTLeadEff = 0.2;
      if (dTLead > 10.) dTLeadEff = 10.;
      Double_t tRange = 4.;
      Double_t x = dTLeadEff/tRange-1.;  
      Double_t fitval = 0;
      for (Int_t i=0; i<nSlewParsHigh; i++) fitval += slewParsHigh[i]*TMath::Power(x,i);
      return fitval;
    }
    else if (fEdgeMask & 1 && fEdgeMask & 8) {
      Double_t ToTLow = fTrailingEdgeLow - fLeadingEdgeLow;
      Double_t ToTLowEff = ToTLow;
      if (ToTLow <10) ToTLowEff = 10.;
      if (ToTLow >48) ToTLowEff = 48;
      Double_t tRange = 20.;
      Double_t x = ToTLowEff/tRange-1.;
      Double_t fitval = 0;
      for (Int_t i=0; i<nSlewParsLow; i++) fitval += slewParsLow[i]*TMath::Power(x,i);
      return fitval;
    }
    else return 0;
  }
  else if (method == 1) {
    if (fEdgeMask & 1 && fEdgeMask & 2) {
      Double_t dTLead = fLeadingEdgeHigh - fLeadingEdgeLow;
      Double_t dTLeadEff = dTLead;
      if (dTLead < 0.2) dTLeadEff = 0.2;
      if (dTLead > 10.) dTLeadEff = 10.;
      Double_t tRange = 4.;
      Double_t x = dTLeadEff/tRange-1.;  
      Double_t fitval = 0;
      for (Int_t i=0; i<nSlewParsHigh; i++) fitval += slewParsHigh[i]*TMath::Power(x,i);
      return fitval;
    }
    else return 0;
  }
  else if (method == 2) {
    if (fEdgeMask & 1 && fEdgeMask & 8) {
      Double_t ToTLow = fTrailingEdgeLow - fLeadingEdgeLow;
      Double_t ToTLowEff = ToTLow;
      if (ToTLow <10) ToTLowEff = 10.;
      if (ToTLow >48) ToTLowEff = 48;
      Double_t tRange = 20.;
      Double_t x = ToTLowEff/tRange-1.;
      Double_t fitval = 0;
      for (Int_t i=0; i<nSlewParsLow; i++) fitval += slewParsLow[i]*TMath::Power(x,i);
      return fitval;
    }
    else return 0;
  }
  else return 0;
}



void LAVRecoHit::Print() {
  std::cout << "Summary for the BlockID " << fBlockID << " with mask " << fEdgeMask << " Edges:";
  if (fEdgeMask & 1) std::cout << " " << fLeadingEdgeLow;
  else std::cout << " --";

  if (fEdgeMask & 2) std::cout << " " <<  fLeadingEdgeHigh;
  else std::cout << " --";

  if (fEdgeMask & 4) std::cout << " " << fTrailingEdgeHigh;
  else std::cout << " --";

  if (fEdgeMask & 8) std::cout << " " << fTrailingEdgeLow;
  else std::cout << " --";

  std::cout << std::endl;
}
