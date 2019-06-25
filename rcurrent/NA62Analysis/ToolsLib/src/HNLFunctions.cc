// ---------------------------------------------------------------
//
// History:
//
// Created by Lorenza Iacobuzio (lorenza.iacobuzio@cern.ch) February 2018
//
// ---------------------------------------------------------------
/// \class HNLFunctions
/// \Brief
/// Functions to compute quantities related to heavy neutral leptons. All BR formulae are taken from
/// Shaposhnikov's JHEP10(2007)015 and JHEP11(2013)101
/// \EndBrief

#include "HNLFunctions.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

// Masses

Double_t fMe        = 0.511;
Double_t fMmu       = 105.66;
Double_t fMtau      = 1776.82;
Double_t fMpi       = 139.57;
Double_t fMpi0      = 134.98;
Double_t fMrho      = 775.45;
Double_t fMrho0     = 775.49;
Double_t fMeta      = 547.86;
Double_t fMetaprime = 957.78;
Double_t fMD        = 1869.62;
Double_t fMDS       = 1968.47;
Double_t fMD0       = 1864.84;
Double_t fMK        = 493.68;
Double_t fMK0       = 497.65;
Double_t fMp        = 938.27;
Double_t fMKStar    = 891.76;
Double_t fMK0Star   = 895.55;

// Lifetimes

Double_t fDlife   = 1.04E-3;
Double_t fDSlife  = 5.E-4;
Double_t fD0life  = 4.1E-4;
Double_t ftaulife = 2.91E-4;

// Constants

Double_t fhc       = 197.327E-12; // MeV mm
Double_t fcLight   = 299.792; //mm/ns
Double_t fGF       = 1.166E-11; // MeV^-2
Double_t fPi       = 130.41;  // MeV
Double_t fRho      = 1.04E5; // MeV^2
Double_t fD        = 222.6;
Double_t fDS       = 280.1;
Double_t fK        = 159.8;
Double_t fEta      = 1.2*fPi;
Double_t fEtaprime = -0.45*fPi;
Double_t fsigmacc  = 2.3*75.; //mubarn at sqrt(s) = 82 GeV (400 GeV proton on Be(9) (mBe = 9*1 GeV), taken from Gaia's note
Double_t fDtoTauBR    = 1.2E-3;
Double_t fDStoTauBR   = 0.0555;
Double_t fSin2thetaW = 0.2223;

// CKM

Double_t fVcs = 0.9734;
Double_t fVcd = 0.2252;
Double_t fVud = 0.9743;
Double_t fVus = 0.2253;

// Form factors, pseudoscalar and vector mesons

Double_t fDK0   = 0.745; // f+
Double_t fDpi0  = 0.648;
Double_t fD0K   = 0.736;
Double_t fD0pi  = 0.637;
Double_t fgDK0  = -0.495; // f-
Double_t fgDpi0 = -0.435;
Double_t fgD0K  = fgDK0;
Double_t fgD0pi = fgDpi0;

Double_t fA0D  = 0.398;
Double_t fA1D  = 0.47;
Double_t fA2D  = -1.24;
Double_t fVD   = 0.66;
Double_t fA0D0 = 0.4;
Double_t fA1D0 = 0.47;
Double_t fA2D0 = -1.24;
Double_t fVD0  = 0.66;

// Fragmentation fractions

Double_t ffD  = 0.246;
Double_t ffD0 = 0.565;
Double_t ffDS = 0.08;

// NA62 parameters

Double_t fpMom                   = 400000.; // MeV
Double_t fBeA                    = 4;
Double_t fBeDensity              = 1.85; // g/cm3
Double_t fpBeLambda              = 421.; // mm
Double_t ftargetLength           = 400.; // mm
Double_t fCuA                    = 29;
Double_t fCuDensity              = 8.96; // g/cm3
Double_t fpCuLambda              = 153.; // mm
Double_t fTAXLength              = 1615.; // mm
Double_t fTAXDistance            = 23070.;
Double_t fbeamLength             = 102425.; // mm
Double_t fLFV                    = 77575.;
Double_t fLInitialFV             = 102425.;

// Other parameters

Double_t fDBeProdProb = 0.002129;
Double_t fDCuProdProb = 0.002596;
Double_t fDDecayProb  = 1.;
Double_t fUSquared;
Double_t fUeSquared;
Double_t fUmuSquared;
Double_t fUtauSquared;
Double_t fUeSquaredRatio;
Double_t fUmuSquaredRatio;
Double_t fUtauSquaredRatio;
Double_t fInitialUeSquaredRatio;
Double_t fInitialUmuSquaredRatio;
Double_t fInitialUtauSquaredRatio;

// HNL mass

Double_t ComputeHNLMass(KinePart* p) {

  Double_t MN = TMath::Sqrt(p->GetInitial4Momentum().E()*p->GetInitial4Momentum().E() - p->GetInitial4Momentum().P()*p->GetInitial4Momentum().P());

  return MN;
}

// Distance between two points

Double_t ComputeL(TVector3 p1, TVector3 p2, TVector3 mom1) {

  TVector3 r(p1.x() + mom1.Px()/mom1.Pz()*(p2.z()-p1.z()), p1.y() + mom1.Py()/mom1.Pz()*(p2.z()-p1.z()), p2.z());
  Double_t x = r.x()-p1.x();
  Double_t y = r.y()-p1.y();
  Double_t z = r.z()-p1.z();
  Double_t L = TMath::Sqrt(x*x + y*y + z*z);

  return L;
}

// Probability of HNL decaying in FV

Double_t ComputeNDecayProb(KinePart* p, Double_t tau, Double_t l) {

  Double_t NProb  = 1. - ComputeNReachProb(p, tau, l);

  return NProb;
}

// Probability of HNL reaching FV

Double_t ComputeNReachProb(KinePart* p, Double_t tau, Double_t l) {

  Double_t NProb = TMath::Exp(-l/(p->GetInitial4Momentum().Beta()*p->GetInitial4Momentum().Gamma()*fcLight*tau));

  return NProb;
}

Double_t ReweightZDecay(KinePart* p, Double_t tau) {

  Double_t Z = TMath::Exp(-(p->GetEndPos().Z()-fLInitialFV)/(p->GetInitial4Momentum().Beta()*p->GetInitial4Momentum().Gamma()*fcLight*tau));

  return Z;
}

// Phasespace for 2-body HNL production mode

Double_t PhaseSpace(Double_t Mass1, Double_t Mass2, Double_t Mass3) {

  Double_t phaseSpace = TMath::Power(Mass1*Mass1 - Mass2*Mass2 - Mass3*Mass3, 2) - 4.*Mass2*Mass2*Mass3*Mass3;

  return phaseSpace;
}

// Phasespace factor for 2-body HNL production mode

Double_t PhaseSpaceFactor(Double_t Mass1, Double_t Mass2, Double_t Mass3) {

  Double_t factor = 0.;
  Double_t phaseSpace = PhaseSpace(Mass1, Mass2, Mass3);

  if(phaseSpace > 0.) {
    factor = (Mass1*Mass1*(Mass2*Mass2 + Mass3*Mass3) - TMath::Power(Mass2*Mass2 - Mass3*Mass3,2))*TMath::Power(phaseSpace, 0.5)/(Mass3*Mass3*TMath::Power(Mass1*Mass1 - Mass3*Mass3, 2));
  }

  return factor;
}

// Total BR for 2-body HNL production mode

Double_t TwoBodyBR(Double_t Mass1, Double_t Mass2, Double_t Mass3, Int_t Dorigin, Bool_t noU) {

  Double_t brt = 0.;
  Double_t life = 0.;
  Double_t V = 0.;
  Double_t f = 0.;
  Double_t a, b, c, d;
  Double_t U2 = 0.;

  if (Mass1 == fMD) {
    Dorigin = 0;
  }
  else if (Mass1 == fMDS) {
    Dorigin = 1;
  }

  if (Mass1 >= (Mass2 + Mass3) && PhaseSpaceFactor(Mass1, Mass2, Mass3) > 0.) {
    if (Mass1 == fMD) {
      life = fDlife;
      V = fVcd;
      f = fD;
    }
    else if (Mass1 == fMDS) {
      life = fDSlife;
      V = fVcs;
      f = fDS;
    }
    else if (Mass1 == fMtau) {
      life = ftaulife;
      V = fVud;
      if (Mass3 == fMpi)
	f = fPi;
      else if (Mass3 == fMK)
	f = fK;
      else if (Mass3 == fMrho)
	f = fRho;
    }
    else {
      cout<<"[TwoBodyBR] Unknown mother hadron"<<endl;
      _exit(kWrongConfiguration);
    }

    if (Mass3 != fMe && Mass3 != fMmu && Mass3 != fMtau && Mass3 != fMpi && Mass3 != fMrho && Mass3 != fMK) {
      cout<<"[TwoBodyBR] Unknown 2-body decay"<<endl;
      _exit(kWrongConfiguration);
    }

    if (noU == true)
      U2 = 1.;
    else {
      if (Mass3 == fMe)
        U2 = fUeSquared;
      else if (Mass3 == fMmu)
        U2 = fUmuSquared;
      else if (Mass3 == fMtau)
        U2 = fUtauSquared;
      else if (Mass3 == fMpi || Mass3 == fMrho || Mass3 == fMK)
        U2 = fUtauSquared;
    }

    if (Mass1 != fMtau) { // D,DS->Nl
      a = U2*life*fGF*fGF*f*f*V*V*Mass1*Mass2*Mass2/(8.*TMath::Pi());
      b = 1. - Mass2*Mass2/(Mass1*Mass1) + 2.*Mass3*Mass3/(Mass1*Mass1);
      c = (1. - Mass3*Mass3/(Mass1*Mass1))*Mass3*Mass3/(Mass2*Mass2);
      d = TMath::Power(1. + Mass2*Mass2/(Mass1*Mass1) - Mass3*Mass3/(Mass1*Mass1), 2.) - 4.*Mass2*Mass2/(Mass1*Mass1);
      brt = a*(b+c)*TMath::Sqrt(d);
    }
    //else if (Mass1 == fMtau) { // D,DS->taunu; tau->NH (H = pi, rho)
    else {// No other possibility than Mass1==fMtau at the moment
      if ((Dorigin == 0 && PhaseSpaceFactor(fMD, fMtau, 0.) > 0.) || (Dorigin == 1 && PhaseSpaceFactor(fMDS, fMtau, 0.))) {
        if (Mass3 == fMpi || Mass3 == fMK) {
          a = U2*life*fGF*fGF*V*V*f*f*Mass1*Mass1*Mass1/(16.*TMath::Pi());
          b = TMath::Power(1. - Mass2*Mass2/(Mass1*Mass1), 2.) - (1. + Mass2*Mass2/(Mass1*Mass1))*Mass3*Mass3/(Mass1*Mass1);
          c = 1. - ((Mass3 - Mass2)*(Mass3 - Mass2)/(Mass1*Mass1));
          d = 1. - ((Mass3 + Mass2)*(Mass3 + Mass2)/(Mass1*Mass1));
          brt = a*b*TMath::Sqrt(c*d);
        }
        else if (Mass3 == fMrho) {
          a = U2*life*fRho*fRho*fGF*fGF*V*V*Mass1*Mass1*Mass1/(8.*TMath::Pi()*Mass3*Mass3);
          b = TMath::Power(1. - Mass2*Mass2/(Mass1*Mass1), 2.) + (1. + (Mass2*Mass2 - 2.*Mass3*Mass3)/(Mass1*Mass1))*Mass3*Mass3/(Mass1*Mass1);
          c = 1. - ((Mass3 - Mass2)*(Mass3 - Mass2)/(Mass1*Mass1));
          d = 1. - ((Mass3 + Mass2)*(Mass3 + Mass2)/(Mass1*Mass1));
          brt = a*b*TMath::Sqrt(c*d);
        }
      }
    }
  }

  return brt;
}

// Total BR for 3-body HNL production mode

Double_t ThreeBodyBR(Double_t Mass1, Double_t Mass2, Double_t Mass3, Double_t Mass4, Int_t Dorigin, Bool_t noU) {

  Double_t br = 0.;
  Double_t U2 = 0.;

  if (Mass1 == fMD) {
    Dorigin = 0;
  }
  else if (Mass1 == fMDS) {
    Dorigin = 1;
  }

  if (Mass1 >= (Mass2 + Mass3 + Mass4)) {
    if (Mass3 == fMK || Mass3 == fMK0 || Mass3 == fMpi || Mass3 == fMpi0) { // D,D0->NHl (H = pi, pi0, K, K0)
      if (Mass1 == fMD || Mass1 == fMD0) {
        Double_t ENmin = Mass2; // N at rest, K and e back to back
        Double_t ENmax = (Mass1*Mass1 + Mass2*Mass2 - TMath::Power(Mass4 + Mass3, 2.))/(2.*Mass1); // N one way,K and e other way, their momenta summed equal to the N one
        Double_t q2min = TMath::Power(Mass2 + Mass4, 2.); // sum of masses of lepton pair
        Double_t q2max = TMath::Power(Mass1 - Mass3, 2.); // sum of 4momenta of lepton pair, when K at rest and N and e back to back

        Double_t tau = 0.;
        Double_t V = 0.;
        Double_t f = 0.;
        Double_t a = 0.;
        Double_t b = 0.;
        Double_t g = 0.;

        if (noU == true)
          U2 = 1.;
        else {
          if (Mass4 == fMe)
            U2 = fUeSquared;
          else if (Mass4 == fMmu)
            U2 = fUmuSquared;
          else if (Mass4 == fMtau)
            U2 = fUtauSquared;
        }

	if (Mass1 == fMD) {
          tau = fDlife;
          if (Mass3 == fMK0) {
            V = fVcs;
            f = fDK0;
            g = fgDK0;
          }
          else if (Mass3 == fMpi0) {
            V = fVcd;
            f = fDpi0;
            g = fgDpi0;
          }
          else {
            cout<<"[ThreeBodyBR] Unknown daughter hadron"<<endl;
            _exit(kWrongConfiguration);
          }
        }
        else if (Mass1 == fMD0) {
          tau = fD0life;
          if (Mass3 == fMK) {
            V = fVcs;
            f = fD0K;
            g = fgD0K;
          }
          else if (Mass3 == fMpi) {
            V = fVcd;
            f = fD0pi;
            g = fgD0pi;
          }
          else {
            cout<<"[ThreeBodyBR] Unknown daughter hadron"<<endl;
            _exit(kWrongConfiguration);
          }
        }

        a = U2*tau*V*V*fGF*fGF/(64.*TMath::Power(TMath::Pi(), 3.)*Mass1*Mass1);

	std::string function = ThreeBodyFunction(Mass1, Mass3);
        TF2* func = new TF2("func", function.c_str());

        func->SetParameter(0, f);
	func->SetParameter(1, Mass1);
        func->SetParameter(2, Mass2);
        func->SetParameter(3, Mass3);
        func->SetParameter(4, Mass4);
        func->SetParameter(5, g);
	ROOT::Math::WrappedMultiTF1 wf1(*func, 2);
	ROOT::Math::AdaptiveIntegratorMultiDim ig;
        ig.SetFunction(wf1);
	ig.SetRelTolerance(0.001);
        double xmin[] = {q2min, ENmin};
        double xmax[] = {q2max, ENmax};
        b = ig.Integral(xmin, xmax);
        br = a*b;

        delete func;
        func = nullptr;
      }
    }
    else if (Mass3 == fMKStar || Mass3 == fMK0Star) { // D,D0->NVl (V = K*, K0*)
      if (Mass1 == fMD || Mass1 == fMD0) {
        Double_t ENmin = Mass2; // N at rest, K and e back to back
        Double_t ENmax = (Mass1*Mass1 + Mass2*Mass2 - TMath::Power(Mass4 + Mass3, 2.))/(2.*Mass1); // N one way, K and e other way, their momenta summed equal to the N one
	Double_t q2min = TMath::Power(Mass2 + Mass4, 2.); // sum of masses of lepton pair
        Double_t q2max = TMath::Power(Mass1 - Mass3, 2.); // sum of 4momenta of lepton pair, when K at rest and N and e back to back

        Double_t tau = 0.;
        Double_t V = 0.;
        Double_t f1 = 0.;
        Double_t f2 = 0.;
        Double_t f3 = 0.;
        Double_t f4 = 0.;
        Double_t a = 0.;
	Double_t b = 0.;
        Double_t omega2 = 0.;
	Double_t Omega2 = 0.;

        if (noU == true)
          U2 = 1.;
        else {
          if (Mass4 == fMe)
            U2 = fUeSquared;
          else if (Mass4 == fMmu)
            U2 = fUmuSquared;
          else if (Mass4 == fMtau)
            U2 = fUtauSquared;
        }
        if (Mass1 == fMD) {
          tau = fDlife;
          V = fVcs;
          f1 = fVD/(Mass1 + Mass3);
          f2 = (Mass1 + Mass3)*fA1D;
          f3 = -fA2D/(Mass1 + Mass3);
          f4 = Mass3*(2.*fA0D - fA1D - fA2D) + Mass1*(fA2D - fA1D); // multiply by 1/x
        }
        else if (Mass1 == fMD0) {
          tau = fD0life;
          V = fVcs;
          f1 = fVD0/(Mass1 + Mass3);
          f2 = (Mass1 + Mass3)*fA1D0;
          f3 = -fA2D0/(Mass1 + Mass3);
          f4 = Mass3*(2.*fA0D0 - fA1D0 - fA2D0) + Mass1*(fA2D0 - fA1D0); // multiply by 1/x
        }

        omega2 = Mass1*Mass1 - Mass3*Mass3 + Mass2*Mass2 - Mass4*Mass4; // add - 2.*Mass1*y;
        Omega2 = Mass1*Mass1 - Mass3*Mass3; // add -x
        a = U2*tau*V*V*fGF*fGF/(32.*TMath::Power(TMath::Pi(), 3.)*Mass1*Mass1);

	std::string function = ThreeBodyFunction(Mass1, Mass3);
        TF2* func = new TF2("func", function.c_str());

        func->SetParameter(0, omega2);
        func->SetParameter(1, Omega2);
        func->SetParameter(2, Mass2);
        func->SetParameter(4, Mass4);
	func->SetParameter(3, Mass3);
        func->SetParameter(5, f1);
        func->SetParameter(6, f2);
        func->SetParameter(7, f3);
        func->SetParameter(8, f4);
        func->SetParameter(9, Mass1);

	ROOT::Math::WrappedMultiTF1 wf1(*func, 2);
	ROOT::Math::AdaptiveIntegratorMultiDim ig;
        ig.SetFunction(wf1);
        ig.SetRelTolerance(0.001);
	double xmin[] = {q2min, ENmin};
        double xmax[] = {q2max, ENmax};
        b = ig.Integral(xmin, xmax);
        br = a*b;

	delete func;
        func = nullptr;
      }
    }
    else if (Mass1 == fMtau) {
      if ((Dorigin == 0 && PhaseSpaceFactor(fMD, fMtau, 0.) > 0.) || (Dorigin == 1 && PhaseSpaceFactor(fMDS, fMtau, 0.))) {
        Double_t b;
        Double_t ENmin;
        Double_t ENmax;
        Double_t life = ftaulife;

	if (Mass3 == 0.1) { // D,DS->taunu_tau; tau->Nlnu_tau
	  std::string function = ThreeBodyFunction(Mass1, Mass3);

          if (noU == true)
            U2 = 1.;
          else {
            if (Mass4 == fMe)
              U2 = fUeSquared;
            else if (Mass4 == fMmu)
              U2 = fUmuSquared;
            else if (Mass4 == fMtau)
              U2 = fUtauSquared;
          }

          Mass3 = 0.;
          ENmin = Mass2; // N at rest, l and nu back to back
          ENmax = (Mass1*Mass1 + Mass2*Mass2 - TMath::Power(Mass4 + Mass3, 2.))/(2.*Mass1); // N one way, l and nu other way, their momenta summed equal to the N one

          TF1 func("func", function.c_str());

          func.SetParameter(0, life);
          func.SetParameter(1, Mass1);
          func.SetParameter(2, Mass2);
          func.SetParameter(3, fGF);
          func.SetParameter(4, Mass4);

	  ROOT::Math::WrappedTF1 wf1(func);
	  ROOT::Math::GaussLegendreIntegrator ig;
          ig.SetFunction(wf1);
          b = ig.Integral(ENmin, ENmax);
          br = U2*b;
        }
        else if (Mass3 == 0.01) { // D,DS->taunu_tau; tau->Nlnu_l
	  std::string function = ThreeBodyFunction(Mass1, Mass3);

          if (noU == true)
            U2 = 1.;
          else
            U2 = fUtauSquared;

          Mass3 = 0.;
          ENmin = Mass2; // N at rest, l and nu back to back
          ENmax = (Mass1*Mass1 + Mass2*Mass2 - TMath::Power(Mass4 + Mass3, 2.))/(2.*Mass1); // N one way, l and nu other way, their momenta summed equal to the N one

          TF1 func("func", function.c_str());

          func.SetParameter(0, life);
          func.SetParameter(1, Mass1);
          func.SetParameter(2, Mass2);
          func.SetParameter(3, fGF);
          func.SetParameter(4, Mass4);

	  ROOT::Math::WrappedTF1 wf1(func);
	  ROOT::Math::GaussLegendreIntegrator ig;
          ig.SetFunction(wf1);
          b = ig.Integral(ENmin, ENmax);
          br = U2*b;
        }
        else {
          cout<<"[ThreeBodyBR] Unknown neutrino type in N 3-body production mode"<<endl;
          _exit(kWrongConfiguration);
        }
      }
    }
    else {
      cout<<"[ThreeBodyBR] Unknown N 3-body production mode"<<endl;
      _exit(kWrongConfiguration);
    }
  }

  return br;
}

// Create string function for 3-body total BR of HNL production mode

std::string ThreeBodyFunction(Double_t Mass1, Double_t Mass3) {

  std::string function = "";

  if (Mass1 == fMD || Mass1 == fMD0) {
    if (Mass3 == fMK || Mass3 == fMK0 || Mass3 == fMpi || Mass3 == fMpi0) { // D,D0->NHl
      function = "([5]*[5]*(x*([2]*[2] + [4]*[4]) - TMath::Power([2]*[2] - [4]*[4], 2.)) + 2.*[5]*[0]*([2]*[2]*(2.*[1]*[1] - 2.*[3]*[3] -4.*y*[1] - [4]*[4] + [2]*[2] + x) + [4]*[4]*(4.*y*[1] + [4]*[4] - [2]*[2] - x)) + [0]*[0]*((4.*y*[1] + [4]*[4] - [2]*[2] - x)*(2.*[1]*[1] - 2.*[3]*[3] - 4.*y*[1] - [4]*[4] + [2]*[2] + x) + (2.*[1]*[1] + 2.*[3]*[3] - x)*(x - [2]*[2] - [4]*[4])))";
    }
    else if (Mass3 == fMKStar || Mass3 == fMK0Star) { // D,D0->NVl
      function = "(([6]*[6]/2.)*(x - [2]*[2] - [4]*[4] + ([0] - 2.*[9]*y)*([1] - x - ([0] - 2.*[9]*y))/([3]*[3])) + (([7]+[8]*1./x)*([7]+[8]*1./x)/2.)*([2]*[2] + [4]*[4])*(x - [2]*[2] + [4]*[4])*(([1] - x)*([1] - x)/(4.*[3]*[3]) - x) + 2.*[7]*[7]*[3]*[3]*(([1] - x)*([1] - x)/(4.*[3]*[3]) - x)*([2]*[2] + [4]*[4] - x + ([0] - 2.*[9]*y)*([1] - x - ([0] - 2.*[9]*y))/([3]*[3])) + 2.*[7]*([7]+[8]*1./x)*([2]*[2]*([0] - 2.*[9]*y) + ([1] - x - ([0] - 2.*[9]*y))*[4]*[4])*(([1] - x)*([1] - x)/(4.*[3]*[3]) - x) + 2.*[5]*[6]*(x*(2.*([0] - 2.*[9]*y) - [1] + x) + ([1] - x)*([2]*[2] - [4]*[4])) + ([6]*([7]+[8]*1./x)/2.)*(([0] - 2.*[9]*y)*([1] - x)/([3]*[3])*([2]*[2] - [4]*[4]) + ([1] - x)*([1] - x)*[4]*[4]/([3]*[3]) + 2.*TMath::Power([2]*[2] - [4]*[4], 2.) - 2.*x*([2]*[2] + [4]*[4])) + [6]*[7]*(([1] - x)*([0] - 2.*[9]*y)*([1] - x - ([0] - 2.*[9]*y))/([3]*[3]) + 2.*([0] - 2.*[9]*y)*([4]*[4] - [2]*[2]) + ([1] - x)*([2]*[2] - [4]*[4] - x)) + [5]*[5]*(([1] - x)*([1] - x)*(x - [2]*[2] + [4]*[4]) - 2.*[3]*[3]*(x*x - TMath::Power([2]*[2] - [4]*[4], 2.)) + 2.*([0] - 2.*[9]*y)*([1] - x)*([2]*[2] - x - [4]*[4]) + 2.*([0] - 2.*[9]*y)*([0] - 2.*[9]*y)*x))";
    }
  }
  else if (Mass1 == fMtau) { // D,DS->taunu_tau; tau->Nlnu
    if (Mass3 == 0.1) { //nu_tau
      function = "([0]*[3]*[3]*[1]*[1]*x/(2.*TMath::Power(TMath::Pi(), 3.)))*(1. + ([2]*[2] - [4]*[4])/([1]*[1]) - 2.*x/[1])*(1. - [4]*[4]/([1]*[1] + [2]*[2] - 2.*x*[1]))*(TMath::Sqrt(x*x - [2]*[2]))";
    }
    else if (Mass3 == 0.01) { //nu_e or nu_mu
      function = "([0]*[3]*[3]*[1]*[1]/(4.*TMath::Power(TMath::Pi(), 3.)))*(TMath::Power((1. - [4]*[4]/([1]*[1] + [2]*[2] - 2.*x*[1])), 2.)*TMath::Sqrt(x*x - [2]*[2]))*(([1] - x)*(1. - ([2]*[2] + [4]*[4])/([1]*[1])) - (1. - [4]*[4]/([1]*[1] + [2]*[2] - 2.*x*[1]))*(([1] - x)*([1] - x)/[1] + (x*x - [2]*[2])/(3.*[1])))";
    }
  }

  return function;
}


// Decay width for 2-body HNL decay mode

Double_t Gamma2(Double_t Mass1, Double_t Mass2, Double_t Mass3, Double_t form, Bool_t noU) {

  Double_t gamma_2 = 0.;
  Double_t V = 0.;
  Double_t a, b, c, d, f, g;
  Double_t U2 = 0.;

  if (noU == true)
    U2 = 1.;
  else {
    if (Mass3 == fMe)
      U2 = fUeSquared;
    else if (Mass3 == fMmu)
      U2 = fUmuSquared;
    else if (Mass3 == fMtau)
      U2 = fUtauSquared;
  }

  if (Mass1 >= (Mass2 + Mass3)) {
    if (Mass2 == fMpi || Mass2 == fMK) {
      if (Mass2 == fMpi)
        V = fVud;
      else if (Mass2 == fMK)
        V = fVus;

      a = (U2*fGF*fGF*V*V*form*form*Mass1*Mass1*Mass1)/(16.*TMath::Pi());
      b = TMath::Power(1. - Mass3*Mass3/(Mass1*Mass1), 2.);
      c = (1. + Mass3*Mass3/(Mass1*Mass1))*(Mass2*Mass2/(Mass1*Mass1));
      d = 1. - (Mass2 - Mass3)*(Mass2 - Mass3)/(Mass1*Mass1);
      f = 1. - (Mass2 + Mass3)*(Mass2 + Mass3)/(Mass1*Mass1);
      gamma_2 = a*(b - c)*TMath::Sqrt(d*f);
    }
    else if (Mass2 == fMrho) {

      V = fVud;
      a = (U2*fGF*fGF*form*form*V*V*Mass1*Mass1*Mass1)/(8.*TMath::Pi()*Mass2*Mass2);
      b = (1. - TMath::Power(Mass2/Mass1 - Mass3/Mass1, 2.))*(1. - TMath::Power(Mass2/Mass1 + Mass3/Mass1, 2.));
      c = (1. + (Mass3*Mass3)/(Mass1*Mass1))*(Mass2*Mass2/(Mass1*Mass1));
      d = 2*Mass2*Mass2*Mass2*Mass2/(Mass1*Mass1*Mass1*Mass1);
      f = TMath::Power(1. - (Mass3*Mass3)/(Mass1*Mass1), 2.);
      g = c - d + f;
      gamma_2 = a*TMath::Sqrt(b)*g;
    }
    else if (Mass2 == fMpi0) {
      a = (U2*fGF*fGF*form*form*Mass1*Mass1*Mass1)/(32.*TMath::Pi());
      b = TMath::Power(1. - Mass2*Mass2/(Mass1*Mass1), 2.);
      gamma_2 = a*b;
    }
    else if (Mass2 == fMeta || Mass2 == fMetaprime) {
      a = (U2*fGF*fGF*form*form*Mass1*Mass1*Mass1)/(32.*TMath::Pi());
      b = TMath::Power(1. - Mass2*Mass2/(Mass1*Mass1), 2.);
      gamma_2 = a*b;
    }
    else if (Mass2 == fMrho0) {
      a = (U2*form*form*fGF*fGF*Mass1*Mass1*Mass1)/(16.*TMath::Pi()*Mass2*Mass2);
      b = 1. + 2.*Mass2*Mass2/(Mass1*Mass1);
      c = TMath::Power(1. - Mass2*Mass2/(Mass1*Mass1), 2.);
      gamma_2 = a*b*c;
    }
    else {
      cout<<"[Gamma2] Unknown N two-body decay mode"<<endl;
      _exit(kWrongConfiguration);
    }
  }

  return gamma_2;
}

// Decay width for 3-body HNL decay mode

Double_t GammaLeptonNu3(Double_t Mass1, Double_t Mass2, Double_t Mass3, Bool_t noU) {

  Double_t r, a, b, b1, c, c1, d, d1, f, f1, g, g1, j, j1, L, C1, C2, C3, C4;
  Double_t gamma_l_l_nu = 0.;
  Double_t gamma_l_l_nu0;
  Double_t gamma_l_l_nu1;
  Double_t U2 = 0.;
  Double_t U = 1.;
  Double_t U1 = 1.;

  if (Mass1 >= (Mass2 + Mass3)) {
    if (Mass2 == Mass3 && Mass2 != 0.) {
      r = Mass2/Mass1;
      L = TMath::Log((1. - 3.*r*r - (1. - r*r)*(TMath::Sqrt(1. - 4.*r*r)))/(r*r*(1. + TMath::Sqrt(1. - 4.*r*r))));
      if (noU == false) {
	if (Mass2 == fMe) {
	  U = fUeSquared;
	  U1 = fUmuSquared + fUtauSquared;
	  L = -100.;
	}
	else if (Mass2 == fMmu) {
	  U = fUmuSquared;
	  U1 = fUeSquared + fUtauSquared;
	}
	else if (Mass2 == fMtau) {
	  U = fUtauSquared;
	  U1 = fUeSquared + fUmuSquared;
	}
      }
      else {
	U = 0.33;
	U1 = 0.66;
	L = -100.;
      }
      a = fGF*fGF*TMath::Power(Mass1, 5.)/(192.*TMath::Power(TMath::Pi(), 3.));
      C1 = 0.25*(1. - 4.*fSin2thetaW + 8.*fSin2thetaW*fSin2thetaW);
      C2 = 0.5*fSin2thetaW*(2.*fSin2thetaW - 1.);
      C3 = 0.25*(1. + 4.*fSin2thetaW + 8.*fSin2thetaW*fSin2thetaW);
      C4 = 0.5*fSin2thetaW*(2.*fSin2thetaW + 1.);
      b = C3;
      c = (1. - 14.*r*r - 2.*TMath::Power(r, 4.) - 12.*TMath::Power(r, 6.))*(TMath::Sqrt(1. - 4.*r*r));
      d = 12.*TMath::Power(r, 4.)*(TMath::Power(r, 4.) - 1.)*L;
      f = 4.*C4;
      g = r*r*(2. + 10.*r*r - 12.*TMath::Power(r, 4.))*(TMath::Sqrt(1. - 4.*r*r));
      j = 6.*TMath::Power(r, 4.)*(1. - 2.*r*r + 2.*TMath::Power(r, 4.))*L;
      gamma_l_l_nu0 = U*a*(b*(c+d) + f*(g+j));
      b1 = C1;
      c1 = c;
      d1 = d;
      f1 = 4.*C2;
      g1 = g;
      j1 = j;
      gamma_l_l_nu1 = U1*a*(b1*(c1+d1) + f1*(g1+j1));
      gamma_l_l_nu = gamma_l_l_nu0 + gamma_l_l_nu1;
    }
    else if (Mass2 == Mass3 && Mass2 == 0.) {
      if (noU == true)
	U2 = 1.;
      else
        U2 = fUSquared;

      a = U2*fGF*fGF*TMath::Power(Mass1, 5.)/(192.*TMath::Power(TMath::Pi(), 3.));
      gamma_l_l_nu = a;
    }
    else if (Mass2 != Mass3) {
      if (Mass2 > Mass3) {
        r = Mass2/Mass1;
      }
      else if (Mass2 < Mass3) {
        r = Mass3/Mass1;
      }
      else {
        cout<<"[GammaLeptonNu3] N 3-body decay mode should have equal masses"<<endl;
        _exit(kWrongConfiguration);
      }
      if (noU == true)
        U2 = 1.;
      else {
        if ((Mass2 == fMe || Mass3 == fMe) && (Mass2 == fMmu || Mass3 == fMmu))
	  U2 = fUeSquared + fUmuSquared;
        else if ((Mass2 == fMe || Mass3 == fMe) && (Mass2 == fMtau || Mass3 == fMtau))
          U2 = fUeSquared + fUtauSquared;
        else if ((Mass2 == fMmu || Mass3 == fMmu) && (Mass2 == fMtau || Mass3 == fMtau))
          U2 = fUmuSquared + fUtauSquared;
      }

      a = (U2*fGF*fGF*TMath::Power(Mass1, 5))/(192*TMath::Power(TMath::Pi(), 3));
      b = 1 - 8.*r*r + 8.*TMath::Power(r, 6) - TMath::Power(r, 8) - 12.*TMath::Power(r, 4)*TMath::Log(r*r);
      gamma_l_l_nu = a*b;
    }
  }

  return gamma_l_l_nu;
}

// Total HNL decay width

Double_t GammaTot(Double_t MN, Bool_t noU) {

  Double_t gammaTot = 0.;

  if (MN < 2*fMe) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU);
  }
  else if (MN >= 2*fMe && MN < (fMe+fMmu)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU);
  }
  else if (MN >= (fMe+fMmu) && MN < (fMpi0)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU);
  }
  else if (MN >= (fMpi0) && MN < (fMe+fMpi)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU);
  }
  else if (MN >= (fMe+fMpi) && MN < 2*fMmu) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU);
  }
  else if (MN >= 2*fMmu && MN < (fMmu+fMpi)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU);
  }
  else if (MN >= (fMmu+fMpi) && MN < (fMK+fMe)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU);
  }
  else if (MN >= (fMK+fMe) && MN < (fMeta)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU);
  }
  else if (MN >= (fMeta) && MN < (fMK+fMmu)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU);
  }
  else if (MN >= (fMK+fMmu) && MN < (fMrho0)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU);
  }
  else if (MN >= (fMrho0) && MN < (fMrho+fMe)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU);
  }
  else if (MN >= (fMrho+fMe) && MN < (fMrho+fMmu)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU);
  }
  else if (MN >= (fMmu+fMrho) && MN < (fMetaprime)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU) + Gamma2(MN, fMrho, fMmu, fRho, noU);
  }
  else if (MN >= (fMetaprime) && MN < (fMe+fMtau)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU) + Gamma2(MN, fMrho, fMmu, fRho, noU) + Gamma2(MN, fMetaprime, 0., fEtaprime, noU);
  }
  else if (MN >= (fMe+fMtau) && MN < (fMmu+fMtau)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU) + Gamma2(MN, fMrho, fMmu, fRho, noU) + Gamma2(MN, fMetaprime, 0., fEtaprime, noU) + GammaLeptonNu3(MN, fMe, fMtau, noU);
  }
  else if (MN >= (fMmu+fMtau) && MN < (fMpi+fMtau)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU) + Gamma2(MN, fMrho, fMmu, fRho, noU) + Gamma2(MN, fMetaprime, 0., fEtaprime, noU) + GammaLeptonNu3(MN, fMe, fMtau, noU) + GammaLeptonNu3(MN, fMmu, fMtau, noU);
  }
  else if (MN >= (fMpi+fMtau) && MN < (fMK+fMtau)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU) + Gamma2(MN, fMrho, fMmu, fRho, noU) + Gamma2(MN, fMetaprime, 0., fEtaprime, noU) + GammaLeptonNu3(MN, fMe, fMtau, noU) + GammaLeptonNu3(MN, fMmu, fMtau, noU) + Gamma2(MN, fMpi, fMtau, fPi, noU);
  }
  else if (MN >= (fMK+fMtau) && MN < (fMrho+fMtau)) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU) + Gamma2(MN, fMrho, fMmu, fRho, noU) + Gamma2(MN, fMetaprime, 0., fEtaprime, noU) + GammaLeptonNu3(MN, fMe, fMtau, noU) + GammaLeptonNu3(MN, fMmu, fMtau, noU) + Gamma2(MN, fMpi, fMtau, fPi, noU) + Gamma2(MN, fMK, fMtau, fK, noU);
  }
  else if (MN >= (fMrho+fMtau) && MN < 2*fMtau) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU) + Gamma2(MN, fMrho, fMmu, fRho, noU) + Gamma2(MN, fMetaprime, 0., fEtaprime, noU) + GammaLeptonNu3(MN, fMe, fMtau, noU) + GammaLeptonNu3(MN, fMmu, fMtau, noU) + Gamma2(MN, fMpi, fMtau, fPi, noU) + Gamma2(MN, fMK, fMtau, fK, noU) + Gamma2(MN, fMrho, fMtau, fRho, noU);
  }
  else if (MN >= 2*fMtau) {
    gammaTot = GammaLeptonNu3(MN, 0., 0., noU) + GammaLeptonNu3(MN, fMe, fMe, noU) + GammaLeptonNu3(MN, fMe, fMmu, noU) + Gamma2(MN, fMpi0, 0., fPi, noU) + Gamma2(MN, fMpi, fMe, fPi, noU) + GammaLeptonNu3(MN, fMmu, fMmu, noU) + Gamma2(MN, fMpi, fMmu, fPi, noU) + Gamma2(MN, fMK, fMe, fK, noU) + Gamma2(MN, fMeta, 0., fEta, noU) + Gamma2(MN, fMK, fMmu, fK, noU) + Gamma2(MN, fMrho0, 0., fRho, noU) + Gamma2(MN, fMrho, fMe, fRho, noU) + Gamma2(MN, fMrho, fMmu, fRho, noU) + Gamma2(MN, fMetaprime, 0., fEtaprime, noU) + GammaLeptonNu3(MN, fMe, fMtau, noU) + GammaLeptonNu3(MN, fMmu, fMtau, noU) + Gamma2(MN, fMpi, fMtau, fPi, noU) + Gamma2(MN, fMrho, fMtau, fRho, noU) + GammaLeptonNu3(MN, fMtau, fMtau, noU);
  }

  return 2.*gammaTot;
}

// HNL lifetime

Double_t tauN(Double_t MN, Bool_t noU) {

  Double_t gammaN = GammaTot(MN, noU);

  return fhc/(gammaN*fcLight);
}

// Lambda function

Double_t lambda(Double_t a, Double_t b, Double_t c) {

  return a*a + b*b + c*c - 2.*a*b - 2.*a*c - 2.*b*c;
}

// Factor related to HNL decay

Double_t ComputeDecay(Double_t MN, Int_t mode) {

  Double_t br = 0.;
  Double_t Gamma = 0.;

  if (mode == 0)
    Gamma = Gamma2(MN, fMpi, fMmu, fPi, false);
  else if (mode == 1)
    Gamma = Gamma2(MN, fMpi, fMe, fPi, false);
  else if (mode == 2)
    Gamma = Gamma2(MN, fMrho, fMmu, fRho, false);
  else if (mode == 3)
    Gamma = Gamma2(MN, fMrho, fMe, fRho, false);
  else {
    cout << "Unknown decay mode" << endl;
    _exit(kWrongConfiguration);
  }

  Double_t gammaTot = GammaTot(MN, false);

  if (Gamma > 0. && gammaTot > 0.)
    br = Gamma/gammaTot;

  return br;
}
