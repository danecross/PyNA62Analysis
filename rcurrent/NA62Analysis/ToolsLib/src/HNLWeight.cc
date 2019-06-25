// ---------------------------------------------------------------
//
// History:
//
// Created by Lorenza Iacobuzio (lorenza.iacobuzio@cern.ch) February 2018
//
// ---------------------------------------------------------------

/// \class HNLWeight
/// \Brief
/// Compute and return the weight associated to each heavy neutral lepton in a MC sample
/// \EndBrief
/// \Detailed
/// For each HNL in the MC sample, a weight is computed for the user.
/// This analyzer makes use of a ToolsLib called HNLFunctions. The arguments needed are: a pointer to the current processed event is needed as first argument of the function ComputeWeight; the value of the squared HNL coupling and the values of the ratios between specific-flavour couplings; the values of the beginning of the fiducial volume and its length (must be identical to the ones set in the MC macro for production); the HNL decay mode (0 for pi-mu states, 1 for pi-e, 2 for rho-mu, 3 for rho-e).
/// A vector of maps is produced and returned; each map contains quantities associated to each HNL (between brackets is the name by which the user can retrieve them from the map): mass (Mass), lifetime (Lifetime), modulus of the three-momentum (Momentum), factor associated to its decay (DecayFactor), probability of reaching the FV (ReachProb) and decaying into it (DecayProb), coupling with the lepton produced in pair with (LeptonUSquared), factor associated to its production (ProdFactor), probability of proton producing D meson in target/TAX (DProdProb), weight for each HNL (Weight), number of times the HNL has been regenerated before being good (Bias), a boolean to check if the HNL is associated to the two daughters of the event (IsGood), the Z decay point (Z), and the weight to account for the exponential distribution in Z (to account for flat generation at MC stage).

/// The returned vector can be retrieved in the following way:
/// \code
/// std::vector<std::map<std::string, Double_t>> =
/// ComputeWeight(evt, 1.E-10, 5., 1., 3.5, 100000., 80000., 0);
/// \endcode
///
/// \author Lorenza Iacobuzio (lorenza.iacobuzio@cern.ch)
/// \EndDetailed

#include "Event.hh"
#include "HNLFunctions.hh"
#include "HNLWeight.hh"

using namespace std;

/// \class HNLWeight

std::vector<std::map<std::string, Double_t>> fWeightContainer;

std::vector<std::map<std::string, Double_t>> ComputeWeight(Event* evt, Double_t USquared, Double_t UeSquaredRatio, Double_t UmuSquaredRatio, Double_t UtauSquaredRatio, Double_t LInitialFV, Double_t LFV, Int_t mode) {

  fUSquared = USquared;
  fUeSquaredRatio = UeSquaredRatio;
  fUmuSquaredRatio = UmuSquaredRatio;
  fUtauSquaredRatio = UtauSquaredRatio;
  fUeSquared   = fUSquared/(fUeSquaredRatio + fUmuSquaredRatio + fUtauSquaredRatio)*fUeSquaredRatio;
  fUmuSquared  = fUSquared/(fUeSquaredRatio + fUmuSquaredRatio + fUtauSquaredRatio)*fUmuSquaredRatio;
  fUtauSquared = fUSquared/(fUeSquaredRatio + fUmuSquaredRatio + fUtauSquaredRatio)*fUtauSquaredRatio;
  fLInitialFV = LInitialFV;
  fLFV = LFV;

  Double_t MN;
  Double_t HNLTau;
  Double_t pN;
  Double_t NDecayProb;
  Double_t NReachProb;
  Double_t LReach;
  Double_t LeptonUSquared = 0.;
  Double_t DecayFactor;
  Double_t ProdFactor;
  Double_t Weight;
  Double_t DProdProb;
  Double_t ZDecay;
  Double_t Z;
  Bool_t IsGood = false;
  Int_t Bias;
  TString Name;
  TVector3 point1;
  TVector3 point2;
  TVector3 momentum1;

  fWeightContainer.clear();

  for (Int_t i = 0; i < evt->GetNKineParts(); i++) {
    KinePart *p = evt->GetKinePart(i);
    if (p->GetParentID() == -1 && p->GetPDGcode() == 999) {
      point1.SetXYZ(p->GetProdPos().X(), p->GetProdPos().Y(), p->GetProdPos().Z());
      point2.SetXYZ(0., 0., fLInitialFV);
      momentum1.SetXYZ(p->GetInitial4Momentum().Px(), p->GetInitial4Momentum().Py(), p->GetInitial4Momentum().Pz());
      MN = ComputeHNLMass(p);
      pN = TMath::Sqrt(p->GetInitialEnergy()*p->GetInitialEnergy() - MN*MN);
      HNLTau = tauN(MN, false);
      LReach = ComputeL(point1, point2, momentum1);
      DecayFactor = ComputeDecay(MN, mode);
      NReachProb = ComputeNReachProb(p, HNLTau, LReach);
      NDecayProb = ComputeNDecayProb(p, HNLTau, fLFV);
      ZDecay = ReweightZDecay(p, HNLTau);
      Z = p->GetEndPos().Z();

      if (p->GetParticleName().Contains("e") && !p->GetParticleName().Contains("nu_tau"))
        LeptonUSquared = fUeSquared;
      else if (p->GetParticleName().Contains("mu") && !p->GetParticleName().Contains("nu_tau"))
        LeptonUSquared = fUmuSquared;
      else if (p->GetParticleName().Contains("Ntau") || p->GetParticleName().Contains("nu_tau") || (p->GetParticleName().Contains("tau") && (p->GetParticleName().Contains("rho") || p->GetParticleName().Contains("pi")|| p->GetParticleName().Contains("K"))))
        LeptonUSquared = fUtauSquared;

      if (p->GetParticleName().Contains("tau->")) {
        if (p->GetParticleName().Contains("DS"))
          ProdFactor = fDStoTauBR;
        else
          ProdFactor = fDtoTauBR;
      }
      else
        ProdFactor = 1.;

      if (p->GetProdPos().Z() >= -400. && p->GetProdPos().Z() <= 400.)
        DProdProb = fDBeProdProb;
      else
        DProdProb = fDCuProdProb;

      if (p->GetEndProcessName() == "good")
        IsGood = true;

      Bias = evt->GetEventWeight();

      Weight = DProdProb*fDDecayProb*NReachProb*NDecayProb*DecayFactor*ProdFactor*LeptonUSquared;

      std::map<std::string, Double_t> SingleHNL;

      SingleHNL["Mass"] = MN;
      SingleHNL["Lifetime"] = HNLTau;
      SingleHNL["Momentum"] = pN;
      SingleHNL["DecayFactor"] = DecayFactor;
      SingleHNL["ReachProb"] = NReachProb;
      SingleHNL["DecayProb"] = NDecayProb;
      SingleHNL["LeptonUSquared"] = LeptonUSquared;
      SingleHNL["ProdFactor"] = ProdFactor;
      SingleHNL["ProdProb"] = DProdProb;
      SingleHNL["IsGood"] = IsGood;
      SingleHNL["Weight"] = Weight;
      SingleHNL["Bias"] = Bias;
      SingleHNL["ZDecay"] = ZDecay;
      SingleHNL["Z"] = Z;

      fWeightContainer.push_back(SingleHNL);
    }
  }

  return fWeightContainer;
}
