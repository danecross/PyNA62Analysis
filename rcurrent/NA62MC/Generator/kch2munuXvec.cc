///////////////////////////////////////////////////////////////////////////
// Decay generator: K+ --> mu+ nu_mu A', A' is an invisible vector particle
// Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) & Dominik Vuina
//
// Input: mass of X' (in GeV)
///////////////////////////////////////////////////////////////////////////

#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "masses.hh"
#include <iostream>
#include "TLorentzVector.h"
#include "TGenPhaseSpace.h"
#include "NA62Global.hh"

using namespace std;

double kmunuXvec_decay_dist(double m12, double m23, double m_X);

void kch2munuXvec(TLorentzVector Mom4Parent, double m_X, double tau_X) {

  if (m_X < 0.01) {
    cout << "[KCH2MUNUXVEC] ERROR: Less than 10 MeV exotic particle mass is forbidden by the model" << endl;
    exit(kWrongConfiguration);
  }
  if (MKCH < MMU + m_X) {
    cout << "[KCH2MUNUXVEC] ERROR: exotic particle mass is too large" << endl;
    exit(kWrongConfiguration);
  }

  // These variables are static, as they need to be initialized only once
  static TGenPhaseSpace Xvec_event;
  static TLorentzVector pkaon(0.0, 0.0, 0.0, MKCH); // kaon decay at rest

  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

  double masses[3] = {MMU, 0.0, m_X}; // m_x can change from event to event
  bool Xvec_mass_OK = Xvec_event.SetDecay(pkaon, 3, masses);
  if (!Xvec_mass_OK) {
    cout<<"[KCH2MUNUXVEC] ERROR: decay forbidden by kinematics, check exotic particle mass"<<endl;
    exit(kWrongConfiguration);
  }

  ///////////////////////////////////////////////////////////////////////
  // The maximum weights as function of m_X (weight_max and M2_max)
  // were calculated from MC samples of 10M events
  // for each m_X between 10 MeV and 388 MeV with a step of 2-3 MeV.
  // A constant is added to each fitted function to account for undershoots when fitting
  // the weights as a function of m_X to obtain the smooth parameterization.

  // Maximum weight for the given m_X from TGenPhaseSpace
  double weight_max = 0.36727 + 0.94146*m_X -4.02419*m_X*m_X +10.11075*m_X*m_X*m_X -14.59825*m_X*m_X*m_X*m_X + 0.001; 

  // Maximum decay rate distirbution for the given m_X
  double M2_max = -73.99648+ 844.816743*m_X -3373.12679*m_X*m_X+ 4370.51437*m_X*m_X*m_X  +0.4340479/(m_X*m_X)+17;

  TLorentzVector *muon, *neutrino, *Xvec;
  double M2, weight;
  do {
    double m12, m23;
    weight      = Xvec_event.Generate();
    muon        = Xvec_event.GetDecay(0);
    neutrino    = Xvec_event.GetDecay(1);
    Xvec = Xvec_event.GetDecay(2);
    m12         = (*neutrino + *Xvec).M2();
    m23         = (*muon + *Xvec).M2();
    M2          = kmunuXvec_decay_dist(m12, m23, m_X);
  } while   (M2_max*weight_max*RandomDecay->Uniform() > M2*weight); // accept-reject

  // Save the GeneParts
  mcadd4gencpp (PDG_ID_mup, *muon);
  mcadd4gencpp (PDG_ID_nu,  *neutrino);
  mcadd4gencpp (PDG_ID_exo, *Xvec);

  // Boost into the lab frame
  TLorentzVector Mom4muon = *muon;
  Mom4muon.Boost(Mom4Parent.BoostVector());
  TLorentzVector Mom4dp = *Xvec;
  Mom4dp.Boost(Mom4Parent.BoostVector());

  // Pass the mu+ and the X vector to Geant4 for tracking
  mcadd4cpp(PDG_ID_mup, Mom4muon);
  if (tau_X>=0.0) mcadd4cpp(PDG_ID_exo, Mom4dp); // for unstable particles only
}

//////////////////////////////////////////////////////////////////////
// K+ --> mu+ nu_mu X' decay rate distribution
// from arXiv:1902.07715 (Krnjaic, Marques-Tavares, Redigolo, Tobioka)

double kmunuXvec_decay_dist (double m12, double m23, double m_X) {
  double no_const_vec =
    (2-((m_X*m_X)/(m12*m12))*(MKCH*MKCH-MMU*MMU) + ((m23-2*MKCH*MKCH+MMU*MMU)/(m12)) +
     ((MMU*MMU-MKCH*MKCH)*((m_X*m_X+2*MMU*MMU))/((m23-MMU*MMU)*(m23-MMU*MMU))) +
     ((m12-2*MKCH*MKCH+2*MMU*MMU)/(m23-MMU*MMU)) +
     2*(m_X*m_X*MMU*MMU + (MKCH*MKCH - MMU*MMU)*(MKCH*MKCH - MMU*MMU))/
     (m12*(m23-MMU*MMU)));
  return no_const_vec;
}
