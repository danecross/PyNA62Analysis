///////////////////////////////////////////////////////////////////////////
// Decay generator: K+ --> mu+ nu_mu X', X' is an invisible scalar particle
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

double kmunuXscal_decay_dist(double m12, double m23, double m_X);

void kch2munuXscal (TLorentzVector Mom4Parent, double m_X, double tau_X) {

  if (m_X < 0.01) {
    cout << "[KCH2MUNUXSCAL] ERROR: Less than 10 MeV exotic particle mass is forbidden by the model" << endl;
    exit(kWrongConfiguration);
  }
  if (MKCH < MMU + m_X) {
    cout << "[KCH2MUNUXSCAL] ERROR: exotic particle mass is too large" << endl;
    exit(kWrongConfiguration);
  }

  // These variables are static, as they need to be initialized only once
  static TGenPhaseSpace Xscalar_event;
  static TLorentzVector pkaon(0.0, 0.0, 0.0, MKCH); // kaon decay at rest

  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

  double masses[3] = {MMU, 0.0, m_X}; // m_X can change from event to event
  bool Xscalar_mass_OK = Xscalar_event.SetDecay(pkaon, 3, masses);
  if (!Xscalar_mass_OK) {
    cout<<"[KCH2MUNUXSCAL] ERROR: decay forbidden by kinematics, check exotic particle mass"<<endl;
    exit(kWrongConfiguration);
  }

  ///////////////////////////////////////////////////////////////////////
  // The maximum weights as function of m_X (weight_max and M2_max)
  // were calculated from MC samples of 10M events
  // for each m_X between 10 MeV and 388 MeV with steps of 2-3 MeV.
  // A constant is added to each mass X fitting equation to account for undershoots when fitting
  // the weights as a function of m_X to obtain the smooth parameterization.

  // Maximum weight for the given m_X from TGenPhaseSpace
  double weight_max = 0.367094+ 0.945795*m_X -4.052595*m_X*m_X+ 10.166313*m_X*m_X*m_X -14.615904*m_X*m_X*m_X*m_X + 0.00115;

  // Maximum decay distribution rate for the given m_X
  double M2_max = 0.6802445  -7.4154921*m_X + 28.65089*m_X*m_X -37.197947*m_X*m_X*m_X + 0.0027391168/(m_X*m_X) + 0.16;

  TLorentzVector *muon, *neutrino, *Xscalar;
  double M2, weight;
  do {
    double m12, m23;
    weight      = Xscalar_event.Generate();
    muon        = Xscalar_event.GetDecay(0);
    neutrino    = Xscalar_event.GetDecay(1);
    Xscalar     = Xscalar_event.GetDecay(2);
    m12         = (*neutrino + *Xscalar).M2();
    m23         = (*muon + *Xscalar).M2();
    M2          = kmunuXscal_decay_dist(m12, m23, m_X);
  } while (M2_max*weight_max*RandomDecay->Uniform() > M2*weight); // accept-reject

  // Save the GeneParts
  mcadd4gencpp (PDG_ID_mup, *muon);
  mcadd4gencpp (PDG_ID_nu,  *neutrino);
  mcadd4gencpp (PDG_ID_exo, *Xscalar);

  // Boost into the lab frame
  TLorentzVector Mom4muon = *muon;
  Mom4muon.Boost(Mom4Parent.BoostVector());
  TLorentzVector Mom4dp = *Xscalar;
  Mom4dp.Boost(Mom4Parent.BoostVector());

  // Pass the mu+ and the Xscalar to Geant4 for tracking
  mcadd4cpp(PDG_ID_mup, Mom4muon);
  if (tau_X>=0.0) mcadd4cpp(PDG_ID_exo, Mom4dp); // for unstable particles only
}

//////////////////////////////////////////////////////////////////////
// K+ --> mu+ nu_mu X' decay rate distribution
// from arXiv:1902.07715 (Krnjaic, Marques-Tavares, Redigolo, Tobioka)

double kmunuXscal_decay_dist (double m12, double m23, double m_X) {
  double no_const_scal_num =
    (-m23*(m12*(m23 - MMU*MMU) + (m23 + MMU*MMU)*(m23 + MMU*MMU) - m23*m_X*m_X) +
     MKCH*MKCH*((m23+MMU*MMU)*(m23+MMU*MMU) - MMU*MMU*m_X*m_X));
  double no_const_scal_denom = (m23-MMU*MMU)*(m23-MMU*MMU);
  return no_const_scal_num/no_const_scal_denom;
}
