////////////////////////////////////////////////////////////////////////////////////////////
// NA62 decay generator: K+ --> mu+ nu_mu A', A' is an invisible vector particle
// Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) & Artur Shaikhiev (shaykhiev@inr.ru) March 2016
//
// Input: mass of A' (in GeV)
////////////////////////////////////////////////////////////////////////////////////////////

#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "masses.hh"
#include <iostream>
#include "TLorentzVector.h"
#include "TGenPhaseSpace.h"
#include "NA62Global.hh"

using namespace std;

double kmunuA_matrix_element(double Emu, double Enu, double m_A);

void kch2munuA (TLorentzVector Mom4Parent, double m_A, double tau_A) {

  if (m_A == 0.0) {
    cout << "[KCH2MUNUA] ERROR: Zero exotic particle mass is forbidden by the model" << endl;
    exit(kWrongConfiguration);
  }
  if (MKCH < MMU + m_A) {
    cout << "[KCH2MUNUA] ERROR: exotic particle mass is too large" << endl;
    exit(kWrongConfiguration);
  }

  // These variables are static, as they need to be initialized only once
  static TGenPhaseSpace dark_photon_event;
  static TLorentzVector pkaon(0.0, 0.0, 0.0, MKCH); // kaon decay at rest

  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

  double masses[3] = {MMU, 0.0, m_A}; // m_A can change from event to event
  bool dark_photon_mass_OK = dark_photon_event.SetDecay(pkaon, 3, masses);
  if (!dark_photon_mass_OK) {
    cout << "[KCH2MUNUA] ERROR: decay forbidden by kinematics, check exotic particle mass" << endl;
    exit(kWrongConfiguration);
  }

  ///////////////////////////////////////////////////////////////////////
  // The maximum weights as function of m_A (weight_max and M2_max)
  // were calculated from MC samples of 10M events
  // for each m_A between 1 MeV and 388 MeV with a step of 1 MeV.
  // A factor of 1.05 is applied to account for undershoots when fitting
  // the weights as a function of m_A to obtain the smooth parameterization.

  // Maximum weight for the given m_A from TGenPhaseSpace
  double weight_max = 0.3677 + 0.9254*m_A - 3.8304*m_A*m_A +
    9.2752*m_A*m_A*m_A - 13.4446*m_A*m_A*m_A*m_A;

  // Maximum squared matrix element for the given m_A
  double M2_max = -0.009859 - 1.062*m_A + 4.449*m_A*m_A -
    5.214*m_A*m_A*m_A + 0.00859/(m_A*m_A);

  TLorentzVector *muon, *neutrino, *dark_photon;
  double M2, weight;
  do {
    weight      = dark_photon_event.Generate();
    muon        = dark_photon_event.GetDecay(0);
    neutrino    = dark_photon_event.GetDecay(1);
    dark_photon = dark_photon_event.GetDecay(2);
    M2          = kmunuA_matrix_element(muon->E(), neutrino->E(), m_A);
  } while (1.05*M2_max*weight_max*RandomDecay->Uniform() > M2*weight); // accept-reject

  // Save the GeneParts
  mcadd4gencpp (PDG_ID_mup, *muon);
  mcadd4gencpp (PDG_ID_nu,  *neutrino);
  mcadd4gencpp (PDG_ID_exo, *dark_photon);

  // Boost into the lab frame
  TLorentzVector Mom4muon = *muon;
  Mom4muon.Boost(Mom4Parent.BoostVector());
  TLorentzVector Mom4dp = *dark_photon;
  Mom4dp.Boost(Mom4Parent.BoostVector());

  // Pass the mu+ and the dark photon to Geant4 for tracking
  mcadd4cpp(PDG_ID_mup, Mom4muon);
  if (tau_A>=0.0) mcadd4cpp(PDG_ID_exo, Mom4dp); // for unstable particles only
}

///////////////////////////////////////////////////////////////////////
// K+ --> mu+ nu_mu A' matrix element according to
// C.Carlson and B.Rislow, Phys.Rev.D86 (2012) 035013, arXiv:1206.3587.
// A' couples only to the muon.

double kmunuA_matrix_element (double Emu, double Enu, double m_A) {
  double esq = 4.0*TMath::Pi()/137.0; // e^2
  double QSQ = MKCH*MKCH-2*MKCH*Enu;
  double E_v = MKCH-Emu-Enu; // DP energy in the kaon rest frame

  double denom = 4*TMath::Pi()*TMath::Pi()*MMU*MMU*(MKCH*MKCH-MMU*MMU)*(MKCH*MKCH-MMU*MMU)*(QSQ-MMU*MMU)*(QSQ-MMU*MMU);
  double numerator = 0.6355*esq*MKCH*MKCH*(4*MMU*MMU*MKCH*MKCH*Emu*Enu-12*MMU*MMU*MKCH*QSQ*Enu
  +(QSQ*QSQ-MMU*MMU*MKCH*MKCH)*(MKCH*MKCH+m_A*m_A-MMU*MMU-2*MKCH*E_v)+(1./(m_A*m_A))*(MKCH*MKCH-m_A*m_A-MMU*MMU-2*MKCH*Enu)
  *(4*MMU*MMU*MKCH*MKCH*E_v*Enu+(QSQ*QSQ-MMU*MMU*MKCH*MKCH)*(MKCH*MKCH-m_A*m_A+MMU*MMU-2*MKCH*Emu)));

  return numerator/denom;
}
