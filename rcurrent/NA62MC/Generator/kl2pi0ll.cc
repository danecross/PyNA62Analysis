///////////////////////////////////////////////////////
// KL --> pi0 l+ l- decay generator.
// So far, phase space only.
// Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 16/8/2016.

#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "masses.hh"
#include "TGenPhaseSpace.h"
#include <iostream>

using namespace std;

void kl2pi0ll(TLorentzVector Mom4Parent, int Pi0DecayMode, int mode) {
  static TGenPhaseSpace event;
  static TLorentzVector PKaon(0.0, 0.0, 0.0, MK0);
  static int gen_phase_space_calls = 0;

  static double mass_lepton1[4] = {MEL, MMU, MEL, MMU};
  static double mass_lepton2[4] = {MEL, MMU, MMU, MEL};
  static int    id_lepton1[4]   = {PDG_ID_elp, PDG_ID_mup, PDG_ID_elp, PDG_ID_mup};
  static int    id_lepton2[4]   = {PDG_ID_elm, PDG_ID_mum, PDG_ID_mum, PDG_ID_elm};

  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

  if (!gen_phase_space_calls) {
    double mass[3];
    mass[0] = MP0;
    mass[1] = mass_lepton1[mode];
    mass[2] = mass_lepton2[mode];
    event.SetDecay(PKaon, 3, mass);
    gen_phase_space_calls++;
  }

  double weight;
  do {
    weight = event.Generate();
  } while (0.47*RandomDecay->Uniform() > weight); // Max weight is good for all the four KL->pi0l+l- modes
  TLorentzVector *p1 = event.GetDecay(0);
  TLorentzVector *p2 = event.GetDecay(1);
  TLorentzVector *p3 = event.GetDecay(2);

  // Save the GeneParts
  mcadd4gencpp(PDG_ID_pi0, *p1);
  mcadd4gencpp(id_lepton1[mode], *p2);
  mcadd4gencpp(id_lepton2[mode], *p3);

  // Boost decay products into lab frame
  p1->Boost(Mom4Parent.BoostVector());
  p2->Boost(Mom4Parent.BoostVector());
  p3->Boost(Mom4Parent.BoostVector());

  // Pass decay products to Geant4
  int jpi0 = mcadd4cpp(PDG_ID_pi0, *p1);
  mcadd4cpp(id_lepton1[mode], *p2);
  mcadd4cpp(id_lepton2[mode], *p3);

  // Forced pi0 decays
  pi0decay_manager_cpp(jpi0, *p1, Pi0DecayMode % 100);
}
