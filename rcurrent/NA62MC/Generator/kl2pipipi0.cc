///////////////////////////////////////////////////////
// KL --> pi+ pi- pi0 decay generator.
// So far, phase space only.
// Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 16/8/2016.

#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "masses.hh"
#include "TGenPhaseSpace.h"
#include <iostream>

using namespace std;

void kl2pipipi0(TLorentzVector Mom4Parent, int Pi0DecayMode) {
  static TGenPhaseSpace event;
  static TLorentzVector PKaon(0.0, 0.0, 0.0, MK0);
  static int gen_phase_space_calls = 0;
  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

  if (!gen_phase_space_calls) {
    static double masses[3] = {MPI, MPI, MP0};
    event.SetDecay(PKaon, 3, masses);
    gen_phase_space_calls++;
  }

  double weight;
  do {
    weight = event.Generate();
  } while (0.48*RandomDecay->Uniform() > weight);
  TLorentzVector *p1 = event.GetDecay(0);
  TLorentzVector *p2 = event.GetDecay(1);
  TLorentzVector *p3 = event.GetDecay(2);

  // Save the GeneParts
  mcadd4gencpp(PDG_ID_pip, *p1);
  mcadd4gencpp(PDG_ID_pim, *p2);
  mcadd4gencpp(PDG_ID_pi0, *p3);

  // Boost decay products into lab frame
  p1->Boost(Mom4Parent.BoostVector());
  p2->Boost(Mom4Parent.BoostVector());
  p3->Boost(Mom4Parent.BoostVector());

  // Pass decay products to Geant4
  mcadd4cpp(PDG_ID_pip, *p1);
  mcadd4cpp(PDG_ID_pim, *p2);
  int jpi0 = mcadd4cpp(PDG_ID_pi0, *p3);

  // Forced pi0 decays
  pi0decay_manager_cpp(jpi0, *p3, Pi0DecayMode % 100);
}
