/////////////////////////////////////////////////////////////////////////////////////////
// Two body decay generator of a beam particle (K+, pi+ or KL).
// The mass of the invisible particle (nu or X) is controlled externally.
// NA62 version: E.Goudzovski (eg@hep.ph.bham.ac.uk) 4/03/2011, 1/10/2015, 14/8/2016.
//
// Modified to pass muon polarization to GEANT4: M.Koval (michal.koval@cern.ch) 14/8/2013.
// For details, see internal note NA62-13-09.
//////////////////////////////////////////////////////////////////////////////////////////

#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "masses.hh"
#include <iostream>
#include "NA62Global.hh"

using namespace std;

void two_body_decay(TLorentzVector Mom4Parent,
		    int id0, int id1, int id2, double MExotic, double TExotic, int Pi0DecayMode) {

  ///////////////////////////////////
  // Assign parent and daugher masses

  double m0=-999.999;
  if      (id0==PDG_ID_kp)  m0 = MKCH;
  else if (id0==PDG_ID_pip) m0 = MPI;
  else if (id0==PDG_ID_kl)  m0 = MK0;

  double m1=-999.999;
  if      (id1==PDG_ID_pip || id1==PDG_ID_pim)    m1 = MPI;
  else if (id1==PDG_ID_pi0)                       m1 = MP0;
  else if (id1==PDG_ID_elp || id1==PDG_ID_elm)    m1 = MEL;
  else if (id1==PDG_ID_mup || id1==PDG_ID_mum)    m1 = MMU;
  else if (id1==PDG_ID_nu  || id1==PDG_ID_gam)    m1 = 0.0;
  else if (id1>=PDG_ID_exo && id1<PDG_ID_exo+200) m1 = MExotic;

  double m2=-999.999;
  if      (id2==PDG_ID_pip || id2==PDG_ID_pim)    m2 = MPI;
  else if (id2==PDG_ID_pi0)                       m2 = MP0;
  else if (id2==PDG_ID_elp || id2==PDG_ID_elm)    m2 = MEL;
  else if (id2==PDG_ID_mup || id2==PDG_ID_mum)    m2 = MMU;
  else if (id2==PDG_ID_nu  || id2==PDG_ID_gam)    m2 = 0.0;
  else if (id2>=PDG_ID_exo && id2<PDG_ID_exo+200) m2 = MExotic;

  if (m0<0.0 || m1<0.0 || m2<0.0) {
    cout << "[TWO_BODY_DECAY] Error: invalid parent/daughter IDs" << endl;
    exit(kWrongConfiguration);
  }
  if (m0 < m1+m2) {
    cout << "[TWO_BODY_DECAY] Error: energy not conserved in decay (m0<m1+m2)" << endl;
    exit(kWrongConfiguration);
  }

  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

  // Energies and momenta of the daughters
  Double_t E1 = 0.5*(m0*m0 + m1*m1 - m2*m2) / m0;
  Double_t E2 = 0.5*(m0*m0 + m2*m2 - m1*m1) / m0;
  Double_t P  = sqrt(E1*E1 - m1*m1);

  Double_t x=0.0, y=0.0, z=0.0;
  RandomDecay->Sphere(x, y, z, 1.0);
  TLorentzVector Mom4_1 ( P*x,  P*y,  P*z, E1);
  TLorentzVector Mom4_2 (-P*x, -P*y, -P*z, E2);

  // Save the GeneParts
  mcadd4gencpp(id1, Mom4_1, 0);
  mcadd4gencpp(id2, Mom4_2, 0);

  // Boost decay products into lab frame
  Mom4_1.Boost(Mom4Parent.BoostVector());
  Mom4_2.Boost(Mom4Parent.BoostVector());

  Bool_t PassToGeant1 = (id1<PDG_ID_exo || id1>=PDG_ID_exo+200 || (MExotic>0.0 && TExotic>=0.0));
  Bool_t PassToGeant2 = (id2<PDG_ID_exo || id2>=PDG_ID_exo+200 || (MExotic>0.0 && TExotic>=0.0));

  // For debugging, it is useful to pass all daughters to Geant4
  // PassToGeant1 = PassToGeant2 = true;

  // Pass the first daughter to Geant4 with polarization (K+, pi+ --> mu+ nu) or not?
  int j1=-1, j2=-1;
  if (PassToGeant1) {
    if ((id0==PDG_ID_kp || id0==PDG_ID_pip) && id1==PDG_ID_mup && id2>=PDG_ID_exo && id2<PDG_ID_exo+200) {
      double Scale1 = 2.0*m1/(m0*m0 - m1*m1);
      double Scale2 = (-1.0/(Mom4_1.E()+m1))*(1.0+Scale1*(Mom4Parent.E()+m1));
      TVector3 pol  = Scale1*Mom4Parent.Vect() + Scale2*Mom4_1.Vect();
      j1 = mcadd4pol3cpp(id1, Mom4_1, pol);
    }
    else {
      j1 = mcadd4cpp(id1, Mom4_1);
    }
  }

  // Pass the second daughter to Geant4
  if (PassToGeant2) {
    j2 = mcadd4cpp(id2, Mom4_2);
  }

  // Forced pi0 decays
  if (id1==PDG_ID_pi0) pi0decay_manager_cpp(j1, Mom4_1, Pi0DecayMode % 100);
  if (id2==PDG_ID_pi0) { // distinguish pi0+pi0 vs (non-pi0)+pi0 cases
    if (id1==PDG_ID_pi0) pi0decay_manager_cpp(j2, Mom4_2, (Pi0DecayMode/100) % 100);
    else                 pi0decay_manager_cpp(j2, Mom4_2, Pi0DecayMode % 100);
  }
}
