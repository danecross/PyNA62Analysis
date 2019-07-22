
////////////////////////////////////////////////////////////////////////////////////////////
//
// NA62 decay generator: K+ --> l+ + nothing
// Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) and Artur Shaikhiev (shaykhiev@inr.ru) Feb 2016
//
// History:
// 13-07-2016: Artur Shaikhiev (shaykhiev@inr.ru) - The Standard Model is added.
//
// SM differential rate of the K+ --> l+ nu nu nu decay is simulated
// according to D. Gorbunov and A. Mitrofanov, JHEP 1610 (2016) 039.
// Two other models are available below and can be simulated optionally.
//
// Input:
// parent type:   ptype=1 for K+, 2 for pi+.
// daughter type: dtype=1 for e+, 2 for mu+.
////////////////////////////////////////////////////////////////////////////////////////////

#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "masses.hh"
#include <iostream>

using namespace std;

double diff_br_SM(int dtype, double Ml, double El);
double diff_g_nunu(double Ml, double El);
double diff_g_six_fermion(double Ml, double El);

void kch2lnununu(TLorentzVector Mom4Parent, int ptype, int dtype) {

  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();
  if (ptype!=1 && ptype!=2) ptype = 1; // parent type: kaon (1) or pion (2)
  if (dtype!=1 && dtype!=2) dtype = 1; // daughter type: electron (1) or muon (2)

  // Parent mass
  double M0;
  switch (ptype) {
  case 1: M0 = MKCH; break;
  case 2: M0 = MPI;  break;
  }

  // Daughter type and ID
  double Ml;
  int    ID;
  switch (dtype) {
  case 1: Ml = MEL; ID = PDG_ID_elp; break;
  case 2: Ml = MMU; ID = PDG_ID_mup; break;
  }

  // Lepton energy kinematic endpoint
  double Emax = 0.5*(M0*M0 + Ml*Ml) / M0;

  // Kaon decays are generated according to the SM spectrum.
  // Acceptance-rejection: event density functions for all models and
  // both daughter lepton flavours are tuned to return weights less than one.
  double El = 0.0;
  if (ptype==1) {
    do {
      El = RandomDecay->Uniform(Ml, Emax);
    } while (RandomDecay->Rndm() > diff_br_SM(dtype, Ml, El)); // can switch between the models here
  }
  // Pion decay: flat lepton energy spectrum is generated
  else {
    El = RandomDecay->Uniform(Ml, Emax);
  }
  double Pl = sqrt(El*El-Ml*Ml);

  Double_t x=0.0, y=0.0, z=0.0;
  RandomDecay->Sphere(x, y, z, 1.0);
  TLorentzVector Mom4daughter (Pl*x, Pl*y, Pl*z, El);

  // Save the GenePart
  mcadd4gencpp(ID, Mom4daughter);

  // Boost daughter into lab frame
  Mom4daughter.Boost(Mom4Parent.BoostVector());

  // Pass the daughter to Geant4 for tracking
  mcadd4cpp(ID, Mom4daughter);
}

////////////////////////////////////////////////////////////////////////
// K-->l nu nu nu spectrum: the Standard Model
// D. Gorbunov and A. Mitrofanov, JHEP 1610 (2016) 039, arXiv:1605.08077
// dBR/dp spectrum is given in the paper.
// dBR/dE spectrum is simulated here for mu+ and e+.

double diff_br_SM(int dtype, double Ml, double El) {

  double Pl = 1000*sqrt(El*El - Ml*Ml); // GeV -> MeV because the spectra were fitted in MeV scale in the paper
  if (Pl == 0.0) return 0.0;
  double factor = El/Pl; // dBR/dp -> dBR/dE
  if (dtype==1) // electron
    return factor*(1.040e-20*Pl + 1.990e-22*Pl*Pl - 4.793e-25*Pl*Pl*Pl - 1.328e-26*Pl*Pl*Pl*Pl +
		   6.474e-29*Pl*Pl*Pl*Pl*Pl - 7.752e-32*Pl*Pl*Pl*Pl*Pl*Pl)/1.92e-21;
  else if (dtype==2) // muon
    return factor*(2.234e-22*Pl + 2.043e-22*Pl*Pl + 8.368e-26*Pl*Pl*Pl - 1.632e-26*Pl*Pl*Pl*Pl +
		   7.338e-29*Pl*Pl*Pl*Pl*Pl-9.035e-32*Pl*Pl*Pl*Pl*Pl*Pl)/1.73e-21;
  return -1.0; // Error; should not reach this line
}

///////////////////////////////////////////////////////////////////////////
// K-->l nu nu nu spectrum: neutrino-neutrino interaction.
// D.Yu. Bardin, S.M. Bilenky and B. Pontecorvo, Phys. Lett. B32 (1970) 121
// dGamma/dx is shown here for mu+ and e+

double diff_g_nunu(double Ml, double El) {
  double r = Ml/MKCH;
  double x = El/MKCH;
  return 60.0 * (1.0+r*r-2.0*x) * sqrt(x*x-r*r) * ((1.0-2.0*x)*x+r*r);
}

////////////////////////////////////////////////////////
// K-->l nu nu nu spectrum: six-fermion interaction.
// Pang et al., Phys. Rev. D8 (1973) 1989
// Only the formula for the muons is given in this paper.

double diff_g_six_fermion(double Ml, double El) {
  double Pl = sqrt(El*El - Ml*Ml);
  double A  = MKCH - El - Pl;
  // The following expression for R is slightly differs from the paper.
  // In the paper the second term in brackets has dimension Energy^4 while the rest have Energy^3.
  return 18000.0 * Pl*A*A*El * (2.0*Pl*Pl*Pl + 4.0*Pl*Pl*A + 2.5*Pl*A*A + 0.5*A*A*A) * (1.0+Ml/El);
}
