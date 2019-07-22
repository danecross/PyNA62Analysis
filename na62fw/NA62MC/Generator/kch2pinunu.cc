///////////////////////////////////////////////////////////////////
// Decay generator: K+ --> pi+ nu nu
// Source: private comminucation with Christopher Smith
// Averaged Ke3 form factor slopes from arXiv:0801.1817
// NA62 C++ version: E.Goudzovski (eg@hep.ph.bham.ac.uk) 12/10/2015

#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "masses.hh"
#include <iostream>

using namespace std;

void kch2pinunu(TLorentzVector Mom4Parent) {

  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

  double Emax = 0.5*(SQMKCH+SQMPI)/MKCH;
  double Epi, wtme;

  do {
    Epi        = MPI + RandomDecay->Uniform()*(Emax-MPI);
    double q2  = SQMKCH+SQMPI-2.0*MKCH*Epi; // =(PK-Ppi)^2
    double z   = q2/SQMKCH;
    double lmb = sqrt(RPI2*RPI2 - 2.0*(z+1)*RPI2 + (z-1)*(z-1));
    double V   = 1.0 + 0.99*0.0252*q2/SQMPI + 0.5*0.99*0.0016*(q2/SQMPI)*(q2/SQMPI);
    wtme       = lmb * lmb * lmb * V * V;
  }
  while (0.8 * RandomDecay->Uniform() > wtme);

  double Ppi = sqrt(Epi*Epi-SQMPI);

  Double_t x=0, y=0, z=0;
  RandomDecay->Sphere(x, y, z, 1.0);
  TLorentzVector Mom4pi (Ppi*x, Ppi*y, Ppi*z, Epi);

  // Save the GeneParts
  mcadd4gencpp(PDG_ID_pip, Mom4pi);

  // Boost decay products into lab frame
  Mom4pi.Boost(Mom4Parent.BoostVector());
  mcadd4cpp(PDG_ID_pip, Mom4pi);
}
