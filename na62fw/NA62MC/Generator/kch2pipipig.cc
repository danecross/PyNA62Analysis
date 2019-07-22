
////////////////////////////////////////////////////////////////////////////////////////////
//
//	NA62 decay generator: K+ --> pi+ pi+ pi- gamma
//	Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) and Sergey Kholodenko (sergey.kholodenko@cern.ch) May 2018
//
////////////////////////////////////////////////////////////////////////////////////////////

#include "../include/RandomGenerator.hh"
#include "mcadd4cpp.hh"
#include "masses.hh"
#include "TGenPhaseSpace.h"
#include <iostream>

using namespace std;

double k3pig_matrix_element(TLorentzVector p1, TLorentzVector p2, TLorentzVector p3, TLorentzVector k);


void kch2pipipig(TLorentzVector Mom4Parent) {


  static TGenPhaseSpace event;
  static int gen_phase_space_calls = 0;
  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();


  if (!gen_phase_space_calls) {
    static TLorentzVector PKaon(0.0, 0.0, 0.0, MKCH); // kaon decay at rest
    static double masses[4] = {MPI, MPI, MPI, 0}; //SK: 3 charged pions (+,+,-) and one gamma
    event.SetDecay(PKaon, 4, masses);
    gen_phase_space_calls++;
  }



  TLorentzVector *p1,*p2,*p3,*k;
  double weight_max = 0.121; //SK: max value = 0.120415 after simulating 100M events;
  double M2_max = 43500; //SK: max value = 43448 after simulating 100M events;

  double weight;
  double M2 = 0.;
  do {
	weight = event.Generate();
	p1 = event.GetDecay(0);
	p2 = event.GetDecay(1);
	p3 = event.GetDecay(2);
	k  = event.GetDecay(3);

	if(k->E() < 0.001) continue; //SK: generating for Eg > 1 MeV/c2 in Kaon RF

	M2 = k3pig_matrix_element(*p1, *p2, *p3, *k); // calculating matrix element;
//	cout << " M2*weight " << M2*weight << "  RandomDecay->Uniform()* .. " << 1.05*M2_max*weight_max*RandomDecay->Uniform() << endl;
	}
	while (M2_max*weight_max*RandomDecay->Uniform() > M2 * weight);

  // Save the GeneParts
  mcadd4gencpp(PDG_ID_pip, *p1);
  mcadd4gencpp(PDG_ID_pip, *p2);
  mcadd4gencpp(PDG_ID_pim, *p3);
  mcadd4gencpp(PDG_ID_gam, *k);

  // Boost decay products into lab frame
  p1->Boost(Mom4Parent.BoostVector());
  p2->Boost(Mom4Parent.BoostVector());
  p3->Boost(Mom4Parent.BoostVector());
  k->Boost(Mom4Parent.BoostVector());

  // Pass decay products to Geant4
  mcadd4cpp(PDG_ID_pip, *p1);
  mcadd4cpp(PDG_ID_pip, *p2);
  mcadd4cpp(PDG_ID_pim, *p3);
  mcadd4cpp(PDG_ID_gam, *k);
  
}

// ========================================================================================
// Matrix element from arXiv:hep-ph/9612412v1. LO

double k3pig_matrix_element(TLorentzVector p1, TLorentzVector p2, TLorentzVector p3, TLorentzVector k)
{
TLorentzVector p4;
p4.SetPxPyPzE(0,0,0,MKCH);
p4 *= (-1); // according to arXiv:hep-ph/9612412v1 kaon 4momenta determined as `-p4`

// constants from K->3pi data analysis: Phys.Lett. B649 (2007) 349-358
double kg = -21.134 * 1E-2;
double kh = 1.848 * 1E-2;
double kk = -0.463*1E-2;

double s0 = (1/3)*MKCH*MKCH + MPI*MPI;
double s1 = (p4 - p1)*(p4 - p1);
double s2 = (p4 - p2)*(p4 - p2);
double s3 = (p4 - p3)*(p4 - p3);

double u = (s3 - s0) / (MPI*MPI);
double v = (s1 - s2) / (MPI*MPI);

double A2 = 1 + kg*u + kh*u*u + kk*v*v; // K->3pi amplitude^2 parametrization

double dAdu = 0.5*(1/sqrt(A2))*(kg+2*kh*u);
double dAdv = 0.5*(1/sqrt(A2))*(2*kk*v);

double t1 = k*p1;
double t2 = k*p2;
double t3 = k*p3;
double t4 = k*p4;

TLorentzVector sigma = MPI * ((1/t1)*p1 + (1/t2)*p2 + (-1/t3)*p3 + (1/t4)*p4);
TLorentzVector lambda12 = (1/MPI)*(t2 - t1) * ((1/t1)*p1 - (1/t2)*p2);
TLorentzVector lambda14 = (1/MPI)*(t4 - t1) * ((1/t1)*p1 - (1/t4)*p4);
TLorentzVector lambda24 = (1/MPI)*(t4 - t2) * ((1/t2)*p2 - (1/t4)*p4);

return (sqrt(A2)*sigma + 2*dAdu*lambda12 + dAdv*(lambda14-lambda24))*(sqrt(A2)*sigma + 2*dAdu*lambda12 + dAdv*(lambda14-lambda24));

}

