////////////////////////////////////////////////////////////////////////////
// Pi0 double Dalitz decay generator: pi0 -> e+ e- e+ e-
// NA62 version: Tomas Husek (t.h@cern.ch) 07/11/2017
//
// Routine generate_pi0ddal_ is called from pi0decays.F,
// decay options (macro setting /decay/pizeroDecay):
//
// 31: pi0-->e+e-e+e-    (double Dalitz decay: no radiative corrections);
// 32: pi0-->e+e-e+e-(g) (double Dalitz decay: with NLO radiative corrections);
//
// Any other value will mean that pi0 will be passed to Geant4.
//
// LXPLUS SLC6 average CPU time consumption per event:
// 31: ~ 10 s/evt
// 32: ~ 20 s/evt
//
// NLO radiative corrections based on calculations done by
// Pablo Sanchez Puertas (sanchezp@ipnp.troja.mff.cuni.cz)
/////////////////////////////////////////////////////////////////////////////

#include "pi0DDalitz.hh"
#include "rndmcpp.hh"
#include "mcadd4cpp.hh"

#include <iostream>			// std::cout
// #include <stdio.h>		// printf()
#include <math.h>
#include <complex>
#include <vector>
#include <tuple>

// ROOT
#include "TLorentzVector.h"
// #include "TGenPhaseSpace.h"

// LoopTools
#include "clooptools.h"

#define OUT


// this routine is called from pi0decays.F
void generate_pi0ddal_(double ppi0 [], int * pzmode)
{
    TLorentzVector pi0mom(ppi0[0], ppi0[1], ppi0[2], ppi0[3]);

    if (*pzmode == 31 || *pzmode == 32)
	{
		bool radcor = (*pzmode == 32);

		TLorentzVector pep1, pem1, pep2, pem2;
		generate_ddalitz(pep1, pem1, pep2, pem2, radcor);

		dboost(pi0mom, MP0, pep1);
		dboost(pi0mom, MP0, pem1);
		dboost(pi0mom, MP0, pep2);
		dboost(pi0mom, MP0, pem2);

		mcadd4cpp(PDG_ID_elp, pep1);
		mcadd4cpp(PDG_ID_elm, pem1);
		mcadd4cpp(PDG_ID_elp, pep2);
		mcadd4cpp(PDG_ID_elm, pem2);
    }
	return;
}

// generate 4-momenta of the e+, e-, e+, e-
void generate_ddalitz(TLorentzVector &pep1,
					  TLorentzVector &pem1,
					  TLorentzVector &pep2,
					  TLorentzVector &pem2,
					  bool radcor)
{
    TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();

	/// following variables are defined static, as they need to be initialized only once
	static double M = MP0;				// define masses of a decaying pseudoscalar,
	static double ma = MEL;				// the first
	static double mb = MEL;				// and the second lepton pairs

	/// uncomment if TGenPhaseSpace is to be used
/*	static TLorentzVector ppion(0.0, 0.0, 0.0, M);	// pi0 at rest
	static double masses[4] = {ma, ma, mb, mb};		// e+, e-, e+, e-
	static TGenPhaseSpace pi0DDEvent;
*/
	static bool first_ddalitz_call = true;

	static double maxweight = 0.09732;	// maximal generator weight for TGenPhaseSpace
	static double MDD2max = 1.3539e-8;	// maximal matrix element squared (should work for any form factor, since maximum happens for minimal s)
	static double maxcorrection = 1.62;	// maximal radiative correction

	FDoubleDalitz DD;					// instantiate Double Dalitz class containing all the expressions

	/// initialize the generator during the first call
	if (first_ddalitz_call)
	{
		if (radcor)						// for radiative corrections, LoopTools need to be initialized and tested
		{
			std::cout << std::endl;
			ltini();
			std::cout << std::endl;
//			std::cout << std::endl << "delta_NLO = " << DD.delta_NLO(0.497611, MEL, MEL, 0.03, 0.009, -0.8, 0.06, 120. / 180 * PI, EC, LAMBDA, MUSQ); // should give a reference value of -0.404986;
			if (fabs(1 - DD.delta_NLO(0.497611, MEL, MEL, 0.03, 0.009, -0.8, 0.06, 120. / 180 * PI, EC, LAMBDA, MUSQ) / (-0.404986)) > 0.001)
				{ std::cout << "\nError: LoopTools not initialized properly"; }
			std::cout << std::endl; // new line after the first LoopTools run due to better looking printout
		}
//		std::cout << "LO = " << DD.LO(0.497611, MEL, MEL, 0.03, 0.09, 0.8, 0.6, 20. / 180 * PI) << std::endl << std::endl; // should give a reference value of {2.08671 + 16.6131 - 7.51732 = 11.1825, 13.1286 with FF};
		if (fabs(1 - DD.LO(0.497611, MEL, MEL, 0.03, 0.09, 0.8, 0.6, 20. / 180 * PI) / 13.1286) > 0.001
		 && fabs(1 - DD.LO(0.497611, MEL, MEL, 0.03, 0.09, 0.8, 0.6, 20. / 180 * PI) / 11.1825) > 0.001)
				{ std::cout << "Error: DD_LO problem\n\n"; }

		/// uncomment if TGenPhaseSpace is to be used
//		pi0DDEvent.SetDecay(ppion, 4, masses); // setup the event generator

		first_ddalitz_call = false;
	}

	TLorentzVector p1, p2, p3, p4;		// 4-momenta of decay products (leptons)

	double s12, s34, y12, y34, phi;		// 5 kinematic variables
	double s14, s23;					// 2 additional exchange variables (2<->4)
	double weight = maxweight;			// pi0DDEvent weight, let it have no effect when TGenPhaseSpace is not used
	double MDD2 = MDD2max;				// matrix element squared, initialize on its maximum value
	double correction = maxcorrection;	// radiative corrections for specific kinematics (1 + delta_NLO(...))
										// let corrections have no effect when radcor = false
	do {								// loop until event accepted by matrix element squared at LO (+ NLO)
//		weight = pi0DDEvent.Generate();		// generate momenta and get weight using TGenPhaseSpace
//		p1 = *pi0DDEvent.GetDecay(0);		// collect generated 4-momenta
//		p2 = *pi0DDEvent.GetDecay(1);
//		p3 = *pi0DDEvent.GetDecay(2);
//		p4 = *pi0DDEvent.GetDecay(3);
		DD.GenerateEventKinematics(M, ma, mb, OUT s12, OUT s34, OUT y12, OUT y34, OUT phi, OUT s14, OUT s23); // generate kinematics only with 1/(s12*s34*s14*s23) distribution -> faster for this purpose

		// compute kinematic variables from 4-momenta, necassary for TGenPhaseSpace
//		if (!DD.ConvertToKinematics(p1, p2, p3, p4, M, ma, mb, OUT s12, OUT s34, OUT y12, OUT y34, OUT phi)) { weight = 0; continue; }	// reject event

		// calculates matrix element squared at LO (including by default VMD form factor)
		MDD2 = s12*s34*s14*s23 * DD.LO(M, ma, mb, s12, s34, y12, y34, phi); // such matrix element squared is rather flat

		// find maximal values
//		if (weight > maxweight) { maxweight = weight; std::cout << maxweight << std::endl; }
//		if (MDD2 > MDD2max) { MDD2max = MDD2; std::cout << MDD2max << std::endl; }

		// permutation of electrons (p2 and p4) to check consistency
/*		if (!DD.ConvertToKinematics(p1, p4, p3, p2, M, ma, mb, OUT s12, OUT s34, OUT y12, OUT y34, OUT phi)) { weight = 0; continue; }	// reject event
		double MDD2_b = DD.LO(M, ma, mb, s12, s34, y12, y34, phi);
		if (1-MDD2_b/MDD2>0.001) std::cout << "Warning: exchanged probabilities don't match!\n";
*/
		if (radcor)
		{
			correction = 1 + DD.delta_NLO(M, ma, mb, s12, s34, y12, y34, phi, EC, LAMBDA, MUSQ);
//			if (correction > maxcorrection) { maxcorrection = correction; std::cout << maxcorrection << std::endl; }
		}
	} while (RandomDecay->Uniform() > (weight / maxweight * MDD2 / MDD2max * correction / maxcorrection));	// accept/reject

	if (radcor) { clearcache(); }
//	ltexi();	can be called to get LoopTools error summary - not suitable for MC

	DD.GenerateFourMomenta(M, ma, mb, s12, s34, y12, y34, phi, OUT p1, OUT p2, OUT p3, OUT p4);

	// copy momenta from TGenPhaseSpace internal structure -> use correct pairing!
	pep1 = p1;
	pem1 = p2;
	pep2 = p3;
	pem2 = p4;

	mcadd4gencpp(PDG_ID_elp, pep1, 1);
	mcadd4gencpp(PDG_ID_elm, pem1, 1);
	mcadd4gencpp(PDG_ID_elp, pep2, 1);
	mcadd4gencpp(PDG_ID_elm, pem2, 1);

	return;
}

//##################
// BASIC FUNCTIONS #
//##################

// gives the real part of the logarithm for z real (with potentially negative argument)
inline double log_re(std::complex<double> z)
{
	//	return log(fabs(z));
	return RE(log(z));
}

// gives the real part of the logarithm SQUARED for real argument
inline double log2_re(std::complex<double> z)
{
	//	if (z > 0) { return pow(log(z), 2); }
	//	else { return pow(log_re(z), 2) - pow(PI, 2); }
	return RE(pow(log(z), 2));
}

// gives the real part of dilogarithm for real argument
double dilog_re(double z)
{
	if (z == 0) { return 0; }
	if (z == 1) { return pow(PI, 2) / 6; }
	if (z == -1) { return -pow(PI, 2) / 12; }
	if (fabs(z) > 1) { return -dilog_re(1 / z) - pow(PI, 2) / 6 - log2_re(-z) / 2; }
	if (z < -0.5) { return -dilog_re(-z) + dilog_re(pow(z, 2)) / 2; }
	if (z > 0.5) { return -dilog_re(1 - z) + pow(PI, 2) / 6 - log(z)*log(1 - z); }

	double result = 0.;
	double z_i = z;

	for (int i = 1; i <= 150; i++)
	{
		result += z_i / pow(i, 2);
		z_i *= z;
	}

	return result;
}

// Kallen triangle function
inline double KallenLambda(double a, double b, double c)
{
	return a*a + b*b + c*c - 2 * a*b - 2 * a*c - 2 * b*c;
}

//##################
// EVENT GENERATOR #
//##################

inline double ConvertRandomToRange(double random, double random_min, double random_max)
{
	return random_min + random * (random_max - random_min);
}

void FDoubleDalitz::GenerateEventKinematics(double M, double ma, double mb,
	double &s12, double &s34, double &y12, double &y34, double &phi,
	double &s14, double &s23)
{
	TRandom3* RandomDecay = RandomGenerator::GetInstance()->GetRandomDecay();

	double lmbd_ = 0., sN = 0.;
	double m = ma;

	double m2 = pow(m, 2);
	double M2 = pow(M, 2);

	double s_ma_min = (pow(2 * ma, 2));
	double s_mb_min = (pow(2 * mb, 2));

	double s12min = log(pow(2 * ma, 2));
	double s34min = log(pow(2 * mb, 2));
	double s12max = log(pow(M - 2 * mb, 2));
	double s34max = log(pow(M - 2 * ma, 2));

	double lmbd_max = sqrt(KallenLambda(pow(M, 2), pow(2 * ma, 2), pow(2 * mb, 2)));

	do {
	start:
		s12 = exp(ConvertRandomToRange(RandomDecay->Uniform(), s12min, s12max));
		s34 = exp(ConvertRandomToRange(RandomDecay->Uniform(), s34min, s34max));
		if (s34 > pow(M - sqrt(s12), 2)) { goto start; }

		y12 = ConvertRandomToRange(RandomDecay->Uniform(), -1, 1);
		if (y12*y12 > 1 - s_ma_min / s12) { goto start; }

		y34 = ConvertRandomToRange(RandomDecay->Uniform(), -1, 1);
		if (y34*y34 > 1 - s_mb_min / s34) { goto start; }

		phi = ConvertRandomToRange(RandomDecay->Uniform(), 0, 2 * PI);

		lmbd_ = sqrt(KallenLambda(M2, s12, s34));
		sN = 0.25 * (
			8 * m2
			+ (M2 - s12 - s34)*(1 - y12*y34)
			+ 2 * sqrt((-4 * m2 + s12 - s12*pow(y12, 2))*(-4 * m2 + s34 - s34*pow(y34, 2)))*cos(phi));

		s14 = sN + lmbd_*(y12 - y34) * 0.25;
		s23 = sN - lmbd_*(y12 - y34) * 0.25;

	} while (RandomDecay->Uniform() > lmbd_ / lmbd_max * s_ma_min / s14 * s_mb_min / s23);

	return;
}

void FDoubleDalitz::GenerateFourMomenta(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi,
	TLorentzVector &p1, TLorentzVector &p2, TLorentzVector &p3, TLorentzVector &p4)
{
	TRandom3* RandomDecay = RandomGenerator::GetInstance()->GetRandomDecay();

	double lmbd12N = sqrt(1 - (pow(2 * ma, 2)) / s12);
	double lmbd34N = sqrt(1 - (pow(2 * mb, 2)) / s34);
	double lmbdN = 1 / pow(M, 2) * sqrt(KallenLambda(pow(M, 2), s12, s34));
	double delta = (s12 - s34) / pow(M, 2);

	double E1 = M / 4 * (1 + delta + lmbdN*y12);
	double E2 = M / 4 * (1 + delta - lmbdN*y12);
	double E3 = M / 4 * (1 - delta + lmbdN*y34);
	double E4 = M / 4 * (1 - delta - lmbdN*y34);

	p1.SetPxPyPzE(-.5*sqrt(s12*(pow(lmbd12N, 2) - pow(y12, 2))), M / 4 * (lmbdN + (1 + delta) * y12), 0, E1);
	p2.SetPxPyPzE(.5*sqrt(s12*(pow(lmbd12N, 2) - pow(y12, 2))), M / 4 * (lmbdN - (1 + delta) * y12), 0, E2);
	p3.SetPxPyPzE(-.5*sqrt(s34*(pow(lmbd34N, 2) - pow(y34, 2))) * cos(phi), -M / 4 * (lmbdN + (1 - delta) * y34), .5*sqrt(s34*(pow(lmbd34N, 2) - pow(y34, 2))) * sin(phi), E3);
	p4.SetPxPyPzE(.5*sqrt(s34*(pow(lmbd34N, 2) - pow(y34, 2))) * cos(phi), -M / 4 * (lmbdN - (1 - delta) * y34), -.5*sqrt(s34*(pow(lmbd34N, 2) - pow(y34, 2))) * sin(phi), E4);

	double phi1 = 2 * PI * RandomDecay->Uniform();
	double phi2 = 2 * PI * RandomDecay->Uniform();
	double phi3 = 2 * PI * RandomDecay->Uniform();
	TVector3 xx(1., 0., 0.);
	TVector3 zz(0., 0., 1.);

	p1.Rotate(phi1, zz);
	p2.Rotate(phi1, zz);
	p3.Rotate(phi1, zz);
	p4.Rotate(phi1, zz);
	xx.Rotate(phi1, zz);

	p1.Rotate(phi2, xx);
	p2.Rotate(phi2, xx);
	p3.Rotate(phi2, xx);
	p4.Rotate(phi2, xx);
	zz.Rotate(phi2, xx);

	p1.Rotate(phi3, zz);
	p2.Rotate(phi3, zz);
	p3.Rotate(phi3, zz);
	p4.Rotate(phi3, zz);

	return;
}

//##################
// OTHER FUNCTIONS #
//##################

double FDoubleDalitz::array_product(double *a1, double *a2, int N)
{
	double product = 0.;
	for (int i = 0; i < N; i++) { product += a1[i] * a2[i]; }

	return product;
}

std::vector<double> FDoubleDalitz::C_get(double p2, double q2, double pq2, double m12, double m22, double m32)
{
	return {
		0., // start indexing from 1
		RE(C0i(cc0, p2, q2, pq2, m12, m22, m32)),
		RE(C0i(cc1, p2, q2, pq2, m12, m22, m32)),
		RE(C0i(cc2, p2, q2, pq2, m12, m22, m32)),
		RE(C0i(cc00, p2, q2, pq2, m12, m22, m32)),
		RE(C0i(cc11, p2, q2, pq2, m12, m22, m32)),
		RE(C0i(cc12, p2, q2, pq2, m12, m22, m32)),
		RE(C0i(cc22, p2, q2, pq2, m12, m22, m32))
	};
}

std::vector<double> FDoubleDalitz::D_get(double p12, double p22, double p32, double p42, double q12, double q22, double m12, double m22, double m32, double m42)
{
	return {
		0., // start indexing from 1
		RE(D0i(dd0, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd1, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd2, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd3, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd00, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd11, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd12, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd13, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd22, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd23, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42)),
		RE(D0i(dd33, p12, p22, p32, p42, q12, q22, m12, m22, m32, m42))
	};
}

//################
// LEADING ORDER #
//################

// converts four-momenta into kinematical variables
// in these variables, the fermion-antifermion pairs (+, -) should be named as (1, 2) and (3, 4)
bool FDoubleDalitz::ConvertToKinematics(TLorentzVector &p1, TLorentzVector &p2, TLorentzVector &p3, TLorentzVector &p4,
	double M, double ma, double mb,
	double &s12, double &s34, double &y12, double &y34, double &phi)
{
	s12 = (p1 + p2).M2();
	s34 = (p3 + p4).M2();

	double lmbdN = 1 / pow(M, 2) * sqrt(KallenLambda(pow(M, 2), s12, s34));
	double lmbd12N = sqrt(1 - (4 * pow(ma, 2)) / s12);
	double lmbd34N = sqrt(1 - (4 * pow(mb, 2)) / s34);
	double zN = (pow(M, 2) - s12 - s34) / pow(M, 2);
	double wN = 2 / pow(M, 2) * sqrt(s12*s34);

	y12 = 2 * (p1 - p2) * (p3 + p4) / (lmbdN*pow(M, 2));
	y34 = 2 * (p3 - p4) * (p1 + p2) / (lmbdN*pow(M, 2));

	//	not the unique way to get an angle between 0 and 2Pi, need sgn(sin(phi)) to know more
	// be sure that argument of acos() is between -1 and 1
	phi = acos(MIN(0.9999999999, MAX(-0.9999999999, -(2 * (p1 - p2)*(p3 - p4) - pow(M, 2)*zN*y12*y34) / (pow(M, 2)*wN*sqrt((pow(lmbd12N, 2) - pow(y12, 2))*(pow(lmbd34N, 2) - pow(y34, 2)))))));

	double LCterm = + p1(0)*(p2(1)*(p3(2)*p4(3) - p3(3)*p4(2)) + p2(2)*(p3(3)*p4(1) - p3(1)*p4(3)) + p2(3)*(p3(1)*p4(2) - p3(2)*p4(1)))
					- p1(1)*(p2(0)*(p3(2)*p4(3) - p3(3)*p4(2)) + p2(2)*(p3(3)*p4(0) - p3(0)*p4(3)) + p2(3)*(p3(0)*p4(2) - p3(2)*p4(0)))
					- p1(2)*(p2(1)*(p3(0)*p4(3) - p3(3)*p4(0)) + p2(0)*(p3(3)*p4(1) - p3(1)*p4(3)) + p2(3)*(p3(1)*p4(0) - p3(0)*p4(1)))
					- p1(3)*(p2(1)*(p3(2)*p4(0) - p3(0)*p4(2)) + p2(2)*(p3(0)*p4(1) - p3(1)*p4(0)) + p2(0)*(p3(1)*p4(2) - p3(2)*p4(1)));
	//	double sin_phi = - LCterm * 16 / (pow(M,4)*lmbdN*wN*sqrt((pow(lmbd12N,2)-pow(y12,2))*(pow(lmbd34N,2)-pow(y34,2))));
	if (-LCterm < 0) phi = -phi + 2 * PI;

	// check that resulting values are valid
	if ((TMath::IsNaN(s12) != 0) || (TMath::IsNaN(s34) != 0) || (TMath::IsNaN(y12) != 0) || (TMath::IsNaN(y34) != 0) || (TMath::IsNaN(phi) != 0))
	{
		std::cout << "Warning: calculation of kinematical variables failed!\n"; //  1.5 fails per 10 kEvents in average due to NaN phi -> corrected by using MIN and MAX macros
		printf("(%g, %g, %g, %g, %g)\n", s12, s34, y12, y34, phi);
		return false;
	}

	return true;
}

// phase-space
double FDoubleDalitz::DPhase(int S, double M, double s12, double s34)
{
	return 1 / (8 * M) * S / pow(4 * PI, 6) * sqrt(KallenLambda(1, s12 / pow(M, 2), s34 / pow(M, 2)));
}

void FDoubleDalitz::SetBasicVariables(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi)
{
	lmbd12 = sqrt(1 - (4 * pow(ma, 2)) / s12);
	lmbd34 = sqrt(1 - (4 * pow(mb, 2)) / s34);
	lmbd = sqrt(KallenLambda(pow(M, 2), s12, s34));
	z = (pow(M, 2) - s12 - s34) / pow(M, 2);
	Xi = (2 * sqrt(s12*s34*(-pow(y12, 2) + pow(lmbd12, 2))*(-pow(y34, 2) + pow(lmbd34, 2)))*cos(phi)) / pow(M, 2);

	return;
}

void FDoubleDalitz::SetSVariables(double M, double ma, double mb, double s12, double s34, double y12, double y34)
{
	s13 = s13_(M, s12, s34, y12, y34, lmbd12, lmbd34, lmbd, z, Xi);
	s14 = s13_(M, s12, s34, y12, -y34, lmbd12, lmbd34, lmbd, z, -Xi);
	s23 = s13_(M, s12, s34, -y12, y34, lmbd12, lmbd34, lmbd, z, -Xi);
	s24 = s13_(M, s12, s34, -y12, -y34, lmbd12, lmbd34, lmbd, z, Xi);

	s134 = s134_(M, ma, s12, s34, y12, lmbd);
	s234 = s134_(M, ma, s12, s34, -y12, lmbd);
	s123 = s134_(M, mb, s34, s12, y34, lmbd);
	s124 = s134_(M, mb, s34, s12, -y34, lmbd);

	return;
}

// calculates the exchange variables
void FDoubleDalitz::DirToExc(double M, double m,
	double s12, double s34, double y12, double y34, double phi,
	double &s14, double &s23, double &y14, double &y23, double &phiEx)
{
	lmbd = sqrt(KallenLambda(pow(M, 2), s12, s34));
	double sN = (
		8 * pow(m, 2)
		+ (pow(M, 2) - s12 - s34)*(1 - y12*y34)
//		+ lmbd*(y12 - y34)
		+ 2 * sqrt((-4 * pow(m, 2) + s12 - s12*pow(y12, 2))*(-4 * pow(m, 2) + s34 - s34*pow(y34, 2)))*cos(phi)
		) / 4.;

	s14 = sN + lmbd*(y12 - y34) / 4.;
	s23 = sN - lmbd*(y12 - y34) / 4.;

	double lmbdEx = sqrt(KallenLambda(pow(M, 2), s14, s23));
	y14 = 1 / lmbdEx * ((y12 + y34) * lmbd / 2. + (s12 - s34));
	y23 = 1 / lmbdEx * ((y12 + y34) * lmbd / 2. - (s12 - s34));

	double mySin = -lmbd / lmbdEx * sin(phi) * sqrt(
		(s12 * (1 - pow(y12, 2)) - 4 * pow(m, 2)) * (s34 * (1 - pow(y34, 2)) - 4 * pow(m, 2))
		/ (s14 * (1 - pow(y14, 2)) - 4 * pow(m, 2)) / (s23 * (1 - pow(y23, 2)) - 4 * pow(m, 2))
	);

	double myCos = (-4 * pow(m, 2) + s12 + s34 + (pow(M, 2) - s14 - s23)*y14*y23
		- ((pow(M, 2) - s12 - s34)*(1 + y12*y34)) / 2.
		+ sqrt((-4 * pow(m, 2) + s12 - s12*pow(y12, 2))*(-4 * pow(m, 2) + s34 - s34*pow(y34, 2)))*cos(phi))
		/ (2.*sqrt((-4 * pow(m, 2) + s14 - s14*pow(y14, 2))*(-4 * pow(m, 2) + s23 - s23*pow(y23, 2))));

	if (myCos > 0) { phiEx = asin(mySin); }
	else { phiEx = PI - asin(mySin); }

	return;
}

// direct term
double FDoubleDalitz::Direct(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);

	return pow(lmbd, 2) / (s12 * s34) * (2 - pow(lmbd12, 2) - pow(lmbd34, 2) + pow(y12, 2) + pow(y34, 2) + (pow(lmbd12, 2) - pow(y12, 2))*(pow(lmbd34, 2) - pow(y34, 2))*pow(sin(phi), 2));
}

// exchange and interference terms
std::tuple<double, double> FDoubleDalitz::Identical(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double s14, double s23)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);

	double lmbd14 = sqrt(-pow(8 * ma*mb, 2) + pow(pow(M, 2)*(-1 + y12*y34)*z - (y12 - y34)*lmbd - pow(M, 2)*Xi, 2)) / 4.;
	double lmbd23 = sqrt(-pow(8 * ma*mb, 2) + pow(pow(M, 2)*(-1 + y12*y34)*z + (y12 - y34)*lmbd - pow(M, 2)*Xi, 2)) / 4.;
	double lmbdEx = sqrt(KallenLambda(pow(M, 2), s14, s23));
	double y14 = (y12 + y34) / 2. + (s12 - s34) / lmbd;
	double y23 = (y12 + y34) / 2. - (s12 - s34) / lmbd;

	double EXCH = ((2 - pow(lmbd14, 2) / pow(s14, 2) - pow(lmbd23, 2) / pow(s23, 2))*pow(lmbdEx, 2)
		+ pow(lmbd, 2)*(pow(y14, 2) + pow(y23, 2)
			+ (s12*s34*(-pow(y12, 2) + pow(lmbd12, 2))*(-pow(y34, 2) + pow(lmbd34, 2))*pow(sin(phi), 2)) / (s14*s23))) / (s14*s23);

	double INT = pow(lmbd, 2) / (8.*s12*s14*s23*s34)*(
		128 * pow(ma, 4) + 16 * pow(ma, 2)*(y12 + y34)*(s12*y12 + s34*y34)
		- 4 * s12*s34*(1 + y12*y34)*(2 - pow(y12, 2) - pow(y34, 2))
		+ pow(M, 2)*(32 * pow(ma, 2) - pow(M, 2)*pow(y12 + y34, 2)*z)*Xi + 2 * pow(M, 4)*pow(Xi, 2));

	return std::make_tuple(EXCH, INT);
}

// leading order (LO)
double FDoubleDalitz::LO(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi)
{
//	double PHASE = DPhase(1, M, s12, s34);

	double DIR = Direct(M, ma, mb, s12, s34, y12, y34, phi);
	double EXCH = 0., INT = 0.;

	double s14 = 0., s23 = 0;

	// in case both lepton pairs are of the same flavour, there are additional diagrams
	if (ma == mb)
	{
	    double y14 = 0., y23 = 0., phiEx = 0.;
		DirToExc(M, ma, s12, s34, y12, y34, phi, OUT s14, OUT s23, OUT y14, OUT y23, OUT phiEx);
		std::tie(EXCH, INT) = Identical(M, ma, mb, s12, s34, y12, y34, phi, s14, s23);
	}

	return (DIR*pow(FF(s12, s34), 2) + EXCH*pow(FF(s14, s23), 2) + INT*FF(s12, s34)*FF(s14, s23)); // * PHASE * common factor
}

//#################
// MASTER FORMULA #
//#################

// master formula for radiative corrections (picks all the pieces)
double FDoubleDalitz::delta_NLO(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double EC, double LAMBDA, double MUSQ)
{
	// leading order
//	double PHASE = DPhase(1, M, s12, s34);

	double DIR = Direct(M, ma, mb, s12, s34, y12, y34, phi);
	double EXCH = 0., INT = 0.;

	// bremsstrahlung
	double BREM = Bremsstrahlung(M, ma, mb, s12, s34, y12, y34, phi, EC, LAMBDA);

	// 2 Re(VPol)
	double VPDIR = 2 * (VPol_Re(s12) + VPol_Re(s34));
	double VPEXCH = 0., VPINT = 0.;

	// 2 Re(F1)
	double DIRVF1 = 2 * (F1_Re(s12, ma, LAMBDA) + F1_Re(s34, mb, LAMBDA));
	double EXCHVF1 = 0., INTVF1 = 0.;

	// Re(F2)
	double DIRVF2 = DirF2_Re(M, ma, mb, s12, s34, y12, y34, phi);
	double EXCHVF2 = 0., INTVF2 = 0.;

	// Re(3p)!!!
	double TRID = 2 * ThreePointD_Re(M, ma, mb, s12, s34, y12, y34, phi, 3, MUSQ);
	double TRIE = 0., TRII = 0.;

	double FOURD = 2 * FourPointD_Re(M, ma, mb, s12, s34, y12, y34, phi);
	double FOURE = 0., FOURI = 0.;

	double PENT1D = Pent1DT1(M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, DIR, 0, 0);
	double PENT1E = 0., PENT1I = 0.;

	double PENTDT = PENTDTev(M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, 0, 0, 1);
	double PENTET = 0., PENTIT = 0.;

	// in case both lepton pairs are of the same flavour, there are additional diagrams
	if (ma == mb)
	{
		double s14 = 0., s23 = 0., y14 = 0., y23 = 0., phiEx = 0.;
		DirToExc(M, ma, s12, s34, y12, y34, phi, OUT s14, OUT s23, OUT y14, OUT y23, OUT phiEx);

		std::tie(EXCH, INT) = Identical(M, ma, mb, s12, s34, y12, y34, phi, s14, s23);

		VPEXCH = 2 * (VPol_Re(s14) + VPol_Re(s23));
		VPINT = (VPDIR + VPEXCH) / 2;

		EXCHVF1 = 2 * (F1_Re(s14, ma, LAMBDA) + F1_Re(s23, mb, LAMBDA));
		INTVF1 = (DIRVF1 + EXCHVF1) / 2;

		EXCHVF2 = DirF2_Re(M, mb, ma, s14, s23, y14, y23, phiEx);
		INTVF2 = IntF2_Re(M, ma, mb, s12, s34, y12, y34, phi, s14, s23) + IntF2_Re(M, mb, ma, s14, s23, y14, y23, phiEx, s12, s34);

		TRIE = 2 * ThreePointD_Re(M, ma, mb, s14, s23, y14, y23, phiEx, 3, MUSQ);
		TRII = 2 * (ThreePointINT_Re(M, ma, mb, s12, s34, y12, y34, phi, 3, MUSQ) + ThreePointINT_Re(M, ma, mb, s14, s23, y14, y23, phiEx, 3, MUSQ));

		FOURE = 2 * FourPointD_Re(M, ma, mb, s14, s23, y14, y23, phiEx);
		FOURI = 2 * (FourPointInt_Re(M, ma, mb, s12, s34, y12, y34, phi) + FourPointInt_Re(M, ma, mb, s14, s23, y14, y23, phiEx));

		PENT1E = Pent1DT1(M, mb, ma, s14, s23, y14, y23, phiEx, LAMBDA, EXCH, 0, 0);
		PENT1I = Pent1IT1(M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, INT, 0, 0) + Pent1IT1(M, mb, ma, s14, s23, y14, y23, phiEx, LAMBDA, INT, 0, 0);

		PENTET = PENTDTev(M, ma, mb, s14, s23, y14, y23, phiEx, LAMBDA, 0, 0, 1);
		PENTIT = PENTITev(M, ma, mb, s12, s34, y12, y34, phi, s14, s23, y14, y23, phiEx, LAMBDA, 0, 0, 1, 1);
	}

	return (DIRVF2 + FOURD + PENT1D + PENTDT + TRID + DIR*(BREM + DIRVF1 + VPDIR)
			+ EXCHVF2 + FOURE + PENT1E + PENTET + TRIE + EXCH*(BREM + EXCHVF1 + VPEXCH)
			+ INTVF2 + FOURI + PENT1I + PENTIT + TRII + INT*(BREM + INTVF1 + VPINT)) / (DIR + EXCH + INT);
}

//#################
// BREMSSTRAHLUNG #
//#################

double FDoubleDalitz::Bremsstrahlung(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double EC, double LAMBDA)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);

	double lmbd13 = lmbd13_(M, ma, mb, y12, y34, lmbd, z, Xi);
	double lmbd14 = lmbd13_(M, ma, mb, y12, -y34, lmbd, z, -Xi);
	double lmbd23 = lmbd13_(M, ma, mb, -y12, y34, lmbd, z, -Xi);
	double lmbd24 = lmbd13_(M, ma, mb, -y12, -y34, lmbd, z, Xi);

	double z13 = z13_(ma, mb, lmbd13);
	double z14 = z13_(ma, mb, lmbd14);
	double z23 = z13_(ma, mb, lmbd23);
	double z24 = z13_(ma, mb, lmbd24);

	// define s.. = sigma..
	double sgm12 = s12 / (2 * pow(ma, 2))*(1 + lmbd12) - 1;
	double sgm34 = s34 / (2 * pow(mb, 2))*(1 + lmbd34) - 1;
	double sgm13 = sigma13_(ma, lmbd13, z13); // ??? ma, mb
	double sgm14 = sigma13_(ma, lmbd14, z14);
	double sgm23 = sigma13_(ma, lmbd23, z23);
	double sgm24 = sigma13_(ma, lmbd24, z24);

	// define d1 = 1 + delta1234
	double d1 = 1 + delta1234_(M, lmbd, s12, s34, y12);
	double d2 = 1 + delta1234_(M, lmbd, s12, s34, -y12);
	double d3 = 1 + delta1234_(M, lmbd, s34, s12, y34);
	double d4 = 1 + delta1234_(M, lmbd, s34, s12, -y34);

	// define l1 = lambda1234
	double l1 = lambda1234_(M, lmbd, s12, s34, y12, lmbd12);
	double l2 = lambda1234_(M, lmbd, s12, s34, -y12, lmbd12);
	double l3 = lambda1234_(M, lmbd, s34, s12, y34, lmbd34);
	double l4 = lambda1234_(M, lmbd, s34, s12, -y34, lmbd34);

	// calculation of bremsstrahlung contribution
	double IB11 = IB11_(d1, l1, EC, LAMBDA);
	double IB22 = IB11_(d2, l2, EC, LAMBDA);
	double IB33 = IB11_(d3, l3, EC, LAMBDA);
	double IB44 = IB11_(d4, l4, EC, LAMBDA);

	double IB12 = IB12_(M, ma, s12, lmbd12, sgm12, (d1 + l1) / 2, (d1 - l1) / 2, (d2 + l2) / 2, (d2 - l2) / 2, sgm12 * d1 - d2, EC, LAMBDA);
	double IB34 = IB12_(M, mb, s34, lmbd34, sgm34, (d3 + l3) / 2, (d3 - l3) / 2, (d4 + l4) / 2, (d4 - l4) / 2, sgm34 * d3 - d4, EC, LAMBDA);

	double IB13 = IB13_(M, lmbd13, z13, sgm13, (d1 + l1) / 2, (d1 - l1) / 2, (d3 + l3) / 2, (d3 - l3) / 2, sgm13 * d1 - d3, EC, LAMBDA);
	double IB14 = IB13_(M, lmbd14, z14, sgm14, (d1 + l1) / 2, (d1 - l1) / 2, (d4 + l4) / 2, (d4 - l4) / 2, sgm14 * d1 - d4, EC, LAMBDA);
	double IB23 = IB13_(M, lmbd23, z23, sgm23, (d2 + l2) / 2, (d2 - l2) / 2, (d3 + l3) / 2, (d3 - l3) / 2, sgm23 * d2 - d3, EC, LAMBDA);
	double IB24 = IB13_(M, lmbd24, z24, sgm24, (d2 + l2) / 2, (d2 - l2) / 2, (d4 + l4) / 2, (d4 - l4) / 2, sgm24 * d2 - d4, EC, LAMBDA);

	return 4 * PI*ALPHA*(2 * (IB12 + IB34 + IB14 + IB23 - IB13 - IB24) - IB11 - IB22 - IB33 - IB44);
}

//######################
// VACUUM POLARIZATION #
//######################

// FVPol = Fermionic Vacuum Polarization
// Re(FVPol) modulo ALPHA/PI
double FDoubleDalitz::FVPol_Re(double s, double m)
{
	double beta2 = 1 - pow(2 * m, 2) / s;
	return
		-(8. / 9 - beta2 / 3 -
			2 * sqrt(fabs(beta2)) * (0.5 - beta2 / 6) *
			(MAX(0, SIGN(beta2))*atanh(MIN(0.9999999999, sqrt(fabs(beta2))))
				- MIN(0, SIGN(beta2))*atan(1. / sqrt(fabs(beta2)))));
}

// SVPol = Scalar Vacuum Polarization
// Re(SVPol) modulo ALPHA/PI
double FDoubleDalitz::SVPol_Re(double s, double m)
{
	double beta2 = 1 - pow(2 * m, 2) / s;
	return
		-1. / 2 * (1. / 9 + beta2 / 3 -
			2 * sqrt(fabs(beta2)) * (beta2 / 6) *
			(MAX(0, SIGN(beta2))*atanh(MIN(0.9999999999, sqrt(fabs(beta2))))
				- MIN(0, SIGN(beta2))*atan(1. / sqrt(fabs(beta2)))));
}

// VPol = Overall Vacuum Polarization
// Re(VPol = FVPol(mel) + FVPol(mmu) + SVPol(mpi)), it is necessary to multiply by ALPHA/PI
double FDoubleDalitz::VPol_Re(double s)
{
	return ALPHA / PI * (FVPol_Re(s, MEL) + FVPol_Re(s, MMU) + SVPol_Re(s, MPI));
}

//####################
//VERTEX CORRECTIONS #
//####################

// vertex corrections F1 and F2
double FDoubleDalitz::F1_Re(double s, double m, double LAMBDA)
{
	double beta2 = 1 - pow(2 * m, 2) / s;
	double beta = sqrt(beta2);
	double gamma = (1 - beta) / (1 + beta);

	return ALPHA / PI * (
		-1
		- (1 + 2 * beta2) / (4 * beta) * log(gamma)
		- (1 + beta2) / (2 * beta)
		* (-pow(PI, 2) / 2
			+ pow(log(gamma), 2) / 4
			+ dilog_re(1 - gamma))
		+ (1 + (1 + beta2) / (2 * beta) * log(gamma)) * log(m / LAMBDA));
}

inline double FDoubleDalitz::F2_Re(double beta)
{
	return ALPHA / PI * (1 - pow(beta, 2)) * log((1 - beta) / (1 + beta)) / (4 * beta);
}

// include signatures of LO
double FDoubleDalitz::DirF2_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);
	return pow(lmbd, 2) / (s12*s34)*(2 * (2 + pow(y34, 2) - pow(lmbd34, 2))*F2_Re(lmbd12) + 2 * (2 + pow(y12, 2) - pow(lmbd12, 2))*F2_Re(lmbd34));
}

double FDoubleDalitz::IntF2_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double s14, double s23)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);

	double INT21 = pow(lmbd, 2)*F2_Re(lmbd12) / (4.*s12*s14*s23*s34)*(
		2 * s12*(s12 + s34)*(pow(y12, 2) - pow(lmbd12, 2))
		+ 2 * s12*s34*(2 - pow(y12, 2) + y12*y34)*(pow(y34, 2) - pow(lmbd34, 2))
		+ (pow(M, 2)*(32 * pow(ma, 2) - pow(M, 2)*y34*(y12 + y34)*z + 4 * s12*pow(lmbd12, 2))*Xi) / 2.
		+ pow(M, 4)*pow(Xi, 2));
	double INT22 = pow(lmbd, 2)*F2_Re(lmbd34) / (4.*s12*s14*s23*s34)*(
		2 * s34*(s12 + s34)*(pow(y34, 2) - pow(lmbd34, 2))
		+ 2 * s12*s34*(2 + y12*y34 - pow(y34, 2))*(pow(y12, 2) - pow(lmbd12, 2))
		+ (pow(M, 2)*(32 * pow(ma, 2) - pow(M, 2)*y12*(y12 + y34)*z + 4 * s34*pow(lmbd34, 2))*Xi) / 2.
		+ pow(M, 4)*pow(Xi, 2));

	return INT21 + INT22;
}

//################
//3p CORRECTIONS #
//################

// function for the 1D diagram (direct part)
double FDoubleDalitz::ThreePoint1D_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double chi, double MUSQ)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);

	double p134SQ = (pow(M, 2) + 2 * pow(ma, 2) - s12 + s34 + y12*lmbd) / 2.;
	double p2P = (pow(M, 2) + s12 - s34 - y12*lmbd) / 4.;

	double chiPT = -ALPHA / (8.*PI*s34) * (chi + 3 / 2. * log(MUSQ / pow(MRHO, 2))) * (
		4 * y12*lmbd * (2 + pow(y34, 2) - pow(lmbd34, 2))
		+ (4 * pow(lmbd, 2)*(1 - pow(lmbd12, 2))*(2 + pow(y34, 2) - pow(lmbd34, 2))) / (pow(M, 2) - s12 + s34 + y12*lmbd)
		- (pow(M, 4)*y34*z*lmbd * Xi) / (s12*s34));

	setmudim(MUSQ);

	double ConstFF_Re = RE(ALPHA / (4.*PI*s34) * (
		(4 * y12*lmbd * (2 + pow(y34, 2) - pow(lmbd34, 2))
			+ (4 * pow(lmbd, 2)*(1 - pow(lmbd12, 2))*(2 + pow(y34, 2) - pow(lmbd34, 2))) / (pow(M, 2) - s12 + s34 + y12*lmbd)
			- (pow(M, 4)*y34*z*lmbd * Xi) / (s12*s34)
			)
		* (B0i(bb0, p134SQ, 0, pow(ma, 2))
			- C0i(cc00, pow(M, 2), p134SQ, pow(ma, 2), 0, 0, pow(ma, 2))
			- pow(M, 2)*C0i(cc11, pow(M, 2), p134SQ, pow(ma, 2), 0, 0, pow(ma, 2))
			- p2P*C0i(cc12, pow(M, 2), p134SQ, pow(ma, 2), 0, 0, pow(ma, 2))
			)
		- 2 * pow(lmbd, 2)*(1 - pow(lmbd12, 2))*(2 + pow(y34, 2) - pow(lmbd34, 2)) / (pow(M, 2) - s12 + s34 + y12*lmbd)*(
			pow(M, 2)*C0i(cc12, pow(M, 2), p134SQ, pow(ma, 2), 0, 0, pow(ma, 2))
			+ p2P*C0i(cc22, pow(M, 2), p134SQ, pow(ma, 2), 0, 0, pow(ma, 2)))));

	return chiPT + ConstFF_Re;
}

// combines all the 3p1D results together
double FDoubleDalitz::ThreePointD_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double chi, double MUSQ)
{
	return ThreePoint1D_Re(M, ma, mb, s12, s34, y12, y34, phi, chi, MUSQ)
		+ ThreePoint1D_Re(M, ma, mb, s12, s34, -y12, y34, phi + PI, chi, MUSQ)
		+ ThreePoint1D_Re(M, mb, ma, s34, s12, y34, y12, phi, chi, MUSQ)
		+ ThreePoint1D_Re(M, mb, ma, s34, s12, -y34, y12, phi + PI, chi, MUSQ);
}

// interference part: the result of the first two diagrams calculation
double FDoubleDalitz::ThreePointINT_firstTwo_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double chi, double MUSQ)
{
	double m = ma;
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);
	SetSVariables(M, ma, mb, s12, s34, y12, y34);

	double pP = (pow(M, 2) + s12 - s34) / 4.;
	double p1P = pP + y12*lmbd / 4.;
	double p2P = pP - y12*lmbd / 4.;

	// the chPT counterterms prefactor to be multiplied with PTriD traces
	double chipreD = (ALPHA * (chi + (3 * log(MUSQ / pow(MRHO, 2))) / 2.)) / (4.*PI*s14*s23*s34);
	// the chPT prefactor for 1D
	double chipre1D = chipreD / (s134 - pow(m, 2));
	// the chPT prefactor for 2D
	double chipre2D = chipreD / (s234 - pow(m, 2));

	// required traces results
	// the coefficient from the trace for I Subscript[u, 1](\[Gamma] ^ \[alpha](Subscript[p, 134] + m) P\[Gamma] ^ 5) Subscript[v, 2]
	double PTr1D = -2 * (256 * pow(m, 8) - 32 * pow(m, 6)*(-s12 + 5 * s13 - 2 * s14 + 5 * s24 - s34) + (s13 - s24)*(s13 + s14 + s34)*(s14*s23 + s13*s24 - s12*s34) +
		8 * pow(m, 4)*(-pow(s12, 2) + 3 * pow(s13, 2) + s12*s23 - s13*(5 * s14 + s23 - 10 * s24) + 2 * s12*s24 + s23*s24 + pow(s24, 2) - 8 * s12*s34 - s23*s34 + 2 * s24*s34 - pow(s34, 2) + s14*(-5 * s12 - 2 * s23 + s24 + s34)) -
		2 * pow(m, 2)*(pow(s13, 3) - pow(s13, 2)*(s12 + 2 * s14 + s23 - 5 * s24) + pow(s14, 2)*(-s12 + 2 * s23 + s24 + s34) + s34*(-pow(s24, 2) + s23*(s12 + s24 - s34) + s24*s34 - 3 * s12*(s12 + s34)) -
			s13*(pow(s14, 2) - 4 * s12*s24 - s23*(s12 + s24 - 2 * s34) + 6 * s12*s34 - 4 * s24*s34 + pow(s34, 2) + s14*(6 * s12 + 4 * s23 - s24 + s34)) +
			s14*(-2 * pow(s12, 2) - pow(s24, 2) - s12*s34 + pow(s34, 2) + s24*(s12 + 2 * s34) + s23*(2 * s12 - 2 * s24 + 4 * s34))));
	// the coefficient from the trace for I Subscript[u, 1] (P\[Gamma]^5 (-Subscript[p, 234] + m) \[Gamma]^\[alpha]) Subscript[v, 2]
	double PTr2D = -2 * (256 * pow(m, 8) - 32 * pow(m, 6)*(-s12 + 5 * s13 - 2 * s23 + 5 * s24 - s34) - (s13 - s24)*(s23 + s24 + s34)*(s14*s23 + s13*s24 - s12*s34) +
		8 * pow(m, 4)*(-pow(s12, 2) + pow(s13, 2) - 5 * s12*s23 - 5 * s23*s24 + 3 * pow(s24, 2) - 8 * s12*s34 + s23*s34 - pow(s34, 2) - s14*(-s12 + 2 * s23 + s24 + s34) + s13*(s14 + s23 + 2 * (s12 + 5 * s24 + s34))) +
		2 * pow(m, 2)*(2 * pow(s12, 2)*s23 + s12*pow(s23, 2) + 6 * s12*s23*s24 + pow(s23, 2)*s24 + s12*pow(s24, 2) + 2 * s23*pow(s24, 2) - pow(s24, 3) + 3 * pow(s12, 2)*s34 + s12*s23*s34 - pow(s23, 2)*s34 + 6 * s12*s24*s34 + s23*s24*s34 +
			3 * s12*pow(s34, 2) - s23*pow(s34, 2) + s24*pow(s34, 2) + pow(s13, 2)*(s23 + s34) - s13*(pow(s23, 2) + 4 * s12*s24 + 5 * pow(s24, 2) + 4 * s24*s34 + pow(s34, 2) + s14*(-2 * s23 + s24 + s34) + s23*(s12 + s24 + 2 * s34)) +
			s14*(-2 * pow(s23, 2) + (s24 + s34)*(-s12 + s24 + s34) + s23*(4 * s24 - 2 * (s12 + 2 * s34)))));
	// the coefficient from the trace for I Subscript[u, 1] (\[Gamma]^\[alpha] (Subscript[p, 134] + m) Subscript[p, 2] \[Gamma] ^ 5) Subscript[v, 2]
	double p2Tr1D = -2 * pow(m, 2)*(128 * pow(m, 6) + 2 * pow(s12, 2)*s14 - s12*s14*s23 - 2 * pow(s14, 2)*s23 + pow(s13, 2)*(2 * s14 - 3 * s24) + s14*s23*s24 + 3 * pow(s12, 2)*s34 - 5 * s14*s23*s34 + 3 * s12*s24*s34 + 3 * s12*pow(s34, 2) +
		16 * pow(m, 4)*(s12 - 5 * s13 + 2 * s14 - 5 * s24 + s34) + 4 * pow(m, 2)*(2 * pow(s13, 2) - 2 * s14*(2 * s12 + s23) + 2 * pow(s24, 2) + s12*(-s12 + s24) + (-8 * s12 + s24)*s34 - pow(s34, 2) + s13*(s12 - 4 * s14 + 10 * s24 + s34)) +
		s13*(s14*(4 * s12 + 5 * s23) - 3 * (pow(s24, 2) - s12*s34 + s24*(s12 + s34))));
	// the coefficient from the trace for I Subscript[u, 1] (Subscript[p, 1] \[Gamma] ^ 5 (-Subscript[p, 234] +	m) \[Gamma] ^ \[alpha]) Subscript[v, 2]
	double p1Tr2D = -2 * pow(m, 2)*(128 * pow(m, 6) + 2 * pow(s12, 2)*s23 - s12*s14*s23 - 2 * s14*pow(s23, 2) - 3 * pow(s13, 2)*s24 + 4 * s12*s23*s24 + 5 * s14*s23*s24 + 2 * s23*pow(s24, 2) - 16 * pow(m, 4)*(-s12 + 5 * s13 - 2 * s23 + 5 * s24 - s34) + 3 * pow(s12, 2)*s34 -
		5 * s14*s23*s34 + 3 * s12*s24*s34 + 3 * s12*pow(s34, 2) + 4 * pow(m, 2)*(-pow(s12, 2) + 2 * pow(s13, 2) - 4 * s12*s23 - 2 * s14*s23 + s12*s24 - 4 * s23*s24 + 2 * pow(s24, 2) - 8 * s12*s34 + s24*s34 - pow(s34, 2) + s13*(s12 + 10 * s24 + s34)) +
		s13*(s14*s23 - 3 * (pow(s24, 2) - s12*s34 + s24*(s12 + s34))));

	// the loop integrals prefactors as according to the notes I_1, I_2^a, I_2^b
	setmudim(MUSQ);

	double I11D = RE(B0i(bb0, s134, 0, pow(ma, 2)));
	double I2a1D = RE(C0i(cc00, pow(M, 2), s134, pow(ma, 2), 0, 0, pow(ma, 2)) + pow(M, 2)*C0i(cc11, pow(M, 2), s134, pow(ma, 2), 0, 0, pow(ma, 2)) + p2P*C0i(cc12, pow(M, 2), s134, pow(ma, 2), 0, 0, pow(ma, 2)));
	double I2b1D = RE(pow(M, 2)*C0i(cc12, pow(M, 2), s134, pow(ma, 2), 0, 0, pow(ma, 2)) + p2P*C0i(cc22, pow(M, 2), s134, pow(ma, 2), 0, 0, pow(ma, 2)));
	double I12D = RE(B0i(bb0, s234, 0, pow(ma, 2)));
	double I2a2D = RE(C0i(cc00, pow(M, 2), s234, pow(ma, 2), 0, 0, pow(ma, 2)) + pow(M, 2)*C0i(cc11, pow(M, 2), s234, pow(ma, 2), 0, 0, pow(ma, 2)) + p1P*C0i(cc12, pow(M, 2), s234, pow(ma, 2), 0, 0, pow(ma, 2)));
	double I2b2D = RE(pow(M, 2)*C0i(cc12, pow(M, 2), s234, pow(ma, 2), 0, 0, pow(ma, 2)) + p1P*C0i(cc22, pow(M, 2), s234, pow(ma, 2), 0, 0, pow(ma, 2)));

	// the final result
	return -(ALPHA * (-(I2b1D*p2Tr1D) + (I11D - I2a1D)*PTr1D)) / (2.*PI*(-pow(m, 2) + s134)*s14*s23*s34) - (ALPHA * (-(I2b2D*p1Tr2D) + (I12D - I2a2D)*PTr2D)) / (2.*PI*s14*(-pow(m, 2) + s234)*s23*s34) + PTr1D*chipre1D + PTr2D*chipre2D;
}

// the whole result
double FDoubleDalitz::ThreePointINT_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double chi, double MUSQ)
{
	return ThreePointINT_firstTwo_Re(M, ma, mb, s12, s34, y12, y34, phi, chi, MUSQ)
		+ ThreePointINT_firstTwo_Re(M, mb, ma, s34, s12, y34, y12, phi, chi, MUSQ);
}

//################
//4p CORRECTIONS #
//################

// function for the 1D diagram (direct part)
double FDoubleDalitz::FourPoint1D_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);

	double pSQ = (pow(M, 2) + 2 * pow(ma, 2) - s12 + s34) / 2.;
	double p134SQ = pSQ + y12*lmbd / 2.;
	double p234SQ = pSQ - y12*lmbd / 2.;

	std::vector<double> IntC = C_get(pow(ma, 2), s34, p134SQ, 0, pow(ma, 2), pow(ma, 2));
	std::vector<double> IntCB = C_get(pow(ma, 2), s34, p234SQ, 0, pow(ma, 2), pow(ma, 2));
	std::vector<double> IntD = D_get(pow(ma, 2), s34, pow(ma, 2), pow(M, 2), p134SQ, p234SQ, 0, pow(ma, 2), pow(ma, 2), 0);

	double Part1 =
		-((IntC[6] - IntCB[6] + IntC[7] - IntCB[7])*pow(M, 2)*y34*pow(lmbd, 3)*Xi) / 4.
		+ (IntC[4] - IntCB[4])*(4 * s12*s34*y12*lmbd * (2 + pow(y34, 2) - pow(lmbd34, 2)) - pow(M, 4)*y34*z*lmbd * Xi)
		+ ((IntC[6] + IntCB[6] + IntC[7] + IntCB[7])*pow(lmbd, 2)*(4 * s12*s34*(pow(y12, 2) - pow(lmbd12, 2)) - pow(M, 4)*y12*y34*z*Xi + pow(M, 4)*pow(Xi, 2))) / 4.;
	double Part2 =
		IntD[5] * (2 * s12*s34*lmbd * ((pow(M, 2) + s12 - s34)*y12*(2 + pow(y34, 2) - pow(lmbd34, 2)) + lmbd * (2 + (1 + pow(y12, 2) - pow(lmbd12, 2))*(pow(y34, 2) - pow(lmbd34, 2))))
			+ (pow(M, 2)*y34*lmbd * (-2 * s12*(pow(M, 2) - s12 + s34) + pow(M, 2)*y12*z*lmbd + pow(lmbd, 2))*Xi) / 2. - pow(M, 4)*pow(lmbd, 2)*pow(Xi, 2))
		+ ((IntD[3] + IntD[9] + IntD[10])*pow(lmbd, 2)*(-4 * s12*s34*(-pow(y12, 2) + pow(lmbd12, 2))*(pow(M, 2)*z + y12*lmbd * (1 + pow(y34, 2) - pow(lmbd34, 2)))
			- 4 * pow(M, 2)*s12*s34*y12*y34*Xi + pow(M, 4)*(pow(M, 2)*z - y12*lmbd)*pow(Xi, 2))) / 8.
		+ ((IntD[7] + IntD[3] + IntD[9] + IntD[10])*s12*pow(lmbd, 2)*(-4 * s12*s34*(-pow(y12, 2) + pow(lmbd12, 2))*(2 + (1 - pow(lmbd12, 2))*(1 + pow(y34, 2) - pow(lmbd34, 2)))
			- pow(M, 2)*y34*(2 * pow(M, 2)*y12*z + lmbd * (1 + pow(lmbd12, 2)))*Xi + pow(M, 4)*(1 + pow(lmbd12, 2))*pow(Xi, 2))) / 8.
		- ((IntD[3] + IntD[10])*pow(ma, 2)*pow(lmbd, 2)*(4 * s12*s34*(pow(y12, 2) - pow(lmbd12, 2))*(1 + pow(y34, 2) - pow(lmbd34, 2)) + pow(M, 2)*Xi * (y34*lmbd - pow(M, 2)*Xi))) / 2.;
	double Part3 =
		IntD[5] * (2 * s12*s34*lmbd * (-((pow(M, 2) + s12 - s34)*y12*(2 + pow(y34, 2) - pow(lmbd34, 2))) + lmbd * (2 + (1 + pow(y12, 2) - pow(lmbd12, 2))*(pow(y34, 2) - pow(lmbd34, 2))))
			- (pow(M, 2)*y34*lmbd * (-2 * s12*(pow(M, 2) - s12 + s34) - pow(M, 2)*y12*z*lmbd + pow(lmbd, 2))*Xi) / 2. - pow(M, 4)*pow(lmbd, 2)*pow(Xi, 2))
		+ ((IntD[7] + IntD[8])*pow(lmbd, 2)*(-4 * s12*s34*(pow(y12, 2) - pow(lmbd12, 2))*(pow(M, 2)*z - y12*lmbd * (1 + pow(y34, 2) - pow(lmbd34, 2)))
			+ 4 * pow(M, 2)*s12*s34*y12*y34*Xi - pow(M, 4)*(pow(M, 2)*z + y12*lmbd)*pow(Xi, 2))) / 8.
		- (IntD[8] * s12*pow(lmbd, 2)*(-4 * s12*s34*(-pow(y12, 2) + pow(lmbd12, 2))*(2 + (1 - pow(lmbd12, 2))*(1 + pow(y34, 2) - pow(lmbd34, 2)))
			- pow(M, 2)*y34*(2 * pow(M, 2)*y12*z - lmbd * (1 + pow(lmbd12, 2)))*Xi + pow(M, 4)*(1 + pow(lmbd12, 2))*pow(Xi, 2))) / 8.
		- ((IntD[6] + IntD[7] + IntD[8])*pow(ma, 2)*pow(lmbd, 2)*(-4 * s12*s34*(pow(y12, 2) - pow(lmbd12, 2))*(1 + pow(y34, 2) - pow(lmbd34, 2)) + pow(M, 2)*Xi * (y34*lmbd + pow(M, 2)*Xi))) / 2.;

	return -(ALPHA * (Part1 + Part2 + Part3)) / (4.*PI*s12*pow(s34, 2));
}

double FDoubleDalitz::FourPointD_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi)
{
	return FourPoint1D_Re(M, ma, mb, s12, s34, y12, y34, phi) + FourPoint1D_Re(M, mb, ma, s34, s12, y34, y12, phi);
}

double FDoubleDalitz::FourPoint1Int_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi)
{
	double m = ma;
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);
	SetSVariables(M, ma, mb, s12, s34, y12, y34);

	double pSQ = (pow(M, 2) + 2 * pow(ma, 2) - s12 + s34) / 2.;
	double p134SQ = pSQ + y12*lmbd / 2.;
	double p234SQ = pSQ - y12*lmbd / 2.;

	std::vector<double> IntC = C_get(pow(ma, 2), s34, p134SQ, 0, pow(ma, 2), pow(ma, 2));
	std::vector<double> IntCB = C_get(pow(ma, 2), s34, p234SQ, 0, pow(ma, 2), pow(ma, 2));
	std::vector<double> IntD = D_get(pow(ma, 2), s34, pow(ma, 2), pow(M, 2), p134SQ, p234SQ, 0, pow(ma, 2), pow(ma, 2), 0);

	// part 1
	double Part1D00An = 4 * ((-s13 + s24)*(s14*s23 + s13*s24 - s12*s34) + 2 * pow(m, 2)*(pow(s13, 2) - s12*s14 + s12*s23 + s12*s24 + s14*s24 + s23*s24 - pow(s24, 2) + (s14 - s23 + s24)*s34 - s13*(s12 + s14 + s23 + s34)));
	double Part1DaAn = 2 * pow(m, 2)*(pow(s13, 2)*(-2 * s14 + s24) + s14*(-2 * pow(s12, 2) + 2 * s12*s23 + 2 * s14*s23 + s23*s24) + 16 * pow(m, 4)*(-s12 + s13 - 2 * s14 + s24 - s34) - s12*(2 * s12 + s24)*s34 +
		s13*(-4 * s12*s14 - 3 * s14*s23 + 2 * s12*s24 + pow(s24, 2) - s12*s34) + 2 * pow(m, 2)*(2 * pow(s12, 2) - pow(s13, 2) + 7 * s13*s14 + s13*s23 - 6 * s13*s24 + s14*s24 - s23*s24 - pow(s24, 2) - s12*(s13 - 7 * s14 + s23 + s24) +
		(6 * s12 + s13 + s14 + s23 + s24)*s34));
	double Part1DbAn = -2 * (32 * pow(m, 6)*(s14 - s23) + pow(s13, 2)*(-pow(s24, 2) + s14*(2 * s23 + s24)) - (s14*s23 - s12*s34)*(s14*(s23 + s24) - s12*s34) -
		4 * pow(m, 4)*(pow(s13, 2) + 3 * s12*s14 - s12*s23 - 4 * s14*s23 - s12*s24 + s14*s24 - s23*s24 + pow(s24, 2) + s13*(-s12 + 3 * s14 - 3 * s23 + 2 * s24 - 3 * s34) + (2 * s12 + s14 - 3 * (s23 + s24))*s34 + 2 * pow(s34, 2)) +
		s13*(pow(s14, 2)*s23 + 2 * s12*s24*s34 - s14*(pow(s24, 2) + s12*s34)) - 2 * pow(m, 2)*(pow(s13, 2)*(s14 - 2 * s24) + pow(s14, 2)*(-s12 + s23 + s24 + s34) - s14*(pow(s23, 2) + pow(s24, 2) + s23*(s24 - 4 * s34) + s12*s34 - s24*s34) +
			s13*(-pow(s14, 2) + s12*s24 - 2 * pow(s24, 2) + s23*(s12 + s24) + s14*(-s12 + 5 * s23 - s24 - 2 * s34) + 2 * s12*s34 + 3 * s24*s34) + s34*(s23*(s12 + s24) - s12*(s12 - 2 * s24 + 3 * s34))));
	double Part1DcAn = -2 * pow(m, 2)*(pow(s13, 2)*s24 + s23*(s14*(2 * s12 + 2 * s23 - 3 * s24) - 2 * pow(s12 + s24, 2)) - 2 * pow(m, 2)*(-2 * pow(s12, 2) + pow(s13, 2) + s24*(-s14 - 7 * s23 + s24) + s12*(s14 - 7 * s23 + s24) + s13*(s12 + s14 - s23 + 6 * s24)) +
		16 * pow(m, 4)*(-s12 + s13 - 2 * s23 + s24 - s34) - s12*(2 * s12 + s24)*s34 + 2 * pow(m, 2)*(6 * s12 + s13 + s14 + s23 + s24)*s34 + s13*(s14*s23 + 2 * s12*s24 + pow(s24, 2) - s12*s34));
	double Part1DdAn = -2 * (32 * pow(m, 6)*(s14 - s23) + pow(s13, 2)*s24*(s23 + s24) + s13*s23*(s14*s23 - pow(s24, 2)) + s14*s23*(s14*s23 - s24*(s23 + 2 * s24)) + s12*s23*(-2 * s14 + s24)*s34 - s12*s13*(s23 + 2 * s24)*s34 + pow(s12, 2)*pow(s34, 2) +
		4 * pow(m, 4)*(pow(s13, 2) + 3 * s12*s23 - s12*s24 + 3 * s23*s24 + pow(s24, 2) + (2 * s12 + s23 - 3 * s24)*s34 + 2 * pow(s34, 2) - s13*(s12 + s14 - s23 - 2 * s24 + 3 * s34) - s14*(s12 + 4 * s23 + 3 * s24 + 3 * s34)) +
		2 * pow(m, 2)*(-(pow(s14, 2)*s23) - pow(s13, 2)*(s23 + 2 * s24) - s23*((s12 - s24)*s24 + s23*(s12 + s24)) + (pow(s23, 2) + s12*(-s12 + 2 * s24) - s23*(s12 + 2 * s24))*s34 - 3 * s12*pow(s34, 2) +
			s13*(pow(s23, 2) + s12*s24 - s23*s24 - 2 * pow(s24, 2) + (2 * s12 + s23 + 3 * s24)*s34 + s14*(-s23 + s24 + s34)) + s14*(pow(s23, 2) + s12*(s24 + s34) + s23*(5 * s24 + 4 * s34))));

	double Part1 =
		(IntC[4] - IntCB[4])*Part1D00An
		+ (IntC[5] + 2 * IntC[6] + IntC[7])*Part1DaAn
		+ (IntC[6] + IntC[7])*Part1DbAn
		+ (-IntCB[5] - 2 * IntCB[6] - IntCB[7])*Part1DcAn
		+ (-IntCB[6] - IntCB[7])*Part1DdAn;

	// part 2
	double Part2D00An = -4 * (256 * pow(m, 8) + pow(s13, 3)*s24 - 32 * pow(m, 6)*(s12 + 3 * s13 + 4 * s14 - 2 * s23 + 3 * s24 + s34) + pow(s13, 2)*(s24*(s12 + s24) - s14*(3 * s23 + s24) - s12*s34) + (s14*s23 - s12*s34)*(s14*(2 * s23 + s24) - s12*(s24 + 2 * s34)) -
		s13*(pow(s14, 2)*s23 - s14*(s23*(s12 - s24) + pow(s24, 2) + s12*s34) + s12*(pow(s24, 2) + s12*s34 + 3 * s24*s34)) +
		8 * pow(m, 4)*(pow(s12, 2) + 3 * pow(s13, 2) - s12*s23 - s23*s24 + pow(s24, 2) - 3 * s23*s34 + pow(s34, 2) + s14*(5 * s12 - 6 * s23 + 3 * s24 + 3 * s34) + s13*(5 * s14 - 3 * s23 + 6 * s24 - 2 * (s12 + s34))) -
		2 * pow(m, 2)*(pow(s13, 3) + s12*((s12 - s24)*s24 + s23*(s12 + s24)) - (-3 * pow(s12, 2) + 3 * s12*s23 + 2 * s12*s24 + 2 * s23*s24)*s34 + 3 * s12*pow(s34, 2) - pow(s13, 2)*(s23 - 7 * s24 + s34) - pow(s14, 2)*(-s12 + 4 * s23 + s24 + s34) +
			s14*(2 * pow(s23, 2) + pow(s12 + s24, 2) - (-3 * s12 + s24)*s34 - 2 * s23*(s12 + s24 + 2 * s34)) +
			s13*(-pow(s12, 2) + pow(s14, 2) - 2 * s12*s24 + 2 * pow(s24, 2) - 2 * (4 * s12 + s24)*s34 - s23*(2 * s12 + s24 + s34) + s14*(-8 * s23 + 3 * (s12 + s24) + 4 * s34))));
	double Part2D3An = -2 * (512 * pow(m, 10) - 64 * pow(m, 8)*(s12 + 5 * s13 - s14 - s23 + 5 * s24 - s34) + 8 * pow(m, 6)*(-2 * pow(s12, 2) + 7 * pow(s13, 2) - 3 * s12*s14 - 4 * pow(s14, 2) - 5 * s12*s23 - 8 * s14*s23 + 9 * s12*s24 + 3 * s14*s24 - s23*s24 + 5 * pow(s24, 2) +
		s13*(7 * s12 - 7 * s14 - 3 * s23 + 24 * s24 - 3 * s34) - (14 * s12 + s14 + 3 * s23 + s24)*s34) - 2 * pow(m, 4)*
		(2 * pow(s13, 3) - 3 * pow(s12, 2)*s23 - s12*pow(s23, 2) - pow(s12, 2)*s24 - 5 * s12*s23*s24 - pow(s23, 2)*s24 + 4 * s12*pow(s24, 2) + pow(s24, 3) + pow(s13, 2)*(5 * s12 - 12 * s14 - 3 * s23 + 15 * s24 - 2 * s34) +
			2 * pow(s14, 2)*(-3 * s12 + s23 - s24 - s34) + (-10 * pow(s12, 2) - 7 * s12*s23 + pow(s23, 2) - 13 * s12*s24 - 2 * s23*s24 - pow(s24, 2))*s34 + 2 * s12*pow(s34, 2) +
			s13*(-3 * pow(s12, 2) - 9 * s12*s14 - 6 * pow(s14, 2) - 4 * s12*s23 - 21 * s14*s23 + pow(s23, 2) + 25 * s12*s24 + 9 * s14*s24 + s23*s24 + 10 * pow(s24, 2) + (-19 * s12 + 2 * s14 - s23 + s24)*s34) +
			s14*(-pow(s12, 2) - 7 * s12*s23 + 2 * pow(s23, 2) + 6 * s12*s24 + s23*s24 + pow(s24, 2) - (s12 + s23 + 3 * s24)*s34)) -
		s12*(pow(s13, 2)*(pow(s24, 2) - s14*(2 * s23 + s24)) + (s14*s23 - s12*s34)*(s14*(s23 + s24) - s12*s34) + s13*(-(pow(s14, 2)*s23) - 2 * s12*s24*s34 + s14*(pow(s24, 2) + s12*s34))) -
		pow(m, 2)*(2 * pow(s13, 3)*(s14 - s24) + s14*(pow(s12, 2)*s23 - 2 * s12*(pow(s23, 2) + s24*(-s14 + s24)) - s23*(2 * pow(s14, 2) + 2 * s14*(s23 + s24) + s24*(s23 + s24))) +
			s12*(-pow(s12, 2) + 2 * pow(s14, 2) + 2 * s12*s23 + 7 * s14*s23 + 6 * s12*s24 + 4 * s14*s24 + 3 * s23*s24 + pow(s24, 2))*s34 - 5 * pow(s12, 2)*pow(s34, 2) +
			pow(s13, 2)*(2 * pow(s14, 2) + 6 * s14*(s12 + s23) + (-8 * s12 + s23 - s24)*s24 + 2 * s12*s34) +
			s13*(2 * pow(s12, 2)*s23 + 2 * pow(s14, 2)*(s12 + s23) + pow(s12, 2)*s24 + 2 * s12*s23*s24 - 6 * s12*pow(s24, 2) - s23*pow(s24, 2) - pow(s24, 3) + s12*(8 * s12 - s23 + 6 * s24)*s34 +
				s14*(pow(s23, 2) + s23*(10 * s12 + s24) - 2 * (2 * s12*s24 + pow(s24, 2) + 2 * s12*s34)))));
	double Part2D1An = 2 * pow(m, 2)*(512 * pow(m, 8) + pow(s13, 3)*s24 + s14*(-(s14*s23*s24) + s12*(s12 - 2 * s23)*(s23 + 2 * s24)) - s12*(s12*(s12 - 2 * s23) + s14*(-s23 + s24))*s34 - pow(s12, 2)*pow(s34, 2) -
		64 * pow(m, 6)*(3 * s13 + 3 * s14 - s23 + 3 * (s12 + s24) + s34) - pow(s13, 2)*(s14*(s23 - s24) + s24*(-2 * s12 + 2 * s23 + s24) + s12*s34) +
		8 * pow(m, 4)*(2 * pow(s12, 2) + 3 * pow(s13, 2) - s12*s23 + 9 * s12*s24 - 5 * s23*s24 + pow(s24, 2) + (-2 * s12 + s23 + 3 * s24)*s34 + s13*(7 * s12 + 9 * s14 - 3 * s23 + 8 * s24 + s34) + s14*(9 * s12 - 4 * s23 + 7 * s24 + 3 * s34)) +
		s13*(-(pow(s14, 2)*s23) + s14*(2 * pow(s23, 2) + 4 * s12*s24 + pow(s24, 2) + s23*(2 * s12 + s24) + s12*s34) + s12*((s12 - 2 * s23)*s24 + 2 * (-s12 + s23 + s24)*s34)) -
		2 * pow(m, 2)*(pow(s13, 3) + pow(s12, 2)*s23 - 2 * s12*pow(s23, 2) + 3 * pow(s12, 2)*s24 - 5 * s12*s23*s24 - 2 * pow(s23, 2)*s24 + s12*pow(s24, 2) - 2 * s23*pow(s24, 2) + pow(s13, 2)*(2 * s12 + 2 * s14 - 3 * s23 + 4 * s24 - s34) +
		(5 * s12*s23 + 2 * pow(s23, 2) + 3 * s12*(-2 * s12 + s24))*s34 - 2 * s12*pow(s34, 2) - pow(s14, 2)*(-s12 + 6 * s23 + s24 + s34) + s14*(2 * pow(s23, 2) + pow(s24, 2) + s24*(10 * s12 - s34) + 3 * s12*(s12 + s34) + s23*(-s12 - 3 * s24 + s34)) +
			s13*(pow(s12, 2) + pow(s14, 2) + 2 * pow(s23, 2) + 11 * s12*s24 - 5 * s23*s24 - pow(s24, 2) + (-3 * s12 + s23 + 3 * s24)*s34 + s14*(7 * s12 - s23 + 11 * s24 + 4 * s34))));
	double Part2D2An = -2 * (128 * pow(m, 8)*(s13 - s24) - (pow(s13, 2) - s14*s23)*(s23 + s24)*(-(s14*s23) + s13*s24) + s12*(s13*s23*(s14 + s24) + s14*s23*(2 * s23 + s24) + pow(s13, 2)*(s23 + 2 * s24))*s34 - pow(s12, 2)*(s13 + s23)*pow(s34, 2) -
		16 * pow(m, 6)*(pow(s13, 2) - s12*s23 + 2 * pow(s23, 2) - s12*s24 - 3 * s23*s24 - 3 * pow(s24, 2) + 5 * s23*s34 - s24*s34 + s14*(-s12 + 2 * s23 - s24 + s34) + s13*(3 * s14 + s23 - 2 * s24 + 3 * (s12 + s34))) -
		2 * pow(m, 4)*(pow(s13, 3) + (s23 + s24)*(s23*(s12 + s24) + s24*(3 * s12 + s24)) - (8 * s12*s23 + 9 * pow(s23, 2) + 4 * s12*s24 + 10 * s23*s24 - 3 * pow(s24, 2))*s34 - pow(s14, 2)*(-s12 + s24 + s34) +
			pow(s13, 2)*(-6 * s14 + 4 * s23 + 11 * s24 - 5 * (s12 + s34)) + 2 * s14*(-4 * pow(s23, 2) + s23*(3 * s12 - 2 * s24 - 7 * s34) + s24*(2 * (s12 + s24) + s34)) +
			s13*(pow(s14, 2) - 4 * s12*s23 - 9 * pow(s23, 2) - 6 * s12*s24 + 10 * s23*s24 + 11 * pow(s24, 2) - 2 * (2 * s12 + 7 * s23 + 3 * s24)*s34 - 2 * s14*(2 * s12 + 6 * s23 + 3 * s24 + 5 * s34))) +
		pow(m, 2)*(2 * pow(s13, 3)*(s23 + 2 * s24) + 2 * s14*s23*(s23*(s14 + s23) - pow(s24, 2)) + s12*s14*(s14*s23 + (s23 + s24)*(s23 + 2 * s24)) -
		(pow(s12, 2)*(s14 + s23 + s24) + 2 * s12*(s14*(s23 - s24) + s23*(s23 + 3 * s24)) + s23*(pow(s14, 2) + 2 * s24*(s23 + s24) + s14*(5 * s23 + 3 * s24)))*s34 + s12*(s14 + 5 * s23 - s24)*pow(s34, 2) -
			pow(s13, 2)*(2 * pow(s23, 2) + 3 * s12*s24 - 8 * pow(s24, 2) + 4 * s12*s34 + 3 * s24*s34 + 2 * s23*(s12 - 4 * s24 + s34) + 4 * s14*(s23 + s24 + s34)) +
			s13*(2 * pow(s14, 2)*s23 + s12*s24*(s23 + s24) + (3 * pow(s12, 2) - 2 * pow(s23, 2) - 3 * s23*s24 + pow(s24, 2) - 8 * s12*(s23 + s24))*s34 + 3 * s12*pow(s34, 2) -
				s14*(10 * pow(s23, 2) + 3 * s12*s24 - 2 * pow(s24, 2) + 4 * s12*s34 - s24*s34 + s23*(s12 + 8 * s24 + 7 * s34)))));

	double Part2 =
		IntD[5] * Part2D00An
		+ (IntD[3] + IntD[10])*Part2D1An
		+ (IntD[3] + IntD[9] + IntD[10])*Part2D2An
		+ (IntD[7] + IntD[3] + IntD[9] + IntD[10])*Part2D3An;

	// part 3
	double Part3D00An = -4 * (256 * pow(m, 8) + pow(s13, 2)*s24*(-s12 + s23 + s24) + s14*s23*(2 * s14*s23 + (s12 - s23 - 3 * s24)*s24) + s13*(-s12 + s23 - s24)*(s14*s23 - pow(s24, 2)) + s12*s13*(s12 - s23 - 3 * s24)*s34 -
		s12*(4 * s14*s23 + s24*(s12 - s23 + s24))*s34 + 2 * pow(s12, 2)*pow(s34, 2) - 32 * pow(m, 6)*(s12 + 3 * s13 - 2 * s14 + 4 * s23 + 3 * s24 + s34) +
		8 * pow(m, 4)*(pow(s12, 2) + pow(s13, 2) + 5 * s12*s23 - 2 * s12*s24 + 5 * s23*s24 + 3 * pow(s24, 2) + s13*(-s14 + 3 * s23 + 6 * s24) + 3 * s23*s34 - 2 * s24*s34 + pow(s34, 2) - s14*(s12 + 6 * s23 + 3 * s24 + 3 * s34)) -
		2 * pow(m, 2)*(pow(s12, 2)*s23 + 2 * pow(s14, 2)*s23 + s12*pow(s23, 2) - pow(s12, 2)*s24 + 3 * s12*s23*s24 + pow(s23, 2)*s24 + pow(s24, 3) + pow(s13, 2)*(-s12 + s23 + 2 * s24) -
		(-3 * pow(s12, 2) - 3 * s12*s23 + pow(s23, 2) + 8 * s12*s24 - 4 * s23*s24 + pow(s24, 2))*s34 + 3 * s12*pow(s34, 2) -
			s13*(-pow(s12, 2) + pow(s23, 2) + 2 * s12*s24 - 7 * pow(s24, 2) + 2 * (s12 + s24)*s34 + s23*(-2 * s12 - 3 * s24 + s34) + s14*(-s12 + 2 * s23 + s24 + 2 * s34)) -
			s14*(4 * pow(s23, 2) + pow(s24, 2) - s12*(s12 - 3 * s34) + s24*(2 * s12 + s34) + 2 * s23*(s12 + 4 * s24 + 2 * s34))));
	double Part3D1An = 2 * (512 * pow(m, 10) - 64 * pow(m, 8)*(s12 + 5 * s13 - s14 - s23 + 5 * s24 - s34) - s12*(pow(s13, 2)*s24*(s23 + s24) + s13*s23*(s14*s23 - pow(s24, 2)) + s14*s23*(s14*s23 - s24*(s23 + 2 * s24)) + s12*s23*(-2 * s14 + s24)*s34 -
		s12*s13*(s23 + 2 * s24)*s34 + pow(s12, 2)*pow(s34, 2)) + 8 * pow(m, 6)*(-2 * pow(s12, 2) + 5 * pow(s13, 2) - 3 * s12*s23 - 4 * pow(s23, 2) + 7 * s12*s24 - 7 * s23*s24 + 7 * pow(s24, 2) - (14 * s12 + s23 + 3 * s24)*s34 -
			s13*(s14 - 3 * (3 * s12 + s23 + 8 * s24) + s34) - s14*(5 * s12 + 8 * s23 + 3 * s24 + 3 * s34)) - 2 * pow(m, 4)*
			(pow(s13, 3) - pow(s12, 2)*s23 - 6 * s12*pow(s23, 2) - 3 * pow(s12, 2)*s24 - 9 * s12*s23*s24 - 6 * pow(s23, 2)*s24 + 5 * s12*pow(s24, 2) - 12 * s23*pow(s24, 2) + 2 * pow(s24, 3) + pow(s13, 2)*(4 * s12 + s23 + 10 * s24 - s34) -
		(10 * pow(s12, 2) + s12*(s23 + 19 * s24) + 2 * (pow(s23, 2) - s23*s24 + pow(s24, 2)))*s34 + 2 * s12*pow(s34, 2) + pow(s14, 2)*(-s12 + 2 * s23 + s24 + s34) +
				s13*(-pow(s12, 2) - pow(s14, 2) + 6 * s12*s23 - 2 * pow(s23, 2) + 25 * s12*s24 + 9 * s23*s24 + 15 * pow(s24, 2) + s14*(-5 * s12 + s23 + s24 - 2 * s34) - 13 * s12*s34 - 3 * s23*s34 + s24*s34) +
				s14*(-3 * pow(s12, 2) - 7 * s12*s23 + 2 * pow(s23, 2) - 4 * s12*s24 - 21 * s23*s24 - 3 * pow(s24, 2) - (7 * s12 + s23 + s24)*s34)) +
		pow(m, 2)*(pow(s14, 2)*s23*(2 * s12 + 2 * s23 - s24) + pow(s13, 3)*s24 - 2 * s23*s24*(s23*(s12 + s24) + s24*(3 * s12 + s24)) + s12*(pow(s12, 2) - 2 * pow(s23 - s24, 2) - 8 * s12*s24)*s34 + 5 * pow(s12, 2)*pow(s34, 2) +
			pow(s13, 2)*(6 * s12*s24 + pow(s24, 2) + 2 * s23*(s12 + s24) + s14*(s23 + s24) - s12*s34) +
			s13*(pow(s14, 2)*s23 + 8 * s12*pow(s24, 2) + 2 * pow(s24, 3) - s12*s24*(s12 - 4 * s23 + 6 * s34) + s14*(2 * pow(s23, 2) - s23*s24 - s24*(2 * s12 + s24) - 3 * s12*s34) - 2 * s12*(pow(s23, 2) + 3 * s12*s34 + 2 * s23*s34)) +
			s14*(2 * pow(s23, 3) - 2 * pow(s23, 2)*s24 + s12*(-2 * s12*s34 + s24*(-2 * s12 + s34)) - s23*(10 * s12*s24 + 6 * pow(s24, 2) + s12*(s12 + 7 * s34)))));
	double Part3D2An = -2 * (128 * pow(m, 8)*(s13 - s24) - (s13 + s14)*(-(s14*s23) + s13*s24)*(s14*s23 - pow(s24, 2)) - s12*(s14*(s13 + 2 * s14)*s23 + s14*(s13 + s23)*s24 + (2 * s13 + s14)*pow(s24, 2))*s34 + pow(s12, 2)*(s14 + s24)*pow(s34, 2) -
		16 * pow(m, 6)*(3 * pow(s13, 2) - 2 * pow(s14, 2) + s12*s23 - 3 * s12*s24 - 3 * s23*s24 - pow(s24, 2) - (s23 + 3 * s24)*s34 + s13*(s12 + 3 * s14 + s23 + 2 * s24 + s34) - s14*(-s12 + 2 * s23 + s24 + 5 * s34)) +
		2 * pow(m, 4)*(pow(s13, 3) + s12*pow(s23, 2) - 4 * s12*s23*s24 + pow(s23, 2)*s24 - 5 * s12*pow(s24, 2) - 6 * s23*pow(s24, 2) + pow(s24, 3) + pow(s14, 2)*(s12 - 8 * s23 - 9 * s24 - 9 * s34) -
		(pow(s23, 2) + 4 * s12*s24 + 10 * s23*s24 + 5 * pow(s24, 2))*s34 + s13*(pow(s14, 2) + 4 * s12*s23 - pow(s23, 2) - 6 * s12*s24 - 6 * s23*s24 + 11 * pow(s24, 2) + 2 * s14*(2 * s12 - 2 * s23 + 5 * s24 - 5 * s34) + 2 * (-2 * s12 + s23 - 3 * s24)*s34) -
			2 * s14*(-3 * s12*s23 + 2 * s12*s24 + 6 * s23*s24 - 2 * pow(s24, 2) + 4 * s12*s34 + 7 * s23*s34 + 7 * s24*s34) + pow(s13, 2)*(2 * s14 + 4 * s23 + 11 * s24 + 3 * (s12 + s34))) +
		pow(m, 2)*(2 * s14*(-(s14*s23*(s14 + s23)) + (5 * s14 - s23)*s23*s24 + (s14 + 2 * s23)*pow(s24, 2) - pow(s24, 3)) - s12*s14*(s14*s23 + (s23 - 2 * s24)*(s23 + s24)) +
		(s23*pow(s12 + 2 * s24, 2) + s12*s24*(-3 * s12 + 4 * s24) + s14*(pow(s12, 2) + 2 * s12*s23 + pow(s23, 2) + 8 * s12*s24 + 7 * s23*s24 + 2 * pow(s24, 2)) + pow(s14, 2)*(5 * s23 + 2 * (s12 + s24)))*s34 - s12*(5 * s14 + s23 + 3 * s24)*pow(s34, 2) -
			pow(s13, 2)*(2 * s23*(s12 + s24) - 2 * s14*(s23 + s34) + s24*(s12 + 8 * s24 + s34)) + s13*(s24*(4 * (s23 - s24)*s24 + 3 * s12*(s23 + s24)) + 2 * pow(s14, 2)*s34 + (pow(s12, 2) + 8 * s12*s24 + 3 * pow(s24, 2) - s23*(2 * s12 + s24))*s34 +
				s12*pow(s34, 2) + s14*(-3 * s12*s23 - s12*s24 + 8 * s23*s24 - 8 * pow(s24, 2) + 3 * (2 * s12 + s23 + s24)*s34))));
	double Part3D3An = 2 * pow(m, 2)*(512 * pow(m, 8) + pow(s13, 2)*(s23 - s24)*s24 + s14*s23*(pow(s12, 2) + 2 * s12*s24 + 2 * s14*(-s12 + s24) - s24*(s23 + s24)) + s12*(s23*s24 - pow(s12 + s24, 2) + s14*(s23 + 2 * (s12 + s24)))*s34 - pow(s12, 2)*pow(s34, 2) -
		64 * pow(m, 6)*(3 * s13 - s14 + 3 * (s12 + s23 + s24) + s34) + 8 * pow(m, 4)*(2 * pow(s12, 2) + pow(s13, 2) + 9 * s12*s23 + 7 * s12*s24 + 9 * s23*s24 + 3 * pow(s24, 2) - s14*(s12 + 4 * s23 + 3 * s24 - s34) + (-2 * s12 + 3 * s23 + s24)*s34 +
			s13*(9 * s12 - 5 * s14 + 7 * s23 + 8 * s24 + 3 * s34)) + s13*(-(s14*(pow(s23, 2) - s23*(-4 * s12 + s24) + 2 * s24*(s12 + s24))) + s23*(2 * pow(s12, 2) + 4 * s12*s24 + pow(s24, 2) - s12*s34) + s24*(pow(s12 + s24, 2) + 2 * s12*s34)) +
		2 * pow(m, 2)*(-3 * pow(s12, 2)*s23 - s12*pow(s23, 2) - pow(s12, 2)*s24 - 7 * s12*s23*s24 - pow(s23, 2)*s24 - 2 * s12*pow(s24, 2) - 2 * s23*pow(s24, 2) - pow(s24, 3) + pow(s13, 2)*(-s12 + 2 * s14 - s23 + s24) +
		(6 * pow(s12, 2) - 3 * s12*s23 + pow(s23, 2) + 3 * s12*s24 - 4 * s23*s24 + pow(s24, 2))*s34 + 2 * s12*pow(s34, 2) - 2 * pow(s14, 2)*(-s12 + s23 + s24 + s34) +
			s14*(-pow(s12, 2) + s12*s23 + 6 * pow(s23, 2) + s23*s24 + 3 * pow(s24, 2) - (5 * s12 + s23 + s24)*s34) +
			s13*(-3 * pow(s12, 2) + 2 * pow(s14, 2) + pow(s23, 2) - 11 * s12*s24 - 4 * pow(s24, 2) + s14*(3 * s23 + 5 * (s12 + s24)) - 3 * (s12 + s24)*s34 + s23*(-10 * s12 - 11 * s24 + s34))));

	double Part3 =
		IntD[5] * Part3D00An
		+ IntD[8] * Part3D1An
		+ (IntD[7] + IntD[8])*Part3D2An
		+ (IntD[6] + IntD[7] + IntD[8])*Part3D3An;

	return ALPHA * (Part1 + Part2 + Part3) / (4.*PI*s14*s23*s34);
}

double FDoubleDalitz::FourPointInt_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi)
{
	return FourPoint1Int_Re(M, ma, mb, s12, s34, y12, y34, phi) + FourPoint1Int_Re(M, mb, ma, s34, s12, y34, y12, phi);
}

//################
//5p CORRECTIONS #
//################

//########
//Scalar #
//########

double FDoubleDalitz::Pent1DT1(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double DIR, double M12, double M34)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);
	SetSVariables(M, ma, mb, s12, s34, y12, y34);

	// set the scale for IR divergence
	setlambda(LAMBDA*LAMBDA);

	// DivPart from 1D diagram; the last term is just -4(p2.p3); E0 from I(p3,p4,-P,p1,p2;0,mb,0,0,ma) with ingoing momenta
	double DIV1D = -((DIR*ALPHA * s12*(-pow(ma, 2) - pow(mb, 2) + s23)*s34*RE(E0(pow(mb, 2), pow(mb, 2), pow(M, 2), pow(ma, 2), pow(ma, 2), s34, s123, s234, s12, s23, 0, pow(mb, 2), pow(M34, 2), pow(M12, 2), pow(ma, 2)))) / PI);
	// DivPart from 2D diagram; the last term is just 4(p1.p3); E0 from I(p3,p4,-P,p2,p1;0,mb,0,0,ma) with ingoing momenta
	double DIV2D = (DIR*ALPHA * s12*(-pow(ma, 2) - pow(mb, 2) + s13)*s34*RE(E0(pow(mb, 2), pow(mb, 2), pow(M, 2), pow(ma, 2), pow(ma, 2), s34, s123, s134, s12, s13, 0, pow(mb, 2), pow(M34, 2), pow(M12, 2), pow(ma, 2)))) / PI;
	// DivPart from 3D diagram; the last term is just 4(p2.p4); E0 from I(p4, p3, -P, p1, p2; 0, mb, 0, 0, ma) with ingoing momenta
	double DIV3D = (DIR*ALPHA * s12*(-pow(ma, 2) - pow(mb, 2) + s24)*s34*RE(E0(pow(mb, 2), pow(mb, 2), pow(M, 2), pow(ma, 2), pow(ma, 2), s34, s124, s234, s12, s24, 0, pow(mb, 2), pow(M34, 2), pow(M12, 2), pow(ma, 2)))) / PI;
	// DivPart from 4D diagram; the last term is just - 4(p1.p4); E0 from I(p4, p3, -P, p2, p1; 0, mb, 0, 0, ma) with ingoing momenta
	double DIV4D = -((DIR*ALPHA * s12*(-pow(ma, 2) - pow(mb, 2) + s14)*s34*RE(E0(pow(mb, 2), pow(mb, 2), pow(M, 2), pow(ma, 2), pow(ma, 2), s34, s124, s134, s12, s14, 0, pow(mb, 2), pow(M34, 2), pow(M12, 2), pow(ma, 2)))) / PI);

	return DIV1D + DIV2D + DIV3D + DIV4D;
}

// TODO check
double FDoubleDalitz::Pent1IT1(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double INT, double M12, double M34)
{
	return Pent1DT1(M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, INT / 2, M12, M34);
}

//########
//Tensor #
//########

double FDoubleDalitz::PENTDTev(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34, double FF)
{
	return FF * (Pent1DDir(M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, M12, M34)
		- Pent1DDir(M, ma, mb, s12, s34, -y12, y34, phi + PI, LAMBDA, M12, M34)
		- Pent1DDir(M, ma, mb, s12, s34, y12, -y34, phi + PI, LAMBDA, M12, M34)
		+ Pent1DDir(M, ma, mb, s12, s34, -y12, -y34, phi, LAMBDA, M12, M34));
}

double FDoubleDalitz::PENTITev(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double s14, double s32, double y14, double y32, double phiex, double LAMBDA, double M12, double M34, double FFD, double FFE)
{
	return FFE*(Pent1IDir(M, ma, mb, s12, s34, -y12, -y34, phi, LAMBDA, M12, M34)
		+ Pent1IDir(M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, M12, M34)
		+ Pent2IDir(M, ma, mb, s12, s34, -y12, -y34, phi, LAMBDA, M12, M34)
		+ Pent2IDir(M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, M12, M34))
		+ FFD*(Pent1IDir(M, ma, mb, s14, s32, -y14, -y32, phiex, LAMBDA, M12, M34)
			+ Pent1IDir(M, ma, mb, s14, s32, y14, y32, phiex, LAMBDA, M12, M34)
			+ Pent2IDir(M, ma, mb, s14, s32, -y14, -y32, phiex, LAMBDA, M12, M34)
			+ Pent2IDir(M, ma, mb, s14, s32, y14, y32, phiex, LAMBDA, M12, M34));
}

//######################
/// Pentagon Tensor Dir1

double FDoubleDalitz::Pent1DDir(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34)
{
	return PentDir('D', M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, M12, M34);
}

double FDoubleDalitz::PentDir(char c, double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34)
{
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);
	SetSVariables(M, ma, mb, s12, s34, y12, y34);

	// set the scale for IR divergence
	setlambda(LAMBDA*LAMBDA);

	/// define the integrals
	// this is the E-family: single
	double EFmly = RE(E0i(ee0, pow(ma, 2), s12, pow(mb, 2), pow(mb, 2), s234, pow(ma, 2), s123, s34, s23, pow(M, 2), pow(ma, 2), pow(M12, 2), 0, pow(mb, 2), pow(M34, 2)));
	// this is the D-family: DFmlyN
	std::vector<double> DFmly1 = D_get(pow(mb, 2), s34, pow(M, 2), s123, pow(mb, 2), s12, pow(mb, 2), 0, pow(M34, 2), pow(M12, 2));
	std::vector<double> DFmly2 = D_get(pow(mb, 2), s34, s234, s23, pow(mb, 2), pow(ma, 2), pow(mb, 2), 0, pow(M34, 2), pow(ma, 2));
	std::vector<double> DFmly3 = D_get(pow(ma, 2), s123, pow(mb, 2), s234, s23, pow(M, 2), pow(ma, 2), pow(M12, 2), pow(mb, 2), pow(M34, 2));
	std::vector<double> DFmly4 = D_get(pow(ma, 2), s12, pow(mb, 2), s23, pow(ma, 2), s123, pow(ma, 2), pow(M12, 2), 0, pow(mb, 2));
	std::vector<double> DFmly5 = D_get(pow(ma, 2), s12, s34, s234, pow(ma, 2), pow(M, 2), pow(ma, 2), pow(M12, 2), 0, pow(M34, 2));
	// this is the C-family
	std::vector<double> CFmly1 = C_get(pow(ma, 2), pow(M, 2), s234, pow(ma, 2), pow(M12, 2), pow(M34, 2));
	std::vector<double> CFmly2 = C_get(pow(mb, 2), pow(M, 2), s123, pow(mb, 2), pow(M34, 2), pow(M12, 2));
	std::vector<double> CFmly3 = C_get(pow(ma, 2), s123, s23, pow(ma, 2), pow(M12, 2), pow(mb, 2));
	std::vector<double> CFmly4 = C_get(pow(mb, 2), s234, s23, pow(mb, 2), pow(M34, 2), pow(ma, 2));
	std::vector<double> CFmly5 = C_get(pow(ma, 2), s34, s234, pow(ma, 2), 0, pow(M34, 2));
	std::vector<double> CFmly6 = C_get(pow(mb, 2), s12, s123, pow(mb, 2), 0, pow(M12, 2));
	std::vector<double> CFmly7 = C_get(pow(ma, 2), s12, pow(ma, 2), pow(ma, 2), pow(M12, 2), 0);
	CFmly7.erase(CFmly7.begin() + 1);
	//CFmly7 += 1; // CFmly7 should not contain cc0 part
	std::vector<double> CFmly8 = C_get(pow(mb, 2), s34, pow(mb, 2), pow(mb, 2), 0, pow(M34, 2));
	std::vector<double> CFmly9 = C_get(pow(ma, 2), s23, pow(mb, 2), 0, pow(ma, 2), pow(mb, 2));
	std::vector<double> CFmly10 = C_get(s12, pow(M, 2), s34, 0, pow(M12, 2), pow(M34, 2));
	// this is the B-family; only scalar -> make it a vector anyway
	double BFmly[] = {
		0., // start indexing from 1
		RE(B0i(bb0, pow(M, 2), pow(M12, 2), pow(M34, 2))),
		RE(B0i(bb0, pow(mb, 2), 0, pow(mb, 2))),
		RE(B0i(bb0, pow(mb, 2), pow(M34, 2), pow(mb, 2))),
		RE(B0i(bb0, s34, 0, pow(M34, 2))),
		RE(B0i(bb0, s23, pow(ma, 2), pow(mb, 2))),
		RE(B0i(bb0, s234, pow(M34, 2), pow(ma, 2))),
		RE(B0i(bb0, s123, pow(M12, 2), pow(mb, 2)))
	};

	/// Pent1DR1(): The tensor integrals k.pi for the term j
	double INT1 = -(ALPHA * (EFmly*(pow(M12, 2) - s12) - DFmly1[1] + DFmly2[1])) / (4.*PI);
	double INT2 = (ALPHA * (-DFmly1[1] + DFmly3[1])) / (4.*PI);
	double INT3 = (ALPHA * (-DFmly3[1] + DFmly5[1])) / (4.*PI);
	double INT4 = (ALPHA * (EFmly*(pow(M34, 2) - s34) + DFmly4[1] - DFmly5[1])) / (4.*PI);

	double I1[] = { INT1, INT2, INT3, INT4 };

	/// Pent1DR2(): The tensor integrals (k.pi) (k.pj); write them as INTij
	double INTKK = (ALPHA * DFmly3[1]) / (2.*PI);
	double INT11 = (ALPHA * (EFmly*pow(pow(M12, 2) - s12, 2) + (-pow(M12, 2) + pow(ma, 2) + pow(mb, 2) + s12 - s13)*DFmly1[1] + (pow(ma, 2) + pow(mb, 2) - s13)*DFmly1[2] - (pow(ma, 2) + pow(mb, 2) - s14)*DFmly1[3] +
		(pow(ma, 2) + pow(mb, 2) - s12 - s13)*DFmly1[4] + (pow(M12, 2) - pow(ma, 2) - pow(mb, 2) - s12 + s13)*DFmly2[1] - (pow(ma, 2) + pow(mb, 2) - s13)*DFmly2[2] + (pow(ma, 2) + pow(mb, 2) - s14)*DFmly2[3] -
		(3 * pow(ma, 2) + pow(mb, 2) - s12 - s13)*DFmly2[4])) / (8.*PI);
	double INT12 = (ALPHA * (-CFmly4[1] + CFmly8[1] + (pow(M12, 2) + pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[1] + (pow(ma, 2) + pow(mb, 2) - s23)*DFmly1[2] - (pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[3] + (pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[4] +
		(-pow(M12, 2) + s12)*DFmly3[1])) / (8.*PI);
	double INT13 = -(ALPHA * (-CFmly10[1] + CFmly2[1] - CFmly4[1] + CFmly5[1] - (pow(M12, 2) - s12)*DFmly3[1] + (pow(M12, 2) - s12)*DFmly5[1])) / (8.*PI);
	double INT14 = -(ALPHA * (EFmly*(pow(M12, 2) - s12)*(pow(M34, 2) - s34) + CFmly10[1] - CFmly5[1] - CFmly6[1] + CFmly9[1] - (pow(M34, 2) - s34)*DFmly1[1] + (pow(M34, 2) - s34)*DFmly2[1] + (pow(M12, 2) - s12)*DFmly4[1] - (pow(M12, 2) - s12)*DFmly5[1])) / (8.*PI);
	double INT22 = (ALPHA * (-((pow(ma, 2) + pow(mb, 2) - s23)*DFmly1[1]) - (pow(ma, 2) + pow(mb, 2) - s23)*DFmly1[2] + (pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[3] - (pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[4] + 2 * pow(ma, 2)*DFmly3[1] +
		(2 * pow(ma, 2) - s12)*DFmly3[2] + (pow(ma, 2) - pow(mb, 2) + s23)*DFmly3[3] - (2 * pow(mb, 2) - s23 - s24)*DFmly3[4])) / (8.*PI);
	double INT23 = -(ALPHA * (CFmly10[1] - CFmly2[1] + (pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[1] - (pow(ma, 2) + pow(mb, 2) - s13)*DFmly3[2] - (-pow(ma, 2) + pow(mb, 2) + s23)*DFmly3[3] + (pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[4])) / (8.*PI);
	double INT24 = (ALPHA * (-CFmly1[1] + CFmly10[1] + CFmly3[1] - CFmly6[1] + (-pow(M34, 2) + s34)*DFmly1[1] + (pow(M34, 2) - s34)*DFmly3[1])) / (8.*PI);
	double INT33 = (ALPHA * ((pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[1] - (pow(ma, 2) + pow(mb, 2) - s13)*DFmly3[2] - (-pow(ma, 2) + pow(mb, 2) + s23)*DFmly3[3] + (pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[4] + (-pow(ma, 2) - pow(mb, 2) + s23)*DFmly5[1] +
		(pow(ma, 2) + pow(mb, 2) - s13)*DFmly5[2] - (pow(ma, 2) + pow(mb, 2) - s23)*DFmly5[3] - (pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly5[4])) / (8.*PI);
	double INT34 = -(ALPHA * (-CFmly1[1] + CFmly3[1] + (pow(M34, 2) - s34)*DFmly3[1] + (pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[1] - (pow(ma, 2) + pow(mb, 2) - s14)*DFmly5[2] + (pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[3] +
		(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[4])) / (8.*PI);
	double INT44 = (ALPHA * (EFmly*pow(pow(M34, 2) - s34, 2) + (pow(M34, 2) - pow(ma, 2) - pow(mb, 2) + s24 - s34)*DFmly4[1] + (pow(ma, 2) + pow(mb, 2) - s14)*DFmly4[2] - (pow(ma, 2) + pow(mb, 2) - s24)*DFmly4[3] -
		(pow(ma, 2) + 3 * pow(mb, 2) - s24 - s34)*DFmly4[4] + (-pow(M34, 2) + pow(ma, 2) + pow(mb, 2) - s24 + s34)*DFmly5[1] - (pow(ma, 2) + pow(mb, 2) - s14)*DFmly5[2] + (pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[3] +
		(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[4])) / (8.*PI);

	double I2[] = { INTKK, INT11, INT12, INT13, INT14, INT22, INT23, INT24, INT33, INT34, INT44 };

	/// Pent1DR3(): The tensor integrals (k.pi) (k.pj) (k.pk); write them as INTijk
	double INTKK1 = (ALPHA * (CFmly2[1] - CFmly4[1] + (-pow(M12, 2) + s12)*DFmly3[1])) / (4.*PI);
	double INTKK2 = (ALPHA * (2 * pow(ma, 2)*DFmly3[1] + (2 * pow(ma, 2) - s12)*DFmly3[2] + (pow(ma, 2) - pow(mb, 2) + s23)*DFmly3[3] + (-2 * pow(mb, 2) + s23 + s24)*DFmly3[4])) / (4.*PI);
	double INTKK3 = (ALPHA * ((-pow(ma, 2) - pow(mb, 2) + s23)*DFmly3[1] + (pow(ma, 2) + pow(mb, 2) - s13)*DFmly3[2] + (-pow(ma, 2) + pow(mb, 2) + s23)*DFmly3[3] + (-pow(ma, 2) - pow(mb, 2) + s23 + s34)*DFmly3[4])) / (4.*PI);
	double INTKK4 = (ALPHA * (-CFmly1[1] + CFmly3[1] + (pow(M34, 2) - s34)*DFmly3[1])) / (4.*PI);
	double INT111 = (ALPHA * (EFmly*pow(-pow(M12, 2) + s12, 3) + (pow(M12, 4) + pow(ma, 4) + pow(mb, 4) + pow(mb, 2)*s12 + pow(s12, 2) + pow(ma, 2)*(2 * pow(mb, 2) + s12 - 2 * s13) - pow(M12, 2)*(pow(ma, 2) + pow(mb, 2) + 2 * s12 - s13) - 2 * pow(mb, 2)*s13 - s12*s13 +
		pow(s13, 2))*DFmly1[1] + (-pow(M12, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) + s12 - 2 * s13)*(pow(ma, 2) + pow(mb, 2) - s13)*DFmly1[2] -
		(-pow(M12, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) + s12 - 2 * s13)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly1[3] + (-pow(M12, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) + s12 - 2 * s13)*(pow(ma, 2) + pow(mb, 2) - s12 - s13)*DFmly1[4] +
		4 * pow(ma, 2)*DFmly1[5] + pow(pow(ma, 2) + pow(mb, 2) - s13, 2)*DFmly1[6] - 2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly1[7] +
		2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s12 - s13)*DFmly1[8] + pow(pow(ma, 2) + pow(mb, 2) - s14, 2)*DFmly1[9] - 2 * (pow(ma, 2) + pow(mb, 2) - s12 - s13)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly1[10] +
		pow(pow(ma, 2) + pow(mb, 2) - s12 - s13, 2)*DFmly1[11] + (-pow(M12, 4) - pow(ma, 4) - pow(mb, 4) - pow(mb, 2)*s12 - pow(s12, 2) - pow(ma, 2)*(2 * pow(mb, 2) + s12 - 2 * s13) + pow(M12, 2)*(pow(ma, 2) + pow(mb, 2) + 2 * s12 - s13) +
			2 * pow(mb, 2)*s13 + s12*s13 - pow(s13, 2))*DFmly2[1] - (-pow(M12, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) + s12 - 2 * s13)*(pow(ma, 2) + pow(mb, 2) - s13)*DFmly2[2] +
			(-pow(M12, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) + s12 - 2 * s13)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly2[3] + (3 * pow(ma, 2) + pow(mb, 2) - s12 - s13)*(pow(M12, 2) - 2 * pow(ma, 2) - 2 * pow(mb, 2) - s12 + 2 * s13)*DFmly2[4] -
		4 * pow(ma, 2)*DFmly2[5] - pow(pow(ma, 2) + pow(mb, 2) - s13, 2)*DFmly2[6] + 2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly2[7] -
		2 * (pow(ma, 2) + pow(mb, 2) - s13)*(3 * pow(ma, 2) + pow(mb, 2) - s12 - s13)*DFmly2[8] - pow(pow(ma, 2) + pow(mb, 2) - s14, 2)*DFmly2[9] + 2 * (3 * pow(ma, 2) + pow(mb, 2) - s12 - s13)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly2[10] -
		pow(-3 * pow(ma, 2) - pow(mb, 2) + s12 + s13, 2)*DFmly2[11])) / (16.*PI);
	double INT112 = (ALPHA * (-((pow(ma, 2) - pow(mb, 2) + s23)*CFmly2[1]) - (pow(ma, 2) + pow(mb, 2) - s24)*CFmly2[2] + (pow(ma, 2) + pow(mb, 2) - s12 - s23)*CFmly2[3] + (pow(M12, 2) - pow(ma, 2) - pow(mb, 2) - s12 + s13)*CFmly4[1] +
		(pow(ma, 2) + pow(mb, 2) - s14)*CFmly4[2] - (3 * pow(ma, 2) + pow(mb, 2) - s12 - s13)*CFmly4[3] + (-pow(M12, 2) + 2 * pow(ma, 2) + s12 - s13 + s23)*CFmly8[1] - (s13 - s23)*CFmly8[2] + (s14 - s24)*CFmly8[3] -
		(pow(M12, 4) - 3 * pow(ma, 4) - 2 * pow(ma, 2)*(pow(mb, 2) - s13) + 2 * pow(M12, 2)*(pow(mb, 2) - s12 - s23) + pow(-pow(mb, 2) + s12 + s23, 2))*DFmly1[1] -
		2 * (-pow(ma, 4) + pow(M12, 2)*(pow(ma, 2) + pow(mb, 2) - s23) + (pow(mb, 2) - s23)*(pow(mb, 2) - s12 - s23) + pow(ma, 2)*(-s12 + s13 - s23))*DFmly1[2] +
		2 * (-(pow(ma, 2)*(pow(ma, 2) + pow(mb, 2) - s14)) + (pow(M12, 2) + pow(mb, 2) - s12 - s23)*(pow(ma, 2) + pow(mb, 2) - s24))*DFmly1[3] -
		2 * (-pow(ma, 4) + pow(M12, 2)*(pow(ma, 2) + pow(mb, 2) - s12 - s23) + pow(ma, 2)*(s13 - s23) + pow(-pow(mb, 2) + s12 + s23, 2))*DFmly1[4] - 4 * pow(ma, 2)*DFmly1[5] - pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly1[6] +
		2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[7] - 2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[8] - pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly1[9] +
		2 * (pow(ma, 2) + pow(mb, 2) - s12 - s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[10] - pow(pow(ma, 2) + pow(mb, 2) - s12 - s23, 2)*DFmly1[11] + pow(pow(M12, 2) - s12, 2)*DFmly3[1])) / (16.*PI);
	double INT113 = (ALPHA * ((-pow(M12, 2) + s12)*CFmly10[1] - s12*CFmly10[2] - (2 * pow(ma, 2) + 2 * pow(mb, 2) - s13 - s14)*CFmly10[3] + (pow(M12, 2) - pow(ma, 2) - pow(mb, 2) - s12 + s13)*CFmly2[1] + (pow(ma, 2) + pow(mb, 2) - s14)*CFmly2[2] -
		(pow(ma, 2) + pow(mb, 2) - s12 - s13)*CFmly2[3] + (-pow(M12, 2) + pow(ma, 2) + pow(mb, 2) + s12 - s13)*CFmly4[1] - (pow(ma, 2) + pow(mb, 2) - s14)*CFmly4[2] + (3 * pow(ma, 2) + pow(mb, 2) - s12 - s13)*CFmly4[3] +
		(pow(M12, 2) + 2 * pow(ma, 2) - 2 * s12)*CFmly5[1] + (2 * pow(ma, 2) - s12)*CFmly5[2] + (4 * pow(ma, 2) + 2 * pow(mb, 2) - s12 - s13 - s14)*CFmly5[3] - pow(pow(M12, 2) - s12, 2)*DFmly3[1] + pow(pow(M12, 2) - s12, 2)*DFmly5[1])) / (16.*PI);
	double INT114 = (ALPHA * (EFmly*pow(pow(M12, 2) - s12, 2)*(pow(M34, 2) - s34) + (pow(M12, 2) - s12)*CFmly10[1] + s12*CFmly10[2] + (2 * pow(ma, 2) + 2 * pow(mb, 2) - s13 - s14)*CFmly10[3] - (pow(M12, 2) + 2 * pow(ma, 2) - 2 * s12)*CFmly5[1] +
		(-2 * pow(ma, 2) + s12)*CFmly5[2] - (4 * pow(ma, 2) + 2 * pow(mb, 2) - s12 - s13 - s14)*CFmly5[3] + (-pow(M12, 2) + pow(ma, 2) + pow(mb, 2) + s12 - s13)*CFmly6[1] + (pow(ma, 2) + pow(mb, 2) - s13)*CFmly6[2] +
		(pow(ma, 2) + pow(mb, 2) - s12 - s13)*CFmly6[3] + (pow(M12, 2) - s12)*CFmly9[1] + (-2 * pow(ma, 2) + s12)*CFmly9[2] + (pow(ma, 2) + pow(mb, 2) - s13)*CFmly9[3] -
		(pow(M12, 2) - pow(ma, 2) - pow(mb, 2) - s12 + s13)*(pow(M34, 2) - s34)*DFmly1[1] + (pow(ma, 2) + pow(mb, 2) - s13)*(pow(M34, 2) - s34)*DFmly1[2] - (pow(ma, 2) + pow(mb, 2) - s14)*(pow(M34, 2) - s34)*DFmly1[3] +
		(pow(ma, 2) + pow(mb, 2) - s12 - s13)*(pow(M34, 2) - s34)*DFmly1[4] + (pow(M12, 2) - pow(ma, 2) - pow(mb, 2) - s12 + s13)*(pow(M34, 2) - s34)*DFmly2[1] - (pow(ma, 2) + pow(mb, 2) - s13)*(pow(M34, 2) - s34)*DFmly2[2] +
		(pow(ma, 2) + pow(mb, 2) - s14)*(pow(M34, 2) - s34)*DFmly2[3] - (3 * pow(ma, 2) + pow(mb, 2) - s12 - s13)*(pow(M34, 2) - s34)*DFmly2[4] + pow(pow(M12, 2) - s12, 2)*DFmly4[1] - pow(pow(M12, 2) - s12, 2)*DFmly5[1])) / (16.*PI);
	double INT122 = (ALPHA * (-((pow(ma, 2) + pow(mb, 2) - s23)*CFmly4[1]) + (pow(ma, 2) + pow(mb, 2) - s24)*CFmly4[2] + (pow(ma, 2) - pow(mb, 2) + s23)*CFmly4[3] + (pow(ma, 2) + pow(mb, 2) - s23)*CFmly8[1] + (pow(ma, 2) + pow(mb, 2) - s23)*CFmly8[2] -
		(pow(ma, 2) + pow(mb, 2) - s24)*CFmly8[3] + (pow(ma, 2) + pow(mb, 2) - s23)*(pow(M12, 2) + pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[1] +
		(pow(M12, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) - s12 - 2 * s23)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly1[2] - (pow(M12, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) - s12 - 2 * s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[3] +
		(pow(M12, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) - s12 - 2 * s23)*(pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[4] + 4 * pow(ma, 2)*DFmly1[5] + pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly1[6] -
		2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[7] + 2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[8] + pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly1[9] -
		2 * (pow(ma, 2) + pow(mb, 2) - s12 - s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[10] + pow(pow(ma, 2) + pow(mb, 2) - s12 - s23, 2)*DFmly1[11] - 2 * pow(ma, 2)*(pow(M12, 2) - s12)*DFmly3[1] +
		(pow(M12, 2) - s12)*(-2 * pow(ma, 2) + s12)*DFmly3[2] - (pow(M12, 2) - s12)*(pow(ma, 2) - pow(mb, 2) + s23)*DFmly3[3] + (pow(M12, 2) - s12)*(2 * pow(mb, 2) - s23 - s24)*DFmly3[4])) / (16.*PI);
	double INT123 = -(ALPHA * (BFmly[3] - BFmly[4] - (pow(M12, 2) - s12)*CFmly10[1] + s12*CFmly10[2] + (2 * pow(ma, 2) + 2 * pow(mb, 2) - s23 - s24)*CFmly10[3] + (pow(M12, 2) + pow(ma, 2) + pow(mb, 2) - s12 - s23)*CFmly2[1] - (pow(ma, 2) + pow(mb, 2) - s24)*CFmly2[2] +
		(pow(ma, 2) + pow(mb, 2) - s12 - s23)*CFmly2[3] - 2 * pow(mb, 2)*CFmly4[1] + (-2 * pow(mb, 2) + s34)*CFmly4[2] - (-pow(ma, 2) + pow(mb, 2) + s23)*CFmly4[3] - (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[1] +
		(pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s13)*DFmly3[2] + (pow(M12, 2) - s12)*(-pow(ma, 2) + pow(mb, 2) + s23)*DFmly3[3] - (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[4])) / (16.*PI);
	double INT124 = (ALPHA * (BFmly[2] - BFmly[4] - BFmly[5] + BFmly[6] + (pow(M12, 2) - s12)*CFmly1[1] + (-pow(M12, 2) + s12)*CFmly10[1] + s12*CFmly10[2] + (2 * pow(ma, 2) + 2 * pow(mb, 2) - s23 - s24)*CFmly10[3] + (-pow(M12, 2) + s12)*CFmly3[1] +
		(-pow(M34, 2) + s34)*CFmly4[1] + (pow(M12, 2) + pow(ma, 2) + pow(mb, 2) - s12 - s23)*CFmly6[1] + (pow(ma, 2) + pow(mb, 2) - s23)*CFmly6[2] + (pow(ma, 2) + pow(mb, 2) - s12 - s23)*CFmly6[3] + (pow(M34, 2) - s34)*CFmly8[1] +
		(pow(M12, 2) + pow(ma, 2) + pow(mb, 2) - s12 - s23)*(pow(M34, 2) - s34)*DFmly1[1] + (pow(ma, 2) + pow(mb, 2) - s23)*(pow(M34, 2) - s34)*DFmly1[2] - (pow(ma, 2) + pow(mb, 2) - s24)*(pow(M34, 2) - s34)*DFmly1[3] +
		(pow(ma, 2) + pow(mb, 2) - s12 - s23)*(pow(M34, 2) - s34)*DFmly1[4] - (pow(M12, 2) - s12)*(pow(M34, 2) - s34)*DFmly3[1])) / (16.*PI);
	double INT133 = (ALPHA * ((2 * pow(ma, 2) + 2 * pow(mb, 2) - s13 - s23)*CFmly10[2] + s34*CFmly10[3] + 2 * pow(mb, 2)*CFmly2[1] + (2 * pow(mb, 2) - s34)*CFmly2[2] - (2 * pow(ma, 2) - s13 - s23)*CFmly2[3] - 2 * pow(mb, 2)*CFmly4[1] + (-2 * pow(mb, 2) + s34)*CFmly4[2] -
		(-pow(ma, 2) + pow(mb, 2) + s23)*CFmly4[3] + (pow(ma, 2) + pow(mb, 2) - s23)*CFmly5[1] + (pow(ma, 2) + pow(mb, 2) - s23)*CFmly5[2] + (pow(ma, 2) + pow(mb, 2) - s23 - s34)*CFmly5[3] -
		(pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[1] + (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s13)*DFmly3[2] + (pow(M12, 2) - s12)*(-pow(ma, 2) + pow(mb, 2) + s23)*DFmly3[3] -
		(pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[4] + (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly5[1] - (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s13)*DFmly5[2] +
		(pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly5[3] + (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly5[4])) / (16.*PI);
	double INT134 = (ALPHA * (BFmly[1] + BFmly[5] - BFmly[6] - BFmly[7] + (-pow(M12, 2) + s12)*CFmly1[1] + (2 * pow(ma, 2) + 2 * pow(mb, 2) - s14 - s24)*CFmly10[2] + s34*CFmly10[3] + (-pow(M34, 2) + s34)*CFmly2[1] + (pow(M12, 2) - s12)*CFmly3[1] +
		(pow(M34, 2) - s34)*CFmly4[1] + (pow(ma, 2) + pow(mb, 2) - s24)*CFmly5[1] + (pow(ma, 2) + pow(mb, 2) - s24)*CFmly5[2] + (pow(ma, 2) + pow(mb, 2) - s24 - s34)*CFmly5[3] + (pow(M12, 2) - s12)*(pow(M34, 2) - s34)*DFmly3[1] +
		(pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[1] - (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly5[2] + (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[3] +
		(pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[4])) / (16.*PI);
	double INT144 = (ALPHA * (-(EFmly*(pow(M12, 2) - s12)*pow(pow(M34, 2) - s34, 2)) + (-pow(M34, 2) + s34)*CFmly10[1] - (2 * pow(ma, 2) + 2 * pow(mb, 2) - s14 - s24)*CFmly10[2] - s34*CFmly10[3] + (pow(M34, 2) - pow(ma, 2) - pow(mb, 2) + s24 - s34)*CFmly5[1] -
		(pow(ma, 2) + pow(mb, 2) - s24)*CFmly5[2] - (pow(ma, 2) + pow(mb, 2) - s24 - s34)*CFmly5[3] + (pow(M34, 2) + 2 * pow(mb, 2) - 2 * s34)*CFmly6[1] + (2 * pow(mb, 2) - s34)*CFmly6[2] +
		(2 * pow(ma, 2) + 4 * pow(mb, 2) - s14 - s24 - s34)*CFmly6[3] + (-pow(M34, 2) + s34)*CFmly9[1] - (pow(ma, 2) + pow(mb, 2) - s24)*CFmly9[2] + (2 * pow(mb, 2) - s34)*CFmly9[3] + pow(pow(M34, 2) - s34, 2)*DFmly1[1] -
		pow(pow(M34, 2) - s34, 2)*DFmly2[1] - (pow(M12, 2) - s12)*(pow(M34, 2) - pow(ma, 2) - pow(mb, 2) + s24 - s34)*DFmly4[1] - (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly4[2] +
		(pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly4[3] + (pow(M12, 2) - s12)*(pow(ma, 2) + 3 * pow(mb, 2) - s24 - s34)*DFmly4[4] + (pow(M12, 2) - s12)*(pow(M34, 2) - pow(ma, 2) - pow(mb, 2) + s24 - s34)*DFmly5[1] +
		(pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s14)*DFmly5[2] - (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[3] - (pow(M12, 2) - s12)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[4])) / (16.*PI);
	double INT222 = (ALPHA * (-(pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly1[1]) - 2 * pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly1[2] + 2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[3] -
		2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[4] - 4 * pow(ma, 2)*DFmly1[5] - pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly1[6] +
		2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[7] - 2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s12 - s23)*DFmly1[8] - pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly1[9] +
		2 * (pow(ma, 2) + pow(mb, 2) - s12 - s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly1[10] - pow(pow(ma, 2) + pow(mb, 2) - s12 - s23, 2)*DFmly1[11] + 4 * pow(ma, 4)*DFmly3[1] - 4 * pow(ma, 2)*(-2 * pow(ma, 2) + s12)*DFmly3[2] +
		4 * pow(ma, 2)*(pow(ma, 2) - pow(mb, 2) + s23)*DFmly3[3] - 4 * pow(ma, 2)*(2 * pow(mb, 2) - s23 - s24)*DFmly3[4] + 4 * pow(ma, 2)*DFmly3[5] + pow(-2 * pow(ma, 2) + s12, 2)*DFmly3[6] +
		2 * (2 * pow(ma, 2) - s12)*(pow(ma, 2) - pow(mb, 2) + s23)*DFmly3[7] - 2 * (2 * pow(ma, 2) - s12)*(2 * pow(mb, 2) - s23 - s24)*DFmly3[8] + pow(pow(ma, 2) - pow(mb, 2) + s23, 2)*DFmly3[9] -
		2 * (pow(ma, 2) - pow(mb, 2) + s23)*(2 * pow(mb, 2) - s23 - s24)*DFmly3[10] + pow(-2 * pow(mb, 2) + s23 + s24, 2)*DFmly3[11])) / (16.*PI);
	double INT223 = (ALPHA * ((pow(ma, 2) + pow(mb, 2) - s13)*CFmly1[2] - (pow(ma, 2) + pow(mb, 2) - s23 - s34)*CFmly1[3] + s12*CFmly10[2] + (2 * pow(ma, 2) + 2 * pow(mb, 2) - s23 - s24)*CFmly10[3] + 2 * pow(mb, 2)*CFmly2[1] -
		(pow(ma, 2) - pow(mb, 2) - s24 + s34)*CFmly2[2] - (pow(ma, 2) - pow(mb, 2) + s12 - s13)*CFmly2[3] - 2 * pow(ma, 2)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[1] -
		(pow(ma, 2) - pow(mb, 2) - s12 + s13)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[2] - 2 * (pow(ma, 2) - pow(mb, 2))*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[3] -
		(pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) - pow(mb, 2) + s24 - s34)*DFmly3[4] - 4 * pow(mb, 2)*DFmly3[5] - pow(pow(ma, 2) + pow(mb, 2) - s13, 2)*DFmly3[6] +
		2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) - pow(mb, 2) - s23)*DFmly3[7] + 2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[8] - pow(-pow(ma, 2) + pow(mb, 2) + s23, 2)*DFmly3[9] +
		2 * (-pow(ma, 2) + pow(mb, 2) + s23)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[10] - pow(pow(ma, 2) + pow(mb, 2) - s23 - s34, 2)*DFmly3[11])) / (16.*PI);
	double INT224 = (ALPHA * (-2 * pow(ma, 2)*CFmly1[1] + (-2 * pow(ma, 2) + s12)*CFmly1[2] + (2 * pow(mb, 2) - s23 - s24)*CFmly1[3] - s12*CFmly10[2] - (2 * pow(ma, 2) + 2 * pow(mb, 2) - s23 - s24)*CFmly10[3] + 2 * pow(ma, 2)*CFmly3[1] + (2 * pow(ma, 2) - s12)*CFmly3[2] +
		(pow(ma, 2) - pow(mb, 2) + s23)*CFmly3[3] - (pow(ma, 2) + pow(mb, 2) - s23)*CFmly6[1] - (pow(ma, 2) + pow(mb, 2) - s23)*CFmly6[2] - (pow(ma, 2) + pow(mb, 2) - s12 - s23)*CFmly6[3] -
		(pow(ma, 2) + pow(mb, 2) - s23)*(pow(M34, 2) - s34)*DFmly1[1] - (pow(ma, 2) + pow(mb, 2) - s23)*(pow(M34, 2) - s34)*DFmly1[2] + (pow(ma, 2) + pow(mb, 2) - s24)*(pow(M34, 2) - s34)*DFmly1[3] -
		(pow(ma, 2) + pow(mb, 2) - s12 - s23)*(pow(M34, 2) - s34)*DFmly1[4] + 2 * pow(ma, 2)*(pow(M34, 2) - s34)*DFmly3[1] + (2 * pow(ma, 2) - s12)*(pow(M34, 2) - s34)*DFmly3[2] + (pow(ma, 2) - pow(mb, 2) + s23)*(pow(M34, 2) - s34)*DFmly3[3] -
		(2 * pow(mb, 2) - s23 - s24)*(pow(M34, 2) - s34)*DFmly3[4])) / (16.*PI);
	double INT233 = (ALPHA * (-((2 * pow(ma, 2) + 2 * pow(mb, 2) - s13 - s23)*CFmly10[2]) - s34*CFmly10[3] - 2 * pow(mb, 2)*CFmly2[1] + (-2 * pow(mb, 2) + s34)*CFmly2[2] + (2 * pow(ma, 2) - s13 - s23)*CFmly2[3] + pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly3[1] -
		2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[2] + 2 * (pow(ma, 2) - pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[3] +
		2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[4] + 4 * pow(mb, 2)*DFmly3[5] + pow(pow(ma, 2) + pow(mb, 2) - s13, 2)*DFmly3[6] -
		2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) - pow(mb, 2) - s23)*DFmly3[7] - 2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[8] + pow(-pow(ma, 2) + pow(mb, 2) + s23, 2)*DFmly3[9] -
		2 * (-pow(ma, 2) + pow(mb, 2) + s23)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[10] + pow(pow(ma, 2) + pow(mb, 2) - s23 - s34, 2)*DFmly3[11])) / (16.*PI);
	double INT234 = (ALPHA * (-BFmly[1] + BFmly[7] + (pow(ma, 2) + pow(mb, 2) - s23)*CFmly1[1] - (pow(ma, 2) + pow(mb, 2) - s13)*CFmly1[2] + (pow(ma, 2) + pow(mb, 2) - s23 - s34)*CFmly1[3] - (2 * pow(ma, 2) + 2 * pow(mb, 2) - s14 - s24)*CFmly10[2] - s34*CFmly10[3] +
		(pow(M34, 2) - s34)*CFmly2[1] - (pow(ma, 2) + pow(mb, 2) - s23)*CFmly3[1] + (pow(ma, 2) + pow(mb, 2) - s13)*CFmly3[2] + (-pow(ma, 2) + pow(mb, 2) + s23)*CFmly3[3] - (pow(ma, 2) + pow(mb, 2) - s23)*(pow(M34, 2) - s34)*DFmly3[1] +
		(pow(ma, 2) + pow(mb, 2) - s13)*(pow(M34, 2) - s34)*DFmly3[2] + (-pow(ma, 2) + pow(mb, 2) + s23)*(pow(M34, 2) - s34)*DFmly3[3] + (pow(M34, 2) - s34)*(-pow(ma, 2) - pow(mb, 2) + s23 + s34)*DFmly3[4])) / (16.*PI);
	double INT244 = (ALPHA * ((-pow(M34, 2) + pow(ma, 2) + pow(mb, 2) - s24 + s34)*CFmly1[1] - (pow(ma, 2) + pow(mb, 2) - s14)*CFmly1[2] + (pow(ma, 2) + pow(mb, 2) - s24 - s34)*CFmly1[3] + (pow(M34, 2) - s34)*CFmly10[1] +
		(2 * pow(ma, 2) + 2 * pow(mb, 2) - s14 - s24)*CFmly10[2] + s34*CFmly10[3] + (pow(M34, 2) - pow(ma, 2) - pow(mb, 2) + s24 - s34)*CFmly3[1] + (pow(ma, 2) + pow(mb, 2) - s14)*CFmly3[2] - (pow(ma, 2) + 3 * pow(mb, 2) - s24 - s34)*CFmly3[3] -
		(pow(M34, 2) + 2 * pow(mb, 2) - 2 * s34)*CFmly6[1] + (-2 * pow(mb, 2) + s34)*CFmly6[2] - (2 * pow(ma, 2) + 4 * pow(mb, 2) - s14 - s24 - s34)*CFmly6[3] - pow(pow(M34, 2) - s34, 2)*DFmly1[1] + pow(pow(M34, 2) - s34, 2)*DFmly3[1])) / (16.*PI);
	double INT333 = (ALPHA * (-(pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly3[1]) + 2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[2] - 2 * (pow(ma, 2) - pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly3[3] -
		2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[4] - 4 * pow(mb, 2)*DFmly3[5] - pow(pow(ma, 2) + pow(mb, 2) - s13, 2)*DFmly3[6] +
		2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) - pow(mb, 2) - s23)*DFmly3[7] + 2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[8] - pow(-pow(ma, 2) + pow(mb, 2) + s23, 2)*DFmly3[9] +
		2 * (-pow(ma, 2) + pow(mb, 2) + s23)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly3[10] - pow(pow(ma, 2) + pow(mb, 2) - s23 - s34, 2)*DFmly3[11] + pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly5[1] -
		2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly5[2] + 2 * pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly5[3] + 2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly5[4] +
		4 * pow(mb, 2)*DFmly5[5] + pow(pow(ma, 2) + pow(mb, 2) - s13, 2)*DFmly5[6] - 2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s23)*DFmly5[7] -
		2 * (pow(ma, 2) + pow(mb, 2) - s13)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly5[8] + pow(pow(ma, 2) + pow(mb, 2) - s23, 2)*DFmly5[9] + 2 * (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s23 - s34)*DFmly5[10] +
		pow(pow(ma, 2) + pow(mb, 2) - s23 - s34, 2)*DFmly5[11])) / (16.*PI);
	double INT334 = (ALPHA * ((-pow(ma, 2) - pow(mb, 2) + s23)*CFmly1[1] - (s13 - s14)*CFmly1[2] + (s23 - s24)*CFmly1[3] + (pow(ma, 2) + pow(mb, 2) - s23)*CFmly3[1] - (pow(ma, 2) + pow(mb, 2) - s13)*CFmly3[2] - (-pow(ma, 2) + pow(mb, 2) + s23)*CFmly3[3] +
		(pow(ma, 2) + pow(mb, 2) - s14)*CFmly7[1] - (pow(ma, 2) + pow(mb, 2) - s24)*CFmly7[2] + (pow(ma, 2) + pow(mb, 2) - s23)*(pow(M34, 2) - s34)*DFmly3[1] - (pow(ma, 2) + pow(mb, 2) - s13)*(pow(M34, 2) - s34)*DFmly3[2] -
		(-pow(ma, 2) + pow(mb, 2) + s23)*(pow(M34, 2) - s34)*DFmly3[3] - (pow(M34, 2) - s34)*(-pow(ma, 2) - pow(mb, 2) + s23 + s34)*DFmly3[4] + (pow(ma, 2) + pow(mb, 2) - s23)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[1] +
		(pow(mb, 2)*s13 + pow(M34, 2)*(pow(ma, 2) + pow(mb, 2) - s14) - pow(mb, 2)*s14 - s13*s24 + s14*s24 + pow(ma, 2)*(s13 - s14 - s34) - pow(mb, 2)*s34 + s14*s34)*DFmly5[2] +
		(pow(ma, 2) + pow(mb, 2) - s24)*(-pow(M34, 2) - s23 + s24 + s34)*DFmly5[3] - (pow(mb, 2)*s23 - pow(mb, 2)*s24 - s23*s24 + pow(s24, 2) + pow(M34, 2)*(pow(ma, 2) + pow(mb, 2) - s24 - s34) + pow(ma, 2)*(s23 - s24 - s34) -
			pow(mb, 2)*s34 + s24*s34 + pow(s34, 2))*DFmly5[4] - 4 * pow(mb, 2)*DFmly5[5] - pow(pow(ma, 2) + pow(mb, 2) - s14, 2)*DFmly5[6] + 2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[7] +
		2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[8] - pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly5[9] - 2 * (pow(ma, 2) + pow(mb, 2) - s24)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[10] -
		pow(pow(ma, 2) + pow(mb, 2) - s24 - s34, 2)*DFmly5[11])) / (16.*PI);
	double INT344 = -(ALPHA * ((-pow(M34, 2) + pow(ma, 2) + pow(mb, 2) - s24 + s34)*CFmly1[1] - (pow(ma, 2) + pow(mb, 2) - s14)*CFmly1[2] + (pow(ma, 2) + pow(mb, 2) - s24 - s34)*CFmly1[3] - (-pow(M34, 2) + pow(ma, 2) + pow(mb, 2) - s24 + s34)*CFmly3[1] +
		(pow(ma, 2) + pow(mb, 2) - s14)*CFmly3[2] - (pow(ma, 2) + 3 * pow(mb, 2) - s24 - s34)*CFmly3[3] + pow(pow(M34, 2) - s34, 2)*DFmly3[1] - pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly5[1] +
		2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[2] - 2 * pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly5[3] - 2 * (pow(ma, 2) + pow(mb, 2) - s24)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[4] -
		4 * pow(mb, 2)*DFmly5[5] - pow(pow(ma, 2) + pow(mb, 2) - s14, 2)*DFmly5[6] + 2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[7] +
		2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[8] - pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly5[9] - 2 * (pow(ma, 2) + pow(mb, 2) - s24)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[10] -
		pow(pow(ma, 2) + pow(mb, 2) - s24 - s34, 2)*DFmly5[11])) / (16.*PI);
	double INT444 = (ALPHA * (EFmly*pow(pow(M34, 2) - s34, 3) + (pow(pow(ma, 2) + pow(mb, 2) - s24, 2) - (pow(ma, 2) + pow(mb, 2) - s24)*(pow(M34, 2) - s34) + pow(pow(M34, 2) - s34, 2))*DFmly4[1] -
		(pow(ma, 2) + pow(mb, 2) - s14)*(-pow(M34, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) - 2 * s24 + s34)*DFmly4[2] + (pow(ma, 2) + pow(mb, 2) - s24)*(-pow(M34, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) - 2 * s24 + s34)*DFmly4[3] +
		(pow(ma, 2) + 3 * pow(mb, 2) - s24 - s34)*(-pow(M34, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) - 2 * s24 + s34)*DFmly4[4] + 4 * pow(mb, 2)*DFmly4[5] + pow(pow(ma, 2) + pow(mb, 2) - s14, 2)*DFmly4[6] -
		2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly4[7] - 2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + 3 * pow(mb, 2) - s24 - s34)*DFmly4[8] + pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly4[9] +
		2 * (pow(ma, 2) + pow(mb, 2) - s24)*(pow(ma, 2) + 3 * pow(mb, 2) - s24 - s34)*DFmly4[10] + pow(-pow(ma, 2) - 3 * pow(mb, 2) + s24 + s34, 2)*DFmly4[11] -
		(pow(pow(ma, 2) + pow(mb, 2) - s24, 2) - (pow(ma, 2) + pow(mb, 2) - s24)*(pow(M34, 2) - s34) + pow(pow(M34, 2) - s34, 2))*DFmly5[1] +
		(pow(ma, 2) + pow(mb, 2) - s14)*(-pow(M34, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) - 2 * s24 + s34)*DFmly5[2] - (pow(ma, 2) + pow(mb, 2) - s24)*(-pow(M34, 2) + 2 * pow(ma, 2) + 2 * pow(mb, 2) - 2 * s24 + s34)*DFmly5[3] +
		(pow(ma, 2) + pow(mb, 2) - s24 - s34)*(pow(M34, 2) - 2 * pow(ma, 2) - 2 * pow(mb, 2) + 2 * s24 - s34)*DFmly5[4] - 4 * pow(mb, 2)*DFmly5[5] - pow(pow(ma, 2) + pow(mb, 2) - s14, 2)*DFmly5[6] +
		2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + pow(mb, 2) - s24)*DFmly5[7] + 2 * (pow(ma, 2) + pow(mb, 2) - s14)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[8] - pow(pow(ma, 2) + pow(mb, 2) - s24, 2)*DFmly5[9] -
		2 * (pow(ma, 2) + pow(mb, 2) - s24)*(pow(ma, 2) + pow(mb, 2) - s24 - s34)*DFmly5[10] - pow(pow(ma, 2) + pow(mb, 2) - s24 - s34, 2)*DFmly5[11])) / (16.*PI);

	double I3[] = { INTKK1, INTKK2, INTKK3, INTKK4, INT111, INT112, INT113, INT114, INT122, INT123, INT124, INT133, INT134, INT144, INT222, INT223, INT224, INT233, INT234, INT244, INT333, INT334, INT344, INT444 };

	//###########################
	/// Coefficients for 'D' part

	/// Pent1DR1Coeff(): The coefficients k.pi for the \[Epsilon]^\[Mu]\[Nu]\[Rho]\[Sigma](\*SubsuperscriptBox[\(p\), \(ij\), \(\[Mu]\)]\)(\*SubsuperscriptBox[\(p\), \(kl\), \(\[Rho]\)]\) term; name them CiA
	double C1A = -(((-pow(ma, 2) - pow(mb, 2) + s23)*((-8 * pow(M, 2)*pow(mb, 2)*y12*z - 2 * pow(M, 2)*s34*y12*pow(y34, 2)*z + y34*(16 * pow(ma, 2)*s34 + pow(M, 4)*pow(y12, 2)*pow(z, 2)))*lmbd - 2 * s34*pow(lmbd, 2) - pow(y12, 2)*y34*pow(lmbd, 3) +
		pow(M, 2)*(2 * s34*y34*lmbd - pow(M, 2)*y12*z*lmbd + pow(lmbd, 2))*Xi)) / (s12*s34));
	double C2A = -(2 * (64 * pow(ma, 2)*pow(mb, 2) + (pow(M, 2) + 3 * s12 - s34)*s34)*pow(lmbd, 2) - (-8 * pow(mb, 2)*(pow(M, 2) + 3 * s12 - s34)*pow(y12, 2) +
		(pow(M, 4) + 16 * pow(ma, 2)*s34 - 8 * pow(mb, 2)*(s12 + s34) + (s12 + s34)*(s12 + 5 * s34) + pow(M, 2)*(8 * pow(mb, 2) - 2 * (s12 + 3 * s34)))*y12*y34 - 48 * pow(ma, 2)*s34*pow(y34, 2))*pow(lmbd, 2) +
		(-pow(M, 2) + s12 + 5 * s34)*y12*pow(y34, 2)*pow(lmbd, 3) + pow(y12, 2)*(y12 - 3 * y34)*y34*pow(lmbd, 4) + 2 * lmbd*(s34*y34*(8 * pow(M, 2)*pow(ma, 2)*z + pow(lmbd, 2)) + y12*(-16 * pow(mb, 2)*s12*s34 + (4 * pow(mb, 2) + s34)*pow(lmbd, 2)))) / (4.*s12*s34) +
		((-((4 * (2 * pow(M, 2)*pow(mb, 2) + s12*s34 - 2 * pow(mb, 2)*(s12 + s34))*y12 + (pow(pow(M, 2) - s12, 2) - 4 * (pow(M, 2) + 4 * pow(ma, 2))*s34 + 3 * pow(s34, 2))*y34)*lmbd) + (-pow(M, 2) - 16 * pow(mb, 2) + s12 + 3 * s34)*pow(lmbd, 2) +
		((-s12 - s34)*pow(y12, 2) + pow(M, 2)*y12*(y12 - 6 * y34) + 2 * (3 * s12 + 2 * s34)*y12*y34 - 2 * s34*pow(y34, 2))*pow(lmbd, 2) - pow(y12, 2)*y34*pow(lmbd, 3))*(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi)) / (4.*s12*s34) +
			(lmbd*(-2 * s34*y34 + pow(M, 2)*y12*z + 3 * lmbd)*pow(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi, 2)) / (4.*s12*s34);
	double C3A = -(lmbd*(-2 * (64 * pow(ma, 2)*pow(mb, 2) + s12*(pow(M, 2) - s12 + 3 * s34))*lmbd + (16 * pow(mb, 2)*s12*y12*(-3 * y12 + y34) + y34*(-8 * pow(ma, 2)*(pow(M, 2) - s12 + 3 * s34)*y34 + pow(M, 2)*(pow(M, 2) + 8 * pow(ma, 2) - 5 * s12 - s34)*y12*z))*lmbd +
		(-pow(M, 2) + 5 * s12 + s34)*pow(y12, 2)*y34*pow(lmbd, 2) + y12*(3 * y12 - y34)*pow(y34, 2)*pow(lmbd, 3) + 2 * (8 * pow(M, 2)*pow(mb, 2)*s12*y12*z + s12*(y12 + y34)*pow(lmbd, 2) + 4 * pow(ma, 2)*y34*(-4 * s12*s34 + pow(lmbd, 2))))) / (4.*s12*s34) +
		((-(((pow(M, 4) - 16 * pow(mb, 2)*s12 + 3 * pow(s12, 2) + pow(s34, 2) - 2 * pow(M, 2)*(2 * s12 + s34))*y12 + 4 * (2 * pow(M, 2)*pow(ma, 2) + s12*s34 - 2 * pow(ma, 2)*(s12 + s34))*y34)*lmbd) + (pow(M, 2) + 16 * pow(ma, 2) - 3 * s12 - s34)*pow(lmbd, 2) +
		((pow(M, 2) - s34)*(6 * y12 - y34)*y34 + s12*(2 * pow(y12, 2) - 4 * y12*y34 + pow(y34, 2)))*pow(lmbd, 2) - y12*pow(y34, 2)*pow(lmbd, 3))*(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi)) / (4.*s12*s34) -
			(lmbd*(2 * s12*y12 - pow(M, 2)*y34 + (s12 + s34)*y34 + 3 * lmbd)*pow(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi, 2)) / (4.*s12*s34);
	double C4A = -(((-pow(ma, 2) - pow(mb, 2) + s23)*(2 * ((8 * pow(mb, 2)*s12*y12 + 2 * s12*s34*y12*pow(y34, 2) - pow(M, 2)*(4 * pow(ma, 2) + s12*pow(y12, 2))*y34*z)*lmbd + s12*pow(lmbd, 2)) + pow(M, 2)*(2 * s12*y12 - pow(M, 2)*y34*z - lmbd)*lmbd*Xi)) / (s12*s34));
	/// The coefficients k.pi for the \[Epsilon]^\[Mu]\[Nu]\[Rho]\[Sigma]P^\[Mu]k^\[Rho] term;name them CiB
	double C1B = ((-pow(ma, 2) - pow(mb, 2) + s23)*(-4 * (pow(M, 2) - s12 + s34)*(16 * pow(ma, 2)*pow(mb, 2) + s12*s34) - 8 * pow(mb, 2)*(pow(M, 2) + s12 - s34)*y12*lmbd + 8 * pow(mb, 2)*pow(y12, 2)*pow(lmbd, 2) + y12*pow(y34, 2)*pow(lmbd, 3) -
		y34*lmbd*(pow(M, 2) - s12 + s34 + y12*lmbd)*(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi) + (pow(M, 2) - s12 + s34)*pow(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi, 2))) / (s12*s34);
	double C2B = ((-pow(ma, 2) - pow(mb, 2) + s23)*(-4 * (pow(M, 2) - s12 + s34)*(16 * pow(ma, 2)*pow(mb, 2) + s12*s34) + 8 * pow(mb, 2)*(pow(M, 2) + s12 - s34)*y12*lmbd + 8 * pow(mb, 2)*pow(y12, 2)*pow(lmbd, 2) - y12*pow(y34, 2)*pow(lmbd, 3) +
		y34*lmbd*(pow(M, 2) - s12 + s34 - y12*lmbd)*(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi) + (pow(M, 2) - s12 + s34)*pow(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi, 2))) / (s12*s34);
	double C3B = ((-pow(ma, 2) - pow(mb, 2) + s23)*(4 * (pow(M, 2) + s12 - s34)*(16 * pow(ma, 2)*pow(mb, 2) + s12*s34) + 8 * pow(ma, 2)*(pow(M, 2) - s12 + s34)*y34*lmbd - 8 * pow(ma, 2)*pow(y34, 2)*pow(lmbd, 2) - pow(y12, 2)*y34*pow(lmbd, 3) +
		y12*lmbd*(pow(M, 2) + s12 - s34 + y34*lmbd)*(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi) - (pow(M, 2) + s12 - s34)*pow(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi, 2))) / (s12*s34);
	double C4B = ((-pow(ma, 2) - pow(mb, 2) + s23)*(4 * (pow(M, 2) + s12 - s34)*(16 * pow(ma, 2)*pow(mb, 2) + s12*s34) - 8 * pow(ma, 2)*(pow(M, 2) - s12 + s34)*y34*lmbd - 8 * pow(ma, 2)*pow(y34, 2)*pow(lmbd, 2) + pow(y12, 2)*y34*pow(lmbd, 3) +
		y12*lmbd*(-pow(M, 2) - s12 + s34 + y34*lmbd)*(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi) + (-pow(M, 2) - s12 + s34)*pow(pow(M, 2)*y12*y34*z - pow(M, 2)*Xi, 2))) / (s12*s34);

	double C1D[] = { C1A + C1B, C2A + C2B, C3A + C3B, C4A + C4B };

	/// Pent1DR2Coeff(): The coefficients (k.pi) (k.pj) for the \[Epsilon]^\[Mu]\[Nu]\[Rho]\[Sigma] (\*SubsuperscriptBox[\(p\), \(ij\), \(\[Mu]\)]\) (\*SubsuperscriptBox[\(p\), \(kl\), \(\[Rho]\)]\) term; name them CijA
	double CKKA = -(pow(lmbd, 2)*(-4 * (4 * pow(ma, 2) + s12 + s12*pow(y12, 2))*(4 * pow(mb, 2) + s34 + s34*pow(y34, 2)) + pow(M, 2)*(-y12 + y34)*lmbd*Xi + pow(M, 4)*pow(Xi, 2))) / (4.*s12*s34);
	double C11A = (4 * pow(mb, 2)*(4 * s12*s34 - pow(M, 2)*y12*z*lmbd + pow(lmbd, 2)) + s34*(16 * pow(ma, 2)*s34 + (y12 + y34)*lmbd*(pow(M, 2)*(1 - y12*y34)*z + (-y12 + y34)*lmbd + pow(M, 2)*Xi))) / (s12*s34);
	double C12A = (-2 * (4 * pow(mb, 2)*(-4 * s12*s34 + pow(lmbd, 2)) + s34*(-16 * pow(ma, 2)*s34 + lmbd*((-1 + pow(y12, 2) + y12*y34 + pow(y34, 2))*lmbd + pow(M, 2)*y12*(z + y12*y34*z - Xi))))) / (s12*s34);
	double C13A = (2 * (-8 * pow(M, 2)*(pow(mb, 2)*s12 + pow(ma, 2)*s34)*z + 2 * (2 * pow(mb, 2)*s12*y12 + s34*(-(s12*y12) + 2 * pow(ma, 2)*y34 + s12*(-2 + pow(y12, 2))*y34))*lmbd + (pow(M, 2)*lmbd*(-(pow(M, 2)*y12*z) + lmbd)*Xi) / 2.)) / (s12*s34);
	double C14A = (-16 * pow(M, 2)*(pow(mb, 2)*s12 + pow(ma, 2)*s34)*z + 8 * (pow(mb, 2)*s12*y12 - pow(ma, 2)*s34*y34)*lmbd) / (s12*s34);
	double C22A = (-(pow(M, 2)*(-4 * pow(mb, 2)*y12 + s34*(y34 + y12*(3 + (y12 - y34)*y34)))*z*lmbd) + s34*(-2 - pow(y12, 2) - 2 * y12*y34 + pow(y34, 2))*pow(lmbd, 2) + 4 * (4 * pow(mb, 2)*s12*s34 + 4 * pow(ma, 2)*pow(s34, 2) + pow(mb, 2)*pow(lmbd, 2)) +
		pow(M, 2)*s34*(y12 - y34)*lmbd*Xi) / (s12*s34);
	double C23A = (-16 * pow(M, 2)*(pow(mb, 2)*s12 + pow(ma, 2)*s34)*z + 4 * (-2 * pow(mb, 2)*s12*y12 + s34*(2 * pow(ma, 2)*y34 + s12*(y12 - y34)*(1 + y12*y34)))*lmbd - pow(M, 2)*lmbd*(pow(M, 2)*(y12 - y34)*z + 2 * lmbd)*Xi) / (s12*s34);
	double C24A = (-16 * pow(M, 2)*(pow(mb, 2)*s12 + pow(ma, 2)*s34)*z - 4 * (2 * pow(mb, 2)*s12*y12 + s34*(2 * pow(ma, 2)*y34 + s12*(-y34 + y12*(-2 + pow(y34, 2)))))*lmbd + pow(M, 2)*lmbd*(pow(M, 2)*y34*z + lmbd)*Xi) / (s12*s34);
	double C33A = (pow(M, 2)*(-4 * pow(ma, 2)*y34 + s12*(3 * y34 + y12*(1 - y12*y34 + pow(y34, 2))))*z*lmbd + s12*(-2 + pow(y12, 2) - 2 * y12*y34 - pow(y34, 2))*pow(lmbd, 2) + 4 * (4 * pow(mb, 2)*pow(s12, 2) + 4 * pow(ma, 2)*s12*s34 + pow(ma, 2)*pow(lmbd, 2)) +
		pow(M, 2)*s12*(y12 - y34)*lmbd*Xi) / (s12*s34);
	double C34A = (2 * (16 * pow(mb, 2)*pow(s12, 2) + 4 * pow(ma, 2)*(4 * s12*s34 - pow(lmbd, 2)) + s12*lmbd*(lmbd - (pow(y12, 2) + y12*y34 + pow(y34, 2))*lmbd + pow(M, 2)*y34*(z + y12*y34*z - Xi)))) / (s12*s34);
	double C44A = (16 * pow(mb, 2)*pow(s12, 2) + 4 * pow(ma, 2)*(4 * s12*s34 + lmbd*(pow(M, 2)*y34*z + lmbd)) + s12*(y12 + y34)*lmbd*((y12 - y34)*lmbd + pow(M, 2)*(-z + y12*y34*z - Xi))) / (s12*s34);
	/// The coefficients k.pi)(k.pj) for the \[Epsilon]^\[Mu]\[Nu]\[Rho]\[Sigma]P^\[Mu]k^\[Rho] term;name them CijB
	double CKKB = -(s12*(8 * pow(mb, 2)*(-1 + y12*y34) + 2 * s34*(-1 + y12*y34)*(pow(y12, 2) + pow(y34, 2)))*pow(lmbd, 2) +
		2 * ((pow(M, 2)*(4 * pow(mb, 2)*s12*y12 + s34*(-4 * pow(ma, 2)*y34 - s12*(y12 - y34)*(-1 + y12*y34))) - (s12 - s34)*(4 * pow(mb, 2)*s12*y12 + s34*(4 * pow(ma, 2)*y34 + s12*(y12 + y34)*(-1 + y12*y34))))*lmbd +
			4 * pow(ma, 2)*s34*(-1 + y12*y34)*pow(lmbd, 2)) + (pow(M, 2)*(pow(M, 2)*(pow(M, 2)*(y12 - y34) + (s12 - s34)*(y12 + y34))*z*lmbd - (-8 * pow(ma, 2) - 8 * pow(mb, 2) + pow(M, 2)*(pow(y12, 2) + pow(y34, 2))*z)*pow(lmbd, 2))*Xi) / 2.) / (2.*s12*s34);
	double C11B = (8 * (8 * pow(ma, 4)*pow(mb, 2) + 8 * pow(mb, 6) - 4 * pow(mb, 4)*(s14 + 2 * s23 + s24) + s23*s34*(-s12 + s13 - s24 + s34) + pow(mb, 2)*(pow(s23 + s24, 2) + s14*(3 * s23 + s24) + s13*(s23 - s24 - s34) + (s12 + s24)*s34 - pow(s34, 2)) +
		pow(ma, 2)*(16 * pow(mb, 4) - 4 * pow(mb, 2)*(s14 + 2 * s23 + s24) + s34*(s12 - s13 - 2 * s23 + 3 * s24 + s34)))) / (s12*s34);
	double C12B = (-8 * (16 * pow(mb, 6) + 2 * s24*(s14*s23 - s13*s24) + (pow(s13, 2) - s23*(s14 + s23) + 2 * s12*s24 - 3 * s13*s24)*s34 + (2 * s12 + s13 - s24)*pow(s34, 2) - 2 * pow(ma, 4)*(8 * pow(mb, 2) + s13 - s14 - s23 + s24 + 2 * s34) -
		2 * pow(mb, 4)*(9 * s13 + 3 * s14 - s23 + 5 * s24 + 2 * s34) + pow(mb, 2)*(pow(s13, 2) + pow(s14, 2) - 2 * s12*s23 - pow(s23, 2) - 2 * s12*s24 - 4 * s23*s24 + pow(s24, 2) - 10 * s12*s34 + 3 * s23*s34 + 3 * s24*s34 +
			s14*(-2 * s23 + 2 * (s12 + s24) + s34) + s13*(2 * (s12 + s14 + 2 * s23 + 6 * s24) + s34)) + pow(ma, 2)*
			(2 * s24*(2 * s13 - s23 + s24) - 2 * s14*(s23 + s24) + (-2 * s12 + s13 + s14 + 7 * s23 - s24)*s34 - 4 * pow(s34, 2) + 4 * pow(mb, 2)*(-3 * s13 + s14 + 3 * s23 - s24 + 2 * s34)))) / (s12*s34);
	double C13B = (8 * ((s12 - s13 + s14 - s23 + 3 * s24)*(s14*s23 - s13*s24) - (s12*s23 + pow(s23, 2) + s14*(-s12 + 2 * s23) + pow(s24, 2) - s12*(s12 + 2 * s24))*s34 + s12*pow(s34, 2) + 4 * pow(ma, 4)*(-8 * pow(mb, 2) + s34) -
		4 * pow(mb, 4)*(-2 * s12 + s13 + s14 - s23 - s24 + s34) + pow(mb, 2)*(-pow(s13, 2) - pow(s14, 2) - 8 * s12*s23 + 2 * s14*s23 - pow(s23, 2) - 4 * s12*s24 - 8 * s23*s24 + pow(s24, 2) + 2 * s13*(s14 + s23 + 2 * (s12 + s24)) +
			2 * (-s12 + s14 + 2 * s23 + s24)*s34) + pow(ma, 2)*(-32 * pow(mb, 4) - (-s12 + s13 - s14 + s23 - 3 * s24)*(s13 - s14 - s23 + s24) + (-2 * s12 - s13 - s14 + s23 + s24)*s34 + 4 * pow(mb, 2)*(2 * s12 + s13 + s14 + 3 * s23 + 3 * s24 + 4 * s34)))) /
			(s12*s34);
	double C14B = (-8 * (-(pow(M, 2)*(pow(mb, 2)*s12 + pow(ma, 2)*s34)*(-2 + y12*y34)) + (pow(mb, 2)*pow(s12, 2) + pow(ma, 2)*pow(s34, 2))*(-2 + y12*y34) + (pow(ma, 2) + pow(mb, 2))*s12*s34*(-2 + 3 * y12*y34) - 2 * pow(mb, 2)*s12*y12*lmbd + 2 * pow(ma, 2)*s34*y34*lmbd +
		(pow(M, 2)*(2 * pow(mb, 2)*s12 + 2 * pow(ma, 2)*s34 - pow(M, 2)*(pow(ma, 2) + pow(mb, 2))*z)*Xi) / 2.)) / (s12*s34);
	double C22B = (8 * (8 * pow(mb, 6) + 2 * s14*(s14*s23 - s13*s24) + 2 * pow(ma, 4)*(20 * pow(mb, 2) - s13 + s14 + s23 - s24 - 2 * s34) + 2 * pow(mb, 4)*(-5 * s13 - 5 * s14 + s23 + s24 - 2 * s34) - (s12*s23 + s13*(3 * s14 + s23))*s34 - s14*pow(s34, 2) +
		pow(mb, 2)*(3 * pow(s13, 2) + pow(s14, 2) - 2 * s12*(s23 + s24) + (s12 + s23)*s34 + pow(s34, 2) + s14*(2 * s12 - 3 * s23 + s24 + 3 * s34) + s13*(2 * s12 + 8 * s14 - s23 - s24 + 4 * s34)) +
		pow(ma, 2)*(48 * pow(mb, 4) - 2 * s14*(s14 + 2 * s23 - s24) + (s12 + 3 * s14 - s23 + 2 * s24)*s34 + 3 * pow(s34, 2) + 2 * s13*(s14 + s24 + 2 * s34) - 4 * pow(mb, 2)*(5 * s13 + 4 * s14 + s23 + 2 * s24 + 6 * s34)))) / (s12*s34);
	double C23B = (-2 * (4 * pow(ma, 2)*s34*(-2 * (s12 + s34) + (3 * s12 + s34)*y12*y34) + (s34*(-3 * s12 + s34)*y12 + (4 * pow(ma, 2)*(s12 - s34) + s12*(-s12 + 3 * s34 + (s12 + s34)*pow(y12, 2)))*y34 - s34*(s12 + s34)*y12*pow(y34, 2))*lmbd +
		(4 * (pow(mb, 2)*pow(y12, 2) + pow(ma, 2)*pow(y34, 2)) + s12*(-1 + pow(y12, 2)*pow(y34, 2)) + s34*(-1 + pow(y12, 2)*pow(y34, 2)))*pow(lmbd, 2) + 4 * pow(mb, 2)*(-2 * s12*(s12 + s34) + s12*(s12 + 3 * s34)*y12*y34 + (s12 - s34)*y12*lmbd) +
		pow(M, 2)*(-4 * s12*(s34 - s34*pow(y12, 2)*pow(y34, 2) + pow(mb, 2)*(-2 + y12*y34)) + (4 * pow(mb, 2)*y12 + s12*y34 - y12*(s34 + s12*y12*y34 - s34*pow(y34, 2)))*lmbd - 4 * pow(ma, 2)*(16 * pow(mb, 2) - 2 * s34 + s34*y12*y34 + y34*lmbd)) +
		pow(M, 2)*(2 * s12*(-pow(M, 2) + s12 - s34)*y12*y34 + 2 * s34*(-pow(M, 2) - s12 + s34)*y12*y34 - 2 * pow(M, 2)*(pow(ma, 2) + pow(mb, 2))*z + (1 - y12*y34)*pow(lmbd, 2) + s12*(4 * pow(mb, 2) + y12*lmbd) + s34*(4 * pow(ma, 2) - y34*lmbd))*Xi +
		pow(M, 6)*pow(Xi, 2))) / (s12*s34);
	double C24B = (8 * (4 * pow(mb, 4)*s12 - s12*pow(s13, 2) - 2 * s12*s14*s23 + 3 * s13*s14*s23 + pow(s14, 2)*s23 - s12*pow(s23, 2) - s14*pow(s23, 2) - 3 * pow(s13, 2)*s24 - s13*s14*s24 + s13*s23*s24 - s14*s23*s24 + s13*pow(s24, 2) -
		4 * pow(ma, 4)*(8 * pow(mb, 2) + s12 - s13 + s14 - s23 + s24 - 2 * s34) + (pow(s12, 2) + 2 * s12*s13 - s12*s23 + s14*(s12 + s23) - s13*s24)*s34 + s12*pow(s34, 2) +
		pow(mb, 2)*(3 * pow(s13, 2) - pow(s14, 2) + (s23 - s24)*(s12 + s23 + s24) + s14*(-s12 + 2 * s24 - s34) + (-2 * s12 - s23 + s24)*s34 + s13*(s12 - 2 * s14 - 4 * s23 + 2 * s24 + s34)) +
		pow(ma, 2)*(-32 * pow(mb, 4) + pow(s13, 2) + 2 * s12*s14 - pow(s14, 2) + 4 * s12*s23 + 2 * s14*s23 - pow(s23, 2) + 2 * s14*s24 + 2 * s23*s24 - pow(s24, 2) + 2 * s13*(s12 - 4 * s23 + 2 * s24 - 2 * s34) - 2 * (s12 + 4 * s23 - 2 * s24)*s34 +
			4 * pow(mb, 2)*(4 * s12 + 3 * s13 + s14 + 3 * s23 + s24 + 2 * s34)))) / (s12*s34);
	double C33B = (8 * (8 * pow(ma, 6) - pow(s12, 2)*s14 + 2 * pow(s14, 2)*s23 + 2 * pow(ma, 4)*(24 * pow(mb, 2) - 2 * s12 + s13 - 5 * s14 + s23 - 5 * s24) - 3 * s12*s14*s24 - 2 * s13*s14*s24 - s12*s23*s24 - 2 * pow(mb, 4)*(2 * s12 + s13 - s14 - s23 + s24) -
		s12*s23*s34 + pow(ma, 2)*(40 * pow(mb, 4) + pow(s12, 2) + 3 * s12*s14 + pow(s14, 2) + s12*s23 - 3 * s14*s23 + 4 * s12*s24 + 8 * s14*s24 - s23*s24 + 3 * pow(s24, 2) - 4 * pow(mb, 2)*(6 * s12 + 2 * s13 + 4 * s14 + s23 + 5 * s24) +
			s13*(s14 - s24 - 2 * s34) + (s12 + 2 * (s14 - s23 + s24))*s34) + pow(mb, 2)*(-2 * pow(s14, 2) + 2 * s13*(s12 + s14 + s24) + s14*(3 * s12 - 4 * s23 + 2 * s24) + s12*(3 * s12 - s23 + 4 * s24 + s34)))) / (s12*s34);
	double C34B = (-8 * (16 * pow(ma, 6) - pow(s12, 2)*s13 - s12*s14*s23 + 2 * s13*s14*s23 - s12*pow(s23, 2) + pow(s12, 2)*s24 - 3 * s12*s13*s24 - 2 * pow(s13, 2)*s24 + s12*pow(s24, 2) - 2 * pow(mb, 4)*(2 * s12 + s13 - s14 - s23 + s24) -
		2 * pow(ma, 4)*(2 * s12 + 5 * s13 + 3 * s14 - s23 + 9 * s24) + pow(mb, 2)*(2 * pow(s13, 2) + s14*(s12 - 2 * s23) - s13*(s12 + 2 * (s14 + s23 - 2 * s24)) + s12*(-4 * s12 + 7 * s23 + s24 - 2 * s34)) + 2 * s12*(s12 + s13)*s34 +
		pow(ma, 2)*(-16 * pow(mb, 4) + pow(s13, 2) + s12*s14 + pow(s14, 2) + 3 * s12*s23 - 2 * s14*s23 - pow(s23, 2) + 4 * pow(mb, 2)*(2 * s12 - s13 + s14 + 3 * s23 - 3 * s24) + s12*s24 + 2 * s14*s24 + 4 * s23*s24 + pow(s24, 2) +
			s13*(3 * s12 + 2 * s14 - 4 * s23 + 12 * s24 - 2 * s34) + 2 * (-5 * s12 + s14 - s23 + s24)*s34))) / (s12*s34);
	double C44B = (8 * (8 * pow(ma, 6) + 4 * pow(ma, 4)*(4 * pow(mb, 2) - s13 - s14 - 2 * s23) + pow(ma, 2)*(8 * pow(mb, 4) - pow(s12, 2) + pow(s13, 2) + 3 * s14*s23 + pow(s23, 2) - 4 * pow(mb, 2)*(s13 + s14 + 2 * s23) + s13*(s12 + s14 + 2 * s23 - s24) - s12*s24 +
		s23*s24 + s12*s34) + s12*(s23*(s12 - s13 + s24 - s34) + pow(mb, 2)*(s12 + 3 * s13 - 2 * s23 - s24 + s34)))) / (s12*s34);

	double C2D[] = { CKKA + CKKB, C11A + C11B, C12A + C12B, C13A + C13B, C14A + C14B, C22A + C22B, C23A + C23B, C24A + C24B, C33A + C33B, C34A + C34B, C44A + C44B };

	/// Pent1DR3Coeff(): The coefficients (k.pi) (k.pj) for the \[Epsilon]^\[Mu]\[Nu]\[Rho]\[Sigma] (\*SubsuperscriptBox[\(p\), \(ij\), \(\[Mu]\)]\) (\*SubsuperscriptBox[\(p\), \(kl\), \(\[Rho]\)]\) term; name them CijA
	double CKK1 = (2 * (pow(M, 2) - s12 + s34)*(16 * pow(ma, 2)*pow(mb, 2) + s12*s34*(1 - pow(y12, 2)*pow(y34, 2))) + (pow(M, 2) + s12 - s34)*(4 * pow(mb, 2)*y12 + s34*(-y34 + y12*(2 + pow(y34, 2))))*lmbd +
		(-4 * pow(mb, 2)*pow(y12, 2) + s34*(-2 + y12*y34 - pow(y12, 2)*pow(y34, 2)))*pow(lmbd, 2) + (pow(M, 2)*
		(pow(M, 4)*(2 + y12*y34) - pow(M, 2)*(4 * s12 + 4 * s34 + 2 * s12*y12*y34 - 2 * s34*y12*y34 + y34*lmbd) + (s12 - s34)*(2 * s12 - 2 * s34 + s12*y12*y34 + 3 * s34*y12*y34 + y34*lmbd))*Xi) / 2. - (pow(M, 4)*(pow(M, 2) - s12 + s34)*pow(Xi, 2)) / 2.) / (s12*s34);
	double CKK2 = (2 * (pow(M, 2) - s12 + s34)*(16 * pow(ma, 2)*pow(mb, 2) + s12*s34*(1 - pow(y12, 2)*pow(y34, 2))) - (pow(M, 2) + s12 - s34)*(4 * pow(mb, 2)*y12 + s34*(-y34 + y12*(-2 + pow(y34, 2))))*lmbd +
		(-4 * pow(mb, 2)*pow(y12, 2) + s34*(2 + y12*y34 - pow(y12, 2)*pow(y34, 2)))*pow(lmbd, 2) + (pow(M, 2)*((-2 + y12*y34)*pow(lmbd, 2) + y34*(4 * (pow(M, 2) + s12 - s34)*s34*y12 + (pow(M, 2) - s12 + s34)*lmbd))*Xi) / 2. -
		(pow(M, 4)*(pow(M, 2) - s12 + s34)*pow(Xi, 2)) / 2.) / (s12*s34);
	double CKK3 = (2 * (pow(M, 2) + s12 - s34)*(-16 * pow(ma, 2)*pow(mb, 2) + s12*s34*(-1 + pow(y12, 2)*pow(y34, 2))) - (pow(M, 2) - s12 + s34)*(-(s12*y12) + 4 * pow(ma, 2)*y34 + s12*(-2 + pow(y12, 2))*y34)*lmbd +
		(4 * pow(ma, 2)*pow(y34, 2) + s12*(-2 - y12*y34 + pow(y12, 2)*pow(y34, 2)))*pow(lmbd, 2) + (pow(M, 2)*((2 - y12*y34)*pow(lmbd, 2) + y12*(pow(M, 2)*(-4 * s12*y34 + lmbd) + (s12 - s34)*(4 * s12*y34 + lmbd)))*Xi) / 2. +
		(pow(M, 4)*(pow(M, 2) + s12 - s34)*pow(Xi, 2)) / 2.) / (s12*s34);
	// 2 times defined!!
	double CKK4 = (2 * (pow(M, 2) + s12 - s34)*(-16 * pow(ma, 2)*pow(mb, 2) + s12*s34*(-1 + pow(y12, 2)*pow(y34, 2))) + (pow(M, 2) - s12 + s34)*(-(s12*y12) + 4 * pow(ma, 2)*y34 + s12*(2 + pow(y12, 2))*y34)*lmbd +
		(4 * pow(ma, 2)*pow(y34, 2) + s12*(2 - y12*y34 + pow(y12, 2)*pow(y34, 2)))*pow(lmbd, 2) + (pow(M, 2)*
		(-(pow(M, 4)*(2 + y12*y34)) + (s12 - s34)*(-2 * s12 + 2 * s34 + 3 * s12*y12*y34 + s34*y12*y34 - y12*lmbd) + pow(M, 2)*(4 * s12 + 4 * s34 - 2 * s12*y12*y34 + 2 * s34*y12*y34 - y12*lmbd))*Xi) / 2. + (pow(M, 4)*(pow(M, 2) + s12 - s34)*pow(Xi, 2)) / 2.) / (s12*s34);
	CKK4 = (2 * (pow(M, 2) + s12 - s34)*(-16 * pow(ma, 2)*pow(mb, 2) + s12*s34*(-1 + pow(y12, 2)*pow(y34, 2))) + (pow(M, 2) - s12 + s34)*(-(s12*y12) + 4 * pow(ma, 2)*y34 + s12*(2 + pow(y12, 2))*y34)*lmbd +
		(4 * pow(ma, 2)*pow(y34, 2) + s12*(2 - y12*y34 + pow(y12, 2)*pow(y34, 2)))*pow(lmbd, 2) + (pow(M, 2)*(-4 * s12*(pow(M, 2) - s12 + s34)*y12*y34 - (pow(M, 2) + s12 - s34)*y12*lmbd + (-2 - y12*y34)*pow(lmbd, 2))*Xi) / 2. +
		2 * (pow(M, 2) + s12 - s34)*(4 * pow(ma, 2) + s12*(-1 + pow(y12, 2)))*(4 * pow(mb, 2) + s34*(-1 + pow(y34, 2)))*pow(cos(phi), 2)) / (s12*s34);
	double C111 = (4 * ((s12 + s34)*(-1 + y12*y34) + (-y12 + y34)*lmbd + pow(M, 2)*(1 - y12*y34 + Xi))) / s12;
	double C112 = (-4 * (s12 + s34 - (s12 + s34)*y12*y34 + (3 * y12 + y34)*lmbd + pow(M, 2)*(-1 + y12*y34 - Xi))) / s12;
	double C113 = (4 * s34*((-pow(M, 2) + 5 * s12 + s34)*(-1 + y12*y34) + (-y12 + y34)*lmbd) + 4 * pow(M, 2)*(-pow(M, 2) + s12 + 2 * s34)*Xi) / (s12*s34);
	double C114 = (4 * ((s12 + s34)*(-1 + y12*y34) + (-y12 + y34)*lmbd + pow(M, 2)*(1 - y12*y34 + Xi))) / s12;
	double C122 = (-4 * ((s12 + s34)*(-1 + y12*y34) + (3 * y12 + y34)*lmbd + pow(M, 2)*(1 - y12*y34 + Xi))) / s12;
	double C123 = (16 * s12*s34*(-1 + y12*y34) - 8 * s34*(y12 + y34)*lmbd + 4 * pow(M, 2)*(-pow(M, 2) + s12 + s34)*Xi) / (s12*s34);
	double C124 = (-8 * s34*(2 * s12*(-1 + y12*y34) + (y12 + y34)*lmbd) - 4 * pow(M, 2)*(-pow(M, 2) + s12 + s34)*Xi) / (s12*s34);
	double C133 = (4 * s12*((-pow(M, 2) + s12 + 5 * s34)*(-1 + y12*y34) + (y12 - y34)*lmbd) + 4 * pow(M, 2)*(-pow(M, 2) + 2 * s12 + s34)*Xi) / (s12*s34);
	double C134 = (16 * s12*s34*(-1 + y12*y34) - 8 * s12*(y12 + y34)*lmbd + 4 * pow(M, 2)*(-pow(M, 2) + s12 + s34)*Xi) / (s12*s34);
	double C144 = (4 * (s12 + s34 - (s12 + s34)*y12*y34 + (y12 - y34)*lmbd + pow(M, 2)*(-1 + y12*y34 - Xi))) / s34;
	// same??
	double C222 = (4 * (s12 + s34 - (s12 + s34)*y12*y34 + (-y12 + y34)*lmbd + pow(M, 2)*(-1 + y12*y34 - Xi))) / s12;
	double C223 = (4 * (s12 + s34 - (s12 + s34)*y12*y34 + (-y12 + y34)*lmbd + pow(M, 2)*(-1 + y12*y34 - Xi))) / s12;
	double C224 = (-4 * s34*(-pow(M, 2) + 5 * s12 + s34)*(-1 + y12*y34) + 4 * s34*(-y12 + y34)*lmbd + 4 * pow(M, 2)*(pow(M, 2) - s12 - 2 * s34)*Xi) / (s12*s34);
	double C233 = (4 * ((s12 + s34)*(-1 + y12*y34) + (y12 - y34)*lmbd + pow(M, 2)*(1 - y12*y34 + Xi))) / s34;
	double C234 = (-8 * s12*(2 * s34*(-1 + y12*y34) + (y12 + y34)*lmbd) - 4 * pow(M, 2)*(-pow(M, 2) + s12 + s34)*Xi) / (s12*s34);
	double C244 = (-4 * s12*(-pow(M, 2) + s12 + 5 * s34)*(-1 + y12*y34) + 4 * s12*(y12 - y34)*lmbd + 4 * pow(M, 2)*(pow(M, 2) - 2 * s12 - s34)*Xi) / (s12*s34);
	double C333 = (4 * ((s12 + s34)*(-1 + y12*y34) + (y12 - y34)*lmbd + pow(M, 2)*(1 - y12*y34 + Xi))) / s34;
	double C334 = (-4 * (s12 + s34 - (s12 + s34)*y12*y34 + (y12 + 3 * y34)*lmbd + pow(M, 2)*(-1 + y12*y34 - Xi))) / s34;
	double C344 = (-4 * ((s12 + s34)*(-1 + y12*y34) + (y12 + 3 * y34)*lmbd + pow(M, 2)*(1 - y12*y34 + Xi))) / s34;
	double C444 = (4 * (s12 + s34 - (s12 + s34)*y12*y34 + (y12 - y34)*lmbd + pow(M, 2)*(-1 + y12*y34 - Xi))) / s34;

	double C3D[] = { CKK1, CKK2, CKK3, CKK4, C111, C112, C113, C114, C122, C123, C124, C133, C134, C144, C222, C223, C224, C233, C234, C244, C333, C334, C344, C444 };

	//###########################
	/// Coefficients for 'I' part

	double m = ma;

	/// Pent1IR1Coeff(): The coefficients k.pi for the \[Epsilon]^\[Mu]\[Nu]\[Rho]\[Sigma](\*SubsuperscriptBox[\(p\), \(ij\), \(\[Mu]\)]\)(\*SubsuperscriptBox[\(p\), \(kl\), \(\[Rho]\)]\) term; name them CiA
	C1A = (4 * (384 * pow(m, 8) - 32 * pow(m, 6)*(s12 + 2 * s13 - s23 + 5 * s24 + 4 * s34) + 4 * pow(m, 4)*(-(s12*s23) - 4 * pow(s23, 2) - 5 * s23*s24 + 6 * pow(s24, 2) + s23*s34 + 6 * s24*s34 + 2 * pow(s34, 2) - s14*(-s12 + 6 * s23 + s24 + s34) +
		s13*(2 * s12 + s14 - 3 * s23 + 4 * s24 + 4 * s34)) + s23*(-pow(s24, 3) + s14*s23*(s23 + s24 - s34) + s12*s13*s34 - pow(s13, 2)*s34 - s13*s24*s34 + pow(s24, 2)*s34 + s12*pow(s34, 2) -
			s23*(pow(s24, 2) + (s12 + s13)*s34 + s24*s34)) + pow(m, 2)*(3 * s12*pow(s23, 2) + 4 * s12*s23*s24 + 7 * pow(s23, 2)*s24 + s12*pow(s24, 2) + 10 * s23*pow(s24, 2) - pow(s24, 3) + pow(s13, 2)*(2 * s23 + s24 - s34) - 8 * s12*s23*s34 +
				5 * pow(s23, 2)*s34 + 5 * s12*s24*s34 - 8 * s23*s24*s34 - 2 * pow(s24, 2)*s34 + 2 * s12*pow(s34, 2) + 2 * s23*pow(s34, 2) - s24*pow(s34, 2) - s14*s23*(s12 + 2 * s23 - s24 + s34) +
				s13*(s14*s23 + pow(s23, 2) - s12*s24 - 2 * pow(s24, 2) - 2 * s23*(s12 + s24 - 5 * s34) - 3 * s12*s34 + s24*s34 + pow(s34, 2))))) / (s14*s23);
	C2A = (-4 * (384 * pow(m, 8) + 2 * pow(s14, 2)*pow(s23, 2) + pow(s13, 3)*s24 - s14*s23*pow(s24, 2) - 3 * s12*s14*s23*s34 - s14*pow(s23, 2)*s34 + s12*s23*s24*s34 + pow(s12, 2)*pow(s34, 2) + s12*s23*pow(s34, 2) - s12*s24*pow(s34, 2) -
		32 * pow(m, 6)*(-2 * s12 + 7 * s13 + s14 + 6 * s24 + s34) - pow(s13, 2)*(s14*s23 + (s12 + s24)*s34) +
		4 * pow(m, 4)*(9 * pow(s13, 2) - s12*s24 - 4 * s23*s24 + 3 * pow(s24, 2) - 16 * s12*s34 + 4 * s23*s34 + 7 * s24*s34 - 2 * pow(s34, 2) + s13*(-5 * s12 - 2 * s14 + 26 * s24 + 3 * s34) + s14*(-4 * s12 - 14 * s23 + 6 * s24 + 8 * s34)) +
		s13*(s14*s23*(s23 - s24 - 2 * s34) - s23*s24*(s24 + s34) + s34*(-(s12*s24) + pow(s24, 2) + s12*s34)) +
		pow(m, 2)*(-2 * pow(s13, 3) + 2 * pow(s14, 2)*s23 + 4 * s23*pow(s24, 2) - 2 * pow(s12, 2)*s34 - 5 * s12*s23*s34 + 8 * s12*s24*s34 - s23*s24*s34 - 2 * pow(s24, 2)*s34 + 8 * s12*pow(s34, 2) - 3 * s23*pow(s34, 2) + 2 * s24*pow(s34, 2) +
			s14*(s12*s24 - pow(s24, 2) + s34*(-3 * s12 + s34) + 2 * s23*(7 * s24 + s34)) + pow(s13, 2)*(2 * s14 + s23 + 2 * (s12 - 8 * s24 + 2 * s34)) +
			s13*(s14*(2 * s12 + 16 * s23 - 3 * s24 - 7 * s34) + s23*(-s12 + 3 * s24 + 2 * s34) - 2 * (2 * pow(s24, 2) - 7 * s12*s34 + 5 * s24*s34 + pow(s34, 2)))))) / (s14*s23);
	C3A = (4 * (384 * pow(m, 8) - s12*s14*pow(s23, 2) + 2 * pow(s14, 2)*pow(s23, 2) - 2 * s12*s14*s23*s24 + s14*pow(s23, 2)*s24 - s14*s23*pow(s24, 2) - pow(s13, 2)*(s14*s23 + (-s12 + s23)*s24) -
		32 * pow(m, 6)*(s12 + 6 * s13 + s14 + 7 * s24 - 2 * s34) + pow(s12, 2)*s23*s34 - 3 * s12*s14*s23*s34 + pow(s12, 2)*s24*s34 - s12*pow(s24, 2)*s34 + pow(s12, 2)*pow(s34, 2) -
		s13*(s14*s23*s24 + s12*pow(s24, 2) - pow(s24, 3) + s12*(s12 - s23)*s34 + s12*s24*(s23 + s34)) +
		4 * pow(m, 4)*(-2 * pow(s12, 2) + 3 * pow(s13, 2) + 4 * s12*s23 + 3 * s12*s24 + 9 * pow(s24, 2) + s13*(7 * s12 + 6 * s14 - 4 * s23 + 26 * s24 - s34) - 16 * s12*s34 - 5 * s24*s34 - 2 * s14*(-4 * s12 + 7 * s23 + s24 + 2 * s34)) +
		pow(m, 2)*(-3 * pow(s12, 2)*s23 + 2 * pow(s14, 2)*s23 - 2 * pow(s12, 2)*s24 + 2 * s12*s23*s24 + 4 * s12*pow(s24, 2) + s23*pow(s24, 2) - 2 * pow(s24, 3) - pow(s13, 2)*(2 * s12 + s14 - 4 * s23 + 4 * s24) + 8 * pow(s12, 2)*s34 - 5 * s12*s23*s34 +
			14 * s12*s24*s34 - s23*s24*s34 + 2 * pow(s24, 2)*s34 - 2 * s12*pow(s34, 2) + s14*(pow(s12, 2) - 7 * s12*s24 + 2 * pow(s24, 2) + 2 * s23*(s12 + 8 * s24) - 3 * s12*s34 + 2 * s24*s34) +
			s13*(2 * pow(s12, 2) - s12*s23 - 10 * s12*s24 + 3 * s23*s24 - 16 * pow(s24, 2) + 8 * s12*s34 + s14*(14 * s23 - 3 * s24 + s34))))) / (s14*s23);
	C4A = (-4 * (384 * pow(m, 8) - 32 * pow(m, 6)*(4 * s12 + 5 * s13 - s23 + 2 * s24 + s34) + 4 * pow(m, 4)*(2 * pow(s12, 2) + 6 * pow(s13, 2) + s12*s23 - 4 * pow(s23, 2) - s13*(-6 * s12 + s14 + 5 * s23 - 4 * s24) + 4 * s12*s24 - 3 * s23*s24 - s23*s34 + 2 * s24*s34 +
		s14*(-s12 - 6 * s23 + s24 + s34)) + pow(m, 2)*(-pow(s13, 3) + 2 * pow(s12, 2)*s23 + 5 * s12*pow(s23, 2) + pow(s12, 2)*s24 + 10 * s12*s23*s24 + pow(s23, 2)*s24 - s12*pow(s24, 2) + 2 * s23*pow(s24, 2) + 2 * pow(s12, 2)*s34 -
			8 * s12*s23*s34 + 3 * pow(s23, 2)*s34 - 3 * s12*s24*s34 - 2 * s23*s24*s34 + pow(s13, 2)*(-2 * s12 + 10 * s23 - 2 * s24 + s34) - s14*s23*(s12 + 2 * s23 - s24 + s34) +
			s13*(-pow(s12, 2) + s14*s23 + 7 * pow(s23, 2) + s12*s24 + pow(s24, 2) - 2 * s23*(4 * s12 + s24 - 2 * s34) + 5 * s12*s34 - s24*s34)) +
		s23*(-pow(s13, 3) + pow(s13, 2)*(s12 - s23) + s14*s23*(-s12 + s23) + s13*(s14*s23 - s12*(s23 + s24)) + s12*(-pow(s24, 2) + s12*s34 + s24*s34 - s23*(s24 + s34))))) / (s14*s23);
	/// The coefficients k.pi for the \[Epsilon]^\[Mu]\[Nu]\[Rho]\[Sigma]P^\[Mu]k^\[Rho] term;name them CiB
	C1B = (-4 * (2 * pow(m, 2) - s23)*(64 * pow(m, 6) - pow(s13, 2)*s24 + s23*pow(s24, 2) + s12*s13*s34 + s12*s23*s34 + s13*s23*s34 - s13*s24*s34 + s23*s24*s34 + s12*pow(s34, 2) - s14*s23*(s23 - s24 + s34) -
		16 * pow(m, 4)*(s13 - s23 + 2 * s24 + s34) + 2 * pow(m, 2)*(pow(s13, 2) - 2 * s14*s23 + s12*s24 - 2 * s23*s24 + pow(s24, 2) - 2 * s12*s34 - 4 * s23*s34 + 3 * s24*s34 + s13*(-s12 - 2 * s23 + 4 * s24 + s34)))) / (s14*s23);
	C2B = (-4 * (2 * pow(m, 2) - s23)*(64 * pow(m, 6) + pow(s13, 2)*s14 - pow(s14, 2)*s23 + s12*s14*s34 - s14*s23*s34 + s12*s24*s34 + s14*s24*s34 + s12*pow(s34, 2) - 16 * pow(m, 4)*(2 * s13 - s14 + s24 + s34) +
		s13*(s14*(s23 + s34) - s24*(s24 + s34)) + 2 * pow(m, 2)*(pow(s13, 2) - s12*s24 + pow(s24, 2) - 2 * s12*s34 + s24*s34 - 2 * s14*(s23 + s24 + 2 * s34) + s13*(s12 - 2 * s14 + 4 * s24 + 3 * s34)))) / (s14*s23);
	C3B = (4 * (2 * pow(m, 2) - s23)*(64 * pow(m, 6) - pow(s14, 2)*s23 - 16 * pow(m, 4)*(s12 + s13 - s14 + 2 * s24) - (s12 + s13)*(s13*s24 - s12*s34) +
		2 * pow(m, 2)*(pow(s13, 2) + 3 * s12*s24 + pow(s24, 2) - 2 * s14*(2 * s12 + s23 + s24) + s13*(s12 - 2 * s14 + 4 * s24 - s34) - 2 * s12*s34 + s24*s34) + s14*(s12*s24 + pow(s24, 2) + s23*(-s12 + s24) + s12*(s13 + s34)))) / (s14*s23);
	C4B = (4 * (2 * pow(m, 2) - s23)*(64 * pow(m, 6) + pow(s13, 2)*s23 - s12*s14*s23 - s14*pow(s23, 2) + s12*s23*s24 - 16 * pow(m, 4)*(s12 + 2 * s13 - s23 + s24) + s13*(s12*s23 + s14*s23 - s12*s24 - pow(s24, 2)) + pow(s12, 2)*s34 + s12*s23*s34 +
		s12*s24*s34 + 2 * pow(m, 2)*(pow(s13, 2) - 4 * s12*s23 - 2 * s14*s23 + s12*s24 - 2 * s23*s24 + pow(s24, 2) - 2 * s12*s34 - s24*s34 + s13*(3 * s12 - 2 * s23 + 4 * s24 + s34)))) / (s14*s23);

	double C1I[] = { C1A + C1B, C2A + C2B, C3A + C3B, C4A + C4B };

	/// Pent1IR2Coeff
	CKKA = (2 * (512 * pow(m, 8) - pow(s13, 2)*s14*s23 + 2 * pow(s14, 2)*pow(s23, 2) + pow(s13, 3)*s24 - 2 * s13*s14*s23*s24 - s14*s23*pow(s24, 2) + s13*pow(s24, 3) - 64 * pow(m, 6)*(4 * s13 - s14 + s23 + 4 * s24) -
		s12*(4 * s14*s23 + pow(s13 + s24, 2))*s34 + 2 * pow(s12, 2)*pow(s34, 2) + 16 * pow(m, 4)*(2 * pow(s13, 2) + s12*s23 + s23*s24 + 2 * pow(s24, 2) + s13*(-s14 + s23 + 8 * s24) - 4 * s12*s34 + s23*s34 - s14*(s12 + 4 * s23 + s24 + s34)) +
		2 * pow(m, 2)*(-pow(s13, 3) - 2 * pow(s14, 2)*s23 + 2 * s14*pow(s23, 2) + s12*s14*s24 + s12*s23*s24 + 8 * s14*s23*s24 + s12*pow(s24, 2) + s14*pow(s24, 2) + s23*pow(s24, 2) - pow(s24, 3) +
		(s24*(s14 - 3 * s23 + s24) + 2 * s12*(s14 - s23 + 4 * s24))*s34 + pow(s13, 2)*(s12 + s14 + s23 - 7 * s24 + s34) + s13*(-3 * s12*s23 - 2 * s12*s24 - 4 * s23*s24 - 7 * pow(s24, 2) + (8 * s12 + s23 - 2 * s24)*s34 + s14*(s12 + 8 * s23 + s34))))) /
			(s14*s23);
	C11A = (-8 * (8 * pow(m, 4) + (s13 + s24)*s34 - 2 * pow(m, 2)*(s13 + s14 + 3 * s34))) / s14;
	C12A = (8 * (8 * pow(m, 4)*(s14 - s23) - s34*(-(s14*s23) + s13*(s23 - s24) + s23*s24 + s12*s34) + 2 * pow(m, 2)*(s13*s23 - s14*s24 - (-s12 + s13 + s14 - 3 * s23 + s24)*s34 + pow(s34, 2)))) / (s14*s23);
	C13A = (8 * (s14*s23*(-s23 + s24) + s24*(-pow(s13, 2) + s23*s24) + s12*(s13 + s23)*s34 + 8 * pow(m, 4)*(s12 - s13 + s14 + s23 - s24 + s34) + 2 * pow(m, 2)*(pow(s13, 2) - s23*(s12 + 3 * s24) - s13*(s12 + s14 - 3 * s24 + s34) - s12*(s14 + 2 * s34)))) /
		(s14*s23);
	C14A = (8 * (16 * pow(m, 4) + pow(s13, 2) - 2 * s14*s23 + pow(s24, 2) + 2 * s12*s34 - 2 * pow(m, 2)*(s12 + 3 * s13 - 2 * s14 + 3 * s24 + s34))) / s14;
	C22A = (8 * (8 * pow(m, 4)*s14 + s34*(s14*s23 + s13*s24 - s12*s34) - 2 * pow(m, 2)*((-s12 + s13 + s24 - s34)*s34 + s14*(s23 + s24 + s34)))) / (s14*s23);
	C23A = (-8 * (16 * pow(m, 4)*(-s12 + s13 - s14 + s24 - s34) + (s13 + s24)*(-(s14*s23) + s13*s24 - s12*s34) +
		2 * pow(m, 2)*(-pow(s13, 2) + s12*s24 - pow(s24, 2) + 4 * s12*s34 + s24*s34 + s13*(s12 + s14 - 6 * s24 + s34) + s14*(s12 + 2 * s23 + s24 + s34)))) / (s14*s23);
	C24A = (8 * (pow(s13, 2)*s23 + s13*s14*s23 - s14*pow(s23, 2) - s13*pow(s24, 2) + s12*(s23 + s24)*s34 + 8 * pow(m, 4)*(s12 - s13 + s14 + s23 - s24 + s34) -
		2 * pow(m, 2)*(3 * s13*(s23 - s24) + (s12 + s14 - s24)*s24 + (2 * s12 + s14 + s23 + s24)*s34))) / (s14*s23);
	C33A = (8 * (8 * pow(m, 4)*s14 + s12*(s14*s23 + s13*s24 - s12*s34) - 2 * pow(m, 2)*(s13*(s12 + s14) + s14*(s12 + s23) - s12*(s12 - s24 + s34)))) / (s14*s23);
	C34A = (8 * (8 * pow(m, 4)*(s14 - s23) - s12*(-(s14*s23) + s13*(s23 - s24) + s23*s24 + s12*s34) + 2 * pow(m, 2)*(-(s13*(s12 + s14)) + s23*s24 + s12*(s12 - s14 + 3 * s23 - s24 + s34)))) / (s14*s23);
	C44A = (-8 * (8 * pow(m, 4) + s12*(s13 + s24) - 2 * pow(m, 2)*(3 * s12 + s14 + s24))) / s14;

	CKKB = (-4 * (64 * pow(m, 6)*(-s12 + s13 + s14 + s23 + s24 - s34) + s14*s23*(s13 - s24)*(-s12 + s13 - s24 + s34) -
		8 * pow(m, 4)*(-pow(s12, 2) + pow(s13, 2) + 2 * s12*s23 + 2 * s23*s24 + pow(s24, 2) + 2 * s13*(s14 + s23 + 3 * s24) - 6 * s12*s34 + 2 * s23*s34 - pow(s34, 2) + 2 * s14*(s12 + s24 + s34)) +
		pow(m, 2)*(-4 * pow(s14, 2)*s23 + pow(s13, 2)*(s14 + s23 + 4 * s24) + (s12 + s24 + s34)*(-4 * s12*s34 + s23*(s12 + s24 + s34)) + s14*(-4 * pow(s23, 2) + 4 * s23*(s12 - s24 + s34) + pow(s12 + s24 + s34, 2)) +
			2 * s13*(s23*(s12 + s24 + s34) + s14*(s12 - 2 * s23 + s24 + s34) + 2 * (pow(s24, 2) - s12*s34 + s24*(s12 + s34)))))) / (s14*s23);
	C11B = (-8 * (32 * pow(m, 6) + s23*s34*(-s12 + s13 - s24 + s34) - 8 * pow(m, 4)*(s12 + s24 + 2 * s34) + pow(m, 2)*(2 * pow(s23, 2) + s12*s24 + pow(s24, 2) + 2 * s23*(s12 + 2 * s24 - 2 * s34) + 3 * s12*s34 + 2 * s24*s34 + pow(s34, 2) + s13*(-2 * s23 - s24 + s34)))) /
		(s14*s23);
	C12B = (8 * (-(pow(s13, 2)*s24) + s14*s23*s24 - 16 * pow(m, 4)*(-s12 + s13 + s14 + s23 + s24 - s34) - s14*s23*s34 + s13*(s12 - s24)*s34 + s12*pow(s34, 2) +
		pow(m, 2)*(2 * pow(s13, 2) + 3 * s12*s23 + s23*s24 + s13*(-2 * s12 - s14 + s23 + 10 * s24) - 10 * s12*s34 + 3 * s23*s34 + 2 * s24*s34 - 2 * pow(s34, 2) + s14*(s12 + 3 * s24 + 5 * s34)))) / (s14*s23);
	C13B = (-8 * (s12*s14*s23 + 16 * pow(m, 4)*(s14 + s23) + 2 * s14*s23*s24 - s13*(s14*s23 + s24*(s12 + s24)) + pow(s12, 2)*s34 - 2 * s14*s23*s34 + s12*s24*s34 -
		pow(m, 2)*(2 * pow(s12, 2) + s12*s23 - s23*s24 - 2 * pow(s24, 2) + s13*(s14 + 3 * s23 - 2 * (s12 + s24)) + 2 * s12*s34 + 5 * s23*s34 + 2 * s24*s34 + s14*(s24 + 3 * (s12 + s34))))) / (s14*s23);
	C14B = (8 * (64 * pow(m, 6) + s23*(s13 - s24)*(-s12 + s13 - s24 + s34) - 8 * pow(m, 4)*(s12 + 3 * s13 + 3 * s24 + s34) +
		pow(m, 2)*(pow(s12, 2) + pow(s13, 2) + 2 * s12*s23 - 8 * s14*s23 - 4 * pow(s23, 2) + 2 * s12*s24 - 2 * s23*s24 + pow(s24, 2) - 2 * s12*s34 + 2 * s23*s34 + 2 * s24*s34 + pow(s34, 2) + 2 * s13*(s12 - s23 + 3 * s24 + s34)))) / (s14*s23);
	C22B = (8 * (32 * pow(m, 6) - 8 * pow(m, 4)*(2 * s13 - 2 * s14 + s24 + s34) + s14*(pow(s13, 2) - s14*s23 + 2 * s13*s34 + pow(s34, 2)) +
		pow(m, 2)*(pow(s13, 2) - 2 * pow(s14, 2) + s34*(-s12 + s24 + s34) + s13*(s12 - 10 * s14 + 3 * s24 + 2 * s34) - 2 * s14*(2 * s23 + 3 * s34)))) / (s14*s23);
	C23B = (-8 * (64 * pow(m, 6) - 8 * pow(m, 4)*(3 * s12 + s13 - 4 * s14 + s24 + 3 * s34) + 2 * s14*(-(s14*s23) + (s12 + s24)*(s13 + s34)) +
		pow(m, 2)*(pow(s12, 2) + pow(s13, 2) + 4 * pow(s14, 2) + 2 * s12*s24 + pow(s24, 2) - 2 * s13*(-s12 + 3 * s14 + s24 - s34) + 6 * s12*s34 + 2 * s24*s34 + pow(s34, 2) - 2 * s14*(3 * s24 + 5 * (s12 + s34))))) / (s14*s23);
	C24B = (-8 * (-2 * s12*s14*s23 + 16 * pow(m, 4)*(s14 + s23) - pow(s13, 2)*s24 - s14*s23*s24 + s14*s23*s34 + s12*pow(s34, 2) + s13*(2 * s14*s23 + (s12 - s24)*s34) +
		pow(m, 2)*(2 * pow(s13, 2) - 5 * s12*s23 - 3 * s23*s24 + s13*(-2 * s12 - s14 + s23 + 2 * s24) - 2 * s12*s34 - s23*s34 + 2 * s24*s34 - 2 * pow(s34, 2) - s14*(s24 + 3 * (s12 + s34))))) / (s14*s23);
	C33B = (8 * (32 * pow(m, 6) - 8 * pow(m, 4)*(s12 + s13 - 2 * s14 + 2 * s24) + s14*(-(s14*s23) + pow(s12 + s24, 2)) +
		pow(m, 2)*(pow(s12, 2) + s12*s13 - 2 * pow(s14, 2) + 2 * s12*s24 + 3 * s13*s24 + pow(s24, 2) - 2 * s14*(3 * s12 + 2 * s23 + 5 * s24) - s12*s34 + s24*s34))) / (s14*s23);
	C34B = (8 * (s13*(s14*s23 - s24*(s12 + s24)) - 16 * pow(m, 4)*(-s12 + s13 + s14 + s23 + s24 - s34) + s12*(-(s14*s23) + (s12 + s24)*s34) +
		pow(m, 2)*(-2 * pow(s12, 2) + 3 * s12*s23 + s23*s24 + 2 * pow(s24, 2) + s13*(2 * s12 + 3 * s14 + s23 + 10 * s24) - 10 * s12*s34 + 3 * s23*s34 - 2 * s24*s34 + s14*(5 * s12 - s24 + s34)))) / (s14*s23);
	C44B = (-8 * (32 * pow(m, 6) + s12*s23*(s12 - s13 + s24 - s34) - 8 * pow(m, 4)*(2 * s12 + s13 + s34) + pow(m, 2)*(pow(s13, 2) + 2 * pow(s23, 2) - 2 * s23*(2 * s12 + s24 - s34) + s13*(2 * s12 + 4 * s23 - s24 + s34) + s12*(s12 + s24 + 3 * s34)))) / (s14*s23);

	double C2I[] = { CKKA + CKKB, C11A + C11B, C12A + C12B, C13A + C13B, C14A + C14B, C22A + C22B, C23A + C23B, C24A + C24B, C33A + C33B, C34A + C34B, C44A + C44B };

	/// Pent1IR3Coeff
	CKK1 = (4 * (-(s14*pow(s23, 2)) + pow(s13, 2)*s24 + s14*s23*s24 + s23*pow(s24, 2) + 8 * pow(m, 4)*(-s12 + s13 + 2 * s23 + s24 - s34) + s12*s23*s34 + s14*s23*s34 + s23*s24*s34 - s12*pow(s34, 2) + s13*(-2 * s14*s23 + (-s12 + s23 + s24)*s34) -
		2 * pow(m, 2)*(pow(s13, 2) - s13*(s12 + s23 - 3 * s24) + (-3 * s12 + s24 - s34)*s34 + s23*(3 * s12 + 5 * s24 + s34)))) / (s14*s23);
	CKK2 = (-4 * (128 * pow(m, 6) + pow(s13, 2)*s14 - pow(s14, 2)*s23 - 2 * s14*s23*s24 + s12*s14*s34 + s14*s23*s34 - s12*s24*s34 + s14*s24*s34 - s12*pow(s34, 2) - 8 * pow(m, 4)*(3 * s12 + 5 * s13 - 2 * s14 + s24 + 7 * s34) +
		s13*(s14*(s23 + s34) + s24*(s24 + s34)) + 2 * pow(m, 2)*(2 * pow(s13, 2) + s12*s24 - pow(s24, 2) - s13*(-2 * s12 + 3 * s14 + s24 - 3 * s34) + 5 * s12*s34 + 2 * s24*s34 + 3 * pow(s34, 2) - s14*(s12 + 4 * s23 + s24 + 3 * s34)))) / (s14*s23);
	CKK3 = (4 * (128 * pow(m, 6) + s12*s14*s23 - pow(s14, 2)*s23 + pow(s13, 2)*s24 + s12*s14*s24 + s14*s23*s24 + s14*pow(s24, 2) + s13*(s14*(s12 - 2 * s23) + s12*(s24 - s34)) - pow(s12, 2)*s34 + s12*s14*s34 -
		8 * pow(m, 4)*(7 * s12 + s13 - 2 * s14 + 5 * s24 + 3 * s34) - 2 * pow(m, 2)*(-3 * pow(s12, 2) + pow(s13, 2) - 3 * s12*s24 - 2 * pow(s24, 2) + s13*(-2 * s12 + s14 + s24 - s34) - 5 * s12*s34 - 2 * s24*s34 + s14*(3 * s12 + 4 * s23 + 3 * s24 + s34)))) / (s14*s23);
	CKK4 = (-4 * (pow(s13, 2)*s23 + s12*s14*s23 - s14*pow(s23, 2) + s12*s23*s24 - 2 * s14*s23*s24 + s13*(s12*s23 + s14*s23 + s12*s24 + pow(s24, 2)) + 8 * pow(m, 4)*(-s12 + s13 + 2 * s23 + s24 - s34) - pow(s12, 2)*s34 + s12*s23*s34 - s12*s24*s34 -
		2 * pow(m, 2)*(-pow(s12, 2) + pow(s24, 2) + s13*(s12 + 5 * s23 + 3 * s24) - 3 * s12*s34 - s24*s34 + s23*(s12 - s24 + 3 * s34)))) / (s14*s23);
	C111 = (-16 * (-2 * pow(m, 2) + s34)) / s14;
	C112 = (32 * pow(m, 2) - 16 * s34) / s14;
	C113 = (16 * (s14*s23 - s13*s24 + 2 * pow(m, 2)*(-s12 + s13 + s23 + s24 - s34) + s12*s34 - s23*s34)) / (s14*s23);
	C114 = (16 * (2 * pow(m, 2) + s13 - s24 - s34)) / s14;
	C122 = (16 * (-2 * pow(m, 2) + s34)) / s23;
	C123 = (16 * (s14*s23 + s13*(s14 - s24) - s14*s24 + 2 * pow(m, 2)*(-s12 + s13 + s24 - s34) + s12*s34)) / (s14*s23);
	C124 = (-16 * (s14*s23 + s23*s24 - s13*(s23 + s24) + 2 * pow(m, 2)*(-s12 + s13 + s24 - s34) + s12*s34)) / (s14*s23);
	C133 = (16 * (s14*(-s12 + s23) - s13*s24 + 2 * pow(m, 2)*(-s12 + s13 + s14 + s24 - s34) + s12*s34)) / (s14*s23);
	C134 = (16 * (s14*s23 + s13*(s23 - s24) - s23*s24 + 2 * pow(m, 2)*(-s12 + s13 + s24 - s34) + s12*s34)) / (s14*s23);
	C144 = (-16 * (2 * pow(m, 2) - s12 - s13 + s24)) / s14;
	C222 = (16 * (-2 * pow(m, 2) + s34)) / s23;
	C223 = (-16 * (2 * pow(m, 2) - s13 + s24 - s34)) / s23;
	C224 = (-16 * (-(s13*s24) + s14*(s23 - s34) + 2 * pow(m, 2)*(-s12 + s13 + s14 + s24 - s34) + s12*s34)) / (s14*s23);
	C233 = (16 * (2 * pow(m, 2) - s12 + s13 - s24)) / s23;
	C234 = (-16 * (s14*s23 + s14*s24 - s13*(s14 + s24) + 2 * pow(m, 2)*(-s12 + s13 + s24 - s34) + s12*s34)) / (s14*s23);
	C244 = (-16 * (-(s12*s23) + s14*s23 - s13*s24 + 2 * pow(m, 2)*(-s12 + s13 + s23 + s24 - s34) + s12*s34)) / (s14*s23);
	C333 = (-16 * (-2 * pow(m, 2) + s12)) / s23;
	C334 = (32 * pow(m, 2) - 16 * s12) / s23;
	// stejne?
	C344 = (16 * (-2 * pow(m, 2) + s12)) / s14;
	C444 = (16 * (-2 * pow(m, 2) + s12)) / s14;

	double C3I[] = { CKK1, CKK2, CKK3, CKK4, C111, C112, C113, C114, C122, C123, C124, C133, C134, C144, C222, C223, C224, C233, C234, C244, C333, C334, C344, C444 };

	if (c == 'D') { return array_product(C1D, I1, (sizeof(I1) / sizeof(*I1))) + array_product(C2D, I2, (sizeof(I2) / sizeof(*I2))) + array_product(C3D, I3, (sizeof(I3) / sizeof(*I3))); } // 4, 11, 24
	else if (c == 'I') { return array_product(C1I, I1, (sizeof(I1) / sizeof(*I1))) + array_product(C2I, I2, (sizeof(I2) / sizeof(*I2))) + array_product(C3I, I3, (sizeof(I3) / sizeof(*I3))); }
	else { std::cout << "Warning: PentDir::something is wrong!\n"; return 0; }
}

//######################
/// Pentagon Tensor Int1

double FDoubleDalitz::Pent1IDir(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34)
{
	return PentDir('I', M, ma, mb, s12, s34, y12, y34, phi, LAMBDA, M12, M34);
}

//######################
/// Pentagon Tensor Int2

double FDoubleDalitz::Pent2IDir(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34)
{
	double m = ma;
	SetBasicVariables(M, ma, mb, s12, s34, y12, y34, phi);
	SetSVariables(M, ma, mb, s12, s34, y12, y34);

	// set the scale for IR divergence
	setlambda(LAMBDA*LAMBDA);

	/// define the integrals
	// this is the E-family: single
	double EFmly = RE(E0i(ee0, pow(m, 2), s12, s123, pow(m, 2), s134, pow(m, 2), pow(m, 2), pow(M, 2), s13, s34, pow(m, 2), 0, pow(M12, 2), pow(m, 2), pow(M34, 2)));
	// this is the D-family: DFmlyN
	std::vector<double> DFmly1 = D_get(pow(m, 2), s12, pow(M, 2), s134, pow(m, 2), s34, pow(m, 2), 0, pow(M12, 2), pow(M34, 2));
	std::vector<double> DFmly2 = D_get(pow(m, 2), s134, pow(m, 2), s123, s13, pow(M, 2), pow(m, 2), pow(M34, 2), pow(m, 2), pow(M12, 2));
	std::vector<double> DFmly3 = D_get(pow(m, 2), s34, pow(M, 2), s123, pow(m, 2), s12, pow(m, 2), 0, pow(M34, 2), pow(M12, 2));
	std::vector<double> DFmly4 = D_get(pow(m, 2), s34, s134, s13, pow(m, 2), pow(m, 2), pow(m, 2), 0, pow(M34, 2), pow(m, 2));
	std::vector<double> DFmly5 = D_get(pow(m, 2), s12, s123, s13, pow(m, 2), pow(m, 2), pow(m, 2), 0, pow(M12, 2), pow(m, 2));
	std::vector<double> DFmly6 = D_get(pow(m, 2), s123, pow(m, 2), s134, s13, pow(M, 2), pow(m, 2), pow(M12, 2), pow(m, 2), pow(M34, 2));
	// this is the C-family
	std::vector<double> CFmly1 = C_get(s12, pow(M, 2), s34, 0, pow(M12, 2), pow(M34, 2));
	std::vector<double> CFmly2 = C_get(pow(m, 2), s34, s134, pow(m, 2), 0, pow(M34, 2));
	std::vector<double> CFmly3 = C_get(pow(m, 2), s134, pow(M, 2), pow(M12, 2), pow(m, 2), pow(M34, 2));
	std::vector<double> CFmly4 = C_get(pow(m, 2), s13, s123, pow(M12, 2), pow(m, 2), pow(m, 2));
	std::vector<double> CFmly5 = C_get(pow(m, 2), s12, pow(m, 2), pow(m, 2), 0, pow(M12, 2));
	std::vector<double> CFmly6 = C_get(pow(m, 2), s12, s123, pow(m, 2), 0, pow(M12, 2));
	std::vector<double> CFmly7 = C_get(pow(m, 2), s13, pow(m, 2), 0, pow(m, 2), pow(m, 2));
	std::vector<double> CFmly8 = C_get(pow(m, 2), s123, pow(M, 2), pow(M34, 2), pow(m, 2), pow(M12, 2));
	std::vector<double> CFmly9 = C_get(pow(m, 2), s134, s13, pow(m, 2), pow(M34, 2), pow(m, 2));
	std::vector<double> CFmly10 = C_get(pow(m, 2), s34, pow(m, 2), pow(m, 2), 0, pow(M34, 2));
	// this is the B-family; only scalar -> make it a vector anyway
	double BFmly[] = {
		0., // start indexing from 1
		RE(B0i(bb0, pow(M, 2), pow(M12, 2), pow(M34, 2))),
		0., 0., 0.,
		RE(B0i(bb0, s13, pow(m, 2), pow(m, 2))),
		RE(B0i(bb0, s134, pow(m, 2), pow(M34, 2))),
		RE(B0i(bb0, s123, pow(m, 2), pow(M12, 2)))
	};

	/// Pent2DR1()
	double INT1 = (ALPHA * (DFmly2[1] - DFmly3[1])) / (4.*PI);
	double INT2 = -(ALPHA * (EFmly*(pow(M12, 2) - s12) - DFmly3[1] + DFmly4[1])) / (4.*PI);
	double INT3 = (ALPHA * (DFmly1[1] - DFmly2[1])) / (4.*PI);
	double INT4 = -(ALPHA * (EFmly*(-pow(M34, 2) + s34) + DFmly1[1] - DFmly5[1])) / (4.*PI);

	double I1[] = { INT1, INT2, INT3, INT4 };

	/// Pent2DR2()
	double INTKK = (ALPHA * DFmly2[1]) / (2.*PI);
	double INT11 = (ALPHA * ((2 * pow(m, 2) - s13)*DFmly2[1] + (-2 * pow(m, 2) + s14)*DFmly2[2] - s13*DFmly2[3] + (2 * pow(m, 2) - s12 - s13)*DFmly2[4] + (-2 * pow(m, 2) + s13)*DFmly3[1] + (-2 * pow(m, 2) + s13)*DFmly3[2] + (2 * pow(m, 2) - s14)*DFmly3[3] -
		(2 * pow(m, 2) - s12 - s13)*DFmly3[4])) / (8.*PI);
	double INT12 = -(ALPHA * (-CFmly8[1] + CFmly9[1] + (pow(M12, 2) - s12)*DFmly2[1] + (2 * pow(m, 2) - s23)*DFmly3[1] + (2 * pow(m, 2) - s23)*DFmly3[2] + (-2 * pow(m, 2) + s24)*DFmly3[3] + (2 * pow(m, 2) - s12 - s23)*DFmly3[4])) / (8.*PI);
	double INT13 = -(ALPHA * (CFmly1[1] - CFmly8[1] + 2 * pow(m, 2)*DFmly2[1] + (2 * pow(m, 2) - s34)*DFmly2[2] + s13*DFmly2[3] - (2 * pow(m, 2) - s13 - s23)*DFmly2[4])) / (8.*PI);
	double INT14 = (ALPHA * (CFmly1[1] - CFmly3[1] + CFmly4[1] - CFmly6[1] + (pow(M34, 2) - s34)*DFmly2[1] + (-pow(M34, 2) + s34)*DFmly3[1])) / (8.*PI);
	double INT22 = (ALPHA * (EFmly*pow(pow(M12, 2) - s12, 2) + (2 * pow(m, 2) - pow(M12, 2) + s12 - s23)*DFmly3[1] + (2 * pow(m, 2) - s23)*DFmly3[2] + (-2 * pow(m, 2) + s24)*DFmly3[3] + (2 * pow(m, 2) - s12 - s23)*DFmly3[4] +
		(-2 * pow(m, 2) + pow(M12, 2) - s12 + s23)*DFmly4[1] + (-2 * pow(m, 2) + s23)*DFmly4[2] + (2 * pow(m, 2) - s24)*DFmly4[3] - (4 * pow(m, 2) - s12 - s23)*DFmly4[4])) / (8.*PI);
	double INT23 = -(ALPHA * (-CFmly1[1] + CFmly2[1] + CFmly8[1] - CFmly9[1] + (pow(M12, 2) - s12)*DFmly1[1] + (-pow(M12, 2) + s12)*DFmly2[1])) / (8.*PI);
	double INT24 = -(ALPHA * (EFmly*(pow(M12, 2) - s12)*(pow(M34, 2) - s34) + CFmly1[1] - CFmly2[1] - CFmly6[1] + CFmly7[1] + (-pow(M12, 2) + s12)*DFmly1[1] + (-pow(M34, 2) + s34)*DFmly3[1] + (pow(M34, 2) - s34)*DFmly4[1] + (pow(M12, 2) - s12)*DFmly5[1])) / (8.*PI);
	double INT33 = (ALPHA * ((-2 * pow(m, 2) + s13)*DFmly1[1] + (-2 * pow(m, 2) + s13)*DFmly1[2] + (2 * pow(m, 2) - s23)*DFmly1[3] - (2 * pow(m, 2) - s13 - s34)*DFmly1[4] + 2 * pow(m, 2)*DFmly2[1] + (2 * pow(m, 2) - s34)*DFmly2[2] + s13*DFmly2[3] -
		(2 * pow(m, 2) - s13 - s23)*DFmly2[4])) / (8.*PI);
	double INT34 = -(ALPHA * (-CFmly3[1] + CFmly4[1] + (2 * pow(m, 2) - s14)*DFmly1[1] + (2 * pow(m, 2) - s14)*DFmly1[2] + (-2 * pow(m, 2) + s24)*DFmly1[3] + (2 * pow(m, 2) - s14 - s34)*DFmly1[4] + (pow(M34, 2) - s34)*DFmly2[1])) / (8.*PI);
	double INT44 = (ALPHA * (EFmly*pow(pow(M34, 2) - s34, 2) + (2 * pow(m, 2) - pow(M34, 2) - s14 + s34)*DFmly1[1] + (2 * pow(m, 2) - s14)*DFmly1[2] + (-2 * pow(m, 2) + s24)*DFmly1[3] + (2 * pow(m, 2) - s14 - s34)*DFmly1[4] -
		(2 * pow(m, 2) - pow(M34, 2) - s14 + s34)*DFmly5[1] + (-2 * pow(m, 2) + s14)*DFmly5[2] + (2 * pow(m, 2) - s24)*DFmly5[3] - (4 * pow(m, 2) - s14 - s34)*DFmly5[4])) / (8.*PI);

	double I2[] = { INTKK, INT11, INT12, INT13, INT14, INT22, INT23, INT24, INT33, INT34, INT44 };

	/// Pent2DR3()
	double INTKK1 = (ALPHA * (2 * pow(m, 2)*DFmly2[1] + (2 * pow(m, 2) - s12)*DFmly6[2] + s13*DFmly6[3] + (-2 * pow(m, 2) + s13 + s14)*DFmly6[4])) / (4.*PI);
	double INTKK2 = (ALPHA * (CFmly8[1] - CFmly9[1] + (-pow(M12, 2) + s12)*DFmly2[1])) / (4.*PI);
	double INTKK3 = (ALPHA * ((-2 * pow(m, 2) + s13)*DFmly2[1] + (2 * pow(m, 2) - s23)*DFmly6[2] + s13*DFmly6[3] + (-2 * pow(m, 2) + s13 + s34)*DFmly6[4])) / (4.*PI);
	double INTKK4 = (ALPHA * (-CFmly3[1] + CFmly4[1] + (pow(M34, 2) - s34)*DFmly2[1])) / (4.*PI);
	double INT111 = (ALPHA * (pow(-2 * pow(m, 2) + s13, 2)*DFmly2[1] - 2 * (-2 * pow(m, 2) + s13)*(-2 * pow(m, 2) + s14)*DFmly2[2] + 2 * s13*(-2 * pow(m, 2) + s13)*DFmly2[3] + 2 * (2 * pow(m, 2) - s13)*(2 * pow(m, 2) - s12 - s13)*DFmly2[4] + 4 * pow(m, 2)*DFmly2[5] +
		pow(-2 * pow(m, 2) + s14, 2)*DFmly2[6] + 2 * s13*(2 * pow(m, 2) - s14)*DFmly2[7] - 2 * (2 * pow(m, 2) - s12 - s13)*(2 * pow(m, 2) - s14)*DFmly2[8] + pow(s13, 2)*DFmly2[9] + 2 * s13*(-2 * pow(m, 2) + s12 + s13)*DFmly2[10] +
		pow(-2 * pow(m, 2) + s12 + s13, 2)*DFmly2[11] - pow(-2 * pow(m, 2) + s13, 2)*DFmly3[1] - 2 * pow(-2 * pow(m, 2) + s13, 2)*DFmly3[2] + 2 * (-2 * pow(m, 2) + s13)*(-2 * pow(m, 2) + s14)*DFmly3[3] -
		2 * (2 * pow(m, 2) - s13)*(2 * pow(m, 2) - s12 - s13)*DFmly3[4] - 4 * pow(m, 2)*DFmly3[5] - pow(-2 * pow(m, 2) + s13, 2)*DFmly3[6] + 2 * (-2 * pow(m, 2) + s13)*(-2 * pow(m, 2) + s14)*DFmly3[7] -
		2 * (2 * pow(m, 2) - s13)*(2 * pow(m, 2) - s12 - s13)*DFmly3[8] - pow(-2 * pow(m, 2) + s14, 2)*DFmly3[9] + 2 * (2 * pow(m, 2) - s12 - s13)*(2 * pow(m, 2) - s14)*DFmly3[10] - pow(-2 * pow(m, 2) + s12 + s13, 2)*DFmly3[11])) / (16.*PI);
	double INT112 = -(ALPHA * ((-4 * pow(m, 2) + s12 + s23)*CFmly10[1] + (-2 * pow(m, 2) + s23)*CFmly10[2] + (2 * pow(m, 2) - s24)*CFmly10[3] + (2 * pow(m, 2) - s12 + s13 + s14 - s23 - s24)*CFmly8[1] + (s14 - s24)*CFmly8[2] + (s13 + s14 - s23 - s24)*CFmly8[3] +
		(2 * pow(m, 2) - s13)*CFmly9[1] + (-2 * pow(m, 2) + s14)*CFmly9[2] - s13*CFmly9[3] + (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s13)*DFmly2[1] - (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s14)*DFmly2[2] - (pow(M12, 2) - s12)*s13*DFmly2[3] -
		(pow(M12, 2) - s12)*(-2 * pow(m, 2) + s12 + s13)*DFmly2[4] - (12 * pow(m, 4) + pow(s12, 2) + s12*s13 + 2 * s12*s23 + pow(s23, 2) - pow(M12, 2)*(s12 + s23) + pow(m, 2)*(4 * pow(M12, 2) - 2 * (4 * s12 + s13 + 3 * s23)))*DFmly3[1] -
		(16 * pow(m, 4) + s12*s13 + 2 * pow(m, 2)*(pow(M12, 2) - 3 * s12 - s13 - 5 * s23) - pow(M12, 2)*s23 + 2 * s12*s23 + 2 * pow(s23, 2))*DFmly3[2] +
		(16 * pow(m, 4) + s12*s14 + 2 * pow(m, 2)*(pow(M12, 2) - 3 * s12 - s14 - 2 * s23 - 3 * s24) - pow(M12, 2)*s24 + 2 * s12*s24 + 2 * s23*s24)*DFmly3[3] -
		(16 * pow(m, 4) + 3 * pow(s12, 2) + s12*s13 + 2 * pow(m, 2)*(pow(M12, 2) - 7 * s12 - s13 - 5 * s23) + 4 * s12*s23 + 2 * pow(s23, 2) - pow(M12, 2)*(s12 + s23))*DFmly3[4] - 4 * pow(m, 2)*DFmly3[5] - pow(-2 * pow(m, 2) + s23, 2)*DFmly3[6] +
		2 * (-2 * pow(m, 2) + s23)*(-2 * pow(m, 2) + s24)*DFmly3[7] - 2 * (2 * pow(m, 2) - s23)*(2 * pow(m, 2) - s12 - s23)*DFmly3[8] - pow(-2 * pow(m, 2) + s24, 2)*DFmly3[9] + 2 * (2 * pow(m, 2) - s12 - s23)*(2 * pow(m, 2) - s24)*DFmly3[10] -
		pow(-2 * pow(m, 2) + s12 + s23, 2)*DFmly3[11])) / (16.*PI);
	double INT113 = (ALPHA * (s12*CFmly1[2] + (4 * pow(m, 2) - s13 - s14)*CFmly1[3] + (-2 * pow(m, 2) + s23)*CFmly3[1] + (-2 * pow(m, 2) + s23)*CFmly3[2] - (4 * pow(m, 2) - s13 - s23 - s34)*CFmly3[3] + (2 * pow(m, 2) - s14 + s34)*CFmly8[1] - (s14 - s34)*CFmly8[2] -
		(s12 + s14 - s23 - s34)*CFmly8[3] - (4 * pow(m, 4) - 2 * pow(m, 2)*s13 + pow(s13, 2))*DFmly2[1] + (-2 * pow(m, 2)*(2 * s13 + s14 - s34) + s13*(s14 + s34))*DFmly2[2] - 2 * pow(s13, 2)*DFmly2[3] +
		(2 * pow(m, 2)*(s12 + 2 * s13 - s23) - s13*(s12 + 2 * s13 + s23))*DFmly2[4] - 4 * pow(m, 2)*DFmly2[5] - pow(-2 * pow(m, 2) + s34, 2)*DFmly2[6] - 2 * s13*(2 * pow(m, 2) - s34)*DFmly2[7] + 2 * (2 * pow(m, 2) - s13 - s23)*(2 * pow(m, 2) - s34)*DFmly2[8] -
		pow(s13, 2)*DFmly2[9] - 2 * s13*(-2 * pow(m, 2) + s13 + s23)*DFmly2[10] - pow(-2 * pow(m, 2) + s13 + s23, 2)*DFmly2[11])) / (16.*PI);
	double INT114 = (ALPHA * (-(s12*CFmly1[2]) - (4 * pow(m, 2) - s13 - s14)*CFmly1[3] - s12*CFmly3[1] + (2 * pow(m, 2) - s12)*CFmly3[2] + (4 * pow(m, 2) - s12 - s13 - s14)*CFmly3[3] + s12*CFmly4[1] + (-2 * pow(m, 2) + s12)*CFmly4[2] - (2 * pow(m, 2) - s12 - s13)*CFmly4[3] +
		(-2 * pow(m, 2) + s13)*CFmly6[1] + (-2 * pow(m, 2) + s13)*CFmly6[2] - (2 * pow(m, 2) - s12 - s13)*CFmly6[3] + (-2 * pow(m, 2) + s13)*(-pow(M34, 2) + s34)*DFmly2[1] - (-2 * pow(m, 2) + s14)*(-pow(M34, 2) + s34)*DFmly2[2] -
		s13*(pow(M34, 2) - s34)*DFmly2[3] + (2 * pow(m, 2) - s12 - s13)*(pow(M34, 2) - s34)*DFmly2[4] - (-2 * pow(m, 2) + s13)*(-pow(M34, 2) + s34)*DFmly3[1] - (-2 * pow(m, 2) + s13)*(-pow(M34, 2) + s34)*DFmly3[2] +
		(-2 * pow(m, 2) + s14)*(-pow(M34, 2) + s34)*DFmly3[3] - (2 * pow(m, 2) - s12 - s13)*(pow(M34, 2) - s34)*DFmly3[4])) / (16.*PI);
	double INT122 = (ALPHA * ((4 * pow(m, 2) - pow(M12, 2) + s12 - s23 - s24)*CFmly8[1] + (2 * pow(m, 2) - s24)*CFmly8[2] + (4 * pow(m, 2) - s12 - s23 - s24)*CFmly8[3] - (2 * pow(m, 2) - pow(M12, 2) + s12 - s23)*CFmly9[1] + (2 * pow(m, 2) - s24)*CFmly9[2] -
		(4 * pow(m, 2) - s12 - s23)*CFmly9[3] + pow(pow(M12, 2) - s12, 2)*DFmly2[1] - pow(-2 * pow(m, 2) + s23, 2)*DFmly3[1] - 2 * pow(-2 * pow(m, 2) + s23, 2)*DFmly3[2] + 2 * (-2 * pow(m, 2) + s23)*(-2 * pow(m, 2) + s24)*DFmly3[3] -
		2 * (2 * pow(m, 2) - s23)*(2 * pow(m, 2) - s12 - s23)*DFmly3[4] - 4 * pow(m, 2)*DFmly3[5] - pow(-2 * pow(m, 2) + s23, 2)*DFmly3[6] + 2 * (-2 * pow(m, 2) + s23)*(-2 * pow(m, 2) + s24)*DFmly3[7] -
		2 * (2 * pow(m, 2) - s23)*(2 * pow(m, 2) - s12 - s23)*DFmly3[8] - pow(-2 * pow(m, 2) + s24, 2)*DFmly3[9] + 2 * (2 * pow(m, 2) - s12 - s23)*(2 * pow(m, 2) - s24)*DFmly3[10] - pow(-2 * pow(m, 2) + s12 + s23, 2)*DFmly3[11])) / (16.*PI);
	double INT123 = (ALPHA * (s12*CFmly1[2] + (4 * pow(m, 2) - s23 - s24)*CFmly1[3] + (4 * pow(m, 2) - s23 - s24 - s34)*CFmly8[1] + (4 * pow(m, 2) - s24 - s34)*CFmly8[2] + (8 * pow(m, 2) - s12 - s13 - 2 * s23 - s24 - s34)*CFmly8[3] + 2 * pow(m, 2)*CFmly9[1] +
		(2 * pow(m, 2) - s34)*CFmly9[2] + s13*CFmly9[3] + 2 * pow(m, 2)*(pow(M12, 2) - s12)*DFmly2[1] + (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s34)*DFmly2[2] + (pow(M12, 2) - s12)*s13*DFmly2[3] -
		(pow(M12, 2) - s12)*(2 * pow(m, 2) - s13 - s23)*DFmly2[4])) / (16.*PI);
	double INT124 = -(ALPHA * (BFmly[1] + BFmly[5] - BFmly[6] - BFmly[7] + s12*CFmly1[2] + (4 * pow(m, 2) - s23 - s24)*CFmly1[3] + (-pow(M12, 2) + s12)*CFmly3[1] + (pow(M12, 2) - s12)*CFmly4[1] + (2 * pow(m, 2) - s23)*CFmly6[1] + (2 * pow(m, 2) - s23)*CFmly6[2] +
		(2 * pow(m, 2) - s12 - s23)*CFmly6[3] + (-pow(M34, 2) + s34)*CFmly8[1] + (pow(M34, 2) - s34)*CFmly9[1] + (pow(M12, 2) - s12)*(pow(M34, 2) - s34)*DFmly2[1] + (-2 * pow(m, 2) + s23)*(-pow(M34, 2) + s34)*DFmly3[1] +
		(-2 * pow(m, 2) + s23)*(-pow(M34, 2) + s34)*DFmly3[2] - (-2 * pow(m, 2) + s24)*(-pow(M34, 2) + s34)*DFmly3[3] + (2 * pow(m, 2) - s12 - s23)*(pow(M34, 2) - s34)*DFmly3[4])) / (16.*PI);
	double INT133 = -(ALPHA * ((4 * pow(m, 2) - s13 - s23)*CFmly1[2] + s34*CFmly1[3] + s34*CFmly8[1] + (-2 * pow(m, 2) + s34)*CFmly8[2] - (4 * pow(m, 2) - s13 - s23 - s34)*CFmly8[3] - 4 * pow(m, 4)*DFmly2[1] + 4 * pow(m, 2)*(-2 * pow(m, 2) + s34)*DFmly2[2] -
		4 * pow(m, 2)*s13*DFmly2[3] + 4 * pow(m, 2)*(2 * pow(m, 2) - s13 - s23)*DFmly2[4] - 4 * pow(m, 2)*DFmly2[5] - pow(-2 * pow(m, 2) + s34, 2)*DFmly2[6] - 2 * s13*(2 * pow(m, 2) - s34)*DFmly2[7] +
		2 * (2 * pow(m, 2) - s13 - s23)*(2 * pow(m, 2) - s34)*DFmly2[8] - pow(s13, 2)*DFmly2[9] - 2 * s13*(-2 * pow(m, 2) + s13 + s23)*DFmly2[10] - pow(-2 * pow(m, 2) + s13 + s23, 2)*DFmly2[11])) / (16.*PI);
	double INT134 = (ALPHA * (-BFmly[1] + BFmly[7] - (4 * pow(m, 2) - s14 - s24)*CFmly1[2] - s34*CFmly1[3] - (-4 * pow(m, 2) + s13 + s23)*CFmly3[1] + (2 * pow(m, 2) - s23)*CFmly3[2] + (4 * pow(m, 2) - s13 - s23 - s34)*CFmly3[3] + (-4 * pow(m, 2) + s13 + s23)*CFmly4[1] +
		(-2 * pow(m, 2) + s23)*CFmly4[2] - (2 * pow(m, 2) - s13 - s23)*CFmly4[3] + (pow(M34, 2) - s34)*CFmly8[1] + 2 * pow(m, 2)*(-pow(M34, 2) + s34)*DFmly2[1] + (2 * pow(m, 2) - s34)*(-pow(M34, 2) + s34)*DFmly2[2] -
		s13*(pow(M34, 2) - s34)*DFmly2[3] + (2 * pow(m, 2) - s13 - s23)*(pow(M34, 2) - s34)*DFmly2[4])) / (16.*PI);
	double INT144 = (ALPHA * ((pow(M34, 2) - s34)*CFmly1[1] + (4 * pow(m, 2) - s14 - s24)*CFmly1[2] + s34*CFmly1[3] + (4 * pow(m, 2) - pow(M34, 2) - s14 - s24 + s34)*CFmly3[1] + (2 * pow(m, 2) - s24)*CFmly3[2] + (4 * pow(m, 2) - s14 - s24 - s34)*CFmly3[3] -
		(4 * pow(m, 2) - pow(M34, 2) - s14 - s24 + s34)*CFmly4[1] + (-2 * pow(m, 2) + s24)*CFmly4[2] - (6 * pow(m, 2) - s14 - s24 - s34)*CFmly4[3] - (2 * pow(m, 2) + pow(M34, 2) - 2 * s34)*CFmly6[1] + (-2 * pow(m, 2) + s34)*CFmly6[2] -
		(6 * pow(m, 2) - s14 - s24 - s34)*CFmly6[3] + pow(pow(M34, 2) - s34, 2)*DFmly2[1] - pow(pow(M34, 2) - s34, 2)*DFmly3[1])) / (16.*PI);
	double INT222 = (ALPHA * (EFmly*pow(-pow(M12, 2) + s12, 3) + (4 * pow(m, 4) + pow(M12, 4) + pow(s12, 2) - s12*s23 + pow(s23, 2) + pow(M12, 2)*(-2 * s12 + s23) - 2 * pow(m, 2)*(pow(M12, 2) - s12 + 2 * s23))*DFmly3[1] +
		(4 * pow(m, 2) - pow(M12, 2) + s12 - 2 * s23)*(2 * pow(m, 2) - s23)*DFmly3[2] + (4 * pow(m, 2) - pow(M12, 2) + s12 - 2 * s23)*(-2 * pow(m, 2) + s24)*DFmly3[3] + (4 * pow(m, 2) - pow(M12, 2) + s12 - 2 * s23)*(2 * pow(m, 2) - s12 - s23)*DFmly3[4] +
		4 * pow(m, 2)*DFmly3[5] + pow(-2 * pow(m, 2) + s23, 2)*DFmly3[6] - 2 * (-2 * pow(m, 2) + s23)*(-2 * pow(m, 2) + s24)*DFmly3[7] + 2 * (2 * pow(m, 2) - s23)*(2 * pow(m, 2) - s12 - s23)*DFmly3[8] + pow(-2 * pow(m, 2) + s24, 2)*DFmly3[9] -
		2 * (2 * pow(m, 2) - s12 - s23)*(2 * pow(m, 2) - s24)*DFmly3[10] + pow(-2 * pow(m, 2) + s12 + s23, 2)*DFmly3[11] +
		(-4 * pow(m, 4) - pow(M12, 4) - pow(s12, 2) + s12*s23 - pow(s23, 2) - pow(M12, 2)*(-2 * s12 + s23) + 2 * pow(m, 2)*(pow(M12, 2) - s12 + 2 * s23))*DFmly4[1] + (4 * pow(m, 2) - pow(M12, 2) + s12 - 2 * s23)*(-2 * pow(m, 2) + s23)*DFmly4[2] +
		(4 * pow(m, 2) - pow(M12, 2) + s12 - 2 * s23)*(2 * pow(m, 2) - s24)*DFmly4[3] + (4 * pow(m, 2) - s12 - s23)*(-4 * pow(m, 2) + pow(M12, 2) - s12 + 2 * s23)*DFmly4[4] - 4 * pow(m, 2)*DFmly4[5] - pow(-2 * pow(m, 2) + s23, 2)*DFmly4[6] +
		2 * (-2 * pow(m, 2) + s23)*(-2 * pow(m, 2) + s24)*DFmly4[7] - 2 * (2 * pow(m, 2) - s23)*(4 * pow(m, 2) - s12 - s23)*DFmly4[8] - pow(-2 * pow(m, 2) + s24, 2)*DFmly4[9] + 2 * (4 * pow(m, 2) - s12 - s23)*(2 * pow(m, 2) - s24)*DFmly4[10] -
		pow(-4 * pow(m, 2) + s12 + s23, 2)*DFmly4[11])) / (16.*PI);
	double INT223 = (ALPHA * ((-pow(M12, 2) + s12)*CFmly1[1] - s12*CFmly1[2] - (4 * pow(m, 2) - s23 - s24)*CFmly1[3] + (2 * pow(m, 2) + pow(M12, 2) - 2 * s12)*CFmly2[1] + (2 * pow(m, 2) - s12)*CFmly2[2] + (6 * pow(m, 2) - s12 - s23 - s24)*CFmly2[3] -
		(4 * pow(m, 2) - pow(M12, 2) + s12 - s23 - s24)*CFmly8[1] + (-2 * pow(m, 2) + s24)*CFmly8[2] - (4 * pow(m, 2) - s12 - s23 - s24)*CFmly8[3] + (2 * pow(m, 2) - pow(M12, 2) + s12 - s23)*CFmly9[1] + (-2 * pow(m, 2) + s24)*CFmly9[2] +
		(4 * pow(m, 2) - s12 - s23)*CFmly9[3] + pow(pow(M12, 2) - s12, 2)*DFmly1[1] - pow(pow(M12, 2) - s12, 2)*DFmly2[1])) / (16.*PI);
	double INT224 = (ALPHA * (EFmly*pow(pow(M12, 2) - s12, 2)*(pow(M34, 2) - s34) + (pow(M12, 2) - s12)*CFmly1[1] + s12*CFmly1[2] + (4 * pow(m, 2) - s23 - s24)*CFmly1[3] - (2 * pow(m, 2) + pow(M12, 2) - 2 * s12)*CFmly2[1] + (-2 * pow(m, 2) + s12)*CFmly2[2] -
		(6 * pow(m, 2) - s12 - s23 - s24)*CFmly2[3] + (2 * pow(m, 2) - pow(M12, 2) + s12 - s23)*CFmly6[1] + (2 * pow(m, 2) - s23)*CFmly6[2] + (2 * pow(m, 2) - s12 - s23)*CFmly6[3] + (pow(M12, 2) - s12)*CFmly7[1] + (-2 * pow(m, 2) + s12)*CFmly7[2] +
		(2 * pow(m, 2) - s23)*CFmly7[2] - pow(pow(M12, 2) - s12, 2)*DFmly1[1] + (2 * pow(m, 2) - pow(M12, 2) + s12 - s23)*(pow(M34, 2) - s34)*DFmly3[1] + (-2 * pow(m, 2) + s23)*(-pow(M34, 2) + s34)*DFmly3[2] -
		(-2 * pow(m, 2) + s24)*(-pow(M34, 2) + s34)*DFmly3[3] + (2 * pow(m, 2) - s12 - s23)*(pow(M34, 2) - s34)*DFmly3[4] + (2 * pow(m, 2) - pow(M12, 2) + s12 - s23)*(-pow(M34, 2) + s34)*DFmly4[1] -
		(-2 * pow(m, 2) + s23)*(-pow(M34, 2) + s34)*DFmly4[2] + (-2 * pow(m, 2) + s24)*(-pow(M34, 2) + s34)*DFmly4[3] - (4 * pow(m, 2) - s12 - s23)*(pow(M34, 2) - s34)*DFmly4[4] + pow(pow(M12, 2) - s12, 2)*DFmly5[1])) / (16.*PI);
	double INT233 = (ALPHA * ((4 * pow(m, 2) - s13 - s23)*CFmly1[2] + s34*CFmly1[3] + (2 * pow(m, 2) - s13)*CFmly2[1] + (2 * pow(m, 2) - s13)*CFmly2[2] + (2 * pow(m, 2) - s13 - s34)*CFmly2[3] + s34*CFmly8[1] + (-2 * pow(m, 2) + s34)*CFmly8[2] -
		(4 * pow(m, 2) - s13 - s23 - s34)*CFmly8[3] - 2 * pow(m, 2)*CFmly9[1] + (-2 * pow(m, 2) + s34)*CFmly9[2] - s13*CFmly9[3] + (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s13)*DFmly1[1] + (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s13)*DFmly1[2] -
		(-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s23)*DFmly1[3] + (pow(M12, 2) - s12)*(2 * pow(m, 2) - s13 - s34)*DFmly1[4] + 2 * pow(m, 2)*(-pow(M12, 2) + s12)*DFmly2[1] - (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s34)*DFmly2[2] -
		(pow(M12, 2) - s12)*s13*DFmly2[3] + (pow(M12, 2) - s12)*(2 * pow(m, 2) - s13 - s23)*DFmly2[4])) / (16.*PI);
	double INT234 = (ALPHA * (BFmly[1] + BFmly[5] - BFmly[6] - BFmly[7] + (4 * pow(m, 2) - s14 - s24)*CFmly1[2] + s34*CFmly1[3] + (2 * pow(m, 2) - s14)*CFmly2[1] + (2 * pow(m, 2) - s14)*CFmly2[2] + (2 * pow(m, 2) - s14 - s34)*CFmly2[3] + (-pow(M12, 2) + s12)*CFmly3[1] +
		(pow(M12, 2) - s12)*CFmly4[1] + (-pow(M34, 2) + s34)*CFmly8[1] + (pow(M34, 2) - s34)*CFmly9[1] + (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s14)*DFmly1[1] + (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s14)*DFmly1[2] -
		(-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s24)*DFmly1[3] + (pow(M12, 2) - s12)*(2 * pow(m, 2) - s14 - s34)*DFmly1[4] + (pow(M12, 2) - s12)*(pow(M34, 2) - s34)*DFmly2[1])) / (16.*PI);
	double INT244 = (ALPHA * (EFmly*(-pow(M12, 2) + s12)*pow(pow(M34, 2) - s34, 2) + (-pow(M34, 2) + s34)*CFmly1[1] - (4 * pow(m, 2) - s14 - s24)*CFmly1[2] - s34*CFmly1[3] - (2 * pow(m, 2) - pow(M34, 2) - s14 + s34)*CFmly2[1] + (-2 * pow(m, 2) + s14)*CFmly2[2] -
		(2 * pow(m, 2) - s14 - s34)*CFmly2[3] + (2 * pow(m, 2) + pow(M34, 2) - 2 * s34)*CFmly6[1] + (2 * pow(m, 2) - s34)*CFmly6[2] + (6 * pow(m, 2) - s14 - s24 - s34)*CFmly6[3] + (-pow(M34, 2) + s34)*CFmly7[1] + (-2 * pow(m, 2) + s14)*CFmly7[2] +
		(2 * pow(m, 2) - s34)*CFmly7[2] + (-pow(M12, 2) + s12)*(2 * pow(m, 2) - pow(M34, 2) - s14 + s34)*DFmly1[1] - (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s14)*DFmly1[2] + (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s24)*DFmly1[3] -
		(pow(M12, 2) - s12)*(2 * pow(m, 2) - s14 - s34)*DFmly1[4] + pow(pow(M34, 2) - s34, 2)*DFmly3[1] - pow(pow(M34, 2) - s34, 2)*DFmly4[1] + (pow(M12, 2) - s12)*(2 * pow(m, 2) - pow(M34, 2) - s14 + s34)*DFmly5[1] +
		(-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s14)*DFmly5[2] - (-pow(M12, 2) + s12)*(-2 * pow(m, 2) + s24)*DFmly5[3] + (pow(M12, 2) - s12)*(4 * pow(m, 2) - s14 - s34)*DFmly5[4])) / (16.*PI);
	double INT333 = (ALPHA * (pow(-2 * pow(m, 2) + s13, 2)*DFmly1[1] + 2 * pow(-2 * pow(m, 2) + s13, 2)*DFmly1[2] - 2 * (-2 * pow(m, 2) + s13)*(-2 * pow(m, 2) + s23)*DFmly1[3] + 2 * (2 * pow(m, 2) - s13)*(2 * pow(m, 2) - s13 - s34)*DFmly1[4] + 4 * pow(m, 2)*DFmly1[5] +
		pow(-2 * pow(m, 2) + s13, 2)*DFmly1[6] - 2 * (-2 * pow(m, 2) + s13)*(-2 * pow(m, 2) + s23)*DFmly1[7] + 2 * (2 * pow(m, 2) - s13)*(2 * pow(m, 2) - s13 - s34)*DFmly1[8] + pow(-2 * pow(m, 2) + s23, 2)*DFmly1[9] -
		2 * (2 * pow(m, 2) - s23)*(2 * pow(m, 2) - s13 - s34)*DFmly1[10] + pow(-2 * pow(m, 2) + s13 + s34, 2)*DFmly1[11] - 4 * pow(m, 4)*DFmly2[1] + 4 * pow(m, 2)*(-2 * pow(m, 2) + s34)*DFmly2[2] - 4 * pow(m, 2)*s13*DFmly2[3] +
		4 * pow(m, 2)*(2 * pow(m, 2) - s13 - s23)*DFmly2[4] - 4 * pow(m, 2)*DFmly2[5] - pow(-2 * pow(m, 2) + s34, 2)*DFmly2[6] - 2 * s13*(2 * pow(m, 2) - s34)*DFmly2[7] + 2 * (2 * pow(m, 2) - s13 - s23)*(2 * pow(m, 2) - s34)*DFmly2[8] -
		pow(s13, 2)*DFmly2[9] - 2 * s13*(-2 * pow(m, 2) + s13 + s23)*DFmly2[10] - pow(-2 * pow(m, 2) + s13 + s23, 2)*DFmly2[11])) / (16.*PI);
	double INT334 = (ALPHA * ((-2 * pow(m, 2) + s13 + s23 - s24)*CFmly3[1] + (s23 - s24)*CFmly3[2] + (s13 - s14 + s23 - s24)*CFmly3[3] - (-4 * pow(m, 2) + s13 + s23)*CFmly4[1] + (2 * pow(m, 2) - s23)*CFmly4[2] + (2 * pow(m, 2) - s13 - s23)*CFmly4[3] +
		(-2 * pow(m, 2) + s14)*CFmly5[2] + (2 * pow(m, 2) - s24)*CFmly5[3] + (-2 * pow(m, 2) + s13)*(-2 * pow(m, 2) + s14)*DFmly1[1] + (-2 * pow(m, 2) + s14)*(pow(M34, 2) + s13 - s14 - s34)*DFmly1[2] +
		(s14*(-s23 + s24) + 2 * pow(m, 2)*(pow(M34, 2) + s23 - s24 - s34) + s24*(-pow(M34, 2) + s34))*DFmly1[3] -
		(-(s13*s14) + pow(s14, 2) + 2 * pow(m, 2)*(pow(M34, 2) + s13 - s14 - s34) + s14*s34 + pow(s34, 2) - pow(M34, 2)*(s14 + s34))*DFmly1[4] - 4 * pow(m, 2)*DFmly1[5] - pow(-2 * pow(m, 2) + s14, 2)*DFmly1[6] +
		2 * (-2 * pow(m, 2) + s14)*(-2 * pow(m, 2) + s24)*DFmly1[7] - 2 * (2 * pow(m, 2) - s14)*(2 * pow(m, 2) - s14 - s34)*DFmly1[8] - pow(-2 * pow(m, 2) + s24, 2)*DFmly1[9] + 2 * (2 * pow(m, 2) - s24)*(2 * pow(m, 2) - s14 - s34)*DFmly1[10] -
		pow(-2 * pow(m, 2) + s14 + s34, 2)*DFmly1[11] + 2 * pow(m, 2)*(pow(M34, 2) - s34)*DFmly2[1] + (-2 * pow(m, 2) + s34)*(-pow(M34, 2) + s34)*DFmly2[2] + s13*(pow(M34, 2) - s34)*DFmly2[3] - (2 * pow(m, 2) - s13 - s23)*(pow(M34, 2) - s34)*DFmly2[4]
		)) / (16.*PI);
	double INT344 = (ALPHA * ((-4 * pow(m, 2) + pow(M34, 2) + s14 + s24 - s34)*CFmly3[1] + (-2 * pow(m, 2) + s24)*CFmly3[2] - (4 * pow(m, 2) - s14 - s24 - s34)*CFmly3[3] + (4 * pow(m, 2) - pow(M34, 2) - s14 - s24 + s34)*CFmly4[1] + (2 * pow(m, 2) - s24)*CFmly4[2] +
		(6 * pow(m, 2) - s14 - s24 - s34)*CFmly4[3] + pow(-2 * pow(m, 2) + s14, 2)*DFmly1[1] + 2 * pow(-2 * pow(m, 2) + s14, 2)*DFmly1[2] - 2 * (-2 * pow(m, 2) + s14)*(-2 * pow(m, 2) + s24)*DFmly1[3] +
		2 * (2 * pow(m, 2) - s14)*(2 * pow(m, 2) - s14 - s34)*DFmly1[4] + 4 * pow(m, 2)*DFmly1[5] + pow(-2 * pow(m, 2) + s14, 2)*DFmly1[6] - 2 * (-2 * pow(m, 2) + s14)*(-2 * pow(m, 2) + s24)*DFmly1[7] +
		2 * (2 * pow(m, 2) - s14)*(2 * pow(m, 2) - s14 - s34)*DFmly1[8] + pow(-2 * pow(m, 2) + s24, 2)*DFmly1[9] - 2 * (2 * pow(m, 2) - s24)*(2 * pow(m, 2) - s14 - s34)*DFmly1[10] + pow(-2 * pow(m, 2) + s14 + s34, 2)*DFmly1[11] -
		pow(pow(M34, 2) - s34, 2)*DFmly2[1])) / (16.*PI);
	double INT444 = (ALPHA * (EFmly*pow(pow(M34, 2) - s34, 3) + (-pow(-2 * pow(m, 2) + s14, 2) - pow(pow(M34, 2) - s34, 2) + (-2 * pow(m, 2) + s14)*(-pow(M34, 2) + s34))*DFmly1[1] + (-2 * pow(m, 2) + s14)*(4 * pow(m, 2) - pow(M34, 2) - 2 * s14 + s34)*DFmly1[2] +
		(2 * pow(m, 2) - s24)*(4 * pow(m, 2) - pow(M34, 2) - 2 * s14 + s34)*DFmly1[3] + (2 * pow(m, 2) - s14 - s34)*(-4 * pow(m, 2) + pow(M34, 2) + 2 * s14 - s34)*DFmly1[4] - 4 * pow(m, 2)*DFmly1[5] - pow(-2 * pow(m, 2) + s14, 2)*DFmly1[6] +
		2 * (-2 * pow(m, 2) + s14)*(-2 * pow(m, 2) + s24)*DFmly1[7] - 2 * (2 * pow(m, 2) - s14)*(2 * pow(m, 2) - s14 - s34)*DFmly1[8] - pow(-2 * pow(m, 2) + s24, 2)*DFmly1[9] + 2 * (2 * pow(m, 2) - s24)*(2 * pow(m, 2) - s14 - s34)*DFmly1[10] -
		pow(-2 * pow(m, 2) + s14 + s34, 2)*DFmly1[11] + (pow(-2 * pow(m, 2) + s14, 2) + pow(pow(M34, 2) - s34, 2) - (-2 * pow(m, 2) + s14)*(-pow(M34, 2) + s34))*DFmly5[1] +
		(2 * pow(m, 2) - s14)*(4 * pow(m, 2) - pow(M34, 2) - 2 * s14 + s34)*DFmly5[2] + (-2 * pow(m, 2) + s24)*(4 * pow(m, 2) - pow(M34, 2) - 2 * s14 + s34)*DFmly5[3] + (4 * pow(m, 2) - s14 - s34)*(4 * pow(m, 2) - pow(M34, 2) - 2 * s14 + s34)*DFmly5[4] +
		4 * pow(m, 2)*DFmly5[5] + pow(-2 * pow(m, 2) + s14, 2)*DFmly5[6] - 2 * (-2 * pow(m, 2) + s14)*(-2 * pow(m, 2) + s24)*DFmly5[7] + 2 * (2 * pow(m, 2) - s14)*(4 * pow(m, 2) - s14 - s34)*DFmly5[8] + pow(-2 * pow(m, 2) + s24, 2)*DFmly5[9] -
		2 * (2 * pow(m, 2) - s24)*(4 * pow(m, 2) - s14 - s34)*DFmly5[10] + pow(-4 * pow(m, 2) + s14 + s34, 2)*DFmly5[11])) / (16.*PI);

	double I3[] = { INTKK1, INTKK2, INTKK3, INTKK4, INT111, INT112, INT113, INT114, INT122, INT123, INT124, INT133, INT134, INT144, INT222, INT223, INT224, INT233, INT234, INT244, INT333, INT334, INT344, INT444 };

	/// Pent2IR1Coeff()
	double C1A = (4 * (128 * pow(m, 8) + pow(s14, 2)*pow(s23, 2) + pow(s13, 3)*s24 - 32 * pow(m, 6)*(2 * s12 + 5 * s13 - 3 * s14 - 2 * s23 + 2 * s24 - s34) - 2 * s12*s14*s23*s34 - s14*s23*s24*s34 - s12*pow(s24, 2)*s34 - s23*pow(s24, 2)*s34 +
		pow(s12, 2)*pow(s34, 2) + s12*s24*pow(s34, 2) - pow(s13, 2)*(s14*s23 + s12*s34) + 4 * pow(m, 4)*
		(6 * pow(s13, 2) + 4 * s12*s23 - 2 * pow(s23, 2) + 5 * s12*s24 + s23*s24 + 2 * pow(s24, 2) + s13*(s12 - s14 + s23 + 22 * s24 - 3 * s34) - 8 * s23*s34 - 11 * s24*s34 + 2 * pow(s34, 2) - s14*(12 * s23 + 9 * s24 + 8 * s34)) -
		s13*(s14*s23*(s23 + 2 * s24) - s23*(pow(s24, 2) + s12*s34 - s24*s34) + s24*(-pow(s24, 2) + s12*s34 + s24*s34)) +
		pow(m, 2)*(-2 * pow(s13, 3) - 4 * pow(s14, 2)*s23 + pow(s23, 2)*s24 - pow(s24, 3) + 2 * pow(s12, 2)*s34 - 2 * s12*s23*s34 - 2 * pow(s23, 2)*s34 + 7 * s12*s24*s34 + 11 * s23*s24*s34 + 6 * pow(s24, 2)*s34 - 8 * s12*pow(s34, 2) +
			2 * s23*pow(s34, 2) - s24*pow(s34, 2) + pow(s13, 2)*(2 * s12 + 2 * s14 + 2 * s23 - 11 * s24 + 2 * s34) + s14*(2 * pow(s23, 2) - s12*s24 + 7 * s23*s24 + 4 * pow(s24, 2) + 2 * s12*s34 + 8 * s23*s34 + 4 * s24*s34) +
			s13*(-pow(s23, 2) - 4 * s12*s24 - 12 * pow(s24, 2) - 3 * s23*(s12 + 4 * s24 - 2 * s34) + 11 * s12*s34 + 8 * s24*s34 + pow(s34, 2) + s14*(2 * s12 + 11 * s23 - 2 * s24 + s34))))) / (s14*s23);
	double C2A = (-4 * (-384 * pow(m, 8) + 32 * pow(m, 6)*(s12 + 2 * s13 + 2 * s14 + s23 + s24 + 4 * s34) + 4 * pow(m, 4)*(pow(s13, 2) - 2 * pow(s14, 2) - s12*s23 - 2 * s12*s24 - 2 * s23*s24 + pow(s24, 2) + s14*(s12 + 2 * s24 - 5 * s34) + 2 * s13*(4 * s24 - 5 * s34) + s23*s34 -
		4 * s24*s34 - 2 * pow(s34, 2)) - s13*(pow(s14, 2)*s23 + s12*(s24 - s34)*s34 + s14*(-pow(s24, 2) - s12*s34 + s23*s34 + s24*s34) + s13*(s14*(s23 + s34) + s24*(-s24 + s34))) +
		pow(m, 2)*(-(pow(s13, 2)*(s12 + 2 * s14 + s23 + 8 * s24 - 2 * s34)) + pow(s14, 2)*(-3 * s12 + 2 * s23 + 2 * s24 + s34) + s34*(-pow(s24, 2) + s23*(s12 + 2 * s24 - 3 * s34) - 2 * s12*s34 + 2 * s24*(s12 + s34)) +
			s14*(-3 * pow(s24, 2) + s34*(-s12 + s34) + 2 * s24*(s12 + s34) + s23*(s12 + 3 * s34)) + s13*(-2 * pow(s14, 2) + s12*s24 - 6 * pow(s24, 2) + s23*(s24 - 4 * s34) + 4 * s12*s34 + 9 * s24*s34 - 2 * pow(s34, 2) +
				s14*(-4 * s12 + 6 * s23 - 5 * s24 + 14 * s34))))) / (s14*s23);
	double C3A = (-4 * (128 * pow(m, 8) + pow(s14, 2)*pow(s23, 2) + pow(s13, 3)*s24 - s12*s14*s23*s24 - s12*s14*pow(s24, 2) - 2 * s12*s14*s23*s34 + pow(s12, 2)*s24*s34 - s12*pow(s24, 2)*s34 + pow(s12, 2)*pow(s34, 2) -
		32 * pow(m, 6)*(-s12 + 5 * s13 - 2 * s14 - 3 * s23 + 2 * s24 + 2 * s34) - pow(s13, 2)*(s14*s23 + s12*s34) +
		4 * pow(m, 4)*(2 * pow(s12, 2) + 6 * pow(s13, 2) - 2 * pow(s14, 2) - 8 * s12*s23 - 11 * s12*s24 - 9 * s23*s24 + 2 * pow(s24, 2) + 5 * s24*s34 + s13*(-3 * s12 + s14 - s23 + 22 * s24 + s34) + s14*(-8 * s12 - 12 * s23 + s24 + 4 * s34)) -
		s13*(pow(s14, 2)*s23 + s14*(s12*s24 + 2 * s23*s24 - pow(s24, 2) - s12*s34) + s24*(s12*s24 - pow(s24, 2) + s12*s34)) +
		pow(m, 2)*(-2 * pow(s13, 3) - pow(s12, 2)*s24 + 4 * s12*s23*s24 + 6 * s12*pow(s24, 2) + 4 * s23*pow(s24, 2) - pow(s24, 3) + pow(s14, 2)*(-2 * s12 + 2 * s23 + s24) +
			s14*(-4 * pow(s23, 2) + s23*(8 * s12 + 7 * s24) + s12*(2 * s12 + 11 * s24 - 2 * s34)) - 8 * pow(s12, 2)*s34 + 2 * s12*s23*s34 + 7 * s12*s24*s34 - s23*s24*s34 + 2 * s12*pow(s34, 2) + pow(s13, 2)*(2 * s12 + 2 * s14 + 2 * s23 - 11 * s24 + 2 * s34) +
			s13*(pow(s12, 2) - pow(s14, 2) + 8 * s12*s24 - 12 * pow(s24, 2) + 11 * s12*s34 - 4 * s24*s34 + s23*(s12 - 2 * s24 + 2 * s34) + s14*(11 * s23 - 3 * (-2 * s12 + 4 * s24 + s34)))))) / (s14*s23);
	double C4A = (4 * (-384 * pow(m, 8) + 32 * pow(m, 6)*(4 * s12 + 2 * s13 + s14 + 2 * s23 + s24 + s34) - s13*(s14*s23*(s12 + s23) + s12*s23*s24 - s23*pow(s24, 2) + s13*(s12*s23 + s14*s23 + s12*s24 - pow(s24, 2)) - pow(s12, 2)*s34 - s12*s23*s34 +
		s12*s24*s34) + 4 * pow(m, 4)*(-2 * pow(s12, 2) + pow(s13, 2) + s12*s14 - 2 * pow(s23, 2) - 4 * s12*s24 - 2 * s14*s24 + pow(s24, 2) + 2 * s13*(-5 * s12 + 4 * s24) - s14*s34 - 2 * s24*s34 + s23*(-5 * s12 + 2 * s24 + s34)) +
		pow(m, 2)*(pow(s12, 2)*s23 + s12*pow(s23, 2) + 2 * pow(s12, 2)*s24 + 2 * s12*s23*s24 + 2 * pow(s23, 2)*s24 - s12*pow(s24, 2) - 3 * s23*pow(s24, 2) - 2 * pow(s12, 2)*s34 - s12*s23*s34 - 3 * pow(s23, 2)*s34 + 2 * s12*s24*s34 +
			2 * s23*s24*s34 - pow(s13, 2)*(-2 * s12 + s14 + 2 * s23 + 8 * s24 + s34) + s13*(-2 * pow(s12, 2) + 14 * s12*s23 - 2 * pow(s23, 2) + 9 * s12*s24 - 5 * s23*s24 - 6 * pow(s24, 2) + s14*(-4 * s12 + 6 * s23 + s24) + 4 * s12*s34 - 4 * s23*s34 + s24*s34) +
			s14*(2 * pow(s23, 2) + s23*(3 * s12 + s34) + s12*(-3 * s12 + 2 * s24 + s34))))) / (s14*s23);
	double C1B = (4 * (2 * pow(m, 2) - s13)*(64 * pow(m, 6) - pow(s13, 2)*s24 + s23*pow(s24, 2) + s12*s13*s34 + s12*s23*s34 + s13*s23*s34 - s13*s24*s34 + s23*s24*s34 + s12*pow(s34, 2) - s14*s23*(s23 - s24 + s34) -
		16 * pow(m, 4)*(s13 - s23 + 2 * s24 + s34) + 2 * pow(m, 2)*(pow(s13, 2) - 2 * s14*s23 + s12*s24 - 2 * s23*s24 + pow(s24, 2) - 2 * s12*s34 - 4 * s23*s34 + 3 * s24*s34 + s13*(-s12 - 2 * s23 + 4 * s24 + s34)))) / (s14*s23);
	double C2B = (4 * (2 * pow(m, 2) - s13)*(64 * pow(m, 6) + pow(s13, 2)*s14 - pow(s14, 2)*s23 + s12*s14*s34 - s14*s23*s34 + s12*s24*s34 + s14*s24*s34 + s12*pow(s34, 2) - 16 * pow(m, 4)*(2 * s13 - s14 + s24 + s34) +
		s13*(s14*(s23 + s34) - s24*(s24 + s34)) + 2 * pow(m, 2)*(pow(s13, 2) - s12*s24 + pow(s24, 2) - 2 * s12*s34 + s24*s34 - 2 * s14*(s23 + s24 + 2 * s34) + s13*(s12 - 2 * s14 + 4 * s24 + 3 * s34)))) / (s14*s23);
	double C3B = (-4 * (2 * pow(m, 2) - s13)*(64 * pow(m, 6) - pow(s14, 2)*s23 - 16 * pow(m, 4)*(s12 + s13 - s14 + 2 * s24) - (s12 + s13)*(s13*s24 - s12*s34) +
		2 * pow(m, 2)*(pow(s13, 2) + 3 * s12*s24 + pow(s24, 2) - 2 * s14*(2 * s12 + s23 + s24) + s13*(s12 - 2 * s14 + 4 * s24 - s34) - 2 * s12*s34 + s24*s34) + s14*(s12*s24 + pow(s24, 2) + s23*(-s12 + s24) + s12*(s13 + s34)))) / (s14*s23);
	double C4B = (-4 * (2 * pow(m, 2) - s13)*(64 * pow(m, 6) + pow(s13, 2)*s23 - s12*s14*s23 - s14*pow(s23, 2) + s12*s23*s24 - 16 * pow(m, 4)*(s12 + 2 * s13 - s23 + s24) + s13*(s12*s23 + s14*s23 - s12*s24 - pow(s24, 2)) + pow(s12, 2)*s34 + s12*s23*s34 +
		s12*s24*s34 + 2 * pow(m, 2)*(pow(s13, 2) - 4 * s12*s23 - 2 * s14*s23 + s12*s24 - 2 * s23*s24 + pow(s24, 2) - 2 * s12*s34 - s24*s34 + s13*(3 * s12 - 2 * s23 + 4 * s24 + s34)))) / (s14*s23);

	double C1[] = { C1A + C1B, C2A + C2B, C3A + C3B, C4A + C4B };

	/// Pent2IR2Coeff()
	double CKKA = (-2 * (512 * pow(m, 8) + 2 * pow(s14, 2)*pow(s23, 2) + pow(s13, 3)*s24 + pow(s14, 2)*s23*s24 + s14*pow(s23, 2)*s24 + s14*s23*pow(s24, 2) - 256 * pow(m, 6)*(s13 + s24) - 4 * s12*s14*s23*s34 - s12*s14*s24*s34 - s12*s23*s24*s34 -
		s12*pow(s24, 2)*s34 + 2 * pow(s12, 2)*pow(s34, 2) + 32 * pow(m, 4)*(pow(s13, 2) - 2 * s14*s23 + 4 * s13*s24 + pow(s24, 2) - 2 * s12*s34) - pow(s13, 2)*(s23*s24 + s14*(3 * s23 + s24) + s12*s34) +
		s13*(-(pow(s14, 2)*s23) + pow(s24, 3) - 2 * s12*s24*s34 + s23*(pow(s24, 2) + s12*s34) + s14*(-pow(s23, 2) - 2 * s23*s24 + pow(s24, 2) + s12*s34)) -
		2 * pow(m, 2)*(pow(s13, 3) - s12*pow(s23, 2) - 2 * s12*s23*s24 - pow(s23, 2)*s24 - s12*pow(s24, 2) + pow(s24, 3) + pow(s23, 2)*s34 - 8 * s12*s24*s34 - pow(s24, 2)*s34 - 2 * s14*s24*(s23 + s34) -
			pow(s13, 2)*(s12 + 2 * s14 + 2 * s23 - 7 * s24 + s34) - pow(s14, 2)*(-s12 + s24 + s34) + s13*(pow(s14, 2) + pow(s23, 2) + 2 * s12*s24 + 7 * pow(s24, 2) + 2 * s23*(s12 + s24) - 8 * s12*s34 + 2 * s24*s34 + 2 * s14*(-7 * s23 + s24 + s34))))) /
			(s14*s23);
	double C11A = (8 * pow(m, 2)*(-2 * s14*s23 + s23*s24 + pow(s24, 2) - s13*(3 * s23 + s24) + 4 * pow(m, 2)*(s13 + 2 * s23 - s24 - 2 * s34) + s12*s34 - 4 * s23*s34 + 2 * s24*s34 + pow(s34, 2))) / (s14*s23);
	double C12A = (-8 * pow(m, 2)*(-pow(s13, 2) - s14*s24 - s23*s24 - pow(s24, 2) - 2 * s12*s34 + 4 * s23*s34 - 2 * s24*s34 - 2 * pow(s34, 2) + 8 * pow(m, 2)*(s14 - s23 + 2 * s34) - s13*(s14 - 3 * s23 - 2 * s24 + 2 * s34))) / (s14*s23);
	double C13A = (-8 * pow(m, 2)*(64 * pow(m, 4) + s12*s13 - s12*s23 + s12*s24 + 4 * s13*s24 - 2 * s23*s24 + s13*s34 + s23*s34 + s24*s34 - 8 * pow(m, 2)*(s12 + 2 * s13 - s14 - s23 + 2 * s24 + s34) - s14*(-s12 + 8 * s23 + 2 * s24 + s34))) / (s14*s23);
	double C14A = (-16 * pow(m, 2)*(32 * pow(m, 4) - 2 * s14*s23 + s12*s24 - s23*s24 + 2 * s23*s34 - 4 * pow(m, 2)*(s12 + 2 * s13 + 2 * s24 + s34) + s13*(s23 + 2 * s24 + s34))) / (s14*s23);
	double C22A = (-8 * pow(m, 2)*(-pow(s13, 2) - 2 * s14*s23 - s14*s24 + s13*(-s14 + s24 - 2 * s34) - s12*s34 - pow(s34, 2) + 4 * pow(m, 2)*(s13 + 2 * s14 - s24 + 2 * s34))) / (s14*s23);
	double C23A = (-16 * pow(m, 2)*(32 * pow(m, 4) + 2 * s12*s14 - 2 * s14*s23 - s14*s24 + s13*(s12 + s14 + 2 * s24) + s24*s34 - 4 * pow(m, 2)*(s12 + 2 * s13 + 2 * s24 + s34))) / (s14*s23);
	double C24A = (-8 * pow(m, 2)*(64 * pow(m, 4) + 3 * s12*s14 + s12*s23 + s12*s24 + s14*s34 + 3 * s23*s34 + s24*s34 - 8 * pow(m, 2)*(s12 + 2 * s13 + s14 + s23 + 2 * s24 + s34) + s13*(s12 + 2 * s14 + 2 * s23 + 4 * s24 + s34))) / (s14*s23);
	double C33A = (8 * pow(m, 2)*(pow(s12, 2) - 4 * s12*s14 - 2 * s14*s23 + 4 * pow(m, 2)*(-2 * s12 + s13 + 2 * s14 - s24) + 2 * s12*s24 + s14*s24 + pow(s24, 2) - s13*(3 * s14 + s24) + s12*s34)) / (s14*s23);
	double C34A = (8 * pow(m, 2)*(2 * pow(s12, 2) + pow(s13, 2) - 4 * s12*s14 + 8 * pow(m, 2)*(-2 * s12 + s14 - s23) + s13*(2 * s12 - 3 * s14 + s23 - 2 * s24) + 2 * s12*s24 + s14*s24 + s23*s24 + pow(s24, 2) + 2 * s12*s34)) / (s14*s23);
	double C44A = (-8 * pow(m, 2)*(-pow(s12, 2) - pow(s13, 2) - 2 * s14*s23 + 4 * pow(m, 2)*(2 * s12 + s13 + 2 * s23 - s24) - s23*s24 + s13*(-2 * s12 - s23 + s24) - s12*s34)) / (s14*s23);
	double CKKB = (-2 * (512 * pow(m, 8) + 2 * pow(s13, 3)*s24 - 128 * pow(m, 6)*(s12 + s13 - s14 - s23 + s24 + s34) + pow(s13, 2)*((s12 + s23)*s24 + s14*(2 * s23 + s24) + (-2 * s12 + s24)*s34) +
		(s14*s23 - s12*s34)*(s14*(2 * s23 - s24) - (s12 + s23)*s24 - (2 * s12 + s24)*s34) + 16 * pow(m, 4)*
		(pow(s12, 2) + 2 * pow(s13, 2) - s12*s23 + s12*s24 - s23*s24 + 2 * s12*s34 - 3 * s23*s34 + s24*s34 + pow(s34, 2) - s13*(s12 + 3 * s14 + 3 * s23 - 2 * s24 + s34) - s14*(3 * s12 + 4 * s23 + s24 + s34)) +
		s13*(pow(s14, 2)*s23 - (s12 + s23)*pow(s24, 2) - (s12*s23 + pow(s12 + s24, 2))*s34 - s12*pow(s34, 2) + s14*(pow(s23, 2) - pow(s24, 2) - s12*s34 + s23*(s12 - 6 * s24 + s34))) +
		2 * pow(m, 2)*(-2 * pow(s13, 3) - s12*pow(s23, 2) - pow(s12, 2)*s24 - pow(s23, 2)*s24 + s12*pow(s24, 2) + 2 * s23*pow(s24, 2) + (2 * s12*s23 + pow(s23, 2) + pow(s24, 2) - 2 * s12*(2 * s12 + s24))*s34 +
		(-4 * s12 + 2 * s23 - s24)*pow(s34, 2) + pow(s13, 2)*(s12 + 2 * s14 + 2 * s23 - 8 * s24 + s34) - pow(s14, 2)*(-s12 + 4 * s23 + s24 + s34) +
			s13*(pow(s12, 2) + pow(s14, 2) + 2 * s12*s23 + pow(s23, 2) + 2 * s12*s24 + 2 * pow(s24, 2) + 2 * (5 * s12 + 3 * s23 + s24)*s34 + pow(s34, 2) + 2 * s14*(3 * s12 + s23 + s34)) +
			2 * s14*(-2 * pow(s23, 2) + pow(s24, 2) + s12*(s12 + s34) + s23*(3 * s24 + 2 * (s12 + s34)))))) / (s14*s23);
	double C11B = (-8 * (32 * pow(m, 6) - pow(s13, 2)*s24 + s23*s24*(s14 - s34) + s13*(s12 + s23)*s34 - 4 * pow(m, 4)*(s12 + 3 * s13 + 3 * s24 + s34) +
		pow(m, 2)*(2 * pow(s13, 2) + 2 * pow(s23, 2) + 2 * s12*s24 + s13*(-2 * s12 - 3 * s23 + 6 * s24) + s23*(s12 + 5 * s24 + s34)))) / (s14*s23);
	double C12B = (-8 * (64 * pow(m, 6) + pow(s13, 2)*s14 + s13*s24*s34 - 16 * pow(m, 4)*(s12 + s13 - 2 * s14 - s23 + s34) - (s14 - s34)*(s14*s23 - s12*s34) +
		pow(m, 2)*(pow(s13, 2) + s12*s24 - pow(s24, 2) - 4 * s14*(s12 + s23 + s24) + 6 * s12*s34 - 6 * s14*s34 - 2 * s23*s34 - 3 * s24*s34 + 2 * pow(s34, 2) + s13*(s12 - 6 * s14 - 2 * s23 + s34)))) / (s14*s23);
	double C13B = (8 * (2 * s14*s23*(s13 - s24) + s12*s14*(s13 + s24) + s23*(s13 + s24)*s34 - 8 * pow(m, 4)*(s12 + 2 * s14 + 2 * s23 + 2 * s24 + s34) +
		pow(m, 2)*((s12 + s24 + s34)*(s12 + 2 * s24 + s34) + s13*(s12 + 2 * s14 + 2 * s23 + 2 * s24 + s34) + 2 * (s12*s23 + s14*s34)))) / (s14*s23);
	double C14B = (-8 * (-(pow(s13, 2)*(s23 + s24)) + 8 * pow(m, 4)*(s12 + 2 * s13 + 2 * s23 - s34) - (-s12 + s23 - s24)*(s14*s23 - s12*s34) + s13*(-(s14*s23) + s24*(s12 + 2 * s23 + s24) + s12*s34) -
		2 * pow(m, 2)*(s12*s14 + 2 * s12*s23 - 2 * s14*s23 - 2 * pow(s23, 2) - s14*s24 + s23*s24 + pow(s24, 2) - (2 * s12 + s14 + s24)*s34 + s13*(4 * s12 + s14 + s23 + s24 + s34)))) / (s14*s23);
	double C22B = (-8 * (32 * pow(m, 6) - 2 * s13*s14*s34 + pow(m, 2)*s14*(s12 + 3 * s13 + 2 * s14 - s24 + s34) - 4 * pow(m, 4)*(s12 + s13 + s24 + s34))) / (s14*s23);
	double C23B = (-8 * (-(pow(s13, 2)*(s14 + s24)) + 8 * pow(m, 4)*(-s12 + 2 * s13 + 2 * s14 + s34) - (s14 - s24 - s34)*(s14*s23 - s12*s34) + s13*(-(s14*s23) + 2 * s14*s24 + pow(s24, 2) + (s12 + s24)*s34) -
		2 * pow(m, 2)*(-2 * pow(s14, 2) - s12*s23 - 2 * s14*s23 - s12*s24 + s14*s24 - s23*s24 + pow(s24, 2) + (-2 * s12 + 2 * s14 + s23)*s34 + s13*(s12 + s14 + s23 + s24 + 4 * s34)))) / (s14*s23);
	double C24B = (-8 * (8 * pow(m, 4)*(s12 + 2 * s14 + 2 * s23 - 2 * s24 + s34) + 2 * s13*(s14*s23 - s13*s24 + s12*s34) + pow(m, 2)*(2 * pow(s13, 2) - 2 * s12*s14 - 2 * s23*s34 + (-s12 + s24 - s34)*(s12 + s34) - s13*(2 * s14 + 2 * s23 + 5 * (s12 - 2 * s24 + s34))))) / (s14*s23);
	double C33B = (-8 * (32 * pow(m, 6) - pow(s13, 2)*s24 + s14*(-s12 + s23)*s24 + s12*s13*(s14 + s34) - 4 * pow(m, 4)*(s12 + 3 * s13 + 3 * s24 + s34) + pow(m, 2)*(2 * pow(s13, 2) + s14*(s12 + 2 * s14 + 5 * s24) + s13*(-3 * s14 + 6 * s24 - 2 * s34) + (s14 + 2 * s24)*s34))) /
		(s14*s23);
	double C34B = (-8 * (64 * pow(m, 6) + pow(s13, 2)*s23 + s12*s13*s24 - 16 * pow(m, 4)*(s12 + s13 - s14 - 2 * s23 + s34) - (-s12 + s23)*(s14*s23 - s12*s34) +
		pow(m, 2)*(2 * pow(s12, 2) + pow(s13, 2) - 6 * s12*s23 - 2 * s14*(s12 + 2 * s23) - 3 * s12*s24 - 4 * s23*s24 - pow(s24, 2) + 6 * s12*s34 - 4 * s23*s34 + s24*s34 + s13*(s12 - 2 * s14 - 6 * s23 + s34)))) / (s14*s23);
	double C44B = (-8 * (32 * pow(m, 6) - 2 * s12*s13*s23 + pow(m, 2)*s23*(s12 + 3 * s13 + 2 * s23 - s24 + s34) - 4 * pow(m, 4)*(s12 + s13 + s24 + s34))) / (s14*s23);

	double C2[] = { CKKA + CKKB, C11A + C11B, C12A + C12B, C13A + C13B, C14A + C14B, C22A + C22B, C23A + C23B, C24A + C24B, C33A + C33B, C34A + C34B, C44A + C44B };

	/// Pent2IR3Coeff()
	double CKK1 = (4 * (-64 * pow(m, 6) + 8 * pow(m, 4)*(s12 - s13 + s24 + 3 * s34) - (s13 + s24)*(-(s14*s23) + s13*s24 - (s12 + 2 * s23)*s34) +
		2 * pow(m, 2)*(pow(s13, 2) + s12*s14 + 2 * s12*s23 + 2 * s14*s23 - s14*s24 + s13*(-s12 + s14 + 5 * s24) - (s12 + s14 + 2 * s23 + 3 * s24)*s34 - pow(s34, 2)))) / (s14*s23);
	double CKK2 = (-4 * (-64 * pow(m, 6) - pow(s13, 2)*(2 * s14 + s24) + 8 * pow(m, 4)*(-s12 + 3 * s13 - 4 * s14 + s24 + s34) + (2 * s14 + s24 + 2 * s34)*(s14*s23 - s12*s34) + s13*(-(s14*s23) + pow(s24, 2) + s12*s34 + 2 * s24*s34) +
		2 * pow(m, 2)*(s12*s23 + s12*s24 + s23*s24 - pow(s24, 2) + s13*(6 * s14 - s23 - s24 - 5 * s34) + 5 * s12*s34 - s23*s34 + pow(s34, 2) + 2 * s14*(s23 + s24 + 2 * (s12 + s34))))) / (s14*s23);
	double CKK3 = (4 * (64 * pow(m, 6) + 8 * pow(m, 4)*(-3 * s12 + s13 - s24 - s34) + (s13 + s24)*(-(s14*(2 * s12 + s23)) + s13*s24 - s12*s34) +
		2 * pow(m, 2)*(pow(s12, 2) - pow(s13, 2) + s12*s23 + 3 * s12*s24 + s23*s24 + s12*s34 - s23*s34 - 2 * s14*(-s12 + s23 + s34) + s13*(-s23 - 5 * s24 + s34)))) / (s14*s23);
	double CKK4 = (4 * (-64 * pow(m, 6) - pow(s13, 2)*(2 * s23 + s24) + 8 * pow(m, 4)*(s12 + 3 * s13 - 4 * s23 + s24 - s34) + (2 * s12 + 2 * s23 + s24)*(s14*s23 - s12*s34) + s13*(-(s14*s23) + 2 * s12*s24 + pow(s24, 2) + s12*s34) +
		2 * pow(m, 2)*(pow(s12, 2) + 4 * s12*s23 + 2 * s23*s24 - pow(s24, 2) - s13*(5 * s12 + s14 - 6 * s23 + s24) + (5 * s12 + 4 * s23 + s24)*s34 + s14*(-s12 + 2 * s23 + s24 + s34)))) / (s14*s23);
	double C111 = (-32 * pow(m, 2)) / s14;
	double C112 = (-32 * pow(m, 2)) / s14;
	double C113 = (-32 * pow(m, 2)) / s14;
	double C114 = (-96 * pow(m, 2)) / s14;
	double C122 = (32 * pow(m, 2)) / s23;
	double C123 = (64 * pow(m, 2)) / s23;
	double C124 = (-64 * pow(m, 2)) / s14;
	double C133 = (32 * pow(m, 2)) / s23;
	double C134 = (-64 * pow(m, 2)) / s14;
	double C144 = (-96 * pow(m, 2)) / s14;
	double C222 = (32 * pow(m, 2)) / s23;
	double C223 = (96 * pow(m, 2)) / s23;
	double C224 = (32 * pow(m, 2)) / s23;
	double C233 = (96 * pow(m, 2)) / s23;
	double C234 = (64 * pow(m, 2)) / s23;
	double C244 = (-32 * pow(m, 2)) / s14;
	double C333 = (32 * pow(m, 2)) / s23;
	double C334 = (32 * pow(m, 2)) / s23;
	double C344 = (-32 * pow(m, 2)) / s14;
	double C444 = (-32 * pow(m, 2)) / s14;

	double C3[] = { CKK1, CKK2, CKK3, CKK4, C111, C112, C113, C114, C122, C123, C124, C133, C134, C144, C222, C223, C224, C233, C234, C244, C333, C334, C344, C444 };

	return array_product(C1, I1, (sizeof(I1) / sizeof(*I1))) + array_product(C2, I2, (sizeof(I2) / sizeof(*I2))) + array_product(C3, I3, (sizeof(I3) / sizeof(*I3)));
}
