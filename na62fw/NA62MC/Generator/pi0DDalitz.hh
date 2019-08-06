// set soft-photon energy cut-off for Bremsstrahlung correction; default: EC = 0.0004;
// set form factor used for the LO calculation in FF(double s12, double s34); default: VMD

#ifndef PI0DDALITZ_H
#define PI0DDALITZ_H

#include <complex>
#include <vector>
#include <tuple>

#include "masses.hh"
#include "TLorentzVector.h"

/// provides basic macros
#define MIN(a,b) ((a < b) ? a : b)
#define MAX(a,b) ((a < b) ? b : a)
#define SIGN(a) ((a > 0) ? 1 : ((a < 0) ? -1 : 0))
#define RE(x) std::real(x)

/// constants
const double PI = 3.14159265358979323846;
const double ALPHA = 1 / 137.035999;		// fine structure constant

const double MRHO = 0.770;					// rho meson mass in GeV
const double EC = 0.0004;					// soft-photon energy cut-off in the pi0 frame
const double LAMBDA = 1.;					// photon mass IR regulator (results should be independent on this value)
const double MUSQ = 1.;						// renormalization scale for ChPT part in 3p correction (results should be independent on this value)

// called from fortran routine (pi0decays.F)
extern "C" {
    void generate_pi0ddal_(double ppi0[4], int * pzmode);
}

// generates double Dalitz events distributed according to the matrix element squared (at LO or NLO), returns 4 four-momenta
void generate_ddalitz(TLorentzVector &pep1,
					  TLorentzVector &pem1,
					  TLorentzVector &pep2,
					  TLorentzVector &pem2,
					  bool radcor);
// converts random number within (0,1) to a specified range
double ConvertRandomToRange(double random, double random_min, double random_max);

/// following funtions provide real parts of logarithm and dilogarithm
double log_re(std::complex<double> z);
double log2_re(std::complex<double> z);
double dilog_re(double z);
// Kallen triangle function
double KallenLambda(double a, double b, double c);

// Double Dalitz class contains all the expressions for LO and NLO numerics
class FDoubleDalitz
{
public:
    FDoubleDalitz() : lmbd12(0), lmbd34(0), lmbd(0), z(0), Xi(0), s13(0), s14(0), s23(0), s24(0), s134(0), s234(0), s123(0), s124(0) {}
    ~FDoubleDalitz() {}

	void GenerateEventKinematics(double M, double ma, double mb,
						double &s12, double &s34, double &y12, double &y34, double &phi,
						double &s14, double &s23);
	void GenerateFourMomenta(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi,
								TLorentzVector &p1, TLorentzVector &p2, TLorentzVector &p3, TLorentzVector &p4);
	bool ConvertToKinematics(TLorentzVector &p1, TLorentzVector &p2, TLorentzVector &p3, TLorentzVector &p4,
								double M, double ma, double mb,
								double &s12, double &s34, double &y12, double &y34, double &phi);
	double LO(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi);
	double delta_NLO(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double EC, double LAMBDA, double MUSQ);

private:
	double lmbd12, lmbd34, lmbd, z, Xi;
	double s13, s14, s23, s24;				// same name, different meaning compared to kinematical s14 and s23
	double s134, s234, s123, s124;

	void SetBasicVariables(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi);
	void SetSVariables(double M, double ma, double mb, double s12, double s34, double y12, double y34);

	// phase-space
	double DPhase(int S, double M, double s12, double s34);

	// calculates exchange variables
	void DirToExc(double M, double m,
		double s12, double s34, double y12, double y34, double phi,
		double &s14, double &s23, double &y14, double &y23, double &phiEx);

	/// leading order (LO)
	// form factor
	inline double FF(double s12, double s34)
	{
		double MV = MRHO;
//		return 1.; // constant FF
		return pow(MV, 2) / (pow(MV, 2) - s12)*pow(MV, 2) / (pow(MV, 2) - s34);	// VMD
	}
	double Direct(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi);
	std::tuple<double, double> Identical(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double s14, double s23);

	/// Bremsstrahlung
	double Bremsstrahlung(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double EC, double LAMBDA);

	/// VP and vertex
	double VPol_Re(double s);
	double F1_Re(double s, double m, double LAMBDA);
	double DirF2_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi);
	double IntF2_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double s14, double s23);

	double FVPol_Re(double s, double m);
	double SVPol_Re(double s, double m);
	double F2_Re(double beta);

	/// 3p
	double ThreePointD_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double chi, double MUSQ);
	double ThreePointINT_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double chi, double MUSQ);

	double ThreePoint1D_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double chi, double MUSQ);
	double ThreePointINT_firstTwo_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double chi, double MUSQ);

	/// 4p
	double FourPointD_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi);
	double FourPointInt_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi);

	double FourPoint1D_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi);
	double FourPoint1Int_Re(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi);

	/// 5p
	double Pent1DT1(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double DIR, double M12, double M34);
	double Pent1IT1(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double INT, double M12, double M34);

	double PENTDTev(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34, double FF);
	double PENTITev(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double s14, double s32, double y14, double y32, double phiex, double LAMBDA, double M12, double M34, double FFD, double FFE);

	double PentDir(char c, double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34);
	double Pent1DDir(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34);
	double Pent1IDir(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34);

	double Pent2IDir(double M, double ma, double mb, double s12, double s34, double y12, double y34, double phi, double LAMBDA, double M12, double M34);

	/// useful functions
	inline double lmbd13_(double M, double ma, double mb, double y12, double y34, double lmbd, double z, double Xi)
	{ return 1. / 4 * sqrt(pow(pow(M, 2)*z*(1 + y12*y34) + (y12 + y34)*lmbd - pow(M, 2)*Xi, 2) - pow(8 * ma*mb, 2)); }

	inline double z13_(double ma, double mb, double lmbd13)
	{ return sqrt(pow(lmbd13, 2) + pow(2 * ma*mb, 2)); }

	inline double sigma13_(double m, double lmbd13, double z13)
	{ return 1 / (2 * pow(m, 2))*(lmbd13 + z13); }

	inline double s13_(double M, double s12, double s34, double y12, double y34, double lmbd12, double lmbd34, double lmbd, double z, double Xi)
	{ return (pow(M, 2) + pow(M, 2)*y12*y34*z - s12*pow(lmbd12, 2) - s34*pow(lmbd34, 2) + (y12 + y34)*lmbd - pow(M, 2)*Xi) / 4.; }

	inline double s134_(double M, double ma, double s12, double s34, double y12, double lmbd)
	{ return (pow(M, 2) + 2 * pow(ma, 2) - s12 + s34 + y12*lmbd) / 2.; }

	inline double delta1234_(double M, double lmbd, double s12, double s34, double y12)
	{ return -1 / (2 * pow(M, 2)) * (pow(M, 2) - s12 + s34 - y12 * lmbd); }

	inline double lambda1234_(double M, double lmbd, double s12, double s34, double y12, double lmbd12)
	{ return 1 / (2 * pow(M, 2)) * sqrt(pow(lmbd, 2)*(1 + pow(y12, 2)) + 2 * lmbd*y12*(pow(M, 2) + s12 - s34) + 4 * pow(M, 2)*s12*pow(lmbd12, 2)); }

	inline double IB11_(double d1, double l1, double EC, double LAMBDA)
	{ return 1 / (pow(2 * PI, 2))*(log(2 * EC / LAMBDA) - .5*d1 / l1*log_re((d1 + l1) / (d1 - l1))); }

	double IB12_(double M, double ma, double s12, double lmbd12, double sgm12, double O1P, double O1M, double O2P, double O2M, double U12, double EC, double LAMBDA)
	{
		return
			(1 - (2 * pow(ma, 2)) / s12) / (8.*pow(PI, 2)*lmbd12) * (
				log((2 * EC) / LAMBDA)*log_re((1 - (2 * pow(ma, 2)) / s12 + lmbd12) / (1 - (2 * pow(ma, 2)) / s12 - lmbd12))
				+ log2_re(O1M / O1P) / 4.
				- log2_re(O2M / O2P) / 4.
				+ dilog_re(1 - (pow(M, 2)*U12 * O1M) / (s12*lmbd12))
				+ dilog_re(1 - (pow(M, 2)*U12 * O1P) / (s12*lmbd12))
				- dilog_re(1 - (pow(M, 2)*U12 * O2M) / (s12*lmbd12*sgm12))
				- dilog_re(1 - (pow(M, 2)*U12 * O2P) / (s12*lmbd12*sgm12)));
	}

	double IB13_(double M, double lmbd13, double z13, double sgm13, double O1P, double O1M, double O3P, double O3M, double U13, double EC, double LAMBDA)
	{
		return
			z13 / (8.*pow(PI, 2)*lmbd13) *(
				log((2 * EC) / LAMBDA)*log_re((z13 + lmbd13) / (z13 - lmbd13))
				+ log2_re(O1M / O1P) / 4.
				- log2_re(O3M / O3P) / 4.
				+ dilog_re(1 - (pow(M, 2)*U13 * O1M) / lmbd13)
				+ dilog_re(1 - (pow(M, 2)*U13 * O1P) / lmbd13)
				- dilog_re(1 - (pow(M, 2)*U13 * O3M) / (lmbd13*sgm13))
				- dilog_re(1 - (pow(M, 2)*U13 * O3P) / (lmbd13*sgm13)));
	}

	double array_product(double *a1, double *a2, int N);
	std::vector<double> C_get(double p2, double q2, double pq2, double m12, double m22, double m32);
	std::vector<double> D_get(double p12, double p22, double p32, double p42, double q12, double q22, double m12, double m22, double m32, double m42);
};

#endif // PI0DDALITZ_H
