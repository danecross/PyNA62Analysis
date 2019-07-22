#ifndef FITTER_HH
#define FITTER_HH
#include <stdlib.h>
#include <iostream>
#include <vector>
#include "KinFitConstraint.hh"
#include "KinFitChargedParticle.hh"
#include "KinFitNeutralParticle.hh"

using namespace std;

class Fitter{
	
	public:
	
	Fitter();
	~Fitter(); 
	Fitter(int meas, int unmeas, int param, int constr);															// specify number of measured particles, number of unmeasured particles, number of parameters for each particle and number of contraints 
	
	void AddMeasParticle(KinFitParticle *p);																		// add a measured particle to the fit
	void AddMeasParticle(KinFitParticle *k, KinFitParticle *pi, KinFitParticle *gamma1, KinFitParticle *gamma2);	// add all measured particle to the fit (to test the algorithm)
	void AddMeasParticle(KinFitParticle *k, KinFitParticle *pi, KinFitParticle *gamma1);							// add all measured particle to the fit (to study LKr and LAV efficiency)
	void AddUnmeasParticle(KinFitParticle *p);																		// add an unmeasured particle to the fit
	void AddConstraint(KinFitConstraint *c);																		// add a constraint to the fit
	
	void Initialize();  																							// set the variables to start the fit
	void SetNMaxIterations(int n);																					// set maximum number of iterations
	void SetConvergeThr(double c);																					// set convergence threshold
	void Fit();                     	                         												    // Perform the fit (see P. Avery for formulas)
	double SetChiSquare(TMatrixD Lambda, TMatrixD d); 																// compute ChiSquare
	double GetChiSquare(){return ChiSquare;} 																
	void SetPValue(){PValue = TMath::Prob(ChiSquare,DoF);}
	double GetPValue(){return PValue;}
	// set fitter matrices
	void SetAlpha0(vector<KinFitParticle*> Meas, vector<KinFitParticle*> Unmeas);
	void SetAlpha0(TMatrixD A);																						// set Alpha0 vector
	void SetX0(TMatrixD A);																							// set X0 vector
	void SetConstrMatrix();
	void SetCovarianceMatrix();
	void SetCovarianceMatrix(TMatrixD A);
	// getter and setter for fit variables
	void SetNPartParams(int p){ NPartParams = p; }
	int GetNPartParams(){return NPartParams;}
	void SetNParam(){ NParam = (NMeasPart+NUnmeasPart)*NPARTPARAMS;}
	int GetNParam(){return NParam;}
	void SetNMeasPart(){NMeasPart = MeasPart.size();}
	void SetNUnmeasPart(){NUnmeasPart = UnmeasPart.size();}
	void SetNConstraints();
	void SetDoF();
	double GetDoF(){return DoF;}
	double GetOutputState(){return OutputState;}
	void SetVerbosity(int v){Verbosity = v;}
	
	void GetFitParameters(); 																						// get fit parameters and their covariance matrix
	void FreeMemory();																								// free memory at the end of the fit
	
	
	protected:
	
	private:
	
	// fit variables
	std::vector<KinFitParticle*> MeasPart;
	std::vector<KinFitParticle*> UnmeasPart;
	std::vector<KinFitConstraint*> Constraints;
	
	int NPartParams=7;																								// number of parameters for each particle (default 7)
	int NMeasPart=0;																								// number of measured particles
	int NUnmeasPart=0;																								// number of unmeasured particles
	int NParam=0;																									// number of parameters ( NMeasPart + NUnmeasPart )*NPartParams
	int NConstraints=0;																								// number of constraints
	int NEquations=0;																								// number of equations
	int DoF=0; 																										// degrees of freedom of kinematic fit
	int NMaxIterations;																								// maximum number of iterations
	double ConvergenceThr; 																							// convergence threshold ( if |DeltaChiSquare|<ConvergenceThr fit is converged )
	double DeltaChiSquare; 																							// difference of chisquare between two consecutive iterations
	double PValue; 																									// ChiSquare Probability
	double ChiSquare;																								// ChiSquare of the fit (iteration n° i)
	double PrevChiSquare;																							// previous ChiSquare (iteration n° i-1)
	int Verbosity=0;																								// verbosity level: 0 nothing
																													// 					1 fit status
																													//					2 all
																													
	int OutputState=999;																												
	// internal variables (P.Avery notation)
	TMatrixD *Alpha0;
	TMatrixD *Alpha;
	TMatrixD *d;
	TMatrixD *D;
	
	TMatrixDSym *InitVAlpha0;
	TMatrixD *VAlpha0;
	TMatrixD *VAlpha;
	
};

#endif
