// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-03
//
// --------------------------------------------------------------------
/// \class Fitter
/// \Brief
/// Main class for NA62KinFit 
/// \EndBrief
/// \Detailed
///	This class is used to perform the kinematic fit.
/// Each particle is represented by a six-dimensional vector alpha_i with its covariance matrix.
/// In this class, all alpha_i vectors are put together to obtain a total parameters vector alpha and a total covariance matrix.
/// The initial value of alpha is labelled as alpha0.
/// Linearized constraints equations are put together as well to obtain a set of linearized equations expanded around alpha0.
/// Minimization is performed using the Lagrange multipliers method in a iterative way:
/// when the difference between the chi2 is below a certain threshold (set by the user), the process is stopped and the fitted values are extracted.
/// If not, the value of alpha obtained by the fit become the new alpha0 and the constraints equations are linearized around this new value. 
/// The maximum number of iterations must be set by the user in his analyzer (see the example below).
/// You can use the tool with your own vector of KinFitParticle objects and your KinFitConstraints objects without modifying this class.
/// For details about the algorithm I used see				
/// my talk at Pinunu meeting, March 2018 and			
///	 P. Avery, “Applied Fitting Theory VI: 					 
///  Formulas for Kinematic Fitting”, CLEO CBX 98-37 		 
///  http://www.phys.ufl.edu/~avery/fitting/kinematic.ps.gz 
/// 
/// An example of use is given below.
/// \code
/// #include "Fitter.hh"
/// ...
/// Fitter Fit;
/// Fit.AddMeasParticle(MeasPart);
/// ...
/// Fit.AddUnmeasParticle(UnmeasPart);
/// ...
/// Fit.AddConstraint(Constr);
/// ...
/// Fit.SetNMaxIterations(30);
/// Fit.SetConvergenceThr(0.01);
/// Fit.SetVerbosity(0)
/// Fit.Fit();
/// double pvalue = Fit.GetPValue();
/// ....
/// \endcode
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed
#include <Fitter.hh>


using namespace std;

/// base constructor, inizialize variables
Fitter::Fitter() :
	NMeasPart     (0),
	NUnmeasPart   (0),
	NConstraints  (0),
	NMaxIterations(0),
	ConvergenceThr(0),
	DeltaChiSquare(0),
	PValue        (0),
	ChiSquare     (0),
	PrevChiSquare (1e15),
	Alpha0        (nullptr),
	Alpha         (nullptr),
	d             (nullptr),
	D             (nullptr),
	InitVAlpha0   (nullptr),
	VAlpha0       (nullptr),
	VAlpha        (nullptr)
{
}

/// base destructor
Fitter::~Fitter(){
}

/// add a measured (charged or neutral) particle 
void Fitter::AddMeasParticle(KinFitParticle *p){
	MeasPart.push_back(p);
	NMeasPart++;
	
}

/// add 4 measured particles (useful for three-body decays)
void Fitter::AddMeasParticle(KinFitParticle *k, KinFitParticle *pi, KinFitParticle *gamma1, KinFitParticle *gamma2){
	MeasPart.push_back(k);
	NMeasPart++;
	MeasPart.push_back(pi);
	NMeasPart++;
	MeasPart.push_back(gamma1);
	NMeasPart++;
	MeasPart.push_back(gamma2);
	NMeasPart++;
	 
	Alpha0 = new TMatrixD(NMeasPart*NPARTPARAMS+NUnmeasPart*NPARTPARAMS,1);
	SetAlpha0(MeasPart, UnmeasPart);
}

/// add 3 measured particles (useful for single photon efficiency studies with 3 measured particles and one unmeasured photon)
void Fitter::AddMeasParticle(KinFitParticle *k, KinFitParticle *pi, KinFitParticle *gamma1){
	MeasPart.push_back(k);
	NMeasPart++;
	MeasPart.push_back(pi);
	NMeasPart++;
	MeasPart.push_back(gamma1);
	NMeasPart++;
}

/// add one unmeasured particle
void Fitter::AddUnmeasParticle(KinFitParticle *p){
	UnmeasPart.push_back(p);
	NUnmeasPart++;
	
}
/// add constraint to the fit
void Fitter::AddConstraint(KinFitConstraint *c){
	Constraints.push_back(c);
	NConstraints++;	
	
	
}
/// setting the number of constraints and the dimension of matrices D and d
void Fitter::SetNConstraints(){
	NConstraints = Constraints.size();
	for(unsigned int i=0; i<Constraints.size(); i++) NEquations+= Constraints.at(i)->GetNEq();
	D = new TMatrixD(NEquations,GetNParam());
	d = new TMatrixD(NEquations,1);
}

/// initialize vectors alpha, alpha0 and all the matrices used to perform the fit
void Fitter::Initialize(){
	SetNMeasPart();
	if(Verbosity > 1) cout<<"NMeasPart: "<<NMeasPart<<endl;
	SetNUnmeasPart();
	if(Verbosity > 1) cout<<"NUnmeasPart: "<<NUnmeasPart<<endl;
	SetNParam();
	if(Verbosity > 1) cout<<"NParam: "<<NParam<<endl;
	SetNConstraints();
	if(Verbosity > 1) cout<<"NConstraints: "<<NConstraints<<endl;
	SetDoF();
	if(Verbosity > 1) cout<<"NDoF: "<<DoF<<endl;
	if(Verbosity > 1) cout<<"Setting alpha 0 "<<endl;
	SetAlpha0(MeasPart,UnmeasPart);
	Alpha = new TMatrixD(GetNParam(),1);
	
	if(Verbosity > 1) cout<<"Setting constraints matrix"<<endl;
	SetConstrMatrix();
	if(Verbosity > 1) cout<<"Setting covariance matrix"<<endl;
	SetCovarianceMatrix();
} 	
			
/// Set the maximum number of iterations to achieve fit convergence				
void Fitter::SetNMaxIterations(int n){
	NMaxIterations = n;
}		

/// Set the convergence threshold, i.e. the maximum difference between the chi2 in two iterations to achieve fit convergence
void Fitter::SetConvergeThr(double c){
	ConvergenceThr = c;
}

/// Main class, a loop over the iterations is done and the chi2 is computed.
/// If deltaChi2 < ConvergenceThr, chi2, pvalue, parameters and covariance matrix are stored.
/// If not, the procedure is repeated with new values for alpha0 and VAlpha0.
///
/// verbosity level:
/// 0 (default) nothing printed
/// 1: only information about chi2 and deltachi2 for every iteration
/// 2: info about Lagrange Multiplier
/// >2: all matrices

void Fitter::Fit(){		
	if(Verbosity > 0) cout<<"Status: initializing:------------------"<<endl;
	Initialize();
	TMatrixD InitialAlpha0 = (*Alpha0);
	TMatrixD W(TMatrixD::kInverted, (*VAlpha0));
	TMatrixD InitialD = (*D);
	TMatrixD Initiald = (*d);
	VAlpha = new TMatrixD(GetNParam(),GetNParam());
	if(Verbosity > 0) cout<<"Status: starting:------------------"<<endl;
	for (int Iter=0; Iter<NMaxIterations; Iter++){
		if (Verbosity > 2){
			cout<<"Alpha0------------------------"<<endl;
			Alpha0->Print();
			cout<<"VAlpha0------------------------"<<endl;
			VAlpha0->Print();
			cout<<"------------------------------------------"<<endl;
			cout<<"Constraint matrix D------------------------------------------"<<endl;
			D->Print();
			cout<<"Constraint vector d------------------------------------------"<<endl;
			d->Print();
		}
		
		TMatrixD DT(TMatrixD::kTransposed, (*D) );
		TMatrixD VAlpha0DT( (*VAlpha0), TMatrixD::kMult, DT );
		TMatrixD DVAlpha0DT((*D), TMatrixD::kMult, VAlpha0DT);
		TMatrixD VD(TMatrixD::kInverted, DVAlpha0DT);
		
		TMatrixD Lambda(VD, TMatrixD::kMult, (*d));
		if (Verbosity > 1 ) {
			cout<<"Lagrange multipliers: ------------------------------------"<<endl;
			Lambda.Print();
		}
		TMatrixD LambdaT(TMatrixD::kTransposed, Lambda );
		TMatrixD DVAlpha0DTLambda(DVAlpha0DT, TMatrixD::kMult, Lambda );
		TMatrixD ChiSquareMat(LambdaT, TMatrixD::kMult, (*d) );
		if (Verbosity > 1 ) {
			cout<<"LambdaT*d: ------------------------------------"<<endl;
			ChiSquareMat.Print();
		}
		// fit parameters
		TMatrixD VAlpha0DTLambda(VAlpha0DT, TMatrixD::kMult, Lambda);
		(*Alpha) = (*Alpha0) - VAlpha0DTLambda;
		TMatrixD A( (*D), TMatrixD::kMult, (*VAlpha0) );
		TMatrixD AT(TMatrixD::kTransposed, A);
		// fit covariance matrix
		TMatrixD Unit(GetNParam(), GetNParam());
		Unit.UnitMatrix();
		TMatrixD VDD(VD, TMatrixD::kMult, (*D));
		TMatrixD ATVDD(AT, TMatrixD::kMult, VDD);
		TMatrixD VDA(VD, TMatrixD::kMult, A);
		TMatrixD ATVDA(AT, TMatrixD::kMult, VDA);
		(*VAlpha) = (*VAlpha0) - ATVDA;		
		// iterative "chi square", only to verify convergence
		ChiSquare = ChiSquareMat(0,0);
		if(Verbosity > 0 ) cout<<"Iteration: "<<Iter<<" Debug: Chi2="<<ChiSquare<<" prevChi2="<<PrevChiSquare<<endl;	
		DeltaChiSquare = TMath::Abs(ChiSquare-PrevChiSquare);
		TMatrixD DeltaAlpha = (*Alpha)-InitialAlpha0;
		TMatrixD WDeltaAlpha = TMatrixD(W, TMatrixD::kMult, DeltaAlpha);
		TMatrixD DeltaAlphaTWDeltaAlpha = TMatrixD(DeltaAlpha, TMatrixD::kTransposeMult, WDeltaAlpha);
		TMatrixD DDeltaAlpha(InitialD, TMatrixD::kMult, DeltaAlpha);
		TMatrixD DDeltaAlphaPlusd = DDeltaAlpha + Initiald;
		TMatrixD ChiSquareConstr(LambdaT, TMatrixD::kMult, DDeltaAlphaPlusd);
		// real fit chi square
		double ChiSquareTot = DeltaAlphaTWDeltaAlpha(0,0)+2*ChiSquareConstr(0,0);
		
		if(DeltaChiSquare < ConvergenceThr ) {
			if (Verbosity > 0) cout<<"Status: converged!"<<endl;
			OutputState = 0;
			ChiSquare = ChiSquareTot;
			SetPValue();
			if (Verbosity > 0) cout<<"ChiSquare = "<<ChiSquare<<" Degrees of freedom: "<<GetDoF()<<" PValue = "<<GetPValue()<<endl;
			GetFitParameters();
			FreeMemory();
			return;
		}
		else {
			if (Verbosity > 1) cout<<"Status: running ...... iter n°"<<Iter<<endl;
			PrevChiSquare = ChiSquare;
			SetAlpha0(*Alpha);
			SetCovarianceMatrix(*VAlpha);
			// free memory before allocating it again
			SetConstrMatrix();
			
		}		
	}
	if (Verbosity > 0) cout<<"Status: Max Iter Reached! Aborting"<<endl;
	ChiSquare = 999;
	OutputState = 1;
	FreeMemory();
	return;
}

/// Compute chi2 from matrix
double Fitter::SetChiSquare(TMatrixD Lambda, TMatrixD mat){
	TMatrixD tempChiSquare(Lambda, TMatrixD::kTransposeMult, mat);
	return tempChiSquare(0,0);
}

/// Set the initial parameters vector
void Fitter::SetAlpha0(vector<KinFitParticle*> Meas, vector<KinFitParticle*> Unmeas){
	Alpha0 = new TMatrixD(NMeasPart*NPARTPARAMS+NUnmeasPart*NPARTPARAMS,1);
	for(unsigned int iMeasPart=0; iMeasPart<Meas.size(); iMeasPart++){
		for(int iParam=0; iParam<NPARTPARAMS; iParam++){
			(*Alpha0)(iParam+NPARTPARAMS*iMeasPart,0) = Meas.at(iMeasPart)->InitParameters.at(iParam);
			
		}
	}
	int index = Meas.size();
	index = index*NPARTPARAMS;
	
	for(unsigned int iUnmeasPart=0; iUnmeasPart<Unmeas.size(); iUnmeasPart++){
		for(int iParam=0; iParam<NPARTPARAMS; iParam++){
			(*Alpha0)(iParam+index,0) = Unmeas.at(iUnmeasPart)->InitParameters.at(iParam);
		}
	}
	
}

/// Set the parameters vector for next iteration
void Fitter::SetAlpha0(TMatrixD fMatrix){
	for(int i=0; i<GetNParam();i++){
		(*Alpha0)(i,0) = fMatrix(i,0);
	}
}

	
/// set constraints matrices D and d
void Fitter::SetConstrMatrix(){
	TMatrixD TotalConstrMatrix(NEquations,GetNParam());
	TMatrixD TotaldVector(NEquations,1);
	Int_t iRow = 0;	
	for (unsigned int iConstr =0; iConstr<Constraints.size(); iConstr++){
		
		Constraints.at(iConstr)->SetDerivative(Alpha0);
		Constraints.at(iConstr)->SetdVector(Alpha0);
		TMatrixD tempD = (*Constraints.at(iConstr)->GetDerivative());
		
		TMatrixD tempd = (*Constraints.at(iConstr)->GetdVector());
		
		int NRows = Constraints.at(iConstr)->GetNEq();
		for(int row=0; row<tempD.GetNrows(); row++){	
			for(int col=0; col<tempD.GetNcols(); col++){
				TotalConstrMatrix(row+iRow,col) = tempD(row,col);
			}			
		}
		
		for(int row=0; row<tempd.GetNrows(); row++){	
			for(int col=0; col<tempd.GetNcols(); col++){
				
				TotaldVector(row+iRow,col) = tempd(row,col);
			}
		}
		iRow += NRows;
	}
	*D = TotalConstrMatrix;
	*d = TotaldVector;
}

/// Set full covariance matrix (first iteration)
void Fitter::SetCovarianceMatrix(){
	
	TMatrixD TotalCovMatrix(GetNParam(),GetNParam());
	VAlpha0 = new TMatrixD(GetNParam(),GetNParam());
	for (unsigned int iMeas =0; iMeas<MeasPart.size(); iMeas++){
		
		TMatrixD tempD = (*MeasPart.at(iMeas)->GetCovMatrix());
		for(int row=0; row<tempD.GetNrows(); row++){	
			for(int col=0; col<tempD.GetNcols(); col++){
				TotalCovMatrix(row+NPARTPARAMS*iMeas,col+NPARTPARAMS*iMeas) = tempD(row,col);
				
			}
		}
	}
	
	for(unsigned int iUnmeas=0; iUnmeas<UnmeasPart.size(); iUnmeas++){
		TMatrixD tempD = (*UnmeasPart.at(iUnmeas)->GetCovMatrix());
		for(int row=0; row<tempD.GetNrows(); row++){	
			for(int col=0; col<tempD.GetNcols(); col++){
				TotalCovMatrix(row+NPARTPARAMS*MeasPart.size(),col+NPARTPARAMS*MeasPart.size()) = tempD(row,col);
				
			}
		}
	}
	
	*VAlpha0 = TotalCovMatrix;
	
}

/// Set full covariance matrix (next iterations)
void Fitter::SetCovarianceMatrix(TMatrixD fMatrix){
	for(int i=0; i<fMatrix.GetNrows(); i++){
		for(int j=0; j<fMatrix.GetNcols(); j++) (*VAlpha0)(i,j) = fMatrix(i,j);
	}
	
	
}

/// Once the fit converged, get the fitted values of the parameters and the fit covariance matrix
void Fitter::GetFitParameters(){
	if (Verbosity > 1 ){
		cout<<"Fit Parameters: "<<endl;
		Alpha->Print();
		cout<<"Fit Covariance matrix:"<<endl;
		VAlpha->Print();
	}
	for(unsigned int iMeasPart=0; iMeasPart<MeasPart.size(); iMeasPart++){
		for(int iParam=0; iParam<NPARTPARAMS; iParam++){
			MeasPart.at(iMeasPart)->FitParameters.push_back((*Alpha)(iParam+NPARTPARAMS*iMeasPart,0));
			for(int col=0; col<NPARTPARAMS; col++){
				(*MeasPart.at(iMeasPart)->FitCovMatrix)(iParam,col) = (*VAlpha)(iParam+NPARTPARAMS*iMeasPart,col+NPARTPARAMS*iMeasPart);
				
			}
		}
		
	}
	
	for(unsigned int iUnmeasPart=0; iUnmeasPart<UnmeasPart.size(); iUnmeasPart++){
		for(int iParam=0; iParam<NPARTPARAMS; iParam++){
			UnmeasPart.at(iUnmeasPart)->FitParameters.push_back((*Alpha)(iParam+MeasPart.size()*NPARTPARAMS+iUnmeasPart*NPARTPARAMS,0));
			for(int col=0; col<NPARTPARAMS; col++){
				(*UnmeasPart.at(iUnmeasPart)->FitCovMatrix)(iParam,col) = (*VAlpha)(iParam+MeasPart.size()*NPARTPARAMS+iUnmeasPart*NPARTPARAMS,col+MeasPart.size()*NPARTPARAMS+NPARTPARAMS*iUnmeasPart);
			}
		}
		
	}
	
	
}

/// Free memory used by this class
void Fitter::FreeMemory(){
	delete D;
	delete d;
	delete Alpha;
	delete Alpha0;
	delete VAlpha;
	delete VAlpha0;
}

/// Set fit degrees of freedom
void Fitter::SetDoF(){
	DoF = 0;
	for(unsigned int i=0; i<Constraints.size(); i++){
		DoF += Constraints.at(i)->GetNEq();
	}
		DoF -= NPARTPARAMS*UnmeasPart.size();
}
