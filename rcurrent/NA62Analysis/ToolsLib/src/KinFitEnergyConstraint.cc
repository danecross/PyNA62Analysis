// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-03
// last update: 2017-02-22
//
// --------------------------------------------------------------------
/// \class KinFitEnergyConstraint
/// \Brief
/// Class for energy constraints in NA62KinFit
/// \EndBrief
/// \Detailed
/// This class is used in NA62KinFit to constraint energy in Kaon decays.
/// To use the constraint you need to specify the decay considered passing a string (supported decays are "K2Pi" and "K3Pi")
///
/// An example of use is given below.
/// \code
/// #include "KinFitEnergyConstraint.hh"
/// ...
/// KinFitEnergyConstraint EnConstr(K, pi, gamma1, gamma2);
/// EnConstr.SetDecay("K2Pi");
/// ...
/// Fitter Fit;
/// ...
/// Fit.AddConstraint(EnConstr);
/// ....
/// \endcode
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed
#include "KinFitEnergyConstraint.hh"

KinFitEnergyConstraint::KinFitEnergyConstraint(){
}

KinFitEnergyConstraint::~KinFitEnergyConstraint(){
	delete Derivative;
	delete dVector;

}

/// Constructor for three-body decay:  A->B+C+D
KinFitEnergyConstraint::KinFitEnergyConstraint(const KinFitParticle &K, const KinFitParticle &Pi, const KinFitParticle &G1, const KinFitParticle &G2){
	NEq = 1;
	NParticles = 4;
	SetDerivative(K, Pi, G1, G2);
	SetdVector(K, Pi, G1, G2);
}

/// Generic constructor for n-body A->b1+b2+.....+bn
KinFitEnergyConstraint::KinFitEnergyConstraint(std::vector<KinFitParticle> particles){
	NEq = 1;
	NParticles = particles.size();
	Derivative = new TMatrixD(NEq, NParticles*NPARTPARAMS);
	dVector = new TMatrixD(NEq,1);
	for(int i=0; i<3; i++) (*Derivative)(0,3+i) = particles.at(0).InitParameters.at(3+i)/particles.at(0).GetEnergy();
	(*dVector)(0,0) = particles.at(0).GetEnergy();

	for(int ipart=1; ipart<NParticles; ipart++){
		(*Derivative)(0,3+ipart*NPARTPARAMS) = -particles.at(ipart).InitParameters.at(3)/particles.at(ipart).GetEnergy();
		(*Derivative)(0,4+ipart*NPARTPARAMS) = -particles.at(ipart).InitParameters.at(4)/particles.at(ipart).GetEnergy();
		(*Derivative)(0,5+ipart*NPARTPARAMS) = -particles.at(ipart).InitParameters.at(5)/particles.at(ipart).GetEnergy();
		(*dVector)(0,0) = -particles.at(ipart).GetEnergy();

	}
}

void KinFitEnergyConstraint::SetMasses(){
	if(fDecayName=="K2Pi"){
		fMasses[0]=MKPLUS;
		fMasses[1]=MPIPLUS;
		fMasses[2]=0.;
		fMasses[3]=0.;
	}
	else if(fDecayName=="K2PiGamma"){
		fMasses[0]=MKPLUS;
		fMasses[1]=MPIPLUS;
		fMasses[2]=0.;
		fMasses[3]=0.;
		fMasses[4]=0.;
	}
	else if(fDecayName=="K3Pi"){
		fMasses[0]=MKPLUS;
		fMasses[1]=MPIPLUS;
		fMasses[2]=MPIPLUS;
		fMasses[3]=MPIPLUS;
	}
	else 	cout<<endl<<"[KinFitEnergyConstraint] Not valid decay, constraint not usable!"<<endl;

}

void KinFitEnergyConstraint::SetDecay(TString s){
	fDecayName = s;
	SetMasses();
}

void KinFitEnergyConstraint::SetDerivative(const KinFitParticle &A, const KinFitParticle &B, const KinFitParticle &C, const KinFitParticle &D){
	Derivative = new TMatrixD(NEq,NParticles*NPARTPARAMS);
	double Ea = A.GetEnergy();
	double Eb = B.GetEnergy();
	double Ec = C.GetEnergy();
	double Ed = D.GetEnergy();


	(*Derivative)(0,3) = A.InitParameters.at(3)/Ea;
	(*Derivative)(0,4) = A.InitParameters.at(4)/Ea;
	(*Derivative)(0,5) = A.InitParameters.at(5)/Ea;

	(*Derivative)(0,9) = -B.InitParameters.at(3)/Eb;
	(*Derivative)(0,10) = -B.InitParameters.at(4)/Eb;
	(*Derivative)(0,11) = -B.InitParameters.at(5)/Eb;

	(*Derivative)(0,15) = -C.InitParameters.at(3)/Ec;
	(*Derivative)(0,16) = -C.InitParameters.at(4)/Ec;
	(*Derivative)(0,17) = -C.InitParameters.at(5)/Ec;

	(*Derivative)(0,21) = -D.InitParameters.at(3)/Ed;
	(*Derivative)(0,22) = -D.InitParameters.at(4)/Ed;
	(*Derivative)(0,23) = -D.InitParameters.at(5)/Ed;

}



void KinFitEnergyConstraint::SetdVector(const KinFitParticle &K, const KinFitParticle &Pi, const KinFitParticle &G1, const KinFitParticle &G2){
	dVector = new TMatrixD(NEq,1);
	double Ek = K.GetEnergy();
	double Epi = Pi.GetEnergy();
	double Eg1 = G1.GetEnergy();
	double Eg2 = G2.GetEnergy();

	(*dVector)(0,0) = Ek-Epi-Eg1-Eg2;

}

void KinFitEnergyConstraint::SetDerivative(TMatrixD *Alpha0){
	Derivative = new TMatrixD(NEq,NParticles*NPARTPARAMS);
	int KaonIndex;
	for(int i=0; i< NParticles; i++){
		double tempEnergy = (*Alpha0)(3+(i*NPARTPARAMS),0)*(*Alpha0)(3+(i*NPARTPARAMS),0);
		tempEnergy += (*Alpha0)(4+(i*NPARTPARAMS),0)*(*Alpha0)(4+(i*NPARTPARAMS),0);
		tempEnergy += (*Alpha0)(5+(i*NPARTPARAMS),0)*(*Alpha0)(5+(i*NPARTPARAMS),0);
		tempEnergy += fMasses[i]*fMasses[i];
		tempEnergy = TMath::Sqrt(tempEnergy);
		if (i==0) KaonIndex =1;
		else KaonIndex =-1;
		(*Derivative)(0,3+(i*NPARTPARAMS)) = KaonIndex*(*Alpha0)(3+(i*NPARTPARAMS),0)/tempEnergy;
		(*Derivative)(0,4+(i*NPARTPARAMS)) = KaonIndex*(*Alpha0)(4+(i*NPARTPARAMS),0)/tempEnergy;
		(*Derivative)(0,5+(i*NPARTPARAMS)) = KaonIndex*(*Alpha0)(5+(i*NPARTPARAMS),0)/tempEnergy;

	}

}

void KinFitEnergyConstraint::SetdVector(TMatrixD *Alpha0){
	dVector = new TMatrixD(NEq,1);
	for(int i=0; i< NParticles; i++){
		double tempEnergy = (*Alpha0)(3+(i*NPARTPARAMS),0)*(*Alpha0)(3+(i*NPARTPARAMS),0);
		tempEnergy += (*Alpha0)(4+(i*NPARTPARAMS),0)*(*Alpha0)(4+(i*NPARTPARAMS),0);
		tempEnergy += (*Alpha0)(5+(i*NPARTPARAMS),0)*(*Alpha0)(5+(i*NPARTPARAMS),0);
		tempEnergy += fMasses[i]*fMasses[i];
		tempEnergy = TMath::Sqrt(tempEnergy);

		if (i==0) (*dVector)(0,0) = tempEnergy;
		else (*dVector)(0,0) -= tempEnergy;

	}
}



