/*
 * CherenkovPhotonProd.cpp
 *
 *  Created on: 13.08.2010
 *      Author: imp
 */

#include "CherenkovPhotonProd.hh"
#include <cmath>
#include <iostream>
#include <TMath.h>

#include <iomanip>


CherenkovPhotonProd::CherenkovPhotonProd() {


	m_EPhotMin=2e-6;
	m_EPhotMax=6.6e-6;

	//Todo here we need the one for Neon !!!!!
	m_SpeedOfLight=299792458; // m/s

	f1 = new TF1("f1",this,&CherenkovPhotonProd::FrankTammEquationDummy,0,1,2,"CherenkovPhotonProd","FrankTammEquationDummy");

}

CherenkovPhotonProd::~CherenkovPhotonProd() {
	// TODO Auto-generated destructor stub
}

void CherenkovPhotonProd::produceCherenkovPhoton(const TVector3 &PartMom, const TVector3 &PartPos, double prodLength, double ParticeE ){
	clearPhotonVector();
	//at the moment only for testing
	TVector3 mom;
	//double PhotonE=5e-6;//MeV
	//TVector3 pos(0,0,0);//=PartPos
	double beta = PartMom.Mag()/ParticeE;

	//we loop over all photons which should be generated (the # of photons is poisson distributed)
	int test=getAmountOfPhotons(prodLength,beta);
	for(int i=0;i<test;i++){
		produceSinglePhoton(PartMom,PartPos, prodLength,ParticeE, beta);
	}

}

void CherenkovPhotonProd::produceSinglePhoton(const TVector3 &PartMom,const TVector3 &PartPos, double prodLength, double /*ParticeE*/, double beta){
	//double PhotonE=2e-6;//MeV //todo where to get photo energy
	double PhotonE= getPhotonEnergy(prodLength,beta);

	//First we calculate the cherencov angle with respect to the particle trajectory
	double CherAngle=determineCherencovAngle(beta,PhotonE);
	//and the uniform phi distribution with respect to the particle direction;
	double phi=determinePhiDirection();

	//since the coordinate system of the particle is not necessarily the same as the ref system of RICH
	//we have to rotate to the ref system of RICH
	//std::cout<<"Energy: "<<PhotonE<<std::endl;
	TVector3 PhotonMom;
	PhotonMom.SetPtThetaPhi(1,CherAngle,phi);//in ref system of particle
	//now we want to rotate back in ref system of RICH
	//std::cout<<"Ohne X "<<PhotonMom.X()<<" Y "<<PhotonMom.Y()<<" Z "<<PhotonMom.Z()<<std::endl;
	//PhotonMom.RotateZ(PartMom.Phi());
	//std::cout<<"Phi X "<<PhotonMom.X()<<" Y "<<PhotonMom.Y()<<" Z "<<PhotonMom.Z()<<std::endl;
	//std::cout<<"Theta: "<<PartMom.Theta()<<std::endl;
	//std::cout<<"Phi: "<<PartMom.Phi()<<std::endl;
	//PhotonMom.RotateY(PartMom.Theta());

	double thetaX=atan(PartMom.X()/PartMom.Z());
	double thetaY=atan(PartMom.Y()/PartMom.Z());

	PhotonMom.RotateY(thetaX);
	PhotonMom.RotateX(-thetaY);

	//PhotonMom.SetPtEtaPhi(PhotonE,PhotonMom.Theta(),PhotonMom.Phi());
	PhotonMom.SetMagThetaPhi(PhotonE,PhotonMom.Theta(),PhotonMom.Phi());


	//std::cout<<" Theta X "<<PhotonMom.X()<<" Y "<<PhotonMom.Y()<<" Z "<<PhotonMom.Z()<<std::endl;


	TVector3 PhotonProdPos=getProductionPos(PartMom,PartPos, prodLength);

	//time the particle traveled untill photon was emitted
	double time = std::abs((PhotonProdPos-PartPos).Mag())/m_SpeedOfLight;

	m_PhotonVector.push_back(Photon(PhotonMom,PhotonProdPos,time));


}


//returns a uniform phi direction
double CherenkovPhotonProd::determinePhiDirection(){
	return m_rand.Uniform(-M_PI,M_PI);
}
double CherenkovPhotonProd::determineCherencovAngle(double beta, double PhotonE){

	return std::acos(1./(getNeonRefractiveIndex(PhotonE)*beta));
	//return std::acos(1./(1.000062012*beta));//maybe we need to add here temperature  + pressure


}
void CherenkovPhotonProd::transformIntoRefFrame(){

}
std::vector<Photon> CherenkovPhotonProd::getPhotonVector(){
	return m_PhotonVector;
}

void CherenkovPhotonProd::clearPhotonVector(){
	m_PhotonVector.clear();
}

TVector3 CherenkovPhotonProd::getProductionPos(const TVector3 &PartMom,const TVector3 &PartPos, double prodLength){

	//after r meter in system of partice
	double r = m_rand.Uniform(0.,prodLength);

	double x=r*TMath::Sin(PartMom.Theta())*TMath::Cos(PartMom.Phi());
	double y=r*TMath::Sin(PartMom.Theta())*TMath::Sin(PartMom.Phi());
	double z=r*TMath::Cos(PartMom.Theta());
	return TVector3(PartPos.X()+x,PartPos.Y()+y,PartPos.Z()+z);

}

int CherenkovPhotonProd::getAmountOfPhotons(double length,double beta){



	//some constants where to get form ROOT
//	const double alpha = 0.0072973525;
//	const double hbarc = 197.33e-15;//MeV m

	//old constants they are for testing only
	//double eff_mirr=0.85;
	//double eff_geo=0.9;
	//double eff_coll=0.85;
	//double eff_transp=0.9;//is it %?
	//double eff_quant=0.8e-6;//MeV

	//std::cout<<"alpha/hbarc"<<alpha/hbarc<<std::endl;

	//calculate the average amount of photons
	//double Npe=alpha/hbarc*length*eff_mirr*eff_geo*eff_coll*eff_transp*eff_quant*TMath::Power(TMath::Sin(CherAngle),2.);
	//std::cout<<"N0"<<alpha/hbarc*eff_mirr*eff_geo*eff_coll*eff_transp*eff_quant<<std::endl;


	//now we need the integrate the Frank Tamm equation to determine the average number of photons

	//	TF1 * f1 = new TF1("f1",this,&CherenkovPhotonProd::FrankTammEquationDummy,0,1,2,"CherenkovPhotonProd","FrankTammEquationDummy");

	f1->SetParameter(0,length);
	f1->SetParameter(1,beta);

	double N_Photons=f1->Integral(m_EPhotMin,m_EPhotMax);
	//std::cout<<FrankTammEquation(5.5e-6, 18.,beta)<<std::endl;
	//std::cout<<"Average amount of PhotonsNpe: "<<N_Photons<<std::endl;
	//std::cout<<QuartzTransmision(energy)<<" "<<AlMgF2GlassReflectivity(energy)<<" "<<AlMgF2MylarReflectivity(energy)<<" "<<std::setprecision(10)<<PMQuantumResponseU03(energy)<<std::endl;

	//now we need to poison distribute the photons
	return (int) m_rand.Poisson(N_Photons);


	//return 0;

}

double CherenkovPhotonProd::QuartzTransmision(double E){//transparenz of Quartz window
	E=E*1e6;//to take the MEV into account

	double QuarzTrans=0.01 * (9.13392e+01 - 1.24518e+13 * exp(-1.68759e-01 * 1240./(E)) - 1e5 * exp(-4.8e-2 * 1240./(E)));
	if(QuarzTrans>0){
		return QuarzTrans;
	}
	return 0.;
}

double CherenkovPhotonProd::AlMgF2GlassReflectivity(double E){//PMT???? todo
	E=E*1e6;//to take MeV into account
	double GlassRef = 0.01 * (85. - 11500. * exp( -0.035 * 1240./(E)));
	if(GlassRef>0){
		return GlassRef;
	}
	return 0;
}

double CherenkovPhotonProd::AlMgF2MylarReflectivity(double E){//cone ref or mirror????? todo
	E=E*1e6;
	double MylareRef = 0.01 * (-143.897 + 3.73798 * 1240./(E)
	           - 0.0247806 * 1240./(E) * 1240./(E)
	             + 8.4999e-05 * pow(1240./(E),3)
	            - 1.5824e-07 * pow(1240./(E),4)
	             + 1.51945e-10 * pow(1240./(E),5)
	             - 5.90409e-14 * pow(1240./(E),6));
	if(MylareRef>0){
		return MylareRef;
	}
	return 0;
}

double CherenkovPhotonProd::PMQuantumResponseU03(double E){
	E=E*1e6;
	double PMQuantumResponse = -7408.01 + 184.084 * 1240./(E)
		 - 1.98272 * 1240./(E) * 1240./(E)
		    + 0.0121346   * pow(1240./(E),3)
			     - 4.64744e-05 * pow(1240./(E),4)
		        + 1.15586e-07 * pow(1240./(E),5)
		     - 1.86882e-10 * pow(1240./(E),6)
		        + 1.89628e-13 * pow(1240./(E),7)
		        - 1.09704e-16 * pow(1240./(E),8)
		       + 2.76026e-20 * pow(1240./(E),9);

/*	PMQuantumResponseU03[i] = (PMQuantumResponseU03[i] > 0 ? PMQuantumResponseU03[i] : 0.);
	PMQuantumResponseU03[i] = (1240./(fPhotonEnergy[i]/eV) > 182. && 1240./(fPhotonEnergy[i]/eV) < 650. ? PMQuantumResponseU03[i] : 0.);
	QuartzTransmission[i] += 2*NormalReflection*(1-NormalReflection);
*/
	if(PMQuantumResponse>0){
		return PMQuantumResponse/100.;
	}

	return 0;
}


double CherenkovPhotonProd::getNeonRefractiveIndex(double E, double Temperature, double Pressure){

	/*
	 * From G4
	 *
	 * public static final double 	STP_Pressure 	6.324206322405024E8d
	 * public static final double 	STP_Temperature 	273.15d
	 * public static final double 	bar 	6.241506363094027E8d
	 * public static final double 	s 	1.0E9d
	 */

//	double STP_Pressure=1.;
//	double STP_Temperature=20.;

	const double bar = 6.241506363094027e8;

	double STP_Pressure=6.324206322405024e8;
	double STP_Temperature=273.15;

	Temperature=STP_Temperature+Temperature;
	//Pressure=0.984*bar*1.012;
	Pressure=Pressure*bar;

	/*
	 * fNeonRefIndex[i]=1.+2.61303e27/(39160e27-
                                (fPhotonEnergy[i]/h_Planck)*
                                  (fPhotonEnergy[i]/h_Planck)*(s*s))*
      (fNeonPressure/(fNeonTemperature))/(STP_Pressure/(STP_Temperature));
	 *
	 */

	//double h_Planck=4.13567e-15; //eV*s
	double h_Planck=4.135669239559144E-12;
	double s = 1e9;
	return 1.+2.61303e27/(39160e27-(E/h_Planck)*(E/h_Planck)*(s*s))
		*(Pressure/(Temperature))/(STP_Pressure/(STP_Temperature));
}

double CherenkovPhotonProd::combinedEff(double E){
  return QuartzTransmision(E)*AlMgF2GlassReflectivity(E)*AlMgF2MylarReflectivity(E)*PMQuantumResponseU03(E); // never used!!!! see FrankTamm
 
}

Double_t CherenkovPhotonProd::FrankTammEquationDummy(Double_t *x, Double_t *par){
	return FrankTammEquation(x[0], par[0], par[1]);
}

double CherenkovPhotonProd::FrankTammEquation(double E, double length, double beta){
	//maybe we need to define pressure and temperature
	//for ref index of neon
	//some constants where to get form ROOT
	const double alpha = 0.0072973525;
	const double hbarc = 197.33e-15;//MeV m

	double a = M_PI*std::pow(18./2.,2);
	double b = M_PI*std::pow(8./2.,2);

	//	double eff_coll=(AlMgF2GlassReflectivity(E)*(a-b)+b)/a;  // maybe a bug????? should be Maylar reflect.
	double eff_coll=(AlMgF2MylarReflectivity(E)*(a-b)+b)/a;
	double eff_geom = 1;//0.9;//since otherwise double counted due to PMT Acc check

	//	return alpha/(hbarc)*length*QuartzTransmision(E)*AlMgF2GlassReflectivity(E)*eff_geom*eff_coll*PMQuantumResponseU03(E)*(1-1/(std::pow(beta*getNeonRefractiveIndex(E),2)));

  //rescaled by hand by Tonino from positron run PMQuantumResponseU03(E)*0.752884440336
	//	return alpha*length*QuartzTransmision(E)*AlMgF2GlassReflectivity(E)*eff_geom*eff_coll*PMQuantumResponseU03(E)*0.752884440336*(1-1/(std::pow(beta*getNeonRefractiveIndex(E),2)))/(hbarc);
	
       //rescaled by hand by Eva from positrons in Pollution runs PMQuantumResponseU03(E)*0.752884440336*0.965
		return alpha*length*QuartzTransmision(E)*AlMgF2GlassReflectivity(E)*eff_geom*eff_coll*PMQuantumResponseU03(E)*0.752884440336*0.965*(1-1/(std::pow(beta*getNeonRefractiveIndex(E),2)))/(hbarc);

	//return alpha/(hbarc)*QuartzTransmision(E)*AlMgF2MylarReflectivity(E)*eff_geom*eff_coll;
	//return alpha/(hbarc);

}

double CherenkovPhotonProd::getPhotonEnergy(double length, double beta){



	//	TF1 * f1 = new TF1("f1",this,&CherenkovPhotonProd::FrankTammEquationDummy,0,1,2,"CherenkovPhotonProd","FrankTammEquationDummy");

	f1->SetParameter(0,length);
	f1->SetParameter(1,beta);

	double Max=f1->GetMaximum(m_EPhotMin,m_EPhotMax); //(m_EPhotMin,m_EPhotMax);




	while(true){
		//choose random energy photon between e_max and e_min
        double ePho=m_rand.Uniform(m_EPhotMin,m_EPhotMax);
		//now we check the probability for this E
        double prob=m_rand.Uniform(0,Max/*+0.01*Max*/);//the +1% to be on the save side to be always over max
	//	std::cout<<"ePho "<<ePho<<"Max: "<<Max<<"  prob: "<<prob<<std::endl;
		//new we check if we are inside, we divide by N_photon to normalize distribution

	//	std::cout<<"tmp: "<<tmp<<std::endl;
		if(prob<=FrankTammEquation(ePho, length,beta)){
			//std::cout<<"Finished: "<<ePho<<std::endl;
			return ePho;
		}
		//std::cout<<"new"<<std::endl;
	}


	//Insise this function we are working in eV the result is retruned in MeV!

	//Discreet energy values for min bin 2 to max bin 6.6 : energy width = 0.2
	//integral is normalized to 1
/*	double StartBinEnergy = 2;
	double MaxEnergy=6.6;
	double BinWidth = 0.2;
	double DiscreetEnergy[24] = {0.00516737,0.0203015,0.0456684,0.0588278,0.0676128,0.0692618,0.0714131,0.0705971,0.0695256,0.0679994,0.06562,0.0623627,0.0572375,0.0500905,0.0432089,0.0375842,0.0329756,0.028774,0.0246282,0.0204145,0.0167891,0.0104362,0.00318334,0.000320253};
	int EnergyBin=0;
	double dist;
	double YesNo;
	double Extrapolation=0;
	double slope;//slope between one bin and the other for extrapolation
	while(true){
		dist= m_rand.Uniform(0,24);
	//	std::cout<<"Dist "<<dist<<std::endl;
		EnergyBin =(int) dist;
		if(EnergyBin==24){
	//		std::cout<<"Before Check "<<EnergyBin<<std::endl;
			continue;
		}
		//IMPOTRATNT number has to be higher as highest energy probability
		YesNo = m_rand.Uniform(0,0.08); //this value determines of we are using this energy or not

		Extrapolation=m_rand.Uniform(0,1);
		slope=(DiscreetEnergy[EnergyBin+1]-DiscreetEnergy[EnergyBin])/BinWidth;
	//	std::cout<<"Slope: "<<slope<<std::endl;
	//	std::cout<<"Threshold"<<DiscreetEnergy[EnergyBin]+slope*BinWidth*Extrapolation<<std::endl;
		//yes if we are smaller then value in bin no if we are bigger
		if(YesNo<=DiscreetEnergy[EnergyBin]+slope*BinWidth*Extrapolation){
	//		std::cout<<"Energy "<<StartBinEnergy+EnergyBin*BinWidth+BinWidth*Extrapolation<<" EnergyBin "<<EnergyBin<<" YesNo "<<YesNo<<std::endl;
			if(StartBinEnergy+EnergyBin*BinWidth+BinWidth*Extrapolation<MaxEnergy)
			return (StartBinEnergy+EnergyBin*BinWidth+BinWidth*Extrapolation)*1e-6;
		}
	}
*/
	return 0;

}
