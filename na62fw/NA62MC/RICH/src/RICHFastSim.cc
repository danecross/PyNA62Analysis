/*
 * RICHFastSim.cpp
 *
 *  Created on: 14.07.2010
 *      Author: imp
 */

#include "RICHFastSim.hh"
#include <cmath>
#include <TMath.h>
#include <iostream>
#include <TGraph.h>
#include <TCanvas.h>
#include <TLine.h>
#include <TGaxis.h>

RICHFastSim::RICHFastSim() :
	m_MirrorX0(0),
	m_MirrorY0(0),
	m_MirrorZ0(-17),
	m_MirrorR(34),//17.07;//2*17.07),
	m_PMTPos(0),
	m_Diameter(0.),
	m_debug(false),
	m_TravelTime(0),
	//Todo here we need the one for Neon !!!!!
	m_SpeedOfLight(299792458) // m/s
{}

RICHFastSim::~RICHFastSim() {
	// TODO Auto-generated destructor stub
}


TVector3 RICHFastSim::getPhotonPosAtPMT(double xe, double ye, double ze, double theta, double phi, double PhotonEnergy){
  
	m_TravelTime=0;
	long double a=xe-m_MirrorX0;
	long double b=ye-m_MirrorY0;
	long double c=ze-m_MirrorZ0;

	//convert to rad
//	theta=theta*M_PI/180.;
//	phi=phi*M_PI/180.;
	//std::cout<<"Theta "<<theta<<std::endl;
	//std::cout<<"Phi "<<phi<<std::endl;

	long double epsilon=std::sin(theta)*(a*std::cos(phi)+b*std::sin(phi))+c*std::cos(theta);
	TVector3 MirrorPos;

/*	MirrorPos.SetXYZ(xe+(-epsilon+std::sqrt(epsilon*epsilon-a*a-b*b-c*c+m_MirrorR*m_MirrorR))*std::sin(theta)*std::cos(phi),
			ye+(-epsilon+std::sqrt(epsilon*epsilon-a*a-b*b-c*c+m_MirrorR*m_MirrorR))*std::sin(theta)*std::sin(phi),
			ze+(-epsilon+std::sqrt(epsilon*epsilon-a*a-b*b-c*c+m_MirrorR*m_MirrorR))*std::cos(theta));*/

	long double term = -epsilon+std::sqrt(epsilon*epsilon-a*a-b*b-c*c+m_MirrorR*m_MirrorR);
	//std::cout<<"Term: "<<term<<std::endl;

	MirrorPos.SetXYZ(xe+term*std::sin(theta)*std::cos(phi),
			ye+term*std::sin(theta)*std::sin(phi),
			ze+term*std::cos(theta));
	//std::cout<<"MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM:"<<MirrorPos(0)<<"\t"<<MirrorPos(1)<<std::endl;

	//save the photons mirror Position
	m_PhotonMirrorPos=MirrorPos;

	//This is our lot
	TVector3 NablaF;
	NablaF.SetXYZ(MirrorPos(0)-m_MirrorX0,MirrorPos(1)-m_MirrorY0,MirrorPos(2)-m_MirrorZ0);
	//normalizeVector(NablaF);

	TVector3 TrajRefPhot(std::sin(theta)*std::cos(phi),std::sin(theta)*std::sin(phi),std::cos(theta));

	TrajRefPhot=mirrorVector(TrajRefPhot,NablaF);

	//TVector3 traj=mirrorVector(TrajRefPhot,NablaF);
/*	std::cout<<"Nabla Coordinates "<<std::endl;
	std::cout<<"x "<<NablaF(0)<<std::endl;
	std::cout<<"y "<<NablaF(1)<<std::endl;
	std::cout<<"z "<<NablaF(2)<<std::endl;

	std::cout<<"Vector Coordinates "<<std::endl;
	std::cout<<"x "<<traj(0)<<std::endl;
	std::cout<<"y "<<traj(1)<<std::endl;
	std::cout<<"z "<<traj(2)<<std::endl;

	std::cout<<"Variables"<<std::endl;
	std::cout<<"a: "<<a<<std::endl;
	std::cout<<"b: "<<b<<std::endl;
	std::cout<<"c: "<<c<<std::endl;
	std::cout<<"epsilon: "<<epsilon<<std::endl;
	std::cout<<"sin(theta): "<<std::sin(theta)<<std::endl;
	std::cout<<"std::cos(phi): "<<std::cos(phi)<<std::endl;
	std::cout<<"std::sin(phi): "<<std::sin(phi)<<std::endl;
	std::cout<<"std::cos(theta)"<<std::cos(theta)<<std::endl;
*/
	normalizeVector(TrajRefPhot);
	long double lambda=(m_PMTPos-MirrorPos(2))/TrajRefPhot(2);
	TVector3 PMTPos=MirrorPos+lambda*TrajRefPhot;
	//TVector3 PMTPos=MirrorPos-traj;

//-------------------------------------------------------------------
//From here debug stuff starts
//-------------------------------------------------------------------
if(m_debug){

	std::cout<<"Light"<<std::endl;
	std::cout<<ze<<"\t"<<xe<<std::endl;
	std::cout<<MirrorPos(2)<<"\t"<<MirrorPos(0)<<std::endl;
	std::cout<<PMTPos(2)<<"\t"<<PMTPos(0)<<std::endl;

	std::cout<<"Gradient"<<std::endl;
	TVector3 G_Start = MirrorPos;
	TVector3 G_End = MirrorPos - 5*NablaF;
	std::cout<<G_Start(2)<<"\t"<<G_Start(0)<<std::endl;
	std::cout<<G_End(2)<<"\t"<<G_End(0)<<std::endl;

	double Phot_x[3] = {ze,MirrorPos(2),PMTPos(2)};
	double Phot_y[3] = {xe,MirrorPos(0),PMTPos(0)};

	double Grad_x[2] = {G_Start(2),G_End(2)};
	double Grad_y[2] = {G_Start(0),G_End(0)};

	//produce circle
	int n=50;
	double x[50], y[50];

	for(int i=0;i<n;i++){
		x[i]=14+(m_MirrorR-14)/n*(i+1);
		y[i]=std::sqrt(m_MirrorR*m_MirrorR-(x[i]-m_MirrorZ0)*(x[i]-m_MirrorZ0));
	}

	double x_down[50], y_down[50];

	for(int i=0;i<n;i++){
		x_down[i]=14+(m_MirrorR-14)/n*(i+1);
		y_down[i]=-std::sqrt(m_MirrorR*m_MirrorR-(x_down[i]-m_MirrorZ0)*(x_down[i]-m_MirrorZ0));
	}

	TGraph *gr1 = new TGraph (3, Phot_x, Phot_y);
	TGraph *gr2 = new TGraph (2, Grad_x,Grad_y);
	TGraph *gr3 = new TGraph (50,x,y);
	TGraph *gr4 = new TGraph (50,x_down,y_down);

	//detector
	double det_x[5]={0,18.1,18.1,0,0};
	double det_y[5]={-5,-5,5,5,-5};

	TGraph *det = new TGraph (5,det_x,det_y);
	TLine *l1 = new TLine(0,0,18.1,0);

	//	TCanvas *c1= new TCanvas("c1","c1",800,800);

	det->Draw("A L");
	gr1->Draw("same, L*");
	gr2->SetLineColor(2);
	gr2->Draw("same,L*");
	gr3->Draw("C");
	gr4->Draw("C");
	l1->Draw();

/*
	// create graph
	TGraph *gr = new TGraph(3, Phot_x, Phot_y);
	TCanvas *c1 = new TCanvas("c1","Graph Draw Options",200,10,800,800);
	// draw the graph with axis, contineous line, and put a * at each point
	gr->Draw("AL*");
*/
}

	double n =getNeonRefractiveIndex(PhotonEnergy);//here we calculate the refraction index needed to determine the speed of light velocity in neon
    
	  if(n>0){
	    m_TravelTime=(std::abs(term)+std::abs(lambda))/(m_SpeedOfLight/n); //speed of light in medium = c/n
	  }
	  else{
	    std::cerr<<"There was a mistake ith refraction index n<=0 "<<n<<std::endl;
	  }
	return PMTPos;
}

TVector3 RICHFastSim::getPhotonPosAtPMTSmallAngleApprox(double xe, double ye, double ze, double theta, double phi){
		double a=xe-m_MirrorX0;
		double b=ye-m_MirrorY0;
		double c=ze-m_MirrorZ0;

		//convert to rad
		theta=theta*M_PI/180;
		phi=phi*M_PI/180;

		double epsilon=theta*(a*std::cos(phi)+b*std::sin(phi))+c*1.;
		TVector3 MirrorPos;

		MirrorPos.SetXYZ(xe+(-epsilon+std::sqrt(epsilon*epsilon-a*a-b*b-c*c+m_MirrorR*m_MirrorR))*theta*std::cos(phi),
				ye+(-epsilon+std::sqrt(epsilon*epsilon-a*a-b*b-c*c+m_MirrorR*m_MirrorR))*theta*std::sin(phi),
				ze+(-epsilon+std::sqrt(epsilon*epsilon-a*a-b*b-c*c+m_MirrorR*m_MirrorR))*1.);

		//This is our lot
		TVector3 NablaF;
		NablaF.SetXYZ(MirrorPos(0)-m_MirrorX0,MirrorPos(1)-m_MirrorY0,MirrorPos(2)-m_MirrorZ0);

		NablaF.Unit();

		//Trajectory of refelected phoon in PMT direction
		//TVector3 TrajRefPhot = MirrorPos;

		//the direction of the mirrored photon is -1*A*v where A is the rotation matrix and v is the incoming direction of the photon
		// -1 because the vector points in the opposite direction as the incoming one
/*
		TVector3 TrajRefPhot((-1+2*NablaF(0)*NablaF(0))*MirrorPos(0)+2*NablaF(0)*NablaF(1)*MirrorPos(1)+2*NablaF(0)*NablaF(2)*MirrorPos(2),
				2*NablaF(1)*NablaF(0)*MirrorPos(0)+(-1+2*NablaF(1)*NablaF(1))*MirrorPos(1)+2*NablaF(1)*NablaF(2)*MirrorPos(2),
				2*NablaF(2)*NablaF(0)*MirrorPos(0)+2*NablaF(2)*NablaF(1)*MirrorPos(1)+(-1+2*NablaF(2)*NablaF(2))*MirrorPos(2));
*/

		TVector3 TrajRefPhot(theta*std::cos(phi),theta*std::sin(phi),1.);

		//TrajRefPhot.Rotate(M_PI,NablaF);
		TrajRefPhot=rotateVector(TrajRefPhot,M_PI,NablaF);
		TrajRefPhot.Unit();
		double lambda=(m_PMTPos-MirrorPos(2))/TrajRefPhot(2);
		TVector3 PMTPos=MirrorPos+lambda*TrajRefPhot;
		return PMTPos;
}

void RICHFastSim::setMirrorRadius(double R){
	m_MirrorR=R;
}
void RICHFastSim::setMirrorCenter(TVector3 MirrCent){
	m_MirrorX0=MirrCent(0);
	m_MirrorY0=MirrCent(1);
	m_MirrorZ0=MirrCent(2);
}
void RICHFastSim::setMirrorCenterX(double X){
	m_MirrorX0=X;
}
void RICHFastSim::setMirrorCenterY(double Y){
	m_MirrorY0=Y;
}
void RICHFastSim::setMirrorCenterZ(double Z){
	m_MirrorZ0=Z;
}
void RICHFastSim::setPMTPos(double Z_PMT){
	m_PMTPos=Z_PMT;
}

double RICHFastSim::getPMTPos(){
	return m_PMTPos;
}

//vector to rotated, angle , rot vector
TVector3 RICHFastSim::rotateVector(TVector3 vec, double a, TVector3 v){
	TVector3 Rotated;
/*	Rotated.SetXYZ(std::cos(a)+v(0)*v(0)*(1-std::cos(a))*vec(0) + (v(0)*v(1)*(1-std::cos(a))-v(2)*std::sin(a))*vec(1) + (v(0)*v(2)*(1-std::cos(a))+v(1)*std::sin(a))*vec(2),
			(v(1)*v(0)*(1-std::cos(a))+v(2)*std::sin(a))*vec(0) + (std::cos(a)+v(1)*v(1)*(1-std::cos(a)))*vec(1) + (v(1)*v(2)*(1-std::cos(a))-v(0)*std::sin(a))*vec(2),
			(v(2)*v(0)*(1-std::cos(a))-v(1)*std::sin(a))*vec(0) + (v(2)*v(1)*(1-std::cos(a))+v(0)*std::sin(a))*vec(1) + (std::cos(a)+v(2)*v(2)*(1-std::cos(a)))*vec(2));
*/

	double c = std::cos(a);
	double s = std::sin(a);
/*
	Rotated.SetXYZ((v(0)*v(0)+(1-v(0)*v(0))*c)*vec(0) + (v(0)*v(1)*(1-c)-v(2)*s)*vec(1) + (v(0)*v(2)*(1-c)+v(1)*s)*vec(2),
			(v(0)*v(1)*(1-c)+v(2)*s)*vec(0) + (v(1)*v(1)+(1-v(1)*v(1))*c)*vec(1) + (v(1)*v(2)*(1-c)-v(0)*s)*vec(2),
			(v(0)*v(2)*(1-c)-v(1)*s)*vec(0) + (v(1)*v(2)*(1-c)+v(0)*s)*vec(1) + (v(2)*v(2)+(1-v(2)*v(2))*c)*vec(2));
*/

	double x=v(0);
	double y=v(1);
	double z=v(2);
	Rotated.SetXYZ( (x*x*(1-c)+c)*vec(0) + (x*y*(1-c)-z*s)*vec(1) + (x*z*(1-c)+y*s)*vec(2),
			(x*y*(1-c)+z*s)*vec(0) + (y*y*(1-c)+c)*vec(1) + (y*z*(1-c)+x*s)*vec(2),
			(x*z*(1-c)+y*s)*vec(0) + (y*z*(1-c)+x*s)*vec(1) + (z*z*(1-c)+c)*vec(2));

	return Rotated;
}

TVector3 RICHFastSim::mirrorVector(TVector3 vec, TVector3 axis){
	//axis has to be a unit vector
	//scalar product
	long double projection = (vec(0)*axis(0)+vec(1)*axis(1)+vec(2)*axis(2));

	//TVector3 S = projection*axis;
	TVector3 S = projection/(axis(0)*axis(0)+axis(1)*axis(1)+axis(2)*axis(2))*axis;
	//std::cout<<" Proj "<<projection<<std::endl;

	TVector3 d;
	d.SetXYZ(S(0)-vec(0),S(1)-vec(1),S(2)-vec(2));
	TVector3 ret;
	ret.SetXYZ(S(0)+d(0),S(1)+d(1),S(2)+d(2));
	return ret;
}

void RICHFastSim::normalizeVector(TVector3 &v){//normalize a vector to a unit vector
	v=(1/std::sqrt(v(0)*v(0)+v(1)*v(1)+v(2)*v(2)))*v;
}


void RICHFastSim::setMirrorDiameter(double diameter){
	m_Diameter=diameter;
}

TVector3 RICHFastSim::getPhotonPosOnMirror(){
	return m_PhotonMirrorPos;
}

bool RICHFastSim::checkAcceptanceOfEvent(){
	if(m_Diameter==0){
		std::cerr<<"Diameter not set!!!"<<std::endl;
	}

	if(std::pow(m_Diameter/2.,2)>=std::pow(m_PhotonMirrorPos(0),2)+std::pow(m_PhotonMirrorPos(1),2)){
		return true;
	}
	else{
		return false;
	}
}


double RICHFastSim::getTravelTime(){
	return m_TravelTime;
}

//this is now double implemented one could also have his function only one time (see Cherenkov prod)

double RICHFastSim::getNeonRefractiveIndex(double E, double Temperature, double Pressure){

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

	double h_Planck=4.135669239559144E-12;
	double s = 1e9;
	return 1.+2.61303e27/(39160e27-(E/h_Planck)*(E/h_Planck)*(s*s))
		*(Pressure/(Temperature))/(STP_Pressure/(STP_Temperature));
}
