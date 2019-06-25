/*
 * TwoMirror.cpp
 *
 *  Created on: 21.07.2010
 *      Author: imp
 */

#include "TwoMirror.hh"
#include "RICHGeometryParameters.hh"
#include <cmath>
#include <iostream>
#include <iomanip>


TwoMirror::TwoMirror() {

  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();
  G4double MirrorFocalLength = GeoPars->GetMirrorFocalLength()/m;
  G4double BeamPipeFinOuterRadius = GeoPars->GetBeamPipeFinOuterRadius(0)/m;
  //G4double SubMirrorExternalRadius = GeoPars->GetSubMirrorExternalRadius()/m;
  G4TwoVector * SpotCenter = GeoPars->GetPMTsDiskCenter();
        

 //   m_MirrorCenterZ=-17.1;
	m_MirrorCenterZ=-MirrorFocalLength;

   //	m_MirrorOne.setMirrorCenterX(+1.6);
	m_MirrorOne.setMirrorCenterX(SpotCenter->x()/m);
   //	m_MirrorOne.setMirrorCenterY(0);
	m_MirrorOne.setMirrorCenterY(SpotCenter->y()/m);

	m_MirrorOne.setMirrorCenterZ(m_MirrorCenterZ);

	//	m_MirrorOne.setMirrorRadius(2*17.01);
	m_MirrorOne.setMirrorRadius(2*MirrorFocalLength);
	

	m_MirrorOne.setPMTPos(0.);

	m_MirrorOneDiameter = 3.;  // ask!!!!!!!!!!  approximation

	//	m_MirrorOneRadius = 2*17.1;  
	m_MirrorOneRadius = 2*MirrorFocalLength;

	m_MirrorOne.setMirrorDiameter(m_MirrorOneDiameter);

	//	m_MirrorTwo.setMirrorCenterX(-1.6);
	//m_MirrorTwo.setMirrorCenterY(0);
	m_MirrorTwo.setMirrorCenterX(-SpotCenter->x()/m);
	m_MirrorTwo.setMirrorCenterY(SpotCenter->y()/m);
	m_MirrorTwo.setMirrorCenterZ(m_MirrorCenterZ);

	//	m_MirrorTwo.setMirrorRadius(2*17.01);
	m_MirrorTwo.setMirrorRadius(2*MirrorFocalLength);

	m_MirrorTwo.setPMTPos(0.);

	m_MirrorTwoDiameter=m_MirrorOneDiameter;

	m_MirrorTwoRadius=m_MirrorOneRadius;

	m_MirrorTwo.setMirrorDiameter(m_MirrorTwoDiameter);

	//	m_BeamHoleRadius=0.0779;
	m_BeamHoleRadius=BeamPipeFinOuterRadius;

	m_TravelTime = 0;





	//Some other inits
	m_WhichMirror=0;
	m_HexagonalMirrorCut=true;
	m_HexagonalMirrorOuterShape=true;
	m_BeamHoleAcceptance=true;


}

TwoMirror::~TwoMirror() {
	// TODO Auto-generated destructor stub
}

TVector3 TwoMirror::getPhotonPosAtPMT(double xe, double ye, double ze, double theta, double phi, double PhotonEnergy){
	m_WhichMirror=0;
	m_TravelTime=0;
	//converte to rad
//	theta=theta*M_PI/180;
//	phi=phi*M_PI/180;


	

	//xe=-xe; //Necessary because we have in NA62 a right handed coordinate system so we have to change the sign

	//since the two mirror are on differnt pos on the x axis we have to check if a particle hits the mirror
	//at x>0 of x<0
	//one has to remember where  the edge of the mirror is close to the beam pipe!!!!

	//calcualte x pos

	double edge=m_MirrorOneRadius-std::sqrt(std::pow(m_MirrorOneRadius,2)-std::pow(m_MirrorOneDiameter/2.,2));
	//std::cout<<"edge Y approx"<<edge<<std::endl;
	//This is the distance the photon can travel until it reachs one of the mirrors
	double TravelDistance=m_MirrorCenterZ+m_MirrorOneRadius-ze-edge;
	//we approximate the Y position
	double PosY = ye + TravelDistance/std::cos(theta)*std::sin(theta)*std::sin(phi);
	//Which we need to do a better X aproximation
	edge=m_MirrorOneRadius-std::sqrt(std::pow(m_MirrorOneRadius,2)-std::pow(std::abs(PosY),2));
	//std::cout<<"edge X approx"<<std::setprecision(10)<<edge<<std::endl;
	TravelDistance=m_MirrorCenterZ+m_MirrorOneRadius-ze-edge;
	double PosX = xe + TravelDistance/std::cos(theta)*std::sin(theta)*std::cos(phi);


	//std::cout<<"posX"<<PosX<<std::endl;
/*
	TVector3 PMTPosMirrOne=m_MirrorOne.getPhotonPosAtPMT(xe, ye, ze, theta, phi);
	TVector3 PMTPosMirrTwo=m_MirrorTwo.getPhotonPosAtPMT(xe, ye, ze, theta, phi);

	TVector3 PhotonMirrorPos1 =m_MirrorOne.getPhotonPosOnMirror();
	TVector3 PhotonMirrorPos2 =m_MirrorTwo.getPhotonPosOnMirror();
*/
	TVector3 PMTPos=TVector3(0,0,0);
	int MirrorSide=0;//-1 left +1 right mirror 0 no mirror

	//check which mirror to use
	if(PosX>0) MirrorSide=1;
	if(PosX<0) MirrorSide=-1;

	//std::cout<<"site"<<MirrorSide<<std::endl;

	/*
	//calculate simple case that the mirror crack is a simple line
	//first we check that we are close to the crack
	if(std::abs(PhotonMirrorPos1.X())<0.1 || std::abs(PhotonMirrorPos2.X())<0.1){
		//we check that we take the mirror which has the biggest z, this is because due to the fact that the rack is
		//at 0 the hight of the mirror is for both mirrors at x=0 the same and we can't have any overlap.
		//we always use the xand y of mirror with the larger Z for the determination which mirror was hit
		if(PhotonMirrorPos1.Z()>PhotonMirrorPos2.Z()){
			if(PhotonMirrorPos1.X()>0){
				MirrorSide=1;
			}
			else{
				MirrorSide=-1;
			}
		}
		else{
			if(PhotonMirrorPos2.X()>0){
				MirrorSide=1;
			}
			else{
				MirrorSide=-1;
			}
		}
	}
*/

	if(m_HexagonalMirrorCut){
		MirrorSide=checkHexagonalCrack(PosX,PosY);
	}



	if(m_HexagonalMirrorOuterShape){
		if(!checkOuterMirrorShape(PosX, PosY)){
			MirrorSide=0;//particle got lost
		}
	}

	//we check for the beam hole
	if(m_BeamHoleAcceptance){
		if(!checkBeamHole(PosX,PosY)){
			MirrorSide=0;//particle got lost
		}
	}

	//std::cout<<"site2"<<MirrorSide<<std::endl;

	if(MirrorSide==1){//photon is in the area of mirror 1
		m_WhichMirror=1;
		PMTPos=m_MirrorOne.getPhotonPosAtPMT(xe, ye, ze, theta, phi,PhotonEnergy);
		m_PhotonMirrorPos=m_MirrorOne.getPhotonPosOnMirror();
		m_TravelTime=m_MirrorOne.getTravelTime();
		return PMTPos;
		//after this we have to check if the photon hits the mirror in the acceptance area
	/*	if(m_MirrorOne.checkAcceptanceOfEvent()){
			m_PhotonMirrorPos=m_MirrorOne.getPhotonPosOnMirror();
			//m_PhotonMirrorPos=TVector3(PosX,PosY,0);
			std::cout<<PMTPos(0)<<"\t "<<PMTPos(1)<<"\t "<<PMTPos(2)<<std::endl;
			return PMTPos;
		}
		else{
			std::cerr<<"Lost du to acceptance"<<std::endl;
			m_PhotonMirrorPos=TVector3(0,0,0);
			return TVector3(0,0,0);
		}*/
	}
	else if(MirrorSide==-1){//photon is in the area of mirror 2
		m_WhichMirror=-1;
		PMTPos=m_MirrorTwo.getPhotonPosAtPMT(xe, ye, ze, theta, phi,PhotonEnergy);
		m_PhotonMirrorPos=m_MirrorTwo.getPhotonPosOnMirror();
		m_TravelTime=m_MirrorTwo.getTravelTime();
		return PMTPos;
		//after this we have to check if the photon hits the mirror in the acceptance area
	/*	if(m_MirrorTwo.checkAcceptanceOfEvent()){
			m_PhotonMirrorPos=m_MirrorTwo.getPhotonPosOnMirror();
			//m_PhotonMirrorPos=TVector3(PosX,PosY,0);
			std::cout<<PMTPos(0)<<"\t "<<PMTPos(1)<<"\t "<<PMTPos(2)<<std::endl;
			return PMTPos;
		}
		else{
			std::cerr<<"Lost du to acceptance"<<std::endl;
			m_PhotonMirrorPos=TVector3(0,0,0);
			return TVector3(0,0,0);
		}*/
	}
	else{
	  //	std::cerr<<"Lost Photon"<<std::endl;
		m_PhotonMirrorPos=TVector3(0,0,0);
		m_TravelTime=0;
		return TVector3(0,0,0);
	}






}

void TwoMirror::setMirrorOneDiameter(double diameter){
	m_MirrorOneDiameter=diameter;
	m_MirrorOne.setMirrorDiameter(diameter);

}
void TwoMirror::setMirrorTwoDiameter(double diameter){
	m_MirrorTwoDiameter=diameter;
	m_MirrorTwo.setMirrorDiameter(diameter);
}
void TwoMirror::setMirrorOneRadius(double R){
	m_MirrorOneRadius=R;
	m_MirrorOne.setMirrorRadius(R);
}
void TwoMirror::setMirrorTwoRadius(double R){
	m_MirrorTwoRadius=R;
	m_MirrorTwo.setMirrorRadius(R);
}
void TwoMirror::setMirrorCenterZ(double z){
	m_MirrorCenterZ=z;
	m_MirrorOne.setMirrorCenterZ(m_MirrorCenterZ);
	m_MirrorTwo.setMirrorCenterZ(m_MirrorCenterZ);
}
void TwoMirror::setMirrorOneCenterX(double x){
	m_MirrorOne.setMirrorCenterX(x);
}
void TwoMirror::setMirrorTwoCenterX(double x){
	m_MirrorTwo.setMirrorCenterX(x);
}
void TwoMirror::setMirrorOneCenterY(double y){
	m_MirrorOne.setMirrorCenterY(y);
}
void TwoMirror::setMirrorTwoCenterY(double y){
	m_MirrorTwo.setMirrorCenterY(y);
}
void TwoMirror::setPMTPos(double z){
	m_MirrorOne.setPMTPos(z);
	m_MirrorTwo.setPMTPos(z);
}

double TwoMirror::getPMTPos(){
	return m_MirrorOne.getPMTPos();
}

int TwoMirror::getWhichMirrorWasHit(){
	return m_WhichMirror;
}

TVector3 TwoMirror::getPhotonPosOnMirror(){
	return m_PhotonMirrorPos;
}

bool TwoMirror::checkOuterMirrorShape(double X, double Y){

	double l=0.700/2.;
	double TanAlpha = std::tan(60*M_PI/180); //due to  equilateral  triangle
	double h=std::sqrt(3.)/2.*l;
	Y=std::abs(Y);
	X=std::abs(X);

	if(0<Y && Y<l/2){
		if(X<5*h){
			return true;
		}
	}
	if(l/2<Y && Y<l){
		if(X<5*h-TanAlpha*(Y-l/2)){
			return true;
		}
	}
	if(l<Y && Y<2*l){
		if(X<4*h){
			return true;
		}

	}
	if(2*l<Y && Y<2.5l){
		if(X<4*h-TanAlpha*(Y-2*l)){
			return true;
		}

	}
	if(2.5*l<Y && Y<3.5*l){
		if(X<3*h){
			return true;
		}

	}
	if(3.5*l<Y && Y<4*l){
		if(0<X && X<h){
			if(X<h-TanAlpha*(Y-3.5*l)){
				return true;
			}
		}
		if(h<X && X<2*h){
			if(X>2*h-TanAlpha*(4*l-Y)){
				return true;
			}
		}
		if(2*h<X && X<3*h){
			if(X<3*h-TanAlpha*(Y-3.5*l)){
				return true;
			}
		}
	}


	return false;
}
/*
int TwoMirror::checkHexagonalCrack(double PosX, double PosY){
	int MirrorSide = 0;
	if(PosX>0) MirrorSide=1;
	if(PosX<0) MirrorSide=-1;
	//definition of length used to calculate the hexagonal shape of the crack
	double l=0.700/2.;
	double TanAlpha = std::tan(60*M_PI/180); //due to  equilateral  triangle
	double h=std::sqrt(3.)/2.*l;
	if(PosY>3.5*l && PosY<4*l){
		if(PosX>0-TanAlpha*(4*l-PosY)){
			MirrorSide=+1;
		}
		else{
			MirrorSide=-1;
		}
	}
	if(PosY>2.5*l && PosY<3.5*l){
		if(PosX>0-h){
			MirrorSide=+1;
		}
		else{
			MirrorSide=-1;
		}
	}
	if(PosY>2*l && PosY<2.5*l){
		if(PosX>0-TanAlpha*(PosY-2*l)){
			MirrorSide=+1;
		}
		else{
			MirrorSide=-1;
		}
	}

	//negative y

	if(PosY<-2*l && PosY>-2.5*l){
		if(PosX<TanAlpha*(std::abs(PosY)-2*l)){
			MirrorSide=-1;
		}
		else{
			MirrorSide=1;
		}
	}
	if(PosY<-2.5*l && PosY>-3.5*l){
		if(PosX<0+h){
			MirrorSide=-1;
		}
		else{
			MirrorSide=+1;
		}
	}
	if(PosY<-3.5*l && PosY>-4*l){
		if(PosX<0+TanAlpha*(4*l-std::abs(PosY))){
			MirrorSide=-1;
		}
		else{
			MirrorSide=+1;
		}
	}

	return MirrorSide;
}
*/
//we have a right handet coordinate system
int TwoMirror::checkHexagonalCrack(double PosX, double PosY){
	int MirrorSide = 0;
	if(PosX>0) MirrorSide=1;
	if(PosX<0) MirrorSide=-1;
	//definition of length used to calculate the hexagonal shape of the crack
	double l=0.700/2.;
	double TanAlpha = std::tan(60*M_PI/180); //due to  equilateral  triangle
	double h=std::sqrt(3.)/2.*l;
	if(PosY>3.5*l && PosY<4*l){
		if(PosX<0+TanAlpha*(4*l-std::abs(PosY))){
			MirrorSide=-1;
		}
		else{
			MirrorSide=+1;
		}
	}
	if(PosY>2.5*l && PosY<3.5*l){
		if(PosX<0+h){
			MirrorSide=-1;
		}
		else{
			MirrorSide=+1;
		}
	}
	if(PosY>2*l && PosY<2.5*l){
		if(PosX<TanAlpha*(std::abs(PosY)-2*l)){
			MirrorSide=-1;
		}
		else{
			MirrorSide=1;
		}
	}

	//negative y

	if(PosY<-2*l && PosY>-2.5*l){
		if(PosX>0-TanAlpha*(std::abs(PosY)-2*l)){
			MirrorSide=+1;
		}
		else{
			MirrorSide=-1;
		}
	}
	if(PosY<-2.5*l && PosY>-3.5*l){
		if(PosX>0-h){
			MirrorSide=+1;
		}
		else{
			MirrorSide=-1;
		}
	}
	if(PosY<-3.5*l && PosY>-4*l){
		if(PosX>0-TanAlpha*(4*l-std::abs(PosY))){
			MirrorSide=+1;
		}
		else{
			MirrorSide=-1;
		}
	}

	return MirrorSide;
}

bool TwoMirror::checkBeamHole(double PosX, double PosY){

	if(PosX*PosX+PosY*PosY<m_BeamHoleRadius*m_BeamHoleRadius){
		return false;//inside beam hole
	}

	return true;//outside the hole

}

void TwoMirror::setBeamHoleRadius(double R){
	m_BeamHoleRadius=R;
}

double TwoMirror::getTravelTime(){
	return m_TravelTime;
}
