/*
 * RichAcceptance.cpp
 *
 *  Created on: 26.08.2010
 *      Author: imp
 */

#include "RichAcceptance.hh"
#include "RICHGeometryParameters.hh"
#include <iostream>

RichAcceptance::RichAcceptance() {
	// TODO Auto-generated constructor stub

  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();
  G4double MirrorFocalLength = GeoPars->GetMirrorFocalLength()/m;
  //G4double MirrorFlangeDistance = GeoPars->GetMirrorFlangeDistance()/m;

  //	m_MirrorPos=17.1;
  m_MirrorPos=MirrorFocalLength; // or MirrorFlangeDistance
  m_RichRadius=3;

}

RichAcceptance::~RichAcceptance() {
	// TODO Auto-generated destructor stub
}

double RichAcceptance::calculatePathLength(TVector3 StartPosInRich, TVector3 Momentum){
	//Length between start pos and mirror pos.
	double zLength=m_MirrorPos-StartPosInRich.Z();

	// now we check if we are out of apature in the mirror system

	double lambda=zLength/std::cos(Momentum.Theta());

	double xPos=lambda*std::sin(Momentum.Theta())*std::cos(Momentum.Phi())+StartPosInRich.X();
	double yPos=lambda*std::sin(Momentum.Theta())*std::sin(Momentum.Phi())+StartPosInRich.Y();

	if(zLength<0){
		return 0;
	}

	if(std::sqrt(std::pow(xPos,2)+std::pow(yPos,2))<m_RichRadius){//particle flies through mirror max length
		return lambda;
	}


	if(Momentum.Theta()==0){
		std::cerr<<"There was something wrong"<<std::endl;
		return 0;
	}
	double a = std::sin(Momentum.Theta())*std::cos(Momentum.Phi());
	double b = std::sin(Momentum.Theta())*std::sin(Momentum.Phi());

	double Ox=StartPosInRich.X();//offset
	double Oy=StartPosInRich.Y();

	double factor=(a*Ox+b*Oy)/std::pow(std::sin(Momentum.Theta()),2);
	//std::cout<<"lambda: "<<	-1*factor+std::sqrt(std::pow(factor,2)-(Oy*Oy+Ox*Ox-m_RichRadius*m_RichRadius)/std::pow(std::sin(Momentum.Theta()),2))<<std::endl;

	return -1*factor+std::sqrt(std::pow(factor,2)-(Oy*Oy+Ox*Ox-m_RichRadius*m_RichRadius)/std::pow(std::sin(Momentum.Theta()),2));




}
void RichAcceptance::setMirrorPos(double MirrorPos){
	m_MirrorPos=MirrorPos;
}
double RichAcceptance::getMirrorPos(){
	return m_MirrorPos;
}
void RichAcceptance::setRichRadius(double r){
	m_RichRadius=r;
}
double RichAcceptance::getRichRadius(){
	return m_RichRadius;
}
