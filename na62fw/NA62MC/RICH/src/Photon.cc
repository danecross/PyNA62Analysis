/*
 * Photon.cpp
 *
 *  Created on: 13.08.2010
 *      Author: imp
 */

#include "Photon.hh"

Photon::Photon(TVector3 Momentum, TVector3 Position, double time):m_Momentum(Momentum),m_Position(Position),m_time(time){
	// TODO Auto-generated constructor stub

}

Photon::~Photon() {
	// TODO Auto-generated destructor stub
}

double  Photon::getEnergy(){
	return m_Momentum.Mag();
}

TVector3 Photon::getPosition(){
	return m_Position;
}
void Photon::setPosition(TVector3 Pos){
	m_Position=Pos;
}
TVector3 Photon::getMomentum(){
	return m_Momentum;
}
void Photon::setMomentum(TVector3 Mom){
	m_Momentum=Mom;
}

double Photon::X(){
	return m_Position.X();
}
double Photon::Y(){
	return m_Position.Y();
}
double Photon::Z(){
	return m_Position.Z();
}
double Photon::Phi(){
	return m_Momentum.Phi();
}
double Photon::Theta(){
	return m_Momentum.Theta();
}

double Photon::getTime(){
	return m_time;
}

void Photon::setTime(double time){
	m_time=time;
}
