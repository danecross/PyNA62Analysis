/*
 * Photon.hh
 *
 *  Created on: 13.08.2010
 *      Author: imp
 *
 *  This class holds all properties of the Cherencov photon
 */

#ifndef PHOTON_H_
#define PHOTON_H_
#include <TVector3.h>

class Photon {
public:
	Photon(TVector3 Momentum, TVector3 Position, double time=0);
	virtual ~Photon();
	double getEnergy();
	TVector3 getPosition();
	void setPosition(TVector3 Pos);
	TVector3 getMomentum();
	void setMomentum(TVector3 Mom);

	void setTime(double time);
	double getTime();
	double X();
	double Y();
	double Z();
	double Phi();
	double Theta();



private:
	TVector3 m_Momentum;
	TVector3 m_Position;
	double m_time; //in seconds

};

#endif /* PHOTON_H_ */
