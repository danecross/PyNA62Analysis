/*
 * RICHFastSim.hh
 *
 *  Created on: 14.07.2010
 *      Author: imp
 */

#ifndef RICHFASTSIM_H_
#define RICHFASTSIM_H_

#include <TVector3.h>



class RICHFastSim {
public:
	RICHFastSim();
	virtual ~RICHFastSim();

	TVector3 getPhotonPosAtPMT(double xe, double ye, double ze, double theta, double phi, double PhotonEnergy);
	TVector3 getPhotonPosAtPMTSmallAngleApprox(double xe, double ye, double ze, double theta, double phi);
	double getNeonRefractiveIndex(double E, double Temperature=24, double Pressure=0.987);//in deg C and in bar

	void setMirrorRadius(double R);
	void setMirrorCenter(TVector3 MirrCent);
	void setMirrorCenterX(double X);
	void setMirrorCenterY(double Y);
	void setMirrorCenterZ(double Z);
	void setPMTPos(double Z_PMT);
	double getPMTPos();
	void setMirrorDiameter(double diameter);
	//returns the position of the photon on the mirror
	TVector3 getPhotonPosOnMirror();
	//function to check if the event was accapted or not
	//this function should be called after the position of the photon on PMT
	//is calculated if the mirror has a certain dimater
	bool checkAcceptanceOfEvent();

	TVector3 rotateVector(TVector3 vec, double angle, TVector3 v);
	TVector3 mirrorVector(TVector3 vec, TVector3 axis);
	void normalizeVector(TVector3 &v);

	//return time photon travels from production pos to mirror and pmt
	double getTravelTime();



private:
	//Variables of the mirror
	long double m_MirrorX0;
	long double m_MirrorY0;
	long double m_MirrorZ0;
	long double m_MirrorR;

	//photon observation Pos
	long double m_PMTPos;

	long double m_Diameter;

	TVector3 m_PhotonMirrorPos;

	bool m_debug;

	double m_TravelTime;
	double m_SpeedOfLight;
	
	


};

#endif /* RICHFASTSIM_H_ */
