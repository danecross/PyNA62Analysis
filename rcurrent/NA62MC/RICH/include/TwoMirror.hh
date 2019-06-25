/*
 * TwoMirror.hh
 *
 *  Created on: 21.07.2010
 *      Author: imp
 */

#ifndef TWOMIRROR_H_
#define TWOMIRROR_H_

#include "RICHFastSim.hh"

class TwoMirror {
public:
	TwoMirror();
	virtual ~TwoMirror();

	TVector3 getPhotonPosAtPMT(double xe, double ye, double ze, double theta, double phi, double PhotonEnergy);
	void setMirrorOneDiameter(double diameter);
	void setMirrorTwoDiameter(double diameter);
	void setMirrorOneRadius(double R);
	void setMirrorTwoRadius(double R);
	void setMirrorCenterZ(double z);
	void setMirrorOneCenterX(double x);
	void setMirrorTwoCenterX(double x);
	void setMirrorOneCenterY(double y);
	void setMirrorTwoCenterY(double y);
	void setPMTPos(double z);
	double getPMTPos();
	//sets if in the middle between the two mirrors a hexagonal shape is taken into account
	//like in the G4 Sim
	void setHexagonalMirrorCut(bool HexCut);
	//returns which mirror was hit -1 or 1 according to positive or negative side(0 for not set)
	int getWhichMirrorWasHit();
	//returns the photon pos on the mirror
	TVector3 getPhotonPosOnMirror();
	//returns true if a photon got lost
	bool checkOuterMirrorShape(double X, double Y);
	//checks on which side of the mirror the particle is
	//retuns +/-1 for right/left 0 for failure
	int checkHexagonalCrack(double PosX, double PosY);
	bool checkBeamHole(double PosX, double PosY);
	void setBeamHoleRadius(double R);
	//returns the time the photon travels through the mirror system from Production Pos to PMTs
	double getTravelTime();


private:
	RICHFastSim m_MirrorOne;//x>0
	RICHFastSim m_MirrorTwo;//x<0

	double m_MirrorOneDiameter;
	double m_MirrorTwoDiameter;
	double m_MirrorOneRadius;
	double m_MirrorTwoRadius;
	double m_MirrorCenterZ;

	double m_BeamHoleRadius;


	int m_WhichMirror;

	bool m_HexagonalMirrorOuterShape;
	bool m_HexagonalMirrorCut;
	bool m_BeamHoleAcceptance;

	TVector3 m_PhotonMirrorPos;


	double m_TravelTime;

};

#endif /* TWOMIRROR_H_ */
