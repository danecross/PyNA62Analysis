/*
 * RichAcceptance.hh
 *
 *  Created on: 26.08.2010
 *      Author: imp
 */

#ifndef RICHACCEPTANCE_H
#define RICHACCEPTANCE_H 1
#include <TVector3.h>
#include <cmath>

class RichAcceptance {
public:
	RichAcceptance();
	virtual ~RichAcceptance();
	//calcualtes the length a particle can fly from its Start pos to the pos where it leave the Rich
	double calculatePathLength(TVector3 StartPosInRich, TVector3 Momentum);
	void setMirrorPos(double MirrorPos);
	double getMirrorPos();
	void setRichRadius(double r);
	double getRichRadius();


private:
	double m_MirrorPos;
	double m_RichRadius;
};

#endif 
