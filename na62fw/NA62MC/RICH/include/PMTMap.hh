/*
 * PMTMap.hh
 *
 *  Created on: 21.07.2010
 *      Author: imp
 */

#ifndef PMTMAP_H_
#define PMTMAP_H_

#include <Riostream.h>
#include <TVector2.h>
#include <TString.h>
#include <TRegexp.h>
#include <TObjString.h>
#include <TObjArray.h>
#include <TMath.h>
#include <TArc.h>
#include <TCanvas.h>
#include <TH2F.h>
#include <TStyle.h>
#include <cmath>
#include <string>


class PMTMap {
public:
	PMTMap();
	virtual ~PMTMap();
	TVector2* GetPMPositions(std::string FileName);//stolen function: reads in the coordinates of the PMTs
	void DrawFlange(Double_t Side = 300);
	//check if photon hits a PMT
	//after this function on can call getHitPMTPos() to get the Position of the PMT which was hit
	bool checkAcceptance(double x, double y);
	//checks if Point is inside a circle at pos CircleCenterX,Y with Radius R
	bool isInsideCircle(double CircleR, double CircleCenterX, double CircleCenterY, double PointX, double PointY);
	void setRadiusWinstonCone(double R=0.009);
	TVector2 getHitPMTPos();


private:
	Int_t fNChannels, fNPhotoDetectors;
	TVector2 fSpotCenter, *fPMsPositions;
	Float_t * fT0;


	double m_RadiusWinstonCone;
	TVector2 m_PMTPosition;



};

#endif /* PMTMAP_H_ */
