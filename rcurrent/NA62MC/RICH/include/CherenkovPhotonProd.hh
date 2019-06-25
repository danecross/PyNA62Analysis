/*
 * CherenkovPhotonProd.hh
 *
 *  Created on: 13.08.2010
 *      Author: imp
 */

#ifndef CHERENKOVPHOTONPROD_H_
#define CHERENKOVPHOTONPROD_H_
#include <TVector3.h>
#include "Photon.hh"
#include <vector>
#include <TRandom.h>
#include <TF1.h>

class CherenkovPhotonProd {
public:
	CherenkovPhotonProd();
	virtual ~CherenkovPhotonProd();
	//gets as input the momentum of the partcle and the length the particle, has to produce Cherencov light
	//Momentum in MeV
	void produceCherenkovPhoton(const TVector3 &PartMom,const TVector3 &PartPos, double prodLength, double ParticeE);


	std::vector<Photon> getPhotonVector();//returns a vector with all cherencov photons
	void clearPhotonVector();//empties the Photon vector to get ready for new event
	//determines the production position of a single photon
	TVector3 getProductionPos(const TVector3 &PartMom,const TVector3 &PartPos, double prodLength);
	//determines the amount of photon per event according to a poisson dist
	int getAmountOfPhotons(double length, double beta);
	double getPhotonEnergy(double length, double beta);
	double QuartzTransmision(double E);
	double AlMgF2GlassReflectivity(double E);
	double AlMgF2MylarReflectivity(double E);
	double PMQuantumResponseU03(double E);
	double combinedEff(double E);
	double getNeonRefractiveIndex(double E, double Temperature=24, double Pressure=0.987);//in deg C and in bar
	Double_t FrankTammEquationDummy(Double_t *x, Double_t *par);

private:
	//returns a uniform phi direction
	double determinePhiDirection();
	//calculates the cherencov angle
	double determineCherencovAngle(double beta, double PhotonE);
	void transformIntoRefFrame();
	//produce a single Cherencov photon and saves it in the cherencov collectoon vector
	void produceSinglePhoton(const TVector3 &PartMom,const TVector3 &PartPos, double prodLength, double ParticeE, double beta);
	//dummy to be able to set function as TF1 function

	double FrankTammEquation(double E, double length, double beta);

	std::vector<Photon> m_PhotonVector;
	TRandom m_rand;


	double m_EPhotMax;
	double m_EPhotMin;

	double m_SpeedOfLight;

	//double m_N_Photons;
	TF1 * f1;

};

#endif /* CHERENKOVPHOTONPROD_H_ */
