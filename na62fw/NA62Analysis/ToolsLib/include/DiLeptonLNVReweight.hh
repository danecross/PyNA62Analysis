/*
 * DiLeptonReweight.hh
 *
 *  Created on: 25 Jul 2018
 *      Author: Nicolas Lurkin
 */

#ifndef TOOLSLIB_INCLUDE_DILEPTONLNVREWEIGHT_HH_
#define TOOLSLIB_INCLUDE_DILEPTONLNVREWEIGHT_HH_

#include <TLorentzVector.h>
#include "Event.hh"

class DiLeptonLNVReweight {
public:
	enum Mode {kEEMODE, kMUMUMODE};
	static DiLeptonLNVReweight* GetInstance();

	double GetWeight_VectorAmplitude(TLorentzVector pi, TLorentzVector l1, TLorentzVector l2, bool inMeV = true);
	double GetWeight_VectorAmplitude(double m2_pi_l1, double m2_l1_l2);
	double GetWeight_VectorAmplitude(Event* mcevt);
	double GetWeight_PascoliAmplitude(TLorentzVector pi, TLorentzVector l1, TLorentzVector l2, bool inMeV = true);
	double GetWeight_PascoliAmplitude(Event* mcevt);

	void   SetMode(Mode mode)             {	fMode = mode;                  }
	Mode   GetMode()                const { return fMode;                  }
	double GetGlobalNormalization() const { return fSumEvents/fSumWeights; }

	class{
		friend DiLeptonLNVReweight;
		double fMediatorMass;  ///< [GeV]
		double fMediatorGamma; ///< [GeV]
	public:
		void SetMediatorMass  (double mediatorMass  ) { fMediatorMass  = mediatorMass*1e-3;  } // input in [MeV]
		void SetMediatorWidth (double mediatorGamma ) { fMediatorGamma = mediatorGamma*1e-3; } // input in [MeV]

		double GetMediatorMass()  const { return fMediatorMass*1e3;  } // In [MeV]
		double GetMediatorWidth() const { return fMediatorGamma*1e3; } // In [MeV]
	} Pascoli;
	class{
		friend DiLeptonLNVReweight;
		double fFormFactorA;
		double fFormFactorB;
		double fLoopFactor;
	public:
		void SetFormFactorA(double formFactorA) { fFormFactorA = formFactorA; }
		void SetFormFactorB(double formFactorB) { fFormFactorB = formFactorB; }
		void SetLoopFactor (double loopFactor ) { fLoopFactor = loopFactor;   }

		double GetFormFactorA() const { return fFormFactorA; }
		double GetFormFactorB() const { return fFormFactorB; }
		double GetLoopFactor()  const { return fLoopFactor;  }
	} Vector;

	const static double fSqRatio_el;
	const static double fSqRatio_mu;
	const static double fSqRatio_pi;

	const static double fMKCH;
	const static double fSQMKCH;
	const static double fSQMPI;

	const static double fGFERMI;
	const static double fALPHA;
private:
	DiLeptonLNVReweight();
	virtual ~DiLeptonLNVReweight();

	static DiLeptonLNVReweight* fInstance;
	Mode   fMode;
	double fSumEvents;
	double fSumWeights;
};

#endif /* TOOLSLIB_INCLUDE_DILEPTONLNVREWEIGHT_HH_ */
