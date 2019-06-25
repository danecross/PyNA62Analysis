#ifndef TSLIMRECOMUVHIT_H
#define TSLIMRECOMUVHIT_H

#include "TSlimRecoVHit.hh"
#include "NA62Global.hh"
#include <RtypesCore.h>
#include <TMath.h>

class TSlimRecoMUVHit : public TSlimRecoVHit {

public:
	TSlimRecoMUVHit ()          { Clear(); }
	virtual ~TSlimRecoMUVHit () {}

	virtual void Clear (Option_t * option="");

	// Setters
	void SetChannelID(Short_t channelId)     { fChannelID = channelId;      }
	void SetPeakAmplitude(Float_t amplitude) { fPeakAmplitude = amplitude;  }
	void SetPeakAmplitudeError(Float_t amplitude, Float_t amplitudeError) {
		fPeakAmplitude = amplitude;
		Float_t val = 1.e+3*amplitudeError/amplitude;
		if (val>65535) val=65535;
		fPeakAmplitudeError = static_cast<UShort_t>(val);
	}
	void SetCharge(Float_t charge)           { fCharge = charge;            }
	void SetChargeError(Float_t charge, Float_t chargeError) {
		fCharge = charge;
		if (charge<10) charge=10.;
		Float_t val = 1.e+3*chargeError/charge;
		if (val>65535) val=65535;
		fChargeError = static_cast<UShort_t>(val);
	}
	void SetSigma(Float_t sigma)             { fSigma = sigma;              }
	void SetSigmaError(Float_t sigma, Float_t sigmaError) {
		fSigma = sigma;
		Float_t val = 1.e+3*sigmaError/sigma;
		if (val>65535) val=65535;
		fSigmaError = static_cast<UShort_t> (val);
	}
	void SetTime(Float_t time)               { fTime = time;                }
	void SetTimeError(Float_t time, Float_t timeError) {
		fTime = time;
		Float_t val = 1.e+3*timeError/10.;
		if (val>65535) val=65535;
		fTimeError = static_cast<UShort_t>(val);
	}

	// Getters
	Short_t GetChannelID()          const { return fChannelID;     }
	Float_t GetPeakAmplitude()      const { return fPeakAmplitude; }
	Float_t GetCharge()             const { return fCharge;        }
	Float_t GetSigma()              const { return fSigma;         }
	Float_t GetTime()               const { return fTime;          }
	Float_t GetPeakAmplitudeError() const {
	    return 1.e-3*static_cast<Float_t>(fPeakAmplitudeError)*fPeakAmplitude;
	}
	Float_t GetChargeError() const {
		Float_t charge=fCharge;
		if (charge<10.) charge=10.;
		return 1.e-3*static_cast<Float_t>(fChargeError)*charge;
	}
	Float_t GetSigmaError() const {
		return 1.e-3*static_cast<float>(fSigmaError)*fSigma;
	}
	Float_t GetTimeError() const {
		return 10.e-3*static_cast<Float_t>(fTimeError);
	}

	//Replicating Standard Persistency
	Int_t GetScintillatorOrientation () const { return (fChannelID%100 > 50 ? 0 : 1);                         }
	Int_t GetPlane ()                   const { return (GetSide()%2==0 ? kHorizontalPlane : kVerticalPlane ); }

	// To be implemented in detector specific classes
	virtual Int_t GetQuadrant ()              const { return 0;  }
	virtual Float_t GetScintillatorPosition() const { return 0.; }
	virtual Float_t GetEnergy ()              const { return 0.; }
    virtual Int_t GetScintillatorNumber ()    const = 0;
    virtual Int_t GetSide ()                  const = 0;


protected:
	Short_t fChannelID;
	UShort_t fTimeError, fPeakAmplitudeError, fSigmaError, fChargeError;
	Float_t fTime;
	Float_t fPeakAmplitude;
	Float_t fCharge;
	Float_t fSigma;

	ClassDef(TSlimRecoMUVHit, 1)
};

#endif
