#ifndef CHANTIDigitizer_H
#define CHANTIDigitizer_H 1
#include "TDCBRawDecoder.hh"
#include "CHANTIRawDecoder.hh"
#include "NA62VDigitizer.hh"
#include "TF1.h"

class CHANTIDigitizer : public NA62VDigitizer
{

    public:

        explicit CHANTIDigitizer(NA62VReconstruction*);
        virtual ~CHANTIDigitizer();
        virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);

    public:
        std::vector<double> GetPositiveXtrg(double Th);
		std::vector<double> GetNegativeXtrg(double Th);
		void AddSingleDigi(TCHANTIHit* Hit,double Leading, double Trailing, Int_t Channel, bool IsLowThr);
		void CreateDigiStructure(TCHANTIHit* Hit, double StartSignal, double threshold, int ROCH, Int_t Channel, bool IsLowThr); 
		double SignalContribution_Def(Double_t x, Double_t *par);

    private:

	Double_t *fRingsMCToF;
        Double_t *fPhotonsNumber;
        Double_t *fEnergyEdge;
        Int_t fNbinEnergy;
        Double_t fSlopeAmplNph;		///<	This is the slope Amplitude/Nph of the SiPM signal(mV/Nph)
        Double_t fSigmaAmplSPE;		///<	This is the sigma of the amplitude of the single-photon signal (mV)
        Double_t fMeanNfr;		///<	This is the constant time decay of the SiPM signal(ns)
        Double_t fSigmaNfr; 		///<	This is the mean value of number of degree of freedom of the chi-quadro distribution that simulates the signal
        Double_t fHysteresis; 		///<	This is the sigma value of number of degree of freedom of the chi-quadro distribution that simulates the signal
        Double_t fTauFall;		///<	This is the hysteresis of the threshold (mV)
        Double_t fTHRL;			///<	This is the low threshold (mV)
        Double_t fTHRH;			///<	This is the high threshold (mV)
        Double_t fFiberLightSpeed; 	///<	This is the light speed inside the fiber (mm/ns)
        Double_t fReflectionFactorBoard;	///< Amplitute ratio between reflected (at the ToT board) and direct signal  
        Double_t fSlopeEnergyNph;		///< Slope in the linear function NphVSEnergyr (Nph/MeV)				
	Double_t fOffsetEnergyNph;	///< Offset in the linear function NphVSEnergyr (Nph)  
	Int_t fNpx = 20000;
    	double fXAxis[20000];
    	double fYAxis[20000];
    	double fFuncParameters[4];
    	double fPrimaryFuncParameters[4];
};

#endif
