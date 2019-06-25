#ifndef MUV1Digitizer_H
#define MUV1Digitizer_H 1

#include "NA62VDigitizer.hh"
#include "TMUV1Hit.hh"
#include "TDirectory.h"
#include "TH1.h"

class MUV1Digitizer : public NA62VDigitizer
{

  public:

    explicit MUV1Digitizer(NA62VReconstruction*);
    virtual ~MUV1Digitizer();
    virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);
    void UsePhotoelectronParametrization(Int_t Channel, Double_t HitMinTime);
    Double_t BirksCorrection(Double_t Energy, Double_t StepLength,
        Double_t BirksConstant);
    Double_t PhotoElectronsInScintillator(Double_t edep, Double_t position,
        Double_t photonsPerMeV, Double_t alpha, Double_t lamda1, Double_t lamda2, bool near);
    Double_t AnalogSignal(Double_t T, Double_t Center, Double_t Sigma);

  private:
	Double_t fInvSamplingFraction;
    Int_t fBirksEnable;
    TObjArray fHitsOnChannel[100];
    //TDirectory *fHistoDir;
    //TH1D *fhisto_plot[2];
};

#endif
