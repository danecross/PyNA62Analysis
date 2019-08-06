#ifndef MUV2Digitizer_H
#define MUV2Digitizer_H 1

#include "TDirectory.h"

#include "NA62VDigitizer.hh"
#include "TMUV2Hit.hh"



class MUV2Digitizer : public NA62VDigitizer {

  public:

    explicit MUV2Digitizer(NA62VReconstruction*);
    virtual ~MUV2Digitizer();
    virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

    Double_t BirksCorrection(Double_t Energy, Double_t StepLength,
        Double_t BirksConstant);

    Double_t AnalogSignal(Double_t T, Double_t Center, Double_t Sigma);

  private:
    Double_t fInvSamplingFraction;
    Int_t fBirksEnable;
    TObjArray fHitsOnChannel[4][22];
    TMUV2Hit *fHit;

};

#endif
