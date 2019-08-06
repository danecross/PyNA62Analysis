#ifndef CHODDigitizer_H
#define CHODDigitizer_H 1

#include "NA62VDigitizer.hh"

#include "TF1.h"

class CHODDigitizer : public NA62VDigitizer {

  public:

    explicit CHODDigitizer(NA62VReconstruction*);
    virtual ~CHODDigitizer();
    virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

  public:
    Int_t GetRecoID(Int_t);

  private:
    Int_t fNumberOfChannel;

    Double_t *fInverseVelocity;

    Double_t fSlewCorrSlope[128][16];
    Double_t fSlewCorrConst[128][16];
    Double_t fBeamTOF;
    Double_t fHitEnergyThreshold;
    Double_t fTOTAtPM[128];
    Double_t fTOTSlope[128];
    Double_t fLightVelocity[128];
    Double_t fTOTpdf[12];
    TF1 *TOTPdfMainPeak;
    TF1 *TOTPdfSecondPeak;
};

#endif
