#ifndef MUV0Digitizer_H
#define MUV0Digitizer_H 1

#include "TMUV0Hit.hh"
#include "TMUV0Digi.hh"
#include "TDCBRawDecoder.hh"
#include "NA62VDigitizer.hh"
#include "MUV0Reconstruction.hh"
#include "TDCEvent.hh"
#include "TRegexp.h"

class MUV0Digitizer : public NA62VDigitizer {

  public:

    explicit MUV0Digitizer(NA62VReconstruction*);
    virtual ~MUV0Digitizer() {}
    virtual TDetectorVEvent* ProcessEvent(TDetectorVEvent *);
    virtual void StartOfBurst();
    virtual void EndOfBurst();
    void ParseConfFile(TString FileName);

  private:
    Double_t fEnergyDepositThresholdLow;  ///< Low energy discrimination threshold [MeV]
    Double_t fEnergyDepositThresholdHigh; ///< High energy discrimination threshold [MeV] 
    Double_t fChannelTimeResolution;      ///< Time resolution [ns]
    Double_t fChannelMergeThreshold;      ///< [ns]
};

#endif
