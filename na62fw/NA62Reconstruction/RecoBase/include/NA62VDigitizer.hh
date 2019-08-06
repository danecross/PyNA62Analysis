#ifndef NA62VDigitizer_H
#define NA62VDigitizer_H

#include "NA62VNamedModule.hh"
#include "NA62VReconstruction.hh"
#include "NA62VChannel.hh"
#include "TDigiVEvent.hh"

#include "TRandom3.h"

class NA62VDigitizer : public NA62VNamedModule {
  public:

    NA62VDigitizer(NA62VReconstruction*, TString);
    virtual ~NA62VDigitizer();
    virtual void StartOfBurst() = 0;
    virtual void EndOfBurst() = 0;
    virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent*) = 0;

  public:

    void           SetSeed(UInt_t value)        { fRandom->SetSeed(value); };

    TDigiVEvent *  GetDigiEvent()                    { return fDigiEvent;  };
    void           SetDigiEvent(TDigiVEvent * value) { fDigiEvent = value; };
    Int_t          GetNChannels()                    { return fNChannels;  };
    void           SetNChannels(Int_t value)         { fNChannels = value; };
    NA62VChannel** GetChannels()                     { return fChannels;   };
    void           SetChannels(NA62VChannel** value) { fChannels = value;  };

  protected:

    NA62VReconstruction * fReco;

    TRandom3 * fRandom;

    TDigiVEvent * fDigiEvent;
    Double_t * fStationsMCToF;
    Double_t * fT0;
    Int_t fNChannels;
    NA62VChannel **fChannels;
};
#endif
