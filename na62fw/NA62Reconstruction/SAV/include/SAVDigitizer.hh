#ifndef SAVDigitizer_H
#define SAVDigitizer_H 1

#include "TDirectory.h"

#include "NA62VDigitizer.hh"
//#include "TSAVHit.hh"


class SAVDigitizer : public NA62VDigitizer {

  public:

    SAVDigitizer(NA62VReconstruction*);
    virtual ~SAVDigitizer();
    virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

  private:

};

#endif
