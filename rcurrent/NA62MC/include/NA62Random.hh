#ifndef NA62Random_h
#define NA62Random_h 1

#include "TRandom3.h"

class NA62Random {
  public :
    NA62Random();
    void StoreRandomState(TRandom3* RandomDecayState, TRandom3* RandomBeamState, long *RanecuState);
    TRandom3* GetRandomDecayState () {return fRandomDecayState;}
    TRandom3* GetRandomBeamState () {return fRandomBeamState;}
    long* GetRanecuState() {return fRanecuState;}


  private:

    TRandom3* fRandomDecayState;
    TRandom3* fRandomBeamState;
    Long_t fRanecuState[2];
};

#endif
