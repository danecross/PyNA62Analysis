
#ifndef Rndm_h
#define Rndm_h 1

#include "TObject.h"
#include "TRandom3.h"

class Rndm : public TObject
{
public :
  Rndm();
  void StoreRandomState(TRandom3* RandomDecayState, TRandom3* RandomBeamState, long *RanecuState);
  TRandom3* GetRandomDecayState () {return fRandomDecayState;}
  TRandom3* GetRandomBeamState () {return fRandomBeamState;}
  long* GetRanecuState() {return fRanecuState;}


private:

  TRandom3* fRandomDecayState;
  TRandom3* fRandomBeamState;
  Long_t fRanecuState[2];
  ClassDef(Rndm,1)
};

#endif
