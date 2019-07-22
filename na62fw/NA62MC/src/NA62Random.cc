#include "NA62Random.hh"

NA62Random::NA62Random() {
  fRandomDecayState = new TRandom3();
  fRandomBeamState = new TRandom3();
}


void NA62Random::StoreRandomState(TRandom3* RandomDecayState, TRandom3* RandomBeamState, long *RanecuState) {
  *fRandomDecayState = *RandomDecayState;
  *fRandomBeamState= *RandomBeamState;
  fRanecuState[0] = RanecuState[0];
  fRanecuState[1] = RanecuState[1];
}
