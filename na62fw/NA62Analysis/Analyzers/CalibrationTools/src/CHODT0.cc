#include "CHODT0.hh"

CHODT0::CHODT0(Core::BaseAnalysis *ba) : T0Evaluation(ba, "CHOD") {
  // Non-standard input (IntersectionID, not ReadoutChannelID):
  // config file must be ignored even if it exists
  // fTH2Name        = "RecoHitTimeWrtReferenceVsIntersectionIDNoT0";
  // fUseChannelMap  = false;
}
