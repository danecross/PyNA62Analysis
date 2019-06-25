#include "MUV0T0.hh"

MUV0T0::MUV0T0(Core::BaseAnalysis *ba) : T0Evaluation(ba, "MUV0") {
  // Optional parameters
  fPage1MinChannelID = 160; // adjust the lower limit for plotting in page 1 of the PDF report
  fPage1MaxChannelID = 177; // adjust the lower limit for plotting in page 1 of the PDF report
  fFittingRange        = 8;
  fMaxResol            = 5.0;
  fMaxDeltaT0          = 12;

}
