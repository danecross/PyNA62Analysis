#include "CHANTIT0.hh"

CHANTIT0::CHANTIT0(Core::BaseAnalysis *ba) : T0Evaluation(ba, "CHANTI") {

  // Optional parameters
  fMinIntegral         = 100;  // minimal number of entries (excluding underflows, overflows) for fit attempt
  fFittingRange        = 2.;   // fitting range = [-2.ns:+2.ns], i.e. 20 bins of 0.2ns width 
  fNFilesToAccumulate  = 20;   // for the T0 stability plots
  fHistoTimeLimit      = 30.0; // time half-span of plotted histograms [ns]
  fSignalPeakWidth     = 1.0;  // exclusion region half-width when looking for anomalous shape [ns]
  fInitialResol        = 1.5;  // initial value of the time resolution parameter for the fit [ns]
  fIssueWarnings       = true; // check if the spectrum shape is OK?
  fPlotChannelTimes    = true; // plot times in each channel?
  fPlotTimeDependences = false; // check and plot the time stability of the T0 constants?
}
