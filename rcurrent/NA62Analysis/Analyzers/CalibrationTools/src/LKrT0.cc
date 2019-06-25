// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-12-11
//
// ---------------------------------------------------------

#include "LKrT0.hh"

LKrT0::LKrT0(Core::BaseAnalysis *ba) : T0Evaluation(ba, "LKr") {

  // Optimal parameters
  fMinIntegral         = 1000;   // minimal number of entries (excluding underflows, overflows) for fit attempt
  fMinContentMaxBin    = 200.0;  // minimal content of most populated bin for fit attempt
  fFittingRange        = 2.;     // fitting range = [-2ns,+2ns]
  fNFilesToAccumulate  = 200;    // for the T0 stability plots
  fHistoTimeLimit      = 15.0;   // time half-span of plotted histograms [ns]
  fSignalPeakWidth     = 5.0;    // exclusion region half-width for the spectrum shape check
  fMaxResol            = 10.0;   // max time resolution to consider the fit successful
  fMaxDeltaT0          = 0.5;    // max precision of T0 to consider the fit successful
  fMaxDeltaResol       = 5.0;    // max precision on resolution to consider the fit successful
  fIssueWarnings       = true;   // check if the spectrum shape is OK?
  fPlotChannelTimes    = true;   // plot times in each channel?
  fPlotTimeDependences = false;  // check and plot the time stability of the T0 constants?

  // Non-standard input (CellID, not ReadoutChannelID):
  // config file must be ignored even if it exists
  fTH2Name       = "RecoHitTimeWrtReferenceVsCellNoT0"; 
  fUseChannelMap = false;
}


