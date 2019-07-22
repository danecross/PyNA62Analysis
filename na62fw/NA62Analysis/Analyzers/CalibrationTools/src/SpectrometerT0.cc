// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-04-10
// Developed by Dmitry Madigozhin (madigo@mail.cern.ch) 2015-05-31
//
// ---------------------------------------------------------------

#include <iostream>
#include <fstream>
#include "SpectrometerT0.hh"
#include "StrawAddress.hh"
#include "StrawPlace.hh"

using namespace std;

SpectrometerT0::SpectrometerT0(Core::BaseAnalysis *ba) : T0Evaluation(ba, "Spectrometer") {

  fMinIntegral         = 100;   // minimal number of entries (excluding underflows, overflows) for fit attempt
  fMinContentMaxBin    = 30.0;  // minimal content of most populated bin for fit attempt
  fFittingRange        = 50.0;  // fitting range [ns]
  fNFilesToAccumulate  = 20;    // for the T0 stability plots
  fHistoTimeLimit      = 120.0; // time half-span of plotted histograms [ns]
  fSignalPeakWidth     = 999;   // exclusion region half-width for the spectrum shape check
  fIssueWarnings       = false; // check if the spectrum shape is OK?
  fPlotChannelTimes    = false; // do not plot channel times (they are too many)
  fPlotTimeDependences = false; // do not plot time stability of T0s im channels (they are too many)

}

////////////////////////////
// A custom fitting function

Double_t SpecT0FitFunction(Double_t *v, Double_t *par) {
  Double_t fitval;
  Double_t sigma = par[2]+(v[0] - par[1])*par[3];
  if (sigma > 0.0) {
    Double_t arg = (v[0] - par[1])/sigma;
    fitval = par[0]*TMath::Exp(-0.5*arg*arg);
  }
  else {
    fitval = 0.0;
  }
  return fitval + par[4];
}

//////////////////////////////////////////////////////
// Non-standard time response: a custom fitting method

Bool_t SpectrometerT0::FitChannel(Int_t ich, Double_t c0, Double_t cmin, Double_t cmax, Double_t maxc) {

  fFChannelFit[ich] = new TF1("fitf", SpecT0FitFunction, cmin, cmax, 5);
  fFChannelFit[ich]->SetParameters(maxc,c0,12.0,0.43,0.0);
  fFChannelFit[ich]->SetParLimits(0, maxc*0.5, maxc*2.0);
  fFChannelFit[ich]->SetParLimits(1, c0-20.0, c0+20.0);
  fFChannelFit[ich]->SetParLimits(2, 0.0, 50.0);
  fFChannelFit[ich]->SetParLimits(3, 0.0, 10.0);
  fFChannelFit[ich]->SetParLimits(4, 0.0, maxc);

  fHTime[ich]->Fit(fFChannelFit[ich], "RB0Q");

  // A factor defining the level when distribution become "visible" 
  double LevelFactor = sqrt(2.0*log(1000.0)); // for 1/1000 level of the peak value

  double PeakPosition = fFChannelFit[ich]->GetParameter(1);
  double s0 = fFChannelFit[ich]->GetParameter(2);
  double ss = fFChannelFit[ich]->GetParameter(3);
  double dt = fabs(LevelFactor*s0/(1.0+LevelFactor*ss));
  double dt0 = fabs(s0/ss);
  if (dt>dt0) dt=dt0; // just in case, it should never happen...

  double T0         = PeakPosition - dt;
  double Resol      = s0;                                 // first term of resolution
  double DeltaT0    = fFChannelFit[ich]->GetParError(1);  // approximately
  double DeltaResol = fFChannelFit[ich]->GetParError(2);

  // Check if the fit is successful
  if (Resol>20.0 || DeltaT0>5.0 || DeltaResol>10.0) return false;

  fT0[ich]         = T0;
  fDeltaT0[ich]    = DeltaT0;
  fResol[ich]      = Resol;
  fDeltaResol[ich] = DeltaResol;
  return true;
}
