// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-04-30
// Developed by Dmitry Madigozhin (madigo@mail.cern.ch) 2015-05-31
//
// ------------------------------------------------------------------

#include "T0Evaluation.hh"

#ifndef SPECTROMETERT0_H
#define SPECTROMETERT0_H 1

class SpectrometerT0 : public T0Evaluation {

public:

  explicit SpectrometerT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerT0() {};

  Bool_t FitChannel(Int_t, Double_t, Double_t, Double_t, Double_t);
};

#endif
