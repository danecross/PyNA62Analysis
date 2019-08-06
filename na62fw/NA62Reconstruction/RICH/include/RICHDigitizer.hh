// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
// modified by Francesca Bucci (fbucci@cern.ch), Monica Pepe (monica.pepe@cern.ch)
// ---------------------------------------------------------------

#ifndef RICHDigitizer_H
#define RICHDigitizer_H 1

#include "NA62VDigitizer.hh"
#include "TString.h"

class RICHDigitizer : public NA62VDigitizer {

public:

  explicit RICHDigitizer(NA62VReconstruction*);
  virtual ~RICHDigitizer();
  virtual TDetectorVEvent* ProcessEvent(TDetectorVEvent *);
  virtual void StartOfBurst();
  virtual void EndOfBurst();

  Int_t GetNSCChannels()            { return fNSCChannels;  }
  void  SetNSCChannels(Int_t value) { fNSCChannels = value; }

protected:
  Double_t QE(Double_t wavelength); ///< PMT quantum efficiency (QE)

private:
  Int_t    fNROChannels;
  Int_t    fNSCChannels;
  Double_t fChargeThreshold;
  Double_t fTimeWidthSigma;
  Double_t fWidthConstant;
  Double_t fQEcorrection; ///< Run-dependent correction factor to PMT QE
  TF1 *fTimeResponse;
  TF1 *fLandau;
  TF1 *fPolya;
};

#endif
