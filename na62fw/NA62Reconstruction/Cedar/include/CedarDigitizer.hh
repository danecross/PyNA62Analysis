// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#ifndef CedarDigitizer_H
#define CedarDigitizer_H 1

#include "NA62VDigitizer.hh"

class CedarDigitizer : public NA62VDigitizer {

public:

  explicit CedarDigitizer(NA62VReconstruction*);
  virtual ~CedarDigitizer();
  virtual TDetectorVEvent* ProcessEvent(TDetectorVEvent *);
  virtual void StartOfBurst();
  virtual void EndOfBurst();

protected:
  // Quantum efficiencies of various PMTs
  Double_t QE                 (Double_t wavelength, Int_t PMType);
  Double_t QE_R7400U_03       (Double_t wavelength);
  Double_t QE_R9880U_110      (Double_t wavelength);
  Double_t QE_R9880U_210      (Double_t wavelength);
  Double_t QE_EMI_9820_QB_Lau (Double_t wavelength);
  Double_t QE_EMI_9820_QB     (Double_t wavelength);

private:
  TF1 *fTimeResponse;

  // from config file via CedarReconstruction
  Double_t fPMTTime_min;         ///< min time validity
  Double_t fPMTTime_max;         ///< max time validity
  Double_t fPMTWidth_mean;       ///< Average Width of PMT response (ns)
  Double_t fPMTWidth_sigma;      ///< Smear of Width of PMT response (ns)
  Double_t fPMT_Efficiency;      ///< global PMT efficiency: the QE is multiplied by it
  Double_t fPMT_MergeThreshold;  ///< hit merge threshold in ns
};

#endif
