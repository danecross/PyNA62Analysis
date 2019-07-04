#ifndef LKrDigitizer_H
#define LKrDigitizer_H 1

#include "NA62VDigitizer.hh"

class LKrParameters;
class LKrCommon;

class LKrDigitizer : public NA62VDigitizer
{

  public:
  explicit LKrDigitizer(NA62VReconstruction*);
  virtual ~LKrDigitizer();
  virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);

  public:

  private:
  LKrParameters *fPar;
  LKrCommon *fAdcCommon;
  Double_t fSigmaCorrNoise[4];
  Double_t fSigmaUncorrNoise[4];
  Double_t fSlopes[4];
  Double_t fReadShape[3000];
  Double_t fGevtoCurr1[5000];
  Double_t fGevtoCurr2[5000];
  Double_t fGevtoCurr3[5000];
  Double_t fESwitch[4];

  private:
  // cppcheck-suppress unusedPrivateFunction
  Int_t chooseGain(Double_t);


 };

#endif
