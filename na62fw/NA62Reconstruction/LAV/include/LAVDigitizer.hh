

#ifndef LAVDigitizer_H
#define LAVDigitizer_H 1

#include "TFile.h"

#include "iostream"

#include "NA62VDigitizer.hh"
#include "LAVDigiBlockHandler.hh"
#include "LAVDigiBlock.hh"

#include "TH1D.h"
#include "TH2F.h"

class LAVDigitizer : public NA62VDigitizer
{

public:

  explicit LAVDigitizer(NA62VReconstruction*);
  virtual ~LAVDigitizer();
  virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);

private:

  LAVDigiBlockHandler* fDigiBlockHandler;

  void CreateDigi(Int_t, Double_t, Double_t, LAVDigiBlock*, Int_t );
  Int_t CreateDigiBlock(TLAVHit*);
  void DigitizeHits();

  TFile * fHistoFile;
  Int_t fMakeDigiHistos;
  void InitHisto();
  void FillHisto();
  void CloseHisto();

  TH1D* fHOptPhotons          ;
  TH1D* fHPhotoElectrons      ;
  TH1D* fHRiseTime            ;
  TH2F* fHMCTimeResolution    ;
  TH2F* fHMCEnergyCorrelation ;
  TH1D* fHMCGain              ;

};

#endif
