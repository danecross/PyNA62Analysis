#ifndef IRCDigitizer_H
#define IRCDigitizer_H 1


#include "TH2F.h"
#include "NA62VDigitizer.hh"

class IRCDigitizer : public NA62VDigitizer
{

    public:

        explicit IRCDigitizer(NA62VReconstruction*);
        virtual ~IRCDigitizer();
        virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);

    public:


    private:

  Double_t fXIRCOffset;
  Double_t fTOffset;
  Double_t fTRange;
  Double_t fBinWidth;
  Double_t fNGammaPerMeV;
  Double_t fImpedence   ;
  Double_t fTau         ;
  Double_t fEpsilonReach;
  Double_t fEpsilonPhotocatode;
  Double_t fPMTGain;

  Int_t fNtau              ;
  Int_t fNBinRange         ;
  Double_t fLightSuppressionFactor;
  Double_t fEmissionPars[5];

  Double_t fThLow     ;
  Double_t fThHigh    ;
  Double_t fHyst      ;
  Double_t fHyTimeC   ;
  Double_t fNorm      ;

  std::vector<Float_t> fEdgesLow[4]    ;
  std::vector<Int_t>   fEdgeTypesLow[4];
  std::vector<Float_t> fEdgesHigh[4]   ;
  std::vector<Int_t>   fEdgeTypesHigh[4];

  Int_t fChHit[4];
  Double_t fTotEnergy[4];
  Double_t fTSignalMax[4];
  Double_t fSignalMax[4];

  Bool_t fMakeDigiHistos;
  void InitHisto();
  void FillHisto();
  void CloseHisto();

  TH2F* fHRiseTime            ;
  TH2F* fHMCEnergyCorrelation ;
  TH2F* fHMCEnergyVsTotLow    ;
  TH2F* fHMCEnergyVsTotHigh   ;
  TH2F* fHNTotLow;
  TH2F* fHNTotHigh;


};

#endif
