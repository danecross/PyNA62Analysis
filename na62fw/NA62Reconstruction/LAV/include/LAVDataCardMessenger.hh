#ifndef LAVDataCardMessenger_H
#define LAVDataCardMessenger_H 1
#include "TString.h"

class LAVDataCardMessenger
{

public:

  LAVDataCardMessenger();
  static LAVDataCardMessenger* GetInstance();

  Bool_t HasToMakeLAVT0s(){return fMakeLAVT0s;};

  Bool_t CheckLAVT0NominalValueProvided(){return fT0NominalValuesProvided;}

  Int_t GetLAVT0CorrectionInputMode(){return fLAVT0CorrectionInputMode;}
  TString GetLAVT0CorrectionFileName(){return fLAVT0CorrectionFileName;}
  void    SetLAVT0CorrectionFileName(TString val){fLAVT0CorrectionFileName = val;}
  Double_t GetLAVT0CorrectionValue(Int_t);

  Bool_t CheckLAVDataSlewingCorrectionParametersProvided(){return fLAVDataSlewingCorrectionParametersProvided;}
  Bool_t CheckLAVMCSlewingCorrectionParametersProvided(){return fLAVMCSlewingCorrectionParametersProvided;}

  Double_t GetLAVDataSlewingCorrectionParameter(Int_t,Int_t);
  Double_t GetLAVMCSlewingCorrectionParameter(Int_t,Int_t);
  Double_t GetLAVMCToFLayer(Int_t,Int_t);

  Bool_t CheckLAVThresholdNominalValuesProvided(){return fThresholdNominalValuesProvided;}

  Int_t GetLAVThresholdInputMode(){return fLAVThresholdInputMode;}
  TString GetLAVThresholdFileName(){return fLAVThresholdFileName;}
  Double_t GetLAVThresholdLowValue(){return fLAVThresholdLowValue;}
  Double_t GetLAVThresholdHighValue(){return fLAVThresholdHighValue;}
  Double_t GetLAVHysteresisValue(){return fLAVHysteresisValue;}

  Int_t GetLAVClusterMethod(){return fMakeLAVClusters;}
  Int_t GetLAVTrackMethod(){return fMakeLAVTracks;}
  Int_t GetLAVSlewingCorrectionMethod(){return fLAVSlewingCorrections;}
  Int_t GetMCHitHistoLevel(){return fMakeMCHitHistos;}
  Int_t GetDigiHistoLevel(){return fMakeDigiHistos;}
  Int_t GetRecoHistoLevel(){return fMakeRecoHistos;}
  Int_t GetPreferredTimeAlgorithm(){return fPreferredTimeAlgorithm;}

  void ParseConfFile(TString);

private:
  static LAVDataCardMessenger* fInstance;

  Bool_t fT0NominalValuesProvided;
  Bool_t fT0FileNameProvided;
  Bool_t fThresholdNominalValuesProvided;
  Bool_t fThresholdFileNameProvided;

  Bool_t fMakeLAVT0s;
  Int_t fLAVT0CorrectionInputMode; // 1 --> read from file; 2 --> read from db
  TString fLAVT0CorrectionFileName; // T0 filename
  Double_t fLAVT0CorrectionValues[12]; // T0 nominal values for each station from data card
  Double_t fStationsMCToF[5][12]; // T0's layer by layer for MC

  Int_t fLAVThresholdInputMode; // 1 --> read from file; 2 --> read from db
  TString fLAVThresholdFileName; // threshold filename
  Double_t fLAVThresholdLowValue;
  Double_t fLAVThresholdHighValue;
  Double_t fLAVHysteresisValue;

  Int_t fLAVSlewingCorrections; // 1 --> apply residual LAV slewing corrections
  Int_t fLAVDataSlewingCorrectionParametersProvided; // 1 --> Slewing parameters provided to be used for Data
  Int_t fLAVMCSlewingCorrectionParametersProvided; // 1 --> Slewing parameters provided to be used for MC
  Int_t fMakeLAVClusters; // 1 --> make LAV Clusters
  Int_t fMakeLAVTracks;   // 1 --> make LAV tracks
  Int_t fMakeMCHitHistos; // 0 --> no histos; 1 --> make general histos; 2 --> make detailed histos
  Int_t fMakeDigiHistos; //  0 --> no histos; 1 --> make general histos; 2 --> make detailed histos
  Int_t fMakeRecoHistos; //  0 --> no histos; 1 --> make general histos; 2 --> make detailed histos
  Int_t fPreferredTimeAlgorithm; // 0 --> prefer slope; 1 --> prefer tot_low; 2 --> prefer tot_high

  Double_t fDataSlewingPars[2][5];
  Double_t fMCSlewingPars[2][5];

  // cppcheck-suppress unusedPrivateFunction
  void Print();

};

#endif
