#ifndef MUV3Digitizer_H
#define MUV3Digitizer_H 1

#include "TMUV3Hit.hh"
#include "TMUV3Digi.hh"
#include "TMUV3Event.hh"
#include "TDCBRawDecoder.hh"
#include "MUV3Geometry.hh"
#include "MUV3Reconstruction.hh"
#include "TDCEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "TString.h"
#include "TRegexp.h"
#include "NA62VDigitizer.hh"

class MUV3Digitizer : public NA62VDigitizer {

public:

  explicit MUV3Digitizer(NA62VReconstruction*);
  virtual ~MUV3Digitizer() {}
  virtual TDetectorVEvent* ProcessEvent(TDetectorVEvent *);
  void ParseConfFile(TString FileName);
  void ReadReferenceSignalShape(TString FileName);
  void ReadLightCollectionMatrices(TString FileName1, TString FileName2);
  void PrintParameters();
  void RunSimpleDigitizer(TMUV3Event*);
  void RunDetailedDigitizer(TMUV3Event*);
  void IncrementAnalogSignal(Double_t nPhotoElectrons, Double_t Time);
  void DoCFD();
  Double_t CollectionEfficiency (Int_t PM_no, Double_t x, Double_t y, Int_t iCell);
  Double_t GetCollectionTime (Int_t iPM, Double_t x, Double_t y, Int_t iCell);

private:

  Int_t fMode; ///< Simple (1) or detailed (2) digitizer?

  TString fLightCollectionMatrixAFileName, fLightCollectionMatrixBFileName, fSignalShapeFileName;

  Double_t fEnergyDepositThreshold; ///< Energy deposition threshold to generate a Digi [MeV]
  Double_t fChannelTimeResolution;
  Double_t fChannelMergeThreshold;

  MUV3Geometry *fGeo;
  Int_t    fHitChannels[400];
  Int_t    fNBins;
  Int_t    fNBinCFDDelay;
  Int_t    fNBinCFDPulseWidth;
  Int_t    fHistoFlag;
  Int_t    fDebugFlag;
  Int_t    fCFDDebugFlag;
  Double_t fTransitTime[2][152];
  Double_t fCollectionMatrix[2][22][22];

  Int_t    fNBinsShape;
  Double_t fShape[1000];

  Double_t* fAnalogSignal;
  Double_t fStartTime;
  Double_t fEndTime;
  Double_t fTimeInterval;
  Double_t fDeltaTime;
  Double_t fTransitTimeMean;
  Double_t fTransitTimeSpread;
  Double_t fQuantumEff;
  Double_t fCableDelay;
  Double_t fCFDDelay;
  Double_t fCFDAttenuation;
  Double_t fCFDThreshold;
  Double_t fCFDPulseWidth;
  Double_t fPhotonsPerMeV;
  struct TDCTime {
    Double_t LeadingTime;
    Double_t TrailingTime;
  };
  std::vector<TDCTime> Times;
};

#endif
