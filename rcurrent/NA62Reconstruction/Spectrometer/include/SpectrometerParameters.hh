#ifndef SpectrometerParameters_H
#define SpectrometerParameters_H 1

#include "InfoWriter.h"
#include "TVector3.h"
#include "TROOT.h"

class SpectrometerParameters {

public:

  SpectrometerParameters();
  static SpectrometerParameters* GetInstance();

private:

  static SpectrometerParameters* fInstance; ///< Instance of the class.

public:
  void ParseConfFile(TString ConfFileName);

  Bool_t GetIsRawData() { return fIsRawData; };
  Int_t GetRunNumber() { return fRunNumber; };
  Bool_t GetIsMuonRun() { return fIsMuonRun; };
  TString GetT0FileName() { return fT0FileName; }; 
  TString GetAlignmentFileName() { return fAlignmentFileName; }; 
  TString GetStationsAlignmentFileName() { return fStationsAlignmentFileName; }; 
  TString GetZCorrectionFileName() { return fZCorrectionFileName; }; 
  TString GetBxIntegralFileName() { return fBxIntegralFileName; }; 
  TString GetByIntegralFileName() { return fByIntegralFileName; }; 
  TString GetBMNP33FileName() { return fBMNP33FileName; }; 
  TString GetBFringeFileName() { return fBFringeFileName; }; 
  TString GetRTFileName() { return fRTFileName; }; 
  TString GetXTFileName() { return fXTFileName; }; 
  Double_t GetNullCoordinate() {return fNullCoordinate;};
  Double_t GetEC() {return fEC;};
  Int_t GetNChambers() {return fNChambers;};
  Int_t GetNViews() {return fNViews;};
  Int_t GetNPlanes() {return fNPlanes;};
  Int_t GetNStraws() {return fNStraws;};
  Double_t GetTWindowMin() {return fTWindowMin;};
  Double_t GetTWindowMax() {return fTWindowMax;};
  Double_t GetMaxHitPlane() {return fMaxHitPlane;};
  Double_t GetTDriftMax()          { return fTDriftMax;          };
  Double_t GetTTrailingCut2Hits()  { return fTTrailingCut2Hits;  };
  Double_t GetTTrailingSigma3Hits(){ return fTTrailingSigma3Hits;};
  Double_t GetTTrailingSigma2Hits(){ return fTTrailingSigma2Hits;};
  Double_t GetRadiusSum3HitsMin()  { return fRadiusSum3HitsMin;  };
  Double_t GetRadiusSum3HitsMax()  { return fRadiusSum3HitsMax;  };
  Double_t GetRadiusSum2HitsMax()  { return fRadiusSum2HitsMax;  };
  Double_t GetDeltaStrawPositionMax() {return fDeltaStrawPositionMax;};
  Double_t GetChi2Triplet() {return fChi2Triplet;};
  Double_t GetTripletParameter1() {return fTripletParameter1;};
  Double_t GetTripletParameter2() {return fTripletParameter2;};
  Double_t GetTripletParameter3() {return fTripletParameter3;};
  Double_t GetTripletParameter4() {return fTripletParameter4;};
  Double_t GetPlaneStaggering() {return fPlaneStaggering;}; 
  Double_t GetHitSlopeMax() {return fHitSlopeMax;};
  Double_t GetNSigmaSlopeCut() {return fNSigmaSlopeCut;};
  Double_t GetDoubletParameter1() {return fDoubletParameter1;};
  Double_t GetDoubletParameter2() {return fDoubletParameter2;};
  Double_t GetDoubletParameter3() {return fDoubletParameter3;};
  Double_t GetInterTtrailingCut() {return fInterTtrailingCut;};
  Double_t GetInterDistanceCut() {return fInterDistanceCut;};
  Double_t GetInterQuality4ViewsCut1() {return fInterQuality4ViewsCut1;}; 
  Double_t GetInterQuality4ViewsCut2() {return fInterQuality4ViewsCut2;};
  Double_t GetInterQuality4ViewsCut3() {return fInterQuality4ViewsCut3;};
  Double_t GetInterQuality4ViewsCut4() {return fInterQuality4ViewsCut4;};
  Double_t GetInter4ViewsTtrailingSigma() {return fInter4ViewsTtrailingSigma;};
  Double_t GetInterQuality3ViewsCutXYU() {return fInterQuality3ViewsCutXYU;};
  Double_t GetInterQuality3ViewsCutXYV() {return fInterQuality3ViewsCutXYV;};
  Double_t GetInterQuality3ViewsCutXUV() {return fInterQuality3ViewsCutXUV;};
  Double_t GetInterQuality3ViewsCutYUV() {return fInterQuality3ViewsCutYUV;};
  Double_t GetInter3ViewsTtrailingSigma() {return fInter3ViewsTtrailingSigma;};
  Double_t GetCombinationXCorridor0() {return fCombinationXCorridor0;}; 
  Double_t GetCombinationXCorridor1() {return fCombinationXCorridor1;}; 
  Double_t GetCombinationXCorridor2() {return fCombinationXCorridor2;}; 
  Double_t GetCombinationXCorridor3() {return fCombinationXCorridor3;}; 
  Double_t GetCombinationYCorridor0() {return fCombinationYCorridor0;}; 
  Double_t GetCombinationYCorridor1() {return fCombinationYCorridor1;}; 
  Double_t GetCombinationYCorridor2() {return fCombinationYCorridor2;}; 
  Double_t GetCombinationYCorridor3() {return fCombinationYCorridor3;}; 
  Double_t Get3HitsCombRadiusAtMC() {return f3HitsCombRadiusAtMC;};
  Double_t GetMaxMomentum() {return fMaxMomentum;};
  Double_t GetMaxAngle() {return fMaxAngle;};
  Double_t GetCombQualityCut() {return fCombQualityCut;};
  Double_t GetCombHoughDeltaCut() {return fCombHoughDeltaCut;}; 
  Double_t GetGuessAverageSigma() {return fGuessAverageSigma;};
  Double_t GetGuessSingletSigma() {return fGuessSingletSigma;};
  Double_t GetZRef() {return fZRef;};
  Double_t GetXX0perPlane() {return fXX0perPlane;};
  Double_t GetCutDyFit(Int_t val) {return fCutDyFit[val];};
  Int_t GetHistoDebug() {return fHistoDebug;};
  Int_t GetRTMode() {return fRTMode;};
  Int_t GetXTMode() {return fXTMode;};
  Double_t*  GetChambersMCToF() { return &fChambersMCToF[0];   };
  Double_t*  GetViewsMCToF()    { return &fViewsMCToF[0];      };
  Double_t*  GetPlanesMCToF()   { return &fPlanesMCToF[0];     };

  void SetIsRawData(Bool_t val) { fIsRawData=val; };
  void SetRunNumber(Int_t val) { fRunNumber=val; };
  void SetIsMuonRun(Int_t val) { fIsMuonRun=val; };
  void SetT0FileName(TString val) { fT0FileName=val; }; 
  void SetAlignmentFileName(TString val) { fAlignmentFileName=val; }; 
  void SetStationsAlignmentFileName(TString val) { fStationsAlignmentFileName=val; }; 
  void SetZCorrectionFileName(TString val) { fZCorrectionFileName=val; }; 
  void SetBxIntegralFileName(TString val) { fBxIntegralFileName=val; }; 
  void SetByIntegralFileName(TString val) { fByIntegralFileName=val; }; 
  void SetBMNP33FileName(TString val) { fBMNP33FileName=val; }; 
  void SetBFringeFileName(TString val) { fBFringeFileName=val; }; 
  void SetRTFileName(TString val) { fRTFileName=val; }; 
  void SetXTFileName(TString val) { fXTFileName=val; }; 
  void SetNullCoordinate(Double_t val) {fNullCoordinate=val;};
  void SetEC(Double_t val) {fEC=val;};
  void SetNChambers(Int_t val) {fNChambers=val;};
  void SetNViews(Int_t val) {fNViews=val;};
  void SetNPlanes(Int_t val) {fNPlanes=val;};
  void SetNStraws(Int_t val) {fNStraws=val;};
  void SetTWindowMin(Double_t val) {fTWindowMin=val;};
  void SetTWindowMax(Double_t val) {fTWindowMax=val;};
  void SetMaxHitPlane(Double_t val) {fMaxHitPlane=val;};
  void SetTDriftMax(Double_t val)          { fTDriftMax=val;          };
  void SetTTrailingCut2Hits(Double_t val)  { fTTrailingCut2Hits=val;  };
  void SetTTrailingSigma3Hits(Double_t val){ fTTrailingSigma3Hits=val;};
  void SetTTrailingSigma2Hits(Double_t val){ fTTrailingSigma2Hits=val;};
  void SetRadiusSum3HitsMin(Double_t val)  { fRadiusSum3HitsMin=val;  };
  void SetRadiusSum3HitsMax(Double_t val)  { fRadiusSum3HitsMax=val;  };
  void SetRadiusSum2HitsMax(Double_t val)  { fRadiusSum2HitsMax=val;  };
  void SetDeltaStrawPositionMax(Double_t val) {fDeltaStrawPositionMax=val;};
  void SetChi2Triplet(Double_t val) {fChi2Triplet=val;};
  void SetTripletParameter1(Double_t val) {fTripletParameter1=val;};
  void SetTripletParameter2(Double_t val) {fTripletParameter2=val;};
  void SetTripletParameter3(Double_t val) {fTripletParameter3=val;};
  void SetTripletParameter4(Double_t val) {fTripletParameter4=val;};
  void SetDoubletParameter1(Double_t val) {fDoubletParameter1=val;};
  void SetDoubletParameter2(Double_t val) {fDoubletParameter2=val;};
  void SetDoubletParameter3(Double_t val) {fDoubletParameter3=val;};
  void SetPlaneStaggering(Double_t val) {fPlaneStaggering=val;}; 
  void SetHitSlopeMax(Double_t val) {fHitSlopeMax=val;};
  void SetNSigmaSlopeCut(Double_t val) {fNSigmaSlopeCut=val;};
  void SetInterTtrailingCut(Double_t val) {fInterTtrailingCut=val;};
  void SetInterDistanceCut(Double_t val) {fInterDistanceCut=val;};
  void SetInterQuality4ViewsCut1(Double_t val) {fInterQuality4ViewsCut1=val;};
  void SetInterQuality4ViewsCut2(Double_t val) {fInterQuality4ViewsCut2=val;};
  void SetInterQuality4ViewsCut3(Double_t val) {fInterQuality4ViewsCut3=val;};
  void SetInterQuality4ViewsCut4(Double_t val) {fInterQuality4ViewsCut4=val;};
  void SetInter4ViewsTtrailingSigma(Double_t val) {fInter4ViewsTtrailingSigma=val;};
  void SetInterQuality3ViewsCutXYU(Double_t val) {fInterQuality3ViewsCutXYU=val;};
  void SetInterQuality3ViewsCutXYV(Double_t val) {fInterQuality3ViewsCutXYV=val;};
  void SetInterQuality3ViewsCutXUV(Double_t val) {fInterQuality3ViewsCutXUV=val;};
  void SetInterQuality3ViewsCutYUV(Double_t val) {fInterQuality3ViewsCutYUV=val;};
  void SetInter3ViewsTtrailingSigma(Double_t val) {fInter3ViewsTtrailingSigma=val;};
  void SetCombinationXCorridor0(Double_t val) {fCombinationXCorridor0=val;};
  void SetCombinationXCorridor1(Double_t val) {fCombinationXCorridor1=val;};
  void SetCombinationXCorridor2(Double_t val) {fCombinationXCorridor2=val;};
  void SetCombinationXCorridor3(Double_t val) {fCombinationXCorridor3=val;};
  void SetCombinationYCorridor0(Double_t val) {fCombinationYCorridor0=val;};
  void SetCombinationYCorridor1(Double_t val) {fCombinationYCorridor1=val;};
  void SetCombinationYCorridor2(Double_t val) {fCombinationYCorridor2=val;};
  void SetCombinationYCorridor3(Double_t val) {fCombinationYCorridor3=val;};
  void Set3HitsCombRadiusAtMC(Double_t val) {f3HitsCombRadiusAtMC=val;};
  void SetMaxMomentum(Double_t val) {fMaxMomentum=val;};
  void SetMaxAngle(Double_t val) {fMaxAngle=val;};
  void SetCombQualityCut(Double_t val) {fCombQualityCut=val;};
  void SetCombHoughDeltaCut(Double_t val) {fCombHoughDeltaCut=val;};
  void SetGuessAverageSigma(Double_t val) {fGuessAverageSigma=val;}; 
  void SetGuessSingletSigma(Double_t val) {fGuessSingletSigma=val;};
  void SetZRef(Double_t val) {fZRef=val;}; 
  void SetXX0perPlane(Double_t val) {fXX0perPlane=val;}; 
  void SetCutDyFit(Int_t j, Double_t val) {fCutDyFit[j]=val;};
  void SetHistoDebug(Int_t val) {fHistoDebug=val;};
  void SetRTMode(Int_t val) {fRTMode=val;};
  void SetXTMode(Int_t val) {fXTMode=val;};
  void SetChambersMCToF(Int_t j, Double_t val) {fChambersMCToF[j]=val;};
  void SetViewsMCToF(Int_t j, Double_t val)    {fViewsMCToF[j]=val;};
  void SetPlanesMCToF(Int_t j, Double_t val)   {fPlanesMCToF[j]=val;};

  // Timing and RT
  Double_t* GetT0() {return fT0;};
  Double_t GetT0(Int_t pid, Int_t chid) {return fChannelT0[pid][chid];};
  Double_t GetXAlignment(Int_t chid, Int_t vid) {return fXAlignment[chid][vid];};
  Double_t GetXStrawAlignment(Int_t pid, Int_t chid) {return fXStrawAlignment[pid][chid];};
  Double_t GetZMagnetCorrection() {return fZMagnetCorrection;};
  Double_t GetZChamberCorrection(Int_t j) {return fZChamberCorrection[j];};
  Double_t GetBxIntegral(Int_t j) {return fBxIntegral[j];};
  Double_t GetByIntegral(Int_t j) {return fByIntegral[j];};
  TVector3 GetBMNP33(Int_t jx, Int_t jy, Int_t jz) {return TVector3(fBxMNP33[jx][jy][jz],fByMNP33[jx][jy][jz],fBzMNP33[jx][jy][jz]);};
  Double_t GetZMNP33(Int_t j) {return fZMNP33[j];};
  TVector3 GetBFringe(Int_t jx, Int_t jy, Int_t jz) {return TVector3(fBxFringe[jx][jy][jz],fByFringe[jx][jy][jz],fBzFringe[jx][jy][jz]);};
  Double_t GetZFringe(Int_t j) {return fZFringe[j];};
  //Float_t GetRTH(Int_t pid, Int_t chid, Int_t bin) {return fRTH[pid*chid][bin];};
  void SetT0(Int_t j, Double_t val)            {fT0[j]=val;};
  void ReadMagicT0();
  void DefineT0(Int_t n) {fT0 = new Double_t[n];};
  void SetT0(Int_t j);
  void SetAlignment();
  void SetZPositionCorrection();
  void SetBIntegral();
  void SetBMNP33();
  void SetBFringeField();
  void FillFringeField(Int_t *,Double_t,Double_t);
  void SetRT();
  void SetXT();

  Double_t GetRTDependence(Double_t);
  Double_t GetRTDependenceData(Double_t);
  Double_t GetRTParametricDependence(Double_t);
  Double_t GetRTDependenceDataFull(Int_t,Double_t,Double_t); 
  Double_t GetXTDependence(Int_t,Double_t); 
  //////////
  Bool_t   IsBadChannel(Int_t,Int_t);

  Float_t              GetMaxTime()                                       { return fMaxTime;                      };
  void                 SetMaxTime(Float_t value)                          { fMaxTime = value;                     };
  Float_t              GetTimeStep()                                      { return fTimeStep;                     };
  void                 SetTimeStep(Float_t value)                         { fTimeStep = value;                    };
  Float_t              GetStrawT0()                                       { return fStrawT0;                      };
  void                 SetStrawT0(Float_t value)                          { fStrawT0 = value;                     };
  Float_t              GetNClustersPermm()                                { return fNClustersPermm;               };
  void                 SetNClustersPermm(Float_t value)                   { fNClustersPermm = value;              };
  Float_t              GetDiscrSetTime()                                  { return fDiscrSetTime;                 };
  void                 SetDiscrSetTime(Float_t value)                     { fDiscrSetTime = value;                };
  Float_t              GetEdgeDeadTime()                                  { return fEdgeDeadTime;                 };
  void                 SetEdgeDeadTime(Float_t value)                     { fEdgeDeadTime = value;                };
  Float_t              GetSameEdgeDeadTime()                              { return fSameEdgeDeadTime;             };
  void                 SetSameEdgeDeadTime(Float_t value)                 { fSameEdgeDeadTime = value;            };
  Float_t              GetThreshold()                                     { return fThreshold;                    };
  void                 SetThreshold(Float_t value)                        { fThreshold = value;                   };
  Float_t              GetGain()                                          { return fGain;                         };
  void                 SetGain(Float_t value)                             { fGain = value;                        };
  Float_t              GetCARIOCASlope()                                  { return fCARIOCASlope;                 };
  void                 SetCARIOCASlope(Float_t value)                     { fCARIOCASlope = value;                };
  Float_t              GetEquivalentNoiseCharge()                         { return fEquivalentNoiseCharge;        };
  void                 SetEquivalentNoiseCharge(Float_t value)            { fEquivalentNoiseCharge = value;       };
  Float_t              GetIonizationEnergy()                              { return fIonizationEnergy;             };
  void                 SetIonizationEnergy(Float_t value)                 { fIonizationEnergy = value;            };
  Float_t              GetNTotalPermm()                                   { return fNTotalPermm;                  };
  void                 SetNTotalPermm(Float_t value)                      { fNTotalPermm = value;                 };
  Bool_t               GetNoiseSimu()                                     { return fNoiseSimu;                    };
  void                 SetNoiseSimu(Bool_t value)                         { fNoiseSimu = value;                   };
  Bool_t               GetSavePulseShapes()                               { return fSavePulseShapes;              };
  void                 SetSavePulseShapes(Bool_t value)                   { fSavePulseShapes = value;             };
  Bool_t               GetNoBulkHits()                                    { return fNoBulkHits;                   };
  void                 SetNoBulkHits(Bool_t value)                        { fNoBulkHits = value;                  };
  Double_t             GetMagicT0()                                       { return fMagicT0;                      };
  void                 SetMagicT0(Double_t value)                         { fMagicT0 = value;                     };
  Bool_t               GetMagicT0ScanEnabled()                            { return fMagicT0ScanEnabled;           };
  void                 SetMagicT0ScanEnabled(Bool_t value)                { fMagicT0ScanEnabled = value;          };
  TString              GetMagicT0FileName()                               { return fMagicT0FileName;              };
  void                 SetMagicT0FileName(TString value)                  { fMagicT0FileName = value;             };
  TString              GetTimeReference()                                 { return fTimeReference;                };
  void                 SetTimeReference(TString value)                    { fTimeReference = value;               };

private:

  Bool_t  fIsRawData;
  Double_t fRunNumber;
  TString fTimeReference;
  TString fT0FileName;
  TString fAlignmentFileName;
  TString fStationsAlignmentFileName;
  TString fZCorrectionFileName;
  TString fBxIntegralFileName;
  TString fByIntegralFileName;
  TString fBMNP33FileName;
  TString fBFringeFileName;
  TString fRTFileName;
  TString fXTFileName;
  Double_t fNullCoordinate;
  Double_t fEC; ///< e*c
  Int_t fNChambers; ///< Total number of chambers.
  Int_t fNViews; ///< Total number of views per chamber.
  Int_t fNPlanes; ///< Total number of planes per view.
  Int_t fNStraws; ///< Maximum number of straws per plane.
  Double_t fTWindowMin; ///< Minimum T of accepted hits.
  Double_t fTWindowMax; ///< Maximum T of accepted hits.
  Double_t fMaxHitPlane; ///< Maximum hits per plane.
  Double_t fTDriftMax;           ///< Maximum drift time.
  Double_t fTTrailingCut2Hits;   ///< Trailing time vut for each 2-hit cluster.
  Double_t fTTrailingSigma3Hits; ///< Sigma of the average trailing time of 3-hit clusters.
  Double_t fTTrailingSigma2Hits; ///< Sigma of the average trailing time of 2-hit clusters.
  Double_t fRadiusSum3HitsMin;   ///< Minimum value of the sum of the radius of the 3 hits in a 3-hit cluster.
  Double_t fRadiusSum3HitsMax;   ///< Maximum value of the sum of the radius of the 3 hits in a 3-hit cluster.
  Double_t fRadiusSum2HitsMax;   ///< Maximum value of the sum of the radius of the 3 hits in a 3-hit cluster.
  Double_t fDeltaStrawPositionMax; ///< Maximum difference allowed in the LR algorithm between the straw position of the reference hit and the one of the next hit.  
  Double_t fChi2Triplet; ///< \f$\chi^{2}\f$ of the fitted triplets of tubes.
  Double_t fTripletParameter1; ///< Minimum radius defining the triplet region.
  Double_t fTripletParameter2; ///< Maximum radius defining the triplet region.
  Double_t fTripletParameter3; ///< Radius above which the radius assignment has the same sign in two compatible tubes of the same view.
  Double_t fTripletParameter4; ///< Tolerance for the previous parameter in case of 2-hits.
  Double_t fDoubletParameter1; ///< Number of sigma of the time chi2 cut for 2-hit cluster reconstruction. 
  Double_t fDoubletParameter2; ///< Number of sigma of the sumwire chi2 cut for 2-hit cluster reconstruction. 
  Double_t fDoubletParameter3; ///< Maximum chi2-like of sumwire+time for 2-hit cluster reconstruction (3-hit cluster-like).
  Double_t fPlaneStaggering; ///< Staggering between two half-views. Used for the pattern recognition of triplets. 
  Double_t fHitSlopeMax; ///< Maximum track slope allowed from the reconstructed hits in one view.
  Double_t fNSigmaSlopeCut; ///< Number of sigma on the slope to cut on the maximum slope.
  Double_t fInterTtrailingCut; ///< Maximum T trailing difference between 2 clusters belonging to different views.
  Double_t fInterDistanceCut; ///< Max distance between 2 intersections.
  Double_t fInterQuality4ViewsCut1; ///< Quality cut for 4 views intersection.
  Double_t fInterQuality4ViewsCut2; ///< Quality cut for 4 views intersection.
  Double_t fInterQuality4ViewsCut3; ///< Quality cut for 4 views intersection.
  Double_t fInterQuality4ViewsCut4; ///< Quality cut for 4 views intersection.
  Double_t fInter4ViewsTtrailingSigma; ///< Average trailing time resolution in 4-view chamber-hits. 
  Double_t fInterQuality3ViewsCutXYU; ///<Quality cut for 3 views intersection (XYU).
  Double_t fInterQuality3ViewsCutXYV; ///<Quality cut for 3 views intersection (XYV).
  Double_t fInterQuality3ViewsCutXUV; ///<Quality cut for 3 views intersection (XUV).
  Double_t fInterQuality3ViewsCutYUV; ///<Quality cut for 3 views intersection (YUV).
  Double_t fInter3ViewsTtrailingSigma; ///< Average trailing time resolution in 3-view chamber-hits.
  Double_t fCombinationXCorridor0;
  Double_t fCombinationXCorridor1;
  Double_t fCombinationXCorridor2;
  Double_t fCombinationXCorridor3;
  Double_t fCombinationYCorridor0;
  Double_t fCombinationYCorridor1;
  Double_t fCombinationYCorridor2;
  Double_t fCombinationYCorridor3;
  Double_t f3HitsCombRadiusAtMC;
  Double_t fMaxMomentum; ///< Maximum momentum for combination reconstruction in TrackCollector. 
  Double_t fMaxAngle; ///< Maximum slope for combination reconstruction in TrackCollector.
  Double_t fCombQualityCut;
  Double_t fCombHoughDeltaCut;
  Double_t fGuessAverageSigma; ///< Estimated average sigma on radius measurement.
  Double_t fGuessSingletSigma; ///< Guessed sigma to test the hits in the Kalman filter for pattern recognition (clusters without slope measurement; used in TrackCollector). 
  Double_t fZRef; ///< Longitudinal position of the reference plane for the final track fit.
  Double_t fXX0perPlane; ///< Estimated X/X0 per single plane of straws.
  Double_t fCutDyFit[4]; ///< Cut on difference between measured and fitted point per chamber.
  Int_t fHistoDebug; ///< Flag to switch on and off the histograms for monitoring.
  Bool_t fIsMuonRun;
  Int_t fRTMode;
  Int_t fXTMode;

  // Timing offsets
  Double_t fMagicT0;
  Bool_t   fMagicT0ScanEnabled;
  TString  fMagicT0FileName;
  Double_t fChambersMCToF[4];
  Double_t fViewsMCToF[20];
  Double_t fPlanesMCToF[20];
  Double_t *fT0;
  Double_t fChannelT0[64][123];
  Double_t fXAlignment[4][4];
  Double_t fXStrawAlignment[64][123];
  Double_t fZMagnetCorrection;
  Double_t fZChamberCorrection[4];
  Double_t fBxIntegral[702];
  Double_t fByIntegral[702];
  Double_t fBxMNP33[26][26][169];
  Double_t fByMNP33[26][26][169];
  Double_t fBzMNP33[26][26][169];
  Double_t fZMNP33[169];
  Double_t fby[30][29];
  Double_t fbx[30][29];
  Double_t fbz[30][29];
  Double_t fBxFringe[17][17][30];
  Double_t fByFringe[17][17][30];
  Double_t fBzFringe[17][17][30];
  Double_t fZFringe[30];
  Float_t fRTH[64*123][13];
  Float_t fXTH[16][7];

  // Parameters for full digitization
  Float_t fMaxTime;
  Float_t fTimeStep;
  Float_t fStrawT0;
  Float_t fNClustersPermm;
  Float_t fDiscrSetTime;
  Float_t fEdgeDeadTime;
  Float_t fSameEdgeDeadTime;
  Float_t fThreshold;
  Float_t fGain;
  Float_t fCARIOCASlope; 
  Float_t fEquivalentNoiseCharge; 
  Float_t fIonizationEnergy;
  Float_t fNTotalPermm;
  Bool_t fNoiseSimu;
  Bool_t fSavePulseShapes;
  Bool_t fNoBulkHits;

};
#endif
