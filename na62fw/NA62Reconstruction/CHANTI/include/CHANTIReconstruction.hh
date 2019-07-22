//
// Vito Palladino
//


#ifndef CHANTIReconstruction_H
#define CHANTIReconstruction_H 1

#include "NA62VReconstruction.hh"
#include "CHANTIGeometry.hh"
#include "TDCEvent.hh"

#include "Event.hh"

#include "TH1.h"
#include "TH2.h"
#include "TGraph.h"

#include "vector"

class CHANTIReconstruction : public NA62VReconstruction
{

public:

  CHANTIReconstruction(TFile*, TString);
  ~CHANTIReconstruction();
  void ParseConfFile(TString);
  void ReadChannelInfo();
  //void EvaluateT0s();

  virtual void Init(NA62VReconstruction*);
  virtual void StartOfBurst();
  virtual void EndOfBurst();

  virtual TRecoVEvent* ProcessEvent(TDetectorVEvent* , Event*);
  virtual TDetectorVEvent* Trigger(TDetectorVEvent* tDetEvent, Event* tEvent);
  virtual void EndProcessing();
  virtual void FillTimes(Double_t);

  void InitHistograms();  
  void SaveHistograms();
  void ResetHistograms();
  void DeleteHistograms();
  Double_t* GetRingsMCToF()    						{ return &fRingsMCToF[0];      };
  Double_t GetSlopeAmplNph()                        { return fSlopeAmplNph;                         };
  Double_t GetSigmaAmplSPE()                        { return fSigmaAmplSPE;                         };
  Int_t GetNbinEnergy()                             { return fNbinEnergy;                           };
  Double_t GetMeanNfr()                             { return fMeanNfr;                              };
  Double_t GetSigmaNfr()                            { return fSigmaNfr;                             };
  Double_t GetHysteresis()                          { return fHysteresis;                           };
  Double_t GetTauFall()                             { return fTauFall;                              };	
  Double_t GetFiberLightSpeed()                     { return fFiberLightSpeed;                      };
  Double_t GetReflectionFactorBoard() 				{ return fReflectionFactorBoard;				};
  Double_t GetSlopeEnergyNph()						{ return fSlopeEnergyNph;						};
  Double_t GetOffsetEnergyNph()						{ return fOffsetEnergyNph;						};
  Double_t* GetPhotonsNumber()                      { return fPhotonsNumber;                        };
  Double_t* GetEnergyEdge()                         { return fEnergyEdge;                           }; 
  Double_t GetCutOffFrequencyBoard()				{ return fCutOffFrequency;						};
  Double_t GetZeroFrequencyBoard()					{ return fZeroFrequency;						};


  Int_t GetNTriggersPerBurst()                      { return fNTriggersPerBurst;                    };
  Int_t GetNPlanes()                                { return fNPlanes;                              };
  Int_t GetNRings()                                 { return fNRings;                               };
  Double_t GetBurstLength()                         { return fBurstLength;                          };
 
  TH1D ** GetHNHitsForRing()                        { return fHNHitsForRing;                        }; 
  TH1D ** GetHToTRing0()                            { return fHToTRing0;                            };
  TH1D ** GetHToTRing1()                            { return fHToTRing1;                            };
  TH1D ** GetHToTRing2()                            { return fHToTRing2;                            };
  TH1D*** GetHPositionLow()                         { return fHPositionLow;                         };
  TH1D *  GetHPositionLow(Int_t iSt,Int_t iTrig)    { return fHPositionLow[iSt][iTrig];             };
  TH1D*** GetHPositionHigh()                        { return fHPositionHigh;                        };
  TH1D *  GetHPositionHigh(Int_t iSt,Int_t iTrig)   { return fHPositionHigh[iSt][iTrig];            };
  TH1D ** GetHPosition1()                           { return fHPosition1;                           };
  TH1D ** GetHPosition2()                           { return fHPosition2;                           };
  TH1D ** GetHEffi1Up()                             { return fHEffi1Up;                             };
  TH1D ** GetHEffi2Up()                             { return fHEffi2Up;                             };
  TH1D ** GetHEffi1Down()                           { return fHEffi1Down;                           };
  TH1D ** GetHEffi2Down()                           { return fHEffi2Down;                           };
  TH1D ** GetHEffiPosition1Up()                     { return fHEffiPosition1Up;                     };
  TH1D ** GetHEffiPosition2Up()                     { return fHEffiPosition2Up;                     };
  TH1D ** GetHEffiPosition1Down()                   { return fHEffiPosition1Down;                   };
  TH1D ** GetHEffiPosition2Down()                   { return fHEffiPosition2Down;                   };
  TH1D ** GetHEffiHigh1Up()                         { return fHEffiHigh1Up;                         };
  TH1D ** GetHEffiHigh2Up()                         { return fHEffiHigh2Up;                         };
  TH1D ** GetHEffiHigh1Down()                       { return fHEffiHigh1Down;                       };
  TH1D ** GetHEffiHigh2Down()                       { return fHEffiHigh2Down;                       };
  TH1D ** GetHEffiPositionHigh1Up()                 { return fHEffiPositionHigh1Up;                 };
  TH1D ** GetHEffiPositionHigh2Up()                 { return fHEffiPositionHigh2Up;                 };
  TH1D ** GetHEffiPositionHigh1Down()               { return fHEffiPositionHigh1Down;               };
  TH1D ** GetHEffiPositionHigh2Down()               { return fHEffiPositionHigh2Down;               };
  TH1D ** GetHBarID()                               { return fHBarID;                               };
  TH1D *  GetHBarID(Int_t i)                        { return fHBarID[i];                            };
  TH1D ** GetHNHitsForPlane()                       { return fHNHitsForPlane;                     };
  TH1D ** GetHToTPlane0()                           { return fHToTPlane0;                         };
  TH1D ** GetHToTPlane1()                           { return fHToTPlane1;                         };
  TH1D ** GetHToTPlane2()                           { return fHToTPlane2;                         };
  TH1D *  GetHNTriggersPerBurst()                   { return fHNTriggersPerBurst;                   };
  TH1D *  GetHResoTime()                            { return fHResoTime;                            };
  TH1D ** GetHResoTimePlane()                       { return fHResoTimePlane;                     };
  TH1D *  GetHResoTimeMU()                          { return fHResoTimeMU;                          };
  TH1D ** GetHResoTimePlaneMU()                     { return fHResoTimePlaneMU;                   };
  TH1D *  GetHXHit()                                { return fHXHit;                                };
  TH1D *  GetHYHit()                                { return fHYHit;                                };
  TH1D *  GetHXHitEvent1()                          { return fHXHitEvent1;                          };
  TH1D *  GetHXHitEvent2()                          { return fHXHitEvent2;                          };
  TH1D *  GetHYHitEvent1()                          { return fHYHitEvent1;                          };
  TH1D *  GetHYHitEvent2()                          { return fHYHitEvent2;                          };
  TH1D *  GetHDispPlotX()                           { return fHDispPlotX;                           };
  TH1D *  GetHDispPlotY()                           { return fHDispPlotY;                           };
  TH1D ** GetHDispPlot()                            { return fHDispPlot;                            };
  TH1D ** GetHDispPlotMU()                          { return fHDispPlotMU;                          };
  TH1D *  GetHDispPlotXMU()                         { return fHDispPlotXMU;                         };
  TH1D *  GetHDispPlotYMU()                         { return fHDispPlotYMU;                         };
  TH1D *  GetHToT0()                                { return fHToT0;                                };
  TH1D *  GetHToT1()                                { return fHToT1;                                };
  TH1D *  GetHToT2()                                { return fHToT2;                                };
  TH2F *  GetHTimeVSSlot()                          { return fHTimeVSSlot;                          };
  TH2F *  GetHSlewingVSToTLow()                     { return fHSlewingVSToTLow;                     };
  TH2F *  GetHSlewingVSToTHigh()                    { return fHSlewingVSToTHigh;                    };
  TH2F *  GetHSlewingVSToTLowFit()                  { return fHSlewingVSToTLowFit;                  };
  TH2F *  GetHSlewingVSToTHighFit()                 { return fHSlewingVSToTHighFit;                 };
  TH1D *  GetHNHit()                                { return fHNHit;                                };
  TH1D *  GetHNHitAllRing()                         { return fHNHitAllRing;                         };
  TH1D ** GetHCorrTime()                            { return fHCorrTime;                            };
  TH1D *  GetHNRing()                               { return fHNRing;                               };
  TH1D *  GetHNPlane()                              { return fHNPlane;                            };
  TH1D *  GetHModeX()                               { return fHModeX;                               };
  TH1D *  GetHModeY()                               { return fHModeY;                               };
  TH2F *  GetHNRingVSNHit()                         { return fHNRingVSNHit;                         };
  TH2F *  GetHNPlaneVSNHit()                        { return fHNPlaneVSNHit;                      };
  TH1D ** GetHCorrTimeDispPlot()                    { return fHCorrTimeDispPlot;                    };
  TH1D ** GetHCorrTimeDispPlotMU()                  { return fHCorrTimeDispPlotMU;                  };
  TH1D ** GetHCorrTimeShiftMU()                     { return fHCorrTimeShiftMU;                     };
  TH2F *  GetHCorrTimeVSZ()                         { return fHCorrTimeVSZ;                         };
  TH2F *  GetHCorrTimeVSZMU()                       { return fHCorrTimeVSZMU;                       };
  TH2F*** GetHViewX1THRL()                          { return fHViewX1THRL;                          };
  TH2F *  GetHViewX1THRL(Int_t iSt,Int_t iTrig)     { return fHViewX1THRL[iSt][iTrig];              };
  TH2F*** GetHViewX2THRL()                          { return fHViewX2THRL;                          };
  TH2F *  GetHViewX2THRL(Int_t iSt,Int_t iTrig)     { return fHViewX2THRL[iSt][iTrig];              };
  TH2F*** GetHViewY1THRL()                          { return fHViewY1THRL;                          };
  TH2F *  GetHViewY1THRL(Int_t iSt,Int_t iTrig)     { return fHViewY1THRL[iSt][iTrig];              };
  TH2F*** GetHViewY2THRL()                          { return fHViewY2THRL;                          };
  TH2F *  GetHViewY2THRL(Int_t iSt,Int_t iTrig)     { return fHViewY2THRL[iSt][iTrig];              };
  TH2F*** GetHViewX1THRH()                          { return fHViewX1THRH;                          };
  TH2F *  GetHViewX1THRH(Int_t iSt,Int_t iTrig)     { return fHViewX1THRH[iSt][iTrig];              };
  TH2F*** GetHViewX2THRH()                          { return fHViewX2THRH;                          };
  TH2F *  GetHViewX2THRH(Int_t iSt,Int_t iTrig)     { return fHViewX2THRH[iSt][iTrig];              };
  TH2F*** GetHViewY1THRH()                          { return fHViewY1THRH;                          };
  TH2F *  GetHViewY1THRH(Int_t iSt,Int_t iTrig)     { return fHViewY1THRH[iSt][iTrig];              };
  TH2F*** GetHViewY2THRH()                          { return fHViewY2THRH;                          };
  TH2F *  GetHViewY2THRH(Int_t iSt,Int_t iTrig)     { return fHViewY2THRH[iSt][iTrig];              };
  TH2F *  GetHModeYvsModeXSingleMuon()              { return fHModeYvsModeXSingleMuon;              };
  TH1D *  GetHContOfModeX()                         { return fHContOfModeX;                         };
  TH1D *  GetHContOfModeX1()                        { return fHContOfModeX1;                        };
  TH1D *  GetHContOfModeX2()                        { return fHContOfModeX2;                        };
  TH1D *  GetHContOfModeX3()                        { return fHContOfModeX3;                        };
  TH1D *  GetHContOfModeY()                         { return fHContOfModeY;                         };
  TH1D *  GetHContOfModeY1()                        { return fHContOfModeY1;                        };
  TH1D *  GetHContOfModeY2()                        { return fHContOfModeY2;                        };
  TH1D *  GetHContOfModeY3()                        { return fHContOfModeY3;                        };
  TH1D ** GetHEffiPlotXDen()                        { return fHEffiPlotXDen;                        };
  TH1D ** GetHEffiPlotYDen()                        { return fHEffiPlotYDen;                        };
  TH1D ** GetHEffiPlotXNum()                        { return fHEffiPlotXNum;                        };
  TH1D ** GetHEffiPlotYNum()                        { return fHEffiPlotYNum;                        };
  TH1D ** GetHEffiPlotHighXNum()                    { return fHEffiPlotHighXNum;                    };
  TH1D ** GetHEffiPlotHighYNum()                    { return fHEffiPlotHighYNum;                    };
  TH2F ** GetHEffiPlotXYDen()                       { return fHEffiPlotXYDen;                       };
  TH2F ** GetHEffiPlotXYNum()                       { return fHEffiPlotXYNum;                       };
  TH1D ** GetHHitTimeIn()                           { return fHHitTimeIn;                           };
  TH1D ** GetHHitTimeOut()                          { return fHHitTimeOut;                          };
  TH1D ** GetHHitTimeInMU()                         { return fHHitTimeInMU;                         };
  TH1D ** GetHHitTimeOutMU()                        { return fHHitTimeOutMU;                        };
  TH1D ** GetHHitTimeInAll()                        { return fHHitTimeInAll;                        };
  TH1D ** GetHHitTimeOutAll()                       { return fHHitTimeOutAll;                       };
  TH2F *  GetHXYCluster()                           { return fHXYCluster;                           };
  TH2F ** GetHXYClusterPerPlane()                   { return fHXYClusterPerPlane;                 };
  TH2F *  GetHXYClusterPerPlane(Int_t i)            { return fHXYClusterPerPlane[i];              };

  TH1D *  GetHRecoHitTimeWrtReference()             { return fHRecoHitTimeWrtReference;             };
  TH2F *  GetHRecoHitTimeWrtReferenceVsBurst()      { return fHRecoHitTimeWrtReferenceVsBurst;      };
  TH2F *  GetHRecoHitTimeWrtReferenceVsWidth()      { return fHRecoHitTimeWrtReferenceVsWidth;      };

  TH1D *  GetHRecoHitTimeWrtReferenceNoT0()         { return fHRecoHitTimeWrtReferenceNoT0;         };
  TH2F *  GetHRecoHitTimeWrtReferenceVsBurstNoT0()  { return fHRecoHitTimeWrtReferenceVsBurstNoT0;  };
  TH2F *  GetHRecoHitTimeWrtReferenceVsWidthNoT0()  { return fHRecoHitTimeWrtReferenceVsWidthNoT0;  };

  TH1D *  GetHCandidateTimeWrtReference()           { return fHCandidateTimeWrtReference;           };
  TH2F *  GetHCandidateTimeWrtReferenceVsBurst()    { return fHCandidateTimeWrtReferenceVsBurst;    };
 
  TH1D ** GetHNHitsForRingVSBurst()                 { return fHNHitsForRingVSBurst;                 };
  TH1D *  GetHNHitsForRingVSBurst(Int_t i)          { return fHNHitsForRingVSBurst[i];              };
  TH1D ** GetHNHitsForPlaneVSBurst()                { return fHNHitsForPlaneVSBurst;              };
  TH1D *  GetHNHitsForPlaneVSBurst(Int_t i)         { return fHNHitsForPlaneVSBurst[i];           };
  
  TH1D ** GetHNWordPerTell()                        { return fHNWordPerTell;                        };
  TH1D *  GetHNWordPerTell(Int_t i)                 { return fHNWordPerTell[i];                     };
  TH1D ** GetHNWordPerTDCB()                        { return fHNWordPerTDCB;                        };
  TH1D *  GetHNWordPerTDCB(Int_t i)                 { return fHNWordPerTDCB[i];                     };
  TH1D ** GetHNWordPerTDC()                         { return fHNWordPerTDC;                         };
  TH1D *  GetHNWordPerTDC(Int_t i)                  { return fHNWordPerTDC[i];                      };

  TH2F ** GetHTimeVsDistanceMU()                    { return fHTimeVsDistanceMU;                    };
  TH2F *  GetHTimeVsDistanceMU(Int_t i)             { return fHTimeVsDistanceMU[i];                 };

  public:

  double GetRawEnergy() { return fRawEnergy; }

  void Clear();
  void InitRecoEvent();
  void DeleteDigi(TDCEvent*);
  void MergeDigi(TDCEvent*);
  void DigiToReco(TDCEvent*);
  void SlewingCorrection(TDCEvent*);
  void ResoTimeEvaluation(TDCEvent*);
  void ModeEvaluation(TDCEvent*);
  void EfficiencyEvaluation(TDCEvent*);
  void BarEfficiencyEvaluation(TDCEvent *);
  void FillingHisto(TDCEvent*);
  void SingleMuonSelection(TDCEvent*);
  void SingleMuonFillingHisto(TDCEvent*);
  void SingleMuonResoTimeEvaluation(TDCEvent*);

  //double SetClusterThreshold( double value ) { fClusterThreshold = value; } // default is 0.MeV

private:

  CHANTIGeometry *fGeometry;
  Int_t fNRings;
  Int_t fNPlanes;
  Int_t fNFiredRing;
  Int_t fNFiredPlane;
  Int_t BarFired[12][24];
  Int_t fNElectronicHit;
  Int_t fNWordTell[2];
  Int_t fNWordTDCB[8];
  Int_t fNWordTDC[32];
  Int_t fNHitRing[12];
  Int_t fNHitPlane[6];
  Int_t	fContOfModeX;
  Double_t fModeX;
  Int_t fModeXID;
  Int_t fContOfModeY;
  Double_t fModeY;
  Int_t fModeYID;
  Int_t fCorrTimeBinMode[12];
  Double_t fCorrTimeMode[12];
  Int_t fCorrTimeEntriesMode[12];
  Int_t fEntriesMode[12];
  Double_t fMode[12];
  Bool_t fSingleMuon;
  Bool_t fEnableSlewingCorr;
  Bool_t fEnableTrailing;
  Bool_t fEvaluateT0;
  Bool_t fEvaluateSlewingCorr;
  //Double_t fNewMinWidth; //new MinWidth for SlewingCorrection
  //Double_t fNewMaxWidth; //new MaxWidth for SlewingCorrection
  Double_t fSingleMuonX;
  Double_t fSingleMuonY;
  Double_t fSingleMuonTime;
  Double_t fSingleMuonTotalToT;
  Double_t fSingleMuonTotalXToT[12];
  Double_t fSingleMuonTotalYToT[12];	
  TString fThFileName;
  TString fSlewingFileName;

  Int_t fNTriggersPerBurst;
  Int_t fEffPlaneIndex;
  Double_t fGlobalShift;
  Double_t fStep;
  Double_t fBurstLength;

  //Parameters for digitization of the signal
  Double_t fRingsMCToF[12];	
  TString fSignalParametersFileName;
  TString fMeanNphFileName;
  Double_t *fPhotonsNumber;
  Double_t *fEnergyEdge;
  Int_t fNbinEnergy;
  Double_t fSlopeAmplNph;	///<	This is the slope Amplitude/Nph of the SiPM signal(mV/Nph)
  Double_t fSigmaAmplSPE;	///<	This is the sigma of the amplitude of the single-photon signal (mV)
  Double_t fMeanNfr;		///<	This is the constant time decay of the SiPM signal(ns)
  Double_t fSigmaNfr; 		///<	This is the mean value of number of degree of freedom of the chi-quadro distribution that simulates the signal
  Double_t fHysteresis; 	///<	This is the sigma value of number of degree of freedom of the chi-quadro distribution that simulates the signal
  Double_t fTauFall;		///<	This is the hysteresis of the threshold (mV)
  Double_t fFiberLightSpeed; 	///<	This is the light speed inside the fiber (mm/ns)
  Double_t fReflectionFactorBoard;   ///< Amplitute ratio between reflected (at the ToT board) and direct signal
  Double_t fSlopeEnergyNph;		///< Slope in the linear function NphVSEnergyr (Nph/MeV)				
  Double_t fOffsetEnergyNph;	///< Offset in the linear function NphVSEnergyr (Nph)	
  Double_t fCutOffFrequency; ///< Parameter for the low pass filter (bandwith of the electronic board)				   
  Double_t fZeroFrequency;	///< Parameter for the low pass filter (bandwith of the electronic board)
  //double fClusterThreshold;

  double fRawEnergy;

  //Histograms
  TH1D ** fHBarEffiDen;
  TH1D ** fHBarEffiNum;
  TH1D ** fHNHitsForRing;
  TH1D ** fHToTRing0;
  TH1D ** fHToTRing1;
  TH1D ** fHToTRing2;
  TH1D*** fHPositionLow;
  TH1D*** fHPositionHigh;
  TH1D ** fHPosition1;
  TH1D ** fHPosition2;
  TH1D ** fHEffi1Up;
  TH1D ** fHEffi2Up;
  TH1D ** fHEffi1Down;
  TH1D ** fHEffi2Down;
  TH1D ** fHEffiPosition1Up;
  TH1D ** fHEffiPosition2Up;
  TH1D ** fHEffiPosition1Down;
  TH1D ** fHEffiPosition2Down;
  TH1D ** fHEffiHigh1Up;
  TH1D ** fHEffiHigh2Up;
  TH1D ** fHEffiHigh1Down;
  TH1D ** fHEffiHigh2Down;
  TH1D ** fHEffiPositionHigh1Up;
  TH1D ** fHEffiPositionHigh2Up;
  TH1D ** fHEffiPositionHigh1Down;
  TH1D ** fHEffiPositionHigh2Down;
  TH1D ** fHBarID;
  TH1D ** fHNHitsForPlane;
  TH1D ** fHToTPlane0;
  TH1D ** fHToTPlane1;
  TH1D ** fHToTPlane2;
  TH1D *  fHNTriggersPerBurst;
  TH1D *  fHResoTime;
  TH1D ** fHResoTimePlane;
  TH1D *  fHResoTimeMU;
  TH1D ** fHResoTimePlaneMU;
  TH1D *  fHXHit;
  TH1D *  fHYHit;
  TH1D *  fHXHitEvent1;
  TH1D *  fHXHitEvent2;
  TH1D *  fHYHitEvent1;
  TH1D *  fHYHitEvent2;
  TH1D *  fHDispPlotX;
  TH1D *  fHDispPlotY;
  TH1D ** fHDispPlot;
  TH1D ** fHDispPlotMU;
  TH1D *  fHDispPlotXMU;
  TH1D *  fHDispPlotYMU;
  TH1D *  fHToT0;
  TH1D *  fHToT1;
  TH1D *  fHToT2;
  TH2F *  fHTimeVSSlot;
  TH2F *  fHSlewingVSToTLow;
  TH2F *  fHSlewingVSToTHigh;
  TH2F *  fHSlewingVSToTLowFit;
  TH2F *  fHSlewingVSToTHighFit;
  TH1D *  fHNHit;
  TH1D *  fHNHitAllRing;
  TH1D ** fHCorrTime;
  TH1D *  fHNRing;
  TH1D *  fHNPlane;
  TH1D *  fHModeX;
  TH1D *  fHModeY;
  TH2F *  fHNRingVSNHit;
  TH2F *  fHNPlaneVSNHit;
  TH1D ** fHCorrTimeDispPlot;
  TH1D ** fHCorrTimeDispPlotMU;
  TH1D ** fHCorrTimeShiftMU;
  TH2F *  fHCorrTimeVSZ;
  TH2F *  fHCorrTimeVSZMU;
  TH2F*** fHViewX1THRL;
  TH2F*** fHViewX2THRL;
  TH2F*** fHViewY1THRL;
  TH2F*** fHViewY2THRL;
  TH2F*** fHViewX1THRH;
  TH2F*** fHViewX2THRH;
  TH2F*** fHViewY1THRH;
  TH2F*** fHViewY2THRH; 
  TH2F *  fHModeYvsModeXSingleMuon;
  TH1D *  fHContOfModeX;
  TH1D *  fHContOfModeX1;
  TH1D *  fHContOfModeX2;
  TH1D *  fHContOfModeX3;
  TH1D *  fHContOfModeY;
  TH1D *  fHContOfModeY1;
  TH1D *  fHContOfModeY2;
  TH1D *  fHContOfModeY3;
  TH1D ** fHEffiPlotXDen;
  TH1D ** fHEffiPlotYDen;
  TH1D ** fHEffiPlotXNum;
  TH1D ** fHEffiPlotYNum;
  TH1D ** fHEffiPlotHighXNum;
  TH1D ** fHEffiPlotHighYNum;   
  TH2F ** fHEffiPlotXYDen;
  TH2F ** fHEffiPlotXYNum;
  TH1D ** fHHitTimeIn;
  TH1D ** fHHitTimeOut;
  TH1D ** fHHitTimeInMU;
  TH1D ** fHHitTimeOutMU;
  TH1D ** fHHitTimeInAll;
  TH1D ** fHHitTimeOutAll;
  TH2F *  fHXYCluster;
  TH2F ** fHXYClusterPerPlane; 

  TH1D *  fHRecoHitTimeWrtReference;
  TH2F *  fHRecoHitTimeWrtReferenceVsBurst;
  TH2F *  fHRecoHitTimeWrtReferenceVsWidth;

  TH1D *  fHRecoHitTimeWrtReferenceNoT0;
  TH2F *  fHRecoHitTimeWrtReferenceVsBurstNoT0;
  TH2F *  fHRecoHitTimeWrtReferenceVsWidthNoT0;

  TH1D *  fHCandidateTimeWrtReference;
  TH2F *  fHCandidateTimeWrtReferenceVsBurst;

  TH1D ** fHNHitsForRingVSBurst;    
  TH1D ** fHNHitsForPlaneVSBurst; 

  TH1D ** fHNWordPerTell;
  TH1D ** fHNWordPerTDCB; 
  TH1D ** fHNWordPerTDC;

  TH2F ** fHTimeVsDistanceMU;
};
#endif
