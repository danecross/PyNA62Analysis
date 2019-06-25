// ------------------------------------------------------------------
// History:
//
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-01-06
// Refurbished by Karim Massri (karim.massri@cern.ch)      2013-02-03
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)      2008-05-05
//
// ------------------------------------------------------------------

#ifndef MUV3Reconstruction_H
#define MUV3Reconstruction_H 1

#include "NA62VReconstruction.hh"
#include "TRecoMUV3Candidate.hh"
#include "TDCEvent.hh"
#include "TFile.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TMUV3Event.hh"
#include "MUV3Tile.hh"
#include "MUV3Geometry.hh"

#include <bitset>

class TMUV3Digi;

class MUV3Reconstruction : public NA62VReconstruction {

public:

  MUV3Reconstruction(TFile*, TString);
  ~MUV3Reconstruction();

  void ParseConfFile(TString);
  void ParseMaskedChannelInputFile();
  void TDCEventMonitor(TDCEvent*);
  void EOBEventMonitor(TDetectorVEvent*);
  void InitHistograms();
  void SaveHistograms();
  void DeleteHistograms();
  void ResetHistograms();

  void ReadTileT0s();
  void PrintTileT0s();
  void ResetTileT0s();

  Bool_t ChannelMasked(Int_t);

  virtual void Init(NA62VReconstruction*);
  virtual void StartOfBurst();
  virtual void EndOfBurst();

  virtual TRecoVEvent* ProcessEvent(TDetectorVEvent*, Event*);
  virtual TDetectorVEvent* Trigger(TDetectorVEvent*, Event*);
  virtual void EndProcessing();
  virtual void FillTimes(Double_t);
  void PrintCandidateInfo();

  TRecoMUV3Candidate* GetCandidate()                        { return fCandidate; }
  void                SetCandidate(TRecoMUV3Candidate* val) { fCandidate = val;  }

  Double_t      GetTimeWindow()                { return fTimeWindow;               }
  void          SetTimeWindow(Double_t val)    { fTimeWindow = val;                }
  Double_t      GetNRecoHitsPerBurst()         { return fNRecoHitsPerBurst;        }
  Double_t      GetNLooseCandidatesPerBurst()  { return fNLooseCandidatesPerBurst; }
  Double_t      GetNTightCandidatesPerBurst()  { return fNTightCandidatesPerBurst; }
  Double_t      GetNCandidatesPerBurst()
  { return fNLooseCandidatesPerBurst + fNTightCandidatesPerBurst; }

  // Pointers to monitoring histograms

  // 1) Histograms filled by reconstruction

  TH1D* GetHNDigis()                      { return fHNDigis;                      }
  TH1D* GetHNRecoHits()                   { return fHNRecoHits;                   }
  TH1D* GetHNCandidates()                 { return fHNCandidates;                 }
  TH1D* GetHHitStatus()                   { return fHHitStatus;                   }
  TH1D* GetHTrailingTimeRaw()             { return fHTrailingTimeRaw;             }
  TH1D* GetHLeadingTime()                 { return fHLeadingTime;                 }
  TH1D* GetHTrailingTime()                { return fHTrailingTime;                }
  TH1D* GetHLeadingTimeRaw_NoTrailing()   { return fHLeadingTimeRaw_NoTrailing;   }
  TH1D* GetHTrailingTimeRaw_NoLeading()   { return fHTrailingTimeRaw_NoLeading;   }
  TH1D* GetHRecoHitTimeWrtReference()     { return fHRecoHitTimeWrtReference;     }
  TH1D* GetHRecoHitTimeWrtReferenceNoT0() { return fHRecoHitTimeWrtReferenceNoT0; }
  TH1D* GetHCandidateTimeWrtReference()   { return fHCandidateTimeWrtReference;   }
  TH1D* GetHWidth()                       { return fHWidth;                       }
  TH1D* GetHNErrorWord()                  { return fHNErrorWord;                  }
  TH1D* GetHChannelProfile()              { return fHChannelProfile;              }
  TH1D* GetHChannelProfileEOB()           { return fHChannelProfileEOB;           }
  TH1D* GetHROChannelProfile()            { return fHROChannelProfile;            }
  TH1D* GetHROChannelProfileEOB()         { return fHROChannelProfileEOB;         }
  TH2F* GetHChannelProfileVsBurst()       { return fHChannelProfileVsBurst;       }
  TH2F* GetHChannelProfileVsBurstEOB()    { return fHChannelProfileVsBurstEOB;    }
  TH1D* GetHCandidateProfile()            { return fHCandidateProfile;            }
  TH1D* GetHTightCandidateProfile()       { return fHTightCandidateProfile;       }
  TH1D* GetHTightPrimitiveProfileEOB()    { return fHTightPrimitiveProfileEOB;    }
  TH1D* GetHTileAND()                     { return fHTileAND;                     }
  TH1D* GetHTileOR()                      { return fHTileOR;                      }
  TH1D* GetHDeltaTime()                   { return fHDeltaTime;                   }
  TH1D* GetHNTightMuonsPerBurstEOB()      { return fHNTightMuonsPerBurstEOB;      }
  TH1D* GetHTotalPrimitiveCountsEOB()     { return fHTotalPrimitiveCountsEOB;     }
  TH1D* GetHErrorCountsEOB()              { return fHErrorCountsEOB;              }
  TH1D* GetHNHitsPerBurstEOB()            { return fHNHitsPerBurstEOB;            }
  TH1D* GetHHitFineTimeBits()             { return fHHitFineTimeBits;             }
  TH1D* GetHHitFineTime256()              { return fHHitFineTime256;              }

  TH2F* GetHMaskedProfile2D_PM0()         { return fHMaskedProfile2D_PM0;         }
  TH2F* GetHMaskedProfile2DInner_PM0()    { return fHMaskedProfile2DInner_PM0;    }
  TH2F* GetHMaskedProfile2D_PM1()         { return fHMaskedProfile2D_PM1;         }
  TH2F* GetHMaskedProfile2DInner_PM1()    { return fHMaskedProfile2DInner_PM1;    }
  TH2F* GetHChannelProfile2D_PM0()        { return fHChannelProfile2D_PM0;        }
  TH2F* GetHChannelProfile2D_PM1()        { return fHChannelProfile2D_PM1;        }
  TH2F* GetHChannelProfile2DInner_PM0()   { return fHChannelProfile2DInner_PM0;   }
  TH2F* GetHChannelProfile2DInner_PM1()   { return fHChannelProfile2DInner_PM1;   }

  TH2F* GetHChannelProfile2D_EOB_PM0()          { return fHChannelProfile2D_EOB_PM0;         }
  TH2F* GetHChannelProfile2D_EOB_PM1()          { return fHChannelProfile2D_EOB_PM1;         }
  TH2F* GetHChannelProfile2DInner_EOB_PM0()     { return fHChannelProfile2DInner_EOB_PM0;    }
  TH2F* GetHChannelProfile2DInner_EOB_PM1()     { return fHChannelProfile2DInner_EOB_PM1;    }
  TH2F* GetHCandidateProfile2D()                { return fHCandidateProfile2D;               }
  TH2F* GetHCandidateProfile2DInner()           { return fHCandidateProfile2DInner;          }
  TH2F* GetHTightCandidateProfile2D()           { return fHTightCandidateProfile2D;          }
  TH2F* GetHTightCandidateProfile2DInner()      { return fHTightCandidateProfile2DInner;     }

  TH2F* GetHHitStatusVsChannel()                { return fHHitStatusVsChannel;               }
  TH2F* GetHLeadingTimeVsChannel()              { return fHLeadingTimeVsChannel;             }
  TH2F* GetHLeadingTimeVsROChannel()            { return fHLeadingTimeVsROChannel;           }
  TH2F* GetHRecoHitTimeWrtReferenceVsChannel()  { return fHRecoHitTimeWrtReferenceVsChannel; }
  TH2F* GetHCandidateTimeWrtReferenceVsBurst()  { return fHCandidateTimeWrtReferenceVsBurst; }
  TH2F* GetHDeltaTimeVsTile()                   { return fHDeltaTimeVsTile;                  }
  TProfile* GetPDeltaTimeVsTile()               { return fPDeltaTimeVsTile;                  }

  // 2) Histograms filled by digitization

  TH1D* GetHEventMinusCFDTimePM0()     { return fHEventMinusCFDTimePM0;     }
  TH1D* GetHEventMinusCFDTimePM1()     { return fHEventMinusCFDTimePM1;     }
  TH1D* GetHCFDTimePM0()               { return fHCFDTimePM0;               }
  TH1D* GetHCFDTimePM1()               { return fHCFDTimePM1;               }
  TH1D* GetHEventMinusCFDTimePM0_all() { return fHEventMinusCFDTimePM0_all; }
  TH1D* GetHEventMinusCFDTimePM1_all() { return fHEventMinusCFDTimePM1_all; }
  TH1D* GetHEnergy()                   { return fHEnergy;                   }
  TH1D* GetHEnergyPerCell()            { return fHEnergyPerCell;            }
  TH1D* GetHNHits()                    { return fHNHits;                    }
  TH1D* GetHNChannels()                { return fHNChannels;                }

  TH2F* GetHTimevsEnergyPM0()          { return fHTimevsEnergyPM0;          }
  TH2F* GetHTimevsEnergyPM1()          { return fHTimevsEnergyPM1;          }
  TH2F* GetHTimevsNHitsPM0()           { return fHTimevsNHitsPM0;           }
  TH2F* GetHTimevsNHitsPM1()           { return fHTimevsNHitsPM1;           }
  TH2F* GetHCollectionMatrixA()        { return fHCollectionMatrixA;        }
  TH2F* GetHCollectionMatrixB()        { return fHCollectionMatrixB;        }
  TH1D* GetHReferenceShape()           { return fHReferenceShape;           }

protected:

  Double_t GetRecoTime     (TMUV3Digi*, Int_t);
  Double_t GetRecoTimeNoT0 (TMUV3Digi*, Int_t);

private:

  MUV3Geometry*       fGeo;
  TRecoMUV3Candidate* fCandidate;
  TString             fMaskedChannelInputFile;
  TString             fT0TilesFileName;

  Int_t      fL0DataType;      ///< L0 data type: physics, calibration, control, ...
  UShort_t   fL0TriggerFlags;  ///< L0 trigger bits as in run conditions database
  Int_t      fL1TriggerType;   ///< L1 trigger bits
  Int_t      fL2TriggerType;   ///< L2 trigger bits
  UInt_t     fTimeStamp;       ///< Event timestamp in units of 24.951059536 ns
  Double_t   fTimeWindow;      ///< Matching time window for tight candidates
  Int_t      fEdgeRequirement; ///< Digi Edge requirement, see MUV3.conf for details
  Int_t      fNTiles;
  MUV3Tile** fTiles;
  Bool_t     fPrintCandidateInfo;
  Bool_t     fBuildLooseCandidates;       ///< Reconstruct loose candidates
  Bool_t     fBuildLooseMaskedCandidates; ///< Reconstruct loose masked candidates?
  Bool_t     fEnableT0Tiles;              ///< Enable T0 corrections for the candidates?
  Int_t      fDigitizerMode;
  Int_t      fDigiHistoFlag;
  Double_t   fChannelTimeResolution;
  Double_t   fNRecoHitsPerBurst;
  Double_t   fNLooseCandidatesPerBurst;
  Double_t   fNTightCandidatesPerBurst;
  std::vector<Int_t> fMaskedChannels;

  // 1) Histograms filled by reconstruction

  TH1D
    *fHNDigis,
    *fHNRecoHits,
    *fHNRecoHitsPerTile,
    *fHNCandidates,
    *fHNRecoHitsPerBurst,
    *fHNHitsPerBurstEOB,
    *fHNLooseCandidatesPerBurst,
    *fHNTightCandidatesPerBurst,
    *fHNTightMuonsPerBurstEOB,
    *fHTotalPrimitiveCountsEOB, ///< Counts of the 16 MUV3 L0 primitive types
    *fHErrorCountsEOB,          ///< Counts of the 12 error types
    *fHInterCandidateDistance,
    *fHNCandidatesToRecoHitsPerTile,
    *fHHitStatus,
    *fHTrailingTimeRaw,
    *fHLeadingTime,
    *fHTrailingTime,
    *fHLeadingTimeRaw_NoTrailing,
    *fHTrailingTimeRaw_NoLeading,
    *fHRecoHitTimeWrtReference,
    *fHRecoHitTimeWrtReferenceNoT0,
    *fHCandidateTimeWrtReference,
    *fHCandidateAvgTimeWrtReference,
    *fHCandidateTimeWrtReferenceNoTileT0,
    *fHCandidateTimeWrtReferenceNoT0,
    *fHWidth,
    *fHNErrorWord,
    *fHChannelProfile,
    *fHChannelProfileEOB,
    *fHROChannelProfile,
    *fHROChannelProfileEOB,
    *fHTileAsymmetry,
    *fHTileAsymmetryEOB,
    *fHCandidateProfile,
    *fHTightCandidateProfile,
    *fHTightPrimitiveProfileEOB,
    *fHTileAND,
    *fHTileOR,
    *fHDeltaTime,
    *fHDeltaTimeCandidate,
    *fHHitFineTimeBits,
    *fHHitFineTime256,
    *fHRecoHitTimeWrtReference_Channel147; ///< Monitoring of the 200 MHz beam structure

  TH2F
    *fHNRecoHitsVsL0TriggerBit,
    *fHNRecoHitsVsNoL0TriggerBit,
    *fHNCandidatesVsL0TriggerBit,
    *fHNCandidatesVsNoL0TriggerBit,
    *fHChannelProfileVsBurst,
    *fHChannelProfileVsBurstEOB,
    *fHChannelProfile2D_PM0,
    *fHChannelProfile2D_PM1,
    *fHChannelProfile2DInner_PM0,
    *fHChannelProfile2DInner_PM1,

    *fHChannelProfile2D_EOB_PM0,
    *fHChannelProfile2D_EOB_PM1,
    *fHChannelProfile2DInner_EOB_PM0,
    *fHChannelProfile2DInner_EOB_PM1,

    *fHCandidateProfile2D,
    *fHCandidateProfile2DInner,
    *fHTightCandidateProfile2D,
    *fHTightCandidateProfile2DInner,
    *fHTightPrimitiveProfile2D_EOB,
    *fHTightPrimitiveProfile2DInner_EOB,

    *fHTightCandidateProfileVsL0TriggerBit,
    *fHTightCandidateProfileVsNoL0TriggerBit,

    *fHHitStatusVsChannel,
    *fHHitStatusVsROChannel,
    *fHLeadingTimeVsChannel,
    *fHLeadingTimeVsROChannel,
    *fHRecoHitTimeWrtReferenceVsChannel,
    *fHRecoHitTimeWrtReferenceVsBurst,
    *fHRecoHitTimeWrtReferenceVsWidth,
    *fHRecoHitTimeWrtReferenceVsTimeStamp_Channel147, ///< Monitoring of the 200 MHz beam structure
    *fHCandidateTimeWrtReferenceVsTile,
    *fHCandidateAvgTimeWrtReferenceVsTile,
    *fHCandidateTimeWrtReferenceNoTileT0VsTile,
    *fHCandidateTimeWrtReferenceVsBurst,

    *fHWidthVsChannel,
    *fHWidthVsROChannel,
    *fHSlotVsLeadingTime,
    *fHDeltaTimeVsTile,

    *fHMaskedProfile2D_PM0,
    *fHMaskedProfile2D_PM1,
    *fHMaskedProfile2DInner_PM0,
    *fHMaskedProfile2DInner_PM1;

  TProfile *fPDeltaTimeVsTile;

  // 2) Histograms filled by digitization

  TH1D *fHEventMinusCFDTimePM0, *fHEventMinusCFDTimePM1;
  TH1D *fHCFDTimePM0, *fHCFDTimePM1;
  TH1D *fHEventMinusCFDTimePM0_all, *fHEventMinusCFDTimePM1_all;
  TH1D *fHEnergy;
  TH1D *fHEnergyPerCell;
  TH1D *fHNHits;
  TH1D *fHNChannels;
  TH2F *fHTimevsEnergyPM0, *fHTimevsEnergyPM1;
  TH2F *fHTimevsNHitsPM0, *fHTimevsNHitsPM1;

  TH2F *fHCollectionMatrixA, *fHCollectionMatrixB;
  TH1D *fHReferenceShape;
};

#endif
