// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

#ifndef NewCHODReconstruction_H
#define NewCHODReconstruction_H 1

#include "NA62VReconstruction.hh"
#include "TRecoNewCHODCandidate.hh"
#include "TDCEvent.hh"

#include "TFile.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TProfile.h"

#include "TNewCHODEvent.hh"
#include "NewCHODTile.hh"
#include "NewCHODGeometry.hh"

#include <bitset>

class TNewCHODDigi;

class NewCHODReconstruction : public NA62VReconstruction {

public:

  NewCHODReconstruction(TFile*, TString);
  ~NewCHODReconstruction();

  void ParseConfFile(TString);
  void TDCEventMonitor();
  void EOBEventMonitor(TDetectorVEvent*);
  void InitHistograms();
  void SaveHistograms();
  void DeleteHistograms();
  void ResetHistograms();

  Bool_t ChannelMasked(Int_t);

  virtual void Init(NA62VReconstruction*);
  virtual void StartOfBurst();
  virtual void EndOfBurst();

  virtual TRecoVEvent* ProcessEvent(TDetectorVEvent*, Event*);
  virtual TDetectorVEvent* Trigger(TDetectorVEvent*, Event*);
  virtual void EndProcessing();
  virtual void FillTimes(Double_t);
  void PrintRecoHitInfo(Int_t);

  Double_t GetTimeWindow()                { return fTimeWindow;          }
  void     SetTimeWindow(Double_t val)    { fTimeWindow = val;           }
  Double_t GetNRecoHitsPerBurst()         { return fNRecoHitsPerBurst;   }

  // Pointers to the monitoring histograms

  TH1D* GetHNDigis()                      { return fHNDigis;                      }
  TH1D* GetHNRecoHits()                   { return fHNRecoHits;                   }
  TH1D* GetHHitStatus()                   { return fHHitStatus;                   }
  TH1D* GetHLeadingTime()                 { return fHLeadingTime;                 }
  TH1D* GetHTrailingTime()                { return fHTrailingTime;                }
  TH1D* GetHLeadingTimeRaw_NoTrailing()   { return fHLeadingTimeRaw_NoTrailing;   }
  TH1D* GetHTrailingTimeRaw_NoLeading()   { return fHTrailingTimeRaw_NoLeading;   }
  TH1D* GetHTimeWrtReference()            { return fHTimeWrtReference;            }
  TH1D* GetHTimeWrtReferenceNoT0()        { return fHTimeWrtReferenceNoT0;        }
  TH1D* GetHWidth()                       { return fHWidth;                       }
  TH1D* GetHNErrorWord()                  { return fHNErrorWord;                  }
  TH1D* GetHChannelProfile()              { return fHChannelProfile;              }
  TH1D* GetHChannelProfileEOB()           { return fHChannelProfileEOB;           }
  TH1D* GetHROChannelProfile()            { return fHROChannelProfile;            }
  TH1D* GetHROChannelProfileEOB()         { return fHROChannelProfileEOB;         }
  TH2F* GetHChannelProfileVsBurst()       { return fHChannelProfileVsBurst;       }
  TH2F* GetHChannelProfileVsBurstEOB()    { return fHChannelProfileVsBurstEOB;    }
  TH1D* GetHRecoHitProfile()              { return fHRecoHitProfile;              }
  TH1D* GetHTightRecoHitProfile()         { return fHTightRecoHitProfile;         }
  TH1D* GetHTightPrimitiveProfileEOB()    { return fHTightPrimitiveProfileEOB;    }
  TH1D* GetHTileAND()                     { return fHTileAND;                     }
  TH1D* GetHTileOR()                      { return fHTileOR;                      }
  TH1D* GetHDeltaTime()                   { return fHDeltaTime;                   }
  TH1D* GetHNTightPrimitivesPerBurstEOB() { return fHNTightPrimitivesPerBurstEOB; }
  TH1D* GetHTotalPrimitiveCountsEOB()     { return fHTotalPrimitiveCountsEOB;     }
  TH1D* GetHErrorCountsEOB()              { return fHErrorCountsEOB;              }
  TH1D* GetHNHitsPerBurstEOB()            { return fHNHitsPerBurstEOB;            }
  TH1D* GetHHitFineTimeBits()             { return fHHitFineTimeBits;             }
  TH1D* GetHHitFineTime256()              { return fHHitFineTime256;              }
  TH1D* GetHRecoHitProfileMb1()           { return fHRecoHitProfileMb1;           }
  TH1D* GetHRecoHitProfileMb2()           { return fHRecoHitProfileMb2;           }
  TH1D* GetHRecoHitProfileMb3()           { return fHRecoHitProfileMb3;           }
  TH1D* GetHRecoHitProfileMb4()           { return fHRecoHitProfileMb4;           }
  TH1D* GetHRecoHitProfileMb5()           { return fHRecoHitProfileMb5;           }

  TH2F* GetHChannelProfile2D_PM0()        { return fHChannelProfile2D_PM0;        }
  TH2F* GetHChannelProfile2D_PM1()        { return fHChannelProfile2D_PM1;        }

  TH2F* GetHChannelProfile2D_EOB_PM0()    { return fHChannelProfile2D_EOB_PM0;    }
  TH2F* GetHChannelProfile2D_EOB_PM1()    { return fHChannelProfile2D_EOB_PM1;    }
  TH2F* GetHRecoHitProfile2D()            { return fHRecoHitProfile2D;            }
  TH2F* GetHTightRecoHitProfile2D()       { return fHTightRecoHitProfile2D;       }

  TH2F* GetHHitStatusVsChannel()          { return fHHitStatusVsChannel;          }
  TH2F* GetHTimeWrtReferenceVsChannel()   { return fHTimeWrtReferenceVsChannel;   }
  TH2F* GetHTimeWrtReferenceVsBurst()     { return fHTimeWrtReferenceVsBurst;     }
  TH2F* GetHDeltaTimeVsTile()             { return fHDeltaTimeVsTile;             }
  TProfile* GetPDeltaTimeVsTile()         { return fPDeltaTimeVsTile;             }

protected:

  Double_t GetRecoTime     (TNewCHODDigi*, Int_t);
  Double_t GetRecoTimeNoT0 (TNewCHODDigi*, Int_t);

private:

  NewCHODGeometry* fGeo;
  TDCEvent* fTdcEvent;
  std::map<Int_t, Int_t> fMotherBoardMap;

  Int_t         fL0DataType;      ///< L0 data type: physics, calibration, control, ...
  UShort_t      fL0TriggerFlags;  ///< L0 trigger bits as in run conditions database
  Int_t         fL1TriggerType;   ///< L1 trigger bits
  Int_t         fL2TriggerType;   ///< L2 trigger bits
  UInt_t        fTimeStamp;       ///< Event timestamp in units of 24.951059536 ns
  Double_t      fTimeWindow;      ///< Matching time window for tight candidates
  Int_t         fEdgeRequirement; ///< Digi Edge requirement, see NewCHOD.conf for details
  Int_t         fNDigis;          ///< Number of TDC hits in event
  Int_t         fNTiles;  
  Bool_t        fPrintCandidateInfo;
  Bool_t        fBuildLooseCandidates;       ///< Reconstruct loose candidates?
  Bool_t        fBuildLooseMaskedCandidates; ///< Reconstruct loose masked candidates?
  Double_t      fNRecoHitsPerBurst;
  std::vector<Int_t> fMaskedChannels;

  NewCHODTile** fTiles;

  TH1D
    *fHNDigis,
    *fHNRecoHits,
    *fHNRecoHitsPerTile,
    *fHNQuadrants,
    *fHQuadrantTrigger,
    *fHNHitsPerBurstEOB,
    *fHNTightPrimitivesPerBurstEOB,
    *fHTotalPrimitiveCountsEOB, ///< Counts of the 16 NewCHOD L0 primitive types
    *fHErrorCountsEOB,          ///< Counts of the 12 error types
    *fHInterRecoHitDistance,
    *fHHitStatus,
    *fHRecoHitProfile,
    *fHTightRecoHitProfile,
    *fHTightPrimitiveProfileEOB,
    *fHLeadingTime,
    *fHTrailingTime,
    *fHLeadingTimeRaw_NoTrailing,
    *fHTrailingTimeRaw_NoLeading,
    *fHTimeWrtReference,
    *fHTimeWrtReferenceNoT0,
    *fHTightRecoHitTimeWrtReference,
    *fHTightRecoHitTimeWrtReferenceNoT0,
    *fHLooseRecoHitTimeWrtReference,
    *fHLooseRecoHitTimeWrtReferenceNoT0,
    *fHWidth,
    *fHNErrorWord,
    *fHChannelProfile,
    *fHChannelProfileEOB,
    *fHROChannelProfile,
    *fHROChannelProfileEOB,

    *fHRecoHitProfileMb1,
    *fHRecoHitProfileMb2,
    *fHRecoHitProfileMb3,
    *fHRecoHitProfileMb4,
    *fHRecoHitProfileMb5,

    *fHTileAND,
    *fHTileOR,
    *fHDeltaTime,
    *fHDeltaTimeRecoHit,
    *fHHitFineTimeBits,
    *fHHitFineTime256,
    *fHTileAsymmetry,    ///< booked by EndProcessing()
    *fHTileAsymmetryEOB; ///< booked by EndProcessing()

  TH2F
    *fHChannelProfileVsBurst,
    *fHChannelProfileVsBurstEOB;

  TH2F
    *fHChannelProfile2D_PM0,
    *fHChannelProfile2D_PM1,
    *fHChannelProfile2D_EOB_PM0,
    *fHChannelProfile2D_EOB_PM1,
    *fHRecoHitProfile2D,
    *fHTightRecoHitProfile2D,
    *fHTightPrimitiveProfile2D_EOB,

    *fHNRecoHitsVsL0TriggerBit,
    *fHNRecoHitsVsNoL0TriggerBit,
    *fHHitStatusVsChannel,
    *fHHitStatusVsROChannel,
    *fHTimeWrtReferenceVsChannel,
    *fHTimeWrtReferenceVsChannelNoT0,
    *fHTimeWrtReferenceVsBurst,
    *fHTightRecoHitTimeWrtReferenceVsTile,
    *fHTightRecoHitTimeWrtReferenceVsTileNoT0,
    *fHLooseRecoHitTimeWrtReferenceVsTile,
    *fHLooseRecoHitTimeWrtReferenceVsTileNoT0,
    *fHSlotVsLeadingTime,
    *fHDeltaTimeVsTile,

    *fHTightRecoHitProfileVsL0TriggerBit,
    *fHTightRecoHitProfileVsNoL0TriggerBit;

  TProfile *fPDeltaTimeVsTile;
};

#endif
