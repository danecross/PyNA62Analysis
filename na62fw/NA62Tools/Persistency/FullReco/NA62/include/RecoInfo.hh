// --------------------------------------------------------------
// History:
//
// Variables added  Karim Massri (karim.massri@cern.ch) 2016-12-08
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// --------------------------------------------------------------

#ifndef RECOINFO_H
#define RECOINFO_H 1

#include "TObject.h"
#include "TString.h"

class RecoInfo : public TObject {

public:

  RecoInfo();
  virtual ~RecoInfo() {}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  void UpdateUniqueAttributes(RecoInfo &s);
  void MergeJobAttributes(RecoInfo &s);
  void UpdateAndMergeAttributes(RecoInfo &s);

  TString               GetRevision() const                   { return fRevision;               }
  void                  SetRevision(TString val)              { fRevision = val;                }
  std::vector<UInt_t>   GetRunID() const                      { return fRunID;                  }
  void                  SetRunID(UInt_t val)                  { fRunID.push_back(val);          }
  std::vector<UInt_t>   GetBurstID() const                    { return fBurstID;                }
  void                  SetBurstID(UInt_t val)                { fBurstID.push_back(val);        }
  std::vector<UInt_t>   GetBurstTime() const                  { return fBurstTime;              }
  void                  SetBurstTime(UInt_t val)              { fBurstTime.push_back(val);      }
  UInt_t                GetNReadEvents() const                { return fNReadEvents;            }
  void                  SetNReadEvents(UInt_t val)            { fNReadEvents = val;             }
  UInt_t                GetNProcessedEvents() const           { return fNProcessedEvents;       }
  void                  SetNProcessedEvents(UInt_t val)       { fNProcessedEvents = val;        }
  UInt_t                GetNSkippedEvents() const             { return fNSkippedEvents;         }
  void                  SetNSkippedEvents(UInt_t val)         { fNSkippedEvents = val;          }
  UInt_t                GetNCriticalEvents() const            { return fNCriticalEvents;        }
  void                  SetNCriticalEvents(UInt_t val)        { fNCriticalEvents = val;         }
  UInt_t                GetNPhysicsTriggerEvents() const      { return fNPhysicsTriggerEvents;  }
  void                  SetNPhysicsTriggerEvents(UInt_t val)  { fNPhysicsTriggerEvents = val;   }
  UInt_t                GetNControlTriggerEvents() const      { return fNControlTriggerEvents;  }
  void                  SetNControlTriggerEvents(UInt_t val)  { fNControlTriggerEvents = val;   }
  UInt_t                GetNPeriodicTriggerEvents() const     { return fNPeriodicTriggerEvents; }
  void                  SetNPeriodicTriggerEvents(UInt_t val) { fNPeriodicTriggerEvents = val;  }
  UInt_t                GetNSpecialTriggerEvents() const      { return fNSpecialTriggerEvents;  }
  void                  SetNSpecialTriggerEvents(UInt_t val)  { fNSpecialTriggerEvents = val;   }
  Double_t              GetKaonRate() const                   { return fKaonRate;               }
  void                  SetKaonRate(Double_t val)             { fKaonRate = val;                }
  Double_t              GetKaonRateError() const              { return fKaonRateError;          }
  void                  SetKaonRateError(Double_t val)        { fKaonRateError = val;           }
  Double_t              GetChokeONTime() const                { return fChokeONTime;            }
  void                  SetChokeONTime(Double_t val)          { fChokeONTime = val;             }

private:

  TString  fRevision;                ///< Software revision
  UInt_t   fNReadEvents;             ///< Number of read events in the file
  UInt_t   fNProcessedEvents;        ///< Number of processed events in the file
  UInt_t   fNSkippedEvents;          ///< Number of skipped events in the file
  UInt_t   fNCriticalEvents;         ///< Number of events in the file with critical errors
  UInt_t   fNPhysicsTriggerEvents;   ///< Number of PhysicsTrigger events in the file
  UInt_t   fNControlTriggerEvents;   ///< Number of ControlTrigger in the file
  UInt_t   fNPeriodicTriggerEvents;  ///< Number of PeriodicTrigger in the file
  UInt_t   fNSpecialTriggerEvents;   ///< Number of SpecialTrigger in the file
  Double_t fKaonRate;                ///< Kaon rate [MHz] estimated from Cedar
  Double_t fKaonRateError;           ///< Error of the Kaon rate [MHz] estimated from Cedar

  std::vector<UInt_t>   fRunID;      ///< Run number
  std::vector<UInt_t>   fBurstID;    ///< Burst number
  std::vector<UInt_t>   fBurstTime;  ///< Burst time (UNIX)
  
  // These members are last in the list: they have been introduced in v2 of the class
  Double_t fChokeONTime;

  ClassDef(RecoInfo,2)
};

#endif
