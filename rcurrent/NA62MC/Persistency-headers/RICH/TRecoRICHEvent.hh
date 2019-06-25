// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoRICHEvent_H
#define TRecoRICHEvent_H

#include "TRecoVEvent.hh"
#include "TRecoRICHCandidate.hh"
#include "TRecoRICHHit.hh"

class TRecoRICHEvent : public TRecoVEvent {

public:

  TRecoRICHEvent();
  ~TRecoRICHEvent();

  void Clear(Option_t* = "");

  TRecoRICHCandidate * GetTimeCandidate(Int_t);
  TRecoRICHCandidate * GetRingCandidate(Int_t);
  TRecoRICHCandidate * GetSCTimeCandidate(Int_t);
  TRecoRICHCandidate * GetPMTimeCandidate(Int_t);

  Int_t    GetNTimeCandidates()              { return fNTimeCandidates;    }
  void     SetNTimeCandidates(Int_t value)   { fNTimeCandidates = value;   }
  Int_t    GetNRingCandidates()              { return fNRingCandidates;    }
  void     SetNRingCandidates(Int_t value)   { fNRingCandidates = value;   }
  Int_t    GetNPMTimeCandidates()            { return fNPMTimeCandidates;  }
  void     SetNPMTimeCandidates(Int_t value) { fNPMTimeCandidates = value; }
  Int_t    GetNSCTimeCandidates()            { return fNSCTimeCandidates;  }
  void     SetNSCTimeCandidates(Int_t value) { fNSCTimeCandidates = value; }

private:

  Int_t fNTimeCandidates; ///< number of Time Candidates
  Int_t fNRingCandidates; ///< number of Ring Candidates
  Int_t fNPMTimeCandidates; ///< number of PM Time Candidates 
  Int_t fNSCTimeCandidates; ///< number of SuperCell Time Candidates

  ClassDef(TRecoRICHEvent,1);
};
#endif
