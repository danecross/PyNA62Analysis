// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TRECONEWCHODEVENTSLIM_H
#define TRECONEWCHODEVENTSLIM_H

#include <RtypesCore.h>
#include <vector>

#include "TSlimRecoNewCHODCandidate.hh"
#include "TSlimRecoVEvent.hh"

class TRecoNewCHODEvent;

class TSlimRecoNewCHODEvent : public TSlimRecoVEvent {

public:
  TSlimRecoNewCHODEvent();
  explicit TSlimRecoNewCHODEvent(TRecoNewCHODEvent*);
  virtual ~TSlimRecoNewCHODEvent() {}

  void Reset();
  void ClearCandidates();

  void AddCandidate(TSlimRecoNewCHODCandidate h) { fCandidates.emplace_back(std::move(h)); }

  Int_t GetNCandidates()                            const { return fCandidates.size(); }
  std::vector<TSlimRecoNewCHODCandidate>& GetCandidates() { return fCandidates;        }
  TSlimRecoVCandidate* GetCandidate(UInt_t iCand)         { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }


  // Conversion functions
  virtual void FromReco(TRecoVEvent*);
  virtual void ToReco(TRecoVEvent*);
private:
  std::vector<TSlimRecoNewCHODCandidate> fCandidates;

  ClassDef(TSlimRecoNewCHODEvent, 1)
};

#endif
