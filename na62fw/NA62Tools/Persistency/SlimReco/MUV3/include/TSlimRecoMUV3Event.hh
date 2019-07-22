// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TRECOMUV3EVENTSLIM_H
#define TRECOMUV3EVENTSLIM_H

#include <RtypesCore.h>
#include <vector>
#include "TSlimRecoVEvent.hh"
#include "TSlimRecoMUV3Candidate.hh"

class TRecoMUV3Event;

class TSlimRecoMUV3Event : public TSlimRecoVEvent {

public:
  TSlimRecoMUV3Event();
  explicit TSlimRecoMUV3Event(TRecoMUV3Event *evReco);
  virtual ~TSlimRecoMUV3Event() {}

  void Reset(); // clears the candidate vector
  void ClearCandidates();

  void AddCandidate(TSlimRecoMUV3Candidate c) { fCandidates.emplace_back(std::move(c)); }

  Int_t GetNCandidates()                               const { return fCandidates.size(); }
  std::vector<TSlimRecoMUV3Candidate>& GetCandidates()       { return fCandidates;        }
  TSlimRecoVCandidate* GetCandidate(UInt_t iCand)            { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

  // Conversion functions
  virtual void FromReco(TRecoVEvent*);
  virtual void ToReco  (TRecoVEvent*);
private:
  std::vector<TSlimRecoMUV3Candidate> fCandidates;

  ClassDef(TSlimRecoMUV3Event, 1)
};

#endif
