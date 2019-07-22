#ifndef TSLIMRecoLAVEvent_H
#define TSLIMRecoLAVEvent_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoLAVCandidate.hh"
#include "TSlimRecoLAVHit.hh"

class TRecoLAVEvent;


class TSlimRecoLAVEvent : public TSlimRecoVEvent {

public:

  TSlimRecoLAVEvent();
  explicit TSlimRecoLAVEvent(TRecoLAVEvent *evReco);
  virtual ~TSlimRecoLAVEvent() {}

  void Reset();               // clears the candidate and hit vector
  void ClearHits();
  void ClearCandidates();

  void AddHit(TSlimRecoLAVHit h)             { fHits.emplace_back(std::move(h));       }
  void AddCandidate(TSlimRecoLAVCandidate c) { fCandidates.emplace_back(std::move(c)); }

  Int_t GetNHits()                                   const { return fHits.size();       }
  std::vector<TSlimRecoLAVHit>& GetHits()                  { return fHits;              }
  TSlimRecoVHit* GetHit(UInt_t iHit)                       { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
  Int_t GetNCandidates()                             const { return fCandidates.size(); }
  std::vector<TSlimRecoLAVCandidate>& GetCandidates()      { return fCandidates;        }
  TSlimRecoVCandidate* GetCandidate(UInt_t iCand)          { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

  // conversion functions
  virtual void FromReco(TRecoVEvent *evReco);
  virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoLAVHit> fHits;
    std::vector<TSlimRecoLAVCandidate> fCandidates;

    ClassDef(TSlimRecoLAVEvent, 1)
};
#endif /* TSLIMRecoLAVEvent_H */
