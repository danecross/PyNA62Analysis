#ifndef TSLIMRECOHACEVENT_H
#define TSLIMRECOHACEVENT_H

#include "RtypesCore.h"
#include <vector>

#include "TSlimRecoVEvent.hh"

#include "TSlimRecoHACCandidate.hh"
#include "TSlimRecoHACHit.hh"

class TRecoHACEvent;

class TSlimRecoHACEvent : public TSlimRecoVEvent {
public:
    TSlimRecoHACEvent();
    explicit TSlimRecoHACEvent(TRecoHACEvent *evReco);
    virtual ~TSlimRecoHACEvent() {}

    void Reset(); //clears candidates and hits
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoHACCandidate cand) { fCandidates.emplace_back(std::move(cand)); }
    void AddHit(TSlimRecoHACHit hit)              { fHits.emplace_back(std::move(hit));        }

    Int_t GetNCandidates()                        const {return fCandidates.size(); }
    std::vector<TSlimRecoHACCandidate>& GetCandidates() {return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)     { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }
    std::vector<TSlimRecoHACHit>& GetHits()             {return fHits;              }
    Int_t GetNHits()                              const {return fHits.size();       }
    TSlimRecoVHit* GetHit(UInt_t iHit)                  { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }

    virtual void FromReco(TRecoVEvent *evVReco);
    virtual void ToReco(TRecoVEvent *evVReco);
private:
    std::vector<TSlimRecoHACHit> fHits;
    std::vector<TSlimRecoHACCandidate> fCandidates;

    ClassDef(TSlimRecoHACEvent, 1)
};

#endif /* TSLIMRECOHACEVENT_HH */

