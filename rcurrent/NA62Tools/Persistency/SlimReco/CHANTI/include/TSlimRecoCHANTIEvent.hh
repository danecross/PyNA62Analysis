#ifndef TRECOCHANTIEVENTSLIM_H
#define TRECOCHANTIEVENTSLIM_H

#include <RtypesCore.h>
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoCHANTICandidate.hh"
#include "TSlimRecoCHANTIHit.hh"

class TRecoCHANTIEvent;

class TSlimRecoCHANTIEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoCHANTIEvent()  = default;
    explicit TSlimRecoCHANTIEvent(TRecoCHANTIEvent *evReco);
    virtual ~TSlimRecoCHANTIEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoCHANTICandidate c) { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoCHANTIHit h)             { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                       const { return fHits.size();       }
    std::vector<TSlimRecoCHANTIHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                           { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                 const { return fCandidates.size(); }
    std::vector<TSlimRecoCHANTICandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)              { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoCHANTIHit> fHits;
    std::vector<TSlimRecoCHANTICandidate> fCandidates;

    ClassDef(TSlimRecoCHANTIEvent, 1)
};

#endif /* TRECOCHANTISLIM_H */
