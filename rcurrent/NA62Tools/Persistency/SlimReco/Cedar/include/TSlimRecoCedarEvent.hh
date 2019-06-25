#ifndef TRECOCEDAREVENTSLIM_H
#define TRECOCEDAREVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoCedarCandidate.hh"
#include "TSlimRecoCedarHit.hh"

class TRecoCedarEvent;

class TSlimRecoCedarEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoCedarEvent();
    explicit TSlimRecoCedarEvent(TRecoCedarEvent *evReco);
    virtual ~TSlimRecoCedarEvent() {}

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoCedarCandidate c) { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoCedarHit h)             { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                      const { return fHits.size();       }
    std::vector<TSlimRecoCedarHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                          { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                const { return fCandidates.size(); }
    std::vector<TSlimRecoCedarCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)             { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoCedarHit> fHits;
    std::vector<TSlimRecoCedarCandidate> fCandidates;

    ClassDef(TSlimRecoCedarEvent, 1)
};
#endif /* TRECOCEDAREVENTSLIM_H */
