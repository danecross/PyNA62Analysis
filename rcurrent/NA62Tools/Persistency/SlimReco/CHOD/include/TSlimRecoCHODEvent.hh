#ifndef TRECOCHODEVENTSLIM_H
#define TRECOCHODEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoCHODCandidate.hh"
#include "TSlimRecoCHODHit.hh"

class TRecoCHODEvent;

class TSlimRecoCHODEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoCHODEvent() = default;
    explicit TSlimRecoCHODEvent(TRecoCHODEvent *evReco);
    virtual ~TSlimRecoCHODEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoCHODCandidate c)     { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoCHODHit h)                 { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                         const { return fHits.size();           }
    std::vector<TSlimRecoCHODHit>& GetHits()                       { return fHits;                  }
    TSlimRecoVHit* GetHit(UInt_t iHit)                             { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                   const { return fCandidates.size();     }
    std::vector<TSlimRecoCHODCandidate>& GetCandidates()           { return fCandidates;            }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)                { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }
    Int_t GetNQuadrants()                                    const;

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoCHODHit> fHits;
    std::vector<TSlimRecoCHODCandidate> fCandidates;

    ClassDef(TSlimRecoCHODEvent, 1)
};

#endif /* TRECOCHODEVENTSLIM_H */
