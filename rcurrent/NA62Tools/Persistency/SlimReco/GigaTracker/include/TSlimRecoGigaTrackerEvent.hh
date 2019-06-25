#ifndef TRECOGIGATRACKEREVENTSLIM_H
#define TRECOGIGATRACKEREVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoGigaTrackerCandidate.hh"
#include "TSlimRecoGigaTrackerHit.hh"

class TRecoGigaTrackerEvent;

class TSlimRecoGigaTrackerEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoGigaTrackerEvent() = default;
    explicit TSlimRecoGigaTrackerEvent(TRecoGigaTrackerEvent *evReco);
    virtual ~TSlimRecoGigaTrackerEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddHit(TSlimRecoGigaTrackerHit h)             { fHits.emplace_back(std::move(h));       }
    void AddCandidate(TSlimRecoGigaTrackerCandidate c) { fCandidates.emplace_back(std::move(c)); }

    Int_t GetNHits()                                            const { return fHits.size();       }
    std::vector<TSlimRecoGigaTrackerHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                                { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                      const { return fCandidates.size(); }
    std::vector<TSlimRecoGigaTrackerCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)                   { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoGigaTrackerHit> fHits;
    std::vector<TSlimRecoGigaTrackerCandidate> fCandidates;

    ClassDef(TSlimRecoGigaTrackerEvent, 1)
};

#endif /* TRECOGigaTrackerEVENTSLIM_H */
