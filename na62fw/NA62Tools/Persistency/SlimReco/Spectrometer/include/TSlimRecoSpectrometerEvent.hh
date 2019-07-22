#ifndef TRECOSPECTROMETEREVENTSLIM_H
#define TRECOSPECTROMETEREVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoSpectrometerCandidate.hh"
#include "TSlimRecoSpectrometerHit.hh"

class TRecoSpectrometerEvent;

class TSlimRecoSpectrometerEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoSpectrometerEvent() = default;
    explicit TSlimRecoSpectrometerEvent(TRecoSpectrometerEvent *evReco);
    virtual ~TSlimRecoSpectrometerEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoSpectrometerCandidate c) { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoSpectrometerHit h)             { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                             const { return fHits.size();       }
    std::vector<TSlimRecoSpectrometerHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                                 { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                       const { return fCandidates.size(); }
    std::vector<TSlimRecoSpectrometerCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)                    { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoSpectrometerHit> fHits;
    std::vector<TSlimRecoSpectrometerCandidate> fCandidates;

    ClassDef(TSlimRecoSpectrometerEvent, 1)
};

#endif /* TRECOSPECTROMETEREVENTSLIM_H */
