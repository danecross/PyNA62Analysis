#ifndef TRECOIRCEVENTSLIM_H
#define TRECOIRCEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoIRCCandidate.hh"
#include "TSlimRecoVEvent.hh"

class TRecoIRCEvent;

class TSlimRecoIRCEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoIRCEvent() = default;
    explicit TSlimRecoIRCEvent(TRecoIRCEvent *evReco);
    virtual ~TSlimRecoIRCEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearCandidates();

    void AddCandidate(TSlimRecoIRCCandidate h) { fCandidates.emplace_back(std::move(h));  }

    Int_t GetNCandidates()                              const { return fCandidates.size(); }
    std::vector<TSlimRecoIRCCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoIRCCandidate> fCandidates;

    ClassDef(TSlimRecoIRCEvent, 1)
};

#endif /* TRECOIRCEVENTSLIM_H */
