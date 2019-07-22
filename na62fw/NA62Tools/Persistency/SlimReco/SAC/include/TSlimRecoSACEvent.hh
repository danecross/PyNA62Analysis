#ifndef TRECOSACEVENTSLIM_H
#define TRECOSACEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoSACCandidate.hh"
#include "TSlimRecoVEvent.hh"

class TRecoSACEvent;

class TSlimRecoSACEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoSACEvent() = default;
    explicit TSlimRecoSACEvent(TRecoSACEvent *evReco);
    virtual ~TSlimRecoSACEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearCandidates();

    void AddCandidate(TSlimRecoSACCandidate h) { fCandidates.emplace_back(std::move(h));  }

    Int_t GetNCandidates()                              const { return fCandidates.size(); }
    std::vector<TSlimRecoSACCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoSACCandidate> fCandidates;

    ClassDef(TSlimRecoSACEvent, 1)
};

#endif /* TRECOSACEVENTSLIM_H */
