#ifndef TRECOMUV0EVENTSLIM_H
#define TRECOMUV0EVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoMUV0Candidate.hh"
#include "TSlimRecoVEvent.hh"

class TRecoMUV0Event;

class TSlimRecoMUV0Event : public TSlimRecoVEvent
{
public:
    TSlimRecoMUV0Event() = default;
    explicit TSlimRecoMUV0Event(TRecoMUV0Event *evReco);
    virtual ~TSlimRecoMUV0Event() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearCandidates();

    void AddCandidate(TSlimRecoMUV0Candidate h) { fCandidates.emplace_back(std::move(h));  }

    Int_t GetNCandidates()                               const { return fCandidates.size(); }
    std::vector<TSlimRecoMUV0Candidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)            { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoMUV0Candidate> fCandidates;

    ClassDef(TSlimRecoMUV0Event, 1)
};

#endif /* TRECOMUV0EVENTSLIM_H */
