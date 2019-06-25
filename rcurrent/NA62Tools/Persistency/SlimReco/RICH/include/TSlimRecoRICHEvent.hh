#ifndef TRECORICHEVENTSLIM_H
#define TRECORICHEVENTSLIM_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoRICHCandidate.hh"
#include "TSlimRecoRICHHit.hh"

class TRecoRICHEvent;

class TSlimRecoRICHEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoRICHEvent()          = default;
    explicit TSlimRecoRICHEvent(TRecoRICHEvent *evReco);
    virtual ~TSlimRecoRICHEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddHit(TSlimRecoRICHHit h)                   { fHits.emplace_back(std::move(h));           }
    void AddTimeCandidate(TSlimRecoRICHCandidate c)   { fTimeCandidates.emplace_back(std::move(c)); }
    void AddRingCandidate(TSlimRecoRICHCandidate c)   { fRingCandidates.emplace_back(std::move(c)); }

    Int_t GetNHits()                                          const { return fHits.size();                                                                      }
    std::vector<TSlimRecoRICHHit>& GetHits()                        { return fHits;                                                                             }
    TSlimRecoVHit* GetHit(UInt_t iHit)                              { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr;                           }
    Int_t GetNPMTimeCandidates()                              const { return fTimeCandidates.size() - fNSCTimeCandidates;                                       }
    Int_t GetRingCandidates()                                 const { return fRingCandidates.size();                                                            }
    Int_t GetSCTimeCandidates()                               const { return fNSCTimeCandidates;                                                                }
    Int_t GetNTimeCandidates()                                const { return fTimeCandidates.size();                                                            }
    std::vector<TSlimRecoRICHCandidate>& GetTimeCandidates()        { return fTimeCandidates;                                                                   }
    std::vector<TSlimRecoRICHCandidate>& GetRingCandidates()        { return fRingCandidates;                                                                   }
    std::vector<TSlimRecoRICHCandidate> GetPMTimeCandidates()       {
        std::vector<TSlimRecoRICHCandidate> v; std::copy(fTimeCandidates.begin()+fNSCTimeCandidates, fTimeCandidates.end(), std::back_inserter(v)); return v;   }
    std::vector<TSlimRecoRICHCandidate> GetSCTimeCandidates()       {
        std::vector<TSlimRecoRICHCandidate> v; std::copy(fTimeCandidates.begin(), fTimeCandidates.begin()+fNSCTimeCandidates, std::back_inserter(v)); return v; }
    TSlimRecoVCandidate* GetTimeCandidate(UInt_t iCand)             { if(iCand<fTimeCandidates.size()) return &fTimeCandidates[iCand]; else return nullptr;     }
    TSlimRecoVCandidate* GetRingCandidate(UInt_t iCand)             { if(iCand<fRingCandidates.size()) return &fRingCandidates[iCand]; else return nullptr;     }
    TSlimRecoVCandidate* GetSCTimeCandidate(UInt_t iCand)           { if(iCand<fNSCTimeCandidates) return &fTimeCandidates[iCand]; else return nullptr;         }
    TSlimRecoVCandidate* GetPMTimeCandidate(UInt_t iCand)           {
        if(iCand<fTimeCandidates.size()-fNSCTimeCandidates) return &fTimeCandidates[iCand+fNSCTimeCandidates]; else return nullptr;                             }
    Int_t GetNCandidates()                                    const { return fTimeCandidates.size() + fRingCandidates.size();                                   }
    std::vector<TSlimRecoRICHCandidate> GetCandidates();
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)                 {
        if(iCand<fTimeCandidates.size()) return &fTimeCandidates[iCand];
        else if(iCand-fTimeCandidates.size() < fRingCandidates.size()) return &fRingCandidates[iCand-fTimeCandidates.size()];
        else return nullptr;
    }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    UShort_t fNSCTimeCandidates = 0;
    std::vector<TSlimRecoRICHHit>       fHits;
    std::vector<TSlimRecoRICHCandidate> fRingCandidates;
    std::vector<TSlimRecoRICHCandidate> fTimeCandidates;

    ClassDef(TSlimRecoRICHEvent, 1)
};
#endif /* TRECORICHEVENTSLIM_H */
