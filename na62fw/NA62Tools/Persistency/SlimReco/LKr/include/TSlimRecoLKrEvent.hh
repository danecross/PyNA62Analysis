#ifndef TSlimRecoLKrEvent_H
#define TSlimRecoLKrEvent_H
#include <RtypesCore.h>
#include "TSlimRecoVEvent.hh"
#include "TSlimRecoLKrCandidate.hh"
#include "TSlimRecoLKrHit.hh"

class TRecoLKrEvent;

class TSlimRecoLKrEvent : public TSlimRecoVEvent {

  public:
    TSlimRecoLKrEvent() = default;
    explicit TSlimRecoLKrEvent(TRecoLKrEvent *evVReco);
    virtual ~TSlimRecoLKrEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void SetRecFlag(Int_t val)                 { fRecFlag=val;                           }
    void SetEnergyTotal(Double_t val)          { fEnergyTotal=val;                       }
    void AddHit(TSlimRecoLKrHit h)             { fHits.emplace_back(std::move(h));       }
    void AddCandidate(TSlimRecoLKrCandidate c) { fCandidates.emplace_back(std::move(c)); }

    Int_t GetRecFlag()                                  const { return fRecFlag;           }
    Double_t GetEnergyTotal()                           const { return fEnergyTotal;       }
    Int_t GetNHits()                                    const { return fHits.size();       }
    std::vector<TSlimRecoLKrHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                        { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                              const { return fCandidates.size(); }
    std::vector<TSlimRecoLKrCandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    void FromReco(TRecoVEvent *evVReco);
    void ToReco(TRecoVEvent *evVReco);
  private:
    Int_t     fRecFlag     = 0;
    Double_t  fEnergyTotal = 0.;
    std::vector<TSlimRecoLKrHit> fHits;
    std::vector<TSlimRecoLKrCandidate> fCandidates;

    ClassDef(TSlimRecoLKrEvent, 1)
};
#endif
