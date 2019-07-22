#ifndef TSLIMRECOHACCANDIDATE_H
#define TSLIMRECOHACCANDIDATE_H

#include <vector>
#include "RtypesCore.h"

#include "TSlimRecoVCandidate.hh"

class TRecoHACCandidate;

class TSlimRecoHACCandidate : public TSlimRecoVCandidate {
public:
    TSlimRecoHACCandidate();
    explicit TSlimRecoHACCandidate(TRecoHACCandidate *candReco);
    virtual ~TSlimRecoHACCandidate() { fHitsIndexes.clear(); }

    //Getters and Setters for data members
    void SetTime(Float_t time)      { fTime = time;                     }
    void AddHitIndex(Short_t index) { fHitsIndexes.emplace_back(index); }
    void SetCharge(Float_t charge)  { fCharge = charge;                 }

    Float_t GetTime()                            const { return fTime;        }
    const std::vector<Short_t>& GetHitsIndices() const { return fHitsIndexes; }
    Float_t GetCharge()                          const { return fCharge;      }

    //Methods for standard persistency
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
private:
    Float_t fTime;
    Float_t fCharge;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoHACCandidate, 1)
};

#endif /* TSLIMRECOHACCANDIDATE_HH */

