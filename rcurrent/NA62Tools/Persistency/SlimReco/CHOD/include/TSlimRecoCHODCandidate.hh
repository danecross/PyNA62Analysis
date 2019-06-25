#ifndef TSLIMRECOCHODCANDIDATE_H
#define TSLIMRECOCHODCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>
#include "TVector2.h"

#include "TSlimRecoVCandidate.hh"

class TRecoCHODCandidate;

class TSlimRecoCHODCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoCHODCandidate() = default;
    explicit TSlimRecoCHODCandidate(TRecoCHODCandidate *candReco);
    virtual ~TSlimRecoCHODCandidate() = default;

    // setters for members
    void SetTime(Float_t time)          { fTime = time;                                         }
    void SetPosition(TVector2 position) { fPositionX = position.X(); fPositionY = position.Y(); }
    void SetNHitPairs(Int_t nhitpairs)  { fNHitPairs = nhitpairs;                               }
    void AddHitIndex(Short_t index)     { fHitsIndexes.emplace_back(index);                     }

    // getters for members
    Int_t    GetNHitsPairs()                     const { return fNHitPairs;                          }
    Float_t  GetTime()                           const { return fTime;                               }
    TVector2 GetPosition()                       const { return TVector2(fPositionX, fPositionY);    }
    const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes;                        }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
private:

    Int_t   fNHitPairs   = -9999;
    Float_t fTime        = -9999;
    Float_t fPositionX   = -9999;
    Float_t fPositionY   = -9999;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoCHODCandidate, 1)
};


#endif /* TSLIMRECOCHODCANDIDATE_H */
