#ifndef TSLIMRECOSAVCANDIDATE_H
#define TSLIMRECOSAVCANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVCandidate.hh"
#include <TVector3.h>

class TRecoSAVCandidate;

class TSlimRecoSAVCandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoSAVCandidate() = default;
    explicit TSlimRecoSAVCandidate(TRecoSAVCandidate *);
    virtual ~TSlimRecoSAVCandidate() = default;

    // setters for members
    void SetTime(Float_t value)              { fTime=value;                      }
    void SetEnergy(Float_t value)            { fEnergy = value;                  }
    void SetPosition(Float_t x, Double_t y ) { fX = x; fY = y;                   }
    void SetPosition(TVector2 value )        { fX = value.X(); fY = value.Y();   }
    void AddHitIndex(Short_t index)          { fHitsIndexes.emplace_back(index); }

    Int_t GetNHits()                       const { return fHitsIndexes.size(); }
    std::vector<Short_t>& GetHitsIndexes()       { return fHitsIndexes;        }
    Float_t GetTime()                      const { return fTime;               }
    Float_t GetEnergy()                    const { return fEnergy;             }
    Float_t GetX()                         const { return fX;                  }
    Float_t GetY()                         const { return fY;                  }
    TVector2 GetPosition()                 const { return TVector2(fX,fY);     }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candVReco);
    virtual void ToReco(TRecoVCandidate *candVReco);
private:
    Float_t fTime   = -999.;
    Float_t fEnergy = -999.;
    Float_t fX      = -999.;
    Float_t fY      = -999.;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoSAVCandidate, 1)
};

#endif /* TSLIMRECOSAVCANDIDATE_H */
