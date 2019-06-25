#ifndef TSLIMRECOCHANTICANDIDATE_H
#define TSLIMRECOCHANTICANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVCandidate.hh"

class TRecoCHANTICandidate;

class TSlimRecoCHANTICandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoCHANTICandidate()  = default;
    explicit TSlimRecoCHANTICandidate(TRecoCHANTICandidate *candReco);
    virtual ~TSlimRecoCHANTICandidate() = default;

    // setters for members
    void SetXYMult(UShort_t XYMult)     { fXYMult = XYMult;                 }
    void SetXPos(Float_t XPos)          { fXPos = XPos;                     }
    void SetYPos(Float_t YPos)          { fYPos = YPos;                     }
    void SetTime(Float_t time)          { fTime = time;                     }
    void SetXPCharge(Float_t XPCharge)  { fXPCharge = XPCharge;             }
    void SetYPCharge(Float_t YPCharge)  { fYPCharge = YPCharge;             }
    void AddHitIndex(Short_t index)     { fHitsIndexes.emplace_back(index); }

    // getters for members
    UShort_t GetXYMult()                         const { return fXYMult;      }
    Float_t  GetXPos()                           const { return fXPos;        }
    Float_t  GetYPos()                           const { return fYPos;        }
    Float_t  GetTime()                           const { return fTime;        }
    Float_t  GetXPCharge()                       const { return fXPCharge;    }
    Float_t  GetYPCharge()                       const { return fYPCharge;    }
    const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes; }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
private:
    UShort_t fXYMult   = 0;
    Float_t  fXPos     = -999;
    Float_t  fYPos     = -999;
    Float_t fTime     = 0;
    Float_t fXPCharge = -999;
    Float_t fYPCharge = -999;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoCHANTICandidate, 1)
};

#endif /* TSLIMRECOCHANTICANDIDATE_H */
