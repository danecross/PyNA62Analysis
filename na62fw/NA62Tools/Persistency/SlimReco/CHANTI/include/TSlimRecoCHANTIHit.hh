#ifndef TSLIMRECOCHANTIHIT_H
#define TSLIMRECOCHANTIHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"

class TRecoCHANTIHit;

class TSlimRecoCHANTIHit : public TSlimRecoVHit
{
public:
    TSlimRecoCHANTIHit()  = default;
    explicit TSlimRecoCHANTIHit(TRecoCHANTIHit *hitReco);
    virtual ~TSlimRecoCHANTIHit() = default;

    // setters for members
    void SetChannelID(Int_t channelID)            { fChannelID = channelID;         }
    void SetQualityFlag(UShort_t QualityFlag)     { fQualityFlag = QualityFlag;     }
    void SetThresholdFlag(UShort_t ThresholdFlag) { fThresholdFlag = ThresholdFlag; }
    void SetMult(UShort_t Mult)                   { fMult = Mult;                   }
    void SetTime(Float_t time)                    { fTime = time;                   }
    void SetTimeWidth(Float_t width)              { fTimeWidth = width;             }
    void SetDeltaTime(Float_t DeltaTime)          { fDeltaTime = DeltaTime;         }
    void SetDeltaWidth(Float_t DeltaWidth)        { fDeltaWidth = DeltaWidth;       }

    // getters for members
    Int_t    GetChannelID()     const { return fChannelID;     };
    UShort_t GetQualityFlag()   const { return fQualityFlag;   };
    UShort_t GetThresholdFlag() const { return fThresholdFlag; };
    UShort_t GetMult()          const { return fMult;          };
    Float_t  GetTime()          const { return fTime;          };
    Float_t  GetTimeWidth()     const { return fTimeWidth;     };
    Float_t  GetDeltaTime()     const { return fDeltaTime;     };
    Float_t  GetDeltaWidth()    const { return fDeltaWidth;    };

    // getters for variables from Standard Persistency interface
    UShort_t GetPlaneID()  const;
    UShort_t GetRingType() const;
    UShort_t GetRingID()   const;
    UShort_t GetSideID()   const;
    Short_t  GetBarID()    const;
    Float_t  GetX()        const;
    Float_t  GetY()        const;
    Float_t  GetZ()        const;

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Int_t    fChannelID     = 0;
    UShort_t fQualityFlag   = 0;
    UShort_t fThresholdFlag = 0;
    UShort_t fMult          = 0;
    Float_t  fTime          = -999;
    Float_t  fTimeWidth     = -999;
    Float_t  fDeltaTime     = -999;
    Float_t  fDeltaWidth    = -999;

    ClassDef(TSlimRecoCHANTIHit, 1)
};

#endif /* TSLIMRECOCHANTIHIT_H */
