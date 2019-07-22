#ifndef TSLIMRECORICHHIT_H
#define TSLIMRECORICHHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"
#include "TVector3.h"

class TRecoRICHHit;

class TSlimRecoRICHHit : public TSlimRecoVHit {
  public:
    TSlimRecoRICHHit()          = default;
    explicit TSlimRecoRICHHit(TRecoRICHHit *hitReco);
    virtual ~TSlimRecoRICHHit() = default;

    // setters for members
    void SetTime(Float_t value)         { fTime = value;                                          }
    void SetHitQuality(Short_t value)   { fHitQuality = value;                                    }
    void SetTimeWidth(Float_t value)    { fTimeWidth = value;                                     }
    void SetChannelID(Int_t value)      { fChannelID = value;                                     }
    void SetPtolemy(Float_t value)      { fPtolemy = value;                                       }
    void SetFitPosition(TVector3 value) { fFitPosition_X = value.X(); fFitPosition_Y = value.Y(); }

    // getters for members
    Float_t  GetTime()        const { return fTime;                                        }
    Short_t  GetHitQuality()  const { return fHitQuality;                                  }
    Float_t  GetTimeWidth()   const { return fTimeWidth;                                   }
    Int_t    GetChannelID()   const { return fChannelID;                                   }
    Float_t  GetPtolemy()     const { return fPtolemy;                                     }
    TVector3 GetFitPosition() const { return TVector3(fFitPosition_X, fFitPosition_Y, 0.); }

    //Int_t GetChannelSeqID()                                                { return fChannelSeqID;                 }
    Int_t GetDiskID()        const;
    Int_t GetUpDownDiskID()  const;
    Int_t GetSuperCellID()   const;
    Int_t GetOrSuperCellID() const;
    Int_t GetPmtID()         const;

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);

  private:

    Int_t   fChannelID     =    -1;
    Float_t fTime          = -999.;
    Short_t fHitQuality    =    -1;
    Float_t fTimeWidth     = -999.;
    Float_t fPtolemy       = -999.;
    Float_t fFitPosition_X =  999.; ///< HitPosition corrected including mirror inclination
    Float_t fFitPosition_Y =  999.; ///< HitPosition corrected including mirror inclination

    ClassDef(TSlimRecoRICHHit, 1)
};

#endif /* TSLIMRECORICHHIT_H */
