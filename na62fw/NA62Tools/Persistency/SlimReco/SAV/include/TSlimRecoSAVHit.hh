#ifndef TSLIMRECOSAVHIT_H
#define TSLIMRECOSAVHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <TVector2.h>
#include <TVector3.h>

#include "TSlimRecoVHit.hh"

class TRecoSAVHit;

class TSlimRecoSAVHit : public TSlimRecoVHit
{
public:
    TSlimRecoSAVHit() = default;
    explicit TSlimRecoSAVHit(TRecoSAVHit *);
    virtual ~TSlimRecoSAVHit() = default;

    void SetTime(Float_t time)           { fTime = time;           }
    void SetAmplitude(Float_t amplitude) { fAmplitude = amplitude; }
    void SetBaseline(Float_t baseline)   { fBaseline = baseline;   }
    void SetEnergy(Float_t energy)       { fEnergy = energy;       }
    void AddEnergy(Float_t value)        { fEnergy += value;       }

    Float_t  GetTime()            const { return fTime;         }
    Float_t  GetAmplitude()       const { return fAmplitude;    }
    Float_t  GetBaseline()        const { return fBaseline;     }
    Float_t  GetEnergy()          const { return fEnergy;       }
    Int_t    GetChannelID()       const { return fChannelID;    }
    Int_t    GetChannel()         const { return fChannelID;    }
    Int_t    GetDetector()        const;
    Int_t    GetChannelDetector() const;
    TVector2 GetChannelPosition() const;
    TVector3 GetPosition()        const;

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Int_t   fChannelID = -999 ;
    Float_t fTime      = -999.;
    Float_t fAmplitude = -999.;
    Float_t fBaseline  = -999.;
    Float_t fEnergy    = -999.;

    ClassDef(TSlimRecoSAVHit, 1)
};

#endif /* TSLIMRECOSAVHIT_H */
