#ifndef TSLIMRECOCEDARHIT_H
#define TSLIMRECOCEDARHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"

class TRecoCedarHit;

class TSlimRecoCedarHit : public TSlimRecoVHit
{
public:
    TSlimRecoCedarHit();
    explicit TSlimRecoCedarHit(TRecoCedarHit *hitReco);
    virtual ~TSlimRecoCedarHit() = default;

    // setters for members
    void SetWidth      (Float_t val) { fWidth  = val;    }
    void SetChannelID(Short_t val)   { fChannelID = val; }
    void SetTime(Float_t val)        { fTime = val;      }

    // getters for members
    Float_t GetWidth()       const  { return fWidth;     }
    Short_t GetChannelID()   const  { return fChannelID; }
    Float_t GetTime()        const  { return fTime;      }
    Int_t   GetSectorID()    const;
    Int_t   GetRowID()       const;
    Int_t   GetConeID()      const;

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Short_t fChannelID;
    Float_t fWidth;
    Float_t fTime;

    ClassDef(TSlimRecoCedarHit, 1)
};

#endif /* TSLIMRECOCEDARHIT_H */
