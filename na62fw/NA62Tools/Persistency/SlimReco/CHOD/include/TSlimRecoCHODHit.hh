#ifndef TSLIMRECOCHODHIT_H
#define TSLIMRECOCHODHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"

class TRecoCHODHit;

class TSlimRecoCHODHit : public TSlimRecoVHit
{
public:
    TSlimRecoCHODHit() = default;
    explicit TSlimRecoCHODHit(TRecoCHODHit *hitReco);
    virtual ~TSlimRecoCHODHit() = default;

    // setters for members
    void SetChannelID(Int_t val)   { fChannelID = val; }
    void SetTime(Float_t val)      { fTime = val;      }
    void SetTimeWidth(Float_t val) { fTimeWidth = val; }

    // getters for members
    Int_t   GetPlaneID()    const;
    Int_t   GetQuadrantID() const;
    Int_t   GetCounterID()  const;
    Int_t   GetChannelID()  const { return fChannelID; }
    Float_t GetTime()       const { return fTime;      }
    Float_t GetTimeWidth()  const { return fTimeWidth; }

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:

    Int_t   fChannelID = -1;
    Float_t fTime      = -999;
    Float_t fTimeWidth = -999;

    ClassDef(TSlimRecoCHODHit, 1)
};

#endif /* TSLIMRECOCHODHIT_H */
