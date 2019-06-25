#ifndef TSLIMRECOSACHIT_H
#define TSLIMRECOSACHIT_H

#include "TSlimRecoVHit.hh"

class TRecoSACHit;

class TSlimRecoSACHit : public TSlimRecoVHit
{
public:
    TSlimRecoSACHit()              {}
    explicit TSlimRecoSACHit(TRecoSACHit *) {}
    virtual ~TSlimRecoSACHit()     {}

    // conversion functions
    virtual void FromReco(TRecoVHit *) {}
    virtual void ToReco(TRecoVHit *)   {}
private:

    ClassDef(TSlimRecoSACHit, 1)
};

#endif /* TSLIMRECOSACHIT_H */
