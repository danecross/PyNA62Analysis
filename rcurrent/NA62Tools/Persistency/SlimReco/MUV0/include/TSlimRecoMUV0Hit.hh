#ifndef TSLIMRECOMUV0HIT_H
#define TSLIMRECOMUV0HIT_H

#include "TSlimRecoVHit.hh"

class TRecoMUV0Hit;

class TSlimRecoMUV0Hit : public TSlimRecoVHit
{
public:
    TSlimRecoMUV0Hit()               {}
    explicit TSlimRecoMUV0Hit(TRecoMUV0Hit *) {}
    virtual ~TSlimRecoMUV0Hit()      {}

    // conversion functions
    virtual void FromReco(TRecoVHit *) {}
    virtual void ToReco(TRecoVHit *)   {}
private:

    ClassDef(TSlimRecoMUV0Hit, 1)
};

#endif /* TSLIMRECOMUV0HIT_H */
