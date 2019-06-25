#ifndef TSLIMRECOIRCHIT_H
#define TSLIMRECOIRCHIT_H

#include "TSlimRecoVHit.hh"

class TRecoIRCHit;

class TSlimRecoIRCHit : public TSlimRecoVHit
{
public:
    TSlimRecoIRCHit()              {}
    explicit TSlimRecoIRCHit(TRecoIRCHit *) {}
    virtual ~TSlimRecoIRCHit()     {}

    // conversion functions
    virtual void FromReco(TRecoVHit *) {}
    virtual void ToReco(TRecoVHit *)   {}
private:

    ClassDef(TSlimRecoIRCHit, 1)
};

#endif /* TSLIMRECOIRCHIT_H */
