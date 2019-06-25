#ifndef TSLIMRECOVHIT_H
#define TSLIMRECOVHIT_H

#include <TObject.h>

class TRecoVHit;

class TSlimRecoVHit : public TObject {
public:
    TSlimRecoVHit()          {}
    virtual ~TSlimRecoVHit() {}

    virtual void FromReco(TRecoVHit *hitReco) = 0;
    virtual void ToReco(TRecoVHit *hitReco)   = 0;

    ClassDef(TSlimRecoVHit, 1)
};


#endif /* TSLIMRECOVHIT_H */
