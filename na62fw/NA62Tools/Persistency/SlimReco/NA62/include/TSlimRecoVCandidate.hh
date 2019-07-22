#ifndef TSLIMRECOVCANDIDATE_H
#define TSLIMRECOVCANDIDATE_H

#include <TObject.h>

class TRecoVCandidate;

class TSlimRecoVCandidate : public TObject {
public:
    TSlimRecoVCandidate()          {}
    virtual ~TSlimRecoVCandidate() {}

    virtual void FromReco(TRecoVCandidate *CandReco) = 0;
    virtual void ToReco(TRecoVCandidate *CandReco)   = 0;

    ClassDef(TSlimRecoVCandidate, 1)
};


#endif /* TSLIMRECOVCANDIDATE_H */
