#ifndef PERSISTENCY_SLIM_INCLUDE_TSLIMRECOVEVENT_HH_
#define PERSISTENCY_SLIM_INCLUDE_TSLIMRECOVEVENT_HH_

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <TObject.h>

class TRecoVEvent;
class TSlimRecoVHit;
class TSlimRecoVCandidate;

class TSlimRecoVEvent : public TObject{
public:
    TSlimRecoVEvent()          = default;
    virtual ~TSlimRecoVEvent() = default;

    virtual void Reset();
    virtual void ClearHits();
    virtual void ClearCandidates();

    void SetErrorMask(ULong64_t mask)       { fErrorMask = mask; }

    ULong64_t GetErrorMask()          const { return fErrorMask; }
    virtual TSlimRecoVHit* GetHit(UInt_t ) { return nullptr; }
    virtual TSlimRecoVCandidate* GetCandidate(UInt_t ) { return nullptr; }

    virtual void FromReco(TRecoVEvent *evReco) = 0;
    virtual void ToReco(TRecoVEvent *evReco)   = 0;
private:
    ULong64_t fErrorMask = 0;

    ClassDef(TSlimRecoVEvent, 1)
};

#endif /* PERSISTENCY_SLIM_INCLUDE_TSLIMRECOVEVENT_HH_ */
