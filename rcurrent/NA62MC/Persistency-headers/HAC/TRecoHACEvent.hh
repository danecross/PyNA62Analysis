#ifndef TRecoHACEvent_H
#define TRecoHACEvent_H

#include "TRecoVEvent.hh"
#include "TRecoHACCandidate.hh"
#include "TRecoHACHit.hh"

class TRecoHACEvent : public TRecoVEvent {

    public:

        TRecoHACEvent();
        ~TRecoHACEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoHACEvent,1);
};
#endif
