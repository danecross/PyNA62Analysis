#ifndef TRecoMUV0Event_H
#define TRecoMUV0Event_H

#include "TRecoVEvent.hh"
#include "TRecoMUV0Candidate.hh"
#include "TRecoMUV0Hit.hh"

class TRecoMUV0Event : public TRecoVEvent {

    public:

        TRecoMUV0Event();
        ~TRecoMUV0Event();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoMUV0Event,1);
};
#endif
