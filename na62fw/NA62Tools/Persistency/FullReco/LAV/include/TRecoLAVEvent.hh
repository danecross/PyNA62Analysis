// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoLAVEvent_H
#define TRecoLAVEvent_H

#include "TRecoVEvent.hh"
#include "TRecoLAVCandidate.hh"
#include "TRecoLAVHit.hh"

class TRecoLAVEvent : public TRecoVEvent {

    public:

        TRecoLAVEvent();
        ~TRecoLAVEvent();

        void Clear(Option_t* = "");
  
    private:

        ClassDef(TRecoLAVEvent,1);
};
#endif
