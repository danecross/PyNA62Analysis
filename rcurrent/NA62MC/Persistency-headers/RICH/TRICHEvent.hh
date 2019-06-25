// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TRICHEvent_H
#define TRICHEvent_H

#include "TDetectorVEvent.hh"

class TRICHEvent : public TDetectorVEvent {

    public:

        TRICHEvent();
        ~TRICHEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRICHEvent,1);
};
#endif
