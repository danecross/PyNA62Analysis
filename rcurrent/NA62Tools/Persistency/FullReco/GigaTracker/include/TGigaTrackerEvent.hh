// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TGigaTrackerEvent_H
#define TGigaTrackerEvent_H

#include "TDetectorVEvent.hh"

class TGigaTrackerEvent : public TDetectorVEvent {

    public:
  
        TGigaTrackerEvent();
        ~TGigaTrackerEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TGigaTrackerEvent,1);
};
#endif
