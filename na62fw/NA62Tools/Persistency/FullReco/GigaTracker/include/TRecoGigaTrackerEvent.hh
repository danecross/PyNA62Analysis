// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoGigaTrackerEvent_H
#define TRecoGigaTrackerEvent_H

#include "TRecoVEvent.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "TRecoGigaTrackerHit.hh"

class TRecoGigaTrackerEvent : public TRecoVEvent {

    public:

        TRecoGigaTrackerEvent();
        TRecoGigaTrackerEvent(const TRecoGigaTrackerEvent &);
        ~TRecoGigaTrackerEvent();

        void SetHitNbFromPrevL0(int gtk, int chip, int half, int n);
        int GetHitNbFromPrevL0(int gtk, int chip, int half = -1);

        void Clear(Option_t* = "");

    private:

        int fHitNbFromPrevL0[3][10][2];
        ClassDef(TRecoGigaTrackerEvent,1);

};
#endif
