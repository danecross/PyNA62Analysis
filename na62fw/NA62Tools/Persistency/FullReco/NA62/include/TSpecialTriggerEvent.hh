// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TSpecialTriggerEvent_H
#define TSpecialTriggerEvent_H
#include "TClass.h"
#include "TDetectorVEvent.hh"
#include "TSpecialTrigger.hh"

class TSpecialTriggerEvent : public TDetectorVEvent {

  public:

        TSpecialTriggerEvent();
        explicit TSpecialTriggerEvent(TClass *);
        TSpecialTrigger * AddSpecialTrigger();
        TSpecialTrigger * LastSpecialTrigger();
        void Clear(Option_t* = "");
        ~TSpecialTriggerEvent();

    public:

        Int_t GetNSpecialTriggers()                      { return GetNHits();               };
        //TSpecialTrigger * GetSpecialTrigger()            { return fSOB;                     };


    private:

        //TSpecialTrigger *fSOB;

        ClassDef(TSpecialTriggerEvent,1);

};
#endif
