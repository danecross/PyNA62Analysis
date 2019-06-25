// --------------------------------------------------------------
// History:
//
// Modified by Giuseppe Ruggiero 2012
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//            Evelina.Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------
#ifndef FADCEvent_H
#define FADCEvent_H
#include "TClass.h"
#include "TDigiVEvent.hh"
#include "FADCVHit.hh"


class FADCEvent : public TDigiVEvent {

  public:

    FADCEvent();
    explicit FADCEvent(TClass *);
    FADCEvent(TClass *, Int_t);
    FADCVHit* GetHit(Int_t);
    void Clear(Option_t* = "");

  public:
    Int_t    GetFADCID() {return fFADCID;};
    void     SetFADCID(Int_t val) {fFADCID = val;};
    Int_t    GetNSamples() {return fNSamples;};
    void     SetNSamples(Int_t val) {fNSamples = val;};
    Int_t    GetEventFlag() {return fEventFlag;};
    void     SetEventFlag(Int_t val) {fEventFlag = val;};

  private:
    Int_t fFADCID; // fFADCID = 10*a+b
    //                a = 1 if zero suppression algorithm applied, else 0
    //                b = 1 if pulses in energy units (GeV)
    //                b = 0 if pulses in units of ADC counts 
    Int_t fNSamples;  // Nb of ~25 nsec samplings stored
    Int_t fEventFlag; // Flag problems in this event
    //                   bit  1 set : at least one saturated pulse
    //                   bit  4 set : at least one pulse in underflow
    //                   bit 12 set : at least one L1 error

    ClassDef(FADCEvent,1);
};
#endif
