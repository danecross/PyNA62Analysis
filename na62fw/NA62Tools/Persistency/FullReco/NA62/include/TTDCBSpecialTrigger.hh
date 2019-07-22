// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-07-16
//
// --------------------------------------------------------------
#ifndef TTDCBSpecialTrigger_H
#define TTDCBSpecialTrigger_H
#include "TPrimSpecialTrigger.hh"

class TTDCBSpecialTrigger : public TPrimSpecialTrigger {

  public:

    TTDCBSpecialTrigger();
    ~TTDCBSpecialTrigger(){};
    void Clear(Option_t* = "");

  public:

    void SetFPGAID(Int_t value)             { fFPGAID=value;            };
    Int_t GetFPGAID()                       { return fFPGAID;           };

  private:

    Int_t   fFPGAID;

    ClassDef(TTDCBSpecialTrigger,1);
};
#endif
