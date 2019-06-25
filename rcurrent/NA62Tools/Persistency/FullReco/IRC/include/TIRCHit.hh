// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TIRCHit_H
#define TIRCHit_H

#include "TDetectorVHit.hh"
#include "IRCChannelID.hh"

class TIRCHit : public TDetectorVHit, public IRCChannelID {

  public:

    TIRCHit();
    ~TIRCHit(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

  public:

    Int_t                GetScintillatorID()                                { return fScintillatorID;               };
    void                 SetScintillatorID(Int_t value)                     { fScintillatorID = value;              };

  protected:

    Int_t      fScintillatorID;

    ClassDef(TIRCHit,1);
};
#endif
