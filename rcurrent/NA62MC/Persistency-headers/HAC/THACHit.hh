#ifndef THACHit_H
#define THACHit_H

#include "TDetectorVHit.hh"
#include "HACChannelID.hh"

class THACHit : public TDetectorVHit, public HACChannelID {

  public:

    THACHit();
    ~THACHit(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

  public:

    Int_t                GetScintillatorID()                                { return fScintillatorID;               };
    void                 SetScintillatorID(Int_t value)                     { fScintillatorID = value;              };

  protected:

    Int_t      fScintillatorID;

    ClassDef(THACHit,1);
};
#endif
