//
//  SAVChannelID.hh
//
//
//  Created by letizia peruzzo on 02/06/2016.
//
//

#ifndef _SAVChannelID_hh
#define _SAVChannelID_hh

#include "Rtypes.h"
#include "TVector2.h"

class SAVChannelID {

  public:
    struct chIDDecoded { Int_t fDetectorID, fDetectorChannel; };

    SAVChannelID();
    explicit SAVChannelID(Int_t ChannelID);
    virtual ~SAVChannelID();

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    static struct chIDDecoded DecodeChannelID_Static(Int_t ChannelID);
    void DecodeChannelID(Int_t ChannelID);

    Int_t GetDetector() {return fDetectorID;}
    Int_t GetChannel() {return fDetectorID*10 + fDetectorChannel;}
    Int_t GetChannelDetector() {return fDetectorChannel;}
  
    TVector2 GetChannelPosition();
  
  protected:
    Int_t fDetectorID;
    Int_t fDetectorChannel;
};


#endif
