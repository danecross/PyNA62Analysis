//
//  MUV2ChannelID.hh
//
//
//  Created by riccardo aliberti on 10/10/14.
//
//

#ifndef _MUV2ChannelID_hh
#define _MUV2ChannelID_hh

#include "Rtypes.h"
#include "TVChannelID.hh"

class MUV2ChannelID {

  public:
    MUV2ChannelID();
    explicit MUV2ChannelID(Int_t ChannelID);
    virtual ~MUV2ChannelID();

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void DecodeChannelID(Int_t ChannelID);


  Double_t GetScintillatorPosition ();

  Int_t GetScintillatorOrientation (){ return std::max(fSide%2,0); }

  Int_t GetPlane ();

  Int_t GetQuadrant ();

  void  SetSide (Int_t side) { fSide = side; }
  Int_t GetSide () { return fSide; }

  void  SetScintillatorNumber (Int_t num) { fScintillatorNumber = num; }
  Int_t GetScintillatorNumber (){ return fScintillatorNumber; }

  
  private:
    Int_t fScintillatorNumber;
    Int_t fSide;
    Double_t fScintillatorPosition;


};


#endif
