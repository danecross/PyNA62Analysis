//
//  MUV1ChannelID.hh
//
//
//  Created by riccardo aliberti on 10/10/14.
//
//

#ifndef _MUV1ChannelID_hh
#define _MUV1ChannelID_hh

#include "Rtypes.h"
#include "TVChannelID.hh"

class MUV1ChannelID {

public:
  MUV1ChannelID();
  explicit MUV1ChannelID(Int_t ChannelID);
  virtual ~MUV1ChannelID();

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void DecodeChannelID(Int_t ChannelID);

  
  Double_t GetScintillatorPosition ();

  Int_t GetScintillatorOrientation (){ return (fSide%2); }

  Int_t GetPlane ();

  Int_t GetQuadrant ();

  void  SetSide (Int_t side) { fSide = side; }
  Int_t GetSide () { return fSide; }

  void  SetScintillatorNumber (Int_t num) { fScintillatorNumber = num; }
  Int_t GetScintillatorNumber (){ return fScintillatorNumber; }

  Bool_t IsLongScintillator ();


private:
  Int_t fScintillatorNumber;
  Int_t fSide;

};


#endif
