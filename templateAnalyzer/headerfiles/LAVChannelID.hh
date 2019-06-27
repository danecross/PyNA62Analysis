// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-05-02
//
// First implementation and revision by T. Spadaro and E. Leonardi
// --------------------------------------------------------------
#ifndef LAVChannelID_H
#define LAVChannelID_H
#include "Rtypes.h"

class LAVChannelID {

public:

  LAVChannelID();
  LAVChannelID(Int_t, Int_t, Int_t, Int_t);
  virtual ~LAVChannelID() {}
  void Clear(Option_t* = "");
  Int_t EncodeChannelID();      
  void  DecodeChannelID(Int_t);
  Int_t GetPackedChannelID();
public:

  Int_t GetLAVID() { return fLAVID;};
  void  SetLAVID(Int_t value){ fLAVID = value;};
  Int_t GetLayerID(){ return fLayerID;};
  void  SetLayerID(Int_t value){ fLayerID = value;};
  Int_t GetBananaID(){ return fBananaID;};
  void  SetBananaID(Int_t value){ fBananaID = value;};
  Int_t GetBlockID(){ return fBlockID;};
  void  SetBlockID(Int_t value){ fBlockID = value;};

private:

  Int_t fLAVID;
  Int_t fLayerID;
  Int_t fBananaID;
  Int_t fBlockID;

 ClassDef(LAVChannelID,1);
};
#endif
