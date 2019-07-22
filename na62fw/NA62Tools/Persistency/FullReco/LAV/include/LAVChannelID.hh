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
  static Int_t EncodeChannelIDFromInfo(Int_t LAVID, Int_t LayerID, Int_t BananaID, Int_t BlockID){return LAVID*10000 + LayerID*1000 + BananaID*10 + BlockID;}

  void  DecodeChannelID(Int_t);

  Int_t GetPackedChannelID();
  static Int_t GetPackedChannelIDFromCh(Int_t channelID);
public:

  Int_t GetLAVID() { return fLAVID;};
  void  SetLAVID(Int_t value){ fLAVID = value;};
  static Int_t GetLAVIDFromCh(Int_t ChannelID){return ChannelID/10000;}

  Int_t GetLayerID(){ return fLayerID;};
  void  SetLayerID(Int_t value){ fLayerID = value;};
  static Int_t GetLayerIDFromCh(Int_t ChannelID){return (ChannelID-10000*GetLAVIDFromCh(ChannelID))/1000;}

  Int_t GetBananaID(){ return fBananaID;};
  void  SetBananaID(Int_t value){ fBananaID = value;};
  static Int_t GetBananaIDFromCh(Int_t ChannelID){return (ChannelID-10000*GetLAVIDFromCh(ChannelID)-1000*GetLayerIDFromCh(ChannelID))/10;}


  Int_t GetBlockID(){ return fBlockID;};
  void  SetBlockID(Int_t value){ fBlockID = value;};
  static Int_t GetBlockIDFromCh(Int_t ChannelID){return ChannelID-10000*GetLAVIDFromCh(ChannelID)-1000*GetLayerIDFromCh(ChannelID)-10*GetBananaIDFromCh(ChannelID);}
private:

  Int_t fLAVID;
  Int_t fLayerID;
  Int_t fBananaID;
  Int_t fBlockID;

 ClassDef(LAVChannelID,1);
};
#endif
