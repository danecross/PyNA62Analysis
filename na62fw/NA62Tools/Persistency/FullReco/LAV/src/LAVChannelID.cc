// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-05-02
//
// --------------------------------------------------------------
#include "LAVChannelID.hh"

ClassImp(LAVChannelID)

LAVChannelID::LAVChannelID() :
  fLAVID   (-1),
  fLayerID (-1),
  fBananaID(-1),
  fBlockID (-1)
{
}

LAVChannelID::LAVChannelID(Int_t LAVID, Int_t LayerID, Int_t BananaID, Int_t BlockID) :
  fLAVID   (LAVID),
  fLayerID (LayerID),
  fBananaID(BananaID),
  fBlockID (BlockID)
{
}

Int_t LAVChannelID::EncodeChannelID() {
  /// \MemberDescr
  /// Returns geographic channel ID
  /// \EndMemberDescr
  return EncodeChannelIDFromInfo(fLAVID, fLayerID, fBananaID, fBlockID);
}

void LAVChannelID::DecodeChannelID(Int_t ChannelID) {
  /// \MemberDescr
  /// Converts geographic channel ID into Station, Layer, Banana, Block IDs 
  /// \EndMemberDescr
  fLAVID   = GetLAVIDFromCh(ChannelID); 
  fLayerID = GetLayerIDFromCh(ChannelID);
  fBananaID= GetBananaIDFromCh(ChannelID);
  fBlockID = GetBlockIDFromCh(ChannelID);
}




Int_t LAVChannelID::GetPackedChannelID(){
  /// \MemberDescr
  /// Returns geographical packed channel ID ranging from 0 to nBananas*4*nLayers
  /// \EndMemberDescr
  
  return GetPackedChannelIDFromCh(EncodeChannelIDFromInfo(fLAVID, fLayerID, fBananaID, fBlockID));
}

Int_t LAVChannelID::GetPackedChannelIDFromCh(Int_t channelID){
  Int_t iSt = GetLAVIDFromCh(channelID);
  Int_t numberOfBananas;
  if (iSt < 6) numberOfBananas = 8;
  else if (iSt < 9) numberOfBananas = 12;
  else if (iSt < 12) numberOfBananas = 15;
  else numberOfBananas = 16;
  
  Int_t ilay = GetLayerIDFromCh(channelID);
  Int_t iban = GetBananaIDFromCh(channelID); 
  Int_t ich = GetBlockIDFromCh(channelID); 
  return ich + 4*iban + numberOfBananas*4*ilay;
}

void LAVChannelID::Clear(Option_t* /*option*/){
      fLAVID    = -1;
      fLayerID  = -1;
      fBananaID = -1;
      fBlockID  = -1;
}
