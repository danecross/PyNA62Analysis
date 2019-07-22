// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-10
//
// ---------------------------------------------------------------

#ifndef MUV3ChannelID_H
#define MUV3ChannelID_H

#include "Rtypes.h"
#include "TVChannelID.hh"

class MUV3ChannelID {

public:

  MUV3ChannelID();
  virtual ~MUV3ChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); // returns position ID
  void  DecodeChannelID(Int_t);

  Int_t  GetTileID()      { return fTileID;       }
  Bool_t IsHigh()         { return fIsHigh;       }
  Bool_t IsInnerTile()    { return fIsInnerTile;  }
  Bool_t IsOuterTile()    { return !fIsInnerTile; }
  Int_t  GetPMTLocation() { return fPMTLocation;  }

private:

  Int_t  fTileID;
  Bool_t fIsHigh; ///< Channel with the higher ID in tile (N+200 rather than N)?
  Bool_t fIsInnerTile; ///< Outer (0-143) or inner (144-151) tile?
  Int_t  fPMTLocation; ///< Type of PMT location relative to the tile centre: kBottom, kTop, kJura, kSaleve

  ClassDef(MUV3ChannelID,1);
};
#endif
