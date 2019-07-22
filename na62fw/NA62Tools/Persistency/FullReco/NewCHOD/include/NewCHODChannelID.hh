// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------

#ifndef NewCHODChannelID_H
#define NewCHODChannelID_H

#include "Rtypes.h"

class NewCHODChannelID {

public:
  struct chIDDecoded { Int_t fTileID, fSeqTileID, fQuadrantID; };

  NewCHODChannelID();
  virtual ~NewCHODChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); // returns position ID
  static struct chIDDecoded DecodeChannelID_Static(Int_t);
  void  DecodeChannelID(Int_t);

  Int_t  GetTileID()     { return fTileID;     }
  Int_t  GetSeqTileID()  { return fSeqTileID;  }
  Int_t  GetQuadrantID() { return fQuadrantID; }

private:

  Int_t  fTileID;     ///< ID of the physical tile (101-138, 201-238, 301-338, 401-438)
  Int_t  fSeqTileID;  ///< Sequential ID of the tile (1-151)
  Int_t  fQuadrantID; ///< ID of the quadrant

  ClassDef(NewCHODChannelID,1);
};
#endif
