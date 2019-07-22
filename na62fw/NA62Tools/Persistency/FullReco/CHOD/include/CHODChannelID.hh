// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------

#ifndef CHODChannelID_H
#define CHODChannelID_H

#include "Rtypes.h"
#include "TVChannelID.hh"

class CHODChannelID {

public:
  struct chIDDecoded{ Int_t PlaneID,QuadrantID,CounterID;};

  CHODChannelID();
  virtual ~CHODChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); // returns position ID
  static chIDDecoded DecodeChannelID_Static(int ChannelID); // converts position ID into plane, quadrant, counter IDs (Struct)
  void DecodeChannelID(int ChannelID);

  Int_t GetPlaneID()             { return fPlaneID;    }
  void  SetPlaneID(Int_t val)    { fPlaneID = val;     }
  Int_t GetQuadrantID()          { return fQuadrantID; }
  void  SetQuadrantID(Int_t val) { fQuadrantID = val;  }
  Int_t GetCounterID()           { return fCounterID;  }
  void  SetCounterID(Int_t val)  { fCounterID = val;   }

private:

  Int_t fPlaneID;
  Int_t fQuadrantID;
  Int_t fCounterID;

  ClassDef(CHODChannelID,1);
};
#endif
