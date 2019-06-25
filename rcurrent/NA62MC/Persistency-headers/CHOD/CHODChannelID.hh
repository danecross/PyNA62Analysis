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

  CHODChannelID();
  virtual ~CHODChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID(); // returns position ID
  void  DecodeChannelID(Int_t);

  Int_t GetPlaneID()    { return fPlaneID;    }
  Int_t GetQuadrantID() { return fQuadrantID; }
  Int_t GetCounterID()  { return fCounterID;  }

private:

  Int_t fPlaneID;
  Int_t fQuadrantID;
  Int_t fCounterID;

  ClassDef(CHODChannelID,1);
};
#endif
