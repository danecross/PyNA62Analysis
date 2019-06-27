// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------

#ifndef TVChannelID_H
#define TVChannelID_H

#include "TObject.h"
#include "NA62Global.hh"

class TVChannelID {

public:

  TVChannelID() : fChannelID(-1) {}
  TVChannelID(const TVChannelID & ch) : fChannelID(ch.fChannelID){}
  explicit TVChannelID(Int_t iCh) : fChannelID(iCh) {}
  virtual ~TVChannelID() {}
  void Clear(Option_t* = "");

  void Print(Option_t* option="") const;
  Int_t GetChannelID() const      { return fChannelID;         }
  Int_t SetChannelID(Int_t value) { return fChannelID = value; }

protected:

  Int_t fChannelID;
  ClassDef(TVChannelID,1);
};
#endif
