// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------
#ifndef TCHODDigi_H
#define TCHODDigi_H

#include "TDCVHit.hh"
#include "CHODChannelID.hh"

class TCHODDigi : public TDCVHit, public CHODChannelID {

public:

  TCHODDigi();
  ~TCHODDigi() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return 0; }

  Int_t GetThresholdType() const { return fChannelID/1000; } //Low Threshold: 0, High Threshold: 1

private:

  ClassDef(TCHODDigi,1);
};
#endif
