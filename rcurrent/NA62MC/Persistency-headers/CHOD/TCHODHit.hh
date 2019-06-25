// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------

#ifndef TCHODHit_H
#define TCHODHit_H

#include "TDetectorVHit.hh"
#include "CHODChannelID.hh"

class TCHODHit : public TDetectorVHit, public CHODChannelID {

public:

  TCHODHit();
  ~TCHODHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return 0; }

protected:
  
  ClassDef(TCHODHit,1);
};
#endif
