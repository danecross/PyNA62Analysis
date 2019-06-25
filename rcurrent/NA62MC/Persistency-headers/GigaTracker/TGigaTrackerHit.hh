// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TGigaTrackerHit_H
#define TGigaTrackerHit_H

#include "TDetectorVHit.hh"
#include "GigaTrackerChannelID.hh"

class TGigaTrackerHit : public TDetectorVHit, public GigaTrackerChannelID  {
//class TGigaTrackerHit : public TDCVHit, public GigaTrackerChannelID  {

    public:

        TGigaTrackerHit();
        ~TGigaTrackerHit(){};

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void  DecodeChannelID();
        Int_t GetStationID() { return GetStationNo(); }

    public:

    protected:

        ClassDef(TGigaTrackerHit,1);
};
#endif
