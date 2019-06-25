// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-02-26
//
// --------------------------------------------------------------
#include "TEventInfo.hh"

#include "Riostream.h"

ClassImp(TEventInfo)

TEventInfo::TEventInfo(){
    fID = -1;
    fStreamID = -1;
    fTime = -1;
    fValidity = kFALSE;

    fFirstHit = -1;
    fNHits = -1;
    fLatestHitTime = -1;
    fNKineParts = 0;
}

TEventInfo::~TEventInfo(){
}


void TEventInfo::Clear(Option_t * /*option*/){
    fID = -1;
    fStreamID = -1;
    fTime = -1;
    fValidity = kFALSE;

    fFirstHit = -1;
    fNHits = -1;
    fLatestHitTime = -1;
    fNKineParts = 0;
}

