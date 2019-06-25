// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-01-23
//
// --------------------------------------------------------------
#include "EventBoundary.hh"

ClassImp(EventBoundary)

EventBoundary::EventBoundary(){
    fID = 0;
    fTime = 0;

    fFirstGenePartIndex = 0;
    fNGeneParts = 0;
    fFirstKinePartIndex = 0;
    fNKineParts = 0;
}

void EventBoundary::Shift(Int_t GeneOffset, Int_t KineOffset){
   fFirstGenePartIndex += GeneOffset;
   fFirstKinePartIndex += KineOffset;
}

void EventBoundary::AddGenePart(){
    fNGeneParts++;
}

void EventBoundary::AddKinePart(){
    fNKineParts++;
}

void EventBoundary::Clear(Option_t* /*option*/) {
  fID = 0;
  fTime = 0;

  fFirstGenePartIndex = 0;
  fNGeneParts = 0;
  fFirstKinePartIndex = 0;
  fNKineParts = 0;
}
