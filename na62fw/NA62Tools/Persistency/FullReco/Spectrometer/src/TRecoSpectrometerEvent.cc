// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoSpectrometerEvent.hh"

/// \class TRecoSpectrometerEvent
/// \Brief
/// Global info on the Spectrometer
/// \EndBrief
/// \Detailed
/// This class stores the variables of the reconstructed tracks, etc.
/// \EndDetailed

ClassImp(TRecoSpectrometerEvent)

TRecoSpectrometerEvent::TRecoSpectrometerEvent() : TRecoVEvent(TRecoSpectrometerCandidate::Class(), TRecoSpectrometerHit::Class()){
  Clear();
}

TRecoSpectrometerEvent::~TRecoSpectrometerEvent(){
}

void TRecoSpectrometerEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
