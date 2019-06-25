// --------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-11
// Modified by Maria Brigida Brunetti (maria.brigida.brunetti@cern.ch) 2016-01-21
//
// --------------------------------------------------------------
#include "BinaryEvent.hh"

BinaryEvent::BinaryEvent(uint32_t MaxNProducers, uint32_t MaxNWords) {
  fMaxNProducers = MaxNProducers;
  fMaxNWords = MaxNWords;
  fBuffer = new uint32_t[fMaxNProducers*fMaxNWords];
  fOutputBuffer = new uint32_t*[fMaxNProducers];
  for(uint32_t iProducer=0; iProducer<fMaxNProducers; ++iProducer){
    fOutputBuffer[iProducer] = new uint32_t[fMaxNWords];
  }
}


BinaryEvent::~BinaryEvent(){
  if(fBuffer){
    delete [] fBuffer;
    fBuffer = 0;
  }
  if(fOutputBuffer) {
    for(uint32_t iProducer=0; iProducer<fMaxNProducers; ++iProducer){
     if(fOutputBuffer[iProducer]) delete [] fOutputBuffer[iProducer];
    }
    delete [] fOutputBuffer;
    fOutputBuffer = 0;
  }
}


void BinaryEvent::Clear(){
  for(uint32_t iProducer=0; iProducer<fMaxNProducers; ++iProducer){
    for(uint32_t jWord=0; jWord<fMaxNWords; ++jWord){
      fBuffer[iProducer*fMaxNWords + jWord] = 0; 
      fOutputBuffer[iProducer][jWord] = 0;
    }
  }
}

uint32_t * BinaryEvent::GetBuffer(){
  return fBuffer;
}


uint32_t ** BinaryEvent::GetOutputBuffer(){
  return fOutputBuffer;
}


