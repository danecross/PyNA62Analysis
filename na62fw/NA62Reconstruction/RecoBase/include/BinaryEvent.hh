// --------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-11
// Modified by Maria Brigida Brunetti (maria.brigida.brunetti@cern.ch) 2016-01-21
// 
// --------------------------------------------------------------
#ifndef BinaryEvent_H
#define BinaryEvent_H 1

#include "NA62VRawEncoder.hh"
#include <inttypes.h>
#include "TFile.h"

class BinaryEvent {

  public:

    BinaryEvent(uint32_t, uint32_t);
    ~BinaryEvent();
    void Clear();

  public:

    uint32_t *    GetBuffer();
    uint32_t **   GetOutputBuffer();
    uint32_t      GetMaxNWords()                           {return fMaxNWords;};
    uint32_t      GetMaxNProducers()                       {return fMaxNProducers;};
    void          SetMaxNWords(uint32_t MaxNWords)         {fMaxNWords = MaxNWords;};
    void          SetMaxNProducers(uint32_t MaxNProducers) {fMaxNProducers = MaxNProducers;};  

  private:

    uint32_t fMaxNWords;
    uint32_t fMaxNProducers;
    uint32_t * fBuffer;
    uint32_t ** fOutputBuffer;
};
#endif
