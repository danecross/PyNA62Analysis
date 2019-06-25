// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#include "TTDCBSpecialTrigger.hh"
ClassImp(TTDCBSpecialTrigger)

TTDCBSpecialTrigger::TTDCBSpecialTrigger():TPrimSpecialTrigger(){
  fFPGAID=0;
}

void TTDCBSpecialTrigger::Clear(Option_t* option){
  TPrimSpecialTrigger::Clear(option);
  fFPGAID=0;
}
