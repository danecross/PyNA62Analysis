// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#include "TSpecialTrigger.hh"
ClassImp(TSpecialTrigger)

TSpecialTrigger::TSpecialTrigger(){
  fDataSourceID=0;
  fROBoardID=0;
  fTimeStamp=1e9;
  fTriggerType=0;
  fTriggerCount=0;
  fErrorsFlag=0;
}

void TSpecialTrigger::Clear(Option_t * /*option*/) {
  fDataSourceID=0;
  fROBoardID=0;
  fTimeStamp=1e9;
  fTriggerType=0;
  fTriggerCount=0;
  fErrorsFlag=0;
}
