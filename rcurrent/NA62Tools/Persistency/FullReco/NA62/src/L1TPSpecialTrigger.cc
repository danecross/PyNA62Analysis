// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#include "L1TPSpecialTrigger.hh"

#include "Riostream.h"

ClassImp(L1TPSpecialTrigger)

L1TPSpecialTrigger::L1TPSpecialTrigger(): TObject(){
  Clear();
}                        

L1TPSpecialTrigger::~L1TPSpecialTrigger(){}

void L1TPSpecialTrigger::Clear(Option_t* /*option*/){
  fL1PCsInfo.clear();
}

Bool_t L1TPSpecialTrigger::AddPCInfo(UInt_t * pDataBuffer){

  L1PCSpecialBlock L1PC;
  L1PC.SetL1PCID((*(pDataBuffer+O_L1SPTRGL1PCID)&M_L1SPTRGL1PCID)>>S_L1SPTRGL1PCID);
  L1PC.SetBlockLength((*(pDataBuffer+O_L1SPTRGBLOCKLENGTH)&M_L1SPTRGBLOCKLENGTH)>>S_L1SPTRGBLOCKLENGTH);
  L1PC.SetL1PCID((*(pDataBuffer+O_L1SPTRGL1PCID)&M_L1SPTRGL1PCID)>>S_L1SPTRGL1PCID);
  L1PC.SetTimeStamp((*(pDataBuffer+O_L1SPTRGTIMESTAMP)&M_L1SPTRGTIMESTAMP)>>S_L1SPTRGTIMESTAMP);
  L1PC.SetDataFormat((*(pDataBuffer+O_L1SPTRGDATAFORMAT)&M_L1SPTRGDATAFORMAT)>>S_L1SPTRGDATAFORMAT);
  L1PC.SetTimeoutFlag((*(pDataBuffer+O_L1SPTRGTIMEOUTFLAG)&M_L1SPTRGTIMEOUTFLAG)>>S_L1SPTRGTIMEOUTFLAG);
  L1PC.SetNL1InputEvents(*(pDataBuffer+O_L1SPTRGNL1INPUTEVTS));
  L1PC.SetNL1SpecialEvents(*(pDataBuffer+O_L1SPTRGNL1SPECIALEVTS));
  L1PC.SetNL1ControlEvents(*(pDataBuffer+O_L1SPTRGNL1CONTROLEVTS));
  L1PC.SetNL1PeriodicEvents(*(pDataBuffer+O_L1SPTRGNL1PERIODICEVTS));
  L1PC.SetNL1PhysicsEvents(*(pDataBuffer+O_L1SPTRGNL1PHYSICSEVTS));
  L1PC.SetNL1PhysicsEventsInMultipleMasks(*(pDataBuffer+O_L1SPTRGNL1PHYSICSEVTSMM));
  L1PC.SetNL1DataRequests(*(pDataBuffer+O_L1SPTRGNL1DATAREQUESTS));
  L1PC.SetNL1OutputEvents(*(pDataBuffer+O_L1SPTRGNL1OUTPUTEVTS));
  L1PC.SetNL1AcceptedEvents(*(pDataBuffer+O_L1SPTRGNL1ACCEPTEDEVTS));
  L1PC.SetNL1TimeoutEvents(*(pDataBuffer+O_L1SPTRGNL1TIMEOUTEVTS));
  L1PC.SetNL1AllDisabledEvents(*(pDataBuffer+O_L1SPTRGNL1ALLDISABLEDEVTS));
  L1PC.SetNL1BypassedEvents(*(pDataBuffer+O_L1SPTRGNL1BYPASSEDEVTS));
  L1PC.SetNL1FlagAlgoEvents(*(pDataBuffer+O_L1SPTRGNL1FLAGALGOEVTS));
  L1PC.SetNL1AutopassEvents(*(pDataBuffer+O_L1SPTRGNL1AUTOPASSEVTS));
  std::vector<L1MaskSpecialBlock> L1Masks;
  L1Masks.clear();
  const UInt_t NWordsInMask = 3;
  for(UInt_t iMask=0;iMask<16;iMask++){
    L1MaskSpecialBlock L1Mask;
    L1Mask.SetL0MaskID(iMask);
    L1Mask.SetNL1InputEvents(*(pDataBuffer+O_L1SPTRGL1MASKSINFO+iMask*NWordsInMask+O_L1SPMASKNINPUTEVTS));
    L1Mask.SetNL1OutputEvents(*(pDataBuffer+O_L1SPTRGL1MASKSINFO+iMask*NWordsInMask+O_L1SPMASKNOUTPUTNEVTS));
    L1Masks.push_back(L1Mask);
  }
  L1PC.SetL1MasksInfo(L1Masks);
  fL1PCsInfo.push_back(L1PC);

  return kTRUE;
}

ClassImp(L1PCSpecialBlock)

L1PCSpecialBlock::L1PCSpecialBlock(): TObject(){
  Clear();
}

L1PCSpecialBlock::L1PCSpecialBlock(const L1PCSpecialBlock& c) : TObject(c), fBlockLength(c.fBlockLength), fL1PCID(c.fL1PCID), fDetectorID(c.fDetectorID), fTimeStamp(c.fTimeStamp), fDataFormat(c.fDataFormat), fTimeoutFlag(c.fTimeoutFlag), fReserved(c.fReserved), fNL1InputEvents(c.fNL1InputEvents), fNL1SpecialEvents(c.fNL1SpecialEvents), fNL1ControlEvents(c.fNL1ControlEvents), fNL1PeriodicEvents(c.fNL1PeriodicEvents), fNL1PhysicsEvents(c.fNL1PhysicsEvents), fNL1PhysicsEventsInMultipleMasks(c.fNL1PhysicsEventsInMultipleMasks), fNL1DataRequests(c.fNL1DataRequests), fNL1OutputEvents(c.fNL1OutputEvents), fNL1AcceptedEvents(c.fNL1AcceptedEvents), fNL1TimeoutEvents(c.fNL1TimeoutEvents), fNL1AllDisabledEvents(c.fNL1AllDisabledEvents), fNL1BypassedEvents(c.fNL1BypassedEvents), fNL1FlagAlgoEvents(c.fNL1FlagAlgoEvents), fNL1AutopassEvents(c.fNL1AutopassEvents), fL1MasksInfo(c.fL1MasksInfo){}

void L1PCSpecialBlock::Clear(Option_t* /*option*/){
  fL1PCID = 0;
  fL1MasksInfo.clear();
}

ClassImp(L1MaskSpecialBlock)

L1MaskSpecialBlock::L1MaskSpecialBlock(): TObject(){
  Clear();
}

L1MaskSpecialBlock::L1MaskSpecialBlock(const L1MaskSpecialBlock& c) : TObject(c), fL0MaskID(c.fL0MaskID), fNL1InputEvents(c.fNL1InputEvents), fNL1OutputEvents(c.fNL1OutputEvents), fReserved(c.fReserved){}

void L1MaskSpecialBlock::Clear(Option_t* /*option*/){
  fL0MaskID = 0;
  fNL1InputEvents = 0;
  fNL1OutputEvents = 0;
  fReserved = 0;
}
