// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#include "L2EBSpecialTrigger.hh"

#include "Riostream.h"

ClassImp(L2EBSpecialTrigger)

L2EBSpecialTrigger::L2EBSpecialTrigger(): TObject(){
  Clear();
}                        

L2EBSpecialTrigger::~L2EBSpecialTrigger(){}

void L2EBSpecialTrigger::Clear(Option_t* /*option*/){
  fL2PCsInfo.clear();
}

Bool_t L2EBSpecialTrigger::AddPCInfo(UInt_t * pDataBuffer){

  L2PCSpecialBlock L2PC;
  L2PC.SetL2PCID((*(pDataBuffer+O_L2SPTRGL2PCID)&M_L2SPTRGL2PCID)>>S_L2SPTRGL2PCID);
  L2PC.SetBlockLength((*(pDataBuffer+O_L2SPTRGBLOCKLENGTH)&M_L2SPTRGBLOCKLENGTH)>>S_L2SPTRGBLOCKLENGTH);
  L2PC.SetL2PCID((*(pDataBuffer+O_L2SPTRGL2PCID)&M_L2SPTRGL2PCID)>>S_L2SPTRGL2PCID);
  L2PC.SetTimeStamp((*(pDataBuffer+O_L2SPTRGTIMESTAMP)&M_L2SPTRGTIMESTAMP)>>S_L2SPTRGTIMESTAMP);
  L2PC.SetDataFormat((*(pDataBuffer+O_L2SPTRGDATAFORMAT)&M_L2SPTRGDATAFORMAT)>>S_L2SPTRGDATAFORMAT);
  L2PC.SetTimeoutFlag((*(pDataBuffer+O_L2SPTRGTIMEOUTFLAG)&M_L2SPTRGTIMEOUTFLAG)>>S_L2SPTRGTIMEOUTFLAG);
  L2PC.SetNL2InputEvents(*(pDataBuffer+O_L2SPTRGNL2INPUTEVTS));
  L2PC.SetNL2SpecialEvents(*(pDataBuffer+O_L2SPTRGNL2SPECIALEVTS));
  L2PC.SetNL2ControlEvents(*(pDataBuffer+O_L2SPTRGNL2CONTROLEVTS));
  L2PC.SetNL2PeriodicEvents(*(pDataBuffer+O_L2SPTRGNL2PERIODICEVTS));
  L2PC.SetNL2PhysicsEvents(*(pDataBuffer+O_L2SPTRGNL2PHYSICSEVTS));
  L2PC.SetNL2PhysicsEventsInMultipleMasks(*(pDataBuffer+O_L2SPTRGNL2PHYSICSEVTSMM));
  L2PC.SetNL2OutputEvents(*(pDataBuffer+O_L2SPTRGNL2OUTPUTEVTS));
  L2PC.SetNL2AcceptedEvents(*(pDataBuffer+O_L2SPTRGNL2ACCEPTEDEVTS));
  L2PC.SetNL2TimeoutEvents(*(pDataBuffer+O_L2SPTRGNL2TIMEOUTEVTS));
  L2PC.SetNL2AllDisabledEvents(*(pDataBuffer+O_L2SPTRGNL2ALLDISABLEDEVTS));
  L2PC.SetNL2BypassedEvents(*(pDataBuffer+O_L2SPTRGNL2BYPASSEDEVTS));
  L2PC.SetNL2FlagAlgoEvents(*(pDataBuffer+O_L2SPTRGNL2FLAGALGOEVTS));
  L2PC.SetNL2AutopassEvents(*(pDataBuffer+O_L2SPTRGNL2AUTOPASSEVTS));
  std::vector<L2MaskSpecialBlock> L2Masks;
  L2Masks.clear();
  const UInt_t NWordsInMask = 3;
  for(UInt_t iMask=0;iMask<16;iMask++){
    L2MaskSpecialBlock L2Mask;
    L2Mask.SetL0MaskID(iMask);
    L2Mask.SetNL2InputEvents(*(pDataBuffer+O_L2SPTRGL2MASKSINFO+iMask*NWordsInMask+O_L2SPMASKNINPUTEVTS));
    L2Mask.SetNL2OutputEvents(*(pDataBuffer+O_L2SPTRGL2MASKSINFO+iMask*NWordsInMask+O_L2SPMASKNOUTPUTNEVTS));
    L2Masks.push_back(L2Mask);
  }
  L2PC.SetL2MasksInfo(L2Masks);
  fL2PCsInfo.push_back(L2PC);

  return kTRUE;
}

ClassImp(L2PCSpecialBlock)

L2PCSpecialBlock::L2PCSpecialBlock(): TObject(){
  Clear();
}

L2PCSpecialBlock::L2PCSpecialBlock(const L2PCSpecialBlock& c) : TObject(c), fBlockLength(c.fBlockLength), fL2PCID(c.fL2PCID), fDetectorID(c.fDetectorID), fTimeStamp(c.fTimeStamp), fDataFormat(c.fDataFormat), fTimeoutFlag(c.fTimeoutFlag), fReserved(c.fReserved), fNL2InputEvents(c.fNL2InputEvents), fNL2SpecialEvents(c.fNL2SpecialEvents), fNL2ControlEvents(c.fNL2ControlEvents), fNL2PeriodicEvents(c.fNL2PeriodicEvents), fNL2PhysicsEvents(c.fNL2PhysicsEvents), fNL2PhysicsEventsInMultipleMasks(c.fNL2PhysicsEventsInMultipleMasks), fNL2OutputEvents(c.fNL2OutputEvents), fNL2AcceptedEvents(c.fNL2AcceptedEvents), fNL2TimeoutEvents(c.fNL2TimeoutEvents), fNL2AllDisabledEvents(c.fNL2AllDisabledEvents), fNL2BypassedEvents(c.fNL2BypassedEvents), fNL2FlagAlgoEvents(c.fNL2FlagAlgoEvents), fNL2AutopassEvents(c.fNL2AutopassEvents), fL2MasksInfo(c.fL2MasksInfo){}

void L2PCSpecialBlock::Clear(Option_t* /*option*/){
  fBlockLength = 0;
  fL2PCID = 0;
  fDetectorID = 0;
  fTimeStamp = 0;
  fDataFormat = 0;
  fTimeoutFlag = 0;
  fReserved = 0;
  fNL2InputEvents = 0;
  fNL2SpecialEvents = 0;
  fNL2ControlEvents = 0;
  fNL2PeriodicEvents = 0;
  fNL2PhysicsEvents = 0;
  fNL2PhysicsEventsInMultipleMasks = 0;
  fNL2OutputEvents = 0;
  fNL2AcceptedEvents = 0;
  fNL2TimeoutEvents = 0;
  fNL2AllDisabledEvents = 0;
  fNL2BypassedEvents = 0;
  fNL2FlagAlgoEvents = 0;
  fNL2AutopassEvents = 0;
  fL2MasksInfo.clear();
}

ClassImp(L2MaskSpecialBlock)

L2MaskSpecialBlock::L2MaskSpecialBlock(): TObject(){
  Clear();
}

L2MaskSpecialBlock::L2MaskSpecialBlock(const L2MaskSpecialBlock& c) : TObject(c), fL0MaskID(c.fL0MaskID), fNL2InputEvents(c.fNL2InputEvents), fNL2OutputEvents(c.fNL2OutputEvents), fReserved(c.fReserved){}

void L2MaskSpecialBlock::Clear(Option_t* /*option*/){
  fL0MaskID = 0;
  fNL2InputEvents = 0;
  fNL2OutputEvents = 0;
  fReserved = 0;
}
