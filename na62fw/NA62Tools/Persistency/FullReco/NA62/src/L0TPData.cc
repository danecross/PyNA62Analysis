// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2014-11-20
//
// ---------------------------------------------------------------

#include "L0TPData.hh"
#include "Riostream.h"

ClassImp(L0TPData)

L0TPData::L0TPData(){
  Clear();
}                        

L0TPData::~L0TPData(){}

Bool_t L0TPData::PrimitiveExists(UInt_t iL0Slot,UInt_t iL0Detector) {
  if (iL0Slot*L0NMAXDETECTORS+iL0Detector>=fPrimitives.size()) return false;
  return true;
}

L0Primitive L0TPData::GetPrimitive(UInt_t iL0Slot,UInt_t iL0Detector){
  L0Primitive EmptyPrim;
  if(iL0Slot*L0NMAXDETECTORS+iL0Detector>=fPrimitives.size()) return EmptyPrim; // Out of range
  return fPrimitives[iL0Slot*L0NMAXDETECTORS+iL0Detector];
}

UChar_t  L0TPData::GetNDeltaPrimitives(UInt_t iL0Detector){
  if(iL0Detector>=fNDeltaPrimitives.size()) return 0; // Out of range
  return fNDeltaPrimitives[iL0Detector];
}

void L0TPData::Clear(Option_t* /*option*/){
  // Subdetector Block Header (generic part)
  fEventLength = 0;
  fDetectorID = 0;
  fDataBlockFormat = 0;
  fTimeStamp = 0;
  fNBlockHeaderWords = 0;

  // Subdetector Block Header (L0TP-specific part)
  fReferenceFineTime = 0;
  fDataType = 0;
  fPreviousTimeStamp = 0;
  fTriggerType = 0;
  fPreviousTriggerType = 0;
  fTriggerFlags = 0;
  fReserved = 0;
  fNDeltaPrimitives.clear();
  fNDeltaPrimitivesMapping.clear();
  for(UInt_t iDetector=0;iDetector<L0NMAXDETECTORS;iDetector++) {
    fNDeltaPrimitives.push_back(0);
    fNDeltaPrimitivesMapping.push_back(-1);
  }
  fNDeltaPrimitivesMapping[kL0CHOD]    = 0;
  fNDeltaPrimitivesMapping[kL0RICH]    = 1;
  fNDeltaPrimitivesMapping[kL0MUV3]    = 2;
  fNDeltaPrimitivesMapping[kL0NewCHOD] = 3;
  fPrimitives.clear();
}

Bool_t L0TPData::SetHeader(UInt_t * pDataBuffer){

  Clear(); //Reset L0 info

  // Subdetector Block Header (generic part)
  fEventLength         = (*(pDataBuffer+O_L0EVENTLENGTH)&M_L0EVENTLENGTH)>>S_L0EVENTLENGTH;
  fDetectorID          = (*(pDataBuffer+O_L0DETECTORID)&M_L0DETECTORID)>>S_L0DETECTORID;
  fDataBlockFormat     = (*(pDataBuffer+O_L0DATABLOCKFORMAT)&M_L0DATABLOCKFORMAT)>>S_L0DATABLOCKFORMAT;
  fNBlockHeaderWords = 1;
  if(fDataBlockFormat==1) { // 2015 format: read the 32-bit timestamp
    fTimeStamp         = (*(pDataBuffer+O_L0TIMESTAMP)&M_L0TIMESTAMP)>>S_L0TIMESTAMP;
    fNBlockHeaderWords++;
  }

  // Subdetector Block Header (L0TP-specific part)
  fReferenceFineTime   = (*(pDataBuffer+fNBlockHeaderWords+O_L0REFFINETIME)&M_L0REFFINETIME)>>S_L0REFFINETIME;
  fDataType            = (*(pDataBuffer+fNBlockHeaderWords+O_L0DATATYPE)&M_L0DATATYPE)>>S_L0DATATYPE;
  fPreviousTimeStamp   = (*(pDataBuffer+fNBlockHeaderWords+O_L0PREVTIMESTAMP)&M_L0PREVTIMESTAMP)>>S_L0PREVTIMESTAMP;
  fTriggerType         = (*(pDataBuffer+fNBlockHeaderWords+O_L0TRIGTYPE)&M_L0TRIGTYPE)>>S_L0TRIGTYPE;
  fPreviousTriggerType = (*(pDataBuffer+fNBlockHeaderWords+O_L0PREVTRIGTYPE)&M_L0PREVTRIGTYPE)>>S_L0PREVTRIGTYPE;
  fTriggerFlags        = (*(pDataBuffer+fNBlockHeaderWords+O_L0TRIGFLAGS)&M_L0TRIGFLAGS)>>S_L0TRIGFLAGS;
  for(UInt_t iDetector=0;iDetector<L0NMAXDETECTORS;iDetector++){
    if(fNDeltaPrimitivesMapping[iDetector]<0) continue;
    UInt_t DeltaPrimOffset = O_L0NDELTAPRIM+fNDeltaPrimitivesMapping[iDetector]/4;
    UInt_t DeltaPrimShift  = S_L0NDELTAPRIM+fNDeltaPrimitivesMapping[iDetector]*8;
    UInt_t DeltaPrimMask   = (M_L0NDELTAPRIM<<DeltaPrimShift);
    fNDeltaPrimitives[iDetector] = (*(pDataBuffer+fNBlockHeaderWords+DeltaPrimOffset)&DeltaPrimMask)>>DeltaPrimShift;
  }

  // Fill fPrimitives
  Int_t NPrimitiveWords = 0;
  UInt_t O_L0FINETIMES = O_L0FINETIMES_TRIGGERSLOT; //First slot to be decoded (islot=0) is the trigger slot
  for(UInt_t iL0Slot=0;iL0Slot<3;iL0Slot++){
    if(iL0Slot!=kL0TriggerSlot) O_L0FINETIMES = O_L0FINETIMES_OTHERSLOTS;
    UInt_t PrimIDOffset = NPrimitiveWords+O_L0PRIMITIVES;
    UInt_t FTOffset     = NPrimitiveWords+O_L0FINETIMES;
    if(fNBlockHeaderWords+PrimIDOffset>=fEventLength/4) continue; //continue if less than 3 slots in data [only trigger slot]
    for(UInt_t iPrimitive=0;iPrimitive<L0NMAXDETECTORS;iPrimitive++){
      L0Primitive L0Prim;
      UInt_t PrimitiveID = 0;
      if(iPrimitive%2) PrimitiveID = (*(pDataBuffer+fNBlockHeaderWords+PrimIDOffset+(iPrimitive+1)/2)&M_L0PRIMITIVE_LSB)>>S_L0PRIMITIVE_LSB;
      else             PrimitiveID = (*(pDataBuffer+fNBlockHeaderWords+PrimIDOffset+(iPrimitive+1)/2)&M_L0PRIMITIVE_MSB)>>S_L0PRIMITIVE_MSB;
      L0Prim.SetPrimitiveID(PrimitiveID);
      UInt_t iFTShift  = (iPrimitive*8)%32;
      L0Prim.SetFineTime((*(pDataBuffer+fNBlockHeaderWords+FTOffset+((iPrimitive*8)/32))&(M_L0FINETIME<<iFTShift))>>(S_L0FINETIME+iFTShift));
      fPrimitives.push_back(L0Prim);
    }
    NPrimitiveWords+=6; //each slot has 6 32-bit primitive words
    if(iL0Slot==kL0TriggerSlot) NPrimitiveWords+=4; //4 extra 32-bit words for TriggerSlot
  }

  return kTRUE;
}

void L0TPData::PrintInfo(){
  std::cout << "Printing L0TP info:" << std::endl;
  for(UInt_t iPrim=0;iPrim<fPrimitives.size();iPrim++){
    fPrimitives[iPrim].PrintInfo();
  }
}

Int_t L0TPData::GetPrimitiveCorrectedFineTime(UInt_t iL0Slot, UInt_t iL0Detector, Int_t L0TPGranularity){
  if(iL0Slot*L0NMAXDETECTORS+iL0Detector>=fPrimitives.size()) return -9999; // Out of range
  return fPrimitives[iL0Slot*L0NMAXDETECTORS+iL0Detector].GetCorrectedFineTime(iL0Slot,fReferenceFineTime,L0TPGranularity);
}

void L0TPData::SetPrimitives(std::vector<L0Primitive> &input){
  fPrimitives=input;
}

ClassImp(L0Primitive)

L0Primitive::L0Primitive():TObject(),fPrimitiveID(0),fFineTime(0){
}

L0Primitive::L0Primitive(const L0Primitive &c): TObject(c), fPrimitiveID(c.fPrimitiveID), fFineTime(c.fFineTime){
}

void L0Primitive::Clear(Option_t* /*option*/){
  fPrimitiveID=0;
  fFineTime=0;
}

void L0Primitive::PrintInfo(){
  std::cout << "[L0Primitive]   PrimitiveID: " << (Int_t)fPrimitiveID << std::endl;
  std::cout << "[L0Primitive]   FineTime:    " << (Int_t)fFineTime    << std::endl;
}

Int_t L0Primitive::GetCorrectedFineTime(Int_t iTrigSlot, Int_t ReferenceFineTime, Int_t BitFineTime) {
  //BitFineTime = 0 -> Granularity = 25 ns 
  //BitFineTime = 1 -> Granularity = 12.5 ns 
  //BitFineTime = 2 -> Granularity = 6.25 ns 
  
  Int_t CorrectedFineTime=0;
  Int_t Shift = -9999;

  switch (iTrigSlot) {
  case kL0TriggerSlot:
    return fFineTime;
  case kL0PreviousSlot:
    Shift = 0;
    break;
  case kL0NextSlot:
    Shift = 1;
    break;
  }

  if (BitFineTime==0) {
    if (iTrigSlot==kL0PreviousSlot) {
      CorrectedFineTime = fFineTime - 256;
      return CorrectedFineTime;
    }
    if (iTrigSlot==kL0NextSlot) {
      CorrectedFineTime = fFineTime + 256;
      return CorrectedFineTime;
    }
  }

  Int_t Compare = 0;
  for (Int_t i = 0; i < BitFineTime; i++) { 
    Compare += (128 >> i); 
  }

  if (ReferenceFineTime >= Compare) {
    CorrectedFineTime = Shift*256 + (Int_t)fFineTime;
    return CorrectedFineTime;
  }
  else if (ReferenceFineTime < 256-Compare) {
    CorrectedFineTime = (Shift-1)*256 + (Int_t)fFineTime;
    return CorrectedFineTime;
  }
  else {
    CorrectedFineTime = (Int_t)fFineTime;
    return CorrectedFineTime;
  }
  return -9999;
}
