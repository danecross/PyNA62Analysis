#include "EventHeader.hh"

#include "Riostream.h"

ClassImp(EventHeader)

EventHeader::EventHeader() {
  fEventNumber = 0;
  fEventLength = 0;
  fBurstID = 0;
  fRunID = 0;
  fTimeStamp = 0;
  fPreviousTimeStamp = 0;
  fTriggerType = 0;
  fNumOfDetectors = 0;
  fFineTime = 0;
  fBurstTime = 0;
  fDetectorID = 0; //!  Transient data member
  fStartByte = 0; //!  Transient data member
  fEOBFileDescriptor = 0; //!  Transient data member
  fL0TPData = new L0TPData();
  fL1TPData = new L1TPData();
  fL2EBData = new L2EBData();
  fBeamData = new BeamData();
  fHLTEvent = new HLTEvent();
  fL0TPSpecialTrigger = new L0TPSpecialTrigger();
  fL1TPSpecialTrigger = new L1TPSpecialTrigger();
  fL2EBSpecialTrigger = new L2EBSpecialTrigger();
  fBeamSpecialTrigger = new BeamSpecialTrigger();
  fEventQualityMask = 0;
}                        

EventHeader::~EventHeader(){
  if(fL0TPData) {
    delete fL0TPData;
    fL0TPData=0;
  }
  if(fL1TPData) {
    delete fL1TPData;
    fL1TPData=0;
  }
  if(fL2EBData) {
    delete fL2EBData;
    fL2EBData=0;
  }
  if(fBeamData) {
    delete fBeamData;
    fBeamData=0;
  }
  if(fHLTEvent) {
    delete fHLTEvent;
    fHLTEvent=0;
  }
  if(fL0TPSpecialTrigger) {
    delete fL0TPSpecialTrigger;
    fL0TPSpecialTrigger=0;
  }
  if(fL1TPSpecialTrigger) {
    delete fL1TPSpecialTrigger;
    fL1TPSpecialTrigger=0;
  }
  if(fL2EBSpecialTrigger) {
    delete fL2EBSpecialTrigger;
    fL2EBSpecialTrigger=0;
  }
  if(fBeamSpecialTrigger) {
    delete fBeamSpecialTrigger;
    fBeamSpecialTrigger=0;
  }
}

Bool_t EventHeader::SetHeader(UInt_t * pDataBuffer){
  UInt_t marker = (*pDataBuffer&0xff000000)>>24;
  if (marker != 0x00000062) return kFALSE;
  if(fTimeStamp != *(pDataBuffer+O_TIMESTAMP)) fPreviousTimeStamp   = fTimeStamp; //avoid overwriting if SetHeader is called twice
  fEventNumber         = (*(pDataBuffer+O_EVENTNUMBER)&M_EVENTNUMBER);
  fEventLength         = *(pDataBuffer+O_EVENTLENGTH);
  fBurstID             = *(pDataBuffer+O_BURSTID);
  fTimeStamp           = *(pDataBuffer+O_TIMESTAMP);
  fTriggerType         = *(pDataBuffer+O_TRIGGERTYPE)&M_TRIGGERTYPE;
  fNumOfDetectors      = (*(pDataBuffer+O_NSUBDET)&M_NSUBDET)>>S_NSUBDET;
  fFineTime            = (*(pDataBuffer+O_FINETIME)&M_FINETIME);
  fBurstTime           = *(pDataBuffer+O_BURSTTIME);
  fEventQualityMask    = 0;
  return kTRUE;
}

Bool_t EventHeader::SetL0TPData(UInt_t * pDataBuffer){
  return fL0TPData->SetHeader(pDataBuffer);
}

Bool_t EventHeader::SetL1TPData(UInt_t * pDataBuffer){
  return fL1TPData->SetHeader(pDataBuffer);
}

Bool_t EventHeader::SetL2EBData(UInt_t * pDataBuffer){
  return fL2EBData->SetHeader(pDataBuffer);
}

void EventHeader::SetBeamInstantaneousIntensity(Double_t Intensity, Double_t IntensityError){
  fBeamData->Clear(); //std::vector used for possible future additions
  fBeamData->AddInstantaneousIntensity(Intensity);
  fBeamData->AddInstantaneousIntensityError(IntensityError);
}

Bool_t EventHeader::SetL0TPSpecialTrigger(UInt_t * pSpecialTriggerBuffer){
  return fL0TPSpecialTrigger->SetHeader(pSpecialTriggerBuffer);
}

Bool_t EventHeader::SetL1TPSpecialTrigger(UInt_t * pSpecialTriggerBuffer){
  return fL1TPSpecialTrigger->AddPCInfo(pSpecialTriggerBuffer);
}

Bool_t EventHeader::SetL2EBSpecialTrigger(UInt_t * pSpecialTriggerBuffer){
  return fL2EBSpecialTrigger->AddPCInfo(pSpecialTriggerBuffer);
}

Bool_t EventHeader::SetBeamSpecialTrigger(UInt_t * pSpecialTriggerBuffer,UInt_t NumberOfWords){
  if((fTriggerType&0xff)!=0x23) { // beam info not available
    fBeamSpecialTrigger->Clear();
    return kFALSE;
  }
  return fBeamSpecialTrigger->SetHeader(pSpecialTriggerBuffer,NumberOfWords);
}

void EventHeader::UpdateEventQualityMask(UInt_t DetectorID){
  if(DetectorID<32 && !(fEventQualityMask&(1U<<DetectorID))) fEventQualityMask+=(1U<<DetectorID);
}

void EventHeader::ClearDIMBlock(){
  fBeamSpecialTrigger->Clear();
  fL1TPSpecialTrigger->Clear();
  fL2EBSpecialTrigger->Clear();
}
