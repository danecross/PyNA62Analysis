// ---------------------------------------------------------------
// History:
//
// Created by LorenzaIacobuzio (lorenza.iacobuzio@cern.ch) June 2016
//
// ---------------------------------------------------------------

#include "L0TPRawEncoder.hh"
#include "TSpecialTrigger.hh"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "L0TPData.hh"
#include "Riostream.h"
#include <stdlib.h>
#include <stdint.h>
#include <string>

//#define DBGTHIS "Encoder"
//#define DEBUG "Encoder" 
//#define DBGALL "Encoder"
//#include "Debug.h" 

#define MAXNWORDS 1024
#define MAXNTEL62S 1

using namespace std;

// Constructor

L0TPRawEncoder::L0TPRawEncoder(NA62VReconstruction * Reco) : NA62VRawEncoder(Reco, "L0TP") {

  fBinaryEvent = new BinaryEvent(MAXNTEL62S,MAXNWORDS);
  fBuffer = (uint32_t*)fBinaryEvent->GetBuffer();
  fL0TP = NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData();
}

// Destructor

L0TPRawEncoder::~L0TPRawEncoder(){
}

// Init

void L0TPRawEncoder::Init() {

  fBinaryFileName = "L0TP.dat";
  fHeaderFileName = "L0TP_header.txt";

  cout << "RawEncoder: Setting header filename " << fHeaderFileName << endl;
  cout << "RawEncoder: Setting binary filename " << fBinaryFileName << endl;

  Open();
  fprintf(fHeaderFile,fBinaryFileName.Append("\n"));

  fDetID = 0x40;
  cout << "Detector ID " << std::hex << fDetID << dec << endl; 

  fprintf(fHeaderFile,"%#x:",fDetID);
  fprintf(fHeaderFile, "1:");
}

// Encoding
// fBuffer: global buffer needed to assemble global NA62 Data Format (merger output)

BinaryEvent * L0TPRawEncoder::EncodeNextEvent(TDetectorVEvent * /*tEvent*/, Bool_t fIsRawData) {

  fBinaryEvent->Clear();
  
  Int_t EventNumber = NA62RecoManager::GetInstance()->GetEventHeader()->GetEventNumber();

  if(!static_cast<NA62Reconstruction*>(fReco)->GetNProcessedEventsInTotal()){
    fprintf(fHeaderFile,"%d\n",(static_cast<NA62Reconstruction*>(fReco)->GetNEvt()));
  }

  if(!fIsRawData){
    if(!EventNumber) fTimeStamp = 0x3E8;
    else fTimeStamp = 0x3E8 + EventNumber*0xC8; //This is 200KHz Hardcoded Timestamp - starting from 1000 
    fFineTime = 0;
    //cout << "Event TimeStamp = " << std::hex << fTimeStamp << dec << endl;
  }
  else {
    fTimeStamp = NA62RecoManager::GetInstance()->GetEventHeader()->GetTimeStamp();
    fFineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime();
    //cout << "Event TimeStamp = " << std::hex << fTimeStamp << dec << endl;
    //cout << "Event FineTime = " << std::hex << fFineTime << dec << endl;
  }

  if (fFineTime == 0) return fBinaryEvent;

  UInt_t NEventInMep = 1;
  UInt_t MEPlength = 0;
  UInt_t EventFlag = 0;
  UInt_t Eventlength = 0;
  UInt_t PcfarmEventlength = 0;
  UInt_t jBoard = 0;
  UInt_t nWords = 0;
  UInt_t Pcfarm_nWords = 0;
  UInt_t EvtOffset = 0;
  UInt_t EvtPcFarmOffset = 0;
  UInt_t BoardOffset = 4;

  for(Int_t iWord=0; iWord<MAXNWORDS; ++iWord)
    fBuffer[iWord] = 0;

  // MEP Header //
  fBuffer[EvtOffset+0] = ((uint32_t)fDetID << 24) + 
                          (uint32_t)EventNumber;

  MEPlength++;
  nWords++;

  fBuffer[EvtOffset+1] = ((uint32_t)jBoard      << 24) + 
                         ((uint32_t)NEventInMep << 16);

  MEPlength++;
  nWords++;

  // Event Header //
  fBuffer[EvtOffset+2] = ((uint32_t)EventFlag            << 24) + 
                         ((uint32_t)(EventNumber & 0xFF) << 16);

  MEPlength++;
  Eventlength++;
  nWords++; 

  fBuffer[EvtOffset+3] = (uint32_t)fTimeStamp; 

  MEPlength++;
  Eventlength++;
  nWords++; 

  UChar_t DataType = fL0TP->GetDataType();
  UChar_t RefFineTime = fL0TP->GetReferenceFineTime();
  ULong_t PreviousTS = fL0TP->GetPreviousTimeStamp();
  UInt_t TriggerFlags = fL0TP->GetTriggerFlags();
  UChar_t TriggerType = fL0TP->GetTriggerType();
  UChar_t PreviousTriggerType = fL0TP->GetPreviousTriggerType();

  EvtOffset = nWords;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetPrimitiveID() << 16) + 
                       ((uint32_t)DataType << 8) + RefFineTime;

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot,  kL0LAV).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0RICH).GetPrimitiveID();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;
  
  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0NewCHOD).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot,    kL0MUV3).GetPrimitiveID();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0Calo).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0TALK).GetPrimitiveID();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = (uint32_t)PreviousTS;

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;
  
  fBuffer[EvtOffset] = ((uint32_t)TriggerFlags        << 16) + 
                       ((uint32_t)PreviousTriggerType << 8) +
                        (uint32_t)TriggerType;

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = 0x00000000;

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = 0x00000000;

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0MUV3).GetFineTime() << 24) + 
                       ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot,  kL0LAV).GetFineTime() << 16) + 
                       ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0RICH).GetFineTime() << 8) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetFineTime();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot,   kL0CHOD).GetFineTime() << 24) + 
                       ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot,    kL0Calo).GetFineTime() << 16) + 
                       ((uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot,    kL0TALK).GetFineTime() << 8) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0TriggerSlot, kL0NewCHOD).GetFineTime();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot, kL0NewCHOD).GetFineTime() << 24) + 
                       ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot,    kL0MUV3).GetFineTime() << 16) + 
                       ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot,     kL0LAV).GetFineTime() << 8) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot,    kL0RICH).GetFineTime();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0NextSlot,     kL0RICH).GetFineTime() << 24) +
                       ((uint32_t)fL0TP->GetPrimitive(kL0NextSlot,     kL0CHOD).GetFineTime() << 16) + 
                       ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot, kL0Calo).GetFineTime() << 8) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot, kL0TALK).GetFineTime(); 

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0NextSlot,    kL0TALK).GetFineTime() << 24) +
                       ((uint32_t)fL0TP->GetPrimitive(kL0NextSlot, kL0NewCHOD).GetFineTime() << 16) +
                       ((uint32_t)fL0TP->GetPrimitive(kL0NextSlot,    kL0MUV3).GetFineTime() << 8) +
                        (uint32_t)fL0TP->GetPrimitive(kL0NextSlot,     kL0LAV).GetFineTime();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot, kL0CHOD).GetPrimitiveID() << 16) +
                       ((uint32_t)0x00 << 8) +
                        (uint32_t)fL0TP->GetPrimitive(kL0NextSlot,     kL0Calo).GetFineTime();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot,  kL0LAV).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot, kL0RICH).GetPrimitiveID();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot, kL0NewCHOD).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot,    kL0MUV3).GetPrimitiveID(); 

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot, kL0Calo).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0PreviousSlot, kL0TALK).GetPrimitiveID();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0NextSlot, kL0RICH).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0NextSlot, kL0CHOD).GetPrimitiveID(); 

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0NextSlot, kL0MUV3).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0NextSlot,  kL0LAV).GetPrimitiveID();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = ((uint32_t)fL0TP->GetPrimitive(kL0NextSlot,    kL0TALK).GetPrimitiveID() << 16) + 
                        (uint32_t)fL0TP->GetPrimitive(kL0NextSlot, kL0NewCHOD).GetPrimitiveID();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = (uint32_t)(0x0000 << 16) + 
                       (uint32_t)fL0TP->GetPrimitive(kL0NextSlot, kL0Calo).GetPrimitiveID();

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  fBuffer[EvtOffset] = (uint32_t)0x00000000;

  MEPlength++;
  Eventlength++;
  EvtOffset++;
  nWords++;

  EvtPcFarmOffset = MEPlength;
  EvtOffset = nWords;           //nWords is never reset

  // MEP Header - Overwritten to add the total MEP length //

  fBuffer[EvtOffset-EvtPcFarmOffset+1] = ((uint32_t)jBoard      << 24) + 
                                         ((uint32_t)NEventInMep << 16) +
                                          (uint32_t)((MEPlength*4) & 0xFFFF);

  // Event Header - Overwritten to add the total Event length //

  fBuffer[EvtOffset-EvtPcFarmOffset+2] = ((uint32_t)EventFlag << 24) + 
                                         ((uint32_t)(EventNumber & 0xFF) << 16) +
                                          (uint32_t)((Eventlength*4) & 0xFFFF);

  EvtPcFarmOffset = 0;
  EvtOffset = nWords;

  //  for (UInt_t iWord = 0/*BoardOffset*/; iWord < sizeof(fBuffer); iWord++) {    
  for (UInt_t iWord = BoardOffset; iWord < 26; iWord++) {  
    fwrite(&fBuffer[iWord], sizeof(uint32_t), 1, fBinaryFile);
    PcfarmEventlength++;
    Pcfarm_nWords++;
  }

  // Encoding event info requested by Pcfarm header input file for L1 testing procedure //

  fprintf(fHeaderFile,"%d,",Pcfarm_nWords);
  fprintf(fHeaderFile,"0x%lx,",fTimeStamp);
  fprintf(fHeaderFile,"%lu:",fFineTime);
  fprintf(fHeaderFile,"%d\n",PcfarmEventlength);
   
  return fBinaryEvent;
}

// End of Processing

void L0TPRawEncoder::End(){
  Close();
}
