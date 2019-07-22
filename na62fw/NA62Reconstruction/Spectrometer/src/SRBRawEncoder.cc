// ---------------------------------------------------------------
// History:
//
// Created by Jacopo Pinzino (jacopo.pinzino@cern.ch) 2015-04-27
// Modified by Maria Brigida Brunetti (maria.brigida.brunetti@cern.ch) 2016-01-21
//
// ---------------------------------------------------------------

#include "SRBRawEncoder.hh"
#include "SRBRawDecoder.hh"
#include "BinaryEvent.hh"
#include <stdlib.h>
#include <stdint.h>
#include "TSpectrometerDigi.hh"
#include "SRBVHit.hh"
#include "NA62Reconstruction.hh"
#include "NA62RecoManager.hh"
#include "TSpectrometerDigi.hh"
#include "Riostream.h"
#include "Debug.h" 

#define MAXNSRB 32
#define MAXNWORDS 10000
#define LEADING_EDGE_TYPE 0
#define TRAILING_EDGE_TYPE 1

//Constructor
SRBRawEncoder::SRBRawEncoder(NA62VReconstruction * Reco) :
  NA62VRawEncoder(Reco, "SRB"),
  fNSlots(nullptr),
  fLastSlotID(nullptr),
  fOffset(0),
  fLatency(nullptr)
{
  if(Reco->GetName()=="Spectrometer"){
    fBinaryEvent = new BinaryEvent(MAXNSRB,MAXNWORDS); 
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TSpecialTrigger::Class());
  }
  else NA62VReconstruction::Exception("SRBRawEncoder: requested subdetector not available");
  
  //fBuffer: global buffer needed to assemble global NA62 Data Format (merger output)
  //fBufferPerSRB: array of buffers (per SRB) needed to arrange data output in binary file requested by L1 testing procedure
  fBuffer = (uint32_t*)fBinaryEvent->GetBuffer();
  fBufferPerSRB = (uint32_t**)fBinaryEvent->GetOutputBuffer();
}

//Destructor
SRBRawEncoder::~SRBRawEncoder(){	 
}

//Encoder
BinaryEvent * SRBRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData)
{
  Int_t wrn = fReco->GetRawDecoder()->GetDecoder()->GetWarningsLevel();

  fNHitsOutOfSlot = 0;
  fBinaryEvent->Clear();

  Int_t EventNumber = tEvent->GetID(); 
  debug_cout(1, "Event Number = " << EventNumber);
  
  if(!static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetNProcessedEventsInTotal()){
    fprintf(fHeaderFile,"%d\n",static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetNEvt());
  }

  if(!fIsRawData){
    if(!EventNumber) fTimeStamp = 0x3E8;
    else fTimeStamp = 0x3E8 + EventNumber*0xC8; //200KHz Hardcoded TimeStamp - starting from 1000
    fFineTime = 0;
    debug_cout(1, "Event TimeStamp = " << std::hex << fTimeStamp << dec);
  }
  else {
    fTimeStamp = NA62RecoManager::GetInstance()->GetEventHeader()->GetTimeStamp();
    fFineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime();
    debug_cout(1, "Event TimeStamp = " << std::hex << fTimeStamp << dec);
    debug_cout(1, "Event FineTime = " << std::hex << fFineTime << dec);
  }

  if (fFineTime == 0) return fBinaryEvent;

  Int_t fDataFormat = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetDataFormat();

  const Int_t MaxNSlots = *std::max_element(fNSlots,fNSlots+fNROBoards);
  const int NSlotWords = 4;
  UInt_t SlotTS[fNROBoards][MaxNSlots];
  Int_t NEventInMep = 1;
  Int_t blocklength[fNROBoards];
  Int_t EventFlag = 0; 
  Int_t Eventlength[fNROBoards]; //printed to header file
  Int_t DatawordErrorFlag = 0; 

  Int_t L0TriggerType = fDataFormat? 0x1 : 0xc; //2015 vs 2014 trigger word
 

  TClonesArray &  Digis = (*(tEvent->GetHits()));
  //Digis.Sort();
  Int_t nDigis = tEvent->GetNHits();

  Int_t nWords = 0; //printed to header file
  Int_t SRBDataBlockOffset = 4;
  Int_t SRBDataWordOffset = 2;
  Int_t iData[fNROBoards]; //dataword index - used to arrange pairs of 16 bit datawords in 32 bit datawords.

  UInt_t NEdgesInSlot[fNROBoards][MaxNSlots];

  //Initializing arrays and filling slots with timestamp for each SRB
  for(Int_t iSRB=0;iSRB<fNROBoards;++iSRB){

    blocklength[iSRB]=0;
    Eventlength[iSRB]=0;
    iData[iSRB]=0;
      //Filling time slots coarse times (starting from Event time)
      for(Int_t iSlot=0;iSlot<fNSlots[iSRB];++iSlot){
        NEdgesInSlot[iSRB][iSlot]=0;
        SlotTS[iSRB][iSlot] = fTimeStamp + fOffset - fLatency[iSRB] + iSlot;
      }
  }
  
  //Writing header words
  for(Int_t jSRB=0; jSRB<fNROBoards; ++jSRB){

    // MEP Header - 1 //
    fBufferPerSRB[jSRB][0]     = (fDetID << 24) + EventNumber;

    // MEP Header -2 //
    fBufferPerSRB[jSRB][1]     = (jSRB << 24) + (NEventInMep << 16) + (((SRBDataBlockOffset +SRBDataWordOffset)*4) & 0x0000FFFF);

    // Event Header // 
    fBufferPerSRB[jSRB][2]     = (EventFlag << 24) + ((EventNumber & 0xFF) << 16); //total event lenght added later

    // Event TimeStamp // 
    fBufferPerSRB[jSRB][3]     = fTimeStamp;

    // Event Data Block - SRB Board Header //
    fBufferPerSRB[jSRB][SRBDataBlockOffset] = (SRBDataWordOffset*4 << 16) + (L0TriggerType << 8) + (EventFlag << 6) + (jSRB << 0);
    blocklength[jSRB]++; //event data block lenght (packet lenght)

    //First timeslot coarse time: 25ns timestamp of the first timeslot.
    //Fine time: hit time (LSB = 780 ps).
  
    fBufferPerSRB[jSRB][SRBDataBlockOffset+1]     = fTimeStamp + fOffset - fLatency[jSRB];
    blocklength[jSRB]++; //event data block lenght (packet lenght)

    if(jSRB==fNROBoards-1) { debug_cout(2, "Eventlength[" << jSRB << "] = " << Eventlength[jSRB]); }
  }
  //End of loop over SRBs
  
  Double_t Leading = -1e28;
  Double_t Trailing = -1e28;
  Int_t StrawAddr = -1;
  Int_t SRBAddr = -1;
  Int_t EdgeType = -1;
  ULong_t EdgeCoarseTime = -99999;
  Double_t EdgeFineTime = -99999;
  Int_t EdgeFineTimeStamp = -99999;
  Double_t SRBCalib = ClockPeriod/32.;

  //Loop over Digi to find the Hits
  for (Int_t iDigi = 0; iDigi < nDigis ; ++ iDigi ){
   bool HitOutOfSlot=false;
   if(fReco->GetName()=="Spectrometer"){
    TSpectrometerDigi *Digi = static_cast<TSpectrometerDigi*>(Digis[iDigi]);
    Leading = Digi->GetLeadingEdge();
    Trailing = Digi->GetTrailingEdge();
    StrawAddr = Digi->GetStrawAddr();
    SRBAddr = Digi->GetSRBAddr();
    EdgeType = Digi->GetDetectedEdge();
   }
   else NA62VReconstruction::Exception("SRBRawEncoder: (Digi) requested subdetector not available");

   if(SRBAddr > fNROBoards) {
     debug_cout(1, "ERROR!!! SRBAddr > fNSRB! SRBAddr = " << SRBAddr << "StrawAddr = " << StrawAddr);
     continue;
   }

   if(SRBAddr < 0 || StrawAddr < 0 || EdgeType < 0) {
      debug_cout(1, "warning! SRBAddr = " << SRBAddr << " StrawAddr = " << StrawAddr);
      continue;
    }

    if ((EdgeType == LEADING_EDGE_TYPE) && !(Leading > 0 && Trailing < 0)) {
      debug_cout(1, "warning! EdgeType = " << EdgeType << " Leading = " << Leading << " Trailing = " << Trailing);
    }

    if ((EdgeType == TRAILING_EDGE_TYPE) && !(Leading < 0 && Trailing > 0)) {
      debug_cout(1, "warning! EdgeType = " << EdgeType << " Leading = " << Leading << " Trailing = " << Trailing);
    }
    //Reconstructing edge coarse and fine times from digi time
    if(EdgeType == LEADING_EDGE_TYPE) {
      EdgeCoarseTime = (Int_t)(Leading/ClockPeriod);
      EdgeFineTime = (Leading/ClockPeriod) - EdgeCoarseTime;
      EdgeFineTimeStamp = EdgeFineTime*ClockPeriod/SRBCalib;
      }
    else if(EdgeType == TRAILING_EDGE_TYPE) {
      EdgeCoarseTime = (Int_t)(Trailing/ClockPeriod);
      EdgeFineTime = (Trailing/ClockPeriod) - EdgeCoarseTime;
      EdgeFineTimeStamp = EdgeFineTime*ClockPeriod/SRBCalib;
      }
    else { debug_cout(1, "Warning! edge type is not Leading or Trailing!"); }

    debug2_cout(1, "StrawAddr = " << StrawAddr << "SRBAddr = " << SRBAddr);
    debug2_cout(1, "Leading Edge (ns) = " << Leading << hex);
    debug2_cout(1, "Trailing Edge (ns) = " << Trailing << hex);
    Int_t EdgeSlotID = -1;    

    //Loop over slots to match slot time to edge coarse time; save matching slot ID
    for(Int_t iSlot=0; iSlot<fNSlots[SRBAddr]; ++iSlot){
      if(EdgeCoarseTime == SlotTS[SRBAddr][iSlot]){  
	EdgeSlotID = iSlot;
        NEdgesInSlot[SRBAddr][iSlot]++;
      }
    
    }

    if(EdgeSlotID==-1){
      HitOutOfSlot=true;
      fNHitsOutOfSlot++;
    }
    
    //Writing 16 bit datawords - arranged in pairs
    if(!HitOutOfSlot){    
     if ((iData[SRBAddr]%2) == 0) {
        fBufferPerSRB[SRBAddr][SRBDataBlockOffset + SRBDataWordOffset + (Int_t)(iData[SRBAddr]/2)] = (DatawordErrorFlag << 14) + (StrawAddr << 6) + (EdgeType << 5) + (EdgeFineTimeStamp << 0);
      }
     else {
        fBufferPerSRB[SRBAddr][SRBDataBlockOffset + SRBDataWordOffset + (Int_t)(iData[SRBAddr]/2)] |= (DatawordErrorFlag << 30) + (StrawAddr << 22) + (EdgeType << 21) + (EdgeFineTimeStamp << 16);
      }
    iData[SRBAddr]++;	
    }
    else {
      debug_cout(1,"Warning: slot not found. SRBAddr = " << SRBAddr << " coarse time = " << EdgeCoarseTime);
    }
  } //end of loop over Digis

  //Initializing total number of words in all fNROBoards Event Data Blocks;
  nWords = 0;
  Int_t iBufferWord = 0;
  Int_t NSRBDataWords;
  for(Int_t iSRB=0;iSRB<fNROBoards;++iSRB){
    Eventlength[iSRB] = 0;
    NSRBDataWords = 0;
    if(iData[iSRB]%2==0) NSRBDataWords = (Int_t)(iData[iSRB]/2);
    else NSRBDataWords  = (Int_t)(iData[iSRB]/2)+1; //odd word
    blocklength[iSRB] += NSRBDataWords;

    //Event Data Block - SRB Board Header // OVERWRITTEN with event data block length
    fBufferPerSRB[iSRB][SRBDataBlockOffset] = ((blocklength[iSRB]+NSlotWords)*4 << 16) + (L0TriggerType << 8) + (EventFlag << 6) + (iSRB << 0);

    //Filling global buffer:
    fBuffer[iBufferWord++]=fBufferPerSRB[iSRB][0];
    fBuffer[iBufferWord++]=fBufferPerSRB[iSRB][1];
    fBuffer[iBufferWord++]=fBufferPerSRB[iSRB][2];
    fBuffer[iBufferWord++]=fBufferPerSRB[iSRB][3];
    fBuffer[iBufferWord++]=fBufferPerSRB[iSRB][SRBDataBlockOffset];
    fBuffer[iBufferWord++]=fBufferPerSRB[iSRB][SRBDataBlockOffset+1];
    for(int iSRBDataWord = 0; iSRBDataWord < NSRBDataWords; iSRBDataWord++){
      fBuffer[iBufferWord++]=fBufferPerSRB[iSRB][iSRBDataWord];
    }

    //Checking that sum of edges in slots = number of edges
    Int_t NSlotsCheck = 0;
    for (Int_t iSlotCheck = 0; iSlotCheck < fNSlots[iSRB]; ++iSlotCheck){
       NSlotsCheck+=NEdgesInSlot[iSRB][iSlotCheck]; 
    }
    if (NSlotsCheck!=iData[iSRB]) {
      debug_cout(1,"ERROR! Slot counter and number of edges do not match! Nr. edges = " << iData[iSRB] << " Total slot counter = "  << NSlotsCheck);
      
    }

    //Saving slot counters (4 32-bit words, each word = 4 slots) for each SRB
    //Slot0-3
    fBufferPerSRB[iSRB][SRBDataBlockOffset+blocklength[iSRB]] = (NEdgesInSlot[iSRB][3] << 24) + (NEdgesInSlot[iSRB][2] << 16) + (NEdgesInSlot[iSRB][1] << 8) + (NEdgesInSlot[iSRB][0] << 0);
    //Slot4-7
    fBufferPerSRB[iSRB][SRBDataBlockOffset+blocklength[iSRB]+1] = (NEdgesInSlot[iSRB][7] << 24) + (NEdgesInSlot[iSRB][6] << 16) + (NEdgesInSlot[iSRB][5] << 8) + (NEdgesInSlot[iSRB][4] << 0);
    //Slot8-11
    fBufferPerSRB[iSRB][blocklength[iSRB]+SRBDataBlockOffset+2] = (NEdgesInSlot[iSRB][11] << 24) + (NEdgesInSlot[iSRB][10] << 16) + (NEdgesInSlot[iSRB][9] << 8) + (NEdgesInSlot[iSRB][8] << 0);
    //Slot12-15
    fBufferPerSRB[iSRB][blocklength[iSRB]+SRBDataBlockOffset+3] = (NEdgesInSlot[iSRB][15] << 24) + (NEdgesInSlot[iSRB][14] << 16) + (NEdgesInSlot[iSRB][13] << 8) + (NEdgesInSlot[iSRB][12] << 0);

    for(Int_t iSRBEventWord=0;iSRBEventWord<(blocklength[iSRB]+NSlotWords);++iSRBEventWord){
      fwrite(&fBufferPerSRB[iSRB][SRBDataBlockOffset+iSRBEventWord], sizeof(uint32_t), 1, fBinaryFile);
    }

    Eventlength[iSRB] = blocklength[iSRB] + NSlotWords;
    nWords +=  Eventlength[iSRB];
  }

  //Writing header file
  fprintf(fHeaderFile,"%d,",nWords);
  fprintf(fHeaderFile,"0x%lx,",fTimeStamp);
  fprintf(fHeaderFile,"%lu:",fFineTime);
  
  for(Int_t iSRB=0;iSRB<fNROBoards;++iSRB){
    if(iSRB==fNROBoards-1) fprintf(fHeaderFile,"%d\n",Eventlength[iSRB]); //writing number of words in Event Data Block for iSRB
    else fprintf(fHeaderFile,"%d,",Eventlength[iSRB]);
  }

  if(fNHitsOutOfSlot){
    cerr_en(wrn,WARN_DET) << "[SRBRawEncoder]     WARNING: Hits out of Slot!    Event: " << tEvent->GetID() << " SubDet: " << fReco->GetName() << " TS: " << std::hex << fTimeStamp << " - Number of Hits out of Slot " << std::dec << fNHitsOutOfSlot << std::endl;
  } 

  return fBinaryEvent;
}
 
//Init
void SRBRawEncoder::Init(){
  fBinaryFileName = fReco->GetBinaryFileName();
  fHeaderFileName = fReco->GetHeaderFileName();
  debug_cout(1, "RawEncoder: Setting header filename " << fHeaderFileName);
  debug_cout(1, "RawEncoder: Setting binary filename " << fBinaryFileName);

  Open();
  fprintf(fHeaderFile,fBinaryFileName.Append("\n"));
  fDetID = fReco->GetDetID();
  debug_cout(1,"Detector ID " << std::hex << fDetID); 
  fNROBoards = fReco->GetRawDecoder()->GetDecoder()->GetNROBoards();
  debug_cout(1,"Number or Readout Boards " << std::dec << fNROBoards); 

  fNSlots = fReco->GetRawDecoder()->GetDecoder()->GetNSlots();
  fLatency = static_cast<SRBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetSRBLatency();
  fOffset = static_cast<SRBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetSRBOffset();
  fLastSlotID = fReco->GetRawDecoder()->GetDecoder()->GetLastSlotID();

  Int_t iBoardTot=0;
  for(Int_t iBoard=0; iBoard<fNROBoards; iBoard++){      
      debug_cout(1,"Number of Slots: " << fNSlots[iBoardTot] << " per Readout Board " << iBoard << " of Station " << iStation);
      debug_cout(1,"Index of Last Slot: " << fLastSlotID[iBoardTot] << " per Readout Board nr. " << iBoard << " of Station " << iStation);
      debug_cout(1,"Latency: " << fLatency[iBoardTot] << " per Readout Board nr. " << iBoard << " of Station " << iStation);
      debug_cout(1,"SRB Offset: " << fOffset);
      iBoardTot++; 
    }
  
  fprintf(fHeaderFile,"%#x:",fDetID);
  fprintf(fHeaderFile,"%d:",fNROBoards);
}

//End of Processing

void SRBRawEncoder::End(){
  Close();
}
