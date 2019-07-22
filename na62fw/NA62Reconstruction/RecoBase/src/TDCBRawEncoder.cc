// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-24
// Modified by Maria Brigida Brunetti (maria.brigida.brunetti@cern.ch) 2016-01-21
// ---------------------------------------------------------------

#include "TDCBRawEncoder.hh"
#include "TDCBRawDecoder.hh"
#include "BinaryEvent.hh"
#include "TDCBBuffer.hh"
#include "TDCBBufferProto.hh"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "TDCVHit.hh"
#include "TCedarDigi.hh"
#include "TCHANTIDigi.hh"
#include "TCHODDigi.hh"
#include "TNewCHODDigi.hh"
#include "TGigaTrackerDigi.hh"
#include "TIRCDigi.hh"
#include "TLAVDigi.hh"
#include "TMUV3Digi.hh"
#include "TRICHDigi.hh"
#include "TSACDigi.hh"
#include "TCedarSpecialTriggerEvent.hh"
#include "TTDCBSpecialTrigger.hh"
#include "Riostream.h"
#include <stdlib.h>
#include <stdint.h>

#define DBGTHIS "Encoder"
//#define DEBUG "Encoder"
//#define DBGALL "Encoder"
#include "Debug.h" 

#define MAXNTEL62S 12
#define MAXNFPGAS 4
#define MAXNFRAME 32
#define MAXNWORDS 1000

// Constructor

TDCBRawEncoder::TDCBRawEncoder(NA62VReconstruction * Reco) : NA62VRawEncoder(Reco, "TDCB"), fNFPGAs(0), fNSlots(0), fLastSlotID(0){

  fBinaryEvent = new BinaryEvent(MAXNTEL62S, MAXNWORDS);
  if(Reco->GetName()=="Cedar"){
    fSpecialTriggerEvent = new TCedarSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="CHANTI"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="CHOD"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="GigaTracker"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="HAC"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="IRC"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="LAV"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="LKr"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="MUV0"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="MUV1"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="MUV2"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  } 
  else if(Reco->GetName()=="MUV3"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="NewCHOD"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="RICH"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(Reco->GetName()=="SAC"){
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else NA62VReconstruction::Exception("TDCBRawEncoder: requested subdetector '"+Reco->GetName()+"' not available");

  fSubBuffer = new uint32_t**[MAXNTEL62S];
  fSubBuffer_tmp = new uint32_t**[MAXNTEL62S];
  for(Int_t iTel62=0; iTel62<MAXNTEL62S; ++iTel62){
    fSubBuffer[iTel62] = new uint32_t*[MAXNFPGAS];
    fSubBuffer_tmp[iTel62] = new uint32_t*[MAXNFPGAS];
    for(Int_t iFPGA=0; iFPGA<MAXNFPGAS; ++iFPGA){
      fSubBuffer[iTel62][iFPGA] = new uint32_t[MAXNWORDS];
      fSubBuffer_tmp[iTel62][iFPGA] = new uint32_t[MAXNWORDS];
    }
  }

  fBuffer = (uint32_t*)fBinaryEvent->GetBuffer();
  fBufferPerTel62 = (uint32_t**)fBinaryEvent->GetOutputBuffer();

}

// Destructor

TDCBRawEncoder::~TDCBRawEncoder(){
  for(Int_t iTel62=0; iTel62<MAXNTEL62S; ++iTel62){
    for(Int_t iFPGA=0; iFPGA<MAXNFPGAS; ++iFPGA){
      if(fSubBuffer && fSubBuffer[iTel62] && fSubBuffer[iTel62][iFPGA])             delete [] fSubBuffer[iTel62][iFPGA];
      if(fSubBuffer_tmp && fSubBuffer_tmp[iTel62] && fSubBuffer_tmp[iTel62][iFPGA]) delete [] fSubBuffer_tmp[iTel62][iFPGA];
    }
    if(fSubBuffer && fSubBuffer[iTel62])         delete [] fSubBuffer[iTel62];
    if(fSubBuffer_tmp && fSubBuffer_tmp[iTel62]) delete [] fSubBuffer_tmp[iTel62];
  }
  if(fSubBuffer){
    delete [] fSubBuffer;
    fSubBuffer = 0;
  }
  if(fSubBuffer_tmp){
    delete [] fSubBuffer_tmp;
    fSubBuffer_tmp = 0;
  }
}

// Init

void TDCBRawEncoder::Init(){

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

  fNROMezzanines = fReco->GetRawDecoder()->GetDecoder()->GetNROMezzanines();
  debug_cout(1,"Number or Readout Mezzanines " << std::dec << fNROMezzanines); 

  fNFPGAs = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetNFPGAs();
  fNSlots = fReco->GetRawDecoder()->GetDecoder()->GetNSlots();
  fLastSlotID = fReco->GetRawDecoder()->GetDecoder()->GetLastSlotID();
  fROMezzaninesT0 = fReco->GetRawDecoder()->GetDecoder()->GetROMezzaninesT0();

  Int_t iStation = 0;
  UInt_t iMezzanineTot=0;
  for(Int_t iROBoard=0; iROBoard<fNROBoards; iROBoard++){
    Int_t BoardCounter=0;
    while(iStation<fReco->GetNStations()-1 && iROBoard>=fReco->GetRawDecoder()->GetDecoder()->GetNROBoardsPerStation(iStation)+BoardCounter){
      BoardCounter+=fReco->GetRawDecoder()->GetDecoder()->GetNROBoardsPerStation(iStation);
      iStation++;
    }
    for(UInt_t iMezzanine=0; iMezzanine<fReco->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard(); iMezzanine++){
      debug_cout(1,"ROMezzanine T0 Offset " << fROMezzaninesT0[iMezzanineTot] << " per Mezzanine " << iMezzanineTot << " of Station " << iStation);
      debug_cout(1,"Number of Slots in Time Window " << fNSlots[iMezzanineTot] << " per Mezzanine " << iMezzanineTot << " of Station " << iStation);
      debug_cout(1,"Index of Last Slot in Time Window " << fLastSlotID[iMezzanineTot] << " per Mezzanine " << iMezzanineTot << " of Station " << iStation);
      iMezzanineTot++;
    }
  }
  fprintf(fHeaderFile,"%#x:",fDetID);
  fprintf(fHeaderFile,"%d:",fNROBoards);
}

// Encoding
// Four buffers are filled inside this method: fBuffer, fBufferPerTel62, fSubBuffer_tmp
// fBuffer: global buffer needed to assemble global NA62 Data Format (merger output)
// fBufferPerTel62: array of buffers (per Tel62) needed to arrange data output in binary file requested by L1 testing procedure
// fSubBuffer and fSubBuffer_tmp: arrays of buffers (per Tel62, per FPGA) used only for the construction of the final data output  

BinaryEvent * TDCBRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData)
{

  UInt_t wrn = fReco->GetRawDecoder()->GetDecoder()->GetWarningsLevel();

  fNHitsOutOfSlot = 0;
  fNLeadsOutOfSlot = 0;
  fNTrailsOutOfSlot = 0;
  fNLeadingsOnly = 0;
  fNTrailingsOnly = 0;
  fBinaryEvent->Clear();

  Int_t EventNumber = tEvent->GetID();
  debug_cout(1, "Event Number = " << EventNumber);

  if(!static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetNProcessedEventsInTotal()){
    fprintf(fHeaderFile,"%d\n",(static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetNEvt()));
  }

  if(!fIsRawData){
    if(!EventNumber) fTimeStamp = 0x3E8;
    else fTimeStamp = 0x3E8 + EventNumber*0xC8; //This is 200KHz Hardcoded Timestamp - starting from 1000 
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

  ULong_t SlotTS[MAXNTEL62S][MAXNFPGAS][MAXNFRAME];
  Int_t NEventInMep = 1;
  Int_t MEPlength[MAXNTEL62S];
  Int_t EventFlag = 0;
  Int_t Eventlength[MAXNTEL62S];
  Int_t PcfarmEventlength[MAXNTEL62S];

  Int_t fDataFormat = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetDataFormat();
  Int_t L0TriggerType = 0x4;
  Int_t BoardFlag[MAXNTEL62S];
  Int_t FPGAFlag = 0; 

  Int_t OffLeading = 4;
  Int_t OffTrailing = 5;

  ULong_t raw_time_leading = 0x0;
  ULong_t raw_time_trailing = 0x0;

  bool HitOutOfSlot=false;
  //bool LeadOutOfSlot=false;
  //bool TrailOutOfSlot=false;
  //bool LeadOnly=false;
  //bool TrailOnly=false;

  TClonesArray &  Digis = (*(tEvent->GetHits()));
  Int_t nDigis = tEvent->GetNHits();
  Digis.Sort();

  Int_t nWords = 0;
  Int_t Pcfarm_nWords = 0;
  Int_t nTDChit = 0;

  Int_t EvtOffset = 0;
  Int_t EvtPcFarmOffset = 0;
  Int_t Tel62Offset = 4;
  Int_t FPGAOffset = 5;
  Int_t HitOffset = 0;

  Int_t nNoEmptySlot[MAXNTEL62S][MAXNFPGAS];
  Int_t nWordsInSlot[MAXNTEL62S][MAXNFPGAS][MAXNFRAME];

  for(Int_t iTel62=0;iTel62<MAXNTEL62S;++iTel62){
    MEPlength[iTel62]=0;
    Eventlength[iTel62]=0;
    PcfarmEventlength[iTel62]=0;
    for(Int_t iFPGA=0;iFPGA<MAXNFPGAS;++iFPGA){
      nNoEmptySlot[iTel62][iFPGA]=0;
      for(Int_t iSlot=0;iSlot<MAXNFRAME;++iSlot){
        nWordsInSlot[iTel62][iFPGA][iSlot]=0;
      }
      for(Int_t iWord=0;iWord<MAXNWORDS;++iWord){
        fSubBuffer[iTel62][iFPGA][iWord] = 0;
        fSubBuffer_tmp[iTel62][iFPGA][iWord] = 0;
      }
    }
  }

  Int_t reloopID=0;
  Int_t jSlot_tmp;

  for(Int_t jTel62=0; jTel62<fNROBoards; ++jTel62){
    // MEP Header - 1 //
    fBuffer[EvtOffset+0]         = (fDetID << 24) + EventNumber;     //Global Buffer
    fBufferPerTel62[jTel62][0]   = (fDetID << 24) + EventNumber;     //Buffer per Tel62
    fSubBuffer[jTel62][0][0]     = (fDetID << 24) + EventNumber;     //SubBuffer per Tel62 per FPGA
    fSubBuffer_tmp[jTel62][0][0] = (fDetID << 24) + EventNumber;     //SubBuffer per Tel62 per FPGA

    MEPlength[jTel62]++;
    nWords++;

    // MEP Header -2 //
    fBuffer[EvtOffset+1]         = (jTel62 << 24) + (NEventInMep << 16);
    fBufferPerTel62[jTel62][1]   = (jTel62 << 24) + (NEventInMep << 16);
    fSubBuffer[jTel62][0][1]     = (jTel62 << 24) + (NEventInMep << 16);
    fSubBuffer_tmp[jTel62][0][1] = (jTel62 << 24) + (NEventInMep << 16);

    MEPlength[jTel62]++;
    nWords++;

    // Event Header //
    fBuffer[EvtOffset+2]         = (EventFlag << 24) + ((EventNumber & 0xFF) << 16);
    fBufferPerTel62[jTel62][2]   = (EventFlag << 24) + ((EventNumber & 0xFF) << 16);
    fSubBuffer[jTel62][0][2]     = (EventFlag << 24) + ((EventNumber & 0xFF) << 16);
    fSubBuffer_tmp[jTel62][0][2] = (EventFlag << 24) + ((EventNumber & 0xFF) << 16);

    MEPlength[jTel62]++;
    Eventlength[jTel62]++;
    nWords++;

    // Event Timestamp //
    fBuffer[EvtOffset+3]         = fTimeStamp;
    fBufferPerTel62[jTel62][3]   = fTimeStamp;
    fSubBuffer[jTel62][0][3]     = fTimeStamp;
    fSubBuffer_tmp[jTel62][0][3] = fTimeStamp;

    MEPlength[jTel62]++;
    Eventlength[jTel62]++;
    nWords++;

    // Event Data Block - Tel62 Board Header //
    BoardFlag[jTel62] = pow(2,fNFPGAs[jTel62])-1;

    fBuffer[EvtOffset+Tel62Offset]         = (fDataFormat << 24) + (jTel62 << 16) + (L0TriggerType << 8) + (BoardFlag[jTel62]);
    fBufferPerTel62[jTel62][Tel62Offset]   = (fDataFormat << 24) + (jTel62 << 16) + (L0TriggerType << 8) + (BoardFlag[jTel62]);
    fSubBuffer[jTel62][0][Tel62Offset]     = (fDataFormat << 24) + (jTel62 << 16) + (L0TriggerType << 8) + (BoardFlag[jTel62]);
    fSubBuffer_tmp[jTel62][0][Tel62Offset] = (fDataFormat << 24) + (jTel62 << 16) + (L0TriggerType << 8) + (BoardFlag[jTel62]);

    MEPlength[jTel62]++;
    Eventlength[jTel62]++;
    nWords++;

    for(Int_t jFPGA=0; jFPGA<fNFPGAs[jTel62]; ++jFPGA){
      if(!fDataFormat){
        // Event Data Block - FPGA header - 2014 DATA//
        fBuffer[EvtOffset+FPGAOffset]                       = (FPGAFlag << 24) + (jFPGA << 16) + (nNoEmptySlot[jTel62][jFPGA] << 8) + (fNSlots[4*jTel62+jFPGA] & 0x000000FF);
        fBufferPerTel62[jTel62][EvtPcFarmOffset+FPGAOffset] = (FPGAFlag << 24) + (jFPGA << 16) + (nNoEmptySlot[jTel62][jFPGA] << 8) + (fNSlots[4*jTel62+jFPGA] & 0x000000FF);  // buffer used to write Pcfarm binary input file for L1 testing procedure 
        fSubBuffer[jTel62][jFPGA][FPGAOffset]               = (FPGAFlag << 24) + (jFPGA << 16) + (nNoEmptySlot[jTel62][jFPGA] << 8) + (fNSlots[4*jTel62+jFPGA] & 0x000000FF);
        fSubBuffer_tmp[jTel62][jFPGA][FPGAOffset]           = (FPGAFlag << 24) + (jFPGA << 16) + (0 << 8) + (fNSlots[4*jTel62+jFPGA] & 0x000000FF);
      }
      else{
        // Event Data Block - FPGA header - 2015 DATA//
        fBuffer[EvtOffset+FPGAOffset]                       = (FPGAFlag << 24) + (jFPGA << 16) + (0xff << 8) + (nNoEmptySlot[jTel62][jFPGA] & 0x000000FF); 
        fBufferPerTel62[jTel62][EvtPcFarmOffset+FPGAOffset] = (FPGAFlag << 24) + (jFPGA << 16) + (0xff << 8) + (nNoEmptySlot[jTel62][jFPGA] & 0x000000FF);  // buffer used to write Pcfarm binary input file for L1 testing procedure 
        fSubBuffer[jTel62][jFPGA][FPGAOffset]               = (FPGAFlag << 24) + (jFPGA << 16) + (0xff << 8) + (nNoEmptySlot[jTel62][jFPGA] & 0x000000FF); 
        fSubBuffer_tmp[jTel62][jFPGA][FPGAOffset]           = (FPGAFlag << 24) + (jFPGA << 16) + (0xff << 8) + (0 & 0x000000FF);
      }
      MEPlength[jTel62]++;
      Eventlength[jTel62]++;
      nWords++;

      for(Int_t jSlot=0; jSlot<fNSlots[4*jTel62+jFPGA]; ++jSlot){

        jSlot_tmp = jSlot - (fNSlots[4*jTel62+jFPGA] - (fLastSlotID[4*jTel62+jFPGA]+1));
        if(jSlot_tmp < 0) SlotTS[jTel62][jFPGA][jSlot] = ((fTimeStamp + (Int_t)(fROMezzaninesT0[jTel62]/ClockPeriod)) + jSlot_tmp) & 0x0000FFFF;
        else if(!jSlot_tmp) SlotTS[jTel62][jFPGA][jSlot] = (fTimeStamp + (Int_t)(fROMezzaninesT0[jTel62]/ClockPeriod)) & 0x0000FFFF;
        else if(jSlot_tmp > 0) SlotTS[jTel62][jFPGA][jSlot] = ((fTimeStamp + (Int_t)(fROMezzaninesT0[jTel62]/ClockPeriod)) + jSlot_tmp) & 0x0000FFFF;

        // Event Data Block - Slot header //
        nWordsInSlot[jTel62][jFPGA][jSlot]++;

        fBuffer[EvtOffset+FPGAOffset+jSlot+nWordsInSlot[jTel62][jFPGA][jSlot]]                       = (nWordsInSlot[jTel62][jFPGA][jSlot] << 16) + (SlotTS[jTel62][jFPGA][jSlot] & 0x0000FFFF);
        fBufferPerTel62[jTel62][EvtPcFarmOffset+FPGAOffset+jSlot+nWordsInSlot[jTel62][jFPGA][jSlot]] = (nWordsInSlot[jTel62][jFPGA][jSlot] << 16) + (SlotTS[jTel62][jFPGA][jSlot] & 0x0000FFFF); 
        fSubBuffer[jTel62][jFPGA][FPGAOffset+jSlot+nWordsInSlot[jTel62][jFPGA][jSlot]]               = (nWordsInSlot[jTel62][jFPGA][jSlot] << 16) + (SlotTS[jTel62][jFPGA][jSlot] & 0x0000FFFF);
        fSubBuffer_tmp[jTel62][jFPGA][FPGAOffset+jSlot+nWordsInSlot[jTel62][jFPGA][jSlot]]           = (nWordsInSlot[jTel62][jFPGA][jSlot] << 16) + (SlotTS[jTel62][jFPGA][jSlot] & 0x0000FFFF);

        MEPlength[jTel62]++;
        Eventlength[jTel62]++;
        nWords++;
      }
      EvtPcFarmOffset=MEPlength[jTel62];    //MEPlength is reset for each Tel62
      EvtOffset=nWords;                     //nWords is never reset
      FPGAOffset=0;
    }

    // MEP Header - Overwritten to add the total MEP length //
    fBuffer[EvtOffset-EvtPcFarmOffset+1] = (jTel62 << 24) + (NEventInMep << 16) + ((MEPlength[jTel62]*4) & 0x0000FFFF);
    fBufferPerTel62[jTel62][1]           = (jTel62 << 24) + (NEventInMep << 16) + ((MEPlength[jTel62]*4) & 0x0000FFFF);
    fSubBuffer[jTel62][0][1]             = (jTel62 << 24) + (NEventInMep << 16) + ((MEPlength[jTel62]*4) & 0x0000FFFF);
    fSubBuffer_tmp[jTel62][0][1]         = (jTel62 << 24) + (NEventInMep << 16) + ((MEPlength[jTel62]*4) & 0x0000FFFF);

    // Event Header - Overwritten to add the total Event length //
    fBuffer[EvtOffset-EvtPcFarmOffset+2] = (EventFlag << 24) + ((EventNumber & 0xFF) << 16) + ((Eventlength[jTel62]*4) & 0x0000FFFF);
    fBufferPerTel62[jTel62][2]           = (EventFlag << 24) + ((EventNumber & 0xFF) << 16) + ((Eventlength[jTel62]*4) & 0x0000FFFF);
    fSubBuffer[jTel62][0][2]             = (EventFlag << 24) + ((EventNumber & 0xFF) << 16) + ((Eventlength[jTel62]*4) & 0x0000FFFF);
    fSubBuffer_tmp[jTel62][0][2]         = (EventFlag << 24) + ((EventNumber & 0xFF) << 16) + ((Eventlength[jTel62]*4) & 0x0000FFFF);

    EvtPcFarmOffset=0;
    if(jTel62==fNROBoards-1){
      debug_cout(2, "MEPlength[" << jTel62 << "] = " <<  MEPlength[jTel62]);
      debug_cout(2, "Eventlength[" << jTel62 << "] = " << Eventlength[jTel62]);
      debug_cout(2, "PcfarmEventlength[" << jTel62 << "] = " << PcfarmEventlength[jTel62]);
      debug_cout(2, "nWords = " << nWords);
    }
    EvtOffset=nWords;
    FPGAOffset=5;
  }

  Int_t jSlot = 0;
  bool LeadingFlag=false;
  bool TrailingFlag=false;
  Int_t DetectedEdge = 0;
  Double_t Leading = -1e28;
  Double_t Trailing = -1e28;
  Int_t ROChID = -1;
  Int_t iSlot_tmp;

  // Loop over Digi to find the Hits //
  for (Int_t iDigi = 0; iDigi < nDigis ; ++ iDigi ){

    if(fReco->GetName()=="Cedar"){
      TCedarDigi *Digi = static_cast<TCedarDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="CHANTI"){
      TCHANTIDigi *Digi = static_cast<TCHANTIDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="CHOD"){
      TCHODDigi *Digi = static_cast<TCHODDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="NewCHOD"){
      TNewCHODDigi *Digi = static_cast<TNewCHODDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="GigaTracker"){
      TGigaTrackerDigi *Digi = static_cast<TGigaTrackerDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="IRC"){
      TIRCDigi *Digi = static_cast<TIRCDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="LAV"){
      TLAVDigi *Digi = static_cast<TLAVDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="MUV3"){
      TMUV3Digi *Digi = static_cast<TMUV3Digi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="RICH"){
      TRICHDigi *Digi = static_cast<TRICHDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else if(fReco->GetName()=="SAC"){
      TSACDigi *Digi = static_cast<TSACDigi*>(Digis[iDigi]);
      DetectedEdge = (Int_t)Digi->GetDetectedEdge();
      Leading = Digi->GetLeadingEdge();
      Trailing = Digi->GetTrailingEdge();
      ROChID = static_cast<TDCBRawDecoder*>(fReco->GetRawDecoder()->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    }
    else NA62VReconstruction::Exception("TDCBRawEncoder: (Digi) requested subdetector '"+fReco->GetName()+"' not available");

    if(!DetectedEdge || (ROChID==-1)) continue;    

    Int_t Tel62ID = ROChID/512;
    Int_t tdcID = (ROChID % 512) / 32.;
    Int_t FPGAID =  tdcID / 4;
    Int_t FPGAChID = (ROChID % 512) % 32;

    Int_t LeadingSlotID = -1;
    Int_t TrailingSlotID = -1;

    debug2_cout(1, "Tel62ID = " << Tel62ID << " tdcID = " << tdcID << " FPGAID = " << FPGAID << " FPGAChID = " << FPGAChID << std::endl);

    raw_time_leading = ((Leading/TdcCalib) + fTimeStamp*256.);
    debug2_cout(1, "Leading Edge (ns) = " << Leading << std::hex << " raw_time_leading = " << raw_time_leading << std::dec << std::endl);

    raw_time_trailing = ((Trailing/TdcCalib) + fTimeStamp*256.);
    debug2_cout(1, "Trailing Edge (ns) = " << Trailing << std::hex << " raw_time_trailing = " << raw_time_trailing << std::dec << std::endl);

    for(Int_t iSlot=0; iSlot<fNSlots[Tel62ID]; ++iSlot){

      iSlot_tmp = iSlot - (fNSlots[Tel62ID] - (fLastSlotID[Tel62ID]+1));
      if(iSlot_tmp < 0) SlotTS[Tel62ID][FPGAID][iSlot] = ((fTimeStamp + (Int_t)(fROMezzaninesT0[Tel62ID]/ClockPeriod)) + iSlot_tmp) & 0x0000FFFF;
      else if(!iSlot_tmp) SlotTS[Tel62ID][FPGAID][iSlot] = (fTimeStamp + (Int_t)(fROMezzaninesT0[Tel62ID]/ClockPeriod)) & 0x0000FFFF;
      else if(iSlot_tmp > 0) SlotTS[Tel62ID][FPGAID][iSlot] = ((fTimeStamp + (Int_t)(fROMezzaninesT0[Tel62ID]/ClockPeriod)) + iSlot_tmp) & 0x0000FFFF;
      debug2_cout(2, "Slot TS = " << std::hex << SlotTS[Tel62ID][FPGAID][iSlot] << " Central Slot TS = " << fTimeStamp << std::dec << std::endl);

      SlotTS[Tel62ID][FPGAID][iSlot] += (fTimeStamp & 0xFFFF0000);

      if((fTimeStamp & 0xf000) == 0xf000 && (SlotTS[Tel62ID][FPGAID][iSlot] & 0xf000) == 0x0000) SlotTS[Tel62ID][FPGAID][iSlot] += 0x10000;  //16 bits overflow
      if((fTimeStamp & 0xf000) == 0x0000 && (SlotTS[Tel62ID][FPGAID][iSlot]&0xf000) == 0xf000) SlotTS[Tel62ID][FPGAID][iSlot] -= 0x10000;  //16 bits overflow

      if((raw_time_leading/0x100) == SlotTS[Tel62ID][FPGAID][iSlot]){
        LeadingSlotID = iSlot;
      }
      if((raw_time_trailing/0x100) == SlotTS[Tel62ID][FPGAID][iSlot]){
        TrailingSlotID = iSlot;
      }
    }
    debug2_cout(1, "Detected Edge = " << DetectedEdge << " LeadingSlotID = " << LeadingSlotID << " TrailingSlotID = " << TrailingSlotID << std::endl);

    if((LeadingSlotID == -1) && (TrailingSlotID == -1) && (DetectedEdge == 3)){
      debug_cout(1, "Hit Out of Slot or Tel62s Offset not correct !");
      HitOutOfSlot=true;
      fNHitsOutOfSlot++;
    }
    else if(((LeadingSlotID == -1) || (TrailingSlotID == -1)) && (DetectedEdge == 3)){
      debug_cout(1, "Leading or Trailing Out of Slot or Tel62s Offset not correct !");
      HitOutOfSlot=true;
      if(LeadingSlotID == -1){
        fNLeadsOutOfSlot++;
      }
      if(TrailingSlotID == -1){
        fNTrailsOutOfSlot++;
      } 
    }

    if((LeadingSlotID == -1) && (TrailingSlotID == -1) && (DetectedEdge == 2)){
      debug_cout(1, "Leading or Trailing Out of Slot or Tel62s Offset not correct !");
      HitOutOfSlot=true;
      if(Trailing){
        fNTrailingsOnly++;
        fNTrailsOutOfSlot++;
      }
      else if(Leading){
        fNLeadingsOnly++;
        fNLeadsOutOfSlot++;
      }
      else { debug_cout(2, "Incorrect Option for Detected Edge == 2 "); }
    }
    else if(((LeadingSlotID == -1) || (TrailingSlotID == -1)) && (DetectedEdge == 2)){
      if(LeadingSlotID == -1) fNTrailingsOnly++;
      if(TrailingSlotID == -1) fNLeadingsOnly++;
    }

    if((LeadingSlotID == -1) && (TrailingSlotID == -1) && (DetectedEdge == 1)){
      debug_cout(1, "Leading or Trailing Out of Slots or Tel62s Offset not correct !");
      HitOutOfSlot=true;
      if(Trailing){
        fNTrailingsOnly++;
        fNTrailsOutOfSlot++;
      }
      else if(Leading){
        fNLeadingsOnly++;
        fNLeadsOutOfSlot++;
      }
      else { debug_cout(2, "Incorrect Option: No detected edges !"); }
    }
    else if(((LeadingSlotID == -1) || (TrailingSlotID == -1)) && (DetectedEdge == 1)){
      if(LeadingSlotID == -1) fNTrailingsOnly++;
      if(TrailingSlotID == -1) fNLeadingsOnly++;
    }

    fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset];

    if(!FPGAID) reloopID = FPGAOffset;
    reloopID++;

    while(((DetectedEdge == 3) && ((!LeadingFlag) || (!TrailingFlag)) && (!HitOutOfSlot)) || (((DetectedEdge == 2) || (DetectedEdge == 1)) && ((!LeadingFlag) && (!TrailingFlag)) && (!HitOutOfSlot))){
      while(((jSlot != LeadingSlotID) || LeadingFlag) && ((jSlot != TrailingSlotID) || TrailingFlag)){

        if(nWordsInSlot[Tel62ID][FPGAID][jSlot] == 1){  //the slot is empty: only slotTS word is stored
          fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset];
          reloopID++;
        }
        else if(nWordsInSlot[Tel62ID][FPGAID][jSlot] > 1){
          fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset];

          nTDChit = nWordsInSlot[Tel62ID][FPGAID][jSlot] - 1;
          while(nTDChit){
            HitOffset++;
            fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset];
            nTDChit--;
          }
          reloopID++;
        }
        else {
          debug2_cout(2, "WordsInSlot[" << Tel62ID << "][" << FPGAID << "][" << jSlot << "] = " << nWordsInSlot[Tel62ID][FPGAID][jSlot] << std::endl);
          debug_cout(2, "WARNINGS: Number of Words in Slot cannot be zero - At least SlotTS word - Index jSlot out of range !");
          break;
        }
        jSlot++;
      }

      if(nWordsInSlot[Tel62ID][FPGAID][jSlot] > 1){ //the slot is NOT empty: slotTS + tdc words
        nWordsInSlot[Tel62ID][FPGAID][jSlot]++;	

        Int_t FPGAOffset_tmp = 0;
        if(!FPGAID) FPGAOffset_tmp = FPGAOffset;

        if(!fDataFormat) fSubBuffer[Tel62ID][FPGAID][FPGAOffset_tmp] = (FPGAFlag << 24) + (FPGAID << 16) + (nNoEmptySlot[Tel62ID][FPGAID] << 8) + (fNSlots[Tel62ID] & 0x000000FF);
        else fSubBuffer[Tel62ID][FPGAID][FPGAOffset_tmp] = (FPGAFlag << 24) + (FPGAID << 16) + (0xff << 8) + (nNoEmptySlot[Tel62ID][FPGAID] & 0x000000FF); 

        fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = (nWordsInSlot[Tel62ID][FPGAID][jSlot] << 16) + (SlotTS[Tel62ID][FPGAID][jSlot] & 0x0000FFFF);
        fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset] = fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset];

        nTDChit = nWordsInSlot[Tel62ID][FPGAID][jSlot] - 2;
        while(nTDChit){
          HitOffset++;
          fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset];
          nTDChit--;
        }
        reloopID++;
        if((jSlot == LeadingSlotID) && (!LeadingFlag)){
          fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = (OffLeading << 28)+(tdcID << 24)+(FPGAChID << 19)+(raw_time_leading & 0x7FFFF);
          LeadingFlag=true;
        }
        else if((jSlot==TrailingSlotID)&&(!TrailingFlag)){
          fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = (OffTrailing << 28)+(tdcID << 24)+(FPGAChID << 19)+(raw_time_trailing & 0x7FFFF);
          TrailingFlag=true;
        }
        else { debug_cout(1, "WARNINGS: Incorrect option"); }

        MEPlength[Tel62ID]++;
        Eventlength[Tel62ID]++;

        // MEP Header - Overwritten to update the total MEP length - fSubBuffer only //
        fSubBuffer[Tel62ID][0][1] = (Tel62ID << 24) + (NEventInMep << 16) + ((MEPlength[Tel62ID]*4) & 0x0000FFFF);

        // Event Header - Overwritten to update the total Event length - fSubBuffer only //
        fSubBuffer[Tel62ID][0][2] = (EventFlag << 24) + ((EventNumber & 0xFF) << 16) + ((Eventlength[Tel62ID]*4) & 0x0000FFFF);

        nWords++;
        reloopID++;
      }
      else if(nWordsInSlot[Tel62ID][FPGAID][jSlot] == 1){    //the slot is empty: only slotTS word is stored
        nWordsInSlot[Tel62ID][FPGAID][jSlot]++;	
        nNoEmptySlot[Tel62ID][FPGAID]++;

        Int_t FPGAOffset_tmp = 0;
        if(!FPGAID) FPGAOffset_tmp = FPGAOffset;

        if(!fDataFormat) fSubBuffer[Tel62ID][FPGAID][FPGAOffset_tmp] = (FPGAFlag << 24) + (FPGAID << 16) + (nNoEmptySlot[Tel62ID][FPGAID] << 8) + (fNSlots[Tel62ID] & 0x000000FF);
        else fSubBuffer[Tel62ID][FPGAID][FPGAOffset_tmp] = (FPGAFlag << 24) + (FPGAID << 16) + (0xff << 8) + (nNoEmptySlot[Tel62ID][FPGAID] & 0x000000FF); 

        fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = (nWordsInSlot[Tel62ID][FPGAID][jSlot] << 16) + (SlotTS[Tel62ID][FPGAID][jSlot] & 0x0000FFFF);
        fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset] = fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset];
        HitOffset++;

        if((jSlot == LeadingSlotID) && (!LeadingFlag)){
          fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = (OffLeading << 28)+(tdcID << 24)+(FPGAChID << 19)+(raw_time_leading & 0x7FFFF);
          LeadingFlag=true;
        }
        else if((jSlot==TrailingSlotID)&&(!TrailingFlag)){
          fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = (OffTrailing << 28)+(tdcID << 24)+(FPGAChID << 19)+(raw_time_trailing & 0x7FFFF);
          TrailingFlag=true;
        }
        else { debug_cout(1, "WARNINGS: Incorrect option"); }

        MEPlength[Tel62ID]++;
        Eventlength[Tel62ID]++;

        // MEP Header - Overwritten to update the total MEP length - fSubBuffer only //
        fSubBuffer[Tel62ID][0][1] = (Tel62ID << 24) + (NEventInMep << 16) + ((MEPlength[Tel62ID]*4) & 0x0000FFFF);	

        // Event Header - Overwritten to update the total Event length - fSubBuffer only //
        fSubBuffer[Tel62ID][0][2] = (EventFlag << 24) + ((EventNumber & 0xFF) << 16) + ((Eventlength[Tel62ID]*4) & 0x0000FFFF);

        nWords++;
        reloopID++;
      }
      else {
        debug2_cout(2, "WordsInSlot[" << Tel62ID << "][" << FPGAID << "][" << jSlot << "] = " << nWordsInSlot[Tel62ID][FPGAID][jSlot] << std::endl);
        debug_cout(2, "WARNINGS: Number of Words in Slot cannot be zero - At least SlotTS word - Index jSlot out of range !");
        break;     
      }
      jSlot++;

      while(fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset-1]){
        fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset] = fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset-1];
        fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset-1] = fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset-1];
        reloopID++;
        jSlot++;
      }
      fSubBuffer_tmp[Tel62ID][FPGAID][reloopID+HitOffset-1] = fSubBuffer[Tel62ID][FPGAID][reloopID+HitOffset-1];

      HitOffset=0;
      if(!FPGAID) reloopID = FPGAOffset+1;
      else reloopID=1;
      jSlot=0;
    }
    reloopID=0;
    LeadingFlag=false;
    TrailingFlag=false;
  }

  Int_t WordCounter = 1;
  Int_t nHeaderWords = 0;
  // Encoding event requested by Pcfarm binary input file for L1 testing procedure //
  for(Int_t iTel62=0;iTel62<fNROBoards;++iTel62){
    for(Int_t iFPGA=0;iFPGA<fNFPGAs[iTel62];++iFPGA){
      if(!iFPGA){
        WordCounter = Tel62Offset;
        nHeaderWords = 2; //1 BoardHeader & 1 FPGAHeader
      }
      else{
        WordCounter = 0;
        nHeaderWords = 1; //1 FPGAHeader
      }
      for (Int_t iHeaderWord=0; iHeaderWord<nHeaderWords; iHeaderWord++){
        if(((WordCounter+iHeaderWord) == FPGAOffset) && (((fSubBuffer[iTel62][iFPGA][WordCounter+iHeaderWord] & 0x0000FF00) >> 8) != 0xFF))
          printf("Inconsistency in FPGA Header: Reserved bits should be 0xff !!!");

        if(fSubBuffer[iTel62][iFPGA][WordCounter+iHeaderWord]){
          fwrite(&fSubBuffer[iTel62][iFPGA][WordCounter+iHeaderWord], sizeof(uint32_t), 1, fBinaryFile);
          fread(&fSubBuffer[iTel62][iFPGA][WordCounter+iHeaderWord], sizeof(uint32_t), 1, fBinaryFile);
          //printf("%d, %08x\n",WordCounter, fSubBuffer[iTel62][iFPGA][WordCounter]);
          PcfarmEventlength[iTel62]++; // Event length requested by Pcfarm input files for L1 testing procedure
          Pcfarm_nWords++;             // Total number of words requested by Pcfarm input files for L1 testing procedure
        }
      }
      if(!nNoEmptySlot[iTel62][iFPGA] && fDataFormat==1) continue;
      for(Int_t iSlot=0; iSlot<fNSlots[iTel62]; iSlot++){
        if(nWordsInSlot[iTel62][iFPGA][iSlot] == 1 && fDataFormat==1) continue;
        if(nWordsInSlot[iTel62][iFPGA][iSlot] > 1){
          for (Int_t iWrdInSlot=0; iWrdInSlot<nWordsInSlot[iTel62][iFPGA][iSlot]; iWrdInSlot++){
            if(fSubBuffer[iTel62][iFPGA][WordCounter+nHeaderWords+iSlot+iWrdInSlot]){
              fwrite(&fSubBuffer[iTel62][iFPGA][WordCounter+nHeaderWords+iSlot+iWrdInSlot], sizeof(uint32_t), 1, fBinaryFile);
              fread(&fSubBuffer[iTel62][iFPGA][WordCounter+nHeaderWords+iSlot+iWrdInSlot], sizeof(uint32_t), 1, fBinaryFile);
              //printf("%d, %08x\n",WordCounter+iSlot+iWrdInSlot,fSubBuffer[iTel62][iFPGA][WordCounter+iSlot+iWrdInSlot]);
              PcfarmEventlength[iTel62]++; // Event length requested by Pcfarm input files for L1 testing procedure
              Pcfarm_nWords++;             // Total number of words requested by Pcfarm input files for L1 testing procedure
            }
          }
        }
        else printf("Inconsistency: nWordsInSlot cannot be zero !!!");
        WordCounter += nWordsInSlot[iTel62][iFPGA][iSlot] - 1;
      }
    }
  }

  // Encoding event info requested by Pcfarm header input file for L1 testing procedure //
  fprintf(fHeaderFile,"%d,",Pcfarm_nWords);
  fprintf(fHeaderFile,"0x%lx,",fTimeStamp);
  fprintf(fHeaderFile,"%lu:",fFineTime);
  for(Int_t iTel62=0;iTel62<fNROBoards;++iTel62){
    if(iTel62==fNROBoards-1) fprintf(fHeaderFile,"%d\n",PcfarmEventlength[iTel62]);
    else fprintf(fHeaderFile,"%d,",PcfarmEventlength[iTel62]);
  }


  if(fNHitsOutOfSlot){
    cerr_en(wrn,WARN_DET) << "[TDCBRawEncoder]     WARNING: Hit out of slot !    Event: " << tEvent->GetID() << " SubDet: " << fReco->GetName() << " TS: " << std::hex << fTimeStamp << " - Number of Hits out of Slot " << std::dec << fNHitsOutOfSlot << std::endl;
  }
  if(fNLeadingsOnly){
    cerr_en(wrn,WARN_DET) << "[TDCBRawEncoder]     WARNING: Trailing Missing !    Event: " << tEvent->GetID() << " SubDet: " << fReco->GetName() << " - Number of Hits with only Leading Detected " << fNLeadingsOnly << std::endl;
  }
  if(fNTrailingsOnly){
    cerr_en(wrn,WARN_DET) << "[TDCBRawEncoder]     WARNING: Leading Missing !    Event: " << tEvent->GetID() << " SubDet: " << fReco->GetName() << " - Number of Hits with only Trailing Detected " << fNTrailingsOnly << std::endl;
  }

  return fBinaryEvent;
}

// End of Processing

void TDCBRawEncoder::End(){
  Close();
}
