#include "NA62Reconstruction.hh"
#include "TDCBRawDecoder.hh"
#include "NA62Buffer.hh"
#include "NA62BufferProto.hh"
#include "TDCBBuffer.hh"
#include "TDCBBufferProto.hh"
#include <stdlib.h>
#include <byteswap.h>

#include "Riostream.h"

#include "TDCEvent.hh"
#include "TDCError.hh"
#include "TCedarDigi.hh"
#include "TCHANTIDigi.hh"
#include "TCHODDigi.hh"
#include "TIRCDigi.hh"
#include "TSACDigi.hh"
#include "TMUV0Digi.hh"
#include "THACDigi.hh"
#include "TLAVDigi.hh"
#include "TMUV3Digi.hh"
#include "TNewCHODDigi.hh"
#include "TRICHDigi.hh"
#include "TCedarSpecialTriggerEvent.hh"
#include "TTDCBSpecialTrigger.hh"
#include "TRegexp.h"
#include "NA62ConditionsService.hh"

#define NMAXDBGWORDS 100

////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////

TDCBRawDecoder::TDCBRawDecoder(NA62VReconstruction * Reco) : NA62VRawDecoder(Reco, "TDCB", 4), fNFPGAs(0), fHTEL62Errors(0){
  if(fReco->GetName()=="Cedar"){
    fDigiEvent = new TDCEvent(TCedarDigi::Class());
    fSpecialTriggerEvent = new TCedarSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="CHANTI"){
    fDigiEvent = new TDCEvent(TCHANTIDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="CHOD"){
    fDigiEvent = new TDCEvent(TCHODDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="HAC"){
    fDigiEvent = new TDCEvent(THACDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="IRC"){
    fDigiEvent = new TDCEvent(TIRCDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="LAV"){
    fDigiEvent = new TDCEvent(TLAVDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="MUV0"){
    fDigiEvent = new TDCEvent(TMUV0Digi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="MUV1"){
    fDigiEvent = new TDCEvent(TVDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="MUV3"){
    fDigiEvent = new TDCEvent(TMUV3Digi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="NewCHOD"){
    fDigiEvent = new TDCEvent(TNewCHODDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="RICH"){
    fDigiEvent = new TDCEvent(TRICHDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else if(fReco->GetName()=="SAC"){
    fDigiEvent = new TDCEvent(TSACDigi::Class());
    fSpecialTriggerEvent = new TSpecialTriggerEvent(TTDCBSpecialTrigger::Class());
  }
  else NA62VReconstruction::Exception("TDCBRawDecoder: requested subdetector '"+fReco->GetName()+"' not available");

  //----------- Init common parameters and instances ----------//

  NA62VRawDecoder::CreateObjects();
  ParseRawDecoderSettingsFile(fReco->GetRawDecoderSettingsFileName()); //TDCB-specific parameters

  //-----------------------------------------------------------//

  fDatumBuffer = new ULong_t*[128]; //for debug
  fSlotBuffer = new Int_t*[128]; //for debug
  fNRepeatedWords = new UInt_t[128];  //for debug
  fNWordsPerChannel = new UInt_t[128]; //for debug
  for(UInt_t iCh=0;iCh<128;iCh++) {
    fDatumBuffer[iCh] = new ULong_t[NMAXDBGWORDS];
    fSlotBuffer[iCh] = new Int_t[NMAXDBGWORDS];
    for(UInt_t iWord=0;iWord<NMAXDBGWORDS;iWord++) {
      fDatumBuffer[iCh][iWord] = 0;
      fSlotBuffer[iCh][iWord] = 0;
    }
    fNRepeatedWords[iCh] = 0;
    fNWordsPerChannel[iCh] = 0;
  }

  // TEL62ErrString - set bin names
  // Initialise all the fTEL62ErrString/Type strings as "Reserved"/-1
  fTEL62ErrString.clear(); 
  fTEL62ErrType.clear(); 
  fTEL62ErrString.resize(4);
  fTEL62ErrType.resize(4);
  for(UInt_t iErrorWord=0;iErrorWord<4;iErrorWord++){
    fTEL62ErrString[iErrorWord].resize(30,"Reserved");
    fTEL62ErrType[iErrorWord].resize(30,-1);
  }

  // tdc error flags (ErrorWordID==0 and 1)
  for(UInt_t iWord=0;iWord<2;iWord++){
    for(UInt_t iErrorBit=0;iErrorBit<30;iErrorBit++){ //0-14 tdc_i,15-29 tdc_j
      if(iErrorBit==14 || iErrorBit==29) fTEL62ErrString[iWord][iErrorBit] = Form("TDC%d Fatal",2*iWord+iErrorBit/15); ///< Fatal error in TDC_i
      else fTEL62ErrString[iWord][iErrorBit] = Form("TDC%d Error",2*iWord+iErrorBit/15); ///< Other error in TDC_i
    }
  }
  // common error flags (ErrorWordID==2)
  for(UInt_t iTDC=0; iTDC<4;iTDC++){
    fTEL62ErrString[2][4*iTDC+0] = Form("TDC%d frame error",iTDC);  ///< Error in TDC_i flow
    fTEL62ErrString[2][4*iTDC+1] = Form("IB%d repeated word",iTDC); ///< Repeated word in IB_i
    fTEL62ErrString[2][4*iTDC+2] = Form("IB%d frame count",iTDC);   ///< IB_i frame count error
    fTEL62ErrString[2][4*iTDC+3] = Form("IB%d bad word",iTDC);      ///< IB_i bad word (bitflip)
  }
  fTEL62ErrString[2][16] = "OB frame structure";     ///< OB frame structure error (inconsistent frame, missing frame, etc) in ANY event
  fTEL62ErrString[2][17] = "OB repeated word";       ///< Repeated word in OB
  fTEL62ErrString[2][18] = "OB hit/frame mismatch";  ///< OB mismatch between hit time and frame TS
  fTEL62ErrString[2][19] = "OB bad word";            ///< OB unrecognised word error
  fTEL62ErrString[2][20] = "Organizer overflow";     ///< Data organizer overflow (128 or more words in a 25ns timeslot)
  fTEL62ErrString[2][21] = "Organizer frame";        ///< Data organizer frame error (missing frame, wrong TDC ID in error word)
  fTEL62ErrString[2][22] = "Compressor overflow";    ///< Data compressor overflow (more than 4096 words in a 6.4us frame)
  fTEL62ErrString[2][23] = "Hit/Slot Time mismatch"; ///< Mismatch between hit time and timeslot time
  fTEL62ErrString[2][24] = "Internal compressor";    ///< Internal compressor error (frame processing order, unrecognised word)
  fTEL62ErrString[2][25] = "DDR writer request";     ///< DDR writer illegal write request (zero word or unexpected empty frame)
  fTEL62ErrString[2][26] = "DDR writer timeout";     ///< DDR writer timeout
  fTEL62ErrString[2][27] = "TDCB global error";      ///< TDCB global error flag (any error occurred within the TDCB during the burst)
  fTEL62ErrString[2][28] = "Frame TS/Timeout";       ///< Frame timestamp or timeout error from any IB
  // data error flags (ErrorWordID==3)
  for(UInt_t iTDC=0; iTDC<4;iTDC++){
    fTEL62ErrString[3][8*(3-iTDC)+0] = Form("TDC%d Limiter ON",iTDC);    ///< Limiter_i active for the current frame
    fTEL62ErrString[3][8*(3-iTDC)+1] = Form("TDC%d Suppressor ON",iTDC); ///< Suppressor_i active for the current frame
  }
  Int_t NTEL62Errors=0; //counter of non-reserved bins
  for(UInt_t iErrorWord=0;iErrorWord<4;iErrorWord++){
    for(UInt_t iBit=0; iBit<30;iBit++){
      if(fTEL62ErrString[iErrorWord][iBit].Contains("Reserved")) continue; //skip reserved bits
      // Initialise all the fTEL62ErrType
      fTEL62ErrType[iErrorWord][iBit] = FindTEL62ErrorType(fTEL62ErrString[iErrorWord][iBit]);
      Bool_t StringFound = false;
      for(UInt_t jErrorWord=0;jErrorWord<=iErrorWord;jErrorWord++){
        for(UInt_t jBit=0;jBit<iBit;jBit++){
          if(!fTEL62ErrString[iErrorWord][iBit].CompareTo(fTEL62ErrString[jErrorWord][jBit])) StringFound=true;
        }
      }
      if(StringFound) continue;
      NTEL62Errors++;
    }
  }

  if(fNROMezzanines){
    fHDecoderErrors = new TH2F("DecoderErrors","Errors from the TDCBDecoder for the "+fReco->GetName(),
        fNROMezzanines,-0.5,fNROMezzanines-0.5,fMapTDCBError.size(), 0.5, (double)fMapTDCBError.size() + 0.5);
    fHDecoderErrors->GetXaxis()->SetTitle("ROMezzanine ID");
    for (auto & err : fMapTDCBError) {
      if((int)err.first<=GetNCriticalErrorTypes()) err.second = Form("#color[2]{%s}",err.second.Data());  // make label red for critical errors
      fHDecoderErrors->GetYaxis()->SetBinLabel((int)err.first, err.second);
    }
    fHTEL62Errors = new TH2F("TEL62Errors","Errors from the TEL62 for the "+fReco->GetName(),
        fNROMezzanines,-0.5,fNROMezzanines-0.5,NTEL62Errors, 0.5, (double)NTEL62Errors+0.5);
    fHTEL62Errors->GetXaxis()->SetTitle("ROMezzanine ID");
    Int_t iBin=1; //bin numbering starts from 1
    for(UInt_t iErrorWord=0;iErrorWord<4;iErrorWord++){
      for(UInt_t iBit=0; iBit<30;iBit++){
        if(fTEL62ErrString[iErrorWord][iBit].Contains("Reserved")) continue; //skip reserved bits
        Bool_t StringFound = false;
        for(UInt_t jErrorWord=0;jErrorWord<=iErrorWord;jErrorWord++){
          for(UInt_t jBit=0;jBit<iBit;jBit++){
            if(!fTEL62ErrString[iErrorWord][iBit].CompareTo(fTEL62ErrString[jErrorWord][jBit])) StringFound=true;
          }
        }
        if(StringFound) continue;
        fHTEL62Errors->GetYaxis()->SetBinLabel(iBin,fTEL62ErrString[iErrorWord][iBit]);
        iBin++;
      }
    }
  }
  else std::cerr << "[TDCBRawDecoder]     WARNING: fNROMezzanines = 0! [SubDet: " << fReco->GetName() << "]" << std::endl;

}

////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////
TDCBRawDecoder::~TDCBRawDecoder(){

  if(fDatumBuffer){
    for(UInt_t iCh=0;iCh<128;iCh++) {
      delete [] fDatumBuffer[iCh];
    }
    delete [] fDatumBuffer;
    fDatumBuffer=0;
  }
  if(fSlotBuffer){
    for(UInt_t iCh=0;iCh<128;iCh++) {
      delete [] fSlotBuffer[iCh];
    }
    delete [] fSlotBuffer;
    fSlotBuffer=0;
  }
  if(fNRepeatedWords){
    delete [] fNRepeatedWords;
    fNRepeatedWords=0;
  }
  if(fNWordsPerChannel){
    delete [] fNWordsPerChannel;
    fNWordsPerChannel=0;
  }
  if(fNFPGAs){
    delete [] fNFPGAs;
    fNFPGAs=0;
  }
  if(fHTEL62Errors){
    delete fHTEL62Errors;
    fHTEL62Errors=0;
  }
}

void TDCBRawDecoder::StartOfBurst() { //called for RawData only
}

void TDCBRawDecoder::EndOfBurst() { //called for RawData only
}

////////////////////////////////////////////////////////////////
// Decoding
////////////////////////////////////////////////////////////////

TDetectorVEvent * TDCBRawDecoder::DecodeNextEvent(UInt_t *pDataBuffer, EventHeader *pEventHeader, UInt_t * NextOffset){

  Bool_t dbg = 0;
  Bool_t FlagSpecialTrigger=kFALSE;
  TDCEvent *TdcEvent = static_cast<TDCEvent*>( fDigiEvent);
  ULong_t FirstWrongSlotTS=0;           //for debug
  Int_t FirstWrongSlotID=0;             //for debug
  ULong_t PreviousDatum=0;              //for debug
  TString CurrentFileName = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetCurrentFileName(); //for debug

  // Event Header
  cout_en(dbg,1) << "Decode TDCB Buffer ---- start -------- " << std::endl;
  cout_en(dbg,1) << "Detector: " << fReco->GetName() << " EventNumber " << pEventHeader->GetEventNumber() << ", EventLength " << std::hex << pEventHeader->GetEventLength() << ", NextOffset = " << NextOffset << ", TimeStamp " << pEventHeader->GetTimeStamp() << std::dec;
  cout_en(dbg,1) << ", BurstID " << pEventHeader->GetBurstID() << ", NumOfDetectors " << pEventHeader->GetNumOfDetectors() << ", FineTime " << pEventHeader->GetFineTime();
  cout_en(dbg,1) << ", TriggerType " << std::hex<< pEventHeader->GetTriggerType() << std::dec << "\n" << std::endl;
  cout_en(dbg,1) << "Decode TDCB Buffer ---- start -------- " << std::endl;

  TdcEvent->SetTimeStamp(pEventHeader->GetTimeStamp());
  TdcEvent->SetStartByte(pEventHeader->GetStartByte());
  TdcEvent->SetIsMC(kFALSE);
  TdcEvent->SetID(pEventHeader->GetEventNumber());
  TdcEvent->SetBurstID(pEventHeader->GetBurstID());
  TdcEvent->SetRunID(pEventHeader->GetRunID());
  TdcEvent->SetTriggerType(pEventHeader->GetTriggerType());
  if(isL0SpecialFrame(pEventHeader->GetTriggerType()<<2)) {
    FlagSpecialTrigger=kTRUE;
    fSpecialTriggerEvent->SetTimeStamp(pEventHeader->GetTimeStamp());
    fSpecialTriggerEvent->SetStartByte(pEventHeader->GetStartByte());
    fSpecialTriggerEvent->SetIsMC(kFALSE);
    fSpecialTriggerEvent->SetID(pEventHeader->GetEventNumber());
    fSpecialTriggerEvent->SetBurstID(pEventHeader->GetBurstID());
    fSpecialTriggerEvent->SetRunID(pEventHeader->GetRunID());
    fSpecialTriggerEvent->SetTriggerType(pEventHeader->GetTriggerType()<<2);
  }

  UInt_t Detector = pEventHeader->GetDetectorID();
  cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Detector ID " <<std::hex<< Detector << " Start Byte " << pEventHeader->GetStartByte() << std::endl;
  cout_en(dbg,1) <<"pDataBuffer: " << pDataBuffer << " NextOffset: " << NextOffset <<std::dec<< " NWords: "<< NextOffset - pDataBuffer<<std::endl;

  UInt_t datum;
  for (UInt_t iFPGA=0;iFPGA<5;iFPGA++) fFPGAFlags[iFPGA]=kFALSE;
  while(pDataBuffer < NextOffset){ // LOOP ON ALL SUBDETECTOR BLOCKS
    UInt_t NumberOfBlockDataRead=0;
    // Subdetector Block Header (generic part)
    UInt_t DataBlockByteSize = DataBlockSize(*pDataBuffer);
    UInt_t TEL62Board        = DetectorSubType(*pDataBuffer);
    UInt_t DataBlockFormatID = DataBlockFormat(*pDataBuffer);
    Long_t DataBlockTS = 0;
    cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TEL62 board: " << TEL62Board << " Data block size in bytes: " << DataBlockByteSize << " Data format: " << DataBlockFormatID << std::endl;
    if(TEL62Board>=(UInt_t)fNROBoards){
      TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_TEL62_WRONG_ID));
      TdcError->SetROBoardID(TEL62Board);
      TdcError->SetFatal(kTRUE);
      for(int iFPGA=0;iFPGA<fNROMezzaninesPerFullBoard;iFPGA++){
        fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA, (int)TDCBDecoderErr::TDCB_TEL62_WRONG_ID); // entry in error histogram
      }
      fNCriticalErrors[0]++;
      cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Wrong TEL62 ID! [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
      cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " datum: " << std::hex << datum << std::dec << "]" << std::endl;
      if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
      return TdcEvent; //stop decoding of catastrophic event
    }
    if(DataBlockFormatID==1) { // 2015 format: read the 32-bit timestamp
      pDataBuffer++;
      NumberOfBlockDataRead++;
      datum=*pDataBuffer;
      DataBlockTS = DataBlockTimeStamp(datum); //temporarily not used, just read
      cout_en(dbg,1) << "DataBlockTS: " << std::hex << DataBlockTS << std::dec << std::endl;
      if((ULong_t)DataBlockTS!=pEventHeader->GetTimeStamp() && !isL0SpecialFrame(pEventHeader->GetTriggerType()<<2)){ //Special Triggers have 0 latency
        TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_BLOCKTS_MISMATCH));
        TdcError->SetROBoardID(TEL62Board);
        TdcError->SetFatal(kTRUE);
        for(int iFPGA=0;iFPGA<fNROMezzaninesPerFullBoard;iFPGA++){
          fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA,(int)TDCBDecoderErr::TDCB_BLOCKTS_MISMATCH); // entry in error histogram
          if(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA<(UInt_t)fNROMezzanines) fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+iFPGA]++;
          else fNCriticalErrors[0]++;
        }
        cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: BlockTS/TriggerTS mismatch!     [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
        cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " BlockTS-TriggerTS: " << DataBlockTS-(Long_t)pEventHeader->GetTimeStamp()<< "]" << std::endl;
      }
    }
    pDataBuffer++;
    NumberOfBlockDataRead++;

    if(DataBlockByteSize<=NumberOfBlockDataRead*4) { // empty block
      if(isL0EOB(pEventHeader->GetTriggerType()<<2)){ //fake EOB
        TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_FAKE_EOB));
        TdcError->SetROBoardID(TEL62Board);
        for(int iFPGA=0;iFPGA<fNROMezzaninesPerFullBoard;iFPGA++){
          fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA,(int)TDCBDecoderErr::TDCB_FAKE_EOB);
        }
        cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Fake EOB! [EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << "]" << std::endl;
      }
      continue;
    }
    // Subdetector Block Header (TEL62-specific part)
    datum=*pDataBuffer;
    cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Datum: " <<std::hex<< datum <<std::dec<< std::endl;
    UInt_t Format = TDCBFormatIdentifier(datum);
    UInt_t l0TriggerType = TDCBL0TriggerType(datum);
    UInt_t TEL62Header = TDCBDetectorSubType(datum);
    if (TEL62Board != TEL62Header){
      TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_TEL62_WRONG_ID));
      TdcError->SetROBoardID(TEL62Board);
      TdcError->SetFatal(kTRUE);
      for(int iFPGA=0;iFPGA<fNROMezzaninesPerFullBoard;iFPGA++){
        fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA, (int)TDCBDecoderErr::TDCB_TEL62_WRONG_ID); // entry in error histogram
        fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Header+iFPGA,(int)TDCBDecoderErr::TDCB_TEL62_WRONG_ID); // entry in error histogram
        if(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA<(UInt_t)fNROMezzanines)  fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+iFPGA]++;
        else if(fNROMezzaninesPerFullBoard*TEL62Header+iFPGA<(UInt_t)fNROMezzanines) fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Header+iFPGA]++;
        else fNCriticalErrors[0]++;
      }
      cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: TEL62 boards are not matching [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
      cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " datum: " << std::hex << datum << std::dec << "]" << std::endl;
      if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
      return TdcEvent; //stop decoding of catastrophic event
    }
    if(l0TriggerType!= (pEventHeader->GetTriggerType()&0xff)<<2){
      TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_TRIG_TYPE_MISMATCH));
      TdcError->SetROBoardID(TEL62Board);
      TdcError->SetFatal(kTRUE);
      for(int iFPGA=0;iFPGA<fNROMezzaninesPerFullBoard;iFPGA++){
        fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA, (int)TDCBDecoderErr::TDCB_TRIG_TYPE_MISMATCH); // entry in error histogram
        if(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA<(UInt_t)fNROMezzanines)  fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+iFPGA]++;
        else fNCriticalErrors[0]++;
      }
      cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Trigger types are not matching [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
      cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " L0TriggerType: " << std::hex << l0TriggerType << " Expected: " << ((pEventHeader->GetTriggerType()&0xff)<<2) << std::dec << "]" << std::endl;
      if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
      return TdcEvent; //stop decoding of catastrophic event
    }
    cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Format identifier: " << Format << ", TEL62 board: " << TEL62Header << " , L0 trigger type: " << l0TriggerType << std::endl;
    ULong_t FPGAMask = FPGAIDBoardHeader(datum);
    UInt_t NFPGA=0;
    if (FlagPPFPGA0(FPGAMask)){
      fFPGAFlags[0]=kTRUE;
      NFPGA++;
    }
    if (FlagPPFPGA1(FPGAMask)){
      fFPGAFlags[1]=kTRUE;
      NFPGA++;
    }
    if (FlagPPFPGA2(FPGAMask)){
      fFPGAFlags[2]=kTRUE;
      NFPGA++;
    }
    if (FlagPPFPGA3(FPGAMask)){
      fFPGAFlags[3]=kTRUE;
      NFPGA++;
    }
    if (FlagSLFPGA(FPGAMask)){
      fFPGAFlags[4]=kTRUE;
      NFPGA++;
    }

    TdcEvent->SetL0TriggerType(l0TriggerType);

    cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " FPGAID board header: " << FPGAMask << " Number of FPGA: " << NFPGA << std::endl;
    for (UInt_t iFPGA=0;iFPGA<NFPGA;iFPGA++) {
      if (!isL0SpecialFrame(l0TriggerType)){ // FPGA data block for physics/calibration/random triggers

        pDataBuffer++;
        NumberOfBlockDataRead++;
        datum=*pDataBuffer;

        Bool_t ErrorFlag = TDCBFlagError(datum);
        UInt_t FPGAid = FPGAID(datum);
        UInt_t nSlots = TDCBNSlots(datum);
        if(FPGAid>=(UInt_t)fNROMezzaninesPerFullBoard || fNROMezzaninesPerFullBoard*TEL62Board+FPGAid>=(UInt_t)fNROMezzanines) {
          for(int iFPGA=0;iFPGA<fNROMezzaninesPerFullBoard;iFPGA++){
            fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA,(int)TDCBDecoderErr::TDCB_FPGA_WRONG_ID);
          }
          TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_FPGA_WRONG_ID));
          TdcError->SetROBoardID(TEL62Board);
          TdcError->SetFatal(kTRUE);
          fNCriticalErrors[0]++;
          cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Wrong FPGA ID [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " FPGAID: " << FPGAid << " MezzanineID: " << fNROMezzaninesPerFullBoard*TEL62Board+FPGAid << " fNROMezzanines: " << fNROMezzanines << std::endl;
          if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
          return TdcEvent; //stop decoding of catastrophic event
        }
        if(!(fROMezzanineMasksPerBoard[TEL62Board]&(1<<FPGAid)) && !fIsAGuestOrHostDetector) { // Ignore guest/host detectors [the check is not suitable for them]
          fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_MASKED_CH);
          cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Current FPGA is masked! [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << "]" << std::endl;
          fNHitsFromMaskedChannels[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
        }
        fNTotalSlots[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid] += nSlots; //for debug
        cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Error flag: " << ErrorFlag << ", FPGAID: " << FPGAid << ", Number of slots: " << nSlots << " [tot: " << fNTotalSlots[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]<< "]" << std::endl;

        for (UInt_t iFPGA=0; iFPGA<5; iFPGA++) if (iFPGA==FPGAid) fFPGAFlags[iFPGA]=kFALSE;
        for(int iCh=0;iCh<128;iCh++) {
          for(UInt_t iWord=0;iWord<NMAXDBGWORDS;iWord++) {
            fDatumBuffer[iCh][iWord] = 0;
            fSlotBuffer[iCh][iWord] = 0;
          }
          fNRepeatedWords[iCh] = 0;
          fNWordsPerChannel[iCh] = 0;
        }

        // Check the slot range (for debug)
        Int_t StationID=0;
        Int_t BoardCounter=0;
        while(StationID<(Int_t)fNROBoardsPerStation.size()-1 && TEL62Board>=(UInt_t)fNROBoardsPerStation[StationID]+BoardCounter){
          BoardCounter+=fNROBoardsPerStation[StationID];
          StationID++;
        }
        Int_t ExpectedLastSlotID  = (Int_t)((fReco->GetStationT0(StationID) + fROMezzaninesT0[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid])/ClockPeriod)+fLastSlotID[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid];
        Int_t ExpectedFirstSlotID = ExpectedLastSlotID-(fNSlots[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]-1);
        const Int_t DeltaSlot = 1; // Check the slots are with the expected range +/- 1

        // Slot
        for (UInt_t iSlot=0;iSlot<nSlots;iSlot++){
          pDataBuffer++;
          NumberOfBlockDataRead++;
          datum=*pDataBuffer;
          Int_t nSlotWords=TDCBNSlotWords(datum);
          if(DataBlockByteSize<=(NumberOfBlockDataRead+nSlotWords-1)*4) { // wrong data size
            TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_BAD_DATA_SIZE));
            TdcError->SetROBoardID(TEL62Board);
            TdcError->SetFatal(kTRUE);
            fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_BAD_DATA_SIZE);
            fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
            cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Bad number of data read! " << (NumberOfBlockDataRead+nSlotWords-1)*4 << "/" << DataBlockByteSize << " [File: " << CurrentFileName;
            cerr_en(fWarningsLevel,WARN_DET) << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << "]" << std::endl;
            if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
            return TdcEvent; //stop decoding of catastrophic event
          }
          if(!(fROMezzanineMasksPerBoard[TEL62Board]&(1<<FPGAid))) { //skip masked mezzanine
            pDataBuffer+=nSlotWords-1;
            NumberOfBlockDataRead+=nSlotWords-1;
            continue;
          }
          ULong_t SlotTS = TDCBSlotTimeStamp(datum); //Lower TS bits
          SlotTS += pEventHeader->GetTimeStamp() & 0xFFFF0000;
          if((pEventHeader->GetTimeStamp()&0xf000)==0xf000 && (SlotTS&0xf000)==0x0000) SlotTS += 0x10000; //16 bits overflow
          if((pEventHeader->GetTimeStamp()&0xf000)==0x0000 && (SlotTS&0xf000)==0xf000) SlotTS -= 0x10000; //16 bits overflow
          Int_t SlotID = (Int_t) (SlotTS-pEventHeader->GetTimeStamp());
          if(SlotID<ExpectedFirstSlotID-DeltaSlot || SlotID>ExpectedLastSlotID+DeltaSlot) { // Current slot is out of the expected range 
            cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Inconsistent slot [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
            cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " SlotID: " << SlotID << " ExpectedRange: [" << ExpectedFirstSlotID << "," << ExpectedLastSlotID << "] TEL62: " << TEL62Board << " FPGA: " << FPGAid << "]" << std::endl;
            if(!fNWrongSlots[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]) {
              FirstWrongSlotTS = SlotTS;
              FirstWrongSlotID = SlotID;
            }
            fNWrongSlots[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
          }
          cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " SlotID: " << SlotID << ", nSlotWords: " << nSlotWords;
          cout_en(dbg,1) << ", SlotTS: " << std::hex << SlotTS << ", TriggerTS: " << pEventHeader->GetTimeStamp() << std::dec << std::endl;
          if (nSlotWords<1) cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Slot " << SlotID << " empty" << std::endl;
          // TDC data
          for (Int_t iData=1;iData<nSlotWords;iData++) {
            pDataBuffer++;
            NumberOfBlockDataRead++;
            datum=*pDataBuffer;
            cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TDC datum " <<std::hex<< datum <<std::dec<< std::endl;
            if(isTDCBError(datum)){
              TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_TDCB_ERR));
              TdcError->SetROBoardID(TEL62Board);
              TdcError->SetROMezzanineID(FPGAid);
              TdcError->SetTDCID(TDCID(datum));
              fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_TDCB_ERR);
              cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: TDC Error Word!                 [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
              cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << " datum: " << std::hex << datum << std::dec << "]" << std::endl;
              cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Error word! " <<std::hex<< datum <<std::dec<< std::endl;
              continue;
            }
            else if(isTDCBLeading(datum) || isTDCBTrailing(datum)){
              ULong_t nValue = TDCBChannelValue(datum);
              nValue += (SlotTS & 0xFFFFF800)*0x100;
              UInt_t nROChannel = TDCBChannelNumber(datum);
              nROChannel += 512*TEL62Board+128*FPGAid;
              Int_t nChannel = GetChannelRemap(nROChannel);

              if (nChannel==-1 && !fIsAGuestOrHostDetector) { // Ignore guest/host detectors [the check is not suitable for them]
                fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_MASKED_CH);
                cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Hit from masked channel!        [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName();
                cerr_en(fWarningsLevel,WARN_DET) << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << std::hex << " datum: " << datum << std::dec << " Channel: " << nROChannel << "]" << std::endl;
                fNHitsFromMaskedChannels[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
              }
              if(nChannel==-1) continue;

              bool RepeatedWord=false;
              Int_t FirstOccurrenceSlot = 0;
              UInt_t NWordsPerChannel = fNWordsPerChannel[nROChannel%128];
              if(NWordsPerChannel>=NMAXDBGWORDS) {
                NWordsPerChannel=NMAXDBGWORDS-1;
                cerr_en(fWarningsLevel,WARN_MAX) << "[TDCBRawDecoder]     WARNING: fNWordsPerChannel["<<(nROChannel%128)<<"]=="<<fNWordsPerChannel[nROChannel%128]<< ". Truncating to " << NMAXDBGWORDS;
                cerr_en(fWarningsLevel,WARN_MAX) << " [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << "]" << std::endl;
              }
              for(UInt_t iWord=0;iWord<NWordsPerChannel;iWord++){
                if(fDatumBuffer[nROChannel%128][iWord]==datum) {
                  FirstOccurrenceSlot = fSlotBuffer[nROChannel%128][iWord];
                  RepeatedWord=true;
                  break;
                }
              }
              if(RepeatedWord){ //repeated word
                TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_REPEATED_WORD));
                TdcError->SetROBoardID(TEL62Board);
                TdcError->SetROMezzanineID(FPGAid);
                fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_REPEATED_WORD);
                fNRepeatedWords[nROChannel%128]++;
                TString th_str = "th";
                if(fNRepeatedWords[nROChannel%128]==1)      th_str = "st";
                else if(fNRepeatedWords[nROChannel%128]==2) th_str = "nd";
                else if(fNRepeatedWords[nROChannel%128]==3) th_str = "rd";
                cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Word repeated for the " << fNRepeatedWords[nROChannel%128] << th_str << " time! [File: " << CurrentFileName;
                cerr_en(fWarningsLevel,WARN_DET) << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid <<std::hex<< " datum: " << datum << " PreviousDatum: " << PreviousDatum;
                cerr_en(fWarningsLevel,WARN_DET) << std::dec << " Channel: " << nROChannel << " Time: " << nValue << " CurrentSlot: " << SlotID << " First seen in Slot: " << FirstOccurrenceSlot << "]" << std::endl;
              }
              else if(NWordsPerChannel>0) {
                ULong_t PreviousnValueInCh = TDCBChannelValue(fDatumBuffer[nROChannel%128][NWordsPerChannel-1]);
                PreviousnValueInCh += ((fSlotBuffer[nROChannel%128][NWordsPerChannel-1]+pEventHeader->GetTimeStamp()) & 0xFFFFF800)*0x100;
                if(PreviousnValueInCh>nValue){ //hits from the same channel are not time-ordered!
                  TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_NOT_TIME_ORDER));
                  TdcError->SetROBoardID(TEL62Board);
                  TdcError->SetROMezzanineID(FPGAid);
                  fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_NOT_TIME_ORDER);
                  cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Hits are not time-ordered!      [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName();
                  cerr_en(fWarningsLevel,WARN_DET) << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << std::hex << " datum: " << datum  << " PreviousDatum: " << PreviousDatum << " PreviousDatumInCh: " << fDatumBuffer[nROChannel%128][NWordsPerChannel-1];
                  cerr_en(fWarningsLevel,WARN_DET) << std::dec << " Channel: " << nROChannel << " PreviousTimeInCh: " << PreviousnValueInCh << " Time: " << nValue << "]" << std::endl;
                }
              }
              if(nROChannel/128!=FPGAid+4*TEL62Board){
                TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_TDC_WRONG_ID));
                TdcError->SetROBoardID(TEL62Board);
                TdcError->SetROMezzanineID(FPGAid);
                TdcError->SetFatal(kTRUE);
                fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_TDC_WRONG_ID);
                fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
                cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: TDC Channel not consistent with FPGA ID!  [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
                cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << std::hex << " datum: " << datum << " PreviousDatum: " << PreviousDatum;
                cerr_en(fWarningsLevel,WARN_DET) << std::dec << " Channel: " << nROChannel << "]" << std::endl;
              }
              ULong_t HitTS = nValue/0x100;
              Double_t Time = (nValue - pEventHeader->GetTimeStamp()*256.)*TdcCalib;
              if(!RepeatedWord && HitTS < SlotTS){
                TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_LEFT_SLOT_OVERFLOW));
                TdcError->SetROBoardID(TEL62Board);
                TdcError->SetROMezzanineID(FPGAid);
                TdcError->SetFatal(kTRUE);
                fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_LEFT_SLOT_OVERFLOW);
                fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
                cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Left-hand slot overflow!       [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName();
                cerr_en(fWarningsLevel,WARN_DET) << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << std::hex << " datum: " << datum << " PreviousDatum: " << PreviousDatum << " SlotTS: " << SlotTS << " HitTS: " << HitTS;
                cerr_en(fWarningsLevel,WARN_DET) << std::dec << " SlotShift: " << ((Int_t)(HitTS-SlotTS)) << "]" << std::endl;
              }
              else if(!RepeatedWord && HitTS > SlotTS){
                TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_RIGHT_SLOT_OVERFLOW));
                TdcError->SetROBoardID(TEL62Board);
                TdcError->SetROMezzanineID(FPGAid);
                TdcError->SetFatal(kTRUE);
                fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_RIGHT_SLOT_OVERFLOW);
                fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
                cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Right-hand slot overflow!      [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName();
                cerr_en(fWarningsLevel,WARN_DET) << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << std::hex << " datum: " << datum << " PreviousDatum: " << PreviousDatum << " SlotTS: " << SlotTS << " HitTS: " << HitTS;
                cerr_en(fWarningsLevel,WARN_DET) << std::dec << " SlotShift: " << ((Int_t)(HitTS-SlotTS)) << "]" << std::endl;
              }
              fDatumBuffer[nROChannel%128][NWordsPerChannel] = datum;
              fSlotBuffer[nROChannel%128][NWordsPerChannel] = SlotID;
              fNWordsPerChannel[nROChannel%128]++;
              PreviousDatum = datum;

              TDCVHit *LastHit = static_cast<TDCVHit*>( TdcEvent->GetLastHitOnChannel(nChannel));
              TDCVHit *TdcHit = LastHit;
              if(!LastHit || (!isTDCBTrailing(datum) && LastHit->GetDetectedEdge() & 1) || (LastHit->GetDetectedEdge() & 2)){
                TdcHit = static_cast<TDCVHit*>(TdcEvent->AddDigi(nChannel));
                TdcHit->DecodeChannelID();
                TdcHit->SetSlot(SlotID);
                TdcHit->SetSlotTS(SlotTS);
                TdcHit->SetFPGAID(FPGAid);
              }
              cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Channel number " << nChannel << ", Time " << Time << " Nhits " << TdcEvent->GetNHits() << std::endl;
              if(isTDCBLeading(datum)){
                TdcHit->SetLeadingEdge(Time);
                TdcHit->UpdateDetectedEdge(1);
              }
              if(isTDCBTrailing(datum)){
                TdcHit->SetTrailingEdge(Time);
                TdcHit->UpdateDetectedEdge(2);
              }
            }
            else{
              TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_DATA_TYPE));
              TdcError->SetROBoardID(TEL62Board);
              TdcError->SetROMezzanineID(FPGAid);
              TdcError->SetFatal(kTRUE);
              fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_DATA_TYPE);
              fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
              cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Wrong TDCB data type! [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName();
              cerr_en(fWarningsLevel,WARN_DET) << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << " datum: " << std::hex << datum << std::dec <<"]" << std::endl;
              if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
              return TdcEvent; //stop decoding of catastrophic event
            }
          }
        }
        // Decode error frame
        if (ErrorFlag) {
          // -- temporary patch to correct the ErrorFlag bitflip -- //
          if((*(pDataBuffer+1)&0x0000ff00)==0x0000ff00 || (NumberOfBlockDataRead+1)*4 == DataBlockByteSize){
            TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_WRONG_ERRORFLAG));
            TdcError->SetROBoardID(TEL62Board);
            TdcError->SetROMezzanineID(FPGAid);
            fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_WRONG_ERRORFLAG);
            cerr_en(fWarningsLevel,WARN_MAX) << "[TDCBRawDecoder]     WARNING: Wrong error flag [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
            cerr_en(fWarningsLevel,WARN_MAX) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << "]" << std::endl;
            ErrorFlag=0;
            continue;
          }
          // ------------------------------------------------------ //
          pDataBuffer++;
          NumberOfBlockDataRead++;
          datum=*pDataBuffer;
          UInt_t ErrorFrameHeader = datum; //for debug
          UInt_t nErrorWords=TDCBNErrorWords(ErrorFrameHeader);
          UInt_t nErrorWordsPerFrame[2]; //2 error frames
          nErrorWordsPerFrame[0] = TDCBNErrorWordsFrame0(ErrorFrameHeader);
          nErrorWordsPerFrame[1] = TDCBNErrorWordsFrame1(ErrorFrameHeader);
          fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_TEL62_ERROR);
          cerr_en(fWarningsLevel,WARN_MAX) << "[TDCBRawDecoder]     WARNING: TEL62 Error frame [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
          cerr_en(fWarningsLevel,WARN_MAX) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << "]" << std::endl;
          // TEL62 Error frame
          cout_en(dbg,1) << "nErrorWords: " << nErrorWords << " nFrame0ErrW: " << nErrorWordsPerFrame[0] << " nFrame1ErrW: " << nErrorWordsPerFrame[1] << std::endl;
          if(nErrorWords-1!=nErrorWordsPerFrame[0]+nErrorWordsPerFrame[1]){
            TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_WRONG_NERRWORDS));
            TdcError->SetROBoardID(TEL62Board);
            TdcError->SetROMezzanineID(FPGAid);
            TdcError->SetFatal(kTRUE);
            fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,(int)TDCBDecoderErr::TDCB_WRONG_NERRWORDS);
            fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+FPGAid]++;
            cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Inconsistent Number of Error frame words: " << std::hex << datum << std::dec << " nErrW " << nErrorWords << " nErrWFrame0: " << nErrorWordsPerFrame[0] << " nErrWFrame1: " << nErrorWordsPerFrame[1] << std::endl;
            cerr_en(fWarningsLevel,WARN_DET) << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << std::endl;
            if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
            return TdcEvent; //stop decoding of catastrophic event
          }
          for(UInt_t iErrorFrame=0;iErrorFrame<2;iErrorFrame++){
            for(UInt_t iData=0;iData<nErrorWordsPerFrame[iErrorFrame];iData++){
              pDataBuffer++;
              NumberOfBlockDataRead++;
              datum=*pDataBuffer;
              cout_en(dbg,1) << "ErrorFrame"<< iErrorFrame << ", EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << " TDC datum " <<std::hex<< datum <<std::dec<< std::endl;
              cerr_en(fWarningsLevel,WARN_MAX) << "[TDCBRawDecoder]     WARNING: ErrorFrame"<< iErrorFrame << " word : " << std::hex << datum << std::dec << " iData: " << iData << " nErrW " << nErrorWords << " ErrorFrameHeader: " << std::hex << ErrorFrameHeader << std::dec << " [EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << "]" << std::endl;
              UInt_t ErrorWordID = TEL62ErrorWordID(datum);
              for(UInt_t iBit=0;iBit<30;iBit++){
                if(fTEL62ErrType[ErrorWordID][iBit]<0) continue; //skip reserved/disabled bits
                if(datum&(1<<iBit)) {
                  Int_t TDCID=-1;
                  if( fTEL62ErrType[ErrorWordID][iBit]==(int)TEL62DecoderErr::TEL62_TDC_FATAL ||
                      fTEL62ErrType[ErrorWordID][iBit]==(int)TEL62DecoderErr::TEL62_TDC_ERROR ||
                      fTEL62ErrType[ErrorWordID][iBit]==(int)TEL62DecoderErr::TEL62_IB_ERROR ||
                      fTEL62ErrType[ErrorWordID][iBit]==(int)TEL62DecoderErr::TEL62_LIMITER_ON ||
                      fTEL62ErrType[ErrorWordID][iBit]==(int)TEL62DecoderErr::TEL62_SUPPRESSOR_ON){ 
                    TDCID = ((TString)fTEL62ErrString[ErrorWordID][iBit](TRegexp("[0-9]+"))).Atoi();
                  }
                  TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError(fMapTDCBError.size()+fTEL62ErrType[ErrorWordID][iBit]));
                  TdcError->SetROBoardID(TEL62Board);
                  TdcError->SetROMezzanineID(FPGAid);
                  TdcError->SetTDCID(TDCID);
                  fHTEL62Errors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+FPGAid,fTEL62ErrString[ErrorWordID][iBit],1.);
                  cerr_en(fWarningsLevel,WARN_MAX) << "[TDCBRawDecoder]     WARNING: TEL62 Error '" << fTEL62ErrString[ErrorWordID][iBit] << "' [EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << FPGAid << "]" << std::endl;
                }
              }
            }
          }
        }
      }
      else {  // Special triggers
        DecodeSpecialTrigger(pDataBuffer,pEventHeader,NumberOfBlockDataRead,TEL62Board);
      }
    }
    pDataBuffer++;
    NumberOfBlockDataRead++;
    for (int iFPGA=0;iFPGA<5;iFPGA++) {
      if (fFPGAFlags[iFPGA]){
        TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_FPGA_MISSING));
        TdcError->SetROBoardID(TEL62Board);
        TdcError->SetROMezzanineID(iFPGA);
        TdcError->SetFatal(kTRUE);
        fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+iFPGA]++;
        fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA,(int)TDCBDecoderErr::TDCB_FPGA_MISSING);
        cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: FPGA " << iFPGA << " missing!  [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
        cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " FPGA: " << iFPGA << "]" << std::endl;
        if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
        return TdcEvent; //stop decoding of catastrophic event
      }
    }
    if (NumberOfBlockDataRead*4 != DataBlockByteSize){
      TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_BAD_DATA_SIZE));
      TdcError->SetROBoardID(TEL62Board);
      TdcError->SetFatal(kTRUE);
      for(int iFPGA=0;iFPGA<fNROMezzaninesPerFullBoard;iFPGA++){
        fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA,(int)TDCBDecoderErr::TDCB_BAD_DATA_SIZE);
        fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+iFPGA]++;
      }
      cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Bad number of data read! " << NumberOfBlockDataRead*4 << "/" << DataBlockByteSize << " [File: " << CurrentFileName;
      cerr_en(fWarningsLevel,WARN_DET) << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << "]" << std::endl;
      if (FlagSpecialTrigger) return fSpecialTriggerEvent; //stop decoding of catastrophic event
      return TdcEvent; //stop decoding of catastrophic event
    }

    //---------------- end of TEL62 data ----------------//

    datum=*pDataBuffer;
    if(isDIMEOB(datum) && !DataBlockFormatID){ //Sub-detector DIM EOB [2014 data format]
      //UInt_t SubDetID      = DIMEOBSubDetID(datum)/4;
      UInt_t NumberOfWords = DIMEOBNumberOfWords(datum);
      static_cast<NA62Reconstruction*>(fReco->GetMainReco())->StoreDIMBlock(fReco->GetName(),pDataBuffer,NumberOfWords,fSpecialTriggerEvent);
      pDataBuffer+=NumberOfWords;
    }
  }
  cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " End of NextOffset" << std::endl;
  for (Int_t iMezzanine=0; iMezzanine<fNROMezzanines; iMezzanine++) {
    if (fNWrongSlots[iMezzanine]){
      if(fNWrongSlots[iMezzanine] == fNTotalSlots[iMezzanine]) {
        TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_BAD_TRIGGERTS));
        TdcError->SetROBoardID(iMezzanine/fNROMezzaninesPerFullBoard);
        TdcError->SetROMezzanineID(iMezzanine);
        TdcError->SetFatal(kTRUE);
        fHDecoderErrors->Fill(iMezzanine,(int)TDCBDecoderErr::TDCB_BAD_TRIGGERTS);
        cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Wrong TriggerTS!               [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName();
        cerr_en(fWarningsLevel,WARN_DET) << std::hex << " TriggerTS: " << pEventHeader->GetTimeStamp() << " FirstSlotTS: " << FirstWrongSlotTS << " FirstSlotID: " << std::dec << FirstWrongSlotID;
        cerr_en(fWarningsLevel,WARN_DET) << " nWrongSlots/tot: " << fNWrongSlots[iMezzanine] << "/" << fNTotalSlots[iMezzanine] << "]" << std::endl;
      }
      else if(fNWrongSlots[iMezzanine] < fNTotalSlots[iMezzanine]) {
        TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_BAD_SLOT));
        TdcError->SetROBoardID(iMezzanine/fNROMezzaninesPerFullBoard);
        TdcError->SetROMezzanineID(iMezzanine);
        TdcError->SetFatal(kTRUE);
        fHDecoderErrors->Fill(iMezzanine,(int)TDCBDecoderErr::TDCB_BAD_SLOT);
        cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: " << fNWrongSlots[iMezzanine] << "/" << fNTotalSlots[iMezzanine] << " slots are inconsistent with the TriggerTS! [File: " << CurrentFileName;
        cerr_en(fWarningsLevel,WARN_DET) << " EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << iMezzanine/fNROMezzaninesPerFullBoard<<","<<iMezzanine%fNROMezzaninesPerFullBoard <<std::hex<< " TriggerTS: " << pEventHeader->GetTimeStamp();
        cerr_en(fWarningsLevel,WARN_DET) << " FirstWrongSlotTS: " << FirstWrongSlotTS << " FirstWrongSlotID: " << std::dec << FirstWrongSlotID << "]" << std::endl;
      }
    }
  }
  cout_en(dbg,1) << "\n\n****************************************\n* DecodeTDCBBuffer ---- end ---------- *\n****************************************\n" << std::endl;
  if (FlagSpecialTrigger) return fSpecialTriggerEvent;
  return TdcEvent;
}

void TDCBRawDecoder::DecodeSpecialTrigger(UInt_t *&pDataBuffer, EventHeader *pEventHeader, UInt_t &NumberOfBlockDataRead, Int_t TEL62Board){

  Bool_t dbg = 0;
  UInt_t datum;
  TDCEvent *TdcEvent = static_cast<TDCEvent*>( fDigiEvent);
  TTDCBSpecialTrigger *SpecTrig=0;
  pDataBuffer++;
  NumberOfBlockDataRead++;
  datum=*pDataBuffer;
  UInt_t NSpecWords = TDCBNumberOfSpecialWords(datum);
  UInt_t FPGAid = FPGAID(datum);
  UInt_t l0TriggerType = TdcEvent->GetL0TriggerType();
  UInt_t Detector = pEventHeader->GetDetectorID();
  TString CurrentFileName = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetCurrentFileName(); //for debug
  cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SubDet: " << fReco->GetName() << " Special trigger - Number of special words: " << NSpecWords << ", FPGAID: " << FPGAid << std::endl;
  for (UInt_t iFPGA=0;iFPGA<5;iFPGA++) if (iFPGA==FPGAid) fFPGAFlags[iFPGA]=kFALSE;
  SpecTrig = static_cast<TTDCBSpecialTrigger*>( fSpecialTriggerEvent->AddSpecialTrigger());
  SpecTrig->SetDataSourceID(Detector);
  SpecTrig->SetROBoardID(TEL62Board);
  SpecTrig->SetFPGAID(FPGAid);
  SpecTrig->SetTriggerType(l0TriggerType);
  if(isL0SOB(l0TriggerType)){ //read SOB timestamp
    SpecTrig->SetTimeStamp(*(pDataBuffer+1));
    cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " SOB - Size: " << NSpecWords << " TimeStamp: " << SpecTrig->GetTimeStamp() << std::endl;
  }
  else if(isL0EOB(l0TriggerType)){
    FILE *EOBFileDescriptor = pEventHeader->GetEOBFileDescriptor();
    if(EOBFileDescriptor) {
      fprintf(EOBFileDescriptor,
          "%s Burst: %d, Board number %d, FPGA id %i, L0 trigger type 0x%x, Number of words %d\n",
          fReco->GetName().Data(),pEventHeader->GetBurstID(),TEL62Board,FPGAid,l0TriggerType,NSpecWords);
    }

    // ------------- EOB ASCII decoder ------------- //

    const char eol=0x0A;         //EOL character
    Int_t  iEOLChar = 0;         //index of the next EOL character

    // Change endianity: define temporary array to store "swapped" words
    if (NSpecWords <= 0) return; //it should never happen
    UInt_t* swapDataBuffer = new UInt_t[NSpecWords];
    for (UInt_t iEOBWord=0; iEOBWord<NSpecWords; iEOBWord++) swapDataBuffer[iEOBWord] = *(pDataBuffer+iEOBWord);

    UInt_t *tmp = swapDataBuffer;
    for (UInt_t iEOBWord=1;iEOBWord<NSpecWords;iEOBWord++){ //EOB header not to be swapped
      UInt_t swapped = bswap_32(*(tmp+iEOBWord));
      *(tmp+iEOBWord) = swapped;
    }

    // --- Decode EOB block and write it in EOBFile --- //
    for (UInt_t iEOBWord=1; iEOBWord<NSpecWords; iEOBWord++) {
      for(Int_t iChar=0;iChar<4;iChar++){ // Scan over the 4 characters of the iEOBWord
        if(dbg) PrintASCIIWord(FPGAid,swapDataBuffer,iEOBWord); // Debug printout
        Int_t iCurrentChar = iEOBWord*4+iChar;
        if(iCurrentChar>iEOLChar) { // Need to look for next EOL character
          iEOLChar = iCurrentChar;
          while((UInt_t)iEOLChar<NSpecWords*4){ // Look for next EOL character
            if(*(reinterpret_cast<char *>(swapDataBuffer)+iEOLChar)==eol) break;
            else iEOLChar++;
          }
        }
        TString EOBStr(reinterpret_cast<char *>(swapDataBuffer)+iCurrentChar,iEOLChar-iCurrentChar);
        cout_en(dbg,1) << "EOBStr = '" << EOBStr << "'" << std::endl; 

        if(EOBFileDescriptor){ //dump the EOB data on the .eob file
          fwrite (EOBStr.Data(),sizeof(char),EOBStr.Length(), EOBFileDescriptor);
          fwrite ((int*) (&eol),sizeof(char),1,EOBFileDescriptor);
        }

        if(EOBStr.Length()){
          TObjArray *Line = EOBStr.Tokenize("=");
          if (Line->GetEntries()>=2) {
            TString Label  = static_cast<TObjString*>(Line->At(0))->GetString();
            TString Values = static_cast<TObjString*>(Line->At(1))->GetString();

            Bool_t IsACounter = Values.First(':')>=0; //whatever has a ':' is a counter
            TObjArray *ValuesArray = Values.Tokenize(":");
            Bool_t ChannelCountFlag = false;
            Bool_t M3PSTAFlag = false;
            if(Label.BeginsWith("CHANNEL_COUNT_")) ChannelCountFlag = true;
            if(!Label.CompareTo("M3PSTA")) M3PSTAFlag = true;
            for (Int_t iValue=0; iValue<ValuesArray->GetEntries(); iValue++) {
              TString ValueStr = static_cast<TObjString*>(ValuesArray->At(iValue))->GetString();
              if(ValueStr.BeginsWith("0x")) ValueStr.Remove(0,2);
              UInt_t  Value = strtol(TString(ValueStr(TRegexp("[0-9a-fA-F]+"))),NULL,16);
              if(ChannelCountFlag && ValueStr.BeginsWith("ALPHA")) continue; // Ignore any bogus "ALPHA=0.007"
              else if(!Label.CompareTo("PI")) continue; // Ignore any bogus "PI=3.14"
              else if(M3PSTAFlag){ //custom treatment 
                // M3PSTA=XXXXXXXX: ... :XXXXXXXX: [64 loose-hit counters]
                //        XXXXXXXX: ... :XXXXXXXX: [64 tight-hit counters]
                //        XXXXXXXX:                [1 word with error flags]
                //        XXXXXXXX: ... :XXXXXXXX: [8 error counters]
                //        XXXXXXXX:                [1 counter for the number of 25 ns time-slots during which an error was flagged]
                //        XXXXXXXX:                [1 counter for the number of 25 ns time-slots while inburst flag was high]
                if(iValue<64)      {
                  Label = "M3PSTA_LooseHits";
                  IsACounter = true;
                }
                else if(iValue<128){
                  Label = "M3PSTA_TightHits";
                  IsACounter = true;
                }
                else if(iValue<129){
                  Label = "M3PSTA_ErrorFlags";
                  IsACounter = false;
                }
                else if(iValue<137){
                  Label = "M3PSTA_ErrorCounts";
                  IsACounter = true;
                }
                else if(iValue<138){
                  Label = "M3PSTA_LiveTimeNum";
                  IsACounter = false;
                }
                else if(iValue<139){
                  Label = "M3PSTA_LiveTimeDen";
                  IsACounter = false;
                }
              }
              if(IsACounter) {
                Int_t ROChannelID = (Value&0xff000000)>>24;
                Int_t ChannelID = ROChannelID;
                if(ChannelCountFlag){ // Use the remapped ChannelIDs
                  ROChannelID += 512*TEL62Board+128*FPGAid;
                  ChannelID = GetChannelRemap(ROChannelID);
                }
                Value = (Value&0xffffff);
                SpecTrig->AddCounter(Label,ChannelID,Value);
              }
              else {
                SpecTrig->AddRegister(Label,Value);
                if(!Label.CompareTo("LAST_TIMESTAMP")) SpecTrig->SetTimeStamp(Value);
              }
            }
            delete ValuesArray;
          }
          else{
            cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Wrong EOB label format! [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
            cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " EOBStr: '" << EOBStr << "'" << std::endl;
          }
          delete Line;
        }

        // Jump to the next EOL character
        iEOBWord = iEOLChar/4;
        iChar = iEOLChar%4;
      }
    }
    delete[] swapDataBuffer;
    // --------------------------------------------- //
    cout_en(dbg,1) << "EventID: " << std::hex << TdcEvent->GetID() << std::dec << " EOB - Size: " << NSpecWords << " TimeStamp: " << SpecTrig->GetTimeStamp() << std::endl;
  }
  else if(isL0CHOKEON(l0TriggerType)){
    //choke ON, do nothing!
  }
  else if(isL0CHOKEOFF(l0TriggerType)){
    //choke OFF, do nothing!
  }
  else { //add here other special trigger cases!
    for(int iFPGA=0;iFPGA<fNROMezzaninesPerFullBoard;iFPGA++){
      TDCError* TdcError = static_cast<TDCError*>(TdcEvent->AddError((int)TDCBDecoderErr::TDCB_BAD_SPECIAL_TRIGGER));
      TdcError->SetROBoardID(TEL62Board);
      TdcError->SetROMezzanineID(iFPGA);
      TdcError->SetFatal(kTRUE);
      fHDecoderErrors->Fill(fNROMezzaninesPerFullBoard*TEL62Board+iFPGA,(int)TDCBDecoderErr::TDCB_BAD_SPECIAL_TRIGGER);
      fNCriticalErrors[fNROMezzaninesPerFullBoard*TEL62Board+iFPGA]++;
      cerr_en(fWarningsLevel,WARN_DET) << "[TDCBRawDecoder]     WARNING: Unknown special trigger type!  [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
      cerr_en(fWarningsLevel,WARN_DET) << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " datum: " << std::hex << l0TriggerType << std::dec << "] Skipping.." << std::endl;
      std::cerr << "[TDCBRawDecoder]     WARNING: Unknown special trigger type!  [File: " << CurrentFileName << " EventID: " << std::hex << TdcEvent->GetID() << std::dec;
      std::cerr << " SubDet: " << fReco->GetName() << " TEL62: " << TEL62Board << " datum: " << std::hex << l0TriggerType << std::dec << "] Skipping.." << std::endl;
    }
  }
  pDataBuffer+=NSpecWords-1;
  NumberOfBlockDataRead+=NSpecWords-1;
}

void TDCBRawDecoder::PrintASCIIWord(Int_t FPGAid, UInt_t* pBuffer, UInt_t iWord){ //Debug printout
  char *pCounter = reinterpret_cast<char*>(pBuffer)+iWord;
  char temp[4]; strncpy (temp, pCounter, 4);
  std::cout << "### " << fReco->GetName() << " FPGA" << FPGAid << " Word " << iWord <<" : '";
  for (int i=0; i<4; i++) std::cout << temp[i];
  std::cout << "'" << std::endl;
}

///////////////////////////////////////////////////////////////
// ParseRawDecoderSettingsFile
////////////////////////////////////////////////////////////////

void TDCBRawDecoder::ParseRawDecoderSettingsFile(TString RawDecFileName){
  //Int_t nItems=0;
  //Int_t MaxChannelID = -1;
  TString Line;
  if(NA62ConditionsService::GetInstance()->Open(RawDecFileName)!=kSuccess) return;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(RawDecFileName))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if (Line.BeginsWith("NFPGAs")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries()-1;
      if(NEntries!=fNROBoards) {
        std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: " << NEntries << " NFPGAs for " << fNROBoards << " boards!";
        if(NEntries>fNROBoards) {
          std::cerr << " Ignoring last " << NEntries-fNROBoards << " occurrences in NFPGAs.." << std::endl;
        }
        else {
          std::cerr << " Using '" << static_cast<TObjString*>(l->At(1))->GetString() << "' for last " << fNROBoards-NEntries << " boards.." << std::endl;
          for(Int_t i=0;i<fNROBoards-NEntries;i++) {
            TObject *obj = l->At(1)->Clone();
            l->Add(obj);
          }
        }
      }
      if(fNROBoards) {
        fNFPGAs = new Int_t[fNROBoards];
        for(Int_t i = 0; i < fNROBoards; i++) {
          fNFPGAs[i] = static_cast<TObjString*>(l->At(i+1))->GetString().Atoi();
        }
      }
      else std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: fNROBoards = 0!" << std::endl;
      delete l;
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(RawDecFileName);

}

void TDCBRawDecoder::EndProcessing(){
  NA62VRawDecoder::EndProcessing();
  //Write TDCBDecoder-specific histos
  if(fHTEL62Errors) fHTEL62Errors->Write();
}

Int_t TDCBRawDecoder::FindTEL62ErrorType(TString str){

  // data error flags (ErrorWordID==3)
  if(str.Contains("Suppressor ON"))      return (int)TEL62DecoderErr::TEL62_SUPPRESSOR_ON;
  else if(str.Contains("Limiter ON"))    return (int)TEL62DecoderErr::TEL62_LIMITER_ON;
  // common error flags (ErrorWordID==2)
  else if(str=="Frame TS/Timeout")       return (int)TEL62DecoderErr::TEL62_FRAMETS_TIMEOUT;
  else if(str=="TDCB global error")      return (int)TEL62DecoderErr::TEL62_TDCB_GLOBAL_ERROR;
  else if(str.BeginsWith("DDR"))         return (int)TEL62DecoderErr::TEL62_DDR_ERROR;
  else if(str=="Internal compressor")    return (int)TEL62DecoderErr::TEL62_INTERNAL_COMPRESSOR;
  else if(str=="Hit/Slot Time mismatch") return (int)TEL62DecoderErr::TEL62_HITTS_MISMATCH;
  else if(str=="Compressor overflow")    return (int)TEL62DecoderErr::TEL62_COMPRESSOR_OVERFLOW;
  else if(str.BeginsWith("Organizer"))   return (int)TEL62DecoderErr::TEL62_ORGANIZER_ERROR;
  else if(str.BeginsWith("OB"))          return (int)TEL62DecoderErr::TEL62_OB_ERROR;
  else if(str.BeginsWith("IB"))          return (int)TEL62DecoderErr::TEL62_IB_ERROR;
  else if(str.BeginsWith("TDC")){ //TDCB and Suppressor/Limiter conditions must be not satisfied
    if(str.Contains("Fatal")) return (int)TEL62DecoderErr::TEL62_TDC_FATAL;
    else return (int)TEL62DecoderErr::TEL62_TDC_ERROR;
  }
  // ErrorWordID==1 and ==0 are included in the line above

  return -1;
}
