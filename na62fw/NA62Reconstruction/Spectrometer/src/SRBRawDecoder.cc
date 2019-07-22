#include "NA62Buffer.hh"
#include "NA62BufferProto.hh"
#include "SRBEvent.hh"
#include "SRBError.hh"
#include "SRBRawDecoder.hh"
#include "NA62Reconstruction.hh"
#include "SpectrometerReconstruction.hh"
#include "NA62ConditionsService.hh"
#include "TRegexp.h"

#include <numeric>
#include <deque>

SRBRawDecoder::SRBRawDecoder(NA62VReconstruction* Reco) :
  NA62VRawDecoder(Reco, "SRB", 16),
  fSRBOffset(0),
  fSRBLatency(0),
  fMinTime(-10000),
  fMaxTime(0),
  fT0j(nullptr),
  fHFineTime(nullptr)
{
  fDigiEvent = new SRBEvent(TSpectrometerDigi::Class());
  fSpecialTriggerEvent = new TSpecialTriggerEvent(TSpecialTrigger::Class());
  //----------- Init common parameters and instances ----------//
  NA62VRawDecoder::CreateObjects();
  fIfToJump=false; // may be changed in ParseRawDecoderSettingsFile
  ParseRawDecoderSettingsFile(fReco->GetRawDecoderSettingsFileName()); //SRB-specific parameters
  //-----------------------------------------------------------//

  // Reset histos
  fHDigiTimeRaw = 0;
  fHDigiTimeRawFine = 0;
  fHDigiTimeRawFineVsROChannel = 0;
  fHDecoderErrors = 0;
}

SRBRawDecoder::~SRBRawDecoder() {
  if(fSRBLatency) {
    delete [] fSRBLatency;
    fSRBLatency=0;
  }
}

void SRBRawDecoder::Init() {

  TFile *histoFile=fReco->GetHistoFile();
  if(!histoFile) return;
  // switch to a subdirectory in the output file to avoid histogram name clash from decoders
  // base class is instatiated first

  TDirectory * monitorDir =histoFile->GetDirectory(fReco->GetName()+"Monitor");
  if (!monitorDir) histoFile->mkdir(fReco->GetName()+"Monitor")->cd();
  else monitorDir->cd();

  // Book histograms to evaluate StationsT0, NSlots, LastSlotID (data only)
  if(fNROBoards){
    fHDecoderErrors = new TH2F("DecoderErrors",
        "Errors from the SRBRawDecoder for the "+fReco->GetName(),
        fNROBoards, -0.5, fNROBoards-0.5,
        fMapSRBError.size(), 0.5, (double)fMapSRBError.size() + 0.5);
    fHDecoderErrors->GetXaxis()->SetTitle("SRB ID");
    for (auto & err : fMapSRBError) {
      if ((Int_t)err.first <= GetLastCriticalErrorEnum()) {
        // make label red for critical errors
        err.second = Form("#color[2]{%s}",err.second.Data());
      }
      fHDecoderErrors->GetYaxis()->SetBinLabel((int)err.first, err.second);
    }
    fHDecoderErrors->SetOption("colz");
    fHFineTime = new TH2F("FineTimeVsSRB",
        "Fine time distribution for all SRBs; SRB ID; fine time",
        fNROBoards, -0.5, fNROBoards-0.5, 32, -0.5, 31.5);
    fHFineTime->SetOption("colz");
  }
  else std::cerr << "[SRBRawDecoder] WARNING: fNROBoards = 0!" << std::endl;

  Int_t nBins = (Int_t)((fMaxTime - fMinTime) / kTIMEBINSIZE);
  nBins = (nBins / 32) * 32;      // round down to multiple of 32 (one 25 ns time slot)
  fMaxTime = fMinTime + nBins*kTIMEBINSIZE;

  fHDigiTimeRaw = new TH2F
    ("DigiTimeRaw", "Spectrometer DigiTimeRaw Vs MezzanineID",
     fNROMezzanines,-0.5,fNROMezzanines-0.5, nBins, fMinTime, fMaxTime);
  fHDigiTimeRawFine = new TH2F("DigiTimeRawFine", "Spectrometer DigiTimeRawFine Vs MezzanineID",
      fNROMezzanines,-0.5,fNROMezzanines-0.5, nBins, fMinTime, fMaxTime);
  fHDigiTimeRawFineVsROChannel = new TH2F
    ("DigiTimeRawFineVsROChannel", "Spectrometer DigiTimeRawFine Vs ROChannel",
     fNROChannels, -0.5, fNROChannels-0.5, nBins, fMinTime, fMaxTime);
  fHDigiTimeRaw->SetOption("colz");
  fHDigiTimeRawFine->SetOption("colz");
  fHDigiTimeRawFineVsROChannel->SetOption("colz");
}

void SRBRawDecoder::StartOfBurst() { //called for RawData only
}

void SRBRawDecoder::EndOfBurst() { //called for RawData only
}

TDetectorVEvent * SRBRawDecoder::DecodeNextEvent(UInt_t * pDataBuffer, EventHeader * pEventHeader, UInt_t * NextOffset)
{
  Int_t dbg = 0;
  TString CurrentFileName = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetCurrentFileName(); //for debug
  fDigiEvent->SetTimeStamp(pEventHeader->GetTimeStamp());
  fDigiEvent->SetID(pEventHeader->GetEventNumber());
  fDigiEvent->SetBurstID(pEventHeader->GetBurstID());
  fDigiEvent->SetRunID(pEventHeader->GetRunID());
  fDigiEvent->SetIsMC(false);

  // Special trigger treatment
  if (isL0SpecialFrame(pEventHeader->GetTriggerType()<<2)) {
    TSpecialTrigger *SpecTrig= static_cast<TSpecialTrigger*>( fSpecialTriggerEvent->AddSpecialTrigger());
    SpecTrig->SetTimeStamp(pEventHeader->GetTimeStamp());
    SpecTrig->SetTriggerType(pEventHeader->GetTriggerType()<<2);
    fSpecialTriggerEvent->SetTimeStamp(pEventHeader->GetTimeStamp());
    fSpecialTriggerEvent->SetStartByte(pEventHeader->GetStartByte());
    fSpecialTriggerEvent->SetIsMC(kFALSE);
    fSpecialTriggerEvent->SetID(pEventHeader->GetEventNumber());
    fSpecialTriggerEvent->SetBurstID(pEventHeader->GetBurstID());
    fSpecialTriggerEvent->SetRunID(pEventHeader->GetRunID());
    return fSpecialTriggerEvent;
  }

  UInt_t *datum = (UInt_t *)pDataBuffer;
  UInt_t nhits;
  UInt_t NumberOfBlockDataRead=0;
  UInt_t DataBlockFormatID = DataBlockFormat(*datum); // read the format ID from the first header word

  if (DataBlockFormatID == 0) { // 2014 data (Straw trigger-less data, not supported anymore)
    cerr_en(fWarningsLevel, WARN_DET)
      << "[SRBRawDecoder]        WARNING: Straw spectrometer reconstruction of 2014 data not supported anymore. Use older revision!"  << std::endl;
  } else if (DataBlockFormatID == 1) {   // 2015 data onwards (trigger matched data)
    while (datum < NextOffset) { // Loop over SRBs
      // Subdetector Block Header (NA62 generic part)
      UInt_t DataBlockByteSize = DataBlockSize(*datum);
      UInt_t SRBBoard          = DetectorSubType(*datum);
      cout_en(dbg,1) << "----------------------------------" << std::endl
        << "EventID: " << std::hex << fDigiEvent->GetID() << std::dec << std::endl
        << " SubDet: " << fReco->GetName() << std::endl
        << " SRB board: " << SRBBoard << std::endl
        << " Data block size in bytes: " << DataBlockByteSize << std::endl
        << " Data format: " << DataBlockFormatID << std::endl;
      datum++;
      NumberOfBlockDataRead++;

      // 2nd generic NA62 word, time stamp
      ULong_t DataBlockTS = DataBlockTimeStamp(*datum);
      datum++;
      NumberOfBlockDataRead++;
      cout_en(dbg,1)<< " DataBlockTS: 0x" << std::hex << DataBlockTS << std::dec << std::endl;

      UInt_t offsetBlockTrigger = 0;
      if (DataBlockTS > pEventHeader->GetTimeStamp()) {
        offsetBlockTrigger = DataBlockTS - pEventHeader->GetTimeStamp();
        cout_en(dbg,1)<< " DataBlockTS - TriggerTS: 0x" << std::hex << offsetBlockTrigger << std::dec << std::endl;
      }
      if (offsetBlockTrigger != fSRBOffset) { // Block timestamp mismatch
        fHDecoderErrors->Fill(SRBBoard, (int)SRBDecoderErr::SRB_BLOCKTS_MISMATCH);
        SRBError* srbError = static_cast<SRBError*>(fDigiEvent->AddError((int)SRBDecoderErr::SRB_BLOCKTS_MISMATCH));
        srbError->SetSRBAddr(SRBBoard);
        srbError->SetFatal(kTRUE);
        for (int iCover = 0; iCover < 16; iCover++) {
          fNCriticalErrors[16*SRBBoard+iCover]++;
        }
      }

      UInt_t fSrbID         = (*datum & SRB_ID_MASK)            >> SRB_ID_SHIFT;
      UInt_t fFlags         = (*datum & SRB_FLAGS_MASK)         >> SRB_FLAGS_SHIFT;
      UInt_t fL0TriggerType = (*datum & SRB_L0TRG_TYPE_MASK)    >> SRB_L0TRG_TYPE_SHIFT;
      UInt_t fPacketLength  = (*datum & SRB_PACKET_LENGTH_MASK) >> SRB_PACKET_LENGTH_SHIFT;
      // number of 32-bit words with hits, computed from packet length (in bytes)
      UInt_t nWordLines = fPacketLength/4 - 4 - 2; // 4  words for counters, 2  words for header
      cout_en(dbg,1)<< " SRB header datum(1): 0x" << std::hex << *datum << std::dec << std::endl
        << "  - fSrbID = " << fSrbID << std::endl
        << "  - fFlags = " << fFlags << std::endl
        << "  - fL0TriggerType = " << fL0TriggerType << std::endl
        << "  - fPacketLength  = " << fPacketLength << std::endl
        << "  - N hit lines (32 bit words) = " << nWordLines << std::endl;
      if (fSrbID != SRBBoard || fSrbID >= (UInt_t)fNROBoards || fPacketLength < 24) {
        fHDecoderErrors->Fill(fSrbID, (int)SRBDecoderErr::SRB_HEADER_MISMATCH);
        SRBError* srbError = static_cast<SRBError*>(fDigiEvent->AddError((int)SRBDecoderErr::SRB_HEADER_MISMATCH));
        srbError->SetSRBAddr(fSrbID);
        srbError->SetFatal(kTRUE);
        for (int iCover = 0; iCover < 16; iCover++) {
          fNCriticalErrors[16*fSrbID+iCover]++;
        }
        return fDigiEvent; // critical error blocking the decoder to process other SRBs
      }

      // move to second line (coarse time of the first slot)
      datum++;
      NumberOfBlockDataRead++;
      Int_t fCoarseTimeFirst = (*datum & SRB_32BITWORD_MASK);
      Int_t latency = DataBlockTS - fCoarseTimeFirst;
      cout_en(dbg,1)<< " SRB header datum(2): 0x" << std::hex << *datum << std::endl
        << "  - EventHeader time   = 0x" << pEventHeader->GetTimeStamp() << std::endl
        << "  - fCoarseTimeFirst = " << std::dec << fCoarseTimeFirst << std::endl
        << "  - latency =  0x" << std::hex << latency << std::dec << " = "
        << latency * ClockPeriod << " ns " << std::endl;

      // wrong latency measured by SRB wrt Block TS: negative or more than 1.5ms
      if (latency < 0 || TMath::Abs(latency) > 60000) {
        fHDecoderErrors->Fill(fSrbID, (int)SRBDecoderErr::SRB_OUT_OF_TIME);
        SRBError* srbError = static_cast<SRBError*>(fDigiEvent->AddError((int)SRBDecoderErr::SRB_OUT_OF_TIME));
        srbError->SetSRBAddr(fSrbID);
        srbError->SetFatal(kTRUE);
        for (int iCover = 0; iCover < 16; iCover++) {
          fNCriticalErrors[16*fSrbID+iCover]++;
        }
      }

      // shift time by global trigger event timestamp
      fCoarseTimeFirst-= pEventHeader->GetTimeStamp();

      // counters for number of hits in 16 time slots (8 bits each)
      std::vector<UInt_t> fCounters;
      // hit words (16 bits each), using std container deque as a fifo
      std::deque<UInt_t> fHits;
      // move to third line (first data word)
      datum++;
      NumberOfBlockDataRead++;

      // read hit words into the deque (FIFO)
      for (UInt_t ihit_line = 0; ihit_line < nWordLines; ihit_line++) {
        UInt_t dataLine = *datum & SRB_32BITWORD_MASK;
        cout_en(dbg,1) << " Hit word datum("<< ihit_line <<"): 0x" << std::hex << *datum << std::dec << std::endl;
        fHits.push_back((dataLine & SRB_WORD1_MASK) >> SRB_WORD1_SHIFT);
        fHits.push_back((dataLine & SRB_WORD2_MASK) >> SRB_WORD2_SHIFT);
        // move to next word
        datum++;
        NumberOfBlockDataRead++;
      }
      // the last hit will be 0xFFFF (not a real hit), if there was odd number of hits in event
      if (!fHits.empty() &&  fHits.back() == 0xFFFF) {
        fHits.pop_back();         // remove last element
      }

      // 4 32-bit words with 8-bit counters -> 16 counters in total
      for (UInt_t iCounterLine = 0; iCounterLine < 4; iCounterLine++ ) {
        cout_en(dbg,1) << " Counter word datum("<< iCounterLine <<"): 0x"<< std::hex << *datum << std::dec << std::endl;
        UInt_t dataLine = *datum & SRB_32BITWORD_MASK;
        fCounters.push_back((dataLine & SRB_COUNTER1_MASK) >> SRB_COUNTER1_SHIFT);
        fCounters.push_back((dataLine & SRB_COUNTER2_MASK) >> SRB_COUNTER2_SHIFT);
        fCounters.push_back((dataLine & SRB_COUNTER3_MASK) >> SRB_COUNTER3_SHIFT);
        fCounters.push_back((dataLine & SRB_COUNTER4_MASK) >> SRB_COUNTER4_SHIFT);
        datum++;
        NumberOfBlockDataRead++;
      }

      // important check: sum of counters should match total number of hits
      nhits = std::accumulate(fCounters.cbegin(), fCounters.cend(), 0);
      if (nhits != fHits.size()){
        cerr_en(fWarningsLevel, WARN_DET) << "[SRBRawDecoder]      WARNING: Sum of counters does not match number of hits [File: "
          << CurrentFileName << " EventID: " << std::hex << fDigiEvent->GetID() << std::dec << std::endl;
        fHDecoderErrors->Fill(fSrbID, (int)SRBDecoderErr::SRB_COUNTER_MISMATCH);
        SRBError* srbError = static_cast<SRBError*>(fDigiEvent->AddError((int)SRBDecoderErr::SRB_COUNTER_MISMATCH));
        srbError->SetSRBAddr(fSrbID);
        srbError->SetFatal(kTRUE);
        for (int iCover = 0; iCover < 16; iCover++) {
          fNCriticalErrors[16*fSrbID+iCover]++;
        }
        return fDigiEvent; // critical error blocking the decoder to process other SRBs
      }

      // decode hits
      UInt_t nhitsDecoded = 0;
      for (size_t iCounterSlot = 0; iCounterSlot < fCounters.size(); ++iCounterSlot) {
        UInt_t nHitsSlot = fCounters[iCounterSlot];
        cout_en(dbg,1) << " N hits in slot "<< iCounterSlot <<" = " << nHitsSlot << std::endl;
        if (nHitsSlot == 0) continue; // slot is empty
        Int_t coarseTime = fCoarseTimeFirst + iCounterSlot; // shift coarse time by slot number

        UInt_t previousWord = 0xFFFF;
        for (size_t iHit = 0; iHit < nHitsSlot; ++iHit) {
          UInt_t word = fHits.front(); // read first hit from FIFO
          fHits.pop_front();           // remove the hit from FIFO
          cout_en(dbg,1) << " Adding word: 0x"<<  std::hex << word
            << " with coarse time: 0x" <<  std::dec << coarseTime << std::endl;
          if (word == 0xffff) { // Padding error
            fHDecoderErrors->Fill(fSrbID, (int)SRBDecoderErr::SRB_PADDING_ERROR);
          } else if (word == previousWord) { // Repeated word in the same time slot
            fHDecoderErrors->Fill(fSrbID, (int)SRBDecoderErr::SRB_REPEATED_WORD);
          } else {    // good word
            AddSpectrometerHit(word, coarseTime, fSrbID, pEventHeader);
          }
          previousWord = word;
          nhitsDecoded++;
        }
      }

      // last check of number of hits decoded
      if (nhits != nhitsDecoded) {
        cerr_en(fWarningsLevel, WARN_DET) << "[SRBRawDecoder]      WARNING: Not all hits were decoded  [File: "
          << CurrentFileName << " EventID: " << std::hex << fDigiEvent->GetID() << std::dec << std::endl;
        fHDecoderErrors->Fill(fSrbID, (int)SRBDecoderErr::SRB_COUNTER_MISMATCH);
      }
    }
  } else {
    cerr_en(fWarningsLevel, WARN_DET) << "[SRBRawDecoder]      WARNING: Unknown data format ID: " << DataBlockFormatID
      << "  [File: " << CurrentFileName
      << " EventID: " << std::hex << fDigiEvent->GetID() << std::dec << std::endl;
  }

  return fDigiEvent;
}

// Add new TSpectrometerDigi or update an existing one with missing leading or trailing
void SRBRawDecoder::AddSpectrometerHit(UInt_t hitWord, Int_t coarseTime, UInt_t srbID, EventHeader * pEventHeader)
{
  Int_t dbg = 0;

  TString CurrentFileName = static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetCurrentFileName(); //for debug
  UInt_t error    = (hitWord & SRB_HIT_ERROR_MASK)   >>SRB_HIT_ERROR_SHIFT;
  UInt_t strawID  = (hitWord & SRB_HIT_STRAWID_MASK) >>SRB_HIT_STRAWID_SHIFT;
  UInt_t edge     = (hitWord & SRB_HIT_EDGE_MASK)    >>SRB_HIT_EDGE_SHIFT; // firmware: 0=lead,1=trail
  UInt_t finetime = (hitWord & SRB_HIT_FINETIME_MASK)>>SRB_HIT_FINETIME_SHIFT;

  if(fHFineTime) fHFineTime->Fill(srbID, finetime);

  if(fIfToJump){
    int icover = GetROMezzanine(256*srbID+strawID);                    // (srbID*16 + strawID/16)
    Int_t jump = fT0j->GetJump(icover,pEventHeader->GetBurstTime());
    if(jump)coarseTime -= jump; // T0 jump
  }

  Double_t time = ClockPeriod*coarseTime + (ClockPeriod/32.)*finetime;
  Int_t nChannel = GetChannelRemap(256*srbID+strawID);
  cout_en(dbg,1) << " - channel ID = " << nChannel << std::endl
    << " - straw ID   = " << strawID << std::endl
    << " - edge       = " << edge << std::endl
    << " - finetime   = " << finetime << std::endl
    << " - time       = " << time << " ns" << std::endl;
  if (nChannel < 0) {
    cerr_en(fWarningsLevel,WARN_DET) << "[SRBRawDecoder]\t WARNING: Hit from masked channel! File: " << CurrentFileName
      << " EventID: 0x" << std::hex << fDigiEvent->GetID() << std::dec
      << " SubDet: " << fReco->GetName() << " SRB: " << srbID << " strawID: " << strawID << std::endl;
    fNHitsFromMaskedChannels[GetROMezzanine(256*srbID+strawID)]++;
    fHDecoderErrors->Fill(srbID, (int)SRBDecoderErr::SRB_MASKED_HIT);
    return;
  }

  bool digiUpdated = false;
  // loop over previously added digis and update the leading/trailing time
  for (int iDigi = 0; iDigi < fDigiEvent->GetNHits(); iDigi++) {
    auto digi = static_cast<TSpectrometerDigi*>(fDigiEvent->GetHit(iDigi));
    if (digi->GetChannelID() != nChannel) {
      continue;             // skip other channels
    } else if (digi->GetDetectedEdge() == SRB_HIT_EDGE_LEADING + SRB_HIT_EDGE_TRAILING) {
      continue;             // skip complete digis
    } else if (digi->GetDetectedEdge() == SRB_HIT_EDGE_LEADING && edge == 1 &&
        time > digi->GetLeadingEdge()) {  // preserve l/t time ordering
      digi->SetTrailingEdge(time);
      digi->UpdateDetectedEdge(SRB_HIT_EDGE_TRAILING);
      digiUpdated = true;
      break;
    } else if (digi->GetDetectedEdge() == SRB_HIT_EDGE_TRAILING && edge == 0 &&
        time < digi->GetTrailingEdge()) { // preserve l/t time ordering
      digi->SetLeadingEdge(time);
      digi->UpdateDetectedEdge(SRB_HIT_EDGE_LEADING);
      digiUpdated = true;
      break;
    } else {
      continue;
    }
  }

  if (!digiUpdated) {           // create new Digi if the hit was not used to update an existing one
    TSpectrometerDigi *SpectrometerDigi = static_cast<TSpectrometerDigi*>(fDigiEvent->AddHit(nChannel));
    SpectrometerDigi->DecodeChannelID();
    SpectrometerDigi->SetStrawAddr(strawID);
    SpectrometerDigi->SetSRBAddr(srbID);
    SpectrometerDigi->SetLeadingEdge(-999999.);
    SpectrometerDigi->SetTrailingEdge(-999999.);
    SpectrometerDigi->SetMultiHit((Bool_t)error);
    if (edge == 0) {
      SpectrometerDigi->SetLeadingEdge(time);
      SpectrometerDigi->SetDetectedEdge(SRB_HIT_EDGE_LEADING);
    } else {
      SpectrometerDigi->SetTrailingEdge(time);
      SpectrometerDigi->SetDetectedEdge(SRB_HIT_EDGE_TRAILING);
    }
  }
}


///////////////////////////////////////////////////////////////
// ParseRawDecoderSettingsFile
////////////////////////////////////////////////////////////////

void SRBRawDecoder::ParseRawDecoderSettingsFile(TString RawDecFileName){
  TString Line;
  if(NA62ConditionsService::GetInstance()->Open(RawDecFileName)!=kSuccess) return;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(RawDecFileName))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if(Line.BeginsWith("T0Jumps=")){
      TObjArray * l = Line.Tokenize(" ");
      Int_t Nl = l->GetLast();
      Line = static_cast<TObjString*>(l->At(1))->GetString();
      std::cout << "SRBRawDecoder: T0Jumps file is requested: " << Line << std::endl;
      fT0j = new T0Jumps(Line.Data(),0);
      if(Nl > 1){  // there is a list of runs where the correction to be implemented exclusively
        Line = static_cast<TObjString*>(l->At(2))->GetString();
        std::cout << "SRBRawDecoder: List of runs with a jumps is requested: " << Line << std::endl;
        fT0j->SetExclusive(Line.Data());
      }

      fIfToJump=true;
      delete l;
      continue;
    }
    else if(Line.BeginsWith("SRBOffset=")){
      TObjArray * l = Line.Tokenize(" ");
      TString LineItem = static_cast<TObjString*>(l->At(1))->GetString();
      fSRBOffset = strtol(TString(LineItem(TRegexp("[0-9a-fA-F]+"))),NULL,16);
      delete l;
      continue;
    } else if (Line.BeginsWith("minTime=")) {
      fMinTime = TString(Line(TRegexp("[-]?[0-9]+[.][0-9]*"))).Atof();
      continue;
    } else if (Line.BeginsWith("maxTime=")) {
      fMaxTime = TString(Line(TRegexp("[-]?[0-9]+[.][0-9]*"))).Atof();
      continue;
    } else if (Line.BeginsWith("SRBLatency")) {
      TObjArray *l = Line.Tokenize(" ");
      Int_t NEntries = l->GetEntries() - 1;
      if(NEntries!=fNROBoards) {
        std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: " << NEntries << " SRBLatency for " << fNROMezzanines << " boards!";
        if(NEntries>fNROBoards) {
          std::cerr << " Ignoring last " << NEntries-fNROBoards << " occurrences in SRBLatency.." << std::endl;
        }
        else {
          std::cerr << " Using '" << static_cast<TObjString*>(l->At(1))->GetString() << "' for last " << fNROBoards-NEntries << " mezzanines.." << std::endl;
          for(Int_t i=0;i<fNROBoards-NEntries;i++) {
            TObject *obj = l->At(1)->Clone();
            l->Add(obj);
          }
        }
        NEntries = fNROBoards;
      }
      if(NEntries) fSRBLatency = new UInt_t[NEntries];
      else std::cerr << "["<<fReco->GetName()<<"RawDecoder] WARNING: fNROBoards = 0!" << std::endl;
      for(Int_t i = 0; i < NEntries; i++) {
        TString LineItem = static_cast<TObjString*>(l->At(i+1))->GetString();
        fSRBLatency[i] = strtol(TString(LineItem(TRegexp("[0-9a-fA-F]+"))),NULL,16);
      }
      delete l;
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(RawDecFileName);
}

void SRBRawDecoder::EndProcessing() {
  NA62VRawDecoder::EndProcessing();
  if (fHFineTime) fHFineTime->Write();
}
