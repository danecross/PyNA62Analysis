//
// Created by B. Velghe (bob.velghe@cern.ch) - Dec. 2014
// Modified by M. Perrin-Terrin (mathieu.perrin-terrin@cern.ch) - Jun 2015
//
#include "NA62BufferProto.hh"//isEOS isSOB
#include "GTKTLRawDecoder.hh"
#include "TGigaTrackerDigi.hh"
#include "NA62Reconstruction.hh"

// The id "GTK_TL" must match the one in GigaTracker/src/GigaTrackerRawDecoder.cc
//GTKTLRawDecoder::GTKTLRawDecoder() : NA62VRawDecoder(0, "GTK_TL") {
GTKTLRawDecoder::GTKTLRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "GTK_TL") {

  //  fCurrentEvent = 0;
  //  fDigiEvent = new TGigaTrackerEvent();
  fDigiEvent = new TDCEvent(TGigaTrackerDigi::Class());
  mGTKBlock = new GTK::GigaTrackerTLDataBlock();
  fSpecialTriggerEvent = new TSpecialTriggerEvent(TSpecialTrigger::Class());

  //----------- Init common parameters and instances ----------//

  NA62VRawDecoder::CreateObjects();
  ParseRawDecoderSettingsFile(fReco->GetConfigFileName());

  //-----------------------------------------------------------//
}


GTKTLRawDecoder::~GTKTLRawDecoder() {
   delete mGTKBlock;
}

TDetectorVEvent * GTKTLRawDecoder::DecodeNextEvent(UInt_t* Current, EventHeader* Header, UInt_t* NextOffset) {
  
  //  Int_t wrn = (static_cast<NA62Reconstruction*>(fReco->GetMainReco())->GetWarningsLevel());
  fDigiEvent->SetTimeStamp(Header->GetTimeStamp());
  fDigiEvent->SetBurstID(Header->GetBurstID());
  fDigiEvent->SetID(Header->GetEventNumber());
  fDigiEvent->SetIsMC(false);



  //###### Special Triggers ######
  // Special trigger treatment
  if (isL0SpecialFrame(Header->GetTriggerType()<<2)) {
    TSpecialTrigger *SpecTrig= static_cast<TSpecialTrigger*>( fSpecialTriggerEvent->AddSpecialTrigger());
    SpecTrig->SetTimeStamp(Header->GetTimeStamp());
    SpecTrig->SetTriggerType(Header->GetTriggerType()<<2);
    fSpecialTriggerEvent->SetTimeStamp(Header->GetTimeStamp());
    fSpecialTriggerEvent->SetStartByte(Header->GetStartByte());
    fSpecialTriggerEvent->SetIsMC(kFALSE);
    fSpecialTriggerEvent->SetID(Header->GetEventNumber());
    fSpecialTriggerEvent->SetBurstID(Header->GetBurstID());
    fSpecialTriggerEvent->SetTriggerType(Header->GetTriggerType()<<2);
    return fSpecialTriggerEvent;
  }

  std::vector<GTK::GigaTrackerTLHit> EventHits;  
  NextOffset = NextOffset + mGTKBlock->ReadFromBuffer(Current,EventHits);
  for(unsigned int i(0);i<EventHits.size();i++) {
    //    std::cout<<EventHits[i];
 
    TGigaTrackerDigi *GTKDigi = static_cast<TGigaTrackerDigi*>(fDigiEvent->AddHit());

    GTKDigi->SetSourceId( EventHits[i].GetSourceId() );

    GTKDigi->SetStationNo(EventHits[i].GetStationId());
    GTKDigi->SetChipID( EventHits[i].GetChipId() );
    //GTKDigi->SetqChipId( EventHits[i].GetqChipId() );

    GTKDigi->SetFrameCounter( EventHits[i].GetFrameCounter() );
    GTKDigi->SetPixelAddress( EventHits[i].GetPixelAddress() ); 
    GTKDigi->SetHitArbiterAddress( EventHits[i].GetHitArbiterAddress() );
    GTKDigi->SetPileUpAddress( EventHits[i].GetPileUpAddress() );
    GTKDigi->SetLeadingSelector( EventHits[i].GetLeadingSelector() );
    GTKDigi->SetLeadingCoarse( EventHits[i].GetLeadingCoarse() ); 
    GTKDigi->SetLeadingFine( EventHits[i].GetLeadingFine() ); 
    GTKDigi->SetTotSelector( EventHits[i].GetTotSelector() );
    GTKDigi->SetTotCoarse( EventHits[i].GetTotCoarse() ); 
    GTKDigi->SetTotFine( EventHits[i].GetTotFine() ); 
    GTKDigi->SetDelay(EventHits[i].GetDelay() );



    //TDCVHit
    GTKDigi->SetLeadingEdge(EventHits[i].GetLeadingTime());
    GTKDigi->SetTrailingEdge(EventHits[i].GetTrailingTime());
    GTKDigi->SetPixelID(EventHits[i].GetPixelUID());
    GTKDigi->EncodeChannelID();


    //    printf("Leading Coarse Time %u\n",GTKDigi->GetLeadingCoarse());
    //    printf("Nb of Hits in Event %d\n",fDigiEvent->GetNHits());


  }
  return fDigiEvent;
  
}


