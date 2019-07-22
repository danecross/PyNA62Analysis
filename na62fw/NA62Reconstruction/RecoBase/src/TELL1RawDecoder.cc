#include "TELL1RawDecoder.hh"
#include "TELL1Buffer.h"
#include "TELL1BufferProto.h"
#include <stdlib.h>

#include "Riostream.h"

#define printf_raw(x) if((x)) printf

//#define debug_cout(x) std::cout << x << std::endl
#define debug_cout(x)

TELL1RawDecoder::TELL1RawDecoder(Bool_t TriggerLess, Bool_t BothEdges) :
    NA62VRawDecoder(0, "TELL1"),
    fTriggerLess(TriggerLess),
    fBothEdges(BothEdges),
    fCurrentEventChanged(kFALSE),
    fCurrentEventFlushed(kFALSE),
    fNTrig(-1),
    fpDataBuffer(nullptr),
    fNWords(0),
    fNMEPs(0),
    fLastiTimeStamp(0)
{
    fTdcEvents = new TClonesArray("TDCEvent",20);
}

TELL1RawDecoder::~TELL1RawDecoder(){}

void TELL1RawDecoder::Reset(UInt_t * pDataBuffer, Int_t NWords)
{
    fpDataBuffer = pDataBuffer;
    fNWords = NWords;
    fNMEPs = 0;
    for(Int_t iBoard = 0; iBoard < 4; iBoard++){
        ftTime[iBoard]=0;
        fiTime[iBoard]=0;
    }
    fTdcEvents->Clear("C");
    fLastiTimeStamp = -1;
    fTdcEventStatus.clear();
}

TDCEvent* TELL1RawDecoder::GetDecodedEvent()
{
    debug_cout("TELL1RawDecoder::GetDecodedEvent: fCurrentEventChanged(" << fCurrentEventChanged << ") " << fDigiEvent 
            << " is a " << (fDigiEvent ? fDigiEvent->IsA()->GetName() : "0" ));
    if(fCurrentEventChanged || PollEventBuffer()){
        fCurrentEventChanged = kFALSE;
        fCurrentEventFlushed = kTRUE;
        return static_cast<TDCEvent*>(fDigiEvent);
    }else
        return nullptr;
}

Bool_t TELL1RawDecoder::PollEventBuffer()
{
    Bool_t en = 0;
    fCurrentEventChanged = kFALSE;
    if(fCurrentEventFlushed){
        fCurrentEventFlushed = kFALSE;
        fTdcEvents->Remove(fDigiEvent);
        fTdcEvents->Compress();
    }
    if(fTriggerLess){
        for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t)(fTdcEvents->GetEntries()); iTimeStamp++){
            if(en) std::cout << "PollEventBuffer: Status " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
                << static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " "
                    << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
        }
        for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t)(fTdcEvents->GetEntries() - 2); iTimeStamp++){
            if(en) std::cout << "PollEventBuffer: Scanning  " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
                << static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " "
                    << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
            if(fTdcEventStatus[iTimeStamp + 2] == 0xf){
                Int_t nhits=(static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->GetNHits());
                if(nhits>0)
                    for(Int_t iHit = 0; iHit < nhits; iHit++){ // loop on hits for current channel
                        TDCVHit * TdcHitOld=static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->GetHit(iHit);
                        if((( (TdcHitOld->GetLeadingEdge() > 40000) | ((TdcHitOld->GetTrailingEdge() > 40000)<<1) ) &  
                                    TdcHitOld->GetDetectedEdge()
                           ) == TdcHitOld->GetDetectedEdge() &&
                                TdcHitOld->GetMCTrackID() <= 1){
                            printf_raw(en)("TELL1Header: moving measurement nChannel %d(%d), %7.2f - %7.2f from TimeStamp %ld to %ld\n",
                                    TdcHitOld->GetChannelID(),TdcHitOld->GetDetectedEdge(),TdcHitOld->GetLeadingEdge(),
                                    TdcHitOld->GetTrailingEdge(),
                                    static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->GetTimeStamp(),static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp());
                            //printf_raw(en)("---- %f %f %d %d\n",TdcHitOld->GetLeadingEdge(),TdcHitOld->GetTrailingEdge(),
                            //        TdcHitOld->GetDetectedEdge(),TdcHitOld->GetMCTrackID());

                            TDCVHit * TdcHit = static_cast<TDCVHit*>(static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->AddHit(TdcHitOld->GetChannelID()));
                            TdcHit->SetDetectedEdge(TdcHitOld->GetDetectedEdge());
                            TdcHit->SetLeadingEdge(TdcHitOld->GetLeadingEdge());
                            TdcHit->SetTrailingEdge(TdcHitOld->GetTrailingEdge());
                            TdcHit->SetMCTrackID(4);
                            static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->RemoveHit(iHit);
                            //------------delete TdcHitOld;
                            nhits=static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->GetNHits();
                            iHit--;
                            if(nhits == 0) break;
                        }
                    }
                if(static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp() != 0){
                    if(fTdcEventStatus[iTimeStamp] == 0xf && static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetNHits() > 0){
                        fDigiEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp]);
                        debug_cout("TELL1RawDecoder::PollEventBuffer: " << fDigiEvent << " is a " 
                                << (fDigiEvent ? fDigiEvent->IsA()->GetName() : "0" ));
                            fCurrentEventChanged = kTRUE;
                        printf_raw(en) ("TELL1Header: # of TimeStamps %d; saving %dth TimeStamp (%ld), with %d hits\n",
                                (int)fTdcEvents->GetEntries(),iTimeStamp,static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp(),fDigiEvent->GetNHits());
                        if(en){
                            //if(fDigiEvent->GetID()%1000 == 0)
                            //    std::cout << "Event " << fDigiEvent->GetID()  << " " << std::endl;
                            Int_t nhits=fDigiEvent->GetNHits();
                            if(nhits>0)
                                for(Int_t iHit = 0; iHit < nhits; iHit++) {
                                    printf_raw(en) ("TELL1Header: Ch %d Leading %7.2f Trailing %7.2f on %dth TimeStamp\n",
                                            static_cast<TDCVHit*>(fDigiEvent->GetHit(iHit))->GetChannelID(),
                                            static_cast<TDCVHit*>(fDigiEvent->GetHit(iHit))->GetLeadingEdge(),
                                            static_cast<TDCVHit*>(fDigiEvent->GetHit(iHit))->GetTrailingEdge(),
                                            static_cast<TDCVHit*>(fDigiEvent->GetHit(iHit))->GetMCTrackID());
                                }
                            //if(fDigiEvent->GetTimeStamp()%2==1 && fDigiEvent->GetNHits()>10)
                            //cout << "+++++++++++ " << fDigiEvent->GetTimeStamp() << "+++++++++++ " << fDigiEvent->GetNHits() << std::endl;
                        }
                    }else{
                        fDigiEvent = 0;
                        fNTrig--;
                    }
                }
                printf_raw(en) ("TELL1Header: # of TimeStamps %d; erasing %dth TimeStamp (%ld)\n",
                        (int)fTdcEvents->GetEntries(),iTimeStamp,static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp());
            //if(en) std::cout << "YYYY1 " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
            //    << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " " 
            //        << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
                if(!fCurrentEventChanged){
                    fTdcEvents->RemoveAt(iTimeStamp);
                    fTdcEvents->Compress();
                }else{
                    fLastiTimeStamp = iTimeStamp;
                }
                fTdcEventStatus.erase(fTdcEventStatus.begin()+iTimeStamp);
            //if(en) std::cout << "YYYY2 " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
            //    << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " " 
            //        << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
                iTimeStamp--;
                if(fCurrentEventChanged)
                    break;
            }
        }
    }
        for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t)(fTdcEvents->GetEntries()); iTimeStamp++){
            if(en) std::cout << "PollEventBuffer: Status2  " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
                << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " " 
                    << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
        }
    if(en) std::cout << "PollEventBuffer: CurrentEvent  " << fDigiEvent << " "<< fCurrentEventChanged << std::endl;    
    return fCurrentEventChanged;
}

Bool_t TELL1RawDecoder::DecodeNextEvent(Long_t * CurrentWord, Int_t iBurst) {
    UInt_t nReadEvent = 0;
    UInt_t datum;
    UInt_t nChannel,EventSize=0,tBoard=0;
    Long_t nValue;
    Bool_t en = 0;

    TDCEvent* TdcEvent = 0;
    fCurrentEventFlushed = kFALSE;

    printf_raw(en) ("DecodeTELL1Buffer ---- start -------- \n");

    for(; *CurrentWord < fNWords; (*CurrentWord)++) {
        //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0============== " << TdcEvent->GetID() << " " << *CurrentWord << std::endl;
        //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
        datum = *fpDataBuffer;
        printf_raw(en) (" DATUM: %ld 0x%08lx %d\n",(long)*CurrentWord,(unsigned long)datum,nReadEvent);
        if(isTELL1Header(datum)) {
            if(nReadEvent*4 != EventSize) std::cout << "WARNING: Event Size mismatch (" 
                << nReadEvent << " bytes instead of " << EventSize/4 << " bytes)" << std::endl;
                 //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0.1============== " << TdcEvent->GetID() << std::endl;
            //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0.1============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
            if(PollEventBuffer())
                return kTRUE;
            fNMEPs++;
            TdcEvent = 0;
            //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0.2============== " << TdcEvent->GetID() << std::endl;
            //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0.2============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
            nReadEvent = 1;
            EventSize = TELL1EventSize(datum);
            printf_raw(en) ("TELL1Header: EventSize %d, NTrig %d\n",EventSize,fNTrig);
            if(fTriggerLess){
                if(fNTrig == -1 || fTdcEvents->GetEntries() == 0){
                    TdcEvent=static_cast<TDCEvent*>( new ((*fTdcEvents)[0]) TDCEvent(TDCVHit::Class()));
                    fTdcEventStatus.push_back(0xf);
                    TdcEvent->SetTimeStamp(0);
                    TdcEvent->SetID(fNTrig);
                    TdcEvent->SetBurstID(iBurst);
                    TdcEvent->Clear();
                    fNTrig++;
                    if(en) std::cout << "TELL1Header: EventBufferSize " << fTdcEvents->GetEntries() << ", Event ID "  
                        << TdcEvent->GetID() << ", TimeStamp " 
                        << TdcEvent->GetTimeStamp() << ", " 
                        << TdcEvent << " "
                        << std::endl;
                }else if(TdcEvent == 0){
                    TdcEvent=static_cast<TDCEvent*>(fTdcEvents->Last());
                }
            }else{
                TdcEvent->Clear();
                TdcEvent->SetID(fNTrig);
                fNTrig++;
            }
            //if(en) std::cout << TdcEvent << " $$$$$$$$$1============== " << TdcEvent->GetID() << std::endl;
            //if(en) std::cout << TdcEvent << " $$$$$$$$$1============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
        }
        else if(isTELL1Error(datum)) { 
            nReadEvent++;
            printf_raw(en) ("TELL1Error \n");
        }
        else if(isTELL1Measurement(datum)) {
            //if(en) std::cout << TdcEvent << " $$$$$$$$$3============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
            nReadEvent++;
            nChannel = TELL1ChannelNumber(datum); 
            nChannel = fChannelRemap[nChannel];
            nValue = TELL1ChannelValue(datum);
            Int_t iTimeStamp = 0;
            if(fTriggerLess){
                for(iTimeStamp = 0; iTimeStamp < (Int_t) fTdcEvents->GetEntries(); iTimeStamp++){
                    if(en) std::cout << "TELL1Measurement: EventBufferScan: " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
                        << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetID() << " " 
                        << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " 
                        << (*fTdcEvents)[iTimeStamp] << " " << ftTime[(Int_t)(fChannelRO[nChannel]/128)] << " "
                        << fTdcEventStatus[iTimeStamp] << std::endl;
                    if(((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() == (ULong_t)ftTime[(Int_t)(fChannelRO[nChannel]/128)]){
                        TdcEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp]);
                        //if(en) std::cout << "///////////||||||||| " << TdcEvent << std::endl;
                        //                fTdcEventStatus[iTimeStamp]|=(1<<tBoard);
                        break;
                    }
                }
            }
            //if(en) std::cout << TdcEvent << " $$$$$$$$$4============== " << std::endl;
            //if(en) std::cout << TdcEvent << " $$$$$$$$$4============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
            TDCVHit * LastHit = static_cast<TDCVHit*>( TdcEvent->GetLastHitOnChannel(nChannel));
            if(!(fBothEdges && LastHit) ||
                    (!isTELL1Trailing(datum) && LastHit->GetDetectedEdge() & 1) ||
                    (LastHit->GetDetectedEdge() & 2)
              ){
                if(iTimeStamp > 0){
                    TDCVHit * LastHitInPrevTimeStamp = static_cast<TDCVHit*>( static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp-1])->GetLastHitOnChannel(nChannel));
                    if((fBothEdges && LastHitInPrevTimeStamp) &&
                            LastHitInPrevTimeStamp->GetLeadingEdge() > 40000 && 
                            (isTELL1Trailing(datum) && LastHitInPrevTimeStamp->GetDetectedEdge() == 1)
                      ){
                        TdcEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp-1]);
                    }else{
                        TDCVHit * Hit = static_cast<TDCVHit*>(TdcEvent->AddHit(nChannel));
                        Hit->SetMCTrackID(fiTime[(Int_t)(fChannelRO[nChannel]/128)]);
                    }
                }else{
                    TDCVHit * Hit = static_cast<TDCVHit*>(TdcEvent->AddHit(nChannel));
                    Hit->SetMCTrackID(fiTime[(Int_t)(fChannelRO[nChannel]/128)]);
                }
            }
            TDCVHit * TdcHit = static_cast<TDCVHit*>(TdcEvent->GetLastHitOnChannel(nChannel));
            if(!isTELL1Trailing(datum)){
                TdcHit->SetLeadingEdge(nValue*TdcCalib);
                TdcHit->UpdateDetectedEdge(1);
            }else{
                if(nValue*TdcCalib - TdcHit->GetLeadingEdge() < -10000)
                    TdcHit->SetTrailingEdge((nValue+524287)*TdcCalib);
                else
                    TdcHit->SetTrailingEdge(nValue*TdcCalib);
                TdcHit->UpdateDetectedEdge(2);
            }
            printf_raw(en) ("Measurement(%d,%d): nChannel %d(%d), nValue(%ld) %10.3f TimeStamp %ld\n",fNTrig,TdcEvent->GetNHits(),
                    nChannel,TdcHit->GetDetectedEdge(),nValue,nValue*TdcCalib,TdcEvent->GetTimeStamp());
            //if(en) std::cout << TdcEvent << " $$$$$$$$$5============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
        }
        else if(isTELL1TimeStamp(datum)) {
            nReadEvent++;
            tBoard = TELL1TimeStampBoard(datum); 
            Int_t TimeStampDiff=TELL1TimeStamp(datum) - ftTime[tBoard];
            fiTime[tBoard]++;
            printf_raw(en)("TELL1TimeStamp: # of TimeStamps %d DeltaTimeStamp %d for board %d\n",
                    (int)fTdcEvents->GetEntries(),TimeStampDiff,tBoard);
            if(fTriggerLess && TimeStampDiff >=1){
                TdcEvent = 0;
                for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t) fTdcEvents->GetEntries(); iTimeStamp++){
                    if(((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() == TELL1TimeStamp(datum)){
                        TdcEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp]);
                        //cout << "/////////// " << TdcEvent << std::endl;
                        fTdcEventStatus[iTimeStamp]|=(1<<tBoard);
                        break;
                    }   
                }
                if(TdcEvent == 0){
                    for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t) fTdcEvents->GetEntries(); iTimeStamp++)
                        if(((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() > TELL1TimeStamp(datum)){
                            TdcEvent = static_cast<TDCEvent*>( new ((*fTdcEvents)[fTdcEvents->GetEntries()]) TDCEvent(TDCVHit::Class()));
                            TdcEvent->SetTimeStamp(TELL1TimeStamp(datum));
                            fTdcEvents->Sort();
                            fTdcEventStatus.insert(fTdcEventStatus.begin()+iTimeStamp, 1<<tBoard);
		            if(TdcEvent != (TDCEvent*)(*fTdcEvents)[iTimeStamp]){
                                std::cout << "Sort error" << std::endl;
                                exit(kGenericError);
                            }
                            break;
                        }
                    if(TdcEvent == 0){
                        TdcEvent=static_cast<TDCEvent*>( new ((*fTdcEvents)[fTdcEvents->GetEntries()]) TDCEvent(TDCVHit::Class()));
                        fTdcEventStatus.push_back(1<<tBoard);
                        TdcEvent->SetTimeStamp(TELL1TimeStamp(datum));
                    }
                    TdcEvent->SetID(fNTrig);
                    TdcEvent->SetBurstID(iBurst);
                    TdcEvent->Clear();
                    fNTrig++;
                    if(en) std::cout << "TELL1TimeStamp: Adding event " << fTdcEvents->GetEntries() << " "  
                        << TdcEvent->GetID() << " " 
                        << TdcEvent->GetTimeStamp() << " " 
                        << TdcEvent << " "
                        << std::endl;
                }
            //if(en) std::cout << TdcEvent << " $$$$$$$$$6============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
                fiTime[tBoard] = 0;
                for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t) fTdcEvents->GetEntries(); iTimeStamp++){
                    if(((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() == (ULong_t)ftTime[tBoard]){
                        TDCEvent * TdcEventOld=static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp]);
                            Int_t nhits = TdcEventOld->GetNHits();
                            if(nhits>0)
                                for(Int_t iHit = 0; iHit < nhits; iHit++){ // loop on hits for current channel
                                    TDCVHit * TdcHitOld = TdcEventOld->GetHit(iHit);
                                    if(fChannelRO[TdcHitOld->GetChannelID()] < (Int_t)tBoard*128 ||
                                            fChannelRO[TdcHitOld->GetChannelID()] >= (Int_t)(127 + tBoard*128))
                                        continue;
                                    if(ftTime[tBoard] == 0 ||  
                                                ((( (TdcHitOld->GetLeadingEdge() < 20000) | ((TdcHitOld->GetTrailingEdge() < 20000)<<1) ) &  
                                                  TdcHitOld->GetDetectedEdge()
                                                 ) == TdcHitOld->GetDetectedEdge() &&
//         (TdcHitOld->GetLeadingEdge()<20000 && TdcHitOld->GetDetectedEdge() & 1) ||
//                                                (TdcHitOld->GetTrailingEdge()<20000 && TdcHitOld->GetDetectedEdge() & 2)) &&
                                                TdcHitOld->GetMCTrackID() >= 4)
                                        ){
                                        printf_raw(en)("TELL1TimeStamp: moving measurement nChannel %d(%d), %7.2f - %7.2f from TimeStamp %ld to %ld\n",
                                                TdcHitOld->GetChannelID(),TdcHitOld->GetDetectedEdge(),TdcHitOld->GetLeadingEdge(),
                                                TdcHitOld->GetTrailingEdge(),ftTime[tBoard],TELL1TimeStamp(datum));
                                        
                                        TDCVHit * TdcHit = static_cast<TDCVHit*>(TdcEvent->AddHit(TdcHitOld->GetChannelID()));
                                        TdcHit->SetDetectedEdge(TdcHitOld->GetDetectedEdge());
                                        TdcHit->SetLeadingEdge(TdcHitOld->GetLeadingEdge());
                                        TdcHit->SetTrailingEdge(TdcHitOld->GetTrailingEdge());
                                        TdcHit->SetMCTrackID(0);
                                        TdcEventOld->RemoveHit(iHit);
                                        //------------------------------delete TdcHitOld;
                                        nhits=TdcEventOld->GetNHits();
                                        iHit--;
                                        if(nhits == 0) break;
                                    }
                                }
                    }
                }
            }
            ftTime[tBoard] = TELL1TimeStamp(datum); 
            printf_raw(en)("TELL1TimeStamp: tTime %ld(%d) for board %d\n",ftTime[tBoard],fiTime[tBoard],tBoard);
            //if(en) std::cout << TdcEvent << " $$$$$$$$$7============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
        }else if(isTELL1DummyWord(datum)) { 
            nReadEvent++;
            printf_raw(en) ("DummyWord \n");
        }else{
            nReadEvent++;
            printf_raw(en) ("Unknown Word \n");
        }
        fpDataBuffer++;      
        //if(en) std::cout << TdcEvent << " $$$$$$$$$8============== " << TdcEvent->GetID() << std::endl;
        //if(en) std::cout << TdcEvent << " $$$$$$$$$8============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
    } 

    printf_raw(en) ("DecodeTELL1Buffer ---- end ---------- \n");

    return (*CurrentWord < fNWords ? kTRUE : kFALSE);

}
