#include "CAENRawDecoder.hh"
#include "CAENBuffer.h"
#include "CAENBufferProto.h"

#include "Riostream.h"

#define printf_raw(x) if((x)) printf

CAENRawDecoder::CAENRawDecoder(Bool_t TriggerLess, Bool_t BothEdges) :
    NA62VRawDecoder(0, "CAEN"),
    fTriggerLess(TriggerLess),
    fBothEdges(BothEdges),
    fNTrig(0),
    fpDataBuffer(nullptr),
    fNWords(0)
{
    fTdcEvents = new TClonesArray("TDCEvent",20);
    for(Int_t iCh = 0; iCh < 512; iCh++)
        fChannelRemap[iCh] = iCh;
    //for(Int_t iCh = 368; iCh < 384; iCh++){
    //    fChannelRemap[iCh] = iCh + 47;
    //    fChannelRemap[iCh + 47] = iCh;
    //}
}                        
                         
CAENRawDecoder::~CAENRawDecoder(){}
                         
void CAENRawDecoder::Reset(UInt_t * pDataBuffer, Int_t NWords)
{                        
    fpDataBuffer = pDataBuffer;
    fNWords = NWords;    
    for(Int_t iBoard = 0; iBoard < 4; iBoard++){
        ftTime[iBoard]=0;
        fiTime[iBoard]=0;
    }                    
    fTdcEvents->Clear("C");
    fTdcEventStatus.clear();
}                        
                         
TDCEvent* CAENRawDecoder::GetDecodedEvent()
{                        
    if(PollEventBuffer())
        return static_cast<TDCEvent*>(fDigiEvent);
    else                 
        return 0;        
}                        
                         
Bool_t CAENRawDecoder::PollEventBuffer()
{                        
    if(fDigiEvent){
        fTdcEvents->Remove(fDigiEvent);
        fTdcEvents->Compress();
    }
    Bool_t CurrentEventChanged = kFALSE;
    for(Int_t iTrig = 0; iTrig < (Int_t) fTdcEvents->GetEntries(); iTrig++)
        if(static_cast<TDCEvent*>((*fTdcEvents)[iTrig])->GetLastHitOnChannel(127) && static_cast<TDCEvent*>((*fTdcEvents)[iTrig])->GetLastHitOnChannel(255) &&
                static_cast<TDCEvent*>((*fTdcEvents)[iTrig])->GetLastHitOnChannel(383) && static_cast<TDCEvent*>((*fTdcEvents)[iTrig])->GetLastHitOnChannel(511)){
            fDigiEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTrig]);
            CurrentEventChanged = kTRUE;
            break;       
        }                
    return CurrentEventChanged;
}                        
                         
Bool_t CAENRawDecoder::DecodeNextEvent(Long_t * CurrentWord, Int_t iBurst)
{                        
  //UInt_t nRead = 0;    
    UInt_t nReadEvent = 0;
    UInt_t datum, wCount;
    //Int_t FormatOK=-1;
    Bool_t EventStatus = kFALSE;
    Int_t nSlot = 0, nTrig, iTrig;
    UInt_t nChannel;     
    Long_t nValue, tTime;
    Bool_t en = 0;       

    TDCEvent* TdcEvent = 0;

    printf_raw(en) ("DecodeCAENBuffer ---- start -------- \n");

    //nRead = 0;
    for(; *CurrentWord < fNWords; (*CurrentWord)++) { 
        datum = *fpDataBuffer;
        printf_raw(en) (" DATUM: 0x%08lx %d\n",(unsigned long)datum,nReadEvent);
        if(isGlobalHeader(datum) && !EventStatus) {
            //FormatOK = 0;
            nReadEvent = 1;
            EventStatus = kTRUE;
            nSlot = BoardNumber(datum);
            nTrig = TriggerNumber(datum);
            printf_raw(en) ("GlobalHeader: nSlot %d, nTrig %d\n",nSlot,nTrig);
            //if(fNTrig > 101000) en = 1;
            //if(fNTrig > 102000) break;
            //if(ftTime[0] > 510920) en = 1;
            //if(ftTime[0] > 511000) break;
            TdcEvent = 0;
            for(iTrig = 0; iTrig < (Int_t) fTdcEvents->GetEntries(); iTrig++)
                if(static_cast<TDCEvent*>((*fTdcEvents)[iTrig])->GetID() == nTrig){
                    TdcEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTrig]);
                    break;
                }
            if(TdcEvent == 0){
                TdcEvent=static_cast<TDCEvent*>( new ((*fTdcEvents)[0]) TDCEvent(TDCVHit::Class()));
                fNTrig++;
                TdcEvent->SetID(nTrig);
                TdcEvent->SetBurstID(iBurst);
                TdcEvent->Clear();
            }
            printf_raw(en) ("GlobalHeader: nTrigs %d, iTrig %d\n",(int)fTdcEvents->GetEntries(),iTrig);
        }
        else if(isTDCHeader(datum) && EventStatus) {
            nReadEvent++;
            printf_raw(en) ("GlobalHeader \n");
        }
        else if(isTDCTrailer(datum) && EventStatus) {
            nReadEvent++;
            printf_raw(en) ("GlobalTrailer \n");
        }
        else if(isTDCError(datum) && EventStatus) {
            nReadEvent++;
            printf_raw(en) ("TDCError \n");
        }
        else if(isMeasurement(datum) && EventStatus) {
            nReadEvent++;
            nChannel = ChannelNumber(datum) + (nSlot-1)*128;
            nChannel = fChannelRemap[nChannel];
            nValue = ChannelValue(datum);
            TDCVHit * Hit = static_cast<TDCVHit*>( TdcEvent->GetLastHitOnChannel(nChannel));
            if(!(fBothEdges && Hit) ||
                    (!isTrailing(datum) && Hit->GetDetectedEdge() & 1) ||
                    (Hit->GetDetectedEdge() & 2)
              )
                    Hit = static_cast<TDCVHit*>(TdcEvent->AddHit(nChannel));
            if(!isTrailing(datum)){
                Hit->SetLeadingEdge(nValue*TdcCalib);
                Hit->UpdateDetectedEdge(1);
                if((nChannel+1)%128==0){
                    Hit->SetTrailingEdge(nValue*TdcCalib+8);
                    Hit->UpdateDetectedEdge(2);
                }
            }else{
                Hit->SetTrailingEdge(nValue*TdcCalib);
                Hit->UpdateDetectedEdge(2);
            }
            printf_raw(en) ("Measurement(%d,%d): nChannel %d(%d), nValue(%ld) %10.3f\n",fNTrig,TdcEvent->GetNHits(),
                    nChannel,Hit->GetDetectedEdge(),nValue,nValue*TdcCalib);
        }
        else if(isTriggerTimeTag(datum) && EventStatus) {
            nReadEvent++;
            tTime = ExtendedTriggerTime(datum);
            printf_raw(en)("TriggerTimeTag: tTime %ld \n",tTime);
        }
        else if(isGlobalTrailer(datum) && EventStatus) {
            nReadEvent++;
            wCount = EventSize(datum);
            printf_raw(en) ("GlobalTrailer: wCount %ld \n",(long int)wCount);
            //if(TdcEvent->GetID()%1000==0)
            //    std::cout <<"Burst "<< TdcEvent->GetBurstID() << ": Tdc trigger " << TdcEvent->GetID() << std::endl;
            //if(wCount != nReadEvent) {
                //FormatOK = 0;
            //}
            //else {
                //FormatOK = 1;
            //}
            EventStatus = kFALSE;
            if(PollEventBuffer())
                return kTRUE;
        }
        else if(isFiller(datum)) {
            printf_raw(en) ("Filler \n");
        }
        else{
            nReadEvent++;
            printf_raw(en) ("Unknown Word \n");
        }

        //if(isGlobalHeader(datum)) {
        //    trigNprev = trigN;
        //    trigN = TriggerNumber(datum);
        //    if (trigN != trigNprev+1 && trigN>0) {
        //        TotTrigLost += trigN-trigNprev-1;
        //        printf_raw(0) ("Jump >>> trigN %ld -> %ld (tot lost %d)\n",trigNprev,trigN,TotTrigLost);
        //    }
        //}

        if(isFiller(datum)) break;

        fpDataBuffer++;      
    } 

    printf_raw(en) ("DecodeCAENBuffer ---- end ---------- \n");

    if(fTdcEvents->GetEntries() > 0 && *CurrentWord >= fNWords)
        std::cout << fTdcEvents->GetEntries() << " unmatched triggers" << std::endl;

    return (*CurrentWord < fNWords ? kTRUE : kFALSE);

}
