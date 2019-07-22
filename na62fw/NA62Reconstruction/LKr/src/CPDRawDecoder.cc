#include "CPDRawDecoder.hh"
#include "CPDBuffer.h"
#include "CPDBufferProto.h"
#include "FADCEvent.hh"
//#include "TLKrDigi.hh"
//#include "TLKrEvent.hh"
//#include "LKrDigitizer.hh"

#include "EventHeader.hh"

#include "Riostream.h"

#define printf_raw(x) if((x)) printf

CPDRawDecoder::CPDRawDecoder() :
    NA62VRawDecoder(0, "CPD"),
    fTriggerLess(false),
    fBothEdges(false),
    fNTrig(0),
    fpDataBuffer(nullptr),
    fNWords(0)
{
    fCPDEvent = new FADCEvent(TLKrDigi::Class());
}

CPDRawDecoder::~CPDRawDecoder(){}

void CPDRawDecoder::Reset(UInt_t * pDataBuffer, Int_t NWords)
{
    fpDataBuffer = pDataBuffer;
    fNWords = NWords;
}

void CPDRawDecoder::StartOfBurst() {
}

void CPDRawDecoder::EndOfBurst() {
}


TDetectorVEvent * CPDRawDecoder::DecodeNextEvent(UInt_t * pDataBuffer, EventHeader * pEventHeader, UInt_t * /*NextOffset*/)
{
    UInt_t nReadEvent = 0;
    UInt_t datum;
    //Int_t FormatOK=-1;
    Int_t  nTrig;
    //Bool_t en = 1;
    Bool_t en = 0;
    UInt_t nTrigWord, nTimeStamp, nRioHeader, nRioTrailer;
    UShort_t nChannels;
    UInt_t nBlockLength, nSubDetLength;
    UInt_t CPDIdentifier, sampword1, sampword2, sampword3, s;
    Bool_t DecodeChannel;
    UInt_t Gain, Amplitude;

    printf_raw(en) ("DecodeCPDBuffer ---- start -------- \n");
    fCPDEvent->Clear("C");
//  Format checks   to be adjusted to exit properly
    if ((*pDataBuffer&0xff000000)!=0x24000000) printf_raw(en) ("Wrong detector id %x\n",*pDataBuffer);
    if ((*pDataBuffer&0x00ffffff)!=pEventHeader->GetEventNumber()) printf_raw(en) ("Wrong event number %x\n",*pDataBuffer);
    if (*(pDataBuffer+2)!=pEventHeader->GetTimeStamp()) printf_raw(en) ("Wrong timestamp %x\n",*(pDataBuffer+2));

    nSubDetLength = (*(pDataBuffer+1))&0x00ffffff;
    if (nSubDetLength<=4) {printf_raw(en) ("Empty Event\n"); return 0;}
    pDataBuffer += 4;

    datum = *pDataBuffer;
    printf_raw(en) (" DATUM: 0x%08lx %d\n",(unsigned long)datum,nReadEvent);
    if(isLKrBlock(datum)) {
        //FormatOK = 0;

        nTrig = TrigNumber(datum);
        pDataBuffer++; datum = *pDataBuffer;
        nTrigWord = TrigWord(datum);
        if (TrigNumber(datum)!=(unsigned int) nTrig) std::cout << "Trigger number Error" << std::endl;
        pDataBuffer++; datum = *pDataBuffer;
        nTimeStamp = TimeStamp(datum);
        // Skip the two ghost words
        pDataBuffer++; pDataBuffer++; pDataBuffer++;
        nReadEvent = 5;

        printf_raw(en) ("GlobalHeader:  nTrig %d nTrigWord %x nTimeStamp %x\n",nTrig,nTrigWord,nTimeStamp);


        //            printf_raw(en) ("GlobalHeader: nTrigs %d, iTrig %d\n",(int)fFadcEvents->GetEntries(),iTrig);
    }
    Int_t nch=1;
    for (UInt_t rio=0; rio<NRIO; rio++) {
        // skip superheader and dcpheader
        pDataBuffer++; pDataBuffer++; pDataBuffer++;
        datum = *pDataBuffer; pDataBuffer++;
        nRioHeader = RioHeader(datum, (unsigned long)rio);
        nChannels   = RioNChannels(datum);
        nReadEvent += 4;
        printf_raw(en) (" Rio %d Header  %x  channels %d\n",rio,nRioHeader,nChannels);

        for (UShort_t chan=0; chan<nChannels; chan++) {
            datum = *pDataBuffer; pDataBuffer++;
            DecodeChannel = isToBeDecoded(datum);
            if (DecodeChannel) {
                TLKrDigi * LKrDigi = static_cast<TLKrDigi*>(fCPDEvent->AddHit());
            //cout << "Nhits " << fCPDEvent->GetNHits() << std::endl;
                CPDIdentifier = ChannelId (datum);
                LKrDigi->SetCPDID(scpd_scha(CPDIdentifier));
                //cout << "LKrDigi, CPDID " << LKrDigi << " " << LKrDigi->GetCPDID() << std::endl;
                LKrDigi->SetChannelID(schacpd_scha(CPDIdentifier));
                //cout << "LKrDigi, nsamples " << LKrDigi << " " << LKrDigi->GetNSamples() << std::endl;
                sampword1 = *pDataBuffer; pDataBuffer++;
                sampword2 = *pDataBuffer; pDataBuffer++;
                sampword3 = *pDataBuffer; pDataBuffer++;
                // here decode samples and fill output variables
                s = sampword1 & 0x00000fff;
                Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
                LKrDigi->AddSample((Gain<<16)|Amplitude);

                s = (sampword1 >> 12) & 0x00000fff;
                Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
                LKrDigi->AddSample((Gain<<16)|Amplitude);

                s = ((sampword1 >> 24) & 0x000000ff) | ((sampword2 << 8)&0x00000f00);
                Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
                LKrDigi->AddSample((Gain<<16)|Amplitude);

                s = (sampword2 >> 4) & 0x00000fff;
                Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
                LKrDigi->AddSample((Gain<<16)|Amplitude);

                s = (sampword2 >> 16) & 0x00000fff;
                Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
                LKrDigi->AddSample((Gain<<16)|Amplitude);

                s = ((sampword2 >> 28) & 0x0000000f) | ((sampword3 << 4)&0x00000ff0);
                Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
                LKrDigi->AddSample((Gain<<16)|Amplitude);

                s = (sampword3 >> 8) & 0x00000fff;
                Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
                LKrDigi->AddSample((Gain<<16)|Amplitude);

                s = (sampword3 >> 20) & 0x00000fff;
                Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
                LKrDigi->AddSample((Gain<<16)|Amplitude);

                printf_raw(en) (" nch %d CPD Id %d samples %x %x %x\n",nch,datum,sampword1,sampword2,sampword3);
                nch++;
            }
            nReadEvent += 4;
        }
        datum = *pDataBuffer; pDataBuffer++;
        nRioTrailer = RioTrailer(datum,(unsigned long) rio); nReadEvent++;
        printf_raw(en) (" Rio %d Trailer  %x channels %d\n",rio,nRioTrailer,nChannels);
    }
    pDataBuffer++;  // contains feed marker
    datum = *pDataBuffer; pDataBuffer++;
    nReadEvent += 2;
    nBlockLength = BlockLength(datum);
    if (nBlockLength!=nReadEvent) std::cout << "Length mismatch: block "<<nBlockLength <<" read " << nReadEvent << std::endl;
    if (nSubDetLength-4!=nReadEvent) std::cout << "Length mismatch: SubDet "<<nSubDetLength <<" read " << nReadEvent << std::endl;

    printf_raw(en) ("DecodeCPDBuffer ---- end ---------- \n");

    return fCPDEvent;
}
