#include "SLMRawDecoder.hh"
#include "SLMBuffer.h"
#include "CPDBufferProto.h"
#include "SLMBufferProto.h"
#include "FADCEvent.hh"
#include "LKrCommon.hh"
#include "EventHeader.hh"
#include "Riostream.h"

#define dec000 dec000_
#define type_of_call
extern "C" {void type_of_call dec000(UInt_t&, UInt_t&, UInt_t*, UInt_t*, UInt_t&);}

#define printf_raw(x) if((x)) printf

SLMRawDecoder::SLMRawDecoder() :
    NA62VRawDecoder(0, "SLM"),
    fTriggerLess(false),
    fBothEdges(false),
    fNTrig(0),
    fpDataBuffer(nullptr),
    fNWords(0)
{
    fSLMEvent = new FADCEvent(TLKrDigi::Class());
    fAdcCommon = LKrCommon::GetInstance();
}

SLMRawDecoder::~SLMRawDecoder(){}

void SLMRawDecoder::Reset(UInt_t * pDataBuffer, Int_t NWords)
{
    fpDataBuffer = pDataBuffer;
    fNWords = NWords;
}

void SLMRawDecoder::StartOfBurst() {
}

void SLMRawDecoder::EndOfBurst() {
}

TDetectorVEvent * SLMRawDecoder::DecodeNextEvent(UInt_t * pDataBuffer, EventHeader * pEventHeader, UInt_t * /*NextOffset*/)
{
    UInt_t nReadEvent;
    UInt_t datum;
    //    Bool_t EventStatus = kFALSE;
    Bool_t en = 0;
    UShort_t nChannels;
    UInt_t nch=0;
    UInt_t nSubDetLength;
    UInt_t nSLM;
    UInt_t SLMIdentifier, sampword1, sampword2, sampword3, s;
    Bool_t DecodeChannel;
    UInt_t Gain, Amplitude;
    UInt_t xcha, ycha, nsamples, count[256], gain[256];

    printf_raw(en) ("DecodeSLMBuffer ---- start -------- \n");
    fSLMEvent->Clear("C");
    while ((*pDataBuffer&0xff000000)==0x24000000) {

//  Format checks   to be adjusted to exit properly
    if ((*pDataBuffer&0x00ffffff)!=pEventHeader->GetEventNumber()) printf_raw(en) ("Wrong event number %x\n",*pDataBuffer);
    if (*(pDataBuffer+2)!=pEventHeader->GetTimeStamp()) printf_raw(en) ("Wrong timestamp %d\n",*(pDataBuffer+2));
    fSLMEvent->SetID((*pDataBuffer&0x00ffffff));
    fSLMEvent->SetTimeStamp(*(pDataBuffer+2));

    nSubDetLength = (*(pDataBuffer+1))&0x00ffffff;
    if (nSubDetLength<=5) {printf_raw(en) ("Empty Event\n"); return fSLMEvent;}
    //    printf ("SLM ID %d\n",*(pDataBuffer+3));
    nSLM = *(pDataBuffer+4);
    if (nSLM <=0) {printf_raw(en) ("Wrong number of SLMs\n"); return fSLMEvent;}
    else printf_raw(en) ("Read %d SLMs\n",nSLM);
    pDataBuffer += 5;
    nReadEvent = 5;

    // Some settings
    fSLMEvent->SetNSamples(8);
    fSLMEvent->SetFADCID(10);

    for (UInt_t iSLM=0; iSLM<nSLM; iSLM++) {
       datum = *pDataBuffer;
       if (!isSLMGoodMarker(datum)) printf_raw(en) (" Wrong SLM marker\n");
// skip for the moment the decoding of SLM id and SCPD reference
       pDataBuffer++; datum = *pDataBuffer;
       nChannels = SLMNChannels(datum);
       pDataBuffer++; nReadEvent += 2;
       for (UShort_t chan=0; chan<nChannels; chan++) {
	      datum = *pDataBuffer; pDataBuffer++;
	      DecodeChannel = isSLMToBeDecoded(datum);
	      if (DecodeChannel) {
		TLKrDigi * LKrDigi = static_cast<TLKrDigi*>(fSLMEvent->AddDigi());
		SLMIdentifier = SLMChannelId (datum);
		LKrDigi->SetCPDID(scpd_scha(SLMIdentifier));
		LKrDigi->SetChannelID(schacpd_scha(SLMIdentifier));
                LKrDigi->SetCPDChannelID(schacpd_scha(SLMIdentifier));
                xcha = xcha_scha (SLMIdentifier);
		ycha = ycha_scha (SLMIdentifier);
		nsamples = 8;
////	        std::cout << "Nhits " << fSLMEvent->GetNHits();
////		cout << " CPDID " << LKrDigi->GetCPDID();
////		cout << " nsamples " << LKrDigi->GetNSamples() << " " << LKrDigi->GetChannelID() << std::endl;
		sampword1 = *pDataBuffer; pDataBuffer++;
		sampword2 = *pDataBuffer; pDataBuffer++;
 		sampword3 = *pDataBuffer; pDataBuffer++;
		// here decode samples and fill output variables
		int is = 0;
		s = sampword1 & 0x00000fff;
		Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
		LKrDigi->AddSample(Gain*10000+Amplitude);
		count[is] = Amplitude; gain[is] = Gain; is++;

	        s = (sampword1 >> 12) & 0x00000fff;
		Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
		LKrDigi->AddSample(Gain*10000+Amplitude);
 	        count[is] = Amplitude; gain[is] = Gain; is++;

		s = ((sampword1 >> 24) & 0x000000ff) | ((sampword2 << 8)&0x00000f00);
		Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
		LKrDigi->AddSample(Gain*10000+Amplitude);
 	        count[is] = Amplitude; gain[is] = Gain; is++;

		s = (sampword2 >> 4) & 0x00000fff;
		Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
		LKrDigi->AddSample(Gain*10000+Amplitude);
		count[is] = Amplitude; gain[is] = Gain; is++;

		s = (sampword2 >> 16) & 0x00000fff;
		Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
		LKrDigi->AddSample(Gain*10000+Amplitude);
	        count[is] = Amplitude; gain[is] = Gain; is++;

		s = ((sampword2 >> 28) & 0x0000000f) | ((sampword3 << 4)&0x00000ff0);
		Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
		LKrDigi->AddSample(Gain*10000+Amplitude);
		count[is] = Amplitude; gain[is] = Gain; is++;

		s = (sampword3 >> 8) & 0x00000fff;
		Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
		LKrDigi->AddSample(Gain*10000+Amplitude);
		count[is] = Amplitude; gain[is] = Gain; is++;

		s = (sampword3 >> 20) & 0x00000fff;
		Gain = (s&0x00000c00)>>10; Amplitude = s&0x000003ff;
		LKrDigi->AddSample(Gain*10000+Amplitude);
		count[is] = Amplitude; gain[is] = Gain; is++;

		printf_raw(en) (" nch %d Ch Id %d samples %x %x %x\n",nch,datum,sampword1,sampword2,sampword3);
//                Int_t thisgain = 0;
//                for (Int_t i=0;i<8;i++)
//                {
//                  if (gain[i]==2) thisgain = 3;
//                }
//                if (thisgain==3)
//                {
//                  for (Int_t i=0;i<8;i++) std::cout << LKrDigi->GetCPDID() << " " << LKrDigi->GetChannelID() << " " << count[i]+10000*gain[i] << " ";
//                  std::cout << std::endl;
//                }
		nch++;
	      	dec000(xcha,ycha, count,gain,nsamples);
                LKrDigi->SetADCPeakEnergy((Double_t)fAdcCommon->GetADC()->PEAKENE);
                LKrDigi->SetADCPeakTime((Double_t)fAdcCommon->GetADC()->PEAKTIME-1); // Warning! peaktime = maxsample -> -1 from fortran to c++:
                                                                                  // samples in fortran 1:8, in c 0:7
                LKrDigi->SetQuality(fAdcCommon->GetADC()->IQUALITY);
                LKrDigi->SetFlags(fAdcCommon->GetADC()->IFLAG);
	      }
              else pDataBuffer +=4;

	      nReadEvent += 4;
       } // End loop on channels
    }	 // End loop on SLMs

	if (nSubDetLength!=nReadEvent) std::cout << "Length mismatch: read from block "<<nSubDetLength <<"  words processed " << nReadEvent << std::endl;
	//    pDataBuffer += nSubDetLength;
    } //end of while on 0x24
    printf_raw(en) ("DecodeSLMBuffer ---- end ---------- \n");

    return fSLMEvent;

}
