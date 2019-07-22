// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-05-15
//
// ---------------------------------------------------------------

#include "L0TPSpecialTrigger.hh"

#include "Riostream.h"

ClassImp(L0TPSpecialTrigger)

L0TPSpecialTrigger::L0TPSpecialTrigger(){
  Clear();
}                        

L0TPSpecialTrigger::~L0TPSpecialTrigger(){}

void L0TPSpecialTrigger::Clear(Option_t* /*option*/){

  fNMaxDetectors = 7;

  // Subdetector Block Header (generic part)
  fEventLength = 0;
  fDetectorID = 0;
  fDataBlockFormat = 0;
  fTimeStamp = 0;
  fNBlockHeaderWords = 0;

  // Subdetector Block Header (L0TP-specific part)
  fDataType = 0;            
  fLatency = 0;             
  fFineTimeBits = 0;        
  fReferenceDetector = 0;  
  fControlDetector = 0;  
  fPreviousTimeStamp = 0;   
  fTriggerType = 0;         
  fPreviousTriggerType = 0; 
  fTriggerFlags = 0;        
  fNPrimitives.clear();
  fNChokeTriggers = 0;      
  fNErrorTriggers = 0;      
  fNPeriodicTriggers = 0;     
  fNCalibTriggers = 0;      
  fNControlTriggersSent = 0;
  fNControlTriggersGenerated = 0;  
  fControlTriggerDownscalingFactor = 0;
  fReserved = 0;
  // Info from the L0 masks
  fL0Masks.clear();
}

Bool_t L0TPSpecialTrigger::SetHeader(UInt_t * pDataBuffer){

  Clear(); //Reset L0 info

  // Subdetector Block Header (generic part)
  fEventLength         = (*(pDataBuffer+O_L0EVENTLENGTH)&M_L0EVENTLENGTH)>>S_L0EVENTLENGTH;
  fDetectorID          = (*(pDataBuffer+O_L0DETECTORID)&M_L0DETECTORID)>>S_L0DETECTORID;
  fDataBlockFormat     = (*(pDataBuffer+O_L0DATABLOCKFORMAT)&M_L0DATABLOCKFORMAT)>>S_L0DATABLOCKFORMAT;
  fNBlockHeaderWords = 1;
  if(fDataBlockFormat==1) { // 2015 format: read the 32-bit timestamp
    fTimeStamp         = (*(pDataBuffer+O_L0TIMESTAMP)&M_L0TIMESTAMP)>>S_L0TIMESTAMP;
    fNBlockHeaderWords++;
  }

  // Subdetector Block Header (L0TP-specific part)
  fDataType               = (*(pDataBuffer+fNBlockHeaderWords+O_L0DATATYPE)&M_L0DATATYPE)>>S_L0DATATYPE;
  fTriggerType            = (*(pDataBuffer+fNBlockHeaderWords+O_L0TRIGTYPE)&M_L0TRIGTYPE)>>S_L0TRIGTYPE;
  fTriggerFlags           = (*(pDataBuffer+fNBlockHeaderWords+O_L0TRIGFLAGS)&M_L0TRIGFLAGS)>>S_L0TRIGFLAGS;

  if((fTriggerType&0xff)==0x23){
    // --- EOB-exclusive info
    fLatency              = (*(pDataBuffer+fNBlockHeaderWords+O_L0LATENCY)&M_L0LATENCY)>>S_L0LATENCY;
    fFineTimeBits         = (*(pDataBuffer+fNBlockHeaderWords+O_L0FINETIMEBITS)&M_L0FINETIMEBITS)>>S_L0FINETIMEBITS;
    fReferenceDetector    = (*(pDataBuffer+fNBlockHeaderWords+O_L0REFDET)&M_L0REFDET)>>S_L0REFDET;
    fControlDetector      = (*(pDataBuffer+fNBlockHeaderWords+O_L0CTRLDET)&M_L0CTRLDET)>>S_L0CTRLDET;
    fControlTriggerDownscalingFactor = (*(pDataBuffer+fNBlockHeaderWords+O_L0CTRLDWN)&M_L0CTRLDWN)>>S_L0CTRLDWN;
    fNControlTriggersGenerated = (*(pDataBuffer+fNBlockHeaderWords+O_L0NCTRLTRIGGEN)&M_L0NCTRLTRIGGEN)>>S_L0NCTRLTRIGGEN;
    fPreviousTimeStamp    = (*(pDataBuffer+fNBlockHeaderWords+O_L0PREVTIMESTAMP)&M_L0PREVTIMESTAMP)>>S_L0PREVTIMESTAMP;
    fPreviousTriggerType  = (*(pDataBuffer+fNBlockHeaderWords+O_L0PREVTRIGTYPE)&M_L0PREVTRIGTYPE)>>S_L0PREVTRIGTYPE;
    // Fill fNPrimitives
    for(UInt_t iPrimOffset=0;fNPrimitives.size()<fNMaxDetectors;iPrimOffset++){
      fNPrimitives.push_back((*(pDataBuffer+fNBlockHeaderWords+O_L0NPRIMITIVES+iPrimOffset)&M_L0NPRIMITIVES)>>S_L0NPRIMITIVES);
    }
    fNChokeTriggers       = (*(pDataBuffer+fNBlockHeaderWords+O_L0NCHOKES)&M_L0NCHOKES)>>S_L0NCHOKES;
    fNErrorTriggers       = (*(pDataBuffer+fNBlockHeaderWords+O_L0NERRORS)&M_L0NERRORS)>>S_L0NERRORS;
    fNPeriodicTriggers    = (*(pDataBuffer+fNBlockHeaderWords+O_L0NPERIODIC)&M_L0NPERIODIC)>>S_L0NPERIODIC;
    fNCalibTriggers       = (*(pDataBuffer+fNBlockHeaderWords+O_L0NCALIB)&M_L0NCALIB)>>S_L0NCALIB;
    fNControlTriggersSent = (*(pDataBuffer+fNBlockHeaderWords+O_L0NCTRLTRIGSNT)&M_L0NCTRLTRIGSNT)>>S_L0NCTRLTRIGSNT;
    // Info from the L0 masks
    UInt_t NMaskWords = 0;
    for(UInt_t iMask=0;iMask<16;iMask++){
      if(fNBlockHeaderWords+NMaskWords+O_L0MASK_ID>=fEventLength/4) continue; //continue if less than 16 masks in data
      L0Mask Mask;
      Mask.SetMaskID((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L0MASK_ID)&M_L0MASK_ID)>>S_L0MASK_ID);
      Mask.SetDownscalingFactor((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L0MASK_DWN)&M_L0MASK_DWN)>>S_L0MASK_DWN);
      Mask.SetNTriggersSent((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L0MASK_NTRGSENT)&M_L0MASK_NTRGSENT)>>S_L0MASK_NTRGSENT);
      Mask.SetNTriggersGenerated((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L0MASK_NTRGGENERATED)&M_L0MASK_NTRGGENERATED)>>S_L0MASK_NTRGGENERATED);
      std::vector<UInt_t> RequiredPrimBitMask;
      std::vector<UInt_t> DontcarePrimBitMask;
      RequiredPrimBitMask.clear();
      DontcarePrimBitMask.clear();
      for(UInt_t iDetector=0;iDetector<fNMaxDetectors;iDetector++){
        RequiredPrimBitMask.push_back((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L0MASK_REQUIREDPRIMBIT+iDetector)&M_L0MASK_REQUIREDPRIMBIT)>>S_L0MASK_REQUIREDPRIMBIT);
        DontcarePrimBitMask.push_back((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L0MASK_DONTCAREPRIMBIT+iDetector)&M_L0MASK_DONTCAREPRIMBIT)>>S_L0MASK_DONTCAREPRIMBIT);
      }
      Mask.SetRequiredPrimBitMask(RequiredPrimBitMask);
      Mask.SetDontcarePrimBitMask(DontcarePrimBitMask);
      fL0Masks.push_back(Mask);
      NMaskWords+=12; //12 32-bit words for each mask
    }
    std::cout << "[L0TPEOB] >>> Enabled L0 masks:" << std::endl;
    for(UInt_t iMask=0;iMask<fL0Masks.size();iMask++){
      if(fL0Masks[iMask].GetNTriggersSent()){
        std::cout << "[L0TPEOB] --> Mask " << iMask << " MaskID:             " << fL0Masks[iMask].GetMaskID() << std::endl;
        std::cout << "[L0TPEOB]     Mask " << iMask << " DownscalingFactor:  " << fL0Masks[iMask].GetDownscalingFactor() << std::endl;
        std::cout << "[L0TPEOB]     Mask " << iMask << " NTriggersSent:      " << fL0Masks[iMask].GetNTriggersSent() << std::endl;
        std::cout << "[L0TPEOB]     Mask " << iMask << " NTriggersGenerated: " << fL0Masks[iMask].GetNTriggersGenerated() << std::endl;
        //for(UInt_t iDetector=0;iDetector<fNMaxDetectors;iDetector++){
        //  std::cout << "[L0TPEOB]     Mask " << iMask << " Detector " << iDetector << " DontcareBits: " << std::hex << fL0Masks[iMask].GetDontcarePrimBitMask()[iDetector] << " RequiredBits: " << fL0Masks[iMask].GetRequiredPrimBitMask()[iDetector] << std::dec << std::endl;
        //}
      }
    }
    std::cout << "[L0TPEOB] >>> Control Trigger:" << std::endl;
    std::cout << "[L0TPEOB]     DownscalingFactor:  " << fControlTriggerDownscalingFactor << std::endl;
    std::cout << "[L0TPEOB]     NTriggersSent:      " << fNControlTriggersSent << std::endl;
    std::cout << "[L0TPEOB]     NTriggersGenerated: " << fNControlTriggersGenerated << std::endl;
  }
  return kTRUE;
}

ClassImp(L0Mask)

L0Mask::L0Mask(){
  Clear();
}

L0Mask::L0Mask(const L0Mask &c) : TObject(c), fMaskID(c.fMaskID), fDownscalingFactor(c.fDownscalingFactor), fNTriggersSent(c.fNTriggersSent), fNTriggersGenerated(c.fNTriggersGenerated), fRequiredPrimBitMask(c.fRequiredPrimBitMask), fDontcarePrimBitMask(c.fDontcarePrimBitMask)
{
}

void L0Mask::Clear(Option_t* /*option*/){
  fMaskID = 0;
  fDownscalingFactor = 0;
  fNTriggersSent = 0;
  fNTriggersGenerated = 0;
  fRequiredPrimBitMask.clear();
  fDontcarePrimBitMask.clear();
}
