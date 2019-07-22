// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-06-26
//
// ---------------------------------------------------------------

#include "L1TPData.hh"

#include "Riostream.h"

ClassImp(L1TPData)

L1TPData::L1TPData(): TObject(){
  Clear();
}                        

L1TPData::~L1TPData(){}

L1TPData::L1TPData(const L1TPData& c) : TObject(c), fEventLength(c.fEventLength), fDetectorID(c.fDetectorID), fDataBlockFormat(c.fDataBlockFormat), fTimeStamp(c.fTimeStamp), fNBlockHeaderWords(c.fNBlockHeaderWords), fL1Format(c.fL1Format), fL1FlagMode(c.fL1FlagMode), fL1ReferenceDetector(c.fL1ReferenceDetector), fL1ReferenceFineTime(c.fL1ReferenceFineTime), fL1GlobalHeaderLength(c.fL1GlobalHeaderLength), fL1ReductionFactor(c.fL1ReductionFactor), fL1DownscalingFactor(c.fL1DownscalingFactor), fL1AutoflagFactor(c.fL1AutoflagFactor), fL1BypassFactor(c.fL1BypassFactor), fL1NEnabledMasks(c.fL1NEnabledMasks), fL0Masks(c.fL0Masks){}

void L1TPData::Clear(Option_t* /*option*/){
  // Subdetector Block Header (generic part)
  fEventLength = 0;
  fDetectorID = 0;
  fDataBlockFormat = 0;
  fTimeStamp = 0;
  fNBlockHeaderWords = 0;

  // Subdetector Block Header (L1TP-specific part)
  fL1Format = 0;
  fL1FlagMode = 0;
  fL1ReferenceDetector = 0;
  fL1ReferenceFineTime = 0;
  fL1GlobalHeaderLength = 0;
  fL1ReductionFactor = 0;
  fL1DownscalingFactor = 0;
  fL1AutoflagFactor = 0;
  fL1BypassFactor = 0;
  fL1NEnabledMasks = 0;
  fL0Masks.clear();
}

Bool_t L1TPData::SetHeader(UInt_t * pDataBuffer){

  Clear(); //Reset L1 info

  // Subdetector Block Header (generic part)
  fEventLength         = (*(pDataBuffer+O_L1EVENTLENGTH)&M_L1EVENTLENGTH)>>S_L1EVENTLENGTH;
  fDetectorID          = (*(pDataBuffer+O_L1DETECTORID)&M_L1DETECTORID)>>S_L1DETECTORID;
  fDataBlockFormat     = (*(pDataBuffer+O_L1DATABLOCKFORMAT)&M_L1DATABLOCKFORMAT)>>S_L1DATABLOCKFORMAT;
  fNBlockHeaderWords = 1;
  if(fDataBlockFormat==1) { // 2015 format: read the 32-bit timestamp
    fTimeStamp         = (*(pDataBuffer+O_L1TIMESTAMP)&M_L1TIMESTAMP)>>S_L1TIMESTAMP;
    fNBlockHeaderWords++;
  }

  if(fEventLength<=12) return kTRUE; //2015 format: no L1 block

  // Subdetector Block Header (L1TP-specific part)
  fL1Format             = (*(pDataBuffer+fNBlockHeaderWords+O_L1FORMAT            )&M_L1FORMAT            )>>S_L1FORMAT            ;
  fL1FlagMode           = (*(pDataBuffer+fNBlockHeaderWords+O_L1FLAGMODE          )&M_L1FLAGMODE          )>>S_L1FLAGMODE          ;
  fL1ReferenceDetector  = (*(pDataBuffer+fNBlockHeaderWords+O_L1REFERENCEDETECTOR )&M_L1REFERENCEDETECTOR )>>S_L1REFERENCEDETECTOR ;
  fL1ReferenceFineTime  = (*(pDataBuffer+fNBlockHeaderWords+O_L1REFERENCEFINETIME )&M_L1REFERENCEFINETIME )>>S_L1REFERENCEFINETIME ;
  fL1GlobalHeaderLength = (*(pDataBuffer+fNBlockHeaderWords+O_L1GLOBALHEADERLENGTH)&M_L1GLOBALHEADERLENGTH)>>S_L1GLOBALHEADERLENGTH;
  fL1ReductionFactor    = (*(pDataBuffer+fNBlockHeaderWords+O_L1GLOBALREDUCTION   )&M_L1GLOBALREDUCTION   )>>S_L1GLOBALREDUCTION   ;
  fL1DownscalingFactor  = (*(pDataBuffer+fNBlockHeaderWords+O_L1GLOBALDOWNSCALING )&M_L1GLOBALDOWNSCALING )>>S_L1GLOBALDOWNSCALING ;
  fL1AutoflagFactor     = (*(pDataBuffer+fNBlockHeaderWords+O_L1AUTOFLAGFACTOR    )&M_L1AUTOFLAGFACTOR    )>>S_L1AUTOFLAGFACTOR    ;
  fL1BypassFactor       = (*(pDataBuffer+fNBlockHeaderWords+O_L1BYPASSFACTOR      )&M_L1BYPASSFACTOR      )>>S_L1BYPASSFACTOR      ;
  fL1NEnabledMasks      = (*(pDataBuffer+fNBlockHeaderWords+O_L1NENABLEDMASKS     )&M_L1NENABLEDMASKS     )>>S_L1NENABLEDMASKS     ;
  fNBlockHeaderWords+=3;  //3 32-bit header words for global part
  // Info for each of the L0 masks
  UInt_t NMaskWords = 0;
  for(UInt_t iMask=0;iMask<fL1NEnabledMasks;iMask++){
    L1MaskBlock L0Mask;
    L0Mask.SetL0MaskID           ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1MASKID       )&M_L1MASKID       )>>S_L1MASKID       );
    L0Mask.SetL1Flags            ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1FLAGS        )&M_L1FLAGS        )>>S_L1FLAGS        );
    L0Mask.SetL1TriggerWord      ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1TRIGGERWORD  )&M_L1TRIGGERWORD  )>>S_L1TRIGGERWORD  );
    L0Mask.SetL1NEnabledAlgos    ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1NENABLEDALGOS)&M_L1NENABLEDALGOS)>>S_L1NENABLEDALGOS);
    L0Mask.SetL1ReferenceDetector((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1REFDETECTOR  )&M_L1REFDETECTOR  )>>S_L1REFDETECTOR  );
    L0Mask.SetL1ReferenceFineTime((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1REFFINETIME  )&M_L1REFFINETIME  )>>S_L1REFFINETIME  );
    L0Mask.SetL1ReductionFactor  ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1REDUCTION    )&M_L1REDUCTION    )>>S_L1REDUCTION    );
    NMaskWords+=2; //2 32-bit header words for each mask
    // Info for each L1 algo enabled
    std::vector<L1AlgoBlock> L1Algos;
    L1Algos.clear();
    for(UInt_t iAlgo=0;iAlgo<L0Mask.GetL1NEnabledAlgos();iAlgo++){
      L1AlgoBlock L1Algo;
      L1Algo.SetL1AlgoID           ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1ALGOID      )&M_L1ALGOID      )>>S_L1ALGOID      );
      L1Algo.SetL1QualityFlags     ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1QUALITYFLAGS)&M_L1QUALITYFLAGS)>>S_L1QUALITYFLAGS);
      L1Algo.SetL1ProcessID        ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1PROCESSID   )&M_L1PROCESSID   )>>S_L1PROCESSID   );
      L1Algo.SetL1NAlgoWords       ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1NALGOWORDS  )&M_L1NALGOWORDS  )>>S_L1NALGOWORDS  );
      L1Algo.SetL1AlgoFlags        ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1ALGOFLAGS   )&M_L1ALGOFLAGS   )>>S_L1ALGOFLAGS   );
      L1Algo.SetL1TimeWindow       ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1TIMEWINDOW  )&M_L1TIMEWINDOW  )>>S_L1TIMEWINDOW  );
      L1Algo.SetL1DownscalingFactor((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L1DOWNSCALING )&M_L1DOWNSCALING )>>S_L1DOWNSCALING );
      //Store data words for each algo (if any)
      std::vector<UInt_t> DataWords;
      DataWords.clear();
      for(UInt_t iDataWord=2;iDataWord<L1Algo.GetL1NAlgoWords();iDataWord++){ //skip first 2 words (Algo header)
        DataWords.push_back(*(pDataBuffer+fNBlockHeaderWords+NMaskWords+iDataWord));
      }
      L1Algo.SetL1DataWords(DataWords);
      L1Algos.push_back(L1Algo);
      NMaskWords+=L1Algo.GetL1NAlgoWords();
    }
    L0Mask.SetL1Algorithms(L1Algos);
    fL0Masks.push_back(L0Mask);
  }

  return kTRUE;
}

void L1TPData::PrintInfo(){
  std::cout << "[L1TPData]  --> Printing L1TP info!" << std::endl;
  std::cout << "[L1TPData]  L1Format:             " << (Int_t)fL1Format             << std::endl;
  std::cout << "[L1TPData]  L1FlagMode:           " << (Int_t)fL1FlagMode           << std::endl;
  std::cout << "[L1TPData]  L1ReferenceDetector:  " << (Int_t)fL1ReferenceDetector  << std::endl;
  std::cout << "[L1TPData]  L1ReferenceFineTime:  " << (Int_t)fL1ReferenceFineTime  << std::endl;
  std::cout << "[L1TPData]  L1GlobalHeaderLength: " << (Int_t)fL1GlobalHeaderLength << std::endl;
  std::cout << "[L1TPData]  L1ReductionFactor:    " << (Int_t)fL1ReductionFactor    << std::endl;
  std::cout << "[L1TPData]  L1DownscalingFactor:  " << (Int_t)fL1DownscalingFactor  << std::endl;
  std::cout << "[L1TPData]  L1AutoflagFactor:     " << (Int_t)fL1AutoflagFactor     << std::endl;
  std::cout << "[L1TPData]  L1BypassFactor:       " << (Int_t)fL1BypassFactor       << std::endl;
  std::cout << "[L1TPData]  L1NEnabledMasks:      " << (Int_t)fL1NEnabledMasks      << std::endl;
  for(UInt_t iMask=0;iMask<fL0Masks.size();iMask++){
    fL0Masks[iMask].PrintInfo();
  }
}

ClassImp(L1MaskBlock)

L1MaskBlock::L1MaskBlock(): TObject(){
  Clear();
}

L1MaskBlock::L1MaskBlock(const L1MaskBlock& c) : 
  TObject(c),
  fL0MaskID(c.fL0MaskID),
  fL1Flags(c.fL1Flags),
  fL1TriggerWord(c.fL1TriggerWord),
  fL1NEnabledAlgos(c.fL1NEnabledAlgos),
  fL1ReferenceDetector(c.fL1ReferenceDetector),
  fL1ReferenceFineTime(c.fL1ReferenceFineTime),
  fL1ReductionFactor(c.fL1ReductionFactor),
  fL1Algorithms(c.fL1Algorithms)
{
}

void L1MaskBlock::Clear(Option_t* /*option*/){
  fL0MaskID = 0;
  fL1Flags = 0;
  fL1TriggerWord = 0;
  fL1NEnabledAlgos = 0;
  fL1ReferenceDetector = 0;
  fL1ReferenceFineTime = 0;
  fL1ReductionFactor = 0;
  fL1Algorithms.clear();
}

void L1MaskBlock::PrintInfo(){
  std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " L1Flags:             " << (Int_t)fL1Flags             << std::endl;
  std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " L1TriggerWord:       " << (Int_t)fL1TriggerWord       << std::endl;
  std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " L1NEnabledAlgos:     " << (Int_t)fL1NEnabledAlgos     << std::endl;
  std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " L1ReferenceDetector: " << (Int_t)fL1ReferenceDetector << std::endl;
  std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " L1ReferenceFineTime: " << (Int_t)fL1ReferenceFineTime << std::endl;
  std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " L1ReductionFactor:   " << (Int_t)fL1ReductionFactor   << std::endl;
  for(UInt_t iAlgo=0;iAlgo<fL1Algorithms.size();iAlgo++){
    std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL1Algorithms[iAlgo].GetL1AlgoID() << " L1QualityFlags:      " << (Int_t)fL1Algorithms[iAlgo].GetL1QualityFlags()      << std::endl;
    std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL1Algorithms[iAlgo].GetL1AlgoID() << " L1ProcessID:         " << (Int_t)fL1Algorithms[iAlgo].GetL1ProcessID()         << std::endl;
    std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL1Algorithms[iAlgo].GetL1AlgoID() << " L1NAlgoWords:        " << (Int_t)fL1Algorithms[iAlgo].GetL1NAlgoWords()        << std::endl;
    std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL1Algorithms[iAlgo].GetL1AlgoID() << " L1AlgoFlags:         " << (Int_t)fL1Algorithms[iAlgo].GetL1AlgoFlags()         << std::endl;
    std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL1Algorithms[iAlgo].GetL1AlgoID() << " L1TimeWindow:        " << (Int_t)fL1Algorithms[iAlgo].GetL1TimeWindow()        << std::endl;
    std::cout << "[L1TPData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL1Algorithms[iAlgo].GetL1AlgoID() << " L1DownscalingFactor: " << (Int_t)fL1Algorithms[iAlgo].GetL1DownscalingFactor() << std::endl;
  }
}

ClassImp(L1AlgoBlock)

L1AlgoBlock::L1AlgoBlock(): TObject(){
  Clear();
}

L1AlgoBlock::L1AlgoBlock(const L1AlgoBlock& c) : TObject(c), fL1AlgoID(c.fL1AlgoID),fL1QualityFlags(c.fL1QualityFlags), fL1ProcessID(c.fL1ProcessID), fL1NAlgoWords(c.fL1NAlgoWords), fL1AlgoFlags(c.fL1AlgoFlags), fL1TimeWindow(c.fL1TimeWindow), fL1DownscalingFactor(c.fL1DownscalingFactor), fL1DataWords(c.fL1DataWords){}

void L1AlgoBlock::Clear(Option_t* /*option*/){
  fL1AlgoID = 0;
  fL1QualityFlags = 0;
  fL1ProcessID = 0;
  fL1NAlgoWords = 0;
  fL1AlgoFlags = 0;
  fL1TimeWindow = 0;
  fL1DownscalingFactor = 0;
  fL1DataWords.clear();
}
