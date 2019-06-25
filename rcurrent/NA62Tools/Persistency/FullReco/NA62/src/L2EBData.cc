// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-06-26
//
// ---------------------------------------------------------------

#include "L2EBData.hh"

#include "Riostream.h"

ClassImp(L2EBData)

L2EBData::L2EBData(): TObject(){
  Clear();
}                        

L2EBData::~L2EBData(){}

L2EBData::L2EBData(const L2EBData& c) : TObject(c), fEventLength(c.fEventLength), fDetectorID(c.fDetectorID), fDataBlockFormat(c.fDataBlockFormat), fTimeStamp(c.fTimeStamp), fNBlockHeaderWords(c.fNBlockHeaderWords), fL2Format(c.fL2Format), fL2FlagMode(c.fL2FlagMode), fL2ReferenceDetector(c.fL2ReferenceDetector), fL2ReferenceFineTime(c.fL2ReferenceFineTime), fL2GlobalHeaderLength(c.fL2GlobalHeaderLength), fL2ReductionFactor(c.fL2ReductionFactor), fL2DownscalingFactor(c.fL2DownscalingFactor), fL2AutoflagFactor(c.fL2AutoflagFactor), fL2BypassFactor(c.fL2BypassFactor), fL2NEnabledMasks(c.fL2NEnabledMasks), fL0Masks(c.fL0Masks){}

void L2EBData::Clear(Option_t* /*option*/){
  // Subdetector Block Header (generic part)
  fEventLength = 0;
  fDetectorID = 0;
  fDataBlockFormat = 0;
  fTimeStamp = 0;
  fNBlockHeaderWords = 0;

  // Subdetector Block Header (L2EB-specific part)
  fL2Format = 0;
  fL2FlagMode = 0;
  fL2ReferenceDetector = 0;
  fL2ReferenceFineTime = 0;
  fL2GlobalHeaderLength = 0;
  fL2ReductionFactor = 0;
  fL2DownscalingFactor = 0;
  fL2AutoflagFactor = 0;
  fL2BypassFactor = 0;
  fL2NEnabledMasks = 0;
  fL0Masks.clear();
}

Bool_t L2EBData::SetHeader(UInt_t * pDataBuffer){

  Clear(); //Reset L2 info

  // Subdetector Block Header (generic part)
  fEventLength         = (*(pDataBuffer+O_L2EVENTLENGTH)&M_L2EVENTLENGTH)>>S_L2EVENTLENGTH;
  fDetectorID          = (*(pDataBuffer+O_L2DETECTORID)&M_L2DETECTORID)>>S_L2DETECTORID;
  fDataBlockFormat     = (*(pDataBuffer+O_L2DATABLOCKFORMAT)&M_L2DATABLOCKFORMAT)>>S_L2DATABLOCKFORMAT;
  fNBlockHeaderWords = 1;
  if(fDataBlockFormat==1) { // 2015 format: read the 32-bit timestamp
    fTimeStamp         = (*(pDataBuffer+O_L2TIMESTAMP)&M_L2TIMESTAMP)>>S_L2TIMESTAMP;
    fNBlockHeaderWords++;
  }

  if(fEventLength<=12) return kTRUE; //2015 format: no L2 block

  // Subdetector Block Header (L2EB-specific part)
  fL2Format             = (*(pDataBuffer+fNBlockHeaderWords+O_L2FORMAT            )&M_L2FORMAT            )>>S_L2FORMAT            ;
  fL2FlagMode           = (*(pDataBuffer+fNBlockHeaderWords+O_L2FLAGMODE          )&M_L2FLAGMODE          )>>S_L2FLAGMODE          ;
  fL2ReferenceDetector  = (*(pDataBuffer+fNBlockHeaderWords+O_L2REFERENCEDETECTOR )&M_L2REFERENCEDETECTOR )>>S_L2REFERENCEDETECTOR ;
  fL2ReferenceFineTime  = (*(pDataBuffer+fNBlockHeaderWords+O_L2REFERENCEFINETIME )&M_L2REFERENCEFINETIME )>>S_L2REFERENCEFINETIME ;
  fL2GlobalHeaderLength = (*(pDataBuffer+fNBlockHeaderWords+O_L2GLOBALHEADERLENGTH)&M_L2GLOBALHEADERLENGTH)>>S_L2GLOBALHEADERLENGTH;
  fL2ReductionFactor    = (*(pDataBuffer+fNBlockHeaderWords+O_L2GLOBALREDUCTION   )&M_L2GLOBALREDUCTION   )>>S_L2GLOBALREDUCTION   ;
  fL2DownscalingFactor  = (*(pDataBuffer+fNBlockHeaderWords+O_L2GLOBALDOWNSCALING )&M_L2GLOBALDOWNSCALING )>>S_L2GLOBALDOWNSCALING ;
  fL2AutoflagFactor     = (*(pDataBuffer+fNBlockHeaderWords+O_L2AUTOFLAGFACTOR    )&M_L2AUTOFLAGFACTOR    )>>S_L2AUTOFLAGFACTOR    ;
  fL2BypassFactor       = (*(pDataBuffer+fNBlockHeaderWords+O_L2BYPASSFACTOR      )&M_L2BYPASSFACTOR      )>>S_L2BYPASSFACTOR      ;
  fL2NEnabledMasks      = (*(pDataBuffer+fNBlockHeaderWords+O_L2NENABLEDMASKS     )&M_L2NENABLEDMASKS     )>>S_L2NENABLEDMASKS     ;
  fNBlockHeaderWords+=3;  //3 32-bit header words for global part
  // Info for each of the L0 masks
  UInt_t NMaskWords = 0;
  for(UInt_t iMask=0;iMask<fL2NEnabledMasks;iMask++){
    L2MaskBlock L0Mask;
    L0Mask.SetL0MaskID           ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2MASKID       )&M_L2MASKID       )>>S_L2MASKID       );
    L0Mask.SetL2Flags            ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2FLAGS        )&M_L2FLAGS        )>>S_L2FLAGS        );
    L0Mask.SetL2TriggerWord      ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2TRIGGERWORD  )&M_L2TRIGGERWORD  )>>S_L2TRIGGERWORD  );
    L0Mask.SetL2NEnabledAlgos    ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2NENABLEDALGOS)&M_L2NENABLEDALGOS)>>S_L2NENABLEDALGOS);
    L0Mask.SetL2ReferenceDetector((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2REFDETECTOR  )&M_L2REFDETECTOR  )>>S_L2REFDETECTOR  );
    L0Mask.SetL2ReferenceFineTime((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2REFFINETIME  )&M_L2REFFINETIME  )>>S_L2REFFINETIME  );
    L0Mask.SetL2ReductionFactor  ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2REDUCTION    )&M_L2REDUCTION    )>>S_L2REDUCTION    );
    NMaskWords+=2; //2 32-bit header words for each mask
    // Info for each L2 algo enabled
    std::vector<L2AlgoBlock> L2Algos;
    L2Algos.clear();
    for(UInt_t iAlgo=0;iAlgo<L0Mask.GetL2NEnabledAlgos();iAlgo++){
      L2AlgoBlock L2Algo;
      L2Algo.SetL2AlgoID           ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2ALGOID      )&M_L2ALGOID      )>>S_L2ALGOID      );
      L2Algo.SetL2QualityFlags     ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2QUALITYFLAGS)&M_L2QUALITYFLAGS)>>S_L2QUALITYFLAGS);
      L2Algo.SetL2ProcessID        ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2PROCESSID   )&M_L2PROCESSID   )>>S_L2PROCESSID   );
      L2Algo.SetL2NAlgoWords       ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2NALGOWORDS  )&M_L2NALGOWORDS  )>>S_L2NALGOWORDS  );
      L2Algo.SetL2AlgoFlags        ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2ALGOFLAGS   )&M_L2ALGOFLAGS   )>>S_L2ALGOFLAGS   );
      L2Algo.SetL2TimeWindow       ((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2TIMEWINDOW  )&M_L2TIMEWINDOW  )>>S_L2TIMEWINDOW  );
      L2Algo.SetL2DownscalingFactor((*(pDataBuffer+fNBlockHeaderWords+NMaskWords+O_L2DOWNSCALING )&M_L2DOWNSCALING )>>S_L2DOWNSCALING );
      //Store data words for each algo (if any)
      std::vector<UInt_t> DataWords;
      DataWords.clear();
      for(UInt_t iDataWord=2;iDataWord<L2Algo.GetL2NAlgoWords();iDataWord++){ //skip first 2 words (Algo header)
        DataWords.push_back(*(pDataBuffer+fNBlockHeaderWords+NMaskWords+iDataWord));
      }
      L2Algo.SetL2DataWords(DataWords);
      L2Algos.push_back(L2Algo);
      NMaskWords+=L2Algo.GetL2NAlgoWords();
    }
    L0Mask.SetL2Algorithms(L2Algos);
    fL0Masks.push_back(L0Mask);
  }

  return kTRUE;
}

void L2EBData::PrintInfo(){
  std::cout << "[L2EBData]  --> Printing L2EB info!" << std::endl;
  std::cout << "[L2EBData]  L2Format:             " << (Int_t)fL2Format             << std::endl;
  std::cout << "[L2EBData]  L2FlagMode:           " << (Int_t)fL2FlagMode           << std::endl;
  std::cout << "[L2EBData]  L2ReferenceDetector:  " << (Int_t)fL2ReferenceDetector  << std::endl;
  std::cout << "[L2EBData]  L2ReferenceFineTime:  " << (Int_t)fL2ReferenceFineTime  << std::endl;
  std::cout << "[L2EBData]  L2GlobalHeaderLength: " << (Int_t)fL2GlobalHeaderLength << std::endl;
  std::cout << "[L2EBData]  L2ReductionFactor:    " << (Int_t)fL2ReductionFactor    << std::endl;
  std::cout << "[L2EBData]  L2DownscalingFactor:  " << (Int_t)fL2DownscalingFactor  << std::endl;
  std::cout << "[L2EBData]  L2AutoflagFactor:     " << (Int_t)fL2AutoflagFactor     << std::endl;
  std::cout << "[L2EBData]  L2BypassFactor:       " << (Int_t)fL2BypassFactor       << std::endl;
  std::cout << "[L2EBData]  L2NEnabledMasks:      " << (Int_t)fL2NEnabledMasks      << std::endl;
  for(UInt_t iMask=0;iMask<fL0Masks.size();iMask++){
    fL0Masks[iMask].PrintInfo();
  }
}

ClassImp(L2MaskBlock)

L2MaskBlock::L2MaskBlock(): TObject(){
  Clear();
}

L2MaskBlock::L2MaskBlock(const L2MaskBlock &c) :
  TObject(c),
  fL0MaskID(c.fL0MaskID),
  fL2Flags(c.fL2Flags),
  fL2TriggerWord(c.fL2TriggerWord),
  fL2NEnabledAlgos(c.fL2NEnabledAlgos),
  fL2ReferenceDetector(c.fL2ReferenceDetector),
  fL2ReferenceFineTime(c.fL2ReferenceFineTime),
  fL2ReductionFactor(c.fL2ReductionFactor),
  fL2Algorithms(c.fL2Algorithms)
{
}

void L2MaskBlock::Clear(Option_t* /*option*/){
  fL0MaskID = 0;
  fL2Flags = 0;
  fL2TriggerWord = 0;
  fL2NEnabledAlgos = 0;
  fL2ReferenceDetector = 0;
  fL2ReferenceFineTime = 0;
  fL2ReductionFactor = 0;
  fL2Algorithms.clear();
}

void L2MaskBlock::PrintInfo(){
  std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " L2Flags:             " << (Int_t)fL2Flags             << std::endl;
  std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " L2TriggerWord:       " << (Int_t)fL2TriggerWord       << std::endl;
  std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " L2NEnabledAlgos:     " << (Int_t)fL2NEnabledAlgos     << std::endl;
  std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " L2ReferenceDetector: " << (Int_t)fL2ReferenceDetector << std::endl;
  std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " L2ReferenceFineTime: " << (Int_t)fL2ReferenceFineTime << std::endl;
  std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " L2ReductionFactor:   " << (Int_t)fL2ReductionFactor   << std::endl;
  for(UInt_t iAlgo=0;iAlgo<fL2Algorithms.size();iAlgo++){
    std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL2Algorithms[iAlgo].GetL2AlgoID() << " L2QualityFlags:      " << (Int_t)fL2Algorithms[iAlgo].GetL2QualityFlags()      << std::endl;
    std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL2Algorithms[iAlgo].GetL2AlgoID() << " L2ProcessID:         " << (Int_t)fL2Algorithms[iAlgo].GetL2ProcessID()         << std::endl;
    std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL2Algorithms[iAlgo].GetL2AlgoID() << " L2NAlgoWords:        " << (Int_t)fL2Algorithms[iAlgo].GetL2NAlgoWords()        << std::endl;
    std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL2Algorithms[iAlgo].GetL2AlgoID() << " L2AlgoFlags:         " << (Int_t)fL2Algorithms[iAlgo].GetL2AlgoFlags()         << std::endl;
    std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL2Algorithms[iAlgo].GetL2AlgoID() << " L2TimeWindow:        " << (Int_t)fL2Algorithms[iAlgo].GetL2TimeWindow()        << std::endl;
    std::cout << "[L2EBData]  * Mask " << (Int_t)fL0MaskID << " * Algo " << (Int_t)fL2Algorithms[iAlgo].GetL2AlgoID() << " L2DownscalingFactor: " << (Int_t)fL2Algorithms[iAlgo].GetL2DownscalingFactor() << std::endl;
  }
}

ClassImp(L2AlgoBlock)

L2AlgoBlock::L2AlgoBlock(): TObject(){
  Clear();
}

L2AlgoBlock::L2AlgoBlock(const L2AlgoBlock& c) : TObject(c), fL2AlgoID(c.fL2AlgoID),fL2QualityFlags(c.fL2QualityFlags), fL2ProcessID(c.fL2ProcessID), fL2NAlgoWords(c.fL2NAlgoWords), fL2AlgoFlags(c.fL2AlgoFlags), fL2TimeWindow(c.fL2TimeWindow), fL2DownscalingFactor(c.fL2DownscalingFactor), fL2DataWords(c.fL2DataWords){}

void L2AlgoBlock::Clear(Option_t* /*option*/){
  fL2AlgoID = 0;
  fL2QualityFlags = 0;
  fL2ProcessID = 0;
  fL2NAlgoWords = 0;
  fL2AlgoFlags = 0;
  fL2TimeWindow = 0;
  fL2DownscalingFactor = 0;
  fL2DataWords.clear();
}
