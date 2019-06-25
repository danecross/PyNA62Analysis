// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-06-26
//
// ---------------------------------------------------------------

#ifndef L1TPData_H
#define L1TPData_H 1
#include "Rtypes.h"
#include "TObject.h"

// Subdetector Block Header (generic part)
#define O_L1EVENTLENGTH 0
#define O_L1DETECTORID 0
#define O_L1DATABLOCKFORMAT 0
#define O_L1TIMESTAMP 1

#define M_L1EVENTLENGTH 0x0000ffff
#define M_L1DETECTORID 0x00ff0000
#define M_L1DATABLOCKFORMAT 0xff000000
#define M_L1TIMESTAMP 0xffffffff

#define S_L1EVENTLENGTH 0
#define S_L1DETECTORID 16
#define S_L1DATABLOCKFORMAT 24
#define S_L1TIMESTAMP 0

// Subdetector Block Header (L1TP-specific part)
#define O_L1FORMAT 0
#define O_L1FLAGMODE 0
#define O_L1REFERENCEDETECTOR 0
#define O_L1REFERENCEFINETIME 0
#define O_L1GLOBALHEADERLENGTH 0
#define O_L1GLOBALREDUCTION 1
#define O_L1GLOBALDOWNSCALING 1 
#define O_L1AUTOFLAGFACTOR 2
#define O_L1BYPASSFACTOR 2
#define O_L1NENABLEDMASKS 2

#define M_L1FORMAT 0xff000000
#define M_L1FLAGMODE 0x00f00000
#define M_L1REFERENCEDETECTOR 0x000f0000
#define M_L1REFERENCEFINETIME 0x0000ff00
#define M_L1GLOBALHEADERLENGTH 0x000000ff
#define M_L1GLOBALREDUCTION 0xffff0000
#define M_L1GLOBALDOWNSCALING 0x0000ffff
#define M_L1AUTOFLAGFACTOR 0xffff0000
#define M_L1BYPASSFACTOR 0x0000ff00
#define M_L1NENABLEDMASKS 0x000000ff

#define S_L1FORMAT 24
#define S_L1FLAGMODE 20
#define S_L1REFERENCEDETECTOR 16
#define S_L1REFERENCEFINETIME 8
#define S_L1GLOBALHEADERLENGTH 0
#define S_L1GLOBALREDUCTION 16
#define S_L1GLOBALDOWNSCALING 0 
#define S_L1AUTOFLAGFACTOR 16
#define S_L1BYPASSFACTOR 8
#define S_L1NENABLEDMASKS 0

//L0 mask block
#define O_L1FLAGS 0
#define O_L1MASKID 0
#define O_L1TRIGGERWORD 0
#define O_L1NENABLEDALGOS 0
#define O_L1REFDETECTOR 1
#define O_L1REFFINETIME 1
#define O_L1REDUCTION 1

#define M_L1FLAGS 0xff000000
#define M_L1MASKID 0x00ff0000
#define M_L1TRIGGERWORD 0x0000ff00
#define M_L1NENABLEDALGOS 0x000000ff
#define M_L1REFDETECTOR 0xff000000
#define M_L1REFFINETIME 0x00ff0000
#define M_L1REDUCTION 0x0000ffff

#define S_L1FLAGS 24
#define S_L1MASKID 16
#define S_L1TRIGGERWORD 8
#define S_L1NENABLEDALGOS 0
#define S_L1REFDETECTOR 24
#define S_L1REFFINETIME 16
#define S_L1REDUCTION 0

//L1 algo block
#define O_L1QUALITYFLAGS 0
#define O_L1ALGOID 0
#define O_L1PROCESSID 0
#define O_L1NALGOWORDS 0
#define O_L1ALGOFLAGS 1
#define O_L1TIMEWINDOW 1
#define O_L1DOWNSCALING 1

#define M_L1QUALITYFLAGS 0xff000000
#define M_L1ALGOID 0x00ff0000
#define M_L1PROCESSID 0x0000ff00
#define M_L1NALGOWORDS 0x000000ff
#define M_L1ALGOFLAGS 0xff000000
#define M_L1TIMEWINDOW 0x00ff0000
#define M_L1DOWNSCALING 0x0000ffff

#define S_L1QUALITYFLAGS 24
#define S_L1ALGOID 16
#define S_L1PROCESSID 8
#define S_L1NALGOWORDS 0
#define S_L1ALGOFLAGS 24
#define S_L1TIMEWINDOW 16
#define S_L1DOWNSCALING 0

class L1AlgoBlock;
class L1MaskBlock;

class L1TPData : public TObject {

  public:

    L1TPData();
    L1TPData(const L1TPData&);
    ~L1TPData();
    void Clear(Option_t* = "");
    Bool_t   SetHeader(UInt_t *);
    UInt_t   GetEventLength()              { return fEventLength;         }
    ULong_t  GetTimeStamp()                { return fTimeStamp;           }
    void     SetTimeStamp(ULong_t val)     { fTimeStamp = val;            }
    UChar_t  GetL1FlagMode()               { return fL1FlagMode;          }
    UChar_t  GetL1ReferenceDetector()      { return fL1ReferenceDetector; }
    UChar_t  GetL1ReferenceFineTime()      { return fL1ReferenceFineTime; }
    UShort_t GetL1ReductionFactor()        { return fL1ReductionFactor;   }
    UShort_t GetL1DownscalingFactor()      { return fL1DownscalingFactor; }
    UShort_t GetL1AutoflagFactor()         { return fL1AutoflagFactor;    }
    UChar_t  GetL1BypassFactor()           { return fL1BypassFactor;      }
    std::vector<L1MaskBlock> GetL0Masks()  { return fL0Masks;             }
    void     PrintInfo();

  private:

    // Subdetector Block Header (generic part)
    UInt_t  fEventLength;
    UInt_t  fDetectorID;           //!  Transient data member
    UInt_t  fDataBlockFormat;      //!  Transient data member
    ULong_t fTimeStamp;
    UInt_t  fNBlockHeaderWords;    //!  Transient data member

    // Subdetector Block Header (L1TP-specific part)
    UChar_t  fL1Format;             //!  Transient data member
    UChar_t  fL1FlagMode;
    UChar_t  fL1ReferenceDetector;
    UChar_t  fL1ReferenceFineTime;
    UChar_t  fL1GlobalHeaderLength; //!  Transient data member
    UShort_t fL1ReductionFactor;
    UShort_t fL1DownscalingFactor;
    UShort_t fL1AutoflagFactor;
    UChar_t  fL1BypassFactor;
    UChar_t  fL1NEnabledMasks;      //! Transient data member
    std::vector<L1MaskBlock> fL0Masks;

    ClassDef(L1TPData,1);
};


class L1MaskBlock : public TObject {

  public:

    L1MaskBlock();
    L1MaskBlock(const L1MaskBlock&);
    ~L1MaskBlock() {}
    void Clear(Option_t* = "");
    UChar_t  GetL0MaskID()                       { return fL0MaskID;              }
    void     SetL0MaskID(UChar_t val)            { fL0MaskID = val;               }
    UChar_t  GetL1Flags()                        { return fL1Flags;               }
    void     SetL1Flags(UChar_t val)             { fL1Flags = val;                }
    UChar_t  GetL1TriggerWord()                  { return fL1TriggerWord;         }
    void     SetL1TriggerWord(UChar_t val)       { fL1TriggerWord = val;          }
    UChar_t  GetL1NEnabledAlgos()                { return fL1NEnabledAlgos;       }
    void     SetL1NEnabledAlgos(UChar_t val)     { fL1NEnabledAlgos = val;        }
    UChar_t  GetL1ReferenceDetector()            { return fL1ReferenceDetector;   }
    void     SetL1ReferenceDetector(UChar_t val) { fL1ReferenceDetector = val;    }
    UChar_t  GetL1ReferenceFineTime()            { return fL1ReferenceFineTime;   }
    void     SetL1ReferenceFineTime(UChar_t val) { fL1ReferenceFineTime = val;    }
    UShort_t GetL1ReductionFactor()              { return fL1ReductionFactor;     }
    void     SetL1ReductionFactor(UShort_t val)  { fL1ReductionFactor = val;      }
    std::vector<L1AlgoBlock> GetL1Algorithms()   { return fL1Algorithms;          }
    void     SetL1Algorithms(std::vector<L1AlgoBlock> &val){ fL1Algorithms = val; }
    void     PrintInfo();

  private:

    UChar_t  fL0MaskID;
    UChar_t  fL1Flags;
    UChar_t  fL1TriggerWord;
    UChar_t  fL1NEnabledAlgos;      //! Transient data member
    UChar_t  fL1ReferenceDetector;
    UChar_t  fL1ReferenceFineTime;
    UShort_t fL1ReductionFactor;
    std::vector<L1AlgoBlock> fL1Algorithms;

    ClassDef(L1MaskBlock,1);
};

class L1AlgoBlock : public TObject {

  public:

    L1AlgoBlock();
    L1AlgoBlock(const L1AlgoBlock&);
    ~L1AlgoBlock(){};
    void Clear(Option_t* = "");
    UChar_t  GetL1AlgoID()                          { return fL1AlgoID;            }
    void     SetL1AlgoID(UChar_t val)               { fL1AlgoID = val;             }
    UChar_t  GetL1QualityFlags()                    { return fL1QualityFlags;      }
    void     SetL1QualityFlags(UChar_t val)         { fL1QualityFlags = val;       }
    UChar_t  GetL1ProcessID()                       { return fL1ProcessID;         }
    void     SetL1ProcessID(UChar_t val)            { fL1ProcessID = val;          }
    UChar_t  GetL1NAlgoWords()                      { return fL1NAlgoWords;        }
    void     SetL1NAlgoWords(UChar_t val)           { fL1NAlgoWords = val;         }
    UChar_t  GetL1AlgoFlags()                       { return fL1AlgoFlags;         }
    void     SetL1AlgoFlags(UChar_t val)            { fL1AlgoFlags = val;          }
    UChar_t  GetL1TimeWindow()                      { return fL1TimeWindow;        }
    void     SetL1TimeWindow(UChar_t val)           { fL1TimeWindow = val;         }
    UShort_t GetL1DownscalingFactor()               { return fL1DownscalingFactor; }
    void     SetL1DownscalingFactor(UShort_t val)   { fL1DownscalingFactor = val;  }
    std::vector<UInt_t> GetL1DataWords()            { return fL1DataWords;         }
    void     SetL1DataWords(std::vector<UInt_t> &val){ fL1DataWords = val;          }

  private:

    UChar_t  fL1AlgoID;             // 8-bit word
    UChar_t  fL1QualityFlags;
    UChar_t  fL1ProcessID;
    UChar_t  fL1NAlgoWords;         //! Transient data member
    UChar_t  fL1AlgoFlags;
    UChar_t  fL1TimeWindow;
    UShort_t fL1DownscalingFactor; // 16-bit word
    std::vector<UInt_t> fL1DataWords;

    ClassDef(L1AlgoBlock,1);
};
#endif
