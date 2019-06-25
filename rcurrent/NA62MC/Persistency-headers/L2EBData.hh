// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-06-26
//
// ---------------------------------------------------------------

#ifndef L2EBData_H
#define L2EBData_H 1
#include "Rtypes.h"
#include "TObject.h"

// Subdetector Block Header (generic part)
#define O_L2EVENTLENGTH 0
#define O_L2DETECTORID 0
#define O_L2DATABLOCKFORMAT 0
#define O_L2TIMESTAMP 1

#define M_L2EVENTLENGTH 0x0000ffff
#define M_L2DETECTORID 0x00ff0000
#define M_L2DATABLOCKFORMAT 0xff000000
#define M_L2TIMESTAMP 0xffffffff

#define S_L2EVENTLENGTH 0
#define S_L2DETECTORID 16
#define S_L2DATABLOCKFORMAT 24
#define S_L2TIMESTAMP 0

// Subdetector Block Header (L2EB-specific part)
#define O_L2FORMAT 0
#define O_L2FLAGMODE 0
#define O_L2REFERENCEDETECTOR 0
#define O_L2REFERENCEFINETIME 0
#define O_L2GLOBALHEADERLENGTH 0
#define O_L2GLOBALREDUCTION 1
#define O_L2GLOBALDOWNSCALING 1 
#define O_L2AUTOFLAGFACTOR 2
#define O_L2BYPASSFACTOR 2
#define O_L2NENABLEDMASKS 2

#define M_L2FORMAT 0xff000000
#define M_L2FLAGMODE 0x00f00000
#define M_L2REFERENCEDETECTOR 0x000f0000
#define M_L2REFERENCEFINETIME 0x0000ff00
#define M_L2GLOBALHEADERLENGTH 0x000000ff
#define M_L2GLOBALREDUCTION 0xffff0000
#define M_L2GLOBALDOWNSCALING 0x0000ffff
#define M_L2AUTOFLAGFACTOR 0xffff0000
#define M_L2BYPASSFACTOR 0x0000ff00
#define M_L2NENABLEDMASKS 0x000000ff

#define S_L2FORMAT 24
#define S_L2FLAGMODE 20
#define S_L2REFERENCEDETECTOR 16
#define S_L2REFERENCEFINETIME 8
#define S_L2GLOBALHEADERLENGTH 0
#define S_L2GLOBALREDUCTION 16
#define S_L2GLOBALDOWNSCALING 0 
#define S_L2AUTOFLAGFACTOR 16
#define S_L2BYPASSFACTOR 8
#define S_L2NENABLEDMASKS 0

//L0 mask block
#define O_L2FLAGS 0
#define O_L2MASKID 0
#define O_L2TRIGGERWORD 0
#define O_L2NENABLEDALGOS 0
#define O_L2REFDETECTOR 1
#define O_L2REFFINETIME 1
#define O_L2REDUCTION 1

#define M_L2FLAGS 0xff000000
#define M_L2MASKID 0x00ff0000
#define M_L2TRIGGERWORD 0x0000ff00
#define M_L2NENABLEDALGOS 0x000000ff
#define M_L2REFDETECTOR 0xff000000
#define M_L2REFFINETIME 0x00ff0000
#define M_L2REDUCTION 0x0000ffff

#define S_L2FLAGS 24
#define S_L2MASKID 16
#define S_L2TRIGGERWORD 8
#define S_L2NENABLEDALGOS 0
#define S_L2REFDETECTOR 24
#define S_L2REFFINETIME 16
#define S_L2REDUCTION 0

//L2 algo block
#define O_L2QUALITYFLAGS 0
#define O_L2ALGOID 0
#define O_L2PROCESSID 0
#define O_L2NALGOWORDS 0
#define O_L2ALGOFLAGS 1
#define O_L2TIMEWINDOW 1
#define O_L2DOWNSCALING 1

#define M_L2QUALITYFLAGS 0xff000000
#define M_L2ALGOID 0x00ff0000
#define M_L2PROCESSID 0x0000ff00
#define M_L2NALGOWORDS 0x000000ff
#define M_L2ALGOFLAGS 0xff000000
#define M_L2TIMEWINDOW 0x00ff0000
#define M_L2DOWNSCALING 0x0000ffff

#define S_L2QUALITYFLAGS 24
#define S_L2ALGOID 16
#define S_L2PROCESSID 8
#define S_L2NALGOWORDS 0
#define S_L2ALGOFLAGS 24
#define S_L2TIMEWINDOW 16
#define S_L2DOWNSCALING 0

class L2AlgoBlock;
class L2MaskBlock;

class L2EBData : public TObject {

  public:

    L2EBData();
    ~L2EBData();
    void Reset();
    Bool_t   SetHeader(UInt_t *);
    UInt_t   GetEventLength()             { return fEventLength;         };
    ULong_t  GetTimeStamp()               { return fTimeStamp;           };
    UChar_t  GetL2FlagMode()              { return fL2FlagMode;          };
    UChar_t  GetL2ReferenceDetector()     { return fL2ReferenceDetector; };
    UChar_t  GetL2ReferenceFineTime()     { return fL2ReferenceFineTime; };
    UShort_t GetL2ReductionFactor()       { return fL2ReductionFactor;   };
    UShort_t GetL2DownscalingFactor()     { return fL2DownscalingFactor; };
    UShort_t GetL2AutoflagFactor()        { return fL2AutoflagFactor;    };
    UChar_t  GetL2BypassFactor()          { return fL2BypassFactor;      };
    std::vector<L2MaskBlock> GetL0Masks() { return fL0Masks;             };

  private:

    // Subdetector Block Header (generic part)
    UInt_t  fEventLength;
    UInt_t  fDetectorID;           //!  Transient data member
    UInt_t  fDataBlockFormat;      //!  Transient data member
    ULong_t fTimeStamp;
    UInt_t  fNBlockHeaderWords;    //!  Transient data member

    // Subdetector Block Header (L2EB-specific part)
    UChar_t  fL2Format;             //!  Transient data member
    UChar_t  fL2FlagMode;
    UChar_t  fL2ReferenceDetector;
    UChar_t  fL2ReferenceFineTime;
    UChar_t  fL2GlobalHeaderLength; //!  Transient data member
    UShort_t fL2ReductionFactor;
    UShort_t fL2DownscalingFactor;
    UShort_t fL2AutoflagFactor;
    UChar_t  fL2BypassFactor;
    UChar_t  fL2NEnabledMasks;      //! Transient data member
    std::vector<L2MaskBlock> fL0Masks;

    ClassDef(L2EBData,1);
};


class L2MaskBlock : public TObject {

  public:

    L2MaskBlock();
    L2MaskBlock(const L2MaskBlock&);
    ~L2MaskBlock(){};
    void Reset();
    UChar_t  GetL0MaskID()                       { return fL0MaskID;              };
    void     SetL0MaskID(UChar_t val)            { fL0MaskID = val;               };
    UChar_t  GetL2Flags()                        { return fL2Flags;               };
    void     SetL2Flags(UChar_t val)             { fL2Flags = val;                };
    UChar_t  GetL2TriggerWord()                  { return fL2TriggerWord;         };
    void     SetL2TriggerWord(UChar_t val)       { fL2TriggerWord = val;          };
    UChar_t  GetL2NEnabledAlgos()                { return fL2NEnabledAlgos;       };
    void     SetL2NEnabledAlgos(UChar_t val)     { fL2NEnabledAlgos = val;        };
    UChar_t  GetL2ReferenceDetector()            { return fL2ReferenceDetector;   };
    void     SetL2ReferenceDetector(UChar_t val) { fL2ReferenceDetector = val;    };
    UChar_t  GetL2ReferenceFineTime()            { return fL2ReferenceFineTime;   };
    void     SetL2ReferenceFineTime(UChar_t val) { fL2ReferenceFineTime = val;    };
    UShort_t GetL2ReductionFactor()              { return fL2ReductionFactor;     };
    void     SetL2ReductionFactor(UShort_t val)  { fL2ReductionFactor = val;      };
    std::vector<L2AlgoBlock> GetL2Algorithms()   { return fL2Algorithms;          };
    void     SetL2Algorithms(std::vector<L2AlgoBlock> &val){ fL2Algorithms = val; };

  private:

    UChar_t  fL0MaskID;
    UChar_t  fL2Flags;
    UChar_t  fL2TriggerWord;
    UChar_t  fL2NEnabledAlgos;      //! Transient data member
    UChar_t  fL2ReferenceDetector;
    UChar_t  fL2ReferenceFineTime;
    UShort_t  fL2ReductionFactor;
    std::vector<L2AlgoBlock> fL2Algorithms;

    ClassDef(L2MaskBlock,1);
};

class L2AlgoBlock : public TObject {

  public:

    L2AlgoBlock();
    ~L2AlgoBlock(){};
    void Reset();
    UChar_t  GetL2AlgoID()                          { return fL2AlgoID;            };
    void     SetL2AlgoID(UChar_t val)               { fL2AlgoID = val;             };
    UChar_t  GetL2QualityFlags()                    { return fL2QualityFlags;      };
    void     SetL2QualityFlags(UChar_t val)         { fL2QualityFlags = val;       };
    UChar_t  GetL2ProcessID()                       { return fL2ProcessID;         };
    void     SetL2ProcessID(UChar_t val)            { fL2ProcessID = val;          };
    UChar_t  GetL2NAlgoWords()                      { return fL2NAlgoWords;        };
    void     SetL2NAlgoWords(UChar_t val)           { fL2NAlgoWords = val;         };
    UChar_t  GetL2AlgoFlags()                       { return fL2AlgoFlags;         };
    void     SetL2AlgoFlags(UChar_t val)            { fL2AlgoFlags = val;          };
    UChar_t  GetL2TimeWindow()                      { return fL2TimeWindow;        };
    void     SetL2TimeWindow(UChar_t val)           { fL2TimeWindow = val;         };
    UShort_t GetL2DownscalingFactor()               { return fL2DownscalingFactor; };
    void     SetL2DownscalingFactor(UShort_t val)   { fL2DownscalingFactor = val;  };
    std::vector<UInt_t> GetL2DataWords()            { return fL2DataWords;         };
    void     SetL2DataWords(std::vector<UInt_t> &val){ fL2DataWords = val;          };

  private:

    UChar_t  fL2AlgoID;
    UChar_t  fL2QualityFlags;
    UChar_t  fL2ProcessID;
    UChar_t  fL2NAlgoWords;      //! Transient data member
    UChar_t  fL2AlgoFlags;
    UChar_t  fL2TimeWindow;
    UShort_t  fL2DownscalingFactor;
    std::vector<UInt_t> fL2DataWords;

    ClassDef(L2AlgoBlock,1);
};
#endif
