#ifndef SLMRawDecoder_H
#define SLMRawDecoder_H 1

#include "NA62VRawDecoder.hh"

#include "TLKrDigi.hh"
#include "TLKrEvent.hh"
#include "FADCEvent.hh"
#include "EventHeader.hh"

#include "TClonesArray.h"

#include <vector>
class LKrCommon;

class SLMRawDecoder : public NA62VRawDecoder
{

    public:

        SLMRawDecoder();
        ~SLMRawDecoder();
        using NA62VRawDecoder::Reset;
        void Reset(UInt_t *,Int_t);
        void StartOfBurst();
        void EndOfBurst();
        virtual TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

    public:

        Bool_t               GetTriggerLess()                                   { return fTriggerLess;                  };
        void                 SetTriggerLess(Bool_t value)                       { fTriggerLess = value;                 };
        Bool_t               GetBothEdges()                                     { return fBothEdges;                    };
        void                 SetBothEdges(Bool_t value)                         { fBothEdges = value;                   };

        Int_t                GetNTrig()                                         { return fNTrig;                        };
        void                 SetNTrig(Int_t value)                              { fNTrig = value;                       };
        UInt_t *             GetpDataBuffer()                                   { return fpDataBuffer;                  };
        void                 SetpDataBuffer(UInt_t * value)                     { fpDataBuffer = value;                 };
        Int_t                GetNWords()                                        { return fNWords;                       };
        void                 SetNWords(Int_t value)                             { fNWords = value;                      };

    private:

        Bool_t fTriggerLess;
        Bool_t fBothEdges;

        Int_t fNTrig;
        UInt_t * fpDataBuffer;
        Int_t fNWords;
  //        TClonesArray * fFadcEvents;
        std::vector<int> fFadcEventStatus;
        Long_t ftTime[4];
        Int_t fiTime[4];
//        TDetectorVEvent * fSLMEvent;
        FADCEvent * fSLMEvent;
        LKrCommon *fAdcCommon;
};
#endif
