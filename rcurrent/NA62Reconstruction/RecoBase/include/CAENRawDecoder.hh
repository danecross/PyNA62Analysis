#ifndef CAENRawDecoder_H
#define CAENRawDecoder_H 1

#include "NA62VRawDecoder.hh"

#include "TDCEvent.hh"

#include "TClonesArray.h"

#include <vector>

class CAENRawDecoder : public NA62VRawDecoder
{

    public:

        CAENRawDecoder(Bool_t, Bool_t);
        ~CAENRawDecoder();
        using NA62VRawDecoder::Reset;
        void Reset(UInt_t *,Int_t);
        TDCEvent * GetDecodedEvent();
        Bool_t PollEventBuffer();
        Bool_t DecodeNextEvent(Long_t*, Int_t);
        TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*) { return nullptr;}; //Fixes warning

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
        TClonesArray * fTdcEvents;
        std::vector<int> fTdcEventStatus;
        Long_t ftTime[4];
        Int_t fiTime[4];

        Int_t fChannelRemap[512];
};
#endif
