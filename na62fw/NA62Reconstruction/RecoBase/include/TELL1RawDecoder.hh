#ifndef TELL1RawDecoder_H
#define TELL1RawDecoder_H 1

#include "NA62VRawDecoder.hh"

#include "TDCEvent.hh"

#include "TClonesArray.h"

#include <vector>

class TELL1RawDecoder : public NA62VRawDecoder
{

    public:

        TELL1RawDecoder(Bool_t, Bool_t);
        ~TELL1RawDecoder();
        using NA62VRawDecoder::Reset;
        void Reset(UInt_t *,Int_t);
        TDCEvent * GetDecodedEvent();
        Bool_t PollEventBuffer();
        TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*) {return nullptr;}; //Fixes warning
        Bool_t DecodeNextEvent(Long_t*, Int_t);

    public:

        Bool_t               GetTriggerLess()                                   { return fTriggerLess;                  };
        void                 SetTriggerLess(Bool_t value)                       { fTriggerLess = value;                 };
        Bool_t               GetBothEdges()                                     { return fBothEdges;                    };
        void                 SetBothEdges(Bool_t value)                         { fBothEdges = value;                   };

        Bool_t               GetCurrentEventChanged()                           { return fCurrentEventChanged;          };
        void                 SetCurrentEventChanged(Bool_t value)               { fCurrentEventChanged = value;         };
        Int_t                GetNTrig()                                         { return fNTrig;                        };
        void                 SetNTrig(Int_t value)                              { fNTrig = value;                       };
        UInt_t *             GetpDataBuffer()                                   { return fpDataBuffer;                  };
        void                 SetpDataBuffer(UInt_t * value)                     { fpDataBuffer = value;                 };
        Int_t                GetNWords()                                        { return fNWords;                       };
        void                 SetNWords(Int_t value)                             { fNWords = value;                      };
        Int_t                GetNMEPs()                                         { return fNMEPs;                        };
        void                 SetNMEPs(Int_t value)                              { fNMEPs = value;                       };
        TClonesArray *       GetTdcEvents()                                     { return fTdcEvents;                    };


    private:

        Bool_t fTriggerLess;
        Bool_t fBothEdges;

        Bool_t fCurrentEventChanged;
        Bool_t fCurrentEventFlushed;
        Int_t fNTrig;
        UInt_t * fpDataBuffer;
        Int_t fNWords;
        Int_t fNMEPs;
        TClonesArray * fTdcEvents;
        Int_t fLastiTimeStamp;
        std::vector<int> fTdcEventStatus;
        Long_t ftTime[4];
        Int_t fiTime[4];
};
#endif
