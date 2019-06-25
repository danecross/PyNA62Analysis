// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TVEvent_H
#define TVEvent_H

#include "TObject.h"
#include "NA62Global.hh"

class TVEvent : public TObject {

    public:

        TVEvent();
        TVEvent(TVEvent &);
        virtual ~TVEvent();
        void Clear(Option_t* = "");
        Double_t GetTime(){ return fTimeStamp*ClockPeriod + fFineTime; }
        void SetTime(Double_t Time){ fTimeStamp = (Int_t)Time/ClockPeriod; fFineTime = Time - fTimeStamp*ClockPeriod; }
        Int_t Compare(const TObject *obj) const;
        Bool_t IsSortable() const { return kTRUE; }

        ULong64_t        GetStartByte() const                               { return fStartByte;                    }
        void             SetStartByte(ULong64_t value)                      { fStartByte = value;                   }
        Int_t            GetID() const                                      { return fID;                           }
        void             SetID(Int_t value)                                 { fID = value;                          }
        Int_t            GetBurstID() const                                 { return fBurstID;                      }
        void             SetBurstID(Int_t value)                            { fBurstID = value;                     }
        Int_t            GetRunID() const                                   { return fRunID;                        }
        void             SetRunID(Int_t value)                              { fRunID = value;                       }
        Bool_t           GetIsMC() const                                    { return fIsMC;                         }
        void             SetIsMC(Bool_t value)                              { fIsMC = value;                        }
        ULong64_t        GetTriggerType() const                             { return fTriggerType;                  }
        void             SetTriggerType(ULong64_t value)                    { fTriggerType = value;                 }
        Int_t            GetL0TriggerType() const                           { return fL0TriggerType;                }
        void             SetL0TriggerType(Int_t value)                      { fL0TriggerType = value;               }
        ULong_t          GetTimeStamp() const                               { return fTimeStamp;                    }
        void             SetTimeStamp(ULong_t value)                        { fTimeStamp = value;                   }
        Float_t          GetFineTime() const                                { return fFineTime;                     }
        void             SetFineTime(Float_t value)                         { fFineTime = value;                    }
        Float_t          GetLatency() const                                 { return fLatency;                      }
        void             SetLatency(Float_t value)                          { fLatency = value;                     }

    private:

        ULong64_t  fStartByte;
        Int_t      fID;
        Int_t      fBurstID;
        Int_t      fRunID;
        Bool_t     fIsMC;
        ULong64_t  fTriggerType;
        Int_t      fL0TriggerType;
        ULong_t fTimeStamp;
        Float_t fFineTime;
        Float_t fLatency;

        ClassDef(TVEvent,1);
};
#endif
