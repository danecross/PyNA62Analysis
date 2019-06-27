// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoVEvent_H
#define TRecoVEvent_H

#include "TDetectorVEvent.hh"
#include "TRecoVCandidate.hh"
#include "TDigiVEvent.hh"
#include "TClonesArray.h"

class TRecoVEvent : public TDetectorVEvent {

  public:

    TRecoVEvent();
    TRecoVEvent(const TRecoVEvent &);
    TRecoVEvent(TClass *, TClass *);
    ~TRecoVEvent();

    TRecoVHit * AddHit(TDetectorVHit*);
    TRecoVHit * AddHit(TVDigi*);
    TRecoVCandidate * AddCandidate();
    TRecoVCandidate * GetCandidate(Int_t);
    void Clear(Option_t* = "");
    void RemoveCandidate(Int_t);

    Bool_t               GetStatus()                        { return fStatus;       }
    void                 SetStatus(Bool_t value)            { fStatus = value;      }
    Int_t                GetNCandidates()                   { return fNCandidates;  }
    void                 SetNCandidates(Int_t value)        { fNCandidates = value; }
    TClonesArray *       GetCandidates()                    { return fCandidates;   }
    void                 SetCandidates(TClonesArray* value) { fCandidates = value;  }
    TDigiVEvent *        GetDigiEvent()                     { return fDigiEvent;    }
    void                 SetDigiEvent(TDigiVEvent* value)   { fDigiEvent = value;   }
    ULong64_t            GetErrorMask()                     { return fErrorMask;    }
    void                 SetErrorMask(ULong64_t value)      { fErrorMask = value;   }
    void                 SetErrorMaskBit(Int_t bit,Bool_t value);

  private:

    Bool_t        fStatus;
    Int_t         fNCandidates;
    TClonesArray* fCandidates;
    TDigiVEvent*  fDigiEvent;    //!  Transient data member
    ULong64_t     fErrorMask;

    ClassDef(TRecoVEvent,1);
};
#endif
