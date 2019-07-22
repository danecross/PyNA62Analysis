// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TVCandidate_H
#define TVCandidate_H

#include "TObject.h"
#include "TClass.h"

#include "TArrayI.h"

#include <vector>
#include "TDetectorVEvent.hh"
#include "TDetectorVHit.hh"

class TVCandidate : public TObject {

    public:

        TVCandidate();
        TVCandidate(const TVCandidate &);
        explicit TVCandidate(Int_t);
        virtual ~TVCandidate();

        Bool_t AddHit(Int_t);
        TDetectorVHit * GetHit(Int_t);
        void Clear(Option_t* = "");
        void RemoveHit(Int_t);
        void Merge(TVCandidate*);

    public:

        Int_t                GetNHits()                        { return fNHits;       }
        Int_t *              GetHitsIndexes()                  { return fHitsIndexes.GetArray(); }
        TDetectorVEvent *    GetEvent()                        { return fEvent;       }
        void                 SetEvent(TDetectorVEvent * value) { fEvent = value;      }

    private:

        Int_t fNHits;
        Int_t fNMaxHits;

        TArrayI  fHitsIndexes; //[fNMaxHits];

        TDetectorVEvent * fEvent; //! Transient data member for simpler manipulation

        ClassDef(TVCandidate,1);
};
#endif
