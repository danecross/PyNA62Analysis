// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TVHit_H
#define TVHit_H

#include "TVChannelID.hh"
#include "TObject.h"


class TVHit : public TObject, public TVChannelID {

    public:

        TVHit();
	    TVHit(const TVHit &);
	    explicit TVHit(Int_t);
        virtual ~TVHit(){};
        void Clear(Option_t* = "");
        virtual void UpdateReferenceTime(Double_t) = 0;
        void ShiftMCTrackID(Int_t value){ fMCTrackID += value; };
        Bool_t IsSortable() const { return kTRUE; }
        Int_t Compare(const TObject *obj) const {if(fChannelID < static_cast<const TVHit*>(obj)->GetChannelID()) return -1;
                                                 else if(fChannelID > static_cast<const TVHit*>(obj)->GetChannelID()) return 1;
                                                 else return 0;}
        void Print(Option_t* option="") const;

    public:

        Int_t                GetMCTrackID()                                     { return fMCTrackID;                    };
        void                 SetMCTrackID(Int_t value)                          { fMCTrackID = value;                   };
        Bool_t               GetDirectInteraction()                             { return fDirectInteraction;            };
        void                 SetDirectInteraction(Bool_t value)                 { fDirectInteraction = value;           };


    private:

        Int_t      fMCTrackID; // For MCTruth Association
        Bool_t     fDirectInteraction;

        ClassDef(TVHit,1);
};
#endif
