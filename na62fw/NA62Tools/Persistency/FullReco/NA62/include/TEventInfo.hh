// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-02-26
//
// --------------------------------------------------------------
#ifndef TEventInfo_H
#define TEventInfo_H

#include "TObject.h"

#define NMAXEVINFO 1000

class TEventInfo : public TObject {

    public:

        TEventInfo();
        ~TEventInfo();
        void Clear(Option_t* = "");
        void  ProposeLatestHitTime(Double_t value){ fLatestHitTime = value > fLatestHitTime ? value : fLatestHitTime; };

    public:

        Int_t                GetID()                                            { return fID;                           };
        void                 SetID(Int_t value)                                 { fID = value;                          };
        Int_t                GetStreamID()                                      { return fStreamID;                     };
        void                 SetStreamID(Int_t value)                           { fStreamID = value;                    };
        Double_t             GetTime()                                          { return fTime;                         };
        void                 SetTime(Double_t value)                            { fTime = value;                        };
        Bool_t               GetValidity()                                      { return fValidity;                     };
        void                 SetValidity(Bool_t value)                          { fValidity = value;                    };

        Int_t                GetFirstHit()                                      { return fFirstHit;                     };
        void                 SetFirstHit(Int_t value)                           { fFirstHit = value;                    };
        Int_t                GetNHits()                                         { return fNHits;                        };
        void                 SetNHits(Int_t value)                              { fNHits = value;                       };
        Double_t             GetLatestHitTime()                                 { return fLatestHitTime;                };
        void                 SetLatestHitTime(Double_t value)                   { fLatestHitTime = value;               };
        Int_t                GetNKineParts()                                    { return fNKineParts;                   };
        void                 SetNKineParts(Int_t value)                         { fNKineParts = value;                  };

    private:

        Int_t      fID;
        Int_t      fStreamID;
        Double_t   fTime;
        Bool_t     fValidity;

        Int_t      fFirstHit;
        Int_t      fNHits;
        Double_t   fLatestHitTime;
        Int_t      fNKineParts;

        ClassDef(TEventInfo,1);
};
#endif
