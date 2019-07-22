// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-01-23
//
// --------------------------------------------------------------
#ifndef EventBoundary_H
#define EventBoundary_H

#include "TObject.h"

class EventBoundary : public TObject {

    public:

        EventBoundary();
        virtual ~EventBoundary(){};
        void Clear(Option_t* option="");
        void AddGenePart();
        void AddKinePart();
        void Shift(Int_t, Int_t);

    public:

        Int_t                GetID()                                            { return fID;                           };
        void                 SetID(Int_t value)                                 { fID = value;                          };
        Int_t                GetStreamID()                                      { return fStreamID;                     };
        void                 SetStreamID(Int_t value)                           { fStreamID = value;                    };
        Double_t             GetTime()                                          { return fTime;                         };
        void                 SetTime(Double_t value)                            { fTime = value;                        };

        Int_t                GetFirstGenePartIndex()                            { return fFirstGenePartIndex;           };
        void                 SetFirstGenePartIndex(Int_t value)                 { fFirstGenePartIndex = value;          };
        Int_t                GetNGeneParts()                                    { return fNGeneParts;                   };
        void                 SetNGeneParts(Int_t value)                         { fNGeneParts = value;                  };
        Int_t                GetFirstKinePartIndex()                            { return fFirstKinePartIndex;           };
        void                 SetFirstKinePartIndex(Int_t value)                 { fFirstKinePartIndex = value;          };
        Int_t                GetNKineParts()                                    { return fNKineParts;                   };
        void                 SetNKineParts(Int_t value)                         { fNKineParts = value;                  };


    private:

        Int_t      fID;
        Int_t      fStreamID;
        Double_t   fTime;

        Int_t      fFirstGenePartIndex;
        Int_t      fNGeneParts;
        Int_t      fFirstKinePartIndex;
        Int_t      fNKineParts;

        ClassDef(EventBoundary,1);
};
#endif
