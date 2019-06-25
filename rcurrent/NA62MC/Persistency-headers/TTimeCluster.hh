// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-24
//
// --------------------------------------------------------------
#ifndef TTimeCluster_H
#define TTimeCluster_H

#include "TDigiVCandidate.hh"

class TTimeCluster : public TDigiVCandidate {

    public:

        TTimeCluster();
        ~TTimeCluster(){};
        void Clear(Option_t* = "");

        Bool_t AddDigi(Int_t);
        Int_t Compare(const TObject *obj) const;
        Bool_t IsSortable() const { return kTRUE; }

    public:

        Double_t             GetAverage() const                                 { return fAverage;                      };
        void                 SetAverage(Double_t value)                         { fAverage = value;                     };
        Double_t             GetRMS() const                                     { return fRMS;                          };
        void                 SetRMS(Double_t value)                             { fRMS = value;                         };
        Double_t             GetMinTime() const                                 { return fMinTime;                      };
        void                 SetMinTime(Double_t value)                         { fMinTime = value;                     };
        Double_t             GetMaxTime() const                                 { return fMaxTime;                      };
        void                 SetMaxTime(Double_t value)                         { fMaxTime = value;                     };
        Int_t                GetStationID() const                               { return fStationID;                    };
        void                 SetStationID(Int_t value)                          { fStationID = value;                   };

    private:

        Double_t fAverage;
        Double_t fRMS;
        Double_t fMinTime;
        Double_t fMaxTime;
        Int_t fStationID;

        ClassDef(TTimeCluster,1);
};
#endif
