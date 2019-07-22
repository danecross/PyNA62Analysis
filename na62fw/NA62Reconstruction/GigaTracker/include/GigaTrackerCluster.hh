// --------------------------------------------------------------
// History:
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-07-23
//
// --------------------------------------------------------------

#ifndef GigaTrackerCluster_H
#define GigaTrackerCluster_H 1

#include "TRecoVCandidate.hh"
#include "Riostream.h"
#include "math.h"

class GigaTrackerCluster : public TRecoVCandidate
{

    public:

        GigaTrackerCluster();

        ~GigaTrackerCluster(){};

        Bool_t ProposeDigi(Int_t iDigi);
        Double_t GetMinDistance(GigaTrackerCluster*);
        void MergeWithCluster(GigaTrackerCluster*);

        TVector3 GetPosition();
        Double_t GetTime();

    private:

};
#endif
