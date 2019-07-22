// --------------------------------------------------------------
// History:
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-07-23
//
// --------------------------------------------------------------

/// \class GigaTrackerCluster
/// \Brief
/// In this class, clusters are created gathering together adjacent pixels.
/// \EndBrief

#include "GigaTrackerCluster.hh"
#include "TGigaTrackerHit.hh"
#include "TRecoVEvent.hh"
#include "TRecoGigaTrackerHit.hh"

GigaTrackerCluster::GigaTrackerCluster() : TRecoVCandidate()
{

}



Bool_t GigaTrackerCluster::ProposeDigi(Int_t RecoHitIndex){
    
  if(GetNHits() == 0){
    AddHit(RecoHitIndex);
    return kTRUE;
  }
  TRecoGigaTrackerHit* RecoHit = static_cast<TRecoGigaTrackerHit*>(GetEvent()->GetHit(RecoHitIndex));
  TVector3 RecoHitPosition = RecoHit->GetPosition();
  for(Int_t iRecoHit = 0 ; iRecoHit < GetNHits() ; iRecoHit++){
    TRecoGigaTrackerHit *OwnRecoHit = static_cast<TRecoGigaTrackerHit*>(GetHit(iRecoHit));
    //    double Time = OwnRecoHit->GetTime();
    TVector3 OwnRecoHitPosition = OwnRecoHit->GetPosition();
    if( (RecoHitPosition - OwnRecoHitPosition).Mag() < 0.43 ){
      AddHit(RecoHitIndex);
      return kTRUE;
    }
  }
    return kFALSE;
}

Double_t GigaTrackerCluster::GetMinDistance(GigaTrackerCluster *OtherCluster){
    Double_t MinDistance = 99999999. ;
    Int_t NRecoHits1 = GetNHits();
    Int_t NRecoHits2 = OtherCluster->GetNHits();
    for(Int_t iRecoHit1 = 0 ; iRecoHit1 < NRecoHits1 ; iRecoHit1++){
        TRecoGigaTrackerHit* RecoHit1 = static_cast<TRecoGigaTrackerHit*>(GetHit(iRecoHit1));
        TVector3 Position1 = RecoHit1->GetPosition();   
        for(Int_t iRecoHit2 = 0 ; iRecoHit2 < NRecoHits2 ; iRecoHit2++){
            TRecoGigaTrackerHit* RecoHit2 = static_cast<TRecoGigaTrackerHit*>(OtherCluster->GetHit(iRecoHit2));
            TVector3 Position2 = RecoHit2->GetPosition();   
            Double_t Distance = (Position1 - Position2).Mag() ;
            if(Distance < MinDistance) MinDistance = Distance;
        }
    }
    return MinDistance;
}

void GigaTrackerCluster::MergeWithCluster(GigaTrackerCluster *OtherCluster){
    Int_t NRecoHits2 = OtherCluster->GetNHits();
    for(Int_t iRecoHit2 = 0 ; iRecoHit2 < NRecoHits2 ; iRecoHit2++){   // loop on the RecoHits of the OtherCluster
        Int_t RecoHitIndex2 = OtherCluster->GetHitsIndexes()[iRecoHit2] ;
        AddHit(RecoHitIndex2);
    }
}

TVector3 GigaTrackerCluster::GetPosition(){
    Double_t AverageX = 0.;
    Double_t AverageY = 0.;
    Double_t PositionZ = -999999999.;
    Int_t NRecoHits = GetNHits();
    for(Int_t iRecoHit=0 ; iRecoHit < NRecoHits ; iRecoHit++){
        Int_t RecoHitIndex = GetHitsIndexes()[iRecoHit] ;
        TRecoGigaTrackerHit* RecoHit = static_cast<TRecoGigaTrackerHit*>(GetEvent()->GetHit(RecoHitIndex));
        TVector3 RecoHitPosition = RecoHit->GetPosition();
        AverageX += RecoHitPosition.X() ;
        AverageY += RecoHitPosition.Y() ;
        PositionZ = RecoHit->GetPosition().Z();
    }
    AverageX = AverageX / (Double_t)NRecoHits ;
    AverageY = AverageY / (Double_t)NRecoHits ;
    TVector3 ClusterPosition = TVector3(AverageX,AverageY,PositionZ);
    return ClusterPosition;
}

Double_t GigaTrackerCluster::GetTime(){
    Double_t AverageTime = 0.;
    Int_t NRecoHits = GetNHits();
    for(Int_t iRecoHit=0 ; iRecoHit < NRecoHits ; iRecoHit++){
        Int_t RecoHitIndex = GetHitsIndexes()[iRecoHit] ;
        TRecoGigaTrackerHit* RecoHit = static_cast<TRecoGigaTrackerHit*>(GetEvent()->GetHit(RecoHitIndex));
        Double_t Time = RecoHit->GetTime();
        AverageTime += Time ;
    }
    AverageTime = AverageTime / (Double_t)NRecoHits ;
    return AverageTime;
}
