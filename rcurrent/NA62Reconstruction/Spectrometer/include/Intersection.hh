#ifndef Intersection_H
#define Intersection_H 1

#include "TVector3.h"
#include "TMath.h"

class Intersection 
{

public:
  Intersection();
  ~Intersection();

  size_t GetNCluster()
  {
    /// \MemberDescr
    /// \return Total number of clusters forming the intersection. 
    /// \EndMemberDescr
    
    return fClusterId.size();
  };
  Int_t GetClusterId(Int_t jCluster)
  {
    /// \MemberDescr
    /// \param jCluster Current Cluster.
    /// \return the element jCluster of the vector Intersection::fClusterId. 
    /// \EndMemberDescr
    
    return fClusterId[jCluster];
  };
  Double_t GetX()
  {
    /// \MemberDescr
    /// \return Intersection::fX. 
    /// \EndMemberDescr
    
    return fX;
  }; 
  Double_t GetY()
  {
    /// \MemberDescr
    /// \return Intersection::fY. 
    /// \EndMemberDescr
    
    return fY;
  }; 
  Double_t GetU()
  {
    /// \MemberDescr
    /// \return Intersection::fU. 
    /// \EndMemberDescr
    
    return fU;
  }; 
  Double_t GetV()
  {
    /// \MemberDescr
    /// \return Intersection::fV. 
    /// \EndMemberDescr
    
    return fV;
  }; 
  Double_t GetXcoor()
  {
    /// \MemberDescr
    /// \return Intersection::fXcoor. 
    /// \EndMemberDescr
    
    return fXcoor;
  }; 
  Double_t GetYcoor()
  {
    /// \MemberDescr
    /// \return Intersection::fYcoor. 
    /// \EndMemberDescr
    
    return fYcoor;
  }; 
  Double_t GetSlopeX()
  {
    /// \MemberDescr
    /// \return Intersection::fSlopeX. 
    /// \EndMemberDescr
    
    return fSlopeX;
  }; 
  Double_t GetSlopeY()
  {
    /// \MemberDescr
    /// \return Intersection::fSlopeY. 
    /// \EndMemberDescr
    
    return fSlopeY;
  }; 
  Double_t GetSlopeU()
  {
    /// \MemberDescr
    /// \return Intersection::fSlopeU. 
    /// \EndMemberDescr
    
    return fSlopeU;
  }; 
  Double_t GetSlopeV()
  {
    /// \MemberDescr
    /// \return Intersection::fSlopeV. 
    /// \EndMemberDescr
    
    return fSlopeV;
  }; 
  Double_t GetSlopeXcoor()
  {
    /// \MemberDescr
    /// \return Intersection::fSlopeXcoor. 
    /// \EndMemberDescr
    
    return fSlopeXcoor;
  }; 
  Double_t GetSlopeYcoor()
  {
    /// \MemberDescr
    /// \return Intersection::fSlopeYcoor. 
    /// \EndMemberDescr
    
    return fSlopeYcoor;
  }; 
  Int_t GetPhysical()
  {
    /// \MemberDescr
    /// \return Intersection::fPhysical. 
    /// \EndMemberDescr
    
    return fPhysical;
  };
  Int_t GetFlag()
  {
    /// \MemberDescr
    /// \return Intersection::fFlag. 
    /// \EndMemberDescr

    return fFlag;
  };
  Int_t GetUsed()
  {
    /// \MemberDescr
    /// \return Intersection::fUsed. 
    /// \EndMemberDescr

    return fUsed;
  };
  Int_t GetType()
  {
    /// \MemberDescr
    /// \return Number of clusters forming the intersection. 
    /// \EndMemberDescr

    return fClusterId.size();
  };
  Int_t GetNoHitFlag(Int_t jView)
  {
    /// \MemberDescr
    /// \param jView Current view.
    /// \return Intersection::SetTheView(Int_t) with argument jView. 
    /// \EndMemberDescr
    
    return SetTheView(jView);
  }; 
  Double_t GetQuality()
  {
    return fQuality;
  }
  Double_t GetTrailingTime()
  {
    return fTrailingTime;
  }
  Int_t GetSubType()
  {
    return fSubType;
  }
  size_t GetNViews()
  {
    
    return fViewId.size();
  };
  Int_t GetViewId(Int_t jCluster)
  {
    return fViewId[jCluster];
  };

  void SetSubType(Int_t val){fSubType=val;};
  void SetClusterId(Int_t val){fClusterId.push_back(val);};
  void SetClusterId(Int_t j,Int_t val){fClusterId[j]=val;};
  void SetX(Double_t val){fX=val;}; 
  void SetY(Double_t val){fY=val;}; 
  void SetU(Double_t val){fU=val;}; 
  void SetV(Double_t val){fV=val;}; 
  void SetXcoor(Double_t val){fXcoor=val;}; 
  void SetYcoor(Double_t val){fYcoor=val;}; 
  void SetSlopeX(Double_t val){fSlopeX=val;}; 
  void SetSlopeY(Double_t val){fSlopeY=val;}; 
  void SetSlopeU(Double_t val){fSlopeU=val;}; 
  void SetSlopeV(Double_t val){fSlopeV=val;}; 
  void SetSlopeXcoor(Double_t val){fSlopeXcoor=val;}; 
  void SetSlopeYcoor(Double_t val){fSlopeYcoor=val;}; 
  void SetPhysical(Int_t val){fPhysical=val;};
  void SetFlag(Int_t val){fFlag=val;};
  void SetUsed(Int_t val){fUsed=val;};
  void SetQuality(Double_t val){fQuality=val;};
  void SetTrailingTime(Double_t val){fTrailingTime=val;};
  void SetViewId(Int_t val){fViewId.push_back(val);};
  void SetViewId(Int_t j,Int_t val){fViewId[j]=val;};
  void SetCoordinate(Bool_t,Double_t *val);

  void SetSlope();

  void Reset();

private:
  Double_t fSQ2; 
  Int_t SetTheView(Int_t); 
  std::vector<Int_t> fClusterId; ///< Vector of the Id of the clusters forming the intersection. 
  Double_t fX; ///< x coordinate of the intersection as measured in the corresponding views.
  Double_t fY; ///< y coordinate of the intersection as measured in the corresponding views.
  Double_t fU; ///< u coordinate of the intersection as measured in the corresponding views.
  Double_t fV; ///< v oordinate of the intersectino as measured in the corresponding views.
  Double_t fXcoor; ///< x coordinate of the intersection computed from the existing views. 
  Double_t fYcoor; ///< x coordinate of the intersection computed from the existing views. 
  Double_t fSlopeX; ///< Slope in X view of the intersection estimated by using the X slope of the corresponding clusters.
  Double_t fSlopeY; ///< Slope in Y view of the intersection estimated by using the Y slope of the corresponding clusters.
  Double_t fSlopeU; ///< Slope in U view of the intersection estimated by using the U slope of the corresponding clusters.
  Double_t fSlopeV; ///< Slope in V view of the intersection estimated by using the V slope of the corresponding clusters.
  Double_t fSlopeXcoor; ///< Slope in X view of the intersection estimated by using the slopes of the corresponding clusters.
  Double_t fSlopeYcoor; ///< Slope in Y view of the intersection estimated by using the slopes of the corresponding clusters.
  Int_t fFlag; ///< Flag for definition of chamber-hits with coordinates in common with other chamber-hits (see ChamberHitCollector::FakeHitRejection). 
  Int_t fPhysical; ///< Flag for definition of a physical chamber-hit (see ChamberHitCollector::FakeHitRejection). 
  Int_t fUsed; ///< Flag for definition of chamber-hits already used for track pattern recognition (see TrackCollector).
  Double_t fQuality;
  Double_t fTrailingTime;
  Int_t fSubType;
  std::vector<Int_t> fViewId;
};
#endif
