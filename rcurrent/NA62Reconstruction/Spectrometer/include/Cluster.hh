#ifndef Cluster_H
#define Cluster_H 1

#include "TVector3.h"

class Cluster
{

public:
  Cluster();
  ~Cluster();
  Cluster(const Cluster&);

  size_t GetNHit()
  {
    /// \MemberDescr
    /// \return Total number of tube-hits forming the cluster. 
    /// \EndMemberDescr

    return fHitId.size();
  };
  Int_t GetHitId(Int_t jHit)
  {
    /// \MemberDescr
    /// \param jHit Current tube-hit.
    /// \return The element jHit of the vector Cluster::fHitId. 
    ///
    /// \EndMemberDescr

    return fHitId[jHit];
  }; 
  TVector3 GetLocalPosition()
  {
    /// \MemberDescr
    /// \return Cluster::fLocalPosition. 
    /// \EndMemberDescr

    return fLocalPosition;
  };
  TVector3 GetPosition()
  {
    /// \MemberDescr
    /// \return Cluster::fPosition. 
    /// \EndMemberDescr

    return fPosition;
  };
  Double_t GetLocalSlope()
  {
    /// \MemberDescr
    /// \return Cluster::fLocalSlope. 
    /// \EndMemberDescr

    return fLocalSlope;
  };
  Double_t GetError()
  {
    /// \MemberDescr
    /// \return Cluster::fError. 
    /// \EndMemberDescr

    return fError;
  };
  Double_t GetUsedForHit()
  {
    /// \MemberDescr
    /// \return Cluster::fUsedForHit. 
    /// \EndMemberDescr

    return fUsedForHit;
  };
  Double_t GetFlagUsed()
  {
    /// \MemberDescr
    /// \return Cluster::fFlagUsed. 
    /// \EndMemberDescr

    return fFlagUsed;
  };
  Double_t GetTrailingTime()
  {
    return fTrailingTime;
  }
  Double_t GetQuality()
  {
    return fQuality;
  }
  Int_t GetEdge() {
    return fEdge;
  }
  void SetHitId(Int_t val){fHitId.push_back(val);}; 
  void SetLocalPosition(TVector3 val){fLocalPosition=val;};
  void SetPosition(TVector3 val){fPosition=val;};
  void SetLocalSlope(Double_t val){fLocalSlope=val;};
  void SetError(Double_t val){fError=val;};
  void SetUsedForHit(Int_t val) {fUsedForHit=val;};
  void SetFlagUsed(Int_t val){fFlagUsed=val;};
  void SetTrailingTime(Double_t val){fTrailingTime=val;};
  void SetQuality(Double_t val){fQuality=val;};
  void SetEdge(Int_t val){fEdge=val;};
 
  void AddHit(Int_t);   

  void Reset();

private:
  std::vector<Int_t> fHitId; ///< Vector of the id of the tube-hits forming the cluster. 
  TVector3 fLocalPosition; ///< Position of the cluster in the view reference frame.
  TVector3 fPosition; ///< Position of the cluster in the laboratory reference frame.
  Double_t fLocalSlope; ///< Slope associated to a hit as extracted from the LR algorithm.
  Double_t fError; ///< Uncertainty on the straw position.
  Int_t fUsedForHit; ///< Flag which identifies the clusters used for chamber hit reconstruction (see ChamberHitCollector).
  Int_t fFlagUsed; ///< Flag which identifies the clusters used for track reconstruction (see TrackCollector).
  Double_t fTrailingTime;
  Double_t fQuality;
  Int_t fEdge;
};
#endif
