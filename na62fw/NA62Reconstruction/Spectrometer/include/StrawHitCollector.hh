#ifndef StrawHitCollector_H
#define StrawHitCollector_H 1

#include "TSpectrometerHit.hh"

class StrawHitCollector
{

public:
  StrawHitCollector();
  ~StrawHitCollector();

  Int_t GetN()
  {
    /// \MemberDescr
    /// \return Total number of tube-hits. 
    /// \EndMemberDescr
     
    return (Int_t)fHitId.size();
  };
  Int_t GetHitId(Int_t jHit)
  {
    /// \MemberDescr
    /// \param j Current tube-hit.
    /// \return The element j of the vector StrawHitCollector::fHitId. 
    ///
    /// \EndMemberDescr
    
    return fHitId[jHit];
  };
  void Reset();
  void AddHit(Int_t hitID, Int_t hitFlag, Bool_t good);
  Int_t GetHitFlag(Int_t jHit) { return fHitFlag[jHit]; }; 
  void  SetHitFlag(Int_t jHit, Int_t val){fHitFlag[jHit]=val; };
  Int_t GetNGoodHit() { return nGoodHit; };

private:
  std::vector<Int_t> fHitId; ///< Vector of the Id of the tube-hits belonging to a view-plane.  
  std::vector<Int_t> fHitFlag;
  Int_t nGoodHit;
};
#endif
