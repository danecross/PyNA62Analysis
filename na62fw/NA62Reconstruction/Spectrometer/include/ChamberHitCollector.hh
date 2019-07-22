#ifndef ChamberHitCollector_H
#define ChamberHitCollector_H 1

#include "SpectrometerGeometry.hh"
#include "SpectrometerParameters.hh"
#include "ViewHitCollector.hh"
#include "Intersection.hh"
////#include "IntersectionCollector.hh"
#include "TMath.h"

class SpectrometerParameters;

class ChamberHitCollector
{

public:
  explicit ChamberHitCollector(Int_t);
  virtual ~ChamberHitCollector();

  void ReconstructHit();
  void Reset();
  void Init();

  size_t GetNHit() 
  {
    /// \MemberDescr
    /// \return Total number of chamber-hits in the current chamber. 
    /// \EndMemberDescr

    return fChamberHit.size();
  };
  ViewHitCollector *GetView(Int_t jView)
  {
    /// \MemberDescr
    /// \param jView Current view (XYUV).
    /// \return The pointer to the view jView of the vector ChamberHitCollector::fViewHitCollector. 
    ///
    /// \EndMemberDescr
  
    return fViewHitCollector.at(jView);
  };   
////  IntersectionCollector *GetIntersectionType(Int_t jInters)
////  {
////    /// \MemberDescr
////    /// \param jInters Intersection type.
////    /// \return The pointer to the intersection of type jInters of the vector ChamberHitCollector::fIntersectionCollector. 
////    ///
////    /// \EndMemberDescr
////
////    return fIntersectionCollector->at(jInters);
////  };
  std::vector<Intersection>::iterator GetHit(Int_t j) 
  {
    /// \MemberDescr
    /// \param Identifier of the chamber hits.
    /// \return The pointer to the element j of the vector ChamberHitCollector::fChamberHit. 
    ///
    /// \EndMemberDescr
    
    return fChamberHit.begin()+j;
  };
  Int_t GetNHitType(Int_t j) 
  { 
    /// \MemberDescr
    /// \param type of intersection.
    /// \return ChamberHitCollector::fNHit. 
    ///
    /// \EndMemberDescr

    return fNHit[j]; 
  }; 

protected:
  SpectrometerParameters& fPar;
  SpectrometerGeometry& fGeo;
  std::vector<ViewHitCollector *> fViewHitCollector; ///< Vector of pointers to the ensambles of reconstructed tube-hits and clusters in the views.
////  std::vector<IntersectionCollector *> *fIntersectionCollector; ///< Pointer to the vector of pointers to the ensambles of the various types of intersections.
  std::vector<Intersection> fChamberHit; ///< Vector of reconstructed chamber-hits.
private:
  static const Int_t NVIEWS[15];  ///< Number of views of all the possible intersections. Also the 1-view intersections are included.
  static const Int_t INTCODE[15]; ///< Code of all the possible intersections. The coded views forming the intersections are (xy xu xv yu yv uv xyu xyv xuv yuv xyuv).
  static const Int_t CODEINT[15]; ///< Second type of code for the intersections.
  Int_t fChamber; ///< Chamber under study.
  Int_t fNHit[4]; ///< Number of intersection per type (type=number of views forming an intersection).
////  void Create2ViewIntersection(ViewHitCollector*,ViewHitCollector*,IntersectionCollector*);
  void AddHit(Int_t);
  Bool_t fDATA;
  void CreateIntersection();
  void StoreSingleHit();
  Double_t fDistCut2;
  Int_t IntType2(Int_t, Int_t); 
  void ComputeCoordinate(const Int_t &, const Double_t &, const Double_t &, Double_t*);
  void ComputeCoordinate(const Int_t &, const Double_t &, Double_t*);
  Int_t IntType3(Int_t, Int_t); 
  Int_t IntersectionQuality(Int_t, Double_t *, Double_t, Double_t *);
  void UpdateCoordinate(Int_t, Double_t *, Double_t);
  Int_t StoreHit(Double_t *, Int_t *, Double_t *);
  Bool_t AcceptanceTwoView(Double_t *);
  Int_t IntType1(Int_t); 
};
#endif
