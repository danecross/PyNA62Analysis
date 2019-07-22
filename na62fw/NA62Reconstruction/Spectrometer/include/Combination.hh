#ifndef Combination_H
#define Combination_H 1

#include <RtypesCore.h>
#include <vector>
#include <cmath>

class Combination
{
public:
  Int_t GetNClusters()             const {return (Int_t)fClusterId.size();}
  Int_t GetClusterId(Int_t val)    const {return fClusterId[val];}
  Int_t GetViewId(Int_t val)       const {return fViewId[val];}
  Int_t GetChamberId(Int_t val)    const {return fChamberId[val];}
  Int_t GetChamberHitId(Int_t val) const {return fChamberHitId[val];}
  Int_t GetNTotalHits()            const {return fNTotalHits;}
  Int_t GetType()                  const {return fType; }
  Int_t GetNCommon()               const {return fNCommon; }
  Int_t GetNChambers() const;

  Double_t GetX0()                 const {return fX0;}
  Double_t GetY0()                 const {return fY0;}
  Double_t GetZ0()                 const {return fZ0;}
  Double_t GetThetaX()             const {return fThetaX;}
  Double_t GetThetaY()             const {return fThetaY;}
  Double_t GetP()                  const {return fP;}
  Double_t GetHDelta()             const {return fHDelta;}
  Double_t GetDeltaX()             const {return fDeltaX;}
  Double_t GetQuality()            const {return fQuality;}
  Double_t GetTrailingTime()       const {return fTrailingTime; }

  void SetClusterId(Int_t val){fClusterId.push_back(val);}
  void SetViewId(Int_t val){fViewId.push_back(val);}
  void SetChamberId(Int_t val){fChamberId.push_back(val);}
  void SetChamberHitId(Int_t val) {fChamberHitId.push_back(val);}
  void SetNTotalHits(Int_t val) {fNTotalHits=val;}
  void SetType(Int_t val) {fType=val;}
  void SetNCommon(Int_t val) {fNCommon=val;}

  void SetX0(Double_t val) {fX0=val;}
  void SetY0(Double_t val) {fY0=val;}
  void SetZ0(Double_t val) {fZ0=val;}
  void SetThetaX(Double_t val) {fThetaX=val;}
  void SetThetaY(Double_t val) {fThetaY=val;}
  void SetP(Double_t val) {fP=val;}
  void SetHDelta(Double_t val) {fHDelta=val;}
  void SetDeltaX(Double_t val) {fDeltaX=val;}
  void SetQuality(Double_t hd, Double_t dx);
  void SetTrailingTime(Double_t val) {fTrailingTime=val;}

  void AddCluster(Int_t,Int_t,Int_t);
  void Sort();
  void Reset();

private:
  std::vector<Int_t> fClusterId; ///< Vector of the id of the clusters forming the combination. 
  std::vector<Int_t> fViewId; ///< Vector of the id  of the view of the clusters forming the combination.
  std::vector<Int_t> fChamberId; ///< Vector of Chamber IDs with clusters forming the combianation.
  std::vector<Int_t> fChamberHitId; ///< Vector of the id of the reconstructed chamber hits forming the combianation.
  Int_t fNTotalHits = -1; ///< Total number of straw hits forming the combination
  Int_t fType = -1; ///< Combination type, either 3 or 4 chambers
  Int_t fNCommon = -1; ///< Number of common chamber hits with another combination
  Double_t fX0 = -9999.; ///< X0 at Z0 of the track fitted in TrackCollector associated to the combination.
  Double_t fY0 = -9999.; ///< Y0 at Z0 of the track fitted in TrackCollector associated to the combination.
  Double_t fZ0 = -9999.; ///< Starting position of the track fitted in TrackCollector associated to the combination.
  Double_t fThetaX = -9999.; ///< Slope in X of the track fitted in TrackCollector associated to the combination.
  Double_t fThetaY = -9999.; ///< Slope in Y of the track fitted in TrackCollector associated to the combination.
  Double_t fP = -9999.; ///< Momentum of the track fitted in TrackCollector associated to the combination.
  Double_t fHDelta = -9999.; ///< Hough quality of the Combination
  Double_t fDeltaX = -9999.; ///< Delta quality of the Combination
  Double_t fQuality = -9999.; ///< Total quality of the Combination
  Double_t fTrailingTime = -9999.; ///< Trailing time of the Combination

  void SortChamber();
  void SortView(Int_t);
};
#endif
