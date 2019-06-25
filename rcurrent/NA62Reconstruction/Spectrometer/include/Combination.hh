#ifndef Combination_H
#define Combination_H 1

#include <RtypesCore.h>
#include <vector>
#include <cmath>

class Combination
{

public:
  Combination();
  ~Combination() = default;
  Combination(const Combination&);

  Int_t GetNClusters()             const {return (Int_t)fClusterId.size();}
  Int_t GetClusterId(Int_t val)    const {return fClusterId[val];}
  Int_t GetViewId(Int_t val)       const {return fViewId[val];}
  Int_t GetChamberId(Int_t val)    const {return fChamberId[val];}
  Double_t GetX0()                 const {return fX0;}
  Double_t GetY0()                 const {return fY0;}
  Double_t GetZ0()                 const {return fZ0;}
  Double_t GetThetaX()             const {return fThetaX;}
  Double_t GetThetaY()             const {return fThetaY;}
  Double_t GetP()                  const {return fP;}
  Int_t GetNViewAfterMagnet()      const {return fNViewAfterMagnet;}
  Double_t GetChi2()               const {return fChi2;}
  Double_t GetSigmaX0()            const {return fSigmaX0;}
  Double_t GetSigmaY0()            const {return fSigmaY0;}
  Double_t GetSigmaThetaX()        const {return fSigmaThetaX;}
  Double_t GetSigmaThetaY()        const {return fSigmaThetaY;}
  Double_t GetSigmaP()             const {return fSigmaP;}
  Int_t GetNTotalHits()            const {return fNTotalHits;}
  Int_t GetType()                  const {return fType; }
  Double_t GetHDelta()             const {return fHDelta; }
  Double_t GetDeltaX()             const {return fDeltaX; }
  Double_t GetTrailingTime()       const {return fTrailingTime; }
  Int_t GetChamberHitId(Int_t val) const {return fChamberHitId[val]; }
  Double_t GetQuality()            const {return fQuality; }
  Double_t GetSubHDelta(Int_t val) const {return fSubHDelta[val]; }
  Double_t GetSubThetaY(Int_t val) const {return fSubThetaY[val]; }
  Int_t GetSubType(Int_t val)      const {return fSubType[val]; }
  Int_t GetNCommon()               const {return fNCommon; }
  Int_t GetNChambers() const;

  void SetClusterId(Int_t val){fClusterId.push_back(val);}; 
  void SetViewId(Int_t val){fViewId.push_back(val);}; 
  void SetChamberId(Int_t val){fChamberId.push_back(val);}; 
  void SetChamberHitId(Int_t val){fChamberHitId.push_back(val);}; 
  void SetX0(Double_t val) {fX0=val;};
  void SetY0(Double_t val) {fY0=val;};
  void SetZ0(Double_t val) {fZ0=val;};
  void SetThetaX(Double_t val) {fThetaX=val;};
  void SetThetaY(Double_t val) {fThetaY=val;};
  void SetP(Double_t val) {fP=val;};
  void SetNViewAfterMagnet(Int_t val) {fNViewAfterMagnet=val;};
  void SetChi2(Double_t val) {fChi2=val;};
  void SetSigmaX0(Double_t val) {fSigmaX0=val;};
  void SetSigmaY0(Double_t val) {fSigmaY0=val;};
  void SetSigmaThetaX(Double_t val) {fSigmaThetaX=val;};
  void SetSigmaThetaY(Double_t val) {fSigmaThetaY=val;};
  void SetSigmaP(Double_t val) {fSigmaP=val;};
  void SetNTotalHits(Int_t val) {fNTotalHits=val;};

  void SetType(Int_t val) {fType=val;};  
  void SetHDelta(Double_t val) {fHDelta=val;};
  void SetDeltaX(Double_t val) {fDeltaX=val;};
  void SetTrailingTime(Double_t val) {fTrailingTime=val;};
  void SetQuality(Double_t, Double_t);
  void SetSubHDelta(Int_t j, Double_t val) {fSubHDelta[j]=val;};  
  void SetSubThetaY(Int_t j, Double_t val) {fSubThetaY[j]=val;};  
  void SetSubType(Int_t j, Int_t val) {fSubType[j]=val;};  
  void SetNCommon(Int_t val) {fNCommon=val;};

  void AddCluster(Int_t,Int_t,Int_t);
  void Sort();
  void Reset();
  Double_t Project(Double_t, Double_t);

private:
  std::vector<Int_t> fClusterId; ///< Vector of the id of the clusters forming the combination. 
  std::vector<Int_t> fViewId; ///< Vector of the id  of the view of the clusters forming the combination.
  std::vector<Int_t> fChamberId; ///< Vector of the id of the chamber of the clusters forming the combianation.
  Double_t fX0; ///< X0 at Z0 of the track fitted in TrackCollector associated to the combination.
  Double_t fY0; ///< Y0 at Z0 of the track fitted in TrackCollector associated to the combination.
  Double_t fZ0; ///< Starting position of the track fitted in TrackCollector associated to the combination.
  Double_t fThetaX; ///< Slope in X of the track fitted in TrackCollector associated to the combination.
  Double_t fThetaY; ///< Slope in Y of the track fitted in TrackCollector associated to the combination.
  Double_t fP; ///< Momentum of the track fitted in TrackCollector associated to the combination.
  Int_t fNViewAfterMagnet; ///< Number of chambers after the magnet used to form the candidate combination.
  Double_t fChi2; ///< Chi2 of the track fitted in TrackCollector associated to the combination.
  Double_t fSigmaX0; ///< Estimated uncertainty on fX0 (given by the correlation matrix of the fit).
  Double_t fSigmaY0; ///< Estimated uncertainty on fY0 (given by the correlation matrix of the fit).
  Double_t fSigmaThetaX; ///< Estimated uncertainty on fThetaX (given by the correlation matrix of the fit).
  Double_t fSigmaThetaY; ///< Estimated uncertainty on fThetaY (given by the correlation matrix of the fit).
  Double_t fSigmaP; ///< Estimated uncertainty on fP (given by the correlation matrix of the fit).
  Int_t fNTotalHits; ///< Total number of straw hits forming the combination

  // Constant
  Double_t fDMag; ///< Thickness of the magnet.
  Double_t fZMag; ///< Position of the front face of the magnet.
  Double_t fBMag; ///< Intensity of the magnetic field.
  Double_t fEC; ///< e*c

  void SortChamber();
  void SortView(Int_t);

  // Added
  Int_t fType;
  Double_t fHDelta;
  Double_t fDeltaX;
  Double_t fQuality;
  Double_t fTrailingTime;
  std::vector<Int_t> fChamberHitId;
  Double_t fSubHDelta[4];
  Double_t fSubThetaY[4];
  Int_t fSubType[4];
  Int_t fNCommon;

};
#endif
