// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSpectrometerCandidate_H
#define TRecoSpectrometerCandidate_H

#include "TRecoVCandidate.hh"
#include "TLorentzVector.h"

class TRecoSpectrometerCandidate : public TRecoVCandidate {

public:
  TRecoSpectrometerCandidate();
  ~TRecoSpectrometerCandidate() {}
  void Clear(Option_t* = "");

  void SetSlopeXBeforeFit(Double_t val)                { fSlopeXBeforeFit=val;             }
  void SetSlopeYBeforeFit(Double_t val)                { fSlopeYBeforeFit=val;             }
  void SetMomentumBeforeFit(Double_t val)              { fMomentumBeforeFit=val;           }
  void SetCombinationTotalQuality(Double_t val)        { fCombinationTotalQuality=val;     }
  void SetCombinationHoughQuality(Double_t val)        { fCombinationHoughQuality=val;     }
  void SetSlopeXBeforeMagnet(Double_t val)             { fSlopeXBeforeMagnet=val;          }
  void SetSlopeYBeforeMagnet(Double_t val)             { fSlopeYBeforeMagnet=val;          }
  void SetPositionBeforeMagnet(TVector3 val)           { fPositionBeforeMagnet=val;        }
  void SetSlopeXAfterMagnet(Double_t val)              { fSlopeXAfterMagnet=val;           }
  void SetSlopeYAfterMagnet(Double_t val)              { fSlopeYAfterMagnet=val;           }
  void SetPositionAfterMagnet(TVector3 val)            { fPositionAfterMagnet=val;         }
  void SetMomentum(Double_t val)                       { fMomentum=val;                    }
  void SetCharge(Int_t val)                            { fCharge=val;                      }
  void SetChi2(Double_t val)                           { fChi2=val;                        }
  void SetNChambers(Int_t val)                         { fNChambers=val;                   }
  void SetChamberId(Int_t ich, Int_t val)              { fChamberId[ich]=val;              }
  void SetNTotalHitPerChamber(Int_t ich, Int_t val)    { fNTotalHitPerChamber[ich]=val;    }
  void SetNViewsPerChamber(Int_t ich, Int_t val)       { fNViewsPerChamber[ich]=val;       }
  void SetN2HitClusterPerChamber(Int_t ich, Int_t val) { fN2HitClusterPerChamber[ich]=val; }
  void SetN3HitClusterPerChamber(Int_t ich, Int_t val) { fN3HitClusterPerChamber[ich]=val; }
  void SetCovariance(Int_t i, Int_t j, Double_t val)   { fCovariance[i][j]=val;            }
  void SetPositionBeforeFit(TVector3 val)              { fPositionBeforeFit = val;         }
  void SetLeadingTime(Double_t val)                    { fLeadingTime = val;               }
  Double_t GetLeadingTime()                    { return fLeadingTime;                 }
  TVector3 GetPositionBeforeFit()              { return fPositionBeforeFit;           }
  Double_t GetSlopeXBeforeFit()                { return fSlopeXBeforeFit;             }
  Double_t GetSlopeYBeforeFit()                { return fSlopeYBeforeFit;             }
  Double_t GetChargeTimesMomentumBeforeFit()   { return fMomentumBeforeFit;           } // signed
  Double_t GetMomentumBeforeFit()              { return fabs(fMomentumBeforeFit);     } // unsigned
  Double_t GetCombinationTotalQuality()        { return fCombinationTotalQuality;     }
  Double_t GetCombinationHoughQuality()        { return fCombinationHoughQuality;     }
  Double_t GetSlopeXBeforeMagnet()             { return fSlopeXBeforeMagnet;          }
  Double_t GetSlopeYBeforeMagnet()             { return fSlopeYBeforeMagnet;          }
  TVector3 GetPositionBeforeMagnet()           { return fPositionBeforeMagnet;        }
  Double_t GetSlopeXAfterMagnet()              { return fSlopeXAfterMagnet;           }
  Double_t GetSlopeYAfterMagnet()              { return fSlopeYAfterMagnet;           }
  TVector3 GetPositionAfterMagnet()            { return fPositionAfterMagnet;         }
  Double_t GetMomentum()                       { return fMomentum;                    }
  Int_t    GetCharge()                         { return fCharge;                      }
  Double_t GetChi2()                           { return fChi2;                        }
  Int_t    GetNChambers()                      { return fNChambers;                   }
  Int_t    GetChamberId(Int_t ich)             { return fChamberId[ich];              }
  Int_t    GetNTotalHitPerChamber(Int_t ich)   { return fNTotalHitPerChamber[ich];    }
  Int_t    GetNViewsPerChamber(Int_t ich)      { return fNViewsPerChamber[ich];       }
  Int_t    GetN2HitClusterPerChamber(Int_t ich){ return fN2HitClusterPerChamber[ich]; }
  Int_t    GetN3HitClusterPerChamber(Int_t ich){ return fN3HitClusterPerChamber[ich]; }
  Double_t GetCovariance(Int_t i, Int_t j)     { return fCovariance[i][j];            }

  Double_t xAtBeforeMagnet(Double_t z); ///< X position of track before magnet in a certain Z plane
  Double_t yAtBeforeMagnet(Double_t z); ///< Y position of track before magnet in a certain Z plane
  Double_t xAtAfterMagnet (Double_t z); ///< X position of track after magnet in a certain Z plane
  Double_t yAtAfterMagnet (Double_t z); ///< Y position of track after magnet in a certain Z plane
  Double_t xAt(Double_t z); ///< Either xAtBeforeMagnet() or xAtAfterMagnet(), depending on the Z plane
  Double_t yAt(Double_t z); ///< Either yAtBeforeMagnet() or yAtAfterMagnet(), depending on the Z plane
  TVector3 GetThreeMomentumBeforeMagnet();
  TVector3 GetThreeMomentumAfterMagnet();
  static constexpr Double_t kZReferenceBeforeMagnet = 180000; ///< [mm], Z reference plane position before the magnet
  static constexpr Double_t kZReferenceAfterMagnet  = 222000; ///< [mm], Z reference plane position after the magnet

  TVector3 GetBlueFieldCorrectedPositionAtZ(Double_t);
  TVector3 GetBlueFieldCorrectedMomentumAtZ(Double_t);

private:
     Double_t fSlopeXBeforeFit;           ///< Track candidate slope in XZ plane from pattern recognition
     Double_t fSlopeYBeforeFit;           ///< Track candidate slope in YZ plane from pattern recognition
     TVector3 fPositionBeforeFit;         ///< X,Y position of the track candidate from pattern recognition (reference plane at Z = 0)
     Double_t fMomentumBeforeFit;         ///< Momentum*Charge of the track candidate from pattern recognition
     Double_t fCombinationTotalQuality;   ///< Quality of the track candidate from pattern recognition (to be used for track candidate built with 4 chambers)
     Double_t fCombinationHoughQuality;   ///< Quality of the Hough transformed in the YZ plane used in pattern recognition (usable for candidates with either 3 or 4 chambers)
     Double_t fSlopeXBeforeMagnet;        ///< Track slope in XZ before the MNP33 after fit
     Double_t fSlopeYBeforeMagnet;        ///< Track slope in YZ before the MNP33 after fit
     TVector3 fPositionBeforeMagnet;      ///< X,Y position of the track before MNP33 after fit
     Double_t fSlopeXAfterMagnet;         ///< Track slope in XZ after the MNP33 after fit
     Double_t fSlopeYAfterMagnet;         ///< Track slope in YZ after the MNP33 after fit
     TVector3 fPositionAfterMagnet;       ///< X,Y position of the track after MNP33 after fit
     Double_t fMomentum;                  ///< Absolute value of track momentum after fit
     Int_t    fCharge;                    ///< Charge of the track
     Double_t fChi2;                      ///< Chi2 of the fit
     Double_t fLeadingTime;               ///< Leading time of the track
     Int_t    fNChambers;                 ///< Number of chambers used to make the track candidate
     Int_t    fChamberId[4];              ///< Id of the chambers used to make the track candidate
     Int_t    fNTotalHitPerChamber[4];    ///< Number of straws per chambers used to make the track candidate
     Int_t    fNViewsPerChamber[4];       ///< Number of view per chambers used to make the track candidate
     Int_t    fN2HitClusterPerChamber[4]; ///< Number of 2-straw clusters per chamber used to make the track candidate
     Int_t    fN3HitClusterPerChamber[4]; ///< Number of 3-straw clusters per chamber used to make the track candidate
     Double_t fCovariance[5][5];          ///< Covariance matrix provided by the fit (slopes, positions, momentum -> thetaXY, XY, P)
                                          ///< s2(thetaX)  s(thetaX)s(thetaY)  s(thetaX)s(X)  s(thetaX)s(Y)  s(thetaX)s(1/P)
                                          ///<                s2(thetaY)       s(thetaY)s(X)  s(thetaY)s(Y)  s(thetaY)s(1/P)
                                          ///<                                     s2(X)        s(X)s(Y)       s(X)s(1/P)
                                          ///<                                                   s2(Y)         s(Y)s(1/P)
                                          ///<                                                                   s2(1/P)

     ClassDef(TRecoSpectrometerCandidate,1);
};
#endif
