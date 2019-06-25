// --------------------------------------------------------------
// History:
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-05-04
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoGigaTrackerCandidate_H
#define TRecoGigaTrackerCandidate_H

#include "TRecoVCandidate.hh"
#include "TLorentzVector.h"

class TRecoGigaTrackerCandidate : public TRecoVCandidate {

public:
  
  TRecoGigaTrackerCandidate();
  TRecoGigaTrackerCandidate(const TRecoGigaTrackerCandidate &);
  ~TRecoGigaTrackerCandidate(){};
  
  void Clear(Option_t* = "");

  TVector3       GetMomentum(){                                    return fMomentum;                 }
  void           SetMomentum(TVector3 value){                      fMomentum = value;                }

  Double_t       GetTimeError(){                                   return fTimeError;                }
  void           SetTimeError(Double_t value){                     fTimeError = value;               }

  Double_t       GetTimeStation(Int_t StationNo);
  void           SetTimeStation(Int_t StationNo,Double_t value);

  TVector3       GetPosition(Int_t StationNo);
  void           SetPosition(Int_t StationNo,TVector3 value);

  Double_t       GetChi2X(){                                       return fChi2X;                    }
  void           SetChi2X(Double_t value){                         fChi2X = value;                   }

  Double_t       GetChi2Y(){                                       return fChi2Y;                    }
  void           SetChi2Y(Double_t value){                         fChi2Y = value;                   }

  Double_t       GetChi2Time(){                                    return fChi2Time;                 }
  void           SetChi2Time(Double_t value){                      fChi2Time = value;                }

  Double_t       GetChi2(){                                        return fChi2;                     }
  void           SetChi2(Double_t value){                          fChi2 = value;                    }

  Int_t          GetType(){                                        return fType;                     }
  void           SetType(Int_t value){                             fType = value;                    }
 
  Double_t       GetCovariance(Int_t i, Int_t j)               { return fCovariance[i][j];           }
  void           SetCovariance(Int_t i, Int_t j, Double_t val) { fCovariance[i][j]=val;              }

public:
  
  
private:
  
  TVector3 fMomentum;
  Double_t fTimeError;
  Double_t fTimeStation[3];
  TVector3 fPosition[3];

  Int_t fType;

  Double_t fChi2X;
  Double_t fChi2Y;
  Double_t fChi2Time;
  Double_t fChi2;

  Double_t fCovariance[5][5];          ///< Covariance matrix provided by the fit (slopes, positions, momentum -> thetaXY, XY, P)
                                       ///< s2(thetaX)  s(thetaX)s(thetaY)  s(thetaX)s(X)  s(thetaX)s(Y)  s(thetaX)s(1/P)
                                       ///<                s2(thetaY)       s(thetaY)s(X)  s(thetaY)s(Y)  s(thetaY)s(1/P)
                                       ///<                                     s2(X)        s(X)s(Y)       s(X)s(1/P)
                                       ///<                                                   s2(Y)         s(Y)s(1/P)
                                       ///<                                                                   s2(1/P)

  // From v2 fTime1,2,3 and corresponding functions are removed
  // From v3 fTime and corresponding functions are removed, fCovariance Matrix is added
  ClassDef(TRecoGigaTrackerCandidate,3);
};
#endif
