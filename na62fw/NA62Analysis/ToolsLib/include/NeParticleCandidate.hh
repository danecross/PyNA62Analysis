// ---------------------------------------------------------------
// History:
//
// Created by Jacopo Pinzino (jacopo.pinzino@cern.ch) 2016-11-30
//
// ---------------------------------------------------------------

#ifndef NEPARTICLECANDIDATE_HH
#define NEPARTICLECANDIDATE_HH

#include "TMath.h"
#include "TVector2.h"
#include "TVector3.h"
#include <iostream>
#include <vector>

class NeParticleCandidate
{
  //parte public accessibile a tutti
public:

  NeParticleCandidate();
  ~NeParticleCandidate() {}
  void Clear();

  Double_t GetEnergyLKr()                          { return fEnergyLKr;              }
  void     SetEnergyLKr(Double_t val)              { fEnergyLKr = val;               }
  Double_t GetTime()                               { return fTime;                   }
  void     SetTime(Double_t val)                   { fTime = val;                    }
  TVector3 GetPosition()               { return fPosition;    }
  void     SetPosition(TVector3 val)   { fPosition = val;     }

private:

  Double_t fEnergyLKr;                  ///< ChParticleCandidate Energy
  Double_t fTime;                       ///< ChParticleCandidate time
  TVector3 fPosition;                    ///< the point at the LKr

};//non dimenticate il ;

#endif