// ---------------------------------------------------------------
// History:
//
// Created by Jacopo Pinzino (jacopo.pinzino@cern.ch) 2016-11-30
//
// ---------------------------------------------------------------

#ifndef CHPARTICLECANDIDATE_HH
#define CHPARTICLECANDIDATE_HH

#include "TMath.h"
#include "TVector2.h"
#include "TVector3.h"
#include <iostream>
#include <vector>

class ChParticleCandidate
{
  //parte public accessibile a tutti
public:

  ChParticleCandidate();
  ~ChParticleCandidate() {}
  void Clear();

  Int_t    GetTrack()                              { return ftrack;                  }
  void     SetTrack(Int_t val)                     { ftrack = val;                   }
  Int_t    GetCharge()                             { return fCharge;                 }
  void     SetAcceptance(Int_t val)                { fAcceptance = val;              }
  Int_t    GetAcceptance()                         { return fAcceptance;             }
  void     SetCharge(Int_t val)                    { fCharge = val;                  }
  Double_t GetMomentum()                           { return fMomentum;               }
  void     SetMomentum(Double_t val)               { fMomentum = val;                }
  Double_t GetCDA()                                { return fCDA;                    }
  void     SetCDA(Double_t val)                    { fCDA = val;                     }
  Double_t GetEnergyLKr()                          { return fEnergyLKr;              }
  void     SetEnergyLKr(Double_t val)              { fEnergyLKr = val;               }
  Double_t GetEnergyMUV1()                         { return fEnergyMUV1;             }
  void     SetEnergyMUV1(Double_t val)             { fEnergyMUV1 = val;              }
  Double_t GetEnergyMUV2()                         { return fEnergyMUV2;             }
  void     SetEnergyMUV2(Double_t val)             { fEnergyMUV2 = val;              }
  Double_t GetEnergyCAL()                          { return fEnergyCAL;              }
  void     SetEnergyCAL(Double_t val)              { fEnergyCAL = val;               }
  Double_t GetEop()                                { return fEop;                    }
  void     SetEop(Double_t val)                    { fEop = val;                     }
  Double_t GetTime()                               { return fTime;                   }
  void     SetTime(Double_t val)                   { fTime = val;                    }
  Double_t GetMmiss2()                             { return fMmiss2;                 }
  void     SetMmiss2(Double_t val)                 { fMmiss2 = val;                  }
  Int_t    GetMUV3Ass()                            { return MUV3Ass;                 }
  void     SetMUV3Ass(Int_t val)                   { MUV3Ass = val;                  }
  TVector3 GetVertex()                             { return fVertex;                 }
  void     SetVertex(TVector3 val)                 { fVertex = val;                  }
  TVector3 GetMomentumBeforeMagnet()               { return fMomentumBeforeMagnet;   }
  void     SetMomentumBeforeMagnet(TVector3 val)   { fMomentumBeforeMagnet = val;    }
  TVector3 GetMomentumAfterMagnet ()               { return fMomentumAfterMagnet;    }
  void     SetMomentumAfterMagnet (TVector3 val)   { fMomentumAfterMagnet = val;     }
  TVector3 GetPositionBeforeMagnet()               { return fPositionBeforeMagnet;   }
  void     SetPositionBeforeMagnet(TVector3 val)   { fPositionBeforeMagnet = val;    }
  TVector3 GetPositionAfterMagnet ()               { return fPositionAfterMagnet;    }
  void     SetPositionAfterMagnet (TVector3 val)   { fPositionAfterMagnet = val;     }
  TVector3 GetMomentumKaon()                       { return fMomentumKaon;           }
  void     SetMomentumKaon(TVector3 val)           { fMomentumKaon = val;            }
  Double_t GetfElectronProbabilityCombo()                    { return fElectronProbabilityCombo;        }
  void     SetfElectronProbabilityCombo(Double_t val)        { fElectronProbabilityCombo = val;         }
  Double_t GetfMuonProbabilityCombo()                        { return fMuonProbabilityCombo;            }
  void     SetfMuonProbabilityCombo(Double_t val)            { fMuonProbabilityCombo = val;             }
  Double_t GetfPionProbabilityCombo()                        { return fPionProbabilityCombo;            }
  void     SetfPionProbabilityCombo(Double_t val)            { fPionProbabilityCombo = val;             }
  void     SetLKrElectronProbability(Double_t val)        { fLKrElectronProbability = val;         }
  void     SetLKrMuonProbability(Double_t val)            { fLKrMuonProbability = val;             }
  void     SetLKrPionProbability(Double_t val)            { fLKrPionProbability = val;             }
  void     SetRiCHElectronProbability(Double_t val)       { fRiCHElectronProbability = val;        }
  void     SetRiCHMuonProbability(Double_t val)           { fRiCHMuonProbability = val;            }
  void     SetRiCHPionProbability(Double_t val)           { fRiCHPionProbability = val;            }  
  Double_t     GetLKrElectronProbability()        { return fLKrElectronProbability;         }
  Double_t     GetLKrMuonProbability()            { return fLKrMuonProbability;             }
  Double_t     GetLKrPionProbability()            { return fLKrPionProbability;             }
  Double_t     GetRiCHElectronProbability()       { return fRiCHElectronProbability;        }
  Double_t     GetRiCHMuonProbability()           { return fRiCHMuonProbability;            }
  Double_t     GetRiCHPionProbability()           { return fRiCHPionProbability;            }

private:

  Int_t    ftrack;                      ///< n track in downstream tracks
  Int_t    fCharge;                     ///< Electric charge (+1 or -1)
  Int_t    fAcceptance;                 ///< inside Geometrical Acceptance (1 or 0)
  Double_t fMomentum;                   ///< Track momentum measured by the spectrometer (absolute value)
  Double_t fCDA;                        ///< CDA with respect to nominal beam axis
  Double_t fEnergyLKr;                  ///< ChParticleCandidate Energy
  Double_t fEnergyMUV1;                 ///< ChParticleCandidate Energy
  Double_t fEnergyMUV2;                 ///< ChParticleCandidate Energy
  Double_t fEnergyCAL;                  ///< ChParticleCandidate Energy
  Double_t fEop;                        ///< Particle eop
  Double_t fTime;                       ///< ChParticleCandidate time
  Double_t fMmiss2;                     ///< missing mass ChParticleCandidate
  Int_t    MUV3Ass;                     ///< association hit mav3
  TVector3 fVertex;                     ///< Track-beam axis vertex
  TVector3 fMomentumBeforeMagnet;       ///< Three-momentum before magnet
  TVector3 fMomentumAfterMagnet;        ///< Three-momentum after magnet
  TVector3 fPositionBeforeMagnet;       ///< A point along the trajectory before magnet
  TVector3 fPositionAfterMagnet;        ///< A point along the trajectory after magnet
  TVector3 fMomentumKaon;               ///< Three-momentum Kaon ChParticleCandidate
  Double_t fLKrElectronProbability;     ///< probability of ChParticleCandidate selection from LKR
  Double_t fLKrMuonProbability;         ///< probability of ChParticleCandidate selection from LKR
  Double_t fLKrPionProbability;         ///< probability of ChParticleCandidate selection from LKR
  Double_t fRiCHElectronProbability;    ///< probability of ChParticleCandidate selection from RiCH
  Double_t fRiCHMuonProbability;        ///< probability of ChParticleCandidate selection from RiCH
  Double_t fRiCHPionProbability;        ///< probability of ChParticleCandidate selection from RiCH
  Double_t fElectronProbabilityCombo;   ///< combined probability (LKR and RICH with momentum efficiency) of ChParticleCandidate selection
  Double_t fMuonProbabilityCombo;       ///< combined probability (LKR and RICH with momentum efficiency) of ChParticleCandidate selection
  Double_t fPionProbabilityCombo;       ///< combined probability (LKR and RICH with momentum efficiency) of ChParticleCandidate selection

};//non dimenticate il ;

#endif