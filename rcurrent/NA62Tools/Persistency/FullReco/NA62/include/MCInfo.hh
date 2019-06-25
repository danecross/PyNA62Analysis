// --------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// --------------------------------------------------------------

#ifndef MCINFO_H
#define MCINFO_H 1

#include "TObject.h"
#include "TString.h"

class MCInfo : public TObject {

public:

  MCInfo();
  virtual ~MCInfo() {}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  void UpdateUniqueAttributes(MCInfo &s);
  void MergeJobAttributes(MCInfo &s);
  void UpdateAndMergeAttributes(MCInfo &s);

  TString  GetRevision()                    { return fRevision;           }
  void     SetRevision(TString val)         { fRevision = val;            }
  Int_t    GetBeamType()                    { return fBeamType;           }
  void     SetBeamType(Int_t val)           { fBeamType = val;            }
  Int_t    GetFastSimulationMode()          { return fFastSimulationMode; }
  void     SetFastSimulationMode(Int_t val) { fFastSimulationMode = val;  }
  Double_t GetBrPie2()                      { return fBrPie2;             }
  void     SetBrPie2(Double_t val)          { fBrPie2 = val;              }
  Bool_t   GetForcedDecay()                 { return fForcedDecay;        }
  void     SetForcedDecay(Bool_t val)       { fForcedDecay = val;         }
  Bool_t   GetForcedMuonDecay()             { return fForcedMuonDecay;    }
  void     SetForcedMuonDecay(Bool_t val)   { fForcedMuonDecay = val;     }
  Bool_t   GetForcedPionDecay()             { return fForcedPionDecay;    }
  void     SetForcedPionDecay(Bool_t val)   { fForcedPionDecay = val;     }
  Int_t    GetDecayType()             	    { return fDecayType;          }
  void     SetDecayType(Int_t val)    	    { fDecayType = val;           }
  Int_t    GetRadCor()                      { return fRadCor;             }
  void     SetRadCor(Int_t val)             { fRadCor = val;              }
  Int_t    GetPiZeroDecay()                 { return fPiZeroDecay;        }
  void     SetPiZeroDecay(Int_t val)        { fPiZeroDecay = val;         }
  Double_t GetZDecayMin()                   { return fZDecayMin;          }
  void     SetZDecayMin(Double_t val)       { fZDecayMin = val;           }
  Double_t GetZDecayMax()                   { return fZDecayMax;          }
  void     SetZDecayMax(Double_t val)       { fZDecayMax = val;           }

  Double_t GetExoticParticleMass()                 { return fExoticParticleMass;      }
  void     SetExoticParticleMass(Double_t val)     { fExoticParticleMass = val;       }
  Int_t    GetExoticParticleDecayMode()            { return fExoticParticleDecayMode; }
  void     SetExoticParticleDecayMode(Int_t val)   { fExoticParticleDecayMode = val;  }
  Double_t GetExoticParticleLifetime()             { return fExoticParticleLifetime;  }
  void     SetExoticParticleLifetime(Double_t val) { fExoticParticleLifetime = val;   }

  std::vector<TString> GetFileName()            { return fFileName;           }
  void                 AddFileName(TString val) { fFileName.push_back(val);   }
  std::vector<Int_t>   GetRunNumber()           { return fRunNumber;          }
  void                 AddRunNumber(Int_t val)  { fRunNumber.push_back(val);  }
  std::vector<Int_t>   GetRandomSeed()          { return fRandomSeed;         }
  void                 AddRandomSeed(Int_t val) { fRandomSeed.push_back(val); }
  std::vector<Int_t>   GetNEvents()             { return fNEvents;            }
  void                 AddNEvents(Int_t val)    { fNEvents.push_back(val);    }

private:

  // Unique attributes
  TString  fRevision;        ///< Software revision
  Int_t    fBeamType;        ///< Beam type: turtle, gps, external, ...
  Bool_t   fForcedDecay;     ///< Forced decay requested?
  Bool_t   fForcedMuonDecay; ///< Forced muon decay requested?
  Int_t    fDecayType;       ///< Decay type
  Int_t    fRadCor;          ///< Radiative corrections mode
  Int_t    fPiZeroDecay;     ///< pi0 decay modes (encoded for up to 3 pi0 mesons)
  Double_t fZDecayMin;       ///< Lower limit of the decay region generated
  Double_t fZDecayMax;       ///< Upper limit of the decay region generated
  Double_t fExoticParticleMass;
  Int_t    fExoticParticleDecayMode;
  Double_t fExoticParticleLifetime;

  // Job-dependent attributes
  std::vector<TString> fFileName;
  std::vector<Int_t>   fRunNumber;
  std::vector<Int_t>   fRandomSeed;
  std::vector<Int_t>   fNEvents;

  // These members are last in the list: they have been introduced in v2 of the class
  Int_t    fFastSimulationMode;
  Double_t fBrPie2;          ///< BR(pi+ --> e+ nu)
  Bool_t   fForcedPionDecay; ///< Forced pion decay requested?

  ClassDef(MCInfo,2)
};

#endif
