// --------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 6 Apr 2011
//
// --------------------------------------------------------------
#ifndef GenePart_h
#define GenePart_h 1

#include "TObject.h"
#include "TVector3.h"
#include "TLorentzVector.h"

class GenePart : public TObject {

public:

  GenePart();
  virtual ~GenePart() {Clear();}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

  inline void SetParticleName(TString val)     { fParticleName = val;          }
  inline void SetParticleName(char* val)       { fParticleName = TString(val); }
  inline void SetInitialEnergy(Double_t val)   { fInitialEnergy = val;         }
  inline void SetInitialMomentum(TVector3 val) { fInitialMomentum = val;       }

  void SetPDGcode(Int_t val);
  void SetParticleGroup(Int_t val);

  void SetInitial4Momentum(TLorentzVector val)
  { SetInitialMomentum(val.Vect()); SetInitialEnergy(val.T()); }

  inline Int_t    GetPDGcode()           { return fPDGcode;           }
  inline Int_t    GetCharge()            { return fCharge;            }
  inline TString  GetParticleName()      { return fParticleName;      }
  inline Double_t GetInitialEnergy()     { return fInitialEnergy;     }
  inline TVector3 GetInitialMomentum()   { return fInitialMomentum;   }
  inline Int_t    GetParticleGroup()     { return fParticleGroup;     }
  inline TString  GetParticleGroupName() { return fParticleGroupName; }

  TLorentzVector GetInitial4Momentum()
  { return TLorentzVector(fInitialMomentum,fInitialEnergy); }

protected:

  Int_t    fPDGcode;           ///< PDG code
  Int_t    fCharge;            ///< Electric charge, in units of the elementary charge
  TString  fParticleName;
  Double_t fInitialEnergy;     ///< Energy at the production position, MeV
  TVector3 fInitialMomentum;   ///< Momentum at the production position, MeV
  Int_t    fParticleGroup;     ///< Encoding of the "group" (0-3), see GenePart.cc for explanation
  TString  fParticleGroupName; ///< Self-explanatory name of the group

  ClassDef(GenePart,1)
};

#endif
