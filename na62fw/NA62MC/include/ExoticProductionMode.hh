//
// --------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (Tommaso.Spadaro@cern.ch) 2017-01-18
//
// --------------------------------------------------------------
#ifndef ExoticProductionMode_h
#define ExoticProductionMode_h 1

#include "G4ParticleTable.hh"
#include "globals.hh"
#include "G4SystemOfUnits.hh"
#include "G4LorentzVector.hh"
#include "ExoticParticle.hh"
#include "RandomGenerator.hh"
#include "TRandom3.h"
#include "TH2D.h"
#include <iostream>
#include "G4ThreeVector.hh"
using namespace std;
#define NMAXDAUGHTERS 2
#define kDMeson 1.040e-12
#define kDSMeson 5.00e-13
#define CSPEED 299792458

enum ExoticParticleID {kAxion=0, kHeavyNeutralLepton=1, kDarkPhoton=2};
enum ProductionTypeID {kDirect=0, kViaMesonDecay=1};

class ExoticProductionMode
{

public:

  ExoticProductionMode(G4String&, G4String&, G4int, std::vector<G4String>&);    ///< Pass the Exotic Particle Name, the mesonName (if any),
                                                                                ///< the number of daughters (if any), the name of the daughters, mode
  ~ExoticProductionMode();

  void SetIOInfo(G4String& filename, G4String& mode, G4String& histogramName) ; ///< Pass production mode properties: fileName, mode (textFile|rootFile, histogramName)
  void SetExoticParticle(G4String&);
  void SetMesonParticle(G4String&);
  void SetMesonDecayType(G4String&, G4int, std::vector<G4String>&);
  void SetMesonDecayType(G4int, std::vector<G4String>&);

  void SetTargetOrTax(G4bool origin)                      { fUseTarget    = origin;            }
  void SetProcessPackedName(G4String& ProcessPackedName)  { fPackedName   = ProcessPackedName; }
  void SetDirectType(G4int kDirect)                       { fModeType     = kDirect;           }
  void SetHNLCouplingE(G4double coupling)                 { fCouplingE    = coupling;          }
  void SetHNLCouplingM(G4double coupling)                 { fCouplingMu   = coupling;          }
  void SetUCouplingG(G4double coupling)                   { fCouplingU    = coupling;          }

  void GetExoticParticleProperties(G4LorentzVector startPosition, G4LorentzVector startMomentum);
  
  G4double GetModeCrossSection()                          { return fModeCrossSection; } ///< Total cross section for proton interaction to Exotic
  G4double GetTotalCrossSection()                         { return fCrossSection;     }
  G4ThreeVector GetExoticPos()                            { return fExoticPos;        }
  G4ThreeVector GetExoticMom()                            { return fExoticMom;        }
  G4double GetExoticEng()                                 { return fExoticEng;        }
  G4ThreeVector GetPartnerPos()                           { return fPartnerPos;       }
  G4ThreeVector GetPartnerMom()                           { return fPartnerMom;       }
  G4double GetPartnerEng()                                { return fPartnerEng;       }
  std::vector<G4String>& GetDaughterNames()               { return fDaughterNames;    } 
  G4double GetMesonFlyTime()                              { return fMesonTime;        }
  G4String GetMesonName()                                 { return fMesonName;        }
  G4double GetExoticFlyTime()                             { return fExoticTime;       }
  G4ThreeVector GetExoticDaughter1Mom()                   { return fMomD1;            }
  G4ThreeVector GetExoticDaughter2Mom()                   { return fMomD2;            }
  G4double GetExoticDaughter1Eng()                        { return fDaughter1Eng;     }
  G4double GetExoticDaughter2Eng()                        { return fDaughter2Eng;     }
  G4ThreeVector GetExoticDecayPos()                       { return fExoticDecayPos;   }
  G4int GetCounts()                                       { return fAcceptCounter;    }

  G4bool IsPartner()                                      { return fPartners;         }
  G4bool IsDirect();                                            ///< A production mode can be of two types: "Direct" or "ViaMesonDecay"
  G4bool IsValid();                                             ///< All the needed input was provided and is valid, and the mode can be generated
  void InitializeMode();                                        ///< Cross-check inputs, retrieve histograms, initialize cumulative integrals, evaluate cross section 
  void GenerateKinematics();                                    ///< Generate an exotic particle from this production mode 
  void ExoticDaughters(G4double, G4double, G4double, G4double); ///< Skips over the exotics to the daughters
  G4bool IsEquivalentTo(ExoticProductionMode*);                 ///< Compare with a given production process using internal uniqueID   
  void DefaultInit(G4String&); 

private:
  G4int GetExoticID()                                     { return fExoticParticleID; }
  void ReadAxionSpectrum(G4double);
  void ReadDPBremSpectrum(G4double);
  void ReadDPMesonSpectrum();
  G4bool InAcceptance(G4double, G4double, G4ThreeVector, G4ThreeVector); 
  void ReadCharmedSpectrum();
  void PropagateMeson(G4double, G4double);                            ///< Pass angles of meson
  G4double ExponentialDistribution(G4double);
  G4double BranchingRatioHNL(G4double, G4double, G4double, G4double); ///< From Shrock paper
  G4double lambda(G4double, G4double, G4double);
  G4double f(G4double, G4double);
  G4double TwoBodyDecayEnergy(G4double, G4double, G4double);
  G4double mesonXSecPhoton(G4double, G4double, G4double); 
  G4double mesonXSecVector(G4double, G4double, G4double, G4double); 
  void EvaluateTotalCrossSection();
  
  const G4double cspeed = 299792458*m/s;
  G4double fModeCrossSection;
  G4ParticleTable* fParticleTable;
  G4double fMesonTime; 
  G4ThreeVector fExoticPos;             ///< Position of beginning of exotic particle 
  G4ThreeVector fExoticMom;             ///< Momentum of exotic particle
  G4double fExoticEng; 
  G4double exoticMass; 
  G4ThreeVector mesonPos;               ///< For indirect
  G4double mesonMass;
  G4ThreeVector mesonMom;               ///< For indirect
  G4ThreeVector fPartnerMom;            ///< Partners from Exotic production
  G4ThreeVector fPartnerPos;            ///< Partners from Exotic Production
  G4double fPartnerEng;
  G4ThreeVector fExoticDecayPos;
  G4double fExoticTime;

  G4ThreeVector fPos1;                  ///< Vertex of decay daughters
  G4ThreeVector fMomD1; 
  G4ThreeVector fMomD2;
  G4double fDaughter2Eng; 
  G4double fDaughter1Eng;

  G4int fAcceptCounter;

  G4int fExoticParticleID;              ///< Internal use: ID of exotic particle
  G4int fModeType;                      ///< Production mode: direct (e.g., bremsstrahlung) or via meson decay
  G4String fMesonName;                  ///< Meson name
  G4int fNDaughters;                    ///< Number of daughters (exotic particle excluded)
  std::vector<G4String> fDaughterNames; ///< Name of daughters

  G4String fIOFileName;                 ///< FileName where the Production mode information is stored
  G4String fIOMode;                     ///< IOMode: "textFile", or "rootFile"
  G4String fHistogramName;              ///< Nmae of histogram generated from textFile or taken from the rootFile

  G4String fPackedName;                 ///< Packed Name of production process ("ExoticParticleName Direct" for direct production, "ExoticParticleName Meson Daughter1 Daughter2.."")

  G4double fCrossSection;               ///< Cross section[nb] of the production mode [cross section x BR for exotic production]
  G4double fSigmaDSPlus;
  G4double fSigmaDPlus;
  G4double fSigmaDMinus;
  G4double fSigmaDSMinus;

  G4double fSigmaEta; 
  G4double fSigmaEtaP; 
  G4double fSigmaPhi; 
  G4double fSigmaRho; 
  G4double fSigmaOmega; 
  G4double fSigmaPi0;

  G4double fBremSigma;

  G4double fBR;

  G4double cTau;
  G4double fCoupling;
  G4double fCouplingE; 
  G4double fCouplingMu;
  G4double fCouplingU;

  G4bool fUseTarget; 
  G4bool fPartners;

  //Histograms and Cross Sections 
  //HNL
  TH2D *fHAxionETheta;
  TH2D *fHDPzPt2;
  TH2D *fHDSPlusPlPt;
  TH2D *fHDPlusPlPt;
  TH2D *fHDMinusPlPt; 
  TH2D *fHDSMinusPlPt;
  //DP
  TH1D *fHTotalEntries;
  TH2D *fHPvTEta;
  TH2D *fHPvTEtaP; 
  TH2D *fHPvTPhi;
  TH2D *fHPvTRho; 
  TH2D *fHPvTOmega; 
  TH2D *fHPvTPi0;
};

#endif
