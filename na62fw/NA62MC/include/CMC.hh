#ifndef CMC_h
#define CMC_h 1

#include <vector>
#include "math.h"
#include "TVector3.h"
#include "TLorentzVector.h"

//////////////////////////////////////////////////////////
// Contents of the common memory blocks:
// Number of particles (for tracking)
// number of generated particles (for GeneParts);
// PDG IDs of the partiles and generated particles;
// KEEP flags of the particles;
// particle group flags (FLAG) of the generated particles;
// 4-momenta of particles and generated particles;
// polarizations of the particles.

struct CMCint_t {
  int NPART,NPARTGEN,PID[50],PIDGEN[50],KEEP[50],FLAG[50];
};

struct CMCdouble_t {
  double P4INI[50][4],P4INIGEN[50][4],POL3INI[50][3],EXOMASS;
};


struct CMCParticle {
  CMCParticle(int pid, double energy, double px, double py, double pz):
    fPid(pid),fEnergy(energy),fPx(px),fPy(py),fPz(pz),fPolX(0.0),fPolY(0.0),fPolZ(0.0) {}
  CMCParticle(int pid, double energy, double px, double py, double pz, double polx, double poly, double polz):
    fPid(pid),fEnergy(energy),fPx(px),fPy(py),fPz(pz),fPolX(polx),fPolY(poly),fPolZ(polz) {}
  int    fPid;
  double fEnergy;
  double fPx;
  double fPy;
  double fPz;
  double fPolX;
  double fPolY;
  double fPolZ;
};

typedef std::vector<CMCParticle*> ParticleVector;

class CMC {

public:

  CMC();
  virtual ~CMC();
  static CMC *GetInstance();

  void InitDecay (int, double, double, double, double);

  void Kch2pinunu();
  void Kch2pipi0();
  void Kch2pipi0g_ib();
  void Kch2pipi0dg();
  void Kch2piX0();
  void Kch2pipipi();
  void Kch2pipi0pi0();

  void Kch2enu();
  void Kch2enug_ib();
  void Kch2enug_ib_cutoff();
  void Kch2enug_sdp();
  void Kch2enug_sdm();
  void Kch2enug_intp();
  void Kch2enug_intm();
  void Kch2enununu();

  void Kch2munu();
  void Kch2munug_ib();
  void Kch2munug_ib_cutoff();
  void Kch2munug_sdp();
  void Kch2munug_sdm();
  void Kch2munug_intp();
  void Kch2munug_intm();
  void Kch2munununu();
  void Kch2munuA();
  void Kch2munuXscal();
  void Kch2munuXvec();

  void Kch2pienu();
  void Kch2pimunu();
  void Kch2pienug_kloe();
  void Kch2pienug_ib_de();
  void Kch2pimunug_kloe();

  void Kch2pipienu();
  void Kch2pi0pi0enu();

  void Kch2pipienu_ferrara();
  void Kch2pipimunu_ferrara();
  void Kch2pi0pi0enu_ferrara();
  void Kch2pi0pi0munu_ferrara();

  void Kch2pipi0g_ib_cutoff();
  void Kch2pipi0g_de();
  void Kch2pipi0g_int();

  void Kch2pigg();
  void Kch2pigee();
  void Kch2pigmumu();
  void Kch2pipi0ee_ib();
  void Kch2pipipig();

  void Kch2enuee();
  void Kch2munuee();
  void Kch2enumumu();
  void Kch2munumumu();

  void Kch2piee();
  void Kch2pimumu();
  void Kch2pip_mup_em();
  void Kch2pip_mum_ep();
  void Kch2pim_mup_ep();
  void Kch2pim_ep_ep();
  void Kch2pim_mup_mup();

  void Kch2enumumu_LFV();
  void Kch2munuee_LFV();

  void Pich2enu();
  void Pich2enug_ib();
  void Pich2enug_ib_cutoff();
  void Pich2enug_sdp();
  void Pich2enug_sdm();
  void Pich2enug_intp();
  void Pich2enug_intm();
  void Pich2enununu();

  void Pich2munu();
  void Pich2munug_ib();
  void Pich2munug_ib_cutoff();
  void Pich2munug_sdp();
  void Pich2munug_sdm();
  void Pich2munug_intp();
  void Pich2munug_intm();
  void Pich2munununu();

  void Kl2pipi();
  void Kl2pi0pi0();
  void Kl2pipipi0();
  void Kl2pi0pi0pi0();
  void Kl2ee();
  void Kl2mumu();
  void Kl2emu();
  void Kl2mue();
  void Kl2pi0ee();
  void Kl2pi0mumu();
  void Kl2pi0emu();
  void Kl2pi0mue();

  ////////////////////////////////////

  int      ImportParticles(ParticleVector* particles);
  int      GetNParticles()                 { return fCMCint->NPART;             }
  int      GetNGeneratedParticles()        { return fCMCint->NPARTGEN;          }
  int      GetPDGCode(int i)               { return fCMCint->PID[i];            }
  int      GetGeneratedPDGCode(int i)      { return fCMCint->PIDGEN[i];         }
  int      GetKeepFlag(int i)              { return fCMCint->KEEP[i];           }
  int      GetFlag(int i)                  { return fCMCint->FLAG[i];           }
  int      GetVerbose()                    { return fVerbose;                   }
  int      GetNTrials()                    { return fNTrials;                   }

  double         GetEnergy(int i)          { return fCMCdouble->P4INI[i][3];    }
  double         GetGeneratedEnergy(int i) { return fCMCdouble->P4INIGEN[i][3]; }
  TLorentzVector Get4Momentum(int i);
  TLorentzVector GetGenerated4Momentum(int);
  TVector3       Get3Momentum(int i);
  TVector3       GetGenerated3Momentum(int);
  TString        GetGeneratedName(int);

  void SetPDGCode           (int i, int val) { fCMCint->PID[i]    = val; }
  void SetGeneratedPDGCode  (int i, int val) { fCMCint->PIDGEN[i] = val; }
  void SetKeepFlag          (int i, int val) { fCMCint->KEEP[i]   = val; }
  void SetFlag              (int i, int val) { fCMCint->FLAG[i]   = val; }
  void SetNTrials           (int val)        { fNTrials           = val; }
  void SetGenerated4Momentum(int, TLorentzVector);
  void Set4Momentum         (int, TLorentzVector);
  void SetPolarization      (int, TVector3);

  void IncrementNParticles()          { fCMCint->NPART++;    }
  void IncrementNGeneratedParticles() { fCMCint->NPARTGEN++; }

private:

  static CMC*     fgInstance;
  static int      fParentType, fDecayType, fDecayMode, fRadcor, fPizeroDecay;
  static double   fTwoPhotonsMaxAngle; ///< Maximum angle between photon pairs for biased MC
  static double   fRadiativePhotonMinEnergy, fRadiativePhotonMaxEnergy;
  static double   fLeptonPhotonMinAngle; ///< Minimum lepton-photon angle (for biased MC)
  static double   fMinTracksMomentum; ///< Minimum sum of momenta of all charged tracks (for biased MC) [MeV]
  static int      fNTrials; ///< Number of trials for biased MC
  static int      fCounter;
  static bool     fPhotosInitialised;
  static double   fExoticParticleMass[200];
  static double   fExoticParticleLifetime[200];

  int             fVerbose;
  int             fBeamIndex; ///< Index of the beam particle (currently set to 1)
  int             fNumberOfGeneratedParticles; ///< Number of exotic particles generated
  ParticleVector* fParticles;
  CMCint_t*       fCMCint;
  CMCdouble_t*    fCMCdouble;

  void InitPhotos();
};

#endif
