#include "CMC.hh"
#include "DatacardManager.hh"
#include "ExoticParticle.hh"
#include "G4SystemOfUnits.hh"
#include "masses.hh"
#include "../include/RandomGenerator.hh"
#include "PhysicsList.hh"
#include "NA62Global.hh"

// C++ generators

void kch2pinunu(TLorentzVector);
void kch2lnununu(TLorentzVector, int, int);
void kch2munuA(TLorentzVector, double, double);
void kl2pi0pi0pi0(TLorentzVector, int);
void kl2pipipi0(TLorentzVector, int);
void kl2pi0ll(TLorentzVector, int, int);
void two_body_decay(TLorentzVector, int, int, int, double, double, int);
void kch2pipipig(TLorentzVector);

// Fortran generators

#define kch2pipi0g_ib        kch2pipi0g_ib_
#define kch2pipi0dg          kch2pipi0dg_
#define kch2pipipi           kch2pipipi_
#define kch2pipi0pi0         kch2pipi0pi0_

#define kch2lnug_ib          kch2lnug_ib_
#define kch2lnug             kch2lnug_

#define kch2pi0enu           kch2pi0enu_
#define kch2pi0munu          kch2pi0munu_
#define kch2pi0enug_kloe     kch2pi0enug_kloe_
#define kch2pi0enug_ib_de    kch2pi0enug_ib_de_
#define kch2pi0munug_kloe    kch2pi0munug_kloe_

#define kch2pipienu          kch2pipienu_
#define kch2pi0pi0enu        kch2pi0pi0enu_
#define kch2pipilnu_ferrara  kch2pipilnu_ferrara_

#define kch2pipi0g           kch2pipi0g_
#define kch2pigg             kch2pigg_
#define kch2pigll            kch2pigll_
#define kch2pipi0ee_ib       kch2pipi0ee_ib_

#define kch2lnull            kch2lnull_
#define kch2pill             kch2pill_
#define kch3body             kch3body_
#define kch4body             kch4body_

#define phoini phoini_
#define na48phoin na48phoin_
#define type_of_call

extern "C" {
  // Kaon decay routines
  void type_of_call kch2pipi0g_ib(int&,int&,double&,double&,double&,double&,int&);
  void type_of_call kch2pipi0dg  (int&,int&);
  void type_of_call kch2pipipi   (int&,int&);
  void type_of_call kch2pipi0pi0 (int&,int&,int&,double&,int&);

  void type_of_call kch2lnug_ib(int&,int&,int&);
  void type_of_call kch2lnug   (int&,int&,int&,int&);

  void type_of_call kch2pi0enu       (int&,int&,int&);
  void type_of_call kch2pi0munu      (int&,int&,int&);
  void type_of_call kch2pi0enug_kloe (int&,int&,double&,double&,double&,int&);
  void type_of_call kch2pi0enug_ib_de(int&,int&,double&,double&);
  void type_of_call kch2pi0munug_kloe(int&,int&,double&,int&);

  void type_of_call kch2pipienu        (int&,int&);
  void type_of_call kch2pi0pi0enu      (int&,int&,int&);
  void type_of_call kch2pipilnu_ferrara(int&,int&,int&);

  void type_of_call kch2pipi0g    (int&,int&,int&,double&,int&);
  void type_of_call kch2pigg      (int&);
  void type_of_call kch2pigll     (int&,int&);
  void type_of_call kch2pipi0ee_ib(int&,int&);

  void type_of_call kch2lnull(int&,int&);
  void type_of_call kch2pill (int&,int&,int&);
  void type_of_call kch3body (int&,int&);
  void type_of_call kch4body (int&,int&);

  // Photos radiative corrections
  void type_of_call phoini();
  void type_of_call na48phoin();

  // Common
  void* cmc_common_address(const char*);
}

CMC*   CMC::fgInstance                   =  0;
int    CMC::fParentType                  = -1;
int    CMC::fDecayType                   = -1;
int    CMC::fDecayMode                   = -1;
int    CMC::fRadcor                      = -1;
double CMC::fTwoPhotonsMaxAngle          = -1;
int    CMC::fNTrials                     = -1;
double CMC::fRadiativePhotonMinEnergy    = -1;
double CMC::fRadiativePhotonMaxEnergy    = -1;
double CMC::fLeptonPhotonMinAngle        = -1;
double CMC::fMinTracksMomentum           = -1;
int    CMC::fPizeroDecay                 = -1;
int    CMC::fCounter                     = -1;
bool   CMC::fPhotosInitialised           = kFALSE;
double CMC::fExoticParticleMass[200]     = {0.};
double CMC::fExoticParticleLifetime[200] = {-999}; // stable

///////////////////////
// Static access method

CMC* CMC::GetInstance() {
  if (!fgInstance) fgInstance = new CMC();
  return fgInstance;
}

////////////////////////////////////////////////////////////////////////////////////////
// CMC constructor: creates a vector of CMCParticle in which it will store all particles

CMC::CMC() : fParticles(0) {
  if (fgInstance) { // Protect against multiple objects
    G4cerr << "[CMC] There is an instance of CMC already" << G4endl;
    exit(kGenericError);
  }

  // Initialize common blocks
  fCMCint    = static_cast<CMCint_t*>   (cmc_common_address("cmc_int"));
  fCMCdouble = static_cast<CMCdouble_t*>(cmc_common_address("cmc_double"));

  fParticles   = new ParticleVector();
  fBeamIndex   = 1; // Index of the beam particle in fCMCdouble (Fortran numbering): fixed
  fRadcor      = DatacardManager::GetInstance()->GetDecayRadcor();
  fPizeroDecay = DatacardManager::GetInstance()->GetDecayPizeroDecay();
  fVerbose     = DatacardManager::GetInstance()->GetDecayVerbose();
  fTwoPhotonsMaxAngle       = DatacardManager::GetInstance()->GetTwoPhotonsMaxAngle();
  fRadiativePhotonMinEnergy = DatacardManager::GetInstance()->GetRadiativePhotonMinEnergy();
  fRadiativePhotonMaxEnergy = DatacardManager::GetInstance()->GetRadiativePhotonMaxEnergy();
  fLeptonPhotonMinAngle     = DatacardManager::GetInstance()->GetLeptonPhotonMinAngle();
  fMinTracksMomentum        = DatacardManager::GetInstance()->GetMinTracksMomentum();
  fNumberOfGeneratedParticles = PhysicsList::GetInstance()->GetExoticParticleNumberOfGeneratedParticles();
  if (fNumberOfGeneratedParticles<1 || fNumberOfGeneratedParticles>=200) {
    G4cout <<"[CMC] Error: exotic particle is configured incorrectly" << G4endl;
    exit(kWrongConfiguration);
  }
  for (G4int i=0; i<200; i++) {
    fExoticParticleMass[i]     = ExoticParticle::Definition(i)->GetPDGMass() / GeV; // decay generators use [GeV]
    fExoticParticleLifetime[i] = ExoticParticle::Definition(i)->GetPDGLifeTime() / nanosecond;
    // Stable exotic particle (decay mode 0) should not be passed to Geant4 for tracking 
    if (ExoticParticle::Definition(i)->GetPDGStable()) fExoticParticleLifetime[i] = -999;
  }
  fCMCdouble->EXOMASS = fExoticParticleMass[0];

  // [MeV] --> [GeV] conversions of the macro settings
  fRadiativePhotonMinEnergy *= 0.001;
  fRadiativePhotonMaxEnergy *= 0.001;
  fMinTracksMomentum        *= 0.001;

  if (fRadiativePhotonMinEnergy<0.0 || fRadiativePhotonMinEnergy>=MKCH) fRadiativePhotonMinEnergy = 0.0;
  if (fRadiativePhotonMaxEnergy<=0.0 || fRadiativePhotonMaxEnergy>MKCH) fRadiativePhotonMaxEnergy = MKCH;
  if (fLeptonPhotonMinAngle<0.0 || fLeptonPhotonMinAngle>TMath::Pi()) fLeptonPhotonMinAngle = 0.0;

  if (fRadcor>0) InitPhotos();
}

/////////////////////////////////////////////////////////////////////
// Destroy the object, delete and all CMC Particles currently on list

CMC::~CMC() {
  if (fParticles) {
    ParticleVector::const_iterator it;
    for (it=fParticles->begin(); it!=fParticles->end(); ++it) delete *it;
    delete fParticles;
  }
}

//////////////////////////
// Initialization routines

void CMC::InitPhotos() {
  if (!fPhotosInitialised) {
    phoini();
    na48phoin();
    G4cout << "Photos initialised" << G4endl;
    fPhotosInitialised = kTRUE;
  }
}

void CMC::InitDecay(int idbeam, double px, double py, double pz, double en) {
  fCMCint->NPART    = 0;
  fCMCint->NPARTGEN = 0;
  fCMCint->PID[0]   = idbeam;
  fCMCdouble->P4INI[0][0] = px;
  fCMCdouble->P4INI[0][1] = py;
  fCMCdouble->P4INI[0][2] = pz;
  fCMCdouble->P4INI[0][3] = en;
  fCounter++;
}

/////////////////////////////////////////////////
// S T A R T   D E C A Y   G E N E R A T O R S //
/////////////////////////////////////////////////

///////////////////
// K+ --> pi+ nu nu

void CMC::Kch2pinunu() {
  kch2pinunu(Get4Momentum(fBeamIndex-1));
}

/////////////////
// K+ --> pi+ pi0

void CMC::Kch2pipi0() {
  two_body_decay(Get4Momentum(fBeamIndex-1), PDG_ID_kp, PDG_ID_pip, PDG_ID_pi0, 0.0, 0.0, fPizeroDecay);
}

/////////////////////////////////////////////
// K + --> pi+ pi0 gamma (IB) fully inclusive

void CMC::Kch2pipi0g_ib() {
  kch2pipi0g_ib(fBeamIndex, fPizeroDecay,
		fRadiativePhotonMinEnergy, fRadiativePhotonMaxEnergy,
		fTwoPhotonsMaxAngle, fMinTracksMomentum, fNTrials);
}

/////////////////////////////////////////////
// K + --> pi+ pi0 (dark-gamma)

void CMC::Kch2pipi0dg() {
  kch2pipi0dg(fBeamIndex, fPizeroDecay);
}

//////////////////////////////////////////////
// K+ --> pi+ X0
// Exotic particle mass may depend on event ID
void CMC::Kch2piX0() {
  G4int iParticle = fCounter % fNumberOfGeneratedParticles;
  two_body_decay(Get4Momentum(fBeamIndex-1),
		 PDG_ID_kp, PDG_ID_pip, PDG_ID_exo+iParticle,
		 fExoticParticleMass[iParticle], fExoticParticleLifetime[iParticle], 0);
}

/////////////////////
// K+ --> pi+ pi+ pi-

void CMC::Kch2pipipi() {
  kch2pipipi(fBeamIndex, fRadcor);
}

/////////////////////
// K+ --> pi+ pi0 pi0

void CMC::Kch2pipi0pi0() {
  kch2pipi0pi0(fBeamIndex, fRadcor, fPizeroDecay, fTwoPhotonsMaxAngle, fNTrials);
}

//////////////////////////
// K+ --> e+ nu (two body)

void CMC::Kch2enu() {
  G4int iParticle = fCounter % fNumberOfGeneratedParticles;
  two_body_decay(Get4Momentum(fBeamIndex-1),
		 PDG_ID_kp, PDG_ID_elp, PDG_ID_exo+iParticle,
		 fExoticParticleMass[iParticle], fExoticParticleLifetime[iParticle], 0);
}

//////////////////////////////////////
// K+ --> e+ nu g (IB) fully inclusive

void CMC::Kch2enug_ib() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 1;
  kch2lnug_ib(fBeamIndex, fParentType, fDecayType);
}

///////////////////////////////////////////
// K+ --> e+ nu g (IB) with a Egamma cutoff

void CMC::Kch2enug_ib_cutoff() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 1;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

///////////////////////
// K+ --> e+ nu g (SD+)

void CMC::Kch2enug_sdp() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 2;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

///////////////////////
// K+ --> e+ nu g (SD-)

void CMC::Kch2enug_sdm() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 3;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

////////////////////////
// K+ --> e+ nu g (INT+)

void CMC::Kch2enug_intp() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 4;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

////////////////////////
// K+ --> e+ nu g (INT-)

void CMC::Kch2enug_intm() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 5;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

//////////////////////
// K+ --> e+ nu nu nu

void CMC::Kch2enununu() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 1;
  kch2lnununu(Get4Momentum(fBeamIndex-1), fParentType, fDecayType);
}

///////////////////////////
// K+ --> mu+ nu (two body)

void CMC::Kch2munu() {
  G4int iParticle = fCounter % fNumberOfGeneratedParticles;
  two_body_decay(Get4Momentum(fBeamIndex-1),
		 PDG_ID_kp, PDG_ID_mup, PDG_ID_exo+iParticle,
		 fExoticParticleMass[iParticle], fExoticParticleLifetime[iParticle], 0);
}

///////////////////////////////////////
// K+ --> mu+ nu g (IB) fully inclusive

void CMC::Kch2munug_ib () {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 2;
  kch2lnug_ib(fBeamIndex, fParentType, fDecayType);
}

////////////////////////////////////////////
// K+ --> mu+ nu g (IB) with a Egamma cutoff

void CMC::Kch2munug_ib_cutoff() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 1;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

////////////////////////
// K+ --> mu+ nu g (SD+)

void CMC::Kch2munug_sdp() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 2;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

////////////////////////
// K+ --> mu+ nu g (SD-)

void CMC::Kch2munug_sdm() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 3;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

/////////////////////////
// K+ --> mu+ nu g (INT+)

void CMC::Kch2munug_intp() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 4;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

/////////////////////////
// K+ --> mu+ nu g (INT-)

void CMC::Kch2munug_intm() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 5;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

//////////////////////
// K+ --> mu+ nu nu nu

void CMC::Kch2munununu() {
  if (fParentType==-1) fParentType = 1;
  if (fDecayType==-1)  fDecayType  = 2;
  kch2lnununu(Get4Momentum(fBeamIndex-1), fParentType, fDecayType);
}

///////////////////
// K+ --> mu+ nu A'

void CMC::Kch2munuA() {
  kch2munuA(Get4Momentum(fBeamIndex-1), fExoticParticleMass[0], fExoticParticleLifetime[0]);
}

/////////////////////////
// K+ --> pi0 e+ nu (Ke3)

void CMC::Kch2pienu() {
  kch2pi0enu(fBeamIndex, fRadcor, fPizeroDecay);
}

///////////////////////////
// K+ --> pi0 mu+ nu (Kmu3)

void CMC::Kch2pimunu() {
  kch2pi0munu(fBeamIndex, fRadcor, fPizeroDecay);
}

////////////////////////////////////////////
// K+ --> pi0 e+ nu (Ke3) imported from KLOE

void CMC::Kch2pienug_kloe() {
  kch2pi0enug_kloe(fBeamIndex, fPizeroDecay, fRadiativePhotonMinEnergy,
		   fLeptonPhotonMinAngle, fMinTracksMomentum, fNTrials);
}

////////////////////////////////////////////
// K+ --> pi0 e+ nu g (Ke3g) imported from Kubis

void CMC::Kch2pienug_ib_de() {
  kch2pi0enug_ib_de(fBeamIndex, fPizeroDecay,
		    fRadiativePhotonMinEnergy,fLeptonPhotonMinAngle);
}

//////////////////////////////////////////////
// K+ --> pi0 mu+ nu (Kmu3) imported from KLOE

void CMC::Kch2pimunug_kloe() {
  kch2pi0munug_kloe(fBeamIndex, fPizeroDecay, fMinTracksMomentum, fNTrials);
}

////////////////////////////////
// K+ --> e+ pi+ pi- nu (Ke4 +-)

void CMC::Kch2pipienu() {
  kch2pipienu(fBeamIndex, fRadcor);
}

////////////////////////////////
// K+ --> pi0 pi0 e+ nu (Ke4 00)

void CMC::Kch2pi0pi0enu() {
  kch2pi0pi0enu(fBeamIndex, fRadcor, fPizeroDecay);
}

//////////////////////////////////////////////////
// K+ --> pi+ pi- e+ nu (Ke4 +-) Ferrara generator

void CMC::Kch2pipienu_ferrara() {
  if (fDecayType==-1) fDecayType = 4;
  kch2pipilnu_ferrara(fBeamIndex, fDecayType, fPizeroDecay);
}

////////////////////////////////////////////////////
// K+ --> pi+ pi- mu+ nu (Kmu4 +-) Ferrara generator

void CMC::Kch2pipimunu_ferrara() {
  if (fDecayType==-1) fDecayType = 1;
  kch2pipilnu_ferrara(fBeamIndex, fDecayType, fPizeroDecay);
}

//////////////////////////////////////////////////
// K+ --> pi0 pi0 e+ nu (Ke4 00) Ferrara generator

void CMC::Kch2pi0pi0enu_ferrara() {
  if (fDecayType==-1) fDecayType = 3;
  kch2pipilnu_ferrara(fBeamIndex, fDecayType, fPizeroDecay);
}

////////////////////////////////////////////////////
// K+ --> pi0 pi0 mu+ nu (Kmu4 00) Ferrara generator

void CMC::Kch2pi0pi0munu_ferrara() {
  if (fDecayType==-1) fDecayType = 2;
  kch2pipilnu_ferrara(fBeamIndex, fDecayType, fPizeroDecay);
}

////////////////////////////////////////////////////////////
// K+ --> pi+ pi0 gamma (IB) with Egamma IR cutoff at 10 MeV

void CMC::Kch2pipi0g_ib_cutoff() {
  if (fDecayType==-1) fDecayType = 1;
  kch2pipi0g(fBeamIndex, fDecayType, fPizeroDecay, fTwoPhotonsMaxAngle, fNTrials);
}

////////////////////////////
// K+ --> pi+ pi0 gamma (DE)

void CMC::Kch2pipi0g_de() {
  if (fDecayType==-1) fDecayType = 2;
  kch2pipi0g(fBeamIndex, fDecayType, fPizeroDecay, fTwoPhotonsMaxAngle, fNTrials);
}

/////////////////////////////
// K+ --> pi+ pi0 gamma (INT)

void CMC::Kch2pipi0g_int() {
  if (fDecayType==-1) fDecayType = 3;
  kch2pipi0g(fBeamIndex, fDecayType, fPizeroDecay, fTwoPhotonsMaxAngle, fNTrials);
}

/////////////////////////
// K+ --> pi+ gamma gamma

void CMC::Kch2pigg() {
  kch2pigg(fBeamIndex);
}

/////////////////////////
// K+ --> pi+ gamma e+ e-

void CMC::Kch2pigee() {
  if (fDecayType==-1) fDecayType = 1;
  kch2pigll(fBeamIndex, fDecayType);
}

///////////////////////////
// K+ --> pi+ gamma mu+ mu-

void CMC::Kch2pigmumu() {
  if (fDecayType==-1) fDecayType = 2;
  kch2pigll(fBeamIndex, fDecayType);
}

////////////////////////////
// K+ --> pi+ pi0 e+ e- (IB)

void CMC::Kch2pipi0ee_ib() {
  kch2pipi0ee_ib(fBeamIndex, fPizeroDecay);
}

////////////////////////////
// K+ --> pi+ pi+ pi- gamma with Egamma > 1 MeV in kaon rest frame

void CMC::Kch2pipipig() {
  kch2pipipig(Get4Momentum(fBeamIndex-1));
}

/////////////////////
// K+ --> e+ nu e+ e-

void CMC::Kch2enuee() {
  if (fDecayType==-1) fDecayType = 1;
  kch2lnull(fBeamIndex, fDecayType);
}

//////////////////////
// K+ --> mu+ nu e+ e-

void CMC::Kch2munuee() {
  if (fDecayType==-1) fDecayType = 2;
  kch2lnull(fBeamIndex, fDecayType);
}

///////////////////////
// K+ --> e+ nu mu+ mu-

void CMC::Kch2enumumu() {
  if (fDecayType==-1) fDecayType = 3;
  kch2lnull(fBeamIndex, fDecayType);
}

///////////////////////
// K+ --> e+ nu mu+ mu-

void CMC::Kch2munumumu() {
  if (fDecayType==-1) fDecayType = 4;
  kch2lnull(fBeamIndex, fDecayType);
}

///////////////////
// K+ --> pi+ e+ e-

void CMC::Kch2piee() {
  if (fDecayType==-1) fDecayType = 1;
  kch2pill(fBeamIndex, fDecayType, fRadcor);
}

/////////////////////
// K+ --> pi+ mu+ mu-

void CMC::Kch2pimumu() {
  if (fDecayType==-1) fDecayType = 2;
  kch2pill(fBeamIndex, fDecayType, fRadcor);
}

////////////////////////////////////////
// K+ --> pi+ mu+ e- (uniform diff.rate)

void CMC::Kch2pip_mup_em() {
  if (fDecayType==-1) fDecayType = 1;
  kch3body(fBeamIndex, fDecayType);
}

/////////////////////////////////////////
// K+ --> pi+ mu- e+  (uniform diff.rate)

void CMC::Kch2pip_mum_ep() {
  if (fDecayType==-1) fDecayType = 2;
  kch3body(fBeamIndex, fDecayType);
}

////////////////////////////////////////
// K+ --> pi- mu+ e+ (uniform diff.rate)

void CMC::Kch2pim_mup_ep() {
  if (fDecayType==-1) fDecayType = 3;
  kch3body(fBeamIndex, fDecayType);
}

////////////////////////////////////////
// K+ --> pi- e+ e+  (uniform diff.rate)

void CMC::Kch2pim_ep_ep() {
  if (fDecayType==-1) fDecayType = 4;
  kch3body(fBeamIndex, fDecayType);
}

//////////////////////////////////////////
// K+ --> pi- mu+ mu+  (uniform diff.rate)

void CMC::Kch2pim_mup_mup() {
  if (fDecayType==-1) fDecayType = 5;
  kch3body(fBeamIndex, fDecayType);
}

////////////////////////////////////////////
// K+ --> e- nu mu+ mu+  (uniform diff.rate)

void CMC::Kch2enumumu_LFV() {
  if (fDecayType==-1) fDecayType = 1;
  kch4body(fBeamIndex, fDecayType);
}

///////////////////////////////////////////
// K+ --> mu- nu e+ e+  (uniform diff.rate)

void CMC::Kch2munuee_LFV() {
  if (fDecayType==-1) fDecayType = 2;
  kch4body(fBeamIndex, fDecayType);
}

///////////////////////////
// pi+ --> e+ nu (two body)

void CMC::Pich2enu() {
  G4int iParticle = fCounter % fNumberOfGeneratedParticles;
  two_body_decay(Get4Momentum(fBeamIndex-1),
                 PDG_ID_pip, PDG_ID_elp, PDG_ID_exo+iParticle,
                 fExoticParticleMass[iParticle], fExoticParticleLifetime[iParticle], 0);
}

///////////////////////////////////////
// pi+ --> e+ nu g (IB) fully inclusive

void CMC::Pich2enug_ib() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 1;
  kch2lnug_ib(fBeamIndex, fParentType, fDecayType);
}

////////////////////////////////////////////
// pi+ --> e+ nu g (IB) with a Egamma cutoff

void CMC::Pich2enug_ib_cutoff() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 1;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

////////////////////////
// pi+ --> e+ nu g (SD+)

void CMC::Pich2enug_sdp() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 2;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

////////////////////////
// pi+ --> e+ nu g (SD-)

void CMC::Pich2enug_sdm() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 3;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

/////////////////////////
// pi+ --> e+ nu g (INT+)

void CMC::Pich2enug_intp() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 4;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

/////////////////////////
// pi+ --> e+ nu g (INT-)

void CMC::Pich2enug_intm() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 1;
  if (fDecayMode==-1)  fDecayMode  = 5;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

///////////////////////
// pi+ --> mu+ nu nu nu

void CMC::Pich2enununu() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 1;
  kch2lnununu(Get4Momentum(fBeamIndex-1), fParentType, fDecayType);
}

////////////////////////////
// pi+ --> mu+ nu (two body)

void CMC::Pich2munu() {
  G4int iParticle = fCounter % fNumberOfGeneratedParticles;
  two_body_decay(Get4Momentum(fBeamIndex-1),
                 PDG_ID_pip, PDG_ID_mup, PDG_ID_exo+iParticle,
                 fExoticParticleMass[iParticle], fExoticParticleLifetime[iParticle], 0);
}

////////////////////////////////////////
// pi+ --> mu+ nu g (IB) fully inclusive

void CMC::Pich2munug_ib () {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 2;
  kch2lnug_ib(fBeamIndex, fParentType, fDecayType);
}

/////////////////////////////////////////////
// pi+ --> mu+ nu g (IB) with a Egamma cutoff

void CMC::Pich2munug_ib_cutoff() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 1;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

/////////////////////////
// pi+ --> mu+ nu g (SD+)

void CMC::Pich2munug_sdp() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 2;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

/////////////////////////
// pi+ --> mu+ nu g (SD-)

void CMC::Pich2munug_sdm() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 3;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

//////////////////////////
// pi+ --> mu+ nu g (INT+)

void CMC::Pich2munug_intp() {
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 4;
  if (fParentType==-1) fParentType = 2;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

//////////////////////////
// pi+ --> mu+ nu g (INT-)

void CMC::Pich2munug_intm() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 2;
  if (fDecayMode==-1)  fDecayMode  = 5;
  kch2lnug(fBeamIndex, fParentType, fDecayType, fDecayMode);
}

///////////////////////
// pi+ --> mu+ nu nu nu

void CMC::Pich2munununu() {
  if (fParentType==-1) fParentType = 2;
  if (fDecayType==-1)  fDecayType  = 2;
  kch2lnununu(Get4Momentum(fBeamIndex-1), fParentType, fDecayType);
}

/////////////////
// KL --> pi+ pi-

void CMC::Kl2pipi() {
  two_body_decay(Get4Momentum(fBeamIndex-1), PDG_ID_kl, PDG_ID_pip, PDG_ID_pim, 0.0, 0.0, 0);
}

/////////////////
// KL --> pi0 pi0

void CMC::Kl2pi0pi0() {
  two_body_decay(Get4Momentum(fBeamIndex-1), PDG_ID_kl, PDG_ID_pi0, PDG_ID_pi0, 0.0, 0.0, fPizeroDecay);
}

/////////////////////
// KL --> pi+ pi- pi0

void CMC::Kl2pipipi0() {
  kl2pipipi0(Get4Momentum(fBeamIndex-1), fPizeroDecay);
}

/////////////////////
// KL --> pi0 pi0 pi0

void CMC::Kl2pi0pi0pi0() {
  kl2pi0pi0pi0(Get4Momentum(fBeamIndex-1), fPizeroDecay);
}

///////////////
// KL --> e+ e-

void CMC::Kl2ee() {
  two_body_decay(Get4Momentum(fBeamIndex-1), PDG_ID_kl, PDG_ID_elp, PDG_ID_elm, 0.0, 0.0, 0);
}

/////////////////
// KL --> mu+ mu-

void CMC::Kl2mumu() {
  two_body_decay(Get4Momentum(fBeamIndex-1), PDG_ID_kl, PDG_ID_mup, PDG_ID_mum, 0.0, 0.0, 0);
}

////////////////
// KL --> e+ mu-

void CMC::Kl2emu() {
  two_body_decay(Get4Momentum(fBeamIndex-1), PDG_ID_kl, PDG_ID_elp, PDG_ID_mum, 0.0, 0.0, 0);
}

////////////////
// KL --> mu+ e-

void CMC::Kl2mue() {
  two_body_decay(Get4Momentum(fBeamIndex-1), PDG_ID_kl, PDG_ID_mup, PDG_ID_elm, 0.0, 0.0, 0);
}

///////////////////
// KL --> pi0 e+ e-

void CMC::Kl2pi0ee() {
  kl2pi0ll(Get4Momentum(fBeamIndex-1), fPizeroDecay, 0);
}

/////////////////////
// KL --> pi0 mu+ mu-

void CMC::Kl2pi0mumu() {
  kl2pi0ll(Get4Momentum(fBeamIndex-1), fPizeroDecay, 1);
}

////////////////////
// KL --> pi0 e+ mu-

void CMC::Kl2pi0emu() {
  kl2pi0ll(Get4Momentum(fBeamIndex-1), fPizeroDecay, 2);
}

////////////////////
// KL --> pi0 mu+ e-

void CMC::Kl2pi0mue() {
  kl2pi0ll(Get4Momentum(fBeamIndex-1), fPizeroDecay, 3);
}

/////////////////////////////////
// Import the generated particles

int CMC::ImportParticles(ParticleVector* particles) {

  if (!particles) return 0;

  ParticleVector::const_iterator it;
  for (it=particles->begin(); it!=particles->end(); ++it) delete *it;
  particles->clear();

  int nparts = 0;
  for (int i=0; i<fCMCint->NPART; i++) {
    if (!fCMCint->KEEP[i]) continue;
    nparts++;
    particles->push_back
      (new CMCParticle
       (fCMCint->PID[i],
	fCMCdouble->P4INI[i][3], fCMCdouble->P4INI[i][0],
	fCMCdouble->P4INI[i][1], fCMCdouble->P4INI[i][2],
	fCMCdouble->POL3INI[i][0],
	fCMCdouble->POL3INI[i][1],
	fCMCdouble->POL3INI[i][2]));
  }
  return nparts;
}

////////////////
// Other methods

TLorentzVector CMC::Get4Momentum(int index) {
  return TLorentzVector
    (fCMCdouble->P4INI[index][0], fCMCdouble->P4INI[index][1],
     fCMCdouble->P4INI[index][2], fCMCdouble->P4INI[index][3]);
}

TLorentzVector CMC::GetGenerated4Momentum(int index) {
  return TLorentzVector
    (fCMCdouble->P4INIGEN[index][0], fCMCdouble->P4INIGEN[index][1],
     fCMCdouble->P4INIGEN[index][2], fCMCdouble->P4INIGEN[index][3]);
}

TVector3 CMC::Get3Momentum(int index) {
  return TVector3
    (fCMCdouble->P4INI[index][0],
     fCMCdouble->P4INI[index][1],
     fCMCdouble->P4INI[index][2]);
}

TVector3 CMC::GetGenerated3Momentum(int index) {
  return TVector3
    (fCMCdouble->P4INIGEN[index][0],
     fCMCdouble->P4INIGEN[index][1],
     fCMCdouble->P4INIGEN[index][2]);
}

void CMC::SetGenerated4Momentum(int i, TLorentzVector momentum) {
  // conversion from GeV (CMC) to MeV (Geant)
  fCMCdouble->P4INIGEN[i][0] = 1e3 * momentum.Px();
  fCMCdouble->P4INIGEN[i][1] = 1e3 * momentum.Py();
  fCMCdouble->P4INIGEN[i][2] = 1e3 * momentum.Pz();
  fCMCdouble->P4INIGEN[i][3] = 1e3 * momentum.E();
}

void CMC::Set4Momentum(int index, TLorentzVector momentum) {
  fCMCdouble->P4INI[index][0] = momentum.Px();
  fCMCdouble->P4INI[index][1] = momentum.Py();
  fCMCdouble->P4INI[index][2] = momentum.Pz();
  fCMCdouble->P4INI[index][3] = momentum.E();
}

void CMC::SetPolarization(int i, TVector3 polar) {
  fCMCdouble->POL3INI[i][0] = polar.X();
  fCMCdouble->POL3INI[i][1] = polar.Y();
  fCMCdouble->POL3INI[i][2] = polar.Z();
}


//////////////////////////////////////////////////////////
// Input: generated particle index, output: particle name.
// Required to fill the GeneParts.

TString CMC::GetGeneratedName (int index) {
  switch (fCMCint->PIDGEN[index]) {
  case PDG_ID_pip: return "pi+";
  case PDG_ID_pim: return "pi-";
  case PDG_ID_pi0: return "pi0";
  case PDG_ID_elp: return "e+";
  case PDG_ID_elm: return "e-";
  case PDG_ID_mup: return "mu+";
  case PDG_ID_mum: return "mu-";
  case PDG_ID_gam: return "gamma";
  case PDG_ID_nu:  return "nu";
  case PDG_ID_kp:  return "kaon+";
  case PDG_ID_km:  return "kaon-";
  case PDG_ID_kl:  return "KL";
  }

  if (fCMCint->PIDGEN[index]>=PDG_ID_exo &&
      fCMCint->PIDGEN[index]<PDG_ID_exo+200) return "Exotic";
  return "Unknown";
}
