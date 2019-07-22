#include "G4CMCDecayer.hh"
#include "CMC.hh"
#include "G4DynamicParticle.hh"
#include "G4DecayProducts.hh"
#include "G4DecayTable.hh"
#include "G4ParticleTable.hh"
#include "G4Track.hh"
#include <CLHEP/Vector/LorentzVector.h>
#include "DatacardManager.hh"
#include "masses.hh"

G4CMCDecayer::G4CMCDecayer() :
  G4VExtDecayer("G4CMCDecayer"), fDecayProductsArray(0), fDecayType(-999), fNTrials(-1) {
  fDecayProductsArray = new ParticleVector();
}

G4CMCDecayer::~G4CMCDecayer() {
  delete fDecayProductsArray;
}

//////////////////////////////////////////////////////
// Return G4 particle definition for given CMCParticle

G4ParticleDefinition* G4CMCDecayer::GetParticleDefinition(const CMCParticle* particle) const {
  G4int pdgEncoding = particle->fPid;
  G4ParticleTable* particleTable = G4ParticleTable::GetParticleTable();
  G4ParticleDefinition* particleDefinition = 0;
  if (pdgEncoding) particleDefinition = particleTable->FindParticle(pdgEncoding);
  return particleDefinition;
}

//////////////////////////////
// Create G4DynamicParticle //
//////////////////////////////
G4DynamicParticle* G4CMCDecayer::CreateDynamicParticle(const CMCParticle* particle) const { 
  G4ParticleDefinition* particleDefinition = GetParticleDefinition(particle);    
  if (!particleDefinition) return 0;
  G4ThreeVector momentum = GetParticleMomentum(particle);
  G4DynamicParticle* dynamicParticle = new G4DynamicParticle(particleDefinition, momentum);
  dynamicParticle->SetPolarization(particle->fPolX,particle->fPolY, particle->fPolZ);
  return dynamicParticle;
}

//////////////////////////////                        
// Return particle momentum //
//////////////////////////////                        
G4ThreeVector G4CMCDecayer::GetParticleMomentum(const CMCParticle* particle) const {
  G4ThreeVector momentum = G4ThreeVector(particle->fPx*GeV,particle->fPy*GeV,particle->fPz*GeV);
  return momentum;
}

/////////////////////////////////
// Force a particle decay mode //
/////////////////////////////////
void G4CMCDecayer::Decay(G4int idbeam, const CLHEP::HepLorentzVector& p) {

  if (fDecayType<0) fDecayType = DatacardManager::GetInstance()->GetDecayType();
  CMC::GetInstance()->InitDecay(idbeam,p[0],p[1],p[2],p[3]);

  // Hadronic modes (0-19)

  if      (fDecayType==0)   CMC::GetInstance()->Kch2pinunu();
  else if (fDecayType==1)   CMC::GetInstance()->Kch2pipi0();
  else if (fDecayType==2)   CMC::GetInstance()->Kch2pipi0g_ib();
  else if (fDecayType==3)   CMC::GetInstance()->Kch2piX0();
  else if (fDecayType==4)   CMC::GetInstance()->Kch2pipi0dg();
  else if (fDecayType==10)  CMC::GetInstance()->Kch2pipipi();
  else if (fDecayType==11)  CMC::GetInstance()->Kch2pipi0pi0();

  // Leptonic modes (20-39)

  else if (fDecayType==20)  CMC::GetInstance()->Kch2enu();
  else if (fDecayType==21)  CMC::GetInstance()->Kch2enug_ib();
  else if (fDecayType==22)  CMC::GetInstance()->Kch2enug_ib_cutoff();
  else if (fDecayType==23)  CMC::GetInstance()->Kch2enug_sdp();
  else if (fDecayType==24)  CMC::GetInstance()->Kch2enug_sdm();
  else if (fDecayType==25)  CMC::GetInstance()->Kch2enug_intp();
  else if (fDecayType==26)  CMC::GetInstance()->Kch2enug_intm();
  else if (fDecayType==27)  CMC::GetInstance()->Kch2enununu();

  else if (fDecayType==30)  CMC::GetInstance()->Kch2munu();
  else if (fDecayType==31)  CMC::GetInstance()->Kch2munug_ib();
  else if (fDecayType==32)  CMC::GetInstance()->Kch2munug_ib_cutoff();
  else if (fDecayType==33)  CMC::GetInstance()->Kch2munug_sdp();
  else if (fDecayType==34)  CMC::GetInstance()->Kch2munug_sdm();
  else if (fDecayType==35)  CMC::GetInstance()->Kch2munug_intp();
  else if (fDecayType==36)  CMC::GetInstance()->Kch2munug_intm();
  else if (fDecayType==37)  CMC::GetInstance()->Kch2munununu();

  // Semileptonic modes -- Kl3 (40-59)

  else if (fDecayType==40)  CMC::GetInstance()->Kch2pienu();
  else if (fDecayType==41)  CMC::GetInstance()->Kch2pimunu();
  else if (fDecayType==42)  CMC::GetInstance()->Kch2pienug_kloe();
  else if (fDecayType==43)  CMC::GetInstance()->Kch2pimunug_kloe();
  else if (fDecayType==44)  CMC::GetInstance()->Kch2pienug_ib_de();

  // Semileptonic modes -- Kl4 (60-79)
  // NB: the Ferrara generators have not been validated

  else if (fDecayType==60)  CMC::GetInstance()->Kch2pipienu();
  else if (fDecayType==61)  CMC::GetInstance()->Kch2pi0pi0enu();
  else if (fDecayType==70)  CMC::GetInstance()->Kch2pipienu_ferrara();
  else if (fDecayType==71)  CMC::GetInstance()->Kch2pi0pi0enu_ferrara();
  else if (fDecayType==72)  CMC::GetInstance()->Kch2pipimunu_ferrara();
  else if (fDecayType==73)  CMC::GetInstance()->Kch2pi0pi0munu_ferrara();

  // Hadronic modes with photons (80-99)

  else if (fDecayType==80)  CMC::GetInstance()->Kch2pipi0g_ib_cutoff();
  else if (fDecayType==81)  CMC::GetInstance()->Kch2pipi0g_de();
  else if (fDecayType==82)  CMC::GetInstance()->Kch2pipi0g_int();
  else if (fDecayType==83)  CMC::GetInstance()->Kch2pigg();
  else if (fDecayType==84)  CMC::GetInstance()->Kch2pigee();
  else if (fDecayType==85)  CMC::GetInstance()->Kch2pigmumu();
  else if (fDecayType==86)  CMC::GetInstance()->Kch2pipi0ee_ib();
  else if (fDecayType==87)  CMC::GetInstance()->Kch2pipipig();

  // Four-lepton modes

  else if (fDecayType==100) CMC::GetInstance()->Kch2enuee();
  else if (fDecayType==101) CMC::GetInstance()->Kch2munuee();
  else if (fDecayType==102) CMC::GetInstance()->Kch2enumumu();
  else if (fDecayType==103) CMC::GetInstance()->Kch2munumumu();

  // Flavour-changing neutral currents, lepton flavour and number violating modes

  else if (fDecayType==120) CMC::GetInstance()->Kch2piee();
  else if (fDecayType==121) CMC::GetInstance()->Kch2pimumu();

  else if (fDecayType==130) CMC::GetInstance()->Kch2pip_mup_em();
  else if (fDecayType==131) CMC::GetInstance()->Kch2pip_mum_ep();
  else if (fDecayType==132) CMC::GetInstance()->Kch2pim_mup_ep();
  else if (fDecayType==133) CMC::GetInstance()->Kch2pim_ep_ep();
  else if (fDecayType==134) CMC::GetInstance()->Kch2pim_mup_mup();

  else if (fDecayType==140) CMC::GetInstance()->Kch2enumumu_LFV();
  else if (fDecayType==141) CMC::GetInstance()->Kch2munuee_LFV();

  // Exotic particle modes
  else if (fDecayType==150)  CMC::GetInstance()->Kch2munuA();
  else if (fDecayType==151)  CMC::GetInstance()->Kch2munuXscal();
  else if (fDecayType==152)  CMC::GetInstance()->Kch2munuXvec();

  // Beam pion leptonic decays

  else if (fDecayType==220) CMC::GetInstance()->Pich2enu();
  else if (fDecayType==221) CMC::GetInstance()->Pich2enug_ib();
  else if (fDecayType==222) CMC::GetInstance()->Pich2enug_ib_cutoff();
  else if (fDecayType==223) CMC::GetInstance()->Pich2enug_sdp();
  else if (fDecayType==224) CMC::GetInstance()->Pich2enug_sdm();
  else if (fDecayType==225) CMC::GetInstance()->Pich2enug_intp();
  else if (fDecayType==226) CMC::GetInstance()->Pich2enug_intm();
  else if (fDecayType==227) CMC::GetInstance()->Pich2enununu();

  else if (fDecayType==230) CMC::GetInstance()->Pich2munu();
  else if (fDecayType==231) CMC::GetInstance()->Pich2munug_ib();
  else if (fDecayType==232) CMC::GetInstance()->Pich2munug_ib_cutoff();
  else if (fDecayType==233) CMC::GetInstance()->Pich2munug_sdp();
  else if (fDecayType==234) CMC::GetInstance()->Pich2munug_sdm();
  else if (fDecayType==235) CMC::GetInstance()->Pich2munug_intp();
  else if (fDecayType==236) CMC::GetInstance()->Pich2munug_intm();
  else if (fDecayType==237) CMC::GetInstance()->Pich2munununu();

  // Beam KL decays

  // Hadronic modes (300-319)
  else if (fDecayType==300) CMC::GetInstance()->Kl2pipi();
  else if (fDecayType==301) CMC::GetInstance()->Kl2pi0pi0();
  else if (fDecayType==302) CMC::GetInstance()->Kl2pipipi0();
  else if (fDecayType==303) CMC::GetInstance()->Kl2pi0pi0pi0();

  // Leptonic modes (320-339)
  else if (fDecayType==320) CMC::GetInstance()->Kl2ee();
  else if (fDecayType==321) CMC::GetInstance()->Kl2mumu();
  else if (fDecayType==322) CMC::GetInstance()->Kl2emu();
  else if (fDecayType==323) CMC::GetInstance()->Kl2mue();

  else if (fDecayType==340) CMC::GetInstance()->Kl2pi0ee();
  else if (fDecayType==341) CMC::GetInstance()->Kl2pi0mumu();
  else if (fDecayType==342) CMC::GetInstance()->Kl2pi0emu();
  else if (fDecayType==343) CMC::GetInstance()->Kl2pi0mue();
}

///////////////////////////
// Import decay products //
///////////////////////////
G4DecayProducts* G4CMCDecayer::ImportDecayProducts(const G4Track& track) {

  // Get particle momentum
  G4ThreeVector momentum = track.GetMomentum();
  G4double etot = track.GetDynamicParticle()->GetTotalEnergy();
  CLHEP::HepLorentzVector p;
  p[0] = momentum.x()/GeV;
  p[1] = momentum.y()/GeV;
  p[2] = momentum.z()/GeV;
  p[3] = etot/GeV;
  G4int id = track.GetDefinition()->GetPDGEncoding();

  // Let CMCDecayer decay the particle and import the decay products
  Decay (id, p);
  G4int NParticles = CMC::GetInstance()->ImportParticles(fDecayProductsArray);

  // Convert decay products CMCParticle type to G4DecayProducts  
  G4DecayProducts* decayProducts = new G4DecayProducts(*(track.GetDynamicParticle()));
  G4int counter = 0;
  for (G4int i=0; i<NParticles; i++) {
    // Get particle from ParticleVector
    CMCParticle* particle = (*fDecayProductsArray)[i];

    // Create G4DynamicParticle
    G4DynamicParticle* dynamicParticle = CreateDynamicParticle(particle);
    if (dynamicParticle) {
      // Add dynamicParticle to decayProducts
      decayProducts->PushProducts(dynamicParticle);
      counter++;
    }
  }
  return decayProducts;
}

G4int G4CMCDecayer::Generate(G4double px, G4double py, G4double pz, G4double e) {
  CLHEP::HepLorentzVector p;
  p[0] = px;
  p[1] = py;
  p[2] = pz;
  p[3] = e;
  Decay(PDG_ID_kp, p);
  fNTrials = CMC::GetInstance()->GetNTrials();
  return CMC::GetInstance()->ImportParticles(fDecayProductsArray);
}
