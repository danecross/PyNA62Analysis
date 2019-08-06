// ---------------------------------------------------------------
//
// History:
//
// Handling of +ve and -ve halo particles added by Chris Parkinson, 2019-07-16
// Pileup in LKr added by Michele Corvino 2019-04-30
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2019-02-07
//
// ---------------------------------------------------------------

/// \class DownstreamPileupGenerator
/// \Brief
/// Generation of pileup in the downstream detectors
/// \EndBrief
/// \Detailed
/// Pileup particles (beam and halo, separately) are taken from the PileupParticleGenerator.
/// Beam particles (can) decay according to specified rules (e.g. kaons decays in the 6 main modes).
/// For each pileup particle, an appropriate entry from one of the MC hits libraries is read.
/// The hits are then added into the MC event at the time determined in the PileupParticleGenerator.
///
/// This analyzer should be used as a pre-analyzer, best to put it at the start of the list of
/// pre-analyzers in the config file. It should normally be followed by SpectrometerRecoAnalyzer
/// so that the spectrometer reconstruction can be run again.
///
/// The MC hits are stored in (currently) 6 libraries, which are ROOT TTrees stored in a ROOT TFile.
/// The library TFile is stored at NA62Tools/Conditions/MC/DownstreamPileupGeneratorLibrary.root,
/// and is accessed via the NA62ConditionsService.
/// Each library contains O(10^5) events; each event contains several detectors; each detector contains
/// O(100) MC hits; each MC hit contains 3 pieces of information.
///
/// The MC hits are read into memory, being stored in a nest of std::vectors.
/// Each MC hit is stored as a std::tuple<Int_t, Float_t, Float_t> corresponding to the
/// channel/tile ID, the time of the hit w.r.t. zero, and the time width (Spectrometer only).
/// There is a typedef of the std::tuple as "pileuphit" for convenience.
/// The typedef is contained inside the "dpg" namespace.
///
/// The number of libraries expected in the ROOT TFile (11) and the number of supported
/// detectors (currently 7) are hard-coded in the constructor of this class.
///
/// Histograms produced are:
///  "Gen_Time": the time of the pileup event, set by the PileupGenerator;<br>
///  "Gen_Entry": which event (number) was read from the library (should be flat);<br>
///  "Gen_Kaon": number of kaon decays generated;<br>
///  "Gen_Pion": number of pion decays generated (currently zero);<br>
///  "Gen_Halo": number of halo tracks generated (currently equal to number of kaon decays in 102-180m);<br>
///  "Gen_Total": total number of decays and halo tracks generated;<br>
///  "nDetectorHits": 2D plot with number of Detector (Spectrometer, NewCHOD, MUV3) hits before and after the pileup hits were added to the event;<br>
///  "KaonDecayMode": the fraction of kaon decays in each decay mode (mode 6 should be empty!);<br>
///  "LibraryUsed": how often was each of the libraries used?<br>
///  "LibrarySize": number of entries in each of the libraries.
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "DownstreamPileupGenerator.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

#include "NA62ConditionsService.hh"
#include "PileupParticleGenerator.hh"
#include "TSpectrometerDigi.hh"
#include "TNewCHODDigi.hh"
#include "TMUV3Digi.hh"
#include "TLAVDigi.hh"
#include "TIRCDigi.hh"
#include "TSACDigi.hh"

DownstreamPileupGenerator::DownstreamPileupGenerator(Core::BaseAnalysis *ba) :
  Analyzer(ba, "DownstreamPileupGenerator"),
  fNLibraries(13), fNDetectors(7)
{
  fSpectrometerEvent = 0;
  fNewCHODEvent      = 0;
  fMUV3Event         = 0;
  fLKrEvent          = 0;
  fLAVEvent          = 0;

  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
  RequestTree("LKr", new TRecoLKrEvent, "Reco");
  RequestTree("LAV", new TRecoLAVEvent, "Reco");
  RequestTree("IRC", new TRecoIRCEvent, "Reco");
  RequestTree("SAC", new TRecoSACEvent, "Reco");

  // should generate pileup hits in these detectors?
  AddParam("GenerateSpectrometerPileup", &fGenerateSpectrometerPileup, true);
  AddParam("GenerateNewCHODPileup", &fGenerateNewCHODPileup, true);
  AddParam("GenerateMUV3Pileup", &fGenerateMUV3Pileup, true);
  AddParam("GenerateLKrPileup", &fGenerateLKrPileup, true);
  AddParam("GenerateLAVPileup", &fGenerateLAVPileup, true);
  AddParam("GenerateIRCPileup", &fGenerateIRCPileup, true);
  AddParam("GenerateSACPileup", &fGenerateSACPileup, true);

  // should generate pileup from halo?
  AddParam("GenerateHaloPileup",&fGenerateHaloPileup, true);

  // MUV3 deadtime.
  AddParam("ApplyMUV3Deadtime", &fApplyMUV3Deadtime, true);
  AddParam("ApplyNewCHODDeadtime", &fApplyNewCHODDeadtime, true);
  AddParam("CFDDeadtime", &fCFDDeadtime, 40.0);
  AddParam("ForcedOnData",&fForcedOnData,false);

  // equation to compute the decay fractions below is df = 1-e^(L/gamma.c.tau)
  // L = zMax [m] - zMin [m] (generation volume)
  // decay volumes: 102.425 - 180 [m] (standard) and 102.425 - 265 [m] (extended)
  // tau = 1.238e-8 [s] (kaon) or 2.603e-8 [s] (pion)
  // c = 299792458 [ms-1]
  // gamma = p/m = 75 [GeV] / (0.493667 [GeV] (kaon) or 0.139571 [GeV] (pion))

  fKaonDecayFraction    = 0.1285; // Fraction of kaons that decay in 102.425 - 180.000 m
  fKaonEDRDecayFraction = 0.2505; // Fraction of kaons that decay in 102.425 - 265.000 m
  fPionEDRDecayFraction = 0.0380; // Fraction of pions that decay in 102.425 - 265.000 m
  fHaloPositiveFraction = (110934.0/135342.0); // Fraction of halo that is positively charged

  fKaonDecays.resize(6);
  fKaonDecays[0] = 0.63560; // Km2
  fKaonDecays[1] = 0.20670; // K2pi
  fKaonDecays[2] = 0.05583; // K3pi
  fKaonDecays[3] = 0.05070; // Ke3
  fKaonDecays[4] = 0.03352; // Km3
  fKaonDecays[5] = 0.01760; // K3pi0

  fTotalKaonDecay = 0.99995; // total of the 6 main branching fractions

  AddParam("UseExtendedDecayRegion", &fUseEDR, true);
  fKaonEDRDecays.resize(3);
  fKaonEDRDecays[0] = 0.63560 + 0.03352; // inc. single muon
  fKaonEDRDecays[1] = 0.20670 + 0.05070 + 0.01760; // inc. single !muon track
  fKaonEDRDecays[2] = 0.05583; // inc. three-track

  fRandom = new TRandom2();
  fMUV3Geo = MUV3Geometry::GetInstance();

  // names of libraries within the library file
  fLibNames.resize(fNLibraries);
  fLibNames[0] = "Km2";
  fLibNames[1] = "K2pi";
  fLibNames[2] = "K3pi";
  fLibNames[3] = "Ke3";
  fLibNames[4] = "Km3";
  fLibNames[5] = "K3pi0";
  fLibNames[6] = "Km2EDR";
  fLibNames[7] = "K2piEDR";
  fLibNames[8] = "K3piEDR";
  fLibNames[9] = "HaloPlus";
  fLibNames[10] = "HaloMinus";
  fLibNames[11] = "Pim2";
  fLibNames[12] = "CTRL"; // real data from control trigger
  
  fHitLibrary.clear();
}

void DownstreamPileupGenerator::InitOutput(){}

void DownstreamPileupGenerator::InitHist(){

  BookHisto(new TH1F("Gen_Entry", "Gen_Entry", 404, -0.5, 20099.5));

  BookHisto(new TH2F("nSpectrometerHits", "nSpectrometerHits", 126, -0.5, 251.5, 101, -0.5, 504.5));
  BookHisto(new TH2F("nNewCHODHits", "nNewCHODHits", 101, -0.5, 100.5, 101, -0.5, 203.5));
  BookHisto(new TH2F("nMUV3Hits", "nMUV3Hits", 21, -0.5, 20.5, 31, -0.5, 30.5));
  BookHisto(new TH2F("nLKrHits", "nLKrHits", 21, -0.5, 20.5, 31, -0.5, 30.5));
  BookHisto(new TH2F("nLAVHits", "nLAVHits", 21, -0.5, 20.5, 31, -0.5, 30.5));

  BookHisto(new TH1F("KaonDecayMode", "KaonDecayMode", 7, -0.5, 6.5));
  BookHisto(new TH1F("LibraryUsed", "LibraryUsed", fNLibraries, -0.5, fNLibraries-0.5));
  BookHisto(new TH1F("LibrarySize", "LibrarySize", fNLibraries, -0.5, fNLibraries-0.5));

  BookHistoArray(new TH1F("MUV3Channels", "MUV3Channels", 152, -0.5, 151.5), fNLibraries);
  BookHistoArray(new TH1F("MUV3ChannelsAD", "MUV3ChannelsAD", 152, -0.5, 151.5), fNLibraries);

  BookHistoArray(new TH1F("NewCHODChannels", "NewCHODChannels", 350, 100.5, 450.5), fNLibraries);
  BookHistoArray(new TH1F("NewCHODChannelsAD", "NewCHODChannelsAD", 350, 100.5, 450.5), fNLibraries);

  BookHistoArray(new TH1F("NewCHODSeqChannels", "NewCHODSeqChannels", 152, -0.5, 151.5), fNLibraries);
  BookHistoArray(new TH1F("NewCHODSeqChannelsAD", "NewCHODSeqChannelsAD", 152, -0.5, 151.5), fNLibraries);

  BookHisto(new TH1F("MUV3HitRemoved", "MUV3HitRemoved", 152, -0.5, 151.5));
  BookHisto(new TH1F("NewCHODHitRemoved", "NewCHODHitRemoved", 350, 100.5, 450.5));

  // don't reconfigure unless needed.
  if(!GetWithMC() && !fForcedOnData)        return;
  if(!GetIsTree())        return;
  ReconfigureAnalyzer("SpectrometerRecoAnalyzer", "UpdateWireDistance", true);
}

void DownstreamPileupGenerator::DefineMCSimple(){
}

void DownstreamPileupGenerator::StartOfRunUser(){
  if(!GetWithMC() && !fForcedOnData)        return;
  if(!GetIsTree())        return;
  ReadHitLibrary();
}

void DownstreamPileupGenerator::StartOfBurstUser(){
  if(!GetIsTree())      return;
  if(!GetEventHeader()) return;
  fRandom->SetSeed(GetEventHeader()->GetBurstID()); // to ensure reproducibility
}

void DownstreamPileupGenerator::ProcessSpecialTriggerUser(int , unsigned int ){
}

void DownstreamPileupGenerator::Process(int ){
  if(!GetWithMC() && !fForcedOnData)        return;
  if(!GetIsTree())        return;
  if(!fHitLibrary.size()) return; // ensure that hit library was created

  // get list of beam particles
  const UpstreamParticles* beamParticles =
    GetOutput<UpstreamParticles>("PileupParticleGenerator.BeamParticles");

  // get list of halo particles
  const UpstreamParticles* haloParticles =
    GetOutput<UpstreamParticles>("PileupParticleGenerator.HaloParticles");

  // get upstream particle iterator.
  UpstreamParticles::const_iterator particle = beamParticles->begin();

  fSpectrometerEvent = GetEvent<TRecoSpectrometerEvent>("Reco");
  fNewCHODEvent = GetEvent<TRecoNewCHODEvent>("Reco");
  fMUV3Event = GetEvent<TRecoMUV3Event>("Reco");
  fLKrEvent = GetEvent<TRecoLKrEvent>("Reco");
  fLAVEvent = GetEvent<TRecoLAVEvent>("Reco");
  Int_t nSpectrometerHitsBeforePileup = fSpectrometerEvent->GetNHits();
  Int_t nNewCHODHitsBeforePileup = fNewCHODEvent->GetNHits();
  Int_t nMUV3HitsBeforePileup = fMUV3Event->GetNCandidates();
  Int_t nLKrHitsBeforePileup = fLKrEvent->GetNCandidates();
  Int_t nLAVHitsBeforePileup = fLAVEvent->GetNHits();
  ////////////////////////////////////////////////////////
  // add pileup from kaon decays and beam pions
  ////////////////////////////////////////////////////////
  for( ; particle != beamParticles->end(); ++particle){
    Float_t time     = particle->first;
    Int_t particleID = particle->second;

    // Determine particle behaviour from particleID
    UInt_t lib = DecodePID(particleID);
    if(lib==99) continue;

    if(lib>=fLibNames.size()){
      std::cout << user_normal() << "Error: library value is not in valid range!" << std::endl;
      continue;
    }

    // Generate downstream pileup!
    GeneratePileup(time, lib);
  }

  ////////////////////////////////////////////////////////
  // add pileup from beam halo
  ////////////////////////////////////////////////////////
  if(fGenerateHaloPileup){
    particle = haloParticles->begin(); // re-use 'particle' iterator ...
    for( ; particle != haloParticles->end(); ++particle){
      Float_t time     = particle->first;    
      Int_t particleID = particle->second;
      
      UInt_t lib = DecodePID(particleID);
      if(lib==99) continue; 

      if(lib>=fLibNames.size()){
	std::cout << user_normal() << "Error: library value is not in valid range!" << std::endl;
	continue;
      }
      
      // Generate halo pileup!
      GeneratePileup(time, lib);
    }
  }

  ////////////////////////////////////////////////////////
  // apply deadtime in MUV3
  ////////////////////////////////////////////////////////
  if(fApplyMUV3Deadtime)    ApplyDeadtime<TRecoMUV3Event, TRecoMUV3Candidate>();
  if(fApplyNewCHODDeadtime) ApplyDeadtime<TRecoNewCHODEvent, TRecoNewCHODHit>();

  // check the number of hits/candi after pileup
  Int_t nSpectrometerHitsAfterPileup = fSpectrometerEvent->GetNHits();
  Int_t nNewCHODHitsAfterPileup = fNewCHODEvent->GetNHits();
  Int_t nMUV3HitsAfterPileup = fMUV3Event->GetNCandidates();
  Int_t nLKrHitsAfterPileup = fLKrEvent->GetNCandidates();
  Int_t nLAVHitsAfterPileup = fLAVEvent->GetNHits();
  FillHisto("nSpectrometerHits", nSpectrometerHitsBeforePileup, nSpectrometerHitsAfterPileup);
  FillHisto("nNewCHODHits", nNewCHODHitsBeforePileup, nNewCHODHitsAfterPileup);
  FillHisto("nMUV3Hits", nMUV3HitsBeforePileup, nMUV3HitsAfterPileup);
  FillHisto("nLKrHits", nLKrHitsBeforePileup, nLKrHitsAfterPileup);
  FillHisto("nLAVHits", nLAVHitsBeforePileup, nLAVHitsAfterPileup);
}

UInt_t DownstreamPileupGenerator::KaonDecay(std::vector<Double_t>& kaonDecays){
  // pick kaon decay mode
  Double_t rn=fRandom->Rndm()*fTotalKaonDecay;
  Double_t c = 0.0;
  for(UInt_t i=0 ; i<kaonDecays.size(); ++i){
    c+= kaonDecays[i];
    if(rn<c){
      FillHisto("KaonDecayMode", i);
      return i;
    }
  }
  std::cout << user_normal() << "Error: Kaon decay generation failed with rn=" << rn
    << " returning 0 (km2 library)" << std::endl;
  return 0;
}

UInt_t DownstreamPileupGenerator::DecodePID(Int_t particleID){

  // Decide which library to read, depending on particle ID
  UInt_t lib=99;

  // control-triggered data events
  if(particleID==1000){
    lib = 12;
  }
  // natural kaons
  else if(particleID==0||particleID==100){ 
    Double_t rn = fRandom->Rndm();
    if(rn>fKaonEDRDecayFraction) return 99; // kaon does not decay
    if(rn>fKaonDecayFraction){
      if(fUseEDR) lib = KaonDecay(fKaonEDRDecays)+6; // kaon decayed in extended decay region.
      else        return 99; // kaon does not decay because EDR is not being used.
    }
    else{
      lib = KaonDecay(fKaonDecays); // kaon decays in FV
    }
  }
  // forced kaons
  else if(particleID>100 && particleID<110){
    lib = particleID-101; // forced kaon decays, particleID{101 to 109} become lib{0 to 8}
  }
  // natural pions
  else if(particleID==1||particleID==200){
    // check pion decay fraction
    Double_t rn = fRandom->Rndm();
    if(rn>fPionEDRDecayFraction) return 99; // pion did not decay
    lib=11; // Pim2 is library 11
  }
  // forced pions
  else if(particleID==201){
    lib = 11; // forced pim2
  }
  // natural proton 
  else if(particleID==2||particleID==300){
    return 99; // never do anything for protons (?)
  }
  // halo particles
  else if(particleID==3||particleID==400){
    Double_t rn = fRandom->Rndm();
    if(rn>fHaloPositiveFraction) lib=10; // negative
    else                         lib=9;  // positive
  }
  else if(particleID==401){
    lib=9; // positive beam halo 
  }
  else if(particleID==402){
    lib=10; // negative beam halo
  }
  else{
    std::cout << user_normal() << "Error: unknown particle ID "
      << particleID << " in DecodePID function." << std::endl;
  }

  FillHisto("LibraryUsed", lib);
  return lib;
}

void DownstreamPileupGenerator::GeneratePileup(Double_t time, UInt_t lib){

  // pick a pileup track ID here.
  UInt_t trackID = fRandom->Integer(fHitLibrary[lib].size());
  FillHisto("Gen_Entry", trackID);

  if(fGenerateSpectrometerPileup){
    TRecoSpectrometerEvent* event = GetEvent<TRecoSpectrometerEvent>("Reco");
    if(!event){
      std::cout << user_normal() << "Spectrometer event not found!" << std::endl;
      return;
    }
    GenerateSpectrometerPileup(event, time, fHitLibrary[lib][trackID][0]);
  }

  if(fGenerateNewCHODPileup){
    TRecoNewCHODEvent* event = GetEvent<TRecoNewCHODEvent>("Reco");
    if(!event){
      std::cout << user_normal() << "NewCHOD event not found!" << std::endl;
      return;
    }
    GenerateNewCHODPileup(event, time, fHitLibrary[lib][trackID][1],lib);
  }

  if(fGenerateMUV3Pileup){
    TRecoMUV3Event* event = GetEvent<TRecoMUV3Event>("Reco");
    if(!event){
      std::cout << user_normal() << "MUV3 event not found!" << std::endl;
      return;
    }
    GenerateMUV3Pileup(event, time, fHitLibrary[lib][trackID][2], lib);
  }
  if(fGenerateLKrPileup){
    TRecoLKrEvent* event = GetEvent<TRecoLKrEvent>("Reco");
    if(!event){
      std::cout << user_normal() << "LKr event not found!" << std::endl;
      return;
    }
    GenerateLKrPileup(event, time, fHitLibrary[lib][trackID][3]);
  }
  if(fGenerateLAVPileup){
    TRecoLAVEvent* event = GetEvent<TRecoLAVEvent>("Reco");
    if(!event){
      std::cout << user_normal() << "LAV event not found!" << std::endl;
      return;
    }
    GenerateLAVPileup(event, time, fHitLibrary[lib][trackID][4]);
  }
  if(fGenerateIRCPileup){
    TRecoIRCEvent* event = GetEvent<TRecoIRCEvent>("Reco");
    if(!event){
      std::cout << user_normal() << "IRC event not found!" << std::endl;
      return;
    }
    GenerateIRCPileup(event, time, fHitLibrary[lib][trackID][5]);
  }
  if(fGenerateSACPileup){
    TRecoSACEvent* event = GetEvent<TRecoSACEvent>("Reco");
    if(!event){
      std::cout << user_normal() << "SAC event not found!" << std::endl;
      return;
    }
    GenerateSACPileup(event, time, fHitLibrary[lib][trackID][6]);
  }
}

void DownstreamPileupGenerator::GenerateSpectrometerPileup(TRecoSpectrometerEvent* event, Double_t time, const std::vector<dpg::pileuphit>& pileup){

  // skip empty event
  if(!pileup.size()) return;
  TSpectrometerDigi* SpecDigi = new TSpectrometerDigi();

  Int_t   C;
  Float_t T;
  dpg::SupplInfo W;
  dpg::SupplInfo def;

  for(UInt_t j=0; j<pileup.size(); ++j){

    // unpack tuple (ChannelID, Time, TimeWidth)
    std::tie(C, T, W, def, def) = pileup[j];

    if(C<0){
      std::cout << user_normal() << "Error: Spectrometer hit with invalid channel ID "
        << C << " detected. This hit will be skipped!" << std::endl;
      continue;
    }
    TRecoSpectrometerHit* hit = static_cast<TRecoSpectrometerHit*>(event->AddHit(SpecDigi));
    hit->SetChannelID(C);
    hit->SetTime(time + T);
    hit->SetTimeWidth(W.f);

    // decode channel ID
    hit->SetChamberID(C/1952);
    hit->SetViewID((C%1952)/488);
    hit->SetHalfViewID((C%488)/244);
    hit->SetPlaneID((C%244)/122);
    hit->SetStrawID(C%122);

    hit->SetEdgeStatus(1);
    hit->SetWireDistance(-1.0); // must be evaluated by SpectrometerRecoAlgorithm
  }
  delete SpecDigi;
}

void DownstreamPileupGenerator::GenerateNewCHODPileup(TRecoNewCHODEvent* event, Double_t time, const std::vector<dpg::pileuphit>& pileup, Int_t lib){

  // skip empty event
  if(!pileup.size()) return;

  TNewCHODDigi* NDigi = new TNewCHODDigi();

  Int_t   C;
  Float_t T;
  dpg::SupplInfo def;

  for(UInt_t j=0; j<pileup.size(); ++j){

    // unpack tuple (tileID, time). Ignore 'width'.
    std::tie(C, T, def, def, def) = pileup[j];

    if(C<0){
      std::cout << user_normal() << "Error: NewCHOD hit with invalid channel ID "
        << C << " detected. This hit will be skipped!" << std::endl;
      continue;
    }

    TRecoNewCHODHit* hit = static_cast<TRecoNewCHODHit*>(event->AddHit(NDigi));
    hit->SetType(kUndefinedCandidate);

    hit->SetChannelID(C);
    hit->SetChannel1(C);
    hit->SetChannel2(lib);

    hit->DecodeChannelID();
    hit->SetTime(time + T);
    hit->SetTime1(time + T);
    hit->SetTime2(time + T);
  }
  delete NDigi;
}

void DownstreamPileupGenerator::GenerateMUV3Pileup(TRecoMUV3Event* event, Double_t time, const std::vector<dpg::pileuphit>& pileup, Int_t lib){

  // skip empty event
  if(!pileup.size()) return;

  Int_t   C;
  Float_t T;
  dpg::SupplInfo def;

  for(UInt_t j=0; j<pileup.size(); ++j){

    // unpack tuple (tileID, time). Ignore 'width'.
    std::tie(C, T, def, def, def) = pileup[j];

    if(C<0){
      std::cout << user_normal() << "Error: MUV3 hit with invalid channel ID "
        << C << " detected. This hit will be skipped!" << std::endl;
      continue;
    }

    TRecoMUV3Candidate* cand = static_cast<TRecoMUV3Candidate*>(event->AddCandidate());
    cand->SetType(kUndefinedCandidate);

    cand->SetTileID(C);
    cand->SetChannel1(C);
    cand->SetChannel2(lib);

    TVector2 pos(fMUV3Geo->GetTileCentreX(C), fMUV3Geo->GetTileCentreY(C));
    cand->SetX(pos.X());
    cand->SetY(pos.Y());

    cand->SetTime(time + T);
    cand->SetTime1(time + T);
    cand->SetTime2(time + T);

    cand->SetTimeNoT0(time + T);
    cand->SetTime1NoT0(time + T);
    cand->SetTime2NoT0(time + T);

    cand->SetTimeNoTileT0(time + T);
  }
}

void DownstreamPileupGenerator::GenerateLAVPileup(TRecoLAVEvent* event, Double_t time, const std::vector<dpg::pileuphit>& pileup){

  // skip empty event
  if(!pileup.size()) return;

  TLAVDigi* NDigi = new TLAVDigi();

  Int_t pu_channelID;
  Float_t pu_time;
  dpg::SupplInfo pu_edgeMask;
  dpg::SupplInfo def;

  for(UInt_t j=0; j<pileup.size(); ++j){

    // unpack tuple (tileID, time). Ignore 'width'.
    std::tie(pu_channelID, pu_time, pu_edgeMask, def, def) = pileup[j];

    if(pu_channelID<0){
      std::cout << user_normal() << "Error: LAV hit with invalid channel ID "
        << pu_channelID << " detected. This hit will be skipped!" << std::endl;
      continue;
    }

    TRecoLAVHit* hit = static_cast<TRecoLAVHit*>(event->AddHit(NDigi));

    hit->SetChannelID(pu_channelID);
    hit->DecodeChannelID();
    hit->SetTime(time + pu_time);
    if(pu_edgeMask.i & 1) hit->SetLeadingEdgeLow(0);
    if(pu_edgeMask.i & 2) hit->SetLeadingEdgeHigh(0);
    if(pu_edgeMask.i & 4) hit->SetTrailingEdgeHigh(0);
    if(pu_edgeMask.i & 8) hit->SetTrailingEdgeLow(0);
  }
  delete NDigi;
}

void DownstreamPileupGenerator::GenerateIRCPileup(TRecoIRCEvent* event, Double_t time, const std::vector<dpg::pileuphit>& pileup){

  // skip empty event
  if(!pileup.size()) return;

  TIRCDigi* NDigi = new TIRCDigi();

  Int_t pu_channelID;
  Float_t pu_time;
  dpg::SupplInfo pu_edgeMask;
  dpg::SupplInfo def;

  for(UInt_t j=0; j<pileup.size(); ++j){

    // unpack tuple (tileID, time). Ignore 'width'.
    std::tie(pu_channelID, pu_time, pu_edgeMask, def, def) = pileup[j];

    if(pu_channelID<0){
      std::cout << user_normal() << "Error: IRC hit with invalid channel ID "
        << pu_channelID << " detected. This hit will be skipped!" << std::endl;
      continue;
    }

    TRecoIRCHit* hit = static_cast<TRecoIRCHit*>(event->AddHit(NDigi));

    hit->SetChannelID(pu_channelID);
    hit->DecodeChannelID();
    hit->SetTime(time + pu_time);
    if(pu_edgeMask.i & 1) hit->SetLeadingEdgeLow(0);
    if(pu_edgeMask.i & 2) hit->SetLeadingEdgeHigh(0);
    if(pu_edgeMask.i & 4) hit->SetTrailingEdgeHigh(0);
    if(pu_edgeMask.i & 8) hit->SetTrailingEdgeLow(0);
  }
  delete NDigi;
}

void DownstreamPileupGenerator::GenerateSACPileup(TRecoSACEvent* event, Double_t time, const std::vector<dpg::pileuphit>& pileup){

  // skip empty event
  if(!pileup.size()) return;

  TSACDigi* NDigi = new TSACDigi();

  Int_t pu_channelID;
  Float_t pu_time;
  dpg::SupplInfo pu_edgeMask;
  dpg::SupplInfo def;

  for(UInt_t j=0; j<pileup.size(); ++j){

    // unpack tuple (tileID, time). Ignore 'width'.
    std::tie(pu_channelID, pu_time, pu_edgeMask, def, def) = pileup[j];

    if(pu_channelID<0){
      std::cout << user_normal() << "Error: SAC hit with invalid channel ID "
        << pu_channelID << " detected. This hit will be skipped!" << std::endl;
      continue;
    }

    TRecoSACHit* hit = static_cast<TRecoSACHit*>(event->AddHit(NDigi));

    hit->SetChannelID(pu_channelID);
    hit->DecodeChannelID();
    hit->SetTime(time + pu_time);
    if(pu_edgeMask.i & 1) hit->SetLeadingEdgeLow(0);
    if(pu_edgeMask.i & 2) hit->SetLeadingEdgeHigh(0);
    if(pu_edgeMask.i & 4) hit->SetTrailingEdgeHigh(0);
    if(pu_edgeMask.i & 8) hit->SetTrailingEdgeLow(0);
  }
  delete NDigi;
}

void DownstreamPileupGenerator::GenerateLKrPileup(TRecoLKrEvent* event, Double_t time, const std::vector<dpg::pileuphit>& pileup){

  // skip empty event
  if(!pileup.size()) return;

  dpg::SupplInfo E, X, Y;
  Float_t T;


  for(UInt_t j=0; j<pileup.size(); ++j){

    // unpack tuple (tileID, time). Ignore 'width'.
    std::tie(std::ignore,T,E,X,Y) = pileup[j];

    if(E.f<0){
      std::cout << user_normal() << "Error: LKr cluster with negative energy "
        << E.f << " detected. This candidate will be skipped!" << std::endl;
      continue;
    }

    TRecoLKrCandidate* cand = static_cast<TRecoLKrCandidate*>(event->AddCandidate());

    cand->SetClusterEnergy(E.f);
    cand->SetClusterX(X.f);
    cand->SetClusterY(Y.f);
    cand->SetTime(time + T);
  }
}

void DownstreamPileupGenerator::FillChannelPlot(TRecoMUV3Candidate* cand, TString name){
  FillHistoArray(name, cand->GetChannel2(), cand->GetTileID());
}

void DownstreamPileupGenerator::FillChannelPlot(TRecoNewCHODHit* cand, TString name){
  FillHistoArray(name, cand->GetChannel2(), cand->GetTileID());
  FillHistoArray(name.ReplaceAll("Chan", "SeqChan"), cand->GetChannel2(), cand->GetSeqTileID());
}

template<typename EventType, typename HitType> void DownstreamPileupGenerator::ChannelPlot(TString name){

  HitType* cand = nullptr; // for later
  EventType* event = GetEvent<EventType>("Reco");
  Bool_t type = TString(event->ClassName()).EqualTo("TRecoMUV3Event");

  Int_t nCand=0;
  if(type) nCand=event->GetNCandidates(); // muv3
  else     nCand=event->GetNHits(); // newchod

  // Fill plot of muv3 channels per library after deadtime
  for(int i=0; i<nCand; ++i){
    if(type) cand = dynamic_cast<HitType*>(event->GetCandidate(i));
    else     cand = dynamic_cast<HitType*>(event->GetHit(i));
    if(cand->GetType()!=kUndefinedCandidate) continue;
    if(fabs(cand->GetTime())<45.0) continue;
    if(fabs(cand->GetTime())>75.0) continue;
    FillChannelPlot(cand, name);
  }
}

template<typename EventType, typename HitType> void DownstreamPileupGenerator::ApplyDeadtime(){

  HitType* cand = nullptr; // for later
  EventType* event = GetEvent<EventType>("Reco");
  Bool_t type = TString(event->ClassName()).EqualTo("TRecoMUV3Event");

  TString detname = "MUV3";
  if(!type) detname = "NewCHOD";

  // record channel of muv3 pileup hits
  ChannelPlot<EventType, HitType>(detname+"Channels");

  Int_t nCand=0;
  if(type) nCand=event->GetNCandidates(); // muv3
  else     nCand=event->GetNHits(); // newchod

  std::vector<std::pair<Int_t, HitType*>> hits(nCand);

  // build list of hits and their index
  for(int i=0; i<nCand; ++i){
    if(type) cand = dynamic_cast<HitType*>(event->GetCandidate(i));
    else     cand = dynamic_cast<HitType*>(event->GetHit(i));
    hits[i] = (std::make_pair(i,cand));
  }

  // sort hits according to hit time.
  std::sort(hits.begin(), hits.end(),
      [](const std::pair<Int_t, HitType*>& a, const std::pair<Int_t, HitType*>& b){
      return ((a.second)->GetTime()<(b.second)->GetTime());
      }
      );

  // loop through all hits, checking for earlier hits within 35ns in the same tile.
  // remove the later hit if there is one.

  // read all "tag" hits
  for(unsigned i=0; i<hits.size(); ++i){
    Double_t ta = (hits[i].second)->GetTime();

    // read all "probe" hits
    for(unsigned j=0; j<hits.size(); ++j){

      // don't compare to itself
      if(i==j) continue;

      // ignore hits in different tiles
      if( (hits[i].second)->GetTileID() != (hits[j].second)->GetTileID()) continue;

      // get time of probe hit.
      Double_t tb = (hits[j].second)->GetTime();

      // ignore probe hits after tag hit.
      if(tb>ta) continue;

      // get time difference (tag - probe)
      Double_t td = (ta-tb);

      // if the probe hit is within 35ns of the tag hit ...
      if(td<fCFDDeadtime){
        // remove tag hit from event
        if(type) event->RemoveCandidate( hits[i].first );
        else     event->RemoveHit( hits[i].first );

        // remove tag hit from local (time ordered) list too
        hits.erase(hits.begin()+i);

        // record tile ID of removed hits.
        FillHisto(detname+"HitRemoved", (hits[i].second)->GetTileID());

        // set appropriate value of i and break to 'i' loop
        i--;
        break;
      }
    }
  }

  // record channel of muv3 pileup hits after deadtime
  ChannelPlot<EventType, HitType>(detname+"ChannelsAD");

  // nasty hack to fix the candidates after making the channel plots.
  if(type) nCand=event->GetNCandidates();
  else     nCand=event->GetNHits();

  for(int i=0; i<nCand; ++i){
    if(type) cand = dynamic_cast<HitType*>(event->GetCandidate(i));
    else     cand = dynamic_cast<HitType*>(event->GetHit(i));
    // for my pileup hits...
    if(cand->GetType()==kUndefinedCandidate){
      cand->SetType(kTightCandidate);
      if(type) cand->SetChannel2(cand->GetChannel1()+200); // muv3
      else     cand->SetChannel2(cand->GetChannel1()+50);  // newchod
    }
  }
  return;
}

void DownstreamPileupGenerator::ReadHitLibrary(){
  fHitLibrary.clear();

  TString LibraryFileName = "DownstreamPileupGeneratorLibrary.root";
  TString FullLibraryFileName =
    NA62ConditionsService::GetInstance()->GetFullPath(LibraryFileName);

  if(FullLibraryFileName==""){
    std::cout << user_normal() << "WARNING: " << LibraryFileName << " was not found."
      << ". Pileup will not be generated." << std::endl;
    return;
  }

  // Open library file
  std::cout << user_normal() << "Reading pileup library file ..." << std::endl;
  std::cout << user_normal() << FullLibraryFileName << std::endl;

  TFile* f = TFile::Open(FullLibraryFileName);
  if(!f || !f->IsOpen() || f->IsZombie()){
    std::cout << user_normal() << "WARNING: Tried to read " << FullLibraryFileName << std::endl;
    std::cout << user_normal() << "WARNING: File did not open properly, will not generate pileup!" << std::endl;
    return;
  }

  // for each library in the ROOT file:
  fHitLibrary.resize(fNLibraries); // always

  // for each event in the library (Km2, K2pi, K3pi, ...)
  std::vector<std::vector<std::vector<dpg::pileuphit>>> lib_j; // unknown number of events.

  // for each detector in the event:
  std::vector<std::vector<dpg::pileuphit>> detectors(fNDetectors);

  // arrays to read libraries
  dpg::DetLib<2000, 4> lib_spectrometer;
  dpg::DetLib<500 , 3> lib_newchod;
  dpg::DetLib<500 , 3> lib_muv3;
  dpg::DetLib<25  , 5> lib_lkr;
  dpg::DetLib<500 , 4> lib_lav;
  dpg::DetLib<20  , 4> lib_irc;
  dpg::DetLib<15  , 4> lib_sac;

  for(unsigned j=0; j<fLibNames.size(); ++j){

    TTree* t = nullptr;
    f->GetObject(fLibNames[j],t);
    if(t==nullptr){
      std::cout << user_normal() << "WARNING: Could not find tree called " << fLibNames[j]
        << " (library " << j << "). Pileup will not be generated." << std::endl;
      fHitLibrary.clear();
      return;
    }

    Int_t entries = t->GetEntries();
    FillHisto("LibrarySize", j, entries);
    lib_j.resize(entries);

    std::cout << user_normal() << "Loading pileup library " << fLibNames[j]
      << ", which has " << entries << " entries." << std::endl;

    t->SetBranchAddress("Spectrometer_NHits",  &lib_spectrometer.fNHits    , &(lib_spectrometer.fBranches[0]));
    t->SetBranchAddress("Spectrometer_Channel",&lib_spectrometer.fChannelID, &(lib_spectrometer.fBranches[1]));
    t->SetBranchAddress("Spectrometer_Time",   &lib_spectrometer.fTime     , &(lib_spectrometer.fBranches[2]));
    t->SetBranchAddress("Spectrometer_Width",  &lib_spectrometer.fOther1.f , &(lib_spectrometer.fBranches[3]));

    t->SetBranchAddress("NewCHOD_NHits",  &lib_newchod.fNHits    , &(lib_newchod.fBranches[0]));
    t->SetBranchAddress("NewCHOD_Channel",&lib_newchod.fChannelID, &(lib_newchod.fBranches[1]));
    t->SetBranchAddress("NewCHOD_Time",   &lib_newchod.fTime     , &(lib_newchod.fBranches[2]));

    t->SetBranchAddress("LAV_NHits",  &lib_lav.fNHits    , &(lib_lav.fBranches[0]));
    t->SetBranchAddress("LAV_Channel",&lib_lav.fChannelID, &(lib_lav.fBranches[1]));
    t->SetBranchAddress("LAV_Time",   &lib_lav.fTime     , &(lib_lav.fBranches[2]));
    t->SetBranchAddress("LAV_Edge",   &lib_lav.fOther1.i , &(lib_lav.fBranches[3]));

    t->SetBranchAddress("IRC_NHits",  &lib_irc.fNHits    , &(lib_irc.fBranches[0]));
    t->SetBranchAddress("IRC_Channel",&lib_irc.fChannelID, &(lib_irc.fBranches[1]));
    t->SetBranchAddress("IRC_Time",   &lib_irc.fTime     , &(lib_irc.fBranches[2]));
    t->SetBranchAddress("IRC_Edge",   &lib_irc.fOther1.i , &(lib_irc.fBranches[3]));

    t->SetBranchAddress("SAC_NHits",  &lib_sac.fNHits    , &(lib_sac.fBranches[0]));
    t->SetBranchAddress("SAC_Channel",&lib_sac.fChannelID, &(lib_sac.fBranches[1]));
    t->SetBranchAddress("SAC_Time",   &lib_sac.fTime     , &(lib_sac.fBranches[2]));
    t->SetBranchAddress("SAC_Edge",   &lib_sac.fOther1.i , &(lib_sac.fBranches[3]));

    t->SetBranchAddress("MUV3_NHits",  &lib_muv3.fNHits    , &(lib_muv3.fBranches[0]));
    t->SetBranchAddress("MUV3_Channel",&lib_muv3.fChannelID, &(lib_muv3.fBranches[1]));
    t->SetBranchAddress("MUV3_Time",   &lib_muv3.fTime     , &(lib_muv3.fBranches[2]));

    t->SetBranchAddress("LKr_NCandidates",  &lib_lkr.fNHits   , &(lib_lkr.fBranches[0]));
    t->SetBranchAddress("LKr_Energy",       &lib_lkr.fOther1.f, &(lib_lkr.fBranches[1]));
    t->SetBranchAddress("LKr_X",            &lib_lkr.fOther2.f, &(lib_lkr.fBranches[2]));
    t->SetBranchAddress("LKr_Y",            &lib_lkr.fOther3.f, &(lib_lkr.fBranches[3]));
    t->SetBranchAddress("LKr_Time",         &lib_lkr.fTime    , &(lib_lkr.fBranches[4]));

    for(Int_t i=0; i<entries; ++i){ // entries

      // get entry from Spectrometer branches only.
      detectors[0] = (LoadDetectorEntry(i, lib_spectrometer));

      // get entry from NewCHOD branches only
      detectors[1] = (LoadDetectorEntry(i, lib_newchod));

      // get entry from MUV3 branches only
      detectors[2] = (LoadDetectorEntry(i, lib_muv3));

      // get entry from LKr branches only
      detectors[3] = (LoadDetectorEntry(i, lib_lkr));

      // get entry from LAV  branches only
      detectors[4] = (LoadDetectorEntry(i, lib_lav));

      // get entry from IRC branches only
      detectors[5] = (LoadDetectorEntry(i, lib_irc));

      // get entry from SAC branches only
      detectors[6] = (LoadDetectorEntry(i, lib_sac));

      // fill event into library
      lib_j[i] = (detectors);
    }
    // fill library into list of libraries
    fHitLibrary[j] = (lib_j);
  }

  // close library file
  f->Close();
}

void DownstreamPileupGenerator::PostProcess(){
  if (!GetWithMC() && !fForcedOnData) return;
  if (!GetIsTree()) return;
  //manually clear events to avoid memory leaks with Fast MC
  fSpectrometerEvent->Clear();
  fNewCHODEvent->Clear();
  fMUV3Event->Clear();
  fLKrEvent->Clear();
  fLAVEvent->Clear();
}

void DownstreamPileupGenerator::EndOfBurstUser(){}

void DownstreamPileupGenerator::EndOfRunUser(){}

void DownstreamPileupGenerator::EndOfJobUser(){
  SaveAllPlots();
}

void DownstreamPileupGenerator::DrawPlot(){}

DownstreamPileupGenerator::~DownstreamPileupGenerator(){}

