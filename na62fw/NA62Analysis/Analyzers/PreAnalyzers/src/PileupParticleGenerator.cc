// ---------------------------------------------------------------
// History:
//
// Handling of +ve and -ve halo particles added by Chris Parkinson, 2019-07-16
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2019-02-19
// ---------------------------------------------------------------
/// \class PileupParticleGenerator
/// \Brief
/// Generation of pileup particles based on beam intensity
/// \EndBrief
/// \Detailed
/// The beam intensity from BeamIntensityGenerator is used to create a vector
/// of beam particles and a vector of halo particles. The beam particles are proton,
/// kaon, and pion. These beam particles are used to correlate the
/// UpstreamPileupGenerator and the DownstreamPileupGenerator.
///  
/// The beam intensity can be increased using the "BeamIntensityFudgeFactor" parameter.
/// The rate of halo muons can be increased using the "HaloFudgeFactor" parameter.
/// The code can be forced to run on data using the "ForcedOnData" parameter.
///
/// A single particle of a given ID can be added at t=0 using the "InTimeParticleID" parameter.
/// Valid particle ID values are:
/// 100  -- kaon, decaying according to normal rules;<br>
/// 101  -- kaon, forced Km2 decay inside FV;<br>
/// 102  -- kaon, forced K2pi decay inside FV;<br>
/// 103  -- kaon, forced K3pi decay inside FV;<br>
/// 104  -- kaon, forced Ke3 decay inside FV;<br>
/// 105  -- kaon, forced Km3 decay inside FV;<br>
/// 106  -- kaon, forced K3pi0 decay inside FV;<br>
/// 107  -- kaon, forced Km2 decay in extended decay region (180-265m);<br>
/// 108  -- kaon, forced K2pi decay in extended decay region (180-265m);<br>
/// 109  -- kaon, forced K3pi decay in extended decay region (180-265m);<br>
/// 200  -- pion, decaying according to normal rules;<br>
/// 201  -- pion, forced Pim2 decay in full region (102.425-265m);<br>
/// 300  -- proton, decaying according to normal rules;<br>
/// 400  -- halo muon, from any source.;<br>
/// 401  -- halo muon, from a positive particle;<br>
/// 402  -- halo muon, from a negative particle;<br>
/// 1000 -- control triggered data event (downstream only)
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include "PileupParticleGenerator.hh"
#include "NA62ConditionsService.hh"
#include "EventHeader.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

PileupParticleGenerator::PileupParticleGenerator(Core::BaseAnalysis *ba) :
  Analyzer(ba, "PileupParticleGenerator") {

  // --> 150.0 means +-75 ns (same window used as standard to compute MUV3 pileup)
  AddParam("TimeWindowWidth", &fTimeWindowWidth, 150.0); // [ns]

  // fractions of kaons and pions
  fKaonToTotalRatioAtGTK3 = 0.06000; // Kaon fraction in the beam (at GTK3)
  fPionToTotalRatioAtGTK3 = 0.70208; // Pion fraction in the beam (at GTK3)
  // Above pion fraction gives 526560 pions per ms, C.F. 526561 in the beam pi+ MC sample.

  AddParam("BeamIntensityFudgeFactor", &fBeamIntensityFudgeFactor, 1.0);
  AddParam("HaloFudgeFactor", &fHaloFudgeFactor, 1.0);
  AddParam("ForcedOnData", &fForcedOnData, false);
  AddParam("InTimeParticleID", &fInTimeParticleID, -1);

  fRandom = new TRandom2();
  fBeamParticles.clear();
  fHaloParticles.clear();
}

PileupParticleGenerator::~PileupParticleGenerator() {
  if (fRandom) delete fRandom;
}

void PileupParticleGenerator::InitOutput() {
  RegisterOutput("BeamParticles", &fBeamParticles);
  RegisterOutput("HaloParticles", &fHaloParticles);
  RegisterOutput("NumberOfTracksGenerated", &fNTracksGenerated);
}

void PileupParticleGenerator::InitHist() {
  if (!GetWithMC() && !fForcedOnData) return;
  if (!GetIsTree()) return;
  BookHisto(new TH1F("Gen_Time", "Gen_Time", 200, -0.5*fTimeWindowWidth, 0.5*fTimeWindowWidth));
  BookHisto(new TH1F("Gen_Kaon", "Gen_Kaon", 41, -0.5, 40.5));
  BookHisto(new TH1F("Gen_Pion", "Gen_Pion", 101, -0.5, 201.5));
  BookHisto(new TH1F("Gen_Proton", "Gen_Proton", 41, -0.5, 40.5));
  BookHisto(new TH1F("Gen_Halo", "Gen_Halo", 41, -0.5, 40.5));
  BookHisto(new TH1F("Gen_Total", "Gen_Total", 101, -0.5, 201.5));
}

void PileupParticleGenerator::StartOfBurstUser() {
  if (!GetIsTree()) return;
  if (!GetEventHeader()) return;
  fRandom->SetSeed(GetEventHeader()->GetBurstID()); // to ensure reproducibility
}

void PileupParticleGenerator::Process(Int_t) {
  if (!GetWithMC() && !fForcedOnData) return;
  if (!GetIsTree()) return;
  if (!GetEventHeader()) return; // do not run on NA62MC output
  
  //////////////////////////////////////////////////////////////////////////
  // Overlay a control trigger event. For now no activity in the upstream,
  // so using the Halo container.
  if(fInTimeParticleID==1000){
    Double_t time = GetEventHeader()->GetFineTime()*TdcCalib;    
    fNTracksGenerated = 1;
    fBeamParticles.resize(0);
    fHaloParticles.resize(fNTracksGenerated);
    fHaloParticles[0] = std::pair<Double_t, Int_t>(time, fInTimeParticleID);
  }

  //////////////////////////////////////////////////////////////////////////
  // Generate one particle in time with the MC event, with given particle ID
  // Generate a (positive, negative) halo muon if ID is 400 (401, 402).
  if(fInTimeParticleID==400 || fInTimeParticleID==401 || fInTimeParticleID==402){
    Double_t time = GetEventHeader()->GetFineTime()*TdcCalib;    
    fNTracksGenerated = 1;
    fBeamParticles.resize(0);
    fHaloParticles.resize(fNTracksGenerated);
    fHaloParticles[0] = std::pair<Double_t, Int_t>(time, fInTimeParticleID);
    return;
  }

  //////////////////////////////////////////////////////////////////////////
  // Generate one particle in time with the MC event, with given particle ID
  if(fInTimeParticleID>=0){
    Double_t time = GetEventHeader()->GetFineTime()*TdcCalib;
    fNTracksGenerated = 1;
    fHaloParticles.resize(0);
    fBeamParticles.resize(fNTracksGenerated);
    fBeamParticles[0] = std::pair<Double_t, Int_t>(time, fInTimeParticleID);
    return;
  }

  ///////////////////////////////////////////////////////////
  // Generate the instantaneous beam intensity for this event

  Double_t beamIntensity = -1.0;
  beamIntensity = *(Double_t*)GetOutput("BeamIntensityGenerator.BeamIntensity");

  ///////////////////////////////////////////////////////////
  // Artificially increase/decrease the beam intensity
  beamIntensity *= fBeamIntensityFudgeFactor;

  ////////////////////////////////////////
  // Generate beam particles according to the intensity

  // get number of beam particles to generate
  Double_t lambda = 0.001*beamIntensity*fTimeWindowWidth;
  fNTracksGenerated = fRandom->Poisson(lambda);
  fBeamParticles.resize(fNTracksGenerated);

  Int_t type[4]={0};

  // for each beam particle, determine time, particle ID, and add to output containers
  for (Int_t i=0; i<fNTracksGenerated; i++) {
    // get time of beam particle
    Double_t time = fRandom->Uniform(fTimeWindowWidth) - 0.5*fTimeWindowWidth;

    // get ID of beam particle. default ID is a proton, otherwise it is kaon or pion.
    Int_t particleID=2; // proton
    Double_t RandomValue = fRandom->Uniform();
    if(RandomValue<fKaonToTotalRatioAtGTK3){
      particleID=0; // kaon
    }
    else if(RandomValue<(fKaonToTotalRatioAtGTK3+fPionToTotalRatioAtGTK3)){
      particleID=1; // pion
    }

    // add beam particle to beam particles container
    fBeamParticles[i] = std::pair<Double_t, Int_t>(time, particleID);

    // store particle generation type and time
    type[particleID]++;
    FillHisto("Gen_Time", time);
  }

  ////////////////////////////////////////
  // Generate halo particles relative to Kaon decays (for now) ...

  // get number of halo particles to generate
  // halo rate is 135.342MHz at nominal beam intensity.
  lambda  = (beamIntensity/750.0) * (fTimeWindowWidth/1000.0) * 135.342;
  lambda *= fHaloFudgeFactor;
  fNTracksGenerated = fRandom->Poisson(lambda);
  fHaloParticles.resize(fNTracksGenerated);

  // beam halo particle ID is always 3
  Int_t particleID=3;

  // for each halo particle, determine time and add to output containers
  for (Int_t i=0; i<fNTracksGenerated; i++) {
    // get time of halo particle
    Double_t time = fRandom->Uniform(fTimeWindowWidth) - 0.5*fTimeWindowWidth;

    // add halo particle to halo particles container
    fHaloParticles[i] = std::pair<Double_t, Int_t>(time, particleID);

    // store particle generation type and time
    type[particleID]++;
    FillHisto("Gen_Time", time);
  }

  // record particle generation type
  FillHisto("Gen_Kaon",   type[0]);
  FillHisto("Gen_Pion",   type[1]);
  FillHisto("Gen_Proton", type[2]);
  FillHisto("Gen_Halo",   type[3]);

  fNTracksGenerated = type[0]+type[1]+type[2]+type[3];
  FillHisto("Gen_Total", fNTracksGenerated);
}

void PileupParticleGenerator::EndOfJobUser() {
  if (!GetWithMC() && !fForcedOnData) return;
  if (!GetIsTree()) return;
  SaveAllPlots();
}
