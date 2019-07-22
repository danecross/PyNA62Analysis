// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-04-30
//
// ---------------------------------------------------------------

#include "MUV3MCTester.hh"
#include "TMUV3Hit.hh"
#include "GeometricAcceptance.hh"
#include "TMUV3Event.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

/// \class MUV3MCTester
/// \Brief
/// A test tool to check MUV3 simulation in NA62MC
/// \EndBrief

MUV3MCTester::MUV3MCTester(Core::BaseAnalysis *ba) : Analyzer(ba, "MUV3MCTester") {
  RequestTree("MUV3", new TMUV3Event);
  fZMUV3 = GeometricAcceptance::GetInstance()->GetZMUV3();
}

void MUV3MCTester::InitHist() {
  BookHisto(new TH1F("NHits",
		     "Number of MUV3 hits per event;Number of hits",
		     30, -0.5, 29.5));
  BookHisto(new TH1F("NMuonHits",
		     "Number of MUV3 muon hits per event;Number of hits",
		     30, -0.5, 29.5));
  BookHisto(new TH1F("ChannelID",
		     "Hit channel ID: normal hits;Geometric channel ID",
		     360, -0.5, 359.5));
  BookHisto(new TH1F("ChannelIDCherenkov",
		     "Hit channel ID: Cherenkov hits;Geometric channel ID",
		     360, -0.5, 359.5));
  BookHisto(new TH1F("EnergyDeposit",
		     "Energy deposit in a counter: expect a MIP peak at 10 MeV;Energy deposit [MeV]",
		     100, 0, 50));
  BookHisto(new TH1F("EnergyDepositMuonHit",
		     "Energy deposit from muon hits in a counter;Energy deposit [MeV]",
		     100, 0, 50));
  BookHisto(new TH2F("EnergyDepositVsTime",
		     "Energy deposit vs time;Hit time [ns];Energy deposit [MeV]",
		     50, 810, 860, 100, 0, 100));
  BookHisto(new TH1F("Zhit",
		     "Hit Z position (i.e. the position of the first energy deposit);Hit Z coordinate [m]",
		     75, 246.75, 246.90));
  BookHisto(new TH1F("ZhitMuonHit",
		     "Hit Z position for muon hits;Hit Z coordinate [m]",
		     75, 246.75, 246.90));
  BookHisto(new TH2F("EnergyDepositVsZhit",
		     "Energy deposit vs Z position;Hit Z coordinate [m]",
		     100, 246.75, 246.90, 100, 0, 100));
  BookHisto(new TH2F("XYhit",
		     "Hit XY positions;Hit X coordinate [m];Hit Y coordinate [m]",
		     150, -1.5, 1.5, 150, -1.5, 1.5));
  BookHisto(new TH2F("XYhitMuonHit",
		     "Hit XY positions for muon hits;Hit X coordinate [m];Hit Y coordinate [m]",
		     150, -1.5, 1.5, 150, -1.5, 1.5));
  BookHisto(new TH1F("DistanceToMuonX",
		     "X distance: MUV3 hit to extrapolated true muon;Delta(x) [mm]",
		     100, -250, 250));
  BookHisto(new TH1F("DistanceToMuonY",
		     "Y distance: MUV3 hit to extrapolated true muon;Delta(y) [mm]",
		     100, -250, 250));
  BookHisto(new TH1F("DistanceToMuon",
		     "Distance: MUV3 hit to extrapolated true muon;Distance [mm]",
		     100, 0, 500));
}

void MUV3MCTester::Process(Int_t) {
  if (!GetWithMC()) return;
  if (!GetIsTree()) return;

  TMUV3Event *MUV3Event = GetEvent<TMUV3Event>();
  Int_t NHits = MUV3Event->GetNHits();
  Int_t NMuonHits = 0;
  for (Int_t ihit=0; ihit<NHits; ihit++) {
    TMUV3Hit *Hit = static_cast<TMUV3Hit*>(MUV3Event->GetHits()->At(ihit));
    if (Hit->IsMuonHit()) NMuonHits++;
  }

  FillHisto("NHits", NHits);
  FillHisto("NMuonHits", NMuonHits);

  ////////////////////////////////////////
  // Loop over hits, fill a few histograms

  for (Int_t ihit=0; ihit<NHits; ihit++) {
    TMUV3Hit *Hit    = static_cast<TMUV3Hit*>(MUV3Event->GetHits()->At(ihit));
    Int_t   id       = Hit->GetChannelID();
    Double_t x       = Hit->GetPosition().X();
    Double_t y       = Hit->GetPosition().Y();
    Double_t z       = Hit->GetPosition().Z();
    Double_t Energy  = Hit->GetEnergy();
    Double_t Time    = Hit->GetTime();
    Bool_t   MuonHit = Hit->IsMuonHit();

    if (Energy>0) { // Scintillator (not PMT window) hits

      FillHisto("ChannelID", id);
      if (id>=200) continue; // hits are duplicated for the PMTs

      FillHisto("EnergyDeposit", Energy);
      if (MuonHit) FillHisto("EnergyDepositMuonHit", Energy);
      FillHisto("EnergyDepositVsTime", Time, Energy);
      FillHisto("EnergyDepositVsZhit", 0.001*z, Energy);
      FillHisto("XYhit", 0.001*x, 0.001*y); // [m]
      FillHisto("Zhit", 0.001*z); // [m]
      if (MuonHit) {
	FillHisto("XYhitMuonHit", 0.001*x, 0.001*y); // [m]
	FillHisto("ZhitMuonHit", 0.001*z); // [m]
      }
    }

    if (Energy<0) { // "Cherenkov" hits directly on the PMT window
      FillHisto("ChannelIDCherenkov", id);
    }
  }

  /////////////////////////////////////////////////////////
  // Look for a single generated muon for muon-hit matching

  Event *evt = GetMCEvent();
  Int_t Npart = evt->GetNKineParts();
  Int_t NMuons = 0, imuon = -999;
  for (Int_t ipart=0; ipart<Npart; ipart++) {
    KinePart *Particle = static_cast<KinePart*>(evt->GetKineParts()->At(ipart));
    Int_t code = Particle->GetPDGcode();
    if (abs(code)==13) { // muon
      NMuons++;
      imuon = ipart;
    }
  }

  ////////////////////////////////////
  // Evaluate the muon to hit distance

  if (NMuons==1 && NHits==2) {
    KinePart *Muon = static_cast<KinePart*>(evt->GetKineParts()->At(imuon));
    Double_t xmu = Muon->xAt(fZMUV3);
    Double_t ymu = Muon->yAt(fZMUV3);

    TMUV3Hit *Hit = static_cast<TMUV3Hit*>(MUV3Event->GetHits()->At(0));
    Double_t xhit = Hit->GetPosition().X();
    Double_t yhit = Hit->GetPosition().Y();
    Double_t dist = sqrt((xhit-xmu)*(xhit-xmu) + (yhit-ymu)*(yhit-ymu));
    FillHisto("DistanceToMuonX", xhit-xmu);
    FillHisto("DistanceToMuonY", yhit-ymu);
    FillHisto("DistanceToMuon", dist);
  }
}

void MUV3MCTester::EndOfJobUser() {
  SaveAllPlots();
}
