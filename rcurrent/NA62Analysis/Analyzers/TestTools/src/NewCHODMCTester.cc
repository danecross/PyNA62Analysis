// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-16
//
// ---------------------------------------------------------------

#include "NewCHODMCTester.hh"
#include "TNewCHODHit.hh"
#include "GeometricAcceptance.hh"
#include "TNewCHODEvent.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

/// \class NewCHODMCTester
/// \Brief
/// A test tool to check NewCHOD simulation in NA62MC
/// \EndBrief

NewCHODMCTester::NewCHODMCTester(Core::BaseAnalysis *ba) : Analyzer(ba, "NewCHODMCTester") {
  RequestTree("NewCHOD", new TNewCHODEvent);
  fZNewCHOD = GeometricAcceptance::GetInstance()->GetZNewCHOD();
}

void NewCHODMCTester::InitHist() {
  BookHisto(new TH1F("NHits",
		     "Number of NewCHOD hits per event;Number of hits",
		     30, -0.5, 29.5));
  BookHisto(new TH1F("ChannelID",
		     "Hit channel ID: normal hits;Geometric channel ID",
		     350, 100.5, 450.5));
  BookHisto(new TH1F("EnergyDeposit",
		     "Energy deposit in a counter: expect a MIP peak at 10 MeV;Energy deposit [MeV]",
		     100, 0, 50));
  BookHisto(new TH1F("Zhit",
		     "Hit Z position (i.e. the position of the first energy deposit);Hit Z coordinate [m]",
		     150, 238.05, 238.20));
  BookHisto(new TH2F("XYhit",
		     "Hit XY positions;Hit X coordinate [m];Hit Y coordinate [m]",
		     150, -1.5, 1.5, 150, -1.5, 1.5));
  BookHisto(new TH1F("DistanceToMuonX",
		     "X distance: NewCHOD hit to extrapolated true muon;Delta(x) [mm]",
		     100, -50, 50));
  BookHisto(new TH1F("DistanceToMuonY",
		     "Y distance: NewCHOD hit to extrapolated true muon;Delta(y) [mm]",
		     100, -50, 50));
  BookHisto(new TH1F("DistanceToMuon",
		     "Distance: NewCHOD hit to extrapolated true muon;Distance [mm]",
		     100, 0, 100));
}

void NewCHODMCTester::Process(Int_t) {
  if (!GetWithMC()) return;
  if (!GetIsTree()) return;

  TNewCHODEvent *NewCHODEvent = GetEvent<TNewCHODEvent>();
  Int_t NHits = NewCHODEvent->GetNHits();
  FillHisto("NHits", NHits);

  ////////////////////////////////////////
  // Loop over hits, fill a few histograms

  for (Int_t ihit=0; ihit<NHits; ihit++) {
    TNewCHODHit *Hit = static_cast<TNewCHODHit*>(NewCHODEvent->GetHits()->At(ihit));
    Int_t   id      = Hit->GetChannelID();
    Double_t x      = Hit->GetPosition().X();
    Double_t y      = Hit->GetPosition().Y();
    Double_t z      = Hit->GetPosition().Z();
    Double_t Energy = Hit->GetEnergy();

    FillHisto("ChannelID", id);
    FillHisto("EnergyDeposit", Energy);
    FillHisto("XYhit", 0.001*x, 0.001*y); // [m]
    FillHisto("Zhit", 0.001*z); // [m]
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

  if (NMuons==1 && NHits==1) {
    KinePart *Muon = static_cast<KinePart*>(evt->GetKineParts()->At(imuon));
    Double_t xmu = Muon->xAt(fZNewCHOD);
    Double_t ymu = Muon->yAt(fZNewCHOD);

    TNewCHODHit *Hit = static_cast<TNewCHODHit*>(NewCHODEvent->GetHits()->At(0));
    Double_t xhit = Hit->GetPosition().X();
    Double_t yhit = Hit->GetPosition().Y();
    Double_t dist = sqrt((xhit-xmu)*(xhit-xmu) + (yhit-ymu)*(yhit-ymu));
    FillHisto("DistanceToMuonX", xhit-xmu);
    FillHisto("DistanceToMuonY", yhit-ymu);
    FillHisto("DistanceToMuon", dist);
  }
}

void NewCHODMCTester::EndOfJobUser() {
  SaveAllPlots();
}
