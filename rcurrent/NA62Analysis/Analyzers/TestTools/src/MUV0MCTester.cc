// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-05-11
//
// ---------------------------------------------------------------

#include "MUV0MCTester.hh"
#include "TMUV0Hit.hh"
#include "TMUV0Event.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

/// \class MUV0MCTester
/// \Brief
/// A test tool to check MUV0 simulation in NA62MC
/// \EndBrief

MUV0MCTester::MUV0MCTester(Core::BaseAnalysis *ba) : Analyzer(ba, "MUV0MCTester") {
  RequestTree("MUV0", new TMUV0Event);
}

void MUV0MCTester::InitHist() {
  BookHisto(new TH1F("NHits", "Number of MUV0 hits per event;Number of hits",
		     10, -0.5, 9.5));
  BookHisto(new TH1F("ChannelID", "Hit channel ID;Geometric channel ID",
		     9, 0.5, 9.5));
  BookHisto(new TH1F("EnergyDeposit",
		     "Energy deposit in a counter: expect a MIP peak at 4 MeV;Energy deposit [MeV]",
		     100, 0, 25));
  BookHisto(new TH1F("HitTime", "Hit time [ns];", 100, 0, 1000));
  BookHisto(new TH2F("XYhit", "Hit XY positions;Hit X coordinate [m];Hit Y coordinate [m]",
		     75, 1.50, 3.00, 75, -0.75, 0.75));
  BookHisto(new TH2F("XVsTileID", "Tile ID;Hit X coordinate [m]",
		     9, 0.5, 9.5, 75, 1.50, 3.00));
  BookHisto(new TH2F("YVsTileID", "Tile ID;Hit Y coordinate [m]",
		     9, 0.5, 9.5, 75, -0.75, 0.75));
}

void MUV0MCTester::Process(int) {
  if (!GetWithMC()) return;
  if (!GetIsTree()) return;

  TMUV0Event *MUV0Event = GetEvent<TMUV0Event>();
  Int_t NHits = MUV0Event->GetNHits();
  FillHisto("NHits", NHits);

  ////////////////////////////////////////
  // Loop over hits, fill a few histograms

  for (Int_t ihit=0; ihit<NHits; ihit++) {
    TMUV0Hit *Hit   = static_cast<TMUV0Hit*>(MUV0Event->GetHits()->At(ihit));
    Int_t    id     = Hit->GetChannelID();
    Double_t x      = Hit->GetPosition().X();
    Double_t y      = Hit->GetPosition().Y();
    Double_t Energy = Hit->GetEnergy();
    Double_t Time   = Hit->GetTime();

    FillHisto("ChannelID", id);
    FillHisto("EnergyDeposit", Energy);   // [MeV]
    FillHisto("HitTime", Time);           // [ns]
    FillHisto("XYhit", 0.001*x, 0.001*y); // [m]
    FillHisto("XVsTileID", id, 0.001*x);  // [m]
    FillHisto("YVsTileID", id, 0.001*y);  // [m]
  }
}

void MUV0MCTester::EndOfJobUser() {
  SaveAllPlots();
}
