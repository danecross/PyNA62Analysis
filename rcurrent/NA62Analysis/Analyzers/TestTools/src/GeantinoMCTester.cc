// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-04-08
//
// ---------------------------------------------------------------

#include "GeantinoMCTester.hh"

using namespace NA62Analysis;

/// \class GeantinoMCTester
/// \Brief
/// Plot endpoints of all geantinos from KineParts
/// \EndBrief

GeantinoMCTester::GeantinoMCTester(Core::BaseAnalysis *ba) : Analyzer(ba, "GeantinoMCTester") {}

void GeantinoMCTester::InitHist() {
  BookHisto("hXY", new TH2F
	    ("xy", "Geantino endpoint (x,y);x [mm];y [mm]",
	     2000, -1000, 1000, 2000, -1000, 1000));
}

void GeantinoMCTester::Process(Int_t) {

  if (!GetWithMC()) return;
  if (!GetIsTree()) return;

  Event *evt = GetMCEvent();
  for (int iK=0; iK<evt->GetNKineParts(); iK++) {
    if (abs(evt->GetKinePart(iK)->GetPDGcode())==0 && // geantino
	evt->GetKinePart(iK)->GetEndPos().Z()>299999) {
      FillHisto("hXY",
		evt->GetKinePart(iK)->GetEndPos().X(),
		evt->GetKinePart(iK)->GetEndPos().Y());
    }
  }
}

void GeantinoMCTester::EndOfJobUser() {
  SaveAllPlots();
}
