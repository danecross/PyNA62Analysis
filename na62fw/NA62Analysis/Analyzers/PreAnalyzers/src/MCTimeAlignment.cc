// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2018-03-02
// ---------------------------------------------------------------

/// \class MCTimeAlignment
/// \Brief
/// Fine time alignment for MC
/// \EndBrief
/// \Detailed
/// Hard-coded fine time offsets depending on the MC version
/// are applied to Cedar and RICH hits and candidates
/// to make up for the imprefections in earlier versions of NA62MC.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "MCTimeAlignment.hh"
#include "TRecoCedarEvent.hh"
#include "TRecoRICHEvent.hh"
#include "Stream.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

MCTimeAlignment::MCTimeAlignment(NA62Analysis::Core::BaseAnalysis *ba) :
  Analyzer(ba, "MCTimeAlignment"), fWarnOnce(false) {
  RequestTree("Cedar", new TRecoCedarEvent, "Reco");
  RequestTree("RICH",  new TRecoRICHEvent,  "Reco");
  fMCRevision  = "";
  fCedarOffset = 0.0;
  fRICHOffset  = 0.0;
}

void MCTimeAlignment::StartOfRunUser() {
  fCedarOffset = 0.0;
  fRICHOffset  = 0.0;
  fMCRevision = GetStreamInfo()->GetMCInfo().GetRevision();
  if (fMCRevision=="v0.11.1") {
    fCedarOffset = -0.04; // [ns]
    fRICHOffset  = +0.34; // [ns]
  }
  else if (fMCRevision=="v0.11.3" || fMCRevision=="v1.0.0" || fMCRevision=="v1.0.1") {
    fCedarOffset = +0.06; // [ns]
  }
}

void MCTimeAlignment::Process(Int_t) {
  if (!GetWithMC()) return;
  if (!GetIsTree()) return;

  // Protection against multiple application of the corrections
  if (IsAnalyzerInHistory(GetAnalyzerName())) {
    if (!fWarnOnce) cout << user_normal() << "Warning: fine MC timing correction already applied" << endl;
    fWarnOnce = true;
    return;
  }

  // Align the Cedar
  if (fabs(fCedarOffset)>0.001) { // [ns]
    TRecoCedarEvent* CedarEvent = GetEvent<TRecoCedarEvent>();
    for (Int_t iHit=0; iHit<CedarEvent->GetNHits(); iHit++) {
      TRecoCedarHit* hit = static_cast<TRecoCedarHit*>(CedarEvent->GetHit(iHit));
      Double_t time = hit->GetTime() + fCedarOffset;
      hit->SetTime(time);
    }
    for (Int_t iCand=0; iCand<CedarEvent->GetNCandidates(); iCand++) {
      TRecoCedarCandidate* cand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCand));
      Double_t time = cand->GetTime() + fCedarOffset;
      cand->SetTime(time);
    }
  }

  // Align the RICH
  if (fabs(fRICHOffset)>0.001) { // [ns]
    TRecoRICHEvent* RICHEvent = GetEvent<TRecoRICHEvent>();
    for (Int_t iHit=0; iHit<RICHEvent->GetNHits(); iHit++) {
      TRecoRICHHit* hit = static_cast<TRecoRICHHit*>(RICHEvent->GetHit(iHit));
      Double_t time = hit->GetTime() + fRICHOffset;
      hit->SetTime(time);
    }
    for (Int_t iCand=0; iCand<RICHEvent->GetNCandidates(); iCand++) {
      TRecoRICHCandidate* cand = static_cast<TRecoRICHCandidate*>(RICHEvent->GetCandidate(iCand));
      Double_t time = cand->GetTime() + fRICHOffset;
      cand->SetTime(time);
    }
  }
}
