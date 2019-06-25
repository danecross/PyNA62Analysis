#include "PnnKinematicTails_AdditionalTimeAlignment.hh"

#include "TRecoCedarEvent.hh"
#include "TRecoLKrEvent.hh"
#include "TRecoSpectrometerEvent.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "Stream.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

PnnKinematicTails_AdditionalTimeAlignment::PnnKinematicTails_AdditionalTimeAlignment(NA62Analysis::Core::BaseAnalysis *ba) :
  Analyzer(ba, "PnnKinematicTails_AdditionalTimeAlignment"), fWarnOnce(false) {
  RequestTree("Cedar", new TRecoCedarEvent, "Reco");
  RequestTree("GigaTracker",  new TRecoGigaTrackerEvent,  "Reco");
  RequestTree("LKr", new TRecoLKrEvent, "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  fRecoRevision  = "";
  fCedarOffset = 0.0;
  fLKrOffset = 0.0;
  fSpectrometerOffset = 0.0;
  fGTKOffset  = 0.0;

  fReadingData = true;
}

void PnnKinematicTails_AdditionalTimeAlignment::InitHist() {
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1D("hCedarOffset", "hCedarOffset", 200, -2., 2.));
    BookHisto(new TH1D("hGTKOffset", "hGTKOffset", 200, -2., 2.));
    BookHisto(new TH1D("hLKrOffset", "hLKrOffset", 200, -2., 2.));
    BookHisto(new TH1D("hSpectrometerOffset", "hSpectrometerOffset", 200, -4., 4.));

    BookHisto(new TH1D("hTimeDiffHitCedar", "hTimeDiffHitCedar", 500, -5., 5.));
    BookHisto(new TH1D("hTimeDiffCandidateCedar", "hTimeDiffCandidateCedar", 500, -5., 5.));
    BookHisto(new TH1D("hTimeDiffHitLKr", "hTimeDiffHitLKr", 500, -5., 5.));
    BookHisto(new TH1D("hTimeDiffCandidateLKr", "hTimeDiffCandidateLKr", 500, -5., 5.));
    BookHisto(new TH1D("hTimeDiffClusterLKr", "hTimeDiffClusterLKr", 500, -5., 5.));
    BookHisto(new TH1D("hTimeDiffCandidateSpectrometer", "hTimeDiffCandidateSpectrometer", 500, -5., 5.));
    BookHisto(new TH1D("hTimeDiffHitGTK", "hTimeDiffHitGTK", 500, -5., 5.));
  };
}

void PnnKinematicTails_AdditionalTimeAlignment::StartOfRunUser() {
  fRecoRevision = GetStreamInfo()->GetRecoInfo().GetRevision();

  fCedarOffset = +0.0; // [ns]
  fLKrOffset = 0.0; // [ns]
  fSpectrometerOffset = -2.1; // [ns] fine v1.0.3
  fGTKOffset = +0.0; // [ns]

  if(GetWithMC()){
    fSpectrometerOffset = +1.85; // [ns] v1.0.3
  };
}

void PnnKinematicTails_AdditionalTimeAlignment::Process(Int_t){
  if(!fReadingData) return;

  // Protection against multiple application of the corrections
  if(IsAnalyzerInHistory(GetAnalyzerName())){
    if(!fWarnOnce) cout<<user_normal()<<"Warning: My time alignment already applied"<<endl;
    fWarnOnce = true;
    return;
  };

  FillHisto("hCedarOffset", fCedarOffset);
  FillHisto("hGTKOffset", fGTKOffset);
  FillHisto("hLKrOffset", fLKrOffset);
  FillHisto("hSpectrometerOffset", fSpectrometerOffset);

  // Align the Cedar
  if(fabs(fCedarOffset)!=0.){ // [ns]
    TRecoCedarEvent* CedarEvent = GetEvent<TRecoCedarEvent>();
    for(Int_t iHit=0; iHit<CedarEvent->GetNHits(); iHit++){
      TRecoCedarHit* hit = static_cast<TRecoCedarHit*>(CedarEvent->GetHit(iHit));
      Double_t time = hit->GetTime() + fCedarOffset;
      FillHisto("hTimeDiffHitCedar", time - hit->GetTime());
      hit->SetTime(time);
    };
    for(Int_t iCand=0; iCand<CedarEvent->GetNCandidates(); iCand++){
      TRecoCedarCandidate* cand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCand));
      Double_t time = cand->GetTime() + fCedarOffset;
      FillHisto("hTimeDiffCandidateCedar", time - cand->GetTime());
      cand->SetTime(time);
    };
  };

  // Align the LKr
  if(fabs(fLKrOffset)!=0.){ // [ns]
    TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
    for(Int_t iHit=0; iHit<LKrEvent->GetNHits(); iHit++){
      TRecoLKrHit* hit = static_cast<TRecoLKrHit*>(LKrEvent->GetHit(iHit));
      Double_t time = hit->GetTime() + fLKrOffset;
      FillHisto("hTimeDiffHitLKr", time - hit->GetTime());
      hit->SetTime(time);
    };
    for(Int_t iCand=0; iCand<LKrEvent->GetNCandidates(); iCand++){
      TRecoLKrCandidate* cand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(iCand));
      Double_t time = cand->GetTime() + fLKrOffset;
      Double_t clustertime = cand->GetTime() + fLKrOffset;
      FillHisto("hTimeDiffCandidateLKr", time - cand->GetTime());
      cand->SetTime(time);
      FillHisto("hTimeDiffClusterLKr", clustertime - cand->GetTime());
      cand->SetTime(clustertime);
    };
  };

  // Align the Spectrometer
  if(fabs(fSpectrometerOffset)!=0.){ // [ns]
    TRecoSpectrometerEvent* StrawEvent = GetEvent<TRecoSpectrometerEvent>();
    for(Int_t iCand=0; iCand<StrawEvent->GetNCandidates(); iCand++){
      TRecoSpectrometerCandidate* cand = static_cast<TRecoSpectrometerCandidate*>(StrawEvent->GetCandidate(iCand));
      Double_t time = cand->GetTime() + fSpectrometerOffset;
      FillHisto("hTimeDiffCandidateSpectrometer", time - cand->GetTime());
      cand->SetTime(time);
    };
  };

  // Align the GTK
  if(fabs(fGTKOffset)!=0.){ // [ns]
    TRecoGigaTrackerEvent* GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
    for(Int_t iHit=0; iHit<GTKEvent->GetNHits(); iHit++){
      TRecoGigaTrackerHit* hit = static_cast<TRecoGigaTrackerHit*>(GTKEvent->GetHit(iHit));
      Double_t time = hit->GetTime() + fGTKOffset;
      FillHisto("hTimeDiffHitGTK", time - hit->GetTime());
      hit->SetTime(time);
    };
  };
}


void PnnKinematicTails_AdditionalTimeAlignment::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}
