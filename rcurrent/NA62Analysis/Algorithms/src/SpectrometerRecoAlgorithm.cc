/// \class SpectrometerRecoAlgorithm
/// \Brief
/// Spectrometer reconstruction at the NA62Analysis stage
/// \EndBrief
/// \Detailed
/// Rebuilds Spectrometer Tracker candidates (i.e. the Spectrometer tracks).
/// This algorithm can be called from an Analyzer,
/// optionally passing a new reference time for track recontruction.
/// The algorithm is created in the constructor of user's analyser as follows:
/// \code
/// fSpecRecoAlgo = new SpectrometerRecoAlgorithm(ba, this, "SpecRecoAlgo");
/// \endcode
/// Most importantly a different config file can be used to set
/// Spectrometer reconstruction parameters to custom values (e.g. exotics configuration)
/// \code
/// fSpecRecoAlgo->SetConfFileName("PathToSpectrometer.conf");
/// \endcode
/// It could be run in the Process() method of user's analyser as follows:
/// \code
/// TRecoSpectrometerEvent* Event = GetEvent<TRecoSpectrometerEvent>();
/// fSpecRecoAlgo->Process(Event, RefTime);
/// \endcode
/// Alpha/beta corrections are not applied by default for the reconstructed tracks, but
/// they can be enabled by user, their values are read from SpectrometerTrackCorrections
/// analyzer output:
/// \code
/// fSpecRecoAlgo->SetApplyAlphaBetaCorrections(false);
/// \endcode
/// In this file, there are custom versions of classes derived from the main
/// SpectrometerReconstruction classes, namely ChamberHitCollector, ViewHitCollector and
/// TrackCollector. At the moment, they do not contain an alternative version
/// of the reconstruction algorithm, they simply call the original algorithm.
/// However, they provide a framework for testing different algorithms.
/// The usage of custom classes is disabled default, and can be switched on by user:
/// \code
/// fSpecRecoAlgo->SetUseCustomViewCollector(true);
/// fSpecRecoAlgo->SetUseCustomChamberCollector(true);
/// fSpecRecoAlgo->SetUseCustomTrackCollector(true);
/// \endcode
/// \author Michal Koval (michal.koval@cern.ch)
/// \EndDetailed

#include "SpectrometerRecoAlgorithm.hh"
#include "TSystem.h"
#include "NA62ConditionsService.hh"

#include <memory>

using namespace std;

SpectrometerRecoAlgorithm::SpectrometerRecoAlgorithm
(BaseAnalysis *ba, Analyzer* ana, const string &name) : Algorithm(ba, ana, name) {
  // set default Straw config file path
  TString configName=gSystem->Getenv("NA62RECOSOURCE");
  configName.Append("/config/Spectrometer.conf");
  fConfFileName = configName;
  // pointers to Straw parameters and geometry instances
  fPar = SpectrometerParameters::GetInstance();
  fGeo = SpectrometerGeometry::GetInstance();
  fIsInitialized = false;
  // run and burst id, input=MC?
  fRunID = -1;
  fBurstID = -1;
  fIsMC = false;
  // reconstruction algorithm flags
  fUpdateWireDistance = false;
  fUseCustomChamberCollector = false;
  fUseCustomViewCollector = false;
  fUseCustomTrackCollector = false;
  fApplyAlphaBetaCorrections = false;
  // debug printout flag
  fDebugMode = false;
  // copy of current event candidates (used in debug mode)
  fRecoCandidatesCopy = nullptr;
}

void SpectrometerRecoAlgorithm::Init()
{
  // parse spectrometer config file
  fPar->ParseConfFile(fConfFileName);
  fPar->SetIsRawData(!fIsMC);
  fPar->SetT0FileName("Spectrometer-T0.dat");
  fPar->SetT0(0);
  fPar->SetRT(); // Read RT parameters
  fPar->SetXT(); // Read XT parameters
  fPar->SetAlignment(); // Read alignment offsets
  fPar->SetZPositionCorrection();
  fGeo->CreateDataGeometry();
  // instantiate chamber hit collectors
  if (fUseCustomChamberCollector) {
    for (Int_t iCh = 0; iCh < fPar->GetNChambers(); iCh++)
      fChamberHitCollector.push_back(new ChamberHitCollectorCustom(iCh, fUseCustomViewCollector));
  } else {
    for (Int_t iCh = 0; iCh < fPar->GetNChambers(); iCh++)
      fChamberHitCollector.push_back(new ChamberHitCollector(iCh));
  }
  for (Int_t iCh = 0; iCh < fPar->GetNChambers(); iCh++)
    fChamberHitCollector[iCh]->Init();

  // instantiate track collector
  if (fUseCustomTrackCollector) {
    fTrackCollector = make_unique<TrackCollectorCustom>(&fChamberHitCollector);
  } else {
    fTrackCollector = make_unique<TrackCollector>(&fChamberHitCollector);
  }
  fIsInitialized = true;
}

SpectrometerRecoAlgorithm::~SpectrometerRecoAlgorithm()
{
  for (ChamberHitCollector* c : fChamberHitCollector) delete c;
}

void SpectrometerRecoAlgorithm::Process(TRecoSpectrometerEvent *event, Double_t refTime)
{
  if (!GetIsTree()) return;
  if (!GetEventHeader()) return; // do not run on NA62MC (i.e. not NA62Reco) output

  if (fRunID != GetRunID() || fBurstID != GetBurstID() || fIsMC != GetWithMC() ) {
    fRunID = GetRunID();
    fBurstID = GetBurstID();
    fIsMC = GetWithMC();
  }
  if (!fIsInitialized) Init();

  Double_t triggerTime = GetEventHeader()->GetFineTime() * TdcCalib;
  // set default reference time for the event to the trigger time
  if (refTime == -999.) refTime = triggerTime;
  Double_t originalRefTime = triggerTime; // trigger time is used in the NA62Reconstruction

  // copy SpectrometerReconstruction candidates in debugging mode
  if (fDebugMode) fRecoCandidatesCopy = (TClonesArray *)event->GetCandidates()->Clone();

  // remove all existing reconstructed candidates (keep RecoHits)
  Int_t nCand = event->GetNCandidates();
  for (Int_t iC = 0; iC < nCand; iC++) event->RemoveCandidate(0);

  // Adjust time of all reco hits, possible to adjust reconstructed radius as well
  if ( (refTime != originalRefTime) || fUpdateWireDistance) {
    for (int ihit = 0; ihit < event->GetNHits(); ihit++) {
      auto hit = dynamic_cast<TRecoSpectrometerHit*>(event->GetHit(ihit));
      double time = hit->GetDriftTime();
      double newTime = time + originalRefTime - refTime; // take new reference instead of old one
      hit->SetDriftTime(newTime);
      if (fUpdateWireDistance) {
        double wireDistance = 0.;
        if (fPar->GetIsRawData()) {
          wireDistance = fPar->GetRTDependenceData(newTime/1000.);
        } else {
          wireDistance = fPar->GetRTParametricDependence(newTime/1000.);
        }
        if (wireDistance<0)   wireDistance = 0;   // Inner boundary condition
        if (wireDistance>4.9) wireDistance = 4.9; // Outer boundary condition
        hit->SetWireDistance(wireDistance);
      }
    }
  }

  // Reset hit collectors
  for (Int_t iChamb=0; iChamb < fPar->GetNChambers(); iChamb++) {
    for (Int_t iView=0; iView < fPar->GetNViews(); iView++) {
      for (Int_t iPlane=0; iPlane < fPar->GetNViews(); iPlane++)
        fChamberHitCollector[iChamb]->GetView(iView)->GetPlane(iPlane)->Reset();
      fChamberHitCollector[iChamb]->GetView(iView)->Reset();
    }
    fChamberHitCollector[iChamb]->Reset();
  }
  // Reset track collector
  fTrackCollector->Reset();

  // Sort hits per view
  Bool_t isGoodEvent = SeparateHitPerViewPlane(event);
  if (!isGoodEvent) return;
  // Resolve L-R ambiguity + reconstruct hit positions in all views
  for (Int_t jChamber = 0; jChamber < fPar->GetNChambers(); jChamber++) {
    for (Int_t jView = 0; jView < fPar->GetNViews(); jView++) {
      ViewHitCollector *v = fChamberHitCollector[jChamber]->GetView(jView);
      if (fUseCustomChamberCollector && fUseCustomViewCollector) {
        auto *p = dynamic_cast<ViewHitCollectorCustom*>(v);
        p->ReconstructHitPositions(event);
      } else {
        v->ReconstructHitPositions(event);
      }
    }
  }
  // Reconstruct hits per chamber
  for (ChamberHitCollector *c : fChamberHitCollector) {
    if (fUseCustomChamberCollector) { // use custom algorithm if requested
      auto *p = dynamic_cast<ChamberHitCollectorCustom*>(c);
      p->ReconstructHit();
    } else {
      c->ReconstructHit();
    }
  }

  // Reconstruct tracks from chamber hits
  if (fUseCustomTrackCollector) {
    auto *p = dynamic_cast<TrackCollectorCustom*>(fTrackCollector.get());
    p->Reconstruct(event);
  } else {
    fTrackCollector->Reconstruct(event);
  }

  // Set leading and trailing candidate times
  for (Int_t jj=0; jj < event->GetNCandidates(); jj++) {
    TRecoSpectrometerCandidate *cand = static_cast<TRecoSpectrometerCandidate*>(event->GetCandidate(jj));
    Double_t ttime = cand->GetTime() + refTime;
    cand->SetTime(ttime);
    Double_t ltime = cand->GetLeadingTime() + refTime;
    cand->SetLeadingTime(ltime);
  }

  // reapply alpha/beta track corrections after reconstruction
  if (fApplyAlphaBetaCorrections) {
    OutputState state = kOUninit;
    Double_t alpha = *GetOutput<Double_t>("SpectrometerTrackCorrections.Alpha", state);
    Double_t beta = *GetOutput<Double_t>("SpectrometerTrackCorrections.Beta", state);
    if (state != kOValid) {
      cout << "[SpectrometerRecoAlgorithm]: SpectrometerTrackCorrections output not found, alpha/beta corrections not applied!\n";
    } else {
      for (Int_t iTrack = 0; iTrack < event->GetNCandidates(); iTrack++) {
        TRecoSpectrometerCandidate* cand = static_cast<TRecoSpectrometerCandidate*>(event->GetCandidate(iTrack));
        Double_t ptrack0 = cand->GetMomentum();
        Double_t ptrack = ptrack0 * (1.0 + beta) * (1.0 + cand->GetCharge() * alpha * ptrack0);
        cand->SetMomentum(ptrack);
      }
    }
  }

  // Print the results of standard reprocessing reconstruction with the result of user algorithms
  if (fDebugMode) {
    cout << "[SpectrometerRecoAlgorithm]: Spectrometer candidates reconstructed \n";
    cout << "[SpectrometerRecoAlgorithm]: Old reconstruction candidates: \n";
    PrintRecoCandidates(fRecoCandidatesCopy);
    cout << "[SpectrometerRecoAlgorithm]: New reconstruction candidates: \n";
    PrintRecoCandidates(event->GetCandidates());
    fRecoCandidatesCopy->Clear("C");
  }
}

void SpectrometerRecoAlgorithm::PrintRecoCandidates(TClonesArray *candidates)
{
  for (int ic = 0; ic < candidates->GetEntries(); ic++) {
    auto *cand = static_cast<TRecoSpectrometerCandidate*>(candidates->ConstructedAt(ic));
    cout << "Candidate #: " << ic+1 << " / " << candidates->GetEntries() << "\n";
    cout << " - momentum: " << cand->GetMomentum() << "\n";
    cout << " - slope X: " << cand->GetSlopeXBeforeMagnet() << "\n";
    cout << " - slope Y: " << cand->GetSlopeYBeforeMagnet() << "\n";
  }
}

Bool_t SpectrometerRecoAlgorithm::SeparateHitPerViewPlane(TRecoSpectrometerEvent *event)
{
/// \MemberDescr
/// This function sort the hits per view-plane on the basis of the Chamber ID and on
/// the hit straw-id code. Return true if event is good, false otherwise.
/// \EndMemberDescr
  for (Int_t jHit = 0;jHit < event->GetNHits(); jHit++) {
    TRecoSpectrometerHit* fHit = static_cast<TRecoSpectrometerHit*>(event->GetHits()->At(jHit));
    Straw *straw = fGeo->GetStraw(fHit->GetChannelID());
    if (straw == nullptr) {
      cout << "[SpectrometerRecoAlgorithm] WARNING: No straw geometry found for channel ID: "
           << fHit->GetChannelID() << ", hit skipped\n";
      continue;
    }
    fHit->SetLocalPosition(TVector3(straw->GetLocalPosition().X(),-9999.,straw->GetPosition().Z()));
    Int_t planeID = 2*fHit->GetHalfViewID()+fHit->GetPlaneID(); // plane ID inside view
    Double_t dtime = fHit->GetDriftTime();
    Bool_t goodHit = (dtime>=0 && dtime<=170);
    Int_t hitFlag = 0;
    fChamberHitCollector[fHit->GetChamberID()]->GetView(fHit->GetViewID())->GetPlane(planeID)->AddHit(jHit, hitFlag, goodHit);
  }

  if (event->GetNHits()>900) return false;
  Int_t nGoodHitPerChamber[4];
  Int_t nChamberOverflow = 0;
  Int_t singleChamberOverflow = 0;
  for (Int_t jChamber=0;jChamber<4;jChamber++) {
    nGoodHitPerChamber[jChamber] = 0;
    for (Int_t jView=0;jView<4;jView++) {
      for (Int_t jPlane=0;jPlane<4;jPlane++) {
        auto plane = fChamberHitCollector[jChamber]->GetView(jView)->GetPlane(jPlane);
        nGoodHitPerChamber[jChamber] += plane->GetNGoodHit();
      }
    }
    if (nGoodHitPerChamber[jChamber]>150) singleChamberOverflow++;
    if (nGoodHitPerChamber[jChamber]>64) nChamberOverflow++;
  }
  if (nChamberOverflow>2 && singleChamberOverflow>1) {
    return false;
  }
  return true;
}

ChamberHitCollectorCustom::ChamberHitCollectorCustom(Int_t iChamber, Bool_t useCustomViewCollector)
  : ChamberHitCollector(iChamber)
{
  if (useCustomViewCollector) {
    // replace the view hit collectors with custom ones
    for (ViewHitCollector *vhc : fViewHitCollector) delete vhc;
    fViewHitCollector.clear();
    for (Int_t iView = 0; iView < fPar->GetNViews(); iView++)
      fViewHitCollector.push_back(new ViewHitCollectorCustom(iChamber, iView));
  }
}

ViewHitCollectorCustom::ViewHitCollectorCustom(Int_t ichamber, Int_t iview)
  : ViewHitCollector(ichamber, iview)
{}


void ChamberHitCollectorCustom::ReconstructHit()
{
  // Standard Spectrometer reconstruction function can be replaced here with a custom algorithm
  ChamberHitCollector::ReconstructHit();
}

void ViewHitCollectorCustom::ReconstructHitPositions(TRecoSpectrometerEvent *event)
{
  // Standard Spectrometer reconstruction function can be replaced here with a custom algorithm
  ViewHitCollector::ReconstructHitPositions(event);
}

void TrackCollectorCustom::Reconstruct(TRecoSpectrometerEvent *event)
{
  // Standard Spectrometer reconstruction function can be replaced here with a custom algorithm
  TrackCollector::Reconstruct(event);
}
