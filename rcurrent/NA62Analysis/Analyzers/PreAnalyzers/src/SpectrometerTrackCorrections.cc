// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-01-26
//
// ---------------------------------------------------------------

/// \class SpectrometerTrackCorrections
/// \Brief
/// Apply corrections to spectrometer track momenta
/// \EndBrief
/// \Detailed
/// Apply the so called beta and alpha corrections to spectrometer track momenta
/// for the overall momentum scale (i.e. MNP33 integral) and the effective chamber alignment, correspondingly.
/// The run-dependent values of the correction constants (different sets for data and MC)
/// are provided by NA62ConditionsService.
/// The original candidate data are overwritten, so this analyzer must be run as a pre-analyzer
/// to make sure the corrections are applied exactly once to each event before user processing.
/// Protection is implemented against the corrections applied multiple times.
/// <br><br>
/// The values of the correction constants can be "forced" externally, for example
/// the following is recommended when running the AlphaBetaComputation tool:
/// \code
/// ./MyApplication -l <list> -p "SpectrometerTrackCorrections:ExternalAlpha=0;ExternalBeta=0"
/// \endcode
/// External (alpha,beta) parameters are also useful to evaluate the beam properties with K3piSelection
/// tool after the (alpha,beta) evaluation with AlphaBetaComputation but before (alpha,beta)
/// are propagated into the database.
/// The unit of alpha is 1/MeV, while beta is dimensionless.
/// The externally defined corrections are used if the values specified satisfy |alpha|<1, |beta|<1.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "SpectrometerTrackCorrections.hh"
#include "NA62ConditionsService.hh"
#include "TRecoSpectrometerEvent.hh"

using namespace std;

SpectrometerTrackCorrections::SpectrometerTrackCorrections(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerTrackCorrections"), fAlpha(0.0), fBeta(0.0), fWarnOnce(false) {
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  AddParam("ExternalAlpha", &fExternalAlpha, -999.999);
  AddParam("ExternalBeta",  &fExternalBeta,  -999.999);
}

void SpectrometerTrackCorrections::InitOutput() {
  RegisterOutput("Alpha", &fAlpha);
  RegisterOutput("Beta",  &fBeta);
}

void SpectrometerTrackCorrections::StartOfRunUser() {
  fAlpha = fBeta = 0.0;
  if (!GetIsTree()) return;
  Bool_t isData = !GetWithMC(); // data or MC?
  TString AlphaBetaFileName = isData ? "AlphaBeta.dat" : "AlphaBetaMC.dat";

  Bool_t ParametersFound = false;
  if (NA62ConditionsService::GetInstance()->Open(AlphaBetaFileName)==kSuccess) {
    TString Line;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(AlphaBetaFileName))) {
      if (Line.BeginsWith("#")) continue;
      TObjArray *l = Line.Tokenize(" ");
      Int_t    Run = ((TObjString*)(l->At(0)))->GetString().Atoi();
      Double_t a   = ((TObjString*)(l->At(1)))->GetString().Atof();
      Double_t b   = ((TObjString*)(l->At(2)))->GetString().Atof();
      delete l;
      if (Run!=GetRunID()) continue; // wrong run number
      fAlpha = a; // unit: [1/MeV]
      fBeta  = b; // dimensionless
      ParametersFound = true;
    }
    NA62ConditionsService::GetInstance()->Close(AlphaBetaFileName);
  }

  // Override with external parameters if they are provided
  if (fabs(fExternalAlpha)<1.0) fAlpha = fExternalAlpha;
  if (fabs(fExternalBeta)<1.0)  fBeta  = fExternalBeta;

  // No external parameters provided and run not found in the database: issue a warning
  if (fabs(fExternalAlpha)>1.0 && fabs(fExternalBeta)>1.0 && !ParametersFound) {
    cout << user_normal() << "Warning: " << (isData?"data":"MC") <<
      " parameters for run " << GetRunID() << " not found in database, using alpha=beta=0" << endl;
  }
  else { // Otherwise (parameters provided by user or found in DB), print the parameters
    cout << user_normal() << (isData?"Data":"MC") << " run "
      << GetRunID() << ": using alpha=" << fAlpha << "; beta=" << fBeta << endl;
  }
}

//////////////////////////////////////////////////////////////
// Apply the corrections. The units are standard: [MeV], [mm].

void SpectrometerTrackCorrections::Process(Int_t) {
  if (!GetIsTree()) return;

  // Initialize outputs
  SetOutputState("Alpha", kOValid);
  SetOutputState("Beta",  kOValid);

  // Protection against multiple application of the corrections
  if (IsAnalyzerInHistory(GetAnalyzerName())) {
    if (!fWarnOnce) {
      cout << user_normal() << "Warning: corrections already applied" << endl;
      fWarnOnce = true;
    }
    return;
  }

  TRecoSpectrometerEvent* SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  if (!SpectrometerEvent) return;
  if (!SpectrometerEvent->GetNCandidates()) return;
  for (Int_t iTrack=0; iTrack<SpectrometerEvent->GetNCandidates(); iTrack++) {
    TRecoSpectrometerCandidate* Scand =
      static_cast<TRecoSpectrometerCandidate*>(SpectrometerEvent->GetCandidate(iTrack));
    Double_t Ptrack0 = Scand->GetMomentum();
    Double_t Ptrack = Ptrack0 * (1.0+fBeta) * (1.0+Scand->GetCharge()*fAlpha*Ptrack0);
    Scand->SetMomentum(Ptrack);
  }
}
