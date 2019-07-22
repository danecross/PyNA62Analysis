// ---------------------------------------------------------------
//
// History:
//
// Created by Michal Koval (michal.koval@cern.ch) 2019-01-08
//
// ---------------------------------------------------------------

/// \class SpectrometerRecoAnalyzer
/// \Brief
/// Analyzer that uses the SpectrometerRecoAlgorithm to reconstruct tracks from reco hits
/// \EndBrief
/// \Detailed
/// This analyzer runs the Spectrometer reconstruction in the analysis stage using
/// a user-specified Spectrometer.conf file. By default, the standard
/// $NA62RECOSOURCE/config/Spectrometer.conf is used. The conf file path is configurable
/// by a user as an Analyzer parameter ConfigFileName
/// \code
/// ./MyApplication -l <list> -p "SpectrometerRecoAnalyzer:ConfigFileName=<file_path>"
/// \endcode
/// Alternatively, one can change the parameter UseExoticsConfig to true to use the
/// config file located in: $NA62RECOSOURCE/config/Spectrometer.exotics.conf
///
/// This analyzer is meant to be run as pre-analyzer so DownstreamTrackBuilder uses the
/// output of the reconstruction. It should be placed in the build config file *before*
/// the SpectrometerTrackCorrections pre-analyzer, in order to apply the track corrections
/// after the SpectrometerRecoAlgorithm reconstructs the tracks.
///
/// \author Michal Koval (michal.koval@cern.ch)
/// \EndDetailed

#include "SpectrometerRecoAnalyzer.hh"
#include "SpectrometerRecoAlgorithm.hh"
#include "NA62ConditionsService.hh"
#include "TSystem.h"
using namespace std;

#include "TSpectrometerDigi.hh"
#include "BeamIntensityGenerator.hh"

SpectrometerRecoAnalyzer::SpectrometerRecoAnalyzer(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerRecoAnalyzer")
{
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  // config file name
  fRecoConfPath = gSystem->Getenv("NA62RECOSOURCE");
  fRecoConfPath.Append("/config");
  TString defaultConfigName(fRecoConfPath + "/Spectrometer.conf");
  AddParam("ConfigFileName", &fConfFileName, defaultConfigName);
  AddParam("UseExoticsConfig", &fUseExoticsConfig, false);
  // create SpectrometerRecoAlgorithm object
  fSpecReco = make_unique<SpectrometerRecoAlgorithm>(ba, this, "SpectrometerRecoAnalyzer");

  AddParam("UpdateWireDistance", &fUpdateWireDistance, false);
}

void SpectrometerRecoAnalyzer::InitHist(){
}

void SpectrometerRecoAnalyzer::StartOfRunUser()
{
  // UseExoticsConfig flag takes precedence over default specified path
  if (fUseExoticsConfig) {
    fConfFileName = fRecoConfPath.Append("/Spectrometer.exotics.conf");
  }
  // set Spectrometer reco conf file path
  fSpecReco->SetConfFileName(fConfFileName);

  // set correct algorithm flags settings for the use case of this pre-analyzer
  fSpecReco->SetDebugMode(false);
  fSpecReco->SetUseCustomViewCollector(false);
  fSpecReco->SetUseCustomChamberCollector(false);
  fSpecReco->SetUseCustomTrackCollector(false);
  fSpecReco->SetApplyAlphaBetaCorrections(false);
  fSpecReco->SetUpdateWireDistance(fUpdateWireDistance); // needed by DPG.
}

void SpectrometerRecoAnalyzer::StartOfBurstUser(){
}

void SpectrometerRecoAnalyzer::Process(Int_t )
{
  auto spectrometerEvent = static_cast<TRecoSpectrometerEvent*>(GetEvent("Spectrometer"));
  if (!spectrometerEvent) return;
  fSpecReco->Process(spectrometerEvent);
}

void SpectrometerRecoAnalyzer::EndOfJobUser(){
  SaveAllPlots();
}
