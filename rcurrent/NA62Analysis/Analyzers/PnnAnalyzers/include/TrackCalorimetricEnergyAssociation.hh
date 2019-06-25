#ifndef TRACKCALORIMETRICENERGYASSOCIATION_HH
#define TRACKCALORIMETRICENERGYASSOCIATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "TRecoLKrCandidate.hh"
#include <TCanvas.h>

class TrackCalorimetricEnergyAssociation : public NA62Analysis::Analyzer
{
public:
  TrackCalorimetricEnergyAssociation(NA62Analysis::Core::BaseAnalysis *ba);
  ~TrackCalorimetricEnergyAssociation();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void Process(int iEvent);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();
  void PrepareOutputs();
  void ValidateOutputs();

protected:
  bool fReadingData;

  std::vector<int> fAssocMUV1ID;
  std::vector<int> fAssocMUV2ID;
  std::vector<double> fTotalEnergy;
  std::vector<double> fTimeOfEnergy;
  std::vector<double> fMuonProbability;
  std::vector<double> fPionProbability;
  std::vector<double> fElectronProbability;
  std::vector<double> fMUV1Energy;
  std::vector<double> fMUV2Energy;
  std::vector<double> fExtraMUV1Energy;
  std::vector<double> fExtraMUV2Energy;
  std::vector<double> fDMIP;

  //parameters
  bool verb;
  double fLKrEnergyCenter;
  double fLKrEnergySigma;
  double fMUV1EnergyCenter;
  double fMUV1EnergySigma;
  double fMUV2EnergyCenter;
  double fMUV2EnergySigma;
};
#endif
