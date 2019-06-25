// ---------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-04-27
// 
// ---------------------------------------------------------

/// \class FilterOneTrackPnn
/// \Brief
/// Filter events from PNN mask with at least one track with p<40 GeV, in time with the trigger
/// \EndBrief
/// \Detailed
/// Filter events from PNN mask with at least one reconstructed track with p<40 GeV,
/// in time with the trigger (+/- 2ns), and with no in-time activity in LAVs (except for LAV12) and SAVs.
/// The filter is used as follows:
/// \code
/// ./MyApplication -i <input_file> -o <output_file> --filter
/// \endcode
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "FilterOneTrackPnn.hh"
#include "DownstreamTrack.hh"
#include "EnergyCluster.hh"
#include "GeometricAcceptance.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

FilterOneTrackPnn::FilterOneTrackPnn(Core::BaseAnalysis *ba) :
  Analyzer(ba, "FilterOneTrackPnn") {
  RequestAllMCTrees();
  RequestAllRecoTrees();
}

void FilterOneTrackPnn::Process(Int_t) {

  if(!(GetL0Data()->GetDataType()&0x1))     return; // physics triggers only
  if(!(GetL0Data()->GetTriggerFlags()&0x2)) return; // PNN mask only 

  Double_t TriggerTime = GetL0Data()->GetReferenceFineTime()*TdcCalib;

  const Double_t DeltaTrackTriggerTime = 2.; //ns

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if (!Tracks.size()) return;

  TRecoLAVEvent* LAVEvent = GetEvent<TRecoLAVEvent>();
  TRecoIRCEvent* IRCEvent = GetEvent<TRecoIRCEvent>();
  TRecoSACEvent* SACEvent = GetEvent<TRecoSACEvent>();
  LAVMatching* pLAVMatching = *GetOutput<LAVMatching*>("PhotonVetoHandler.LAVMatching");
  SAVMatching* pSAVMatching = *GetOutput<SAVMatching*>("PhotonVetoHandler.SAVMatching");
  std::vector<EnergyCluster> Clusters =  *GetOutput<std::vector<EnergyCluster>>("EnergyClusterBuilder.Output");

  Int_t NGoodTracks = 0;
  for(UInt_t iTrack=0; iTrack<Tracks.size();iTrack++){
    Int_t MLHp = Tracks[iTrack].GetRICHMostLikelyHypothesis();
    Double_t TrackTime = Tracks[iTrack].GetRICHRingTime(MLHp);
    if (fabs(TrackTime-TriggerTime)>DeltaTrackTriggerTime)     continue; // in-time wrt trigger
    if (Tracks[iTrack].GetCharge()!=1)                         continue;
    if (Tracks[iTrack].GetMomentum()< 5000.)                   continue; // spectrometer calibration included
    if (Tracks[iTrack].GetMomentum()>40000.)                   continue; // spectrometer calibration included
    if (Tracks[iTrack].GetChi2()>40.0)                         continue;
    Bool_t InAcceptance = true;
    for (Int_t iCh=0; iCh<4; iCh++) {
      if (!GeometricAcceptance::GetInstance()->InAcceptance(Tracks[iTrack].GetSpectrometerCandidate(), kSpectrometer,iCh)) InAcceptance = false;
    }
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Tracks[iTrack].GetSpectrometerCandidate(), kCHOD,0,125.,-1)) InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Tracks[iTrack].GetSpectrometerCandidate(), kNewCHOD))        InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Tracks[iTrack].GetSpectrometerCandidate(), kLKr,0,140.,-1))  InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Tracks[iTrack].GetSpectrometerCandidate(), kMUV3,0,-1,-1))   InAcceptance = false;

    if (!InAcceptance) continue;

    /////////////////////////////////
    // IRC and SAC veto (with timing)

    pSAVMatching->SetReferenceTime(TrackTime);
    if (pSAVMatching->SAVHasTimeMatching(IRCEvent, SACEvent)) continue;

    /////////////////////////
    // LAV veto (with timing)

    pLAVMatching->SetReferenceTime(TrackTime);
    pLAVMatching->UnmaskAllStations();
    pLAVMatching->SetMaskedStation(12); // ignore LAV12
    if (pLAVMatching->LAVHasTimeMatching(LAVEvent)) continue;

    /////////////////////////////////////////////////////
    // MUV3 veto: no in-time (<1ns) track association
    // (for pathological bursts with no MUV3 veto at L0)

    Double_t MinMUV3TrackTime = 1000.;
    if (Tracks[iTrack].MUV3AssociationExists()) {
      for(Int_t iMUV3Cand=0; iMUV3Cand<Tracks[iTrack].GetNMUV3AssociationRecords(); iMUV3Cand++){
        if(fabs(Tracks[iTrack].GetMUV3Time(iMUV3Cand)-TrackTime)<MinMUV3TrackTime) {
          MinMUV3TrackTime = fabs(Tracks[iTrack].GetMUV3Time(iMUV3Cand)-TrackTime);
        }
      }
    }
    if(MinMUV3TrackTime<1.) continue; //ns

    //////////////////////////////////////////////////////////////
    // LKr veto: no more than 40GeV energy in-time (<3ns) with the track
    // (for pathological bursts with no LKr veto at L0)
    Double_t TotalInTimeEnergy = 0.; 
    for(UInt_t iCls=0; iCls<Clusters.size(); iCls++){
      if(fabs(Clusters[iCls].GetLKrCandidate()->GetTime()-TrackTime)>3.) continue; //out of time
      TotalInTimeEnergy+=Clusters[iCls].GetLKrCandidate()->GetClusterEnergy();
    }
    if(TotalInTimeEnergy>40000.) continue; //MeV

    NGoodTracks++;
  }

  //At least a good track
  if (!NGoodTracks) return;

  FilterAccept();
}
