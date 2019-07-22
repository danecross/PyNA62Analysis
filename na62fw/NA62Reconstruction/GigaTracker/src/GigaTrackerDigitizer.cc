// --------------------------------------------------------------
// History:
//
// Modified by Bob Velghe 2014-10-13
// - Refactor the code, correct a bug in the energy sums
//
// Modified by Bob Velghe 2014-08-04
// - Add misalignment (time,postion) parameters
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-10-29
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-18
//
// --------------------------------------------------------------

/// \class GigaTrackerDigitizer
/// \Brief
/// In this class the energies of GEANT4 (unphysical) hits belonging to the same pixel are summed,
/// in order to have only one physical hit per traversing particle.
/// \EndBrief

#include "GigaTrackerParameterTools.hh"
#include "TGigaTrackerHit.hh"
#include "TGigaTrackerDigi.hh"
#include "TGigaTrackerEvent.hh"
#include "GigaTrackerReconstruction.hh"
#include "TDCEvent.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"

#include "GigaTrackerDigitizer.hh"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

bool THitComp::operator()(const TGigaTrackerHit *h1, const TGigaTrackerHit *h2) const {
    return h1->GetTime() <  h2->GetTime();
}


GigaTrackerDigitizer::GigaTrackerDigitizer(NA62VReconstruction *Reco) : NA62VDigitizer(Reco, "GigaTracker") {
    fDigiEvent = new TDCEvent(TGigaTrackerDigi::Class());
    fParTools = nullptr;
}


GigaTrackerDigitizer::~GigaTrackerDigitizer() {
}

TDetectorVEvent *GigaTrackerDigitizer::ProcessEvent(TDetectorVEvent *tEvent){
  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") ||
      tEvent->IsA()->InheritsFrom("TSpecialTriggerEvent"))
    return tEvent;

  TGigaTrackerEvent *GigaTrackerEvent = static_cast<TGigaTrackerEvent *>(tEvent);

  *((TVEvent *)fDigiEvent) = *((TVEvent *)GigaTrackerEvent);
  fDigiEvent->Clear();

  THitMap HitMap;
  // Load Parameters
  int runID = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  bool isMC = true;
  fParTools = GigaTrackerParameterTools::GetInstance(runID, isMC);

  double TimeWindow = fParTools->GetClusterTimeWindow();

  // Loop One
  // Sum in-time energy release in the same pixel.
  // Loop Two
  // Build Digis

  Int_t NHits = GigaTrackerEvent->GetNHits();
  TClonesArray &Hits = (*(GigaTrackerEvent->GetHits()));

  // Loop over the G4 hits
  for (Int_t iHit = 0; iHit < NHits; iHit++) {
    TGigaTrackerHit *Hit = static_cast<TGigaTrackerHit *>(Hits[iHit]);
    // Unique key for each pixel.
    long key = Hit->GetChannelID();
    if (HitMap.find(key) == HitMap.end()) {
      THitSet HitSet;
      HitSet.insert(Hit);
      HitMap.insert(std::pair<long, THitSet>(key, HitSet));
    }
    else {
      HitMap[key].insert(Hit);
      // Build time cluster
      bool flag = true;
      size_t hitCount = HitMap[key].size();
      while (flag) {
        flag = false;
        for (std::pair<THitSet::iterator, THitSet::iterator> it(HitMap[key].begin(), ++(HitMap[key].begin())); it.second != HitMap[key].end(); ++it.first, ++it.second) {
          if (((*it.second)->GetTime() - (*it.first)->GetTime()) < TimeWindow) {
            (*it.first)->SetTime(((*it.second)->GetTime() + (*it.first)->GetTime()) * 0.5);
            (*it.first)->AddEnergy((*it.second)->GetEnergy());
            HitMap[key].erase(it.second++); // Erase invalidate the iterator.
            if (it.second == HitMap[key].end())
              break;
          }
        }
        if (HitMap[key].size() != hitCount) {
          flag = true;
          hitCount = HitMap[key].size();
        }
      }
    }
  }

  for (THitMap::iterator it = HitMap.begin(); it != HitMap.end(); ++it) {
    long key = it->first;
    //int pixel_id;
    //int station_id;
    //GigaTrackerChannelID ChId;
    //ChId.DecodeChannelID(key,station_id,pixel_id);

    for (THitSet::iterator iit = HitMap[key].begin(); iit != HitMap[key].end(); ++iit) {
      //
      // Naive pulse digitization: Energy released --> Charge --> T1/ToT (from curves measured in the lab: 0.6 fC threshold and 2.5 ns rise-time injection)
      //

      Int_t station = (*iit)->GetStationNo();
      double Sigma = fParTools->GetSigmaT(station);
      Double_t Q = fParTools->GetQFromEn((*iit)->GetEnergy());                   // Charge released [fC]
      if (Q < fParTools->GetQCut()) continue;                                    // Don't spawn a Digi if Q < 0.7 fC
      else if (Q > fParTools->GetQSaturation()) Q = fParTools->GetQSaturation(); // Saturation

      //Double_t T1 = (SplineT1(Q) - T1Offset) ; // Pulse leading time after shaping [ns]
      //Double_t ToT = SplineToT(Q) ; // Pulse time-over-threshold after shaping [ns]

      Double_t ToT = fParTools->GetToTFix();
      Double_t MCTime = (*iit)->GetTime();
      Double_t Smearing = fRandom->Gaus(0., Sigma);
      Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
      Double_t LeadingTime = MCTime + FineTime - fReco->GetStationMCToF(station) + Smearing;
      // + fReco->GetT0Correction((*iit)->GetChannelID(),station) // removed to fix NARKD-913: EG, 30/8/18
      Double_t TrailingTime = LeadingTime + ToT;

      // Spawn a Digi
      TGigaTrackerDigi *Digi = static_cast<TGigaTrackerDigi*>(fDigiEvent->AddDigi(*iit));
      Digi->DecodeChannelID();
      Digi->SetLeadingEdge(LeadingTime);
      Digi->SetTrailingEdge(TrailingTime);
      //Digi->SetChannelID((*iit)->GetChannelID());
      //Digi->SetPixelID((*iit)->GetPixelID());
      //Digi->SetChipId((*iit)->GetChipId());
      //Digi->SetqChipId((*iit)->GetqChipId());
      //Digi->SetStationNo((*iit)->GetStationNo());
      //Digi->SetMCHit((*iit));
    }
  }
  return fDigiEvent;
}
