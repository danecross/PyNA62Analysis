//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------
// Based on GEANT 4 - MCTruthTrackingAction class
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)
//            Francesca Bucci (Francesca.Bucci@cern.ch) 2008-01-17
//
// 2011-03-11 Spasimir Balev (Spasimir.Balev@cern.ch)
//            - Update on handling of KineParts
//
// Modified by Sergey Podolsky 2013-10-25
//
// November 2015: major revision of KineParts treatment (goudzovs@cern.ch)
//
// --------------------------------------------------------------

#include "G4Track.hh"
#include "G4TrackVector.hh"
#include "G4TrackingManager.hh"
#include "MCTruthTrackingAction.hh"
#include "MCTruthTrackInformation.hh"

void MCTruthTrackingAction::PreUserTrackingAction(const G4Track* track) {
  fInitialMomentum = G4LorentzVector(track->GetMomentum(), track->GetTotalEnergy());

  if (track->GetParentID()==0 && track->GetUserInformation()==0) {
    MCTruthTrackInformation* info = new MCTruthTrackInformation(track);
    info->SetDepthLevel(0);
    info->SetSavedParentID(0);
    G4Track* theTrack = const_cast<G4Track*>(track);
    theTrack->SetUserInformation(info);
  }
}

////////////////////////////////////////////////////
// User action at the end of tracking for each track

void MCTruthTrackingAction::PostUserTrackingAction(const G4Track* Track) {
  if (Track->GetTrackID()==1) { // Perform checks on the beam particle
    fRejectAll = false;
    Double_t BeamParticleEndZ = Track->GetPosition().z()/mm;
    TString  BeamParticleEndProcess =
      Track->GetStep()->GetPostStepPoint()->GetProcessDefinedStep()->GetProcessName();

    // The beam particle endpoint must be in the range specified to save KineParts
    if (BeamParticleEndZ < MCTruthManager::GetInstance()->GetConfig()->GetZMin() ||
	BeamParticleEndZ > MCTruthManager::GetInstance()->GetConfig()->GetZMax()) fRejectAll = true;

    // The beam particle end process must not be in the reject list to save KineParts
    std::vector<G4String>::iterator it;
    for (it = MCTruthManager::GetInstance()->GetConfig()->GetBeamParticleEndProcessToReject().begin();
	 it < MCTruthManager::GetInstance()->GetConfig()->GetBeamParticleEndProcessToReject().end(); ++it) {
      if (BeamParticleEndProcess.Contains((*it).data())) fRejectAll = true;
    }
  }

  MCTruthTrackInformation* info = static_cast<MCTruthTrackInformation*>((Track->GetUserInformation()));
  G4TrackVector* secondaries = fpTrackingManager->GimmeSecondaries();
  if (secondaries) {
    for (size_t i=0; i<secondaries->size(); i++) {
      MCTruthTrackInformation* infoNew = new MCTruthTrackInformation(info);
      G4int newDepthLevel = infoNew->GetDepthLevel()+1;
      infoNew->SetDepthLevel(newDepthLevel);
      (*secondaries)[i]->SetUserInformation(infoNew);
    }
  }

  fFinalMomentum = G4LorentzVector(Track->GetMomentum(), Track->GetTotalEnergy());
  G4LorentzVector prodpos(Track->GetGlobalTime()-Track->GetLocalTime(), Track->GetVertexPosition());
  G4LorentzVector endpos (Track->GetGlobalTime(), Track->GetPosition());

  MCTruthConfig* config = MCTruthManager::GetInstance()->GetConfig();
  G4int* SavedParentIDMap = MCTruthManager::GetInstance()->GetSavedParentIDMap();
  G4int TrackID  = Track->GetTrackID();
  G4int ParentID = Track->GetParentID();

  /////////////////////////////
  // The track should be stored

  if (trackToBeStored(Track)) {
    if (TrackID<1000000) { // prevent overflow
      SavedParentIDMap[TrackID] = TrackID;
      if (config->GetSaveTrackVerbose()) {
	G4cout <<
	  " TrackID="<<TrackID<<
	  " ParentID="<<ParentID<<
	  " Depth="<<info->GetDepthLevel()<<
	  " SavedParent="<<SavedParentIDMap[TrackID]<<
	  " Name="<<Track->GetDefinition()->GetParticleName()<< G4endl <<
	  "  E0="<<fInitialMomentum.e()<<
	  "  E1="<<fFinalMomentum.e()<<
	  " Creator="<<(ParentID>0 ? Track->GetCreatorProcess()->GetProcessName() : "Undefined")<<
	  " Destructor="<<Track->GetStep()->GetPostStepPoint()->GetProcessDefinedStep()->GetProcessName()<<
	  " stored" << G4endl;
      }
    }

    if (secondaries) {
      for (size_t i=0; i<secondaries->size(); i++) {
	if (trackToBeStored((*secondaries)[i]))
	  static_cast<MCTruthTrackInformation*>((*secondaries)[i]->GetUserInformation())->
	    SetSavedParentID((*secondaries)[i]->GetTrackID());
	else
	  static_cast<MCTruthTrackInformation*>((*secondaries)[i]->GetUserInformation())->SetSavedParentID(TrackID);
      }
    }

    MCTruthTrackInformation* mcinfo = static_cast<MCTruthTrackInformation*>(Track->GetUserInformation());

    KinePart *part = MCTruthManager::GetInstance()->AddParticle();
    part->SetParticleName(Track->GetDefinition()->GetParticleName());
    part->SetPDGcode(Track->GetDefinition()->GetPDGEncoding());
    part->SetProdPos
      (TLorentzVector(prodpos.x()/mm, prodpos.y()/mm, prodpos.z()/mm, prodpos.t()/ns));
    part->SetEndPos
      (TLorentzVector(endpos.x()/mm,endpos.y()/mm,endpos.z()/mm,endpos.t()/ns));
    part->SetInitial4Momentum
      (TLorentzVector
       (fInitialMomentum.x()/MeV,fInitialMomentum.y()/MeV,fInitialMomentum.z()/MeV,fInitialMomentum.e()/MeV));
    part->SetFinal4Momentum
      (TLorentzVector
       (fFinalMomentum.x()/MeV,fFinalMomentum.y()/MeV,fFinalMomentum.z()/MeV,fFinalMomentum.e()/MeV));
    part->SetID(TrackID);
    part->SetParentID(ParentID);
    part->SetDirectParent(mcinfo->GetDirectParent());
    part->SetProdProcessName(ParentID>0 ? Track->GetCreatorProcess()->GetProcessName() : "Undefined");
    part->SetEndProcessName(Track->GetStep()->GetPostStepPoint()->GetProcessDefinedStep()->GetProcessName());

    for (G4int i=0; i<15; i++) {
      part->SetPosAtCheckPoint(i, mcinfo->GetPosAtCheckPoint(i));
      part->SetMomAtCheckPoint(i, mcinfo->GetMomAtCheckPoint(i));
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // If track is not to be stored, propagate its parent ID to its secondaries

  else {
    if (TrackID<1000000 && ParentID<1000000) { // prevent overflow
      SavedParentIDMap[TrackID] = SavedParentIDMap[ParentID];
      if (config->GetSaveTrackVerbose()) {
	G4cout <<
	  " TrackID="<<TrackID<<
	  " ParentID="<<ParentID<<
	  " Depth="<<info->GetDepthLevel()<<
	  " SavedParent="<<SavedParentIDMap[TrackID]<<
	  " Name="<<Track->GetDefinition()->GetParticleName()<< G4endl <<
	  "  E0="<<fInitialMomentum.e()<<
	  "  E1="<<fFinalMomentum.e()<<
	  " Creator="<<(ParentID>0 ? Track->GetCreatorProcess()->GetProcessName() : "Undefined")<<
	  " Destructor="<<Track->GetStep()->GetPostStepPoint()->GetProcessDefinedStep()->GetProcessName()<<
	  " not stored" << G4endl;
      }
    }

    G4TrackVector* children = fpTrackingManager->GimmeSecondaries();
    for (unsigned int index = 0; index < children->size(); ++index) {
      G4Track* tr = (*children)[index];
      tr->SetParentID(ParentID);
      // set the flag saying that the direct mother is not stored
      MCTruthTrackInformation* mcinf = static_cast<MCTruthTrackInformation*>(tr->GetUserInformation());
      if (!mcinf) tr->SetUserInformation(mcinf = new MCTruthTrackInformation);
      mcinf->SetDirectParent(false); 
    }
  }
}

G4bool MCTruthTrackingAction::trackToBeStored(const G4Track* track) {

  MCTruthConfig* config = MCTruthManager::GetInstance()->GetConfig();

  // If required, save all tracks crossing a plane after the spectrometer for regeneration
  if (config->GetSaveTracksForRegeneration()) {
    if ((track->GetVertexPosition().z()<219546.0 && track->GetPosition().z()>219546.0) ||
	(track->GetNextVolume() &&
	 track->GetNextVolume()->GetLogicalVolume()->GetMaterial()->GetName()=="NA62BlackHole"))
      return true;
  }

  // Save all the beam particles
  if (track->GetParentID()<1) return true;

  // Save all steps (then no need to save the tracks)?
  if (config->GetSaveAllSteps()) return false;

  // Save all tracks?
  if (config->GetSaveAllTracks()) return true;

  ///////////////////////////////////////////////////////////////////////////////////////
  // If the beam particle end process is in the reject list, or its endpoint is
  // outside the Z range specified, all secondary particles are not saved into KineParts

  if (fRejectAll) return false;

  /////////////////////////////////////////////////
  // Save two generations of daughters (via decays)

  MCTruthTrackInformation* info = static_cast<MCTruthTrackInformation*>(track->GetUserInformation());
  if (info->GetDepthLevel()>=1 && info->GetDepthLevel()<=2 &&
      track->GetCreatorProcess()->GetProcessName().contains("Decay")) { // Decay or DecayWithSpin
    return true;
  }

  //////////////////////////////////////////////////////////
  // Save by particle type, creator process and end process.
  // Exact name matches are required in this category.

  // Particles to be stored for any interaction level and energy
  std::vector<G4String> ParticleTypeToSave = config->GetParticleTypeToSave();
  if (std::find(ParticleTypeToSave.begin(),
		ParticleTypeToSave.end(),
		track->GetDefinition()->GetParticleName())
      != ParticleTypeToSave.end()) return true;

  // Particles with Creator process to be stored for any interaction level and energy
  if (track->GetParentID()>0) {
    std::vector<G4String> ParticleCreatorProcessToSave = config->GetParticleCreatorProcessToSave();
    if (std::find(ParticleCreatorProcessToSave.begin(),
		  ParticleCreatorProcessToSave.end(),
		  track->GetCreatorProcess()->GetProcessName())
	!= ParticleCreatorProcessToSave.end()) return true;
  }

  // Particles with End process to be stored for any interaction level and energy
  std::vector<G4String> ParticleEndProcessToSave = config->GetParticleEndProcessToSave();
  if (track->GetStep() &&
      std::find(ParticleEndProcessToSave.begin(),
		ParticleEndProcessToSave.end(),
		track->GetStep()->GetPostStepPoint()->GetProcessDefinedStep()->GetProcessName())
      != ParticleEndProcessToSave.end()) return true;

  /////////////////////////////////////////////////////////////////////////////////////////
  // Rejection by particle type, creator process and end process.
  // Partial name matches are sufficient in this category. For example,
  // "Inelastic" suppresses all inelastic interactions (kaon+Inelastic, pi+Inelastic, etc).

  // Particles to be rejected for any interaction level and energy
  // (unless the are already stored for one of the reasons above)
  std::vector<G4String> ParticleTypeToReject = config->GetParticleTypeToReject();
  std::vector<G4String>::iterator it;
  for (it = ParticleTypeToReject.begin();
       it < ParticleTypeToReject.end(); ++it) {
    if (track->GetDefinition()->GetParticleName().contains((*it).data())) return false;
  }

  // Particles with Creator process to be rejected for any interaction level and energy
  // (unless the are already stored for one of the reasons above)
  if (track->GetParentID()>0) {
    std::vector<G4String> ParticleCreatorProcessToReject = config->GetParticleCreatorProcessToReject();
    for (it = ParticleCreatorProcessToReject.begin();
	 it < ParticleCreatorProcessToReject.end(); ++it) {
      if (track->GetCreatorProcess()->GetProcessName().contains((*it).data())) return false;
    }
  }

  // Particles with End process to be rejected for any interaction level and energy
  // (unless the are already stored for one of the reasons above)
  if (track->GetStep()) {
    std::vector<G4String> ParticleEndProcessToReject = config->GetParticleEndProcessToReject();
    for (it = ParticleEndProcessToReject.begin();
	 it < ParticleEndProcessToReject.end(); ++it) {
      if (track->GetStep()->GetPostStepPoint()->GetProcessDefinedStep()->GetProcessName().
	  contains((*it).data())) return false;
    }
  }

  /////////////////////////////////////////////////////////////////
  // Check interaction level and energy for the remaining particles

  if (info->GetDepthLevel() <= config->GetMaxInteractionLevel() &&
      fInitialMomentum.e() > config->GetEMin()) return true;

  return false;
}
