//
// ********************************************************************
// * DISCLAIMER                                                       *
// *                                                                  *
// * The following disclaimer summarizes all the specific disclaimers *
// * of contributors to this software. The specific disclaimers,which *
// * govern, are listed with their locations in:                      *
// *   http://cern.ch/geant4/license                                  *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.                                                             *
// *                                                                  *
// * This  code  implementation is the  intellectual property  of the *
// * GEANT4 collaboration.                                            *
// * By copying,  distributing  or modifying the Program (or any work *
// * based  on  the Program)  you indicate  your  acceptance of  this *
// * statement, and all its terms.                                    *
// ********************************************************************
//
// --------------------------------------------------------------
//
// Antonino Sergi (Antonino.Sergi@cern.ch)
// Francesca Bucci (Francesca.Bucci@cern.ch)
//
// --------------------------------------------------------------

#include "SteppingAction.hh"
#include "EventAction.hh"
#include "G4SteppingManager.hh"
#include "G4RunManager.hh"
#include "G4Track.hh"
#include "G4UnitsTable.hh"
#include "G4VProcess.hh"
#include "G4SteppingManager.hh"
#include "MCTruthManager.hh"
#include "MCTruthTrackInformation.hh"
#include "PrimaryGeneratorAction.hh"

SteppingAction::SteppingAction(EventAction* EvAct) :
  eventAction(EvAct), myVerbose(nullptr), fNCheckPoints(15) {

  /////////////////////////////////////////////////////////////////////////////////
  // The kinematics control points are defined at the following steps.
  // The "<" or ">" sign defines if pre-step or post-step point should be recorded.

  fCheckPointNames [0] = "World            > CedarRespReg";          // Cedar entry    [z= 69200mm]
  fCheckPointNames [1] = "CedarRespReg     > CedarQuadMagnetField0"; // Cedar exit     [z= 76305mm]
  fCheckPointNames [2] = "CedarRespReg     > GigaTracker";       // Gigatraker entry   [z= 79440mm]
  fCheckPointNames [3] = "GigaTracker      < CHANTI";            // Gigatracker exit   [z=102420mm]
  fCheckPointNames [4] = "LAV_Station_08   > LAV_RR0";           // Spectrometer entry [z=181115mm]
  fCheckPointNames [5] = "Spectrometer_RR1 < MNP33Magnet";       // MNP33 magnet entry [z=194262.9mm]
  fCheckPointNames [6] = "LAV_Station_10   > Spectrometer_RR2";  // Magnet exit        [z=203764mm]
  fCheckPointNames [7] = "RICHRadiator     > RICHMirror";        // RICH mirror entry  [z=236875mm approx]
  fCheckPointNames [8] = "LAV_Station_12   > CHODRespReg";       // CHOD entry         [z=238850mm]
  fCheckPointNames [9] = "IRC              > LKr";               // LKr entry          [z=240388mm]
  fCheckPointNames[10] = "World            > MUV1";              // MUV1 entry         [z=243224mm]
  fCheckPointNames[11] = "MUV1             > MUV2";              // MUV2 entry         [z=244385mm]
  fCheckPointNames[12] = "MUV3             > FISC5";             // FISC5 beam monitor [z=245392mm, inside beam pipe]
  fCheckPointNames[13] = "MUV3FeWall       > MUV3";              // MUV3 entry         [z=246700mm, outside beam pipe]
  fCheckPointNames[14] = "SAC              > World";             // XWCM beam monitor  [z=264325mm]

  for (int i=0; i<fNCheckPoints; i++) {
    TObjArray *l = fCheckPointNames[i].Tokenize(" ");
    fVolumeName1[i] = ((TObjString*)(l->At(0)))->String();
    fVolumeName2[i] = ((TObjString*)(l->At(2)))->String();
    fPreStep[i]     = (((TObjString*)(l->At(1)))->String()=="<");
  }
}

void SteppingAction::UserSteppingAction(const G4Step* aStep) {

  //PrintStep(aStep); // print steps for all particles
  //PrintStep(aStep, "kaon+"); // print steps for a certain particle type only
  //ShortPrintStep(aStep, "kaon+"); // short printout: one line per step, convenient for "radiography"

  G4Track *Track = aStep->GetTrack();

  ////////////////////////////////////////
  // Save track coordinates at checkpoints

  if (aStep->GetPreStepPoint()->GetPhysicalVolume() &&
      aStep->GetPostStepPoint()->GetPhysicalVolume()) {

    for (int iCheckPoint=0; iCheckPoint<fNCheckPoints; iCheckPoint++) {
      if (!fVolumeName1[iCheckPoint].CompareTo
	  (aStep->GetPreStepPoint()-> GetPhysicalVolume()->GetName().data()) &&
	  !fVolumeName2[iCheckPoint].CompareTo
	  (aStep->GetPostStepPoint()->GetPhysicalVolume()->GetName().data())) {

	MCTruthTrackInformation* info =
	  static_cast<MCTruthTrackInformation*>(Track->GetUserInformation());
	G4StepPoint *Point = (fPreStep[iCheckPoint]) ?
	  aStep->GetPreStepPoint() : aStep->GetPostStepPoint();
	info->SetPosAtCheckPoint
	  (iCheckPoint,
	   TVector3(Point->GetPosition().x(),
		    Point->GetPosition().y(),
		    Point->GetPosition().z()));
	info->SetMomAtCheckPoint
	  (iCheckPoint,
	   TLorentzVector(Point->GetMomentum().x(),
			  Point->GetMomentum().y(),
			  Point->GetMomentum().z(),
			  Point->GetTotalEnergy()));
	break;
      }
    }
  }

  if (aStep->GetPostStepPoint()->GetPhysicalVolume()) {

    /////////////////////////////////////////////////
    // Suppress particles touching black hole volumes

    if (aStep->GetPostStepPoint()->GetPhysicalVolume()->
	GetLogicalVolume()->GetMaterial()->GetName()=="NA62BlackHole" &&
	Track->GetParticleDefinition()->GetParticleName()!="geantino" &&
	Track->GetParticleDefinition()->GetParticleName()!="Exotic") {
      Track->SetTrackStatus(fStopAndKill);
    }

    ////////////////////////////////////////////////////////////
    // Suppress low momentum particles entering GTK magnet yokes

    if (aStep->GetPostStepPoint()->GetPhysicalVolume()->GetName().contains("GigaTrackerMCBMagnet") ||
        aStep->GetPostStepPoint()->GetPhysicalVolume()->GetName() == "GigaTrackerMDXMagnet0" ||
	aStep->GetPostStepPoint()->GetPhysicalVolume()->GetName() == "GigaTrackerScraperIron") {
      if (Track->GetMomentum().mag()<100.0) { // [MeV/c]
        Track->SetTrackStatus(fStopAndKill);
      }
    }

    //////////////////////////////////////////////////////////
    // Suppress kaons at GTK/CHANTI boundary (CP3) if required

    if (aStep->GetPreStepPoint()->GetPhysicalVolume()->GetName()=="GigaTracker" &&
	aStep->GetPostStepPoint()->GetPhysicalVolume()->GetName()=="CHANTI" &&
	Track->GetParticleDefinition()->GetParticleName()=="kaon+") {
      const PrimaryGeneratorAction *PGA =
        static_cast<const PrimaryGeneratorAction*>(G4RunManager::GetRunManager()->GetUserPrimaryGeneratorAction());
      G4double MaxKaonMomentum = PGA->GetMaxKaonMomentum();
      if (MaxKaonMomentum>0.0 && Track->GetMomentum().mag()>MaxKaonMomentum) {
	Track->SetTrackStatus(fKillTrackAndSecondaries);
	eventAction->SetSkip(true); // this event will not be saved
      }
    }
  }

  /////////////////////////////////////////////////////////////////
  // Forbid Cherenkov light emission by secondaries in CEDAR lenses

  if (aStep->GetPreStepPoint()->GetPhysicalVolume()) {
    if ((aStep->GetPreStepPoint()->GetPhysicalVolume()->
	 GetLogicalVolume()->GetMaterial()->GetName()=="Cedar_Quartz" ||
	 aStep->GetPreStepPoint()->GetPhysicalVolume()->
	 GetLogicalVolume()->GetMaterial()->GetName()=="Cedar_IdealQuartz") &&
	(Track->GetParticleDefinition()->GetParticleName()!="opticalphoton" &&
	 Track->GetParticleDefinition()->GetParticleName()!="geantino" &&
	 Track->GetParticleDefinition()->GetParticleName()!="Exotic")) {
      Track->SetTrackStatus(fKillTrackAndSecondaries);
    }
  }

  //////////////////
  // Save all steps?

  MCTruthConfig* config = MCTruthManager::GetInstance()->GetConfig();
  if (config->GetSaveAllSteps()) {    
    fFinalMomentum = G4LorentzVector(Track->GetMomentum(), Track->GetTotalEnergy());
    fInitialMomentum = fFinalMomentum - G4LorentzVector(aStep->GetDeltaMomentum(), aStep->GetDeltaEnergy());
    if(!Track->GetUserInformation()) {
      G4VUserTrackInformation* mcinfo = new MCTruthTrackInformation;
      Track->SetUserInformation(mcinfo);
    }

    G4LorentzVector prodpos(Track->GetGlobalTime()-Track->GetLocalTime(), Track->GetVertexPosition());
    G4LorentzVector endpos (Track->GetGlobalTime(),Track->GetPosition());

    MCTruthTrackInformation* mcinfo =
      static_cast<MCTruthTrackInformation*>(Track->GetUserInformation());

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
    part->SetID(Track->GetTrackID());
    part->SetParentID(Track->GetParentID());
    part->SetDirectParent(mcinfo->GetDirectParent());
    part->SetProdProcessName(Track->GetParentID()>0 ? Track->GetCreatorProcess()->GetProcessName() : "Undefined");
    part->SetEndProcessName(Track->GetStep()->GetPostStepPoint()->GetProcessDefinedStep()->GetProcessName());
  }
}

// Detailed two-lines printout of a step
void SteppingAction::PrintStep(const G4Step* aStep, G4String ParticleName) {
  G4Track *Track = aStep->GetTrack();
  if (!ParticleName.length() ||
      Track->GetParticleDefinition()->GetParticleName()==ParticleName) {
    if (aStep->GetPreStepPoint()->GetPhysicalVolume()) {
      G4cout << 
	Form("PreStep:  %7.1f %7.1f %8.1f %4.1f | %f %f %f %f | %s %s\n",
	     aStep->GetPreStepPoint()->GetPosition().x(),
	     aStep->GetPreStepPoint()->GetPosition().y(),
	     aStep->GetPreStepPoint()->GetPosition().z(),
             aStep->GetPreStepPoint()->GetGlobalTime(),
	     aStep->GetPreStepPoint()->GetMomentum().x(),
	     aStep->GetPreStepPoint()->GetMomentum().y(),
	     aStep->GetPreStepPoint()->GetMomentum().z(),
	     aStep->GetPreStepPoint()->GetTotalEnergy(),
	     Track->GetParticleDefinition()->GetParticleName().data(),
	     aStep->GetPreStepPoint()->GetPhysicalVolume()->GetName().data());
    }
    if (aStep->GetPostStepPoint()->GetPhysicalVolume()) {
      G4cout << 
	Form("PostStep: %7.1f %7.1f %8.1f %4.1f | %f %f %f %f | %s %s\n",
	     aStep->GetPostStepPoint()->GetPosition().x(),
	     aStep->GetPostStepPoint()->GetPosition().y(),
	     aStep->GetPostStepPoint()->GetPosition().z(),
             aStep->GetPostStepPoint()->GetGlobalTime(),
	     aStep->GetPostStepPoint()->GetMomentum().x(),
	     aStep->GetPostStepPoint()->GetMomentum().y(),
	     aStep->GetPostStepPoint()->GetMomentum().z(),
	     aStep->GetPostStepPoint()->GetTotalEnergy(),
	     Track->GetParticleDefinition()->GetParticleName().data(),
	     aStep->GetPostStepPoint()->GetPhysicalVolume()->GetName().data());
    }
  }
}

// Short one-line printout of a step
void SteppingAction::ShortPrintStep(const G4Step* aStep, G4String ParticleName) {
  G4Track *Track = aStep->GetTrack();
  if (!ParticleName.length() ||
      Track->GetParticleDefinition()->GetParticleName()==ParticleName) {
    if (aStep->GetPreStepPoint()->GetPhysicalVolume() &&
	aStep->GetPostStepPoint()->GetPhysicalVolume()) {
      G4cout << 
	Form("Step: %7.1f %7.1f %8.1f %4.1f | %7.1f %7.1f %8.1f %4.1f | %s %s %s\n",
	     aStep->GetPreStepPoint()->GetPosition().x(),
	     aStep->GetPreStepPoint()->GetPosition().y(),
	     aStep->GetPreStepPoint()->GetPosition().z(),
             aStep->GetPreStepPoint()->GetGlobalTime(),
	     aStep->GetPostStepPoint()->GetPosition().x(),
	     aStep->GetPostStepPoint()->GetPosition().y(),
	     aStep->GetPostStepPoint()->GetPosition().z(),
             aStep->GetPostStepPoint()->GetGlobalTime(),
	     Track->GetParticleDefinition()->GetParticleName().data(),
	     aStep->GetPreStepPoint()->GetPhysicalVolume()->GetName().data(),
	     aStep->GetPostStepPoint()->GetPhysicalVolume()->GetName().data());
    }
  }
}
