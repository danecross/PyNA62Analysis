#include "MUV1HitContainer.hh"
#include "MUV1Hit.hh"
#include "MUV1GeometryParameters.hh"

/// \class MUV1HitContainer
/// \Brief
/// MUV1HitContainer class.
/// \EndBrief
///
/// \Detailed
/// This class takes care about the hits in one channel. The channels are segmented in space (along the scintillators) into 10 cells.
/// Each cell can contain one ore more hits segmented in 10 ns sections.
///
///
/// \EndDetailed

// Constructor
MUV1HitContainer::MUV1HitContainer(G4ThreeVector HitPosition, G4double Energy,
		G4double Time, G4double PositionOfHitInScintillatorFrame,
		G4int ScintillatorID, G4int ChannelID, G4int Orientation, G4int TrackID) {
	ReadGeometryParameters();
//	The first hit is created
    fHitArray = new std::vector <MUV1Hit*> [fHitContainerDimension];
	G4int ArrayIndex = (G4int) ((HitPosition[Orientation] + (fScintLengthStandard*0.5)) / fHitContainerScintillatorSegmentation);
	fHitArray[ArrayIndex].push_back(new MUV1Hit);
	fHitArray[ArrayIndex].at(0)->SetScintillatorID(ScintillatorID);
	fHitArray[ArrayIndex].at(0)->SetChannelID(ChannelID);
	fHitArray[ArrayIndex].at(0)->SetEnergy(Energy);
	fHitArray[ArrayIndex].at(0)->SetTime(Time);
	fHitArray[ArrayIndex].at(0)->SetPosition(HitPosition);
	fHitArray[ArrayIndex].at(0)->SetPositionInScintillator(
			PositionOfHitInScintillatorFrame);
        fHitArray[ArrayIndex].at(0)->SetTrackID(TrackID);


}

// Destructor
MUV1HitContainer::~MUV1HitContainer() {
	delete [] fHitArray;
}

void MUV1HitContainer::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();
  //Length of scintillator
  fScintLengthStandard    = GeoPars->GetScintLengthStandard();
  fHitContainerTimeLimit = GeoPars->GetHitContainerTimeLimit();
  fHitContainerScintillatorSegmentation = GeoPars->GetHitContainerScintillatorSegmentation();
  fHitContainerDimension = GeoPars->GetHitContainerDimension();
    //G4cout <<"fHitContainerDimension = "<<fHitContainerDimension<<G4endl;
}

void MUV1HitContainer::AddData(G4ThreeVector HitPosition, G4double Energy,
		G4double Time, G4double PositionOfHitInScintillatorFrame,
		G4int ScintillatorID, G4int ChannelID, G4int Orientation, G4int TrackID) {
	ReadGeometryParameters();

	G4int FoundMatches = 0;
	G4int ArrayIndex = (G4int) ((HitPosition[Orientation] + (fScintLengthStandard*0.5)) / fHitContainerScintillatorSegmentation);
    //*----*/G4cout <<"ArrayIndex  = "<<ArrayIndex<<std::endl;
	G4int NEntries = fHitArray[ArrayIndex].size();

//	Checking, if in this cell already hits are stored
	if (NEntries == 0) {
		fHitArray[ArrayIndex].push_back(new MUV1Hit);
		fHitArray[ArrayIndex].at(NEntries)->SetScintillatorID(ScintillatorID);
		fHitArray[ArrayIndex].at(NEntries)->SetChannelID(ChannelID);
		fHitArray[ArrayIndex].at(NEntries)->SetEnergy(Energy);
		fHitArray[ArrayIndex].at(NEntries)->SetTime(Time);
		fHitArray[ArrayIndex].at(NEntries)->SetPosition(HitPosition);
		fHitArray[ArrayIndex].at(NEntries)->SetPositionInScintillator(
				PositionOfHitInScintillatorFrame);
                fHitArray[ArrayIndex].at(NEntries)->SetTrackID(TrackID);
	} else {
//	If in this cell already hits are, this are checked if the actual one is in the time window
		for (G4int i = 0; i < NEntries; i++) {
			if (fabs(fHitArray[ArrayIndex].at(i)->GetTime() - Time)
					< fHitContainerTimeLimit) {
				fHitArray[ArrayIndex].at(i)->AddHit(HitPosition, Energy, Time, PositionOfHitInScintillatorFrame, TrackID);
				FoundMatches++;
				break;
			}
		}
//		If there have not been hits in the correct time window a new hit is created
		if (FoundMatches == 0) {
			fHitArray[ArrayIndex].push_back(new MUV1Hit);
			fHitArray[ArrayIndex].at(NEntries)->SetScintillatorID(
					ScintillatorID);
			fHitArray[ArrayIndex].at(NEntries)->SetChannelID(ChannelID);
			fHitArray[ArrayIndex].at(NEntries)->SetEnergy(Energy);
			fHitArray[ArrayIndex].at(NEntries)->SetTime(Time);
			fHitArray[ArrayIndex].at(NEntries)->SetPosition(HitPosition);
			fHitArray[ArrayIndex].at(NEntries)->SetPositionInScintillator(
					PositionOfHitInScintillatorFrame);
                        fHitArray[ArrayIndex].at(NEntries)->SetTrackID(TrackID);
		}

	}
}

void MUV1HitContainer::InsertCollection(MUV1HitsCollection* HitsCollection) {

	for (G4int i = 0; i < fHitContainerDimension; i++) {
		G4int NEntries = fHitArray[i].size();
		if (NEntries != 0) {
			for(G4int j = 0; j < NEntries; j++){
				HitsCollection->insert(fHitArray[i].at(j));
			}
		}

	}
}

void MUV1HitContainer::Clear(){
	for (G4int i = 0; i < fHitContainerDimension; i++) {
		fHitArray[i].clear();
	}
}
