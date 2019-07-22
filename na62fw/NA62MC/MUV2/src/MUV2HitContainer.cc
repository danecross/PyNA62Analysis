#include "MUV2HitContainer.hh"
#include "MUV2Hit.hh"
#include "MUV2GeometryParameters.hh"

/// \class MUV2HitContainer
/// \Brief
/// MUV2HitContainer class.
/// \EndBrief
///
/// \Detailed
/// This class takes care about the his in one channel. The channels are segmented in space (along the scintillators) into 10 cells.
/// Each cell can contain one ore more hits segmented in 10 ns sections.
///
///
/// \EndDetailed

// Constructor
MUV2HitContainer::MUV2HitContainer(G4ThreeVector HitPosition, G4double Energy,
                                   G4double Time, G4double PositionOfHitInScintillatorFrame,
                                   G4int ScintillatorID, G4int ChannelID, G4int Orientation, G4int TrackID) {
	ReadGeometryParameters();
    
    fHitArray = new std::vector<MUV2Hit*> [fHitContainerDimension];
    //	The first hit is created
	G4int ArrayIndex = (G4int) ((HitPosition[Orientation] + fScintLengthStandard) / fHitContainerScintillatorSegmentation);
    //	G4cout << "ArrayIndex: " << ArrayIndex << G4endl;
	fHitArray[ArrayIndex].push_back(new MUV2Hit);
	fHitArray[ArrayIndex].at(0)->SetScintillatorID(ScintillatorID);
	fHitArray[ArrayIndex].at(0)->SetChannelID(ChannelID);
	fHitArray[ArrayIndex].at(0)->SetEnergy(Energy);
	fHitArray[ArrayIndex].at(0)->SetTime(Time);
	fHitArray[ArrayIndex].at(0)->SetPosition(HitPosition);
	fHitArray[ArrayIndex].at(0)->SetPositionInScintillator(PositionOfHitInScintillatorFrame);
    fHitArray[ArrayIndex].at(0)->SetTrackID(TrackID);
    
}

void MUV2HitContainer::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    MUV2GeometryParameters* GeoPars = MUV2GeometryParameters::GetInstance();
    //Length of scintillator
    fScintLengthStandard    = GeoPars->GetScintLengthStandard();
    fHitContainerTimeLimit = GeoPars->GetHitContainerTimeLimit();
    fHitContainerScintillatorSegmentation = GeoPars->GetHitContainerScintillatorSegmentation();
    fHitContainerDimension = GeoPars->GetHitContainerDimension();
}

// Destructor
MUV2HitContainer::~MUV2HitContainer() {
	delete [] fHitArray;
}

void MUV2HitContainer::AddData(G4ThreeVector HitPosition, G4double Energy,
                               G4double Time, G4double PositionOfHitInScintillatorFrame,
                               G4int ScintillatorID, G4int ChannelID, G4int Orientation, G4int TrackID) {
    
	ReadGeometryParameters();
    
	G4int FoundMatches = 0;
	G4int ArrayIndex = (G4int) ((HitPosition[Orientation] + fScintLengthStandard) / fHitContainerScintillatorSegmentation);
	G4int NEntries = fHitArray[ArrayIndex].size();
    
    
    //	Checking, if in this cell already hits are stored
	if (NEntries == 0) {
		fHitArray[ArrayIndex].push_back(new MUV2Hit);
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
			fHitArray[ArrayIndex].push_back(new MUV2Hit);
			fHitArray[ArrayIndex].at(NEntries)->SetScintillatorID(
                                                                  ScintillatorID);
			fHitArray[ArrayIndex].at(NEntries)->SetChannelID(ChannelID);
			fHitArray[ArrayIndex].at(NEntries)->SetEnergy(Energy);
			fHitArray[ArrayIndex].at(NEntries)->SetTime(Time);
			fHitArray[ArrayIndex].at(NEntries)->SetPosition(HitPosition);
			fHitArray[ArrayIndex].at(NEntries)->SetPositionInScintillator(PositionOfHitInScintillatorFrame);
            fHitArray[ArrayIndex].at(NEntries)->SetTrackID(TrackID);
		}
        
	}
}

void MUV2HitContainer::InsertCollection(MUV2HitsCollection* HitsCollection) {
    
	for (G4int i = 0; i < fHitContainerDimension; i++) {
		G4int NEntries = fHitArray[i].size();
		if (NEntries != 0) {
			for(G4int j = 0; j < NEntries; j++){
				HitsCollection->insert(fHitArray[i].at(j));
			}
		}
        
	}
}

void MUV2HitContainer::Clear(){
	for (G4int i = 0; i < fHitContainerDimension; i++) {
		fHitArray[i].clear();
	}
}
