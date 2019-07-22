#ifndef MUV2HitContainer_h
#define MUV2HitContainer_h 1

#include "MUV2Hit.hh"

class MUV2HitContainer{

public:
	MUV2HitContainer(G4ThreeVector HitPosition, G4double Energy, G4double Time, G4double PositionOfHitInScintillatorFrame, G4int ScintillatorID, G4int ChannelID, G4int Orientation, G4int TrackID);
	~MUV2HitContainer();
	void AddData(G4ThreeVector HitPosition, G4double Energy, G4double Time, G4double PositionOfHitInScintillatorFrame, G4int ScintillatorID, G4int ChannelID, G4int Orientation, G4int TrackID);
	void InsertCollection(MUV2HitsCollection* HitsCollection);
	void ReadGeometryParameters();
	void Clear();
private:
	std::vector<MUV2Hit*> *fHitArray;
	G4double fScintLengthStandard;
	G4double fHitContainerTimeLimit;
	G4double fHitContainerScintillatorSegmentation;
    G4int fHitContainerDimension;
};


#endif
