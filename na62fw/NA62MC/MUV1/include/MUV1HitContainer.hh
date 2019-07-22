#ifndef MUV1HitContainer_h
#define MUV1HitContainer_h 1

#include "MUV1Hit.hh"


class MUV1HitContainer{

public:
	MUV1HitContainer(G4ThreeVector HitPosition, G4double Energy, G4double Time, G4double PositionOfHitInScintillatorFrame, G4int ScintillatorID, G4int ChannelID, G4int Orientation, G4int TrackID);
	~MUV1HitContainer();
	void AddData(G4ThreeVector HitPosition, G4double Energy, G4double Time, G4double PositionOfHitInScintillatorFrame, G4int ScintillatorID, G4int ChannelID, G4int Orientation, G4int TrackID);
	void InsertCollection(MUV1HitsCollection* HitsCollection);
	void ReadGeometryParameters();
	void Clear();

private:
	std::vector<MUV1Hit*> *fHitArray;
	G4double fScintLengthStandard;
	G4double fHitContainerTimeLimit;
	G4double fHitContainerScintillatorSegmentation;
    G4int fHitContainerDimension;
};


#endif
