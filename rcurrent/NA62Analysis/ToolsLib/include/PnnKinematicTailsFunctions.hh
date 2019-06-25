#ifndef PNNKINEMATICTAILSFUNCTIONS_HH
#define PNNKINEMATICTAILSFUNCTIONS_HH

#include "SpectrometerMUV3AssociationOutput.hh"
#include "SpectrometerRICHAssociationOutputSingleRing.hh"
#include "TRecoMUV3Event.hh"
#include "TRecoCHODEvent.hh"

bool EvaluateLKrCluster(double, double, double, double, double, double, double, double, double, double, double, double, bool);

bool isPositronEoP(double, double, double, bool);

bool isMuonMUV3Candidates(TRecoMUV3Event*, double, double, bool);

bool isMuonMUV3Associations(SpectrometerMUV3AssociationOutput*, double, double, bool);

bool isMuonProbability(double, double, bool);

bool isMuonMIP(double, double, bool);

bool isPionProbability(double, double, bool);

bool isPionCaloEnergy(double, double, bool);

bool isExtraCaloEnergy(double, double, double, bool);

bool isPionCellSeed(double, double, double, double, double, double, bool);

bool isPionRICH(SpectrometerRICHAssociationOutput*, SpectrometerRICHAssociationOutputSingleRing*, double, double, double, bool);

void ApplyBlueTube(int, TVector3, TVector3, double, TVector3*, TVector3*);

TVector3 GetPositionAtZ(TVector3, TVector3, double);

TVector3 MomAfterKick(TVector3, double);

double momRICH(SpectrometerRICHAssociationOutputSingleRing*, int, time_t);

TVector3 GetVertexCDA(TVector3, TVector3, TVector3, TVector3);

void GetSimpleVertex(int, TVector3, TVector3, TVector3, TVector3, TVector3*, TVector3*, TVector3*, TVector3*, TVector3*);

TVector3 GetIterativeVertex(double, TVector3, TVector3, double, TVector3, TVector3, TVector3*, TVector3*, TVector3*, TVector3*, double dist = 5000., bool verb = false);

TVector3 GetRadoVertex(double, TVector3, TVector3, double, TVector3, TVector3, TVector3*, TVector3*, TVector3*, TVector3*);

TVector3 GetLSFVertex(double, TRecoSpectrometerCandidate*, double, TRecoGigaTrackerCandidate*, TVector3*, TVector3*, TVector3*, TVector3*, double&);

double GetCDA(TRecoSpectrometerCandidate*, TRecoSpectrometerCandidate*);

double GetCDA(TVector3, TVector3, TVector3, TVector3);

double CorrectLKrCellCluster(int, double);

void SortCHODHits(TRecoCHODEvent*, std::array<std::vector<int>, 4>&, std::array<std::vector<int>, 4>&);

int WhichCHODQuadrant(TVector2);

int NCHODSlabs(TRecoCHODEvent*, double, int, double);

void ReadOffsets(double&, double&);

void ReadSlewCorrections(double[128][16], double[128][16], bool);

void ReadLightVelocities(double*);

void ReadSlabCenters(double*);

void ReadSlabWidths(double*);

void CorrectCHODHitsTime(TRecoCHODEvent*, int, int, double*, double*, double[128][16], double[128][16], bool, double&, double&);

double LVCorrection(int, int, double*, double*);

#endif
