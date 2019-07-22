/// \class DownstreamTrack
/// \Brief
/// A container for the results of spectrometer track association to downstream subdetectors
/// \EndBrief
/// \Detailed
/// Contains spectrometer track geometry and kinematics, and the results of track association to CHOD,
/// NewCHOD, RICH, LKr, and MUV1-3 hits and/or candidates (including pointers to associated candidates).
/// Usage example:
/// \code
/// std::vector<DownstreamTrack> Tracks =*(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
/// \endcode
/// DownstreamTracks are matched to MCTruth particles (see DownstreamTrackBuilder). 
/// To find the KinePart ID of the MCTruth particle matched to this DownstreamTrack use: 
/// \code
/// Int_t MCTruthParticleID = Tracks[i].GetMatchedKinePartIndex();
/// \endcode
/// and to get the value of the matching Chi2 use:
/// \code 
/// Double_t MatchingChi2 = Tracks[i].GetMatchedChi2();
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include <iostream>

using namespace std;

DownstreamTrack::DownstreamTrack() {
  Clear();
}

DownstreamTrack::~DownstreamTrack() {
  Clear();
}

void DownstreamTrack::Clear() {
  fTrackID = -1;
  fSpectrometerCandidate = nullptr;
  fCHANTICandidates.clear();
  fCHODCandidates.clear();
  fNewCHODCandidates.clear();
  fRICHSingleRingTrkSeededCandidate = nullptr;
  fLKrCandidates.clear();
  fMUV1Candidate = nullptr;
  fMUV2Candidate = nullptr;
  fMUV3CandidateIndices.clear();
  fMUV3Candidates.clear();

  fNChambers = 0;
  fCharge = 0;
  fChi2 = 0.0;
  fMomentum = 0.0;
  fMomentumBeforeFit = 0.0;
  fMomentumBeforeMagnet.SetXYZ(0.0,0.0,0.0);
  fMomentumAfterMagnet.SetXYZ (0.0,0.0,0.0);
  fPositionBeforeMagnet.SetXYZ(0.0,0.0,0.0);
  fPositionAfterMagnet.SetXYZ (0.0,0.0,0.0);
  fSlopeXBeforeMagnet = 0.0;
  fSlopeYBeforeMagnet = 0.0;
  fSlopeXAfterMagnet = 0.0;
  fSlopeYAfterMagnet = 0.0;
  for (Int_t i=0; i<5; i++) for (Int_t j=0; j<5; j++) fCovariance[i][j] = 0.0;
  fBeamAxisVertex.SetXYZ(0.0,0.0,0.0);
  fBeamAxisCDA = 0.0;
  fNominalBeamAxisVertex.SetXYZ(0.0,0.0,0.0);
  fNominalBeamAxisCDA = 0.0;
  fTrackTime = 0.0;
  fTrackLeadingTime = 0.0;
  fIsFake = false;

  fCHANTIAssociationOutput.Clear();
  fCHODAssociationOutput.Clear();

  fRICHAssociationSuccessful = false;
  fTrackTimeForRICHAssociation = 0.0;
  fRICHNumberOfInTimeHits = 0;
  fRICHNumberOfOutOfTimeHits = 0;
  fRICHMostLikelyHypothesis = -999;
  fRICHRingPredictedCentrePosition.Set(0.0,0.0);
  fRICHRingCentrePosition.Set(0.0, 0.0);
  fRICHRingCentrePositionError.Set(0.0, 0.0);
  fRICHRingRadius = 0.0;
  fRICHRingRadiusError = 0.0;
  fRICHRingFitChi2 = 0.0;
  for (Int_t i=0; i<MaxHypos; i++) {
    fRICHLikelihood[i] = 1.0;
    fRICHRingTime[i] = -999;
    fRICHRingNHits[i] = 0;
    fRICHRingPredictedNHits[i] = 0.0;
    fRICHRingPredictedRadius[i] = 0.0;
    fRICHAssignedHits[i].clear();
  }

  fRICHSingleRingTrkSeededCandidateID = -1;
  fRICHSingleRingTrkSeededCentrePosition.Set(-9999., -9999.);
  fRICHSingleRingTrkSeededCentrePositionError.Set(-9999.,-9999.);
  fRICHSingleRingTrkSeededRadius = -99.;
  fRICHSingleRingTrkSeededRadiusError = -99.;
  fRICHSingleRingTrkSeededFitChi2 = -1.;
  fRICHSingleRingTrkSeededTrkDist = -99.;
  fRICHSingleRingTrkSeededTime = -999.;
  fRICHSingleRingTrkSeededMass = -999999.9;
  fRICHSingleRingTrkSeededMass2 = -999999.9;
  fRICHSingleRingTrkSeededNHits = -1;

  fRICHSingleRingTrkCentredCandidateID = -99999;
  fRICHSingleRingTrkCentredCentrePosition.Set(-9999., -9999.);
  fRICHSingleRingTrkCentredRadius = -99999.;
  fRICHSingleRingTrkCentredFitChi2 = -99999.;
  fRICHSingleRingTrkCentredTime = -99999.;
  fRICHSingleRingTrkCentredMass = -999999.;
  fRICHSingleRingTrkCentredNHits = -999999;

  fLKrAssociationOutput.Clear();
  fClustersForWhichThisTrackIsBestMatch.clear(); // EnergyCluster best matches

  fMUV1SimpleAssociationExists = false;
  fMUV1SimpleAssociationTime = -99999.;
  fMUV1SimpleAssociationEnergy = 0.0;
  fMUV1SimpleAssociationPosition.Set(0.0,0.0);
  fMUV2SimpleAssociationExists = false;
  fMUV2SimpleAssociationTime = -99999.;
  fMUV2SimpleAssociationEnergy = 0.0;
  fMUV2SimpleAssociationPosition.Set(0.0,0.0);

  fMUV1AssociationExists = false;
  fMUV1ClusterPosition.Set(0.0,0.0);
  fMUV1ClusterTime = -99999.;
  fMUV1ClusterEnergy = 0.0;

  fMUV2AssociationExists = false;
  fMUV2ClusterPosition.Set(0.0,0.0);
  fMUV2ClusterTime = -99999.;
  fMUV2ClusterEnergy = 0.0;
  fCalorimetricEnergy = 0.0;

  fNewCHODSearchRadius = 0.0;
  fNewCHODBestRecordID = -1;
  fMUV3SearchRadius = 0.0;

  fMatchedKinePartIndex = -999; /// -999 for data, -1 for MC without match
  fMatchedKinePartChi2 = 99999.;
  fTrueThreeMomentum.SetXYZ(0.0, 0.0, 0.0);
}

// Compute the velocity for a given mass hypothesis
Double_t DownstreamTrack::GetBeta(Double_t mass) {
  return fMomentum / sqrt(fMomentum*fMomentum + mass*mass);
}

Double_t DownstreamTrack::xAtBeforeMagnet(Double_t z) {
  return fPositionBeforeMagnet.X()+ fSlopeXBeforeMagnet*(z-fPositionBeforeMagnet.Z());
}
Double_t DownstreamTrack::yAtBeforeMagnet(Double_t z) {
  return fPositionBeforeMagnet.Y()+ fSlopeYBeforeMagnet*(z-fPositionBeforeMagnet.Z());
}
Double_t DownstreamTrack::rAtBeforeMagnet(Double_t z) {
  Double_t x = xAtBeforeMagnet(z);
  Double_t y = yAtBeforeMagnet(z);
  return sqrt(x*x+y*y);
}

Double_t DownstreamTrack::xAtAfterMagnet(Double_t z) {
  return fPositionAfterMagnet.X()+ fSlopeXAfterMagnet*(z-fPositionAfterMagnet.Z());
}
Double_t DownstreamTrack::yAtAfterMagnet(Double_t z) {
  return fPositionAfterMagnet.Y()+ fSlopeYAfterMagnet*(z-fPositionAfterMagnet.Z());
}
Double_t DownstreamTrack::rAtAfterMagnet(Double_t z) {
  Double_t x = xAtAfterMagnet(z);
  Double_t y = yAtAfterMagnet(z);
  return sqrt(x*x+y*y);
}

Double_t DownstreamTrack::xAt(Double_t z) {
  return (z<196995.0) ? xAtBeforeMagnet(z) : xAtAfterMagnet(z);
}
Double_t DownstreamTrack::yAt(Double_t z) {
  return (z<196995.0) ? yAtBeforeMagnet(z) : yAtAfterMagnet(z);
}
Double_t DownstreamTrack::rAt(Double_t z) {
  return (z<196995.0) ? rAtBeforeMagnet(z) : rAtAfterMagnet(z);
}

void DownstreamTrack::Print() {
  if (fMatchedKinePartIndex!=-999) // means MC
    cout << "KinePart index= " << fMatchedKinePartIndex << "; KinePart match chi2= " << fMatchedKinePartChi2 << endl;
  cout << "q*p= " << fCharge*fMomentum << " MeV/c" << endl;
  cout << "x,y(STRAW1)= " << xAtBeforeMagnet(183508.0) << " " << yAtBeforeMagnet(183508.0) <<
    "; x,y(LKr)= " << xAtAfterMagnet(241093.0) << " " << yAtAfterMagnet(241093.0) <<
    "; x,y(MUV3)= " << xAtAfterMagnet(246800.0) << " " << yAtAfterMagnet(246800.0) << endl;
  //cout << "x,y(LKr-undeflected)= " << xAtBeforeMagnet(241093.0) << " " << yAtBeforeMagnet(241093.0) << endl;
  cout << "E/p (total)= " << GetLKrEoP() << " (" << GetLKrTotalEoP() <<
    "); N(clust): " << GetLKrNAssociatedClusters() <<
    "; D(clust) [mm]: " << GetLKrClusterDistance() <<
    "; t(clust) [ns]: " << GetLKrClusterTime() <<
    "; D(DeadCell) [mm]: " << GetLKrClusterDDeadCell() << endl;
  cout << "Straw, CHOD, NewCHOD, RICH times [ns]: " << fTrackTime << " " <<
    GetCHODTime() << " " << GetNewCHODTime() << " " << GetRICHTime() << endl;
  cout << "MUV3 search radius [mm]: " << fMUV3SearchRadius <<"; N(MUV3 candidates)= " << fMUV3Candidates.size() << endl;
  PrintRICHInformation();
  cout << "----" << endl;
}

/////////////////
// CHANTI methods

TRecoCHANTICandidate* DownstreamTrack::GetCHANTICandidate(UInt_t index) {
  if (index >= fCHANTICandidates.size()) {
    cout << "[DownstreamTrack::GetCHANTICandidate] index exceeds vector size" << endl;
    return nullptr;
  }
  return fCHANTICandidates[index];
}

///////////////
// RICH methods

std::vector<TRecoRICHHit*>DownstreamTrack::GetRICHAssignedHits(Int_t h) {
  if (h<0 || h>=MaxHypos) {
    cout << "[DownstreamTrack::GetRICHAssignedHits] Invalid hypothesis" << endl;
    std::vector<TRecoRICHHit*> tmp;
    return tmp;
  }
  return fRICHAssignedHits[h];
}

void DownstreamTrack::SetRICHAssignedHits(Int_t i, const std::vector<TRecoRICHHit*> &val) {
  fRICHAssignedHits[i] = val;
}

void DownstreamTrack::PrintRICHInformation() {
  cout << "RICH association successful? " << (fRICHAssociationSuccessful ? "yes" : "no") << endl;
  TString hyp = "none";
  if      (fRICHMostLikelyHypothesis== 0) hyp = "bkg";
  else if (fRICHMostLikelyHypothesis== 1) hyp = "e";
  else if (fRICHMostLikelyHypothesis== 2) hyp = "mu";
  else if (fRICHMostLikelyHypothesis== 3) hyp = "pi";
  else if (fRICHMostLikelyHypothesis== 4) hyp = "K";
  else if (fRICHMostLikelyHypothesis==99) hyp = "multiple";
  cout << Form("RICH log-likelihoods (bkg/e/mu/pi/K): %.2f %.2f %.2f %.2f %.2f; MostLikelyHyp = %s\n",
	       log10(fRICHLikelihood[kRICHHypothesisBackground]),
	       log10(fRICHLikelihood[kRICHHypothesisElectron]),
	       log10(fRICHLikelihood[kRICHHypothesisMuon]),
	       log10(fRICHLikelihood[kRICHHypothesisPion]),
	       log10(fRICHLikelihood[kRICHHypothesisKaon]),
	       hyp.Data());
  cout << "Time used by SpectrometerRICHAssociation: " << fTrackTimeForRICHAssociation << endl;
  cout << "Numbers of RICH in-time, out-of-time hits: " <<
    fRICHNumberOfInTimeHits << ", " << fRICHNumberOfOutOfTimeHits << endl;
  cout << Form("RICH N(hits) expected (bkg/e/mu/pi/K): %.2f %.2f %.2f %.2f %.2f\n",
	       fRICHRingPredictedNHits[kRICHHypothesisBackground],
	       fRICHRingPredictedNHits[kRICHHypothesisElectron],
	       fRICHRingPredictedNHits[kRICHHypothesisMuon],
	       fRICHRingPredictedNHits[kRICHHypothesisPion],
	       fRICHRingPredictedNHits[kRICHHypothesisKaon]);
  cout << Form("RICH N(hits) assigned (bkg/e/mu/pi/K): %d %d %d %d %d\n",
	       fRICHRingNHits[kRICHHypothesisBackground],
	       fRICHRingNHits[kRICHHypothesisElectron],
	       fRICHRingNHits[kRICHHypothesisMuon],
	       fRICHRingNHits[kRICHHypothesisPion],
	       fRICHRingNHits[kRICHHypothesisKaon]);
  cout << Form("RICH times (bkg/e/mu/pi/K): %.2f %.2f %.2f %.2f %.2f; time(MostLikelyHyp) = %.2f\n",
	       fRICHRingTime[kRICHHypothesisBackground],
	       fRICHRingTime[kRICHHypothesisElectron],
	       fRICHRingTime[kRICHHypothesisMuon],
	       fRICHRingTime[kRICHHypothesisPion],
	       fRICHRingTime[kRICHHypothesisKaon],
	       GetRICHRingTime(fRICHMostLikelyHypothesis));
  cout << Form("RICH expected ring centre: %.2f %.2f\n",
	       fRICHRingPredictedCentrePosition.X(), fRICHRingPredictedCentrePosition.Y());
  cout << Form("RICH expected ring radii (bkg/e/mu/pi/K): %.2f %.2f %.2f %.2f %.2f\n",
	       fRICHRingPredictedRadius[kRICHHypothesisBackground],
	       fRICHRingPredictedRadius[kRICHHypothesisElectron],
	       fRICHRingPredictedRadius[kRICHHypothesisMuon],
	       fRICHRingPredictedRadius[kRICHHypothesisPion],
	       fRICHRingPredictedRadius[kRICHHypothesisKaon]);
  cout << Form("RICH ring fit (best hypothesis): x,y,R = %.2f %.2f %.2f\n",
	       fRICHRingCentrePosition.X(), fRICHRingCentrePosition.Y(), fRICHRingRadius);

  if (fRICHMostLikelyHypothesis>=1 && fRICHMostLikelyHypothesis<=4 && // e,mu,pi,K
      fRICHRingNHits[fRICHMostLikelyHypothesis]) {
    cout << "RICH hit (x,y) assigned to best hypothesis:" << endl;
    vector<Double_t>x, y;
    for (Int_t iHit=0; iHit<fRICHRingNHits[fRICHMostLikelyHypothesis]; iHit++) {
      x.push_back(fRICHAssignedHits[fRICHMostLikelyHypothesis].at(iHit)->GetFitPosition().X());
      y.push_back(fRICHAssignedHits[fRICHMostLikelyHypothesis].at(iHit)->GetFitPosition().Y());
    }
    for (Int_t iHit=0; iHit<fRICHRingNHits[fRICHMostLikelyHypothesis]; iHit++) {
      cout << Form(" (%.2f, %.2f)", x[iHit], y[iHit]);
    }
    cout << endl;
    cout << "RICH hit distances from expected/fitted rings:" << endl;
    for (Int_t iHit=0; iHit<fRICHRingNHits[fRICHMostLikelyHypothesis]; iHit++) {
      Double_t Rexp = sqrt(pow(x[iHit]-fRICHRingPredictedCentrePosition.X(),2) +
			   pow(y[iHit]-fRICHRingPredictedCentrePosition.Y(),2));
      Rexp -= fRICHRingPredictedRadius[fRICHMostLikelyHypothesis];
      Rexp = fabs(Rexp);
      Double_t Rfit = sqrt(pow(x[iHit]-fRICHRingCentrePosition.X(),2) +
			   pow(y[iHit]-fRICHRingCentrePosition.Y(),2));
      Rfit -= fRICHRingRadius;
      Rfit = fabs(Rfit);
      cout << Form(" %.2f/%.2f", Rexp, Rfit);
    }
    cout << endl;
  }
}

///////////////
// CHOD methods

TRecoCHODCandidate* DownstreamTrack::GetCHODCandidate(UInt_t index) {
  if (index >= fCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetCHODCandidate] index exceeds vector size" << endl;
    return nullptr;
  }
  return fCHODCandidates[index];
}

TVector3 DownstreamTrack::GetCHODCandidatePosition(UInt_t index) { 
  if (index >= fCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetCHODCandidatePosition] index exceeds vector size" << endl;
    return TVector3(0.0,0.0,0.0);
  }
  TVector2 v2 = fCHODCandidates[index]->GetHitPosition();
  TVector3 v3(v2.X(), v2.Y(), GeometricAcceptance::GetInstance()->GetZCHODVPlane());
  return v3;
}

Double_t DownstreamTrack::GetCHODCandidateTime(UInt_t index) {
  if (index >= fCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetCHODCandidateTime] index exceeds vector size" << endl;
    return 0.0;
  }
  return fCHODCandidates[index]->GetTime();
}

Double_t DownstreamTrack::GetCHODTime() { // based on "best" record
  if (fCHODAssociationOutput.GetBestAssociationRecord())
    return fCHODAssociationOutput.GetBestAssociationRecord()->GetCHODCandidate()->GetTime();
  return -999.0;
}

Bool_t DownstreamTrack::CHODTimeExists() {
  if (fCHODAssociationOutput.GetBestAssociationRecord()) return true;
  return false;
}

//////////////////
// NewCHOD methods

TRecoNewCHODHit* DownstreamTrack::GetNewCHODCandidate(UInt_t index) {
  if (index >= fNewCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetNewCHODCandidate] index exceeds vector size" << endl;
    return nullptr;
  }
  return fNewCHODCandidates[index];
}

TVector3 DownstreamTrack::GetNewCHODCandidatePosition(UInt_t index) {
  if (index >= fNewCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetNewCHODCandidatePosition] index exceeds vector size" << endl;
    return TVector3(0.0,0.0,0.0);
  }
  return fNewCHODCandidates[index]->GetPosition();
}

Int_t DownstreamTrack::GetNewCHODCandidateTileID(UInt_t index) {
  if (index >= fNewCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetNewCHODCandidateTileID] index exceeds vector size" << endl;
    return -1;
  }
  return fNewCHODCandidates[index]->GetChannelID();
}

Double_t DownstreamTrack::GetNewCHODCandidateX(UInt_t index) {
  if (index >= fNewCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetNewCHODCandidateX] index exceeds vector size" << endl;
    return 0.0;
  }
  return fNewCHODCandidates[index]->GetPosition().X();
}

Double_t DownstreamTrack::GetNewCHODCandidateY(UInt_t index) {
  if (index >= fNewCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetNewCHODCandidateY] index exceeds vector size" << endl;
    return 0.0;
  }
  return fNewCHODCandidates[index]->GetPosition().Y();
}

Double_t DownstreamTrack::GetNewCHODCandidateTime(UInt_t index) {
  if (index >= fNewCHODCandidates.size()) {
    cout << "[DownstreamTrack::GetNewCHODCandidateTime] index exceeds vector size" << endl;
    return 0.0;
  }
  return fNewCHODCandidates[index]->GetTime();
}

Double_t DownstreamTrack::GetNewCHODTime() { // based on the "best" record
  if (fNewCHODBestRecordID>=0) return fNewCHODCandidates[fNewCHODBestRecordID]->GetTime();
  return -999.0;
}

Bool_t DownstreamTrack::NewCHODTimeExists() {
  return (fNewCHODBestRecordID>=0);
}

//////////////
// LKr methods

TRecoLKrCandidate* DownstreamTrack::GetLKrCandidate(UInt_t index) {
  if (index >= fLKrCandidates.size()) {
    cout << "[DownstreamTrack::GetLKrCandidate] index exceeds vector size" << endl;
    return nullptr;
  }
  return fLKrCandidates[index];
}

Double_t DownstreamTrack::GetLKrEnergy() {
  if (fLKrAssociationOutput.GetBestAssociationRecord())
    return fLKrAssociationOutput.GetBestAssociationRecord()->GetLKrCandidate()->GetClusterEnergy();
  return 0.0;
}

Double_t DownstreamTrack::GetLKrEoP() {
  if (fLKrAssociationOutput.GetBestAssociationRecord())
    return fLKrAssociationOutput.GetBestAssociationRecord()->GetEoP();
  return 0.0;
}

Double_t DownstreamTrack::GetLKrClusterX() {
  if (fLKrAssociationOutput.GetBestAssociationRecord())
    return fLKrAssociationOutput.GetBestAssociationRecord()->GetLKrCandidate()->GetClusterX();
  return 99999.9;
}

Double_t DownstreamTrack::GetLKrClusterY() {
  if (fLKrAssociationOutput.GetBestAssociationRecord())
    return fLKrAssociationOutput.GetBestAssociationRecord()->GetLKrCandidate()->GetClusterY();
  return 99999.9;
}

Double_t DownstreamTrack::GetLKrClusterDistance() {
  if (fLKrAssociationOutput.GetBestAssociationRecord()) {
    Double_t xc = fLKrAssociationOutput.GetBestAssociationRecord()->GetLKrCandidate()->GetClusterX();
    Double_t yc = fLKrAssociationOutput.GetBestAssociationRecord()->GetLKrCandidate()->GetClusterY();
    Double_t xt = xAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZLKr());
    Double_t yt = yAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZLKr());
    return sqrt((xc-xt)*(xc-xt)+(yc-yt)*(yc-yt));
  }
  return 99999.9;
}

Double_t DownstreamTrack::GetLKrClusterTime() {
  if (fLKrAssociationOutput.GetBestAssociationRecord())
    return fLKrAssociationOutput.GetBestAssociationRecord()->GetLKrCandidate()->GetTime();
  return 99999.9;
}

Double_t DownstreamTrack::GetLKrClusterDDeadCell() {
  if (fLKrAssociationOutput.GetBestAssociationRecord())
    return fLKrAssociationOutput.GetBestAssociationRecord()->GetLKrCandidate()->GetClusterDDeadCell();
  return 99999.9;
}

Int_t DownstreamTrack::GetLKrXCellID() {
  Int_t XCellID = xAt(GeometricAcceptance::GetInstance()->GetZLKr())/
    GeometricAcceptance::GetInstance()->GetLKrCellSize()+0.5*127;
  if (0<=XCellID && XCellID<128) return XCellID;
  return -1;
}

Int_t DownstreamTrack::GetLKrYCellID() {
  Int_t YCellID = yAt(GeometricAcceptance::GetInstance()->GetZLKr())/
    GeometricAcceptance::GetInstance()->GetLKrCellSize()+0.5*127;
  if (0<=YCellID && YCellID<128) return YCellID;
  return -1;
}

EnergyCluster* DownstreamTrack::GetClusterForWhichThisTrackIsBestMatch(UInt_t i) {
  if (i<fClustersForWhichThisTrackIsBestMatch.size())
    return fClustersForWhichThisTrackIsBestMatch[i];
  return nullptr;
}

///////////////
// MUV3 methods

TRecoMUV3Candidate* DownstreamTrack::GetMUV3Candidate(UInt_t i) {
  if (i >= fMUV3Candidates.size()) {
    cout << "[DownstreamTrack::GetMUV3Candidate] index exceeds vector size" << endl;
    return nullptr;
  }
  return fMUV3Candidates[i];
}

std::vector<TRecoMUV3Candidate*>& DownstreamTrack::GetMUV3Candidates() {
  return fMUV3Candidates;
}

Int_t DownstreamTrack::GetMUV3CandidateIndex(Int_t i) {
  if (i<0) return -1;
  if ((UInt_t)i >= fMUV3CandidateIndices.size()) {
    cout << "[DownstreamTrack::GetMUV3Candidate] index exceeds vector size" << endl;
    return -1;
  }
  return fMUV3CandidateIndices[i];
}

Bool_t DownstreamTrack::MUV3InnerAssociationExists() {
  return (GetNMUV3InnerAssociationRecords()>0);
}

Bool_t DownstreamTrack::MUV3OuterAssociationExists() {
  return (GetNMUV3OuterAssociationRecords()>0);
}

Bool_t DownstreamTrack::MUV3InTimeAssociationExists(Double_t RefTime, Double_t HalfWindow) {
  return (GetNMUV3InTimeAssociationRecords(RefTime, HalfWindow)>0);
}

Bool_t DownstreamTrack::MUV3InTimeInnerAssociationExists(Double_t RefTime, Double_t HalfWindow) {
  return (GetNMUV3InTimeInnerAssociationRecords(RefTime, HalfWindow)>0);
}

Bool_t DownstreamTrack::MUV3InTimeOuterAssociationExists(Double_t RefTime, Double_t HalfWindow) {
  return (GetNMUV3InTimeOuterAssociationRecords(RefTime, HalfWindow)>0);
}

Bool_t DownstreamTrack::MUV3OutOfTimeAssociationExists
(Double_t RefTime, Double_t t1, Double_t t2, Double_t t3, Double_t t4) {
  return (GetNMUV3OutOfTimeAssociationRecords(RefTime, t1, t2, t3, t4)>0);
}

Bool_t DownstreamTrack::MUV3OutOfTimeInnerAssociationExists
(Double_t RefTime, Double_t t1, Double_t t2, Double_t t3, Double_t t4) {
  return (GetNMUV3OutOfTimeInnerAssociationRecords(RefTime, t1, t2, t3, t4)>0);
}

Bool_t DownstreamTrack::MUV3OutOfTimeOuterAssociationExists
(Double_t RefTime, Double_t t1, Double_t t2, Double_t t3, Double_t t4) {
  return (GetNMUV3OutOfTimeOuterAssociationRecords(RefTime, t1, t2, t3, t4)>0);
}

Int_t DownstreamTrack::GetNMUV3InnerAssociationRecords() {
  Int_t N=0;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fMUV3Candidates[i]->IsInner()) N++;
  }
  return N;
}

Int_t DownstreamTrack::GetNMUV3OuterAssociationRecords() {
  Int_t N=0;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fMUV3Candidates[i]->IsOuter()) N++;
  }
  return N;
}

Int_t DownstreamTrack::GetNMUV3InTimeAssociationRecords(Double_t RefTime, Double_t HalfWindow) {
  Int_t N=0;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fabs(fMUV3Candidates[i]->GetTime()-RefTime)<HalfWindow) N++;
  }
  return N;
}

Int_t DownstreamTrack::GetNMUV3InTimeInnerAssociationRecords(Double_t RefTime, Double_t HalfWindow) {
  Int_t N=0;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fMUV3Candidates[i]->IsInner() &&
	fabs(fMUV3Candidates[i]->GetTime()-RefTime)<HalfWindow) N++;
  }
  return N;
}

Int_t DownstreamTrack::GetNMUV3InTimeOuterAssociationRecords(Double_t RefTime, Double_t HalfWindow) {
  Int_t N=0;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fMUV3Candidates[i]->IsOuter() &&
	fabs(fMUV3Candidates[i]->GetTime()-RefTime)<HalfWindow) N++;
  }
  return N;
}

// These methods can be used to check track-MUV3 associations in time sidebands
Int_t DownstreamTrack::GetNMUV3OutOfTimeAssociationRecords
(Double_t RefTime, Double_t t1, Double_t t2, Double_t t3, Double_t t4) {
  Int_t N=0;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    Double_t dt = fMUV3Candidates[i]->GetTime()-RefTime;
    if ((dt>t1 && dt<t2) || (dt>t3 && dt<t4)) N++;
  }
  return N;
}

Int_t DownstreamTrack::GetNMUV3OutOfTimeInnerAssociationRecords
(Double_t RefTime, Double_t t1, Double_t t2, Double_t t3, Double_t t4) {
  Int_t N=0;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fMUV3Candidates[i]->IsInner()) {
      Double_t dt = fMUV3Candidates[i]->GetTime()-RefTime;
      if ((dt>t1 && dt<t2) || (dt>t3 && dt<t4)) N++;
    }
  }
  return N;
}

Int_t DownstreamTrack::GetNMUV3OutOfTimeOuterAssociationRecords
(Double_t RefTime, Double_t t1, Double_t t2, Double_t t3, Double_t t4) {
  Int_t N=0;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fMUV3Candidates[i]->IsOuter()) {
      Double_t dt = fMUV3Candidates[i]->GetTime()-RefTime;
      if ((dt>t1 && dt<t2) || (dt>t3 && dt<t4)) N++;
    }
  }
  return N;
}

TVector3 DownstreamTrack::GetMUV3CandidatePosition(Int_t index) {
  if (index<0) return TVector3(0.0,0.0,0.0);
  if ((UInt_t)index >= fMUV3Candidates.size()) {
    cout << "[DownstreamTrack::GetMUV3CandidatePosition] index exceeds vector size" << endl;
    return TVector3(0.0,0.0,0.0);
  }
  return fMUV3Candidates[index]->GetPosition();
}

Int_t DownstreamTrack::GetMUV3CandidateTileID(Int_t index) {
  if (index<0) return -1;
  if ((UInt_t)index >= fMUV3Candidates.size()) {
    cout << "[DownstreamTrack::GetMUV3CandidateTileID] index exceeds vector size" << endl;
    return -1;
  }
  return fMUV3Candidates[index]->GetTileID();
}

Int_t DownstreamTrack::GetMUV3InTimeCandidateTileID(Double_t RefTime, Double_t HalfWindow, Int_t index) {
  if (index<0) return -1;
  if (index >= GetNMUV3InTimeAssociationRecords(RefTime, HalfWindow)) {
    cout << "[DownstreamTrack::GetMUV3InTimeCandidateTileID] index exceeds the number of records" << endl;
    return -1;
  }
  Int_t N=-1;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fabs(fMUV3Candidates[i]->GetTime()-RefTime)<HalfWindow) N++;
    if (N==index) return fMUV3Candidates[i]->GetTileID();
  }
  return -1;
}

Int_t DownstreamTrack::GetMUV3InTimeInnerCandidateTileID(Double_t RefTime, Double_t HalfWindow, Int_t index) {
  if (index<0) return -1;
  if (index >= GetNMUV3InTimeInnerAssociationRecords(RefTime, HalfWindow)) {
    cout << "[DownstreamTrack::GetMUV3InTimeInnerCandidateTileID] index exceeds the number of records" << endl;
    return -1;
  }
  Int_t N=-1;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fMUV3Candidates[i]->IsInner() && fabs(fMUV3Candidates[i]->GetTime()-RefTime)<HalfWindow) N++;
    if (N==index) return fMUV3Candidates[i]->GetTileID();
  }
  return -1;
}

Int_t DownstreamTrack::GetMUV3InTimeOuterCandidateTileID(Double_t RefTime, Double_t HalfWindow, Int_t index) {
  if (index<0) return -1;
  if (index >= GetNMUV3InTimeOuterAssociationRecords(RefTime, HalfWindow)) {
    cout << "[DownstreamTrack::GetMUV3InTimeOuterCandidateTileID] index exceeds the number of records" << endl;
    return -1;
  }
  Int_t N=-1;
  for (UInt_t i=0; i<fMUV3Candidates.size(); i++) {
    if (fMUV3Candidates[i]->IsOuter() && fabs(fMUV3Candidates[i]->GetTime()-RefTime)<HalfWindow) N++;
    if (N==index) return fMUV3Candidates[i]->GetTileID();
  }
  return -1;
}

Double_t DownstreamTrack::GetMUV3Time(Int_t index) {
  if (index<0) return 999;
  if ((UInt_t)index >= fMUV3Candidates.size()) {
    cout << "[DownstreamTrack::GetMUV3CandidatePosition] index exceeds vector size" << endl;
    return 999;
  }
  return fMUV3Candidates[index]->GetTime();
}

// Distance to the central MUV3 hole (R=103mm) in the MUV3 front plane (z=246800mm)
Double_t DownstreamTrack::GetMUV3DistanceToHole() {
  Double_t R = rAtAfterMagnet(GeometricAcceptance::GetInstance()->GetZMUV3());
  return (R<103.0) ? 0.0 : R-103.0;
}

// Distance to MUV3 edge (|x|,|y|<1320mm) in the MUV3 front plane (z=246800mm)
Double_t DownstreamTrack::GetMUV3DistanceToEdge() {
  Double_t z = GeometricAcceptance::GetInstance()->GetZMUV3();
  Double_t x = fabs(xAtAfterMagnet(z));
  Double_t y = fabs(yAtAfterMagnet(z));
  if (x>1320.0 || y>1320.0) return 0.0;
  Double_t dx = 1320.0 - x;
  Double_t dy = 1320.0 - y;
  return (dx<dy) ? dx : dy;
}
