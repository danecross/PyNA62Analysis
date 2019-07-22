// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-29
//
// ---------------------------------------------------------------

/// \class DownstreamTrackBuilder
/// \Brief
/// Build a DownstreamTrack container for each spectrometer track
/// \EndBrief
/// \Detailed
/// For each track, a DownstreamTrack container is filled. The container includes the information
/// about the association of the track with candidates in other downstream subdetectors
/// (CHOD, NewCHOD, RICH, LKr, MUV1-3), including the pointers to the relevant candidates in subdetectors.
/// A vector of DownstreamTrack containers output by the algorithm
/// can be accessed in an analyzer in the following way.
/// \code
/// std::vector<DownstreamTrack> Tracks =
///   *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "DownstreamTrackBuilder.hh"
#include "DownstreamTrack.hh"
#include "Event.hh"
#include "Persistency.hh"

#include "SpectrometerCHANTIAssociationOutput.hh"
#include "SpectrometerCHODAssociationOutput.hh"
#include "SpectrometerNewCHODAssociationOutput.hh"
#include "SpectrometerLKrAssociationOutput.hh"
#include "SpectrometerMUV12AssociationOutput.hh" // for simple geometrical MUV1+2 association
#include "CalorimeterCluster.hh" // for full MUV1+2 association
#include "SpectrometerMUV3AssociationOutput.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "SpectrometerRICHAssociationOutputSingleRing.hh"
#include "SpectrometerRICHAssociationOutputTrackCentredRing.hh"
#include "GeometricAcceptance.hh"
#include "BeamParameters.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

DownstreamTrackBuilder::DownstreamTrackBuilder(Core::BaseAnalysis *ba) :
  Analyzer(ba, "DownstreamTrackBuilder"), fCDAcomp1(nullptr), fCDAcomp2(nullptr) {
  if (!GetIsTree()) return;

  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("CHOD",         new TRecoCHODEvent,    "Reco");
  RequestTree("NewCHOD",      new TRecoNewCHODEvent, "Reco");
  RequestTree("MUV3",         new TRecoMUV3Event,    "Reco");

  fZTrim5   = GeometricAcceptance::GetInstance()->GetZTrim5();  // 101.8m
  fZSTRAW1  = GeometricAcceptance::GetInstance()->GetZStraw(0); // 183.508m
  fCDAcomp1 = new TwoLinesCDA(); // for CDA computaton wrt beam axis from the database
  fCDAcomp2 = new TwoLinesCDA(); // for CDA computaton wrt nominal beam axis
  fCDAcomp2->SetLine1PointDir(TVector3(0.0, 0.0, fZTrim5), // a point on the nominal beam axis
			      BeamParameters::GetInstance()->GetNominalBeamThreeMomentum());
}

DownstreamTrackBuilder::~DownstreamTrackBuilder() {
  if (fCDAcomp1) delete fCDAcomp1;
  if (fCDAcomp2) delete fCDAcomp2;
}

void DownstreamTrackBuilder::InitOutput() {
  if (!GetIsTree()) return;
  RegisterOutput("Output", &fContainer);
}

void DownstreamTrackBuilder::StartOfRunUser() {
  if (!GetIsTree()) return;
  // Run-dependent beam axis for track to beam axis vertex computation
  fCDAcomp1->SetLine1PointDir(BeamParameters::GetInstance()->GetBeamXYZ(),
			      BeamParameters::GetInstance()->GetBeamThreeMomentum());
}

void DownstreamTrackBuilder::Process(Int_t) {
  SetOutputState("Output", kOValid);
  fContainer.clear();

  if (!GetIsTree()) return;
  TRecoSpectrometerEvent* STRAWevent   = GetEvent<TRecoSpectrometerEvent>();
  TRecoNewCHODEvent*      NewCHODevent = GetEvent<TRecoNewCHODEvent>();
  TRecoMUV3Event*         MUV3event    = GetEvent<TRecoMUV3Event>();
  TRecoRICHEvent*         RICHevent    = GetEvent<TRecoRICHEvent>();

  std::vector<SpectrometerCHANTIAssociationOutput> SpecCHANTI =
    *(std::vector<SpectrometerCHANTIAssociationOutput>*)GetOutput("SpectrometerCHANTIAssociation.Output");
  std::vector<SpectrometerRICHAssociationOutputSingleRing> SpecRICHTrkSeededSingleRing = // single "track seeded" ring association
    *(std::vector<SpectrometerRICHAssociationOutputSingleRing>*)GetOutput("SpectrometerRICHAssociationSingleRing.Output");
  std::vector<SpectrometerRICHAssociationOutput> SpecRICH = // Full likelihood analysis
    *(std::vector<SpectrometerRICHAssociationOutput>*)GetOutput("SpectrometerRICHAssociation.Output");
  std::vector<SpectrometerRICHAssociationOutputTrackCentredRing> SpecRICHTrkCentredSingleRing = // single "track centred" ring association
    *(std::vector<SpectrometerRICHAssociationOutputTrackCentredRing>*)GetOutput("SpectrometerRICHAssociationTrackCentredRing.Output");
  std::vector<SpectrometerCHODAssociationOutput> SpecCHOD =
    *(std::vector<SpectrometerCHODAssociationOutput>*)GetOutput("SpectrometerCHODAssociation.Output");
  std::vector<SpectrometerNewCHODAssociationOutput> SpecNewCHOD =
    *(std::vector<SpectrometerNewCHODAssociationOutput>*)GetOutput("SpectrometerNewCHODAssociation.Output");
  std::vector<SpectrometerLKrAssociationOutput> SpecLKr =
    *(std::vector<SpectrometerLKrAssociationOutput>*)GetOutput("SpectrometerLKrAssociation.Output");
  std::vector<SpectrometerMUV12AssociationOutput> SpecMUV12Simple =
    *(std::vector<SpectrometerMUV12AssociationOutput>*)GetOutput("SpectrometerMUV12Association.Output");
  TClonesArray* SpecMUV12 =
    (TClonesArray*)GetOutput("SpectrometerCalorimetersAssociation.MatchedClusters");
  std::vector<SpectrometerMUV3AssociationOutput> SpecMUV3 =
    *(std::vector<SpectrometerMUV3AssociationOutput>*)GetOutput("SpectrometerMUV3Association.Output");

  for (Int_t iTrack=0; iTrack<STRAWevent->GetNCandidates(); iTrack++) {
    fTrack.Clear();
    // Fill the spectrometer track variables
    TRecoSpectrometerCandidate* Scand = static_cast<TRecoSpectrometerCandidate*>(STRAWevent->GetCandidate(iTrack));
    fTrack.SetTrackID(iTrack);
    fTrack.SetSpectrometerCandidate(Scand);
    fTrack.SetNChambers           (Scand->GetNChambers());
    fTrack.SetCharge              (Scand->GetCharge());
    fTrack.SetChi2                (Scand->GetChi2());
    fTrack.SetMomentum            (Scand->GetMomentum());
    fTrack.SetMomentumBeforeFit   (Scand->GetMomentumBeforeFit());
    fTrack.SetMomentumBeforeMagnet(Scand->GetThreeMomentumBeforeMagnet());
    fTrack.SetPositionBeforeMagnet(Scand->GetPositionBeforeMagnet());
    fTrack.SetMomentumAfterMagnet (Scand->GetThreeMomentumAfterMagnet());
    fTrack.SetPositionAfterMagnet (Scand->GetPositionAfterMagnet());
    fTrack.SetSlopeXBeforeMagnet  (Scand->GetSlopeXBeforeMagnet());
    fTrack.SetSlopeYBeforeMagnet  (Scand->GetSlopeYBeforeMagnet());
    fTrack.SetSlopeXAfterMagnet   (Scand->GetSlopeXAfterMagnet());
    fTrack.SetSlopeYAfterMagnet   (Scand->GetSlopeYAfterMagnet());
    fTrack.SetTrackTime           (Scand->GetTime()); // 5ns resolution
    fTrack.SetTrackLeadingTime    (Scand->GetLeadingTime()); // 1.2ns resolution; uses information from several detectors
    fTrack.SetIsFake              (IsFakeTrack(STRAWevent, iTrack));

    // Track fit covariance matrix: (x', y', x, y, 1/p)
    for (Int_t i=0; i<5; i++) {
      for (Int_t j=0; j<5; j++) {
	fTrack.SetCovariance(i, j, Scand->GetCovariance(i, j));
      }
    }

    // Computation of Vertex+CDA of the track with respect to the run-dependent & nominal beam axes
    fCDAcomp1->SetLine2PointDir(Scand->GetPositionBeforeMagnet(), Scand->GetThreeMomentumBeforeMagnet());
    fCDAcomp1->ComputeVertexCDA();
    fCDAcomp2->SetLine2PointDir(Scand->GetPositionBeforeMagnet(), Scand->GetThreeMomentumBeforeMagnet());
    fCDAcomp2->ComputeVertexCDA();
    fTrack.SetBeamAxisVertex(fCDAcomp1->GetVertex());
    fTrack.SetBeamAxisCDA(fCDAcomp1->GetCDA());
    fTrack.SetNominalBeamAxisVertex(fCDAcomp2->GetVertex());
    fTrack.SetNominalBeamAxisCDA(fCDAcomp2->GetCDA());

    // CHANTI association output
    fTrack.SetCHANTIAssociationOutput(SpecCHANTI[iTrack]);
    for (UInt_t iRecord=0; iRecord<SpecCHANTI[iTrack].GetNAssociationRecords(); iRecord++) {
      fTrack.AddCHANTICandidate(static_cast<TRecoCHANTICandidate*>(SpecCHANTI[iTrack].GetAssociationRecord(iRecord)->GetCHANTICandidate()));
    }
    
    // RICH association output: PID likelihoods based in RICH hits
    for (Int_t i=0; i<MaxHypos; i++) {
      fTrack.SetRICHLikelihood(i, SpecRICH[iTrack].GetLikelihood(i));
      fTrack.SetRICHRingPredictedRadius(i, SpecRICH[iTrack].GetPredictedRadius(i));
      fTrack.SetRICHRingTime(i, SpecRICH[iTrack].GetRingTime(i));
      fTrack.SetRICHRingNHits(i, SpecRICH[iTrack].GetNHitsAssigned(i));
      fTrack.SetRICHAssignedHits(i, SpecRICH[iTrack].GetAssignedHits(i));
      fTrack.SetRICHRingPredictedNHits(i, SpecRICH[iTrack].GetNExpectedSignalHits(i));
    }
    fTrack.SetRICHAssociationSuccessful(SpecRICH[iTrack].isValid()); // success = there is at least one RICH hit in the event
    fTrack.SetTrackTimeForRICHAssociation(SpecRICH[iTrack].GetTrackTimeForAssociation());
    fTrack.SetRICHNumberOfInTimeHits(SpecRICH[iTrack].GetNInTimeHits());
    fTrack.SetRICHNumberOfOutOfTimeHits(SpecRICH[iTrack].GetNOutOfTimeHits());
    fTrack.SetRICHMostLikelyHypothesis(SpecRICH[iTrack].GetMostLikelyHypothesis());
    fTrack.SetRICHRingPredictedCentrePosition(SpecRICH[iTrack].GetPredictedCentre());
    fTrack.SetRICHRingCentrePosition(SpecRICH[iTrack].GetRingCentre());
    fTrack.SetRICHRingCentrePositionError(SpecRICH[iTrack].GetRingCentreError());
    fTrack.SetRICHRingRadius(SpecRICH[iTrack].GetRingRadius());
    fTrack.SetRICHRingRadiusError(SpecRICH[iTrack].GetRingRadiusError());
    fTrack.SetRICHRingFitChi2(SpecRICH[iTrack].GetRingFitChi2());
    //SpecRICH[iTrack].Print(); // debugging

    // Alternative RICH association output - "track seeded"
    if (SpecRICHTrkSeededSingleRing[iTrack].GetRingID() > -1) {
      fTrack.SetRICHSingleRingTrkSeededCandidate
	(static_cast<TRecoRICHCandidate*>(RICHevent->GetTimeCandidate(SpecRICHTrkSeededSingleRing[iTrack].GetRingID())));
      fTrack.SetRICHSingleRingTrkSeededCandidateID(SpecRICHTrkSeededSingleRing[iTrack].GetRingID());
      fTrack.SetRICHSingleRingTrkSeededRadius(SpecRICHTrkSeededSingleRing[iTrack].GetRingRadius());
      fTrack.SetRICHSingleRingTrkSeededFitChi2(SpecRICHTrkSeededSingleRing[iTrack].GetRingChi2());
      fTrack.SetRICHSingleRingTrkSeededCentrePosition(SpecRICHTrkSeededSingleRing[iTrack].GetRingPosition());     
      fTrack.SetRICHSingleRingTrkSeededTime(SpecRICHTrkSeededSingleRing[iTrack].GetRingTime());
      fTrack.SetRICHSingleRingTrkSeededMass(SpecRICHTrkSeededSingleRing[iTrack].GetMass());
      fTrack.SetRICHSingleRingTrkSeededMass2(SpecRICHTrkSeededSingleRing[iTrack].GetMass2());
      fTrack.SetRICHSingleRingTrkSeededTrkDist(SpecRICHTrkSeededSingleRing[iTrack].GetMinDistanceTrackRing());
      fTrack.SetRICHSingleRingTrkSeededNHits(SpecRICHTrkSeededSingleRing[iTrack].GetNHits());
    }
    else {
      fTrack.SetRICHSingleRingTrkSeededCandidate(nullptr);
      fTrack.SetRICHSingleRingTrkSeededCandidateID(SpecRICHTrkSeededSingleRing[iTrack].GetRingID()); // < 0 if association not done
    }

    // Alternative "track centred" RICH association output
    fTrack.SetRICHSingleRingTrkCentredCandidateID(SpecRICHTrkCentredSingleRing[iTrack].GetRingID()); // RingID = -1 if association not done
    fTrack.SetRICHSingleRingTrkCentredRadius(SpecRICHTrkCentredSingleRing[iTrack].GetRingRadius());
    fTrack.SetRICHSingleRingTrkCentredFitChi2(SpecRICHTrkCentredSingleRing[iTrack].GetRingChi2());
    fTrack.SetRICHSingleRingTrkCentredCentrePosition(SpecRICHTrkCentredSingleRing[iTrack].GetRingPosition());
    fTrack.SetRICHSingleRingTrkCentredTime(SpecRICHTrkCentredSingleRing[iTrack].GetRingTime());
    fTrack.SetRICHSingleRingTrkCentredMass(SpecRICHTrkCentredSingleRing[iTrack].GetMass());
    fTrack.SetRICHSingleRingTrkCentredNHits(SpecRICHTrkCentredSingleRing[iTrack].GetNHits());

    // CHOD association output
    fTrack.SetCHODAssociationOutput(SpecCHOD[iTrack]);
    for (Int_t i=0; i<SpecCHOD[iTrack].GetNAssociationRecords(); i++) {
      fTrack.AddCHODCandidate
	(static_cast<TRecoCHODCandidate*>(SpecCHOD[iTrack].GetAssociationRecord(i)->GetCHODCandidate()));
    }

    // NewCHOD association output
    for (Int_t i=0; i<SpecNewCHOD[iTrack].GetNAssociationRecords(); i++) {
      fTrack.AddNewCHODCandidate
	(static_cast<TRecoNewCHODHit*>(NewCHODevent->GetHit
	 (SpecNewCHOD[iTrack].GetAssociationRecord(i)->GetRecoHitID())));
    }
    fTrack.SetNewCHODSearchRadius(SpecNewCHOD[iTrack].GetSearchRadius());
    fTrack.SetNewCHODBestRecordID(SpecNewCHOD[iTrack].GetBestAssociationRecordID());

    Double_t TotalCaloEnergy = 0.0; // summed over LKr, MUV1 and MUV2

    // LKr association output
    fTrack.SetLKrAssociationOutput(SpecLKr[iTrack]);
    for (UInt_t iRecord=0; iRecord<SpecLKr[iTrack].GetNAssociationRecords(); iRecord++) {
      fTrack.AddLKrCandidate(static_cast<TRecoLKrCandidate*>(SpecLKr[iTrack].GetAssociationRecord(iRecord)->GetLKrCandidate()));
      TotalCaloEnergy += SpecLKr[iTrack].GetTotalEnergy();
    }

    // MUV1+2 simple geometrical association output (from SpectrometerMUV12Association)
    fTrack.SetMUV1SimpleAssociationExists(false);
    fTrack.SetMUV2SimpleAssociationExists(false);
    if (SpecMUV12Simple[iTrack].GetMUV1ClusterID()>=0) {
      fTrack.SetMUV1SimpleAssociationExists(true);
      fTrack.SetMUV1SimpleAssociationTime(SpecMUV12Simple[iTrack].GetMUV1ClusterTime());
      fTrack.SetMUV1SimpleAssociationEnergy(SpecMUV12Simple[iTrack].GetMUV1ClusterEnergy());
      TVector2 MUV1SimpleAssoPos
	(SpecMUV12Simple[iTrack].GetMUV1ClusterPosition().X(), SpecMUV12Simple[iTrack].GetMUV1ClusterPosition().Y());
      fTrack.SetMUV1SimpleAssociationPosition(MUV1SimpleAssoPos);
    }
    if (SpecMUV12Simple[iTrack].GetMUV2ClusterID()>=0) {
      fTrack.SetMUV2SimpleAssociationExists(true);
      fTrack.SetMUV2SimpleAssociationTime(SpecMUV12Simple[iTrack].GetMUV2ClusterTime());
      fTrack.SetMUV2SimpleAssociationEnergy(SpecMUV12Simple[iTrack].GetMUV2ClusterEnergy());
      TVector2 MUV2SimpleAssoPos
	(SpecMUV12Simple[iTrack].GetMUV2ClusterPosition().X(), SpecMUV12Simple[iTrack].GetMUV2ClusterPosition().Y());
      fTrack.SetMUV2SimpleAssociationPosition(MUV2SimpleAssoPos);
    }

    // MUV1+2 full association output (from SpectrometerCalorimetersAssociation)
    fTrack.SetMUV1AssociationExists(false);
    fTrack.SetMUV2AssociationExists(false);
    CalorimeterCluster *Cluster = static_cast<CalorimeterCluster*>(SpecMUV12->At(iTrack));
    if (Cluster->IsMUV1Associated()) {
      TRecoMUV1Candidate* Cand = Cluster->GetMUV1Candidate();
      fTrack.SetMUV1AssociationExists(true);
      fTrack.SetMUV1Candidate(Cand);
      fTrack.SetMUV1ClusterPosition(Cand->GetPosition());
      fTrack.SetMUV1ClusterTime    (Cand->GetTime());
      fTrack.SetMUV1ClusterEnergy  (Cand->GetEnergy());
    }
    if (Cluster->IsMUV2Associated()) {
      TRecoMUV2Candidate* Cand = Cluster->GetMUV2Candidate();
      fTrack.SetMUV2AssociationExists(true);
      fTrack.SetMUV2Candidate(Cand);
      fTrack.SetMUV2ClusterPosition(Cand->GetPosition());
      fTrack.SetMUV2ClusterTime    (Cand->GetTime());
      fTrack.SetMUV2ClusterEnergy  (Cand->GetEnergy());
    }
    TotalCaloEnergy += Cluster->GetMUV1Energy();
    TotalCaloEnergy += Cluster->GetMUV2Energy();
    fTrack.SetCalorimetricEnergy(TotalCaloEnergy);

    // MUV3 association output
    for (Int_t i=0; i<SpecMUV3[iTrack].GetNAssociationRecords(); i++) {
      Int_t MuonIndex = SpecMUV3[iTrack].GetAssociationRecord(i)->GetMuonID();
      fTrack.AddMUV3CandidateIndex(MuonIndex);
      fTrack.AddMUV3Candidate(static_cast<TRecoMUV3Candidate*>(MUV3event->GetCandidate(MuonIndex)));
    }
    fTrack.SetMUV3SearchRadius(SpecMUV3[iTrack].GetSearchRadius());

    // DownstreamTrack/KinePart matching for MC event
    if (GetWithMC()) MatchKinePart();

    fContainer.push_back(fTrack);
  }
}

////////////////////////////////////////////////////////////////
// Fake track identification according to Giuseppe's definition:
// 1) has information in 3 chambers, AND
// 2) >1 hit in common with another track OR chi2>30

Bool_t DownstreamTrackBuilder::IsFakeTrack(TRecoSpectrometerEvent* STRAWevent, Int_t iTrack) {

  TRecoSpectrometerCandidate* tr1 = static_cast<TRecoSpectrometerCandidate*>(STRAWevent->GetCandidate(iTrack));

  if (tr1->GetNChambers()==4)          return false; // not fake
  if (tr1->GetChi2()>30.0)             return true;  // fake
  if (STRAWevent->GetNCandidates()<=1) return false; // not fake

  Int_t *Hits1 = tr1->GetHitsIndexes();
  for (Int_t i=0; i<STRAWevent->GetNCandidates(); i++) {
    if (i==iTrack) continue;
    Int_t NCommonHits = 0;
    TRecoSpectrometerCandidate *tr2 = static_cast<TRecoSpectrometerCandidate*>(STRAWevent->GetCandidate(i));
    Int_t *Hits2 = tr2->GetHitsIndexes();
    for (Int_t iHit1=0; iHit1<tr1->GetNHits(); iHit1++) {
      for (Int_t iHit2=0; iHit2<tr2->GetNHits(); iHit2++) {
	if (Hits1[iHit1]==Hits2[iHit2]) NCommonHits++;
	if (NCommonHits>1) return true; // fake: more than one common hit with another track
      }
    }
  }
  return false; // not fake
}

void DownstreamTrackBuilder::MatchKinePart() {
  fTrack.SetMatchedKinePartIndex(-1); // -999 for data, -1 for MC without match

  Event *evt = GetMCEvent();
  if (!evt) return;

  Int_t    Q_DST    = fTrack.GetCharge();
  Double_t p_DST    = fTrack.GetMomentum(); // [MeV]
  Double_t dxdz_DST = fTrack.GetSlopeXBeforeMagnet();
  Double_t dydz_DST = fTrack.GetSlopeYBeforeMagnet();
  Double_t x1_DST   = fTrack.xAtBeforeMagnet(fZSTRAW1); // [mm]
  Double_t y1_DST   = fTrack.yAtBeforeMagnet(fZSTRAW1); // [mm]

  // Standard deviations: from fits to DST-MCT distributions
  Double_t sigma_p    = 55.;
  Double_t sigma_dxdz = 3.3e-5;
  Double_t sigma_dydz = 3e-5;
  std::vector<Double_t> MatchKPIndex;
  std::vector<Double_t> MatchChi2;

  for (Int_t i=1; i<evt->GetNKineParts(); i++) { // Loop over all K+ daughter MC particles in the event
    KinePart *p = evt->GetKinePart(i);
    if (p->GetCharge()==Q_DST) { // First make sure charges match
      Double_t p_MCT    = p->GetInitialMomentum().Mag();
      Double_t dxdz_MCT = p->GetInitialMomentum().X()/p->GetInitialMomentum().Z();
      Double_t dydz_MCT = p->GetInitialMomentum().Y()/p->GetInitialMomentum().Z();
      Double_t x1_MCT   = p->xAt(fZSTRAW1); 
      Double_t y1_MCT   = p->yAt(fZSTRAW1);
      Double_t D2Straw1 = (x1_MCT-x1_DST)*(x1_MCT-x1_DST) + (y1_MCT-y1_DST)*(y1_MCT-y1_DST);
      if (p->GetProdPos().Z()>=96950. && p->GetProdPos().Z()<102425.) { // True particle produced in decay in near upstream region (in or between BEND6 and TRIM5)
	// BEND6 ends at z = 99.460 m,  TRIM 5 at z = 102.000 m, GTK3 at z = 102.400 m; checkpoint 3 "Gigatracker exit" is at z = 102.420 m (102.425 m before v1.0.2)
	if (p->GetPosAtCheckPoint(3).Z()>100000.) { // pre-v1.0.2 a bug (NARKD-867) means that sometimes info at Checkpoint 3 is unavialable/corrupted. If it is GetPosAtCheckPoint(3) = (0,0,0)
          p_MCT =  p->GetMomAtCheckPoint(3).Vect().Mag();
          dxdz_MCT = p->GetMomAtCheckPoint(3).Px()/p->GetMomAtCheckPoint(3).Pz();
          dydz_MCT = p->GetMomAtCheckPoint(3).Py()/p->GetMomAtCheckPoint(3).Pz();
          double lambda = (fZSTRAW1 - p->GetPosAtCheckPoint(3).Z())/p->GetMomAtCheckPoint(3).Pz();
          x1_MCT = p->GetPosAtCheckPoint(3).X()+lambda*p->GetMomAtCheckPoint(3).Px();
          y1_MCT = p->GetPosAtCheckPoint(3).Y()+lambda*p->GetMomAtCheckPoint(3).Py();
        }
        else { // If Checkpoint 3 info is corrupted calculate effect of TRIM5 on momentum & angle (good approximation) [x,y pos. at STRAW should be OK - track projected through B fields]
          TVector3 p_AfterTrim5;  // momentum after bending by TRIM5
          TVector3 Pos_Trim5Exit; // (x,y,z=10200) position at TRIM5 exit
          if (p->GetProdPos().Z()<101600.) { // Decay hapens before the TRIM5
            p_AfterTrim5.SetXYZ
	      (p->GetInitialMomentum().X()+90.0*p->GetCharge(),
	       p->GetInitialMomentum().Y(), p->GetInitialMomentum().Z()); // Apply momentum kick in x direction
            double lambda = (102000. - p->GetProdPos().Z())/p->GetInitialMomentum().Z();
	    // Project track to end of TRIM5 (assume pT kick happens here)
            Pos_Trim5Exit.SetXYZ
	      (p->GetProdPos().X() + lambda*p->GetInitialMomentum().X(),
	       p->GetProdPos().Y() + lambda*p->GetInitialMomentum().Y(),
	       102000.0);
	  }
          else if (p->GetProdPos().Z()>=101600. && p->GetProdPos().Z()<=102000.) { // Decay happens inside TRIM5 magnet
            p_AfterTrim5.SetXYZ
	      (p->GetInitialMomentum().X()+(102000.-p->GetProdPos().Z())/400.
	       *90.0*p->GetCharge(),
	       p->GetInitialMomentum().Y(),
	       p->GetInitialMomentum().Z()); // Apply (fraction of B field remaining)*(+90) MeV kick in x direction
	    double lambda = (102000. - p->GetProdPos().Z())/p_AfterTrim5.Z();
	    // Project track to end of TRIM5 (assume pT kick happens here)
            Pos_Trim5Exit.SetXYZ
	      (p->GetProdPos().X() + lambda*p_AfterTrim5.X(),
	       p->GetProdPos().Y() + lambda*p_AfterTrim5.Y(),
	       102000.0);
          } //else if(p->GetProdPos().Z()>102000){ // Decay happens AFTER TRIM5 - assume initial momentum and agles are OK.
            p_MCT = p_AfterTrim5.Mag();
            dxdz_MCT = p_AfterTrim5.X()/p_AfterTrim5.Z();
	    dydz_MCT = p_AfterTrim5.Y()/p_AfterTrim5.Z();
            double Lambda = (fZSTRAW1-102000.)/p_AfterTrim5.Z();
            x1_MCT = Pos_Trim5Exit.X() + Lambda*p_AfterTrim5.X(); // Project track from end of TRIM5 to STRAW1 (straight line approx)
            y1_MCT = Pos_Trim5Exit.X() + Lambda*p_AfterTrim5.Y(); // Project track from end of TRIM5 to STRAW1 (straight line approx)
        }
	D2Straw1 = (x1_MCT-x1_DST)*(x1_MCT-x1_DST) + (y1_MCT-y1_DST)*(y1_MCT-y1_DST); // re-calculate after re-setting XY position at STRAW for MCT for upstream decay cases
      }

      if (D2Straw1<400.0) { // distance < 20 mm
	Double_t chi2 =
	  ((p_MCT-p_DST)/sigma_p) * ((p_MCT-p_DST)/sigma_p) +
	  ((dxdz_MCT-dxdz_DST)/sigma_dxdz) * ((dxdz_MCT-dxdz_DST)/sigma_dxdz) +
	  ((dydz_MCT-dydz_DST)/sigma_dydz) * ((dydz_MCT-dydz_DST)/sigma_dydz);
	MatchKPIndex.push_back(i); 
	MatchChi2.push_back(chi2);
      }
    }
  }

  // Store KinePart index which best matches the track
  // (lowest Chi2 if more than 1 good match) and the calculated Chi2
  if (MatchKPIndex.size()>=1) {
    Double_t LowestChi2  = MatchChi2[0];
    Int_t BestMatchIndex = MatchKPIndex[0];
    for (UInt_t i=0; i<MatchChi2.size(); i++) {
      if (MatchChi2[i]<LowestChi2) {
	LowestChi2 = MatchChi2[i];
	BestMatchIndex = MatchKPIndex[i];
      }
    }
    fTrack.SetMatchedKinePartIndex(BestMatchIndex);
    fTrack.SetMatchedKinePartChi2(LowestChi2);
    fTrack.SetTrueThreeMomentum(evt->GetKinePart(BestMatchIndex)->GetInitialMomentum());
  }
}
