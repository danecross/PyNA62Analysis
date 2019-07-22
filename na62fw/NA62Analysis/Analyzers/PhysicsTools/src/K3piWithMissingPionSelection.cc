#include <iostream>
#include "K3piWithMissingPionSelection.hh"
#include "TwoLinesCDA.hh"
#include "BeamParameters.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class K3piWithMissingPionSelection
/// \Brief
/// K+ -> pi+ pi+ pi- with one pi+ not in Straw
/// \EndBrief
/// \Detailed
/// K+ -> pi+ pi+ pi- with one pi+ not in Straw
/// 
/// The analyzer searches for pairs of tracks in the Straw forming a good vertex
/// in the fiducial region (default z = 110 m to z = 180 m) and cda < 15 mm.
/// 
/// First, a loop over all objects in the output of DownstreamTrack is performed
/// and the following criteria are imposed:
/// 1. Match in CHOD with a discriminant similar to pinunu selection
/// 2. Match in RICH with P(pi)/max(P(mu),P(e)) > 1.2
/// 3. No match in time with MUV3 (default time window = 10 ns)
/// 
/// Then, a loop over all pairs retrieves the ones with a good vertex in the fiducial
/// region while rejecting events with all 3 pions in Straw.
/// 
/// The last part of the pair selection concerns the match of all pairs surviving until 
/// now with a candidate in Cedar and one in GTK.
/// The match with Cedar is time based taking the average of the CHOD times as
/// reference.
/// The candidate in GTK is the one which forms a vertex with the smallest CDA
/// with the pair. Further, a time cut is applied (default time window 4 ns)
/// 
/// With the kaon determined, the kinematics is closed so the starting position
/// and the momentum of the missing pion are known. The missing mass is computed
/// and some cuts on it's distribution are applied.
/// 
/// The analyzer has two outputs:
/// EventSelected = true if at least one good pi0 candidate was found
/// MissingPions = vector of MissingPion objects (struct defined in K3piWithMissingPionSelection.hh)
///
/// An example of requesting the output:
/// \code
///   std::vector<K3piWithMissingPionSelection::MissingPion> missPiSelected =
///    *(std::vector<K3piWithMissingPionSelection::MissingPion>*)GetOutput("K3piWithMissingPionSelection.MissingPions");
/// \endcode
///
///\param ba
/// \EndDetailed

K3piWithMissingPionSelection::K3piWithMissingPionSelection(Core::BaseAnalysis *ba) : Analyzer(ba, "K3piWithMissingPionSelection") {
  RequestTree("Cedar", new TRecoCedarEvent);
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent);
  RequestTree("CHOD", new TRecoCHODEvent);
  RequestTree("LAV", new TRecoLAVEvent);
  RequestTree("SAC", new TRecoSACEvent);
  RequestTree("IRC", new TRecoIRCEvent);

  fReadingData = kTRUE;
  fEventSelected = kFALSE;

  AddParam("PiPlusMass",                  "double", &fPiPlusMass,                   139.570);
  AddParam("KaonPlusMass",                "double", &fKaonMass,                     493.677);

  AddParam("CutPiPlusMassLow",            "double", &fCutPiPlusMassLow,             136.014);
  AddParam("CutPiPlusMassHigh",           "double", &fCutPiPlusMassHigh,            143.126);

  AddParam("CutNStrawChambers",           "int",    &fCutNStrawChambers,            4);
  AddParam("CutTrackChi2",                "double", &fCutTrackChi2,                 20.);
  AddParam("CutTrackCHODMinDiscr",        "double", &fCutTrackChodMinDiscr,         5.);
  AddParam("CutRICHPionLH",               "double", &fCutRichPionLH,                1.2);
  AddParam("CutTrackMUV3MaxTimeDiff",     "double", &fCutTrackMuvMaxTimeDiff,       5.);
  AddParam("CutPairMinTimeDiff",          "double", &fCutPairMinTimeDiff,           -5.);
  AddParam("CutPairMaxTimeDiff",          "double", &fCutPairMaxTimeDiff,           5.);
  AddParam("CutPairCDA",                  "double", &fCutPairCda,                   15.);
  AddParam("CutPairMinZvtx",              "double", &fCutPairMinZVtx,               110000.);
  AddParam("CutPairMaxZvtx",              "double", &fCutPairMaxZVtx,               180000.);

  AddParam("CutPairCedarMinTimeDiff",     "double", &fCutPairCedarMinTimeDiff,      -2.);
  AddParam("CutPairCedarMaxTimeDiff",     "double", &fCutPairCedarMaxTimeDiff,      2.);

  AddParam("CutPairGTKMatchCDA",          "double", &fCutPairGtkMatchCda,           15.);
  AddParam("CutPairGTKMatchMinZvtx",      "double", &fCutPairGtkMatchMinZVtx,       110000.);
  AddParam("CutPairGTKMatchMaxZvtx",      "double", &fCutPairGtkMatchMaxZVtx,       180000.);
  AddParam("CutPairGTKMatchMinTimeDiff",  "double", &fCutPairGtkMatchMinTimeDiff,   -2.);
  AddParam("CutPairGTKMatchMaxTimeDiff",  "double", &fCutPairGtkMatchMaxTimeDiff,   2.);

  AddParam("CutPhotonVetoLAVMinTimeDiff", "double", &fCutPhotonVetoLavMinTimeDiff,  3.);
  AddParam("CutPhotonVetoLAVMaxTimeDiff", "double", &fCutPhotonVetoLavMaxTimeDiff,  3.);
  AddParam("CutPhotonVetoSACMinTimeDiff", "double", &fCutPhotonVetoSacMinTimeDiff,  5.);
  AddParam("CutPhotonVetoSACMaxTimeDiff", "double", &fCutPhotonVetoSacMaxTimeDiff,  5.);
  AddParam("CutPhotonVetoIRCMinTimeDiff", "double", &fCutPhotonVetoIrcMinTimeDiff,  5.);
  AddParam("CutPhotonVetoIRCMaxTimeDiff", "double", &fCutPhotonVetoIrcMaxTimeDiff,  5.);
}

void K3piWithMissingPionSelection::InitOutput() {
  fReadingData = GetIsTree();
  if (!fReadingData) return;
  
  RegisterOutput("EventSelected", &fEventSelected);
  RegisterOutput("MissingPions", &fMissingPionsOutput);
}

void K3piWithMissingPionSelection::InitHist() {
  if (!fReadingData) return;
  
  BookHisto("hTrackCHODMatchMinDiscr", 
          new TH1D("TrackCHODMatchMinDiscr", "TrackCHODMatchMinDiscr", 400, 0., 30.));
  BookHisto("hTwoTracksDt",
          new TH1D("TwoTracksDt", "hTwoTracksDt", 400, -20., 20.));
  BookHisto("hTwoTracksZvtxVsCDA",
          new TH2D("TwoTracksZvtxVsCDA", "TwoTracksZvtxVsCDA", 100, 0., 100., 200, 90000., 200000.));
  BookHisto("hThreeTracksZvtxVsCDA",
          new TH2D("ThreeTracksZvtxVsCDA", "ThreeTracksZvtxVsCDA", 100, 0., 100., 200, 90000., 200000.));
  BookHisto("hCedarMatchDt",
          new TH1D("CedarMatchDt", "hCedarMatchDt", 400, -10., 10.));
  BookHisto("hPairGTKmatch_zVtxVsCDA",
          new TH2D("PairGTKmatch_zVtxVsCDA", "PairGTKmatch_zVtxVsCDA", 500, 0., 100., 400, 90000., 200000.));
  BookHisto("hPairGTKmatch_mindt",
          new TH1D("PairGTKmatch_mindt", "PairGTKmatch_mindt", 400, -10., 10.));
  BookHisto("hMissingMass2MissingTrack",
          new TH1D("MissingMass2MissingTrack", "MissingMass2MissingTrack", 200, 0.0169, 0.0225));
  BookHisto("hMissingPionP",
          new TH1D("MissingPionP", "MissingPionP", 300, 0., 75.));
}

void K3piWithMissingPionSelection::DefineMCSimple() {
}

void K3piWithMissingPionSelection::StartOfRunUser() {
}

void K3piWithMissingPionSelection::StartOfBurstUser() {
}

void K3piWithMissingPionSelection::ProcessSpecialTriggerUser(int /*iEvent*/, unsigned int /*triggerType*/) {
}

void K3piWithMissingPionSelection::Process(int iEvent) {
  if (!fReadingData) return;
  
  SetOutputState("EventSelected", kOValid);
  fEventSelected = kFALSE;
  
  SetOutputState("MissingPions", kOValid);
  fMissingPions.clear();
  fMissingPionsOutput.clear();
  
  if (fMCSimple.fStatus == MCSimple::kMissing) {
    printIncompleteMCWarning(iEvent);
    return;
  }
  //  if (fMCSimple.fStatus == MCSimple::kEmpty) {
  //    printNoMCWarning();
  //    return;
  //  }
  fCedarEvent = GetEvent<TRecoCedarEvent>();;
  fGigaTrackerEvent = GetEvent<TRecoGigaTrackerEvent>();;
  fChodEvent = GetEvent<TRecoCHODEvent>();;
  fLavEvent = GetEvent<TRecoLAVEvent>();;
  fSacEvent = GetEvent<TRecoSACEvent>();;
  fIrcEvent = GetEvent<TRecoIRCEvent>();;

  fLavMatching = *GetOutput<LAVMatching*>("PhotonVetoHandler.LAVMatching");
  fSavMatching = *GetOutput<SAVMatching*>("PhotonVetoHandler.SAVMatching");

  fDownstreamTracks =
          *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");

  std::vector< std::pair<GoodTrack, GoodTrack> > trackPairs = GetTrackPairs();
  GetMissingPion(trackPairs);
  
  fEventSelected = fMissingPions.size() > 0;
  for (UInt_t i = 0; i < fMissingPions.size(); i++){
    K3piWithMissingPionInfo misspi;
    misspi.SetChodId(fMissingPions[i].fChodId);
    misspi.SetDownstreamTrackId(fMissingPions[i].fDownstreamTrackId);
    misspi.SetGtkId(fMissingPions[i].fGtkId);
    misspi.SetKaonMomentum(fMissingPions[i].fKaonMomentum);
    misspi.SetKaonTime(fMissingPions[i].fKaonTime);
    misspi.SetKtagId(fMissingPions[i].fKtagId);
    misspi.SetMissingPionMomentum(fMissingPions[i].fMissingPionMomentum);
    misspi.SetPairKaonVertex(fMissingPions[i].fPairKaonVertex);
    misspi.SetPairTime(fMissingPions[i].fPairTime);
    misspi.SetTrackFourMomenta(fMissingPions[i].fTrackFourMomenta);
    
    fMissingPionsOutput.push_back(misspi);
  }
}

std::vector<std::pair<K3piWithMissingPionSelection::GoodTrack,
K3piWithMissingPionSelection::GoodTrack> > K3piWithMissingPionSelection::GetTrackPairs() {

  std::vector<std::pair<GoodTrack, GoodTrack> > trackPairs;
  //Loop over all pairs of tracks and select those matching Pi+ Pi- with common vertex

  std::vector<GoodTrack> goodTracks = GetGoodTracks();
  UInt_t nTracks = goodTracks.size();

  Double_t chodTime[3];
  std::vector<TVector3> trackPos(0);
  std::vector<TVector3> trackMom(0);
  std::vector<Bool_t> isAdded(nTracks, kFALSE);
  for (UInt_t iTrack = 0; iTrack < nTracks; iTrack++) {
    if (isAdded[iTrack])
      continue;
    chodTime[0] = fDownstreamTracks[goodTracks[iTrack].fDownstreamTrackId].GetCHODCandidateTime(goodTracks[iTrack].fChodId);
    trackPos.push_back(fDownstreamTracks[goodTracks[iTrack].fDownstreamTrackId].GetPositionBeforeMagnet());
    trackMom.push_back(fDownstreamTracks[goodTracks[iTrack].fDownstreamTrackId].GetMomentumBeforeMagnet());

    TwoLinesCDA tlcda;
    tlcda.SetLine1PointDir(trackPos[0], trackMom[0]);

    //==========================================================================
    //==============Second track search=========================================
    for (UInt_t jTrack = iTrack + 1; jTrack < nTracks; jTrack++) {
      if (isAdded[jTrack])
        continue;

      if (fDownstreamTracks[goodTracks[iTrack].fDownstreamTrackId].GetCharge() +
	  fDownstreamTracks[goodTracks[jTrack].fDownstreamTrackId].GetCharge() != 0)
        continue;

      chodTime[1] = fDownstreamTracks[goodTracks[jTrack].fDownstreamTrackId].
              GetCHODCandidateTime(goodTracks[jTrack].fChodId);
      FillHisto("hTwoTracksDt", chodTime[1] - chodTime[0]);
      if (chodTime[1] - chodTime[0] < fCutPairMinTimeDiff ||
	  chodTime[1] - chodTime[0] > fCutPairMaxTimeDiff)
        continue;

      //Check if the two fDownstreamTracks make a good vertex in the fiducial region
      trackPos.push_back(fDownstreamTracks[goodTracks[jTrack].fDownstreamTrackId].GetPositionBeforeMagnet());
      trackMom.push_back(fDownstreamTracks[goodTracks[jTrack].fDownstreamTrackId].GetMomentumBeforeMagnet());
      tlcda.SetLine2PointDir(trackPos[1], trackMom[1]);
      tlcda.ComputeVertexCDA();
      Double_t zVTX = tlcda.GetVertex().Z();
      Double_t cda = tlcda.GetCDA();

      FillHisto("hTwoTracksZvtxVsCDA", cda, zVTX);
      if (cda > fCutPairCda || zVTX < fCutPairMinZVtx || zVTX > fCutPairMaxZVtx)
        continue;
      //========================================================================
      //=======================Third track search===============================

      Bool_t isThirdPionMissing = kTRUE;
      for (UInt_t kTrack = jTrack + 1; kTrack < nTracks; kTrack++) {
        if (isAdded[kTrack])
          continue;

        if (fDownstreamTracks[goodTracks[kTrack].fDownstreamTrackId].GetCharge() != 1) //Only Pi+ remains
          continue;
        trackPos.push_back(fDownstreamTracks[goodTracks[kTrack].fDownstreamTrackId].GetPositionBeforeMagnet());
        trackMom.push_back(fDownstreamTracks[goodTracks[kTrack].fDownstreamTrackId].GetMomentumBeforeMagnet());

        Double_t threeTracksCda = 9999.;
        TVector3 threeTracksVtx = GetMultiTrackVertex(trackPos, trackMom, &threeTracksCda);
        FillHisto("hThreeTracksZvtxVsCDA", threeTracksCda, threeTracksVtx.Z());
        if (threeTracksCda > fCutPairCda || 
                threeTracksVtx.Z() < fCutPairMinZVtx || threeTracksVtx.Z() > fCutPairMaxZVtx)
          continue;
        isThirdPionMissing = kFALSE;
        break;
      }

      if (!isThirdPionMissing) {
        isAdded[iTrack] = kTRUE;
        isAdded[jTrack] = kTRUE;
        continue;
      }

      std::pair<GoodTrack, GoodTrack> pair;

      pair.first.fDownstreamTrackId = 
              goodTracks[iTrack].fDownstreamTrackId;
      pair.second.fDownstreamTrackId = 
              goodTracks[jTrack].fDownstreamTrackId;
      pair.first.fChodId = 
              goodTracks[iTrack].fChodId;
      pair.second.fChodId = 
              goodTracks[jTrack].fChodId;

      trackPairs.push_back(pair);
      isAdded[iTrack] = kTRUE;
      isAdded[jTrack] = kTRUE;
    }
  }

  return trackPairs;
}

void K3piWithMissingPionSelection::GetMissingPion(std::vector<std::pair<GoodTrack, GoodTrack> >& pairs) {
  UInt_t nPairs = pairs.size();

  for (UInt_t iPair = 0; iPair < nPairs; iPair++) {
    MissingPion missPi;
    missPi = pairs[iPair];
    Int_t hasKaon = MatchKaon(missPi);
    if (hasKaon < 0)
      continue;

    missPi.fMissingPionMomentum = missPi.fKaonMomentum - 
            missPi.fTrackFourMomenta.first - missPi.fTrackFourMomenta.second;

    TLorentzVector missPiMom = missPi.fMissingPionMomentum;
    Double_t mmiss2 = missPiMom.M2();
    FillHisto("hMissingMass2MissingTrack", mmiss2* pow(10., -6));
    if (sqrt(mmiss2) < fCutPiPlusMassLow || sqrt(mmiss2) > fCutPiPlusMassHigh)
      continue;

    Int_t hasPhoton = PhotonVeto(missPi.fPairTime);
    if (hasPhoton > 0)
      continue;
    fMissingPions.push_back(missPi);
    FillHisto("hMissingPionP", 0.001 * missPi.fMissingPionMomentum.P());
  }
}

Int_t K3piWithMissingPionSelection::MatchKaon(MissingPion& missPi) {
  Int_t hasKaon = 1;

  Int_t cedarID = MatchCedarCandidate(missPi);
  if (cedarID < 0)
    return -1;

  Int_t gtkID = MatchGtkCandidate(missPi);
  if (gtkID < 0)
    return -1;
  return hasKaon;
}

Int_t K3piWithMissingPionSelection::MatchCedarCandidate(MissingPion& missPi) {
  Int_t minid = -1;
  Double_t pairTime = 0.5 * (
          fDownstreamTracks[missPi.fDownstreamTrackId.first].GetCHODCandidateTime(missPi.fChodId.first) +
          fDownstreamTracks[missPi.fDownstreamTrackId.second].GetCHODCandidateTime(missPi.fChodId.second));
  missPi.fPairTime = pairTime;

  Double_t mindt = 9999.;
  for (Int_t iKtag = 0; iKtag < fCedarEvent->GetNCandidates(); iKtag++) {
    TRecoCedarCandidate *cedar =
            static_cast<TRecoCedarCandidate*>(fCedarEvent->GetCandidate(iKtag));
    if (cedar->GetNSectors() < 4)
      continue;

    Double_t dt = cedar->GetTime() - pairTime;
    if (fabs(dt) > fabs(mindt))
      continue;
    mindt = dt;
    minid = iKtag;
  }

  FillHisto("hCedarMatchDt", mindt);
  if (mindt > fCutPairCedarMaxTimeDiff || 
          mindt < fCutPairCedarMinTimeDiff)
    minid = -1;

  missPi.fKtagId = minid;
  missPi.fKaonTime = mindt + pairTime;
  return minid;
}

Int_t K3piWithMissingPionSelection::MatchGtkCandidate(MissingPion& missPi) {
  Int_t minid = -1;

  std::vector<TVector3> tracksPostions = {
    TVector3(0., 0., 0.),
    fDownstreamTracks[missPi.fDownstreamTrackId.first].GetPositionBeforeMagnet(),
    fDownstreamTracks[missPi.fDownstreamTrackId.second].GetPositionBeforeMagnet()
  };
  std::vector<TVector3> tracksMomenta = {
    TVector3(0., 0., 0.),
    fDownstreamTracks[missPi.fDownstreamTrackId.first].GetMomentumBeforeMagnet(),
    fDownstreamTracks[missPi.fDownstreamTrackId.second].GetMomentumBeforeMagnet()
  };
  std::vector<TLorentzVector> dummy(2);
  dummy[0].SetXYZM(tracksMomenta[1].X(), tracksMomenta[1].Y(), tracksMomenta[1].Z(), fPiPlusMass);
  dummy[1].SetXYZM(tracksMomenta[2].X(), tracksMomenta[2].Y(), tracksMomenta[2].Z(), fPiPlusMass);
  missPi.fTrackFourMomenta = make_pair(dummy[0], dummy[1]);

  Double_t mincda = 9999.;
  Double_t mindt = 9999.;
  TVector3 vtx = TVector3(0., 0., 0.);
  TVector3 minVtx = TVector3(0., 0., 0.);
  TVector3 kaonMom = TVector3(0., 0., 0.);
  //  tracksPostions[0] = TVector3(BeamParameters::GetInstance()->GetBeamX(),
  //                                BeamParameters::GetInstance()->GetBeamY(),
  //                                GeometricAcceptance::GetInstance()->GetZGTK3());
  //  tracksMomenta[0] = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  //  
  //  TVector3 vtx = GetMultiTrackVertex(tracksPostions, tracksMomenta, &mincda);
  //  FillHisto("hPairGTKmatch_zVtxVsCDA", mincda, vtx.Z());
  //  
  for (Int_t iGtk = 0; iGtk < fGigaTrackerEvent->GetNCandidates(); iGtk++) {
    TRecoGigaTrackerCandidate *gtk =
    		static_cast<TRecoGigaTrackerCandidate*>(fGigaTrackerEvent->GetCandidate(iGtk));
    if (gtk->GetType() != 123)
      continue;

    tracksPostions[0] = gtk->GetPosition(2);
    tracksMomenta[0] = gtk->GetMomentum();
    Double_t cda = 9999;

    vtx = GetMultiTrackVertex(tracksPostions, tracksMomenta, &cda);
    if (cda > mincda || vtx.Z() < 110000. || vtx.Z() > 180000.)
      continue;
    mincda = cda;
    mindt = gtk->GetTime() - missPi.fPairTime;
    kaonMom = tracksMomenta[0];
    minVtx = vtx;
    minid = iGtk;
  }

  FillHisto("hPairGTKmatch_zVtxVsCDA", mincda, minVtx.Z());
  if (mincda > fCutPairGtkMatchCda || 
          minVtx.Z() > fCutPairGtkMatchMaxZVtx ||
          minVtx.Z() < fCutPairGtkMatchMinZVtx)
    minid = -1;

  FillHisto("hPairGTKmatch_mindt", mindt);
  if (mindt > fCutPairGtkMatchMaxTimeDiff||
          mindt < fCutPairGtkMatchMinTimeDiff)
    minid = -1;

  missPi.fGtkId = minid;
  TLorentzVector kaonFourMomentum;
  kaonFourMomentum.SetXYZM(kaonMom.X(), kaonMom.Y(), kaonMom.Z(), fKaonMass);
  missPi.fKaonMomentum = kaonFourMomentum;
  missPi.fPairKaonVertex = minVtx;

  return minid;
}

std::vector<K3piWithMissingPionSelection::GoodTrack> K3piWithMissingPionSelection::GetGoodTracks() {
  std::vector<GoodTrack> goodTracks;

  UInt_t nTracks = fDownstreamTracks.size();
  Double_t trackTime = 9999.;
  for (UInt_t iTrack = 0; iTrack < nTracks; iTrack++) {
    //Integrity of track check
    if (fabs(fDownstreamTracks[iTrack].GetCharge()) != 1)
      continue;
    if (fDownstreamTracks[iTrack].GetNChambers() < fCutNStrawChambers)
      continue;
    if (fDownstreamTracks[iTrack].GetChi2() > fCutTrackChi2)
      continue;
    trackTime = fDownstreamTracks[iTrack].GetTrackTime();

    //Chod association
    Double_t mindiscr = 9999.;
    Int_t idCHODMatch = -1;
    Double_t chodTime = 9999.;
    //std::cout<<"Shower flag = "<<fDownstreamTracks[iTrack].GetCHODAssociationOutput().GetShowerFlag()<<std::endl;
    //if (!fDownstreamTracks[iTrack].GetCHODAssociationOutput().GetShowerFlag())
    //  continue;
    SpectrometerCHODAssociationOutput sca = 
      fDownstreamTracks[iTrack].GetCHODAssociationOutput();
    for (Int_t iCHOD = 0; iCHOD < sca.GetNAssociationRecords(); iCHOD++) {
      SpectrometerCHODAssociationRecord *chod = sca.GetAssociationRecord(iCHOD);
      if (chod == NULL)
        continue;
      if (chod->GetCHODCandidateID() < 0 || chod->GetCHODCandidateID() > fChodEvent->GetNCandidates())
          continue;
      Double_t distance = chod->GetTrackCandidateDistance();
      Double_t dtTrackCHOD = chod->GetCHODCandidateTime() - trackTime;
      Int_t *hitsIndexes = chod->GetCHODCandidate()->GetHitsIndexes();
      TRecoCHODHit *hit1 = static_cast<TRecoCHODHit*>(fChodEvent->GetHit(hitsIndexes[0]));
      TRecoCHODHit *hit2 = static_cast<TRecoCHODHit*>(fChodEvent->GetHit(hitsIndexes[1]));
      Double_t dtHV = hit1->GetTime() - hit2->GetTime();
      Double_t discr = pow(distance / (2. * 13.), 2) + pow(dtTrackCHOD / (3. * 7.), 2) + pow(dtHV / (3. * 3.), 2);
      if (discr > mindiscr)
        continue;
      mindiscr = discr;
      idCHODMatch = iCHOD;
      chodTime = trackTime + dtTrackCHOD;
    }
    
    FillHisto("hTrackCHODMatchMinDiscr", mindiscr);
    if (idCHODMatch < 0 || mindiscr > fCutTrackChodMinDiscr)
      continue;

    //Pion identification
    Double_t pionLH = fDownstreamTracks[iTrack].GetRICHLikelihoodPion() /
            max(fDownstreamTracks[iTrack].GetRICHLikelihoodElectron(), fDownstreamTracks[iTrack].GetRICHLikelihoodMuon());
    if (pionLH < fCutRichPionLH)
      continue;
    //    if (fDownstreamTracks[iTrack].GetRICHMostLikelyHypothesis() != 3)
    //      continue;
    //Check if there is a MUV3 match
    if (fDownstreamTracks[iTrack].GetNMUV3InTimeAssociationRecords(chodTime, fCutTrackMuvMaxTimeDiff) > 0)
      continue;

    GoodTrack tr;
    tr.fChodId = idCHODMatch;
    tr.fDownstreamTrackId = iTrack;
    goodTracks.push_back(tr);
  }

  return goodTracks;
}

TVector3 K3piWithMissingPionSelection::GetMultiTrackVertex(std::vector<TVector3>& positions, std::vector<TVector3>& momenta, Double_t* cda) {
  UInt_t nTracks = positions.size();

  TVector3 avPosition(0, 0, 0);
  TVector3 avSlope(0, 0, 0);
  TVector3 avSlope2(0, 0, 0);
  TVector3 avMixed(0, 0, 0);

  // Compute Z as the position of minimum apporach between tracks
  Double_t z0 = 0;
  for (UInt_t j = 0; j < nTracks; j++) {
    TVector3 position = positions[j];
    TVector3 momentum = momenta[j];
    avPosition += position;
    TVector3 ddz = momentum * (1. / momentum.Z());
    avSlope += ddz;
    avSlope2 += TVector3(ddz.X() * ddz.X(), ddz.Y() * ddz.Y(), ddz.Z() * ddz.Z());
    avMixed += TVector3(position.X() * ddz.X(), position.Y() * ddz.Y(), position.Z() * ddz.Z());
    z0 = position.Z();
  }
  avPosition = (1. / nTracks) * avPosition;
  avSlope = (1. / nTracks) * avSlope;
  avSlope2 = (1. / nTracks) * avSlope2;
  avMixed = (1. / nTracks) * avMixed;
  Double_t num = nTracks * (avMixed.X() + avMixed.Y()) - nTracks * (avPosition.X() * avSlope.X() + avPosition.Y() * avSlope.Y());
  Double_t den = nTracks * (avSlope2.X() + avSlope2.Y()) - nTracks * (avSlope.X() * avSlope.X() + avSlope.Y() * avSlope.Y());
  Double_t zvertex = z0 - num / den;

  // Compute the trasnverse position and the cda
  TVector3 avPosVtx(0, 0, 0);
  TVector3 avPosVtx2(0, 0, 0);
  for (UInt_t j = 0; j < nTracks; j++) {
    TVector3 position = positions[j];
    TVector3 momentum = momenta[j];
    TVector3 posvtx = position + momentum * (1. / momentum.Z())*(zvertex - position.Z());
    avPosVtx += posvtx;
    avPosVtx2 += TVector3(posvtx.X() * posvtx.X(), posvtx.Y() * posvtx.Y(), posvtx.Z() * posvtx.Z());
  }
  avPosVtx = (1. / nTracks) * avPosVtx;
  avPosVtx2 = (1. / nTracks) * avPosVtx2;
  *cda = sqrt(avPosVtx2.X() + avPosVtx2.Y() - avPosVtx.X() * avPosVtx.X() - avPosVtx.Y() * avPosVtx.Y());

  return TVector3(avPosVtx.X(), avPosVtx.Y(), zvertex);
}

Int_t K3piWithMissingPionSelection::PhotonVeto(const Double_t &timeRef) {
  Int_t hasPhoton = 0;

  fLavMatching->SetReferenceTime(timeRef);
  fLavMatching->SetTimeCuts(fCutPhotonVetoLavMinTimeDiff, fCutPhotonVetoLavMaxTimeDiff);
  hasPhoton = fLavMatching->LAVHasTimeMatching(fLavEvent);
  if (hasPhoton)
    return 1;

  fSavMatching->SetReferenceTime(timeRef);
  fSavMatching->SetIRCTimeCuts(fCutPhotonVetoIrcMinTimeDiff, fCutPhotonVetoIrcMaxTimeDiff);
  fSavMatching->SetSACTimeCuts(fCutPhotonVetoSacMinTimeDiff, fCutPhotonVetoSacMaxTimeDiff);
  hasPhoton = fSavMatching->SAVHasTimeMatching(fIrcEvent, fSacEvent);
  return hasPhoton;
}

void K3piWithMissingPionSelection::PostProcess() {
  SetOutputState("EventSelected", kOInvalid);
  SetOutputState("MissingPions", kOInvalid);
}

void K3piWithMissingPionSelection::EndOfBurstUser() {
}

void K3piWithMissingPionSelection::EndOfRunUser() {
}

void K3piWithMissingPionSelection::EndOfJobUser() {
  if (!fReadingData) return;
  SaveAllPlots();
  return;
}

void K3piWithMissingPionSelection::DrawPlot() {
}

K3piWithMissingPionSelection::~K3piWithMissingPionSelection() {
}
