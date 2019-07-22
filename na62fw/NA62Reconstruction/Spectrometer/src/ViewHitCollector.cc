#include "Riostream.h"

#include "ViewHitCollector.hh"
#include "SpectrometerGeometry.hh"
#include "SpectrometerParameters.hh"

/// \class ViewHitCollector
/// \Brief
/// Ensemble of the reconstructed tube-hits and clusters in a view.
/// \EndBrief
///
/// \Detailed
/// This class performs the following actions:
/// - Store the tube-hits with the reconstructed position: ViewHitCollector::AddHit.
/// - Groups the hits belonging to the same particle to form a view-cluster: void ViewHitCollector::AddCluster.
/// \EndDetailed

ViewHitCollector::ViewHitCollector(Int_t chamberID, Int_t viewID):
  fPar(SpectrometerParameters::GetInstance()),
  fGeo(SpectrometerGeometry::GetInstance())
{
/// \MemberDescr
/// Declaration of the pointers.
/// \EndMemberDescr
  for (Int_t iPlane=0; iPlane < 4; iPlane++)
    fPlaneHitCollector.push_back(new StrawHitCollector);
  fChamberID = chamberID;
  fViewID = viewID;

  fHTTime3 = nullptr;
  fHTTime2 = nullptr;
  fHTTime  = nullptr;
  for (int k = 0; k < 4; k++) {
    for (int l = 0; l < 4; l++) {
      fProva[k][l] = nullptr;
      fHIDvsID[k][l] = nullptr;
      fHPairTime[k][l] = nullptr;
      fHPairRadius[k][l] = nullptr;
    }
    fHTripleTime[k] = nullptr;
    fHTripleRadius[k] = nullptr;
  }
  fHTripletChi2 = nullptr;
  fHTripleSlope = nullptr;
  fHDoubleRR01_3 = nullptr;
  fHDoubleRR02_3 = nullptr;
  fHDoubleRR03_3 = nullptr;
  fHDoubleRR12_3 = nullptr;
  fHDoubleRR13_3 = nullptr;
  fHDoubleRR23_3 = nullptr;
  fHYCoorTot3 = nullptr;
  fHYCoorTot2 = nullptr;
  fHYCoorTot = nullptr;
  fHDoubleRR01 = nullptr;
  fHDoubleRR02 = nullptr;
  fHDoubleRR03 = nullptr;
  fHDoubleRR12 = nullptr;
  fHDoubleRR13 = nullptr;
  fHDoubleRR23 = nullptr;
  fRecoHitWireSum2 = nullptr;
  fRecoHitWireSum22 = nullptr;
  fRecoHitWireSum23 = nullptr;
  fRecoHitWireSum24 = nullptr;
  fRecoHitWireSum25 = nullptr;
  fHSlope = nullptr;
  fRecoHitWireSum2VsMagicT0 = nullptr;
  fRecoHitWireSum2VsROMezzanine = nullptr;
}

ViewHitCollector::~ViewHitCollector()
{
/// \MemberDescr
/// Delete the allocated memory space.
/// \EndMemberDescr

  Reset();
  for (StrawHitCollector *shc : fPlaneHitCollector)
    delete shc;
}

void ViewHitCollector::Reset()
{
/// \MemberDescr
/// Clear all the std::vector variables.
/// \EndMemberDescr

  for (TRecoSpectrometerHit *hit : fHitView)
    delete hit;
  fHitView.clear();

  for (Cluster *clus : fcluster) 
    delete clus;
  fcluster.clear();
}

void ViewHitCollector::InitHistograms(TString subDir)
{
  // Get global reco histogram pointers
  gDirectory->GetObject("RecoHitWireSum2", fRecoHitWireSum2);
  gDirectory->GetObject("RecoHitWireSum22", fRecoHitWireSum22);
  gDirectory->GetObject("RecoHitWireSum23", fRecoHitWireSum23);
  gDirectory->GetObject("RecoHitWireSum24", fRecoHitWireSum24);
  gDirectory->GetObject("RecoHitWireSum25", fRecoHitWireSum25);
  gDirectory->GetObject("hSlope", fHSlope);
  gDirectory->GetObject("RecoHitWireSum2VsMagicT0", fRecoHitWireSum2VsMagicT0);
  gDirectory->GetObject("RecoHitWireSum2VsROMezzanine", fRecoHitWireSum2VsROMezzanine);

  // Create various debug histos for monitoring of hit position reconstruction
  if (fPar.GetHistoDebug() >= 3) {
    // create new directory for storage
    TDirectory *curDir = gDirectory;
    TDirectory *newDir = curDir->GetDirectory(subDir);
    if(newDir == nullptr) newDir = curDir->mkdir(subDir);
    newDir->cd();
    fHTTime3 = new TH1F(Form("tTimeTriplet_Chamber_%d_View_%d"  , fChamberID, fViewID), "",200,0,600);
    fHTTime2 = new TH1F(Form("tTimeDoublet_Chamber_%d_View_%d"  , fChamberID, fViewID), "",200,0,600);
    fHTTime  = new TH1F(Form("tTime_Chamber_%d_View_%d"         , fChamberID, fViewID), "",200,0,600);
    for (Int_t jpl=0; jpl<4; jpl++) {
      for (Int_t jpl2=jpl+1; jpl2<4; jpl2++) {
        fHIDvsID[jpl][jpl2] = new TH2F(Form("strawIDvsID_Chamber_%d_View_%d_%d%d", fChamberID, fViewID, jpl, jpl2),"",122,0,122,122,0,122);
        fHPairTime[jpl][jpl2] = new TH1F(Form("pairTime_Chamber_%d_View_%d_%d%d", fChamberID, fViewID, jpl, jpl2),"", 400,-200,600);
        fHPairRadius[jpl][jpl2] = new TH1F(Form("pairRadius_Chamber_%d_View_%d_%d%d", fChamberID, fViewID, jpl, jpl2),"", 400,0,10);
        fProva[jpl][jpl2] = new TH1F(Form("prova_Chamber_%d_View_%d_%d%d", fChamberID, fViewID, jpl, jpl2),"", 2000,-1000,1000);
      }
    }
    for (Int_t itr=0; itr<4; itr++) {
      fHTripleTime[itr]   = new TH1F(Form("tripleTime_Chamber_%d_View_%d_%d", fChamberID, fViewID, itr),"",600,-300,900);
      fHTripleRadius[itr] = new TH1F(Form("tripleRadius_Chamber_%d_View_%d_%d"  , fChamberID, fViewID, itr),"",600,0,15);
    }

    fHTripletChi2  = new TH2F(Form("tripleChi2_Chamber_%d_View_%d"  , fChamberID, fViewID),"",200,0,100,600,0,15);
    fHTripleSlope  = new TH2F(Form("tripleSlope_Chamber_%d_View_%d" , fChamberID, fViewID),"",160,0,8,600,-0.5,0.5);
    fHDoubleRR01_3 = new TH2F(Form("doubleRR01_3_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR02_3 = new TH2F(Form("doubleRR02_3_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR03_3 = new TH2F(Form("doubleRR03_3_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR12_3 = new TH2F(Form("doubleRR12_3_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR13_3 = new TH2F(Form("doubleRR13_3_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR23_3 = new TH2F(Form("doubleRR23_3_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHYCoorTot3    = new TH1F(Form("yCoorTot3_Chamber_%d_View_%d"   , fChamberID, fViewID),"",5412,-1097.8,1071.4);
    fHYCoorTot2    = new TH1F(Form("yCoorTot2_Chamber_%d_View_%d"   , fChamberID, fViewID),"",5412,-1097.8,1071.4);
    fHYCoorTot     = new TH1F(Form("yCoorTot_Chamber_%d_View_%d"    , fChamberID, fViewID),"",5412,-1097.8,1071.4);
    fHDoubleRR01 = new TH2F(Form("doubleRR01_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR02 = new TH2F(Form("doubleRR02_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR03 = new TH2F(Form("doubleRR03_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR12 = new TH2F(Form("doubleRR12_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR13 = new TH2F(Form("doubleRR13_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    fHDoubleRR23 = new TH2F(Form("doubleRR23_Chamber_%d_View_%d", fChamberID, fViewID),"",200,0,5,200,0,5);
    curDir->cd();
  }
}

void ViewHitCollector::SaveHistograms(TString subDir)
{
  TDirectory *curDir = gDirectory;
  TDirectory *newDir = curDir->GetDirectory(subDir);
  if(newDir == nullptr) newDir = curDir->mkdir(subDir);
  newDir->cd();
  if (fHTTime3) fHTTime3->Write();
  if (fHTTime2) fHTTime2->Write();
  if (fHTTime) fHTTime->Write();
  for (Int_t jpl=0; jpl<4; jpl++) {
    for (Int_t jpl2=jpl+1; jpl2<4; jpl2++) {
      if (fHIDvsID[jpl][jpl2]) fHIDvsID[jpl][jpl2]->Write();
      if (fHPairTime[jpl][jpl2]) fHPairTime[jpl][jpl2]->Write();
      if (fHPairRadius[jpl][jpl2]) fHPairRadius[jpl][jpl2]->Write();
      if (fProva[jpl][jpl2]) fProva[jpl][jpl2]->Write();
    }
  }
  for (Int_t itr=0; itr<4; itr++) {
    if (fHTripleTime[itr]) fHTripleTime[itr]->Write();
    if (fHTripleRadius[itr]) fHTripleRadius[itr]->Write();
  }
  if (fHTripletChi2) fHTripletChi2->Write();
  if (fHTripleSlope) fHTripleSlope->Write();
  if (fHDoubleRR01_3) fHDoubleRR01_3->Write();
  if (fHDoubleRR02_3) fHDoubleRR02_3->Write();
  if (fHDoubleRR03_3) fHDoubleRR03_3->Write();
  if (fHDoubleRR12_3) fHDoubleRR12_3->Write();
  if (fHDoubleRR13_3) fHDoubleRR13_3->Write();
  if (fHDoubleRR23_3) fHDoubleRR23_3->Write();
  if (fHYCoorTot3) fHYCoorTot3->Write();
  if (fHYCoorTot2) fHYCoorTot2->Write();
  if (fHYCoorTot) fHYCoorTot->Write();
  if (fHDoubleRR01) fHDoubleRR01->Write();
  if (fHDoubleRR02) fHDoubleRR02->Write();
  if (fHDoubleRR03) fHDoubleRR03->Write();
  if (fHDoubleRR12) fHDoubleRR12->Write();
  if (fHDoubleRR13) fHDoubleRR13->Write();
  if (fHDoubleRR23) fHDoubleRR23->Write();
  curDir->cd();
}

void ViewHitCollector::AddHit(TRecoSpectrometerHit* fHitSpec,
                              const Double_t &xLocal,
                              const Double_t &zStraw,
                              const Double_t &fRotationAngle,
                              const Int_t &hitID)
{
/// \MemberDescr
/// \param fHitSpec Pointer to the TSpectrometerHit class.
/// \param fRecoSpecEvent Pointer to the TRecoSpectrometerEvent class.
/// \param xLocal View reference-frame hit position as measured in ViewHitCollector::ReconstructPosition.
/// \param zStraw Position of the straw of the hit along the beam direction.
/// \param fRotationAngle Rotation angle of the view taken from SpectrometerGeometry::fViewRotationAngle.
/// \param hitRecoID ID of the tube-hit according to the TRecoSpectrometerHit sequence.
/// \param hitId ID of the tube-hit according to the TSpectrometerHit sequence.
///
/// This member function stores a hit and performs the transformation between the view and lab reference frames.
/// The stored hits are ViewHitCollector::fHitView.
/// \EndMemberDescr

  // Store the hit in the tree
  TRecoSpectrometerHit candidateHit;
  TVector3 localPosition(xLocal,0.,zStraw);
  candidateHit.SetLocalPosition(localPosition);
  fHitSpec->SetLocalPosition(localPosition);
  TVector3 labPosition = localPosition;
  candidateHit.SetTime(fHitSpec->GetTime());
  candidateHit.SetDriftTime(fHitSpec->GetDriftTime());
  candidateHit.SetTimeWidth(fHitSpec->GetTimeWidth());
  candidateHit.SetEdgeStatus(fHitSpec->GetEdgeStatus());
  candidateHit.SetNUsedDigis(fHitSpec->GetNUsedDigis());
  candidateHit.SetChamberID(fHitSpec->GetChamberID());
  candidateHit.SetViewID(fHitSpec->GetViewID());
  candidateHit.SetHalfViewID(fHitSpec->GetHalfViewID());
  candidateHit.SetStrawID(fHitSpec->GetStrawID());
  candidateHit.SetPlaneID(fHitSpec->GetPlaneID());
  candidateHit.SetWireDistance(fHitSpec->GetWireDistance());
  candidateHit.SetRadius(fHitSpec->GetWireDistance());
  candidateHit.SetTDCID(fHitSpec->GetTDCID());
  candidateHit.SetRecoID(fHitSpec->GetRecoID());
  candidateHit.SetID(hitID);
  candidateHit.SetChannelID(fHitSpec->GetChannelID());

  // MC only info
  candidateHit.SetEnergy(fHitSpec->GetEnergy());
  candidateHit.SetMCID(fHitSpec->GetMCTrackID());

  // Lab position
  labPosition.RotateZ(fRotationAngle);
////  fCandidateHit->SetPosition(labPosition);
  candidateHit.SetPosition(labPosition);

  // Fill the hit
  fHitView.emplace_back(new TRecoSpectrometerHit(candidateHit));
}

void ViewHitCollector::AddCluster(Int_t nhitperview,TRecoSpectrometerHit **hit,Double_t *var)
{
  Cluster fCandidateCluster;
  fCandidateCluster.Reset();

  // Hit ordering
  Int_t nHitPerView = nhitperview%1000000;
  Int_t nHits = nhitperview/1000000;
  Int_t id[3];
  Double_t z[3];
  for (Int_t j=0; j<nHits; j++) {
    id[j] = nHitPerView-(nHits-j);
    z[j] = hit[j]->GetLocalPosition().Z();
  }
  for (Int_t i=0; i<nHits; i++) {
    for (Int_t j=i+1; j<nHits; j++) {
      if (z[j]<z[i]) {
        Int_t istore = id[i];
        Int_t jstore = id[j];
        Double_t zistore = z[i];
        Double_t zjstore = z[j];
        id[i] = jstore;
        id[j] = istore;
        z[i] = zjstore;
        z[j] = zistore;
      }
    }
  }

  //
  TVector3 posLocal = TVector3(0,0,0);
  TVector3 pos = TVector3(0,0,0);
  Double_t tTime = 0.;
  Int_t nedge = 0;
  for (Int_t j=0; j<nHits; j++) {
    fCandidateCluster.AddHit(id[j]);
    posLocal += hit[j]->GetLocalPosition();
    pos += hit[j]->GetPosition();
    tTime += hit[j]->GetDriftTime()+hit[j]->GetTimeWidth();
    if (hit[j]->GetEdgeStatus()) nedge++;
  }
  posLocal *= 1./((Double_t)nHits);
  pos *= 1./((Double_t)nHits);
  tTime *= 1./((Double_t)nHits);
  fCandidateCluster.SetLocalPosition(posLocal);
  fCandidateCluster.SetPosition(pos);
  fCandidateCluster.SetLocalSlope(var[0]);
  fCandidateCluster.SetTrailingTime(tTime);
  fCandidateCluster.SetQuality(var[1]);
  if (nedge==nHits) fCandidateCluster.SetEdge(1);
  else fCandidateCluster.SetEdge(0);
  fcluster.emplace_back(new Cluster(fCandidateCluster));
}

void ViewHitCollector::ReconstructHitPositions(TRecoSpectrometerEvent *event)
{
/// \MemberDescr
/// This member function reconstructs the hit positions by solving the left-right ambiguity. The steps are:
/// - Loop over all the hits sorted per chamber, view and plane (reference-hits).
/// - Check if the reference-hit has been already used.
/// - Loop over all the hits of the plane next to the plane of the reference-hit.
/// - Check if a hit in one of the next planes (next-hit) has been already used.
/// - Require that the position of the straw tube of the reference-hit is within
///   SpectrometerParameters::fPar.GetDeltaStrawPositionMax() from the position of the straw tube of the next-hit.
/// - Loop over all the possible next to next planes.
/// - Loop over all the hits of each next to next plane.
/// - Check if a hit in one of the next to next planes (next-to-next-hit) has been already used.
/// - Require that the position of the straw tube of the next-hit is within
///   SpectrometerParameters::fPar.GetDeltaStrawPositionMax() from the position of the straw tube of the next-to-next-hit.
/// - If one next-to-next-hit satisfied the above conditions, the positions of the hits is
///   determined using a triplet of hits (reference-hit, next-hit, next-to-next hit) via the function
///   ViewHitCollector::ChooseTheCombination.
/// - In case of a good triplet of hits, the hits are all stored using the method ViewHitCollector::AddHit.
/// - If no next-to-next-hits satisfied the previous, the positions of the hits are determined using only two hits.
///   Two tubes may be hit in the bulk of the straw (approximately within 3.9 mm and 0.5 mm from the wire).
///   In this case the R assignement depends on the relative position of the two tubes. Two tubes may also
///   be hit at radii larger than 3.9 mm or less than 0.5 mm. This is a pure triplet configuration in which one
///   of the two tubes hit at large radius is inefficient. In this case the R assignement depends on the
///   relative position of the two tubes and on the plane of the tubes. Finally, if the two hits do not belong
///   to one of the two above cases, an R assignement is assumed and checked with respect to the slope of the
///   segment reconstructed with this R assignement.
/// - The hits are then stored.
/// - If no next-hits satisfied the above conditions, the reference hit is stored with a position set by default to
///   fPar.GetNullCoordinate()=-9999. This case may happen close to edges of the chamber holes.
/// \EndMemberDescr

  Double_t fViewAngle = fGeo.GetViewRotationAngle(fViewID);
  Int_t nHitPerView = 0;
  for (Int_t jPlane=0;jPlane<4;jPlane++) { // Plane Loop
    Int_t planeGlobID = 16*fChamberID+4*fViewID+jPlane;
    for (Int_t jHit=0;jHit<GetPlane(jPlane)->GetN();jHit++) { // Hit Loop
      Int_t fHitId = GetPlane(jPlane)->GetHitId(jHit); // Id of the hit according to the TRecoSpectrometerHit sequence
      TRecoSpectrometerHit *fHit = static_cast<TRecoSpectrometerHit*>((event->GetHits())->At(fHitId));
      if (fHit->GetDriftTime()<0||fHit->GetDriftTime()>170) continue; // In time hits
      if (GetPlane(jPlane)->GetHitFlag(jHit)==3) continue; // Hit not used to form a triplet
      Straw *straw = fGeo.GetStraw(fHit->GetChannelID());
      if (!straw) continue;
      Double_t hitTTime = fHit->GetDriftTime()+fHit->GetTimeWidth();
      for (Int_t jPlaneNext=jPlane+1; jPlaneNext<4;jPlaneNext++) { // Loop on hits of the next plane (pair search)
        for (Int_t jHitNext=0; jHitNext<GetPlane(jPlaneNext)->GetN(); jHitNext++) {
          if (GetPlane(jPlane)->GetHitFlag(jHit)==3) continue;
          if (GetPlane(jPlaneNext)->GetHitFlag(jHitNext)==3) continue;
          Int_t fHitNextId = GetPlane(jPlaneNext)->GetHitId(jHitNext); // Id of the hit according to the TRecoSpectrometerHit sequence
          TRecoSpectrometerHit *fHitNext = static_cast<TRecoSpectrometerHit*>((event->GetHits())->At(fHitNextId));
          if (fHitNext->GetDriftTime()<0||fHitNext->GetDriftTime()>170) continue; // In time hits
          Straw *strawNext = fGeo.GetStraw(fHitNext->GetChannelID());
          if(!strawNext) continue;
          Double_t deltaStrawX = straw->GetLocalXPosition()-strawNext->GetLocalXPosition();
          if (fHIDvsID[jPlane][jPlaneNext])
            fHIDvsID[jPlane][jPlaneNext]->Fill(fHit->GetStrawID(),fHitNext->GetStrawID());
          if (fProva[jPlane][jPlaneNext])
            fProva[jPlane][jPlaneNext]->Fill(deltaStrawX);
          if (fabs(deltaStrawX)>fPar.GetDeltaStrawPositionMax()) continue; // Straw far more than 9 mm in consecutive planes cannot be hit by the same (~straight) particle
          Double_t nextHitTTime = fHitNext->GetDriftTime()+fHitNext->GetTimeWidth(); // Trailing time
          Bool_t trailing_pair = fHit->GetEdgeStatus() && fHitNext->GetEdgeStatus() ? 1 : 0;
          if (trailing_pair && fabs(nextHitTTime-hitTTime)>=fPar.GetTTrailingCut2Hits()) continue; // Cut on the hit time if the trailing time exists for all the hits
          if (fHPairTime[jPlane][jPlaneNext])
            fHPairTime[jPlane][jPlaneNext]->Fill(fHit->GetDriftTime()+fHitNext->GetDriftTime());
          if (fHPairRadius[jPlane][jPlaneNext])
            fHPairRadius[jPlane][jPlaneNext]->Fill(fHit->GetWireDistance()+fHitNext->GetWireDistance());
          Double_t fPos(fPar.GetNullCoordinate());
          Double_t fPosNext(fPar.GetNullCoordinate());
          Double_t fPosNextToNext(fPar.GetNullCoordinate());
          Double_t fSlope(fPar.GetNullCoordinate());
          Double_t slope(fPar.GetNullCoordinate());
          Int_t matchHitId(-1);
          Int_t lastPlane(-1);
          Int_t lastHit(-1);
          Double_t chi2min(999999.);
          for (Int_t jPlaneNextToNext=jPlaneNext+1; jPlaneNextToNext<4; jPlaneNextToNext++) {  // Loop on hits of the next to next plane (triplet search)
            for (Int_t jHitNextToNext=0;jHitNextToNext<GetPlane(jPlaneNextToNext)->GetN();jHitNextToNext++) {
              if (GetPlane(jPlane)->GetHitFlag(jHit)==3) continue;
              if (GetPlane(jPlaneNext)->GetHitFlag(jHitNext)==3) continue;
              if (GetPlane(jPlaneNextToNext)->GetHitFlag(jHitNextToNext)==3) continue;
              Int_t fHitNextToNextId = GetPlane(jPlaneNextToNext)->GetHitId(jHitNextToNext);
              TRecoSpectrometerHit *fHitNextToNext = static_cast<TRecoSpectrometerHit*>((event->GetHits())->At(fHitNextToNextId));
              if (fHitNextToNext->GetDriftTime()<0||fHitNextToNext->GetDriftTime()>170) continue; // In time hits
              Straw *fStrawNextToNext = fGeo.GetStraw(fHitNextToNext->GetChannelID());
              if(!fStrawNextToNext) continue;
              if (fabs(fStrawNextToNext->GetLocalXPosition()-strawNext->GetLocalXPosition())>fPar.GetDeltaStrawPositionMax()) continue;
              Double_t nextToNextHitTTime = fHitNextToNext->GetDriftTime()+fHitNextToNext->GetTimeWidth();
              Bool_t trailing_triplet = trailing_pair && fHitNextToNext->GetEdgeStatus() ? 1 : 0;
              if (trailing_triplet && fabs(nextToNextHitTTime-nextHitTTime)>=fPar.GetTTrailingCut2Hits()) continue; // Cut on the hit time if the trailing time exists for all the hits
              if (trailing_triplet && fabs(nextToNextHitTTime-hitTTime)>=fPar.GetTTrailingCut2Hits()) continue; // Cut on the hit time if the trailing time exists for all the hits
              Double_t tripleDrift = fHit->GetDriftTime()+fHitNext->GetDriftTime()+fHitNextToNext->GetDriftTime();
              Double_t tripleSum = fHit->GetWireDistance()+fHitNext->GetWireDistance()+fHitNextToNext->GetWireDistance();
              if (jPlane==0&&jPlaneNext==1&&jPlaneNextToNext==2)
                if (fHTripleTime[0])
                  fHTripleTime[0]->Fill(tripleDrift);
              if (jPlane==0&&jPlaneNext==1&&jPlaneNextToNext==3)
                if (fHTripleTime[1])
                  fHTripleTime[1]->Fill(tripleDrift);
              if (jPlane==1&&jPlaneNext==2&&jPlaneNextToNext==3)
                if (fHTripleTime[2])
                  fHTripleTime[2]->Fill(tripleDrift);
              if (jPlane==0&&jPlaneNext==2&&jPlaneNextToNext==3)
                if (fHTripleTime[3])
                  fHTripleTime[3]->Fill(tripleDrift);
              if (jPlane==0&&jPlaneNext==1&&jPlaneNextToNext==2)
                if (fHTripleRadius[0])
                  fHTripleRadius[0]->Fill(tripleSum);
              if (jPlane==0&&jPlaneNext==1&&jPlaneNextToNext==3)
                if (fHTripleRadius[1])
                  fHTripleRadius[1]->Fill(tripleSum);
              if (jPlane==1&&jPlaneNext==2&&jPlaneNextToNext==3)
                if (fHTripleRadius[2])
                  fHTripleRadius[2]->Fill(tripleSum);
              if (jPlane==0&&jPlaneNext==2&&jPlaneNextToNext==3)
                if (fHTripleRadius[3])
                  fHTripleRadius[3]->Fill(tripleSum);

              if (tripleSum<7.5 || tripleSum>11.) continue;
              Double_t fRadiusVec[3] = {fHit->GetWireDistance(),fHitNext->GetWireDistance(),fHitNextToNext->GetWireDistance()};
              Double_t xGood[3] = {fPar.GetNullCoordinate(),-fPar.GetNullCoordinate(),-fPar.GetNullCoordinate()};
              Straw *strawArray[3] = {straw,strawNext,fStrawNextToNext};
              Double_t chi2 = ChooseTheCombination(strawArray,fRadiusVec,xGood,&slope);
              Double_t avTTime = trailing_triplet ? (hitTTime+nextHitTTime+nextToNextHitTTime)/3. - fPar.GetTDriftMax() : 0;
              chi2 += fabs(avTTime)/fPar.GetTTrailingSigma3Hits(); // If no trailing in one of the hits the trailing time is not used for chi2
              if (fHTripletChi2)
                fHTripletChi2->Fill(chi2,tripleSum);
              if (chi2<chi2min && chi2<fPar.GetChi2Triplet() && chi2>=0) { // Choice of the best triplet given the first pair of hits. Triplet differing for the last hit only, not allowed.
                fPos = xGood[0];
                fPosNext = xGood[1];
                fPosNextToNext = xGood[2];
                fSlope = slope;
                matchHitId = fHitNextToNextId;
                lastPlane = jPlaneNextToNext;
                lastHit = jHitNextToNext;
                chi2min = chi2;
              }
            } // End hit in next-to-next plane Loop
          } // End next-to-next plane Loop

          // Triplet: 012, 013, 023, 123
          if (matchHitId>-1) {
            if (fHTripleSlope)
              fHTripleSlope->Fill(chi2min,fSlope);
            TRecoSpectrometerHit *matchHit = static_cast<TRecoSpectrometerHit*>((event->GetHits())->At(matchHitId));
            Straw *matchStraw = fGeo.GetStraw(matchHit->GetChannelID());
            AddHit(fHit,fPos,straw->GetGlobalZPosition(),fViewAngle,fHitId);
            GetPlane(jPlane)->SetHitFlag(jHit,3);
            AddHit(fHitNext,fPosNext,strawNext->GetGlobalZPosition(),fViewAngle,fHitNextId);
            GetPlane(jPlaneNext)->SetHitFlag(jHitNext,3);
            AddHit(matchHit,fPosNextToNext,matchStraw->GetGlobalZPosition(),fViewAngle,matchHitId);
            GetPlane(lastPlane)->SetHitFlag(lastHit,3);
            if (jPlane==0&&jPlaneNext==1)
              if (fHDoubleRR01_3)
                fHDoubleRR01_3->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            if (jPlane==0&&jPlaneNext==2)
              if (fHDoubleRR02_3)
                fHDoubleRR02_3->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            if (jPlane==1&&jPlaneNext==2)
              if (fHDoubleRR12_3)
                fHDoubleRR12_3->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            if (jPlane==0&&lastPlane==2)
              if (fHDoubleRR02_3)
                fHDoubleRR02_3->Fill(fHit->GetWireDistance(),matchHit->GetWireDistance());
            if (jPlane==0&&lastPlane==3)
              if (fHDoubleRR03_3)
                fHDoubleRR03_3->Fill(fHit->GetWireDistance(),matchHit->GetWireDistance());
            if (jPlane==1&&lastPlane==3)
              if (fHDoubleRR13_3)
                fHDoubleRR13_3->Fill(fHit->GetWireDistance(),matchHit->GetWireDistance());
            if (jPlaneNext==1&&lastPlane==2)
              if (fHDoubleRR12_3)
                fHDoubleRR12_3->Fill(fHitNext->GetWireDistance(),matchHit->GetWireDistance());
            if (jPlaneNext==1&&lastPlane==3)
              if (fHDoubleRR13_3)
                fHDoubleRR13_3->Fill(fHitNext->GetWireDistance(),matchHit->GetWireDistance());
            if (jPlaneNext==2&&lastPlane==3)
              if (fHDoubleRR23_3)
                fHDoubleRR23_3->Fill(fHitNext->GetWireDistance(),matchHit->GetWireDistance());
            nHitPerView += 3;
            TRecoSpectrometerHit *hits[3] = {GetHit(nHitPerView-3),GetHit(nHitPerView-2),GetHit(nHitPerView-1)};
            Double_t variable[2] = {fSlope,chi2min};
            AddCluster(nHitPerView+3000000,hits,variable);
            Double_t xcoor = (fPos+fPosNext+fPosNextToNext)/3.;
            Double_t matchHitTTime = matchHit->GetDriftTime()+matchHit->GetTimeWidth();
            Double_t taver = (hitTTime+nextHitTTime+matchHitTTime)/3.;
            if (fHYCoorTot3) fHYCoorTot3->Fill(xcoor);
            if (fHYCoorTot) fHYCoorTot->Fill(xcoor);
            if (fHTTime3) fHTTime3->Fill(taver);
            if (fHTTime) fHTTime->Fill(taver);
          }

          // Doublet: 01, 02, 03, 12, 13, 23
          if (matchHitId==-1) {
            Int_t sign = -1;
            Int_t isign = deltaStrawX ? deltaStrawX/fabs(deltaStrawX) : 0;
            Int_t hitPlanes[2] = {jPlane,jPlaneNext};
            Double_t hitWireDistance[2] = {fHit->GetWireDistance(),fHitNext->GetWireDistance()};
            if (jPlane==0&&jPlaneNext==1)
              if (fHDoubleRR01)
                fHDoubleRR01->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            if (jPlane==0&&jPlaneNext==2)
              if (fHDoubleRR02)
                fHDoubleRR02->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            if (jPlane==0&&jPlaneNext==3)
              if (fHDoubleRR03)
                fHDoubleRR03->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            if (jPlane==1&&jPlaneNext==2)
              if (fHDoubleRR12)
                fHDoubleRR12->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            if (jPlane==1&&jPlaneNext==3)
              if (fHDoubleRR13)
                fHDoubleRR13->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            if (jPlane==2&&jPlaneNext==3)
              if (fHDoubleRR23)
                fHDoubleRR23->Fill(fHit->GetWireDistance(),fHitNext->GetWireDistance());
            // Pairing
            Bool_t paired = 0;
            if (Pairing(hitWireDistance,hitPlanes,fChamberID)) {
              fPos = straw->GetLocalXPosition()+isign*sign*hitWireDistance[0];
              fPosNext = strawNext->GetLocalXPosition()-isign*sign*hitWireDistance[1];
              if (fabs(deltaStrawX-isign*fPar.GetPlaneStaggering())<1) {
                if (hitWireDistance[1]>fPar.GetTripletParameter3()) fPos = straw->GetLocalXPosition()-isign*sign*hitWireDistance[0];
                if (hitWireDistance[0]>fPar.GetTripletParameter3()) fPosNext = strawNext->GetLocalXPosition()+isign*sign*hitWireDistance[1];
              }
              paired = 1;
            }

            // Store doublets
            if (paired) {
              Double_t aslope = (fPos-fPosNext)/(straw->GetGlobalZPosition()-strawNext->GetGlobalZPosition());
              Double_t sumwire = hitWireDistance[0]+hitWireDistance[1];
              Double_t sr1 = SigmaRadius(hitWireDistance[0]);
              Double_t sr2 = SigmaRadius(hitWireDistance[1]);
              Double_t s2sumwire = sr1*sr1+sr2*sr2;
              Double_t s2aslope = s2sumwire/(fabs(straw->GetGlobalZPosition()-strawNext->GetGlobalZPosition())*fabs(straw->GetGlobalZPosition()-strawNext->GetGlobalZPosition()));
              Double_t timechi2 = trailing_pair ? fabs((hitTTime+nextHitTTime)/2.-fPar.GetTDriftMax())/fPar.GetTTrailingSigma2Hits() : 0;
              Double_t diffcor = -(fPar.GetXStrawAlignment(planeGlobID,fHit->GetStrawID()+1)-fPar.GetXStrawAlignment(planeGlobID,fHitNext->GetStrawID()+1));
              Double_t sumchi2 = sumwire>fPar.GetRadiusSum2HitsMax() ? fabs(sumwire-2*(fPar.GetPlaneStaggering()+diffcor))/sqrt(s2sumwire) : fabs(sumwire-(fPar.GetPlaneStaggering()+diffcor))/sqrt(s2sumwire);
              if (fRecoHitWireSum22)
                fRecoHitWireSum22->Fill(sumwire,sqrt(sumchi2*sumchi2+timechi2*timechi2));
              if (fRecoHitWireSum23)
                fRecoHitWireSum23->Fill(sumchi2,timechi2);
              if (fRecoHitWireSum25)
                fRecoHitWireSum25->Fill(sumwire,fabs(aslope)/s2aslope);
              if ((sumchi2<5 && timechi2<3) || (sumchi2>=5 && timechi2<1.4)) {
                if (((sumwire>fPar.GetRadiusSum2HitsMax() && sumchi2+timechi2<fPar.GetDoubletParameter3())
                     || (sumwire<fPar.GetRadiusSum2HitsMax()))) {
                  if (fHSlope)  fHSlope->Fill(aslope,sumwire);
                  AddHit(fHit,fPos,straw->GetGlobalZPosition(),fViewAngle,fHitId);
                  GetPlane(jPlane)->SetHitFlag(jHit,2);
                  AddHit(fHitNext,fPosNext,strawNext->GetGlobalZPosition(),fViewAngle,fHitNextId);
                  GetPlane(jPlaneNext)->SetHitFlag(jHitNext,2);
                  nHitPerView += 2;
                  TRecoSpectrometerHit *hits[2] = {GetHit(nHitPerView-2),GetHit(nHitPerView-1)};
                  Double_t variable[2] = {fabs(aslope),sumchi2+timechi2};
                  AddCluster(nHitPerView+2000000,hits,variable);

                  // Checks
                  Double_t xcoor = (fPos+fPosNext)/2.;
                  Double_t taver = (hitTTime+nextHitTTime)/2.;
                  if (fHYCoorTot2) fHYCoorTot2->Fill(xcoor);
                  if (fHYCoorTot) fHYCoorTot->Fill(xcoor);
                  if (fRecoHitWireSum2) fRecoHitWireSum2->Fill(sumwire);
                  if (fRecoHitWireSum2VsMagicT0) fRecoHitWireSum2VsMagicT0->Fill(fPar.GetMagicT0(),sumwire);
                  if (fRecoHitWireSum2VsROMezzanine) fRecoHitWireSum2VsROMezzanine->Fill(fHit->GetTDCID(),sumwire);
                  if (fRecoHitWireSum24) fRecoHitWireSum24->Fill(fChamberID,sumwire);
                  if (fHTTime2) fHTTime2->Fill(taver);
                  if (fHTTime) fHTTime->Fill(taver);
                }
              } // End condition on timechi2
            } // End pairing
          } // End if 2 hit condition

        } // End hit in next-plane loop
      } // End next-plane loop

    } // End Hit Loop
  } // End Plane Loop
}



Bool_t ViewHitCollector::Pairing(Double_t *dist, Int_t *hits, Int_t /*chamber*/)
{
  Double_t sumdist = dist[0]+dist[1];

  if ((dist[0]<1.8 && dist[1]>3.8) || (dist[0]>3.8 && dist[1]<1.8)) {
    if (hits[0]==0 && hits[1]==2) return true;
    if (hits[0]==0 && hits[1]==3) return true;
    if (hits[0]==1 && hits[1]==2) return true;
    if (hits[0]==1 && hits[1]==3) return true;
  }

  if (dist[0]>3.5 && dist[1]>3.5 && sumdist>8.2 && sumdist<9.3) {
    if (hits[0]==0 && hits[1]==1) return true;
    if (hits[0]==2 && hits[1]==3) return true;
  }

  if (dist[0]<=3.8 && dist[1]<=3.8) {
    if (hits[0]==0 && hits[1]==2) {
      if (sumdist<7.0 && sumdist>0.5) return true;
    }
    if (hits[0]==0 && hits[1]==3) {
      if (sumdist<7.5 && sumdist>0.3) return true;
    }
    if (hits[0]==1 && hits[1]==2) {
      if (sumdist<7.5 && sumdist>0.3) return true;
    }
    if (hits[0]==1 && hits[1]==3) {
      if (sumdist<7.0 && sumdist>0.5) return true;
    }
  }

  return false;
}

// Best triplet assignment according to the chi2
Double_t ViewHitCollector::ChooseTheCombination(Straw** straw, Double_t* fRadius, Double_t* xGood, Double_t* slopeGood) {
  Int_t c[3];
  Double_t z[3];
  Double_t chi2min(999999999.);
  Int_t icounter(1);
  for (Int_t j=0; j<3; j++) z[j] = straw[j]->GetLocalZPosition();
  for (Int_t jc=0;jc<8;jc++){
    c[0] = jc<4 ? 1 : -1;
    c[1] = (jc==0 || jc==1 || jc==4 || jc==5) ? 1 : -1;
    c[2] = icounter;
    icounter = - icounter;
    if (c[0]==c[1] && c[0]==c[2]) continue;
    Double_t x[3],error[3];
    for (Int_t j=0;j<3;j++){
      x[j] = straw[j]->GetLocalXPosition()+c[j]*fRadius[j];
      error[j] = SigmaRadius(fRadius[j]);
    }
    Double_t slope;
    Double_t chi2 = Chi2LinearFit(x,z,error,&slope);
    if (chi2<chi2min && fabs(slope)<=0.1){
      chi2min = chi2;
      for (Int_t jg=0; jg<3; jg++) xGood[jg] = x[jg];
      *slopeGood = slope;
    }
  }

  return chi2min;
}

Double_t ViewHitCollector::SigmaRadius(Double_t radius)
{
  Double_t sigRad1 = 0.7-0.363*radius;
  Double_t sigRad2 = 0.476-0.147*radius+0.0092*radius*radius+0.00135*radius*radius*radius;
  Double_t sigRad3 = 0.1218;
  if (radius==0) return 5./sqrt(12.);
  if (radius<1) return sqrt(sigRad1*sigRad1);
  if (radius>=1 && radius<4) return sqrt(sigRad2*sigRad2);
  if (radius>=4) return sqrt(sigRad3*sigRad3);
//  if (radius<1) return sqrt(sigRad1*sigRad1 + 0.1*0.1);
//  if (radius>=1 && radius<4) return sqrt(sigRad2*sigRad2 + 0.1*0.1);
//  if (radius>=4) return sqrt(sigRad3*sigRad3 + 0.1*0.1);
  return 0.;
}

Double_t ViewHitCollector::Chi2LinearFit(Double_t *x, Double_t *z, Double_t *error, Double_t *s)
{
  Double_t chi22=0.,sumx=0.,sumy=0.,sumxy=0.,sumx2=0.;
  for(Int_t i=0;i<3;i++) {
    sumy += x[i];
    sumx += z[i];
    sumx2 += z[i]*z[i];
    sumxy += (z[i]*x[i]);
  }
  Double_t den = 3*sumx2-(sumx*sumx);
  Double_t num1 = sumy*sumx2-sumx*sumxy;
  Double_t q = num1/den;
  Double_t num = 3*sumxy-sumx*sumy;
  Double_t m = num/den;
  *s = m;
  for(Int_t i=0;i<3;i++) chi22 += (x[i]-q-m*z[i])*(x[i]-q-m*z[i])/(error[i]*error[i]);
  return chi22;
}
