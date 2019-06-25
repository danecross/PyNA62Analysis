#include "Riostream.h"
#include "TMath.h"

#include "SpectrometerGeometry.hh"
#include "NA62Reconstruction.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"
#include "SpectrometerReconstruction.hh"
#include "SpectrometerParameters.hh"
#include "TString.h"
#include "SRBEvent.hh"
#include "SRBRawDecoder.hh"
#include "TSpectrometerDigi.hh"
#include "SpectrometerDigiManager.hh"

using namespace std;

/// \class SpectrometerReconstruction
/// \Brief
/// Steering class for Straw Spectrometer track reconstruction.
/// \EndBrief
///
/// \Detailed
/// Spectrometer Reconstruction, main steps:
/// - Initialization: SpectrometerReconstruction::SpectrometerReconstruction (constructor) + SpectrometerReconstruction::Init.
/// \n
/// - Pattern recognition and reconstruction: SpectrometerReconstruction::ProcessEvent (steering method).
/// \n
///  The algorithm reconstructs chamber hits with 2,3 and 4 views starting from clusters with 2 or 3 straws in each
///  view. The 2-hit chamber hits are considered only if they are in regions where 3-view or 2-view chambers hits are
///  expected. Tracks with 4 or 3 chamber hits are looked for. All the combinations of 3 chambers are possible.
/// \EndDetailed

SpectrometerReconstruction::SpectrometerReconstruction(TFile* HistoFile, TString ConfigFileName) : NA62VReconstruction(HistoFile, "Spectrometer", ConfigFileName){
/// \MemberDescr
/// \param HistoFile   Name of the output file.
///
/// Main steps:
/// - Allocate space for various pointers: SpectrometerReconstruction::fChamberHitCollector, SpectrometerReconstruction::fTrackCollector.
/// - Read the configuration file: SpectrometerReconstruction::ParseConfFile.
/// - Initialize variables used in the reconstruction
/// \EndMemberDescr

  fRecoEvent = new TRecoSpectrometerEvent();

  fPar = SpectrometerParameters::GetInstance();
  fGeo = SpectrometerGeometry::GetInstance();
  fRecoCHODEvent = nullptr;
  // Read configuration file
  ParseConfFile(ConfigFileName);
  // Initialize common variables
  fNEvent = 0;
  fNChambers = fPar->GetNChambers();
  fNViews = fPar->GetNViews();
  fNPlanes = fPar->GetNPlanes();
  fNCHODHitsInTime = 0;

  // create collectors
  for (Int_t iChamb = 0; iChamb < fNChambers; iChamb++)
    fChamberHitCollector.push_back(new ChamberHitCollector(iChamb));
  fTrackCollector = new TrackCollector(&fChamberHitCollector);
  fSpectrometerDigiManager = new SpectrometerDigiManager(this);

  // initialize histograms to null pointers
  for (int i = 0; i < 4; i++) {
    fRecoChamberHitTotal[i] = nullptr;
    fRecoChamberHit4Total[i] = nullptr;
    fRecoChamberHitQuality4[i] = nullptr;
    fRecoChamberHit3Total[i] = nullptr;
    fRecoChamberHitQuality3[i] = nullptr;
    fRecoChamberHit2Total[i] = nullptr;
    fHIllumTotal[i] = nullptr;
    fHIllum4Chambers[i] = nullptr;
    fHIllum3Chambers[i] = nullptr;
    fHIllum123[i] = nullptr;
    fHIllum023[i] = nullptr;
    fHIllum013[i] = nullptr;
    fHIllum012[i] = nullptr;
  }
  for (int i = 0; i < 8; i++) {
    fHPic_mmiss_fit_4Chambers_pmom_trig[i] = nullptr;
  }
  fTrailingVSDrift = nullptr;
  fTrailingVSDrift2 = nullptr;
  fDetectedEdge = nullptr;
  fDetectedEdge2 = nullptr;
  fOverflowHisto = nullptr;
  fNGoodHitPerPlane = nullptr;
  fNGoodHitPerEvent = nullptr;
  fRecoChamberHitTime = nullptr;
  fRecoChamberDHitTime = nullptr;
  fRecoHitWireSum2 = nullptr;
  fRecoHitWireSum22 = nullptr;
  fRecoHitWireSum23 = nullptr;
  fRecoHitWireSum24 = nullptr;
  fRecoHitWireSum25 = nullptr;
  fRecoHitWireSum2VsMagicT0 = nullptr;
  fRecoHitWireSum2VsROMezzanine = nullptr;
  fHSlope = nullptr;
  fViewClusterQuality = nullptr;
  fViewClusterDT = nullptr;
  fViewCluster3TT = nullptr;
  fHCombQuality = nullptr;
  fHCombHough = nullptr;
  fHPic_mmiss = nullptr;
  fHPic_mmiss_le = nullptr;
  fHPic_mmiss_zoom = nullptr;
  fHPic_momentum = nullptr;
  fHPvsVertex = nullptr;
  fHPic_mmiss2 = nullptr;
  fHPic_momentum_fit = nullptr;
  fHPic_momentum_fit_trig = nullptr;
  fHPic_mmiss_fit = nullptr;
  fHPic_mmiss_fit_4Chambers = nullptr;
  fHPic_mmiss_fit_4Chambers_trig = nullptr;
  fHPic_mmiss_fit_4Chambers_chi2 = nullptr;
  fHPic_mmiss_fit_4Chambers_pmom = nullptr;
  fHMissMassTime = nullptr;
}

SpectrometerReconstruction::~SpectrometerReconstruction()
{
/// \MemberDescr
/// Delete the allocated memory space.
/// \EndMemberDescr

  for (ChamberHitCollector *chc : fChamberHitCollector) delete chc;
  delete fTrackCollector;
  delete fSpectrometerDigiManager;
}

void SpectrometerReconstruction::ParseConfFile(TString ConfFileName){
/// \MemberDescr
/// \param ConfFileName   Name of the configuration file.
///
/// Read the reconstruction configuration file NA62Reconstruction/conf/Spectrometer.conf.
/// \EndMemberDescr
  fPar->ParseConfFile(ConfFileName);
  fPar->SetT0FileName(fT0FileName); // in NA62VReconstruction
  if (fPar->GetMagicT0ScanEnabled())
    std::cout << "[SpectrometerReconstruction] *** MagicT0Scan enabled! ***" << std::endl;
}

void SpectrometerReconstruction::Init(NA62VReconstruction* MainReco) {

  // Common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);
  fPar->SetIsRawData(static_cast<NA62Reconstruction*>(fMainReco)->GetIsRawData());

  // Build geometry and some initializations
  fPar->ReadMagicT0();
  fPar->SetRT(); // Read RT parameters
  fPar->SetXT(); // Read XT parameters
  fPar->SetAlignment(); // Read alignment offsets
  fPar->SetZPositionCorrection();

  fGeo->CreateDataGeometry();
  for (Int_t iChamb = 0; iChamb < fNChambers; iChamb++)
    GetChamber(iChamb)->Init();

  // Initialize histograms
  if (fPar->GetHistoDebug()) InitHistograms();
}

TDetectorVEvent * SpectrometerReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/){
/// \MemberDescr
/// Trigger algorithm.
/// \EndMemberDescr

  return tEvent;
}

void SpectrometerReconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  //-------------------- Update Run Number --------------------//
  if (fPar->GetIsRawData()) {   // Data
    fPar->SetRunNumber(NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID());
  } else {                      // MC
    fPar->SetRunNumber(0);
  }
  //-----------------------------------------------------------//
  fPar->SetT0(fPar->GetRunNumber());

  // exit if CHOD event is not found in data
  if(!fRecoCHODEvent && (fPar->GetTimeReference()=="CHOD")) {
    std::cerr << "[SpectrometerReconstruction] ERROR: CHOD needed as ReferenceDetector but not found! Disabling SpectrometerReconstruction.."<< std::endl;
    exit(kGenericError);
  }
}

void SpectrometerReconstruction::EndOfBurst() {
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
}

TRecoVEvent * SpectrometerReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

    /// \MemberDescr
    /// \param tEvent  Pointer to the TDetectorVEvent class.
    /// \param tGenEvent  Pointer to the Event class.
    /// \return        The pointer to TRecoVEvent class.
    ///
    /// This is the steering routines which contains the flow of the reconstruction.
    /// It calls all the private methods to build the tracks starting from the tube hits.
    /// The main steps are
    ///    -   Digitization:  SpectrometerReconstruction::Digitization.
    ///    -   Coordinate reconstruction (LR ambiguity solution): SpectrometerReconstruction::ReconstructHitPerView.
    ///    -   Chamber-hit reconstruction: SpectrometerReconstruction::MakeHitsPerChamber
    ///    -   Track pattern recognition and reconstruction:  TrackCollector::Reconstruct
    /// \EndMemberDescr

    if(tEvent->IsA() == TSpecialTriggerEvent::Class()){
      //read it
      return 0;
    }

    //common part for all the subdetectors
    NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

    if(fPar->GetMagicT0ScanEnabled()) {
      Double_t MinMagicT0 = 20.;
      Double_t MaxMagicT0 = 30.;
      Double_t MagicT0 = MinMagicT0+(MaxMagicT0-MinMagicT0)*(tEvent->GetID()%100)/100.;
      fPar->SetMagicT0(MagicT0);
    }

    // Reset hit collectors
    for (Int_t iChamb=0; iChamb<fNChambers; iChamb++) {
      for (Int_t iView=0; iView<fNViews; iView++) {
        GetChamber(iChamb)->GetView(iView)->Reset();
        for (Int_t iPlane=0; iPlane<fNPlanes; iPlane++)
          GetChamber(iChamb)->GetView(iView)->GetPlane(iPlane)->Reset();
      }
      GetChamber(iChamb)->Reset();
    }

    // From digi to reco hits
    SRBEvent* TdcEvent = static_cast<SRBEvent*>(tEvent);
    fSpectrometerDigiManager->ImportEvent(TdcEvent,reinterpret_cast<SpectrometerRawDecoder*>(fRawDecoder)); // Link TdcEvent in DigiManager

    fNCHODHitsInTime = 0;
    Double_t refTime = ReferenceTime();
    if (fPar->GetIsRawData() &&  fPar->GetTimeReference()=="CHOD") {
      if (fRecoCHODEvent && !fRecoCHODEvent->GetNHits()) return fRecoEvent;
      if (!fNCHODHitsInTime) return fRecoEvent;
    }
    fSpectrometerDigiManager->DigiToReco(fRecoEvent,refTime,fNEvent);

    // Reset track collector
    fTrackCollector->Reset();

    // Reconstruct hits per view
    SeparateHitPerViewPlane();
    Int_t isGoodEvent = ReconstructHitPerView();
    if (!isGoodEvent) return fRecoEvent;

    // Reconstruct hits per chamber
    for (Int_t jChamber=0; jChamber<fNChambers; jChamber++)
      GetChamber(jChamber)->ReconstructHit();

    // Reconstruct tracks
    fTrackCollector->Reconstruct(static_cast<TRecoSpectrometerEvent *>(fRecoEvent));
    for (Int_t jj=0; jj<fRecoEvent->GetNCandidates(); jj++) {
      TRecoSpectrometerCandidate *cand = static_cast<TRecoSpectrometerCandidate*>(fRecoEvent->GetCandidate(jj));
      Double_t ttime = cand->GetTime();
      cand->SetTime(ttime+refTime);
      Double_t ltime = cand->GetLeadingTime();
      cand->SetLeadingTime(ltime + refTime);
    }

    // Monitoring
    if (fPar->GetHistoDebug()) MakeHistograms(TdcEvent);
    return fRecoEvent;
}

void SpectrometerReconstruction::EndProcessing() {
/// \MemberDescr
/// End of event processing.
/// \EndMemberDescr
  NA62VReconstruction::EndProcessing();
  if (fPar->GetHistoDebug()) SaveHistograms();
}

void SpectrometerReconstruction::FillTimes(Double_t ReferenceTime) {
  NA62VReconstruction::FillTimes(ReferenceTime);
}

void SpectrometerReconstruction::SeparateHitPerViewPlane()
{
/// \MemberDescr
/// This class sort the hits per view-plane on the basis of the straw ID code.
/// \EndMemberDescr

    // Separate the residual hits per view plane
    for (Int_t jHit=0;jHit<fRecoEvent->GetNHits();jHit++)
    {
      TRecoSpectrometerHit* fHit = static_cast<TRecoSpectrometerHit*>((fRecoEvent->GetHits())->At(jHit));
      Straw *straw = fGeo->GetStraw(fHit->GetChannelID());
      fHit->SetLocalPosition(TVector3(straw->GetLocalPosition().X(),-9999.,straw->GetPosition().Z()));
      Int_t planeID = 2*fHit->GetHalfViewID()+fHit->GetPlaneID(); // plane ID inside view
      Double_t dtime = fHit->GetDriftTime();
      Bool_t goodHit = (dtime>=0 && dtime<=170);
      Int_t hitFlag = 0;
      GetChamber(fHit->GetChamberID())->GetView(fHit->GetViewID())->GetPlane(planeID)->AddHit(jHit, hitFlag, goodHit);
      Double_t ttime = fHit->GetEdgeStatus() ? dtime+fHit->GetTimeWidth() : -999999.;
      if (fTrailingVSDrift) fTrailingVSDrift->Fill(dtime,ttime); // trailing vs leading time
      if (fTrailingVSDrift2) fTrailingVSDrift2->Fill(dtime,ttime-dtime); // Time width vs drift time
      Int_t planeGlobID = 16*fHit->GetChamberID()+4*fHit->GetViewID()+planeID;
      Int_t strawGlobID = 122*planeGlobID+fHit->GetStrawID();
      if (fDetectedEdge) fDetectedEdge->Fill(strawGlobID,fHit->GetEdgeStatus());
      if (fDetectedEdge2) fDetectedEdge2->Fill(fHit->GetStrawID(),planeGlobID,fHit->GetEdgeStatus());
    }
}

Bool_t SpectrometerReconstruction::ReconstructHitPerView()
{
  if (fRecoEvent->GetNHits()>900) return 0;
  Int_t nGoodHitPerChamber[4];
  Int_t nChamberOverflow = 0;
  Int_t singleChamberOverflow = 0;
  for (Int_t jChamber=0;jChamber<4;jChamber++) {
    nGoodHitPerChamber[jChamber] = 0;
    for (Int_t jView=0;jView<4;jView++) {
      for (Int_t jPlane=0;jPlane<4;jPlane++) nGoodHitPerChamber[jChamber] += GetChamber(jChamber)->GetView(jView)->GetPlane(jPlane)->GetNGoodHit();
    }
    if (nGoodHitPerChamber[jChamber]>150) singleChamberOverflow++;
    if (nGoodHitPerChamber[jChamber]>64) nChamberOverflow++;
  }
  if (nChamberOverflow>2 && singleChamberOverflow>1) {
    if (fOverflowHisto) fOverflowHisto->Fill(NA62RecoManager::GetInstance()->GetEventHeader()->GetTimeStamp());
    return 0;
  }

  Int_t nTotalHit = 0;
  Int_t nGoodHit = 0;
  // Resolve L-R ambiguity + reconstruct hit positions in all views
  for (Int_t jChamber = 0; jChamber < 4; jChamber++) {
    for (Int_t jView = 0; jView < 4; jView++) {
      TRecoSpectrometerEvent* event = dynamic_cast<TRecoSpectrometerEvent*>(fRecoEvent);
      GetChamber(jChamber)->GetView(jView)->ReconstructHitPositions(event);
      for (Int_t jPlane = 0; jPlane < 4; jPlane++) {
        Int_t planeGlobID = 16*jChamber + 4*jView + jPlane;
        Int_t planeNTotalHit = GetChamber(jChamber)->GetView(jView)->GetPlane(jPlane)->GetN();
        Int_t planeNGoodHit = GetChamber(jChamber)->GetView(jView)->GetPlane(jPlane)->GetNGoodHit();
        nTotalHit+= planeNTotalHit;
        nGoodHit+= planeNGoodHit;
        if (fNGoodHitPerPlane) fNGoodHitPerPlane->Fill(planeGlobID, planeNGoodHit);
      }
    }
  }
  if (fNGoodHitPerEvent) fNGoodHitPerEvent->Fill(fNEvent,nGoodHit);

  fNEvent++;
  return 1;
}

void SpectrometerReconstruction::MakeHistograms(SRBEvent */*TdcEvent*/)
{

  for (Int_t jChamber=0; jChamber<4; jChamber++) {
    ChamberHitCollector *thisChamber = GetChamber(jChamber);

    // View Hit Debug
    for (Int_t jView=0;jView<4;jView++) {
      ViewHitCollector *viewj = thisChamber->GetView(jView);
      for (Int_t jCluster=0; jCluster<viewj->GetNcluster(); jCluster++) {
        Cluster *clusterj = viewj->Getcluster(jCluster);
        fViewClusterQuality->Fill(clusterj->GetNHit(),clusterj->GetQuality());
        for (Int_t kView=jView+1;kView<4;kView++) {
          ViewHitCollector *viewk = thisChamber->GetView(kView);
          for (Int_t kCluster=0; kCluster<viewk->GetNcluster(); kCluster++) {
            Cluster *clusterk = viewk->Getcluster(kCluster);
            fViewClusterDT->Fill(clusterj->GetTrailingTime()-clusterk->GetTrailingTime());
            for (Int_t iView=kView+1;iView<4;iView++) {
              ViewHitCollector *viewi = thisChamber->GetView(iView);
              for (Int_t iCluster=0; iCluster<viewi->GetNcluster(); iCluster++) {
                Cluster *clusteri = viewi->Getcluster(iCluster);
                Double_t tave3 = (clusterj->GetTrailingTime()+clusterk->GetTrailingTime()+clusteri->GetTrailingTime())/3.;
                Double_t stave3 = (clusteri->GetTrailingTime()-tave3)*(clusteri->GetTrailingTime()-tave3)+
                                  (clusterj->GetTrailingTime()-tave3)*(clusterj->GetTrailingTime()-tave3)+
                                  (clusterk->GetTrailingTime()-tave3)*(clusterk->GetTrailingTime()-tave3);
                stave3 /= 3;
                fViewCluster3TT->Fill(sqrt(stave3)/fPar->GetInter3ViewsTtrailingSigma());
              }
            }
          }
        }
      }
    }

    // Chamber Hits Debug
    for (UInt_t jChHit=0; jChHit<thisChamber->GetNHit(); jChHit++) {
      std::vector<Intersection>::iterator thisChamberHit = thisChamber->GetHit(jChHit);
      fRecoChamberHitTime->Fill(thisChamberHit->GetTrailingTime());
      for (Int_t iChamber=jChamber+1; iChamber<4; iChamber++) {
        ChamberHitCollector *otherChamber = GetChamber(iChamber);
        for (UInt_t iChHit=0; iChHit<otherChamber->GetNHit(); iChHit++) {
          std::vector<Intersection>::iterator otherChamberHit = otherChamber->GetHit(iChHit);
          if (otherChamberHit->GetTrailingTime()!=-999999 && thisChamberHit->GetTrailingTime()!=-999999) fRecoChamberDHitTime->Fill(otherChamberHit->GetTrailingTime()-thisChamberHit->GetTrailingTime());
        }
      }
      fRecoChamberHitTotal[jChamber]->Fill(thisChamberHit->GetXcoor(),thisChamberHit->GetYcoor());
      Int_t type = thisChamberHit->GetType();
      if (type==4) {
        fRecoChamberHit4Total[jChamber]->Fill(thisChamberHit->GetXcoor(),thisChamberHit->GetYcoor());
        fRecoChamberHitQuality4[jChamber]->Fill(thisChamberHit->GetQuality());
      }
      if (type==3) {
        fRecoChamberHit3Total[jChamber]->Fill(thisChamberHit->GetXcoor(),thisChamberHit->GetYcoor());
        fRecoChamberHitQuality3[jChamber]->Fill(thisChamberHit->GetQuality());
      }
      if (type==2) {
        fRecoChamberHit2Total[jChamber]->Fill(thisChamberHit->GetXcoor(),thisChamberHit->GetYcoor());
      }
    }

  }

  // Combinations
  TLorentzVector kaonmomentum;
  Double_t tthx =  0.0012;
  Double_t tthy =  0.;
  Double_t pkaon = 75.0;
  Double_t pkaonz = pkaon/sqrt(1.+tthx*tthx+tthy*tthy);
  Double_t pkaonx = pkaonz*tthx;
  Double_t pkaony = pkaonz*tthy;
  kaonmomentum.SetXYZM(pkaonx,pkaony,pkaonz,0.493667);
  Int_t nComb = fTrackCollector->GetNCombinations();
  if (nComb==1) {
    for (Int_t jComb=0; jComb<nComb; jComb++) {
      Combination *comb = fTrackCollector->GetCombination(jComb);
      fHCombQuality->Fill(comb->GetType(),comb->GetQuality());
      fHCombHough->Fill(comb->GetType(),comb->GetHDelta());
      TLorentzVector pmom;
      Double_t pmomz = fabs(comb->GetP())/sqrt(1.+comb->GetThetaX()*comb->GetThetaX()+comb->GetThetaY()*comb->GetThetaY());
      Double_t pmomx = pmomz*comb->GetThetaX();
      Double_t pmomy = pmomz*comb->GetThetaY();
      pmom.SetXYZM(pmomx/1000,pmomy/1000,pmomz/1000,0.13957018);
      TLorentzVector picmom = kaonmomentum-pmom;
      fHPic_mmiss->Fill(picmom.M2());
      fHPic_mmiss_zoom->Fill(picmom.M2());
      if ((comb->GetP()/1000.)<65 && (comb->GetP()/1000.)>10) fHPic_mmiss_le->Fill(picmom.M2());
      fHPic_momentum->Fill(comb->GetP()/1000.);
      Double_t xback = comb->GetX0()+comb->GetThetaX()*100000;
      Double_t yback = comb->GetY0()+comb->GetThetaY()*100000;
      TVector3 v1 = kaonmomentum.Vect();
      TVector3 v2 = pmom.Vect();
      TVector3 pos1(0,0,100000);
      TVector3 pos2(xback,yback,100000);
      TVector3 r12 = pos1-pos2;
      Double_t v1xv2 = v1.Dot(v2);
      Double_t det   = pow(v1xv2,2)-v1.Mag2()*v2.Mag2();
      if (det) {
        Double_t t1 = (v2.Mag2()*r12.Dot(v1)-v1.Dot(v2)*r12.Dot(v2))/det;
        Double_t t2 = (v1.Dot(v2)*r12.Dot(v1)-v1.Mag2()*r12.Dot(v2))/det;
        TVector3 q1 = pos1+t1*v1;
        TVector3 q2 = pos2+t2*v2;
        TVector3 vertex = 0.5*(q1+q2);
        fHPvsVertex->Fill(vertex.Z(),fabs(comb->GetP()/1000));
        if (vertex.Z()>105000 && comb->GetP()/1000.<35 && comb->GetP()/1000.>15) {
          fHPic_mmiss2->Fill(picmom.M2());
        }
      }

    }
  }

  // Fit
  Int_t trigflag = NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetTriggerFlags();
  for (Int_t jj=0; jj<fRecoEvent->GetNCandidates(); jj++) {
    TRecoSpectrometerCandidate *cand = static_cast<TRecoSpectrometerCandidate*>(fRecoEvent->GetCandidate(jj));
    TLorentzVector pmom;
    Double_t thetaX = cand->GetSlopeXBeforeMagnet();
    Double_t thetaY = cand->GetSlopeYBeforeMagnet();
    Double_t pmag = cand->GetMomentum();
    Double_t pmomz = pmag/sqrt(1.+thetaX*thetaX+thetaY*thetaY);
    Double_t pmomx = pmomz*thetaX;
    Double_t pmomy = pmomz*thetaY;
    pmom.SetXYZM(pmomx/1000,pmomy/1000,pmomz/1000,0.13957018);
    TLorentzVector picmom = kaonmomentum-pmom;

    // All tracks debug
    fHPic_mmiss_fit->Fill(picmom.M2());
    fHPic_momentum_fit->Fill(pmom.P());
    fHMissMassTime->Fill(1e-9*ClockPeriod*NA62RecoManager::GetInstance()->GetEventHeader()->GetTimeStamp(), picmom.M2());
    if (trigflag==1) fHPic_momentum_fit_trig->Fill(pmom.P(),0);
    if (trigflag==2) fHPic_momentum_fit_trig->Fill(pmom.P(),1);
    for (Int_t kmask=0; kmask<8; kmask++) {
      if ((trigflag>>kmask)&1) fHPic_momentum_fit_trig->Fill(pmom.P(),kmask);
    }

    // Good tracks debug
    if (cand->GetNChambers()==4) {
      fHPic_mmiss_fit_4Chambers_pmom->Fill(pmom.P(),picmom.M2());
      if (pmom.P()<65) fHPic_mmiss_fit_4Chambers->Fill(picmom.M2());
      if (pmom.P()<65) fHPic_mmiss_fit_4Chambers_chi2->Fill(picmom.M2(),cand->GetChi2());
      if (trigflag==1) {
        if (pmom.P()<65) fHPic_mmiss_fit_4Chambers_trig->Fill(picmom.M2(),0);
        fHPic_mmiss_fit_4Chambers_pmom_trig[0]->Fill(pmom.P(),picmom.M2());
      }
      if (trigflag==2) {
        if (pmom.P()<65) fHPic_mmiss_fit_4Chambers_trig->Fill(picmom.M2(),1);
        fHPic_mmiss_fit_4Chambers_pmom_trig[1]->Fill(pmom.P(),picmom.M2());
      }
      for (Int_t kmask=0; kmask<8; kmask++) {
        if ((trigflag>>kmask)&1) {
          if (pmom.P()<65) fHPic_mmiss_fit_4Chambers_trig->Fill(picmom.M2(),kmask);
          fHPic_mmiss_fit_4Chambers_pmom_trig[kmask]->Fill(pmom.P(),picmom.M2());
        }
      }
    }

    // Projection at chambers
    for (Int_t jcha=0; jcha<4; jcha++) {
      Double_t posx;
      Double_t posy;
      Double_t zchamber = fGeo->GetChamberZPosition(jcha);
      if (jcha<2) {
        posx = cand->GetPositionBeforeMagnet().X()+(cand->GetSlopeXBeforeMagnet())*(zchamber-cand->GetPositionBeforeMagnet().Z());
        posy = cand->GetPositionBeforeMagnet().Y()+(cand->GetSlopeYBeforeMagnet())*(zchamber-cand->GetPositionBeforeMagnet().Z());
      } else {
        posx = cand->GetPositionAfterMagnet().X()+(cand->GetSlopeXAfterMagnet())*(zchamber-cand->GetPositionAfterMagnet().Z());
        posy = cand->GetPositionAfterMagnet().Y()+(cand->GetSlopeYAfterMagnet())*(zchamber-cand->GetPositionAfterMagnet().Z());
      }
      fHIllumTotal[jcha]->Fill(posx,posy);
      if (cand->GetNChambers()==4) fHIllum4Chambers[jcha]->Fill(posx,posy);
      else {
        fHIllum3Chambers[jcha]->Fill(posx,posy);
        Int_t skipChamber = -1;
        for (Int_t kcha=0; kcha<4; kcha++) {
          if (cand->GetChamberId(kcha)==-1) skipChamber = kcha;
        }
        if (skipChamber==0) fHIllum123[jcha]->Fill(posx,posy);
        if (skipChamber==1) fHIllum023[jcha]->Fill(posx,posy);
        if (skipChamber==2) fHIllum013[jcha]->Fill(posx,posy);
        if (skipChamber==3) fHIllum012[jcha]->Fill(posx,posy);
      }
    }

  }

}

void SpectrometerReconstruction::InitHistograms()
{
/// \MemberDescr
/// Initialization of the histograms.
/// \EndMemberDescr

  GetOrMakeDir(fHistoFile,"SpectrometerMonitor")->cd();

  // Digi handling
  fSpectrometerDigiManager->InitHistograms(fPar->GetHistoDebug());

  fDetectedEdge = new TH2F("detectededge","",7808,0,7808,2,0,2);
  fDetectedEdge2 = new TH2F("detectededge2","",122,0,122,64,0,64);
  fTrailingVSDrift = new TH2F("trailingvsdrift","",200,-100,300,200,-100,300);
  fTrailingVSDrift2 = new TH2F("trailingvsdrift2","",200,-100,300,200,-100,300);

  fHSlope = new TH2F("hSlope","",300,-0.15,0.15,200,0.,20.);
  fNGoodHitPerPlane = new TH2F("ngoodvsplaneid","",64,0,64,122,0,122);
  fNGoodHitPerEvent = new TH2F("ngoodvseventid","",100,0,100000,200,0,200);
  fOverflowHisto = new TH1F("overflowhisto","",1000,0,200000000);

  for (Int_t jch = 0; jch < fNChambers; jch++){
    fRecoChamberHitTotal[jch] = new TH2F(Form("RecoHit_ch%d", jch)," ",220,-1100,1100,220,-1100,1100);
    fRecoChamberHit4Total[jch] = new TH2F(Form("RecoHit4Views_ch%d", jch)," ",220,-1100,1100,220,-1100,1100);
    fRecoChamberHitQuality4[jch] = new TH1F(Form("RecoHit4Quality_ch%d", jch)," ",200,0,25);
    fRecoChamberHit3Total[jch] = new TH2F(Form("RecoHit3Views_ch%d", jch)," ",220,-1100,1100,220,-1100,1100);
    fRecoChamberHitQuality3[jch] = new TH1F(Form("RecoHit3Quality_ch%d", jch)," ",200,0,25);
    fRecoChamberHit2Total[jch] = new TH2F(Form("RecoHit2Views_ch%d", jch)," ",220,-1100,1100,220,-1100,1100);
  }
  fRecoChamberHitTime = new TH1F("ChamberTrailingTime"," ",400,-200,400);
  fRecoChamberDHitTime = new TH1F("ChamberDTrailingTime"," ",200,-200,200);
  fRecoHitWireSum2 = new TH1F("RecoHitWireSum2","",200,0.,20.);
  fRecoHitWireSum22 = new TH2F("RecoHitWireSum22","",200,0.,20.,200,0.,10.);
  fRecoHitWireSum23 = new TH2F("RecoHitWireSum23","",200,0.,10.,200,0.,10.);
  fRecoHitWireSum24 = new TH2F("RecoHitWireSum24","",4,0,4,200,0.,20.);
  fRecoHitWireSum25 = new TH2F("RecoHitWireSum25","",200,0,20,500,0.,2000.);
  fRecoHitWireSum2VsMagicT0 = new TH2F("RecoHitWireSum2VsMagicT0","",100,19.95,29.95,400,0.0125,10.0125);
  fRecoHitWireSum2VsROMezzanine = new TH2F("RecoHitWireSum2VsROMezzanine","",
      fRawDecoder->GetDecoder()->GetNROMezzanines(),0.5,fRawDecoder->GetDecoder()->GetNROMezzanines()-0.5,200,0.,20.);
  fViewClusterQuality = new TH2F("ViewClusterQuality","",4,0,4,100,0,10);
  fViewClusterDT = new TH1F("ViewClusterDT","",200,-200,200);
  fViewCluster3TT = new TH1F("ViewCluster3TT","",200,0,20);

  fHCombQuality = new TH2F("CombQuality","",5,0,5,100,0,10);
  fHCombHough = new TH2F("CombHough","",5,0,5,100,0,10);
  fHPic_mmiss = new TH1F("Pic_mmiss","",125,-0.1,0.15);
  fHPic_mmiss_le = new TH1F("Pic_mmiss_le","",125,-0.1,0.15);
  fHPic_mmiss_zoom = new TH1F("Pic_mmiss_zoom","",375,-0.1,0.15);
  fHPic_momentum = new TH1F("Pic_momentum","",200,-100,100);
  fHPvsVertex = new TH2F("PvsVertex","",210,90000,200000,100,0,100);
  fHPic_mmiss2 = new TH1F("Pic_mmiss2","",125,-0.1,0.15);
  fHPic_mmiss_fit = new TH1F("Pic_mmiss_fit","",125,-0.1,0.15);
  fHPic_mmiss_fit_4Chambers = new TH1F("Pic_mmiss_fit_4Chambers","",125,-0.1,0.15);
  fHPic_mmiss_fit_4Chambers_trig = new TH2F("Pic_mmiss_fit_4Chambers_trig","",125,-0.1,0.15,8,0,8);
  fHPic_mmiss_fit_4Chambers_chi2 = new TH2F("Pic_mmiss_fit_4Chambers_chi2","",125,-0.1,0.15,200,0,100);
  fHPic_mmiss_fit_4Chambers_pmom = new TH2F("Pic_mmiss_fit_4Chambers_pmom","",100,0,100,125,-0.1,0.15);
  for (int imask = 0; imask < 8; imask++) {
    fHPic_mmiss_fit_4Chambers_pmom_trig[imask] =
      new TH2F(Form("Pic_mmiss_fit_4Chambers_pmom_trig%d", imask),"",50,0,100,125,-0.1,0.15);
  }
  fHPic_momentum_fit = new TH1F("Pic_momentum_fit","",200,-100,100);
  fHPic_momentum_fit_trig = new TH2F("Pic_momentum_fit_trig","",200,-100,100,8,0,8);
  fHMissMassTime = new TH2F("MissingMassVSrefTime", "Missing Mass vs. Reference Time; T_{reference}; M_{miss}^{2}(#pi) [GeV^{2}/c^{4}]", 850, 0., 6.8, 70, -0.15, 0.13);
  for (Int_t jcha = 0; jcha < fNChambers; jcha++) {
    fHIllumTotal[jcha] =  new TH2F(Form("XYTotal_ch%d", jcha),"",220,-1100,1100,220,-1100,1100);
    fHIllum4Chambers[jcha] = new TH2F(Form("XY4Chambers_ch%d", jcha),"",220,-1100,1100,220,-1100,1100);
    fHIllum3Chambers[jcha] = new TH2F(Form("XY3Chambers_ch%d", jcha),"",220,-1100,1100,220,-1100,1100);
    fHIllum123[jcha] = new TH2F(Form("XY123_ch%d", jcha),"",220,-1100,1100,220,-1100,1100);
    fHIllum023[jcha] = new TH2F(Form("XY023_ch%d", jcha),"",220,-1100,1100,220,-1100,1100);
    fHIllum013[jcha] = new TH2F(Form("XY013_ch%d", jcha),"",220,-1100,1100,220,-1100,1100);
    fHIllum012[jcha] = new TH2F(Form("XY012_ch%d", jcha),"",220,-1100,1100,220,-1100,1100);
  }
  // Debug histos for LR ambiguity and View hit reconstruction
  for (Int_t jChamber = 0; jChamber < 4; jChamber++)
    for (Int_t jView = 0; jView < 4; jView++)
      GetChamber(jChamber)->GetView(jView)->InitHistograms("ViewHitCollector");

  // Checks for full digitization
  fHStep = new TH1F("Step","Step",100,0.,10.);
  fHNIonPairs = new TH1F("NIonPairs","# of total Ion Pairs per mm",100,0.,10.);
  fHMaxClusterDelay = new TH2F("MaxClusterDelay","Maximum Cluster Delay (Measured Vs True)",1000,0.,200.,1000,0.,200.);

  fHistoFile->cd("/");
}

void SpectrometerReconstruction::SaveHistograms()
{
/// \MemberDescr
/// Writeout of the histograms.
/// \EndMemberDescr
  fHistoFile->cd("SpectrometerMonitor");

  fSpectrometerDigiManager->SaveHistograms();

  if (fHSlope) fHSlope->Write();

  if (fTrailingVSDrift) fTrailingVSDrift->Write();
  if (fTrailingVSDrift2) fTrailingVSDrift2->Write();
  if (fDetectedEdge) fDetectedEdge->Write();
  if (fDetectedEdge2) fDetectedEdge2->Write();
  if (fOverflowHisto) fOverflowHisto->Write();
  if (fNGoodHitPerPlane) fNGoodHitPerPlane->Write();
  if (fNGoodHitPerEvent) fNGoodHitPerEvent->Write();

  for (Int_t jch=0;jch<fNChambers;jch++)
  {
    if (fRecoChamberHitTotal[jch]) fRecoChamberHitTotal[jch]->Write();
    if (fRecoChamberHit4Total[jch]) fRecoChamberHit4Total[jch]->Write();
    if (fRecoChamberHitQuality4[jch]) fRecoChamberHitQuality4[jch]->Write();
    if (fRecoChamberHit3Total[jch]) fRecoChamberHit3Total[jch]->Write();
    if (fRecoChamberHitQuality3[jch]) fRecoChamberHitQuality3[jch]->Write();
    if (fRecoChamberHit2Total[jch]) fRecoChamberHit2Total[jch]->Write();
  }
  if (fRecoChamberHitTime) fRecoChamberHitTime->Write();
  if (fRecoChamberDHitTime) fRecoChamberDHitTime->Write();
  if (fRecoHitWireSum2) fRecoHitWireSum2->Write();
  if (fRecoHitWireSum22) fRecoHitWireSum22->Write();
  if (fRecoHitWireSum23) fRecoHitWireSum23->Write();
  if (fRecoHitWireSum24) fRecoHitWireSum24->Write();
  if (fRecoHitWireSum25) fRecoHitWireSum25->Write();
  if (fRecoHitWireSum2VsMagicT0) fRecoHitWireSum2VsMagicT0->Write();
  if (fRecoHitWireSum2VsROMezzanine) fRecoHitWireSum2VsROMezzanine->Write();
  if (fViewClusterQuality) fViewClusterQuality->Write();
  if (fViewClusterDT) fViewClusterDT->Write();
  if (fViewCluster3TT) fViewCluster3TT->Write();

  if (fHCombQuality) fHCombQuality->Write();
  if (fHCombHough) fHCombHough->Write();
  if (fHPic_mmiss) fHPic_mmiss->Write();
  if (fHPic_mmiss_le) fHPic_mmiss_le->Write();
  if (fHPic_mmiss_zoom) fHPic_mmiss_zoom->Write();
  if (fHPic_momentum) fHPic_momentum->Write();
  if (fHPvsVertex) fHPvsVertex->Write();
  if (fHPic_mmiss2) fHPic_mmiss2->Write();
  if (fHPic_momentum_fit) fHPic_momentum_fit->Write();
  if (fHPic_momentum_fit_trig) fHPic_momentum_fit_trig->Write();
  if (fHPic_mmiss_fit) fHPic_mmiss_fit->Write();
  if (fHPic_mmiss_fit_4Chambers) fHPic_mmiss_fit_4Chambers->Write();
  if (fHPic_mmiss_fit_4Chambers_trig) fHPic_mmiss_fit_4Chambers_trig->Write();
  if (fHPic_mmiss_fit_4Chambers_chi2) fHPic_mmiss_fit_4Chambers_chi2->Write();
  if (fHPic_mmiss_fit_4Chambers_pmom) fHPic_mmiss_fit_4Chambers_pmom->Write();
  if (fHMissMassTime) fHMissMassTime->Write();
  for (Int_t kk=0; kk<8; kk++)
    if (fHPic_mmiss_fit_4Chambers_pmom_trig[kk]) fHPic_mmiss_fit_4Chambers_pmom_trig[kk]->Write();
  for (Int_t jcha=0; jcha<fNChambers; jcha++) {
    if (fHIllumTotal[jcha]) fHIllumTotal[jcha]->Write();
    if (fHIllum4Chambers[jcha]) fHIllum4Chambers[jcha]->Write();
    if (fHIllum3Chambers[jcha]) fHIllum3Chambers[jcha]->Write();
    if (fHIllum123[jcha]) fHIllum123[jcha]->Write();
    if (fHIllum023[jcha]) fHIllum023[jcha]->Write();
    if (fHIllum013[jcha]) fHIllum013[jcha]->Write();
    if (fHIllum012[jcha]) fHIllum012[jcha]->Write();
  }
  fTrackCollector->SaveHistograms();

  for (Int_t jChamber = 0; jChamber < 4; jChamber++)
    for (Int_t jView = 0; jView < 4; jView++)
      GetChamber(jChamber)->GetView(jView)->SaveHistograms("ViewHitCollector");

  // CHECK FOR FULL DIGITIZATION
  if (fHStep) fHStep->Write();
  if (fHNIonPairs) fHNIonPairs->Write();
  if (fHMaxClusterDelay) fHMaxClusterDelay->Write();

  fHistoFile->cd("/");
}

Double_t SpectrometerReconstruction::ReferenceTime() {
  Double_t refTime = 0.;
  Double_t fineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256;

  if (fPar->GetTimeReference()=="CHOD") {
    refTime = 0.;
    Int_t nchodhits = 0;
    Double_t DTimeMax = 15.;
    if (!fRecoCHODEvent) return refTime; //protection for no CHOD
    for (Int_t j=0; j<fRecoCHODEvent->GetNHits(); j++) {
      TRecoCHODHit* recoHit = static_cast<TRecoCHODHit*>(fRecoCHODEvent->GetHit(j));
      if (recoHit->GetChannelID()>=128) continue;
      if (fabs(recoHit->GetTime()-fineTime+0.8)<DTimeMax) {
        refTime += recoHit->GetTime();
        nchodhits++;
      }
    }
    if (nchodhits) refTime /= (Double_t)nchodhits;
    else {
      for (Int_t j=0; j<fRecoCHODEvent->GetNHits(); j++) {
        TRecoCHODHit* recoHit = static_cast<TRecoCHODHit*>(fRecoCHODEvent->GetHit(j));
        if (recoHit->GetChannelID()>=128) continue;
        refTime += recoHit->GetTime();
        nchodhits++;
      }
      for (Int_t j=0; j<fRecoCHODEvent->GetNHits(); j++) {
        TRecoCHODHit* recoHit = static_cast<TRecoCHODHit*>(fRecoCHODEvent->GetHit(j));
        if (recoHit->GetChannelID()>=128) continue;
        if (fabs(recoHit->GetTime()-refTime/((Double_t)nchodhits))>=15) {
          refTime -= recoHit->GetTime();
          nchodhits--;
        }
      }
      if (nchodhits) refTime /= (Double_t)nchodhits;

    }
    fNCHODHitsInTime = nchodhits;
  } else { // Use trigger as a time reference to be used for year >= 2015
    refTime = fineTime;
  }

  return refTime;
}
