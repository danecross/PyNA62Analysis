#include "SpectrometerDigiManager.hh"
#include "SRBEvent.hh"
#include "SpectrometerRawDecoder.hh"
#include "NA62RecoManager.hh"
#include "SpectrometerReconstruction.hh"
#include "TRecoSpectrometerEvent.hh"
#include "SpectrometerParameters.hh"
#include "SRBRawDecoder.hh"
#include "TSpectrometerDigi.hh"
#include "TSpectrometerHit.hh"

#define PARAMETRIZEDDIGI

/// \class SpectrometerDigiManager
/// \Brief
/// Class for straw digi handling.
/// \EndBrief
/// \Detailed
/// The class performs the handling of the straw hits:
///  - DigiToReco: selection of the first leading and last trailing time.
///    A time window of [-100,300] ns ([-100,5250] ns) around the trigger time (t=0) is
///    opened to determine the leading time in case of the parametric (full) digitization.
///
/// PARAMETRIZEDDIGI allows the switching between the full and parametric digitization.
/// \EndDetailed

SpectrometerDigiManager::SpectrometerDigiManager(SpectrometerReconstruction * Reco):
    fTdcEvent(nullptr), fRawDecoder(nullptr),
    fReco(Reco),
    fMagicT0(.0),
    fHRTDependence(nullptr), fHTRDependence(nullptr),
    fHStrawSpaceResolution(nullptr), fHAll(nullptr), fHAllLeading(nullptr),
    fHAllLeadingSRB(nullptr), fHAllLeadingCover(nullptr), fHFirstLeading(nullptr),
    fHAllTrailing(nullptr), fHLastTrailing(nullptr), fHDiff(nullptr), fHRadius(nullptr),
    fHChannelActivity(nullptr), fHNleadingsInTime(nullptr), fHRadius2D(nullptr),
    fHRadiusTotal(nullptr), fHLeadingTimeTotal(nullptr), fHTrailingTimeTotal(nullptr)
{
  fPar = SpectrometerParameters::GetInstance();
  fTWindowLeadMin = 0.; ;       // ns
  fTWindowLeadMax = 190.; ;     // ns
  fTWindowTrailMin = 0.; ;      // ns
  fTWindowTrailMax = 300.; ;    // ns
  fRTMode = fPar->GetRTMode();
  for (int ich = 0; ich < 8000; ich++) {
    driftPerChannel[ich] = 0;
    digiLeadingPerChannel[ich] = 0;
    trailingPerChannel[ich] = 0;
    digiTrailingPerChannel[ich] = 0;
  }
}

SpectrometerDigiManager::~SpectrometerDigiManager()
{ }

void SpectrometerDigiManager::DigiToReco(TRecoVEvent *recoEvent, Double_t refTime, Int_t /*nevent*/) {
/// \MemberDescr
/// Straw drift time computation as the smallest leading within a time window selected on the basis of the total
/// drift time and on the leading time resolution (to be optimized).
/// \n
/// Straw trailing time computation as the largest trailing within a time window selected on the basis of the trailing
/// time resolution (to be optimized).
/// \EndMemberDescr
  Int_t nDigis = fTdcEvent->GetNHits();
  if (!nDigis) return;
  TClonesArray& Digis = (*(fTdcEvent->GetHits()));

  Int_t RunNumber = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  if (!fPar->GetIsRawData()) RunNumber = 0;

  // Compute the drift time in each straw in time with the reference time
  fill(driftPerChannel,driftPerChannel+8000,99999999999.);
  fill(digiLeadingPerChannel,digiLeadingPerChannel+8000,-1);
  TRecoSpectrometerHit* Hit = 0;
  Int_t ntotleadings = 0;
  Int_t NleadingsInTime = 0; // OnlineMonitor variable
  for (Int_t iDigi=0; iDigi<nDigis; iDigi++) { // Loop over leadings
    TSpectrometerDigi *Digi = static_cast<TSpectrometerDigi*>( Digis[iDigi]);
    if (Digi->GetMultiHit()) continue; // Skip channels with decoding errors
    if (!fPar->GetIsRawData()) Digi->DecodeChannelID();
    Int_t ChID = Digi->GetChannelID();
    Int_t planeid = 16*Digi->GetChamberID()+4*Digi->GetViewID()+2*Digi->GetHalfViewID()+Digi->GetPlaneID();
    Int_t planeidMon = 1 + 21*Digi->GetChamberID()+5*Digi->GetViewID()+2*Digi->GetHalfViewID()+Digi->GetPlaneID();// planeid used in online monitor plots
    Int_t strawid = Digi->GetStrawID()+122*planeid;
    if (fHAll) fHAll->Fill(strawid); // Straw profile (leading + trailings)
    if ((Digi->GetDetectedEdge() & SRB_HIT_EDGE_LEADING) == 0) continue; // Select digis containing leadings
    ntotleadings++; // counter for monitoring only

    // Compute the leading time wrt reference time
    Double_t ChannelT0 = fPar->GetIsRawData() ? fPar->GetT0(planeid,Digi->GetStrawID()+1) : 0;
    Double_t DriftTime = Digi->GetLeadingEdge()-fReco->GetT0Correction(Digi)-ChannelT0-refTime+fPar->GetMagicT0();
    if (fHAllLeadingSRB) fHAllLeadingSRB->Fill(DriftTime,Digi->GetSRBAddr()); // Leading time per SRB
    if (fHAllLeadingCover) fHAllLeadingCover->Fill(DriftTime,GetGlobalCoverID(Digi)); // Leading time per Cover
    if (fHAllLeading) fHAllLeading->Fill(DriftTime,strawid); // Leading time per straw
    if (fHChannelActivity) fHChannelActivity->Fill(Digi->GetStrawID(), planeidMon); // Channel activity

    // Skip leadings not in time wrt the reference time
    if (DriftTime >= fTWindowLeadMax || DriftTime <= fTWindowLeadMin) continue;

    // Assign this leading to ChID if no other leading has been already assigned to this straw
    if (digiLeadingPerChannel[ChID]==-1) {
      driftPerChannel[ChID] = DriftTime;
      digiLeadingPerChannel[ChID] = iDigi;
      continue;
    }

    // If another leading has been assigned to this straw, keep the smalles leading time in this straw
    if (DriftTime!=driftPerChannel[ChID] && !SelectDriftTime(DriftTime,driftPerChannel[ChID])) continue; // condition against accidentals
    if (DriftTime<driftPerChannel[ChID]) {
      driftPerChannel[ChID] = DriftTime;
      digiLeadingPerChannel[ChID] = iDigi;
    }

  } // End of loop over leadings

  // Store the drift time
  for (Int_t jch=0; jch<8000; jch++) { // Loop over the channels
    if (digiLeadingPerChannel[jch]<0) continue; // channels without drift time
    TSpectrometerDigi *Digi = static_cast<TSpectrometerDigi*>( Digis[digiLeadingPerChannel[jch]]);
    Double_t DriftTime = driftPerChannel[jch];
    Int_t planeid = 16*Digi->GetChamberID()+4*Digi->GetViewID()+2*Digi->GetHalfViewID()+Digi->GetPlaneID();
    Int_t strawid = Digi->GetStrawID()+122*planeid;
    Double_t ChannelT0 = 0;
    if (fPar->GetIsRawData()) {
      if (RunNumber==1348) ChannelT0 = fPar->GetT0(planeid,Digi->GetStrawID());
      else                 ChannelT0 = fPar->GetT0(planeid,Digi->GetStrawID()+1);
    }
    Double_t DriftTimeNoT0 = DriftTime+ChannelT0;

    // Compute the wire distance
    Double_t t0var = fReco->GetT0Correction(Digi)-ChannelT0-refTime+fPar->GetMagicT0();
    Double_t WireDistance = 0;
    TSpectrometerHit* MCHit = static_cast<TSpectrometerHit*>(Digi->GetMCHit());
    if (MCHit) { // Use the RT dependence for MC according to the digitization algorithm used
#ifdef PARAMETRIZEDDIGI
      WireDistance = fPar->GetRTParametricDependence(DriftTime/1000.);
#else
      WireDistance = fPar->GetRTDependence(DriftTime);
#endif
    } else { // DATA
      if (fRTMode) WireDistance = fPar->GetRTDependenceDataFull((122*planeid+Digi->GetStrawID()+1),DriftTime,t0var    ); // RT measured on data: to be debugged yet
      if (!fRTMode) WireDistance = fPar->GetRTDependenceData(DriftTime/1000.);
      //  WireDistance = fPar->GetRTDependenceData(DriftTime/1000.); // RT extracted from garfield
      //  WireDistance = fPar->GetRTDependenceDataFull(planeid,Digi->GetStrawID()+1,DriftTime); // RT measured on data: to be debugged yet
      if (WireDistance<0)   WireDistance = 0; // Inner boundary condition
      if (WireDistance>4.9) WireDistance = 4.9; // Outer boundary condition
    }

    // Digi Check for MC only
    if (MCHit) {
      Double_t TrueWireDistance = MCHit->GetWireDistance();
      if (fHRTDependence) fHRTDependence->Fill(DriftTime/1000.,TrueWireDistance);
      if (fHTRDependence) fHTRDependence->Fill(TrueWireDistance,DriftTime/1000.);
      if (fHStrawSpaceResolution) fHStrawSpaceResolution->Fill(TrueWireDistance,WireDistance-TrueWireDistance);
    }

    // Store the reconstructed hits
    Hit = static_cast<TRecoSpectrometerHit*>(recoEvent->AddHit(Digi));
    Hit->SetDriftTime(DriftTime);
    Hit->SetWireDistance(WireDistance);
    Hit->SetTimeWidth(-999999.); // Wait for trailing to define the time width
    Hit->SetNUsedDigis(1);
    Hit->SetHalfViewID(Digi->GetHalfViewID());
    Hit->SetViewID(Digi->GetViewID());
    Hit->SetChamberID(Digi->GetChamberID());
    Hit->SetPlaneID(Digi->GetPlaneID());
    Hit->SetStrawID(Digi->GetStrawID());
    Hit->SetSingle(0);
    Hit->SetTDCID(GetGlobalCoverID(Digi));
    Hit->SetChannelID(Digi->GetChannelID());
    Hit->SetRecoID(Digi->GetChannelID());
    Hit->SetEdgeStatus(0);
    Hit->SetEnergy(-fReco->GetT0Correction(Digi)-ChannelT0-refTime+fPar->GetMagicT0());

    // Histos for monitoring
    if (fHFirstLeading) fHFirstLeading->Fill(DriftTime,strawid);
    if (fHLeadingTimeTotal) fHLeadingTimeTotal->Fill(DriftTime);
    Int_t srbid = Digi->GetSRBAddr();
    if (fReco->GetHRecoHitTimeWrtReferenceVsROChannel())
      fReco->GetHRecoHitTimeWrtReferenceVsROChannel()->Fill(256*srbid+Digi->GetStrawAddr(),DriftTime);
    if (fReco->GetHRecoHitTimeWrtReferenceVsROChannelNoT0())
      fReco->GetHRecoHitTimeWrtReferenceVsROChannelNoT0()->Fill(256*srbid+Digi->GetStrawAddr(),DriftTimeNoT0);
    if (DriftTime>=0&&DriftTime<170) {
      if (fHRadius) fHRadius->Fill(WireDistance,strawid);
      if (fHRadiusTotal) fHRadiusTotal->Fill(WireDistance);
      if (fHRadius2D) fHRadius2D->Fill(DriftTime,WireDistance);
    }
    NleadingsInTime++;
  } // end loop over channels

  if (fHNleadingsInTime) fHNleadingsInTime->Fill(NleadingsInTime);

  // Compute the trailing time in each straw in time with the reference time
  fill(trailingPerChannel,trailingPerChannel+8000,-99999999999.);
  fill(digiTrailingPerChannel,digiTrailingPerChannel+8000,-1);
  for (Int_t iDigi=0; iDigi<nDigis; iDigi++) {
    TSpectrometerDigi *Digi = static_cast<TSpectrometerDigi*>( Digis[iDigi]);
    if (Digi->GetMultiHit()) continue;
    if ((Digi->GetDetectedEdge() & SRB_HIT_EDGE_TRAILING) == 0) continue; // Select digis containing trailings
    if (!fPar->GetIsRawData()) Digi->DecodeChannelID();
    Int_t ChID = Digi->GetChannelID();
    Int_t planeid = 16*Digi->GetChamberID()+4*Digi->GetViewID()+2*Digi->GetHalfViewID()+Digi->GetPlaneID();
    Int_t strawid = Digi->GetStrawID()+122*planeid;
    Double_t ChannelT0 = 0;
    if (fPar->GetIsRawData()) {
      if (RunNumber==1348) ChannelT0 = fPar->GetT0(planeid,Digi->GetStrawID());
      else                 ChannelT0 = fPar->GetT0(planeid,Digi->GetStrawID()+1);
    }
    Double_t TrailingTime = Digi->GetTrailingEdge()-fReco->GetT0Correction(Digi)-ChannelT0-refTime+fPar->GetMagicT0();
    if (fHAllTrailing) fHAllTrailing->Fill(TrailingTime,strawid); // trailing time per straw

    // Skip trailings not in time wrt the reference time
    if (TrailingTime >= fTWindowTrailMax || TrailingTime<=fTWindowTrailMin) continue;

    // Assign this trailing to ChID if no other trailing has been already assigned to this straw
    if (digiTrailingPerChannel[ChID]==-1) {
      trailingPerChannel[ChID] = TrailingTime;
      digiTrailingPerChannel[ChID] = iDigi;
      continue;
    }

    // If another trailing has been assigned to this straw, keep the largest trailing time in the same straw
    if (TrailingTime>trailingPerChannel[ChID]) {
      trailingPerChannel[ChID] = TrailingTime;
      digiTrailingPerChannel[ChID] = iDigi;
    }

  } // End loop over trailings

  // Store the trailing time
  for (Int_t jch=0; jch<8000; jch++) { // Loop over channels
    if (digiTrailingPerChannel[jch]<0) continue;
    TSpectrometerDigi *Digi = static_cast<TSpectrometerDigi*>( Digis[digiTrailingPerChannel[jch]]);
    Double_t TrailingTime = trailingPerChannel[jch];
    Int_t planeid = 16*Digi->GetChamberID()+4*Digi->GetViewID()+2*Digi->GetHalfViewID()+Digi->GetPlaneID();
    Int_t strawid = Digi->GetStrawID()+122*planeid;
    Int_t ChID = Digi->GetChannelID();

    // Pair this trailing with an existing drift time
    for (Int_t jHit=0; jHit<recoEvent->GetNHits(); jHit++) { // Loop over reco hits with drift time
      Hit = static_cast<TRecoSpectrometerHit*>((recoEvent->GetHits())->At(jHit));
      if (Hit->GetChannelID()==ChID) {
        Double_t TimeWidth = TrailingTime-Hit->GetDriftTime();
        Hit->SetTimeWidth(TimeWidth);
        Hit->SetEdgeStatus(1); // Define the type of hits: 1 if both leadings and trailings exist
        if (fHLastTrailing) fHLastTrailing->Fill(TrailingTime,strawid);
        if (fHTrailingTimeTotal) fHTrailingTimeTotal->Fill(TrailingTime);
      }
    } // End loop over reco hits

  } // End loop over channels

}

void SpectrometerDigiManager::InitHistograms(Int_t monitorLevel)
{
  if (monitorLevel <= 0) return;
  double minTime = -150.;   // [ns]
  double maxTime = 450.;    // [ns]
  Int_t nBins = (Int_t)((maxTime - minTime) / SRBRawDecoder::kTIMEBINSIZE);
  nBins = (nBins / 32) * 32;      // round down to multiple of 32 (one 25 ns time slot)
  maxTime = minTime + nBins*SRBRawDecoder::kTIMEBINSIZE;

  if (monitorLevel >= 1) {
    fHChannelActivity = new TH2F("strawChannelActivity","Straw Channel Activity; Straw ID",
                                 122, 0, 122, 84, 0, 84);
    fHAll = new TH1F("strawID",";straw ID", 7808, 0, 7808);
    fHAllLeadingSRB = new TH2F("srbIDvsAllLeadingTime",";t_{lead} [ns];SRB ID",
                               nBins, minTime, maxTime, 32, 0, 32);
    fHAllLeadingCover = new TH2F("coverIDvsAllLeadingTime","",
                                 nBins, minTime, maxTime, 512, 0, 512);
  }
  if (monitorLevel >= 3) {
    fHAllLeading = new TH2F("strawIDvsAllLeadingTime",";t_{lead} [ns];straw ID",
                            nBins, minTime, maxTime, 7808, 0, 7808);
    fHFirstLeading = new TH2F("strawIDvsFirstLeadingTime",";t_{lead} [ns];straw ID",
                              nBins, minTime, maxTime, 7808, 0, 7808);
    fHAllTrailing  = new TH2F("strawIDvsAllTrailingTime",";t_{trail} [ns];straw ID",
                              nBins, minTime, maxTime, 7808, 0, 7808);
    fHLastTrailing = new TH2F("strawIDvsLastTrailingTime",";t_{trail} [ns];straw ID",
                              nBins, minTime, maxTime, 7808, 0, 7808);
    fHDiff = new TH2F("diff",";t_{lead1} [ns];t_{lead2} [ns]",
                      nBins, minTime, maxTime, nBins, minTime, maxTime);
    fHRadius = new TH2F("strawIDvsRadius",";R [mm]; straw ID", 200, 0, 5, 7808,0,7808);
  }

  // OnlineMonitor histograms
  if (monitorLevel >= 2) {
    fHNleadingsInTime  = new TH1I("strawLeadingsInTime","N in-time leadings; N_{leadings}",
                                  201, -0.5, 200.5);
    fHLeadingTimeTotal = new TH1F("strawLeadingTotal","All straws: first leading time",
                                  nBins, minTime, maxTime);
    fHTrailingTimeTotal = new TH1F("strawTrailingTotal","All straws: last trailing time",
                                   nBins, minTime, maxTime);
    fHRadiusTotal = new TH1F("strawRadius", "All straws: hit radius; R[mm]", 200, 0, 5);
    fHRadius2D = new TH2F("strawRadius2D", "All straws: Radius vs Leading; t_{lead} [ns]; R [mm]",
                          nBins, minTime, maxTime, 200, 0, 5);
  }
  // MC true histograms
  if (monitorLevel >= 2) {
    fHRTDependence = new TH2F("RTDependence","Wire Distance (mm) Vs Leading Time (mus)",
                              400,-0.05,0.35,600,0.,6.);
    fHTRDependence = new TH2F("TRDependence","Leading Time Vs Wire (mus) Distance (mm)",
                              600,0.,6.,400,-0.05,0.35);
    fHStrawSpaceResolution = new TH2F("StrawSpaceResolution",
                                      "Straw space resolution (mm) Vs Wire Distance (mm)",
                                      600,0.,6.,1200,-6.,6.);
  }

}

void SpectrometerDigiManager::SaveHistograms() {
  if (fHChannelActivity) fHChannelActivity->Write();
  if (fHAll) fHAll->Write();
  if (fHAllLeadingSRB) fHAllLeadingSRB->Write();
  if (fHAllLeadingCover) fHAllLeadingCover->Write();
  if (fHAllLeading) fHAllLeading->Write();
  if (fHFirstLeading) fHFirstLeading->Write();
  if (fHAllTrailing) fHAllTrailing->Write();
  if (fHLastTrailing) fHLastTrailing->Write();
  if (fHDiff) fHDiff->Write();
  if (fHRadius) fHRadius->Write();

  // SpectrometerOnlineMonitor histograms
  if (fHLeadingTimeTotal) fHLeadingTimeTotal->Write();
  if (fHTrailingTimeTotal) fHTrailingTimeTotal->Write();
  if (fHNleadingsInTime) fHNleadingsInTime->Write();
  if (fHRadiusTotal) fHRadiusTotal->Write();
  if (fHRadius2D) fHRadius2D->Write();

  // True (MC) histograms
  if (fHRTDependence) fHRTDependence->Write();
  if (fHTRDependence) fHTRDependence->Write();
  if (fHStrawSpaceResolution) fHStrawSpaceResolution->Write();
}

Int_t SpectrometerDigiManager::GetGlobalCoverID(TSpectrometerDigi* digi) {
  Int_t srbid = digi->GetSRBAddr(); // from 0 to 31
  Int_t coverid = (digi->GetStrawAddr()&0xf0)>>4; // from 0 to 15
  return srbid*16 + coverid;
}

Bool_t SpectrometerDigiManager::SelectDriftTime(Double_t DriftTime, Double_t DriftTime2) {
  if (DriftTime<=50.) return 0;
  if (DriftTime>50.) {
    Bool_t accept = 0;
    Double_t a1 = (188.-0.)/(225-36.9);
    Double_t b1 = 188.-a1*225.;
    Double_t a2 = (136.-109.)/(225.-198.);
    Double_t b2 = 136.-a2*225.;
    Double_t a3 = (15.-109.)/(225.-198.);
    Double_t b3 = 15.-a3*225.;
    Double_t a4 = (15.-0.)/(225.-36.9);
    Double_t b4 = 15.-a4*225.;
    if (DriftTime2<a1*DriftTime+b1) {
      if (DriftTime2>a4*DriftTime+b4) {
        accept = 1;
        if (DriftTime>198 && DriftTime2>109) {
          if (DriftTime2<a2*DriftTime+b2) accept = 0;
        }
        if (DriftTime>198 && DriftTime2<=109) {
          if (DriftTime2>a3*DriftTime+b3) accept = 0;
        }
      }
      if (DriftTime2<=a4*DriftTime+b4) accept = 0;
    }
    if (!accept) return 0;
  }
  if (fHDiff) fHDiff->Fill(DriftTime,DriftTime2);
  return 1;
}
