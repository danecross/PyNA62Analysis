#include "Riostream.h"
#include "TGraph.h"
#include "TLine.h"
#include "TText.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TVirtualFFT.h"

#include "TSpectrometerHit.hh"
#include "TSpectrometerDigi.hh"
#include "TSpectrometerEvent.hh"
#include "SRBEvent.hh"
#include "SpectrometerParameters.hh"
#include "SpectrometerDigitizer.hh"
#include "SpectrometerGeometry.hh"
#include "SpectrometerReconstruction.hh"
#include "SRBRawDecoder.hh"
#include "NA62RecoManager.hh"

/// \class SpectrometerDigitizer 
/// \Brief
/// Steering class for Straw Chamber digitization. 
/// \EndBrief
/// 
/// \Detailed
/// This class performs the digitization of the straw chamber spectrometer. Steps:
/// - Initizalization: SpectrometerDigitizer::SpectrometerDigitizer.
/// - Digitization: SpectrometerDigitizer::ProcessEvent. 
/// 
/// PARAMETRIZEDDIGI allows the switching between the full and parametric digitization.
/// \EndDetailed

#define DBGTHIS "SpecDigi"
#include "Debug.h" 

SpectrometerDigitizer::SpectrometerDigitizer(NA62VReconstruction* Reco) : NA62VDigitizer(Reco, "Spectrometer"){
  /// \MemberDescr
  /// \param Reco  Pointer to NA62VReconstruction.
  ///
  /// The initialization is performed in the constructor of the StrawResponse class.
  /// \EndMemberDescr

  fStationsMCToF = fReco->GetStationsMCToF();
  fChambersMCToF = SpectrometerParameters::GetInstance().GetChambersMCToF();
  fViewsMCToF    = SpectrometerParameters::GetInstance().GetViewsMCToF();
  fPlanesMCToF   = SpectrometerParameters::GetInstance().GetPlanesMCToF();
  fDigiEvent = new SRBEvent(TSpectrometerDigi::Class());

#ifdef PARAMETRIZEDDIGI
  Response = new StrawResponse(fRandom);     // Initialize straw response simulation
#else

  fMaxTime =               SpectrometerParameters::GetInstance().GetMaxTime();
  fTimeStep =              SpectrometerParameters::GetInstance().GetTimeStep();
  fStrawT0 =               SpectrometerParameters::GetInstance().GetStrawT0();
  fNClustersPermm =        SpectrometerParameters::GetInstance().GetNClustersPermm();
  fDiscrSetTime =          SpectrometerParameters::GetInstance().GetDiscrSetTime();
  fEdgeDeadTime =          SpectrometerParameters::GetInstance().GetEdgeDeadTime();
  fSameEdgeDeadTime =      SpectrometerParameters::GetInstance().GetSameEdgeDeadTime();
  fCARIOCASlope =          SpectrometerParameters::GetInstance().GetCARIOCASlope();
  fThreshold = -           SpectrometerParameters::GetInstance().GetThreshold()*fCARIOCASlope;
  fGain =                  SpectrometerParameters::GetInstance().GetGain();
  fEquivalentNoiseCharge = SpectrometerParameters::GetInstance().GetEquivalentNoiseCharge();
  fIonizationEnergy =      SpectrometerParameters::GetInstance().GetIonizationEnergy()*eV;
  fNTotalPermm =           SpectrometerParameters::GetInstance().GetNTotalPermm();
  fNoiseSimu =             SpectrometerParameters::GetInstance().GetNoiseSimu();
  fSavePulseShapes =       SpectrometerParameters::GetInstance().GetSavePulseShapes();
  fNoBulkHits =            SpectrometerParameters::GetInstance().GetNoBulkHits();

  fSingleCluster = new Float_t[(Int_t)(2.*fMaxTime/fTimeStep)];
  Float_t Max = 0;
  for(Int_t iTime = 0; iTime < 0*fMaxTime/fTimeStep; iTime++){ //From theory
    Float_t Time = iTime * fTimeStep;
    fSingleCluster[iTime]= 0;
    if(Time < 6.8)
      continue;
    for(Float_t tConv = 0; tConv <= Time - 6.8; tConv += fTimeStep)
      fSingleCluster[iTime] -= CariocaTransferFunction(0.7*tConv) * StrawSignal(Time - 6.8 - tConv, fStrawT0);
    Max = (TMath::Abs(fSingleCluster[iTime]) > Max ? TMath::Abs(fSingleCluster[iTime]) : Max);
  }
  for(Int_t iTime = 0; iTime < fMaxTime/fTimeStep; iTime++){ //From measurements
    Float_t Time = iTime * fTimeStep;
    fSingleCluster[iTime] = -1.02317*TMath::Gaus(Time,23.89,6.61773) + 0.196639*TMath::Landau(Time,113.015,49.919) - 2.13952*TMath::Landau(Time,37.4526,4.60676); //Hybrid Left=Fe55 Right=CARIOCA with 0.7*t
    //fSingleCluster[iTime] = -0.144291*TMath::Gaus(Time,21.6545,5.60022) + 0.121935*TMath::Landau(Time,88.9075,25.4615) - 0.160931*TMath::Gaus(Time,30.5575,8.52979); //Fe55
    //fSingleCluster[iTime] = (Time > 12 && Time < 40. ? -1. : 0.); 
    Max = (TMath::Abs(fSingleCluster[iTime]) > Max ? TMath::Abs(fSingleCluster[iTime]) : Max);
  }
  for(Int_t iTime = 0; iTime < fMaxTime/fTimeStep; iTime++)
    fSingleCluster[iTime] /= Max;
  fSignalPre = new Float_t[(Int_t)(2*fMaxTime/fTimeStep)];
  fSignalPost = new Float_t[(Int_t)(2*fMaxTime/fTimeStep)];
  fSignal = fSignalPre;
  fNoise = 0;

#endif
}

SpectrometerDigitizer::~SpectrometerDigitizer()
{
#ifndef PARAMETRIZEDDIGI
  if(fSingleCluster) {
    delete [] fSingleCluster;
    fSingleCluster = 0;
  }
  if(fNoise){
    delete fNoise;
    fNoise = 0;
  }
  if(fSignalPre){
    delete fSignalPre;
    fSignalPre = 0;
  }
  if(fSignalPost){
    delete fSignalPost;
    fSignalPost = 0;
  }
#else
  if(Response) {
    delete Response;
    Response = 0;
  }
#endif
}

TDetectorVEvent * SpectrometerDigitizer::ProcessEvent(TDetectorVEvent * tEvent)
{
  /// \MemberDescr
  /// \param tEvent  Pointer to TDetectorVEvent.
  ///
  /// \EndMemberDescr

  if (tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") ||
      tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent;

#ifndef PARAMETRIZEDDIGI

  debug_cout(1, "SpectrometerDigitizer::ProcessEvent(tEvent = " << tEvent << "): NHits = " << tEvent->GetNHits());

  if(tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi")) return tEvent; 
  TSpectrometerEvent * SpectrometerEvent = static_cast<TSpectrometerEvent*>(tEvent);
  *((TVEvent*)fDigiEvent) = *((TVEvent*)SpectrometerEvent); 
  debug_cout(1, "SpectrometerDigitizer::ProcessEvent: Event ID= " << SpectrometerEvent->GetID() << " -> " << fDigiEvent->GetID());
  debug_cout(1, "SpectrometerDigitizer::ProcessEvent: Event Time= " << SpectrometerEvent->GetTime() << " -> " << fDigiEvent->GetTime());
  fDigiEvent->Clear();
  if(tEvent->GetNHits() == 0)
    return fDigiEvent;

  // Generation and propagation of ionization clusters from MC energy deposits
  for(Int_t iHit = 0; iHit < SpectrometerEvent->GetNHits(); iHit++){
    TSpectrometerHit *Hit = static_cast<TSpectrometerHit*>(SpectrometerEvent->Hit(iHit));

    if(Hit->GetChannelID() == -1){ //WORKAROUND for compatibility with previous MC files 
      Hit->SetPlaneID(Hit->GetStrawID()/1000);
      Hit->SetStrawID(Hit->GetStrawID()%1000);
      Hit->EncodeChannelID();
    }
    Hit->SetStrawID(Hit->GetStrawID() + 1000*Hit->GetPlaneID());//WORKAROUND for compatibility with Reco

    debug_cout(1,"SpectrometerDigitizer::ProcessEvent: iHit = " << iHit  << " ChannelID = " << Hit->GetChannelID() << " ChamberID = " << Hit->GetChamberID() << " ViewID = " << Hit->GetViewID() << " HalfViewID = " << Hit->GetHalfViewID() << " PlaneID = " << Hit->GetPlaneID() << " StrawID = " << Hit->GetStrawID());
    AddIonizationClusters(Hit);
  }

  // Analogue signal generation from ionization clusters; noise and discriminator simulation
  fDigiEvent->GetHits()->Sort();
  Int_t CurrentChannelID = -1;
  Float_t CurrentStart = 0;
  Float_t Delay[1000];
  Int_t nAddedClusters = 0;
  for(Int_t iDigi = 0; iDigi <= fDigiEvent->GetNHits(); iDigi++){
    TSpectrometerDigi *Digi = 0;
    Float_t NSecondaries = 0; 
    if(iDigi < fDigiEvent->GetNHits()){
      Digi = static_cast<TSpectrometerDigi*>(fDigiEvent->GetHits()->At(iDigi));
      if(Digi->GetDetectedEdge() != 0) continue;
      NSecondaries = (Int_t)(Digi->GetTrailingEdge() - Digi->GetLeadingEdge());
      static_cast<SpectrometerReconstruction*>(fReco)->GetHNIonPairs()->Fill(NSecondaries);
    }

    if(iDigi == fDigiEvent->GetNHits() || CurrentChannelID != Digi->GetChannelID()){
      if(CurrentChannelID != -1){
        if(fNoiseSimu){
          AddNoise(fCARIOCASlope*fEquivalentNoiseCharge, CurrentStart, CurrentChannelID);
          fSignal = fSignalPost;
        }
        Int_t NAddedDigis = Discriminate(iDigi, CurrentStart, Delay[nAddedClusters -1]);
        nAddedClusters = 0;
        if(iDigi == fDigiEvent->GetNHits() - NAddedDigis) break;
      }
      CurrentChannelID = Digi->GetChannelID();
      CurrentStart = Digi->GetLeadingEdge();
      TSpectrometerHit* MCHit = static_cast<TSpectrometerHit*>(Digi->GetMCHit());
      debug_cout(1, "         SpectrometerDigitizer::ProcessEvent: CurrentStart = " << CurrentStart << " iDigi/NDigis = " << iDigi << "/" << fDigiEvent->GetNHits() << " ChannelID = " << Digi->GetChannelID() << " -> [" << Digi->GetLeadingEdge() <<  " , " << Digi->GetTrailingEdge() << "]" << Digi->GetDetectedEdge() << " from MC Time = " << MCHit->GetTime() << " Energy = " << MCHit->GetEnergy()/eV << " -> MeanSecondaries = " << MCHit->GetEnergy()/fIonizationEnergy << "(" << NSecondaries << ")" << " secondaries");
      Delay[nAddedClusters++] = 0;
      for(Int_t iTime = 0; iTime < 2*fMaxTime/fTimeStep - 1; iTime++)
        fSignal[iTime] = (iTime < fMaxTime/fTimeStep ? -NSecondaries*electron_charge*fGain*fSingleCluster[iTime]/(coulomb*1e-15)*fCARIOCASlope : 0);
    }else{
      debug_cout(1,"                SpectrometerDigitizer::ProcessEvent: Summing cluster " << iDigi << " on channel " << CurrentChannelID);
      Delay[nAddedClusters] = Digi->GetLeadingEdge() - CurrentStart;
      for(Int_t iTime = 0; iTime < fMaxTime/fTimeStep; iTime++){
        Int_t TimeBin = (Int_t)(Delay[nAddedClusters]/fTimeStep) + iTime;
        if(TimeBin < 0)
          std::cout << "SpectrometerDigitizer::ProcessEvent: WARNING!! TimeBin = " << TimeBin << " ... skipping " << std::endl <<
            CurrentStart << " " << Digi->GetLeadingEdge() << " " << iTime << std::endl;
        else if(TimeBin < 2*fMaxTime/fTimeStep - 1)
          fSignal[TimeBin] += -NSecondaries*electron_charge*fGain*fSingleCluster[iTime]/(coulomb*1e-15)*fCARIOCASlope;
      }
      nAddedClusters++;
      fDigiEvent->RemoveHit(iDigi);
      iDigi--;
    }
  }

  for(Int_t iDigi=0;iDigi<fDigiEvent->GetNHits();iDigi++){
    TSpectrometerDigi *Digi = static_cast<TSpectrometerDigi*>(fDigiEvent->GetHits()->At(iDigi));
    debug_cout(1,"                SpectrometerDigitizer::ProcessEvent: iDigi/NDigis = " << iDigi << "/" << fDigiEvent->GetNHits() << " ChannelID = " << Digi->GetChannelID() << " -> [" << Digi->GetLeadingEdge() <<  " , " << Digi->GetTrailingEdge() << "]" << Digi->GetDetectedEdge() << " from MCTime = (" << static_cast<TSpectrometerHit*>(Digi->GetMCHit())->GetTime() << ")");
  }

#else // Parametric digitization

  // init
  if(tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi")) return tEvent; 
  TSpectrometerEvent * SpectrometerEvent = static_cast<TSpectrometerEvent*>(tEvent);
  fDigiEvent->Clear();
  *(static_cast<TVEvent*>(fDigiEvent)) = *(static_cast<TVEvent*>(SpectrometerEvent));

  // Loop on MC hits
  Int_t nDigi = 0;
  for (Int_t jHit=0;jHit<SpectrometerEvent->GetNHits();jHit++){
    TSpectrometerHit *hit = static_cast<TSpectrometerHit*>(SpectrometerEvent->GetHits()->At(jHit));

    // Straw Inefficiency simulation
    if (Response->StrawInefficient(hit->GetWireDistance())) continue;

    //WORKAROUND for compatibility with previous MC files and Reco
    if(hit->GetChannelID() == -1)
    {  
      hit->SetPlaneID(hit->GetStrawID()/1000);
      hit->SetStrawID(hit->GetStrawID()%1000);
      hit->EncodeChannelID();
    }
    hit->SetStrawID(hit->GetStrawID() + 1000*hit->GetPlaneID());

    // Leading and Trailing Time simulation (in microsecond)
    Double_t leadingtime = Response->TimeSimulated(hit->GetWireDistance());
    Double_t trailingtime;
    while (1) 
    {
      trailingtime = 0.15+fRandom->Gaus(0.,0.03);
      if (trailingtime>leadingtime+0.005) break;
    }

    // Copy everything in digi (allowed variables only)
    Double_t atime = hit->GetTime();
    Int_t chamberID = hit->GetChamberID();
    Int_t viewID = hit->GetViewID();
    Int_t planeID = 2*hit->GetHalfViewID()+hit->GetPlaneID();
    Double_t MagicT0 = SpectrometerParameters::GetInstance().GetMagicT0();
    Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
    Double_t tmcoffset = fStationsMCToF[hit->GetStationID()] + fChambersMCToF[chamberID]+fViewsMCToF[viewID]+fPlanesMCToF[planeID];

    // Add Leading + Trailing digi
    TSpectrometerDigi *digi = static_cast<TSpectrometerDigi*>(fDigiEvent->AddDigi(hit));
    digi->SetLeadingEdge(leadingtime*1000+atime+FineTime-tmcoffset+fReco->GetT0Correction(digi)-MagicT0); // in ns, mc offset subtracted; overcorrect for T0s
    digi->UpdateDetectedEdge(SRB_HIT_EDGE_LEADING);
    digi->SetTrailingEdge(trailingtime*1000+atime+FineTime-tmcoffset+fReco->GetT0Correction(digi)-MagicT0);  // in ns, mc offset subtracted; overcorrect for T0s
    digi->UpdateDetectedEdge(SRB_HIT_EDGE_TRAILING);
    digi->SetChamberID(chamberID);
    digi->SetViewID(viewID);
    digi->SetHalfViewID(hit->GetHalfViewID());
    digi->SetStrawID(hit->GetStrawID());
    digi->SetHitID(nDigi);
    digi->SetMultiHit(0);
    nDigi++;
  }

#endif

  return fDigiEvent;
}

#ifndef PARAMETRIZEDDIGI

void SpectrometerDigitizer::AddIonizationClusters(TSpectrometerHit* Hit){
  TVector3 LocalPosition = Hit->GetLocalPosition();
  if(Hit->GetViewID() < 2)
    LocalPosition.RotateZ(TMath::Pi()/4.*(1 - 2*Hit->GetViewID()));
  else if(Hit->GetViewID() == 2)
    LocalPosition.RotateZ(TMath::Pi()/2.);
  Double_t DistanceFromWire = TMath::Sqrt(LocalPosition.Z()*LocalPosition.Z() + LocalPosition.Y()*LocalPosition.Y());
  Double_t LocalTime =  Hit->GetTime() - (fStationsMCToF[Hit->GetStationID()] + fChambersMCToF[Hit->GetChamberID()]);
  debug2_cout(1,"        SpectrometerDigitizer::AddIonizationClusters: DistanceFromWire = " << DistanceFromWire << " ");

  if(DistanceFromWire < 4.874){
    debug2_cout(1,"Energy = " << Hit->GetEnergy() << " LocalTime = " << LocalTime << " Bulk" << std::endl);
    if(fNoBulkHits)
      return;
    Float_t DriftDistance = DistanceFromWire; 
    DriftDistance += fRandom->Gaus(0,Diffusion(DriftDistance));
    TSpectrometerDigi *Digi = static_cast<TSpectrometerDigi*>(fDigiEvent->AddDigi(Hit)); // add one hit
    *((SpectrometerChannelID*)Digi) = *((SpectrometerChannelID*)Hit);
    Digi->SetLeadingEdge(LocalTime + (SpectrometerGeometry::GetInstance()->GetViewSize()*0.5 - LocalPosition.X())/c_light + 
        + DriftTime(DriftDistance));
    Digi->SetTrailingEdge(Digi->GetLeadingEdge() + (Int_t)(Hit->GetEnergy()/fIonizationEnergy) + 0.1); //Encoding number of ion pairs
    debug_cout(1, "         SpectrometerDigitizer::AddIonizationClusters: Added Leading = " << Digi->GetLeadingEdge() << " on Channel " <<  Digi->GetChannelID() << "  with " << Digi->GetTrailingEdge() - Digi->GetLeadingEdge() << " ion pairs");
  }else{
    debug_cout(1,"WireDistance = " << Hit->GetWireDistance() << " Cord = " << Hit->GetDirection().Mag() 
        << " Bo = " << TMath::Sqrt(Hit->GetWireDistance()*Hit->GetWireDistance() 
          + (Hit->GetDirection().Z()*Hit->GetDirection().Z() + Hit->GetDirection().Y()*Hit->GetDirection().Y())*0.25) << " Energy = " << Hit->GetEnergy() << " LocalTime = " << LocalTime);
    Float_t Step = 0;
    Int_t nClusters = 0;
    while(Step < Hit->GetDirection().Mag()){
      Float_t ClusterDistance = fRandom->Exp(1/fNClustersPermm);
      Step += ClusterDistance;
      debug_cout(1, "         SpectrometerDigitizer::AddIonizationClusters: ClusterDistance = " << ClusterDistance << " TotalStep/Cord " <<  Step << "/" << Hit->GetDirection().Mag());
      if(Step < Hit->GetDirection().Mag()){
        Float_t DriftDistance = TMath::Sqrt(
            Hit->GetWireDistance()*Hit->GetWireDistance()
            + (Step - Hit->GetDirection().Mag()*0.5) * (Step - Hit->GetDirection().Mag()*0.5)
            );
        DriftDistance += fRandom->Gaus(0,Diffusion(DriftDistance));
        TSpectrometerDigi *Digi = static_cast<TSpectrometerDigi*>(fDigiEvent->AddDigi(Hit)); // add one hit
        *((SpectrometerChannelID*)Digi) = *((SpectrometerChannelID*)Hit);
        Digi->SetLeadingEdge(LocalTime + (SpectrometerGeometry::GetInstance()->GetViewSize()*0.5 - LocalPosition.X())/c_light 
            + DriftTime(DriftDistance));
        static_cast<SpectrometerReconstruction*>(fReco)->GetHStep()->Fill(ClusterDistance);
        Digi->SetTrailingEdge(Digi->GetLeadingEdge() + fRandom->Poisson(fNTotalPermm/fNClustersPermm) + 0.1); //Encoding number of ion pairs
        //debug_cout(1,Hit->GetTime() << " " << Step << " " << TMath::Sqrt(Hit->GetWireDistance()*Hit->GetWireDistance()+ (Step - Hit->GetDirection().Mag()*0.5) * (Step - Hit->GetDirection().Mag()*0.5)));
        debug_cout(1, "         SpectrometerDigitizer::AddIonizationClusters: Added Leading = " << Digi->GetLeadingEdge() << " on Channel " <<  Digi->GetChannelID() << "  with " << Digi->GetTrailingEdge() - Digi->GetLeadingEdge() << " ion pairs");
        nClusters++;
      }
    }
    Hit->SetEnergy(Hit->GetEnergy()/nClusters);
  }
}

Int_t SpectrometerDigitizer::Discriminate(Int_t iDigi, Float_t CurrentStart, Float_t MaxClusterDelay){
  TSpectrometerDigi *OldDigi = static_cast<TSpectrometerDigi*>(fDigiEvent->GetHits()->At(iDigi - 1));
  if(OldDigi->GetDetectedEdge() != 0) return 0;
  Int_t CurrentChannelID = OldDigi->GetChannelID();
  Int_t nAddedDigis = 0;
  Float FirstLeading = -200., LastTrailing = -300., FirstLeadingPre = -200.;
  Float_t Max = 0;
  //mod//    Int_t ROChannelID = static_cast<TDCBRawDecoder*>((static_cast<SpectrometerReconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(CurrentChannelID));
  Int_t ROChannelID = static_cast<SRBRawDecoder*>(static_cast<SpectrometerReconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(CurrentChannelID);
  Int_t T0 =  fReco->GetT0Correction(OldDigi);

  //TCanvas * PulseShapeC = 0;
  //TGraph * PulseShape = 0;
  //if(fSavePulseShapes){
  //    gStyle->SetCanvasBorderMode(0);
  //    gStyle->SetFrameBorderMode(0);
  //    gStyle->SetTitleFillColor(kWhite);
  //    gStyle->SetFrameFillColor(kWhite);
  //    gStyle->SetStatColor(kWhite);
  //    gStyle->SetCanvasColor(kWhite);
  //    gStyle->SetPadColor(kWhite);
  //    PulseShapeC = new TCanvas();
  //    PulseShapeC->SetName(Form("Pulse_%05d_%02d",fDigiEvent->GetID(), CurrentChannelID));
  //    PulseShape = new TGraph();
  //    PulseShape->SetName(Form("Pulse_%05d_%02d",fDigiEvent->GetID(), CurrentChannelID));
  //}

  for(Int_t iTime = 0; iTime < 2*fMaxTime/fTimeStep - 1; iTime++){
    debug_cout(2,"         SpectrometerDigitizer::Discriminate: Signal: iTime = " << iTime << " CurrentChannelID = " << CurrentChannelID << " CurrentStart = " << CurrentStart << " Signal[iTime] =" << fSignal[iTime]);
    Max = (TMath::Abs(fSignal[iTime]) > Max ? TMath::Abs(fSignal[iTime]) : Max);

    //if(fSavePulseShapes)
    //    PulseShape->SetPoint(iTime, CurrentStart + iTime*fTimeStep, fSignal[iTime]);

    //if(fSignalPre[iTime + 1] < fThreshold && fSignalPre[iTime] >= fThreshold && nAddedDigis == 0)
    //    FirstLeadingPre = CurrentStart + (iTime + 0.5)*fTimeStep;
    if(fSignal[iTime + 1] < fThreshold && fSignal[iTime] >= fThreshold){
      Float_t Slope = (fSignal[iTime + 1] - fSignal[iTime])/fTimeStep;
      Float_t NoiseTimeSmear = (!fNoiseSimu)*fRandom->Exp(fCARIOCASlope*fEquivalentNoiseCharge) * (fRandom->Uniform() > 0.5 ? 1 : -1);
      NoiseTimeSmear /=  Slope < -1. ? Slope : -1.;
      debug_cout(1,"                SpectrometerDigitizer::Discriminate: Leading: " << CurrentStart + (iTime + 0.5)*fTimeStep << " + " << NoiseTimeSmear);
      Float_t LeadingEdge = CurrentStart + (iTime + 0.5)*fTimeStep + NoiseTimeSmear - ClockPeriod; //~ -25ns to shift for correct T0
      if(nAddedDigis == 0)
        FirstLeading = LeadingEdge;
      if(OldDigi->GetDetectedEdge() & SRB_HIT_EDGE_TRAILING && LeadingEdge - OldDigi->GetTrailingEdge() < fEdgeDeadTime)
        continue;
      if(OldDigi->GetDetectedEdge() & SRB_HIT_EDGE_LEADING){
        if(LeadingEdge - OldDigi->GetLeadingEdge() < fSameEdgeDeadTime)
          continue;
        TSpectrometerDigi* NewDigi = static_cast<TSpectrometerDigi*>(fDigiEvent->AddDigi());
        *NewDigi = *OldDigi;
        OldDigi = NewDigi;
        OldDigi->SetDetectedEdge(0);
        OldDigi->SetTrailingEdge(-300.);
        nAddedDigis++;
        debug_cout(1,"                SpectrometerDigitizer::Discriminate: New Digi(Leading)");
      }
      OldDigi->SetLeadingEdge(LeadingEdge + T0);
      OldDigi->UpdateDetectedEdge(SRB_HIT_EDGE_LEADING);
    }else if(fSignal[iTime + 1] > fThreshold && fSignal[iTime] <= fThreshold){
      Bool_t DiscrSet = kTRUE;
      for(Int_t iSettlingTime = 0; iSettlingTime < fDiscrSetTime/fTimeStep; iSettlingTime++){

        //if(fSavePulseShapes)
        //    PulseShape->SetPoint(iTime + iSettlingTime + 1, CurrentStart + (iTime  + iSettlingTime + 1)*fTimeStep, fSignal[iTime + iSettlingTime + 1]);

        if(fSignal[iTime + iSettlingTime + 1] < fThreshold){
          debug_cout(1,"                SpectrometerDigitizer::Discriminate: Neglecting Trailing: " << CurrentStart + (iTime + 0.5)*fTimeStep);
          iTime += iSettlingTime;
          debug_cout(1,"                SpectrometerDigitizer::Discriminate: Skipping to: " << CurrentStart + (iTime + 0.5)*fTimeStep << " iTime = " << iTime);
          DiscrSet = kFALSE;
        }
      }
      if(!DiscrSet)
        continue;
      Float_t Slope = (fSignal[iTime + 1] - fSignal[iTime])/fTimeStep;
      Float_t NoiseTimeSmear = (!fNoiseSimu)*fRandom->Exp(fCARIOCASlope*fEquivalentNoiseCharge) * (fRandom->Uniform() > 0.5 ? 1 : -1);
      NoiseTimeSmear /=  Slope > 1. ? Slope : 1.;
      debug_cout(1,"                SpectrometerDigitizer::Discriminate: Trailing: " << CurrentStart + (iTime + 0.5)*fTimeStep << " + " << NoiseTimeSmear);
      Float_t TrailingEdge = CurrentStart + (iTime + 0.5)*fTimeStep + NoiseTimeSmear - ClockPeriod; //-25ns to shift for correct T0
      LastTrailing = TrailingEdge;
      if(OldDigi->GetDetectedEdge() & SRB_HIT_EDGE_LEADING && TrailingEdge - OldDigi->GetLeadingEdge() < fEdgeDeadTime)
        continue;
      if(OldDigi->GetDetectedEdge() & SRB_HIT_EDGE_TRAILING){
        if(TrailingEdge - OldDigi->GetTrailingEdge() < fSameEdgeDeadTime)
          continue;
        TSpectrometerDigi* NewDigi = static_cast<TSpectrometerDigi*>(fDigiEvent->AddDigi());
        *NewDigi = *OldDigi;
        OldDigi = NewDigi;
        OldDigi->SetDetectedEdge(0);
        OldDigi->SetLeadingEdge(-300.);
        nAddedDigis++;
        debug_cout(1,"                SpectrometerDigitizer::Discriminate: New Digi(Trailing)");
      }
      OldDigi->SetTrailingEdge(TrailingEdge + T0);
      OldDigi->UpdateDetectedEdge(SRB_HIT_EDGE_TRAILING);
    }
  }

  //if(fSavePulseShapes){
  //    PulseShapeC->cd();
  //    PulseShape->Draw("ALP");
  //    PulseShape->GetXaxis()->SetTitle("ns");
  //    PulseShape->GetYaxis()->SetTitle("mV");
  //    TText * t = new TText(CurrentStart, 10, Form("Wire distance = %3.2f mm %d clusters - %d digis", static_cast<TSpectrometerHit*>(OldDigi->GetMCHit())->GetWireDistance(), nAddedClusters, nAddedDigis + 1));
  //    t->Draw();
  //    for(Int_t iClus = 0; iClus < nAddedClusters; iClus++){
  //        TLine * l = new TLine(CurrentStart + Delay[iClus], 0, CurrentStart + Delay[iClus], -10);
  //        l->Draw();
  //    }
  //    TLine * l;
  //    l = new TLine(CurrentStart, fThreshold, CurrentStart + fMaxTime, fThreshold);
  //    l->SetLineColor(kGreen);
  //    l->Draw();
  //    if(OldDigi->GetDetectedEdge() & SRB_HIT_EDGE_LEADING){
  //        l = new TLine(OldDigi->GetLeadingEdge(), 0, OldDigi->GetLeadingEdge(), 10);
  //        l->SetLineColor(kRed);
  //        l->Draw();
  //    }
  //    if(OldDigi->GetDetectedEdge() & SRB_HIT_EDGE_TRAILING){
  //        l = new TLine(OldDigi->GetTrailingEdge(), 0, OldDigi->GetTrailingEdge(), 10);
  //        l->SetLineColor(kBlue);
  //        l->Draw();
  //    }
  //    PulseShapeC->Write();
  //}

  fSignal = fSignalPre;
  static_cast<SpectrometerReconstruction*>(fReco)->GetHMaxClusterDelay()->Fill(MaxClusterDelay, LastTrailing - FirstLeading);
  return nAddedDigis; 
}

void SpectrometerDigitizer::AddNoise(Float_t Noise, Float_t CurrentStart, Int_t CurrentChannelID){
  if(Noise <= 1)
    return;
  if(fNoise){
    Int_t NoiseOffset = fRandom->Uniform(0,(3000 - 2*fMaxTime)/fTimeStep - 1);
    for(Int_t iTime = 0; iTime < 2*fMaxTime/fTimeStep - 1; iTime++)
      fSignalPost[iTime] = fSignal[iTime] + fNoise[iTime + NoiseOffset];
  }else{
    const Int_t NBins = 10000;
    Int_t Nbins = NBins;
    TGraph * gSig = new TGraph();
    for(Int_t iTime = 0; iTime < 2*fMaxTime/fTimeStep - 1; iTime++)
      gSig->SetPoint(iTime, CurrentStart + iTime*fTimeStep, fSignal[iTime]);
    Double_t SigT0 = gSig->GetX()[0];
    Double_t SigBinWidth = (gSig->GetX()[1] - SigT0);
    //Double_t * Sig = gSig->GetY();
    Double_t Time[NBins], Signal[NBins];
    Double_t SignalBinWidth = 1.;
    Noise /= 2.*SignalBinWidth;//Empirical normalization
    Double_t SignalT0 = SigT0 - (SignalBinWidth*NBins - SigBinWidth*gSig->GetN())/2.;
    Int_t Modulus;
    Int_t FirstModulus = (Int_t)((SignalT0 - SigT0)/(SigBinWidth*gSig->GetN()));
    Int_t LastModulus = (Int_t)((SignalT0 + SignalBinWidth*NBins - SigT0)/(SigBinWidth*gSig->GetN()) - 1.);
    for(Int_t i = 0; i < NBins; i++){
      Time [i] = SignalT0 + SignalBinWidth*i; 
      //Signal[i] = (Time [i] < SigT0 || Time [i] > SigT0 + SigBinWidth*gSig->GetN() ? 0 : gSig->Eval(Time[i]));
      Modulus = (Int_t)((Time[i] - SigT0 < 0. ? -1. : 0.) + (Time[i] - SigT0)/(SigBinWidth*gSig->GetN()));
      if(Modulus%4 == 0 && Modulus >= FirstModulus && Modulus <= LastModulus)
        Signal[i] = gSig->Eval(Time[i] - SigBinWidth*gSig->GetN()*Modulus);
      else
        Signal[i] = 0;
    }
    delete gSig;

    TVirtualFFT* SigFFT = TVirtualFFT::FFT(1, &Nbins, "R2C ES");
    SigFFT->SetPoints(Signal);
    SigFFT->Transform();
    Double_t SignalRe[NBins];
    Double_t SignalIm[NBins];
    Double_t NoiseRe[NBins];
    Double_t NoiseIm[NBins];

    SigFFT->GetPointsComplex(SignalRe, SignalIm);
    Double_t ProdRe[NBins];
    Double_t ProdIm[NBins];
    Double_t omega[NBins];
    Double_t Prod[NBins];

    Double_t SignalPower[NBins];
    Double_t NoisePower[NBins];
    Double_t ProdPower[NBins];

    Float_t R2CFFTNorm = 10.;
    Float_t C2RFFTNorm = NBins/R2CFFTNorm;
    Float_t NoisePhase = TMath::Tan(fRandom->Uniform(-TMath::PiOver2(),TMath::PiOver2()));
    for(Int_t i = 0; i < NBins; i++){
      omega[i] = i/(SignalBinWidth*1e-9)/NBins;
      NoiseRe[i] = Noise;
      //NoiseRe[i] = omega[i] < 30e6 ? Noise : 0;
      NoiseIm[i] = NoisePhase*NoiseRe[i];
      NoiseRe[i] -= Noise*TMath::Sign((Float_t)1.,NoisePhase)*50.*TMath::Gaus(omega[i],6.0e7,2e5);
      NoiseIm[i] -= Noise*TMath::Sign((Float_t)1.,NoisePhase)*50.*TMath::Gaus(omega[i],6.0e7,2e5);
      NoiseRe[i] *= (0.7*TMath::Gaus(omega[i],2.1e7,4e6) + 1.9*TMath::Landau(omega[i],1.7e7,6.3e6))*10.7*5.;
      NoiseIm[i] *= (0.7*TMath::Gaus(omega[i],2.1e7,4e6) + 1.9*TMath::Landau(omega[i],1.7e7,6.3e6))*10.7*5.;
      NoiseRe[i] /= C2RFFTNorm*TMath::Sqrt(1 + NoisePhase*NoisePhase);
      NoiseIm[i] /= C2RFFTNorm*TMath::Sqrt(1 + NoisePhase*NoisePhase);
      SignalRe[i] /= R2CFFTNorm*C2RFFTNorm;
      SignalIm[i] /= R2CFFTNorm*C2RFFTNorm;
      SignalPower[i] = TMath::Sqrt(SignalRe[i]*SignalRe[i] + SignalIm[i]*SignalIm[i]);
      NoisePower[i] = TMath::Sqrt(NoiseRe[i]*NoiseRe[i] + NoiseIm[i]*NoiseIm[i]);
      //        ProdRe[i] = (NoiseRe[i]*SignalRe[i]-NoiseIm[i]*SignalIm[i])/NBins;
      //        ProdIm[i] = (NoiseRe[i]*SignalIm[i]+NoiseIm[i]*SignalRe[i])/NBins; 
      ProdRe[i] = TMath::Sign(1.,SignalRe[i])*TMath::Sqrt(NoiseRe[i]*NoiseRe[i] + SignalRe[i]*SignalRe[i]);
      ProdIm[i] = TMath::Sign(1.,SignalIm[i])*TMath::Sqrt(NoiseIm[i]*NoiseIm[i] + SignalIm[i]*SignalIm[i]);
      //ProdIm[i] = SignalIm[i]; 
      ProdPower[i] = TMath::Sqrt(ProdRe[i]*ProdRe[i] + ProdIm[i]*ProdIm[i]);
    }
    TVirtualFFT* AntiFFT = TVirtualFFT::FFT(1, &Nbins, "C2R ES");
    AntiFFT->SetPointsComplex(ProdRe, ProdIm);
    AntiFFT->Transform();
    AntiFFT->GetPoints(Prod);
    TGraph * gProd = new TGraph(NBins, Time, Prod);
    TGraph * gSignal = new TGraph(NBins, Time, Signal);
    for(Int_t iTime = 0; iTime < 2*fMaxTime/fTimeStep - 1; iTime++)
      fSignalPost[iTime] = gProd->Eval(CurrentStart + iTime*fTimeStep);

    fNoise = new Float_t[(Int_t)(3000/fTimeStep)];
    for(Int_t iTime = 0; iTime < 3000/fTimeStep; iTime++){
      fNoise[iTime] = gProd->Eval(CurrentStart + iTime*fTimeStep - 1500.) - gSignal->Eval(CurrentStart + iTime*fTimeStep - 1500.);
    }
    if(fSavePulseShapes){
      TCanvas * c = new TCanvas();
      c->SetName(Form("FFT_%05d_%02d",fDigiEvent->GetID(), CurrentChannelID));
      TGraph * gSignalPower = new TGraph(NBins, omega, SignalPower);
      TGraph * gNoisePower = new TGraph(NBins, omega, NoisePower);
      TGraph * gRe = new TGraph(NBins, omega, ProdRe);
      TGraph * gIm = new TGraph(NBins, omega, ProdIm);
      TGraph * gPower = new TGraph(NBins, omega, ProdPower);
      c->Divide(3,2);
      c->cd(1);
      gSignal->Draw("AL");
      gSignal->SetTitle("Signal");
      gSignal->GetXaxis()->SetRangeUser(CurrentStart - 100., CurrentStart + 2*fMaxTime);
      //cout << "Signal: " << gSignal->Integral() << std::endl;
      c->cd(2);
      gRe->Draw("AL");
      gRe->SetTitle("Signal + Noise FFT Re");
      gRe->GetXaxis()->SetRangeUser(1e5,1e9);
      gPad->SetLogx();
      c->cd(3);
      gIm->Draw("AL");
      gIm->GetXaxis()->SetRangeUser(1e5,1e9);
      gIm->SetTitle("Signal + Noise FFT Im");
      gPad->SetLogx();
      c->cd(4);
      gNoisePower->Draw("AL");
      gNoisePower->GetXaxis()->SetRangeUser(1e5,1e9);
      gNoisePower->GetYaxis()->SetRangeUser(1e-3,2e2);
      gNoisePower->SetTitle("Signal and Noise Power Spectrum");
      gNoisePower->SetLineColor(kRed);
      //cout << "Noise Power Spectrum: " << gNoisePower->Integral() << std::endl;
      gSignalPower->Draw("L");
      gSignalPower->SetTitle("Signal and Noise Power Spectrum");
      //cout << "Signal Power Spectrum: " << gSignalPower->Integral() << std::endl;
      gPad->SetLogx();
      gPad->SetLogy();
      c->cd(5);
      gPower->Draw("AL");
      gPower->GetXaxis()->SetRangeUser(1e5,1e9);
      gPower->GetYaxis()->SetRangeUser(1e-3,2e2);
      gPower->SetTitle("Signal + Noise Power Spectrum");
      //cout << "Signal + Noise Power Spectrum: " << gPower->Integral() << std::endl;
      gPad->SetLogx();
      gPad->SetLogy();
      c->cd(6);
      gProd->Draw("AL");
      gProd->SetTitle("Signal + Noise");
      gProd->GetXaxis()->SetRangeUser(CurrentStart - 100., CurrentStart + 2*fMaxTime);
      //cout << "Signal + Noise: " << gProd->Integral() << std::endl;
      c->Write();
    }else{
      delete gSignal;
      delete gProd;
    }
  }
}

Double_t SpectrometerDigitizer::CariocaTransferFunction(Double_t t){//Hybrid: measurements + theory to disentangle experimental effects
  return 1298.485*exp(-0.25641025*t)-271.027752*exp(-0.2*t)-0.6212207*exp(-0.025*t)-0.035308018*exp(-0.002*t)-1026.800997*exp(-0.3846153846*t)-116.1996689*t*exp(-0.38461538*t)-6.00986090236*t*t*exp(-0.3846153846*t);
}

Double_t SpectrometerDigitizer::StrawSignal(Double_t t, Double_t t0){
  return 1./(t+t0);
}

Float_t SpectrometerDigitizer::Diffusion(Float_t Distance){
  Distance /= 10.; //go to cm
  Float_t Distance2 = Distance * Distance;
  Float_t Sigma = 10.214 + 142.11 * Distance - 101.71 * Distance2; //MagBoltz ArCO2 70:30 760Torr 20deg 1750V
  Sigma /= 1000.; //go to mm
  return Sigma;
}

Float_t SpectrometerDigitizer::DriftTime(Float_t Distance){
  Float_t Distance2 = Distance * Distance;
  Float_t Pressure = 810.59776; //To fit 2010 Test Beam data
  Float_t Temperature = 20;
  Float_t Corr = Pressure/1013.2472 * (273.15 + 20)/(273.15 + Temperature);
  return Corr*(  0.00000e+00
      + 1.32000e+01 * Distance
      - 1.97862e+00 * Distance2
      + 1.63943e+00 * Distance2 * Distance
      - 9.18433e-02 * Distance2 * Distance2
      ); //MagBoltz ArCO2 70:30 760Torr 20deg 1750V
}

#endif
