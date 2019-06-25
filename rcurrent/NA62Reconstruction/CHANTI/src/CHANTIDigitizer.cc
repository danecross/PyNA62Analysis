#include "Riostream.h"
#include "TCHANTIHit.hh"
#include "TCHANTIDigi.hh"
#include "CHANTIDigitizer.hh"
#include "CHANTIReconstruction.hh"
#include "TDCEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62RecoManager.hh"

bool MySortFunction(const vector<double>& Vect1, const vector<double>& Vect2) {
  return (Vect1[0] < Vect2[0]);
}

CHANTIDigitizer::CHANTIDigitizer(NA62VReconstruction* Reco) : NA62VDigitizer(Reco, "CHANTI"){

  // read number of photons and energy bin
  fPhotonsNumber=static_cast<CHANTIReconstruction*>(fReco)->GetPhotonsNumber();
  fEnergyEdge=   static_cast<CHANTIReconstruction*>(fReco)->GetEnergyEdge();
  fNbinEnergy=   static_cast<CHANTIReconstruction*>(fReco)->GetNbinEnergy();

  // read and set signal parameters  
  fSlopeEnergyNph =        static_cast<CHANTIReconstruction*>(fReco)->GetSlopeEnergyNph();
  fOffsetEnergyNph =       static_cast<CHANTIReconstruction*>(fReco)->GetOffsetEnergyNph();
  fSlopeAmplNph=           static_cast<CHANTIReconstruction*>(fReco)->GetSlopeAmplNph();
  fSigmaAmplSPE=           static_cast<CHANTIReconstruction*>(fReco)->GetSigmaAmplSPE();
  fMeanNfr=                static_cast<CHANTIReconstruction*>(fReco)->GetMeanNfr();
  fSigmaNfr=               static_cast<CHANTIReconstruction*>(fReco)->GetSigmaNfr();
  fHysteresis=             static_cast<CHANTIReconstruction*>(fReco)->GetHysteresis();
  fTauFall=                static_cast<CHANTIReconstruction*>(fReco)->GetTauFall();
  fTHRL=                   static_cast<CHANTIReconstruction*>(fReco)->GetTHRL();
  fTHRH=                   static_cast<CHANTIReconstruction*>(fReco)->GetTHRH();
  fFiberLightSpeed=        static_cast<CHANTIReconstruction*>(fReco)->GetFiberLightSpeed();
  fReflectionFactorBoard = static_cast<CHANTIReconstruction*>(fReco)->GetReflectionFactorBoard();
  fRingsMCToF=             static_cast<CHANTIReconstruction*>(fReco)->GetRingsMCToF();

  fDigiEvent = new TDCEvent(TCHANTIDigi::Class());

  for(int i=0; i<20000; ++i){
    fXAxis[i] = 0.;
    fYAxis[i] = 0.;
  }

  for(int i=0; i<4; ++i){
    fFuncParameters[i] = 0.;
    fPrimaryFuncParameters[i] = 0.;
  }
}

CHANTIDigitizer::~CHANTIDigitizer(){
}

TDetectorVEvent * CHANTIDigitizer::ProcessEvent(TDetectorVEvent * tEvent){

  if(tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") || tEvent->IsA() == TSpecialTriggerEvent::Class())
    return tEvent; 
  TCHANTIEvent * CHANTIEvent = static_cast<TCHANTIEvent*>(tEvent);

  //Inizialization
  Int_t NHits = CHANTIEvent->GetNHits();
  fDigiEvent->Clear();  
  (*(TVEvent*)fDigiEvent)=(*(TVEvent*)CHANTIEvent);
  if (!NHits) return fDigiEvent; 

  //Loop on MC hit 
  std::vector<Int_t> CHid, IndexHit;
  std::vector<Double_t> CHenergy, CHtime, DistanceFromSimp;
  for (Int_t iHit=0;iHit<NHits;iHit++){
    TCHANTIHit *CHANTIHit = static_cast<TCHANTIHit*>( CHANTIEvent->GetHit(iHit));
    Int_t cid = CHANTIHit->EncodeChannelID();
    Double_t ene = CHANTIHit->GetEnergy();
    Double_t time = CHANTIHit->GetTime();
    Double_t dfs = CHANTIHit->GetDistanceFromSiPM();
    bool NewBar = true;
    //Sum energy release in each bar
    for (UInt_t iBar=0; iBar<CHenergy.size(); iBar++){
      if (CHid[iBar] == cid) {
        CHenergy[iBar] += ene; 
        if (time < CHtime[iBar]){
          DistanceFromSimp[iBar] = dfs; 
          CHtime[iBar] = time;  
          IndexHit[iBar] = iHit;
        }
        NewBar = false;
        break;
      }
    } 
    if (NewBar) { 
      DistanceFromSimp.push_back(dfs); 
      CHtime.push_back(time);
      CHid.push_back(cid);
      CHenergy.push_back(ene);
      IndexHit.push_back(iHit);
    }
  }

  double DelayReflectionBoard = 53./(fTauFall/2);

  //Loop on Bars fired and Digi generation
  for (UInt_t iBar=0; iBar<CHenergy.size(); iBar++){

    TCHANTIHit * Hit = static_cast<TCHANTIHit*>( CHANTIEvent->GetHit(IndexHit[iBar]));
    Double_t MeanNph = (CHenergy[iBar]*fSlopeEnergyNph)-fOffsetEnergyNph;

    MeanNph = 667*(1-TMath::Exp(-MeanNph/667));

    Int_t errnoOld = errno;
    Double_t N = fRandom->PoissonD(MeanNph); // this line may overflow due to mu^N
    Int_t errnoNew = errno;
    if(errnoOld!=errnoNew && errnoNew==ERANGE){ //the root poisson generator seems to be fine anyway
      errno=errnoOld;
      //// begin home-made poisson generator
      //N = 0.;
      //Double_t sum=0.;
      //Double_t Integral = fRandom->Rndm();
      //Double_t LogNFact = 0.;
      //while (sum<Integral){
      //  if(N>=2) LogNFact+=TMath::Log(N);
      //  Double_t Poiss_N = 0.;
      //  if(N*TMath::Log(MeanNph)-MeanNph-LogNFact>-700){ //cut off to avoid ERANGE error, happening at -745
      //    Poiss_N = TMath::Exp(N*TMath::Log(MeanNph)-MeanNph-LogNFact);
      //  }
      //  else Poiss_N = TMath::Exp(-700.);
      //  sum+=Poiss_N;
      //  if(sum>=Integral) break;
      //  N++;
      //}
      // end home-made poisson generator
    }
    Int_t NphCollected = N;

    Double_t StartSignal = CHtime[iBar]+DistanceFromSimp[iBar]/fFiberLightSpeed;
    Double_t TimeScaleConstant = fTauFall/2;    
    Double_t MaxSignal = fRandom->Gaus(NphCollected*fSlopeAmplNph,TMath::Sqrt(NphCollected)*fSigmaAmplSPE); 

    //Define digitized signal

    fFuncParameters[0] = MaxSignal;
    fFuncParameters[1] = fRandom->Gaus(fMeanNfr/TimeScaleConstant,fSigmaNfr/TimeScaleConstant);
    fFuncParameters[2] = fReflectionFactorBoard;
    fFuncParameters[3] = DelayReflectionBoard;
    fPrimaryFuncParameters[0] = MaxSignal;
    fPrimaryFuncParameters[1] = fFuncParameters[1];
    fPrimaryFuncParameters[2] = 1.;
    fPrimaryFuncParameters[3] = 0.;

    for (int iPoint=0; iPoint<fNpx; iPoint++){
      fXAxis[iPoint] = iPoint*30./fNpx;
      fYAxis[iPoint] = SignalContribution_Def(fXAxis[iPoint],fPrimaryFuncParameters) + SignalContribution_Def(fXAxis[iPoint], fFuncParameters);
    }

    if (MaxSignal>fTHRL){
      Int_t ROCH = static_cast<TDCBRawDecoder*>(static_cast<CHANTIReconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(CHid[iBar]);
      CreateDigiStructure(Hit, StartSignal, fTHRL, ROCH, CHid[iBar], true);
      if (MaxSignal>fTHRH)
        CreateDigiStructure(Hit, StartSignal, fTHRH, ROCH, CHid[iBar], false);        
    }
  }
  return fDigiEvent;
}

vector<double> CHANTIDigitizer::GetPositiveXtrg(double Th){

  vector<double> TrgPositiveX;
  for ( int i=1; i<fNpx; i++ )
    if( fYAxis[i-1]<Th && fYAxis[i]>=Th )
      TrgPositiveX.push_back( (fXAxis[i-1]+fXAxis[i])/2 );

  return TrgPositiveX;

}

vector<double> CHANTIDigitizer::GetNegativeXtrg(double Th){

  vector<double> TrgNegativeX;
  for ( int i=1; i<fNpx; i++ )
    if( fYAxis[i-1]>Th && fYAxis[i]<=Th )
      TrgNegativeX.push_back( (fXAxis[i-1]+fXAxis[i])/2 );

  return TrgNegativeX;

}

void CHANTIDigitizer::AddSingleDigi(TCHANTIHit* Hit,double Leading, double Trailing, Int_t Channel, bool IsLowThr){
  TCHANTIDigi *Digi = static_cast<TCHANTIDigi*>(fDigiEvent->AddDigi(Hit));
  if (IsLowThr) Digi->SetChannelID(Channel);
  else Digi->SetChannelID(Channel+1);
  Digi->DecodeChannelID();
  Digi->SetLeadingEdge(Leading);
  Digi->SetTrailingEdge(Trailing);
}


void CHANTIDigitizer::CreateDigiStructure(TCHANTIHit* Hit, double StartSignal, double threshold, int ROCH, Int_t Channel, bool IsLowThr){
  Double_t TimeScaleConstant = fTauFall/2;
  vector<double> LeadingTimeLow = GetPositiveXtrg(threshold);
  vector<double> TrailingTimeLow = GetNegativeXtrg(threshold-fHysteresis);
  UInt_t NEdge = LeadingTimeLow.size()+TrailingTimeLow.size();
  vector<double> EdgeTime;
  vector < vector <double> > MatrixTime;
  int iRing =2*(Channel/100000 - 1) + (Channel%100000)/10000;

  Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;
  for (UInt_t iTime = 0; iTime<NEdge; iTime++){
    if (iTime<LeadingTimeLow.size()){
      EdgeTime.clear();
      EdgeTime.push_back(LeadingTimeLow[iTime]*TimeScaleConstant + StartSignal + FineTime - static_cast<CHANTIReconstruction*>(fReco)->GetStationMCToF(Hit->GetStationID()) - fRingsMCToF[iRing] + fReco->GetT0Correction(Hit->GetChannelID(),Hit->GetStationID()) + fChannels[ROCH]->GetT0());
      EdgeTime.push_back(1);
      MatrixTime.push_back(EdgeTime);
    } else {
      EdgeTime.clear();
      EdgeTime.push_back(TrailingTimeLow[iTime-LeadingTimeLow.size()]*TimeScaleConstant + StartSignal + FineTime - static_cast<CHANTIReconstruction*>(fReco)->GetStationMCToF(Hit->GetStationID()) - fRingsMCToF[iRing] + fReco->GetT0Correction(Hit->GetChannelID(),Hit->GetStationID()) + fChannels[ROCH]->GetT0());
      EdgeTime.push_back(0);
      MatrixTime.push_back(EdgeTime);
    }
  }
  sort(MatrixTime.begin(), MatrixTime.end(), MySortFunction);

  if (NEdge==0) return;
  UInt_t NDetectedEdge = 1;
  for (UInt_t iEdge=0;iEdge<NEdge-1; iEdge++){
    if ( MatrixTime[NDetectedEdge][0]-MatrixTime[NDetectedEdge-1][0] < 5.){
      MatrixTime.erase(MatrixTime.begin()+NDetectedEdge); 
    } else NDetectedEdge++; 
  }

  bool FlagLeading = false;
  for (UInt_t iEdge=0;iEdge<NDetectedEdge; iEdge++){

    if (!FlagLeading && MatrixTime[iEdge][1]==0) { 
      AddSingleDigi(Hit,-1e28,(Int_t)(MatrixTime[iEdge][0]/TdcCalib)*TdcCalib, Channel, IsLowThr);
      continue;
    }
    if (!FlagLeading && MatrixTime[iEdge][1]==1) { 
      FlagLeading=true;
      continue;
    }
    if (FlagLeading && MatrixTime[iEdge][1]==0) {
      AddSingleDigi(Hit,(Int_t)(MatrixTime[iEdge-1][0]/TdcCalib)*TdcCalib,(Int_t)(MatrixTime[iEdge][0]/TdcCalib)*TdcCalib, Channel, IsLowThr);
      FlagLeading=false;
      continue;
    }
    if (FlagLeading && MatrixTime[iEdge][1]==1) {AddSingleDigi(Hit,(Int_t)(MatrixTime[iEdge-1][0]/TdcCalib)*TdcCalib,-1e28, Channel, IsLowThr);
      continue;	
    }
  }
}


double CHANTIDigitizer::SignalContribution_Def(Double_t x, Double_t *par){
  if (x<=par[3]) return 0;
  else return ( par[2]*par[0]*TMath::Sqrt(TMath::Power((x-par[3])/(par[1]),par[1])*TMath::Exp(par[1]-(x-par[3]))));
}
