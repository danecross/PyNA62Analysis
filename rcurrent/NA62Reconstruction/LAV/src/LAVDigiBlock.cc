// LAVDigiBlock.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - added doxygen compliant documentation
// Created by T. Spadaro and E. Leonardi (tommaso.spadaro@cern.ch, emanuele.leonardi@cern.ch) 2015-01-22
// --------------------------------------------------------------
/// \class LAVDigiBlock
/// \Brief
/// Class for LAV digitized hits (i.e., the digi hit information soft class living in the context of the reconstruction code)
/// \EndBrief
#include "LAVGeometry.hh"
#include "LAVpmt.hh"
#include "LAVDigiBlock.hh"

#include "LAVConfiguration.hh" 

#define ELECTRON_CHARGE 1.6025e-19 

LAVDigiBlock::LAVDigiBlock(Int_t BlockID, Int_t nOpticalPhotons) :
  fBlockID(BlockID),
  fNOpticalPhotons(nOpticalPhotons),
  fNPhotoElectrons(0),
  fPhotoElectronTMin(0),
  fPhotoElectronTMax(0),
  fOpticalPhotonTMin(0),
  fOpticalPhotonTMax(0),
  fHitTime(0),
  fMCHit(nullptr),
  fTOffset(0),
  fTRange(0)
{
  // Optical Photons
  fOpticalPhotonTime = new Double_t[fNOpticalPhotons];
  fOpticalPhotonEnergy = new Double_t[fNOpticalPhotons];

  // PMT parameters

  LAVGeometry* Geom = LAVGeometry::GetInstance();
  fBinWidth         = Geom->GetBinWidth();
  fImpedence        = Geom->GetPMTImpedence(); 
  fTau              = fImpedence*Geom->GetPMTCapacitance();
  fNtau             = Geom->GetNtau();
  fHyTimeC          = Geom->GetHysteresiTimeConstant();

  LAVConfiguration* Config = LAVConfiguration::GetInstance();
  if (!Config->HasReadConfigurationInfos()) {
    std::cerr << "LAVDigiBlock: cannot retrieve threshold values " << std::endl;
    exit(kWrongConfiguration);
  }

  Double_t hyst = Config->GetBlockHysteresis(fBlockID);
  fLeThLow          = Config->GetBlockThresholdLow(fBlockID);
  fTrThLow          = fLeThLow - hyst;
  fLeThHigh         = Config->GetBlockThresholdHigh(fBlockID);
  fTrThHigh         = fLeThHigh - hyst;

  fNorm             = Geom->GetChargeNormalization();

  // Photo-electron distribution and generaed signal

  fNBins = 0;
  fSignal = NULL;
  fGeneratedSignal = NULL;

  // Evaluated edges and signal properties
 
  fEdgesLow.clear();
  fEdgeTypesLow.clear();
  
  fEdgesHigh.clear();
  fEdgeTypesHigh.clear();
  fSignalPeak = -1;
  fTSignalMax = -1;
  fCharge = 0;

}

LAVDigiBlock::~LAVDigiBlock(){
  if (fSignal) delete[] fSignal;  
  fSignal = NULL;
  if (fGeneratedSignal) delete[] fGeneratedSignal;  
  fGeneratedSignal = NULL;
  if (fOpticalPhotonTime) delete[] fOpticalPhotonTime;  
  fOpticalPhotonTime = NULL; 
  if (fOpticalPhotonEnergy) delete[] fOpticalPhotonEnergy;  
  fOpticalPhotonEnergy = NULL; 
}

void LAVDigiBlock::SetSignalElement(Int_t i, Double_t content){
  if (i>=0 && i < fNBins) {
    fSignal[i] = content;
  }
  else {
    std::cerr << "LAVDigiBlock SetSignalElement Error " << i << " " << fNBins << std::endl;
    exit(kGenericError);
  }
}

Double_t LAVDigiBlock::GetSignalElement(Int_t i){
  if (i>=0 && i < fNBins) {
    return fSignal[i];
  }
  else {
    std::cerr << "LAVDigiBlock GetSignalElement Error " << i << " " << fNBins << std::endl;
    exit(kGenericError);
  }
}

Double_t LAVDigiBlock::GetGeneratedSignalElement(Int_t i){
  if (i>=0 && i < fNBins) {
    return fGeneratedSignal[i];
  }
  else {
    std::cerr << "LAVDigiBlock GetGeneratedSignalElement Error " << i << " " << fNBins << std::endl;
    exit(kGenericError);
  }
}


void LAVDigiBlock::Add(LAVDigiBlock* DigiBlockExt) {

  // Perform Add for the Optical photon distribution

  Int_t newOPNumber = this->GetNOpticalPhotons() + DigiBlockExt->GetNOpticalPhotons();

  Double_t* newOPTime = new Double_t[newOPNumber];
  Double_t* newOPEnergy = new Double_t[newOPNumber];

  for (Int_t i=0; i< this->GetNOpticalPhotons(); i++) {
    newOPTime[i] = this->GetOpticalPhotonTimeElement(i);
    newOPEnergy[i] = this->GetOpticalPhotonEnergyElement(i);
  }
  for (Int_t i=this->GetNOpticalPhotons(); i< newOPNumber; i++) {
    newOPTime[i] = DigiBlockExt->GetOpticalPhotonTimeElement(i-this->GetNOpticalPhotons());
    newOPEnergy[i] = DigiBlockExt->GetOpticalPhotonEnergyElement(i-this->GetNOpticalPhotons());
  }

  // decide which optical photon collection dominates and store the MC infos

  if (this->GetNOpticalPhotons() < DigiBlockExt->GetNOpticalPhotons()) {
    fHitTime = DigiBlockExt->GetHitTime();
    fMCHit = DigiBlockExt->GetMCHit();
  }

  // merge the optical photon collections

  delete[] fOpticalPhotonTime;
  fOpticalPhotonTime = newOPTime;
  delete[] fOpticalPhotonEnergy;
  fOpticalPhotonEnergy = newOPEnergy;

  fNOpticalPhotons = newOPNumber;
  
  fOpticalPhotonTMin = TMath::Min(this->GetOpticalPhotonTimeMin(), DigiBlockExt->GetOpticalPhotonTimeMin());
  fOpticalPhotonTMax = TMath::Max(this->GetOpticalPhotonTimeMax(), DigiBlockExt->GetOpticalPhotonTimeMax());

  // Perform Add for the Last dynode photo-electron distribution

  Int_t oldNBinsi = this->GetNBins();
  Int_t oldNBinsj = DigiBlockExt->GetNBins();

  if ( oldNBinsj == 0) {} // do nothing
  else if (oldNBinsi == 0 ) { 

    fTOffset = DigiBlockExt->GetHitOffset();
    fTRange = DigiBlockExt->GetTimeRange();
    fNBins = DigiBlockExt->GetNBins();

    if (fSignal) delete[] fSignal; // should not happen, but this is safer
    fSignal = new Double_t[fNBins]; 
    for (Int_t i = 0; i<fNBins; i++) {
      fSignal[i] = DigiBlockExt->GetSignalElement(i);
    }
    fHitTime = DigiBlockExt->GetHitTime();
    fMCHit = DigiBlockExt->GetMCHit();    
  }
  else {

    Double_t oldTIni = this->GetTimeBegin();
    Double_t oldTEndi = this->GetTimeEnd();
    Double_t oldTInj = DigiBlockExt->GetTimeBegin();
    Double_t oldTEndj = DigiBlockExt->GetTimeEnd();

    Double_t newTIn;
    Double_t newTEnd;

    Int_t nShiftLi;
    Int_t nShiftLj;
    Int_t nShiftRi;

    if (oldTIni < oldTInj) {
      newTIn = oldTIni;
      nShiftLi = 0;
      nShiftLj = (oldTInj-oldTIni)/fBinWidth+0.5;
    }
    else {
      nShiftLi = (oldTIni-oldTInj)/fBinWidth+0.5;
      newTIn = oldTIni - nShiftLi*fBinWidth;
      nShiftLj = 0;
    }

    if (oldTEndi > oldTEndj) {
      newTEnd = oldTEndi;
      nShiftRi = 0;
    }
    else {
      nShiftRi = (oldTEndj-oldTEndi)/fBinWidth+0.5;
      newTEnd = oldTEndi + nShiftRi*fBinWidth;
    }

    Int_t newBins = (nShiftLi + oldNBinsi + nShiftRi);
  
    Double_t* newSignal = new Double_t[newBins];

    Double_t integrali=0;
    Double_t integralj=0;

    for (Int_t k = 0; k< newBins; k++) {
      newSignal[k] = 0;    
      if (k-nShiftLi >=0 && k-nShiftLi < oldNBinsi) {
	newSignal[k] += this->GetSignalElement(k-nShiftLi);
	integrali += this->GetSignalElement(k-nShiftLi);
      }
      if (k-nShiftLj >=0 && k-nShiftLj < oldNBinsj) {
	newSignal[k] += DigiBlockExt->GetSignalElement(k-nShiftLj);
	integralj += DigiBlockExt->GetSignalElement(k-nShiftLj);
      }
    }

    // Allocate new set of private variables and exit;

    fTOffset = newTIn; 
    fTRange = newTEnd - newTIn;
    fNBins = newBins;

    delete[] fSignal;
    fSignal = newSignal;

    if (integrali > integralj) {
      // fHitTime, fMCHit do not change
      //      fTOffset += newTIn - oldTIni;
    }
    else {
      fHitTime = DigiBlockExt->GetHitTime();
      //      fTOffset = DigiBlockExt->GetHitOffset() + newTIn - oldTInj;
      fMCHit = DigiBlockExt->GetMCHit();
    }

  }
}


Int_t LAVDigiBlock::ApplyPMT(TRandom3* aRandom){

  // instance a PMT object

  LAVpmt* pmt = new LAVpmt(aRandom);

  // set BlockID to PMT --> PMT retrieves its characteristics

  pmt->SetChannelID(fBlockID);

  // pass one-by-one the optical photons to PMT

  for (Int_t i=0; i<fNOpticalPhotons; i++){
    pmt->AddPhotonElement(fOpticalPhotonTime[i], fOpticalPhotonEnergy[i]);
  }
  fNPhotoElectrons = pmt->GetNPhotoElectrons();
  fPhotoElectronTMin = pmt->GetPhotoElectronTMin();
  fPhotoElectronTMax = pmt->GetPhotoElectronTMax();

  // ask PMT to process the optical photons
  pmt->Process();

  // retrieve last dynode photoelectron distribution from PMT

  if (pmt->SignalIsPresent()) {
    fNBins = pmt->GetNBins();

    fTOffset = pmt->GetOffset(); // absolute time at the beginning of the signal histogram 
    fTRange = pmt->GetRange();   // signal histogram time range

//toBeRemovedAfterVerification    fOpticalPhotonTMin = fTOffset;
//toBeRemovedAfterVerification    fOpticalPhotonTMax = fTOffset + fTRange;

    if (fSignal) delete[] fSignal;
    fSignal = new Double_t[fNBins];
    for (Int_t i=0; i<fNBins; i++) {
      fSignal[i] = pmt->GetLastDynodeElectronDistribution(i);
    }
  }

  delete pmt;
  return fNBins;

}

void LAVDigiBlock::Digitize(){
  
  fGeneratedSignal = new Double_t[fNBins];
  Double_t ToV = ( fImpedence * ELECTRON_CHARGE ) / ( fBinWidth * 1.e-9 ) ; // fBinWidth is ns
  
  Double_t dumpFactor = TMath::Exp(-fBinWidth/fTau);
  fGeneratedSignal[0] = fSignal[0]*ToV/fNorm;
  fSignalPeak = fGeneratedSignal[0];

  fCharge += fSignal[0]*ELECTRON_CHARGE*1.e12;
 
  Int_t IsAleading = 1;
  Int_t IsAtrailing = -1;

//  Bool_t LeadingDetectedLow = 0;
//  Bool_t LeadingDetectedHigh = 0;

  Int_t aboveThLo = 0;
  Int_t aboveThHi = 0;
  Double_t thresholdLo = fLeThLow;  // default value
  Double_t thresholdHi = fLeThHigh; // default value

  Double_t timeLo; // time passed above Lo threshold
  Double_t timeHi; // time passed above Hi threshold

  if (fGeneratedSignal[0] > thresholdLo) {
    std::cout << "LAVDigiBlock >> Warning: Signal starts above threshold Lo: V= " << fGeneratedSignal[0] << " thr = " << thresholdLo << std::endl;
    aboveThLo = 1;
    thresholdLo = fTrThLow;
    timeLo = 0; // initialize
  }
  if (fGeneratedSignal[0] > thresholdHi) {
    std::cout << "LAVDigiBlock >> Warning: Signal starts above threshold Hi: V= " << fGeneratedSignal[0] << " thr = " << thresholdHi << std::endl;
    aboveThHi = 1;
    thresholdHi = fTrThHigh; 
    timeHi = 0; // initialize
  }

  // define the starting levels: above or below threshold

  for (Int_t i = 1; i < fNBins; i++){
    
    fGeneratedSignal[i] = ( fSignal[i] * ToV / fNorm + fGeneratedSignal[i-1] * dumpFactor );
    fCharge += fSignal[i]*ELECTRON_CHARGE*1.e12;

    ////////////  Low Th

    if (aboveThLo) {
      timeLo += fBinWidth;
      thresholdLo = fLeThLow - (fLeThLow-fTrThLow)*exp(-timeLo/fHyTimeC); // update threshold taking into account feedback circuit time constant

      if (fGeneratedSignal[i] < thresholdLo) {
	aboveThLo = 0;
	fEdgesLow.push_back( fTOffset + i*fBinWidth );
	fEdgeTypesLow.push_back( IsAtrailing );
	thresholdLo = fLeThLow;
      }
      // else (take care of comparator time constant: zero again after a given time)      
    }
    else {
      if (fGeneratedSignal[i] > thresholdLo) {
	aboveThLo = 1;
	fEdgesLow.push_back( fTOffset + i*fBinWidth );
	fEdgeTypesLow.push_back( IsAleading );
	thresholdLo = fTrThLow;
	timeLo = 0;
      }
    }

//    if( fGeneratedSignal[i] >= fLeThLow && fGeneratedSignal[i-1] < fLeThLow ){
//      fEdgesLow.push_back( i*fBinWidth+fTOffset );
//      fEdgeTypesLow.push_back( IsAleading );
//      LeadingDetectedLow = 1;
//    }
//
//    if( fGeneratedSignal[i] <= fTrThLow && fGeneratedSignal[i-1] > fTrThLow && LeadingDetectedLow){
//      fEdgesLow.push_back( i*fBinWidth+fTOffset );
//      fEdgeTypesLow.push_back( IsAtrailing );
//      LeadingDetectedLow = 0;
//    }


    ////////////  High Th

    if (aboveThHi) {
      timeHi += fBinWidth;
      thresholdHi = fLeThHigh - (fLeThHigh-fTrThHigh)*exp(-timeHi/fHyTimeC); // update threshold taking into account feedback circuit time constant

      if (fGeneratedSignal[i] < thresholdHi) {
	aboveThHi = 0;
	fEdgesHigh.push_back( fTOffset + i*fBinWidth );
	fEdgeTypesHigh.push_back( IsAtrailing );
	thresholdHi = fLeThHigh;
      }
      // else (take care of comparator time constant: zero again after a given time)      

    }
    else {
      if (fGeneratedSignal[i] > thresholdHi) {
	aboveThHi = 1;
	fEdgesHigh.push_back( fTOffset + i*fBinWidth );
	fEdgeTypesHigh.push_back( IsAleading );
	thresholdHi = fTrThHigh;
	timeHi = 0;
      }
    }

    //////////// signal max

    if( fGeneratedSignal[i] > fSignalPeak ){
      fSignalPeak  = fGeneratedSignal[i];
      fTSignalMax = fTOffset + i*fBinWidth;
    }  
  }

  if (aboveThLo) {
    std::cout << "LAVDigiBlock >> Warning: Signal ends above threshold Lo: V= " << fGeneratedSignal[fNBins] << " thr = " << thresholdLo << std::endl;
  }
  if (aboveThHi) {
    std::cout << "LAVDigiBlock >> Warning: Signal ends above threshold Hi: V= " << fGeneratedSignal[fNBins] << " thr = " << thresholdHi << std::endl;
  }  
}

void LAVDigiBlock::Print() {
  std::cout << "---LAVDigiBlock Summary for Block " << fBlockID << " with " << fNBins << " Timebins and max = " << fTSignalMax << std::endl;
  if (fGeneratedSignal) {
    for (Int_t i=0; i< fNBins; i++) {      
      std::cout << "Bin " << i << " T= " << i*fBinWidth + fTOffset << " V= " << fGeneratedSignal[i] << std::endl;
    }
  }
}

Double_t LAVDigiBlock::GetRiseTime(){ 
  Int_t tBin10 = -1; // timebin of crossing of 10% x Max 
  Int_t tBin90 = -1; // timebin of crossing of 90% x Max 

  if (fGeneratedSignal) {
    for (Int_t i=0; i< fNBins; i++) {
      if (tBin10 == -1 && fGeneratedSignal[i] > 0.1*fSignalPeak) tBin10 = i;
      if (fGeneratedSignal[i] > 0.9*fSignalPeak) {
	tBin90 = i; 
	break;
      }
    }
  }
  return (tBin90-tBin10)*fBinWidth;
}
