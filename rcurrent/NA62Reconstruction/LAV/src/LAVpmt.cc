// Created by Vito Palladino 18/1/2011
// Modified by E.Leonardi 2014-06-13

#include "LAVpmt.hh"
#include "LAVGeometry.hh"

#include "TMath.h"
#include "TVirtualFFT.h"


LAVpmt::LAVpmt(TRandom3* random) :
  fRandom(random),
  fBlockGlobalID(-1),
  fPhotoElectronTMin(.0),
  fPhotoElectronTMax(.0),
  fNPhotoElectrons(0),
  fNoSignal(false),
  fSignalShape(nullptr),
  fTimeOffset(.0),
  fTimeRange(.0),
  fTimeNBins(0)
{
  fGeometry = LAVGeometry::GetInstance();
  fPhotocathode = new LAVPhotocathode(fGeometry->GetQuantumEfficiency());

  IsFirstElement = kTRUE;
  fTransitTime = fGeometry->GetTransitTime();
  fTransitTimeSpread = fGeometry->GetTransitTimeSpread();
  fTau = fGeometry->GetPMTCapacitance() * fGeometry->GetPMTImpedence();
  fNsigmaTTS  = fGeometry->GetNsigmaTTS();
  fNtauSignal = fGeometry->GetNtauSignal();

  fTimeBinWidth    = fGeometry->GetBinWidth();

  fNDynodes    = fGeometry->GetNdynodes();
  fFFTLimit    = fGeometry->FFTlimit();
  fFFTon       = fGeometry->FFTisON();
}

// Delete all LAVpmt private components
LAVpmt::~LAVpmt(){
  delete fPhotocathode;
  if(fSignalShape) delete [] fSignalShape;
}

// Attach PMT to a block
void LAVpmt::SetChannelID(Int_t BlockId){

  fBlockGlobalID = BlockId;
  LAVpmt::Clear();

}

void LAVpmt::AddPhotonElement(Double_t PhT, Double_t PhE){

  // Store primary photon
  fPrimaryPhotons.push_back(PhT);

  // Store primary electrons after applying photocathode quantum efficiency
  gRandom = fRandom; // set gRandom to fRandom to ensure reproducibility of ApplyQE
  if ( fPhotocathode->ApplyQE(PhE) ) {

    fPhotoElectrons.push_back(PhT);
    if (IsFirstElement) {
      fPhotoElectronTMin = PhT;
      fPhotoElectronTMax = PhT;
      IsFirstElement = kFALSE;
    }
    else {
      if ( PhT < fPhotoElectronTMin ) fPhotoElectronTMin = PhT;
      if ( PhT > fPhotoElectronTMax ) fPhotoElectronTMax = PhT;
    }
    fNPhotoElectrons++;

  }

}

void LAVpmt::Process(){

  if ( fBlockGlobalID == -1 ) {
    std::cout << "==== LAVpmt ==== ERROR: Process() called with no block defined" << std::endl;
    return;
  }

  // See if we have primary electrons
  if ( fPhotoElectrons.size() == 0 ) return;

  // Some signal is present
  fNoSignal = 0;

  // Compute extension of time interval that will be used for PMT simulation
  fTimeOffset = fPhotoElectronTMin - fNsigmaTTS*fTransitTimeSpread;
  Double_t endInterval = fPhotoElectronTMax + fNtauSignal * fTau;
  fTimeNBins = (Int_t)( (endInterval - fTimeOffset) / fTimeBinWidth + 0.5 );
  fTimeRange = fTimeNBins * fTimeBinWidth;

  // Create array to hold the final signal shape
  if(fSignalShape) delete [] fSignalShape;
  fSignalShape = new Double_t[fTimeNBins];

  // Refer all electron times to this time interval
  for (Int_t i=0; i<(Int_t) fPhotoElectrons.size(); i++) {
    fPhotoElectrons.at(i) -= fTimeOffset;
  }

  // Simulate each dynode separately
  for ( Int_t dynode = 0; dynode < fNDynodes; dynode++ ) {

    // For efficiency reasons, we only apply exact dynode simulation
    // to the first fFFTLimit+1 dynodes, then we generate the signal shape
    // histogram and apply the effect of the other dynodes to single bins

    if ( dynode <= fFFTLimit ) {

      ApplyExactDynode(dynode);
      if ( dynode == fFFTLimit ) GenerateSignal();

    } else {

      ApplyHistoDynode(dynode);

    }

    if ( fNoSignal ) break;

  }

  // Add average transit time to final signal
  fTimeOffset += fTransitTime;

}

void LAVpmt::Clear(){

  fPhotoElectrons.clear();
  fPhotoElectronsOut.clear();

  IsFirstElement = kTRUE;
  fPhotoElectronTMin = 0;  // min time will be assigned using the first photo-electron and updated afterwards
  fPhotoElectronTMax = 0;  // max time will be assigned using the first photo-electron and updated afterwards
  fNPhotoElectrons = 0;

  if (fSignalShape) delete[] fSignalShape;
  fSignalShape = NULL;

  fNoSignal = 1;


  fTimeOffset = 0.;
  fTimeRange  = 0.;
  fTimeNBins  = 0;

}

void LAVpmt::ApplyExactDynode(Int_t dynode) {

  // Get dynode parameters
  Double_t collection_efficiency;
  Double_t dynode_gain;
  Double_t transit_time_spread;

  // First dynode is special
  if( dynode == 0 ) {
    collection_efficiency   = fGeometry->GetFirstDynodeCollectionEfficiency();
    dynode_gain             = fGeometry->GetFirstDynodeGain();
    transit_time_spread     = fGeometry->GetTransitTimeSpread_FirstDynode();
  } else {
    collection_efficiency   = fGeometry->GetDynodeCollectionEfficiency();
    dynode_gain             = fGeometry->GetDynodeGain();
    transit_time_spread     = fGeometry->GetTransitTimeSpread();
  }

  //  std::cout << " Dynode " << dynode << " pars: " << collection_efficiency << " " << dynode_gain << " " << transit_time_spread << std::endl;

  fPhotoElectronsOut.clear();

  // For each electron coming from previous dynode...
  for ( Int_t phe = 0; phe < (Int_t) fPhotoElectrons.size(); phe++ ) {

    // ...see if electron is collected by this dynode...
    if ( fRandom->Binomial(1,collection_efficiency) ) {

      // ...then apply dynode gain...
      Int_t nele = fRandom->Poisson(dynode_gain);

      // ...and finally save all new electrons applying the transit time spread
      Double_t time = fPhotoElectrons.at(phe)+fRandom->Gaus(0.,transit_time_spread);
      for (Int_t i=0; i<nele; i++) fPhotoElectronsOut.push_back(time);

    }

  }

  // Drop old photoelectron list and use new one
  if ( fPhotoElectronsOut.size() == 0 ) {
    fNoSignal = 1;
  } else {

//    if (dynode == 0) {
//      std::cout << "Block " << fBlockGlobalID << " dynode " << dynode << " Before: PhotoElectrons " << fPhotoElectrons.size() << std::endl;
//      for (Int_t i=0; i<(Int_t) fPhotoElectrons.size(); i++) std::cout << " Photon # " << i << " Time = " << fPhotoElectrons.at(i) << std::endl;
//    }

    fPhotoElectrons.swap(fPhotoElectronsOut);

//    if (dynode == 0) {
//      std::cout << "Block " << fBlockGlobalID << " dynode " << dynode << " After: PhotoElectrons " << fPhotoElectrons.size() << std::endl;
//      for (Int_t i=0; i<(Int_t) fPhotoElectrons.size(); i++) std::cout << " Photon # " << i << " Time = " << fPhotoElectrons.at(i) << std::endl;
//    }


  }

}

void LAVpmt::ApplyHistoDynode(Int_t dynode) {

  // Get dynode parameters
  Int_t    binomial_limit          = fGeometry->GetBinomialLimit();
  Double_t collection_efficiency   = fGeometry->GetDynodeCollectionEfficiency();
  Double_t dynode_gain             = fGeometry->GetDynodeGain();

  Double_t transit_time_spread;
  if ( fFFTon ) {
    // Use single-dynode time spread if we apply FFT to each dynode
    transit_time_spread = fGeometry->GetTransitTimeSpread();
  } else {
    // Use global time spread if we apply FFT only to last dynode
    transit_time_spread = fGeometry->GetTransitTimeSpread_FFT();
  }

  //  std::cout << "ApplyHistoDynode " << dynode << " " << transit_time_spread << " " << fFFTon << " " << binomial_limit << " " << collection_efficiency << " " << dynode_gain << " " << fNDynodes << std::endl;

  Double_t binContent;
  Int_t totalHistoContent = 0;
  // Compute effect of dynode for each bin
  for (Int_t b = 0; b < fTimeNBins; b++) {

    binContent = 0.;

    // Get number of electrons exiting previous dynode
    Int_t t = (Int_t)(fSignalShape[b]+0.5);
    if ( t > 0 ) {

      // Apply collection efficiency (choose binomial or gaussian according to bin content)
      if ( t < binomial_limit ) {
	binContent = fRandom->Binomial(t,collection_efficiency);
      } else {
	binContent = fRandom->Gaus( t*collection_efficiency, TMath::Sqrt(t*collection_efficiency*(1.-collection_efficiency)) );
      }

      // Apply dynode gain factor to surviving electrons
      if (binContent) {
        Int_t errnoOld = errno;
        Double_t N = fRandom->PoissonD(binContent*dynode_gain); // this line may overflow due to mu^N
        Int_t errnoNew = errno;
        if(errnoOld!=errnoNew && errnoNew==ERANGE){ //the root poisson generator seems to be fine anyway
          errno=errnoOld;
          //// begin home-made poisson generator (very slow)
          //N = 0.;
          //Double_t sum=0.;
          //Double_t Integral = fRandom->Rndm();
          //Double_t LogNFact = 0.;
          //while (sum<Integral){
          //  if(N>=2) LogNFact+=TMath::Log(N);
          //  Double_t Poiss_N = 0.;
          //  if(N*TMath::Log(binContent*dynode_gain)-binContent*dynode_gain-LogNFact>-700){ //cut off to avoid ERANGE error, happening at -745
          //    Poiss_N = TMath::Exp(N*TMath::Log(binContent*dynode_gain)-binContent*dynode_gain-LogNFact);
          //  }
          //  else Poiss_N = TMath::Exp(-700.);
          //  sum+=Poiss_N;
          //  if(sum>=Integral) break;
          //  N++;
          //}
          //// end home-made poisson generator
        }
        binContent = N;
        totalHistoContent += binContent;
      }
    }

    // Save new bin content
    fSignalShape[b] = binContent;

  }

  if (totalHistoContent == 0) {
    fNoSignal = 1;
    return;
  }

  // Apply FFT-based time spread (if required)
  // If fFFTon = 1, FFT time spread is always applied
  // If fFFTon = 0 (default), FFT time spread is only applied to the last dynode
  if ( fFFTon || (dynode==fNDynodes-1) ) {

    // Get FFT of current signal shape
    Double_t* SigRe = new Double_t[fTimeNBins];
    Double_t* SigIm = new Double_t[fTimeNBins];
    TVirtualFFT* SigFFT = TVirtualFFT::FFT(1,&fTimeNBins,"R2C ES");
    SigFFT->SetPoints(fSignalShape);
    SigFFT->Transform();
    SigFFT->GetPointsComplex(SigRe,SigIm);

    // Prepare time resolution function to use in convolution
    Double_t* ResRe = new Double_t[fTimeNBins];
    Double_t* ResIm = new Double_t[fTimeNBins];
    for (Int_t b=0; b<fTimeNBins; b++) {
      Double_t x = b/fTimeRange;
      if(-TMath::Power(x*2.*TMath::Pi()*transit_time_spread,2.)/2.>-700) { //cut off to avoid ERANGE error, happening at -745
        ResRe[b] = TMath::Exp(-TMath::Power(x*2.*TMath::Pi()*transit_time_spread,2.)/2.);
      }
      else ResRe[b] = TMath::Exp(-700.);
      ResIm[b] = 0.;
    }

    // Convolve signal with time resolution function
    Double_t* ProdRe = new Double_t[fTimeNBins];
    Double_t* ProdIm = new Double_t[fTimeNBins];
    for(Int_t i=0; i<fTimeNBins; i++) {
      ProdRe[i] = (ResRe[i]*SigRe[i]-ResIm[i]*SigIm[i])/fTimeNBins;
      ProdIm[i] = (ResRe[i]*SigIm[i]+ResIm[i]*SigRe[i])/fTimeNBins;
    }

    // Anti-transform the result to get final signal shape
    TVirtualFFT* AntiFFT = TVirtualFFT::FFT(1,&fTimeNBins,"C2R ES");
    AntiFFT->SetPointsComplex(ProdRe,ProdIm);
    AntiFFT->Transform();
    AntiFFT->GetPoints(fSignalShape);

    // Clean up memory
    delete[] SigRe;
    delete[] SigIm;
    delete[] ResRe;
    delete[] ResIm;
    delete[] ProdRe;
    delete[] ProdIm;

  }

}

void LAVpmt::GenerateSignal() {

  for ( Int_t bin = 0; bin < (Int_t) fTimeNBins; bin++ ) {
    fSignalShape[bin] = 0;
  }


  for ( Int_t phe = 0; phe < (Int_t) fPhotoElectrons.size(); phe++ ) {
    Int_t bin = (Int_t)(fPhotoElectrons.at(phe)/fTimeBinWidth);
    if ( (bin<0) || (bin>=fTimeNBins) ) {
      std::cout << "=== LAVpmt === Warning!!! Photoelectron time " << fPhotoElectrons.at(phe) << " out of (0.::" << fTimeRange << ")" << std::endl;
    } else {
      fSignalShape[bin]++;
    }
  }

  //  std::cout << "---Histo maker BlockID " << fBlockGlobalID << " " << fPhotoElectrons.size() << std::endl;
  //  for ( Int_t bin = 0; bin < (Int_t) fTimeNBins; bin++ ) {
  //    std::cout << "---Time histo bin " << bin << " " << fSignalShape[bin] << std::endl;
  //  }

}
