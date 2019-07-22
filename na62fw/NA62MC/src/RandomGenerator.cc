#include "RandomGenerator.hh"
#include "DatacardManager.hh"
using namespace TMath;

RandomGenerator* RandomGenerator::fInstance = 0;

RandomGenerator::RandomGenerator():
  fRandDecay(nullptr),
  fIntegralHasBeenCalculated(false)
{}

RandomGenerator::~RandomGenerator() {
  if (fRandDecay) delete fRandDecay;
}

RandomGenerator* RandomGenerator::GetInstance() {
  if (!fInstance) fInstance = new RandomGenerator();
  return fInstance;
}

void RandomGenerator::Init(unsigned int seed) {
  fRandDecay = new TRandom3();
  fRandDecay->SetSeed(seed);
  fIntegralHasBeenCalculated = kFALSE;
  gRandom = fRandDecay;
}

void RandomGenerator::Init(TRandom3* RandDecay) {
  fRandDecay = new TRandom3();
  *fRandDecay = *RandDecay;
  fIntegralHasBeenCalculated = kFALSE;
}

void RandomGenerator::GetRandom2(TH2D* histoInput, Double_t &x, Double_t &y) {
  Int_t nbinsx = histoInput->GetNbinsX();
  Int_t nbinsy = histoInput->GetNbinsY();
  Int_t nbins  = nbinsx*nbinsy;
  Double_t integral;

// compute integral checking that all bins have positive content (see ROOT-5894)

  if (mapOfHistograms.find(histoInput) != mapOfHistograms.end()) { // check if integral for that histograms has been evaluated
    if (histoInput->GetIntegral()[nbins+1] != histoInput->GetEntries()) integral = histoInput->ComputeIntegral(true);
    else integral = histoInput->GetIntegral()[nbins];
  } else {
    integral = histoInput->ComputeIntegral(true);
    mapOfHistograms.insert(std::make_pair(histoInput, histoInput->GetEntries() ));
  }
  if (integral == 0 ) { x = 0; y = 0; return;}


  // case histogram has negative bins
  if (integral == TMath::QuietNaN() ) { x = TMath::QuietNaN(); y = TMath::QuietNaN(); return;}

  Double_t* gInt = histoInput->GetIntegral();

  Double_t r1 = fRandDecay->Rndm();
  Int_t ibin = TMath::BinarySearch(nbins,gInt,(Double_t) r1);
  Int_t biny = ibin/nbinsx;
  Int_t binx = ibin - nbinsx*biny;

  x = histoInput->GetXaxis()->GetBinLowEdge(binx+1);
  if (r1 > gInt[ibin])
    x += histoInput->GetXaxis()->GetBinWidth(binx+1)*(r1-gInt[ibin])/(gInt[ibin+1] - gInt[ibin]);
  y = histoInput->GetYaxis()->GetBinLowEdge(biny+1) + histoInput->GetYaxis()->GetBinWidth(biny+1)*fRandDecay->Rndm();

}
