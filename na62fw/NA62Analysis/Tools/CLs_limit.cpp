//////////////////////////////////////////////////////////
// Computation of CLs upper limits at 90% CL.
// Evgueni Goudzovski (goudzovs@cern.ch), 8 July 2019.
// The method is described in the internal note NA62-19-04.

#include <iostream>
#include "TMath.h"
#include "TRandom.h"
#include "TF1.h"

using namespace std;

double UpperLimit_gaus(int NObserved, double Nexpected, double dNexpected, double Level);
double UpperLimit_general(int NObserved, int Nbackgrounds, int *Nbkg, double *ScaleFactor, double Level);
double CLs_gaus(int Ndata, double mu_signal, double bkg, double ebkg);
double CLs_zero_ebkg(int Ndata, double mu_signal, double bkg);
double CLs_general(int Ndata, double mu_signal, int Nbackgrounds, int *bkg, double *sf);
void   error();

int main(int argc, char** argv) {
  if (argc<5) error();

  ///////////////////////////////
  // Read command line parameters

  TString mode      = argv[1];
  int     NObserved = atoi(argv[2]);
  if (NObserved<0) error();

  // For gaussian errors on the background
  double Nexpected = 0.0, dNexpected = 0.0;

  // For non-gaussian errors on the background
  int    NBackgrounds = 0;
  int    Nbkg[10];
  double ScaleFactor[10];
  double Level = 0.1;

  bool gaussian_mode = true;
  if (mode=="gaus") {
    if (argc>6) error();
    Nexpected  = atof(argv[3]);
    dNexpected = atof(argv[4]);
    if (argc==6) Level = 1.0-atof(argv[5]);
  }
  else if (mode=="general") {
    if (argc<6) error();
    gaussian_mode = false;
    NBackgrounds = atoi(argv[3]);
    if (NBackgrounds<0 || NBackgrounds>10) error();
    int NexpectedParameters = 2*NBackgrounds + 4;
    if (argc!=NexpectedParameters && argc!=NexpectedParameters+1) error();
    for (int ibkg=0; ibkg<NBackgrounds; ibkg++) {
      Nbkg[ibkg]        = atoi(argv[4+2*ibkg]);
      ScaleFactor[ibkg] = atof(argv[5+2*ibkg]);
    }
    if (argc==NexpectedParameters+1) Level = 1.0-atof(argv[NexpectedParameters]);
  }
  else {
    error();
  }
  if (Level<0.0 || Level>1.0) error();

  //////////////////
  // The computation

  gRandom->SetSeed(1); // to ensure reproducibility

  if (gaussian_mode) {
    if (NObserved>=0.0) { // compute the upper limit
      double UL = UpperLimit_gaus(NObserved, Nexpected, dNexpected, Level);
      printf("%3.1f\n", UL);
    }
    else { // compute the expected upper limit in background-only hypothesis
      double upper_limits[200000];
      for (int i=0; i<200000; i++) upper_limits[i] = -1.0;
      vector<double>Trials;
      for (int iTrial=0; iTrial<1000; iTrial++) { // multiple trials
	double Lambda = gRandom->Gaus(Nexpected, dNexpected);
	int    Nobs   = gRandom->Poisson(Lambda);
	double ul;
	if (Nobs>=200000) { // do not store this UL
	  ul = UpperLimit_gaus(1.0*Nobs, Nexpected, dNexpected, Level);
	}
	else { // store this UL
	  ul = upper_limits[Nobs];
	  if (ul<0.0) {
	    ul = UpperLimit_gaus(1.0*Nobs, Nexpected, dNexpected, Level);
	    upper_limits[Nobs] = ul;
	  }
	}
	Trials.push_back(ul);
	//std::cout << iTrial << " " << std::flush;
      }
      //std::cout << std::endl;
      std::sort(Trials.begin(), Trials.end());
      double ExpBandLow2  = Trials[ 22]; // -2 sigma
      double ExpBandLow1  = Trials[158]; // -1 sigma
      double ExpLimit     = 0.5*(Trials[499]+Trials[500]);
      double ExpBandHigh1 = Trials[841]; // +1 sigma
      double ExpBandHigh2 = Trials[977]; // +2 sigma

      printf("Expected -2sigma, -1sigma, mean, +1sigma, +2sigma\n%3.1f %3.1f %3.1f %3.1f %3.1f\n",
	     ExpBandLow2, ExpBandLow1, ExpLimit, ExpBandHigh1, ExpBandHigh2);
    }
  }

  else { // non-gaussian errors: the upper limit only
    double UL = UpperLimit_general(NObserved, NBackgrounds, Nbkg, ScaleFactor, Level);
    printf("%3.1f\n", UL);
  }
  return 0;
}

double UpperLimit_gaus(int NObserved, double Nexpected, double dNexpected, double Level) {

  // Determine the initial range where to look for the solution
  double mu_min = 0;
  double mu_max = (NObserved) ? 1.0*NObserved : 1.0;
  while (CLs_gaus(NObserved, mu_max, Nexpected, dNexpected)>Level) mu_max *= 2.0;

  // Find the solution within this range
  double mu = 0.5*(mu_min+mu_max);
  while (mu_max-mu_min > 0.1 && mu_max-mu_min > 1e-4*mu) { // the precision required
    double CLs_mid = CLs_gaus(NObserved, mu, Nexpected, dNexpected);
    /*
    cout <<" ... solving: " << mu_min <<" " << mu << " " << mu_max << " --> " <<
      CLs(NObserved, mu_min, Nexpected, dNexpected) <<" " << CLs_mid << " " <<
      CLs(NObserved, mu_max, Nexpected, dNexpected) << endl;
    */
    if (CLs_mid<Level) mu_max = mu;
    else               mu_min = mu;
    mu = 0.5*(mu_min+mu_max);
  }
  return mu;
}

double UpperLimit_general(int NObserved, int NBackgrounds, int *Nbkg, double *ScaleFactor, double Level) {

  // Determine the initial range where to look for the solution
  double mu_min = 0;
  double mu_max = (NObserved) ? 1.0*NObserved : 1.0;
  while (CLs_general(NObserved, mu_max, NBackgrounds, Nbkg, ScaleFactor)>Level) mu_max *= 2.0;

  // Find the solution within this range
  double mu = 0.5*(mu_min+mu_max);
  while (mu_max-mu_min > 0.1 && mu_max-mu_min > 1e-4*mu) { // the precision required
    double CLs_mid = CLs_general(NObserved, mu, NBackgrounds, Nbkg, ScaleFactor);
    /*
    cout <<" ... solving: " << mu_min <<" " << mu << " " << mu_max << " --> " <<
      CLs_general(NObserved, mu_min, NBackgrounds, Nbkg, ScaleFactor) <<" " << CLs_mid << " " <<
      CLs_general(NObserved, mu_max, NBackgrounds, Nbkg, ScaleFactor) << endl;
    */
    if (CLs_mid<Level) mu_max = mu;
    else               mu_min = mu;
    mu = 0.5*(mu_min+mu_max);
  }
  return mu;
}

double CLs_gaus(int Ndata, double mu_signal, double bkg, double ebkg) {
  if (ebkg<0.0001) return CLs_zero_ebkg(Ndata, mu_signal, bkg);

  int mToys = 500000;
  int clb = 0, clsb = 0;

  for (int i=0; i<mToys; i++) {

    // Generation of the number of background events
    double mu_bkg = -1.0;
    while (mu_bkg<=0.0) mu_bkg = gRandom->Gaus(bkg, ebkg);
    double musb = mu_signal + mu_bkg;

    // The test statistic: the likelihood ratio
    //double xobs = TMath::PoissonI(Ndata, musb)/TMath::PoissonI(Ndata, mu_bkg); // the original formula
    //double xobs = pow(musb/mu_bkg, Ndata) * exp(mu_bkg-musb);                  // identical, simplified
    //double xobs = Ndata * log(musb/mu_bkg) + (mu_bkg-musb);                    // logarithm
    double xobs = Ndata * log(musb/mu_bkg);                                      // constant term removed

    // Confidence level for the background hypothesis (CL_b)
    int dd1 = gRandom->Poisson(mu_bkg);
    double x1 = dd1 * log(musb/mu_bkg);
    if (x1<=xobs) clb++;

    // Confidence level for the signal+background hypothesis (CL_{s+b})
    int dd2 = gRandom->Poisson(musb);
    double x2 = dd2 * log(musb/mu_bkg);
    if (x2<=xobs) clsb++;
  }

  // Compute the modified frequentist confidence level CL_s
  double cls = clb ? (1.0*clsb)/(1.0*clb) : 0.0;
  return cls;
}

// The case of zero uncertainty on the expected background
Double_t CLs_zero_ebkg(int Ndata, double mu_signal, double mu_bkg) {
  int mToys = 500000;
  int clb = 0, clsb = 0;
  double musb = mu_signal + mu_bkg;

  // Test statistic: (log of) the likelihood ratio
  double xobs = Ndata * log(musb/mu_bkg);

  for (int i=0; i<mToys; i++) {

    // Confidence level for the background hypothesis (CL_b)
    int dd1 = gRandom->Poisson(mu_bkg);
    double x1 = dd1 * log(musb/mu_bkg);
    if (x1<=xobs) clb++;

    // Confidence level for the signal+background hypothesis (CL_{s+b})
    int  dd2 = gRandom->Poisson(musb);
    double x2 = dd2 * log(musb/mu_bkg);
    if (x2<=xobs) clsb++;
  }

  // Compute the modified frequentist confidence level CL_s
  double cls = clb ? (1.0*clsb)/(1.0*clb) : 0.0;
  return cls;
}

double CLs_general(int Ndata, double mu_signal, int Nbackgrounds, int *bkg, double *sf) {

  int mToys = 500000;
  int clb = 0, clsb = 0;

  // Posterior pdfs for the backgrounds (not normalized)
  TString Formula[10];
  double  MaxLimit[10];
  TF1*    PosteriorPdf[10];
  for (int i=0; i<Nbackgrounds; i++) {
    Formula[i]      = (bkg[i]>0) ? Form("pow(x,%d)*exp(-x)", bkg[i]) : "exp(-x)";
    MaxLimit[i]     = (bkg[i]>0) ? 10.0*bkg[i] : 10.0;
    PosteriorPdf[i] = new TF1(Form("fPDF%d", i), Formula[i].Data(), 0.0, MaxLimit[i]);
  }

  for (int i=0; i<mToys; i++) {

    // Generation of the number of background events
    double mu_bkg = 0.0;
    for (int j=0; j<Nbackgrounds; j++) mu_bkg += (sf[j]*PosteriorPdf[j]->GetRandom());
    double musb = mu_signal + mu_bkg;

    // The test statistic: the likelihood ratio
    double xobs = Ndata * log(musb/mu_bkg);

    // Confidence level for the background hypothesis (CL_b)
    double dd1 = gRandom->Poisson(mu_bkg);
    double x1 = dd1 * log(musb/mu_bkg);
    if (x1<=xobs) clb++;

    // Confidence level for the signal+background hypothesis (CL_{s+b})
    double dd2 = gRandom->Poisson(musb);
    double x2 = dd2 * log(musb/mu_bkg);
    if (x2<=xobs) clsb++;
  }

  // Compute the modified frequentist confidence level CL_s
  double cls = clb ? (1.0*clsb)/(1.0*clb) : 0.0;
  return cls;
}

/////////////////////////////////////////////////////////

void error() {
  std::cout << "Usage:" << std::endl;
  std::cout << "  CLs_limit gaus    <Nobserved> <Nexpected> <dNexpected> [CL=90%], or" << std::endl;
  std::cout << "  CLs_limit general <Nobserved> <NBackgroundSources> <Background1> <ScaleFactor1> ... [CL=90%]" << std::endl;
  std::cout << "  If Nobserved<0 is gaus mode, the expected limit and 1,2-sigma bands are computed" << std::endl;
  std::cout << std::endl;
  exit(0);
}
