//////////////////////////////////////////////////////////
// Computation of CLs upper limits at 90% CL.
// Evgueni Goudzovski (goudzovs@cern.ch), 8 July 2019.
// The method is described in the internal note NA62-19-04.

#include <iostream>
#include "TMath.h"
#include "TRandom.h"

using namespace std;

double UpperLimit(int NObserved, double Nexpected, double dNExpected, double Level);
double CLs(int Ndata, double mu_signal, double bkg, double ebkg);
double CLs_zero_ebkg(int Ndata, double mu_signal, double bkg);

int main(int argc, char** argv) {
  if (argc<4) {
    std::cout << "Usage: CLs_limit <Nobserved> <Nexpected> <dNexpected> [ConfidenceLevel=90%]" << std::endl;
    std::cout << "       If Nobserved<0, the expected limit and 1,2-sigma bands are computed" <<  std::endl;
    exit(0);
  }
  int    NObserved  = atof(argv[1]);
  double Nexpected  = atof(argv[2]);
  double dNExpected = atof(argv[3]);

  double Level = 0.1;
  if (argc>=5) Level = 1.0-atof(argv[4]);

  gRandom->SetSeed(1); // to ensure reproducibility

  if (NObserved>=0.0) { // compute the upper limit
    double UL = UpperLimit(NObserved, Nexpected, dNExpected, Level);
    printf("%3.1f\n", UL);
  }
  else { // compute the expected upper limit in background-only hypothesis
    double upper_limits[50000];
    for (int i=0; i<50000; i++) upper_limits[i] = -1.0;
    vector<double>Trials;
    for (int iTrial=0; iTrial<1000; iTrial++) { // multiple trials
      double Lambda = gRandom->Gaus(Nexpected, dNExpected);
      int    Nobs   = gRandom->Poisson(Lambda);
      double ul     = upper_limits[Nobs];
      if (ul<0.0) {
	ul = UpperLimit(1.0*Nobs, Nexpected, dNExpected, Level);
	upper_limits[Nobs] = ul;
	//cout <<"@@ " << iTrial <<" " << Nobs <<" " << ul << endl;
      }
      Trials.push_back(ul);
    }
    std::sort(Trials.begin(), Trials.end());
    double ExpBandLow2  = Trials[ 22]; // -2 sigma
    double ExpBandLow1  = Trials[158]; // -1 sigma
    double ExpLimit     = 0.5*(Trials[499]+Trials[500]);
    double ExpBandHigh1 = Trials[841]; // +1 sigma
    double ExpBandHigh2 = Trials[977]; // +2 sigma

    printf("Expected -2sigma, -1sigma, mean, +1sigma, +2sigma\n%3.1f %3.1f %3.1f %3.1f %3.1f\n",
	   ExpBandLow2, ExpBandLow1, ExpLimit, ExpBandHigh1, ExpBandHigh2);
  }
  return 0;
}

double UpperLimit(int NObserved, double Nexpected, double dNExpected, double Level) {

  // Determine the initial range where to look for the solution
  double mu_min = 0;
  double mu_max = (NObserved) ? 1.0*NObserved : 1.0;
  while (CLs(NObserved, mu_max, Nexpected, dNExpected)>Level) mu_max *= 2.0;

  // Find the solution within this range
  double mu = 0.5*(mu_min+mu_max);
  while (mu_max-mu_min > 0.1 && mu_max-mu_min > 1e-4*mu) { // the precision required
    double CLs_mid = CLs(NObserved, mu, Nexpected, dNExpected);
    /*
    cout <<" ... solving: " << mu_min <<" " << mu_max << " " << (mu_max-mu_min)/mu << " --> " <<
      CLs(NObserved, mu_min, Nexpected, dNExpected) <<" " << CLs_mid << " " <<
      CLs(NObserved, mu_max, Nexpected, dNExpected) << endl;
    */
    if (CLs_mid<Level) mu_max = mu;
    else               mu_min = mu;
    mu = 0.5*(mu_min+mu_max);
  }
  return mu;
}

double CLs(int Ndata, double mu_signal, double bkg, double ebkg) {
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
