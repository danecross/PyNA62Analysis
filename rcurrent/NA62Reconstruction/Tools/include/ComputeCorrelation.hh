#ifndef ComputeCorrelation_H
#define ComputeCorrelation_H
#include <omp.h>
#include "TGraph.h"
#include "TAxis.h"
#include <vector>

using namespace std;

TGraph* ComputeCorrelation (vector <Long64_t> Early, vector <Long64_t> Late, vector <Double_t> Delta, Int_t Width, Bool_t Verbose = kFALSE, Int_t num_threads = 8) {
  //Int_t num_threads=omp_get_max_threads();
  //if (Verbose) cout<<"[Verbose] Maximum number of threads: "<<num_threads<<endl;
  //num_threads=NTHREADS;
  if(Delta.size()==0) return 0;

  Double_t **Corr = new Double_t*[num_threads];
  for(Int_t i = 0; i <num_threads; ++i) {
    Corr[i] = new Double_t[Delta.size()];
  }

  for(UInt_t j=0;j<Delta.size();j++){
    for(Int_t i=0;i<num_threads;i++){ Corr[i][j] = 0.; }}

#pragma omp parallel num_threads(num_threads)
  {
    Int_t ID = omp_get_thread_num();
    Int_t nthreads;
    if (Verbose) if(ID==2) cout<<"[info] Number of threads used "<<omp_get_num_threads()<<endl;
    nthreads=omp_get_num_threads();
    Double_t diff;

    for (UInt_t d=ID;d<Delta.size();d=d+nthreads) {
      Corr[ID][d]=0;
      Int_t jzero = 0;
      for (UInt_t i=0;i<Early.size();i++) {
	for (UInt_t j=jzero;j<Late.size();j++) {

	  diff = 1.0 * (Late[j] - (Early[i] + Delta[d]));

	  if (fabs(diff) < Width) {
	    Corr[ID][d]++;
	    jzero = j + 1;
	    break;
	  } else {
	    if (diff > 0) {
	      jzero = j;
	      break;
	    }
	  }
	}//end j
      }//end i
      if(!Verbose) if(ID==0)if (d%1000 == 0) cout <<  "." << flush;
    }//end delta
  }//end pragma

  Double_t *CorrTot=NULL;


  CorrTot=new Double_t[Delta.size()];
  for(UInt_t j=0;j<Delta.size();j++){
    CorrTot[j]=0;
    for(Int_t i=0;i<num_threads;i++){
      if (Corr[i][j]<0 || TMath::IsNaN(Corr[i][j])) { cout <<"\n[error] "<<endl<<" i "<<i<<" j "<<j<<" "<<Corr[i][j]<<endl; continue; }
      CorrTot[j]+=Corr[i][j];
    }
  }
  Double_t Max =0.0;
  for(UInt_t i = 0; i < Delta.size(); i ++) {
    Max = CorrTot[i]>Max?CorrTot[i]:Max;
  }
  for(UInt_t i = 0; i < Delta.size(); i ++) {
    if (Max > 0) { 
      CorrTot[i] /= Max;
    } else {
      CorrTot[i] = 0;
    }

    Delta[i] *= 0.1;
  }

  TGraph *gCorr = new TGraph(Delta.size(), Delta.data(), CorrTot );
  gCorr->GetXaxis()->SetTitle("Time [ns]");
  gCorr->GetYaxis()->SetTitle("Correlation [a.u.]");
  return gCorr;
}

#endif
