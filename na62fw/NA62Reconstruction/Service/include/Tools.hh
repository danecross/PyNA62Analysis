// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// --------------------------------------------------------------

#ifndef Tools_H
#define Tools_H

#include "TArrayD.h"
#include "TMath.h"
#include "Riostream.h"

namespace NA62Tools {

  
  void LinearLeastSquareFit(Double_t *x, Double_t *y, Int_t Nsample, Double_t *sigma, Double_t &a,  Double_t &b,  Double_t &rho,  Double_t &chi2){
    

    // least square method applied to straight line (Y = a + b*X)

    Double_t xmean = TMath::Mean(Nsample,x);
    Double_t ymean = TMath::Mean(Nsample,y);
    Double_t varx = 0.;
    Double_t vary = 0.;
    Double_t covxy = 0.;
  
    for(Int_t i=0; i < Nsample ; i++){
      varx += (x[i] - xmean) * (x[i] - xmean) ; 
      vary += (y[i] - ymean) * (y[i] - ymean) ; 
      covxy += (x[i] - xmean) * (y[i] - ymean) ;
    }
    varx = varx / (Double_t)Nsample;
    vary = vary / (Double_t)Nsample;
    covxy = covxy / (Double_t)Nsample;
    b = covxy / varx;
    a = ymean - b * xmean;
    rho = covxy / (TMath::Sqrt(varx) * TMath::Sqrt(vary));

    chi2 = 0;
    for(Int_t i=0; i < Nsample ; i++){
      chi2 += ((y[i] - a - b*x[i])/sigma[i]) * ((y[i] - a - b*x[i])/sigma[i]) ;
    } 
    
  }



}

#endif
