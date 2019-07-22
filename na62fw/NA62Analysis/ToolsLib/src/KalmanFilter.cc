#include "TROOT.h"
#include <iostream>
#include "KalmanFilter.hh"

/* Constructor: */

KalmanFilter::KalmanFilter(bool _verbose) {
  verbose = _verbose;
}

void KalmanFilter::setVerbose(bool isVerbose){
  verbose =isVerbose;
}

/* Set Fixed Matrix */
void KalmanFilter::setMatrices( ROOT::Math::SMatrix<double,n,n> _F, ROOT::Math::SMatrix<double,m,n> _H, ROOT::Math::SMatrix<double,n,n> _Q, ROOT::Math::SMatrix<double,m,m> _V ){

  F.push_back(_F);
  H.push_back(_H);
  Q = _Q;
  V.push_back(_V);

  I = ROOT::Math::SMatrixIdentity();
}


void KalmanFilter::setH( ROOT::Math::SMatrix<double,m,n> _H){
  H.push_back(_H);
}

void KalmanFilter::setV( ROOT::Math::SMatrix<double,m,m> _V){
  V.push_back(_V);
}


void KalmanFilter::setF( ROOT::Math::SMatrix<double,n,n> _F){
  F.push_back(_F);
}

void KalmanFilter::setQ( ROOT::Math::SMatrix<double,n,n> _Q){
  Q = _Q;
  if(verbose){
    cout<<"Q matrix"<<endl;
    cout<<Q<<endl;
  }
}


/* Set Initial Matrix */
void KalmanFilter::setInitial(ROOT::Math::SVector<double,n > _X0, ROOT::Math::SMatrix<double,n,n> _C0 ){

  X.push_back(_X0);
  C0 = _C0;
}

/* Do prediction */
void KalmanFilter::predict(void){

  XPred.push_back( F.back() * X.back() ) ;
  CPred.push_back( F.back()*C0*ROOT::Math::Transpose(F.back()) + Q  );

  //Alternative solutions
  //CPred.push_back( ROOT::Math::Similarity(F.back(),C0) +Q);
  //CPred.push_back( F.back()*(C0*ROOT::Math::Transpose(F.back()) + Q ) );


  if(verbose){
    cout<<"Kalman Filter Step: "<<XPred.size()<<endl<<endl;
    cout<<"\n### PREDICTION ## "<<endl;
    cout<<"\nF matrix:\n"<<endl;
    cout<<F.back()<<endl;
    cout<<"\nPredicted coovariance matrix:\n"<<endl;
    cout<<CPred.back()<<endl;
    cout<<"\nPrediction of state vector:\n"<<endl;
    cout<<XPred.back()<<endl;
  }
}


void KalmanFilter::print(){

  cout<<"##Prediction##\n"<<endl;
  cout<<"\nTransport matrix:\n"<<endl;
  cout<<F.back()<<endl;
  cout<<"\nPrediction of state vector:\n"<<endl;
  cout<<XPred.back()<<endl;
  cout<<"\nPredicted coovariance matrix:\n"<<endl;
  cout<<CPred.back()<<endl;
  cout<<"\nGain matrix:\n"<<endl;
  cout<<K<<endl;
  
  //to be sure filter step has ran
  if(CFilt.size()==CPred.size()){
    cout<<"##Filter##\n"<<endl;
    cout<<"\nFiltered state vector\n"<<endl;
    cout<<X.back()<<endl;
    // cout<<"\nR\n"<<endl;
    // cout<<R.back()<<endl;
    cout<<"\nResidual\n"<<endl;
    cout<<rFilt<<endl;
    cout<<"\nFiltered Covariance matrix\n"<<endl;
    cout<<CFilt.back()<<endl;
    cout<<"\nChi2\n"<<chi2filt.back()<<endl; 
  }
}

/* Correct the prediction, using mesaurement
 * Z: measurement vector
 */
void KalmanFilter::filter ( ROOT::Math::SVector<double,m > Z ) {

  if(verbose){
    cout<<"\n### FILTER ###"<<endl;
    cout<<"\nMeausurements are\n"<<endl;
    cout<<Z<<endl;
  } 

  mV.push_back(Z);

  int fail;
  ROOT::Math::SMatrix<double,2,2> CHTInv = ( H.back()*(CPred.back()*ROOT::Math::Transpose(H.back()))+V.back() );
  CHTInv = CHTInv.Inverse(fail);
  K = (CPred.back()*ROOT::Math::Transpose(H.back())) * CHTInv;

  X.push_back(XPred.back()+ (K*(Z-H.back()*XPred.back())));

  CFilt.push_back((I - K * H.back()) * CPred.back());

  C0 = CFilt.back();
  
  //residuals
  rFilt = Z-H.back()*X.back();

  if(verbose){
    cout<<"\nK matrix:\n"<<endl;
    cout<<K<<endl;
    cout<<"\nResidual\n"<<endl;
    cout<<rFilt<<endl;
    cout<<"\nMeasurements  Covariance matrix\n"<<V.back()<<endl;
}

  ROOT::Math::SMatrix<double,m,m> Im = ROOT::Math::SMatrixIdentity();
  R.push_back( (Im-H.back()*K)*V.back() );

  //ROOT::Math::SMatrix<double,m,m> HCHT  =  ROOT::Math::Similarity(H.back(),CFilt.back());
  //  R.push_back( V.back() - HCHT);

  ROOT::Math::SMatrix<double,m,m> Rinv =  R.back().Inverse(fail);

  chi2filt.push_back(ROOT::Math::Similarity(rFilt,Rinv));


  //covariance matrix
  if(verbose){
    cout<<"\nFiltered state vector:\n"<<endl;
    cout<<X.back()<<endl;
    // cout<<"\nR\n"<<endl;
    // cout<<R.back()<<endl;
    // cout<<"\nR Inverse \n"<<endl;
    // cout<<Rinv<<endl;
    cout<<"\nFiltered Covariance matrix:\n"<<endl;
    cout<<CFilt.back()<<endl;
    cout<<"\nChi2:\n"<<chi2filt.back()<<endl;
  }
}

void KalmanFilter::smoothing () {

  if(verbose)  cout<<"\nSMOOTHING\n"<<endl;

  //reset smooth object if used more than one.
  XSmooth.clear();
  CSmooth.clear();
  chi2Smooth.clear();

  XSmooth.push_back(X.back());
  CSmooth.push_back(CFilt.back());
  chi2Smooth.push_back(chi2filt.back());
  
  for(uint i=2; i<=XPred.size(); i++){
    
    int fail;
    ROOT::Math::SMatrix<double,n,n> CInv = CPred.at(XPred.size()-i+1).Inverse(fail);

    A = CFilt.at(XPred.size()-i)*(ROOT::Math::Transpose(F.at(XPred.size()-i))*CInv);

    XSmooth.push_front( X.at(XPred.size()-i+1) + A*( (XSmooth.front()) - (XPred.at(XPred.size()-i+1)) ));;
   

    // Smoothed coovariance matrix
    CSmooth.push_front( CFilt.at(XPred.size()-i) + A*(CSmooth.front() - CPred.at(XPred.size()-i+1) )*ROOT::Math::Transpose(A) ) ;
     
    rSmooth = mV.at(XPred.size()-i)-H.back()*XSmooth.front();
    
    ROOT::Math::SMatrix<double,m,m> RSmooth = V.at(XPred.size()-i) - H.back()*CSmooth.front()*ROOT::Math::Transpose(H.back()); 

    RSmooth.Inverse(fail);

    chi2Smooth.push_front(ROOT::Math::Dot( rSmooth,(RSmooth*rSmooth)));

    if(verbose){

    cout<<"\nSmoothed X\n"<<endl;
    cout<<XSmooth.front()<<endl;
    cout<<"\nSmoothed Covariance Matrix\n"<<endl;
    cout<<CSmooth.front()<<endl;
    cout<<"\nSmoothed Chi2 "<<chi2Smooth.front()<<endl;

    //  Filter values of same step
    //  cout<<"\nFiltered Covariance Matrix\n"<<endl;
    //  cout<<CFilt.at(XPred.size()-i)<<endl;
    //  cout<<"\nFiltered X\n"<<endl;
    //  cout<<X.at(XPred.size()-i+1)<<endl;
    //  cout<<"\nFiltered Chi2 "<<chi2filt.at(XPred.size()-i)<<endl;

    }
  }
}




