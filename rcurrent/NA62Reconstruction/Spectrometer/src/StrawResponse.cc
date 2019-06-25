#include "Riostream.h"
#include "StrawResponse.hh"
#include "SpectrometerParameters.hh"
#include <math.h>

StrawResponse::StrawResponse(TRandom3* random) : fRandom(random){
// Set constants for the RT-scaling distribution 
// (see http://na62.web.cern.ch/NA62/Documents/NotesDoc/na62_10_01.pdf )
  fTmin=0.006645;
  fTmax=0.122037;

  fD1=3533.856;
  fD2=0.4066525;
  fD3=-0.01527707;
  fD4=3533.856;
  fD5=0.4066525;
  fD6=0.7931013;
  fD7=609.097;
  fD8=0.9374257;

#ifdef NODELTA
  fD9=0.0;
  fD10=0.01601245;
#else
  fD9=2.426933;
  fD10=0.01601245;
#endif

  fP1=1041.134;
  fP2=-0.3735402;
  fP3=0.1577471;
  fP4=1041.134;
  fP5=-0.3735402;
  fP6=0.9745981;
  fP7=3172.85;
  fP8=-0.8103979;

  fAeff = 51.59;
  fBeff = 4.874;

  int ihx=200;    // number of x bins
  float xmin=-90.0; // min. scaling variable
  float xmax=40.0;  // max. scaling variable

  //  Fill in the histogram for the scaling 'time' sampling
  fHt = new TH1F("t","t",ihx,xmin,xmax);
  float xstep=(xmax-xmin)/ihx;
  float xh = xmin+xstep/2.0;
  for(int j=0;j<ihx;j++){
    fHt->SetBinContent(j+1,RTS(xh));
    xh+=xstep;
  }
}

StrawResponse::~StrawResponse(){}

float StrawResponse::RTS(float xh)
{
  float y;
   if(xh<0.0){
    y=fD1*exp(-0.5*pow((xh+fD2)/fD3,2))+fD4*exp(-0.5*pow((xh+fD5)/fD6,2))+fD7*exp(fD8*xh)+fD9*exp(fD10*xh);  
   }else{
    y=fP1*exp(-0.5*pow((xh+fP2)/fP3,2))+fP4*exp(-0.5*pow((xh+fP5)/fP6,2))+fP7*exp(fP8*xh);
   }
   return y;
}

Double_t StrawResponse::TimeSimulated(Double_t rmm)
{
   Double_t htm=0.0;    // Position of maximum in X distribution
   Double_t hrms=0.0;   // Width of X distribution near the peak
   Double_t r=rmm*0.1;  // Distance to wire (in cm)
   int npl=9;           // Order of the polinome

// Constants for the polinomes to calculate htm and hrms as a functions of the 
// distance to wire (see http://na62.web.cern.ch/NA62/Documents/NotesDoc/na62_10_01.pdf ).
  float pl1[10] = {
    0.01662601274,
    0.0135179358,
    1.43621981,
   -8.72512436,
    28.3498421,
   -36.0145798,
    0.00710778404,
    43.8631783,
   -28.528614,
   -2.89884782
  };
  
  float pl2[10]= {
    0.00237651938,
   -0.0166780874,
    0.214162737,
   -1.9370842,
    9.57516575,
   -19.9103031,
   -9.28806496,
    115.895653,
   -187.102692,
    98.0707321
  };

  Double_t q=1.0;

  for(int i=0;i<=npl;i++)
  { 
    htm  +=  pl1[i]*q;
    hrms +=  pl2[i]*q;
    q*=r;
  }

  Double_t time = -1.0;
  gRandom = fRandom; // set gRandom to fRandom to ensure reproducibility of GetRandom()
  while(time<0.0 || time>0.37) time = (fHt->GetRandom())*(2.0*hrms)+htm;

  return time;
}


Double_t StrawResponse::RadiusReconstructed(Double_t tt)
{
// Constants for polinomes to calculate the radius from time
// as a most probable value (R distribution maximum).

  Double_t pol[10];
  pol[0] =      1.40694; 
  pol[1] =     -288.673; 
  pol[2] =      22372.4; 
  pol[3] =      -724749; 
  pol[4] =  1.38739e+07; 
  pol[5] = -1.69746e+08; 
  pol[6] =  1.34466e+09; 
  pol[7] = -6.68268e+09; 
  pol[8] =  1.89464e+10; 
  pol[9] = -2.33756e+10;

  Double_t f1 = 0;
  Double_t d = 1;

  if (tt<0.02)
  {
    Double_t val = 0;
    for (Int_t j=0; j<10; j++)
    {
      val += pol[j]*d;
      d *= 0.02;
    }
    Double_t b = val/(0.02-0.0155);
    Double_t a = -0.0155*b;
    f1 = a+b*tt;
    if (f1<0) f1 = 0;
  }

  if (tt>=0.02 && tt<=0.09)
  {
    for (Int_t j=0; j<10; j++) 
    {
      f1 += pol[j]*d;
      d *= tt;
    }
  }

  if (tt>0.09 && tt<=0.115)
  {
    Double_t val = 0;
    for (Int_t j=0; j<10; j++)
    {
      val += pol[j]*d;
      d *= 0.09;
    }
    Double_t b = (4.26-val)/(0.115-0.09);
    Double_t a = 4.26-0.115*b;
    f1 = a+b*tt;
  }

  if (tt>0.115 && tt<=0.15)
  {
    Double_t b = (4.82767-4.26)/(0.15-0.115);
    Double_t a = 4.82767-0.15*b;
    f1 = a+b*tt;
  }

  if (tt>0.15)
  {
    f1 = 4.82767 ;
  }

  Double_t r = f1-0.017;

  return r; 
}

int StrawResponse::StrawIneff(Double_t r)     // r - distance to the straw center in mm.
{
  if( (fRandom->Uniform() < exp(fAeff*(r - fBeff))) || (fRandom->Uniform() > SMOOTH_EFF) ) return 1;
  return 0;
}
