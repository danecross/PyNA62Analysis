#include "StrawResponse.hh"
#include <cmath>

#define NODELTA

StrawResponse::StrawResponse(TRandom3* random) : fRandom(random)
{
  Int_t ihx = 200;              // number of x bins
  Float_t xmin = -90.0;         // min. scaling variable
  Float_t xmax = 40.0;          // max. scaling variable

  //  Fill in the histogram for the scaling 'time' sampling
  // cppcheck-suppress useInitializationList
  fHt = std::make_unique<TH1F>("t","t",ihx,xmin,xmax);
  fHt->SetDirectory(0); // avoid double deletion attempts in case a file is opened
  Float_t xstep=(xmax-xmin)/ihx;
  Float_t xh = xmin+xstep/2.0;
  for (Int_t j = 0; j < ihx; j++) {
    fHt->SetBinContent(j+1, RTS(xh));
    xh += xstep;
  }
}

Float_t StrawResponse::RTS(Float_t xh)
{
  // Constants for the scaling shape approximation drawing
  // (depend on the gas mixture). Described in the note
  // http://na62.web.cern.ch/NA62/Documents/NotesDoc/na62_10_01.pdf
  static constexpr Float_t kd[10] = {
    3533.856,
    0.4066525,
    -0.01527707,
    3533.856,
    0.4066525,
    0.7931013,
    609.097,
    0.9374257,
#ifdef NODELTA
    0.0,
    0.01601245
#else
    2.426933,
    0.01601245,
#endif
  };

  static constexpr Float_t kp[8] = {
    1041.134,
    -0.3735402,
    0.1577471,
    1041.134,
    -0.3735402,
    0.9745981,
    3172.85,
    -0.8103979
  };

  if (xh < 0.0) {
    // cppcheck-suppress constArgument
    return kd[0] * exp(-0.5*pow((xh + kd[1]) / kd[2], 2)) + kd[3] * exp(-0.5*pow((xh + kd[4]) / kd[5], 2)) + kd[6] * exp(kd[7] * xh) + kd[8] * exp(kd[9] * xh);
  } else {
    // cppcheck-suppress constArgument
    return kp[0] * exp(-0.5*pow((xh + kp[1]) / kp[2], 2)) + kp[3] * exp(-0.5*pow((xh + kp[4]) / kp[5], 2)) + kp[6] * exp(kp[7] * xh);
  }
}

Double_t StrawResponse::TimeSimulated(Double_t rmm)
{
  Double_t htm = 0.0;    // Position of maximum in X distribution
  Double_t hrms = 0.0;   // Width of X distribution near the peak
  Double_t r = rmm*0.1;  // Distance to wire (in cm)
  Int_t npl = 9;           // Order of the polinome

// Constants for the polinomes to calculate htm and hrms as a functions of the
// distance to wire (see http://na62.web.cern.ch/NA62/Documents/NotesDoc/na62_10_01.pdf ).
  static constexpr Float_t pl1[10] = {
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

  static Float_t pl2[10] = {
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

  Double_t q = 1.0;

  for(Int_t i = 0; i <= npl; i++) {
    htm +=  pl1[i]*q;
    hrms +=  pl2[i]*q;
    q *= r;
  }

  Double_t time = -1.0;
  gRandom = fRandom; // set gRandom to fRandom to ensure reproducibility of GetRandom()
  while (time<0.0 || time>0.37) time = (fHt->GetRandom())*(2.0*hrms)+htm;

  return time;
}


Double_t StrawResponse::RadiusReconstructed(Double_t tt)
{
// Constants for polinomes to calculate the radius from time
// as a most probable value (R distribution maximum).

  static Double_t pol[10] = {
    1.40694,
    -288.673,
    22372.4,
    -724749,
    1.38739e+07,
    -1.69746e+08,
    1.34466e+09,
    -6.68268e+09,
    1.89464e+10,
    -2.33756e+10};

  Double_t f1 = 0;
  Double_t d = 1;

  if (tt<0.02) {
    Double_t val = 0;
    for (Int_t j=0; j<10; j++) {
      val += pol[j]*d;
      d *= 0.02;
    }
    Double_t b = val/(0.02-0.0155);
    Double_t a = -0.0155*b;
    f1 = a+b*tt;
    if (f1<0) f1 = 0;
  }

  if (tt>=0.02 && tt<=0.09) {
    for (Int_t j=0; j<10; j++) {
      f1 += pol[j]*d;
      d *= tt;
    }
  }

  if (tt>0.09 && tt<=0.115) {
    Double_t val = 0;
    for (Int_t j=0; j<10; j++) {
      val += pol[j]*d;
      d *= 0.09;
    }
    Double_t b = (4.26-val)/(0.115-0.09);
    Double_t a = 4.26-0.115*b;
    f1 = a+b*tt;
  }

  if (tt>0.115 && tt<=0.15) {
    Double_t b = (4.82767-4.26)/(0.15-0.115);
    Double_t a = 4.82767-0.15*b;
    f1 = a+b*tt;
  }

  if (tt>0.15) f1 = 4.82767;

  Double_t r = f1-0.017;

  return r;
}

Bool_t StrawResponse::StrawInefficient(Double_t radius)
{
/// \MemberDescr
/// Samples straw inefficiency based on Garfield simulation results
///
/// \param radius distance to the straw center in mm.
/// \EndMemberDescr

  // Flat inefficiency independent of radius
  static Float_t smoothEff = 0.985;

  // Constants for the radius-dependent inefficiency, obtained from GARFIELD simulation
  static Float_t aeff = 51.59;
  static Float_t beff = 4.874;

  if ((fRandom->Uniform() < exp(aeff*(radius - beff))) || (fRandom->Uniform() > smoothEff))
    return true;
  else
    return false;
}
