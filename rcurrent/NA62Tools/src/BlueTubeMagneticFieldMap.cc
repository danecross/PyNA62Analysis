// ------------------------------------------------------------------
// History:
//
// Created by Anne Chappuis (anne.chappuis9@gmail.com) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2015-06-18
//
// ------------------------------------------------------------------

/// \class BlueTubeMagneticFieldMap
/// \Brief
/// Magnetic field map in the decay volume
/// \EndBrief
/// \Detailed
/// Magnetic field map in the decay volume (LAV Responsibility Region 0, 104.458m < z < 183.311m).
/// The parameterization is described in the note NA62-15-06, and
/// is based on the measurements reported in note NA62-14-05.
/// The measurements were performed in a number of (x,y) points in 40 z-planes.
/// The parameterized field components have a polynomial (up to quadratic) (x,y)-dependence
/// in each z plane, and a linear z dependence between any two consecutive z planes.
/// The field components are continuous throughout the volume. However their first spatial
/// derivatives (dB/dz) are in general discontinuous in the z planes where the
/// measurements were performed.
/// \author Anne Chappuis (anne.chappuis9@gmail.com), Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "BlueTubeMagneticFieldMap.hh"
#include "TVector3.h"
#include <iostream>

using namespace std;

///////////////////////////////////////////////
// Initialization of the field parameterization

BlueTubeMagneticFieldMap::BlueTubeMagneticFieldMap(Double_t Zmin, Double_t Zmax) :
  fNPlanes(40), fZmin(Zmin), fZmax(Zmax) {

  // z coordinates of the planes where the measurements were performed
  const double Z[40] =
    {104458,106795,107795,108795,109795,110795,111795,112795,113795,114795,
     115795,117395,118445,119395,120195,122706,125206,127706,130315,132815,
     135315,137925,140425,142925,145535,148035,150735,153145,154395,155645,
     156895,158145,159660,161460,163410,168455,170335,175865,177845,181175};

  // In each z plane, Bx(x,y) = p0 + p1*x + p2*y + p3*x^2 + p4*y^2 + p5*x*y.
  // Below are the values of the six parameters in each z plane.
  // Units: p0 [mkT]; p1, p2 [mkT/cm]; p3, p4, p5 [mkT/cm^2]

  const double Bx_param[6][40] =
    {{-68.6863,-39.5778,-16.9,4.68889,-14.0444,-28.3889,-19.2333,-10.1667,-8.65556,5.47778,
      17.1556,41.8667,89.0222,82.7222,65.7556,6.60767,-8.54124,1.65884,7.66039,1.18533,
      -1.42753,4.96917,24.1577,17.3425,15.691,32.6015,17.2901,-6.54695,-27.457,-25.3719,
      -15.5043,-21.206,-8.84444,-9.65,-12.1333,4.17647,3.85294,12.9824,17.1,4.08904}, 

     {-0.220833,-0.220833,-0.220833,-0.220833,-0.220833,-0.220833,-0.220833,-0.220833,-0.220833,-0.220833,
      -0.220833,-0.220833,-0.220833,-0.220833,-0.220833,-0.281667,-0.114713,0.0362069,-0.172701,-0.14592,
      -0.048908,-0.0475287,-0.321322,-0.253276,-0.126092,-0.226494,-0.0725287,0.0277586,-0.136149,-0.188506,
      -0.211264,-0.055977,-0.0127997,-0.00777613,-0.00233387,0.0117462,0.0169931,0.0324268,0.0379527,0.0472464},
     
     {-0.221222,-0.221222,-0.221222,-0.221222,-0.221222,-0.221222,-0.221222,-0.221222,-0.221222,-0.221222,
      -0.221222,-0.221222,-0.221222,-0.221222,-0.221222,-0.284425,-0.306782,0.0132759,-0.0967816,0.108218,
      0.203736,0.0647126,0.235,0.157011,0.0997701,0.369023,0.179828,0.190862,0.0936207,-0.0744828,
      -0.11523,0.0990805,0,0,0,0,0,0,0,0},
     
     {0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,-0.00698196,-0.00463623,-0.00115794,-0.0017114,0.00327568,
      0.00321761,0.00537389,0.00932334,0.00612653,0.00514224,0.00752049,0.0056158,0.00206719,0.000444964,-0.00504569,
      -0.00610117,-0.00174315,0,0,0,0,0,0,0,0},

     {0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0.000824594,0.0035697,0.0013327,0.000602952,-0.00130144,
      -0.00194775,-0.00165809,-0.00912439,-0.00398657,-0.0027938,-0.00598185,-0.00170323,-0.00212782,-0.00039747,0.00303387,
      0.00430679,0.00174359,0,0,0,0,0,0,0,0},

     {0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,-0.00105616,-0.00148323,-0.00111427,0.00050117,0.000487129,
      -0.00148011,-0.00160959,-0.00131552,-0.00116108,0.000333463,-0.000597894,0.00266498,0.00252418,0.000945398,-0.000143526,
      0.00182683,-0.000635725,0,0,0,0,0,0,0,0}};

  // In each z plane, By(x,y) = p0 + p1*x + p2*y + p3*x*y.
  // Below are the values of the four parameters in each z plane.
  // Units: p0 [mkT]; p1, p2 [mkT/cm]; p3 [mkT/cm^2]

  const double By_param[4][40] =
    {{-24.7197,-21.5778,-25.9111,-35.2667,-48.0444,-50.5222,-38.3889,-32.2111,-30.9444,-23.2889,
      -21.4667,-23.9333,-29.4111,-14.7111,-13.1778,-7.15,-16.2722,-23.4611,-17.0722,-15.8722,
      -19.9056,-12.7,-29.6167,-5.88333,4.04444,-2.78333,-10.1389,-3.48889,-10.3667,-12.2444,
      -12.8778,-22.2833,-21.1389,-15.1444,-5.26667,-3.18235,-5.85294,-4.44706,-8.35882,-8.50616},

     {-0.295667,-0.295667,-0.295667,-0.295667,-0.295667,-0.295667,-0.295667,-0.295667,-0.295667,-0.295667,
      -0.295667,-0.295667,-0.295667,-0.295667,-0.295667,-0.456379,-0.385,0.0108621,-0.194483,0.228736,
      0.276839,0.176322,0.440977,0.303851,0.202701,0.559598,0.347701,0.208621,0.132126,-0.17069,
      -0.393966,0.110115,0,0,0,0,0,0,0,0},

     {0.231389,0.231389,0.231389,0.231389,0.231389,0.231389,0.231389,0.231389,0.231389,0.231389,
      0.231389,0.231389,0.231389,0.231389,0.231389,-0.116437,0.0593678,0.117299,0.0767816,0.114368,
      0.169023,0.14023,0.409425,0.126897,0.0732759,0.299253,0.0877012,-0.0182184,0.0152874,0.139598,
      0.103966,0.235805,-0.00803161,-0.00803161,-0.00803161,-0.00803161,-0.00803161,-0.00803161,-0.00803161,-0.00803161},

     {0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0.00680148,0.00770398,0.00228939,0.00413573,-0.003578,
      -0.00446568,-0.00629173,-0.0166193,-0.00963612,-0.00630694,-0.0149353,-0.0075117,-0.0048631,-0.00220827,0.00639431,
      0.0123966,0.00421724,0,0,0,0,0,0,0,0}};

  // In each z plane, Bz(x,y) = p0 + p1*x + p2*y
  // Below are the values of p0 (different in each plane), p1 and p2 (fixed)
  // Units: p0 [mkT]; p1, p2 [mkT/cm]

  const double Bz_param0[40] =
    {13.9998,30.6444,38.3,45.2778,38.3778,17.3333,9.2,24.8889,41.8889,41.0778,
     50.5333,27.6889,-21.1444,-32.7556,-22.5,40.108,39.5567,19.1307,5.82818,20.9803,
     26.9019,7.67615,2.30426,1.5026,12.6371,7.69588,5.31323,25.3994,29.5364,29.4029,
     43.5186,36.8864,-15.4485,-20.3851,-19.3098,5.59546,-3.6922,-16.0463,-2.15629,-7.71665};

  const double Bz_param[2] = {0.022125, 0.011451}; // p1=dBz/dx, p2=dBz/dy [mkT/cm]

  //////////////////////////////////////////////////////////////////
  // Copy the parameter values into the private members of the class
  for (Int_t i=0; i<fNPlanes; i++) {
    fZ[i] = Z[i];
    fBz_param0[i] = Bz_param0[i];
    for (Int_t j=0; j<6; j++) fBx_param[j][i] = Bx_param[j][i];
    for (Int_t j=0; j<4; j++) fBy_param[j][i] = By_param[j][i];  
  }
  for (Int_t i=0; i<2; i++) {
    fBz_param[i] = Bz_param[i];
  }

  // Test: uniform field
  /*
  for (Int_t i=0; i<fNPlanes; i++) {
    fBx_param[0][i] = fBy_param[0][i] = 30.0; // = 0.3 G
    fBz_param0[i] = 0.0;
    for (Int_t j=1; j<6; j++) fBx_param[j][i] = 0.0;
    for (Int_t j=1; j<4; j++) fBy_param[j][i] = 0.0;
  }
  fBz_param[0] = fBz_param[1] = 0.0;
  */

  ////////////////////////////////////////////////////////////////////////
  // Pre-compute the gradients of the parameters (dpi/dz) in the z ranges.
  // This is required for the interpolation of the field.

  for (Int_t i=0; i<fNPlanes; i++) {
    for (Int_t j=0; j<6; j++) {
      fBx_param_gradient[j][i] = (i<fNPlanes-1) ?
	(fBx_param[j][i+1]-fBx_param[j][i])/(fZ[i+1]-fZ[i]) : 0.0;
    }
    for (Int_t j=0; j<4; j++) {
      fBy_param_gradient[j][i] = (i<fNPlanes-1) ?
	(fBy_param[j][i+1]-fBy_param[j][i])/(fZ[i+1]-fZ[i]) : 0.0;
    }
    fBz_param0_gradient[i] = (i<fNPlanes-1) ?
      (fBz_param0[i+1]-fBz_param0[i])/(fZ[i+1]-fZ[i]) : 0.0;
  }

  //////////////////////////////////////////////////////////////////
  // Pre-compute the integrals of the parameters over the z ranges.
  // This is required by BlueTubeFieldTracker in NA62Analysis/Tools.

  for (Int_t i=0; i<fNPlanes; i++) {
    for (Int_t j=0; j<6; j++) {
      fBx_param_integral[j][i] = (i<fNPlanes-1) ?
	0.5*(fBx_param[j][i]+fBx_param[j][i+1])*(fZ[i+1]-fZ[i]) : 0.0;
    }
    for (Int_t j=0; j<4; j++) {
      fBy_param_integral[j][i] = (i<fNPlanes-1) ?
	0.5*(fBy_param[j][i]+fBy_param[j][i+1])*(fZ[i+1]-fZ[i]) : 0.0;
    }
    fBz_param0_integral[i] = (i<fNPlanes-1) ?
      (fBz_param0[i]+fBz_param0[i+1])*(fZ[i+1]-fZ[i]) : 0.0;
  }
}

////////////////////////////////////////////////////////////////
// Evaluation of the field intensity. Input: [mm], output: [mkT]

TVector3 BlueTubeMagneticFieldMap::GetField(Double_t xx, Double_t yy, Double_t zz) {

  if (zz<fZmin || zz>fZmax) return TVector3(0,0,0);

  // Convert input units [mm] into internal units.
  // Transverse scale (x,y) unit: [cm], longitudinal scale (z) unit: [mm].
  double x = 0.1*xx;
  double y = 0.1*yy;
  double z = zz;

  // Simulate constant fields outside the forty z planes
  if (z<fZ[0])          z = fZ[0];
  if (z>fZ[fNPlanes-1]) z = fZ[fNPlanes-1];

  // Find the z range for this point
  Int_t iPlane2 = 1;
  while (iPlane2<fNPlanes-1 && fZ[iPlane2]<=z) iPlane2++;
  Int_t iPlane1 = iPlane2 - 1;

  // Bx computation [mkT]
  TVector3 B;
  double par[6];
  for (Int_t i=0; i<6; i++) {
    par[i] = fBx_param[i][iPlane1] + fBx_param_gradient[i][iPlane1]*(z-fZ[iPlane1]);
  }
  B[0] = par[0] + par[1]*x + par[2]*y + par[3]*x*x + par[4]*y*y + par[5]*x*y;

  // By computation [mkT]
  for (Int_t i=0; i<4; i++) {
    par[i] = fBy_param[i][iPlane1] + fBy_param_gradient[i][iPlane1]*(z-fZ[iPlane1]);
  }
  B[1] = par[0] + par[1]*x + par[2]*y + par[3]*x*y;

  // Bz computation [mkT]
  par[0] = fBz_param0[iPlane1] + fBz_param0_gradient[iPlane1]*(z-fZ[iPlane1]);
  B[2]   = par[0] + fBz_param[0]*x + fBz_param[1]*y;

  return B;
}

/////////////////////////////////////////////
// Print the field intensity at a given point

void BlueTubeMagneticFieldMap::PrintFieldValue(Double_t x, Double_t y, Double_t z) {
  TVector3 B = GetField(x,y,z);
  cout<<"Bx By Bz [mkT] = "<<B.X()<<" "<<B.Y()<<" "<<B.Z()<<endl;
}

void BlueTubeMagneticFieldMap::PrintFieldValue(TVector3 Point) {
  TVector3 B = GetField(Point[0], Point[1], Point[2]);
  cout<<"Bx By Bz [mkT] = "<<B.X()<<" "<<B.Y()<<" "<<B.Z()<<endl;
}

/////////////////////////////////////////
// Print the pre-computed field integrals

void BlueTubeMagneticFieldMap::PrintFieldIntegrals() {
  cout << "Integrals of Bx & By field parameters:" << endl;
  for (Int_t i=0; i<fNPlanes-1; i++) {
    cout <<
      fBx_param_integral[0][i] << " " << fBx_param_integral[1][i] <<" "<<
      fBx_param_integral[2][i] << " " << fBx_param_integral[3][i] <<" "<<
      fBx_param_integral[4][i] << " " << fBx_param_integral[5][i] <<" | "<<
      fBy_param_integral[0][i] << " " << fBy_param_integral[1][i] <<" "<<
      fBy_param_integral[2][i] << " " << fBy_param_integral[3][i] << endl; 
  }

  Double_t Sx = 0.0, Sy = 0.0, Sz = 0.0;
  for (Int_t i=0; i<fNPlanes-1; i++) {
    Sx += fBx_param_integral[0][i];
    Sy += fBy_param_integral[0][i];
    Sz += fBz_param0_integral[i];
  }
  // LAV RR0 spans beyond the last plane
  Sx += (fZmax-fZ[fNPlanes-1])*fBx_param[0][fNPlanes-1];
  Sy += (fZmax-fZ[fNPlanes-1])*fBy_param[0][fNPlanes-1];
  Sz += (fZmax-fZ[fNPlanes-1])*fBz_param0  [fNPlanes-1];
  cout << "Field integrals for x=y=0: Bx*dz, By*dz, Bz*dz [Tm] = " << Sx/1e9<<" "<<Sy/1e9<<" "<<Sz/1e9<<endl;
}
