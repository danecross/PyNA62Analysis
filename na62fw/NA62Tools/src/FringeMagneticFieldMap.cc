///-----------------------------------------------------------
/// History:
///
/// Created by Andrew Sturgess (axs@hep.ph.bham.ac.uk) &
///            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
///
///---------------------------------------------------------

#include "FringeMagneticFieldMap.hh"
#include "MNP33MagneticFieldMap.hh"
#include <iostream>

using namespace std;

///////////////////////////////////////////////
// Initialization of the field parameterization

FringeMagneticFieldMap::FringeMagneticFieldMap
(Double_t Zmin, Double_t Zmax, MNP33MagneticFieldMap *mnp33) :
  // The field region. Unit: [mm]
  fZmin(Zmin), fZmax(Zmax),

  // fSF is the fringe field scale, which multiplies the non-constant terms of the
  // fringe field. It is set to 0.897 to optimally match the MNP33. When scaling the
  // fringe field, fSF is changed in multiples of 0.897. fBT is the blue-tube scale,
  // which multiplies the constant terms of the fringe field to match the blue-tube
  fSF(0.897), fBT(1.0),

  // The following z-values are mostly measurements points in the z-dimension for the
  // fringe field. 193083mm is the start of the MNP33 field, and 192683mm is the z-value 
  // of the buffer region used to swap slowly from fringe to MNP33. The point 192553mm 
  // was also added. The downstream fringe field uses a reflection of these values around 
  // the centre of the MNP33 = 196995mm. There are 16 planes upstream, and measurements
  // were performed in 29 (x,y) grid positions;
  fNPlanes(16), fNGridPos(29) {

  const Double_t Z[16] =
    {183311,184693,185393,186393,187393,189393,190393,190893,191243,191543,
     191843,192143,192443,192553,192683,193083};
  for (Int_t i=0; i<fNPlanes; i++) fZ[i] = Z[i];

  // Below is a list of important z-planes used when evaluating the field. 
  // All are in mm. We also define the buffer width for the three regions.  

  fZStartFringe  = fZ[0];     // First fringe field measurement
  fZStartBuffer1 = fZ[14];    // Start of buffer 1
  fZEndBuffer1   = fZ[15];    // End of Buffer 1, also start of MNP33
  fZStartBuffer2 = 200907.0;  // Start of buffer 2, also end of MNP33
  fZEndBuffer2   = 201307.0;  // End of buffer 2
  fZStartBuffer3 = 210679.0;  // Start of buffer 3, end of FF reflection
  fZEndBuffer3   = 212000.0;  // End of buffer zone 3

  fZReflection   = fZStartBuffer1 + fZEndBuffer2; // Used to reflect fringe field

  fBufferSize1   = fZEndBuffer1 - fZStartBuffer1; // Size of the first buffer
  fBufferSize2   = fZEndBuffer2 - fZStartBuffer2; // Size of the second buffer
  fBufferSize3   = fZEndBuffer3 - fZStartBuffer3; // Size of the third buffer

  // MNP33 map is required to make buffer regions between the fringe field
  fMNP33 = mnp33;
}

////////////////////////////////////////////////////////////////////////////
// Use the MNPP33 object to get the field for a specific x,y at the first
// MNP33 cross-over (buffer region one). Returns B in [mkT].
TVector3 FringeMagneticFieldMap::MNP33FirstBound(Double_t x, Double_t y) {
  return 1e6 * fMNP33->GetField(x, y, fZEndBuffer1);
}

////////////////////////////////////////////////////////////////////////////
// Use the MNP33 object to get the field for a specific x,y at the second
// MNP33 cross-over (buffer region two). Returns B in [mkT].
TVector3 FringeMagneticFieldMap::MNP33SecondBound(Double_t x, Double_t y) {
  return 1e6 * fMNP33->GetField(x, y, fZStartBuffer2);
}

//////////////////////////////////////////////////////////////////////////////
// The ParameterArray function evaluates the values of the eighteen parameters
// (6 each for Bx, By and Bz) for a specific z-value (mm)

void FringeMagneticFieldMap::ParameterArray(Double_t zz) {
  // Define z as (zz - fZStartFringe), so that the functions return to a constant
  // at the blue-tube boundary and matches.

  Double_t z = zz-fZStartFringe;

  //Powers of z used in the z-dependent parameter formulae
  Double_t z2(z*z), z3(z2*z), z4(z2*z2), z5(z2*z3), z6(z3*z3), z7(z4*z3);
  
  // Below, fPar is [6][3] z-dependent array, containing the six parameters that define
  // the B-field for a specific z-plane, for the three field components.
  // The field contains a constant, terms linear in x,y, terms quadratic in x,y and a
  // cross term x*y. The general form of the field is seen below:
  // B(z) = B0i(z) + alpha_x(z)*x + alpha_y(z)*y + alpha_xx(z)*x*x + alpha_yy(z)*y*y
  //        + alpha_xy(z)*x*y;

  /////////////////////////////////////////////
  // The B0i(z) parameter for Bx, By, Byz [mkT]

  fPar[0][0] = fBT*(4.08904) + fSF*((0.0220933)*z + (-1.41954e-5)*z2 + (3.9419e-9)*z3
                                    + (-4.725e-13)*z4 + (2.05177e-17)*z5);

  fPar[0][1] = fBT*(-8.50616) + fSF*((0.4633)*exp(0.001081*(zz-193343))*z + (-0.006294)*z
                                     + (5.499e-6)*z2);

  fPar[0][2] = fBT*(-7.71605) + fSF*( (0.06957)*z + (-3.895e-05)*z2 +  (1.299e-8)*z3
                                      + (-2.483e-12)*z4 + (2.447e-16)*z5 + (-9.313e-21)*z6);

  ///////////////////////////////////////////////////
  // The alpha_x(z) parameter for Bx, By, Bz [mkT/cm]

  fPar[1][0] = fBT*(0.04725) + fSF*((5.703e-5)*z + (-5.966e-8)*z2 + (1.17e-11)*z3 + 
				    (-7.705e-16)*z4);

  fPar[1][1] =  0 + fSF*((-2.023e-5)*z + (2.281e-8)*z2 + (-7.139e-12)*z3
                         + (6.543e-16)*z4 + (-1.353e-20)*z5);

  fPar[1][2] = fBT*(0.02213) + fSF*((-9.185e-05)*z + (5.867e-08)*z2 + (-1.146e-11)*z3
                                    + (7.049e-16)*z4);

  ///////////////////////////////////////////////////
  // The alpha_y(z) parameter for Bx, By, Bz [mkT/cm]

  fPar[2][0] = 0 + fSF*((-9.144e-5)*exp(0.001918*(zz-193343))*z + (1.014e-5)*z +(-4.409e-9)*z2);

  fPar[2][1] = fBT*(-0.008031) + fSF*(((-0.001567)*z + (9.727e-7)*z2 + (-1.746e-10)*z3
                                       + (9.629e-15)*z4)*(0.0001038*z));

  fPar[2][2] = fBT*(0.01145) + fSF*((0.002191)*z + (-3.747e-6)*z2 + (2.454e-9)*z3
                                    + (-7.883e-13)*z4 + (1.328e-16)*z5
                                    + (-1.125e-20)*z6 +(3.795e-25)*z7);

  //////////////////////////////////////////////////////
  // The alpha_xx(z) parameter for Bx, By, Bz [mkT/cm^2]

  fPar[3][0] = 0 + fSF*(-1.042e-7)*z;

  fPar[3][1] = 0 + fSF*((-7.124e-6)*z*exp(-0.001259*(193343-zz)) + (-2.035e-7)*z);

  fPar[3][2] = 0 + fSF*(-1.927e-7)*z;

  //////////////////////////////////////////////////////
  // The alpha_yy(z) parameter for Bx, By, Bz [mkT/cm^2]

  fPar[4][0] =  0 + fSF*((5.165e-7)*z*exp(0.001635*(zz-193343)) + (-3.981e-8)*z + (2.273e-11)*z2);

  fPar[4][1] =  0 + fSF*((-2.916e-5)*z*exp(-0.001363*(193343-zz)) + (-3.165e-7)*z);

  fPar[4][2] =  0 + fSF*(((-1.317e-9)*z + (9.867e-13)*z2 + (-2.596e-16)*z3
                          + (2.817e-20)*z4 + (-1.065e-24)*z5)*z);

  //////////////////////////////////////////////////////
  // The alpha_xy(z) parameter for Bx, By, Bz [mkT/cm^2]

  fPar[5][0] =  0 + fSF*((-1.41e-5)*z*exp(-0.001268*(193343-zz))+ (-4.028e-7)*z);

  fPar[5][1] =  0 + fSF*((-5.918e-7)*z + (4.81e-10)*z2+ (-1.248e-13)*z3 + (4.927e-18)*z4
                         + (1.936e-21)*z5 + (-1.653e-25)*z6);

  fPar[5][2] =  0 + fSF*(-7.255e-8)*z;
}

////////////////////////////////////////////////////////////////
// Evaluation of the field intensity. Input: [mm], output: [mkT]
// The fringe field itself is divided into regions
// The first region 183.311m < z < 192.683m, the map is determined using
// the ParameterArray function.
// The second region 192.683m < z < 193.083m, is a buffer region to slowly
// change between the fringe field and the MNP33 field.
// The third region 200.907m < z < 201.307m, is another buffer region to
// slowly change between the MNP33 and fringe field.
// The fourth region 201.307m < z < 210.679m, the map is determined using
// the ParameterArray function.
// The fifth region 210.679m < z < 212.000, is the final buffer region, to
// take the fringe field components to zero.

TVector3 FringeMagneticFieldMap::GetField(Double_t xx, Double_t yy, Double_t zz) {

  if (zz<fZmin || zz>fZmax) return TVector3(0.0, 0.0, 0.0);
  if (zz<fZStartFringe || zz>fZEndBuffer3) return TVector3(0.0, 0.0, 0.0);

  // Convert input units [mm] into internal units. Transverse scale (x,y) unit: [cm]

  Double_t x = 0.1*xx;
  Double_t y = 0.1*yy;
  Double_t z = zz;

  // Set the longitudinal scale; force z to have specific values to
  // evaluate the fringe field properly in each region

  // The first fringe field region
  if (zz >= fZStartFringe && zz < fZStartBuffer1) z = zz;

  // If z is in first or second buffer region, then
  // Force z = 192683 for correct interpolation
  else if (zz >= fZStartBuffer1 && zz < fZEndBuffer2)
    z = fZStartBuffer1;

  // The fringe field reflection beyond the magnet. Convert z
  // by summing fZStartBuffer1 + fZEndBuffer2 = fZReflection = 393990.0
  else if (zz >= fZEndBuffer2 && zz <= fZStartBuffer3)
    z = fZReflection - zz;

  // This is for buffer zone 3
  else
    z = fZReflection - fZStartBuffer3;

  // We use the ParameterArray member function to fill the fPar with the
  // 18 parameters for a given Z position. Z is tuned for each area above.

  ParameterArray(z);

  // Polynomial functions below determine the magnetic field at z = zz using fPar
  // At z = 192683MM, the functions swap to the buffer zone, which linearly interpolates 
  // to the MNP33. A second buffer region starts at z = 192683mm. A third buffer starts
  // at the end of the fringe field (z = 210679mm)

  TVector3 B;

  // Bx computation [mkT]
  B[0] = fPar[0][0] + fPar[1][0]*x + fPar[2][0]*y + fPar[3][0]*x*x 
    + fPar[4][0]*y*y + fPar[5][0]*x*y;

  // By computation [mkT]
  B[1] = fPar[0][1] + fPar[1][1]*x + fPar[2][1]*y + fPar[3][1]*x*x 
    + fPar[4][1]*y*y + fPar[5][1]*x*y;

  // Bz computation [mkT]
  B[2] = fPar[0][2] + fPar[1][2]*x + fPar[2][2]*y + fPar[3][2]*x*x 
      + fPar[4][2]*y*y + fPar[5][2]*x*y;

  // NOTE: The z-field is opposite sign on the downstream
  // side of the fringe field; this is set here.

  if (zz >= fZStartBuffer2 && zz <= fZEndBuffer3) B.SetZ(-B.Z());

  // The buffer region is determined/described below. To match the MNP33, a buffer region
  // of magnitude 400mm is opened symmetrically on either side of the magnet. The field is
  // linearly interpolated between the fringe field (at 192683mm, 201307mm) to the MNP33 
  // at (193083mm, 200907mm).

  // buffer region dependent
  TVector3 Difference, Beff;
  Double_t BufferSize, StartBuffer;
  // for interpolation in the buffer regions
  TVector3 tanTheta, interpField, NewB;

  // Use trigonometry; tanTheta = opposite/adjacent; here the opposite is the Difference 
  // in the B field, adjacent is the buffer size. interpField is the value of B relative to
  // the start of the buffer. Beff is set to sum with interpField to give the correct B.

  if(zz >= fZStartBuffer1 && zz <= fZEndBuffer1) {          //BUFFER1
    Difference  = MNP33FirstBound(xx,yy) - B;
    BufferSize  = fBufferSize1;
    StartBuffer = fZStartBuffer1;
    Beff        = B;
  }
  else if(zz >= fZStartBuffer2 && zz <= fZEndBuffer2) {     //BUFFER2
    Difference  = B - MNP33SecondBound(xx,yy);
    BufferSize  = fBufferSize2;
    StartBuffer = fZStartBuffer2;
    Beff        = MNP33SecondBound(xx,yy);
  }
  else if(zz >= fZStartBuffer3 && zz <= fZEndBuffer3) {     //BUFFER3
    Difference  = -B;
    BufferSize  = fBufferSize3;
    StartBuffer = fZStartBuffer3;
    Beff        = B;
  }
  else return B;        //NOT IN BUFFER; Return B without any changes

  //continue with the calculation otherwise
  tanTheta    = Difference * (1.0/BufferSize);
  interpField = tanTheta * (zz-(StartBuffer));
  NewB        = Beff + interpField;
  B           = NewB;
  return B;
}

/////////////////////////////////////////////
// Print the field intensity at a given point

void FringeMagneticFieldMap::PrintFieldValue(Double_t x, Double_t y, Double_t z) {
  TVector3 B = GetField(x,y,z);
  cout << "Bx By Bz [mkT] = "<<B.X()<<" "<<B.Y()<<" "<<B.Z()<<endl;
}

void FringeMagneticFieldMap::PrintFieldValue(TVector3 Point) {
  TVector3 B = GetField(Point.X(), Point.Y(), Point.Z());
  cout << "Bx By Bz [mkT] = "<<B.X()<<" "<<B.Y()<<" "<<B.Z()<<endl;
}

///////////////////////////////////////////////////////////////////////////////
// Get the integral of Bi*dz either upstream or downstream of the MNP33 magnet
// Returns the value when given a component of the field and a direction for
// a specific x and y position
Double_t FringeMagneticFieldMap::GetIntegral
(Int_t Component, Int_t x, Int_t y, Bool_t UpOrDown) {

  // Arrays to be filled with upstream(Up) and downstream(Dn) values later.
  TVector3 FieldUp[16], FieldDn[16];

  // Doubles used to load the integrals
  Double_t Sx = 0.0, Sy = 0.0, Sz = 0.0;

  // fZReflection is used to swap between the upstream and downstream
  // BufferSize3 is magnitude in z of the third buffer zone. Used for the 
  // additional buffer component in the downstream.

  // Fills the Field TVector3 with the upstream and downstream field values.
  for(Int_t i = 0; i < fNPlanes; ++i){
    FieldUp[i] = GetField(x,y,fZ[i]);
    FieldDn[i] = GetField(x,y,fZReflection-fZ[i]);
  }

  // Bool 'UpOrDown' to choose whether to get integral upstream or downstream
  // true == upstream, false == downstream

  for(Int_t i = 0; i < fNPlanes-1; i++){
    if (UpOrDown) {
      // integrating upstream
      Sx += 0.5*(FieldUp[i].X() + FieldUp[i+1].X())*(fabs(fZ[i+1]-fZ[i]));
      Sy += 0.5*(FieldUp[i].Y() + FieldUp[i+1].Y())*(fabs(fZ[i+1]-fZ[i]));
      Sz += 0.5*(FieldUp[i].Z() + FieldUp[i+1].Z())*(fabs(fZ[i+1]-fZ[i]));
    }
    else {
      // integrating downstream
      Sx += 0.5*(FieldDn[i].X() + FieldDn[i+1].X())*(fabs(fZ[i+1]-fZ[i]));
      Sy += 0.5*(FieldDn[i].Y() + FieldDn[i+1].Y())*(fabs(fZ[i+1]-fZ[i]));
      Sz += 0.5*(FieldDn[i].Z() + FieldDn[i+1].Z())*(fabs(fZ[i+1]-fZ[i]));
    }
  }

  if (!UpOrDown) {
    // additional buffer component for downstream
    Sx += 0.5*(FieldDn[0].X())*(fBufferSize3);
    Sy += 0.5*(FieldDn[0].Y())*(fBufferSize3);
    Sz += 0.5*(FieldDn[0].Z())*(fBufferSize3);
  }

  // return the wanted field component
  if      (Component==1) return Sx/1e9;
  else if (Component==2) return Sy/1e9;
  else if (Component==3) return Sz/1e9;
  else                   return 0.0;
}

////////////////////////////////////////////////////////////////////////                      
// This function calls GetIntegral to print the field integrals of each                          
// component, for the 29 grid measurements performed in the fringe field              

void FringeMagneticFieldMap::PrintFieldIntegrals() {

  Double_t SxU[fNGridPos], SyU[fNGridPos], SzU[fNGridPos];
  Double_t SxD[fNGridPos], SyD[fNGridPos], SzD[fNGridPos];

  // The arrays below return the X,Y of the grid measurement positions  
  // in the same order that they are presented in the measurements file.  

  Double_t GridX[29] = {200,0,-200,-200,-200,0,200,200,0,500,0,-500,-500,-500,0,
                        500,500,500,0,-500,-800,-800,-800,-500,0,500,800,800,800};

  Double_t GridY[29] = {200,200,200,0,-200,-200,-200,0,0,500,500,500,0,-500,-500,
                        -500,0,800,800,800,500,0,-500,-800,-800,-800,-500,0,500};

  for (Int_t P = 0; P < fNGridPos; ++P) {
    // fill the double arrays with the integrals for each grid position
    SxU[P] = GetIntegral(1,GridX[P],GridY[P],true);
    SyU[P] = GetIntegral(2,GridX[P],GridY[P],true);
    SzU[P] = GetIntegral(3,GridX[P],GridY[P],true);
    SxD[P] = GetIntegral(1,GridX[P],GridY[P],false);
    SyD[P] = GetIntegral(2,GridX[P],GridY[P],false);
    SzD[P] = GetIntegral(3,GridX[P],GridY[P],false);
  }

  // Print the field integrals upstream/downstream for all grid positions

  cout << "Field integrals for each grid pos (mm,mm): By*dz, Bz*dz [Tm] UPSTREAM of MNP33 = " << endl;
  for (Int_t i=0; i<fNGridPos; ++i) {
    cout << "GP = (" << GridX[i] << "," << GridY[i] << ")"
         << "      "     << SxU[i]   << " , " << SyU[i]   << " , " << SzU[i] << endl;
  }

  cout << "Field integrals for each grid pos (mm,mm): By*dz, Bz*dz [Tm] DOWNSTREAM of MNP33 = " << endl;
  for (Int_t i=0; i<fNGridPos; ++i) {
    cout << "GP = (" << GridX[i] << "," << GridY[i] << ")"
         << "      "     << SxD[i]   << " , " << SyD[i]   << " , " << SzD[i] << endl;
  }
}
