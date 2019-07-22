// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2015-10-13
// Modified by Viacheslav Duk (Viacheslav.Duk@cern.ch)     07.05.2018
//
// ------------------------------------------------------------------

/// \class MNP33MagneticFieldMap
/// \Brief
/// Magnetic field map in the MNP33 volume
/// \EndBrief
/// \Detailed
/// Magnetic field map in the MNP33 magnet volume (193.083m <= z <= 200.907m).
/// Tri-linear interpolation of the measurements on a regular grid is performed.
/// The measurements performed in September 2013 are reported in the note NA62-14-09.
/// Measurements in the last z plane (z=200.9432m) are not used (see MagneticField.cc),
/// therefore the field map extent is symmetric with respect to the centre of the magnet (z=196.995m).
/// The interpolation algorithm is optimized due to the periodicity of field measurements in X, Y
/// and quasi-periodiciy in Z. Interpolation coefficients are stored in 12 files.
/// The optimization allows to speed up significantly the magnetic field calculation
/// (at least by a factor of 3) and is important for the fast simulation of decay chains
/// (for example K3pi with two pions decaying before the end of the Spectrometer) which
/// disables time-consuming simulation of detectors downstream the Spectrometer
/// and the Cherenkov light in Cedar.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \author Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// \EndDetailed

#include "MNP33MagneticFieldMap.hh"
#include "NA62ConditionsService.hh"
#include "TVector3.h"
#include "TObjString.h"
#include "TObjArray.h"
#include <iostream>

using namespace std;

///////////////////////////////////////////////
// Initialization of the field parameterization

MNP33MagneticFieldMap::MNP33MagneticFieldMap() :
  // -1000 mm <= x <= +1000 mm, 26 points in 25 steps of 80 mm;
  // -1000 mm <= y <= +1000 mm, 26 points in 25 steps of 80 mm;
  // 169 points in z with irregular spacing, 193083 mm <= z <= 200943.2 mm.
  // Map only up to z=200907 mm is used in the MagneticField class; the last measurement plane is neglected.
  // In this way, the extent of the map is symmetric wrt the centre of the magnet (z=196995 mm).
  // Fringe field takes over at z=193083 mm and z=200907 mm.
  // Internal units in this class: [mm], [Tesla].
  fNPointsX(26), fNPointsY(26), fNPointsZ(169), fZmin(193083), fZmax(200943.2),
  fXmin(-1000), fXmax(+1000), fYmin(-1000), fYmax(+1000),
  fZref(196995),   // to transform into the NA62 reference frame
  fXstep(80), fYstep(80), fZstep((fZmax-fZmin)/fNPointsZ),
  fEmpiricZ(22.) // empiric parameter to optimize Z-bin search during the interpolation
{
  ////////////////////////////////////////////////////////
  // Read the measurements.
  // Input text file format: x, y, z [mm], Bx, By, Bz [T].

  // Reset arrays
  for (Int_t ix=0; ix<fNPointsX; ix++) {
    for (Int_t iy=0; iy<fNPointsY; iy++) {
      fMeasurementExists[ix][iy] = false;
      for (Int_t iz=0; iz<fNPointsZ; iz++) {
	fBx[ix][iy][iz] = 0.0;
      }
    }
  }

  fFieldMapName = NA62ConditionsService::GetInstance()->GetFullPath("MNP33-field-map.dat");
  ifstream mnp33file (fFieldMapName);
  Int_t k=0;
  Double_t xx, yy, zz, bx, by, bz;
  while (mnp33file >> xx >> yy >> zz >> bx >> by >> bz) {
    Int_t iz = k%169;
    if (k<169) fZBinCentre[iz] = zz; // record the irregular Z coordinates
    Int_t ix = (xx-fXmin)/fXstep;
    Int_t iy = (yy-fYmin)/fYstep;
    fBx[ix][iy][iz] = bx; // Unit: [Tesla]
    fBy[ix][iy][iz] = by;
    fBz[ix][iy][iz] = bz;
    fMeasurementExists[ix][iy] = true;
    k++;
  }
  mnp33file.close();

  ///////////////////////////////////////////////////////////////////////////
  // These measurements were made in an approximately octagonal (x,y) region:
  // extend them to a square region of 2x2 m^2 assuming dB_{x,y,z}/dx=0.

  for (Int_t iy=0; iy<fNPointsY; iy++) {
    Int_t ix1 = -1, ix2 = fNPointsX;
    while (!fMeasurementExists[ix1+1][iy]) ix1++;
    while (!fMeasurementExists[ix2-1][iy]) ix2--;
    for (Int_t iz=0; iz<fNPointsZ; iz++) {
      for (int ix=0; ix<=ix1; ix++) {
	fBx[ix][iy][iz] = fBx[ix1+1][iy][iz];
	fBy[ix][iy][iz] = fBy[ix1+1][iy][iz];
	fBz[ix][iy][iz] = fBz[ix1+1][iy][iz];
      }
      for (int ix=ix2; ix<fNPointsX; ix++) {
	fBx[ix][iy][iz] = fBx[ix2-1][iy][iz];
	fBy[ix][iy][iz] = fBy[ix2-1][iy][iz];
	fBz[ix][iy][iz] = fBz[ix2-1][iy][iz];
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////
  // The measurements are stored in 3-dimensional histograms, which
  // allows using the TH3::Interpolate() method for the tri-linear interpolation.
  // The bin boundaries of the histograms are defined below.
  // Z bin boundaries are in the middle between measurement points (irregular grid).
  // NB: this does not mean that the points are in the bin centres.

  for (int i=0; i<fNPointsZ; i++) {
    if (i>0)           fZEdgeLow[i]  = 0.5*(fZBinCentre[i]+fZBinCentre[i-1]);
    if (i<fNPointsZ-1) fZEdgeHigh[i] = 0.5*(fZBinCentre[i]+fZBinCentre[i+1]);
  }
  fZEdgeLow[0]            = 1.5*fZBinCentre[0] - 0.5*fZBinCentre[1];
  fZEdgeHigh[fNPointsZ-1] = 1.5*fZBinCentre[fNPointsZ-1] - 0.5*fZBinCentre[fNPointsZ-2];

  for (int i=0; i<=fNPointsZ; i++) {
    fZEdge[i] = (i==fNPointsZ) ? fZEdgeHigh[fNPointsZ-1] : fZEdgeLow[i];
    fRealZBinCentre[i] = 0.5*(fZEdgeLow[i] + fZEdgeHigh[i]);
  }

  ////////////////////////////////////////////////////////////////////////
  // Bin centres and bin boundaries in X and Y coordinates (regular grids)

  for (int i=0; i<fNPointsX; i++) {
    fXBinCentre[i] = -1000 + fXstep*i;
  }
  for (int i=0; i<fNPointsY; i++) {
    fYBinCentre[i] = -1000 + fYstep*i;
  }
  for (int i=0; i<=fNPointsX; i++) {
    fXEdge[i] = -1040 + fXstep*i;
  }
  for (int i=0; i<=fNPointsY; i++) {
    fYEdge[i] = -1040 + fYstep*i;
  }

  ///////////////////////////////////////////////////////
  // Book and fill histograms containing the measurements

  fHBx = new TH3D("hBx", "hBx", fNPointsX, fXEdge, fNPointsY, fYEdge, fNPointsZ, fZEdge);
  fHBy = new TH3D("hBy", "hBy", fNPointsX, fXEdge, fNPointsY, fYEdge, fNPointsZ, fZEdge);
  fHBz = new TH3D("hBz", "hBz", fNPointsX, fXEdge, fNPointsY, fYEdge, fNPointsZ, fZEdge);

  for (int ix=1; ix<=fNPointsX; ix++) {
    for (int iy=1; iy<=fNPointsY; iy++) {
      for (int iz=1; iz<=fNPointsZ; iz++) {
	fHBx->SetBinContent(fHBx->GetBin(ix,iy,iz), fBx[ix-1][iy-1][iz-1]);
	fHBy->SetBinContent(fHBy->GetBin(ix,iy,iz), fBy[ix-1][iy-1][iz-1]);
	fHBz->SetBinContent(fHBz->GetBin(ix,iy,iz), fBz[ix-1][iy-1][iz-1]);
      }
    }
  }
  // PrintFieldIntegrals();

  ////////////////////////////////////////////////////
  // Read coefficients for the trilinear interpolation

  Int_t iCounter = 0, ix = 0, iy = 0;
  TString Line;

  // Bx
  ifstream CoeffFile1_x(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-x-part1.dat"));  
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile1_x)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff0_x[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff1_x[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile1_x.close();

  ifstream CoeffFile2_x(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-x-part2.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile2_x)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff2_x[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff3_x[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile2_x.close();

  ifstream CoeffFile3_x(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-x-part3.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile3_x)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff4_x[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff5_x[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile3_x.close();

  ifstream CoeffFile4_x(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-x-part4.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile4_x)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff6_x[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff7_x[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile4_x.close();

  // By
  ifstream CoeffFile1_y(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-y-part1.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile1_y)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff0_y[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff1_y[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile1_y.close();

  ifstream CoeffFile2_y(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-y-part2.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile2_y)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff2_y[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff3_y[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile2_y.close();

  ifstream CoeffFile3_y(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-y-part3.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile3_y)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff4_y[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff5_y[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile3_y.close();

  ifstream CoeffFile4_y(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-y-part4.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile4_y)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff6_y[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff7_y[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile4_y.close();

  // Bz
  ifstream CoeffFile1_z(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-z-part1.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile1_z)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff0_z[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff1_z[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile1_z.close();

  ifstream CoeffFile2_z(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-z-part2.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile2_z)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff2_z[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff3_z[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile2_z.close();

  ifstream CoeffFile3_z(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-z-part3.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile3_z)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff4_z[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff5_z[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile3_z.close();

  ifstream CoeffFile4_z(NA62ConditionsService::GetInstance()->GetFullPath("MNP33-coefficients-z-part4.dat"));
  iCounter = ix = iy = 0;
  while (Line.ReadLine(CoeffFile4_z)) {
    if (Line.BeginsWith("#")) continue;
    Int_t iz = iCounter%169;
    if (iz==0 && iCounter>0) {
      iy++;
      if (iy==26) {
        iy=0;
        ix++;
      }
    }
    TObjArray *l = Line.Tokenize(" ");
    coeff6_z[ix][iy][iz] = ((TObjString*)(l->At(0)))->GetString().Atof();
    coeff7_z[ix][iy][iz] = ((TObjString*)(l->At(1)))->GetString().Atof();
    delete l;
    iCounter++;
  }
  CoeffFile4_z.close();

  // print the field map
  //ofstream FileOut("map.dat");
  //FileOut << "# x y z Bx By Bz " << endl;
  //for (Int_t x=-1035; x<1040; x+=80) {
  //for (Int_t y=-1035; y<1040; y+=80) {
  //  for (Int_t z=-3805; z<3900; z+=50) {
  //    FileOut << x << " " << y << " " << z << " " <<
  //      GetField(x, y, z+fZref).X() << " " << GetField(x, y, z+fZref).Z() << " " << GetField(x, y, z+fZref).Z() << endl;
  //  }
  //}
  //}
  //FileOut.close();

}

//////////////////////////////////////////////////////////////
// Evaluation of the field intensity. Input: [mm], output: [T]

TVector3 MNP33MagneticFieldMap::GetField(Double_t x, Double_t y, Double_t z) {
  if (x<fXmin || x>fXmax || y<fYmin || y>fYmax || z<fZmin || z>fZmax) {
    return TVector3(0,0,0);
  }

  // optimized trilinear interpolation

  Int_t TrialBin = (z-fZmin)/fZstep;
  Int_t jz = 167;
  if ((z-fZref)<=fEmpiricZ) {
    Int_t MinBin = (TrialBin>=6) ? (TrialBin-6) : 0;
    for (Int_t i=MinBin; i<=TrialBin+1; i++) {
      if ((z-fZref)>=fRealZBinCentre[i] && (z-fZref)<fRealZBinCentre[i+1]) {
        jz = i;
        break;
      }
    }
  }

  if ((z-fZref)>fEmpiricZ) {
    Int_t MaxBin = (TrialBin<=162) ? (TrialBin+6) : 168;
    for (Int_t i=TrialBin-1; i<=MaxBin; i++) {
      if ((z-fZref)>=fRealZBinCentre[i] && (z-fZref)<fRealZBinCentre[i+1]) {
        jz = i;
        break;
      }
    }
  }

  Int_t This_ix = (x-fXmin)/fXstep;
  Int_t This_iy = (y-fYmin)/fYstep;
  Int_t This_iz = jz; // jz calculated above

  // position in the interpolation cube
  Double_t dx = x - (fXmin + fXstep*This_ix);
  dx = dx/fXstep;
  Double_t dy = y - (fYmin + fYstep*This_iy);
  dy = dy/fYstep;
  Double_t dz = (z - fZref) - fRealZBinCentre[This_iz];
  dz = dz/(fRealZBinCentre[This_iz+1] - fRealZBinCentre[This_iz]);

  Double_t Bx =
    coeff0_x[This_ix][This_iy][This_iz] +
    coeff1_x[This_ix][This_iy][This_iz]*dx +
    coeff2_x[This_ix][This_iy][This_iz]*dy +
    coeff3_x[This_ix][This_iy][This_iz]*dz +
    coeff4_x[This_ix][This_iy][This_iz]*dx*dy +
    coeff5_x[This_ix][This_iy][This_iz]*dy*dz +
    coeff6_x[This_ix][This_iy][This_iz]*dx*dz +
    coeff7_x[This_ix][This_iy][This_iz]*dx*dy*dz;

  Double_t By =
    coeff0_y[This_ix][This_iy][This_iz] +
    coeff1_y[This_ix][This_iy][This_iz]*dx +
    coeff2_y[This_ix][This_iy][This_iz]*dy +
    coeff3_y[This_ix][This_iy][This_iz]*dz +
    coeff4_y[This_ix][This_iy][This_iz]*dx*dy +
    coeff5_y[This_ix][This_iy][This_iz]*dy*dz +
    coeff6_y[This_ix][This_iy][This_iz]*dx*dz +
    coeff7_y[This_ix][This_iy][This_iz]*dx*dy*dz;

  Double_t Bz = 
    coeff0_z[This_ix][This_iy][This_iz] +
    coeff1_z[This_ix][This_iy][This_iz]*dx +
    coeff2_z[This_ix][This_iy][This_iz]*dy +
    coeff3_z[This_ix][This_iy][This_iz]*dz +
    coeff4_z[This_ix][This_iy][This_iz]*dx*dy +
    coeff5_z[This_ix][This_iy][This_iz]*dy*dz +
    coeff6_z[This_ix][This_iy][This_iz]*dx*dz +
    coeff7_z[This_ix][This_iy][This_iz]*dx*dy*dz;

  return TVector3(Bx, By, Bz);

  // old parametrization of the MNP33 field map: the same results but slower
  // return TVector3
  // (fHBx->Interpolate(x, y, z-fZref),
  //  fHBy->Interpolate(x, y, z-fZref),
  //  fHBz->Interpolate(x, y, z-fZref));
}

//////////////////////////////////////////////////////////////////////////////////////////
// Evaluate the integral of By over the whole Z range.
// Input: component=1,2,3 for Bx, By, Bz integrals; bin numbers in X and Y (from 1 to 26).
// Output: By field integral in the centre of the corresponding bin.

Double_t MNP33MagneticFieldMap::GetIntegral(Int_t Component, Int_t BinX, Int_t BinY) {
  if (BinX<1 || BinX>fNPointsX) return 0.0;
  if (BinY<1 || BinX>fNPointsY) return 0.0;
  TH3D *h = nullptr;
  if      (Component==1) h = fHBx;
  else if (Component==2) h = fHBy;
  else if (Component==3) h = fHBz;
  if (h==nullptr) return 0.0;

  Double_t FieldIntegral = 0.0;
  for (Int_t iz=1; iz<fNPointsZ; iz++) {
    Int_t nbin1 = h->GetBin(BinX, BinY, iz);
    Int_t nbin2 = h->GetBin(BinX, BinY, iz+1);
    Double_t Field    = 0.5*(h->GetBinContent(nbin1)+h->GetBinContent(nbin2));
    Double_t StepSize = fZBinCentre[iz] - fZBinCentre[iz-1];
    FieldIntegral += (Field*StepSize);
  }
  return 1e-3*FieldIntegral; // [T*mm] --> [T*m]
}

/////////////////////////////////////////////
// Print the field intensity at a given point

void MNP33MagneticFieldMap::PrintFieldValue(Double_t x, Double_t y, Double_t z) {
  TVector3 B = GetField(x,y,z);
  cout << "Bx By Bz [T] = "<<B.X()<<" "<<B.Y()<<" "<<B.Z()<<endl;
}

void MNP33MagneticFieldMap::PrintFieldValue(TVector3 Point) {
  TVector3 B = GetField(Point[0], Point[1], Point[2]);
  cout << "Bx By Bz [T] = "<<B.X()<<" "<<B.Y()<<" "<<B.Z()<<endl;
}

void MNP33MagneticFieldMap::PrintFieldIntegrals() {
  for (int i=0; i<fNPointsX; i++) {
    for (int j=0; j<fNPointsY; j++) {
      cout << fXBinCentre[i]<<" "<<fYBinCentre[j]<<" "<<
	GetIntegral(1,i+1,j+1)<<" "<<GetIntegral(2,i+1,j+1)<<" "<<GetIntegral(3,i+1,j+1)<<endl;
    }
  }
}
