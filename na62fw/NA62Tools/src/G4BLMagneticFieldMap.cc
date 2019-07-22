// ------------------------------------------------------------------
// History:
//                                                                                                                            
// Created by Simone Schuchmann 2019-07-03
//
// Class to read beamline magnet field maps. Use BLMagnetField 
// class to get the properly transformed info for given magnet.
//
// ------------------------------------------------------------------

#include "G4BLMagneticFieldMap.hh"
#include "NA62ConditionsService.hh"
#include "NA62Global.hh"

#include "TObjArray.h"
#include "TVector3.h"
#include "TObjString.h"
#include "TString.h"
#include <fstream>
#include <string>
#include <iostream>

G4BLMagneticFieldMap::G4BLMagneticFieldMap(TString Filename): fname(Filename){

  ReadFieldMap();
}

G4BLMagneticFieldMap::~G4BLMagneticFieldMap()
{
  if(fmapBx) delete fmapBx;
  if(fmapBy) delete fmapBy;
  if(fmapBz) delete fmapBz;
}

TVector3 G4BLMagneticFieldMap::GetFieldValue(double local[4], double _current)
{
  double x = local[0];
  double y = local[1];
  double z = local[2];
  double field[6];

  field[0] = field[1] = field[2] = field[3] = field[4] = field[5] = 0.0;

  if(fabs(x) >(fnX-1)*fdX || fabs(y) >(fnY-1)*fdY || fabs(z) >(fnZ-1)*fdZ){
    return TVector3(field[0],field[1],field[2]);
  }

  x -= fX0;
  y -= fY0;
  z -= fZ0;

  double factor[6];
  factor[0]=factor[1]=factor[2]=factor[3]=factor[4]=factor[5]=1.0;
  if(fextendX && x < 0.0) {
    x = -x;
    for(int i=0; i<6; ++i) {
      if(fextendXbits & (1<<i)) {factor[i] = -factor[i];
    }}
  }
  if(fextendY && y < 0.0) {
    y = -y;
    for(int i=0; i<6; ++i) {
      if(fextendYbits & (1<<i)) {factor[i] = -factor[i];
      }}
  }
  if(fextendZ && z < 0.0) {
    z = -z;
    for(int i=0; i<6; ++i) {
      if(fextendZbits & (1<<i)){ factor[i] = -factor[i];
      }}
  }

  // We compute a 3D linear average of the 8 surrounding points in the map
  // First, get the X,Y,Z indices into i,j,k
  int i = (int)floor(x/fdX);
  int j = (int)floor(y/fdY);
  int k = (int)floor(z/fdZ);
  if(i < 0 || i >= fnX-1 || j < 0 || j >= fnY-1 || k < 0 || k >= fnZ-1) {
    field[0] = field[1] = field[2] = field[3] = field[4] =
      field[5] = 0.0;
    return TVector3(field[0],field[1],field[2]);;
  }
  // m is the initial index (corner of the cube with minimum X, Y, and Z)
  int m = k*fnY*fnX + j*fnX + i;
  //BLAssert(m+fnY*fnX+fnX+1 < fnX*fnY*fnZ);

  // now compute the fractional weighting factors for X, Y, and Z 
  float fx = 1.0 - (x - i*fdX) / fdX;
  //BLAssert(fx >= 0.0 && fx <= 1.0);
  float fy = 1.0 - (y - j*fdY) / fdY;
  // BLAssert(fy >= 0.0 && fy <= 1.0);
  float fz = 1.0 - (z - k*fdZ) / fdZ;
  //  BLAssert(fz >= 0.0 && fz <= 1.0);

  // now compute the fractional weighting factors for the 8 corners 
  float f0 = fx*fy*fz;
  float f1 = (1.0-fx)*fy*fz;
  float f2 = fx*(1.0-fy)*fz;
  float f3 = (1.0-fx)*(1.0-fy)*fz;
  float f4 = fx*fy*(1.0-fz);
  float f5 = (1.0-fx)*fy*(1.0-fz);
  float f6 = fx*(1.0-fy)*(1.0-fz);
  float f7 = (1.0-fx)*(1.0-fy)*(1.0-fz);

  // Finally, compute the components of the field                                                                               
                                                  
#define COMPONENT(C)							\
  double C = 0.0;							\
  if(fmap##C) C =  fmap##C[m]*f0 + fmap##C[m+1]*f1 +			\
		fmap##C[m+fnX]*f2 + fmap##C[m+fnX+1]*f3 +		\
		fmap##C[m+fnY*fnX]*f4 + fmap##C[m+fnY*fnX+1]*f5 +	\
		fmap##C[m+fnY*fnX+fnX]*f6 + fmap##C[m+fnY*fnX+fnX+1]*f7;
  COMPONENT(Bx);
  COMPONENT(By);
  COMPONENT(Bz);

  field[0] = Bx * factor[0];
  field[1] = By * factor[1];
  field[2] = Bz * factor[2];

  if(_current >0.0){
    field[0] = field[0] * fnormB * _current/fcurrent;
    field[1] = field[1] * fnormB * _current/fcurrent;
    field[2] = field[2] * fnormB * _current/fcurrent;
  }
  return TVector3(field[0],field[1],field[2]);
}

bool G4BLMagneticFieldMap::ReadFieldMap()
{
  TString Line;
  TString fileName = fname;
  if(NA62ConditionsService::GetInstance()->Open(fileName)!=kSuccess){
    std::cout << "[G4BLMagneticFieldMap] Error: failed not open input file " <<fileName<< std::endl;
    exit(kGenericError);
  }

  int linenumber = 0;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fileName))){
    linenumber++;    
    if(Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("param")){
      TObjArray *l  = Line.Tokenize(" ");
      TString snormB   = ((TObjString*)(l->At(1)))->GetString();
      snormB.ReplaceAll("normB=","");
      TString scurrent = ((TObjString*)(l->At(2)))->GetString();
      scurrent.ReplaceAll("current=","");
      delete l;
      fnormB = snormB.Atof();
      fcurrent = scurrent.Atof();
    }
    else if(Line.BeginsWith("grid")){
      TObjArray *l  = Line.Tokenize(" ");
      TString sx0 = ((TObjString*)(l->At(1)))->GetString();
      TString sy0 = ((TObjString*)(l->At(2)))->GetString();
      TString sz0 = ((TObjString*)(l->At(3)))->GetString();
      TString snx = ((TObjString*)(l->At(4)))->GetString();
      TString sny = ((TObjString*)(l->At(5)))->GetString();
      TString snz = ((TObjString*)(l->At(6)))->GetString();
      TString sdx = ((TObjString*)(l->At(7)))->GetString();
      TString sdy = ((TObjString*)(l->At(8)))->GetString();
      TString sdz = ((TObjString*)(l->At(9)))->GetString();

      sx0.ReplaceAll("X0=","");
      sy0.ReplaceAll("Y0=","");
      sz0.ReplaceAll("Z0=","");
      snx.ReplaceAll("nX=","");
      sny.ReplaceAll("nY=","");
      snz.ReplaceAll("nZ=","");
      sdx.ReplaceAll("dX=","");
      sdy.ReplaceAll("dY=","");
      sdz.ReplaceAll("dZ=","");

      delete l;
      fX0 = sx0.Atof();//mm
      fY0 = sy0.Atof();
      fZ0 = sz0.Atof();
      fnX = snx.Atof();
      fnY = sny.Atof();
      fnZ = snz.Atof();
      fdX = sdx.Atof();//mm
      fdY = sdy.Atof();
      fdZ = sdz.Atof();
    }
    else if(Line.BeginsWith("extendX")){
      TObjArray *l  = Line.Tokenize(" ");
      TString snormB = ((TObjString*)(l->At(1)))->GetString() ;
      TString fflipX = snormB;
      fflipX.ReplaceAll("flip=","");
      if(snormB.Contains("flip")) fextendXbits = Bits(fflipX);
      fextendX = true;
      delete l;
    }

    else if(Line.BeginsWith("extendY")){
      TObjArray *l  = Line.Tokenize(" ");
      TString snormB  = ((TObjString*)(l->At(1)))->GetString();
      TString fflipY = snormB;
      fflipY.ReplaceAll("flip=","");
      if(snormB.Contains("flip")) fextendYbits = Bits(fflipY);
      fextendY = true;
      delete l;
    }

    else if(Line.BeginsWith("extendZ")){
      TObjArray *l  = Line.Tokenize(" ");
      TString snormB = ((TObjString*)(l->At(1)))->GetString();
      TString fflipZ = snormB;
      fflipZ.ReplaceAll("flip=","");
      if(snormB.Contains("flip")) fextendZbits = Bits(fflipZ);
      fextendZ = true;
      delete l;
    }

    else if(Line.BeginsWith("data")){
      continue;
    }
    else{
      TObjArray *l  = Line.Tokenize(",");
      float X = ((TObjString*)(l->At(0)))->GetString().Atof();
      float Y = ((TObjString*)(l->At(1)))->GetString().Atof();
      float Z = ((TObjString*)(l->At(2)))->GetString().Atof();
      float Bx = ((TObjString*)(l->At(3)))->GetString().Atof();
      float By = ((TObjString*)(l->At(4)))->GetString().Atof();
      float Bz = ((TObjString*)(l->At(5)))->GetString().Atof();
      SetField(X,Y,Z,Bx,By,Bz,linenumber);//units in mm and tesla
    }
  }
  NA62ConditionsService::GetInstance()->Close(fileName); 
  return true;
}

int G4BLMagneticFieldMap::Bits(TString s)
{
  int v=0;
  if(s.Contains("Bx")) v |= 1;
  if(s.Contains("By")) v |= 2;
  if(s.Contains("Bz")) v |= 4;
  return v;
}

bool G4BLMagneticFieldMap::SetField(double X, double Y, double Z, double Bx,
				      double By, double Bz,int linenumber)
{
  if(!fmapBx && Bx != 0.0) {
    fmapBx = new float[fnX*fnY*fnZ];
    //BLAssert(fmapBx != 0);
    for(int i=0; i<fnX*fnY*fnZ; ++i) 
      fmapBx[i] = 0.0;
  }
  if(!fmapBy && By != 0.0) {
    fmapBy = new float[fnX*fnY*fnZ];
    //BLAssert(fmapBy != 0);
    for(int i=0; i<fnX*fnY*fnZ; ++i) 
      fmapBy[i] = 0.0;
  }
  if(!fmapBz && Bz != 0.0) {
    fmapBz = new float[fnX*fnY*fnZ];
    //BLAssert(fmapBz != 0);
    for(int i=0; i<fnX*fnY*fnZ; ++i) 
      fmapBz[i] = 0.0;
  }

  X -= fX0;
  Y -= fY0;
  Z -= fZ0;
  int i = (int)floor((X/fdX) + 0.5);
  if(i<0 || fabs(i*fdX-X)>ftolerance || i >= fnX) {
    std::cout<<Form("G4BLMagneticFieldMap: ERROR point off"
		 " grid  X=%.2f line=%d\n",X, linenumber)<<std::endl;
    return false;
  }
  int j = (int)floor((Y/fdY) + 0.5);
  if(j<0 || fabs(j*fdY-Y)>ftolerance || j >= fnY) {
    std::cout<<Form("G4BLMagneticFieldMap: ERROR point off"
		 " grid Y=%.2f line=%d\n",Y, linenumber)<<std::endl;
    return false;
  }
  int k = (int)floor((Z/fdZ) + 0.5);
  if(k<0 || fabs(k*fdZ-Z)>ftolerance || k >= fnZ) {
    std::cout<<Form("G4BLMagneticFieldMap: ERROR point off"
		 " grid X=%.2f line=%d\n",Z, linenumber)<<std::endl;
    return false;
  }
 
  int m = k*fnY*fnX + j*fnX + i;
  //BLAssert(m >= 0 && m < fnX*fnY*fnZ);
  if(fmapBx) fmapBx[m] = Bx;
  if(fmapBy) fmapBy[m] = By;
  if(fmapBz) fmapBz[m] = Bz;
  return true;
}
