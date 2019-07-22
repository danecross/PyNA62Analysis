// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "CHANTIGeometry.hh"

#include "TString.h"

//#include "CHANTIGeometry.hh"

#include "iostream"

using namespace std;


CHANTIGeometry* CHANTIGeometry::fInstance = 0;

CHANTIGeometry::CHANTIGeometry():fZPos_Ring(0) {
  // Insert here all the parameters you need to define the geometry
  // waiting for reading everything from DataBase

  CreateGeometry();

}

CHANTIGeometry::~CHANTIGeometry(){
  if(fZPos_Ring){
    delete [] fZPos_Ring;
    fZPos_Ring=0;
  }
}

CHANTIGeometry * CHANTIGeometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new CHANTIGeometry(); }
  return fInstance;
}

void CHANTIGeometry::CreateGeometry(){
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class
  //
  //
  //
  //
  //
  // MAIN parameters
  fSensitiveDetectorName = "/CHANTI";
  fCollectionName = "CHANTICollection";
  //
  fWorldXLength = 10000. ;
  fWorldYLength = 10000. ;
  fWorldZLength = 1605;
  fDetectorXLength = 2000. ;
  fDetectorYLength = 2000. ;
  const double CHANTIStart = 102.425 ;//102.420*m; D.D. & P.M. 24/05/2011
  const double CHANTIEnd = 104.458 ;
  fDetectorZLength =  (CHANTIEnd - CHANTIStart);
  fDetectorZPosition = 0.5*(CHANTIStart + CHANTIEnd);
  //
  fTriangleBase = 300/9.;
  fTriangleAltitude = 16.5 ;
  //
  fSquareLength = 300. ;
  //
  fXInnerHoleLength = 95. ;
  fYInnerHoleLength = 65. ;
  //
  fNofRings = 12;
  fZPos_Ring = new Double_t[fNofRings]; // positions from target 
  fZPos_Ring[0]  = 102428 +   fTriangleAltitude/2 ;
  fZPos_Ring[1]  = 102428 + 3*fTriangleAltitude/2 ;
  fZPos_Ring[2]  = 102486 +   fTriangleAltitude/2 ;      
  fZPos_Ring[3]  = 102486 + 3*fTriangleAltitude/2 ;
  fZPos_Ring[4]  = 102601 +   fTriangleAltitude/2 ;
  fZPos_Ring[5]  = 102601 + 3*fTriangleAltitude/2 ;
  fZPos_Ring[6]  = 102831 +   fTriangleAltitude/2 ;
  fZPos_Ring[7]  = 102831 + 3*fTriangleAltitude/2 ;
  fZPos_Ring[8]  = 103291 +   fTriangleAltitude/2 ;
  fZPos_Ring[9]  = 103291 + 3*fTriangleAltitude/2 ;
  fZPos_Ring[10] = 104211 +   fTriangleAltitude/2 ;
  fZPos_Ring[11] = 104211 + 3*fTriangleAltitude/2 ;

  // DERIVED parameters
  //
  fRingThickness = fTriangleAltitude;  
  //
  //
  int StripIDV[] = {0, 1, 2, 3, 4, -5, 5, -6, 6, -7, 7, -8, 8, -9, 9, -10, 10, -11, 11, 12, 13, 14, 15, 16};
  int StripIDH[] = {0, 1, 2, 3, 4, 5, -6, 6, -7, 7, -8, 8, -9, 9, -10, 10, 11, 12, 13, 14, 15, 16};

  for(int i=0; i<12; i++){
    if( i%2==0 ){
      std::vector<int> StripID(StripIDV, StripIDV+24);
      fStripID.push_back(StripID);
      //cout << "--- " << i << " " << i%2 << std::endl;
      //for(int j=0; j<StripID.size(); j++)
      //cout << StripID.at(j) << std::endl; 
    }
    else{
      std::vector<int> StripID(StripIDH, StripIDH+22);
      fStripID.push_back(StripID);
      //cout << "--- " << i << " " << i%2 << std::endl;
      //for(int j=0; j<StripID.size(); j++)
      // std::cout << StripID.at(j) << std::endl; 
    } 
  }
}


