// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "CHODGeometry.hh"
#include "Riostream.h"

CHODGeometry* CHODGeometry::fInstance = 0;

CHODGeometry::CHODGeometry(){
  // Insert here all the parameters you need to define the geometry
  // waiting for reading everything from DataBase

  CreateGeometry();
}

CHODGeometry * CHODGeometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new CHODGeometry(); }
  return fInstance;
}

void CHODGeometry::CreateGeometry(){
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class
  fNumberOfChannel = 128;
  fNScintillatorCounter = 16;

  fScintillatorPosition[0]  = TVector3( 577.35, -4.85, 0.0);
  fScintillatorPosition[1]  = TVector3( 512.05, -4.85, 0.0);
  fScintillatorPosition[2]  = TVector3( 446.75, -4.85, 0.0);
  fScintillatorPosition[3]  = TVector3( 381.45, -4.85, 0.0);
  fScintillatorPosition[4]  = TVector3( 316.15, -4.85, 0.0);
  fScintillatorPosition[5]  = TVector3( 250.85, -4.85, 0.0);
  fScintillatorPosition[6]  = TVector3( 185.55, -4.85, 0.0);
  fScintillatorPosition[7]  = TVector3( 120.25, -4.85, 0.0);
  fScintillatorPosition[8]  = TVector3(  54.95, -14.35, 0.0);
  fScintillatorPosition[9]  = TVector3( -10.35, -46.85, 0.0);
  fScintillatorPosition[10] = TVector3( -75.65, -79.35, 0.0);
  fScintillatorPosition[11] = TVector3(-157.95,-111.85, 0.0);
  fScintillatorPosition[12] = TVector3(-257.25,-161.35, 0.0);
  fScintillatorPosition[13] = TVector3(-356.55,-210.85, 0.0);
  fScintillatorPosition[14] = TVector3(-455.85,-260.35, 0.0);
  fScintillatorPosition[15] = TVector3(-555.15,-309.85, 0.0);

  // slab length = <half length from MC in cm> * <1cm=10mm> * <half->whole size>
  fSlabLength[0]  =  60.50 * 10. * 2.;
  fSlabLength[1]  =  60.50 * 10. * 2.;
  fSlabLength[2]  =  60.50 * 10. * 2.;
  fSlabLength[3]  =  60.50 * 10. * 2.;
  fSlabLength[4]  =  60.50 * 10. * 2.;
  fSlabLength[5]  =  60.50 * 10. * 2.;
  fSlabLength[6]  =  60.50 * 10. * 2.;
  fSlabLength[7]  =  60.50 * 10. * 2.;
  fSlabLength[8]  =  59.55 * 10. * 2.;
  fSlabLength[9]  =  56.30 * 10. * 2.;
  fSlabLength[10] =  53.05 * 10. * 2.;
  fSlabLength[11] =  49.80 * 10. * 2.;
  fSlabLength[12] =  44.85 * 10. * 2.;
  fSlabLength[13] =  39.90 * 10. * 2.;
  fSlabLength[14] =  34.95 * 10. * 2.;
  fSlabLength[15] =  30.00 * 10. * 2.;

  // slab width
  for(Int_t i=0; i<64; i++){
    fSlabWidth[i] = 65.;
    if( (i>=11 && i<=20) || (i>=43 &&i<=52) ) fSlabWidth[i] = 99.;
  }
  for(Int_t i=64; i<128; i++){
    fSlabWidth[i] = 99.;
    if( (i>=69 &&i<=90) || (i>=101 &&i<=122) ) fSlabWidth[i] = 65.;
  }


}
