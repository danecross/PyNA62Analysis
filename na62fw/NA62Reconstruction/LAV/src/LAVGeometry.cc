// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// 2015-01-22 Implementation of LAVChannelID within a complete revision of LAV SW, by T. Spadaro and E. Leonardi
//
// --------------------------------------------------------------

#include "LAVGeometry.hh"
#include "LAVChannelID.hh"

#include "TMath.h"

LAVGeometry* LAVGeometry::fInstance = 0;

LAVGeometry::LAVGeometry(){
  // Insert here all the parameters you need to define the geometry
  // waiting for reading everything from DataBase

  CreateGeometry();
}

LAVGeometry * LAVGeometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new LAVGeometry(); }
  return fInstance;
}

void LAVGeometry::CreateGeometry(){
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class

  fNofStations = 12;

  fNofLayersPerStation = new Int_t[fNofStations];
  fNofBananasPerLayer = new Int_t[fNofStations];

  fNofLayersPerStation[0] = 5; fNofBananasPerLayer[0] = 8;
  fNofLayersPerStation[1] = 5; fNofBananasPerLayer[1] = 8;
  fNofLayersPerStation[2] = 5; fNofBananasPerLayer[2] = 8;
  fNofLayersPerStation[3] = 5; fNofBananasPerLayer[3] = 8;
  fNofLayersPerStation[4] = 5; fNofBananasPerLayer[4] = 8;
  fNofLayersPerStation[5] = 5; fNofBananasPerLayer[5] = 12;
  fNofLayersPerStation[6] = 5; fNofBananasPerLayer[6] = 12;
  fNofLayersPerStation[7] = 5; fNofBananasPerLayer[7] = 12;
  fNofLayersPerStation[8] = 4; fNofBananasPerLayer[8] = 15;
  fNofLayersPerStation[9] = 4; fNofBananasPerLayer[9] = 15;
  fNofLayersPerStation[10] = 4; fNofBananasPerLayer[10] = 15;
  fNofLayersPerStation[11] = 4; fNofBananasPerLayer[11] = 16;

  fgPhotocathode = new TF1("fgPhotocathode", "[0]+ TMath::Power(x, -1)*[1]+TMath::Power(x, -2)*[2]+TMath::Power(x, -3)*[3]+TMath::Power(x, -4)*[4]+TMath::Power(x, -5)*[5]+TMath::Power(x, -6)*[6]", 0., 30.e-6); // reliable between 2.15 and 4.34 eV

  fgPhotocathode->SetParameter( 0, -1.43984e+03 );
  fgPhotocathode->SetParameter( 1,  1.63376e-02 );
  fgPhotocathode->SetParameter( 2, -6.14292e-08 );
  fgPhotocathode->SetParameter( 3,  4.23202e-14 );
  fgPhotocathode->SetParameter( 4,  2.93878e-19 );
  fgPhotocathode->SetParameter( 5, -7.56440e-25 );
  fgPhotocathode->SetParameter( 6,  5.51569e-31 );


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////
  /////////          FROM MONTECARLO
  /////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////


   // All LAV geometrical data are from:
  // - Niels Doble's K12HIKA+ file of 2011/02/11 <http://doble.home.cern.ch/doble/k12hika+.txt>
  // - LAV drawings available in september 2010

  // LAV Responsibility Regions position of front face
  fLAV_RR_ZofFrontFace[0] = 111.595*m;
  fLAV_RR_ZofFrontFace[1] = 191.941*m;
  fLAV_RR_ZofFrontFace[2] = 202.334*m;
  fLAV_RR_ZofFrontFace[3] = 216.760*m;
  fLAV_RR_ZofFrontFace[4] = 238.200*m;

  // LAV Responsibility Regions position of back face
  fLAV_RR_ZofBackFace[0] = 181.115*m;
  fLAV_RR_ZofBackFace[1] = 193.371*m;
  fLAV_RR_ZofBackFace[2] = 203.764*m;
  fLAV_RR_ZofBackFace[3] = 218.190*m;
  fLAV_RR_ZofBackFace[4] = 238.800*m;

  // LAV Responsibility Regions radius
  fLAV_RR_Radius[0] = 2000.*mm;
  fLAV_RR_Radius[1] = 2000.*mm;
  fLAV_RR_Radius[2] = 2000.*mm;
  fLAV_RR_Radius[3] = 2000.*mm;
  fLAV_RR_Radius[4] = 2000.*mm;

  // Register all responsibility regions
  //for(Int_t iRR=0;iRR<NUMBER_OF_RESPONSIBILITY_REGIONS;iRR++) {
  //  fResponsibilityRegion.push_back(new ResponsibilityRegion(fLAV_RR_ZofFrontFace[iRR],fLAV_RR_ZofBackFace[iRR]));
  //}

  // Beam pipe
  //  fBeamPipeZLength[0] = fLAV_RR_ZofBackFace[4] - fLAV_RR_ZofFrontFace[4];
  //  fBeamPipeZPosition[0] = 0.;
  //fBeamPipeInnerRadius[0] = 84.0*mm;
  //fBeamPipeOuterRadius[0] = 85.0*mm;
  //fBeamPipeFinZLength[0] = 5.0*mm;
  //fBeamPipeFinOuterRadius[0] = 89.0*mm;
  //fBeamPipeFinSpacing[0] = 40.0*mm;
  //fBeamPipeInputDisplacementWRTBeam[0]=0;
  //fBeamPipeOutputDisplacementWRTBeam[0]=0;
  //fBeamPipeZLengthWFins[0] = fBeamPipeZLength[0];

  // LAV Station responsibility region
  //fLAV_Station_ResponsibilityRegion[0]  = 0;
  //fLAV_Station_ResponsibilityRegion[1]  = 0;
  //fLAV_Station_ResponsibilityRegion[2]  = 0;
  //fLAV_Station_ResponsibilityRegion[3]  = 0;
  //fLAV_Station_ResponsibilityRegion[4]  = 0;
  //fLAV_Station_ResponsibilityRegion[5]  = 0;
  //fLAV_Station_ResponsibilityRegion[6]  = 0;
  //fLAV_Station_ResponsibilityRegion[7]  = 0;
  //fLAV_Station_ResponsibilityRegion[8]  = 1;
  //fLAV_Station_ResponsibilityRegion[9]  = 2;
  //fLAV_Station_ResponsibilityRegion[10] = 3;
  //fLAV_Station_ResponsibilityRegion[11] = 4;

  // LAV Station position of front face ("world" reference frame)
  fLAV_Station_ZofFrontFace[0]  = 120.595*m;
  fLAV_Station_ZofFrontFace[1]  = 128.205*m;
  fLAV_Station_ZofFrontFace[2]  = 135.815*m;
  fLAV_Station_ZofFrontFace[3]  = 143.425*m;
  fLAV_Station_ZofFrontFace[4]  = 151.035*m;
  fLAV_Station_ZofFrontFace[5]  = 164.545*m;
  fLAV_Station_ZofFrontFace[6]  = 172.055*m;
  fLAV_Station_ZofFrontFace[7]  = 179.565*m;
  fLAV_Station_ZofFrontFace[8]  = 191.941*m;
  fLAV_Station_ZofFrontFace[9]  = 202.334*m;
  fLAV_Station_ZofFrontFace[10] = 216.760*m;
  fLAV_Station_ZofFrontFace[11] = 238.200*m;

  // LAV Station position of back face ("world" reference frame)
  fLAV_Station_ZofBackFace[0]  = 122.145*m;
  fLAV_Station_ZofBackFace[1]  = 129.755*m;
  fLAV_Station_ZofBackFace[2]  = 137.365*m;
  fLAV_Station_ZofBackFace[3]  = 144.975*m;
  fLAV_Station_ZofBackFace[4]  = 152.585*m;
  fLAV_Station_ZofBackFace[5]  = 166.095*m;
  fLAV_Station_ZofBackFace[6]  = 173.605*m;
  fLAV_Station_ZofBackFace[7]  = 181.115*m;
  fLAV_Station_ZofBackFace[8]  = 193.371*m;
  fLAV_Station_ZofBackFace[9]  = 203.764*m;
  fLAV_Station_ZofBackFace[10] = 218.190*m;
  fLAV_Station_ZofBackFace[11] = 238.800*m;

  // LAV Station inner radius
  fLAV_Station_InnerRadius[0]  =  532.*mm;
  fLAV_Station_InnerRadius[1]  =  532.*mm;
  fLAV_Station_InnerRadius[2]  =  532.*mm;
  fLAV_Station_InnerRadius[3]  =  532.*mm;
  fLAV_Station_InnerRadius[4]  =  532.*mm;
  fLAV_Station_InnerRadius[5]  =  772.*mm;
  fLAV_Station_InnerRadius[6]  =  772.*mm;
  fLAV_Station_InnerRadius[7]  =  772.*mm;
  fLAV_Station_InnerRadius[8]  =  972.*mm;
  fLAV_Station_InnerRadius[9]  =  972.*mm;
  fLAV_Station_InnerRadius[10] =  972.*mm;
  fLAV_Station_InnerRadius[11] = 1072.*mm;

  // LAV Station outer radius
  fLAV_Station_OuterRadius[0]  = 1110.*mm;
  fLAV_Station_OuterRadius[1]  = 1110.*mm;
  fLAV_Station_OuterRadius[2]  = 1110.*mm;
  fLAV_Station_OuterRadius[3]  = 1110.*mm;
  fLAV_Station_OuterRadius[4]  = 1110.*mm;
  fLAV_Station_OuterRadius[5]  = 1368.*mm;
  fLAV_Station_OuterRadius[6]  = 1368.*mm;
  fLAV_Station_OuterRadius[7]  = 1368.*mm;
  fLAV_Station_OuterRadius[8]  = 1518.*mm;
  fLAV_Station_OuterRadius[9]  = 1518.*mm;
  fLAV_Station_OuterRadius[10] = 1518.*mm;
  fLAV_Station_OuterRadius[11] = 1675.*mm;

  // LAV Station length along Z
  fLAV_Station_ZLength[0]  = 1550.*mm;
  fLAV_Station_ZLength[1]  = 1550.*mm;
  fLAV_Station_ZLength[2]  = 1550.*mm;
  fLAV_Station_ZLength[3]  = 1550.*mm;
  fLAV_Station_ZLength[4]  = 1550.*mm;
  fLAV_Station_ZLength[5]  = 1550.*mm;
  fLAV_Station_ZLength[6]  = 1550.*mm;
  fLAV_Station_ZLength[7]  = 1550.*mm;
  fLAV_Station_ZLength[8]  = 1430.*mm;
  fLAV_Station_ZLength[9]  = 1430.*mm;
  fLAV_Station_ZLength[10] = 1430.*mm;
  fLAV_Station_ZLength[11] =  800.*mm;

  // Steel vessel outer radius
  fLAV_Vessel_OuterRadius[0]  = 1110.*mm;
  fLAV_Vessel_OuterRadius[1]  = 1110.*mm;
  fLAV_Vessel_OuterRadius[2]  = 1110.*mm;
  fLAV_Vessel_OuterRadius[3]  = 1110.*mm;
  fLAV_Vessel_OuterRadius[4]  = 1110.*mm;
  fLAV_Vessel_OuterRadius[5]  = 1368.*mm;
  fLAV_Vessel_OuterRadius[6]  = 1368.*mm;
  fLAV_Vessel_OuterRadius[7]  = 1368.*mm;
  fLAV_Vessel_OuterRadius[8]  = 1518.*mm;
  fLAV_Vessel_OuterRadius[9]  = 1518.*mm;
  fLAV_Vessel_OuterRadius[10] = 1518.*mm;
  fLAV_Vessel_OuterRadius[11] = 1675.*mm;

  // Steel vessel inner radius
  fLAV_Vessel_InnerRadius[0]  = 1054.*mm;
  fLAV_Vessel_InnerRadius[1]  = 1054.*mm;
  fLAV_Vessel_InnerRadius[2]  = 1054.*mm;
  fLAV_Vessel_InnerRadius[3]  = 1054.*mm;
  fLAV_Vessel_InnerRadius[4]  = 1054.*mm;
  fLAV_Vessel_InnerRadius[5]  = 1312.*mm;
  fLAV_Vessel_InnerRadius[6]  = 1312.*mm;
  fLAV_Vessel_InnerRadius[7]  = 1312.*mm;
  fLAV_Vessel_InnerRadius[8]  = 1462.*mm;
  fLAV_Vessel_InnerRadius[9]  = 1462.*mm;
  fLAV_Vessel_InnerRadius[10] = 1462.*mm;
  fLAV_Vessel_InnerRadius[11] = 1619.*mm;

  // Steel vessel thickness (to be checked)
  fLAV_Vessel_Thickness[0]  = 20.*mm;
  fLAV_Vessel_Thickness[1]  = 20.*mm;
  fLAV_Vessel_Thickness[2]  = 20.*mm;
  fLAV_Vessel_Thickness[3]  = 20.*mm;
  fLAV_Vessel_Thickness[4]  = 20.*mm;
  fLAV_Vessel_Thickness[5]  = 20.*mm;
  fLAV_Vessel_Thickness[6]  = 20.*mm;
  fLAV_Vessel_Thickness[7]  = 20.*mm;
  fLAV_Vessel_Thickness[8]  = 20.*mm;
  fLAV_Vessel_Thickness[9]  = 20.*mm;
  fLAV_Vessel_Thickness[10] = 20.*mm;
  fLAV_Vessel_Thickness[11] = 20.*mm;

  // Z position of first ring wrt front face of vessel
  fLAV_Station_FirstRingZPos[0]  = 768.*mm;
  fLAV_Station_FirstRingZPos[1]  = 768.*mm;
  fLAV_Station_FirstRingZPos[2]  = 768.*mm;
  fLAV_Station_FirstRingZPos[3]  = 768.*mm;
  fLAV_Station_FirstRingZPos[4]  = 768.*mm;
  fLAV_Station_FirstRingZPos[5]  = 768.*mm;
  fLAV_Station_FirstRingZPos[6]  = 768.*mm;
  fLAV_Station_FirstRingZPos[7]  = 768.*mm;
  fLAV_Station_FirstRingZPos[8]  = 768.*mm;
  fLAV_Station_FirstRingZPos[9]  = 768.*mm;
  fLAV_Station_FirstRingZPos[10] = 768.*mm;
  fLAV_Station_FirstRingZPos[11] = 230.*mm;

  // Number of rings in LAV station
  fLAV_Station_NRings[0]  = 5;
  fLAV_Station_NRings[1]  = 5;
  fLAV_Station_NRings[2]  = 5;
  fLAV_Station_NRings[3]  = 5;
  fLAV_Station_NRings[4]  = 5;
  fLAV_Station_NRings[5]  = 5;
  fLAV_Station_NRings[6]  = 5;
  fLAV_Station_NRings[7]  = 5;
  fLAV_Station_NRings[8]  = 4;
  fLAV_Station_NRings[9]  = 4;
  fLAV_Station_NRings[10] = 4;
  fLAV_Station_NRings[11] = 4;

  // Number of blocks in each ring
  fLAV_Station_NBlocksPerRing[0]  = 32;
  fLAV_Station_NBlocksPerRing[1]  = 32;
  fLAV_Station_NBlocksPerRing[2]  = 32;
  fLAV_Station_NBlocksPerRing[3]  = 32;
  fLAV_Station_NBlocksPerRing[4]  = 32;
  fLAV_Station_NBlocksPerRing[5]  = 48;
  fLAV_Station_NBlocksPerRing[6]  = 48;
  fLAV_Station_NBlocksPerRing[7]  = 48;
  fLAV_Station_NBlocksPerRing[8]  = 60;
  fLAV_Station_NBlocksPerRing[9]  = 60;
  fLAV_Station_NBlocksPerRing[10] = 60;
  fLAV_Station_NBlocksPerRing[11] = 64;

  // Number of bananas in each ring
  fLAV_Station_NBananasPerRing[0]  = 8;
  fLAV_Station_NBananasPerRing[1]  = 8;
  fLAV_Station_NBananasPerRing[2]  = 8;
  fLAV_Station_NBananasPerRing[3]  = 8;
  fLAV_Station_NBananasPerRing[4]  = 8;
  fLAV_Station_NBananasPerRing[5]  = 12;
  fLAV_Station_NBananasPerRing[6]  = 12;
  fLAV_Station_NBananasPerRing[7]  = 12;
  fLAV_Station_NBananasPerRing[8]  = 15;
  fLAV_Station_NBananasPerRing[9]  = 15;
  fLAV_Station_NBananasPerRing[10] = 15;
  fLAV_Station_NBananasPerRing[11] = 16;

  // Banana type used in station
  fLAV_Station_BananaType[0]  = 0;
  fLAV_Station_BananaType[1]  = 0;
  fLAV_Station_BananaType[2]  = 0;
  fLAV_Station_BananaType[3]  = 0;
  fLAV_Station_BananaType[4]  = 0;
  fLAV_Station_BananaType[5]  = 1;
  fLAV_Station_BananaType[6]  = 1;
  fLAV_Station_BananaType[7]  = 1;
  fLAV_Station_BananaType[8]  = 2;
  fLAV_Station_BananaType[9]  = 2;
  fLAV_Station_BananaType[10] = 2;
  fLAV_Station_BananaType[11] = 3;

  // Gap between rings
  fLAV_Station_RingGap[0]  = 1.0*cm;
  fLAV_Station_RingGap[1]  = 1.0*cm;
  fLAV_Station_RingGap[2]  = 1.0*cm;
  fLAV_Station_RingGap[3]  = 1.0*cm;
  fLAV_Station_RingGap[4]  = 1.0*cm;
  fLAV_Station_RingGap[5]  = 1.0*cm;
  fLAV_Station_RingGap[6]  = 1.0*cm;
  fLAV_Station_RingGap[7]  = 1.0*cm;
  fLAV_Station_RingGap[8]  = 1.0*cm;
  fLAV_Station_RingGap[9]  = 1.0*cm;
  fLAV_Station_RingGap[10] = 1.0*cm;
  fLAV_Station_RingGap[11] = 1.0*cm;

  // Phi reference point for banana 0 (phi at center of banana)
  fLAV_Station_PhiReference[0]  = 99.0*deg;
  fLAV_Station_PhiReference[1]  = 99.0*deg;
  fLAV_Station_PhiReference[2]  = 99.0*deg;
  fLAV_Station_PhiReference[3]  = 99.0*deg;
  fLAV_Station_PhiReference[4]  = 99.0*deg;
  fLAV_Station_PhiReference[5]  = 99.0*deg;
  fLAV_Station_PhiReference[6]  = 99.0*deg;
  fLAV_Station_PhiReference[7]  = 99.0*deg;
  fLAV_Station_PhiReference[8]  = 99.0*deg;
  fLAV_Station_PhiReference[9]  = 99.0*deg;
  fLAV_Station_PhiReference[10] = 99.0*deg;
  fLAV_Station_PhiReference[11] = 99.0*deg;

  // Phi rotation between consecutive layers
  fLAV_Station_PhiRotationBetweenLayers[0]  = -2.25*deg; // 360deg/32blocks/5layers
  fLAV_Station_PhiRotationBetweenLayers[1]  = -2.25*deg; // 360deg/32blocks/5layers
  fLAV_Station_PhiRotationBetweenLayers[2]  = -2.25*deg; // 360deg/32blocks/5layers
  fLAV_Station_PhiRotationBetweenLayers[3]  = -2.25*deg; // 360deg/32blocks/5layers
  fLAV_Station_PhiRotationBetweenLayers[4]  = -2.25*deg; // 360deg/32blocks/5layers
  fLAV_Station_PhiRotationBetweenLayers[5]  = -1.50*deg; // 360deg/48blocks/5layers
  fLAV_Station_PhiRotationBetweenLayers[6]  = -1.50*deg; // 360deg/48blocks/5layers
  fLAV_Station_PhiRotationBetweenLayers[7]  = -1.50*deg; // 360deg/48blocks/5layers
  fLAV_Station_PhiRotationBetweenLayers[8]  = -1.50*deg; // 360deg/60blocks/4layers
  fLAV_Station_PhiRotationBetweenLayers[9]  = -1.50*deg; // 360deg/60blocks/4layers
  fLAV_Station_PhiRotationBetweenLayers[10] = -1.50*deg; // 360deg/60blocks/4layers
  fLAV_Station_PhiRotationBetweenLayers[11] = -1.40625*deg; // 360deg/64blocks/4layers

  // Opal id of different blocks
  fBlockOpalId[0] =  7;
  fBlockOpalId[1] =  8;
  fBlockOpalId[2] =  9;
  fBlockOpalId[3] = 10;
  fBlockOpalId[4] = 11;
  fBlockOpalId[5] = 12;
  fBlockOpalId[6] = 13;
  fBlockOpalId[7] = 14;
  fBlockOpalId[8] = 15;

  // Block OPAL 7
  fBlockZLength[0]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[0]      = 109.526*mm; // L1 length (long base of back face)
  fBlockL2Length[0]      = 107.003*mm; // L2 length (short base of back face)
  fBlockL3Length[0]      =  97.575*mm; // L3 length (long base of front face)
  fBlockL4Length[0]      =  95.318*mm; // L4 length (short base of front face)
  fBlockW1Length[0]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[0]      =  98.250*mm; // W2 length (height of front face)
  fLightGuideZLength[0]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[0] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[0]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[0]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[0]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[0] =  15.000*mm; // Steel slab thickness

  // Block OPAL 8
  fBlockZLength[1]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[1]      = 109.824*mm; // L1 length (long base of back face)
  fBlockL2Length[1]      = 107.536*mm; // L2 length (short base of back face)
  fBlockL3Length[1]      =  97.346*mm; // L3 length (long base of front face)
  fBlockL4Length[1]      =  95.310*mm; // L4 length (short base of front face)
  fBlockW1Length[1]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[1]      =  97.740*mm; // W2 length (height of front face)
  fLightGuideZLength[1]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[1] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[1]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[1]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[1]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[1] =  15.000*mm; // Steel slab thickness

  // Block OPAL 9
  fBlockZLength[2]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[2]      = 110.073*mm; // L1 length (long base of back face)
  fBlockL2Length[2]      = 108.040*mm; // L2 length (short base of back face)
  fBlockL3Length[2]      =  97.103*mm; // L3 length (long base of front face)
  fBlockL4Length[2]      =  95.302*mm; // L4 length (short base of front face)
  fBlockW1Length[2]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[2]      =  97.261*mm; // W2 length (height of front face)
  fLightGuideZLength[2]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[2] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[2]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[2]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[2]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[2] =  15.000*mm; // Steel slab thickness

  // Block OPAL 10
  fBlockZLength[3]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[3]      = 110.266*mm; // L1 length (long base of back face)
  fBlockL2Length[3]      = 108.506*mm; // L2 length (short base of back face)
  fBlockL3Length[3]      =  96.846*mm; // L3 length (long base of front face)
  fBlockL4Length[3]      =  95.294*mm; // L4 length (short base of front face)
  fBlockW1Length[3]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[3]      =  96.820*mm; // W2 length (height of front face)
  fLightGuideZLength[3]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[3] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[3]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[3]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[3]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[3] =  15.000*mm; // Steel slab thickness

  // Block OPAL 11
  fBlockZLength[4]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[4]      = 110.391*mm; // L1 length (long base of back face)
  fBlockL2Length[4]      = 108.923*mm; // L2 length (short base of back face)
  fBlockL3Length[4]      =  96.577*mm; // L3 length (long base of front face)
  fBlockL4Length[4]      =  95.288*mm; // L4 length (short base of front face)
  fBlockW1Length[4]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[4]      =  96.426*mm; // W2 length (height of front face)
  fLightGuideZLength[4]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[4] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[4]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[4]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[4]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[4] =  15.000*mm; // Steel slab thickness

  // Block OPAL 12
  fBlockZLength[5]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[5]      = 110.439*mm; // L1 length (long base of back face)
  fBlockL2Length[5]      = 109.281*mm; // L2 length (short base of back face)
  fBlockL3Length[5]      =  96.298*mm; // L3 length (long base of front face)
  fBlockL4Length[5]      =  95.284*mm; // L4 length (short base of front face)
  fBlockW1Length[5]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[5]      =  96.086*mm; // W2 length (height of front face)
  fLightGuideZLength[5]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[5] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[5]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[5]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[5]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[5] =  15.000*mm; // Steel slab thickness

  // Block OPAL 13
  fBlockZLength[6]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[6]      = 110.366*mm; // L1 length (long base of back face)
  fBlockL2Length[6]      = 109.572*mm; // L2 length (short base of back face)
  fBlockL3Length[6]      =  96.010*mm; // L3 length (long base of front face)
  fBlockL4Length[6]      =  95.317*mm; // L4 length (short base of front face)
  fBlockW1Length[6]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[6]      =  95.808*mm; // W2 length (height of front face)
  fLightGuideZLength[6]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[6] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[6]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[6]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[6]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[6] =  15.000*mm; // Steel slab thickness

  // Block OPAL 14
  fBlockZLength[7]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[7]      = 110.204*mm; // L1 length (long base of back face)
  fBlockL2Length[7]      = 109.786*mm; // L2 length (short base of back face)
  fBlockL3Length[7]      =  95.717*mm; // L3 length (long base of front face)
  fBlockL4Length[7]      =  95.353*mm; // L4 length (short base of front face)
  //// Enable for testing purpose only!
  //fBlockL2Length[7]      =  90.000*mm; // L2 length (short base of back face)
  //fBlockL3Length[7]      =  80.000*mm; // L3 length (long base of front face)
  //fBlockL4Length[7]      =  62.434*mm; // L4 length (short base of front face)
  ////
  fBlockW1Length[7]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[7]      =  95.598*mm; // W2 length (height of front face)
  fLightGuideZLength[7]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[7] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[7]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[7]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[7]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[7] =  15.000*mm; // Steel slab thickness

  // Block OPAL 15
  fBlockZLength[8]       = 370.000*mm; // Z length (PbGl only)
  fBlockL1Length[8]      = 109.952*mm; // L1 length (long base of back face)
  fBlockL2Length[8]      = 109.918*mm; // L2 length (short base of back face)
  fBlockL3Length[8]      =  95.420*mm; // L3 length (long base of front face)
  fBlockL4Length[8]      =  95.391*mm; // L4 length (short base of front face)
  fBlockW1Length[8]      = 109.952*mm; // W1 length (height of back face)
  fBlockW2Length[8]      =  95.461*mm; // W2 length (height of front face)
  fLightGuideZLength[8]  =  60.000*mm; // Light Guide Z length
  fLightGuideDiameter[8] =  73.000*mm; // Light Guide diameter
  fMuMetalZLength[8]     = 120.800*mm; // u-metal cylinder Z length
  fMuMetalDiameter[8]    =  83.800*mm; // u-metal cylinder external diameter
  fMuMetalThickness[8]   =   2.100*mm; // u-metal cylinder thickness
  fSteelSlabThickness[8] =  15.000*mm; // Steel slab thickness

  // Map of crystal type (OPAL id) for each station/ring

  // Station 1
  fLAV_TypeOfCrystal[0][0] = 15;
  fLAV_TypeOfCrystal[0][1] = 15;
  fLAV_TypeOfCrystal[0][2] = 15;
  fLAV_TypeOfCrystal[0][3] = 15;
  fLAV_TypeOfCrystal[0][4] = 15;

  // Station 2
  fLAV_TypeOfCrystal[1][0] = 14;
  fLAV_TypeOfCrystal[1][1] = 14;
  fLAV_TypeOfCrystal[1][2] = 14;
  fLAV_TypeOfCrystal[1][3] = 14;
  fLAV_TypeOfCrystal[1][4] = 14;

  // Station 3
  fLAV_TypeOfCrystal[2][0] = 11;
  fLAV_TypeOfCrystal[2][1] = 11;
  fLAV_TypeOfCrystal[2][2] = 11;
  fLAV_TypeOfCrystal[2][3] = 11;
  fLAV_TypeOfCrystal[2][4] = 14;

  // Station 4
  fLAV_TypeOfCrystal[3][0] = 13;
  fLAV_TypeOfCrystal[3][1] = 13;
  fLAV_TypeOfCrystal[3][2] = 13;
  fLAV_TypeOfCrystal[3][3] = 13;
  fLAV_TypeOfCrystal[3][4] = 14;

  // Station 5
  fLAV_TypeOfCrystal[4][0] = 15;
  fLAV_TypeOfCrystal[4][1] = 15;
  fLAV_TypeOfCrystal[4][2] = 15;
  fLAV_TypeOfCrystal[4][3] = 15;
  fLAV_TypeOfCrystal[4][4] = 15;

  // Station 6
  fLAV_TypeOfCrystal[5][0] = 13;
  fLAV_TypeOfCrystal[5][1] = 13;
  fLAV_TypeOfCrystal[5][2] = 13;
  fLAV_TypeOfCrystal[5][3] = 13;
  fLAV_TypeOfCrystal[5][4] = 13;

  // Station 7
  fLAV_TypeOfCrystal[6][0] = 14;
  fLAV_TypeOfCrystal[6][1] = 14;
  fLAV_TypeOfCrystal[6][2] = 14;
  fLAV_TypeOfCrystal[6][3] = 14;
  fLAV_TypeOfCrystal[6][4] = 14;

  // Station 8 (some rings will use type 11)
  fLAV_TypeOfCrystal[7][0] = 15;
  fLAV_TypeOfCrystal[7][1] = 15;
  fLAV_TypeOfCrystal[7][2] = 15;
  fLAV_TypeOfCrystal[7][3] = 15;
  fLAV_TypeOfCrystal[7][4] = 15;

  // Station 9
  fLAV_TypeOfCrystal[8][0] = 12;
  fLAV_TypeOfCrystal[8][1] = 12;
  fLAV_TypeOfCrystal[8][2] = 12;
  fLAV_TypeOfCrystal[8][3] = 12;
  fLAV_TypeOfCrystal[8][4] =  0; // This ring does not exist

  // Station 10
  fLAV_TypeOfCrystal[9][0] = 10;
  fLAV_TypeOfCrystal[9][1] = 10;
  fLAV_TypeOfCrystal[9][2] = 10;
  fLAV_TypeOfCrystal[9][3] = 10;
  fLAV_TypeOfCrystal[9][4] =  0; // This ring does not exist

  // Station 11
  fLAV_TypeOfCrystal[10][0] = 9;
  fLAV_TypeOfCrystal[10][1] = 9;
  fLAV_TypeOfCrystal[10][2] = 9;
  fLAV_TypeOfCrystal[10][3] = 9;
  fLAV_TypeOfCrystal[10][4] = 0; // This ring does not exist

  // Station 12 (some rings will use type 8)
  fLAV_TypeOfCrystal[11][0] = 7;
  fLAV_TypeOfCrystal[11][1] = 7;
  fLAV_TypeOfCrystal[11][2] = 7;
  fLAV_TypeOfCrystal[11][3] = 7;
  fLAV_TypeOfCrystal[11][4] = 0; // This ring does not exist

  // Define some quantities which are the same for all banana types
  for (Int_t iB=0;iB<NUMBER_OF_BANANA_SHAPES;iB++) {

    fBananaNBlocks[iB] = 4; // Number of blocks in banana

    fAlSlabThickness[iB]  = 10.0*mm; // Thickness of aluminum slabs
    fAlColumnLength[iB]   = 90.0*mm; // Length of aluminum columns
    fAlColumnDiameter[iB] = 12.0*mm; // Diameter of aluminum columns

    fBananaThickness[iB] = fAlColumnLength[iB]+2.*fAlSlabThickness[iB]; // Thickness of banana at aluminum slab

    // Banana thickness tolerance
    // Crystal side L1 is sometimes very slightly longer than
    // the banana thickness. We add this tolerance on the two sides of the
    // banana to keep this into account
    fBananaThicknessTolerance[iB] = 0.3*mm;

  }

  // Banana type 0 (Stations 1-5)
  fBananaPhiSpan[0]     =  45.*deg;  // 360deg/8
  fBananaOuterRadius[0] = 1054.*mm;  // External radius of banana (at vessel internal surface)
  fBananaInnerRadius[0] =  532.*mm;  // Internal radius of banana (circle tangent to block front face)

  // Banana type 1 (Stations 6-8)
  fBananaPhiSpan[1]     =  30.*deg;  // 360deg/12
  fBananaOuterRadius[1] = 1348.*mm;  // External radius of banana (at vessel internal surface)
  fBananaInnerRadius[1] =  772.*mm;  // Internal radius of banana (circle tangent to block front face)

}

LAVGeometry::~LAVGeometry(){
  if(fNofLayersPerStation)  delete [] fNofLayersPerStation;
  if(fNofBananasPerLayer)   delete [] fNofBananasPerLayer;

  if(fgPhotocathode) delete fgPhotocathode;
}

void LAVGeometry::Print(){

  //GetNumberOfStations()
  //GetNumberOfLayers(Int_t Station)  { return fNofLayersPerStation[Station]; }
  //GetNumberOfBananas(Int_t Station) { return fNofBananasPerLayer[Station];  }
  //Int_t* GetNumberOfLayers()   { return fNofLayersPerStation; }
  //Int_t* GetNumberOfBananas()    { return fNofBananasPerLayer ;  }

  std::cout << "GetTimeBinLenght \t"			    << GetBinWidth()     		   << std::endl
	    << "GetNbins \t"       			    << GetNbins()       	      	   << std::endl
	    << "GetQuantumEfficiency \t"		    << GetQuantumEfficiency()		   << std::endl
	    << "GetNdynodes \t"			    << GetNdynodes()			   << std::endl
	    << "GetFirstDynodeCollectionEfficiency \t"   << GetFirstDynodeCollectionEfficiency()<< std::endl
	    << "GetDynodeCollectionEfficiency \t"	    << GetDynodeCollectionEfficiency()	   << std::endl
	    << "GetTransitTime \t"			    << GetTransitTime()			   << std::endl
	    << "GetTransitTimeFirstDynode \t"	    << GetTransitTime_FirstDynode()    	   << std::endl
	    << "GetTransitTimeSpread \t"		    << GetTransitTimeSpread()		   << std::endl
	    << "GetTransitTimeSpreadFirstDynode \t"	    << GetTransitTimeSpread_FirstDynode()  << std::endl
	    << "GetGain \t"				    << GetGain()		     	   << std::endl
	    << "GetAlpha \t"				    << GetAlpha()		    	   << std::endl
	    << "GetDynodeGain \t"			    << GetDynodeGain()			   << std::endl
	    << "GetFirstDynodeGain \t"		    << GetFirstDynodeGain()		   << std::endl
	    << "GetPMTCapacitance \t"	     	    << GetPMTCapacitance()		   << std::endl
	    << "GetPMTImpedence \t"                      << GetPMTImpedence()                   << std::endl
	    << "GetChargeNorm \t"                        << GetChargeNormalization()            << std::endl;

}


Int_t LAVGeometry::GetBlockIdFromOpalId(Int_t opalId)
{
  for(Int_t id=0;id<NUMBER_OF_BLOCK_SHAPES;id++) {
    if ( fBlockOpalId[id] == opalId ) { return id; }
  }
  return -1;
}



Double_t LAVGeometry::GetBlockPhi(const Int_t chid){

  LAVChannelID* Name = new LAVChannelID();
  Name->DecodeChannelID(chid);
  Int_t b  = Name->GetBlockID();
  Int_t BB = Name->GetBananaID();
  Int_t L  = Name->GetLayerID();
  Int_t SS = Name->GetLAVID()-1; // geometry wants station numbering from 0
  delete Name;

   Int_t BID = GetStationBananaType(SS);

   Double_t Phi = 0
      // VVV phibanana VVV
      + GetStationPhiReference(SS)
      + L * GetStationPhiRotationBetweenLayers(SS)
      + BB*360.*deg / GetStationNBananasPerRing(SS)
      // VVV phiblock VVV
      - 0.5 * GetBananaPhiSpan(BID)
      + (b+0.5) * GetBananaPhiSpan(BID) / GetBananaNBlocks(BID);
   return Phi;

}

TVector3 LAVGeometry::GetBlockXVersor(const Int_t){

   return TVector3(0,0,-1);

}

TVector3 LAVGeometry::GetBlockYVersor(const Int_t chid){

   //Double_t Phi = GetBlockPhi(chid);
   //return TVector3(sin(Phi),-cos(Phi),0);
  TVector3 zver = GetBlockZVersor(chid).Cross(GetBlockXVersor(chid));
  zver = zver* (1/zver.Mag());
  return zver;

}

TVector3 LAVGeometry::GetBlockZVersor(const Int_t chid){

   Double_t Phi = GetBlockPhi(chid);
   return TVector3( -TMath::Cos(Phi), -TMath::Sin(Phi), 0 );

}

TVector3 LAVGeometry::GetLeadglassCenter(const Int_t chid){

  LAVChannelID* Name = new LAVChannelID();
  Name->DecodeChannelID(chid);

  //Int_t b  = Name->GetBlockID();
  //Int_t BB = Name->GetBananaID();
  Int_t L  = Name->GetLayerID();
  Int_t SS = Name->GetLAVID()-1; // geometry wants station numbering from 0
  delete Name;

   Int_t BID = GetStationBananaType(SS);

   TVector3 xver = GetBlockXVersor(chid);
   TVector3 zver = GetBlockZVersor(chid);

   TVector3 center;
   center  = -zver * (GetStationInnerRadius(SS) );
   center += -zver * (GetBlockZLength(BID) / 2 );
   center += -xver * (GetStationZofFrontFace(SS) );
   center += -xver * (GetStationFirstRingZPos(SS) );
   center += -xver * (GetBananaThickness(BID) / 2 );
   center += -xver * (L * GetBananaThickness(BID) );
   center += -xver * (L * GetStationRingGap(SS) );

   return center;

}

