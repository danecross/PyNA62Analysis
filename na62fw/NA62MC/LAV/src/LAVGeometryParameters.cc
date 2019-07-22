// LAVGeometryParameters.cc
// --------------------------------------------------------------
// History:
//
// 2019-06-11 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added parameters for displacement of central part of banana volume
//     to avoid block/banana overlaps
// 2019-04-24 Zuzana Kucerova (zuzana.kucerova&cern.ch)
//   - Fixed geometry conflict with STRAW RR0
// 2017-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Fixed order of vertices for extruded solids (must be anti-clockwise)
// 2015-05-01 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Enlarged responsibility regions
//   - Added blue tube structure
// 2015-03-19 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Fixed A12 responsibility region to include the full A12 structure
//   - Adjusted block position to "as build" parameters
//   - Define parameters for A6-A8 Aluminum slab
// 2012-01-30 - Giuseppe Ruggiero
//   - LAV reponsibitlity region and Z positions changed 
//     according to the 2011-11-02 beatch file
// 2011-01-24 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Different sensitive detector names
//   - Added matrix file path
//   - Added some wrapping and phototube properties
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added banana structure (slabs, columns, c-shapes)
//   - Fixed PhiRotationBetweenLayers for stations>5
// 2010-11-10 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Cleaned variables and methods names and logic
//   - Updated responsibility regions
//   - Updated LAV12 geometry
// 2010-11-03 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Updated LAV stations positions
//   - Blocks geometry is now correct
//   - Added (part of) block support structure
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it) 
//   - First implementation of LAV geometry
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------

#include "TVector.h"
#include "G4TwoVector.hh"
#include "LAVGeometryParameters.hh"
#include "DetectorParameter.hh"
#include "NA62ConditionsService.hh"

LAVGeometryParameters* LAVGeometryParameters::fInstance = 0;

LAVGeometryParameters::LAVGeometryParameters() : NA62VGeometryParameters(G4String("LAV"))  {

  // Set name of LAV sensitive detector and hit collection
  fLAVFastSensitiveDetectorName = "/LAVFast";
  fLAVLeadglassSensitiveDetectorName = "/LAVLeadglass";
  fLAVGuideSensitiveDetectorName = "/LAVGuide";
  fLAVCathodeSensitiveDetectorName = "/LAVCathode";
  fLAVCollectionName = "LAVCollection";

  // Define all the geometrical parameters and build the
  // responsibility region accordingly

  // Unused World Parameters for stand-alone configurations
  fWorldZLength = 22.*m;
  fWorldXLength = 10.*m;
  fWorldYLength = 10.*m;

  // All LAV geometrical data are from "as-built" LAV drawings

  // LAV Responsibility Regions position of front face
  fLAV_RR_ZofFrontFace[0] = 104.458*m;
  //fLAV_RR_ZofFrontFace[1] = 191.941*m;
  fLAV_RR_ZofFrontFace[1] = 183.705*m; // Enlarged down to STRAW-01
  //fLAV_RR_ZofFrontFace[2] = 202.334*m;
  fLAV_RR_ZofFrontFace[2] = 198.070*m; // Enlarged down to Spectrometer Magnet
  //fLAV_RR_ZofFrontFace[3] = 216.760*m;
  fLAV_RR_ZofFrontFace[3] = 204.656*m; // Enlarged down to STRAW-03
  //fLAV_RR_ZofFrontFace[4] = 238.200*m; // Old setting of CHOD-LAV12 border
  //fLAV_RR_ZofFrontFace[4] = 238.150*m;
  fLAV_RR_ZofFrontFace[4] = 238.198*m; // BEATCH 08/03/2016

  // LAV Responsibility Regions position of back face
  //fLAV_RR_ZofBackFace[0] = 181.115*m;
  fLAV_RR_ZofBackFace[0] = 183.310*m; // Enlarged up to STRAW-01
  fLAV_RR_ZofBackFace[1] = 193.371*m;
  fLAV_RR_ZofBackFace[2] = 203.764*m;
  fLAV_RR_ZofBackFace[3] = 218.190*m;
  //fLAV_RR_ZofBackFace[4] = 238.800*m; // Old setting of CHOD-LAV12 border
  //fLAV_RR_ZofBackFace[4] = 238.850*m;
  fLAV_RR_ZofBackFace[4] = 238.898*m; // BEATCH 08/03/2016

  // LAV Responsibility Regions radius
  fLAV_RR_Radius[0] = 2000.*mm;
  fLAV_RR_Radius[1] = 2000.*mm;
  fLAV_RR_Radius[2] = 2000.*mm;
  fLAV_RR_Radius[3] = 2000.*mm;
  fLAV_RR_Radius[4] = 2000.*mm;

  // Register all responsibility regions
  for(G4int iRR=0;iRR<NUMBER_OF_RESPONSIBILITY_REGIONS;iRR++) {
    fResponsibilityRegion.push_back(new ResponsibilityRegion(fLAV_RR_ZofFrontFace[iRR],fLAV_RR_ZofBackFace[iRR]));
  }

  // Blue tube sections parameters
  // Responsibility region
  fLAV_BT_ResponsibilityRegion[0] = 0;
  fLAV_BT_ResponsibilityRegion[1] = 0;
  fLAV_BT_ResponsibilityRegion[2] = 0;
  fLAV_BT_ResponsibilityRegion[3] = 0;
  fLAV_BT_ResponsibilityRegion[4] = 0;
  fLAV_BT_ResponsibilityRegion[5] = 0;
  fLAV_BT_ResponsibilityRegion[6] = 0;
  fLAV_BT_ResponsibilityRegion[7] = 0;
  fLAV_BT_ResponsibilityRegion[8] = 0;
  fLAV_BT_ResponsibilityRegion[9] = 0;
  fLAV_BT_ResponsibilityRegion[10] = 0;
  fLAV_BT_ResponsibilityRegion[11] = 1;
  fLAV_BT_ResponsibilityRegion[12] = 2;
  fLAV_BT_ResponsibilityRegion[13] = 3;
  // Position of front face (world reference system)
  fLAV_BT_ZofFrontFace[ 0] = 105.595*m; // Start of blue tube (BEATCH.203 VAC.O=1920)
  fLAV_BT_ZofFrontFace[ 1] = 122.145*m; // End of A1 (BEATCH.209)
  fLAV_BT_ZofFrontFace[ 2] = 129.755*m; // End of A2 (BEATCH.214)
  fLAV_BT_ZofFrontFace[ 3] = 137.365*m; // End of A3 (BEATCH.219)
  fLAV_BT_ZofFrontFace[ 4] = 144.975*m; // End of A4 (BEATCH.224)
  fLAV_BT_ZofFrontFace[ 5] = 152.585*m; // End of A5 (BEATCH.229)
  fLAV_BT_ZofFrontFace[ 6] = 158.560*m; // Blue tube radius step (BEATCH.235 VAC.O=2400)
  fLAV_BT_ZofFrontFace[ 7] = 166.095*m; // End of A6 (BEATCH.239)
  fLAV_BT_ZofFrontFace[ 8] = 173.605*m; // End of A7 (BEATCH.244)
  fLAV_BT_ZofFrontFace[ 9] = 181.115*m; // End of A8 (BEATCH.249)
  fLAV_BT_ZofFrontFace[10] = 182.175*m; // Blue tube raidus step (BEATCH.252 VAC.O=2800)
  fLAV_BT_ZofFrontFace[11] = 183.705*m; // End of STRAW-01 (BEATCH.264-265)
  fLAV_BT_ZofFrontFace[12] = 198.070*m; // End of Spectrometer Magnet (BEATCH.289-290)
  fLAV_BT_ZofFrontFace[13] = 204.656*m; // End of STRAW-03 (BEATCH.305-306)
  // Position of back face (world reference system)
  fLAV_BT_ZofBackFace[ 0] = 120.595*m; // Start of A1 (BEATCH.206 Section 5 Cryo-pump 119)
  fLAV_BT_ZofBackFace[ 1] = 128.205*m; // Start of A2 (BEATCH.211 Section 2 Inverted)
  fLAV_BT_ZofBackFace[ 2] = 135.815*m; // Start of A3 (BEATCH.216 Section 4 Inverted)
  fLAV_BT_ZofBackFace[ 3] = 143.425*m; // Start of A4 (BEATCH.221 Section 8)
  fLAV_BT_ZofBackFace[ 4] = 151.035*m; // Start of A5 (BEATCH.226 Section10)
  fLAV_BT_ZofBackFace[ 5] = 158.560*m; // Blue tube radius step (BEATCH.235 VAC.O=2400)
  fLAV_BT_ZofBackFace[ 6] = 164.545*m; // Start of A6 (BEATCH.236 Section12)
  fLAV_BT_ZofBackFace[ 7] = 172.055*m; // Start of A7 (BEATCH.241 Section15)
  fLAV_BT_ZofBackFace[ 8] = 179.565*m; // Start of A8 (BEATCH.246 Section17 Primary-pump)
  fLAV_BT_ZofBackFace[ 9] = 182.175*m; // Blue tube radius step (BEATCH.252 VAC.O=2800)
  fLAV_BT_ZofBackFace[10] = 183.310*m; // Start of STRAW-01 (BEATCH.255.256)
  fLAV_BT_ZofBackFace[11] = 191.941*m; // Start of A9 (BEATCH.267 Section24 Primary-pump)
  fLAV_BT_ZofBackFace[12] = 202.334*m; // Start of A10 (BEATCH.291 Section26 mu1.02 (new))
  fLAV_BT_ZofBackFace[13] = 216.760*m; // Start of A11 (BEATCH.309 Section28 Man-hole(new))
  // Inner radius
  fLAV_BT_InnerRadius[ 0] =  960.*mm;
  fLAV_BT_InnerRadius[ 1] =  960.*mm;
  fLAV_BT_InnerRadius[ 2] =  960.*mm;
  fLAV_BT_InnerRadius[ 3] =  960.*mm;
  fLAV_BT_InnerRadius[ 4] =  960.*mm;
  fLAV_BT_InnerRadius[ 5] =  960.*mm;
  fLAV_BT_InnerRadius[ 6] = 1200.*mm;
  fLAV_BT_InnerRadius[ 7] = 1200.*mm;
  fLAV_BT_InnerRadius[ 8] = 1200.*mm;
  fLAV_BT_InnerRadius[ 9] = 1200.*mm;
  fLAV_BT_InnerRadius[10] = 1400.*mm;
  fLAV_BT_InnerRadius[11] = 1400.*mm;
  fLAV_BT_InnerRadius[12] = 1400.*mm;
  fLAV_BT_InnerRadius[13] = 1400.*mm;
  // Outer radius
  fLAV_BT_OuterRadius[ 0] =  974.*mm;
  fLAV_BT_OuterRadius[ 1] =  974.*mm;
  fLAV_BT_OuterRadius[ 2] =  974.*mm;
  fLAV_BT_OuterRadius[ 3] =  974.*mm;
  fLAV_BT_OuterRadius[ 4] =  974.*mm;
  fLAV_BT_OuterRadius[ 5] =  974.*mm;
  fLAV_BT_OuterRadius[ 6] = 1216.*mm;
  fLAV_BT_OuterRadius[ 7] = 1216.*mm;
  fLAV_BT_OuterRadius[ 8] = 1216.*mm;
  fLAV_BT_OuterRadius[ 9] = 1216.*mm;
  fLAV_BT_OuterRadius[10] = 1415.*mm;
  fLAV_BT_OuterRadius[11] = 1415.*mm;
  fLAV_BT_OuterRadius[12] = 1415.*mm;
  fLAV_BT_OuterRadius[13] = 1415.*mm;
  // Flags to add rails (only O=1920 sections)
  fLAV_BT_HasRails[ 0] = 1;
  fLAV_BT_HasRails[ 1] = 1;
  fLAV_BT_HasRails[ 2] = 1;
  fLAV_BT_HasRails[ 3] = 1;
  fLAV_BT_HasRails[ 4] = 1;
  fLAV_BT_HasRails[ 5] = 1;
  fLAV_BT_HasRails[ 6] = 0;
  fLAV_BT_HasRails[ 7] = 0;
  fLAV_BT_HasRails[ 8] = 0;
  fLAV_BT_HasRails[ 9] = 0;
  fLAV_BT_HasRails[10] = 0;
  fLAV_BT_HasRails[11] = 0;
  fLAV_BT_HasRails[12] = 0;
  fLAV_BT_HasRails[13] = 0;

  // Blue tube rails' vertices numbers and XY coordinates
  // N.B. coordinates are in the beam=(0,0) reference system (i.e. world coordinates)
  // so that the generated solids can be positioned with no displacement in the XY plane
  fBTRailNVertices[0] = N_OF_BTRAIL_VERTICES;
  fBTRailVertex[0][0] = G4TwoVector( 395.00*mm,-760.25*mm);
  fBTRailVertex[0][1] = G4TwoVector( 555.00*mm,-760.25*mm);
  fBTRailVertex[0][2] = G4TwoVector( 555.00*mm,-779.25*mm);
  fBTRailVertex[0][3] = G4TwoVector( 490.00*mm,-779.25*mm);
  fBTRailVertex[0][4] = G4TwoVector( 490.00*mm,-825.53*mm);
  fBTRailVertex[0][5] = G4TwoVector( 460.00*mm,-842.61*mm);
  fBTRailVertex[0][6] = G4TwoVector( 460.00*mm,-779.25*mm);
  fBTRailVertex[0][7] = G4TwoVector( 395.00*mm,-779.25*mm);

  fBTRailNVertices[1] = N_OF_BTRAIL_VERTICES;
  fBTRailVertex[1][0] = G4TwoVector(-395.00*mm,-779.25*mm);
  fBTRailVertex[1][1] = G4TwoVector(-460.00*mm,-779.25*mm);
  fBTRailVertex[1][2] = G4TwoVector(-460.00*mm,-842.61*mm);
  fBTRailVertex[1][3] = G4TwoVector(-490.00*mm,-825.53*mm);
  fBTRailVertex[1][4] = G4TwoVector(-490.00*mm,-779.25*mm);
  fBTRailVertex[1][5] = G4TwoVector(-555.00*mm,-779.25*mm);
  fBTRailVertex[1][6] = G4TwoVector(-555.00*mm,-760.25*mm);
  fBTRailVertex[1][7] = G4TwoVector(-395.00*mm,-760.25*mm);

  // Beam pipe
  for (G4int iLAV = 0; iLAV < 12; iLAV++){
    fBeamPipeZLength[iLAV] = fLAV_RR_ZofBackFace[4] - fLAV_RR_ZofFrontFace[4];
    fBeamPipeZPosition[iLAV] = 0.;
    fBeamPipeInnerRadius[iLAV] = 84.0*mm;
    fBeamPipeOuterRadius[iLAV] = 85.0*mm;  
    fBeamPipeFinZLength[iLAV] = 5.0*mm;
    fBeamPipeFinOuterRadius[iLAV] = iLAV < 11 ? 0 : 89.0*mm;
    fBeamPipeFinSpacing[iLAV] = 40.0*mm;
    fBeamPipeInputDisplacementWRTBeam[iLAV]=0;
    fBeamPipeOutputDisplacementWRTBeam[iLAV]=0;
    fBeamPipeZLengthWFins[iLAV] = fBeamPipeZLength[iLAV];
  }
  // LAV Station responsibility region
  fLAV_Station_ResponsibilityRegion[0]  = 0; // A1
  fLAV_Station_ResponsibilityRegion[1]  = 0; // A2
  fLAV_Station_ResponsibilityRegion[2]  = 0; // A3
  fLAV_Station_ResponsibilityRegion[3]  = 0; // A4
  fLAV_Station_ResponsibilityRegion[4]  = 0; // A5
  fLAV_Station_ResponsibilityRegion[5]  = 0; // A6
  fLAV_Station_ResponsibilityRegion[6]  = 0; // A7
  fLAV_Station_ResponsibilityRegion[7]  = 0; // A8
  fLAV_Station_ResponsibilityRegion[8]  = 1; // A9
  fLAV_Station_ResponsibilityRegion[9]  = 2; // A10
  fLAV_Station_ResponsibilityRegion[10] = 3; // A11
  fLAV_Station_ResponsibilityRegion[11] = 4; // A12

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
  //fLAV_Station_ZofFrontFace[11] = 238.200*m; // Old setting of CHOD-LAV12 border
  //fLAV_Station_ZofFrontFace[11] = 238.150*m;
  fLAV_Station_ZofFrontFace[11] = 238.198*m; // BEATCH 08/03/2016

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
  //fLAV_Station_ZofBackFace[11] = 238.800*m; // Old setting of CHOD-LAV12 border
  //fLAV_Station_ZofBackFace[11] = 238.850*m;
  fLAV_Station_ZofBackFace[11] = 238.898*m; // BEATCH 08/03/2016

  // LAV Station inner radius (circle tangent to front face of crystals)
  fLAV_Station_InnerRadius[0]  =  536.5*mm;
  fLAV_Station_InnerRadius[1]  =  536.5*mm;
  fLAV_Station_InnerRadius[2]  =  536.5*mm;
  fLAV_Station_InnerRadius[3]  =  536.5*mm;
  fLAV_Station_InnerRadius[4]  =  536.5*mm;
  fLAV_Station_InnerRadius[5]  =  767.5*mm;
  fLAV_Station_InnerRadius[6]  =  767.5*mm;
  fLAV_Station_InnerRadius[7]  =  767.5*mm;
  fLAV_Station_InnerRadius[8]  =  980.0*mm;
  fLAV_Station_InnerRadius[9]  =  980.0*mm;
  fLAV_Station_InnerRadius[10] =  980.0*mm;
  fLAV_Station_InnerRadius[11] = 1070.0*mm;

  // LAV Station outer radius (radius of vessel at flange)
  fLAV_Station_OuterRadius[0]  = 1082.*mm;
  fLAV_Station_OuterRadius[1]  = 1082.*mm;
  fLAV_Station_OuterRadius[2]  = 1082.*mm;
  fLAV_Station_OuterRadius[3]  = 1082.*mm;
  fLAV_Station_OuterRadius[4]  = 1082.*mm;
  fLAV_Station_OuterRadius[5]  = 1336.*mm;
  fLAV_Station_OuterRadius[6]  = 1336.*mm;
  fLAV_Station_OuterRadius[7]  = 1336.*mm;
  fLAV_Station_OuterRadius[8]  = 1535.*mm;
  fLAV_Station_OuterRadius[9]  = 1535.*mm;
  fLAV_Station_OuterRadius[10] = 1535.*mm;
  fLAV_Station_OuterRadius[11] = 1705.*mm;

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
  //fLAV_Station_ZLength[11] =  600.*mm; // Old setting of CHOD-LAV12 border
  fLAV_Station_ZLength[11] =  700.*mm;

  // Steel vessel outer radius (in the crystal zone)
  fLAV_Vessel_OuterRadius[0]  = 1082.*mm;
  fLAV_Vessel_OuterRadius[1]  = 1082.*mm;
  fLAV_Vessel_OuterRadius[2]  = 1082.*mm;
  fLAV_Vessel_OuterRadius[3]  = 1082.*mm;
  fLAV_Vessel_OuterRadius[4]  = 1082.*mm;
  fLAV_Vessel_OuterRadius[5]  = 1331.*mm;
  fLAV_Vessel_OuterRadius[6]  = 1331.*mm;
  fLAV_Vessel_OuterRadius[7]  = 1331.*mm;
  fLAV_Vessel_OuterRadius[8]  = 1530.*mm;
  fLAV_Vessel_OuterRadius[9]  = 1530.*mm;
  fLAV_Vessel_OuterRadius[10] = 1530.*mm;
  fLAV_Vessel_OuterRadius[11] = 1685.*mm; // Arbitrary as A12 has no real vessel

  // Steel vessel inner radius (in the crystal zone)
  fLAV_Vessel_InnerRadius[0]  = 1057.*mm;
  fLAV_Vessel_InnerRadius[1]  = 1057.*mm;
  fLAV_Vessel_InnerRadius[2]  = 1057.*mm;
  fLAV_Vessel_InnerRadius[3]  = 1057.*mm;
  fLAV_Vessel_InnerRadius[4]  = 1057.*mm;
  fLAV_Vessel_InnerRadius[5]  = 1301.*mm;
  fLAV_Vessel_InnerRadius[6]  = 1301.*mm;
  fLAV_Vessel_InnerRadius[7]  = 1301.*mm;
  fLAV_Vessel_InnerRadius[8]  = 1500.*mm;
  fLAV_Vessel_InnerRadius[9]  = 1500.*mm;
  fLAV_Vessel_InnerRadius[10] = 1500.*mm;
  fLAV_Vessel_InnerRadius[11] = 1655.*mm; // Arbitrary as A12 has no real vessel

  // Steel vessel thickness
  fLAV_Vessel_Thickness[0]  = 25.*mm;
  fLAV_Vessel_Thickness[1]  = 25.*mm;
  fLAV_Vessel_Thickness[2]  = 25.*mm;
  fLAV_Vessel_Thickness[3]  = 25.*mm;
  fLAV_Vessel_Thickness[4]  = 25.*mm;
  fLAV_Vessel_Thickness[5]  = 25.*mm;
  fLAV_Vessel_Thickness[6]  = 30.*mm;
  fLAV_Vessel_Thickness[7]  = 30.*mm;
  fLAV_Vessel_Thickness[8]  = 30.*mm;
  fLAV_Vessel_Thickness[9]  = 30.*mm;
  fLAV_Vessel_Thickness[10] = 30.*mm;
  fLAV_Vessel_Thickness[11] = 30.*mm;

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
  //fLAV_Station_FirstRingZPos[11] =  65.*mm; // Used with old setting of CHOD-LAV12 border
  fLAV_Station_FirstRingZPos[11] = 115.*mm;

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

  // Phi reference point for banana 0 of layer 0 (phi at center of banana)
  fLAV_Station_PhiReference[0]  =  98.86*deg; // A1
  fLAV_Station_PhiReference[1]  =  98.86*deg; // A2
  fLAV_Station_PhiReference[2]  =  98.86*deg; // A3
  fLAV_Station_PhiReference[3]  =  98.86*deg; // A4
  fLAV_Station_PhiReference[4]  =  98.86*deg; // A5
  fLAV_Station_PhiReference[5]  =  92.28*deg; // A6
  fLAV_Station_PhiReference[6]  =  92.28*deg; // A7
  fLAV_Station_PhiReference[7]  =  92.28*deg; // A8
  fLAV_Station_PhiReference[8]  =  92.25*deg; // A9
  //fLAV_Station_PhiReference[9]  =  92.25*deg; // A10
  fLAV_Station_PhiReference[9]  = 104.25*deg; // A10 (shifted by 1/2 banana to the right)
  fLAV_Station_PhiReference[10] =  92.25*deg; // A11
  //fLAV_Station_PhiReference[11] =  96.1875*deg; // A12 design
  //fLAV_Station_PhiReference[11] =  90.5625*deg; // A12 design (block 0.0.0 shifted by 1 to the left)
  fLAV_Station_PhiReference[11] =  91.125*deg; // A12 experimental

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
  // A12 has same rotation angle as A9-A11 but in the opposite direction
  fLAV_Station_PhiRotationBetweenLayers[11] =  1.50*deg;

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

  // Station 8
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
  for (G4int iB=0;iB<NUMBER_OF_BANANA_SHAPES;iB++) {

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

    fAlCShapeNVertices[iB] = N_OF_ALCSHAPE_VERTICES;    // Number of vertices of C-shape
    // (x,y) coordinates of vertices of C-shape
    fAlCShapeVertex[iB][0] = G4TwoVector(-45.0*mm,  0.0*mm);
    fAlCShapeVertex[iB][1] = G4TwoVector(-45.0*mm, 25.0*mm);
    fAlCShapeVertex[iB][2] = G4TwoVector(-37.0*mm, 25.0*mm);
    fAlCShapeVertex[iB][3] = G4TwoVector(-37.0*mm, 10.0*mm);
    fAlCShapeVertex[iB][4] = G4TwoVector( 37.0*mm, 10.0*mm);
    fAlCShapeVertex[iB][5] = G4TwoVector( 37.0*mm, 25.0*mm);
    fAlCShapeVertex[iB][6] = G4TwoVector( 45.0*mm, 25.0*mm);
    fAlCShapeVertex[iB][7] = G4TwoVector( 45.0*mm,  0.0*mm);
    fAlCShapeHeight[iB] = 70.0*mm;       // Height at center of C-shape
    fAlCShapeHoleDiameter[iB] = 20.0*mm; // Diameter of hole at center of C-shape
  }

  // Banana type 0 (Stations 1-5)
  fBananaPhiSpan[0]     =  45.*deg;  // 360deg/8
  fBananaOuterRadius[0] = 1057.0*mm;  // External radius of banana (at vessel internal surface)
  fBananaInnerRadius[0] =  536.5*mm;  // Internal radius of banana (circle tangent to block front face)
  fBananaNotchLength[0] = 0.*cm; // Length along R of central notch to avoid block-banana overlap
  fBananaNotchAngle[0] = 0.*deg; // Angular displacement of central notch to avoid block-banana overlap
  fBananaExtraRadiusTolerance[0][0] = -100.*um; // Extra tolerance in radial position for block 0
  fBananaExtraRadiusTolerance[0][1] = -100.*um; // Extra tolerance in radial position for block 1
  fBananaExtraRadiusTolerance[0][2] = -100.*um; // Extra tolerance in radial position for block 2
  fBananaExtraRadiusTolerance[0][3] = -100.*um; // Extra tolerance in radial position for block 3
  fAlSlabNVertices[0] = 11;
  fAlSlabVertex[0][0]  = G4TwoVector(   0.0*mm,   0.0*mm);
  fAlSlabVertex[0][1]  = G4TwoVector(-181.8*mm, -18.2*mm);
  fAlSlabVertex[0][2]  = G4TwoVector(-330.9*mm, -63.4*mm);
  fAlSlabVertex[0][3]  = G4TwoVector(-368.7*mm,  35.1*mm);
  fAlSlabVertex[0][4]  = G4TwoVector(-353.7*mm,  67.6*mm);
  fAlSlabVertex[0][5]  = G4TwoVector(-112.3*mm, 122.7*mm);
  fAlSlabVertex[0][6]  = G4TwoVector( 112.3*mm, 122.7*mm);
  fAlSlabVertex[0][7]  = G4TwoVector( 353.7*mm,  67.6*mm);
  fAlSlabVertex[0][8]  = G4TwoVector( 368.7*mm,  35.1*mm);
  fAlSlabVertex[0][9]  = G4TwoVector( 330.9*mm, -63.4*mm);
  fAlSlabVertex[0][10] = G4TwoVector( 181.8*mm, -18.2*mm);
  fBananaNAlColumns[0] = 12;
  fAlColumnPosition[0][0]  = G4TwoVector(  25.0*mm,  11.6*mm);
  fAlColumnPosition[0][1]  = G4TwoVector( 156.7*mm,   1.2*mm);
  fAlColumnPosition[0][2]  = G4TwoVector( 205.8*mm, -10.9*mm);
  fAlColumnPosition[0][3]  = G4TwoVector( 331.3*mm, -20.3*mm);
  fAlColumnPosition[0][4]  = G4TwoVector( 349.2*mm,  26.4*mm);
  fAlColumnPosition[0][5]  = G4TwoVector(  25.0*mm, 115.7*mm);
  fAlColumnPosition[0][6]  = G4TwoVector( -25.0*mm, 115.7*mm);
  fAlColumnPosition[0][7]  = G4TwoVector(-349.2*mm,  26.4*mm);
  fAlColumnPosition[0][8]  = G4TwoVector(-331.3*mm, -20.3*mm);
  fAlColumnPosition[0][9]  = G4TwoVector(-205.8*mm, -10.9*mm);
  fAlColumnPosition[0][10] = G4TwoVector(-156.7*mm,   1.2*mm);
  fAlColumnPosition[0][11] = G4TwoVector( -25.0*mm,  11.6*mm);
  fBananaAlCShapeScrewsPosition[0][0][0] = G4TwoVector( 227.6*mm, 90.8*mm);
  fBananaAlCShapeScrewsPosition[0][0][1] = G4TwoVector( 178.6*mm,100.6*mm);
  fBananaAlCShapeScrewsPosition[0][1][0] = G4TwoVector(-227.6*mm, 90.8*mm);
  fBananaAlCShapeScrewsPosition[0][1][1] = G4TwoVector(-178.6*mm,100.6*mm);

  // Banana type 1 (Stations 6-8)
  fBananaPhiSpan[1]     =  30.*deg;  // 360deg/12
  fBananaOuterRadius[1] = 1301.0*mm;  // External radius of banana (at vessel internal surface)
  fBananaInnerRadius[1] =  767.5*mm;  // Internal radius of banana (circle tangent to block front face)
  fBananaNotchLength[1] = 3.*cm; // Length along R of central notch to avoid block-banana overlap
  fBananaNotchAngle[1] = -0.1*deg; // Angular displacement of central notch to avoid block-banana overlap
  fBananaExtraRadiusTolerance[1][0] = -100.*um; // Extra tolerance in radial position for block 0
  fBananaExtraRadiusTolerance[1][1] = -50.*um; // Extra tolerance in radial position for block 1
  fBananaExtraRadiusTolerance[1][2] = -50.*um; // Extra tolerance in radial position for block 2
  fBananaExtraRadiusTolerance[1][3] = -100.*um; // Extra tolerance in radial position for block 3
  fAlSlabNVertices[1] = 11;
  fAlSlabVertex[1][0]  = G4TwoVector(   0.0*mm,   0.0*mm);
  fAlSlabVertex[1][1]  = G4TwoVector(-150.9*mm,  -9.9*mm);
  fAlSlabVertex[1][2]  = G4TwoVector(-287.4*mm, -37.0*mm);
  fAlSlabVertex[1][3]  = G4TwoVector(-311.5*mm,  84.1*mm);
  fAlSlabVertex[1][4]  = G4TwoVector(-295.7*mm, 108.6*mm);
  fAlSlabVertex[1][5]  = G4TwoVector(-124.7*mm, 136.7*mm);
  fAlSlabVertex[1][6]  = G4TwoVector( 124.7*mm, 136.7*mm);
  fAlSlabVertex[1][7]  = G4TwoVector( 295.6*mm, 108.6*mm);
  fAlSlabVertex[1][8]  = G4TwoVector( 311.5*mm,  84.1*mm);
  fAlSlabVertex[1][9]  = G4TwoVector( 287.4*mm, -37.0*mm);
  fAlSlabVertex[1][10] = G4TwoVector( 150.9*mm,  -9.9*mm);
  fBananaNAlColumns[1] = 12;
  fAlColumnPosition[1][0]  = G4TwoVector(  16.9*mm,  11.4*mm);
  fAlColumnPosition[1][1]  = G4TwoVector( 137.6*mm,   3.3*mm);
  fAlColumnPosition[1][2]  = G4TwoVector( 167.2*mm,  -0.6*mm);
  fAlColumnPosition[1][3]  = G4TwoVector( 285.4*mm,   4.2*mm);
  fAlColumnPosition[1][4]  = G4TwoVector( 301.0*mm,  82.6*mm);
  fAlColumnPosition[1][5]  = G4TwoVector(  25.0*mm, 114.0*mm);
  fAlColumnPosition[1][6]  = G4TwoVector( -25.0*mm, 114.0*mm);
  fAlColumnPosition[1][7]  = G4TwoVector(-301.0*mm,  82.6*mm);
  fAlColumnPosition[1][8]  = G4TwoVector(-285.4*mm,   4.1*mm);
  fAlColumnPosition[1][9]  = G4TwoVector(-167.2*mm,  -0.6*mm);
  fAlColumnPosition[1][10] = G4TwoVector(-137.6*mm,   3.3*mm);
  fAlColumnPosition[1][11] = G4TwoVector( -16.9*mm,  11.4*mm);
  fBananaAlCShapeScrewsPosition[1][0][0] = G4TwoVector( 192.6*mm,115.5*mm);
  fBananaAlCShapeScrewsPosition[1][0][1] = G4TwoVector( 143.1*mm,122.0*mm);
  fBananaAlCShapeScrewsPosition[1][1][0] = G4TwoVector(-192.6*mm,115.5*mm);
  fBananaAlCShapeScrewsPosition[1][1][1] = G4TwoVector(-143.1*mm,122.0*mm);

  // Banana type 2 (Stations 9-11)
  fBananaPhiSpan[2]     =  24.*deg;  // 360deg/15
  fBananaOuterRadius[2] = 1500.*mm;  // External radius of banana (at vessel internal surface)
  fBananaInnerRadius[2] =  980.*mm;  // Internal radius of banana (circle tangent to block front face)
  fBananaNotchLength[2] = 3.*cm; // Length along R of central notch to avoid block-banana overlap
  fBananaNotchAngle[2] = -0.1*deg; // Angular displacement of central notch to avoid block-banana overlap
  fBananaExtraRadiusTolerance[2][0] = -120.*um; // Extra tolerance in radial position for block 0
  fBananaExtraRadiusTolerance[2][1] =  -50.*um; // Extra tolerance in radial position for block 1
  fBananaExtraRadiusTolerance[2][2] =  -60.*um; // Extra tolerance in radial position for block 2
  fBananaExtraRadiusTolerance[2][3] = -130.*um; // Extra tolerance in radial position for block 3
  fAlSlabNVertices[2] = 11;
  fAlSlabVertex[2][0]  = G4TwoVector(   0.0*mm,   0.0*mm);
  fAlSlabVertex[2][1]  = G4TwoVector(-140.7*mm,  -7.5*mm);
  fAlSlabVertex[2][2]  = G4TwoVector(-272.6*mm, -28.6*mm);
  fAlSlabVertex[2][3]  = G4TwoVector(-292.7*mm,  70.2*mm);
  fAlSlabVertex[2][4]  = G4TwoVector(-269.2*mm, 105.3*mm);
  fAlSlabVertex[2][5]  = G4TwoVector( -94.8*mm, 126.7*mm);
  fAlSlabVertex[2][6]  = G4TwoVector(  94.8*mm, 126.7*mm);
  fAlSlabVertex[2][7]  = G4TwoVector( 269.2*mm, 105.3*mm);
  fAlSlabVertex[2][8]  = G4TwoVector( 292.7*mm,  70.2*mm);
  fAlSlabVertex[2][9]  = G4TwoVector( 272.6*mm, -28.6*mm);
  fAlSlabVertex[2][10] = G4TwoVector( 140.7*mm,  -7.5*mm);
  fBananaNAlColumns[2] = 12;
  fAlColumnPosition[2][0]  = G4TwoVector(  13.0*mm,   8.0*mm);
  fAlColumnPosition[2][1]  = G4TwoVector( 133.3*mm,   4.7*mm);
  fAlColumnPosition[2][2]  = G4TwoVector( 155.2*mm,   2.1*mm);
  fAlColumnPosition[2][3]  = G4TwoVector( 268.8*mm,   3.1*mm);
  fAlColumnPosition[2][4]  = G4TwoVector( 280.8*mm,  61.9*mm);
  fAlColumnPosition[2][5]  = G4TwoVector(  25.0*mm, 119.0*mm);
  fAlColumnPosition[2][6]  = G4TwoVector( -25.0*mm, 119.0*mm);
  fAlColumnPosition[2][7]  = G4TwoVector(-280.8*mm,  61.9*mm);
  //fAlColumnPosition[2][8]  = G4TwoVector(-268.8*mm,   3.1*mm);
  fAlColumnPosition[2][8]  = G4TwoVector(-269.5*mm,   3.1*mm); // Shift of 0.7mm wrt nominal position to avoid overlap with block 3 mu-metal
  fAlColumnPosition[2][9]  = G4TwoVector(-155.2*mm,   2.4*mm);
  fAlColumnPosition[2][10] = G4TwoVector(-133.3*mm,   4.7*mm);
  fAlColumnPosition[2][11] = G4TwoVector( -13.0*mm,   8.0*mm);
  fBananaAlCShapeScrewsPosition[2][0][0] = G4TwoVector( 181.7*mm,105.8*mm);
  fBananaAlCShapeScrewsPosition[2][0][1] = G4TwoVector( 132.0*mm,111.0*mm);
  fBananaAlCShapeScrewsPosition[2][1][0] = G4TwoVector(-181.7*mm,105.8*mm);
  fBananaAlCShapeScrewsPosition[2][1][1] = G4TwoVector(-132.0*mm,111.0*mm);

  // Banana type 3 (Station 12)
  fBananaPhiSpan[3]     = 22.5*deg;  // 360deg/16
  fBananaOuterRadius[3] = 1655.*mm;  // External radius of banana (at vessel internal surface)
  fBananaInnerRadius[3] = 1070.*mm;  // Internal radius of banana (circle tangent to block front face)
  fBananaNotchLength[3] = 0.*cm; // Length along R of central notch to avoid block-banana overlap
  fBananaNotchAngle[3] = 0.*deg; // Angular displacement of central notch to avoid block-banana overlap
  fBananaExtraRadiusTolerance[3][0] = 0.*um; // Extra tolerance in radial position for block 0
  fBananaExtraRadiusTolerance[3][1] = 0.*um; // Extra tolerance in radial position for block 1
  fBananaExtraRadiusTolerance[3][2] = 0.*um; // Extra tolerance in radial position for block 2
  fBananaExtraRadiusTolerance[3][3] = 0.*um; // Extra tolerance in radial position for block 3
  fAlSlabNVertices[3] = 0;
  fBananaNAlColumns[3] = 0;

  // Some block building parameters
  fWrapFrontHole = 1.35*cm;
  fWrapThick = 0.07*mm;
  fGlueThick = 0.2*mm;
  fAirThick = 0.2*mm;

  // Path of efficiency/time matrices
  fLAVEfficiencyMatrix = NA62ConditionsService::GetInstance()->GetFullPath("LAVEff.dat");
  fLAVTimeMatrix = NA62ConditionsService::GetInstance()->GetFullPath("LAVDelay.dat");
}

LAVGeometryParameters::~LAVGeometryParameters(){}

LAVGeometryParameters* LAVGeometryParameters::GetInstance() {
  if ( fInstance == 0 ) { fInstance = new LAVGeometryParameters(); }
  return fInstance;
}

G4int LAVGeometryParameters::GetBlockIdFromOpalId(G4int opalId) {
  for(G4int id=0;id<NUMBER_OF_BLOCK_SHAPES;id++) {
    if ( fBlockOpalId[id] == opalId ) { return id; }
  }
  return -1;
}

TObjArray LAVGeometryParameters::GetHashTable() {
  TObjArray LAVGeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fWorldZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldZLength));
  LAVGeometryParameters.Add(new DetectorParameter("fWorldZLength",Value.Data(),
					       "World Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldXLength));
  LAVGeometryParameters.Add(new DetectorParameter("fWorldXLength",Value.Data(),
						  "World X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldYLength));
  LAVGeometryParameters.Add(new DetectorParameter("fWorldYLength",Value.Data(),
						  "World Y Length", ParameterData));
  /*
  ParameterData.Clear();

  Buffer << fLAVDetectorZPosition;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLAVDetectorZPosition));
  LAVGeometryParameters.Add(new DetectorParameter("fLAVDetectorZPosition",Value.Data(),
					       "LAV Detector Z Position", ParameterData));
  ParameterData.Clear();

  Buffer << fLAVDetectorZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLAVDetectorZLength));
  LAVGeometryParameters.Add(new DetectorParameter("fLAVDetectorZLength",Value.Data(),
					       "LAV Detector Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fLAVDetectorXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLAVDetectorXLength));
  LAVGeometryParameters.Add(new DetectorParameter("fLAVDetectorXLength",Value.Data(),
					       "LAV Detector X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fLAVDetectorYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLAVDetectorYLength));
  LAVGeometryParameters.Add(new DetectorParameter("fLAVDetectorYLength",Value.Data(),
					       "LAV Detector Y Length", ParameterData));
  */

  return LAVGeometryParameters;
}

G4double LAVGeometryParameters::GetBlockPhi(const G4int chid){

   int b  = (chid / 1) % 10;
   int BB = (chid / 10) % 100;
   int L  = (chid / 1000) % 10;
   int SS = (chid / 10000) % 100;

   int BID = GetStationBananaType(SS);

   double Phi = 0
      // VVV phibanana VVV
      + GetStationPhiReference(SS)
      + L * GetStationPhiRotationBetweenLayers(SS)
      + BB*360.*deg / GetStationNBananasPerRing(SS)
      // VVV phiblock VVV
      - 0.5 * GetBananaPhiSpan(BID)
      + (b+0.5) * GetBananaPhiSpan(BID) / GetBananaNBlocks(BID);

   return Phi;
}

G4ThreeVector LAVGeometryParameters::GetBlockXVersor(const G4int){
   return G4ThreeVector(0,0,-1);
}

G4ThreeVector LAVGeometryParameters::GetBlockYVersor(const G4int chid){
   //double Phi = GetBlockPhi(chid);
   //return G4ThreeVector(sin(Phi),-cos(Phi),0);
   return GetBlockZVersor(chid).cross(GetBlockXVersor(chid)).unit();
}

G4ThreeVector LAVGeometryParameters::GetBlockZVersor(const G4int chid){
   double Phi = GetBlockPhi(chid);
   return G4ThreeVector(-cos(Phi),-sin(Phi),0);
}

#include "G4RotationMatrix.hh"

G4ThreeVector LAVGeometryParameters::GetLeadglassCenter(const G4int chid){

   //int b  = (chid / 1) % 10;
   //int BB = (chid / 10) % 100;
   int L  = (chid / 1000) % 10;
   int SS = (chid / 10000) % 100;

   int BID = GetStationBananaType(SS);

   G4ThreeVector xver = GetBlockXVersor(chid);
   G4ThreeVector zver = GetBlockZVersor(chid);
   G4ThreeVector center;
   center  = -zver * GetStationInnerRadius(SS);
   center += -zver * GetBlockZLength(BID) / 2;
   center += -xver * GetStationZofFrontFace(SS);
   center += -xver * GetStationFirstRingZPos(SS);
   center += -xver * GetBananaThickness(BID) / 2;
   center += -xver * L * GetBananaThickness(BID);
   center += -xver * L * GetStationRingGap(SS);
   return center;
}

void LAVGeometryParameters::Print(){
  G4cout << "fWorldZLength= "<< fWorldZLength << G4endl
	 << "fWorldXLength= "<< fWorldXLength << G4endl
	 << "fWorldYLength= "<< fWorldYLength << G4endl;
}
