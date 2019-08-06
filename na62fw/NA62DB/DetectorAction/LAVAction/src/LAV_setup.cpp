#include <string>
#include "LAVDetectorSetup.h"
#include "NA62DBSystemOfUnits.h"
// Number of different block shapes
#define NUMBER_OF_BLOCK_SHAPES 9

// Number of Responsibility Regions
#define NUMBER_OF_RESPONSIBILITY_REGIONS 5

// Number of different sections of the blue tube in the LAV RRs
#define NUMBER_OF_BLUETUBE_ZONES 14

// Number of rails in blue tube
#define NUMBER_OF_BTRAILS 2

// Number of vertices for blue tube rails
#define N_OF_BTRAIL_VERTICES 8

int FillAllValues(LAVDetectorSetup *);

int main( int argc, char *argv[])
{   

  std::string connString = "OraCernProxy";
  std::string usrName = "LAV";

  LAVDetectorSetup * mDS = new LAVDetectorSetup(connString,usrName, coral::Update);
  mDS->DeployDetectorStructureToDB(); 

  coral::TimeStamp cacheStartTime(2000, 1, 2, 1, 1, 1, true);
  coral::TimeStamp cacheFinishTime(2038,1,2,0,0,0,0, true);
  std::vector<std::string> tags;
  //  tags.push_back("construction_mc");
  tags.push_back("all");
  mDS->BuildCache(cacheStartTime, cacheFinishTime, tags);    	
  int fAV = FillAllValues(mDS); 
  delete mDS; 
  return 0;
}


int FillAllValues(LAVDetectorSetup * mDS) {
  //coral::TimeStamp d1, d2, d3, d4, d5, d6, d7; 	
  //d1 = coral::TimeStamp(2014,7,20,0,0,0,0);
  //d2 = coral::TimeStamp(2014,8,10,0,0,0,0);
  //d3 = coral::TimeStamp(2014,8,20,0,0,0,0);
  //d4 = coral::TimeStamp(2014,9,10,0,0,0,0);	
  //d5 = coral::TimeStamp(2014,9,20,0,0,0,0);
  //d6 = coral::TimeStamp(2031,10,10,0,0,0,0);
  //d7 = coral::TimeStamp(2031,10,20,0,0,0,0);

  // General names of LAV sensitive detector and hit collection in the NA62MC: single instance
  
  std::string StringInput;
  StringInput = "LAVGENERAL";  
  mDS->RegisterNewSubSystem("LAV",StringInput);

  StringInput = "/LAVFast";  
  mDS->SetAttributeValue("LAV","fLAVFastSensitiveDetectorName", StringInput,0);
 
  StringInput = "/LAVLeadglass";  
  mDS->SetAttributeValue("LAV","fLAVLeadglassSensitiveDetectorName", StringInput,0);

  StringInput = "/LAVGuide";  
  mDS->SetAttributeValue("LAV","fLAVGuideSensitiveDetectorName", StringInput,0);

  StringInput = "/LAVCathode";  
  mDS->SetAttributeValue("LAV","fLAVCathodeSensitiveDetectorName", StringInput,0);

  StringInput = "LAVCollection";  
  int setAttributeValueOutputCode;
  setAttributeValueOutputCode = mDS->SetAttributeValue("LAV","fLAVCollectionName", StringInput,0);

  double fWorldZLength = 22.*m;
  double fWorldXLength = 10.*m;
  double fWorldYLength = 10.*m;
  setAttributeValueOutputCode = mDS->SetAttributeValue("LAV","fWorldZLength",fWorldZLength,0);
  setAttributeValueOutputCode = mDS->SetAttributeValue("LAV","fWorldXLength", fWorldXLength,0);
  setAttributeValueOutputCode = mDS->SetAttributeValue("LAV","fWorldYLength", fWorldYLength,0);

  // Some block building parameters
  double fWrapFrontHole;   // diameter of the hole in the small wrapping face
  double fWrapThick;       // wrap thickness
  double fAirThick;        // air thickness between leadglass and wrapping
  double fGlueThick;       // glue thickness betwen leadglass and metal plate
  fWrapFrontHole = 1.35*cm;
  fWrapThick = 0.07*mm;
  fGlueThick = 0.2*mm;
  fAirThick = 0.2*mm;

  setAttributeValueOutputCode = mDS->SetAttributeValue("LAV","fWrapFrontHole",fWrapFrontHole,0); 
  setAttributeValueOutputCode = mDS->SetAttributeValue("LAV","fWrapThick",fWrapThick,0);
  setAttributeValueOutputCode = mDS->SetAttributeValue("LAV","fGlueThick",fGlueThick,0);
  setAttributeValueOutputCode = mDS->SetAttributeValue("LAV","fAirThick",fAirThick,0);

// LAV Responsibility region parameters

  double fLAV_RR_ZofFrontFace[NUMBER_OF_RESPONSIBILITY_REGIONS];
  double fLAV_RR_ZofBackFace[NUMBER_OF_RESPONSIBILITY_REGIONS];
  double fLAV_RR_Radius[NUMBER_OF_RESPONSIBILITY_REGIONS];

  // LAV Responsibility Regions position of front face
  fLAV_RR_ZofFrontFace[0] = 104.458*m;
  //fLAV_RR_ZofFrontFace[1] = 191.941*m;
  fLAV_RR_ZofFrontFace[1] = 183.705*m; // Enlarged down to STRAW-01
  //fLAV_RR_ZofFrontFace[2] = 202.334*m;
  fLAV_RR_ZofFrontFace[2] = 198.070*m; // Enlarged down to Spectrometer Magnet
  //fLAV_RR_ZofFrontFace[3] = 216.760*m;
  fLAV_RR_ZofFrontFace[3] = 204.656*m; // Enlarged down to STRAW-03
  //fLAV_RR_ZofFrontFace[4] = 238.200*m; // Old setting of CHOD-LAV12 border
  fLAV_RR_ZofFrontFace[4] = 238.150*m;

  // LAV Responsibility Regions position of back face
  //fLAV_RR_ZofBackFace[0] = 181.115*m;
  fLAV_RR_ZofBackFace[0] = 183.311*m; // Enlarged up to STRAW-01
  fLAV_RR_ZofBackFace[1] = 193.371*m;
  fLAV_RR_ZofBackFace[2] = 203.764*m;
  fLAV_RR_ZofBackFace[3] = 218.190*m;
  //fLAV_RR_ZofBackFace[4] = 238.800*m; // Old setting of CHOD-LAV12 border
  fLAV_RR_ZofBackFace[4] = 238.850*m;

  // LAV Responsibility Regions radius
  fLAV_RR_Radius[0] = 2000.*mm;
  fLAV_RR_Radius[1] = 2000.*mm;
  fLAV_RR_Radius[2] = 2000.*mm;
  fLAV_RR_Radius[3] = 2000.*mm;
  fLAV_RR_Radius[4] = 2000.*mm;

  for (int i=0; i<5; i++) {  
    std::ostringstream oss;
    oss << i+1;    
    StringInput = "LAVResponsibilityRegionGeometryRR"+oss.str();
    int ssid = mDS->RegisterNewSubSystem("LAVResponsibilityRegionGeometry",StringInput);
    std::cout << "Register RR Geometry region " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVResponsibilityRegionGeometry","fLAV_RR_ZofFrontFace", fLAV_RR_ZofFrontFace[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVResponsibilityRegionGeometry","fLAV_RR_ZofBackFace", fLAV_RR_ZofBackFace[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVResponsibilityRegionGeometry","fLAV_RR_Radius", fLAV_RR_Radius[i],i,d2,d6);
  }

// Blue-tube section parameters
  int fLAV_BT_ResponsibilityRegion[NUMBER_OF_BLUETUBE_ZONES];
  double fLAV_BT_ZofFrontFace[NUMBER_OF_BLUETUBE_ZONES];
  double fLAV_BT_ZofBackFace[NUMBER_OF_BLUETUBE_ZONES];
  double fLAV_BT_InnerRadius[NUMBER_OF_BLUETUBE_ZONES];
  double fLAV_BT_OuterRadius[NUMBER_OF_BLUETUBE_ZONES];
  int  fLAV_BT_HasRails[NUMBER_OF_BLUETUBE_ZONES];

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
  fLAV_BT_ZofBackFace[10] = 183.311*m; // Start of STRAW-01 (BEATCH.255.256)
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

  for (int i=0; i<NUMBER_OF_BLUETUBE_ZONES; i++) {  
    std::ostringstream oss;
    oss << i+1;    
    StringInput = "GeometryBTRegion"+oss.str();
    int ssid = mDS->RegisterNewSubSystem("LAVBTResponsibilityRegionGeometry",StringInput);
    std::cout << "Register Blue-tube Geometry " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_ResponsibilityRegion", fLAV_BT_ResponsibilityRegion[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_ZofFrontFace", fLAV_BT_ZofFrontFace[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_ZofBackFace", fLAV_BT_ZofBackFace[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_InnerRadius", fLAV_BT_InnerRadius[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_OuterRadius", fLAV_BT_OuterRadius[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_HasRails", fLAV_BT_HasRails[i],i,d2,d6);
  }

  // Number and XY coordinates of blue tube rails' vertices
  int       fBTRailNVertices[NUMBER_OF_BTRAILS];    // Number of vertices of BT rail
  std::vector< std::vector<double> > fBTRailVertex[NUMBER_OF_BTRAILS];

  fBTRailNVertices[0] = N_OF_BTRAIL_VERTICES;

  std::vector<double> TwoDVector;  
  TwoDVector.push_back(395.00*mm); TwoDVector.push_back(-760.25*mm);
  fBTRailVertex[0].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(555.00*mm); TwoDVector.push_back(-760.25*mm);
  fBTRailVertex[0].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(555.00*mm); TwoDVector.push_back(-779.25*mm);
  fBTRailVertex[0].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(490.00*mm); TwoDVector.push_back(-779.25*mm);
  fBTRailVertex[0].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(490.00*mm); TwoDVector.push_back(-825.53*mm);
  fBTRailVertex[0].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(460.00*mm); TwoDVector.push_back(-842.61*mm);
  fBTRailVertex[0].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(460.00*mm); TwoDVector.push_back(-779.25*mm);
  fBTRailVertex[0].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(395.00*mm); TwoDVector.push_back(-779.25*mm);
  fBTRailVertex[0].push_back(TwoDVector); TwoDVector.clear();

  fBTRailNVertices[1] = N_OF_BTRAIL_VERTICES;
  TwoDVector.push_back(-395.00*mm); TwoDVector.push_back(-760.25*mm);
  fBTRailVertex[1].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(-555.00*mm); TwoDVector.push_back(-760.25*mm);
  fBTRailVertex[1].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(-555.00*mm); TwoDVector.push_back(-779.25*mm);
  fBTRailVertex[1].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(-490.00*mm); TwoDVector.push_back(-779.25*mm);
  fBTRailVertex[1].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(-490.00*mm); TwoDVector.push_back(-825.53*mm);
  fBTRailVertex[1].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(-460.00*mm); TwoDVector.push_back(-842.61*mm);
  fBTRailVertex[1].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(-460.00*mm); TwoDVector.push_back(-779.25*mm);
  fBTRailVertex[1].push_back(TwoDVector); TwoDVector.clear();
  TwoDVector.push_back(-395.00*mm); TwoDVector.push_back(-779.25*mm);
  fBTRailVertex[1].push_back(TwoDVector); TwoDVector.clear();


  for (int i=0; i<NUMBER_OF_BTRAILS; i++) {  
    std::ostringstream oss;
    oss << i+1;    
    StringInput = "GeometryBTRail"+oss.str();
    int ssid = mDS->RegisterNewSubSystem("LAVBTRails",StringInput);
    std::cout << "Register Geometry for Rail " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBTRails","fBTRailNVertices",fBTRailNVertices[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBTRails","fBTRailVertex",fBTRailVertex[i],i,d2,d6);
  }

// LAV Beam pipe geometry
  double fBeamPipeZPosition[15]; // here, fill only up to 12 positions
  double fBeamPipeInnerRadius[15];
  double fBeamPipeOuterRadius[15];
  double fBeamPipeFinZLength[15];
  double fBeamPipeFinOuterRadius[15];
  double fBeamPipeFinSpacing[15];
  double fBeamPipeInputDisplacementWRTBeam[15];
  double fBeamPipeOutputDisplacementWRTBeam[15];

  for (int iLAV = 0; iLAV < 12; iLAV++){
    fBeamPipeZPosition[iLAV] = 0.;
    fBeamPipeInnerRadius[iLAV] = 84.0*mm;
    fBeamPipeOuterRadius[iLAV] = 85.0*mm;  
    fBeamPipeFinZLength[iLAV] = 5.0*mm;
    fBeamPipeFinOuterRadius[iLAV] = iLAV < 11 ? 0 : 89.0*mm;
    fBeamPipeFinSpacing[iLAV] = 40.0*mm;
    fBeamPipeInputDisplacementWRTBeam[iLAV]=0;
    fBeamPipeOutputDisplacementWRTBeam[iLAV]=0;
  }

  for (int i=0; i<12; i++) {  
    std::ostringstream oss;
    oss << i+1;    
    StringInput = "GeometryBPLAV"+oss.str();
    int ssid = mDS->RegisterNewSubSystem("LAVBeamPipeGeometry",StringInput);
    std::cout << "Register BP Geometry for LAV " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBeamPipeGeometry","fBeamPipeZPosition", fBeamPipeZPosition[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBeamPipeGeometry","fBeamPipeInnerRadius", fBeamPipeInnerRadius[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBeamPipeGeometry","fBeamPipeOuterRadius", fBeamPipeOuterRadius[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBeamPipeGeometry","fBeamPipeFinZLength", fBeamPipeFinZLength[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBeamPipeGeometry","fBeamPipeFinOuterRadius", fBeamPipeFinOuterRadius[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBeamPipeGeometry","fBeamPipeFinSpacing", fBeamPipeFinSpacing[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBeamPipeGeometry","fBeamPipeInputDisplacementWRTBeam", fBeamPipeInputDisplacementWRTBeam[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBeamPipeGeometry","fBeamPipeOutputDisplacementWRTBeam", fBeamPipeOutputDisplacementWRTBeam[i],i,d2,d6);
  }


// LAV geometry parameters: 12 instances

// prepare parameters

  long long fLAV_Station_NRings[12] = {5,5,5,5,5,5,5,5,4,4,4,4};
  long long fLAV_Station_NBananasPerRing[12] = {8,8,8,8,8,12,12,12,15,15,15,16};


  int fLAV_TypeOfCrystal[12][5]; // vector of opal types used for each layer for each station

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

  // Banana type used in station
  int fLAV_Station_BananaType[12];
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
  
  // LAV Station responsibility region
  int fLAV_Station_ResponsibilityRegion[12];
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
  double fLAV_Station_ZofFrontFace[12];
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
  fLAV_Station_ZofFrontFace[11] = 238.150*m;

  // LAV Station position of back face ("world" reference frame)
  double fLAV_Station_ZofBackFace[12];
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
  fLAV_Station_ZofBackFace[11] = 238.850*m;

  // LAV Station inner radius (circle tangent to front face of crystals)
  double fLAV_Station_InnerRadius[12];
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
  double fLAV_Station_OuterRadius[12];
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

  // Steel vessel outer radius (in the crystal zone)
  double fLAV_Vessel_OuterRadius[12];
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
  double fLAV_Vessel_InnerRadius[12];
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
  double fLAV_Vessel_Thickness[12];
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
  double fLAV_Station_FirstRingZPos[12];
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

  // Gap between rings
  double fLAV_Station_RingGap[12];
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
  double fLAV_Station_PhiReference[12];
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
  double fLAV_Station_PhiRotationBetweenLayers[12];
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


  // LAVStationGeometry filling

  for (int i=0; i<12; i++) {  
    std::ostringstream oss;
    oss << i+1;    
    StringInput = "GeometryLAV"+oss.str();
    int ssid = mDS->RegisterNewSubSystem("LAVStationGeometry",StringInput);
    std::cout << "Register station Geometry " << i << " output is " << ssid << std::endl;

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_NRings", fLAV_Station_NRings[i],i,d2,d6);
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_NBananasPerRing", fLAV_Station_NBananasPerRing[i], i,d2,d6);

    std::vector<int> temporaryVec;
    for (int k=0; k<fLAV_Station_NRings[i]; k++) {      
      temporaryVec.push_back(fLAV_TypeOfCrystal[i][k]);
    }
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_TypeOfCrystal",temporaryVec,i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_BananaType",fLAV_Station_BananaType[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_ResponsibilityRegion",fLAV_Station_ResponsibilityRegion[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_ZofFrontFace",fLAV_Station_ZofFrontFace[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_ZofBackFace",fLAV_Station_ZofBackFace[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_InnerRadius",fLAV_Station_InnerRadius[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_OuterRadius",fLAV_Station_OuterRadius[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Vessel_OuterRadius",fLAV_Vessel_OuterRadius[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Vessel_InnerRadius",fLAV_Vessel_InnerRadius[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Vessel_Thickness",fLAV_Vessel_Thickness[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_FirstRingZPos",fLAV_Station_FirstRingZPos[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_RingGap",fLAV_Station_RingGap[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_PhiReference",fLAV_Station_PhiReference[i],i,d2,d6); 

    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVStationGeometry","fLAV_Station_PhiRotationBetweenLayers",fLAV_Station_PhiRotationBetweenLayers[i],i,d2,d6); 

  }


  int fBlockOpalId[NUMBER_OF_BLOCK_SHAPES];        // Opal Id of block
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

  double fBlockZLength[NUMBER_OF_BLOCK_SHAPES];       // Z length (PbGl only)
  double fBlockL1Length[NUMBER_OF_BLOCK_SHAPES];      // L1 length (long base of back face)
  double fBlockL2Length[NUMBER_OF_BLOCK_SHAPES];      // L2 length (short base of back face)
  double fBlockL3Length[NUMBER_OF_BLOCK_SHAPES];      // L3 length (long base of front face)
  double fBlockL4Length[NUMBER_OF_BLOCK_SHAPES];      // L4 length (short base of front face)
  double fBlockW1Length[NUMBER_OF_BLOCK_SHAPES];      // W1 length (height of back face)
  double fBlockW2Length[NUMBER_OF_BLOCK_SHAPES];      // W2 length (height of front face)
  double fLightGuideZLength[NUMBER_OF_BLOCK_SHAPES];  // Light Guide Z length
  double fLightGuideDiameter[NUMBER_OF_BLOCK_SHAPES]; // Light Guide diameter
  double fMuMetalZLength[NUMBER_OF_BLOCK_SHAPES];     // u-metal cylinder Z length
  double fMuMetalDiameter[NUMBER_OF_BLOCK_SHAPES];    // u-metal cylinder external diameter
  double fMuMetalThickness[NUMBER_OF_BLOCK_SHAPES];   // u-metal cylinder thickness
  double fSteelSlabThickness[NUMBER_OF_BLOCK_SHAPES]; // Steel slab thickness

  
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

  // Path of efficiency/time matrices
  // Path to matrices used in the fast simulation
  std::string fLAVEfficiencyMatrix, fLAVTimeMatrix;
  fLAVEfficiencyMatrix = "LAVEff.txt";
  fLAVTimeMatrix = "LAVDelay.txt";

  for (int i=0; i<NUMBER_OF_BLOCK_SHAPES; i++) {  
    std::ostringstream oss;
    oss << i;
    StringInput = "LAVBlockType"+oss.str();
    int ssid = mDS->RegisterNewSubSystem("LAVBlockType",StringInput);

    std::cout << "Register BlockType " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","OpalID", fBlockOpalId[i],i,d2,d6);

    std::cout << "Register ZLength " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fBlockZLength", fBlockZLength[i],i,d2,d6);

    std::cout << "Register L1Length " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fBlockL1Length", fBlockL1Length[i],i,d2,d6);

    std::cout << "Register L1Length " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fBlockL1Length", fBlockL1Length[i],i,d2,d6);

    std::cout << "Register L2Length " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fBlockL2Length", fBlockL2Length[i],i,d2,d6);

    std::cout << "Register L3Length " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fBlockL3Length", fBlockL3Length[i],i,d2,d6);

    std::cout << "Register L4Length " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fBlockL4Length", fBlockL4Length[i],i,d2,d6);

    std::cout << "Register W1Length " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fBlockW1Length", fBlockW1Length[i],i,d2,d6);

    std::cout << "Register W2Length " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fBlockW2Length", fBlockW2Length[i],i,d2,d6);

    std::cout << "Register LightGuideZLength " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fLightGuideZLength", fLightGuideZLength[i],i,d2,d6);

    std::cout << "Register LightGuideDiameter " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fLightGuideDiameter", fLightGuideDiameter[i],i,d2,d6);

    std::cout << "Register MuMetalZLength " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fMuMetalZLength", fMuMetalZLength[i],i,d2,d6);

    std::cout << "Register MuMetalDiameter " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fMuMetalDiameter", fMuMetalDiameter[i],i,d2,d6);

    std::cout << "Register MuMetalThickness " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fMuMetalThickness", fMuMetalThickness[i],i,d2,d6);

    std::cout << "Register SteelSlabThickness " << i << " output is " << ssid << std::endl;
    setAttributeValueOutputCode = mDS->SetAttributeValue("LAVBlockType","fSteelSlabThickness", fSteelSlabThickness[i],i,d2,d6);

  }

  mDS->RegisterNewSubSystem("LAVStationConfiguration");
  std::vector<float> pippof;
  pippof.push_back(0.);
  pippof.push_back(1.E10);
  pippof.push_back(-40.);
  pippof.push_back(1.E57);
  int cicciu = mDS->SetAttributeValue("LAVStationConfiguration","NominalThreshold0", pippof, 0,d2,d6);
  std::cout << "Return value " << cicciu << std::endl;

  mDS->RegisterNewSubSystem("LAVStationCalibration");
  std::vector<double> pippod;
  pippod.push_back(0.);
  pippod.push_back(1.E57);
  pippod.push_back(-40.);
  pippod.push_back(-4.E57);
  int ciccie = mDS->SetAttributeValue("LAVStationCalibration","BlockTime0", pippod, 0,d2,d6);
  std::cout << "Return value " << ciccie << std::endl;

  return 0;
}


