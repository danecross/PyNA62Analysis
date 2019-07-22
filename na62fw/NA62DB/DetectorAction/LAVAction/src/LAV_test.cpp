//
//

#include <string>
#include "LAVDetectorStorage.h"

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

int GetAllValues(LAVDetectorStorage *);

int main( int argc, char *argv[])
{   

  //  std::string connString = "CernProduction";
  std::string connString = "OraCernProxy";
  std::string usrName = "LAV";
  coral::TimeStamp cacheStartTime(2000, 1, 2, 1, 1, 1, true);
  coral::TimeStamp cacheFinishTime(2038,1,2,0,0,0,0, true);

  std::vector<std::string> tagsRead;
  tagsRead.push_back("all");   //  tagsRead.push_back("construction_mc");

//  LAVDetectorStorage* LAVDetStorage;
//  int paperoga;
//  LAVDetStorage = new LAVDetectorStorage(connString,usrName, coral::Update);
//  LAVDetStorage->BuildCache(cacheStartTime, cacheFinishTime, tagsRead);
//  std::string StringInput = "Qualsiasi";  
//  LAVDetStorage->SetAttributeValue("LAV","Name",StringInput,0);
//  std::string StringOutput;
//  LAVDetStorage->GetAttributeValue("LAV","Name",StringOutput,0);
//  std::cout << "Qui la string in output di DBID=0 vale " << StringOutput << std::endl;
//  long long nRings = 42;
//  int paperingo = LAVDetStorage->SetAttributeValue("LAVStationGeometry","fLAV_Station_NRings", nRings, 0);//,cacheStartTime,cacheFinishTime);
//  std::cout << " Valore di set " << paperingo << std::endl;
//  long long nRingOut;
//  paperoga = LAVDetStorage->GetAttributeValue("LAVStationGeometry","fLAV_Station_NRings", nRingOut, 0);
//  std::cout << " Qui il valore in output vale " << nRingOut << " " << paperoga << std::endl;
//  delete LAVDetStorage;

  LAVDetectorStorage* LAVDetStorage = new LAVDetectorStorage(connString,usrName, coral::ReadOnly);
  LAVDetStorage->BuildCache(cacheStartTime, cacheFinishTime, tagsRead);
  GetAllValues(LAVDetStorage);
  delete LAVDetStorage;

  return 0;
}


int GetAllValues(LAVDetectorStorage * mDS)
{
  coral::TimeStamp d1, d2, d3, d4, d5, d6, d7; 
	
  d1 = coral::TimeStamp(2014,7,20,0,0,0,0);
  d2 = coral::TimeStamp(2014,8,10,0,0,0,0);
  d3 = coral::TimeStamp(2014,8,20,0,0,0,0);
  d4 = coral::TimeStamp(2014,9,10,0,0,0,0);	
  d5 = coral::TimeStamp(2014,9,20,0,0,0,0);
  d6 = coral::TimeStamp(2031,10,10,0,0,0,0);
  d7 = coral::TimeStamp(2031,10,20,0,0,0,0);  

  std::string StringOutput;
  double dOutput;

  int ret = mDS->GetAttributeValue("LAV","Name",StringOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " name= " << StringOutput << std::endl;
  ret = mDS->GetAttributeValue("LAV","Name",StringOutput,1);
  std::cout << "Retrieving content of table LAV: return value for DBID=1 is " << ret << " name= " << StringOutput << std::endl;
  ret = mDS->GetAttributeValue("LAV","fLAVFastSensitiveDetectorName", StringOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fLAVFastSensitiveDetectorName= " << StringOutput << std::endl;
  ret = mDS->GetAttributeValue("LAV","fLAVLeadglassSensitiveDetectorName", StringOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fLAVLeadglassSensitiveDetectorName= " << StringOutput << std::endl;

  ret = mDS->GetAttributeValue("LAV","fLAVGuideSensitiveDetectorName", StringOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fLAVGuideSensitiveDetectorName= " << StringOutput << std::endl;

  ret = mDS->GetAttributeValue("LAV","fLAVCathodeSensitiveDetectorName", StringOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fLAVCathodeSensitiveDetectorName= " << StringOutput << std::endl;

  ret = mDS->GetAttributeValue("LAV","fLAVCollectionName", StringOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fLAVCollectionName= " << StringOutput << std::endl;


  ret = mDS->GetAttributeValue("LAV","fWorldZLength", dOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fWorldZLength= " << dOutput << std::endl;
  ret = mDS->GetAttributeValue("LAV","fWorldXLength", dOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fWorldXLength= " << dOutput << std::endl;
  ret = mDS->GetAttributeValue("LAV","fWorldYLength", dOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fWorldYLength= " << dOutput << std::endl;


  ret = mDS->GetAttributeValue("LAV","fWrapFrontHole", dOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fWrapFrontHole= " << dOutput << std::endl;
  ret = mDS->GetAttributeValue("LAV","fWrapThick", dOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fWrapThick= " << dOutput << std::endl;
  ret = mDS->GetAttributeValue("LAV","fGlueThick", dOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fGlueThick= " << dOutput << std::endl;
  ret = mDS->GetAttributeValue("LAV","fAirThick", dOutput,0);
  std::cout << "Retrieving content of table LAV: return value for DBID=0 is " << ret << " fAirThick= " << dOutput << std::endl;


// LAV Responsibility region parameters

  double fLAV_RR_ZofFrontFace[NUMBER_OF_RESPONSIBILITY_REGIONS];
  double fLAV_RR_ZofBackFace[NUMBER_OF_RESPONSIBILITY_REGIONS];
  double fLAV_RR_Radius[NUMBER_OF_RESPONSIBILITY_REGIONS];
  for (int i=0; i<5; i++) {  
    std::ostringstream oss;
    oss << i+1;    
    ret = mDS->GetAttributeValue("LAVResponsibilityRegionGeometry","fLAV_RR_ZofFrontFace", fLAV_RR_ZofFrontFace[i],i);
    ret = mDS->GetAttributeValue("LAVResponsibilityRegionGeometry","fLAV_RR_ZofBackFace", fLAV_RR_ZofBackFace[i],i);
    ret = mDS->GetAttributeValue("LAVResponsibilityRegionGeometry","fLAV_RR_Radius", fLAV_RR_Radius[i],i);

    std::cout << "Retrieving content of table LAVResponsibilityRegionGeometry: return value for DBID=" << i << " is " << ret 
	      << " fLAV_RR_ZofFrontFace= " << fLAV_RR_ZofFrontFace[i]
	      << " fLAV_RR_ZofBackFace= " << fLAV_RR_ZofBackFace[i]
	      << " fLAV_RR_Radius= " << fLAV_RR_Radius[i] << std::endl;
  }

// Blue-tube section parameters
  int fLAV_BT_ResponsibilityRegion[NUMBER_OF_BLUETUBE_ZONES];
  double fLAV_BT_ZofFrontFace[NUMBER_OF_BLUETUBE_ZONES];
  double fLAV_BT_ZofBackFace[NUMBER_OF_BLUETUBE_ZONES];
  double fLAV_BT_InnerRadius[NUMBER_OF_BLUETUBE_ZONES];
  double fLAV_BT_OuterRadius[NUMBER_OF_BLUETUBE_ZONES];
  int  fLAV_BT_HasRails[NUMBER_OF_BLUETUBE_ZONES];

  for (int i=0; i<NUMBER_OF_BLUETUBE_ZONES; i++) {  
    ret = mDS->GetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_ResponsibilityRegion", fLAV_BT_ResponsibilityRegion[i],i);
    ret = mDS->GetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_ZofFrontFace", fLAV_BT_ZofFrontFace[i],i);
    ret = mDS->GetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_ZofBackFace", fLAV_BT_ZofBackFace[i],i);
    ret = mDS->GetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_InnerRadius", fLAV_BT_InnerRadius[i],i);
    ret = mDS->GetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_OuterRadius", fLAV_BT_OuterRadius[i],i);
    ret = mDS->GetAttributeValue("LAVBTResponsibilityRegionGeometry","fLAV_BT_HasRails", fLAV_BT_HasRails[i],i);

    std::cout << "Retrieving content of table LAVBTResponsibilityRegionGeometry: return value for DBID=" << i << " is " << ret 
	      << " fLAV_BT_ResponsibilityRegion= " << fLAV_BT_ResponsibilityRegion[i]
	      << " fLAV_BT_ZofFrontFace= " << fLAV_BT_ZofFrontFace[i]
	      << " fLAV_BT_ZofBackFace= "  << fLAV_BT_ZofBackFace[i]
	      << " fLAV_BT_InnerRadius= "  << fLAV_BT_InnerRadius[i]
	      << " fLAV_BT_OuterRadius= "  << fLAV_BT_OuterRadius[i]
	      << " fLAV_BT_HasRails= "     << fLAV_BT_HasRails[i]
	      << std::endl;

  }

  // Number and XY coordinates of blue tube rails' vertices

  int       fBTRailNVertices[NUMBER_OF_BTRAILS];    // Number of vertices of BT rail
  std::vector< std::vector<double> > fBTRailVertex[NUMBER_OF_BTRAILS];
  for (int i=0; i<NUMBER_OF_BTRAILS; i++) {  
    ret = mDS->GetAttributeValue("LAVBTRails","fBTRailNVertices",fBTRailNVertices[i],i);
    ret = mDS->GetAttributeValue("LAVBTRails","fBTRailVertex",fBTRailVertex[i],i);

    std::cout << "Retrieving content of table LAVBTRails: return value for DBID= " << i << " is " << ret 
	      << " fBTRailNVertices " << fBTRailNVertices[i]
	      << " size of vtx array " << fBTRailVertex[i].size() 
	      << std::endl;
    for (int j=0; j< (int) fBTRailNVertices[i]; j++){
      std::vector<double> vtxTmp = fBTRailVertex[i].at(j);
      int vtxSize = vtxTmp.size();
      std::cout << "Vertex " << j << " has size " << vtxSize << std::endl;
      for (int k=0; k< (int) vtxSize; k++)
	std::cout << "innerVtx " << k << " = " << vtxTmp.at(k) ;
      std::cout << std::endl;
    }
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

  for (int i=0; i<12; i++) {  
    ret = mDS->GetAttributeValue("LAVBeamPipeGeometry","fBeamPipeZPosition", fBeamPipeZPosition[i],i);
    ret = mDS->GetAttributeValue("LAVBeamPipeGeometry","fBeamPipeInnerRadius", fBeamPipeInnerRadius[i],i);
    ret = mDS->GetAttributeValue("LAVBeamPipeGeometry","fBeamPipeOuterRadius", fBeamPipeOuterRadius[i],i);
    ret = mDS->GetAttributeValue("LAVBeamPipeGeometry","fBeamPipeFinZLength", fBeamPipeFinZLength[i],i);
    ret = mDS->GetAttributeValue("LAVBeamPipeGeometry","fBeamPipeFinOuterRadius", fBeamPipeFinOuterRadius[i],i);
    ret = mDS->GetAttributeValue("LAVBeamPipeGeometry","fBeamPipeFinSpacing", fBeamPipeFinSpacing[i],i);
    ret = mDS->GetAttributeValue("LAVBeamPipeGeometry","fBeamPipeInputDisplacementWRTBeam", fBeamPipeInputDisplacementWRTBeam[i],i);
    ret = mDS->GetAttributeValue("LAVBeamPipeGeometry","fBeamPipeOutputDisplacementWRTBeam", fBeamPipeOutputDisplacementWRTBeam[i],i);

    std::cout << "Retrieving content of table LAVBeamPipeGeometry: return value for DBID=" << i << " is " << ret 
	      << "fBeamPipeZPosition"     << fBeamPipeZPosition[i]
	      << "fBeamPipeInnerRadius" << fBeamPipeInnerRadius[i]
	      << "fBeamPipeOuterRadius" << fBeamPipeOuterRadius[i]
	      << "fBeamPipeFinZLength"  << fBeamPipeFinZLength[i]
	      << "fBeamPipeFinOuterRadius" << fBeamPipeFinOuterRadius[i]
	      << "fBeamPipeFinSpacing" << fBeamPipeFinSpacing[i]
	      << "fBeamPipeInputDisplacementWRTBeam" << fBeamPipeInputDisplacementWRTBeam[i]
	      << "fBeamPipeOutputDisplacementWRTBeam" << fBeamPipeOutputDisplacementWRTBeam[i]
	      << std::endl;
  }

// LAV geometry parameters: 12 instances

  long long fLAV_Station_NRings[12];
  long long fLAV_Station_NBananasPerRing[12];
  int fLAV_TypeOfCrystal[12][5]; // vector of opal types used for each layer for each station
  int fLAV_Station_BananaType[12];
  int fLAV_Station_ResponsibilityRegion[12];
  double fLAV_Station_ZofFrontFace[12];
  double fLAV_Station_ZofBackFace[12];
  double fLAV_Station_InnerRadius[12];
  double fLAV_Station_OuterRadius[12];
  double fLAV_Vessel_OuterRadius[12];
  double fLAV_Vessel_InnerRadius[12];
  double fLAV_Vessel_Thickness[12];
  double fLAV_Station_FirstRingZPos[12];
  double fLAV_Station_RingGap[12];
  double fLAV_Station_PhiReference[12];
  double fLAV_Station_PhiRotationBetweenLayers[12];

  for (int i=0; i<12; i++) {  
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_NRings", fLAV_Station_NRings[i],i);
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_NBananasPerRing", fLAV_Station_NBananasPerRing[i], i);

    std::vector<int> temporaryVec;
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_TypeOfCrystal",temporaryVec,i); 
    for (int k=0; k<(int) temporaryVec.size(); k++) 
      fLAV_TypeOfCrystal[i][k] = temporaryVec.at(k);
    

    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_BananaType",fLAV_Station_BananaType[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_ResponsibilityRegion",fLAV_Station_ResponsibilityRegion[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_ZofFrontFace",fLAV_Station_ZofFrontFace[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_ZofBackFace",fLAV_Station_ZofBackFace[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_InnerRadius",fLAV_Station_InnerRadius[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_OuterRadius",fLAV_Station_OuterRadius[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Vessel_OuterRadius",fLAV_Vessel_OuterRadius[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Vessel_InnerRadius",fLAV_Vessel_InnerRadius[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Vessel_Thickness",fLAV_Vessel_Thickness[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_FirstRingZPos",fLAV_Station_FirstRingZPos[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_RingGap",fLAV_Station_RingGap[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_PhiReference",fLAV_Station_PhiReference[i],i); 
    ret = mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_PhiRotationBetweenLayers",fLAV_Station_PhiRotationBetweenLayers[i],i); 

    std::cout << "Retrieving content of table LAVStationGeometry: return value for DBID=" << i << " is " << ret 
	      << "fLAV_Station_NRings"  << fLAV_Station_NRings[i]
	      << "fLAVStation_NBananasPerRing" << fLAV_Station_NBananasPerRing[i]
	      << "fLAV_TypeOfCristal Nr = " << temporaryVec.size();
    for (int k=0; k<(int) temporaryVec.size(); k++) std::cout << " type " << k << " is " << fLAV_TypeOfCrystal[i][k];
    std::cout << "fLAV_Station_BananaType" << fLAV_Station_BananaType[i]
	      << "fLAV_Station_ResponsibilityRegion" << fLAV_Station_ResponsibilityRegion[i]
	      << "fLAV_Station_ZofFrontFace" << fLAV_Station_ZofFrontFace[i]
	      << "fLAV_Station_ZofBackFace" << fLAV_Station_ZofBackFace[i]
	      << "fLAV_Station_InnerRadius" << fLAV_Station_InnerRadius[i]
	      << "fLAV_Station_OuterRadius" << fLAV_Station_OuterRadius[i]
	      << "fLAV_Vessel_OuterRadius" << fLAV_Vessel_OuterRadius[i]
	      << "fLAV_Vessel_InnerRadius" << fLAV_Vessel_InnerRadius[i]
	      << "fLAV_Vessel_Thickness" << fLAV_Vessel_Thickness[i]
	      << "fLAV_Station_FirstRingZPos" << fLAV_Station_FirstRingZPos[i]
	      << "fLAV_Station_RingGap" << fLAV_Station_RingGap[i]
	      << "fLAV_Station_PhiReference" << fLAV_Station_PhiReference[i]
	      << "fLAV_Station_PhiRotationBetweenLayers" << fLAV_Station_PhiRotationBetweenLayers[i]
	      << std::endl;


  }



//  for (int i=0; i<12; i++) {  
//    long long nring;
//    mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_NRings",nring,i);
//    std::cout << " read " << i << " nring = " << nring << std::endl;
//    //    float ringGap;
//    double ringGap;
//    mDS->GetAttributeValue("LAVStationGeometry","fLAV_Station_RingGap",ringGap,i);
//    std::cout << " read " << i << " ringGap = " << ringGap << std::endl;
//    
//    std::vector<int> pippo;
//    mDS->GetAttributeValue("LAVStationGeometry","fLAV_TypeOfCrystal",pippo,i);
//    std::cout << " read " << i << " typeOfCrystal with " << pippo.size() << " components " << std::endl;
//    for (int j=0; j<(int) pippo.size(); j++) std::cout << " component " << j << " is " << pippo.at(j) << std::endl;
//
//  }
//
//  std::vector<float> pippof;
//  mDS->GetAttributeValue("LAVStationConfiguration","NominalThreshold0",pippof,0);
//  std::cout << " read  NominalThreshold0 with " << pippof.size() << " components " << std::endl;
//  for (int j=0; j<(int) pippof.size(); j++) std::cout << " component " << j << " is " << pippof.at(j) << std::endl;
//
//  std::vector<double> pippod;
//  mDS->GetAttributeValue("LAVStationCalibration","BlockTime0",pippod,0);
//  std::cout << " read BlockTime0 with " << pippod.size() << " components " << std::endl;
//  for (int j=0; j<(int) pippod.size(); j++) std::cout << " component " << j << " is " << pippod.at(j) << std::endl;
//
  return 0;



}


