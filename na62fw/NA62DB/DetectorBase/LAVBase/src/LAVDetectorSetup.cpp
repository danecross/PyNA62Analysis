// --------------------------------------------------------------
// History:
//
// Created by E. Leonardi and T. Spadaro (emanuele.leonardi@cern.ch tommaso.spadaro@cern.ch) 2015-03-11
// Class to define and deploy structure of LAV db to the central database
// Various SetAttributeValue methods have been placed in the BaseDetectorStorage class, with the following data types handled there:
//    int, int64_t, long long, float, double, vector<int>, vector<float>, vector<long long>, vector<double>, vector<int64_t>, string, vector<string>, vector<vector<double>>
// In case other data types are needed, the user should add the related methods here. 
// The RegisterNewSubsystem method has been protected, to avoid unwanted multiple calls produce multiple copies of subsystems
// --------------------------------------------------------------

/// \file LAVDetectorSetup.cpp

#include "LAVDetectorSetup.h"
#include <iostream>
#include <string>

using namespace std;

LAVDetectorSetup::LAVDetectorSetup(std::string connString, std::string UsrName, coral::AccessMode oracleAccessMode):BaseDetectorStorage(connString, "LAV", UsrName, oracleAccessMode)
{
;
}


LAVDetectorSetup::~LAVDetectorSetup()
{
}

/// \bug Always return 0, no checks
int LAVDetectorSetup::DeployDetectorStructureToDB()
{
  
  int tmpid = 0;
  
  //                                                                          //
  // ----------------------- Specific to LAV  ------------------------------- //
  //                                                                          //
  
  // Define data types
  // Syntax: DataTypeName, StorageDataType, Units, Description
  // Allowed StorageDataType are long long, double, multitype and string 
  // Rem. string is not realiable (char *), use multitype and std::string instead
  // Now, the BaseDetectorStorage class includes the GetAttributeValue methods for the following data types:
  //     int, int64_t, long long, float, double, vector<int>, vector<float>, vector<long long>, vector<double>, vector<int64_t>, vector<vector<double>>, string, vector<string>
  // If other data types are needed, please add specific methods in your DetectorStorage specific class.

// Accepted Format "long long"
// Accepted Format "int"
// Accepted Format "float"
// Accepted Format "double"
// Accepted Format "multitype"

  AddNewDataType("UID","long long", "", "Unique Identifier");

  AddNewDataType("String","multitype","","std:string");

  AddNewDataType("VecString","multitype","","std:vector<std::string>");

  AddNewDataType("Count","long long","","");

  AddNewDataType("Type","long long","","");
  AddNewDataType("VecOfTypes","multitype","","std::vector<int>");

  AddNewDataType("Length","double","mm",""); //was multitype
  AddNewDataType("Angle","float","deg",""); // was multitype
  AddNewDataType("VecOf2dCoord","multitype","mm","std::vector<std::vector<double>>");
  AddNewDataType("Time","float","ns","");   //was multitype
  AddNewDataType("VecOfTimes","multitype","ns","std::vector<double>");
  AddNewDataType("VecOfVoltages","multitype","mV","std::vector<float>");
  AddNewDataType("VecOfCounts","multitype","","std::vector<long long>");


  // Define all sub-systems *types*
  // FIXME: rename to AddSubSystemType
  
  AddSubSystemType_stored("LAV");
  AddSubSystemType_stored("LAVStationGeometry");
  AddSubSystemType_stored("LAVResponsibilityRegionGeometry");
  AddSubSystemType_stored("LAVBTResponsibilityRegionGeometry");
  AddSubSystemType_stored("LAVBTRails");

  AddSubSystemType_stored("LAVBeamPipeGeometry");
  AddSubSystemType_stored("LAVBlockType");
  AddSubSystemType_stored("LAVBananaType");
  AddSubSystemType_stored("LAVStationConfiguration");
  AddSubSystemType_stored("LAVStationCalibration");

  // Define all attributes
  // Syntax: AttrName, DataTypeName, makevalid (?)
  // Rem. See above for DataTypeName
 	
  BaseSubSystemTypeManager *stm;
  stm = GetBaseSubSystemTypeManager("LAV");
  tmpid = stm->AddAttribute("DBID", "UID", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("Name", "String", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAVFastSensitiveDetectorName", "String", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAVLeadglassSensitiveDetectorName", "String", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAVGuideSensitiveDetectorName", "String", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAVCathodeSensitiveDetectorName", "String", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAVCollectionName", "String", true); 
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fWorldZLength", "Length", true); 
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fWorldXLength", "Length", true); 
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fWorldYLength", "Length", true); 
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fWrapFrontHole", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fWrapThick", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAirThick", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fGlueThick", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  delete stm;

  stm = GetBaseSubSystemTypeManager("LAVResponsibilityRegionGeometry");
//  tmpid = stm->AddAttribute("DBID", "UID", true);
//  stm->TagAttribute("construction_mc", tmpid);
//  tmpid = stm->AddAttribute("Name", "String", true);
//  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_RR_ZofFrontFace", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_RR_ZofBackFace", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_RR_Radius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  delete stm;

  stm = GetBaseSubSystemTypeManager("LAVBTResponsibilityRegionGeometry");
  tmpid = stm->AddAttribute("fLAV_BT_ResponsibilityRegion", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_BT_ZofFrontFace", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_BT_ZofBackFace", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_BT_InnerRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_BT_OuterRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_BT_HasRails", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  delete stm;


  stm = GetBaseSubSystemTypeManager("LAVBTRails");
  tmpid = stm->AddAttribute("fBTRailNVertices", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBTRailVertex", "VecOf2dCoord", true);
  stm->TagAttribute("construction_mc", tmpid);
  delete stm;

  stm = GetBaseSubSystemTypeManager("LAVBeamPipeGeometry");
  tmpid = stm->AddAttribute("fBeamPipeZPosition", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBeamPipeInnerRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBeamPipeOuterRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBeamPipeFinZLength", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBeamPipeFinOuterRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBeamPipeFinSpacing", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBeamPipeInputDisplacementWRTBeam", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBeamPipeOutputDisplacementWRTBeam", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  delete stm;



  stm = GetBaseSubSystemTypeManager("LAVStationGeometry");
  tmpid = stm->AddAttribute("fLAV_Station_ResponsibilityRegion", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_ZofFrontFace", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_ZofBackFace", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_InnerRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_OuterRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Vessel_OuterRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Vessel_InnerRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Vessel_Thickness", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_FirstRingZPos", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);

  tmpid = stm->AddAttribute("fLAV_Station_NRings", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_NBananasPerRing", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_BananaType", "Type", true); // points to DBID of BananaType subsystem
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_RingGap", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_PhiReference", "Angle", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLAV_Station_PhiRotationBetweenLayers", "Angle", true);
  stm->TagAttribute("construction_mc", tmpid);

  tmpid = stm->AddAttribute("fLAV_TypeOfCrystal", "VecOfTypes", true); // points to DBID of BlockType subsystem
  stm->TagAttribute("construction_mc", tmpid);
//  tmpid = stm->AddAttribute("fLAV_BlockName", "VecString", true);
//  stm->TagAttribute("construction_mc", tmpid);

  delete stm;
  

  stm = GetBaseSubSystemTypeManager("LAVBlockType");
  tmpid = stm->AddAttribute("OpalID", "Type", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBlockZLength", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBlockL1Length", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBlockL2Length", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBlockL3Length", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBlockL4Length", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBlockW1Length", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBlockW2Length", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLightGuideZLength", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fLightGuideDiameter", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fMuMetalZLength", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fMuMetalDiameter", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fMuMetalThickness", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fSteelSlabThickness", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  delete stm;

  stm = GetBaseSubSystemTypeManager("LAVBananaType");
//  tmpid = stm->AddAttribute("DBID", "UID", true);
//  stm->TagAttribute("construction_mc", tmpid);
//  tmpid = stm->AddAttribute("Name", "String", true);
//  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBananaOuterRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBananaInnerRadius", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBananaThickness", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBananaThicknessTolerance", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlSlabThickness", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlSlabNVertices", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlSlabVertex", "VecOf2dCoord", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlColumnLength", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlColumnDiameter", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBananaNAlColumns", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlColumnPosition", "VecOf2dCoord", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlCShapeNVertices", "Count", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlCShapeVertex", "VecOf2dCoord", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlCShapeHeight", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fAlCShapeHoleDiameter", "Length", true);
  stm->TagAttribute("construction_mc", tmpid);
  tmpid = stm->AddAttribute("fBananaAlCShapeScrewsPosition", "VecOf2dCoord", true);
  stm->TagAttribute("construction_mc", tmpid);
  delete stm;


  stm = GetBaseSubSystemTypeManager("LAVStationConfiguration");
//  tmpid = stm->AddAttribute("DBID", "UID", true);
//  stm->TagAttribute("configuration_data", tmpid);
//  tmpid = stm->AddAttribute("Name", "String", true);
//  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("NominalThreshold0", "VecOfVoltages", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("NominalThreshold1", "VecOfVoltages", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("ReadOutThreshold0", "VecOfVoltages", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("ReadOutThreshold1", "VecOfVoltages", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("ElectronicChannelID0", "VecOfCounts", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("ElectronicChannelID1", "VecOfCounts", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("HV", "VecOfVoltages", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("ElectronicChannelStatus0", "VecOfTypes", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("ElectronicChannelStatus1", "VecOfTypes", true);
  stm->TagAttribute("configuration_data", tmpid);
  delete stm;
 

  stm = GetBaseSubSystemTypeManager("LAVStationCalibration");
  //  tmpid = stm->AddAttribute("DBID", "UID", true);
  //  stm->TagAttribute("configuration_data", tmpid);
  //  tmpid = stm->AddAttribute("Name", "String", true);
  //  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("GlobalTime0", "Time", true);
  stm->TagAttribute("configuration_data", tmpid);
  tmpid = stm->AddAttribute("BlockTime0", "VecOfTimes", true);
  stm->TagAttribute("configuration_data", tmpid);
  delete stm;

  coral::sleepSeconds(1);
  
  //                                                                          //
  // ------------------------------------------------------------------------ //
  //                                                                          //
  
  return 0;
}


