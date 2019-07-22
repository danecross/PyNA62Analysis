// --------------------------------------------------------------
// History:
//
// Created by E. Leonardi and T. Spadaro (emanuele.leonardi@cern.ch tommaso.spadaro@cern.ch) 2015-03-11
// Class to retrieve data from LAV db
// Originally, various interfaces were declared here, with different data types, such as:
//    int GetAttributeValue(std::string ss_name, std::string attr_name, int & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
// 
// Now, the BaseDetectorStorage class includes the GetAttributeValue methods for the following data types:
//     int, int64_t, long long, float, double, vector<int>, vector<float>, vector<long long>, vector<double>, vector<int64_t>, vector<vector<double>>, string, vector<string>
// If more types are needed, add specific methods here
// --------------------------------------------------------------

/// \file LAVDetectorStorage.cpp

#include "LAVDetectorStorage.h"
#include <iostream>
#include <string>

using namespace std;

LAVDetectorStorage::LAVDetectorStorage():BaseDetectorStorage("OraCernProxy", "LAV", "LAV", coral::ReadOnly)
{;}
LAVDetectorStorage::LAVDetectorStorage(std::string connString, std::string UsrName, coral::AccessMode oracleAccessMode):BaseDetectorStorage(connString, "LAV", UsrName, oracleAccessMode)
{;}

int LAVDetectorStorage::BuildCache(const coral::TimeStamp &startTime, const coral::TimeStamp &finishTime, const std::vector<std::string> &tagged){
  return BaseDetectorStorage::BuildCache(startTime,finishTime,tagged);
}

int LAVDetectorStorage::BuildCache()
{
  coral::TimeStamp cacheStartTime(2000, 1, 2, 1, 1, 1, true);
  coral::TimeStamp cacheFinishTime(2038,1,2,0,0,0,0, true);
  std::vector<std::string> tagsRead;
  tagsRead.push_back("all");   //  tagsRead.push_back("construction_mc");
  return BuildCache(cacheStartTime, cacheFinishTime, tagsRead);  
}

LAVDetectorStorage::~LAVDetectorStorage()
{
	//FIXME
}

