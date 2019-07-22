// --------------------------------------------------------------
// History:
//           
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) 2013-12-06
// --------------------------------------------------------------

#ifndef BaseCache_H
#define BaseCache_H 1
#include <string>
#include <map>

#include "RelationalAccess/ISessionProxy.h"
#include "RelationalAccess/ISchema.h"
#include "RelationalAccess/IConnectionService.h"

#include "RelationalAccess/AccessMode.h"
#include "CoralCommon/Utilities.h"
#include "CoralBase/TimeStamp.h"
#include "RelationalAccess/IQuery.h"
#include "RelationalAccess/ICursor.h"




class BaseCache
{
public:

  BaseCache(coral::ISessionProxy *m_sess, coral::ISessionProxy *cache_sess, std::string DetName);
  bool CreateValCache(const coral::TimeStamp &startTime, const coral::TimeStamp &finishTime, const std::vector<std::string> & tagged);
  std::string GetStorageType(std::string SubSystem, std::string Attribute);
  bool BuildAttrDataTypesCache(const coral::TimeStamp &startTime, const coral::TimeStamp &finishTime);


  // supported fundamental data types are: 
  // long long (assuming a de-facto standard of 64 bits)
  // int       (assuming a de-facto standard of 32 bits)
  // float     (assuming a de-facto standard of 32 bits)
  // double    (assuming a de-facto standard of 64 bits)
  // multitype involving the previous types (string should be treated as a multitype)
  // WARNING: if the above assumptions fail, the db might become unreadable
  bool GetDataFromCache(void* value, std::string &SubSystemTypeName, int SubSystemID, std::string &AttributeName, const coral::TimeStamp &atTime);
  bool GetDataFromCache(void* value, std::string &SubSystemTypeName, int SubSystemID, std::string &AttributeName, const coral::TimeStamp &atTime, coral::TimeStamp &StartTimeVal, coral::TimeStamp &FinishTimeVal);



  int GetSSID_cached(std::string SubSystemName, int DBID);
  std::string GetSubSystemTypeName(int SubSystem_ID);
  int GetDataSizeFromCache(std::string &SubSystemTypeName, int SubSystemID, std::string &AttributeName, const coral::TimeStamp &atTime, coral::TimeStamp& StartTimeVal, coral::TimeStamp& FinishTimeVal);

  bool ProcessTimeIntervalsAtCache();
  bool RefreshSubSystemsCache();



  
  
private:

  coral::ISessionProxy *m_session;
  coral::ISessionProxy *cache_session;

  std::string DetectorName;
  std::map<std::pair<std::string,std::string>, std::string> cachedAttrTypeMap;  
//  std::map<std::pair<std::TimeStamp,std::TimeStamp>, std::string> cachedAttrValMap;  
  
};




#endif
