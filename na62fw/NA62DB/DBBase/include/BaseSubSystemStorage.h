// --------------------------------------------------------------
// History:
//           
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) 2013-12-06
// --------------------------------------------------------------

#ifndef BaseSubSystemStorage_H
#define BaseSubSystemStorage_H 1
#include <string>
#include "RelationalAccess/ISessionProxy.h"
#include "RelationalAccess/ISchema.h"
#include "RelationalAccess/IConnectionService.h"

#include "RelationalAccess/AccessMode.h"
#include "CoralCommon/Utilities.h"
#include "BaseCache.h"

#include <fstream>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>

//#include <boost/archive/xml_iarchive.hpp>
//#include <boost/archive/xml_oarchive.hpp>



class BaseSubSystemStorage
{
public:

  //constructor
  BaseSubSystemStorage(coral::ISessionProxy *m_sess, std::string DetName, std::string UserName, int SSID, BaseCache *invCache, bool StartWithCache);

  //destructor
  virtual ~BaseSubSystemStorage();
//  int CheckValidity(int AttributeID, time_t time);
//  int CheckValidity(int AttributeID, tm* time);
  int CheckValidity_stored(int AttributeID, tm* time);
  int CheckValidity_stored(int AttributeID, time_t time);
  

//  int GetAttributeValue(void* value, int AttributeNum);
//  int GetAttributeValue(void* value, int AttributeNum, tm* time);
//  int GetAttributeValue(void* value, std::string AttributeName);
  	int GetAttributeValue_cached(void* value, std::string& AttributeName);
  	int GetAttributeValue_cached(void* value, std::string& AttributeName, const coral::TimeStamp &atTime, coral::TimeStamp &StartTimeVal, coral::TimeStamp &FinishTimeVal);

  
//  int SetAttributeValue(void* value, short value_size, int AttributeID);
//  int SetAttributeValue(void* value, short value_size, int AttributeID, tm* atTime);
//  int SetAttributeValue(void* value, short value_size, std::string AttributeName);
//  int SetAttributeValue_stored(void* value, short value_size, std::string DataTypeName, long long AttributeID, tm* atTime);

  // supported fundamental data types are: 
  // long long (assuming a de-facto standard of 64 bits)
  // int       (assuming a de-facto standard of 32 bits)
  // float     (assuming a de-facto standard of 32 bits)
  // double    (assuming a de-facto standard of 64 bits)
  // multitype involving the previous types (string should be treated as a multitype)
  // WARNING: if the above assumptions fail, the db might become unreadable

  int SetAttributeValue_stored(void* value, int value_size, std::string &AttributeName);
  int SetAttributeValue_stored(void* value, int value_size, coral::TimeStamp& startTime, coral::TimeStamp& finishTime, std::string& AttributeName);
  

//  int SetAttributeValue_stored(void* value, short value_size, std::string AttributeName);

  
  int FlushBufferWithAttributeValue(coral::TimeStamp startTime, coral::TimeStamp finishTime, std::string AttributeName);
  int PutAttributeValueToBuffer_cached(std::string &AttributeName, const coral::TimeStamp &atTime, coral::TimeStamp &StartTimeVal, coral::TimeStamp &FinishTimeVal);
  
  int SaveFileToDB(std::string &AttributeName, const std::string &FileName, coral::TimeStamp &startTime, coral::TimeStamp &finishTime);
  int ExtractFileFromDB(std::string &AttributeName, const std::string &FileName, const coral::TimeStamp &atTime, coral::TimeStamp &StartTimeVal, coral::TimeStamp &FinishTimeVal);

  int FinishValueValidityNow(int ValueID);
  long long CountRowsInValuesTable();

  boost::archive::text_oarchive *obuffer;
  boost::archive::text_iarchive *ibuffer;
  

/*
  boost::archive::binary_oarchive *obuffer;
  boost::archive::binary_iarchive *ibuffer;
*/


private:

  coral::ISessionProxy *m_session;
  std::string DetectorName;
  int  SubSystemID;
  std::string SubSystemTypeName;
  BaseCache *currentCache;
  std::stringstream *iss;
  std::stringstream *oss;


//  std::stringstream iss;
//  std::stringstream oss;


};


#endif




















