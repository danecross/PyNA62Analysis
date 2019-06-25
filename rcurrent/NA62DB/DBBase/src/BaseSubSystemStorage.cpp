// --------------------------------------------------------------
// History:
//           
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) 2013-12-06
// --------------------------------------------------------------

#include "BaseSubSystemStorage.h"
#include "RelationalAccess/ITransaction.h"
#include "CoralBase/Exception.h"
#include "CoralBase/TimeStamp.h"
#include "CoralBase/AttributeSpecification.h"
#include "CoralBase/AttributeList.h"
#include "CoralBase/Attribute.h"
#include "RelationalAccess/TableDescription.h"


#include "RelationalAccess/ISessionProxy.h"
#include "RelationalAccess/ITransaction.h"
#include "RelationalAccess/ISchema.h"
#include "RelationalAccess/ITable.h"
#include "RelationalAccess/ITablePrivilegeManager.h"
#include "RelationalAccess/IColumn.h"
#include "RelationalAccess/IIndex.h"
#include "RelationalAccess/IPrimaryKey.h"
#include "RelationalAccess/IForeignKey.h"
#include "RelationalAccess/IUniqueConstraint.h"
#include "RelationalAccess/IViewFactory.h"
#include "RelationalAccess/IView.h"
#include "RelationalAccess/IBulkOperation.h"
#include "RelationalAccess/ITableDataEditor.h"
#include "RelationalAccess/IQuery.h"
#include "RelationalAccess/ICursor.h"

#include "CoralBase/Blob.h"
#include "CoralBase/TimeStamp.h"
#include "CoralBase/Date.h"

#include "CoralCommon/Utilities.h"

#include "base64.h"


#include <climits>
#include <algorithm>
#include <iterator>

using namespace std;

/*
void decrypt(std::istream& input, std::ostream& output)
{
    while (input.good())
    {
        char c = input.get();
        c ^= mask;
        output.put(c);

        if (output.bad())
        {
            throw std::runtime_error("Output to stream failed.");
        }
    }
}
*/


void fixstring(std::string &str)
{
  
  int lastspaceposition = str.size();
  for (int i = str.size()-1; i >=0; i--)
  {
    if (((int)(str.at(i))) !=32)
      break;
    lastspaceposition = i;
  }
  
  
  for (int i = lastspaceposition; i < str.size(); i++)
    {
      str.erase(i);
    }
}


BaseSubSystemStorage::BaseSubSystemStorage(coral::ISessionProxy *m_sess, std::string DetName, 
					   std::string UserName, int SSID,   BaseCache *invCache, bool StartWithCache):m_session(m_sess), DetectorName(DetName), SubSystemID(SSID), currentCache(invCache)
{
  if (!StartWithCache)
  {

   /*
   m_session->transaction().start(true);
   
   coral::IQuery* query1 = m_session->nominalSchema().tableHandle(DetectorName+"_DetectorSubSystem").newQuery();
   std::ostringstream condition;
   condition.str("");
   condition << " SubSystemID=" << SubSystemID;
   query1->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor5 = query1->execute();

   if (cursor5.next())
   {
    const coral::AttributeList& alh = cursor5.currentRow();
    SubSystemTypeName = alh["SubSystemTypeName"].data<std::string>();
   }
   delete query1;
   m_session->transaction().commit();
  */
  invCache->RefreshSubSystemsCache();
  SubSystemTypeName = invCache->GetSubSystemTypeName(SubSystemID);
  
  }
  
  else
  {
    SubSystemTypeName = invCache->GetSubSystemTypeName(SubSystemID);
  }

/*
  obuffer = 0;
  ibuffer = 0;
*/

  iss = new std::stringstream(std::stringstream::in|std::stringstream::out|std::stringstream::binary);
  oss = new std::stringstream(std::stringstream::in|std::stringstream::out|std::stringstream::binary);

  obuffer = new boost::archive::text_oarchive(*oss, boost::archive::no_codecvt | boost::archive::no_header);
  ibuffer = new boost::archive::text_iarchive(*iss, boost::archive::no_codecvt | boost::archive::no_header);


}



BaseSubSystemStorage::~BaseSubSystemStorage()
{
  delete obuffer;
  delete ibuffer;
}


/*
int BaseSubSystemStorage::SetAttributeValue(void* value, short value_size, std::string AttributeName)
{

  struct tm  tm;
  time_t rawtime;
  time ( &rawtime );
  tm = *localtime ( &rawtime );
  tm.tm_year = 2099;
  tm.tm_mon = 1 - 1;
  tm.tm_mday = 1;
  tm.tm_hour = 1;
  tm.tm_min = 0;
  tm.tm_sec = 0;
  mktime(&tm);
  
  m_session->transaction().start(true);
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
   std::ostringstream condition;
   coral::ICursor& cursor = query->execute();
   bool exist = false;
   int count = 0;
   std::string AttrName = "";
   int AttributeID = -1;

  
   while (cursor.next())
    {
      const coral::AttributeList& alh = cursor.currentRow();
	AttrName = alh["Name"].data<std::string>();
	fixstring(&AttrName);	
        if (AttributeName == AttrName)
      {
	break;
      }
      count++;
    }
   delete query;
   
//   std::cout <<  count << std::endl;

  m_session->transaction().commit();
  
  return SetAttributeValue(value, value_size, count, &tm);
  
}
*/



/*
int BaseSubSystemStorage::GetAttributeValue(void* value, std::string AttributeName)
{
  struct tm  tm;
  time_t t = time(NULL);
  tm = *gmtime ( &t );
  
   m_session->transaction().start(true);
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
   std::ostringstream condition;
   coral::ICursor& cursor = query->execute();
   bool exist = false;
   int count = 0;
   std::string AttrName = "";
   int AttributeID = -1;

   while (cursor.next())
    {
      const coral::AttributeList& alh = cursor.currentRow();
	AttrName = alh["Name"].data<std::string>();
	fixstring(&AttrName);	
       if (AttributeName == AttrName)
      {
	break;
      }
      count++;
    }
   delete query;

  m_session->transaction().commit();

  
  return GetAttributeValue(value,  count, &tm);
}

*/

/*
int BaseSubSystemStorage::SetAttributeValue(void* value, short value_size, int AttributeID)
{
  struct tm  tm;
  time_t rawtime;
  time ( &rawtime );
  tm = *localtime ( &rawtime );
  tm.tm_year = 2038;
  tm.tm_mon = 1 - 1;
  tm.tm_mday = 1;
  tm.tm_hour = 1;
  tm.tm_min = 0;
  tm.tm_sec = 0;
  mktime(&tm);
  return SetAttributeValue(value, value_size, AttributeID, &tm);
}

*/


int BaseSubSystemStorage::FlushBufferWithAttributeValue(coral::TimeStamp startTime, coral::TimeStamp finishTime, std::string AttributeName)
{
  
  SetAttributeValue_stored( (void*) (oss->str().c_str()), (oss->str().length()), startTime, finishTime, AttributeName);
  oss->str("");
  oss->str().erase();
  return 0;
}



int BaseSubSystemStorage::PutAttributeValueToBuffer_cached(std::string &AttributeName, const coral::TimeStamp &atTime, coral::TimeStamp &StartTimeVal, coral::TimeStamp &FinishTimeVal)
{
  iss->str("");
  iss->str().erase();
  iss->clear();
  int size = currentCache->GetDataSizeFromCache(SubSystemTypeName, SubSystemID, AttributeName,
				 atTime, StartTimeVal, FinishTimeVal);

  std::cout << "PutAttributeValueToBuffer_cached size = " << size << " time = " << atTime.toString() << " startTime = " << StartTimeVal.toString() << " endTime = " << FinishTimeVal.toString() << std::endl;
  
  // No data
  if(size == -1) return -1;
  
  char *value = (char *)malloc(size*sizeof(char));

  int ret = currentCache->GetDataFromCache(value, SubSystemTypeName, SubSystemID, AttributeName,
				 atTime, StartTimeVal, FinishTimeVal);
  
  *iss << value;
  free(value);
  return ret;
}


int BaseSubSystemStorage::SaveFileToDB(std::string &AttributeName, const std::string &FileName, coral::TimeStamp &startTime, coral::TimeStamp &finishTime)
{
  std::ifstream file(FileName.c_str(), std::ios_base::in | std::fstream::binary);
  if (file.is_open())
  {
    oss->str("");
    oss->str().erase();

    filebuf *pbuf=file.rdbuf();
    long size=pbuf->pubseekoff (0,ios::end,ios::in);
    pbuf->pubseekpos (0,ios::in);

    
    char * buffer=new char[size];
    pbuf->sgetn (buffer,size);
    std::string encoded = base64_encode( (unsigned char*) buffer, size);
    oss->write(encoded.c_str(), encoded.length());
   
    std::cerr << "encoded.length() " << encoded.length()<< std::endl;

    
    delete [] buffer;
    FlushBufferWithAttributeValue(startTime, finishTime, AttributeName);
/*    file.close();
    oss->str("");
    oss->str().erase();
*/
  }
  else
    return -1;
 return 0;
}




int BaseSubSystemStorage::ExtractFileFromDB(std::string &AttributeName, const std::string &FileName, const coral::TimeStamp &atTime, coral::TimeStamp& StartTimeVal, coral::TimeStamp& FinishTimeVal)
{
 if (PutAttributeValueToBuffer_cached(AttributeName, atTime, StartTimeVal, FinishTimeVal) > -1)
 {
    std::ofstream file(FileName.c_str(), std::ios_base::out | std::ios_base::binary);
    if (file.is_open())
    {
	std::string decoded = base64_decode(iss->str());
        file.write(decoded.c_str(), decoded.length());
	file.flush();
	file.close();
        iss->str("");
	iss->str().erase();
    }
    else
     return -2;
 }
 else
  return -1;
}


int BaseSubSystemStorage::SetAttributeValue_stored(void* value, int value_size, coral::TimeStamp& startTime, coral::TimeStamp& finishTime, std::string& AttributeName)
{
  

  fixstring(AttributeName);
  fixstring(SubSystemTypeName);

  
  //currentCache
  m_session->transaction().start();
  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  time_t t;
  t = time(NULL);
  struct tm *curTime;
  curTime = gmtime(&t);
  inputData.extend<std::string>( "Detector_prefix" );
  inputData.extend<coral::Blob>( "StoredValue" );
  inputData.extend<std::string>( "AttributeName" );
  inputData.extend<coral::TimeStamp>( "StartTime" );
  inputData.extend<coral::TimeStamp>( "FinishTime" );
  inputData.extend<std::string>( "OutResult" );
  inputData.extend<int>( "SubSystemID" );
  inputData[0].data<std::string>() = DetectorName;
  coral::Blob &blob = inputData[1].data<coral::Blob>();
  inputData[2].data<std::string>() = AttributeName;
  inputData[3].data<coral::TimeStamp>() = startTime;
  inputData[4].data<coral::TimeStamp>() = finishTime;
  
  std::string &tmpstr = inputData[5].data<std::string>();
  tmpstr.resize (200,' ');
 
  inputData[6].data<int>() = SubSystemID;
  std::string DataTypeName = currentCache->GetStorageType(SubSystemTypeName, AttributeName);
  fixstring(DataTypeName);
  

  std::cout << "DataTypeName is " << DataTypeName << std::endl;
    
  if (DataTypeName == "")
  {
    
  }
  if (DataTypeName == "long long")
  {
    blob.resize(sizeof(long long)+1);
    long long* p = static_cast<long long*>(blob.startingAddress());
    long long * p1 = (long long*) value;
	*p = *p1;
  }
  else if (DataTypeName == "int")
  {
    blob.resize(sizeof(int)+1);
    int* p = static_cast<int*>(blob.startingAddress());
    int* p1 = (int*) value;
    *p = *p1;
  }
  else if (DataTypeName == "float")
  {
    blob.resize(sizeof(float)+1);
    float* p = static_cast<float*>(blob.startingAddress());
    float* p1 = (float*) value;
    *p = *p1;
  }
  else if (DataTypeName == "double")
  {
    blob.resize(sizeof(double)+1);
    double* p = static_cast<double*>(blob.startingAddress());
    double* p1 = (double*) value;

    std::cout << " In the BaseSubsystemStorage " << *p1 ;

    *p = *p1;

    std::cout << " while the other is " << *p << std::endl;

  }
  else if (DataTypeName == "string")
  {
    blob.resize(sizeof(char)*value_size+1);
    char* p = static_cast<char*>(blob.startingAddress());
    char* p1 = (char*) value;
    while ((*p++=*p1++) != 0); 
  }
  else if (DataTypeName == "multitype")
  {    
    blob.resize(sizeof(char)*(value_size)+1);
    char* p = static_cast<char*>(blob.startingAddress());
    char* p1 = (char*) value;
    while ((*p++=*p1++) != 0); 
  }
  else
  {
     return -3;
  }


  schema.callProcedure( "SETATTRIBUTEVALUE", inputData );
  m_session->transaction().commit();
  return 0;
}



int BaseSubSystemStorage::SetAttributeValue_stored(void* value, int value_size, std::string& AttributeName)
{
  fixstring(AttributeName);
  fixstring(SubSystemTypeName);
  m_session->transaction().start();
  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  time_t t;
  t = time(NULL);
  struct tm *curTime;
  curTime = gmtime(&t);
  

  inputData.extend<std::string>( "Detector_prefix" );
  inputData.extend<coral::Blob>( "StoredValue" );
  inputData.extend<std::string>( "AttributeName" );
  inputData.extend<coral::TimeStamp>( "StartTime" );
  inputData.extend<coral::TimeStamp>( "FinishTime" );
  inputData.extend<std::string>( "OutResult" );
  inputData.extend<int>( "SubSystemID" );
  inputData[0].data<std::string>() = DetectorName;
  coral::Blob &blob = inputData[1].data<coral::Blob>();
  inputData[2].data<std::string>() = AttributeName;
//  inputData[3].data<coral::TimeStamp>() = coral::TimeStamp(curTime->tm_year+1900, 
//					  curTime->tm_mon+1, curTime->tm_mday, curTime->tm_hour, curTime->tm_min, curTime->tm_sec, 0);

  inputData[3].data<coral::TimeStamp>() = coral::TimeStamp(2000, 1, 1, 1, 1, 1, 1);
  inputData[4].data<coral::TimeStamp>() = coral::TimeStamp(2038, 01, 01, 00,00,00,0);

  std::string &tmpstr = inputData[5].data<std::string>();
  tmpstr.resize (200,' ');
  inputData[6].data<int>() = SubSystemID;
  std::string DataTypeName = currentCache->GetStorageType(SubSystemTypeName, AttributeName);
  fixstring(DataTypeName);
  if (DataTypeName == "")
  {
    
  }

  if (DataTypeName == "long long")
  {
    
    /*
     * 
     * test
     * 
     * */
    
    
  //  if (AttributeName == "DBID")
    {
    }
    
    
    blob.resize(sizeof(long long)+1);
    long long* p = static_cast<long long*>(blob.startingAddress());
    long long * p1 = (long long*) value;
    *p = *p1;
  }
  else if (DataTypeName == "int")
  {
    blob.resize(sizeof(int)+1);
    int* p = static_cast<int*>(blob.startingAddress());
    int* p1 = (int*) value;
    *p = *p1;
  }
  else if (DataTypeName == "float")
  {
    blob.resize(sizeof(float)+1);
    float* p = static_cast<float*>(blob.startingAddress());
    float* p1 = (float*) value;
    *p = *p1;
  }
  else if (DataTypeName == "double")
  {
    blob.resize(sizeof(double)+1);
    double* p = static_cast<double*>(blob.startingAddress());
    double* p1 = (double*) value;
    *p = *p1;
  }
  else if (DataTypeName == "string")
  {
    blob.resize(sizeof(char)*value_size+1);
    char* p = static_cast<char*>(blob.startingAddress());
    char* p1 = (char*) value;
    while ((*p++=*p1++) != 0); 
  }
  else if (DataTypeName == "multitype")
  {
    blob.resize(sizeof(char)*value_size+1);
    char* p = static_cast<char*>(blob.startingAddress());
    char* p1 = (char*) value;
    while ((*p++=*p1++) != 0); 
  }
  else
  {
     return -3;
  }

  schema.callProcedure( "SETATTRIBUTEVALUE", inputData );
  m_session->transaction().commit();
  return 0;
}







/*



int BaseSubSystemStorage::SetAttributeValue(void* value, short value_size, int AttributeID, tm* ValidityFinishtm)
{
   m_session->transaction().start(true);
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
   std::ostringstream condition;
   coral::ICursor& cursor = query->execute();
   bool exist = false;
   int count = 0;
   std::string DataTypeName = "";
 //  long long AttributeID = -1;

   


   coral::IQuery* query1 = m_session->nominalSchema().tableHandle(DetectorName+"_DataType").newQuery();
   condition.str("");
   condition << " DataTypeName='" << DataTypeName <<"'";
   query1->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor5 = query1->execute();
   std::string DataTypeStorage = "";

   if (cursor5.next())
   {
    const coral::AttributeList& alh = cursor5.currentRow();
    DataTypeStorage = alh["StorageDataType"].data<std::string>();
    fixstring(&DataTypeStorage);	

   }
   delete query1;

  time_t t;
  t = time(NULL);
  
  m_session->transaction().commit();
  

  //    std::cerr << " DataTypeStorage " <<  DataTypeStorage<< " " << DataTypeName<<  std::endl;

  
  
  if (CheckValidity(AttributeID, t))
  {

 
    m_session->transaction().start();
    struct tm *curTime;
    curTime = gmtime(&t);
    const char* timeStringFormat = "%Y-%m-%d %H:%M:%S";
    const int timeStringLength = 20;
    char timeString[timeStringLength];
    strftime(timeString, timeStringLength, timeStringFormat, curTime);
    std::string currTime (timeString) ;
    coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Values").newQuery();
    coral::AttributeList a_list;    
    std::ostringstream condition;
  
    /// !!!!!!!! Oracle Syntax !!!!!
//    condition << "(to_date(ValueValidityStart, 'YYYY-MM-DD HH24:MI:SS')  <= to_date('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND (to_date(ValueValidityFinish, 'YYYY-MM-DD HH24:MI:SS')  >= to_date('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND Attributy_ID="<<AttributeID << " AND SubSystem_ID="<<SubSystemID ;// between RelationValidityStart and RelationValidityFinish ";
    condition << "(ValueValidityStart  <= to_timestamp('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND (ValueValidityFinish  >= to_timestamp('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND AttributeID="<<AttributeID << " AND SubSystemID="<<SubSystemID ;// between RelationValidityStart and RelationValidityFinish ";

        
    /// !!!!!!!! MySQL Syntax !!!!!
    
//    condition << "(STR_TO_DATE(ValueValidityStart, '%Y-%m-%d %H:%i:%s')  <= STR_TO_DATE('" << currTime << "', '%Y-%m-%d %H:%i:%s')) AND (STR_TO_DATE(ValueValidityFinish, '%Y-%m-%d %H:%i:%s')  >= STR_TO_DATE('" << currTime << "', '%Y-%m-%d %H:%i:%s')) AND Attributy_ID="<<AttributeID << " AND SubSystem_ID="<<SubSystemID ;// between RelationValidityStart and RelationValidityFinish ";


    /// !!!!!!!! SQLite Syntax !!!!!

//    std::cerr << " BaseSubSystemStorage: step 0 "<< std::endl;
    
//    condition << "(strftime('%s', ValueValidityStart)  <= strftime('%s', '" << currTime << "')) AND (strftime('%s', ValueValidityFinish)  >= strftime('%s', '" << currTime << "')) AND Attributy_ID="<<AttributeID << " AND SubSystem_ID="<<SubSystemID ;// between RelationValidityStart and RelationValidityFinish ";

//    std::cerr << " BaseSubSystemStorage: step 1 " << condition.str() << std::endl;


    query->setCondition(condition.str(), coral::AttributeList());
    coral::ICursor& cursor = query->execute();

//    std::cerr << " BaseSubSystemStorage: step 1-1 "<< std::endl;


    while (cursor.next())
    {
	const coral::AttributeList& alh = cursor.currentRow();
	long long ValueID = (long long) alh["ValueID"].data<long long>();
 //       FinishValueValidityNow(ValueID);
    }
    delete query;    
    
    
   
    coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_Values");
    coral::AttributeList rowBuffer;
    rowBuffer.extend<long long>("ValueID");
    rowBuffer.extend<int>("SubSystemID");
    rowBuffer.extend<int>("AttributeID");
    rowBuffer.extend<coral::Blob>("ValueContent");
    rowBuffer.extend<coral::TimeStamp>("ValueValidityStart");
    rowBuffer.extend<coral::TimeStamp>("ValueValidityFinish");
 
//   rowBuffer["ID"].data<long long>() = 0;

    
   rowBuffer["ValueID"].data<long long>() = CountRowsInValuesTable();
   rowBuffer["SubSystemID"].data<int>() = SubSystemID;
    rowBuffer["AttributeID"].data<int>() = AttributeID;
    coral::Blob &blob = rowBuffer["ValueContent"].data<coral::Blob>();
    

   
    if (DataTypeStorage == "long long")
    {
      blob.resize(sizeof(long long)+1);
      long long* p = static_cast<long long*>(blob.startingAddress());
      *p = (long long) value;
    }
    else if (DataTypeStorage == "float")
    {
      blob.resize(sizeof(float)+1);
      float* p = static_cast<float*>(blob.startingAddress());
      float* p1 = (float*) value;
      *p = *p1;
    }
    
    
    
    else if (DataTypeStorage == "string")
    {
      blob.resize(sizeof(unsigned char)*value_size);
 //     std::cerr << " blob size " <<  sizeof(unsigned char)*value_size << std::endl;
      char* p = static_cast<char*>(blob.startingAddress());
      char* p1 = (char*) value;
      char* ptmp = p;
      while ((*ptmp++=*p1++) != 0);
    }
     else
    {
   	return -3;
    }
   
  
    
    rowBuffer["ValueValidityStart"].data<coral::TimeStamp>() = coral::TimeStamp(curTime->tm_year+1900, 
										curTime->tm_mon+1, curTime->tm_mday, 
										curTime->tm_hour, 
										curTime->tm_min,
										curTime->tm_sec,
										0
 									      );
    rowBuffer["ValueValidityFinish"].data<coral::TimeStamp>() = coral::TimeStamp(ValidityFinishtm->tm_year, 
										ValidityFinishtm->tm_mon+1, ValidityFinishtm->tm_mday, 
										ValidityFinishtm->tm_hour, 
										ValidityFinishtm->tm_min,
										ValidityFinishtm->tm_sec,
										0
 									      );


     

 //                 std::cerr << "step4-231" <<  std::endl;

    table.dataEditor().insertRow(rowBuffer);
    m_session->transaction().commit();

  }
  else
  {
//     std::cerr << " 2 BaseSubSystemStorage::SetAttributeValue error. Given attribute is not valid at given time. AttributeNum = " <<  AttributeNum << std::endl;
     return -3;
  }

  return 0;
}

*/

/*

int BaseSubSystemStorage::GetAttributeValue(void* value, int AttributeNum)
{
  struct tm  tm;
  time_t t = time(NULL);
  tm = *gmtime ( &t );
  return GetAttributeValue(value,  AttributeNum, &tm);
}


*/


int BaseSubSystemStorage::GetAttributeValue_cached(void* value, std::string &AttributeName)
{
  coral::TimeStamp StubTimeStamp(2000, 1, 1, 1, 1, 1, 1);
  coral::TimeStamp NowTimeStamp(2038,1,1,0,0,0,0, 1);  
  if (currentCache->GetDataFromCache(value, SubSystemTypeName, SubSystemID, AttributeName,
				 coral::TimeStamp().now(), StubTimeStamp, StubTimeStamp)) return 0;
  return -1;
}


int BaseSubSystemStorage::GetAttributeValue_cached(void* value, std::string& AttributeName, const coral::TimeStamp& atTime, coral::TimeStamp &StartTimeVal, coral::TimeStamp &FinishTimeVal)
{
  if (currentCache->GetDataFromCache(value, SubSystemTypeName, SubSystemID, AttributeName,
				 atTime, StartTimeVal, FinishTimeVal)) return 0;
  return -1;
}



/*
int BaseSubSystemStorage::GetAttributeValue(void* value, int AttributeNum, tm* atTime)
{
   m_session->transaction().start(true);
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
   std::ostringstream condition;
   coral::ICursor& cursor = query->execute();
   bool exist = false;
   int count = 0;
   std::string DataTypeName = "";
   int AttributeID = -1;

   while (cursor.next())
    {
      const coral::AttributeList& alh = cursor.currentRow();
      if (AttributeNum == count)
      {
	DataTypeName = alh["DataTypeName"].data<std::string>();
	fixstring(&DataTypeName);	
	AttributeID = alh["AttributeID"].data<int>();
	break;
      }
      count++;
    }
   delete query;
   
   if (DataTypeName == "")
   {
     
     std::cerr << "BaseSubSystemStorage::GetAttributeValue error. Given attribute is not exists in DB. AttributeNum = " <<  AttributeNum << std::endl;
     return -1;
   }
   coral::IQuery* query1 = m_session->nominalSchema().tableHandle(DetectorName+"_DataType").newQuery();
   condition.str("");
   condition << " DataTypeName='" << DataTypeName <<"'";
   query1->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor5 = query1->execute();
   std::string DataTypeStorage = "";

   if (cursor5.next())
   {
    const coral::AttributeList& alh = cursor5.currentRow();
    DataTypeStorage = alh["StorageDataType"].data<std::string>();
    fixstring(&DataTypeStorage);	
   }
   delete query1;

///////////    DataTypeStorage, AttributeID, 
  m_session->transaction().commit();

  if (CheckValidity(AttributeID, atTime))
  {
    m_session->transaction().start(true);
    const char* timeStringFormat = "%Y-%m-%d %H:%M:%S";
    const int timeStringLength = 20;
    char timeString[timeStringLength];
    strftime(timeString, timeStringLength, timeStringFormat, atTime);
    std::string currTime (timeString) ;
    coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Values").newQuery();
    coral::AttributeList a_list;
    std::ostringstream condition;

    
    /// !!!!!!!! Oracle Syntax !!!!!
//  condition << "(to_date(ValueValidityStart, 'YYYY-MM-DD HH24:MI:SS')  <= to_date('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND (to_date(ValueValidityFinish, 'YYYY-MM-DD HH24:MI:SS')  >= to_date('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND Attributy_ID="<<AttributeID << " AND SubSystem_ID="<<SubSystemID ;// between RelationValidityStart and RelationValidityFinish ";
    
    condition << "(ValueValidityStart  <= to_timestamp('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND (ValueValidityFinish  >= to_timestamp('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND AttributeID="<<AttributeID << " AND SubSystemID="<<SubSystemID ;// between RelationValidityStart and RelationValidityFinish ";
    
    
//  MySQL syntax  
//    condition << "(STR_TO_DATE(ValueValidityStart, '%Y-%m-%d %H:%i:%s')  <= STR_TO_DATE('" << currTime << "', '%Y-%m-%d %H:%i:%s')) AND (STR_TO_DATE(ValueValidityFinish, '%Y-%m-%d %H:%i:%s')  >= STR_TO_DATE('" << currTime << "', '%Y-%m-%d %H:%i:%s')) AND Attributy_ID="<<AttributeID << " AND SubSystem_ID="<<SubSystemID ;// between RelationValidityStart and RelationValidityFinish ";

//   std::cerr << " BaseSubSystemStorage: step 2 "<< std::endl;

//    SQLite syntax  
//    condition << "(strftime('%s', ValueValidityStart)  <= strftime('%s', '" << currTime << "')) AND (strftime('%s', ValueValidityFinish)  >= strftime('%s','" << currTime << "')) AND Attributy_ID="<<AttributeID << " AND SubSystem_ID="<<SubSystemID ;// between RelationValidityStart and RelationValidityFinish ";

//   std::cerr << " BaseSubSystemStorage: step 3 "<< std::endl;


    query->setCondition(condition.str(), coral::AttributeList());
    coral::ICursor& cursor = query->execute();
    if (cursor.next())
    {
     if (DataTypeStorage == "long long")
     {
	const coral::AttributeList& alh = cursor.currentRow();
	coral::Blob blob = alh["ValueContent"].data<coral::Blob>();
        long long *p = (long long *)value;
	long long *p1 = static_cast<long long*>(blob.startingAddress());
	*p = *p1;
     }
     else if (DataTypeStorage == "float")
     {
	const coral::AttributeList& alh = cursor.currentRow();
	coral::Blob blob = alh["ValueContent"].data<coral::Blob>();
        float *p = (float *)value;
	float *p1 = static_cast<float*>(blob.startingAddress());
	*p = *p1;
     }
     else if (DataTypeStorage == "string")
     {
        const coral::AttributeList& alh = cursor.currentRow();
	
	coral::Blob blob = alh["ValueContent"].data<coral::Blob>();
        char* p1 = static_cast<char*>(blob.startingAddress());;
        char* p = (char *)value;
 	for (int j = 0; j < blob.size(); j++, ++p1, ++p) 
	{
	  *p=*p1;
	}
	*p='\0';
     }
     else
     {
   	return -3;
     }
     delete query;
     m_session->transaction().commit();
     return 0;
    }
  }
  else
  {
     std::cerr << "BaseSubSystemStorage::GetAttributeValue error. Given attribute is not valid at given time. AttributeNum = " <<  AttributeNum << std::endl;
     return -3;
  }
 
  
  return 0;
}

*/


int BaseSubSystemStorage::CheckValidity_stored(int AttributeID, tm* atTime)
{
  m_session->transaction().start();
  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  inputData.extend<std::string>( "Detector_prefix" );
  inputData.extend<std::string>( "SubSystemTypeName" );
  inputData.extend<int>( "AttributeID" );
  inputData.extend<bool>( "IsValid" );
  inputData[0].data<std::string>() = DetectorName;
  inputData[1].data<std::string>() = SubSystemTypeName;
  inputData[2].data<int>() = AttributeID;
  inputData[3].data<bool>() = false;
  schema.callProcedure( "CHECKATTRVALIDITY", inputData );
  m_session->transaction().commit();
  bool res = inputData[3].data<bool>();
  if (res == true)
    return 1;
  else
    return 0;
}



/*

int BaseSubSystemStorage::CheckValidity(int AttributeID, tm* atTime)
{
   m_session->transaction().start(true);
   const char* timeStringFormat = "%Y-%m-%d %H:%M:%S";
   const int timeStringLength = 20;
   char timeString[timeStringLength];
   strftime(timeString, timeStringLength, timeStringFormat, atTime);
   std::string currTime (timeString) ;
   int AttributiesCount = 0;
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_RelationshipValidity").newQuery();
   coral::AttributeList a_list;
   std::ostringstream condition;
   fixstring(&SubSystemTypeName);	

   
    /// !!!!!!!! Oracle Syntax !!!!!
 //   condition << "(to_date(RelationValidityStart, 'YYYY-MM-DD HH24:MI:SS')  <= to_date('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND (to_date(RelationValidityFinish, 'YYYY-MM-DD HH24:MI:SS')  >= to_date('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND Attributy_ID="<<AttributeID << " AND SubSystemTypeName='"<<SubSystemTypeName<<"'" ;// between RelationValidityStart and RelationValidityFinish ";

    condition << "(RelationshipValidityStart  <= to_timestamp('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND (RelationshipValidityFinish  >= to_timestamp('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND AttributeID="<<AttributeID << " AND SubSystemTypeName='"<<SubSystemTypeName<<"'"  ;// between RelationValidityStart and RelationValidityFinish ";
    
    
    
    
    //  std::cerr << "condition  = " << condition.str()  << std::endl;
   
///////// MySQL Syntax!!!!!!!!!!!!!!!!!!

//   condition << "(STR_TO_DATE(RelationValidityStart, '%Y-%m-%d %H:%i:%s')  <= STR_TO_DATE('" << currTime << "', '%Y-%m-%d %H:%i:%s')) AND (STR_TO_DATE(RelationValidityFinish, '%Y-%m-%d %H:%i:%s')  >= STR_TO_DATE('" << currTime << "', '%Y-%m-%d %H:%i:%s')) AND Attributy_ID="<<AttributeID<< " AND SubSystemTypeName='"<<SubSystemTypeName<<"'";// between RelationValidityStart and RelationValidityFinish ";
 //  std::cerr << "condition  = " << condition.str()  << std::endl;


///////// SQLite Syntax!!!!!!!!!!!!!!!!!!

//   std::cerr << " BaseSubSystemTypeStorage: step 4 "<< std::endl;

//  condition << "(strftime('%s',RelationValidityStart)  <= strftime('%s', '" << currTime << "')) AND (strftime('%s', RelationValidityFinish)  >= strftime('%s', '" << currTime << "')) AND Attributy_ID="<<AttributeID<< " AND SubSystemTypeName='"<<SubSystemTypeName<<"'";// between RelationValidityStart and RelationValidityFinish ";

//   std::cerr << " BaseSubSystemTypeStorage: step 5 "<< std::endl;

   query->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor = query->execute();
   if (cursor.next())
   {
    delete query;
    m_session->transaction().commit();
    return 1;
   }
   delete query;
   m_session->transaction().commit();
   return 0;
  
}




int BaseSubSystemStorage::CheckValidity(int AttributeID, time_t time)
{
   struct tm *atTime;
   atTime = gmtime(&time);
   return CheckValidity_stored(AttributeID, atTime);
}

*/


int BaseSubSystemStorage::CheckValidity_stored(int AttributeID, time_t time)
{
   struct tm *atTime;
   atTime = gmtime(&time);
   return CheckValidity_stored(AttributeID, atTime);
}



int BaseSubSystemStorage::FinishValueValidityNow(int ValueID)
{
   coral::ITableDataEditor& editor2 = m_session->nominalSchema().tableHandle(DetectorName+"_Values").dataEditor();
   std::string updateAction = "ValueValidityFinish = :ValidityFinish";
   std::ostringstream condition;
   condition << " ValueID=" << ValueID ;
   coral::AttributeList updateData;
   updateData.extend<coral::TimeStamp>( "ValueValidityFinish" );
   coral::TimeStamp& ValueValidityFinish = updateData["ValueValidityFinish"].data<coral::TimeStamp>();
   coral::IBulkOperation* bulkUpdater = editor2.bulkUpdateRows( updateAction,
                                                               condition.str(),
                                                               updateData,
                                                               3 );
   time_t t;
   t = time(NULL);
   struct tm *curTime;
   curTime = gmtime(&t);
   
   ValueValidityFinish = coral::TimeStamp(curTime->tm_year+1900, 
					curTime->tm_mon+1, curTime->tm_mday, 
					curTime->tm_hour, 
					curTime->tm_min,
					curTime->tm_sec,
					0);

   
   bulkUpdater->processNextIteration();
   bulkUpdater->flush();
   delete bulkUpdater;
   return 0;
}



long long BaseSubSystemStorage::CountRowsInValuesTable()
{
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Values").newQuery();
   std::ostringstream condition;
   coral::ICursor& cursor = query->execute();
   long long count = 0;
   std::string DataTypeName = "";
   while (cursor.next())
    {
      const coral::AttributeList& alh = cursor.currentRow();
      count++;
    }
   delete query;
  return count;
}
