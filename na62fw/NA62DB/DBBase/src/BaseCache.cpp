// --------------------------------------------------------------
// History:
//           
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) 2013-12-06
// --------------------------------------------------------------

#include "BaseCache.h"

#include <string>
#include <map>
#include <utility>
#include "RelationalAccess/ISessionProxy.h"
#include "RelationalAccess/ISchema.h"
#include "RelationalAccess/IConnectionService.h"

#include "RelationalAccess/AccessMode.h"
#include "CoralCommon/Utilities.h"

#include "BaseSubSystemTypeManager.h"
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

#include "CoralBase/Blob.h"
#include "CoralBase/TimeStamp.h"
#include "CoralBase/Date.h"

#include "CoralCommon/Utilities.h"

#include <climits>

#include "CoralBase/MessageStream.h"

#ifdef CORAL_SEALED

#include "SealKernel/Context.h"
#include "SealKernel/ComponentLoader.h"
#include "SealKernel/IMessageService.h"
#include "PluginManager/PluginManager.h"

#else

#include "CoralKernel/Context.h"

#endif

#include "RelationalAccess/AccessMode.h"

#include "boost/date_time/posix_time/posix_time_types.hpp"
#include "boost/smart_ptr/scoped_ptr.hpp"
#include "boost/variant.hpp"
#include "boost/icl/interval_map.hpp"



using namespace std;
using namespace boost::icl;


using namespace boost::posix_time;

   struct uniqueCombinat
    {
        std::string SUBSYSTEMTYPENAME;
        int SUBSYSTEMID;
	std::string ATTRIBUTENAME;
    };


const ptime StringToPtime(const string &zeitstempel, const string &formatstring)
{
    stringstream ss;
    time_input_facet* input_facet = new time_input_facet();
    ss.imbue(locale(ss.getloc(), input_facet));
    input_facet->format(formatstring.c_str());

    ss.str(zeitstempel);
    ptime timestamp;

    ss >> timestamp;
    return timestamp;
}


void fixstring1(std::string &str)
{
  
  int lastspaceposition = str.size();
  for (int i = str.size()-1; i >=0; i--)
  {
    if (((int)(str.at(i))) !=32)
      break;
    lastspaceposition = i;
  }
  for (int i = lastspaceposition; i < (int)str.size(); i++)
    {
      str.erase(i);
    }
}

typedef interval_map<long long, long long> IntervalsMap;
using namespace boost::gregorian;


bool BaseCache::ProcessTimeIntervalsAtCache()
{
    
  cache_session->transaction().start();
  boost::scoped_ptr<coral::IQuery> query(cache_session->nominalSchema().newQuery());
  query->addToTableList(DetectorName+"_ValuesCacheRaw");
  query->addToOutputList("ATTRIBUTENAME");
  query->addToOutputList("SUBSYSTEMID");
  query->addToOutputList("SUBSYSTEMTYPENAME");
  query->groupBy("ATTRIBUTENAME, SUBSYSTEMID, SUBSYSTEMTYPENAME");
  std::ostringstream condition;
  coral::ICursor& cursor = query->execute();
//  int SSID = -1;
    
  std::list<uniqueCombinat> uniqueCombinatVec;

  while (cursor.next())
  {
    const coral::AttributeList& alh = cursor.currentRow();
    uniqueCombinat t;
    t.SUBSYSTEMTYPENAME = alh["SUBSYSTEMTYPENAME"].data<std::string>();
    t.SUBSYSTEMID = alh["SUBSYSTEMID"].data<int>();
    t.ATTRIBUTENAME = alh["ATTRIBUTENAME"].data<std::string>();
    uniqueCombinatVec.push_back(t);    
  }
  cursor.close();
  
  coral::ITable& CacheTablePreProc = cache_session->nominalSchema().tableHandle(DetectorName+"_ValuesCache");
  coral::AttributeList rowBuffer;
  rowBuffer.extend<long long>("ID");
  rowBuffer.extend<std::string>("SUBSYSTEMTYPENAME");
  rowBuffer.extend<int>("SUBSYSTEMID");
  rowBuffer.extend<std::string>("ATTRIBUTENAME");
  rowBuffer.extend<coral::Blob>("VALUE");
  rowBuffer.extend<long long>("TIMEOFVALADDING");
  rowBuffer.extend<long long>("VALUEVALIDITYSTART");
  rowBuffer.extend<long long>("VALUEVALIDITYFINISH");
  rowBuffer.extend<std::string>("STORAGEDATATYPE");
  
  
  long long rowid = 0;
  for (std::list<uniqueCombinat>::iterator it =  uniqueCombinatVec.begin(); it != uniqueCombinatVec.end(); ++it)
  {
    uniqueCombinat uC = *it;
 //   cache_session->transaction().start();
    boost::scoped_ptr<coral::IQuery> queryIDFetch(cache_session->nominalSchema().newQuery());
    queryIDFetch->addToTableList(DetectorName+"_ValuesCacheRaw");
    queryIDFetch->addToOutputList("ID");
    queryIDFetch->addToOutputList("VALUEVALIDITYSTART");
    queryIDFetch->addToOutputList("VALUEVALIDITYFINISH");
    queryIDFetch->addToOrderList("TIMEOFVALADDING");
    std::ostringstream condition;
    condition.str("");
    condition << " SUBSYSTEMTYPENAME='" << (uC).SUBSYSTEMTYPENAME << "' AND SUBSYSTEMID=" << (uC).SUBSYSTEMID<< " AND ATTRIBUTENAME='" << (uC).ATTRIBUTENAME  << "'"; ;
    queryIDFetch->setCondition(condition.str(), coral::AttributeList());
    
    IntervalsMap intervalsMap;
    coral::ICursor& cursorIDFetch = queryIDFetch->execute();
    while (cursorIDFetch.next())
    {
      const coral::AttributeList& alh1 = cursorIDFetch.currentRow();
      intervalsMap.set(make_pair(interval<long long>::right_open( alh1["VALUEVALIDITYSTART"].data<long long>() , alh1["VALUEVALIDITYFINISH"].data<long long>()) , alh1["ID"].data<long long>() ));
    }

    cursorIDFetch.close();
    IntervalsMap::iterator itm = intervalsMap.begin();
    while(itm != intervalsMap.end())
    {
        interval<long long>::type interval = itm->first;
        long long targetID = (*itm++).second;
	boost::scoped_ptr<coral::IQuery> queryIDFetchRaw(cache_session->nominalSchema().tableHandle(DetectorName+"_ValuesCacheRaw").newQuery());
	condition.str("");
        condition << "ID="<<targetID; 
        queryIDFetchRaw->setCondition(condition.str(), coral::AttributeList());
	coral::ICursor& cursorIDFetch = queryIDFetchRaw->execute();
	cursorIDFetch.next();
	if (cursorIDFetch.currentRow()["ID"].data<long long>() == targetID)
	{
	    rowBuffer["ID"].data<long long>() = rowid;
	    rowBuffer["SUBSYSTEMTYPENAME"].data<std::string>() = cursorIDFetch.currentRow()["SUBSYSTEMTYPENAME"].data<std::string>();
	    rowBuffer["SUBSYSTEMID"].data<int>() = cursorIDFetch.currentRow()["SUBSYSTEMID"].data<int>();
	    rowBuffer["ATTRIBUTENAME"].data<std::string>() = cursorIDFetch.currentRow()["ATTRIBUTENAME"].data<std::string>();
	    rowBuffer["TIMEOFVALADDING"].data<long long>() = cursorIDFetch.currentRow()["TIMEOFVALADDING"].data<long long>();
	    rowBuffer["VALUEVALIDITYSTART"].data<long long>() = interval.lower();
	    rowBuffer["VALUEVALIDITYFINISH"].data<long long>() = interval.upper();
	    rowBuffer["STORAGEDATATYPE"].data<std::string>() = cursorIDFetch.currentRow()["STORAGEDATATYPE"].data<std::string>();
	    rowBuffer["VALUE"].data<coral::Blob>() = cursorIDFetch.currentRow()["VALUE"].data<coral::Blob>();
	    CacheTablePreProc.dataEditor().insertRow(rowBuffer);
	    rowid++;
	   
	}
	cursorIDFetch.close();
    }
  }
  cache_session->transaction().commit();
  return true;
}

bool BaseCache::CreateValCache(const coral::TimeStamp &startTime, const coral::TimeStamp &finishTime, const std::vector<std::string> &tagged/*, const std::vector &tags = NULL**/)
{
  cache_session->transaction().start();
  coral::TableDescription descrValCache;
  descrValCache.setName(DetectorName+"_ValuesCacheRaw");
  descrValCache.insertColumn("ID", coral::AttributeSpecification::typeNameForId(typeid(long long)));
  descrValCache.insertColumn("SUBSYSTEMTYPENAME", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
  descrValCache.insertColumn("SUBSYSTEMID", coral::AttributeSpecification::typeNameForId(typeid(int)));
  descrValCache.insertColumn("ATTRIBUTENAME", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
  descrValCache.insertColumn("VALUE", coral::AttributeSpecification::typeNameForId( typeid(coral::Blob)));
  descrValCache.insertColumn("TIMEOFVALADDING", coral::AttributeSpecification::typeNameForId(typeid( long long )));
  descrValCache.insertColumn("VALUEVALIDITYSTART", coral::AttributeSpecification::typeNameForId(typeid( long long )));
  descrValCache.insertColumn("VALUEVALIDITYFINISH", coral::AttributeSpecification::typeNameForId(typeid( long long )));
  descrValCache.insertColumn("STORAGEDATATYPE", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
  cache_session->nominalSchema().dropIfExistsTable(descrValCache.name());
  coral::ITable& tablerow = cache_session->nominalSchema().createTable( descrValCache );
  tablerow.schemaEditor().createIndex(DetectorName+"_ValuesCacheRaw"+"_IDX_IDRaw", "ID", true);
  tablerow.schemaEditor().createIndex(DetectorName+"_ValuesCacheRaw"+"_IDX_IDSUBSYSTEMTYPENAME", "SUBSYSTEMTYPENAME", false);
  tablerow.schemaEditor().createIndex(DetectorName+"_ValuesCacheRaw"+"_IDX_IDSUBSYSTEMID", "SUBSYSTEMID", false);
  tablerow.schemaEditor().createIndex(DetectorName+"_ValuesCacheRaw"+"_IDX_IDATTRIBUTENAME", "ATTRIBUTENAME", false);
  
  coral::TableDescription descrValCachePreProc;
  descrValCachePreProc.setName(DetectorName+"_ValuesCache");
  descrValCachePreProc.insertColumn("ID", coral::AttributeSpecification::typeNameForId(typeid(long long)));
  descrValCachePreProc.insertColumn("SUBSYSTEMTYPENAME", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
  descrValCachePreProc.insertColumn("SUBSYSTEMID", coral::AttributeSpecification::typeNameForId(typeid(int)));
  descrValCachePreProc.insertColumn("ATTRIBUTENAME", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
  descrValCachePreProc.insertColumn("VALUE", coral::AttributeSpecification::typeNameForId( typeid(coral::Blob)));
  descrValCachePreProc.insertColumn("TIMEOFVALADDING", coral::AttributeSpecification::typeNameForId(typeid( long long )));
  descrValCachePreProc.insertColumn("VALUEVALIDITYSTART", coral::AttributeSpecification::typeNameForId(typeid( long long )));
  descrValCachePreProc.insertColumn("VALUEVALIDITYFINISH", coral::AttributeSpecification::typeNameForId(typeid( long long )));
  descrValCachePreProc.insertColumn("STORAGEDATATYPE", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
  cache_session->nominalSchema().dropIfExistsTable(descrValCachePreProc.name());
  coral::ITable& table = cache_session->nominalSchema().createTable( descrValCachePreProc );
  table.schemaEditor().createIndex(DetectorName+"_ValuesCache"+"_IDX_ID", "ID", true);
  table.schemaEditor().createIndex(DetectorName+"_ValuesCache"+"_IDX_IDSUBSYSTEMTYPENAME", "SUBSYSTEMTYPENAME", false);
  table.schemaEditor().createIndex(DetectorName+"_ValuesCache"+"_IDX_IDSUBSYSTEMID", "SUBSYSTEMID", false);
  table.schemaEditor().createIndex(DetectorName+"_ValuesCache"+"_IDX_IDATTRIBUTENAME", "ATTRIBUTENAME", false);

  
  cache_session->transaction().commit();
  cache_session->transaction().start();
  coral::ITable& CacheTable = cache_session->nominalSchema().tableHandle(DetectorName+"_ValuesCacheRaw");
  coral::AttributeList rowBuffer;
  rowBuffer.extend<long long>("ID");
  rowBuffer.extend<std::string>("SUBSYSTEMTYPENAME");
  rowBuffer.extend<int>("SUBSYSTEMID");
  rowBuffer.extend<std::string>("ATTRIBUTENAME");
  rowBuffer.extend<coral::Blob>("VALUE");
  rowBuffer.extend<long long>("TIMEOFVALADDING");
  rowBuffer.extend<long long>("VALUEVALIDITYSTART");
  rowBuffer.extend<long long>("VALUEVALIDITYFINISH");
  rowBuffer.extend<std::string>("STORAGEDATATYPE");
  long long id = 0;


  
  /*
  coral::AttributeList inputData;
  inputData.extend<std::string>( "Detector_prefix" );
  inputData.extend<coral::TimeStamp>( "StartTime" );
  inputData.extend<coral::TimeStamp>( "FinishTime" );
  inputData.extend<int>( "FetchedCount" );
  int count = 0;
  
  inputData[0].data<std::string>() = DetectorName;
  inputData[1].data<coral::TimeStamp>() = startTime;
  inputData[2].data<coral::TimeStamp>() = finishTime;
  inputData[3].data<int>() = 0;

  
  schema.callProcedure( "FILL_TMPDATAVAL", inputData );
  coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_TMPDATAVAL");
  boost::scoped_ptr<coral::IQuery> DataQuery(table.newQuery());
  std::ostringstream condition;
  condition.str("");
  DataQuery->setCondition(condition.str(), coral::AttributeList());
  coral::ICursor& DataCursor = DataQuery->execute();
*/
  
  m_session->transaction().start(true);

  std::auto_ptr<coral::IQuery> DataQuery(m_session->nominalSchema().newQuery());
  DataQuery->addToTableList(DetectorName+"_CONSOLIDATEVIEW", "CONSOLIDATEVIEW");
  
  DataQuery->addToOutputList("SUBSYSTEMTYPENAME");
  DataQuery->addToOutputList("SUBSYSTEMID");
  DataQuery->addToOutputList("TIMEOFVALADDING");
  DataQuery->addToOutputList("VALUEVALIDITYSTART");
  DataQuery->addToOutputList("VALUEVALIDITYFINISH");
  DataQuery->addToOutputList("VALUE");
  DataQuery->addToOutputList("ATTRIBUTENAME");
  DataQuery->addToOutputList("STORAGEDATATYPE");
  
  coral::AttributeList condition;
  condition.extend<std::string>("VALUEVALIDITYSTARTc");
  condition.extend<std::string>("VALUEVALIDITYFINISHc");
  condition[0].data<std::string>() = startTime.toString();
  condition[1].data<std::string>() = finishTime.toString();
    
  std::ostringstream conditionstr;
  conditionstr.str("");
  conditionstr << "TO_TIMESTAMP(VALUEVALIDITYSTART, 'YYYYMMDDHH24MISSFF')<=TO_TIMESTAMP(:VALUEVALIDITYFINISHc, 'DD/MM/YY HH24:MI:SS.FF')  AND TO_TIMESTAMP(VALUEVALIDITYFINISH, 'YYYYMMDDHH24MISSFF')>=TO_TIMESTAMP(:VALUEVALIDITYSTARTc, 'DD/MM/YY HH24:MI:SS.FF')";
  conditionstr << " AND (ATTRIBUTEID IN (SELECT \"AttributeID\" FROM \""<< DetectorName+"_AttributesTags"<< "\" WHERE \"TagName\" IN (";
  
  std::vector<std::string>::const_iterator the_iterator;
  the_iterator= tagged.begin();
  
  while (the_iterator != tagged.end()) {
     conditionstr << "'"<< *the_iterator << "'" ; 
     the_iterator++;
     if (the_iterator != tagged.end())
     conditionstr << ", "; 
  }
  conditionstr <<")) OR trim(ATTRIBUTENAME)='DBID')";
  DataQuery->setCondition( conditionstr.str(), condition);
  coral::ICursor& DataCursor = DataQuery->execute();
  std::cout << std::endl;
  std::cout << "Loading data from the Offline DataBase for detector:" << DetectorName << std::endl; ;  
  while (DataCursor.next())
  {
    std::cout << "Values fetched: " << ++id << " \r";
    std::cout.flush();
    rowBuffer["ID"].data<long long>() = id;
    const coral::AttributeList& alh = DataCursor.currentRow();
    std::string tmpstr = alh["SUBSYSTEMTYPENAME"].data<std::string>();
    fixstring1(tmpstr);
    rowBuffer["SUBSYSTEMTYPENAME"].data<std::string>() = tmpstr;
    rowBuffer["SUBSYSTEMID"].data<int>() = alh["SUBSYSTEMID"].data<int>();	
    tmpstr = alh["ATTRIBUTENAME"].data<std::string>();
    fixstring1(tmpstr);
    rowBuffer["ATTRIBUTENAME"].data<std::string>() = tmpstr;
    ptime timeofvaladding = StringToPtime(alh["TIMEOFVALADDING"].data<std::string>(), "YYYYMMDDHH24MISSFF");
    ptime timeofvalstart = StringToPtime(alh["VALUEVALIDITYSTART"].data<std::string>(), "YYYYMMDDHH24MISSFF");
    ptime timeofvalfinish = StringToPtime(alh["VALUEVALIDITYFINISH"].data<std::string>(), "YYYYMMDDHH24MISSFF");
    
    /// This timestamp->string stuff is to avoid frontieraccess bug when it does not pass timestamp.
    coral::TimeStamp timeofvaladdingnanosec(StringToPtime(alh["TIMEOFVALADDING"].data<std::string>(), "%Y%m%d%H%M%S%F")); 
    coral::TimeStamp timeofvalstartnanosec(StringToPtime(alh["VALUEVALIDITYSTART"].data<std::string>(), "%Y%m%d%H%M%S%F"));
    coral::TimeStamp timeofvalfinishnanosec(StringToPtime(alh["VALUEVALIDITYFINISH"].data<std::string>(), "%Y%m%d%H%M%S%F"));
    
    rowBuffer["TIMEOFVALADDING"].data<long long>() = timeofvaladdingnanosec.total_nanoseconds();
    rowBuffer["VALUEVALIDITYSTART"].data<long long>() = timeofvalstartnanosec.total_nanoseconds();
    rowBuffer["VALUEVALIDITYFINISH"].data<long long>() = timeofvalfinishnanosec.total_nanoseconds();
    tmpstr = alh["STORAGEDATATYPE"].data<std::string>();
    fixstring1(tmpstr);
    rowBuffer["STORAGEDATATYPE"].data<std::string>() = tmpstr;
    rowBuffer["VALUE"].data<coral::Blob>() = alh["VALUE"].data<coral::Blob>();
    CacheTable.dataEditor().insertRow(rowBuffer);
  }
  
  std::cout << std::endl;
  DataCursor.close();
  coral::TableDescription descrSSIDCACHE;
  descrSSIDCACHE.setName(DetectorName+"_SSIDCACHE");
  descrSSIDCACHE.insertColumn("SubSystemTypeName", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
  descrSSIDCACHE.insertColumn("SubSystemID", coral::AttributeSpecification::typeNameForId(typeid(int)));
  cache_session->nominalSchema().dropIfExistsTable(descrSSIDCACHE.name());
  cache_session->nominalSchema().createTable( descrSSIDCACHE );
  cache_session->transaction().commit();
  m_session->transaction().commit();
  RefreshSubSystemsCache();
  ProcessTimeIntervalsAtCache();
  return true;
}



bool BaseCache::GetDataFromCache(void* value, std::string &SubSystemTypeName, int SubSystemID, std::string &AttributeName, const coral::TimeStamp &atTime)
{
  coral::TimeStamp StubTimeStamp(1900, 0, 0, 0, 0, 0, 0);
  GetDataFromCache(value, SubSystemTypeName, SubSystemID, AttributeName, atTime, StubTimeStamp, StubTimeStamp);
  return true;
}


bool BaseCache::RefreshSubSystemsCache()
{
  m_session->transaction().start(true);
  coral::ISchema& m_schema = m_session->nominalSchema();
  cache_session->transaction().start();
  boost::scoped_ptr<coral::IQuery> DataQuerySSID(m_schema.newQuery());
  DataQuerySSID->addToTableList(DetectorName+"_DetectorSubSystem", "DetectorSubSystem");
  DataQuerySSID->addToOutputList("SubSystemTypeName");
  DataQuerySSID->addToOutputList("SubSystemID");
  
  
  coral::ICursor& DataCursorSSID = DataQuerySSID->execute();
  coral::ITable& CacheTableSSID = cache_session->nominalSchema().tableHandle(DetectorName+"_SSIDCACHE");
  coral::AttributeList rowBufferSSID;
  rowBufferSSID.extend<std::string>("SubSystemTypeName");
  rowBufferSSID.extend<int>("SubSystemID");
  while (DataCursorSSID.next())
  {
    const coral::AttributeList& alh = DataCursorSSID.currentRow();
    std::string tmpstr = alh["SubSystemTypeName"].data<std::string>();
    fixstring1(tmpstr);
    rowBufferSSID["SubSystemTypeName"].data<std::string>() = tmpstr;
    rowBufferSSID["SubSystemID"].data<int>() = alh["SubSystemID"].data<int>();
    CacheTableSSID.dataEditor().insertRow(rowBufferSSID);
  }
  DataCursorSSID.close();
  cache_session->transaction().commit();
  m_session->transaction().commit();
  return true;
}


std::string BaseCache::GetSubSystemTypeName(int SubSystem_ID)
{
  cache_session->transaction().start(true);
  coral::ITable& table = cache_session->nominalSchema().tableHandle(DetectorName+"_SSIDCACHE");
  std::auto_ptr<coral::IQuery> query(table.newQuery());
  std::ostringstream condition;
  condition.str("");
  condition << " SubSystemID=" << SubSystem_ID;
  query->setCondition(condition.str(), coral::AttributeList());
  coral::ICursor& cursor = query->execute();
  std::string SubSystemTypeName= "";
  

  if (cursor.next())
  {
    const coral::AttributeList& alh = cursor.currentRow();
    SubSystemTypeName = alh["SubSystemTypeName"].data<std::string>();
  }
  cursor.close();
  cache_session->transaction().commit();
  return SubSystemTypeName;

}


int BaseCache::GetDataSizeFromCache(std::string &SubSystemTypeName, int SubSystemID, std::string &AttributeName, const coral::TimeStamp &atTime, coral::TimeStamp &StartTimeVal, coral::TimeStamp &FinishTimeVal)
{
  int size = -1;
  fixstring1(AttributeName);
  fixstring1(SubSystemTypeName);
  cache_session->transaction().start(true);
  coral::ITable& table = cache_session->nominalSchema().tableHandle(DetectorName+"_ValuesCache");
  std::auto_ptr<coral::IQuery> query (table.newQuery());

  std::ostringstream condition;
  condition.str("");
  condition << " VALUEVALIDITYSTART < " << atTime.total_nanoseconds() <<" AND VALUEVALIDITYFINISH > " << atTime.total_nanoseconds() << " AND ATTRIBUTENAME='" << AttributeName <<
	"' AND SUBSYSTEMID=" << SubSystemID << " AND SUBSYSTEMTYPENAME='" << SubSystemTypeName << "'";
  
  query->setCondition(condition.str(), coral::AttributeList());
 
  coral::ICursor& cursor = query->execute();
  if (cursor.next())
  {
    const coral::AttributeList& alh = cursor.currentRow();
    std::string StorageDataType = alh["STORAGEDATATYPE"].data<std::string>();
    fixstring1(StorageDataType);	
    
    long long nanosectime = alh["VALUEVALIDITYSTART"].data<long long>();
    coral::TimeStamp StartTimeValData(nanosectime);
    
    nanosectime = alh["VALUEVALIDITYFINISH"].data<long long>();
    coral::TimeStamp FinishTimeValData(nanosectime);
    
    StartTimeVal = StartTimeValData;
    FinishTimeVal = FinishTimeValData;
    coral::Blob blob = alh["VALUE"].data<coral::Blob>();
    fixstring1(StorageDataType);
    
    if (StorageDataType == "multitype")
	{
	  size=blob.size();
	}
  }
  
  cursor.close();
  cache_session->transaction().commit();
  return size;
  
}

bool BaseCache::GetDataFromCache(void* value, std::string &SubSystemTypeName, int SubSystemID, std::string &AttributeName, const coral::TimeStamp &atTime, coral::TimeStamp& StartTimeVal, coral::TimeStamp& FinishTimeVal)
{
  
  fixstring1(AttributeName);
  fixstring1(SubSystemTypeName);
  cache_session->transaction().start(true);
  coral::ITable& table = cache_session->nominalSchema().tableHandle(DetectorName+"_ValuesCache");
  std::auto_ptr<coral::IQuery> query(table.newQuery());

  std::ostringstream condition;
  condition.str("");
  condition << " VALUEVALIDITYSTART < " << atTime.total_nanoseconds() <<" AND VALUEVALIDITYFINISH > " << atTime.total_nanoseconds() << " AND ATTRIBUTENAME='" << AttributeName <<
	"' AND SUBSYSTEMID=" << SubSystemID << " AND SUBSYSTEMTYPENAME='" << SubSystemTypeName << "'";
  query->setCondition(condition.str(), coral::AttributeList());
  coral::ICursor& cursor = query->execute();
  if (cursor.next())
  {
    const coral::AttributeList& alh = cursor.currentRow();
    std::string StorageDataType = alh["STORAGEDATATYPE"].data<std::string>();
    fixstring1(StorageDataType);	
    
    long long nanosectime = alh["VALUEVALIDITYSTART"].data<long long>();
    coral::TimeStamp StartTimeValData(nanosectime);
    
    nanosectime = alh["VALUEVALIDITYFINISH"].data<long long>();
    coral::TimeStamp FinishTimeValData(nanosectime);
    
    StartTimeVal = StartTimeValData;
    FinishTimeVal = FinishTimeValData;
    coral::Blob blob = alh["VALUE"].data<coral::Blob>();
    fixstring1(StorageDataType);
    
	if (StorageDataType == "long long")
	{
	  long long *p = (long long *)value;
	  long long *p1 = static_cast<long long*>(blob.startingAddress());
	  *p = *p1;
	}
	else if (StorageDataType == "int")
	{
	  int *p = (int *)value;
	  int *p1 = static_cast<int*>(blob.startingAddress());
	  *p = *p1;
	}
	else if (StorageDataType == "float")
	{
	  float *p = (float *)value;
	  float *p1 = static_cast<float*>(blob.startingAddress());
	  *p = *p1;
	}
	else if (StorageDataType == "double")
	{
	  double *p = (double *)value;
	  double *p1 = static_cast<double*>(blob.startingAddress());
	  *p = *p1;
	}
	else if (StorageDataType == "string")
	{
	  char* p1 = static_cast<char*>(blob.startingAddress());;
	  char* p = (char *)value;
	  for (int j = 0; j < blob.size(); j++, ++p1, ++p) 
	  {
	    *p=*p1;
	  }
	  *p='\0';
	}
	else if (StorageDataType == "multitype")
	{
	  char* p1 = static_cast<char*>(blob.startingAddress());;
	  char* p = (char *)value;
	  for (int j = 0; j < blob.size(); j++, ++p1, ++p) 
	  {
	    *p=*p1;
	  }
//  	  *p++='\0';

	}
	else
	{
	  return false;
	}
  }
  
  else
  {
    cursor.close();
    cache_session->transaction().commit();
    return false;
  }

  cursor.close();
  cache_session->transaction().commit();
  return true;
  
  
/*  
  CREATE TABLE '||TableName||'  
(    
ID NUMBER(32),       
SUBSYSTEMTYPENAME VARCHAR2(255),
SUBSYSTEMID NUMBER(20,0),
ATTRIBUTENAME VARCHAR2(255),
VALUE BLOB,
ValueValidityStart TIMESTAMP (0),
ValueValidityFinish TIMESTAMP (0),
STORAGEDATATYPE VARCHAR2(255)
)';
*/  
}




int BaseCache::GetSSID_cached(std::string SubSystemName, int DBID)
{
  
  fixstring1(SubSystemName);
  cache_session->transaction().start(true);
  coral::ITable& table = cache_session->nominalSchema().tableHandle(DetectorName+"_ValuesCacheRaw");
  std::auto_ptr<coral::IQuery> query(table.newQuery());
  std::ostringstream condition;
  condition.str("");
  condition << "SUBSYSTEMTYPENAME = '" << SubSystemName <<"' AND ATTRIBUTENAME = 'DBID'";
  query->setCondition(condition.str(), coral::AttributeList());
  coral::ICursor& cursor = query->execute();
  int SSID = -1;
  while (cursor.next())
  {
    const coral::AttributeList& alh = cursor.currentRow();
    coral::Blob blob = alh["VALUE"].data<coral::Blob>();
    long long *p1 = static_cast<long long*>(blob.startingAddress());
    if (*p1 == DBID)
    {
      SSID = alh["SUBSYSTEMID"].data<int>();
    }
  }
cursor.close();
cache_session->transaction().commit();

  return SSID;
}


BaseCache::BaseCache(coral::ISessionProxy *m_sess, coral::ISessionProxy *cache_sess, std::string DetName):m_session(m_sess), cache_session(cache_sess), DetectorName(DetName)
{
  ;
}

bool BaseCache::BuildAttrDataTypesCache(const coral::TimeStamp &startTime, const coral::TimeStamp &finishTime)
{
   
   /// payAttention became true when time of validity of one of the attributes was expired and assigned again.
   bool payAttention = false;
   if (cachedAttrTypeMap.size() > 0)
    cachedAttrTypeMap.erase(cachedAttrTypeMap.begin());
    m_session->transaction().start(true);
    std::auto_ptr<coral::IQuery> query(m_session->nominalSchema().newQuery());
    query->addToTableList(DetectorName+"_AttrDataTypeView", "AttrDataTypeView");
    query->addToOutputList("AttrName");
    query->addToOutputList("SubSystemTypeName");
    query->addToOutputList("StorageDataType");
    coral::AttributeList condition;
    condition.extend<std::string>("VALUEVALIDITYSTARTc");
    condition.extend<std::string>("VALUEVALIDITYFINISHc");
    condition[0].data<std::string>() = startTime.toString();
    condition[1].data<std::string>() = finishTime.toString();
    query->setCondition( "(RelationshipValidityStart  <= to_timestamp(:VALUEVALIDITYFINISHc, 'DD/MM/YY HH24:MI:SS.FF')) AND (RelationshipValidityFinish  >= to_timestamp(:VALUEVALIDITYSTARTc, 'DD/MM/YY HH24:MI:SS.FF'))", condition);// between RelationValidityStart and RelationValidityFinish ";
    coral::ICursor& cursor = query->execute();
    while (cursor.next())
    {
      const coral::AttributeList& alh = cursor.currentRow();
      std::string AttrName = alh["AttrName"].data<std::string>();
      std::string SubSystemTypeName = alh["SubSystemTypeName"].data<std::string>();
      std::string StorageDataType = alh["StorageDataType"].data<std::string>();
      fixstring1(AttrName);
      fixstring1(SubSystemTypeName);
      fixstring1(StorageDataType);
      if ( cachedAttrTypeMap[std::make_pair(SubSystemTypeName, AttrName)].length() > 1 )
      payAttention = true;
      cachedAttrTypeMap[std::make_pair(SubSystemTypeName, AttrName)] = StorageDataType;
    }
    cursor.close();
    
   m_session->transaction().commit();
  return payAttention;
}







std::string BaseCache::GetStorageType(std::string SubSystem, std::string Attribute)
{
  return cachedAttrTypeMap[std::make_pair(SubSystem, Attribute)];
}



