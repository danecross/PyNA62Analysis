// --------------------------------------------------------------
// History:
//           
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) 2013-12-06
// --------------------------------------------------------------

#include "BaseSubSystemTypeManager.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
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
#include <climits>
#include "CoralCommon/URIParser.h"




BaseSubSystemTypeManager::BaseSubSystemTypeManager(coral::ISessionProxy *m_sess, std::string DetName, std::string UserName, std::string SSName):m_session(m_sess), DetectorName(DetName), SubSystemName(SSName)
{
  ;
}



BaseSubSystemTypeManager::~BaseSubSystemTypeManager()
{
  ;
}




// Bug ? 
// BaseSubSystemTypeManager::AddAttribute warning. Given attribute is already exists in DB. Just validity will be set. AttrName = DBID
 
int BaseSubSystemTypeManager::AddAttribute(std::string AttrName, std::string DataTypeName, bool makevalid)
{

//    coral::URIParser *nu = new coral::URIParser();
//technology    std::cerr << nu->();
    
 //   std::cerr << " BaseSubSystemTypeManager::AddAttribute error. Provided DataType not found in DB. DataTypeName = " <<  DataTypeName << std::endl;


/// To do - check in Validity table existing of the required relationship and to prolongate if possible instead of adding one else.


    m_session->transaction().start();
    coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_DataType");
    coral::IQuery* query = table.newQuery();
    std::ostringstream condition;
    condition << "DataTypeName='" << DataTypeName <<"'";
    query->setCondition(condition.str(), coral::AttributeList());
    coral::ICursor& cursor = query->execute();
    int Attributiescount = 0;
    if (!cursor.next())
    {
      std::cerr << " BaseSubSystemTypeManager::AddAttribute error. Provided DataType not found in DB. DataTypeName = " <<  DataTypeName << std::endl;
      delete query;
      m_session->transaction().commit();
      return -1;
    }
    delete query;

    query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
    condition.str("");
    condition << " Name='" << AttrName << "' ";
    query->setCondition(condition.str(), coral::AttributeList());
    coral::ICursor& cursor1 = query->execute();
    bool exist = false;
    if (cursor1.next())
    {
      std::cerr << " BaseSubSystemTypeManager::AddAttribute warning. Given attribute already exists in DB. Just validity will be set. AttrName = " <<  AttrName << std::endl;
      m_session->transaction().commit();
      m_session->transaction().start();
      exist = true;
//      return -2;
    }

    delete query;
    if (exist == false)
    {
      query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
      coral::ICursor& cursor5 = query->execute();
      while (cursor5.next())
      {
	Attributiescount++;
        const coral::AttributeList& alh = cursor5.currentRow();
      }
      delete query;
      coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies");
      coral::AttributeList rowBuffer;
      rowBuffer.extend<std::string>("Name");
      rowBuffer.extend<std::string>("DataTypeName");
      rowBuffer.extend<int>("AttributeID");
      rowBuffer["Name"].data<std::string>() = AttrName;
      rowBuffer["DataTypeName"].data<std::string>() = DataTypeName;
      rowBuffer["AttributeID"].data<int>() = Attributiescount;
      table.dataEditor().insertRow(rowBuffer);
      TagAttribute("all", Attributiescount);
      m_session->transaction().commit();
      m_session->transaction().start();
    }

    if (makevalid)
    {

      query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
      condition.str("");
      condition << " Name='" << AttrName << "' ";
      query->setCondition(condition.str(), coral::AttributeList());
      coral::ICursor& cursor2 = query->execute();
      bool exist = false;
      cursor2.next();
      const coral::AttributeList& alh = cursor2.currentRow();
      int AttrID = alh["AttributeID"].data<int>();
      delete query;

//////////////////////////////////////////////////

      int Validitycount = 0;
      query = m_session->nominalSchema().tableHandle(DetectorName+"_RelationshipValidity").newQuery();
      coral::ICursor& cursor3 = query->execute();
      while (cursor3.next())
      {
	Validitycount++;
        const coral::AttributeList& alh = cursor3.currentRow();
      }
      delete query;

///////////////////////////////////////////////////

      coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_RelationshipValidity");
      coral::AttributeList rowBuffer;
      rowBuffer.extend<coral::TimeStamp>("RelationshipValidityStart");
      rowBuffer.extend<coral::TimeStamp>("RelationshipValidityFinish");
      rowBuffer.extend<std::string>("SubSystemTypeName");
      rowBuffer.extend<int>("AttributeID");
      rowBuffer.extend<int>("RelationshipValidityID");

      char outstr[200];
      time_t t;
      struct tm *curTime;
      t = time(NULL);
      curTime = gmtime(&t);
      
      rowBuffer["RelationshipValidityFinish"].data<coral::TimeStamp>() = coral::TimeStamp(2038,1,1,0,0,0,0,0);
      rowBuffer["RelationshipValidityStart"].data<coral::TimeStamp>() = coral::TimeStamp(2000, 01, 01, 00,00,00,0);
      rowBuffer["SubSystemTypeName"].data<std::string>() = SubSystemName;
      rowBuffer["AttributeID"].data<int>() = AttrID;
      rowBuffer["RelationshipValidityID"].data<int>() = Validitycount;
      table.dataEditor().insertRow(rowBuffer);
    }
    m_session->transaction().commit();
    return Attributiescount;
}

int BaseSubSystemTypeManager::GetAttributiesCount()
{
  time_t t = time(NULL);
  return GetAttributiesCount(t);
}

int BaseSubSystemTypeManager::GetAttributiesCount(time_t time)
{
//   m_session->transaction().commit();
   m_session->transaction().start();
   struct tm *curTime;
   curTime = gmtime(&time);
   const char* timeStringFormat = "%Y-%m-%d %H:%M:%S";
   const int timeStringLength = 20;
   char timeString[timeStringLength];
   strftime(timeString, timeStringLength, timeStringFormat, curTime);
   std::string currTime (timeString) ;
   int AttributiesCount = 0;
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_RelationshipValidity").newQuery();
   coral::AttributeList a_list;
   std::ostringstream condition;


    /// !!!!!!!! Oracle Syntax !!!!!
    condition << "(RelationshipValidityStart <= to_timestamp('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS')) AND (RelationshipValidityFinish  >= to_timestamp('" << currTime << "', 'YYYY-MM-DD HH24:MI:SS'))" ;// between RelationValidityStart and RelationValidityFinish ";
   
   
   
///////// MySQL Syntax!!!!!!!!!!!!!!!!!!
//   if (isDBType("mysql"))
//   condition << "(STR_TO_DATE(RelationValidityStart, '%Y-%m-%d %H:%i:%s')  <= STR_TO_DATE('" << currTime << "', '%Y-%m-%d %H:%i:%s')) AND (STR_TO_DATE(RelationValidityFinish, '%Y-%m-%d %H:%i:%s')  >= STR_TO_DATE('" << currTime << "', '%Y-%m-%d %H:%i:%s'))";// between RelationValidityStart and RelationValidityFinish ";

//////// SQLlite Syntax!!!!!!!!!!!!!!!!!!

//   std::cerr << " BaseSubSystemTypeManager: step 0 "<< std::endl;

//   condition << "(strftime('%s', RelationValidityStart)  <= strftime('%s','" << currTime << "') AND (strftime('%s', RelationValidityFinish)  >= strftime('%s', '" << currTime << "'))";// between RelationValidityStart and RelationValidityFinish ";

//   std::cerr << " BaseSubSystemTypeManager: step 1 "<< std::endl;


   query->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor = query->execute();
   while (cursor.next())
   {
      AttributiesCount++;
      const coral::AttributeList& alh = cursor.currentRow();
   }
   delete query;
   m_session->transaction().commit();
   return AttributiesCount;
}

int BaseSubSystemTypeManager::StopAttributeValidityNow(int AttributeNum)
{
   m_session->transaction().start();
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
   std::ostringstream condition;
   coral::ICursor& cursor = query->execute();
   bool exist = false;
   int count = 0;
   int AttributeID = -1;

   while (cursor.next())
    {
      const coral::AttributeList& alh = cursor.currentRow();
      if (AttributeNum == count)
      {
	AttributeID = alh[0].data<int>();
	break;
      }
      count++;
    }
   delete query;
   
   if (AttributeID == -1)
   {
     std::cerr << " BaseSubSystemTypeManager::StopAttributeVilidityNow error. Given attribute does not exist in DB. AttributeNum = " <<  AttributeNum << std::endl;
     return -1;
   }

   coral::ITableDataEditor& editor2 = m_session->nominalSchema().tableHandle(DetectorName+"_RelationshipValidity").dataEditor();

   std::string updateAction = "RelationshipValidityFinish = :ValidityFinish";
   condition.str("");
   condition << " AttributeID=" << AttributeID ;
   coral::AttributeList updateData;
   
   
   updateData.extend<coral::TimeStamp>( "ValidityFinish" );
   
   coral::TimeStamp& RelationValidityFinish = updateData[0].data<coral::TimeStamp>();
   coral::IBulkOperation* bulkUpdater = editor2.bulkUpdateRows( updateAction,
                                                               condition.str(),
                                                               updateData,
                                                               3 );
   time_t t;
   t = time(NULL);
   struct tm *curTime;
   curTime = gmtime(&t);
  
   RelationValidityFinish = coral::TimeStamp(curTime->tm_year+1900, 
					curTime->tm_mon+1, curTime->tm_mday, 
					curTime->tm_hour, 
					curTime->tm_min,
					curTime->tm_sec,
					0);

   
   
   
   bulkUpdater->processNextIteration();
   bulkUpdater->flush();
   delete bulkUpdater;

   m_session->transaction().commit();
   return 0;
}


int BaseSubSystemTypeManager::GetAttributeName(int AttributeNum, std::string &AttributeName)
{
   m_session->transaction().start(true);
   coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies");
   coral::IQuery* query = table.newQuery();
   int count  = 0;
   coral::ICursor& cursor = query->execute();
   while (cursor.next())
   {
     const coral::AttributeList& alh = cursor.currentRow();
     if (AttributeNum == count)
     AttributeName = alh[1].data<std::string>();
     count++;
   }
   delete query;
   m_session->transaction().commit();
   return count;
}

int BaseSubSystemTypeManager::CreateNewTag(std::string TagName)
{
   m_session->transaction().start();
   coral::IQuery* query = m_session->nominalSchema().tableHandle("AttributeTagNames").newQuery();
   std::ostringstream condition;
   condition.str("");
   condition << " Name='" << TagName <<"'";
   query->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor5 = query->execute();
   if (cursor5.next())
   {
    delete query;
    std::cerr << " BaseSubSystemTypeManager::CreateNewAttributeTag error. Given TagName already exists in DB. TagName = " <<  TagName << std::endl;
    return -1;
   }
   delete query;
   coral::ITable& table = m_session->nominalSchema().tableHandle("AttributeTagNames");
   coral::AttributeList rowBuffer;
   rowBuffer.extend<std::string>("Name");
   rowBuffer["Name"].data<std::string>() = TagName;
   table.dataEditor().insertRow(rowBuffer);
   m_session->transaction().commit();
   return 0;
}

int BaseSubSystemTypeManager::GetTagsCount()
{
   m_session->transaction().start(true);
   int Tagscount = 0;
   coral::IQuery* query = m_session->nominalSchema().tableHandle("AttributeTagNames").newQuery();
   std::ostringstream condition;
   coral::ICursor& cursor5 = query->execute();
   while (cursor5.next())
   {
    Tagscount++;
   }
   delete query;
   m_session->transaction().commit();
   return Tagscount;
}

int BaseSubSystemTypeManager::PrintAvaliableTags()
{
  m_session->transaction().start(true);
  coral::IQuery* query = m_session->nominalSchema().tableHandle("AttributeTagNames").newQuery();
  coral::ICursor& cursor5 = query->execute();
  std::cout << " There are next tags already registered in the offline db: " << "\n"<< std::endl;

  while (cursor5.next())
  {
   const coral::AttributeList& alh = cursor5.currentRow();
   std::cout << " tag name: " <<  alh[0].data<std::string>() << "\n"<< std::endl;
  }
  cursor5.close();
  delete query;
  m_session->transaction().commit();
}



int BaseSubSystemTypeManager::TagAttribute(std::string TagName, int AttributeID)
{
   m_session->transaction().start(true);
   coral::IQuery* query = m_session->nominalSchema().tableHandle(DetectorName+"_Attributies").newQuery();
   std::ostringstream condition;
   condition.str("");
   condition << " AttributeID=" << AttributeID;
   query->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor = query->execute();
   bool exist = false;
   int count = 0;
   if (!cursor.next())
   {
     std::cerr << " BaseSubSystemTypeManager::TagAttribute error. Given attribute does not exist in DB. AttributeID = " <<  AttributeID << std::endl;
     m_session->transaction().commit();
     delete query;
     return -1;

   }
   

//   m_session->transaction().start();
   query = m_session->nominalSchema().tableHandle("AttributeTagNames").newQuery();
   condition.str("");
   condition << " Name='" << TagName <<"'";
   query->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor5 = query->execute();
   if (!cursor5.next())
   {
    delete query;
    m_session->transaction().commit();
    std::cerr << " BaseSubSystemTypeManager::TagAttribute error. Given TagName does not exist in DB. TagName = " <<  TagName << std::endl;
    return -2;
   }
   delete query;
  

   query = m_session->nominalSchema().tableHandle(DetectorName+"_AttributesTags").newQuery();
   condition.str("");
   condition << " TagName='" << TagName <<"' AND AttributeID=" << AttributeID;
   query->setCondition(condition.str(), coral::AttributeList());
   coral::ICursor& cursor1 = query->execute();
   if (cursor1.next())
   {
    delete query;
    std::cerr << " BaseSubSystemTypeManager::TagAttribute error. Given Attribute is already tagged with the same tag = " <<  TagName << std::endl;
    m_session->transaction().commit();
    return -3;
   }
   delete query;

   m_session->transaction().commit();
   m_session->transaction().start();

   coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_AttributesTags");
   coral::AttributeList rowBuffer;
   rowBuffer.extend<int>("AttributeID");
   rowBuffer.extend<std::string>("TagName");
   rowBuffer["AttributeID"].data<int>() = AttributeID;
   rowBuffer["TagName"].data<std::string>() = TagName;
   table.dataEditor().insertRow(rowBuffer);
   m_session->transaction().commit();

 return 0;
}



int BaseSubSystemTypeManager::RemoveTagName(std::string TagName)
{
  
  if (TagName.compare("all") == 0)
  {
     std::cerr << " The tag named \"all\"  cannot be removed" << std::endl;
     return -0;
  }
  
   m_session->transaction().start();
   coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_AttributesTags");
   coral::AttributeList rowBuffer;
   std::ostringstream condition;
   condition.str("");
   condition << " TagName='" << TagName <<"'";
   table.dataEditor().deleteRows(condition.str(), rowBuffer);

   coral::ITable& table1 = m_session->nominalSchema().tableHandle("AttributeTagNames");
   coral::AttributeList rowBuffer1;
   condition.str("");
   condition << " Name='" << TagName <<"'";
   table1.dataEditor().deleteRows(condition.str(), rowBuffer1);


   m_session->transaction().commit();


   return 0;
}

















