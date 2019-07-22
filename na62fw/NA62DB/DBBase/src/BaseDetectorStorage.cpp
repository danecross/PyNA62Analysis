// --------------------------------------------------------------
// History:
//           
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) 2013-12-06
// --------------------------------------------------------------

#include "BaseDetectorStorage.h"

#include <exception>
#include <stdexcept>
#include <boost/filesystem.hpp>

using namespace std;



BaseDetectorStorage::BaseDetectorStorage(std::string connString, coral::ISessionProxy *cache_session, std::string DetName, std::string UsrName, coral::AccessMode oracleAccessMode): DetectorName(DetName), UserName(UsrName)
{
   context = &coral::Context::instance();
   lookSvcH = context->query<coral::IConnectionService>();
   if (!lookSvcH.isValid()) {
   context->loadComponent("CORAL/Services/ConnectionService");
     lookSvcH = context->query<coral::IConnectionService>();
   }
   if (!lookSvcH.isValid()) {
     throw runtime_error("Could not locate the connection service");
   }
   
   m_session = lookSvcH->connect(connString, oracleAccessMode);
   release = true;
   currentCache = new BaseCache(m_session, cache_session, DetName);

}


BaseDetectorStorage::BaseDetectorStorage(std::string connString, std::string DetName, std::string UsrName, coral::AccessMode oracleAccessMode): DetectorName(DetName), UserName(UsrName)
{
   context = &coral::Context::instance();
   lookSvcH = context->query<coral::IConnectionService>();
   if (!lookSvcH.isValid()) {
   context->loadComponent("CORAL/Services/ConnectionService");
     lookSvcH = context->query<coral::IConnectionService>();
   }
   if (!lookSvcH.isValid()) {
     throw runtime_error("Could not locate the connection service");
   }

   m_session = lookSvcH->connect(connString, oracleAccessMode);
   release = true;
   std::string cacheconnname;
   //cachefilename = tempnam, "dbc");
   
   boost::filesystem::path temp = boost::filesystem::unique_path("/tmp/%%%%%%%%.db");
   cachefilename    = temp.native();  // optional
   
   cacheconnname = "sqlite_file:"+ cachefilename;
   std::cerr << " Cache filename " <<  cachefilename  <<  std::endl;
   coral::ISessionProxy *cache_session = lookSvcH->connect(cacheconnname, coral::Update);
   currentCache = new BaseCache(m_session, cache_session, DetName);
   std::cerr << "cache_session created" <<  std::endl;

}


BaseDetectorStorage::~BaseDetectorStorage()
{
 /* if (release)
  {
      m_session->transaction().commit();
      delete m_session;
      lookSvcH->purgeConnectionPool();
  }
  */
       lookSvcH->purgeConnectionPool();
       delete currentCache;
       remove(cachefilename.c_str());
}


BaseDetectorStorage::BaseDetectorStorage(coral::ISessionProxy *m_sess, coral::ISessionProxy *cache_session, std::string DetName, std::string UsrName):m_session(m_sess), DetectorName(DetName), UserName(UsrName)
{
   release = false;
   currentCache = new BaseCache(m_session, cache_session, DetName);

}

int BaseDetectorStorage::GetSSIDbyNum(std::string SubSystemTypeName, int SubSystemNum)
{
    
    SSTNSSN_set::iterator it=SSTNSSN_set_.find(boost::tuples::make_tuple(SubSystemTypeName,SubSystemNum));
    if (it != SSTNSSN_set_.end() )
    {
      return it->SSID_;
    }
    else
    {
         m_session->transaction().start(true);
         coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_DetectorSubSystem");
         coral::IQuery* query = table.newQuery();
     
         std::ostringstream condition;
         condition.str("");
         condition << "SubSystemTypeName='" << SubSystemTypeName <<"'";
         query->setCondition(condition.str(), coral::AttributeList());
         coral::ICursor& cursor = query->execute();
         long long count  = 0;
         long long SSID = -1;
         while (cursor.next())
         {
           if (SubSystemNum == count)
           {
             const coral::AttributeList& alh = cursor.currentRow();
             SSID = alh["SubSystemID"].data<int>();
             break;
           }
           count++;
         }
         m_session->transaction().commit();
         delete query;
         SSTNSSN_set_.insert(SSTNSSNcontainer(SubSystemTypeName,SubSystemNum, SSID));
         return SSID;
    }
}


int BaseDetectorStorage::GetSubSystemsCount(std::string SubSystemTypeName)
{
    m_session->transaction().start(true);
    coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_DetectorSubSystem");
    coral::IQuery* query = table.newQuery();

    std::ostringstream condition;
    condition.str("");
    condition << "SubSystemTypeName='" << SubSystemTypeName <<"'";
    query->setCondition(condition.str(), coral::AttributeList());

    coral::ICursor& cursor = query->execute();
    int count  = 0;
    while (cursor.next())
    {
      count++;
      const coral::AttributeList& alh = cursor.currentRow();
    }
    delete query;
    m_session->transaction().commit();
    return count;
}


int BaseDetectorStorage::GetSubSystemsCount_stored(std::string SubSystemTypeName)
{
  int ret = 0;
  m_session->transaction().start(true);
  boost::scoped_ptr<coral::IQuery> DataQuery(m_session->nominalSchema().newQuery());
  DataQuery->addToTableList(DetectorName+"_CORALAVLOIDERSSTC1", "CORALAVLOIDERSSTC1");
  DataQuery->addToOutputList("SUBSYSTEMCOUNT");
  std::ostringstream condition;
  condition << "SUBSYSTEMTYPENAME='" << SubSystemTypeName <<"'";
  DataQuery->setCondition(condition.str(), coral::AttributeList());
  coral::ICursor& DataCursor = DataQuery->execute();
  if (DataCursor.next())
  {
    const coral::AttributeList& alh = DataCursor.currentRow();
    ret = alh["SUBSYSTEMCOUNT"].data<int>();
  }
  DataCursor.close();
  m_session->transaction().commit();
  return ret;

/*   
  m_session->transaction().start(true);
  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  inputData.extend<std::string>( "Detector_prefix" );
  inputData.extend<std::string>( "SubSystemTypeName1" );
  inputData.extend<int>( "SubSystemCount" );
  int count = 0;
  inputData[0].data<std::string>() = DetectorName;
  inputData[1].data<std::string>() = SubSystemTypeName;
  inputData[2].data<int>() = 0;
  schema.callProcedure( "GETSUBSYSTEMSCOUNT_STORED", inputData );
  m_session->transaction().commit();
  return inputData[2].data<int>();
*/  
}

int BaseDetectorStorage::RegisterNewSubSystem(std::string SubSystemTypeName)
{
  	long long SSID = AddSubSystem(SubSystemTypeName);
	std::cout << "SSID" << SSID << std::endl;
	  
	BaseSubSystemStorage *SS = GetBaseSubSystemStorage(SSID, false);  
	long long DBID = GetSubSystemsCount_stored(SubSystemTypeName)-1;
		    
	std::cout << "I will register subsystem " << SubSystemTypeName << " with DBID " << DBID << std::endl;
			    
	std::string DBIDstr("DBID");

	SS->SetAttributeValue_stored((void*) &DBID, (int) sizeof(long long), DBIDstr); //bug DBID -> &DBID
	delete SS;
	return SSID;
}


int BaseDetectorStorage::RegisterNewSubSystem(std::string SubSystemTypeName, std::string IstanceName){
  
  std::cout << "RegisterNewSubSystem >> Try to Register a New SubSystem of type " << SubSystemTypeName << " and with Name = " << IstanceName << std::endl;

  long long nDBID = GetSubSystemsCount_stored(SubSystemTypeName);

  cout << "RegisterNewSubsystem >> Found " << nDBID << " instances " << endl;

  int countCond = 0;
  if (nDBID) {
    m_session->transaction().start(true);
    boost::scoped_ptr<coral::IQuery> DataQuery(m_session->nominalSchema().newQuery());
    DataQuery->addToTableList(DetectorName+"_CONSOLIDATEVIEW", "CONSOLIDATEVIEW");
    DataQuery->addToOutputList("VALUE");
    std::ostringstream condition;
    condition.str("");
    condition << "ATTRIBUTENAME='Name'";
    condition << "AND SUBSYSTEMTYPENAME='" << SubSystemTypeName <<"'";
    DataQuery->setCondition(condition.str(), coral::AttributeList());

    coral::ICursor& cursorCond = DataQuery->execute();
    int count = 0;    
    while (cursorCond.next()) {
      count++;
      const coral::AttributeList& alh = cursorCond.currentRow();
      coral::Blob nameTemp = alh["VALUE"].data<coral::Blob>();

      //
      // Important: nameTamp.size() seems to account for the string length, 1 space, and the string itself
      //

      char* p1 = static_cast<char*>(nameTemp.startingAddress());
      int stringLength =0;
      for (int kk=0; kk<nameTemp.size(); kk++){
	stringLength = kk;
	if (((int)p1[kk] == 32) || ((int) p1[kk]<48) || ((int) p1[kk]>57)) break;
      }
      std::string readOutString(p1+stringLength+1);
      if (readOutString.compare(IstanceName)==0) countCond++; // a subsystem of the same type and with the same name was already present
    }
    cursorCond.close();
    m_session->transaction().commit(); 
  }

  if (countCond == 0) {
    cout << "RegisterNewSubsystem >> Will register a new subsystem" << SubSystemTypeName << " and will attribute name " << IstanceName << " nDBID " << nDBID << endl;
    int newSSID = RegisterNewSubSystem(SubSystemTypeName);
    SetAttributeValue(SubSystemTypeName,"Name",IstanceName,nDBID);
    return newSSID;
  }
  else {
    cout << "RegisterNewSubsystem >> existing instance of subsystem " << SubSystemTypeName << " with name " << IstanceName << " found " << countCond << " times" << endl;
    return -1;
  }
}



int BaseDetectorStorage::AddSubSystem(std::string SubSystemTypeName)
{
   int count;
   m_session->transaction().start(true);
   boost::scoped_ptr<coral::IQuery> DataQuery(m_session->nominalSchema().newQuery());
   DataQuery->addToTableList(DetectorName+"_CORALAVOIDERTCS", "CORALAVOIDERTCS");
   DataQuery->addToOutputList("TOTALSUBSYSTEM");

   coral::ICursor& DataCursor = DataQuery->execute();
   if (DataCursor.next())
   {
     const coral::AttributeList& alh = DataCursor.currentRow();
     count = alh["TOTALSUBSYSTEM"].data<int>();
   }
   DataCursor.close();
   m_session->transaction().commit();

    
    m_session->transaction().start();
    coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_DetectorSubSystem");
    coral::AttributeList rowBuffer;
    rowBuffer.extend<std::string>("SubSystemTypeName");
    rowBuffer.extend<int>("SubSystemID");
    rowBuffer["SubSystemTypeName"].data<std::string>() = SubSystemTypeName;
    rowBuffer["SubSystemID"].data<int>() = count;
    table.dataEditor().insertRow(rowBuffer);
    m_session->transaction().commit();
    currentCache->RefreshSubSystemsCache();
    return count;
}

/*
int BaseDetectorStorage::AddSubSystem_stored(std::string SubSystemTypeName)
{
  m_session->transaction().start();
  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  inputData.extend<std::string>( "Detector_prefix" );
  inputData.extend<std::string>( "SubSystemTypeName" );
  inputData.extend<int>( "SubSystemCount" );
  int count = 0;
  inputData[0].data<std::string>() = DetectorName;
  inputData[1].data<std::string>() = SubSystemTypeName;
  inputData[2].data<int>() = 0;
  schema.callProcedure( "ADDSUBSYSTEM_STORED", inputData );
  m_session->transaction().commit();
  return inputData[2].data<int>();
}
*/

int BaseDetectorStorage::AddSubSystemType(std::string SubSystemTypeName)
{
    m_session->transaction().start();
    coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_SubSystemType");
    coral::AttributeList rowBuffer;
    rowBuffer.extend<std::string>("SubSystemTypeName");
    rowBuffer[0].data<std::string>() = SubSystemTypeName;
    table.dataEditor().insertRow(rowBuffer);
    m_session->transaction().commit();
    return 0;
}


int BaseDetectorStorage::AddSubSystemType_stored(std::string SubSystemTypeName)
{

  m_session->transaction().start();
  coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_SubSystemType");
  coral::IQuery* query = table.newQuery();

  coral::AttributeList rowBuffer;
  rowBuffer.extend<std::string>("SubSystemTypeName");

  std::ostringstream condition;
  condition.str("");
  condition << "SubSystemTypeName='" << SubSystemTypeName << "'";

  query->setCondition(condition.str(), coral::AttributeList());

  coral::ICursor& cursor2 = query->execute();

  int count  = 0;
  while (cursor2.next()) {count++;}
  delete query;

  if (count > 0) {
    std::cout << "AddSubSystemType_stored >> SubSystemTypeName " << SubSystemTypeName << " already exists in DB" << std::endl;
    return 1;
  }

  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  inputData.extend<std::string>( "Detector_prefix" );
  inputData.extend<std::string>( "SubSystemTypeName1" );
  inputData[0].data<std::string>() = DetectorName;
  inputData[1].data<std::string>() = SubSystemTypeName;
  schema.callProcedure( "ADDSUBSYSTEMTYPE_STORED", inputData );
  m_session->transaction().commit();
  return 0;
  
}





BaseSubSystemTypeManager* BaseDetectorStorage::GetBaseSubSystemTypeManager(std::string SSName)
{
 return new BaseSubSystemTypeManager(m_session, DetectorName, UserName, SSName);
}

BaseSubSystemStorage* BaseDetectorStorage::GetBaseSubSystemStorage(int SSID, bool StartWithCache)
{
 return new BaseSubSystemStorage(m_session, DetectorName, UserName, SSID, currentCache, StartWithCache);

}



int BaseDetectorStorage::GetSubSystemTypesCount()
{
    m_session->transaction().start(true);
    coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_SubSystemType");
    coral::IQuery* query = table.newQuery();
    coral::ICursor& cursor = query->execute();
    int count  = 0;
    while (cursor.next())
    {
      count++;
    }
    delete query;
    m_session->transaction().commit();
    return count;
}



int BaseDetectorStorage::GetSubSystemTypesCount_stored()
{
  int ret = 0;
  m_session->transaction().start(true);
  boost::scoped_ptr<coral::IQuery> DataQuery(m_session->nominalSchema().newQuery());
  DataQuery->addToTableList(DetectorName+"_CORALAVLOIDERSST0", "CORALAVLOIDERSST0");
  DataQuery->addToOutputList("SSTCOUNT");

  std::ostringstream condition;
  condition.str("");
  DataQuery->setCondition(condition.str(), coral::AttributeList());
  coral::ICursor& DataCursor = DataQuery->execute();
  if (DataCursor.next())
  {
    const coral::AttributeList& alh = DataCursor.currentRow();
    ret = (int)alh["SSTCOUNT"].data<int>();
  }
  DataCursor.close();
  m_session->transaction().commit();
  return ret;

   
/*   
  m_session->transaction().start(true);
  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  inputData.extend<std::string>( "Detector_prefix" );
  inputData.extend<int>( "SubSystemTypesCount" );
   inputData[0].data<std::string>() = DetectorName;
  inputData[1].data<int>() = 0;
  schema.callProcedure( "GETSUBSYSTEMTYPESCOUNT_STORED", inputData );
  m_session->transaction().commit();
  return inputData[1].data<int>();
*/
}


int BaseDetectorStorage::DeployTmpTable()
{
  m_session->transaction().start();
  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  inputData.extend<std::string>( "Detector_prefix" );
  inputData[0].data<std::string>() = DetectorName;
  schema.callProcedure( "CREATETMPTDATAVAL", inputData );
  m_session->transaction().commit();
  return 0;
}


int BaseDetectorStorage::SetAutoIncrementValues()
{
  m_session->transaction().start();
  coral::ISchema& schema = m_session->nominalSchema();
  coral::AttributeList inputData;
  schema.callProcedure( "SETAUTOINCREMENT", inputData);
  m_session->transaction().commit();
  return 0;
}


int BaseDetectorStorage::AddNewDataType(std::string DataTypeName,std::string StorageDataType, std::string Units, std::string Description)
{

    m_session->transaction().start();
    coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_DataType");
    coral::IQuery* query = table.newQuery();
    std::ostringstream condition;
    condition.str("");
    condition << "DataTypeName='" << DataTypeName <<"'";
    query->setCondition(condition.str(), coral::AttributeList());
    coral::ICursor& cursor = query->execute();
    if (cursor.next())
    {
      std::cerr << " BaseDetectorStorage::AddNewDataType error. Given DataType is already exists in DB.  DataTypeName = " <<  DataTypeName << std::endl;
      m_session->transaction().commit();
      return -1;
    }
    coral::AttributeList rowBuffer;
    rowBuffer.extend<std::string>("DataTypeName");
    rowBuffer.extend<std::string>("Units");
    rowBuffer.extend<std::string>("Description");
    rowBuffer.extend<std::string>("StorageDataType");
    rowBuffer["DataTypeName"].data<std::string>() = DataTypeName;
    rowBuffer["Units"].data<std::string>() = Units;
    rowBuffer["Description"].data<std::string>() = Description;
    rowBuffer["StorageDataType"].data<std::string>() = StorageDataType;
    table.dataEditor().insertRow(rowBuffer);
    m_session->transaction().commit();
    
}


int BaseDetectorStorage::BuildCache(const coral::TimeStamp &startTime, const coral::TimeStamp &finishTime, const std::vector<std::string> & tagged)
{
  
  currentCache->BuildAttrDataTypesCache(startTime, finishTime);
  currentCache->CreateValCache(startTime, finishTime, tagged);
  std::cerr << "Cache built" <<  std::endl;
  return 0;
}


/*
int BaseDetectorStorage::GetSubSystemTypeName(int num, std::string &DetName)
{
    m_session->transaction().start(true);
    coral::ITable& table = m_session->nominalSchema().tableHandle(DetectorName+"_SubSystemType");
    coral::IQuery* query = table.newQuery();
    coral::ICursor& cursor = query->execute();
    int count  = 0;
    while (cursor.next())
    {
      const coral::AttributeList& alh = cursor.currentRow();
      if (num == count)
      DetName = alh[0].data<std::string>();
      count++;
    }
    delete query;
    m_session->transaction().commit();
    return count;
}
*/






int BaseDetectorStorage::DeployDefaultSchema(bool removeifexist)
{

    // create a table in the assigned schema
    coral::TableDescription descrDataType;
    descrDataType.setName(DetectorName+"_DataType");
    descrDataType.insertColumn("DataTypeName", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrDataType.setPrimaryKey("DataTypeName");
    descrDataType.insertColumn("StorageDataType", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrDataType.insertColumn("Units", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrDataType.insertColumn("Description", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
  
    coral::TableDescription descrAttributesTagsName;
    descrAttributesTagsName.setName("AttributeTagNames");
    descrAttributesTagsName.insertColumn("Name", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrAttributesTagsName.setPrimaryKey("Name");

    coral::TableDescription descrDetectorSubSystemType;
    descrDetectorSubSystemType.setName(DetectorName+"_SubSystemType");
    descrDetectorSubSystemType.insertColumn("SubSystemTypeName", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrDetectorSubSystemType.setPrimaryKey("SubSystemTypeName");

    coral::TableDescription descrAttributies;
    descrAttributies.setName(DetectorName+"_Attributies");
    descrAttributies.insertColumn("AttributeID", coral::AttributeSpecification::typeNameForId(typeid(int)));
    descrAttributies.setPrimaryKey("AttributeID");
    descrAttributies.insertColumn("Name", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrAttributies.insertColumn("DataTypeName", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrAttributies.createForeignKey( DetectorName+"_Attributies" + "_FK1", "DataTypeName", DetectorName+"_DataType", "DataTypeName" );

    coral::TableDescription descrDetectorSubSystem;
    descrDetectorSubSystem.setName(DetectorName+"_DetectorSubSystem");
    descrDetectorSubSystem.insertColumn("SubSystemID", coral::AttributeSpecification::typeNameForId(typeid(int)));
    descrDetectorSubSystem.setPrimaryKey("SubSystemID");
    descrDetectorSubSystem.insertColumn("SubSystemTypeName", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrDetectorSubSystem.createForeignKey( DetectorName+"_DetectorSubSystem" + "_FK1", "SubSystemTypeName", DetectorName+"_SubSystemType", "SubSystemTypeName" );

    coral::TableDescription descrValues;
    descrValues.setName(DetectorName+"_Values");
    descrValues.insertColumn("ValueID", coral::AttributeSpecification::typeNameForId(typeid(long long)));
    descrValues.setPrimaryKey("ValueID");
    descrValues.insertColumn("TimeOfValAdding", coral::AttributeSpecification::typeNameForId(typeid( coral::TimeStamp )), 6);
    descrValues.insertColumn("SubSystemID", coral::AttributeSpecification::typeNameForId(typeid(int)));
    descrValues.insertColumn("AttributeID", coral::AttributeSpecification::typeNameForId(typeid(int)));
    descrValues.insertColumn("ValueContent",coral::AttributeSpecification::typeNameForId( typeid(coral::Blob)));
    descrValues.insertColumn("ValueValidityStart", coral::AttributeSpecification::typeNameForId(typeid( coral::TimeStamp )), 6);
    descrValues.insertColumn("ValueValidityFinish",coral::AttributeSpecification::typeNameForId(typeid( coral::TimeStamp )), 6);
    descrValues.createForeignKey( DetectorName+"_Values" + "_FK1", "AttributeID", DetectorName+"_Attributies", "AttributeID" );
    descrValues.createForeignKey( DetectorName+"_Values" + "_FK2", "SubSystemID", DetectorName+"_DetectorSubSystem", "SubSystemID" );

    coral::TableDescription descrRelationshipValidity;
    descrRelationshipValidity.setName(DetectorName+"_RelationshipValidity");
    descrRelationshipValidity.insertColumn("RelationshipValidityID", coral::AttributeSpecification::typeNameForId(typeid(int)));
    descrRelationshipValidity.setPrimaryKey("RelationshipValidityID");
    descrRelationshipValidity.insertColumn("SubSystemTypeName", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrRelationshipValidity.insertColumn("AttributeID", coral::AttributeSpecification::typeNameForId(typeid(int)));
    descrRelationshipValidity.insertColumn("RelationshipValidityStart",coral::AttributeSpecification::typeNameForId(typeid( coral::TimeStamp )), 6);
    descrRelationshipValidity.insertColumn("RelationshipValidityFinish",coral::AttributeSpecification::typeNameForId(typeid( coral::TimeStamp )), 6);
    descrRelationshipValidity.createForeignKey( DetectorName+"_RelationshipValidity" + "_FK1", "AttributeID", DetectorName+"_Attributies", "AttributeID" );
    descrRelationshipValidity.createForeignKey( DetectorName+"_RelationshipValidity" + "_FK2", "SubSystemTypeName", DetectorName+"_SubSystemType", "SubSystemTypeName" );

    coral::TableDescription descrAttributesGroup;
    descrAttributesGroup.setName(DetectorName+"_AttributesTags");
    descrAttributesGroup.insertColumn("TagName", coral::AttributeSpecification::typeNameForType<std::string>(), 50);
    descrAttributesGroup.insertColumn("AttributeID", coral::AttributeSpecification::typeNameForType<int>());
    descrAttributesGroup.createForeignKey( DetectorName+"_AttributesTags" + "_FK1", "AttributeID", DetectorName+"_Attributies", "AttributeID" );

    if (removeifexist)
    {
      m_session->transaction().start();
      m_session->nominalSchema().dropIfExistsView(DetectorName+"_AttrDataTypeView");
      m_session->nominalSchema().dropIfExistsView(DetectorName+"_CONSOLIDATEVIEW");
	  m_session->nominalSchema().dropIfExistsView(DetectorName+"_CORALAVLOIDERSST0");
      m_session->nominalSchema().dropIfExistsView(DetectorName+"_CORALAVLOIDERSSTC1");
	  m_session->nominalSchema().dropIfExistsView(DetectorName+"_CORALAVOIDERTCS");
	  m_session->transaction().commit();
    }
    m_session->transaction().start();
    coral::IViewFactory* descrAttrDataTypeView = m_session->nominalSchema().viewFactory();
    descrAttrDataTypeView->addToOutputList( DetectorName+"_DataType.StorageDataType", "StorageDataType" );
    descrAttrDataTypeView->addToOutputList( DetectorName+"_Attributies.Name", "AttrName" );
    descrAttrDataTypeView->addToOutputList( DetectorName+"_RelationshipValidity.SubSystemTypeName", "SubSystemTypeName" );
    descrAttrDataTypeView->addToOutputList( DetectorName+"_RelationshipValidity.RelationshipValidityStart", "RelationshipValidityStart" );
    descrAttrDataTypeView->addToOutputList( DetectorName+"_RelationshipValidity.RelationshipValidityFinish", "RelationshipValidityFinish" );
    descrAttrDataTypeView->addToTableList(DetectorName+"_DataType");
    descrAttrDataTypeView->addToTableList(DetectorName+"_Attributies");
    descrAttrDataTypeView->addToTableList(DetectorName+"_RelationshipValidity");
    descrAttrDataTypeView->setCondition( DetectorName+"_DataType.DataTypeName = " + DetectorName + "_Attributies.DataTypeName AND " + DetectorName + "_RelationshipValidity.AttributeID = " + 
	    DetectorName + "_Attributies.AttributeID", coral::AttributeList() );
   
    if (removeifexist)
    { 
      m_session->nominalSchema().dropIfExistsView(DetectorName+"_AttrDataTypeView");
      m_session->nominalSchema().dropIfExistsTable(descrRelationshipValidity.name());
      m_session->nominalSchema().dropIfExistsTable(descrAttributesGroup.name());
      m_session->nominalSchema().dropIfExistsTable(descrValues.name());
      m_session->nominalSchema().dropIfExistsTable(descrDetectorSubSystem.name());
      m_session->nominalSchema().dropIfExistsTable(descrAttributies.name());
      m_session->nominalSchema().dropIfExistsTable(descrDetectorSubSystemType.name());
      m_session->nominalSchema().dropIfExistsTable(descrDataType.name());
 //     m_session->nominalSchema().dropIfExistsTable(descrAttributesTagsName.name());
    }    
    
    if (!m_session->nominalSchema().existsTable(descrAttributesTagsName.name()))
      m_session->nominalSchema().createTable( descrAttributesTagsName );
    if (!m_session->nominalSchema().existsTable(descrDataType.name()))
      m_session->nominalSchema().createTable( descrDataType );
    if (!m_session->nominalSchema().existsTable(descrDetectorSubSystemType.name()))
      m_session->nominalSchema().createTable( descrDetectorSubSystemType );     
    if (!m_session->nominalSchema().existsTable(descrAttributies.name()))
      m_session->nominalSchema().createTable( descrAttributies );
    if (!m_session->nominalSchema().existsTable(descrDetectorSubSystem.name()))
      m_session->nominalSchema().createTable( descrDetectorSubSystem );
    if (!m_session->nominalSchema().existsTable(descrValues.name()))
      m_session->nominalSchema().createTable( descrValues );
    if (!m_session->nominalSchema().existsTable(descrAttributesGroup.name()))
      m_session->nominalSchema().createTable( descrAttributesGroup );
    if (!m_session->nominalSchema().existsTable(descrRelationshipValidity.name()))
      m_session->nominalSchema().createTable( descrRelationshipValidity );

    descrAttrDataTypeView->create(DetectorName+"_AttrDataTypeView");//vilegeManager().grantToPublic( coral::ITablePrivilegeManager::Select );
    coral::ITable& table = m_session->nominalSchema().tableHandle("AttributeTagNames");
    coral::AttributeList rowBuffer;
    coral::IQuery* query = table.newQuery();
    rowBuffer.extend<std::string>("Name");
    std::ostringstream condition;
    condition.str("");
    condition << " Name='" << "all" << "' ";
    query->setCondition(condition.str(), coral::AttributeList());
    coral::ICursor& cursor2 = query->execute();

    if (!cursor2.next())
    {
     rowBuffer["Name"].data<std::string>() = "all";
     table.dataEditor().insertRow(rowBuffer);
    }
    cursor2.close();
    delete query;

    // construction_mc tag

    condition.str("");
    condition << " Name='" << "construction_mc" << "' ";
    query = table.newQuery();
    query->setCondition(condition.str(), coral::AttributeList());
    coral::ICursor& cursor3  = query->execute();
    if (!cursor3.next())
    {
     rowBuffer["Name"].data<std::string>() = "construction_mc";
     table.dataEditor().insertRow(rowBuffer);
    }
    cursor3.close();
    delete query;
    //    m_session->transaction().commit();

    // configuration_data tag

    condition.str("");
    condition << " Name='" << "configuration_data" << "' ";
    query = table.newQuery();
    query->setCondition(condition.str(), coral::AttributeList());
    coral::ICursor& cursor4  = query->execute();
    if (!cursor4.next())
    {
     rowBuffer["Name"].data<std::string>() = "configuration_data";
     table.dataEditor().insertRow(rowBuffer);
    }
    cursor4.close();
    delete query;
    m_session->transaction().commit();



    delete descrAttrDataTypeView;
    
    coral::sleepSeconds(1);
    
    DeployTmpTable();
    //SetAutoIncrementValues();
    return 0;
}




/////////////////////////
// Setters
/////////////////////////

int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, int & value, int id, coral::TimeStamp start_time,coral::TimeStamp end_time) {
	return SetAttributeValue(ss_name, attr_name ,(void *)&value, sizeof(int) ,id, start_time, end_time);
}

int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, int64_t & value, int id, coral::TimeStamp start_time,coral::TimeStamp end_time) {
	return SetAttributeValue(ss_name, attr_name ,(void *)&value, sizeof(int64_t) ,id, start_time, end_time);
}

int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, long long & value, int id, coral::TimeStamp start_time,coral::TimeStamp end_time) {
	return SetAttributeValue(ss_name, attr_name ,(void *)&value, sizeof(long long) ,id, start_time, end_time);
}

int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, float & value, int id, coral::TimeStamp start_time,coral::TimeStamp end_time) {
	return SetAttributeValue(ss_name, attr_name ,(void *)&value, sizeof(float) ,id, start_time, end_time);
}

int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, double & value, int id, coral::TimeStamp start_time,coral::TimeStamp end_time) {
	return SetAttributeValue(ss_name, attr_name ,(void *)&value, sizeof(double) ,id, start_time, end_time);
	//	return SerializeAttributeValue< double >(ss_name,attr_name,value,id,start_time,end_time);
}
	
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, std::vector<int> & value, int id,  coral::TimeStamp start_time,coral::TimeStamp end_time) {
  return SerializeAttributeValue< std::vector<int> >(ss_name,attr_name,value,id,start_time,end_time);
}
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, std::vector<float> & value, int id,  coral::TimeStamp start_time,coral::TimeStamp end_time) {
  return SerializeAttributeValue< std::vector<float> >(ss_name,attr_name,value,id,start_time,end_time);
}
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, std::vector<double> & value, int id,  coral::TimeStamp start_time,coral::TimeStamp end_time) {
  return SerializeAttributeValue< std::vector<double> >(ss_name,attr_name,value,id,start_time,end_time);
}
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, std::vector<long long> & value, int id,  coral::TimeStamp start_time,coral::TimeStamp end_time) {
  return SerializeAttributeValue< std::vector<long long> >(ss_name,attr_name,value,id,start_time,end_time);
}
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, std::vector<int64_t> & value, int id,  coral::TimeStamp start_time,coral::TimeStamp end_time) {
  return SerializeAttributeValue< std::vector<int64_t> >(ss_name,attr_name,value,id,start_time,end_time);
}
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, std::vector<std::string> & value, int id,  coral::TimeStamp start_time,coral::TimeStamp end_time) {
  return SerializeAttributeValue< std::vector<std::string> >(ss_name,attr_name,value,id,start_time,end_time);
}
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, std::string & value, int id,  coral::TimeStamp start_time,coral::TimeStamp end_time) {
  return SerializeAttributeValue< std::string >(ss_name,attr_name,value,id,start_time,end_time);
  //  return SetAttributeValue(ss_name, attr_name ,(void *)&value, value.size() ,id, start_time, end_time);
}
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, std::vector< std::vector<double> > & value, int id,  coral::TimeStamp start_time,coral::TimeStamp end_time) {
	return SerializeAttributeValue< std::vector< std::vector<double> > >(ss_name,attr_name,value,id,start_time,end_time);
}


/////////////////////
// Private methods //
/////////////////////

/// \bug return value is undefined after SS->SetAttributeValue_stored(...) call
int BaseDetectorStorage::SetAttributeValue(std::string ss_name, std::string attr_name, void * value, short size, int id, coral::TimeStamp start_time, coral::TimeStamp end_time) {
  int SSID = -1;
  SSID = GetSSIDbyNum(ss_name, id);
  if (SSID != -1)
    {
      BaseSubSystemStorage *SS = GetBaseSubSystemStorage(SSID, true); // was false 
      int ret = SS->SetAttributeValue_stored(value, size, start_time, end_time, attr_name); 
      delete SS;
      return ret;
    }
  return -1;
}

/////////////////////////
// Getters, we use the original interface, without passing any conversion factor (why was it there, anyhow?)
/////////////////////////

int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, int & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return GetAttributeValue(ss_name,attr_name,(void*)&value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, int64_t & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return GetAttributeValue(ss_name,attr_name,(void*)&value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, long long & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return GetAttributeValue(ss_name,attr_name,(void*)&value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, float & value, int id, coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return GetAttributeValue(ss_name,attr_name,(void*)&value,id,target_time,start_time,end_time);
	//	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, double & value, int id, coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return GetAttributeValue(ss_name,attr_name,(void*)&value,id,target_time,start_time,end_time);
	//	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}


int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<int> & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<float> & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<long long> & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<double> & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<int64_t> & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}

int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, std::vector< std::vector<double> > & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}

int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<std::string> & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, std::string & value, int id,coral::TimeStamp target_time, coral::TimeStamp * start_time, coral::TimeStamp * end_time) {
	return DeserializeAttributeValue(ss_name,attr_name,value,id,target_time,start_time,end_time);
}

/////////////////////
// Private methods //
/////////////////////

/// \bug return value is undefined after SS->SetAttributeValue_stored(...) call
int BaseDetectorStorage::GetAttributeValue(std::string ss_name, std::string attr_name, void * value, int id, coral::TimeStamp target_time,coral::TimeStamp * start_time,coral::TimeStamp * end_time) {
  int SSID = -1;
  SSID=GetSSIDbyNum(ss_name, id);

  if (SSID != -1)
    {
      BaseSubSystemStorage *SS = GetBaseSubSystemStorage(SSID, true);  
      
      //Quick and dirty hack !
      if(start_time == NULL) { 
	coral::TimeStamp ts = coral::TimeStamp();
	start_time = &ts;
	
      }
      
      if(end_time == NULL) { 
	coral::TimeStamp ts = coral::TimeStamp();
	end_time = &ts;
      }  
      
      int ret = SS->GetAttributeValue_cached(value, attr_name,target_time,*start_time,*end_time);

      delete SS;
      return ret;
    }
  return -1;	
}
