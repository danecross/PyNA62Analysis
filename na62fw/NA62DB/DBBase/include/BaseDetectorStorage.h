// --------------------------------------------------------------
// History:
//           
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) 2013-12-06
// --------------------------------------------------------------

#ifndef BaseDetectorStorage_H
#define BaseDetectorStorage_H 1
#include <string>
#include <vector> 
#include "RelationalAccess/ISessionProxy.h"
#include "RelationalAccess/ISchema.h"
#include "RelationalAccess/IConnectionService.h"

#include "RelationalAccess/AccessMode.h"
#include "CoralCommon/Utilities.h"

#include "BaseSubSystemTypeManager.h"
#include "BaseSubSystemStorage.h"
const coral::TimeStamp etime(2038,1,1,0,0,0,0);

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/call_traits.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/composite_key.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/next_prior.hpp>
#include <boost/tokenizer.hpp>
#include <boost/serialization/vector.hpp>


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

#include "BaseSubSystemTypeManager.h"

#ifdef CORAL_SEALED

#include "SealKernel/Context.h"
#include "SealKernel/ComponentLoader.h"
#include "SealKernel/IMessageService.h"
#include "PluginManager/PluginManager.h"

#else

#include "CoralKernel/Context.h"

#endif

#include "RelationalAccess/AccessMode.h"


#include "BaseCache.h"


class BaseSubSystemTypeManager;

class BaseDetectorStorage
{
public:

  //constructor
  BaseDetectorStorage(coral::ISessionProxy *m_sess, coral::ISessionProxy *cache_session, std::string DetName, std::string UsrName);
  BaseDetectorStorage(std::string connString, coral::ISessionProxy *cache_session,  std::string DetName, std::string UsrName, coral::AccessMode oracleAccessMode);
  BaseDetectorStorage(std::string connString, std::string DetName, std::string UsrName,  coral::AccessMode oracleAccessMode);
  
  int DeployTmpTable();
  int SetAutoIncrementValues();

  
  int BuildCache(const coral::TimeStamp &startTime, const coral::TimeStamp &finishTime, const std::vector<std::string> &tagged);
  
  
  int DeployDefaultSchema(bool removeifexist);
  
  int AddSubSystemType(std::string SubSystemTypeName);
  int AddSubSystemType_stored(std::string SubSystemTypeName);
  
  int GetSubSystemTypesCount();
  int GetSubSystemTypesCount_stored();
  
//  int GetSubSystemTypeName(int num, std::string &DetName);
  int AddNewDataType(std::string DataTypeName,std::string StorageDataType, std::string Units, std::string Description);
  BaseSubSystemTypeManager *GetBaseSubSystemTypeManager(std::string SSName);
  BaseSubSystemStorage *GetBaseSubSystemStorage(int SSID, bool StartWithCache);

/// \brief 
/// Direct method to register a new subSystem
/// \param SubSystemTypeName
/// \return subsystem ID 

  int RegisterNewSubSystem(std::string SubSystemTypeName);

/// \brief
/// Protected method to register a new subSystem with a given name
/// \param SubSystemTypeName
/// \param Name of the instance of this specific subsystemtype
/// \return subsystem ID or -1 if subsystem with the wanted name is already present in the db

  int RegisterNewSubSystem(std::string SubSystemTypeName, std::string IstanceName);

  int AddSubSystem(std::string SubSystemTypeName);
//  int AddSubSystem_stored(std::string SubSystemTypeName);
  int GetSubSystemsCount(std::string SubSystemTypeName);
  int GetSSIDbyNum(std::string SubSystemTypeName, int SubSystemNum);
  int GetSubSystemsCount_stored(std::string SubSystemTypeName);


/// Common attribute value methods
/// \brief Store an attribute value in DB
/// \param ss_name name of the subsystem
/// \param attr_name name of the attribute
/// \param value pointer to appropriate buffer
/// \param size size of the buffer
/// \param id ID of the subsystem (0 by default)
/// \return 0 on success

  int SetAttributeValue(std::string ss_name, std::string attr_name, int & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, int64_t & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, long long & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, float & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, double & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);

  int SetAttributeValue(std::string ss_name, std::string attr_name, std::vector< int > & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, std::vector< float > & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, std::vector< long long > & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, std::vector< double > & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, std::vector<int64_t> & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);

  int SetAttributeValue(std::string ss_name, std::string attr_name, std::string & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, std::vector<std::string> & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);
  int SetAttributeValue(std::string ss_name, std::string attr_name, std::vector< std::vector<double> > & value, int id, coral::TimeStamp start_time=coral::TimeStamp().now(), coral::TimeStamp end_time=etime);

// Common GetAttributeValue methods

  int GetAttributeValue(std::string ss_name, std::string attr_name, int & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, int64_t & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, long long & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, float & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, double & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);

  int GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<int> & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<float> & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<long long> & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<double> & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<int64_t> & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, std::vector< std::vector<double> > & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
  int GetAttributeValue(std::string ss_name, std::string attr_name, std::string & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);

  int GetAttributeValue(std::string ss_name, std::string attr_name, std::vector<std::string> & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);

  
  //destructor
  virtual ~BaseDetectorStorage();

protected:

  coral::ISessionProxy *m_session;
  coral::Context* context;
  std::string DetectorName;
  std::string UserName;
  bool release;
  coral::IHandle<coral::IConnectionService> lookSvcH;
  BaseCache* currentCache;
  std::string cachefilename;
  
private:

  int SetAttributeValue(std::string ss_name, std::string attr_name, void * value, short size, int id, coral::TimeStamp start_time, coral::TimeStamp end_time);

  template<typename T>
    int SerializeAttributeValue(std::string ss_name, std::string attr_name, 
				T & value, int id,
				coral::TimeStamp start_time,
				coral::TimeStamp end_time);


  int GetAttributeValue(std::string ss_name, std::string attr_name, void * value, int id,coral::TimeStamp target_time , coral::TimeStamp * start_time , coral::TimeStamp * end_time);	
		
  template<typename T>
  int DeserializeAttributeValue(std::string &ss_name, std::string &attr_name, 
				  T & value, int id,
				  coral::TimeStamp target_time,
				  coral::TimeStamp *start_time,
				  coral::TimeStamp *end_time);


 //   coral::SealSwitcher sw;

    struct SSTNSSNcontainer
    {
        std::string SubSystemTypeName_;
        int SubSystemNum_;
        int SSID_;
        SSTNSSNcontainer(std::string SubSystemTypeName, int SubSystemNum, int SSID):SubSystemTypeName_(SubSystemTypeName),SubSystemNum_(SubSystemNum), SSID_(SSID) {}
    };

    typedef boost::multi_index_container<
            SSTNSSNcontainer,
            boost::multi_index::indexed_by<
            boost::multi_index::ordered_unique<
            boost::multi_index::composite_key<SSTNSSNcontainer,
            boost::multi_index::member<SSTNSSNcontainer, std::string, &SSTNSSNcontainer::SubSystemTypeName_>,
            boost::multi_index::member<SSTNSSNcontainer, int, &SSTNSSNcontainer::SubSystemNum_>,
            boost::multi_index::member<SSTNSSNcontainer, int, &SSTNSSNcontainer::SSID_>
            >
           >
          >
        > SSTNSSN_set;
        SSTNSSN_set SSTNSSN_set_;


};

template<typename T>
int BaseDetectorStorage::SerializeAttributeValue(std::string ss_name, std::string attr_name, 
												T &value, int id,
												coral::TimeStamp start_time,
												coral::TimeStamp end_time) {
	int SSID = -1;
	SSID = GetSSIDbyNum(ss_name, id);
	
	if (SSID != -1)
	{
		BaseSubSystemStorage *SS = GetBaseSubSystemStorage(SSID, true);  
		*(SS->obuffer) << value;
   		int ret = SS->FlushBufferWithAttributeValue(start_time, 
				    end_time, 
			    attr_name);
		delete SS;
   		return ret;
  }
  return -1;
}

template<typename T>
int BaseDetectorStorage::DeserializeAttributeValue(std::string &ss_name, std::string &attr_name, 
						  T & value, int id,
						  coral::TimeStamp target_time,
						  coral::TimeStamp *start_time,
						  coral::TimeStamp *end_time) {
  
  int SSID = -1;

  SSID = GetSSIDbyNum(ss_name, id);
  if (SSID != -1)
    {
      std::cout << " In the deserialize SubSystemTypeName " << ss_name << " SSID = " << SSID << std::endl;
      BaseSubSystemStorage *SS = GetBaseSubSystemStorage(SSID, false); // was true);  // was false

      //Quick and dirty hack !
      if(start_time == NULL) { 
	coral::TimeStamp ts = coral::TimeStamp();
	start_time = &ts;
      }
      if(end_time == NULL) { 
	coral::TimeStamp ts = coral::TimeStamp();
	end_time = &ts;
      }  
      int ret = SS->PutAttributeValueToBuffer_cached(attr_name, target_time,
						     *start_time, *end_time);
		
      std::cout << " In the deserialize SubSystemTypeName " << ss_name << " returncode of the put action = " << ret << std::endl;

      if (ret == -1) return -1;
		
      *(SS->ibuffer) >> value;
      delete SS;
      return ret;
  }
  return -1;
}



#endif
