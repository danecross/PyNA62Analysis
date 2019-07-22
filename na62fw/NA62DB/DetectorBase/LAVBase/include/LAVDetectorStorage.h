// --------------------------------------------------------------
// History:
//           
//
// --------------------------------------------------------------

#ifndef LAVDetectorStorage_H
#define LAVDetectorStorage_H 1

/// \file LAVDetectorStorage.h
/// \brief Interactions with condition DB
/// \author 

#include <string>
#include <vector> 
#include <boost/serialization/vector.hpp>

#include "BaseDetectorStorage.h"


//Configuration
//MAX DATE
//const coral::TimeStamp etime(2038,1,1,0,0,0,0);

/// \class LAVDetectorStorage
/// \brief Interaction with condition DB
///
/// Some explanations
///
class LAVDetectorStorage : public BaseDetectorStorage
{
 public:
		/// \brief
		/// \param connString
		/// \param UsrName
  LAVDetectorStorage(); // default constructor: user LAV, connection string OraCERNProxy
  LAVDetectorStorage(std::string connString, std::string UsrName, coral::AccessMode oracleAccessMode);

		/// \brief Retrive an attribute value in DB
		/// \param ss_name name of the subsystem
		/// \param attr_name name of the attribute
		/// \param value pointer to appropriate buffer
		/// \param id ID of the subsystem (0 by default)
		/// \return 0 on success
        /// Here, we do not use conv_fact


// Originally, various interfaces were declared here, with different data types, such as:
//    int GetAttributeValue(std::string ss_name, std::string attr_name, int & value, int id,coral::TimeStamp target_time = coral::TimeStamp().now(), coral::TimeStamp * start_time = NULL, coral::TimeStamp * end_time = NULL);
// 
// Now, the BaseDetectorStorage class includes the GetAttributeValue methods for the following data types:
//     int, int64_t, long long, float, double, vector<int>, vector<float>, vector<long long>, vector<double>, vector<int64_t>, vector<vector<double>>, string, vector<string>
// If more types are needed, add specific methods here

  int BuildCache(); // default build-cache method  
  //  int BuildCache(coral::TimeStamp&, coral::TimeStamp&, std::vector<std::basic_string<char> >&); // default build-cache method  
  int BuildCache(const coral::TimeStamp&, const coral::TimeStamp&,const std::vector<std::string>&); // default build-cache method  

  virtual ~LAVDetectorStorage();
  		
	private:

// Originally, DeserializeAttributeValue templated function was declared at the sub-detector level. 
// Originally, the standard interface with the backend was declared here: 
//     int GetAttributeValue(std::string ss_name, std::string attr_name, void * value, int id,coral::TimeStamp target_time , coral::TimeStamp * start_time , coral::TimeStamp * end_time);	
// Now, we (T. Spadaro and E. Leonardi) moved them to the BaseDetectorStorage class.

};
/////////////////////////
// Templated functions //
/////////////////////////
// Originally, DeserializeAttributeValue templated function was implemented at the sub-detector level. 
// Now, we (T. Spadaro and E. Leonardi) moved them to the BaseDetectorStorage class.

#endif

