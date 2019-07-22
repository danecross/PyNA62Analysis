// --------------------------------------------------------------
// History:
//           
// Created by E. Leonardi and T. Spadaro (emanuele.leonardi@cern.ch tommaso.spadaro@cern.ch) 2015-03-11
// --------------------------------------------------------------

#ifndef LAVDetectorSetup_H
#define LAVDetectorSetup_H 1

/// \file LAVDetectorSetup.h
/// \brief Interactions with condition DB
/// \author E. Leonardi and T. Spadaro (emanuele.leonardi@cern.ch, tommaso.spadaro@cern.ch)

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
class LAVDetectorSetup : public BaseDetectorStorage
{
	public:
		/// \brief
		/// \param connString
		/// \param UsrName
  		LAVDetectorSetup(std::string connString, std::string UsrName, coral::AccessMode oracleAccessMode);
  		
		/// \brief ???
		/// \param removeifexist
		/// \return 0 on success
		int DeployDetectorStructureToDB();
  	       		

  		virtual ~LAVDetectorSetup();
  		
	private:

		//Interfaces with the backend have been moved to the BaseDetectorStorage class:                
		// int SetAttributeValue(std::string ss_name, std::string attr_name, void * value, short size, int id, coral::TimeStamp start_time, coral::TimeStamp end_time);		
		
		// template<typename T>
		// int SerializeAttributeValue(std::string ss_name, std::string attr_name, 
		//							T & value, int id,
		//							coral::TimeStamp start_time,
		//							coral::TimeStamp end_time);
		
};
#endif
