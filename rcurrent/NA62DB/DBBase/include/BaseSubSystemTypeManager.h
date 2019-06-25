// --------------------------------------------------------------
// History:
//           
// Created by Sergey Podolsky (siarhei.padolski@cern.ch) 2013-12-06
// --------------------------------------------------------------

#ifndef BaseSubSystemTypeManager_H
#define BaseSubSystemTypeManager_H 1
#include <string>
#include "RelationalAccess/ISessionProxy.h"
#include "RelationalAccess/ISchema.h"
#include "RelationalAccess/IConnectionService.h"

#include "RelationalAccess/AccessMode.h"
#include "CoralCommon/Utilities.h"


class BaseSubSystemTypeManager
{
public:

  //constructor
  BaseSubSystemTypeManager(coral::ISessionProxy *m_sess, std::string DetName, std::string UserName, std::string SSName);

  //destructor
  virtual ~BaseSubSystemTypeManager();
  int AddAttribute(std::string AttrName, std::string DataTypeName, bool makevalid);
  int GetAttributiesCount();
  int GetAttributiesCount(time_t time);
  int StopAttributeValidityNow(int AttributeNum);
  int GetAttributeName(int AttributeNum, std::string &AttributeName);
  int CreateNewTag(std::string TagName);
  int PrintAvaliableTags();
  
  int GetTagsCount();
  int TagAttribute(std::string TagName, int AttributeID);
  int RemoveTagName(std::string TagName);




private:

  coral::ISessionProxy *m_session;
  std::string DetectorName;
  std::string SubSystemName;
};




#endif
