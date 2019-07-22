#ifndef NA62VNamedModule_H
#define NA62VNamedModule_H 1

#include "TString.h"

class NA62VNamedModule
{

public:
  
  explicit NA62VNamedModule(TString);
  ~NA62VNamedModule(){};
  NA62VNamedModule * Find(TString);

public:

  TString              GetName()                                         { return fName;                         };
  void                 SetName(TString value)                            { fName = value;                        };
 
protected:

  TString fName;

  typedef std::vector<NA62VNamedModule*> Library;
  Library fLibrary;
};

#endif
