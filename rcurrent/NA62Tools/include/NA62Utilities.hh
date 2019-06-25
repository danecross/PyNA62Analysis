// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-12-27
//
// ---------------------------------------------------------------

#ifndef NA62Utilities_H
#define NA62Utilities_H 1

#include "TString.h"
#include <map>

class NA62Utilities {

  public:

    static NA62Utilities* GetInstance();

    Double_t GetUnitFromString(TString);

  private:

    NA62Utilities();
    ~NA62Utilities();

    static NA62Utilities* fInstance;

    std::map<TString,Double_t> fMapStringUnit;

};

#endif
