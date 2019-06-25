// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef DetectorParameter_H
#define DetectorParameter_H 1

#include "TString.h"
#include "TObject.h"
#include "TObjArray.h"

class DetectorParameter : public TObject {

public:

  DetectorParameter(){};
  DetectorParameter(const char*,const char*,const char*,TObjArray);
  virtual ~DetectorParameter() {}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;

public:

//   TString              GetName()                                          { return fName;                         };
//   void                 SetName(TString value)                             { fName = value;                        };
  TString              GetValue()                                         { return fValue;                        };
  void                 SetValue(TString value)                            { fValue = value;                       };
  TString              GetDescription()                                   { return fDescription;                  };
  void                 SetDescription(TString value)                      { fDescription = value;                 };

  TObjArray *          GetData()                                          { return &fData;                        };
  void                 SetData(TObjArray value)                           { fData = value;                        };

private:

  // N.B. spacial dimensions are always in mm
  TString fName;
  TString fValue;
  TString fDescription;

  TObjArray fData;

  ClassDef(DetectorParameter,1)
};

#endif
