// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-02-12
//
// ---------------------------------------------------------------

#ifndef BeamData_H
#define BeamData_H 1
#include "TObject.h"

class BeamData : public TObject {

  public:

    BeamData();
    BeamData(const BeamData&);
    ~BeamData();
    void Clear(Option_t* = "");

    void AddInstantaneousIntensity(Double_t val)      { fInstantaneousIntensity.push_back(val);      }
    void AddInstantaneousIntensityError(Double_t val) { fInstantaneousIntensityError.push_back(val); }
    Double_t GetInstantaneousIntensity()       { if(fInstantaneousIntensity.size()>0) return fInstantaneousIntensity[0];
                                                 else return 0.;   }
    Double_t GetInstantaneousIntensityError()  { if(fInstantaneousIntensityError.size()>0) return fInstantaneousIntensityError[0];
                                                 else return 0.;   }

    void  PrintInfo();

  private:

    std::vector<Double_t> fInstantaneousIntensity;       ///< Evaluated by NA62Reco using GigaTracker
    std::vector<Double_t> fInstantaneousIntensityError;  ///< Evaluated by NA62Reco using GigaTracker

    ClassDef(BeamData,1);
};

#endif
