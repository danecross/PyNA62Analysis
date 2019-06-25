#ifndef Algorithm_HH
#define Algorithm_HH

#include "UserMethods.hh"
#include "Analyzer.hh"
#include "BaseAnalysis.hh"
//TODO remove these 2 lines
//using namespace NA62Analysis;
//using namespace Core;

namespace NA62Analysis {
  class Algorithm: public UserMethods {
    public:
    Algorithm(BaseAnalysis *ba, Analyzer* ana, const std::string &name);
    virtual ~Algorithm() {};
    virtual void SaveAllPlots();

    template<class T>
    void AddParam(TString name, T* address, T defaultValue) {
      TString name2 = fAlgoName+"."+name;
      fCallingAna->AddParam( name2, address,  defaultValue);
    }

    private:    
    std::string fAlgoName;
    std::string fCallingAnaName;
    Analyzer* fCallingAna;

    protected:
    };
} /* namespace NA62Analysis */

#endif//~Algorithm_HH
