// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-30
//
// --------------------------------------------------------------
#ifndef TPrimSpecialTrigger_H
#define TPrimSpecialTrigger_H
#include "TObject.h"
#include "TSpecialTrigger.hh"
#include "TString.h"

class PrimRegister : public TObject{

  public:

    PrimRegister() { Clear(); }
    PrimRegister(const PrimRegister &c) : TObject(c), fLabel(c.fLabel), fValue(c.fValue) { }
    PrimRegister(TString Label, UInt_t Value);
    ~PrimRegister() {}
    void Clear(Option_t* = "");
    TString GetLabel()          { return fLabel;  }
    UInt_t GetValue()           { return fValue;  }
    void SetValue(UInt_t Value) { fValue = Value; }

  private:

    TString fLabel;
    UInt_t  fValue;
    ClassDef(PrimRegister,1);
};

class PrimCounter : public TObject{

  public:

    PrimCounter(){ Clear(); };
    PrimCounter(const PrimCounter&);
    PrimCounter(TString Label, Int_t ChannelID, UInt_t Value);
    ~PrimCounter(){};
    void Clear(Option_t* = "");
    TString GetLabel()                   { return fLabel;                }
    std::vector<Int_t> GetChannelIDs()   { return fChannelIDs;           }
    std::vector<UInt_t> GetValues()      { return fValues;               }
    Int_t GetChannelID(Int_t iCounter)   { return fChannelIDs[iCounter]; }
    UInt_t GetValue(UInt_t iCounter)     { return fValues[iCounter];     }
    UInt_t GetNEntries() { return fValues.size(); }
    void AddEntry(Int_t ChannelID, UInt_t Value);

  private:

    TString fLabel;
    std::vector<Int_t>  fChannelIDs;
    std::vector<UInt_t> fValues;
    ClassDef(PrimCounter,1);
};


class TPrimSpecialTrigger : public TSpecialTrigger {

  public:

    TPrimSpecialTrigger();
    ~TPrimSpecialTrigger(){};
    void Clear(Option_t* = "");

    std::vector<PrimRegister> GetRegisters()    { return fRegisters;            };
    std::vector<PrimCounter>  GetCounters()     { return fCounters;             };
    PrimRegister* GetRegister(UInt_t iRegister) { if(iRegister<fRegisters.size()) return &(*(fRegisters.begin()+iRegister)); else return 0; };
    PrimCounter*  GetCounter(UInt_t iCounter)   { if(iCounter<fCounters.size()) return &(*(fCounters.begin()+iCounter)); else return 0;     };
    PrimRegister* GetRegister(TString Label);
    PrimCounter*  GetCounter(TString Label);
    Int_t FindRegister(TString Label);
    Int_t FindCounter(TString Label);
    void AddRegister(TString Label, UInt_t Value);
    void AddCounter(TString Label, Int_t ChannelID, UInt_t Value);

  private:

    std::vector<PrimRegister> fRegisters;
    std::vector<PrimCounter>  fCounters;

    ClassDef(TPrimSpecialTrigger,1);
};
#endif
