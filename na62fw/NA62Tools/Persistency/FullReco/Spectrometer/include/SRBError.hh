#ifndef SRBError_H
#define SRBError_H
#include "TDigiVError.hh"

class SRBError : public TDigiVError {

  public:

    SRBError();
    ~SRBError(){};

    void Clear(Option_t* = "");

  public:

    Int_t  GetStrawAddr() {return fStrawAddr;};
    void   SetStrawAddr(Int_t value) {fStrawAddr = value;};
    Int_t  GetSRBAddr() { return GetROBoardID();};
    void   SetSRBAddr(Int_t value){SetROBoardID(value);};
    Int_t  GetFlag() {return fFlag;};
    void   SetFlag(Int_t value) {fFlag=value;};

  private:

    Int_t fStrawAddr;
    Int_t fFlag;

    ClassDef(SRBError,1);
};
#endif
