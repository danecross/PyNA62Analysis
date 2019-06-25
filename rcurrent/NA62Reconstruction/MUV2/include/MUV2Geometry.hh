#ifndef MUV2Geometry_H
#define MUV2Geometry_H 1

#include "Rtypes.h"

class MUV2Geometry
{
    
public:
    
    MUV2Geometry();
    static MUV2Geometry* GetInstance();
    Double_t  GetScintLengthStandard()                    { return fScintLengthStandard;      };
    void      SetScintLengthStandard(Double_t value)      { fScintLengthStandard = value;     };
    
    Double_t GetToF () { return fTimeOfFlight; }
    Double_t      GetScintillatorPosition (Int_t ScintillatorNumber) { return fScintillatorPosition[ScintillatorNumber]; };
    void        SetScintillatorPosition (Int_t ScintillatorNumber, Double_t Position) { fScintillatorPosition[ScintillatorNumber] = Position; };
    
    Int_t GetScintillatorAt (Double_t X);
    
private:
    
    static MUV2Geometry* fInstance;
    Double_t fScintLengthStandard;
    
private:
    
    void CreateGeometry();
    
public:
    
    
private:
    
    Double_t fScintillatorPosition[23];
    Double_t fTimeOfFlight;
    
};
#endif
