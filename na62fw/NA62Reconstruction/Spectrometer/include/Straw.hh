#ifndef Straw_H
#define Straw_H 1

#include <RtypesCore.h>             // ROOT data types, Double_t ...

/// \class Straw
/// \Brief
/// Straw tube position class.
/// \EndBrief
///
/// \Detailed
/// This class serves as a storage of the straw position information.
/// \EndDetailed


class Straw
{
public:
  void SetGlobalID(Int_t i)           {fGlobalID = i;}
  void SetLocalXPosition(Double_t p)  {fLocalXPosition = p;}
  void SetLocalZPosition(Double_t p)  {fLocalZPosition = p;}
  void SetGlobalZPosition(Double_t p) {fGlobalZPosition = p;}

  Int_t GetGlobalID()           const {return fGlobalID;}
  Double_t GetLocalXPosition()  const {return fLocalXPosition;}
  Double_t GetLocalZPosition()  const {return fLocalZPosition;}
  Double_t GetGlobalZPosition() const {return fGlobalZPosition;}

private:
  Int_t fGlobalID = -1; ///< Global ID of the straw, given by position in the plane and global plane ID
  Double_t fLocalXPosition = -99999.;  ///< Local straw x-position in the view.
  Double_t fLocalZPosition = -99999.;  ///< Local straw z-position in the chamber.
  Double_t fGlobalZPosition = -99999.; ///< Global straw z-position in the laboratory reference frame.
};
#endif
