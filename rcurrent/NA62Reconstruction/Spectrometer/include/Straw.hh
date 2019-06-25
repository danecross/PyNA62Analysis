#ifndef Straw_H
#define Straw_H 1

#include "TVector3.h"

class Straw
{

public:
  Straw();
  Straw(TVector3, TVector3, TVector3);

public:

  TVector3  GetLocalPosition()                { return fLocalPosition; };
  void      SetLocalPosition(TVector3 value)  { fLocalPosition = value;};
  TVector3  GetPosition()                     { return fPosition;      };
  void      SetPosition(TVector3 value)       { fPosition = value;     };
  TVector3  GetAxis()                         { return fAxis;          };
  void      SetAxis(TVector3 value)           { fAxis = value;         };

private:

  TVector3 fLocalPosition; ///< Straw position in the view reference frame.
  TVector3 fPosition; ///< Straw position in the laboratory reference frame.
  TVector3 fAxis; ///< Orientation of the straw in the laboratory reference frame.

};
#endif
