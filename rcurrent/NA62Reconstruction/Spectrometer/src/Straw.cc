#include "Straw.hh"

/// \class Straw
/// \Brief
/// Straw tube class.
/// \EndBrief
///
/// \Detailed
/// This class stores and provides the information about the geometry and position of each straw.
/// \EndDetailed

Straw::Straw() :fLocalPosition(), fPosition(), fAxis() // default constructor (zero vectors)
{}

Straw::Straw(TVector3 LocalPosition, TVector3 Position, TVector3 Axis)
{
  fLocalPosition = LocalPosition;
  fPosition = Position;
  fAxis = Axis;
}
