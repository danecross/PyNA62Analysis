#ifndef LKrEnergySpot_h
#define LKrEnergySpot_h

#include "G4SystemOfUnits.hh"
#include "G4ThreeVector.hh"

class G4Colour;

class LKrEnergySpot
{
public:
  LKrEnergySpot();
  LKrEnergySpot(const G4ThreeVector& point, G4double E);
  ~LKrEnergySpot();

  inline void SetEnergy(const G4double& E) {Energy = E;}
  inline G4double GetEnergy() const {return Energy;}

  inline void SetPosition(const G4ThreeVector& point) {Point = point;}
  inline G4ThreeVector GetPosition() const {return Point;}

  G4int operator==(const LKrEnergySpot& eSpot) const
  {
    return (Energy==eSpot.Energy && Point==eSpot.Point) ? 1 : 0;
  }

  // Draw:
  void Draw(G4Colour* color = 0);
  // Print:
  void Print();


private:
  G4double Energy;
  G4ThreeVector Point;
};

#endif
