#include "LKrEnergySpot.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Polyline.hh"
#include "G4VVisManager.hh"
#include "G4Step.hh"


LKrEnergySpot::LKrEnergySpot() :
  Energy(0),
  Point(0)
{;}

LKrEnergySpot::LKrEnergySpot(const G4ThreeVector& point, G4double E) :
  Energy(E),
  Point(point)
{
}

LKrEnergySpot::~LKrEnergySpot()
{;}


void LKrEnergySpot::Draw(G4Colour *color)
{
  G4VVisManager* pVVisManager = G4VVisManager::GetConcreteInstance();
  if (pVVisManager)
    {
      G4Polyline polyline;
      G4Colour colour(1.,.5,.5);
      if (color != 0) colour = *color;
      polyline.SetVisAttributes(colour);
      G4ThreeVector pp(Point);
      // Draw a "home made" marker:
      // Will be better by using a real Marker:
      pp.setZ(pp.z()+1*cm);
      polyline.push_back(pp);
      pp.setZ(pp.z()-2*cm);
      polyline.push_back(pp);
      pp = Point;
      polyline.push_back(pp);
      pp.setX(pp.x()+1*cm);
      polyline.push_back(pp);
      pp.setX(pp.x()-2*cm);
      polyline.push_back(pp);
      pp = Point;
      polyline.push_back(pp);
      pp.setY(pp.y()+1*cm);
      polyline.push_back(pp);
      pp.setY(pp.y()-2*cm);
      polyline.push_back(pp);
      pVVisManager -> Draw(polyline);
    }
}

void LKrEnergySpot::Print()
{
  G4cout << " LKrEnergySpot {E = " << Energy << "; Position = " << Point << " }"<< G4endl;
}
