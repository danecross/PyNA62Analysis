#include "G4Polycone.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "G4Region.hh"

#include "RICHRadiator.hh"




RICHRadiator::RICHRadiator(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

RICHRadiator::~RICHRadiator(){}

void RICHRadiator::ReadGeometryParameters()
{

  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();

  fNSections = GeoPars->GetNVesselSections();
  fInnerRadius = GeoPars->GetRadiatorInnerRadius();
  fZLength = GeoPars->GetVesselZLength();
  fZPosition = GeoPars->GetVesselZPosition();

  fOuterRadii = GeoPars->GetVesselSectionInnerRadius();
  fZLengths = GeoPars->GetVesselSectionZLength();

  fDownstreamFlangeZLengths = GeoPars->GetVesselSectionDownstreamFlangeZLength();
  fDownstreamFlangeThickness = GeoPars->GetVesselSectionDownstreamFlangeRadialThickness();
  fDownstreamFlangeInnerRadii = GeoPars->GetVesselSectionDownstreamFlangeInnerRadius(); 
  fDownstreamFlangeZPosition = GeoPars->GetVesselSectionDownstreamFlangeZPosition();
  
  //G4cout<<"#### Radiator Z position in RICH reference system: "<<fZPosition<<" ####"<<G4endl;
}

void RICHRadiator::CreateGeometry()
{

  G4double HalfZLength = 0.5*fZLength;
  G4double startPhiAngle = 0;
  G4double deltaPhiAngle = 360*deg;

  G4double MinRadii[10], MaxRadii[10], Z[10], z=-HalfZLength;
  G4int nZ =0;

  for(G4int iSection = 0; iSection < fNSections; iSection++){
      Z[nZ] = z;
      MinRadii[nZ] = fInnerRadius;
      MaxRadii[nZ] = fOuterRadii[iSection];
      // to be consistent with RICHVessel
      z += fZLengths[iSection]-0.1*mm;
      //G4cout<<" Z_1: "<<Z[nZ] <<" ***********"<<G4endl;
      //G4cout<<" R_1_min: "<<MinRadii[nZ] <<" R_1_max: "<<MaxRadii[nZ]<<G4endl;
      nZ++;
      Z[nZ] = z;
      MinRadii[nZ] = fInnerRadius;
      MaxRadii[nZ] = fOuterRadii[iSection];                       
      //G4cout<<" Z_2: "<<Z[nZ] <<" ***********"<<G4endl;
      //G4cout<<" R_2_min: "<<MinRadii[nZ] <<" R_2_max: "<<MaxRadii[nZ]<<G4endl;
      nZ++;
  }

  fSolidVolume= new G4Polycone("RICHRadiator",
                          startPhiAngle,
                          deltaPhiAngle,
                          nZ,
                          Z,
                          MinRadii,
                          MaxRadii);

  fSolidDownstreamFlange = new G4Tubs*[fNSections];

  G4VSolid** RadiatorDownFlSubtracted;
  RadiatorDownFlSubtracted = new G4VSolid*[fNSections];

  for(G4int iSection = 0; iSection < fNSections; iSection++){

    fSolidDownstreamFlange[iSection] = new G4Tubs("RICHVesselSectionDownstreamFlange",
                                                 fDownstreamFlangeInnerRadii[iSection],       
                                                 fDownstreamFlangeInnerRadii[iSection] + fDownstreamFlangeThickness[iSection],
                                                 0.5*fDownstreamFlangeZLengths[iSection]+0.5*mm,
                                                 startPhiAngle,
                                                 deltaPhiAngle); 

    if(iSection==0){

         RadiatorDownFlSubtracted[iSection] = new G4SubtractionSolid("RadDownFlSub",
                                                                    new G4DisplacedSolid("DisplacedRadiator",
                                                                                         fSolidVolume,
                                                                                         0,
//                                                                                         G4ThreeVector(0,0,fZPosition)), 
                                                                                         G4ThreeVector(0,0,0)),
                                                                    new G4DisplacedSolid("DisplacedDownstreamFlange",
                                                                                         fSolidDownstreamFlange[iSection],
                                                                                         0,
//                                                                                         G4ThreeVector(0,0,fDownstreamFlangeZPosition[iSection])
                                                                                         G4ThreeVector(0,0,fDownstreamFlangeZPosition[iSection]-fZPosition)
                                                                                        )
                                                                    );

    }else{
 
         RadiatorDownFlSubtracted[iSection] = new G4SubtractionSolid("RadDownFlSub",
                                                                    new G4DisplacedSolid("DisplacedRadiator",
                                                                                         RadiatorDownFlSubtracted[iSection-1],
                                                                                         0,
                                                                                         G4ThreeVector(0,0,0)),
                                                                    new G4DisplacedSolid("DisplacedDownstreamFlange",
                                                                                         fSolidDownstreamFlange[iSection],
                                                                                         0,
//                                                                                         G4ThreeVector(0,0,fDownstreamFlangeZPosition[iSection]) 
                                                                                         G4ThreeVector(0,0,fDownstreamFlangeZPosition[iSection]-fZPosition)
                                                                                        )
                                                                    );


    }

  }


 fLogicalVolume = new G4LogicalVolume(RadiatorDownFlSubtracted[3],        // solid
                                      fMaterial,           // material
                                      "RICHRadiator",      // name
                                      0,                   // field manager
                                      0,                   // sensitive detector
                                      0);                  // user limits

 fPhysicalVolume = new G4PVPlacement(0,                         // no rotation
                                     G4ThreeVector(0,0,0),           // at pos
                                     fLogicalVolume,            // its logical volume  
                                     "RICHRadiator",            // its name
                                     fMotherVolume,             // its mother  volume 
                                     false,                     // no boolean operations
                                     0);                                  // copy number

}

void RICHRadiator::SetProperties()
{

  fVisAtt = new G4VisAttributes(G4Colour(0.5,0.5,0.5));
  fLogicalVolume -> SetVisAttributes(fVisAtt);

}

