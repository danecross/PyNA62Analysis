#include "G4Polycone.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "G4UnionSolid.hh"
#include "G4DisplacedSolid.hh"

#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "RICHVessel.hh"
#include "RICHMaterialParameters.hh"

RICHVessel::RICHVessel(G4Material * Material, G4LogicalVolume * MotherVolume) : 
  NA62VComponent(Material,MotherVolume),
  fZPosition(0.),
  fNSections(0),
  fZLength(0.),
  fInnerRadius(0.),
  fThickness(0.)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
  DefineOpticalSurface();
}

RICHVessel::~RICHVessel(){}

void RICHVessel::ReadGeometryParameters()
{
 RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance(); 

// Vessel parameters

 fNSections = GeoPars->GetNVesselSections();
 fInnerRadius = GeoPars->GetRadiatorInnerRadius();
 fThickness = GeoPars->GetVesselRadialThickness();
 fZLength = GeoPars->GetVesselZLength();
 //G4cout<<" *********** VesselZLength: "<<fZLength<<" ***********"<<G4endl; 

 fZPosition = GeoPars->GetVesselZPosition();
 //G4cout<<" *********** VesselZPosition: "<<fZPosition<<" ***********"<<G4endl; 

 fInnerRadii = GeoPars->GetVesselSectionInnerRadius();
 fZLengths = GeoPars->GetVesselSectionZLength();

// Vessel flanges parameters

  fPressureFlangeZLengths = GeoPars->GetVesselSectionPressureFlangeZLength();
  fPressureFlangeThickness = GeoPars->GetVesselSectionPressureFlangeRadialThickness();
  fPressureFlangeInnerRadii = GeoPars->GetVesselSectionPressureFlangeInnerRadius();
  fPressureFlangeZShift = GeoPars->GetVesselSectionPressureFlangeZShift();  	
  fPressureFlangeZPosition = GeoPars->GetVesselSectionPressureFlangeZPosition();       

  fUpstreamFlangeZLengths = GeoPars->GetVesselSectionUpstreamFlangeZLength();
  fUpstreamFlangeThickness = GeoPars->GetVesselSectionUpstreamFlangeRadialThickness();
  fUpstreamFlangeInnerRadii = GeoPars->GetVesselSectionUpstreamFlangeInnerRadius();
  fUpstreamFlangeZShift = GeoPars->GetVesselSectionUpstreamFlangeZShift();  	
  fUpstreamFlangeZPosition = GeoPars->GetVesselSectionUpstreamFlangeZPosition();        

  fDownstreamFlangeZLengths = GeoPars->GetVesselSectionDownstreamFlangeZLength();
  fDownstreamFlangeThickness = GeoPars->GetVesselSectionDownstreamFlangeRadialThickness();
  fDownstreamFlangeInnerRadii = GeoPars->GetVesselSectionDownstreamFlangeInnerRadius();   
  fDownstreamFlangeZPosition = GeoPars->GetVesselSectionDownstreamFlangeZPosition();

}

 void RICHVessel::CreateGeometry()
 {

  G4double HalfZLength = 0.5*fZLength;
  G4double startPhiAngle=0;
  G4double deltaPhiAngle=360*deg;
  G4double MinRadii[10], MaxRadii[10], Z[10], z = -HalfZLength;
  G4int nZ = 0;

  for(G4int iSection = 0; iSection < fNSections; iSection++){
      //G4cout<<" *********** sezione: "<<iSection<<" ***********"<<G4endl;
      Z[nZ] = z;
      MinRadii[nZ] = fInnerRadius;
      MaxRadii[nZ] = fInnerRadii[iSection] + fThickness;
      z += fZLengths[iSection]-0.1*mm;
      //G4cout<<" Z_1: "<<Z[nZ] <<" ***********"<<G4endl;
      //G4cout<<" R_1_min: "<<MinRadii[nZ] <<" R_1_max: "<<MaxRadii[nZ]<<G4endl;
      nZ++;
      Z[nZ] = z;
      MinRadii[nZ] = fInnerRadius;
      MaxRadii[nZ] = fInnerRadii[iSection] + fThickness;
      //G4cout<<" Z_2: "<<Z[nZ] <<" ***********"<<G4endl;
      //G4cout<<" R_2_min: "<<MinRadii[nZ] <<" R_2_max: "<<MaxRadii[nZ]<<G4endl;
      nZ++;
  }

  //G4cout<<" *********** nZ: "<<nZ<<" ***********"<<G4endl;

  fSolidVolume= new G4Polycone("RICHVessel",
                           startPhiAngle,
                           deltaPhiAngle,
                           nZ,
                           Z,
                           MinRadii,
                           MaxRadii);


  fSolidUpstreamFlange = new G4Tubs*[fNSections];
  fSolidPressureFlange = new G4Tubs*[fNSections];
  fSolidDownstreamFlange = new G4Tubs*[fNSections];

  G4VSolid** VesselUpFl;
  VesselUpFl = new G4VSolid*[fNSections];

  G4VSolid** VesselUpPrFl;
  VesselUpPrFl = new G4VSolid*[fNSections];

  G4VSolid** VesselUpPrDownFl;
  VesselUpPrDownFl = new G4VSolid*[fNSections];


  for(G4int iSection = 0; iSection < fNSections; iSection++){

     fSolidUpstreamFlange[iSection] = new G4Tubs("RICHVesselSectionUpstreamFlange",
                                                 fUpstreamFlangeInnerRadii[iSection],	
                                                 fUpstreamFlangeInnerRadii[iSection] + fUpstreamFlangeThickness[iSection],
                                                 0.5*fUpstreamFlangeZLengths[iSection],
                                                 startPhiAngle,
                                                 deltaPhiAngle);     

    if(iSection==0){
      VesselUpFl[iSection] = new G4UnionSolid("VesselUpFl",
					      fSolidVolume,
					      fSolidUpstreamFlange[iSection],
					      0,
					      G4ThreeVector(0,0,fUpstreamFlangeZPosition[iSection]-fZPosition)
					      );
    }else{
      VesselUpFl[iSection] = new G4UnionSolid("VesselUpFl",
					      VesselUpFl[iSection-1],
					      fSolidUpstreamFlange[iSection],
					      0,
					      G4ThreeVector(0,0,fUpstreamFlangeZPosition[iSection]-fZPosition)
					      );
   }	 

  //G4cout<<" *********** sezione: "<<iSection<<" ***********"<<G4endl;
  //G4cout<<"UpFlInnerRadius: "<<fUpstreamFlangeInnerRadii[iSection]<<"  UpFlOuterRadius: "<<fUpstreamFlangeInnerRadii[iSection] + fUpstreamFlangeThickness[iSection]<<"  UpFlZLength: "<<fUpstreamFlangeZLengths[iSection]<<" UpFlZPosition: "<<fUpstreamFlangeZPosition[iSection]<<G4endl;
 
  }

 for(G4int iSection = 0; iSection < fNSections; iSection++){ 

  //G4cout<<" *********** sezione: "<<iSection<<" ***********"<<G4endl;
  //G4cout<<"PrFlInnerRadius: "<<fPressureFlangeInnerRadii[iSection]<<"  PrFlOuterRadius: "<<fPressureFlangeInnerRadii[iSection] + fPressureFlangeThickness[iSection]<<"  PrFlZLength: "<<fPressureFlangeZLengths[iSection]<<" PrFlZPosition: "<<fPressureFlangeZPosition[iSection]<<G4endl;

      if(iSection == 0 ){

         VesselUpPrFl[iSection] = VesselUpFl[3];

      }else{

         fSolidPressureFlange[iSection] = new G4Tubs("RICHVesselSectionPressureFlange",
                                                 fPressureFlangeInnerRadii[iSection],
                                                 fPressureFlangeInnerRadii[iSection] + fPressureFlangeThickness[iSection],
                                                 0.5*fPressureFlangeZLengths[iSection],
                                                 startPhiAngle,
                                                 deltaPhiAngle);                                                              


         VesselUpPrFl[iSection] = new G4UnionSolid("VesselUpPrFl",
						   VesselUpPrFl[iSection-1],
						   fSolidPressureFlange[iSection],
						   0,
						   G4ThreeVector(0,0,fPressureFlangeZPosition[iSection]-fZPosition)
						   );                                                                          
     }
  }


  for(G4int iSection = 0; iSection < fNSections; iSection++){

     fSolidDownstreamFlange[iSection] = new G4Tubs("RICHVesselSectionDownstreamFlange",
                                                   fDownstreamFlangeInnerRadii[iSection],
                                                   fDownstreamFlangeInnerRadii[iSection] + fDownstreamFlangeThickness[iSection],
                                                   0.5*fDownstreamFlangeZLengths[iSection],
                                                   startPhiAngle,
                                                   deltaPhiAngle);

    if(iSection==0){
      VesselUpPrDownFl[iSection] = new G4UnionSolid("VesselUpPrDownFl",
						    VesselUpPrFl[3],
						    fSolidDownstreamFlange[iSection],
						    0,
						    G4ThreeVector(0,0,fDownstreamFlangeZPosition[iSection]-fZPosition)
						    );
      
    }else{
      VesselUpPrDownFl[iSection] = new G4UnionSolid("VesselUpPrDownFl",
						    VesselUpPrDownFl[iSection-1],
						    fSolidDownstreamFlange[iSection],
						    0,
						    G4ThreeVector(0,0,fDownstreamFlangeZPosition[iSection]-fZPosition)
						    );                                                                           
   }
    

  //G4cout<<" *********** sezione: "<<iSection<<" ***********"<<G4endl;
  //G4cout<<"DownFlInnerRadius: "<<fDownstreamFlangeInnerRadii[iSection]<<"  DownFlOuterRadius: "<<fDownstreamFlangeInnerRadii[iSection] + fDownstreamFlangeThickness[iSection]<<"  DownFlZLength: "<<fDownstreamFlangeZLengths[iSection]<<" DownFlZPosition: "<<fDownstreamFlangeZPosition[iSection]<<G4endl;
}

  G4VSolid * Vessel = NULL;
#ifdef G4VIS_USE
  Vessel = fSolidVolume;
#else
  Vessel = VesselUpPrDownFl[3];
#endif

  fLogicalVolume = new G4LogicalVolume(Vessel,        // solid
                                        fMaterial,             // material
                                        "RICHVessel",           // name
                                        0,                    // field manager
                                        0,               // sensitive detector
                                        0);                     // user limits

   fPhysicalVolume = new G4PVPlacement(0,                         // no rotation
//                                       G4ThreeVector(0,0,0),// at pos
                                       G4ThreeVector(0,0,fZPosition),// at pos
                                       fLogicalVolume,             // its logical volume
                                       "RICHVessel",                // its old name was "RICHMirrorWindow" (?)
                                       fMotherVolume,              // its mother  volume
                                       false,                     // no boolean operations
                                       0);                                  // copy number



}

 void RICHVessel::SetProperties()
 {
 
     fVisAtt= new G4VisAttributes(G4Colour(1.,0.,1.));
     fLogicalVolume ->SetVisAttributes(fVisAtt); 
}

 void RICHVessel::DefineOpticalSurface()
 {

  // RICHVessel reflective surface
  fOpticalSurface = new G4OpticalSurface("RICHVessel");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetFinish(ground);
  fOpticalSurface->SetModel(unified);
  fOpticalSurface->SetSigmaAlpha(RICHMaterialParameters::GetInstance()
                                 ->GetVesselOpticalSurfaceSigmaAlpha());

  fOpticalSurface -> SetMaterialPropertiesTable(RICHMaterialParameters::GetInstance()
                                                ->GetVesselOpticalSurfacePT());
 }
