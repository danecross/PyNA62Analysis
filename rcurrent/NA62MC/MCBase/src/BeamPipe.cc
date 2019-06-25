/// \class BeamPipe
/// \Brief 
/// Beam Pipe class
/// \EndBrief
/// \Detailed
/// Inside each Responsibility Region more than one Beam Pipe segment can be constructed, each with 
/// different geometrical parameters.
/// At the moment, the BeamPipe is considered to be made of Aluminium.
/// \n
/// The BeamPipe constructor needs the following parameters:
/// \n
///     - segment index
///     - material in which the Beam Pipe segment will be located (Neon for RICH)
///     - the LogicalVolume mother of the Beam Pipe segment
/// \n
/// The GeometryParameters.cc of the subdetector must contain the value of the following members:
/// \n
///     - fBeamPipeZPosition[segment index]
///     - fBeamPipeZLength[segment index]
///     - fBeamPipeInnerRadius[segment index]
///     - fBeamPipeOuterRadius[segment index]
///     - fBeamPipeInputDisplacementWRTBeam[segment index]
///     - fBeamPipeOutputDisplacementWRTBeam[segment index]
///     - fBeamPipeFinZLength[segment index]
///     - fBeamPipeFinOuterRadius[segment index]
///     - fBeamPipeFineSpacing[segment index]
///     - fBeamPipeZLengthWFins[segment index]   length covered by fins
/// \n
/// \author Francesca Bucci (francesca.bucci@cern.ch)
/// \EndDetailed

#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4LogicalBorderSurface.hh"

#include "BeamPipe.hh"
#include "BeamTube.hh"
#include "BeamTubeFins.hh"

#include "BeamPipeMaterialParameters.hh"

BeamPipe::BeamPipe(G4int SegmentIndex, G4Material * EnvMaterial, NA62VGeometryParameters* GeoPars, G4LogicalVolume * MotherVolume) : 
NA62VComponent(EnvMaterial,MotherVolume), NA62VNamed("Pipe")
{

  SetSegmentIndex(SegmentIndex);
  SetGeoPars(GeoPars);
  
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

BeamPipe::~BeamPipe(){}

void BeamPipe::ReadGeometryParameters()
{

  fZPosition = fGeoPars->GetBeamPipeZPosition(fSegmentIndex);
  fZLength =  fGeoPars->GetBeamPipeZLength(fSegmentIndex);

  fInputDisplacementWRTBeam = fGeoPars->GetBeamPipeInputDisplacementWRTBeam(fSegmentIndex);
  fOutputDisplacementWRTBeam = fGeoPars->GetBeamPipeOutputDisplacementWRTBeam(fSegmentIndex);
  
  fAngleWRTBeam = (fOutputDisplacementWRTBeam-fInputDisplacementWRTBeam)/fZLength;
 
  fFinOuterRadius = fGeoPars->GetBeamPipeFinOuterRadius(fSegmentIndex);

}

void BeamPipe::CreateGeometry()
{

   G4double HalfZLength = 0.5*fZLength;
   G4double startPhiAngle = 0;
   G4double deltaPhiAngle = 360*deg;
  
   fSolidVolume= new G4Tubs("BeamPipe",
 			   0,
 			   fFinOuterRadius,
 			   HalfZLength,
 			   startPhiAngle,
 			   deltaPhiAngle);
   
   G4RotationMatrix rm;
   rm.rotateY(fAngleWRTBeam);

   fLogicalVolume= new G4LogicalVolume(fSolidVolume,                      // solid
                                       fMaterial,                         // material 
                                       "BeamPipe",                        // name
                                       0,                                 // field manager 
                                       0,                                 // sensitive detector
                                       0);                                // user limits

   fPhysicalVolume = new G4PVPlacement(G4Transform3D(rm,  //rotation
                                                     G4ThreeVector(0.5*(fInputDisplacementWRTBeam
                                                                        -fOutputDisplacementWRTBeam)+
                                                                   fOutputDisplacementWRTBeam,0.,fZPosition)),
                                       fLogicalVolume,      // its logical volume
                                       "BeamPipe",         // its name
                                       fMotherVolume,               // its mother  volume
                                       false,           // no boolean operations
                                       0);              // copy number

   //------------------------------ 
   //  Beam tube
   //------------------------------ 

      fBeamTube = new BeamTube(fSegmentIndex,G4Material::GetMaterial("G4_Al"),fGeoPars,fLogicalVolume); 

   //------------------------------ 
   // Beam tube fins
   //------------------------------ 

      fBeamTubeFins = new BeamTubeFins(fSegmentIndex,G4Material::GetMaterial("G4_Al"),fGeoPars,fLogicalVolume); 

}

void BeamPipe::SetProperties()
{
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}

void BeamPipe::DefineOpticalSurface()
{
// BeamPipe Reflective Surface

  // RICHBeamWindow reflective surface
  fOpticalSurface = new G4OpticalSurface("BeamPipe");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetFinish(ground);
  fOpticalSurface->SetModel(unified);
  fOpticalSurface->SetSigmaAlpha(BeamPipeMaterialParameters::GetInstance()
                                 ->GetTubeOpticalSurfaceSigmaAlpha());

  fOpticalSurface -> SetMaterialPropertiesTable(BeamPipeMaterialParameters::GetInstance()
                                                ->GetTubeOpticalSurfacePT());

  new G4LogicalBorderSurface("Gas/BeamTube Surface",
			     fPhysicalVolume,fBeamTube->GetPhysicalVolume(),fOpticalSurface);

  new G4LogicalBorderSurface("Gas/BeamTubeFins Surface",
                             fPhysicalVolume,fBeamTubeFins->GetPhysicalVolume(),fOpticalSurface);


}
