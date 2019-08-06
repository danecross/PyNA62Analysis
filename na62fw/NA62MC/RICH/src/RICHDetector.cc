// 2011-03-18 Monica Pepe
//   - Added use of RICHDetectorMessenger class
//   - Added code for RICH Fast Simulation
// --------------------------------------------------------------

#include "G4Box.hh"
#include "G4Cons.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4UnionSolid.hh"
#include "globals.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4LogicalBorderSurface.hh"
#include "BeamPipe.hh"
#include "RICHGeometryParameters.hh"
#include "RICHMaterialParameters.hh"
#include "RICHDetector.hh"
#include "RICHDetectorMessenger.hh"
#include "RICHMirrorWindow.hh"
#include "RICHVessel.hh"
#include "RICHRadiator.hh"
#include "RICHMirror.hh"
#include "RICHMirrorSupports.hh"
#include "RICHPMTsWindow.hh"
#include "RICHBeamWindow.hh"
#include "G4MaterialPropertiesTable.hh"
#include "DatacardManager.hh"
#include "NA62Global.hh"
#include "NA62Utilities.hh"
#include "NA62ConditionsService.hh"
#include "RICHParameters.hh"

#ifndef RICHScTh
#define RICHScTh 5.0
#endif

#include "RICHPMTSD.hh"
#include "G4SDManager.hh"

RICHDetector::RICHDetector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
  NA62VComponent(Material,MotherVolume), NA62VNamed("RICH"),
  fXLength(.0), fYLength(.0), fZLength(.0),
  fInputDisplacementWRTXaxis(.0), fOutputDisplacementWRTXaxis(.0),
  fZPosition(.0), fAngleWRTXaxis(.0), fNPMs(0), fNMirrors(0),
  fVessel(nullptr), fRadiator(nullptr), fMirror(nullptr), fMirrorSupports(nullptr),
  fMirrorWindow(nullptr), fPMTsWindow(nullptr), fBeamWindow(nullptr) {

  // Connect to RICHDetectorMessenger to enable datacard configuration
  fRICHMessenger = new RICHDetectorMessenger(this);

  // Mandatory here to Find or Build the needed materials
  RICHMaterialParameters::GetInstance();
}

RICHDetector::~RICHDetector() {
  delete fRICHMessenger;
}

void RICHDetector::ReadGeometryParameters() {
  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();

  fXLength = GeoPars->GetRICHDetectorXLength();
  fYLength = GeoPars->GetRICHDetectorYLength();
  fZLength = GeoPars->GetRICHDetectorZLength();

  // fXLength = 4.0*m;
  // fYLength = 4.0*m;
  // fZLength = GeoPars->GetRespRegionZEnd()-GeoPars->GetRespRegionZStart();

  fInputDisplacementWRTXaxis = GeoPars->GetInputDisplacementWRTXaxis();
  // G4cout<<"************** InputDisplacement: "<<fInputDisplacementWRTXaxis <<" ***********"<<G4endl;   
  fOutputDisplacementWRTXaxis = GeoPars->GetOutputDisplacementWRTXaxis();
  // G4cout<<"************** OutputDisplacement: "<<fOutputDisplacementWRTXaxis <<" ***********"<<G4endl;

  fZPosition = GeoPars->GetRICHDetectorZPosition();
  // G4cout<<"************** ZPosition: "<<fZPosition <<" ***********"<<G4endl;

  fAngleWRTXaxis = GeoPars->GetAngleWRTXaxis();
  // G4cout<<"*********** AngleWRTXaxis: "<<fAngleWRTXaxis<<" ****************"<<G4endl;

  fNPMs = GeoPars->GetNPMs();

  // mirror rotation (implemented by Viacheslav Duk, Viacheslav.Duk@cern.ch)
  // offsets are taken from the RICH-MirrorAlignment.dat file
  G4int RunNumber = DatacardManager::GetInstance()->GetRunNumber();
  G4double GlobalOffsetX[2] = {0., 0.};
  G4double GlobalOffsetY[2] = {0., 0.};
  G4int FirstRun, LastRun;
  G4double SingleMirrorAlignment[25][2];
  TString Line;

  NA62ConditionsService::GetInstance()->Open("RICH-MirrorAlignment.dat");
  G4bool RunNotFound = true;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get("RICH-MirrorAlignment.dat")) && RunNotFound) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    FirstRun = ((TObjString*)(l->At(0)))->GetString().Atoi();
    LastRun  = ((TObjString*)(l->At(1)))->GetString().Atoi();
    if (RunNumber<FirstRun || RunNumber>LastRun) { // Not this entry 
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get("RICH-MirrorAlignment.dat"));
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get("RICH-MirrorAlignment.dat"));
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get("RICH-MirrorAlignment.dat"));
    }
    else { // Here we really read!
      RunNotFound = false;
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get("RICH-MirrorAlignment.dat"));
      l = Line.Tokenize(" ");
      GlobalOffsetX[0] = ((TObjString*)(l->At(0)))->GetString().Atof();
      GlobalOffsetY[0] = ((TObjString*)(l->At(1)))->GetString().Atof();
      GlobalOffsetX[1] = ((TObjString*)(l->At(2)))->GetString().Atof();
      GlobalOffsetY[1] = ((TObjString*)(l->At(3)))->GetString().Atof();
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get("RICH-MirrorAlignment.dat"));
      l = Line.Tokenize(" ");
      for (Int_t m=1; m<25; m++) { // read 24 x-coordinates
	SingleMirrorAlignment[m][0] = ((TObjString*)(l->At(m-1)))->GetString().Atof();
      }
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get("RICH-MirrorAlignment.dat"));
      l = Line.Tokenize(" ");
      for (Int_t m=1; m<25; m++) { // read 24 y-coordinates 
	SingleMirrorAlignment[m][1] = ((TObjString*)(l->At(m-1)))->GetString().Atof();
      }
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close("RICH-MirrorAlignment.dat");

  if (RunNotFound) {
    G4cout << "[RICHDetector] run " << RunNumber << " not found in the mirror alignment DB" << G4endl;
    exit(kWrongConfiguration);
  }

  // introduce global offsets
  GeoPars->ShiftMirrorCenterOfCurvature_Jura(GlobalOffsetX[0], GlobalOffsetY[0]);
  GeoPars->ShiftMirrorCenterOfCurvature_Saleve(GlobalOffsetX[1], GlobalOffsetY[1]);
  
  // mirror numbering is taken from NA62MC/RICH/src/RICHMirror.cc
  G4int MirrorNumberJura[10]   = {4, 12, 21, 3, 1, 13, 16, 22, 15, 23};
  G4int MirrorNumberSaleve[10] = {9, 20, 10, 5, 6, 14, 11, 17,  8, 24};
  G4int ThisMirrorNumber;
  // introduce single mirror misalignment
  for (int irow=0; irow<10; irow++) {
    ThisMirrorNumber = MirrorNumberJura[irow];
    GeoPars->SetMirrorCenterOfCurvatureShift_Jura(irow, SingleMirrorAlignment[ThisMirrorNumber][0], SingleMirrorAlignment[ThisMirrorNumber][1]);
    ThisMirrorNumber = MirrorNumberSaleve[irow];
    GeoPars->SetMirrorCenterOfCurvatureShift_Saleve(irow, SingleMirrorAlignment[ThisMirrorNumber][0], SingleMirrorAlignment[ThisMirrorNumber][1]);
  }
}

void RICHDetector::CreateGeometry() {

  // reset the neon refractive index (make it run-dependent)
  ResetNeonRefractiveIndex();
  ReadGeometryParameters();
  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();

  // Defining the mother volume as a box necessarily creates a conflict with MUV0
  //fSolidVolume = new G4Box("RICH", 0.5*fXLength, 0.5*fYLength, 0.5*fZLength);

  G4double overlap = 10*cm; // add 10 cm to allow the two cylinders intersecting

  G4Tubs* RICHRRUpstream = new G4Tubs("RICHRRUpstream",0,GeoPars->GetRespRegionUpstreamRadius(),
      0.5*GeoPars->GetRespRegionUpstreamLength(), 0.0, 360*degree);

  G4Tubs* RICHRRDownstream = new G4Tubs("RICHRRDwStream",0,GeoPars->GetRespRegionDownstreamRadius(),
      0.5*GeoPars->GetRespRegionDownstreamLength()+overlap, 0.0, 360*degree);

   //G4cout<<" *********** RespRegionUpstreamRadius: "<<GeoPars->GetRespRegionUpstreamRadius()<<" ***********"<<G4endl;
   //G4cout<<" *********** RespRegionUpstreamLength: "<<GeoPars->GetRespRegionUpstreamLength()<<" ***********"<<G4endl;

   //G4cout<<" *********** RespRegionDownstreamRadius: "<<GeoPars->GetRespRegionDownstreamRadius()<<" ***********"<<G4endl;
   //G4cout<<" *********** RespRegionDownstreamLength: "<<GeoPars->GetRespRegionDownstreamLength()<<" ***********"<<G4endl;

  fSolidVolume = new G4UnionSolid("RICHRespReg",
      new G4DisplacedSolid("RICHRRUpstream",
        RICHRRUpstream,
        0,
        G4ThreeVector(0,0,GeoPars->GetRespRegionZStart()+GeoPars->GetRespRegionUpstreamLength()*0.5-fZPosition)
        ),
      new G4DisplacedSolid("RICHRRDownstream",
        RICHRRDownstream,
        0,
        G4ThreeVector(0,0,GeoPars->GetRespRegionZEnd()-GeoPars->GetRespRegionDownstreamLength()*0.5-fZPosition-overlap)
        ),
      0, // null rotation
      G4ThreeVector(0,0,0)
      );

 //     G4cout<<" *********** RICHRRUpstream Z Position: "<<GeoPars->GetRespRegionZStart()+GeoPars->GetRespRegionUpstreamLength()*0.5-fZPosition<<" ***********"<<G4endl;

 //     G4cout<<" *********** RICHRRDownstream Z Position: "<<GeoPars->GetRespRegionZEnd()-GeoPars->GetRespRegionDownstreamLength()*0.5-fZPosition<<" ***********"<<G4endl;

  G4RotationMatrix rm;
  rm.rotateY(-fAngleWRTXaxis);

  //   G4cout << HalfXLength/m << "\t" << HalfYLength/m << "\t" << HalfZLength/m << "\t" 
  //     << fInputDisplacementWRTBeam/mm << "\t" << fOutputDisplacementWRTBeam/mm << "\t" 
  //     << fAngleWRTBeam << "\t" << G4endl;

  //G4cout<< "RICHDetector   Input Output Anglewrtb "
  //	<< fInputDisplacementWRTBeam/mm << "\t" << fOutputDisplacementWRTBeam/mm << "\t" 
  //	<< fAngleWRTBeam << "\t" << G4endl;

  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                      // solid
      fMaterial,                         // material
      "RICHRR",                          // name
      0,                                 // field manager 
      0,                                 // sensitive detector
      0);                                // user limits

  fPhysicalVolume = new G4PVPlacement(G4Transform3D(rm,  //rotation
        G4ThreeVector(0.5*(fInputDisplacementWRTXaxis
            -fOutputDisplacementWRTXaxis)+
          fOutputDisplacementWRTXaxis,0.,fZPosition)),
      fLogicalVolume, // its logical volume
      "RICHRR",         // its name
      fMotherVolume,  // its mother  volume
      false,          // no boolean operations
      0);             // copy number

  //------------------------------
  // Stainless steel tube
  //------------------------------

  fVessel = new RICHVessel(G4Material::GetMaterial("RICH_StainlessSteel"),fLogicalVolume);

  //------------------------------
  // Ne Radiator
  //------------------------------

  fRadiator = new RICHRadiator(G4Material::GetMaterial("RICH_Ne"),fVessel->GetLogicalVolume());

  //------------------------------
  // Mirror
  //------------------------------

  fMirror = new RICHMirror(0,G4Material::GetMaterial("G4_GLASS_PLATE"),fRadiator->GetLogicalVolume());
  
  //------------------------------
  // Mirror Supports
  //------------------------------
  
  fMirrorSupports = new RICHMirrorSupports(G4Material::GetMaterial("G4_Al"),fRadiator->GetLogicalVolume());
  
  //------------------------------
  // Mirror Window
  //------------------------------
  
  fMirrorWindow = new RICHMirrorWindow(G4Material::GetMaterial("G4_Al"),fLogicalVolume);

  //------------------------------
  // PMTs Window (Conical Window + Cilynders + PMTs Disk)
  //------------------------------

  fPMTsWindow = new RICHPMTsWindow(G4Material::GetMaterial("RICH_StainlessSteel"),fLogicalVolume);

  //------------------------------
  // Aluminium Beam Window
  //------------------------------
  
  fBeamWindow = new RICHBeamWindow(G4Material::GetMaterial("G4_Al"),fLogicalVolume);
  
  //------------------------------
  // Aluminium beam pipe
  //------------------------------ 

   new BeamPipe(0,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume,false);
   new BeamPipe(1,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume,false);
   new BeamPipe(2,G4Material::GetMaterial("RICH_Ne"),GeoPars,fLogicalVolume);
   new BeamPipe(3,G4Material::GetMaterial("RICH_Ne"),GeoPars,fLogicalVolume);
   new BeamPipe(4,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume,false);
   new BeamPipe(5,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume);
   
  //------------------------------
  // Optical Surface of Mirror
  //------------------------------
  
  for(G4int iMirr = 0; iMirr < 10; iMirr++){
    new G4LogicalBorderSurface("Neon/RICHMirror Surface",fRadiator->GetPhysicalVolume(),
        fMirror->GetPhysicalMirror_Jura(iMirr),
        fMirror->GetOpticalSurface());
    new G4LogicalBorderSurface("Neon/RICHMirror Surface",fRadiator->GetPhysicalVolume(),
        fMirror->GetPhysicalMirror_Saleve(iMirr),
        fMirror->GetOpticalSurface());                                     
  }

  //------------------------------
  // Optical Surface of RICHVessel
  //------------------------------

  new G4LogicalBorderSurface("Neon/RICHVessel Surface",fRadiator->GetPhysicalVolume(),fVessel->GetPhysicalVolume(),
      fVessel->GetOpticalSurface());
  
  //-------------------------------------
  // Optical Surface of RICHMirrorSupport
  //-------------------------------------

  new G4LogicalBorderSurface("Neon/RICHMirrorSupport Surface",
      fRadiator->GetPhysicalVolume(),fMirrorSupports->GetPhysicalSupport_Jura(),
      fMirrorSupports->GetOpticalSurface());
  new G4LogicalBorderSurface("Neon/RICHMirrorSupport Surface",
      fRadiator->GetPhysicalVolume(),fMirrorSupports->GetPhysicalSupport_Saleve(),
      fMirrorSupports->GetOpticalSurface());

  SetProperties();
}

void RICHDetector::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}

void RICHDetector::ResetNeonRefractiveIndex() {
  G4int    RunNumber = DatacardManager::GetInstance()->GetRunNumber();
  G4long   RunTime   = NA62Utilities::GetInstance()->GetRunTime(RunNumber);
  G4double Radius    = RICHParameters::GetInstance()->GetElectronRingRadius(RunNumber, RunTime); // [mm]
  G4double RefRadius = RICHParameters::GetInstance()->GetReferenceElectronRingRadius(); // [mm]
  G4cout << "[RICHDetector] Electron ring radius = " << Radius << " mm " << G4endl;

  // Calculate the run-dependent neon refractive index
  G4double    NeonPressure    = 0.984*bar*1.012 * pow(Radius/RefRadius, 2);
  G4double    NeonTemperature = STP_Temperature + 24*kelvin;
  G4double   *PhotonEnergy, *NeonRefIndex;
  G4int       MaterialPropertiesNEntries = 17;
  PhotonEnergy = new G4double[MaterialPropertiesNEntries];
  NeonRefIndex = new G4double[MaterialPropertiesNEntries];

  G4double h_Planck = 4.135669239559144E-12; // Planck constant
  G4double s        = 1e9;                   // second (time unit)
  for (G4int i=0; i<MaterialPropertiesNEntries; i++) {
    PhotonEnergy[i] = 2.00*eV + (7.75 - 2.00)*eV / (MaterialPropertiesNEntries - 1) * i;
    NeonRefIndex[i] = 1. + 2.61303e27/(39160e27-
				       (PhotonEnergy[i]/h_Planck)*
				       (PhotonEnergy[i]/h_Planck)*(s*s))*
      (NeonPressure/(NeonTemperature))/(STP_Pressure/(STP_Temperature));
  }

  // Update the neon refractive index in the table
  G4Material *Neon = G4Material::GetMaterial("RICH_Ne");
  G4MaterialPropertiesTable *Table = Neon->GetMaterialPropertiesTable();
  Table->AddProperty("RINDEX", PhotonEnergy, NeonRefIndex, MaterialPropertiesNEntries);
}
