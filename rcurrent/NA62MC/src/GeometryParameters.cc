// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// 2009-02-26 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - added diagnostic output for responsibility regions conflicts
//
// 2014-03-14 Evgueni Goudzovski
//   - optimization and bug fixes
//
// --------------------------------------------------------------

#include "TVector.h"
#include "GeometryParameters.hh"
#include "DetectorParameter.hh"
#include "CedarGeometryParameters.hh"
#include "CHANTIGeometryParameters.hh"
#include "CHODGeometryParameters.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "IRCGeometryParameters.hh"
#include "LAVGeometryParameters.hh"
#include "LKrGeometryParameters.hh"
#include "RICHGeometryParameters.hh"
#include "SpectrometerGeometryParameters.hh"
#include "SACGeometryParameters.hh"
#include "MUV0GeometryParameters.hh"
#include "MUV1GeometryParameters.hh"
#include "MUV2GeometryParameters.hh"
#include "MUV3GeometryParameters.hh"
#include "NewCHODGeometryParameters.hh"
#include "HACGeometryParameters.hh"

GeometryParameters* GeometryParameters::fInstance = nullptr;

GeometryParameters::GeometryParameters() :
  fWorldZLength(600.*m), fWorldXLength(6.*m), fWorldYLength(6.*m) {

  // Add subdetectors' geometrical information
  fSubDetectorsParameters.push_back(CedarGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(CHANTIGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(CHODGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(GigaTrackerGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(IRCGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(LAVGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(LKrGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(RICHGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(SpectrometerGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(SACGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(MUV0GeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(MUV1GeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(MUV2GeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(MUV3GeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(NewCHODGeometryParameters::GetInstance());
  fSubDetectorsParameters.push_back(HACGeometryParameters::GetInstance());
}

GeometryParameters* GeometryParameters::GetInstance() {
  if (!fInstance) { fInstance = new GeometryParameters(); }
  return fInstance;
}

// Check for overlaps of Z coordinates of the responsibility regions
G4bool GeometryParameters::Check() {
  std::vector<ResponsibilityRegion*> FirstRegion,SecondRegion;
  G4int nDetectors = fSubDetectorsParameters.size();
  for (G4int iDet = 0; iDet < nDetectors; iDet++) {
    FirstRegion = fSubDetectorsParameters[iDet]->GetResponsibilityRegion();
    G4int nFirstRegion = FirstRegion.size();
    for(G4int jDet = iDet; jDet < nDetectors; jDet++) {
      SecondRegion = fSubDetectorsParameters[jDet]->GetResponsibilityRegion();
      G4int nSecondRegion = SecondRegion.size();
      for (G4int iReg = 0; iReg < nFirstRegion; iReg++) {
	for (G4int jReg = 0; jReg < nSecondRegion; jReg++) {

	  if (iDet==jDet && iReg==jReg) continue;

	  // An exception: New CHOD and MUV0 responsibility regions
	  // share the same Z range by contruction
	  if (fSubDetectorsParameters[iDet]->GetName()=="RICH" &&
	      fSubDetectorsParameters[jDet]->GetName()=="MUV0" &&
	      iReg==0 && jReg==0) continue;
	  if (fSubDetectorsParameters[iDet]->GetName()=="MUV0" &&
	      fSubDetectorsParameters[jDet]->GetName()=="RICH" &&
	      iReg==0 && jReg==0) continue;

	  if (SecondRegion[jReg]->GetStart() >= FirstRegion[iReg]->GetStop())  continue;
	  if (SecondRegion[jReg]->GetStop()  <= FirstRegion[iReg]->GetStart()) continue;

	  G4cout << "*** Conflict of resp.regions: "
		 << fSubDetectorsParameters[iDet]->GetName() << "(" << iReg << ") "
		 << FirstRegion[iReg]->GetStart() << " " << FirstRegion[iReg]->GetStop()
		 << " vs "
		 << fSubDetectorsParameters[jDet]->GetName() << "(" << jReg << ") "
		 << SecondRegion[jReg]->GetStart() << " " << SecondRegion[jReg]->GetStop()
		 << G4endl;
	  return kTRUE;
	}
      }
    }
  }
  return kFALSE;
}

TObjArray GeometryParameters::GetHashTable() {
  // Temporary solution to keep track of the geometry in the data files
  TObjArray GeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fWorldZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldZLength));
  GeometryParameters.Add(new DetectorParameter("fWorldZLength",Value.Data(),
					       "World Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldXLength));
  GeometryParameters.Add(new DetectorParameter("fWorldXLength",Value.Data(),
					       "World X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldYLength));
  GeometryParameters.Add(new DetectorParameter("fWorldYLength",Value.Data(),
					       "World Y Length", ParameterData));

  return GeometryParameters;
}

void GeometryParameters::Print() {
  G4cout << "fWorldZLength= "<< fWorldZLength << G4endl
	 << "fWorldXLength= "<< fWorldXLength << G4endl
	 << "fWorldYLength= "<< fWorldYLength << G4endl;
}
