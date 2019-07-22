// --------------------------------------------------------------
// History:
// 
// 2009-03-10 S.Bifani
// - Changed the magnets configuration
//
/// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-04
// 
// --------------------------------------------------------------
//

#include "TNA62MagField.hh"
#include "TGeoNode.h"
#include "TGeoBBox.h"
#include "TGeoMatrix.h"
#include "TGeoManager.h"
#include "Riostream.h"

TNA62MagField::TNA62MagField(TGeoManager * GeoManager) : TEveMagField(){

  fMagnets = new TObjArray();
  fTransformations = new TObjArray();
  TGeoTranslation Translation;

  TObjArray * WorldNodes = GeoManager->GetTopVolume()->GetNodes();
  for(Int_t iNode = 0; iNode < WorldNodes->GetEntries(); iNode++){
    TString WorldNodeName = ((TGeoNode*)WorldNodes->At(iNode))->GetName();
    if(WorldNodeName.BeginsWith("GigaTracker") || WorldNodeName.BeginsWith("Spectrometer")){
      TObjArray * SubDetNodes = GeoManager->GetTopVolume()->FindNode(WorldNodeName.Data())->GetVolume()->GetNodes();
      for(Int_t iSubDetNode = 0; iSubDetNode < SubDetNodes->GetEntries(); iSubDetNode++){
        TString SubDetNodeName = ((TGeoNode*)SubDetNodes->At(iSubDetNode))->GetName();
        if(SubDetNodeName.Contains("Magnet")){
          TGeoNode* Magnet = GeoManager->GetTopVolume()->FindNode(WorldNodeName.Data())->GetVolume()
            ->FindNode(SubDetNodeName.Data());
          std::cout << SubDetNodeName.Data() << " found" << std::endl;
          fMagnets->Add(Magnet);
          Translation = *(GeoManager->GetTopVolume()->FindNode(WorldNodeName.Data())->GetMatrix());
          Translation = Translation *(*(Magnet->GetMatrix()));
          fTransformations->Add(new TGeoTranslation(Translation));
        }
      }
    }
  }
}

TEveVector TNA62MagField::GetField(Float_t x, Float_t y, Float_t z) const {

  Double_t MagFields[6] = {-1.6678, 1.6678, 1.6678, -1.6678, 0.7505, -1.6928};
  TEveVector Field(0., 0.*2.e-6, 0.);

  Int_t iMagnet;
  Int_t NMagnets = fMagnets->GetEntries();
  for(iMagnet = 0; iMagnet < NMagnets; iMagnet++){
    TGeoNode* node = (TGeoNode*) fMagnets->At(iMagnet);
    const Double_t * center = ((TGeoMatrix*)fTransformations->At(iMagnet))->GetTranslation();
    //cout << iMagnet << " ++++++ " << center[0] << " ++++++ " << center[1] << " ++++++ " << center[2] << std::endl;
    Double_t point[3];
    point[0] = x; point[1] = y; point[2] = z;
    TGeoBBox * Shape = (TGeoBBox*)node->GetVolume()->GetShape();
    Double_t dx = Shape->GetDX();
    Double_t dy = Shape->GetDY();
    Double_t dz = Shape->GetDZ();
    //cout << iMagnet << " ++++++ " << dx << " ++++++ " << dy << " ++++++ " << dz << std::endl;
    if(Shape->Contains(point, dx, dy, dz, center)){
      if(iMagnet < 4) Field = TEveVector(0., -MagFields[iMagnet], 0.); //TEveTrackPropagator bug => -MagField
      else            Field = TEveVector(-MagFields[iMagnet], 0., 0.); //TEveTrackPropagator bug => -MagField
      //cout << iMagnet << " ++++++ " << x << " ++++++ " << y << " ++++++ " << z <<  " ------- " << -MagFields[iMagnet] << std::endl;
      break;
    }
  }
  //cout << iMagnet << " ------------ " << x << " -- " << y << " -- " << z << " -------------- " << (Field.Arr())[1] << std::endl;
  return Field;

}

