// --------------------------------------------------------------
// History:
// 
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-04
// 
// --------------------------------------------------------------
#ifndef TNA62MagField_H
#define TNA62MagField_H

#include "TEveTrackPropagator.h"
#include "TEveVSDStructs.h"
#include "TGeoManager.h"
#include "TObjArray.h"

class TNA62MagField : public TEveMagField {
  public:
    explicit TNA62MagField(TGeoManager * GeoManager);
    virtual ~TNA62MagField() {}

    using   TEveMagField::GetField;
    virtual TEveVector GetField(Float_t /*x*/, Float_t /*y*/, Float_t /*z*/) const; 

    virtual Float_t    GetMaxFieldMag() const { return 1.7; }

  private:
    TObjArray *fMagnets, *fTransformations;
};
#endif
