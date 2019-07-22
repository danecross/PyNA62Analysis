// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-12-05
//
// ---------------------------------------------------------------

#ifndef HLTEvent_H
#define HLTEvent_H 1
#include "Rtypes.h"
#include "TObject.h"
#include "TVector3.h"
#include "Riostream.h"

class HLTTrack : public TObject {

  public:

    HLTTrack();
    HLTTrack(const HLTTrack&);
    ~HLTTrack() {}
    void Clear(Option_t* = "");
    void PrintInfo();

    Int_t GetHLTTrackID()                      { return fHLTTrackID;             }
    Int_t GetNHoughIntersections()             { return fNHoughIntersections;    }
    Int_t GetNHoughAdjacents()                 { return fNHoughAdjacents;        }
    Float_t Getdydz()                          { return fdydz;                   }
    Float_t GetQy()                            { return fQy;                     }
    Float_t GetdxdzBeforeMagnet()              { return fdxdzBeforeMagnet;       }
    Float_t GetdxdzAfterMagnet()               { return fdxdzAfterMagnet;        }
    Float_t GetQxBeforeMagnet()                { return fQxBeforeMagnet;         }
    Float_t GetQxAfterMagnet()                 { return fQxAfterMagnet;          }
    Float_t GetZVertex()                       { return fZVertex;                }
    Float_t GetPz()                            { return fPz;                     }
    Float_t GetCDA()                           { return fCDA;                    }
    Float_t GetTrailing()                      { return fTrailing;               }
    TVector3 GetMomentumBeforeMagnet()         { return fMomentumBeforeMagnet;   }
    TVector3 GetMomentumAfterMagnet()          { return fMomentumAfterMagnet;    }
    TVector3 GetVertex()                       { return fVertex;                 }

    void SetHLTTrackID(Int_t element)          { fHLTTrackID = element;          }
    void SetNHoughIntersections(Int_t element) { fNHoughIntersections = element; }
    void SetNHoughAdjacents(Int_t element)     { fNHoughAdjacents = element;     }
    void Setdydz(Float_t element)              { fdydz = element;                }
    void SetQy(Float_t element)                { fQy = element;                  }
    void SetdxdzBeforeMagnet(Float_t element)  { fdxdzBeforeMagnet = element;    }
    void SetdxdzAfterMagnet(Float_t element)   { fdxdzAfterMagnet = element;     }
    void SetQxBeforeMagnet(Float_t element)    { fQxBeforeMagnet = element;      }
    void SetQxAfterMagnet(Float_t element)     { fQxAfterMagnet = element;       }
    void SetZVertex(Float_t element)           { fZVertex = element;             }
    void SetPz(Float_t element)                { fPz = element;                  }
    void SetCDA(Float_t element)               { fCDA = element;                 }
    void SetTrailing(Float_t element)          { fTrailing = element;            }
    void SetMomentumBeforeMagnet(Float_t mx, Float_t my, Float_t pz) { fMomentumBeforeMagnet.SetXYZ(mx*pz, my*pz, pz); }
    void SetMomentumAfterMagnet(Float_t mx, Float_t my, Float_t pz)  { fMomentumAfterMagnet.SetXYZ(mx*pz, my*pz, pz);  }
    void SetVertex(Float_t mx, Float_t qx, Float_t my, Float_t qy, Float_t zvertex) { fVertex.SetXYZ(mx*zvertex+qx, my*zvertex+qy, zvertex); }

  private:

    Int_t    fHLTTrackID;
    Int_t    fNHoughIntersections; // number of intersections in the Hough transform
    Int_t    fNHoughAdjacents;     // number of adjacent points in the Hough transform
    Float_t  fdydz;
    Float_t  fQy;
    Float_t  fdxdzBeforeMagnet;
    Float_t  fdxdzAfterMagnet;
    Float_t  fQxBeforeMagnet;
    Float_t  fQxAfterMagnet;
    Float_t  fZVertex;
    Float_t  fPz;
    Float_t  fCDA;
    Float_t  fTrailing;

    TVector3 fMomentumBeforeMagnet;
    TVector3 fMomentumAfterMagnet;
    TVector3 fVertex;

    ClassDef(HLTTrack,1);
};



class HLTEvent : public TObject {

  public:

    HLTEvent();
    HLTEvent(const HLTEvent&);
    ~HLTEvent();
    void Clear(Option_t* = "");
    void SetKTAGSectors(UInt_t NSecL0TP, UInt_t NSecCHOD);
    void SetKTAGProcessInfo(Bool_t is_l1_KTAG_processed, Bool_t is_l1_KTAG_empty_packet, Bool_t is_l1_KTAG_bad_data);
    void SetKTAGResponse(UChar_t ktag_response);

    void SetLAVHits(UInt_t l1_LAV_n_hits);
    void SetLAVProcessInfo(Bool_t is_l1_LAV_processed, Bool_t is_l1_LAV_empty_packet, Bool_t is_l1_LAV_bad_data);
    void SetLAVResponse(UChar_t lav_response);

    void SetSTRAWNTracks(UInt_t l1_straw_n_tracks);
    void SetSTRAWProcessInfo(Bool_t is_l1_straw_processed, Bool_t is_l1_straw_empty_packet, Bool_t is_l1_straw_bad_data, Bool_t is_l1_straw_overflow);
    void SetSTRAWResponse(UChar_t straw_response);

    UInt_t GetKTAGSectorsL0TP()          { return fNKTAGSectorsL0TP;    }
    UInt_t GetLAVHits()                  { return fNLAVHits;            }
    UInt_t GetNStrawTracks()             { return fNStrawTracks;        }
    std::vector<HLTTrack> GetHLTTracks() { return fHLTTracks;           }
    void AddHLTTracks(HLTTrack track)    { fHLTTracks.push_back(track); }
    UChar_t GetKTAGResponse()            { return fL1KTAGResponse;      }
    UChar_t GetLAVResponse()             { return fL1LAVResponse;       }
    UChar_t GetSTRAWResponse()           { return fL1StrawResponse;     }

    // Boolean functions
    Bool_t IsL1KTAGProcessed()    { return fL1KTAGProcessed;    }
    Bool_t IsL1KTAGEmptyPacket()  { return fL1KTAGEmptyPacket;  }
    Bool_t IsfL1KTAGBadData()     { return fL1KTAGBadData;      }
    Bool_t IsL1LAVProcessed()     { return fL1LAVProcessed;     }
    Bool_t IsL1LAVEmptyPacket()   { return fL1LAVEmptyPacket;   }
    Bool_t IsL1LAVBadData()       { return fL1LAVBadData;       }
    Bool_t IsL1StrawProcessed()   { return fL1StrawProcessed;   }
    Bool_t IsL1StrawEmptyPacket() { return fL1StrawEmptyPacket; }
    Bool_t IsL1StrawBadData()     { return fL1StrawBadData;     }
    Bool_t IsL1StrawOverflow()    { return fL1StrawOverflow;    }

    void  PrintInfo();

  private:

    Float_t fCHODTime;
    Float_t fNewCHODTime;
    UChar_t fL0TPRefTime;

    UChar_t fL1CHODResponse;
    UChar_t fL1KTAGResponse;
    UChar_t fL1LAVResponse;
    UChar_t fL1IRCSACResponse;
    UChar_t fL1StrawResponse;
    UChar_t fL1MUV3Response;
    UChar_t fL1NewCHODResponse;

    Bool_t fL1CHODProcessed;
    Bool_t fL1KTAGProcessed;
    Bool_t fL1LAVProcessed;
    Bool_t fL1IRCSACProcessed;
    Bool_t fL1StrawProcessed;
    Bool_t fL1MUV3TriggerMultiProcessed;
    Bool_t fL1MUV3TriggerLeftRightProcessed;
    Bool_t fL1MUV3TriggerNeighboursProcessed;
    Bool_t fL1NewCHODProcessed;

    Bool_t fL1CHODEmptyPacket;
    Bool_t fL1KTAGEmptyPacket;
    Bool_t fL1LAVEmptyPacket;
    Bool_t fL1IRCSACEmptyPacket;
    Bool_t fL1StrawEmptyPacket;
    Bool_t fL1MUV3EmptyPacket;
    Bool_t fL1NewCHODEmptyPacket;

    Bool_t fL1CHODBadData;
    Bool_t fL1KTAGBadData;
    Bool_t fL1LAVBadData;
    Bool_t fL1IRCSACBadData;
    Bool_t fL1StrawBadData;
    Bool_t fL1MUV3BadData;
    Bool_t fL1NewCHODBadData;

    Bool_t fL1StrawOverflow;

    UInt_t fNCHODHits;
    UInt_t fNKTAGSectorsL0TP;
    UInt_t fNKTAGSectorsCHOD;
    UInt_t fNLAVHits;
    UInt_t fNIRCSACHits;
    UInt_t fNStrawTracks;
    UInt_t fNMUV3Tiles;
    UInt_t fNNewCHODHits;

    std::vector<HLTTrack> fHLTTracks;

    ClassDef(HLTEvent,1);
};

#endif
