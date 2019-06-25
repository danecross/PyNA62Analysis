// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-01-28
//
// ------------------------------------------------------------------

#ifndef GEOMETRICACCEPTANCE_HH
#define GEOMETRICACCEPTANCE_HH

#include "TMath.h"
#include "KinePart.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "TRecoLKrCandidate.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "DownstreamTrack.hh"
#include <iostream>

class GeometricAcceptance {

public:
  static GeometricAcceptance* GetInstance();

  Bool_t InAcceptance(DownstreamTrack*,
		      Int_t DetectorID, Int_t StationID=-1, Double_t par1=-1.0, Double_t par2=-1.0);
  Bool_t InAcceptance(TRecoVCandidate*,
		      Int_t DetectorID, Int_t StationID=-1, Double_t par1=-1.0, Double_t par2=-1.0);
  Bool_t InAcceptance(KinePart*,
		      Int_t DetectorID, Int_t StationID=-1, Double_t par1=-1.0, Double_t par2=-1.0);
  Bool_t InAcceptance(TVector3 SpacePoint, TVector3 Direction,
		      Int_t DetectorID, Int_t StationID=-1, Double_t par1=-1.0, Double_t par2=-1.0);
  Bool_t InAcceptance(Double_t x, Double_t y,
		      Int_t DetectorID, Int_t StationID=-1, Double_t par1=-1.0, Double_t par2=-1.0);

  Double_t GetZStraw(Int_t ich)
  { return (ich>=0 && ich<=3) ? fZStraw[ich] : 0.0; }
  Double_t GetXStrawChamberCentre(Int_t ich)
  { return (ich>=0 && ich<=3) ? fXStrawChamberCentre[ich] : 0.0; }

  Double_t GetZGTK1()                 { return fZGTK1;                 }
  Double_t GetZGTK2()                 { return fZGTK2;                 }
  Double_t GetZTrim5()                { return fZTrim5;                }
  Double_t GetZGTK3()		      { return fZGTK3;		       }
  Double_t GetZRICHFrontPlane()       { return fZRICHFrontPlane;       }
  Double_t GetZRICHMirror()           { return fZRICHMirror;           }
  Double_t GetZRICHBackPlane()        { return fZRICHBackPlane;        }
  Double_t GetXRICHFrontPlaneCentre() { return fXRICHFrontPlaneCentre; }
  Double_t GetXRICHBackPlaneCentre()  { return fXRICHBackPlaneCentre;  }
  Double_t GetZCHODVPlane()           { return fZCHODVPlane;           }
  Double_t GetZCHODHPlane()           { return fZCHODHPlane;           }
  Double_t GetZNewCHODFront()         { return fZNewCHODFront;         }
  Double_t GetZNewCHOD()              { return fZNewCHOD;              }
  Double_t GetZNewCHODBack()          { return fZNewCHODBack;          }
  Double_t GetZLAVFront(Int_t i)      { return fZLAVFront[i];          }
  Double_t GetZLAVBack(Int_t i)       { return fZLAVBack[i];           }
  Double_t GetZIRC()                  { return fZIRC;                  }
  Double_t GetZLKr()                  { return fZLKr;                  }
  Double_t GetLKrCellSize()           { return fLKrCellSize;           }
  Double_t GetZMUV0()                 { return fZMUV0;                 }
  Double_t GetZMUV1()                 { return fZMUV1;                 }
  Double_t GetZMUV2()                 { return fZMUV2;                 }
  Double_t GetZMUV3()                 { return fZMUV3;                 }
  Double_t GetZSAC()                  { return fZSAC;                  }
  Double_t GetZArgonion()             { return fZArgonion;             }

  Double_t GetStrawRmin()        { return fRmin_Straw;    }
  Double_t GetStrawRmax()        { return fRmax_Straw;    }
  Double_t GetRICHRmin()         { return fRmin_RICH;     }
  Double_t GetRICHRmax()         { return fRmax_RICH;     }
  Double_t GetCHODRmin()         { return fRmin_CHOD;     }
  Double_t GetCHODRmax()         { return fRmax_CHOD;     }
  Double_t GetNewCHODRmin()      { return fRmin_NewCHOD;  }
  Double_t GetNewCHODRmax()      { return fRmax_NewCHOD;  }
  Double_t GetMUV1Rmin()         { return fRmin_MUV1;     }
  Double_t GetMUV1Rmax()         { return fRmax_MUV1;     }
  Double_t GetMUV2Rmin()         { return fRmin_MUV2;     }
  Double_t GetMUV2Rmax()         { return fRmax_MUV2;     }
  Double_t GetMUV3Rmin()         { return fRmin_MUV3;     }
  Double_t GetMUV3Rmax()         { return fRmax_MUV3;     }
  Double_t GetLAVRinner(Int_t i) { return fRinner_LAV[i]; }
  Double_t GetLAVRouter(Int_t i) { return fRouter_LAV[i]; }
  Double_t GetIRCRmin()          { return fRmin_IRC;      }
  Double_t GetIRCRmax()          { return fRmax_IRC;      }
  Double_t GetLKrRmin()          { return fRmin_LKr;      }
  Double_t GetLKrRmax()          { return fRmax_LKr;      }
  Double_t GetSACRmax()          { return fRmax_SAC;      }

private:

  GeometricAcceptance();
  ~GeometricAcceptance() {}

  //////////////////////
  // Geometry parameters

  Double_t fZGTK1;                  ///< Z coordinate of GTK1
  Double_t fZGTK2;                  ///< Z coordinate of GTK2
  Double_t fZTrim5;                 ///< Z coordinate of Triu5 magnet centre
  Double_t fZGTK3;		    ///< Z coordinate of GTK3
  Double_t fZStraw[4];              ///< Z coordinates of straw planes
  Double_t fStrawViewHoleSize;      ///< Half of the beam hole in each straw view
  Double_t fXStrawChamberCentre[4]; ///< X coordinates of the straw hole centres
  Double_t fZRICHFrontPlane;        ///< RICH front plane
  Double_t fZRICHMirror;            ///< RICH mirror plane
  Double_t fZRICHBackPlane;         ///< RICH back plane
  Double_t fXRICHFrontPlaneCentre;  ///< X coordinate of the cut centre at the RICH front plane
  Double_t fXRICHBackPlaneCentre;   ///< X coordinate of the cut centre at the RICH back plane
  Double_t fZCHODVPlane;            ///< Front of first CHOD plane (vertical slabs)
  Double_t fZCHODHPlane;            ///< Front of second CHOD plane (horizontal slabs)
  Double_t fZNewCHODFront;          ///< NewCHOD scitillator front plane
  Double_t fZNewCHOD;               ///< NewCHOD centre plane
  Double_t fZNewCHODBack;           ///< NewCHOD scintillator back plane
  Double_t fZLAVFront[12];          ///< Front planes of the LAV stations
  Double_t fZLAVBack[12];           ///< Back planes of the LAV stations
  Double_t fZIRC;                   ///< IRC front plane
  Double_t fZLKr;                   ///< LKr front plane
  Double_t fLKrCellSize;            ///< LKr cell size (x = y)
  Double_t fZMUV0;                  ///< MUV0 front plane
  Double_t fZMUV1;                  ///< MUV1 front plane
  Double_t fZMUV2;                  ///< MUV2 front plane
  Double_t fZMUV3;                  ///< MUV3 front plane
  Double_t fZSAC;                   ///< SAC front plane
  Double_t fZArgonion;              ///< The Argonion counter
  Double_t fMUV3GapWidth;           ///< Width of the central vertical gap in MUV3

  /////////////////////////////////
  // Standard acceptance definition

  Double_t fRmin_Straw;   ///< Minimum cut in straw chamber planes [radius of a circle]
  Double_t fRmax_Straw;   ///< Maximum cut in straw chamber planes [radius of a circle]
  Double_t fRmin_RICH;    ///< Minimum cut in RICH front plane [radius of a circle]
  Double_t fRmax_RICH;    ///< Maximum cut in RICH front plane [radius of a circle]
  Double_t fRmin_CHOD;    ///< Minimum cut in the two CHOD planes [radius of a circle]
  Double_t fRmax_CHOD;    ///< Maximum cut in the two CHOD planes [radius of a circle]
  Double_t fRmin_NewCHOD; ///< Minimum cut in NewCHOD plane [radius of a circle]
  Double_t fRmax_NewCHOD; ///< Maximum cut in NewCHOD plane [radius of a circle]
  Double_t fRmin_MUV1;    ///< Minimum cut in MUV1 plane [radius of a circle]
  Double_t fRmax_MUV1;    ///< Maximum cut in MUV1 plane [half-side of a square]
  Double_t fRmin_MUV2;    ///< Minimum cut in MUV2 plane [radius of a circle]
  Double_t fRmax_MUV2;    ///< Maximum cut in MUV2 plane [half-side of a square]
  Double_t fRmin_MUV3;    ///< Minimum cut in MUV3 plane [radius of a circle]
  Double_t fRmax_MUV3;    ///< Maximum cut in MUV2 plane [half-side of vertical size; horizontal size is wider by the gap width]
  Double_t fRinner_LAV[12]; ///< Maximum cut in LAV planes [radius of a circle]
  Double_t fRouter_LAV[12]; ///< Outer radius of LAV stations
  Double_t fRmin_IRC;     ///< Minimum cut in IRC front plane [radius of a circle]
  Double_t fRmax_IRC;     ///< Maximum cut in IRC front plane [radius of a circle]
  Double_t fRmin_LKr;     ///< Minimum cut in LKr front plane [radius of a circle]
  Double_t fRmax_LKr;     ///< Maximum cut in LKr front plane [apothem of a regular octagon]
  Double_t fRmax_SAC;     ///< Maximum cut in SAC front plane [half-side of a square]
};

#endif
