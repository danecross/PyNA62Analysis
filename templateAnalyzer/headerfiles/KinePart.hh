// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)
//            Francesca Bucci (Francesca.Bucci@cern.ch) 2008-01-17
//
// Based on a project by Emanuele Leonardi (Emanuele Leonardi@cern.ch) 2007-01-10
//
// --------------------------------------------------------------

#ifndef KinePart_h
#define KinePart_h 1

#include "GenePart.hh"
#include "TVector3.h"
#include "TLorentzVector.h"

class KinePart : public GenePart {

public:

  KinePart();
  virtual ~KinePart() {Clear();}
  void Clear(Option_t* option="");
  void Print(Option_t* option="") const;
  void PrintCheckPoint(Int_t);
  void PrintAllCheckPoints();

  inline void ShiftParentIndex(Int_t val)     { if (fParentIndex>=0) fParentIndex += val; }

  inline void SetFinalEnergy(Double_t val)    { fFinalEnergy = val;                       }
  inline void SetFinalMomentum(TVector3 val)  { fFinalMomentum = val;                     }
  inline void SetID(Int_t val)                { fID = val;                                }
  inline void SetParentID(Int_t val)          { fParentID = val;                          }
  inline void SetParentIndex(Int_t val)       { fParentIndex = val;                       }
  inline void SetProdPos(TLorentzVector val)  { fProdPos = val;                           }
  inline void SetEndPos(TLorentzVector val)   { fEndPos = val;                            }
  inline void SetProdProcessName(TString val) { fProdProcessName = val;                   }
  inline void SetProdProcessName(char* val)   { fProdProcessName = TString(val);          }
  inline void SetEndProcessName(TString val)  { fEndProcessName = val;                    }
  inline void SetEndProcessName(char* val)    { fEndProcessName = TString(val);           }
  inline void SetDirectParent(bool val)       { fDirectParent = val;                      }

  void SetFinal4Momentum(TLorentzVector val)
  { SetFinalMomentum(val.Vect()); SetFinalEnergy(val.T()); }

  inline Double_t GetFinalEnergy()            { return fFinalEnergy;                      }
  inline TVector3 GetFinalMomentum()          { return fFinalMomentum;                    }
  inline Int_t GetID()                        { return fID;                               }
  inline Int_t GetParentID()                  { return fParentID;                         }
  inline Int_t GetParentIndex()               { return fParentIndex;                      }
  inline TLorentzVector GetProdPos()          { return fProdPos;                          }
  inline TLorentzVector GetEndPos()           { return fEndPos;                           }
  inline TString GetProdProcessName()         { return fProdProcessName;                  }
  inline TString GetEndProcessName()          { return fEndProcessName;                   }
  inline TLorentzVector GetFinal4Momentum()
  { return TLorentzVector(fFinalMomentum,fFinalEnergy); }
  inline bool GetDirectParent()               { return fDirectParent;                     }

  Double_t xAt(Double_t z, Double_t MagFieldScaleFactor=1.0);
  Double_t yAt(Double_t z);
  Double_t xAtBackwards(Double_t z);
  Double_t yAtBackwards(Double_t z);

  TVector3 GetPosAtZ(Double_t z); ///< Obtained by extrapolating between checkpoints

  // Positions and momenta at the check points:
  // a) addressing the checkpoints by name

  TVector3 GetPosCedarEntry()              { return fPosCedarEntry;        }
  TVector3 GetPosCedarExit()               { return fPosCedarExit;         }
  TVector3 GetPosGigaTrackerEntry()        { return fPosGigaTrackerEntry;  }
  TVector3 GetPosGigaTrackerExit()         { return fPosGigaTrackerExit;   }
  TVector3 GetPosSpectrometerEntry()       { return fPosSpectrometerEntry; }
  TVector3 GetPosMNP33Entry()              { return fPosMNP33Entry;        }
  TVector3 GetPosMNP33Exit()               { return fPosMNP33Exit;         }
  TVector3 GetPosRICHMirrorEntry()         { return fPosRICHMirrorEntry;   }
  TVector3 GetPosCHODEntry()               { return fPosCHODEntry;         }
  TVector3 GetPosLKrEntry()                { return fPosLKrEntry;          }
  TVector3 GetPosMUV1Entry()               { return fPosMUV1Entry;         }
  TVector3 GetPosMUV2Entry()               { return fPosMUV2Entry;         }
  TVector3 GetPosFISCEntry()               { return fPosFISCEntry;         }
  TVector3 GetPosMUV3Entry()               { return fPosMUV3Entry;         }
  TVector3 GetPosXWCMEntry()               { return fPosXWCMEntry;         }

  TLorentzVector GetMomCedarEntry()        { return fMomCedarEntry;        }
  TLorentzVector GetMomCedarExit()         { return fMomCedarExit;         }
  TLorentzVector GetMomGigaTrackerEntry()  { return fMomGigaTrackerEntry;  }
  TLorentzVector GetMomGigaTrackerExit()   { return fMomGigaTrackerExit;   }
  TLorentzVector GetMomSpectrometerEntry() { return fMomSpectrometerEntry; }
  TLorentzVector GetMomMNP33Entry()        { return fMomMNP33Entry;        }
  TLorentzVector GetMomMNP33Exit()         { return fMomMNP33Exit;         }
  TLorentzVector GetMomRICHMirrorEntry()   { return fMomRICHMirrorEntry;   }
  TLorentzVector GetMomCHODEntry()         { return fMomCHODEntry;         }
  TLorentzVector GetMomLKrEntry()          { return fMomLKrEntry;          }
  TLorentzVector GetMomMUV1Entry()         { return fMomMUV1Entry;         }
  TLorentzVector GetMomMUV2Entry()         { return fMomMUV2Entry;         }
  TLorentzVector GetMomFISCEntry()         { return fMomFISCEntry;         }
  TLorentzVector GetMomMUV3Entry()         { return fMomMUV3Entry;         }
  TLorentzVector GetMomXWCMEntry()         { return fMomXWCMEntry;         }

  void SetPosCedarEntry      (TVector3 val)       { fPosCedarEntry = val;       }
  void SetPosCedarExit       (TVector3 val)       { fPosCedarExit = val;        }
  void SetPosGigaTrackerEntry(TVector3 val)       { fPosGigaTrackerEntry = val; }
  void SetPosGigaTrackerExit (TVector3 val)       { fPosGigaTrackerExit = val;  }
  void SetPosMNP33Entry      (TVector3 val)       { fPosMNP33Entry = val;       }
  void SetPosMNP33Exit       (TVector3 val)       { fPosMNP33Exit = val;        }
  void SetPosRICHMirrorEntry (TVector3 val)       { fPosRICHMirrorEntry = val;  }
  void SetPosCHODEntry       (TVector3 val)       { fPosCHODEntry = val;        }
  void SetPosLKrEntry        (TVector3 val)       { fPosLKrEntry = val;         }
  void SetPosMUV1Entry       (TVector3 val)       { fPosMUV1Entry = val;        }
  void SetPosMUV2Entry       (TVector3 val)       { fPosMUV2Entry = val;        }
  void SetPosFISCEntry       (TVector3 val)       { fPosFISCEntry = val;        }
  void SetPosMUV3Entry       (TVector3 val)       { fPosMUV3Entry = val;        }
  void SetPosXWCMEntry       (TVector3 val)       { fPosXWCMEntry = val;        }

  void SetMomCedarEntry      (TLorentzVector val) { fMomCedarEntry = val;       }
  void SetMomCedarExit       (TLorentzVector val) { fMomCedarExit = val;        }
  void SetMomGigaTrackerEntry(TLorentzVector val) { fMomGigaTrackerEntry = val; }
  void SetMomGigaTrackerExit (TLorentzVector val) { fMomGigaTrackerExit = val;  }
  void SetMomMNP33Entry      (TLorentzVector val) { fMomMNP33Entry = val;       }
  void SetMomMNP33Exit       (TLorentzVector val) { fMomMNP33Exit = val;        }
  void SetMomRICHMirrorEntry (TLorentzVector val) { fMomRICHMirrorEntry = val;  }
  void SetMomCHODEntry       (TLorentzVector val) { fMomCHODEntry = val;        }
  void SetMomLKrEntry        (TLorentzVector val) { fMomLKrEntry = val;         }
  void SetMomMUV1Entry       (TLorentzVector val) { fMomMUV1Entry = val;        }
  void SetMomMUV2Entry       (TLorentzVector val) { fMomMUV2Entry = val;        }
  void SetMomFISCEntry       (TLorentzVector val) { fMomFISCEntry = val;        }
  void SetMomMUV3Entry       (TLorentzVector val) { fMomMUV3Entry = val;        }
  void SetMomXWCMEntry       (TLorentzVector val) { fMomXWCMEntry = val;        }

  // b) addressing the checkpoints by ID

  TVector3       GetPosAtCheckPoint(Int_t);
  TLorentzVector GetMomAtCheckPoint(Int_t);
  void           SetPosAtCheckPoint(Int_t, TVector3);
  void           SetMomAtCheckPoint(Int_t, TLorentzVector);

private:

  Double_t       fFinalEnergy;   ///< Energy at the end position, MeV
  TVector3       fFinalMomentum; ///< Momentum at the end position, MeV
  Int_t          fID; ///< Geant4 track ID
  Int_t          fParentID; ///< Geant4 parent track ID
  Int_t          fParentIndex; ///< Index in KinePart array of the parent particle
  TLorentzVector fProdPos; ///< Production position and time
  TLorentzVector fEndPos;  ///< End position and time
  TString        fProdProcessName; ///< Name of the production process (e.g. "Decay")
  TString        fEndProcessName;  ///< Name of the end process (e.g. "Decay")
  Bool_t         fDirectParent;

  // Positions and momenta at the check points

  TVector3
    fPosCedarEntry,
    fPosCedarExit,
    fPosGigaTrackerEntry,
    fPosGigaTrackerExit,
    fPosSpectrometerEntry,
    fPosMNP33Entry,
    fPosMNP33Exit,
    fPosRICHMirrorEntry,
    fPosCHODEntry,
    fPosLKrEntry,
    fPosMUV1Entry,
    fPosMUV2Entry,
    fPosFISCEntry,
    fPosMUV3Entry,
    fPosXWCMEntry;

  TLorentzVector
    fMomCedarEntry,
    fMomCedarExit,
    fMomGigaTrackerEntry,
    fMomGigaTrackerExit,
    fMomSpectrometerEntry,
    fMomMNP33Entry,
    fMomMNP33Exit,
    fMomRICHMirrorEntry,
    fMomCHODEntry,
    fMomLKrEntry,
    fMomMUV1Entry,
    fMomMUV2Entry,
    fMomFISCEntry,
    fMomMUV3Entry,
    fMomXWCMEntry;

  ClassDef(KinePart,1)
};

#endif
