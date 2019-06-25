// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-29
//
// ---------------------------------------------------------------

#ifndef DOWNSTREAMTRACK_HH
#define DOWNSTREAMTRACK_HH

#include "TVector2.h"
#include "TVector3.h"
#include <vector>

#include "TRecoSpectrometerCandidate.hh"
#include "TRecoCHANTICandidate.hh"
#include "TRecoCHODCandidate.hh"
#include "TRecoNewCHODHit.hh"
#include "TRecoLKrCandidate.hh"
#include "TRecoMUV1Candidate.hh"
#include "TRecoMUV2Candidate.hh"
#include "TRecoMUV3Candidate.hh"
#include "TRecoRICHCandidate.hh"
#include "TRecoRICHHit.hh"
#include "SpectrometerCHANTIAssociationOutput.hh"
#include "SpectrometerCHODAssociationOutput.hh"
#include "SpectrometerLKrAssociationOutput.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "SpectrometerRICHAssociationOutputSingleRing.hh"
#include "SpectrometerRICHAssociationOutputTrackCentredRing.hh"
#include "ProtoParticle.hh"

class EnergyCluster;

class DownstreamTrack : public ProtoParticle {

public:

  DownstreamTrack();
  ~DownstreamTrack();
  void Clear();
  void Print();
  void PrintRICHInformation();

  void SetTrackID(Int_t val)  { fTrackID = val;  }
  Int_t GetTrackID()          { return fTrackID; }

  /////////////////////////
  // Pointers to candidates

  void SetSpectrometerCandidate(TRecoSpectrometerCandidate* val)    { fSpectrometerCandidate = val;            }
  void AddCHANTICandidate      (TRecoCHANTICandidate* val)          { fCHANTICandidates.push_back(val);        }
  void AddCHODCandidate        (TRecoCHODCandidate* val)            { fCHODCandidates.push_back(val);          }
  void AddNewCHODCandidate     (TRecoNewCHODHit* val)               { fNewCHODCandidates.push_back(val);       }
  void SetRICHSingleRingTrkSeededCandidate(TRecoRICHCandidate* val) { fRICHSingleRingTrkSeededCandidate = val; }
  void AddLKrCandidate         (TRecoLKrCandidate* val)             { fLKrCandidates.push_back(val);           }
  void SetMUV1Candidate        (TRecoMUV1Candidate* val)            { fMUV1Candidate = val;                    }
  void SetMUV2Candidate        (TRecoMUV2Candidate* val)            { fMUV2Candidate = val;                    }
  void AddMUV3CandidateIndex   (Int_t val)                          { fMUV3CandidateIndices.push_back(val);    }
  void AddMUV3Candidate        (TRecoMUV3Candidate* val)            { fMUV3Candidates.push_back(val);          }

  TRecoSpectrometerCandidate* GetSpectrometerCandidate()            { return fSpectrometerCandidate; }
  TRecoCHANTICandidate*       GetCHANTICandidate(UInt_t);
  TRecoCHODCandidate*         GetCHODCandidate(UInt_t);
  TRecoNewCHODHit*            GetNewCHODCandidate(UInt_t);
  TRecoRICHCandidate*         GetRICHSingleRingTrkSeededCandidate() { return fRICHSingleRingTrkSeededCandidate; }
  TRecoLKrCandidate*          GetLKrCandidate(UInt_t);
  TRecoMUV1Candidate*         GetMUV1Candidate()                    { return fMUV1Candidate; }
  TRecoMUV2Candidate*         GetMUV2Candidate()                    { return fMUV2Candidate; }
  TRecoMUV3Candidate*         GetMUV3Candidate(UInt_t);     ///< Pointer to the i-th associated MUV3 candidate
  Int_t                       GetMUV3CandidateIndex(Int_t); ///< Index of the i-th associated MUV3 candidate in TRecoMUV3Event
  std::vector<TRecoMUV3Candidate*>& GetMUV3Candidates();

  /////////////////////////
  // Spectrometer variables

  Int_t    GetNChambers()                          { return fNChambers;              }
  void     SetNChambers(Int_t val)                 { fNChambers = val;               }
  Int_t    GetCharge()                             { return fCharge;                 }
  void     SetCharge(Int_t val)                    { fCharge = val;                  }
  Double_t GetChi2()                               { return fChi2;                   }
  void     SetChi2(Double_t val)                   { fChi2 = val;                    }
  Double_t GetMomentum()                           { return fMomentum;               }
  void     SetMomentum(Double_t val)               { fMomentum = val;                }
  Double_t GetChargeTimesMomentum()                { return fCharge*fMomentum;       }
  Double_t GetMomentumBeforeFit()                  { return fMomentumBeforeFit;      }
  void     SetMomentumBeforeFit(Double_t val)      { fMomentumBeforeFit = val;       }
  TVector3 GetMomentumBeforeMagnet()               { return fMomentumBeforeMagnet;   }
  void     SetMomentumBeforeMagnet(TVector3 val)   { fMomentumBeforeMagnet = val;    }
  TVector3 GetMomentumAfterMagnet()                { return fMomentumAfterMagnet;    }
  void     SetMomentumAfterMagnet(TVector3 val)    { fMomentumAfterMagnet = val;     }
  TVector3 GetPositionBeforeMagnet()               { return fPositionBeforeMagnet;   }
  void     SetPositionBeforeMagnet(TVector3 val)   { fPositionBeforeMagnet = val;    }
  TVector3 GetPositionAfterMagnet()                { return fPositionAfterMagnet;    }
  void     SetPositionAfterMagnet(TVector3 val)    { fPositionAfterMagnet = val;     }
  Double_t GetSlopeXBeforeMagnet()                 { return fSlopeXBeforeMagnet;     }
  void     SetSlopeXBeforeMagnet(Double_t val)     { fSlopeXBeforeMagnet = val;      }
  Double_t GetSlopeYBeforeMagnet()                 { return fSlopeYBeforeMagnet;     }
  void     SetSlopeYBeforeMagnet(Double_t val)     { fSlopeYBeforeMagnet = val;      }
  Double_t GetSlopeXAfterMagnet()                  { return fSlopeXAfterMagnet;      }
  void     SetSlopeXAfterMagnet(Double_t val)      { fSlopeXAfterMagnet = val;       }
  Double_t GetSlopeYAfterMagnet()                  { return fSlopeYAfterMagnet;      }
  void     SetSlopeYAfterMagnet(Double_t val)      { fSlopeYAfterMagnet = val;       }
  Double_t GetCovariance(Int_t i, Int_t j)         { return fCovariance[i][j];       }
  void     SetCovariance(Int_t i, Int_t j, Double_t val) { fCovariance[i][j] = val;  }
  TVector3 GetBeamAxisVertex()                     { return fBeamAxisVertex;         }
  void     SetBeamAxisVertex(TVector3 val)         { fBeamAxisVertex = val;          }
  Double_t GetBeamAxisCDA()                        { return fBeamAxisCDA;            }
  void     SetBeamAxisCDA(Double_t val)            { fBeamAxisCDA = val;             }
  TVector3 GetNominalBeamAxisVertex()              { return fNominalBeamAxisVertex;  }
  void     SetNominalBeamAxisVertex(TVector3 val)  { fNominalBeamAxisVertex = val;   }
  Double_t GetNominalBeamAxisCDA()                 { return fNominalBeamAxisCDA;     }
  void     SetNominalBeamAxisCDA(Double_t val)     { fNominalBeamAxisCDA = val;      }
  Double_t GetTrackTime()                          { return fTrackTime;              }
  void     SetTrackTime(Double_t val)              { fTrackTime = val;               }
  Bool_t   GetIsFake()                             { return fIsFake;                 }
  void     SetIsFake(Bool_t val)                   { fIsFake = val;                  }
  Double_t GetBeta(Double_t mass);

  Double_t xAtBeforeMagnet(Double_t z); ///< X position of track before magnet in a certain Z plane
  Double_t yAtBeforeMagnet(Double_t z); ///< Y position of track before magnet in a certain Z plane
  Double_t rAtBeforeMagnet(Double_t z); ///< Distance to Z axis of track before magnet in a certain Z plane
  Double_t xAtAfterMagnet (Double_t z); ///< X position of track after magnet in a certain Z plane
  Double_t yAtAfterMagnet (Double_t z); ///< Y position of track after magnet in a certain Z plane
  Double_t rAtAfterMagnet (Double_t z); ///< Distance to Z axis of track after magnet in a certain Z plane

  Double_t xAt(Double_t z); ///< Either xAtBeforeMagnet() or xAtAfterMagnet(), depending on the Z plane
  Double_t yAt(Double_t z); ///< Either yAtBeforeMagnet() or yAtAfterMagnet(), depending on the Z plane
  Double_t rAt(Double_t z); ///< Either rAtBeforeMagnet() or rAtAfterMagnet(), depending on the Z plane

  ///////////////////
  // CHANTI variables

  void SetCHANTIAssociationOutput(const SpectrometerCHANTIAssociationOutput &val) { fCHANTIAssociationOutput = val;  }
  SpectrometerCHANTIAssociationOutput GetCHANTIAssociationOutput()         { return fCHANTIAssociationOutput; }

  Bool_t   CHANTIAssociationExists()      { return fCHANTIAssociationOutput.isAssociated();           }
  Int_t    GetNCHANTIAssociationRecords() { return fCHANTIAssociationOutput.GetNAssociationRecords(); }

  //////////////////////////////////////////////////////////////////////////
  // RICH variables
  //
  // Typical use: e.g. to identify muons and discriminate against pions:
  // if (Track.GetRICHLikelihoodMuon()>Track.GetRICHLikelihoodPion()) { ... }
  // or
  // if (Track.GetRICHMostLikelyHypothesis()==kRICHHypothesisMuon) { ... }

  // Valid RICH hypotheses and the corresponding numerical values:
  // kRICHHypothesisBackground 0
  // kRICHHypothesisElectron   1
  // kRICHHypothesisMuon       2
  // kRICHHypothesisPion       3
  // kRICHHypothesisKaon       4
  // kRICHHypothesisMultiple  99

  // RICH "user outputs"
  // Note: likelihood, time and number of hits are not defined for the hypothesis kRICHHypothesisMultiple=99
  Bool_t   RICHAssociationSuccessful()              { return fRICHAssociationSuccessful;                                }
  void     SetRICHAssociationSuccessful(Bool_t val) { fRICHAssociationSuccessful = val;                                 }
  Double_t GetTrackTimeForRICHAssociation()         { return fTrackTimeForRICHAssociation;                              }
  void     SetTrackTimeForRICHAssociation(Double_t val) { fTrackTimeForRICHAssociation = val;                           }
  Int_t    GetRICHNumberOfInTimeHits()              { return fRICHNumberOfInTimeHits;                                   }
  void     SetRICHNumberOfInTimeHits(Int_t val)     { fRICHNumberOfInTimeHits = val;                                    }
  Int_t    GetRICHNumberOfOutOfTimeHits()           { return fRICHNumberOfOutOfTimeHits;                                }
  void     SetRICHNumberOfOutOfTimeHits(Int_t val)  { fRICHNumberOfOutOfTimeHits = val;                                 }
  Double_t GetRICHLikelihood(Int_t i)               { return (i>=0 && i<MaxHypos) ? fRICHLikelihood[i] : 0.0;           }
  Double_t GetRICHRingTime(Int_t i)                 { return (i>=0 && i<MaxHypos) ? fRICHRingTime[i] : -999.;           }
  Double_t GetRICHTime()                            { return GetRICHRingTime(fRICHMostLikelyHypothesis);                } ///< RICH ring time in the most likely hypothesis; -999 if no hits assigned, multiple most hypotheses are most likely, or RICH association not successful
  Bool_t   RICHRingTimeExists(Int_t i)              { return (i>=0 && i<MaxHypos) ? (fRICHRingNHits[i]>0) : false;      }
  Int_t    GetRICHRingNHits(Int_t i)                { return (i>=0 && i<MaxHypos) ? fRICHRingNHits[i] : -999.;          }
  Double_t GetRICHRingPredictedNHits(Int_t i)       { return (i>=0 && i<MaxHypos) ? fRICHRingPredictedNHits[i] : -999.; }
  Double_t GetRICHLikelihoodBackground()            { return fRICHLikelihood[kRICHHypothesisBackground];                }
  Double_t GetRICHLikelihoodElectron()              { return fRICHLikelihood[kRICHHypothesisElectron];                  }
  Double_t GetRICHLikelihoodMuon()                  { return fRICHLikelihood[kRICHHypothesisMuon];                      }
  Double_t GetRICHLikelihoodPion()                  { return fRICHLikelihood[kRICHHypothesisPion];                      }
  Double_t GetRICHLikelihoodKaon()                  { return fRICHLikelihood[kRICHHypothesisKaon];                      }
  void     SetRICHLikelihood(Int_t i, Double_t val) { if (i>=0 && i<MaxHypos) fRICHLikelihood[i] = val;                 }
  void     SetRICHRingTime(Int_t i, Double_t val)   { if (i>=0 && i<MaxHypos) fRICHRingTime[i] = val;                   }
  void     SetRICHRingNHits(Int_t i, Int_t val)     { if (i>=0 && i<MaxHypos) fRICHRingNHits[i] = val;                  }
  void     SetRICHRingPredictedNHits(Int_t i, Double_t val) { if (i>=0 && i<MaxHypos) fRICHRingPredictedNHits[i] = val; }
  Int_t    GetRICHRingNHitsInMostLikelyHypothesis()
  { return (fRICHMostLikelyHypothesis>=0 && fRICHMostLikelyHypothesis<MaxHypos) ?
      fRICHRingNHits[fRICHMostLikelyHypothesis] : -999.; }

  Int_t    GetRICHMostLikelyHypothesis()            { return fRICHMostLikelyHypothesis; }
  void     SetRICHMostLikelyHypothesis(Int_t val)   { fRICHMostLikelyHypothesis = val;  }

  // RICH "expert outputs"
  TVector2 GetRICHRingPredictedCentrePosition()              { return fRICHRingPredictedCentrePosition; }
  void     SetRICHRingPredictedCentrePosition(TVector2 val)  { fRICHRingPredictedCentrePosition = val;  }
  TVector2 GetRICHRingCentrePosition()                       { return fRICHRingCentrePosition;          }
  void     SetRICHRingCentrePosition(TVector2 val)           { fRICHRingCentrePosition = val;           }
  TVector2 GetRICHRingCentrePositionError()                  { return fRICHRingCentrePosition;          }
  void     SetRICHRingCentrePositionError(TVector2 val)      { fRICHRingCentrePositionError = val;      }
  Double_t GetRICHRingPredictedRadius(Int_t i)               { return fRICHRingPredictedRadius[i];      }
  void     SetRICHRingPredictedRadius(Int_t i, Double_t val) { fRICHRingPredictedRadius[i] = val;       }
  Double_t GetRICHRingRadius()                               { return fRICHRingRadius;                  }
  void     SetRICHRingRadius(Double_t val)                   { fRICHRingRadius = val;                   }
  Double_t GetRICHRingRadiusError()                          { return fRICHRingRadiusError;             }
  void     SetRICHRingRadiusError(Double_t val)              { fRICHRingRadiusError = val;              }
  Double_t GetRICHRingFitChi2()                              { return fRICHRingFitChi2;                 }
  void     SetRICHRingFitChi2(Double_t val)                  { fRICHRingFitChi2 = val;                  }
  void     SetRICHAssignedHits (Int_t, const std::vector<TRecoRICHHit*>&);
  std::vector<TRecoRICHHit*> GetRICHAssignedHits(Int_t);

  // Alternative RICH association - track seeded
  Int_t    GetRICHSingleRingTrkSeededCandidateID()                     { return fRICHSingleRingTrkSeededCandidateID;         }
  void     SetRICHSingleRingTrkSeededCandidateID(Int_t val)            { fRICHSingleRingTrkSeededCandidateID = val;          }
  TVector2 GetRICHSingleRingTrkSeededCentrePosition()                  { return fRICHSingleRingTrkSeededCentrePosition;      }
  void     SetRICHSingleRingTrkSeededCentrePosition(TVector2 val)      { fRICHSingleRingTrkSeededCentrePosition = val;       }
  TVector2 GetRICHSingleRingTrkSeededCentrePositionError()             { return fRICHSingleRingTrkSeededCentrePositionError; }
  void     SetRICHSingleRingTrkSeededCentrePositionError(TVector2 val) { fRICHSingleRingTrkSeededCentrePositionError = val;  }
  Double_t GetRICHSingleRingTrkSeededRadius()                          { return fRICHSingleRingTrkSeededRadius;              }
  void     SetRICHSingleRingTrkSeededRadius(Double_t val)              { fRICHSingleRingTrkSeededRadius = val;               }
  Double_t GetRICHSingleRingTrkSeededRadiusError()                     { return fRICHSingleRingTrkSeededRadiusError;         }
  void     SetRICHSingleRingTrkSeededRadiusError(Double_t val)         { fRICHSingleRingTrkSeededRadiusError = val;          }
  Double_t GetRICHSingleRingTrkSeededFitChi2()                         { return fRICHSingleRingTrkSeededFitChi2;             }
  void     SetRICHSingleRingTrkSeededFitChi2(Double_t val)             { fRICHSingleRingTrkSeededFitChi2 = val;              }
  Double_t GetRICHSingleRingTrkSeededTime()                            { return fRICHSingleRingTrkSeededTime;                }
  void     SetRICHSingleRingTrkSeededTime(Double_t val)                { fRICHSingleRingTrkSeededTime = val;                 }
  Double_t GetRICHSingleRingTrkSeededMass()                            { return fRICHSingleRingTrkSeededMass;                }
  void     SetRICHSingleRingTrkSeededMass(Double_t val)                { fRICHSingleRingTrkSeededMass = val;                 }
  Double_t GetRICHSingleRingTrkSeededTrkDist()                         { return fRICHSingleRingTrkSeededTrkDist;             }
  void     SetRICHSingleRingTrkSeededTrkDist(Double_t val)             { fRICHSingleRingTrkSeededTrkDist = val;              }
  Int_t    GetRICHSingleRingTrkSeededNHits()                           { return fRICHSingleRingTrkSeededNHits;               }
  void     SetRICHSingleRingTrkSeededNHits(Int_t val)                  { fRICHSingleRingTrkSeededNHits = val;                }

  // Alternative RICH association - track centred
  Int_t    GetRICHSingleRingTrkCentredCandidateID()                { return fRICHSingleRingTrkCentredCandidateID;    }
  void     SetRICHSingleRingTrkCentredCandidateID(Int_t val)       { fRICHSingleRingTrkCentredCandidateID = val;     }
  TVector2 GetRICHSingleRingTrkCentredCentrePosition()             { return fRICHSingleRingTrkCentredCentrePosition; }
  void     SetRICHSingleRingTrkCentredCentrePosition(TVector2 val) { fRICHSingleRingTrkCentredCentrePosition = val;  }
  Double_t GetRICHSingleRingTrkCentredRadius()                     { return fRICHSingleRingTrkCentredRadius;         }
  void     SetRICHSingleRingTrkCentredRadius(Double_t val)         { fRICHSingleRingTrkCentredRadius = val;          }
  Double_t GetRICHSingleRingTrkCentredFitChi2()                    { return fRICHSingleRingTrkCentredFitChi2;        }
  void     SetRICHSingleRingTrkCentredFitChi2(Double_t val)        { fRICHSingleRingTrkCentredFitChi2 = val;         }
  Double_t GetRICHSingleRingTrkCentredTime()                       { return fRICHSingleRingTrkCentredTime;           }
  void     SetRICHSingleRingTrkCentredTime(Double_t val)           { fRICHSingleRingTrkCentredTime = val;            }
  Double_t GetRICHSingleRingTrkCentredMass()                       { return fRICHSingleRingTrkCentredMass;           }
  void     SetRICHSingleRingTrkCentredMass(Double_t val)           { fRICHSingleRingTrkCentredMass = val;            }
  Int_t    GetRICHSingleRingTrkCentredNHits()                      { return fRICHSingleRingTrkCentredNHits;          }
  void     SetRICHSingleRingTrkCentredNHits(Int_t val)             { fRICHSingleRingTrkCentredNHits = val;           }

  /////////////////
  // CHOD variables

  void SetCHODAssociationOutput(const SpectrometerCHODAssociationOutput &val) { fCHODAssociationOutput = val;  }
  SpectrometerCHODAssociationOutput GetCHODAssociationOutput()         { return fCHODAssociationOutput; }

  Bool_t   isCHODShowerLikeEvent()      { return fCHODAssociationOutput.GetShowerFlag(); } ///< True means that in this event N(ChodHits)-2*N(Tracks)>30; using track CHOD times is not recommended in this case; this flag has the same value for all tracks in the event
  Bool_t   CHODAssociationExists()      { return fCHODAssociationOutput.isAssociated();           }
  Int_t    GetNCHODAssociationRecords() { return fCHODAssociationOutput.GetNAssociationRecords(); }
  TVector3 GetCHODCandidatePosition(UInt_t);
  Double_t GetCHODCandidateTime(UInt_t); ///< Time of i-th associated CHOD candidate
  Double_t GetCHODTime();    ///< Time of the best matching candidate; -999 if no match
  Bool_t   CHODTimeExists(); ///< Is CHOD time defined?

  ////////////////////
  // NewCHOD variables

  Bool_t   NewCHODAssociationExists()           { return !fNewCHODCandidates.empty(); }
  Int_t    GetNNewCHODAssociationRecords()      { return fNewCHODCandidates.size();   }
  Double_t GetNewCHODSearchRadius()             { return fNewCHODSearchRadius;        }
  void	   SetNewCHODSearchRadius(Double_t val) { fNewCHODSearchRadius = val;         }
  Int_t    GetNewCHODBestRecordID()             { return fNewCHODBestRecordID;        }
  void	   SetNewCHODBestRecordID(Double_t val) { fNewCHODBestRecordID = val;         }

  TVector3 GetNewCHODCandidatePosition(UInt_t);
  Int_t    GetNewCHODCandidateTileID(UInt_t);
  Double_t GetNewCHODCandidateX(UInt_t);
  Double_t GetNewCHODCandidateY(UInt_t);
  Double_t GetNewCHODCandidateTime(UInt_t); ///< Time of i-th associated NewCHOD RecoHit
  Double_t GetNewCHODTime(); ///< Time of the best matching candidate; -999 if no match
  Bool_t   NewCHODTimeExists(); ///< Is NewCHOD time defined?

  ////////////////
  // LKr variables

  void SetLKrAssociationOutput(const SpectrometerLKrAssociationOutput &val) { fLKrAssociationOutput = val;  }
  SpectrometerLKrAssociationOutput GetLKrAssociationOutput()                { return fLKrAssociationOutput; }

  Bool_t   LKrAssociationExists()      { return fLKrAssociationOutput.isAssociated();            }
  Int_t    GetLKrNAssociatedClusters() { return fLKrAssociationOutput.GetNAssociationRecords();  }
  Double_t GetLKrTotalEnergy()         { return fLKrAssociationOutput.GetTotalEnergy();          } ///< Total energy summed over clusters within the search radius [MeV]
  Double_t GetLKrTotalEoP()            { return fLKrAssociationOutput.GetTotalEoP();             } ///< Track energy-to-momentum ratio (E/p) based on total energy summed over clusters within an extended search radius and an extended time window

  Double_t GetLKrEnergy();           ///< Energy of the geometrically closest LKr in-time cluster [MeV]
  Double_t GetLKrEoP();              ///< E/p based on the geometrically closest in-time LKr cluster
  Double_t GetLKrClusterX();         ///< x coordinate of the geometrically closest in-time LKr cluster [mm]
  Double_t GetLKrClusterY();         ///< y coordinate of the geometrically closest in-time LKr cluster [mm]
  Double_t GetLKrClusterDistance();  ///< Distance in LKr front plane to the geometrically closest in-time LKr cluster [mm]
  Double_t GetLKrClusterTime();      ///< Time of the geometrically closest LKr in-time cluster [ns]
  Double_t GetLKrClusterDDeadCell(); ///< Distance to dead cell of the geometrically closest in-time LKr cluster [mm]
  Int_t    GetLKrXCellID();          ///< x ID of the LKr cell associated to the track projection [mm]
  Int_t    GetLKrYCellID();          ///< y ID of the LKr cell associated to the track projection [mm]

  void AddClusterForWhichThisTrackIsBestMatch(EnergyCluster* cluster)
  { fClustersForWhichThisTrackIsBestMatch.push_back(cluster); }
  std::vector<EnergyCluster*> GetClustersForWhichThisTrackIsBestMatch()
  { return fClustersForWhichThisTrackIsBestMatch; }
  EnergyCluster* GetClusterForWhichThisTrackIsBestMatch (UInt_t iCluster);

  /////////////////
  // MUV1 variables

  Bool_t   MUV1SimpleAssociationExists()                  { return fMUV1SimpleAssociationExists;   }
  void     SetMUV1SimpleAssociationExists(Bool_t val)     { fMUV1SimpleAssociationExists = val;    }
  Double_t GetMUV1SimpleAssociationTime()                 { return fMUV1SimpleAssociationTime;     }
  void     SetMUV1SimpleAssociationTime(Double_t val)     { fMUV1SimpleAssociationTime = val;      }
  Double_t GetMUV1SimpleAssociationEnergy()               { return fMUV1SimpleAssociationEnergy;   }
  void     SetMUV1SimpleAssociationEnergy(Double_t val)   { fMUV1SimpleAssociationEnergy = val;    }
  TVector2 GetMUV1SimpleAssociationPosition()	          { return fMUV1SimpleAssociationPosition; }
  void     SetMUV1SimpleAssociationPosition(TVector2 val) { fMUV1SimpleAssociationPosition = val;  } 

  Bool_t   MUV1AssociationExists()                        { return fMUV1AssociationExists;         }
  void     SetMUV1AssociationExists(Bool_t val)           { fMUV1AssociationExists = val;          }
  TVector2 GetMUV1ClusterPosition()                       { return fMUV1ClusterPosition;           }
  void     SetMUV1ClusterPosition(TVector2 val)           { fMUV1ClusterPosition = val;            }
  Double_t GetMUV1ClusterEnergy()                         { return fMUV1ClusterEnergy;             }
  void     SetMUV1ClusterEnergy(Double_t val)             { fMUV1ClusterEnergy = val;              }
  Double_t GetMUV1ClusterTime()                           { return fMUV1ClusterTime;               }
  void     SetMUV1ClusterTime(Double_t val)               { fMUV1ClusterTime = val;                }

  /////////////////
  // MUV2 variables

  Bool_t   MUV2SimpleAssociationExists()                  { return fMUV2SimpleAssociationExists;   }
  void     SetMUV2SimpleAssociationExists(Bool_t val)     { fMUV2SimpleAssociationExists = val;    }
  Double_t GetMUV2SimpleAssociationTime()                 { return fMUV2SimpleAssociationTime;     }
  void     SetMUV2SimpleAssociationTime(Double_t val)     { fMUV2SimpleAssociationTime = val;      }
  Double_t GetMUV2SimpleAssociationEnergy()               { return fMUV2SimpleAssociationEnergy;   }
  void     SetMUV2SimpleAssociationEnergy(Double_t val)   { fMUV2SimpleAssociationEnergy = val;    }
  TVector2 GetMUV2SimpleAssociationPosition()             { return fMUV2SimpleAssociationPosition; }
  void     SetMUV2SimpleAssociationPosition(TVector2 val) { fMUV2SimpleAssociationPosition = val;  }

  Bool_t   MUV2AssociationExists()                        { return fMUV2AssociationExists;         }
  void     SetMUV2AssociationExists(Bool_t val)           { fMUV2AssociationExists = val;          }
  TVector2 GetMUV2ClusterPosition()                       { return fMUV2ClusterPosition;           }
  void     SetMUV2ClusterPosition(TVector2 val)           { fMUV2ClusterPosition = val;            }
  Double_t GetMUV2ClusterEnergy()                         { return fMUV2ClusterEnergy;             }
  void     SetMUV2ClusterEnergy(Double_t val)             { fMUV2ClusterEnergy = val;              }
  Double_t GetMUV2ClusterTime()                           { return fMUV2ClusterTime;               }
  void     SetMUV2ClusterTime(Double_t val)               { fMUV2ClusterTime = val;                }

  ////////////////////////////////////////////
  // Total calorimetric energy (LKr+MUV1+MUV2)

  Double_t GetCalorimetricEnergy()             { return fCalorimetricEnergy; }
  void     SetCalorimetricEnergy(Double_t val) { fCalorimetricEnergy = val;  }

  /////////////////
  // MUV3 variables

  Bool_t   MUV3AssociationExists() { return !fMUV3Candidates.empty(); } ///< Is the track geometrically associated with a MUV3 candidate?
  Bool_t   MUV3InnerAssociationExists(); ///< Is the track geometrically associated with a MUV3 candidate in an inner MUV3 tile?
  Bool_t   MUV3OuterAssociationExists(); ///< Is the track geometrically associated with a MUV3 candidate in an outer MUV3 tile?
  Bool_t   MUV3InTimeAssociationExists(Double_t, Double_t); ///< Is an in-time MUV3 candidate i the (t0-dt, t0+dt) time interval associated to the track?
  Bool_t   MUV3InTimeInnerAssociationExists(Double_t, Double_t); ///< Is an in-time MUV3 inner tile candidate associated?
  Bool_t   MUV3InTimeOuterAssociationExists(Double_t, Double_t); ///< Is an in-time MUV3 outer tile candidate associated?
  Bool_t   MUV3OutOfTimeAssociationExists(Double_t, Double_t, Double_t, Double_t, Double_t); ///< Is a MUV3 candidate in the (t0+t1,t0+t2) or the (t0+t3,t0+t4) time range associated to the track?
  Bool_t   MUV3OutOfTimeInnerAssociationExists(Double_t, Double_t, Double_t, Double_t, Double_t); ///< Is an inner tile MUV3 candidate in the (t0+t1,t0+t2) or the (t0+t3,t0+t4) time range associated?
  Bool_t   MUV3OutOfTimeOuterAssociationExists(Double_t, Double_t, Double_t, Double_t, Double_t); ///< Is an outer tile MUV3 candidate in the (t0+t1,t0+t2) or the (t0+t3,t0+t4) time range associated?

  Int_t    GetNMUV3AssociationRecords() { return fMUV3Candidates.size(); }
  Int_t    GetNMUV3InnerAssociationRecords();
  Int_t    GetNMUV3OuterAssociationRecords();
  Int_t    GetNMUV3InTimeAssociationRecords(Double_t, Double_t);
  Int_t    GetNMUV3InTimeInnerAssociationRecords(Double_t, Double_t);
  Int_t    GetNMUV3InTimeOuterAssociationRecords(Double_t, Double_t);
  Int_t    GetNMUV3OutOfTimeAssociationRecords(Double_t, Double_t, Double_t, Double_t, Double_t);
  Int_t    GetNMUV3OutOfTimeInnerAssociationRecords(Double_t, Double_t, Double_t, Double_t, Double_t);
  Int_t    GetNMUV3OutOfTimeOuterAssociationRecords(Double_t, Double_t, Double_t, Double_t, Double_t);

  Int_t    GetMUV3CandidateTileID(Int_t); ///< Tile ID for i-th associated MUV3 candidate
  Int_t    GetMUV3InTimeCandidateTileID(Double_t, Double_t, Int_t); ///< Tile ID for i-th in-time associated MUV3 candidate
  Int_t    GetMUV3InTimeInnerCandidateTileID(Double_t, Double_t, Int_t);
  Int_t    GetMUV3InTimeOuterCandidateTileID(Double_t, Double_t, Int_t);
  TVector3 GetMUV3CandidatePosition(Int_t);
  Double_t GetMUV3Time(Int_t); ///< Time of the i-th geometrically associated MUV3 candidate
  Double_t GetMUV3SearchRadius()             { return fMUV3SearchRadius; }
  void     SetMUV3SearchRadius(Double_t val) { fMUV3SearchRadius = val;  }
  Double_t GetMUV3DistanceToHole(); ///< Distance to the central MUV3 hole (R=103mm) in the MUV3 front plane (z=246800mm)
  Double_t GetMUV3DistanceToEdge(); ///< Distance to MUV3 edge (|x|,|y|<1320mm) in the MUV3 front plane (z=246800mm)

  ////////////////////////////////////
  // DownstreamTrack/KinePart matching

  Bool_t   KinePartMatched()                    { return (fMatchedKinePartIndex>=0); } ///< Is a KinePart matched (always false for data)?
  Int_t    GetMatchedKinePartIndex()            { return fMatchedKinePartIndex; } ///< Get KinePart index for MCTruth particle (best) matching this track
  void     SetMatchedKinePartIndex(Int_t val)   { fMatchedKinePartIndex = val;  } ///< Set KinePart index for MCTruth particle (best) matching this track
  Double_t GetMatchedKinePartChi2()             { return fMatchedKinePartChi2;  } ///< Get MCTruth-DownstreamTrack matching Chi2 for (best - i.e lowest Chi2) [Calculated using q,p,dx/dz,dy/dz]
  void     SetMatchedKinePartChi2(Double_t val) { fMatchedKinePartChi2 = val;   } ///< Set MCTruth-DownstreamTrack matching Chi2 for (best - i.e lowest Chi2) [Calculated using q,p,dx/dz,dy/dz]

  TVector3 GetTrueThreeMomentum()             { return fTrueThreeMomentum; }
  void     SetTrueThreeMomentum(TVector3 val) { fTrueThreeMomentum = val;  }
private:

  Int_t fTrackID; ///< ID of the spectrometer track

  TRecoSpectrometerCandidate* fSpectrometerCandidate; ///< Pointer to the spectrometer candidate
  std::vector<TRecoCHANTICandidate*> fCHANTICandidates;
  std::vector<TRecoCHODCandidate*>   fCHODCandidates;
  std::vector<TRecoNewCHODHit*>      fNewCHODCandidates;
  TRecoRICHCandidate*                fRICHSingleRingTrkSeededCandidate;
  std::vector<TRecoLKrCandidate*>    fLKrCandidates;
  TRecoMUV1Candidate*                fMUV1Candidate;
  TRecoMUV2Candidate*                fMUV2Candidate;
  std::vector<Int_t>                 fMUV3CandidateIndices;
  std::vector<TRecoMUV3Candidate*>   fMUV3Candidates;

  Int_t    fNChambers;             ///< Number of straw chambers used to build the track
  Int_t    fCharge;                ///< Electric charge (+1 or -1)
  Double_t fChi2;                  ///< Chi2 of the track fit
  Double_t fMomentum;              ///< Track momentum measured by the spectrometer (absolute value)
  Double_t fMomentumBeforeFit;     ///< Track momentum measured by the spectrometer, before fit (absolute value)
  TVector3 fMomentumBeforeMagnet;  ///< Three-momentum before the MNP33 magnet
  TVector3 fPositionBeforeMagnet;  ///< A point along the trajectory before the MNP33 magnet
  TVector3 fMomentumAfterMagnet;   ///< Three-momentum after the MNP33 magnet
  TVector3 fPositionAfterMagnet;   ///< A point along the trajectory after the MNP33 magnet
  Double_t fSlopeXBeforeMagnet;    ///< Track slope in XZ plane before the MNP33 magnet
  Double_t fSlopeYBeforeMagnet;    ///< Track slope in YZ plane before the MNP33 magnet
  Double_t fSlopeXAfterMagnet;     ///< Track slope in XZ plane after the MNP33 magnet
  Double_t fSlopeYAfterMagnet;     ///< Track slope in YZ plane after the MNP33 magnet
  Double_t fCovariance[5][5];      ///< Track covariance matrix: (x', y', x, y, 1/p)
  TVector3 fBeamAxisVertex;        ///< Track + run-dependent beam axis vertex
  Double_t fBeamAxisCDA;           ///< CDA with respect to the run-dependent beam axis
  TVector3 fNominalBeamAxisVertex; ///< Track + nominal beam axis vertex
  Double_t fNominalBeamAxisCDA;    ///< CDA with respect to the nominal beam axis
  Double_t fTrackTime;             ///< Spectrometer track time
  Bool_t   fIsFake;                ///< Classified as fake track?

  SpectrometerCHANTIAssociationOutput fCHANTIAssociationOutput; ///< Output of SpectrometerCHANTIAssociation
  SpectrometerCHODAssociationOutput   fCHODAssociationOutput;   ///< Output of SpectrometerCHODAssociation

  Bool_t   fRICHAssociationSuccessful;         ///< There is at least one RICH hit in the event (false e.g. for a single track below Cherenkov threshold)
  Double_t fTrackTimeForRICHAssociation;       ///< Track time used by SpectrometerRICHAssociation algorithm
  Int_t    fRICHNumberOfInTimeHits;            ///< Number of RICH hits found to be in time with the track
  Int_t    fRICHNumberOfOutOfTimeHits;         ///< Number of RICH hits out of time (they are not used for RICH association)
  Int_t    fRICHMostLikelyHypothesis;          ///< Most likely hypothesis (0...4 = bkg/e/mu/pi/K; 99 = multiple; 0 if association not successful)
  Double_t fRICHLikelihood[MaxHypos];          ///< Normalized likelihoods of 5 RICH PID hypotheses (bkg, e, mu, pi, K)
  Double_t fRICHRingTime[MaxHypos];            ///< Ring times for 5 RICH PID hypotheses (bkg, e, mu, pi, K)
  Int_t    fRICHRingNHits[MaxHypos];           ///< Number of hits in a ring for 5 RICH PID hypotheses (bkg, e, mu, pi, K)
  Double_t fRICHRingPredictedNHits[MaxHypos];  ///< Predicted number of hits in a ring for 5 RICH PID hypotheses (bkg, e, mu, pi, K)
  TVector2 fRICHRingPredictedCentrePosition;   ///< Position of the predicted RICH ring centre
  TVector2 fRICHRingCentrePosition;            ///< Position of the RICH ring centre for best ID hypothesis. Computed if >=4 hits, (999999,999999) otherwise.
  TVector2 fRICHRingCentrePositionError;       ///< Error on position of the RICH ring centre for best ID hypothesis. Computed if >=4 hits, (999999,999999) otherwise.
  Double_t fRICHRingPredictedRadius[MaxHypos]; ///< Predicted ring radii for different hypotheses
  Double_t fRICHRingRadius;                    ///< Radius of the RICH ring for best ID hypothesis
  Double_t fRICHRingRadiusError;               ///< Error on radius of the RICH ring for best ID hypothesis. Computed if >=4 hits, 999999 otherwise.
  Double_t fRICHRingFitChi2;                   ///< Chi2 of RICH ring fit for best ID hypothesis
  std::vector<TRecoRICHHit*> fRICHAssignedHits[MaxHypos]; ///< Hits assigned to hypothesis

  Int_t    fRICHSingleRingTrkSeededCandidateID;         ///< TimeCandidate index from RICHReconstruction
  TVector2 fRICHSingleRingTrkSeededCentrePosition;      ///< Position of the RICH ring 
  TVector2 fRICHSingleRingTrkSeededCentrePositionError; ///< Error on position of the RICH ring
  Double_t fRICHSingleRingTrkSeededRadius;              ///< Radius of the RICH ring 
  Double_t fRICHSingleRingTrkSeededRadiusError;         ///< Error on radius of the RICH ring 
  Double_t fRICHSingleRingTrkSeededFitChi2;             ///< Chi2 of RICH ring fit 
  Double_t fRICHSingleRingTrkSeededTrkDist;
  Double_t fRICHSingleRingTrkSeededTime;
  Double_t fRICHSingleRingTrkSeededMass;
  Double_t fRICHSingleRingTrkSeededNHits;

  Int_t    fRICHSingleRingTrkCentredCandidateID;
  TVector2 fRICHSingleRingTrkCentredCentrePosition;
  Double_t fRICHSingleRingTrkCentredRadius;
  Double_t fRICHSingleRingTrkCentredFitChi2;
  Double_t fRICHSingleRingTrkCentredTime;
  Double_t fRICHSingleRingTrkCentredMass;
  Double_t fRICHSingleRingTrkCentredNHits;

  SpectrometerLKrAssociationOutput fLKrAssociationOutput;  ///< Output of SpectrometerLKrAssociation
  std::vector<EnergyCluster*> fClustersForWhichThisTrackIsBestMatch; ///< Container of the EnergyClusters for which this track is the best match

  Bool_t   fMUV1SimpleAssociationExists;   ///< Is a MUV1 cluster associated to the track (simple geometrical association)
  Double_t fMUV1SimpleAssociationTime;     ///< Time of MUV1 cluster from simple association
  Double_t fMUV1SimpleAssociationEnergy;   ///< Energy of MUV1 cluster from simple association
  TVector2 fMUV1SimpleAssociationPosition; ///< Position of a MUV1 cluster from simple association

  Bool_t   fMUV2SimpleAssociationExists;   ///< Is a MUV2 cluster associated to the track (simple geometrical association)
  Double_t fMUV2SimpleAssociationTime;     ///< Time of MUV2 cluster from simple association
  Double_t fMUV2SimpleAssociationEnergy;   ///< Energy of MUV2 cluster from simple association
  TVector2 fMUV2SimpleAssociationPosition; ///< Position of a MUV2 cluster from simple association

  Bool_t   fMUV1AssociationExists; ///< Is a MUV1 cluster associated to the track?
  TVector2 fMUV1ClusterPosition;   ///< Position of the MUV1 cluster associated to the track
  Double_t fMUV1ClusterTime;       ///< Time of the MUV1 cluster matching the track
  Double_t fMUV1ClusterEnergy;     ///< MUV1 energy associated to the track

  Bool_t   fMUV2AssociationExists; ///< Is a MUV2 cluster associated to the track?
  TVector2 fMUV2ClusterPosition;   ///< Position of the MUV2 cluster associated to the track
  Double_t fMUV2ClusterTime;       ///< Time of the MUV2 cluster matching the track
  Double_t fMUV2ClusterEnergy;     ///< MUV2 energy associated to the track

  Double_t fNewCHODSearchRadius;   ///< Search radius at the NewCHOD plane
  Int_t    fNewCHODBestRecordID;   ///< Index of "best" (i.e. spacially closest) NewCHOD association record

  Double_t fMUV3SearchRadius;      ///< Search radius at the MUV3 plane

  Double_t fCalorimetricEnergy;    ///< Calorimetric energy (LKr+MUV1+MUV2) associated to the track

  Int_t    fMatchedKinePartIndex;  ///< Index of KinePart (MC truth) particle best matching this track; -1 if no good match
  Double_t fMatchedKinePartChi2;   ///< Track-KinePart matching chi2 for calculated using p,dx/dz,dy/dz: 99999 if no good match
  TVector3 fTrueThreeMomentum;     ///< True 3-momentum of the matched KinePart
};

#endif
