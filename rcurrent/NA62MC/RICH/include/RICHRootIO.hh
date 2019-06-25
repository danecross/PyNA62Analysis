#ifndef INCLUDE_RICHROOTIO_HH
#define INCLUDE_RICHROOTIO_HH 1

#include "NA62VRootIO.hh"

// Include files
#include "TTree.h"
#include "TBranch.h"
#include "globals.hh"
#include "G4TwoVector.hh"

class TRICHEvent;
class RICHGeometryParameters;
class RICHMaterialParameters;

class TVector2;

class RICHRootIO : public NA62VRootIO {
    public:

        RICHRootIO();
        virtual ~RICHRootIO();

        static RICHRootIO* GetInstance();
        void NewRun();
        void EndRun();
        void SaveEvent(const G4Event*);
        void Close();

    private:

        TBranch* fRICHBranch;
        TRICHEvent *fEvent;
        RICHGeometryParameters * GeoPars;
        RICHMaterialParameters * MatPars;

        G4int fNChannels;
        G4int fNPMs;
        TVector2 * fPMsPositions;
        G4int ** fPMsIDs;
        G4double fInputDisplacementWRTXaxis;
        G4TwoVector* fPMTsDiskCenter;
        G4TwoVector* fPMTsDiskCenter_Jura_lab;
        G4TwoVector* fPMTsDiskCenter_Saleve_lab;
        G4int * fGeoIDs;
 

};
#endif // RICHRootIO_H
