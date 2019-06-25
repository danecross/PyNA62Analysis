// --------------------------------------------------------------------
// History:
//
//           15.11.2012 Sergey Podolsky (siarhei.padolski@cern.ch)
//
// --------------------------------------------------------------------

#include "LKrEMShowers.hh"
#include "G4Electron.hh"
#include "G4Positron.hh"
#include "G4VProcess.hh"
#include "G4Gamma.hh"
#include "G4ios.hh"
#include "G4LogicalVolume.hh"
#include "geomdefs.hh"
#include "G4ParticleTable.hh"
#include "G4TransportationManager.hh"
#include "G4VSensitiveDetector.hh"
#include "G4TouchableHistory.hh"
#include "G4GeometryManager.hh"
#include "G4PionPlus.hh"
#include "boost/iostreams/filtering_stream.hpp"
#include "boost/archive/text_oarchive.hpp"
#include "boost/archive/text_iarchive.hpp"
#include "boost/iostreams/categories.hpp"  // source_tag
#include "boost/archive/binary_oarchive.hpp"
#include "boost/archive/binary_iarchive.hpp"
#include "boost/serialization/binary_object.hpp"
#include "boost/iostreams/device/file.hpp"
#include "boost/iostreams/filter/gzip.hpp"
#include "boost/serialization/vector.hpp"
#include "base64.h"
#include <iostream>
#include <sstream>
#include <lzma.h>

std::string DecompressWithLzma(const std::string& in) {
  static const size_t kMemLimit = 1 << 30;  // 1 GB.
  lzma_stream strm = LZMA_STREAM_INIT;
  std::string result;
  result.resize(8192);
  size_t result_used = 0;
  lzma_ret ret;
  ret = lzma_stream_decoder(&strm, kMemLimit, LZMA_CONCATENATED);
  if (ret != LZMA_OK)
    abort();
  size_t avail0 = result.size();
  strm.next_in = reinterpret_cast<const uint8_t*>(in.data());
  strm.avail_in = in.size();
  strm.next_out = reinterpret_cast<uint8_t*>(&result[0]);
  strm.avail_out = avail0;
  while (true) {
    ret = lzma_code(&strm, strm.avail_in == 0 ? LZMA_FINISH : LZMA_RUN);
    if (ret == LZMA_STREAM_END) {
      result_used += avail0 - strm.avail_out;
      if (0 != strm.avail_in)  // Guaranteed by lzma_stream_decoder().
        abort();
      result.resize(result_used);
      lzma_end(&strm);
      return result;
    }
    if (ret != LZMA_OK)
      abort();
    if (strm.avail_out == 0) {
      result_used += avail0 - strm.avail_out;
      result.resize(result.size() << 1);
      strm.next_out = reinterpret_cast<uint8_t*>(&result[0] + result_used);
      strm.avail_out = avail0 = result.size() - result_used;
    }
  }
}

int loadOrSaveDb(sqlite3 *pInMemory, const char *zFilename, int isSave){
  int rc;                   /* Function return code */
  sqlite3 *pFile;           /* Database connection opened on zFilename */
  sqlite3_backup *pBackup;  /* Backup object used to copy data */
  sqlite3 *pTo;             /* Database to copy to (pFile or pInMemory) */
  sqlite3 *pFrom;           /* Database to copy from (pFile or pInMemory) */

  /* Open the database file identified by zFilename. Exit early if this fails
  ** for any reason. */
  
  /* Retrun if zFilename is empty. "If the filename is an empty string, then a private, 
  ** temporary on-disk database will be created. This private database will be automatically 
  ** deleted as soon as the database connection is closed." */
  if(zFilename[0] == 0) return SQLITE_IOERR;
  rc = sqlite3_open_v2(zFilename, &pFile, SQLITE_OPEN_READONLY, NULL);
  if( rc==SQLITE_OK ){

    /* If this is a 'load' operation (isSave==0), then data is copied
    ** from the database file just opened to database pInMemory. 
    ** Otherwise, if this is a 'save' operation (isSave==1), then data
    ** is copied from pInMemory to pFile.  Set the variables pFrom and
    ** pTo accordingly. */
    pFrom = (isSave ? pInMemory : pFile);
    pTo   = (isSave ? pFile     : pInMemory);

    /* Set up the backup procedure to copy from the "main" database of 
    ** connection pFile to the main database of connection pInMemory.
    ** If something goes wrong, pBackup will be set to NULL and an error
    ** code and  message left in connection pTo.
    **
    ** If the backup object is successfully created, call backup_step()
    ** to copy data from pFile to pInMemory. Then call backup_finish()
    ** to release resources associated with the pBackup object.  If an
    ** error occurred, then  an error code and message will be left in
    ** connection pTo. If no error occurred, then the error code belonging
    ** to pTo is set to SQLITE_OK.
    */
    pBackup = sqlite3_backup_init(pTo, "main", pFrom, "main");
    if( pBackup ){
      (void)sqlite3_backup_step(pBackup, -1);
      (void)sqlite3_backup_finish(pBackup);
    }
    rc = sqlite3_errcode(pTo);
  }

  /* Close the database connection opened on database file zFilename
  ** and return the result of this function. */
  (void)sqlite3_close(pFile);
  return rc;
}

LKrEMShowers::LKrEMShowers(G4String modelName, G4Region* envelope, G4String PathToLKrShowersDb) :
  G4VFastSimulationModel(modelName, envelope),
  issuewithfile(false),
  globaltime(.0),
  PhiRandomRot(.0),
  enecorrection(.0),
  alpha(.0),
  lastE(.0),
  lastEcount(0),
  particleisinstuck(false) {

  //char *szErrMsg = 0;

  int rc = sqlite3_open(":memory:", &db);
  rc = loadOrSaveDb(db, PathToLKrShowersDb, false);
  if (rc != SQLITE_OK) {
    issuewithfile = true;
    G4cout << "[LKrEMShowers] Attention! there is a problem with the DB file (perhaps the given filename is incorrect) '"<< PathToLKrShowersDb<< "'. " <<  G4endl;  
    return;
  }
  else
    G4cout << "[LKrEMShowers] Shower library database used: " << PathToLKrShowersDb << G4endl;

   countEnergies = 0;
   fFakeStep          = new G4Step();
   fFakePreStepPoint  = fFakeStep->GetPreStepPoint();
   fFakePostStepPoint = fFakeStep->GetPostStepPoint();
   fTouchableHandle   = new G4TouchableHistory();
   fpNavigator        = new G4Navigator();
   fNaviSetup         = false;

   char *sql = new char[255];
   sprintf(sql, "SELECT name FROM sqlite_master WHERE type='table' AND name LIKE 'hits';");
   sqlite3_stmt *stmt;

   int retval = sqlite3_prepare_v2(db,sql,-1,&stmt,0);

   while(1)
   {
     // fetch a row’s status
     retval = sqlite3_step(stmt);

     if(retval == SQLITE_ROW)
     {
     // SQLITE_ROW means fetched a row
     const char *val = (const char*)sqlite3_column_text(stmt,0);
     std::string vals = val;
     std::string energystr = vals.substr(4,vals.length()-4 );
     std::istringstream i(energystr);
     int tmpdbEnergy;
     i >> tmpdbEnergy;
     showersEnergy[countEnergies] = tmpdbEnergy;
     countEnergies++;
     }
     else if(retval == SQLITE_DONE)
     {
     // All rows finished
     break;
     }
   }

  
   for (int i = 0; i < countEnergies; i++)
   {
     int count = 0;
     std::stringstream o;
     o << "SELECT weight FROM hits" << showersEnergy[i] <<";";
     //int rc = sqlite3_prepare_v2(db, o.str().c_str(), o.str().size(), &stmt, 0);
     //rc = sqlite3_step(stmt);

     double maxweightval = 0;

     while( sqlite3_step( stmt ) == SQLITE_ROW ) {
         showersweight[count][i] = (( sqlite3_column_double (stmt, 0)));
         if (showersweight[count][i] > maxweightval)
             maxweightval = showersweight[count][i];
         count++;
     }

     maxweight[i] = maxweightval;

     showersCount[i] = count;
     sqlite3_finalize( stmt );

   }
}

LKrEMShowers::~LKrEMShowers() {
  sqlite3_close(db);
  delete fpNavigator;
  delete fFakeStep;
}

G4bool LKrEMShowers::IsApplicable(const G4ParticleDefinition& particleType)
{
  if (issuewithfile)
    return false;

  if (lastEcount > 3)
  {
    lastEcount = 0;
    return false;
  }
  
  if (issuewithfile)
    return false;
  
  return 
    &particleType == G4Electron::ElectronDefinition() ||
    &particleType == G4Positron::PositronDefinition()/* ||
    &particleType == G4PionPlus::PionPlusDefinition() ||
    &particleType == G4Gamma::GammaDefinition()*/;
}


G4bool LKrEMShowers::CheckContainment(const G4FastTrack& fastTrack) {
  if (issuewithfile)
    return false;

  G4bool filter=true;
  // track informations
  G4ThreeVector DirectionShower=fastTrack.GetPrimaryTrackLocalDirection();
  G4ThreeVector InitialPositionShower=fastTrack.GetPrimaryTrackLocalPosition();

  const G4AffineTransform* AffTr =  fastTrack.GetInverseAffineTransformation(); 

  G4ThreeVector OrthoShower, CrossShower; 
  // Returns orthogonal vector 
  OrthoShower = DirectionShower.orthogonal();
  // Shower in direction perpendicular to OrthoShower and DirectionShower
  CrossShower = DirectionShower.cross(OrthoShower);

  G4double  R     = 18*cm;
  G4double  Z     = 35*cm;
  G4double  Z0     = 3.0*cm;
  G4int CosPhi[4] = {1,0,-1,0};
  G4int SinPhi[4] = {0,1,0,-1};
  G4ThreeVector Position;
  G4int NlateralInside=0;
  // pointer to solid we're in
  //G4VSolid *SolidCalo = fastTrack.GetEnvelopeSolid();
  
  G4GeometryManager::GetInstance()->CloseGeometry();
  
  fpNavigator->
        SetWorldVolume(G4TransportationManager::GetTransportationManager()->
                       GetNavigatorForTracking()->GetWorldVolume());

   for(int i=0; i<4 ;i++)
  {
    // polar coordinates
    Position = InitialPositionShower       + 
    Z*DirectionShower           +
    Z0*DirectionShower          +
    R*CosPhi[i]*OrthoShower     +
    R*SinPhi[i]*CrossShower     ;

    G4ThreeVector InitialPositionShowerGlobal = AffTr->TransformPoint(Position);
    
    if (i == 0)
       fpNavigator->LocateGlobalPointAndUpdateTouchableHandle(InitialPositionShowerGlobal, G4ThreeVector(0.,0.,0.), fTouchableHandle, false);
    else
       fpNavigator->LocateGlobalPointAndUpdateTouchableHandle(InitialPositionShowerGlobal, G4ThreeVector(0.,0.,0.), fTouchableHandle);
      
    
    if(fTouchableHandle->GetVolume()->GetLogicalVolume()->GetRegion()->GetName() == "EMCalo_parameterization_region")
      NlateralInside++;
  }

  // choose to parameterise or flag when all inetc...
 if(NlateralInside<4) filter=false;
  NlateralInside = 0;
  if (filter)
  for(int i=0; i<4 ;i++)
  {
    Position = InitialPositionShower       + 
    Z0*DirectionShower           +
    R*CosPhi[i]*OrthoShower     +
    R*SinPhi[i]*CrossShower     ;
    
    G4ThreeVector InitialPositionShowerGlobal = AffTr->TransformPoint(Position);
   
    fpNavigator->LocateGlobalPointAndUpdateTouchableHandle(InitialPositionShowerGlobal, G4ThreeVector(0.,0.,0.), fTouchableHandle);

    if(fTouchableHandle->GetVolume()->GetLogicalVolume()->GetName() == "LKrline") 
      NlateralInside++;
  }
  
  if(NlateralInside<4) filter=false;

  NlateralInside = 0;
  if (filter == true)
  for(int i=0; i<4 ;i++)
  {
    Position = InitialPositionShower       + 
    1*CosPhi[i]*OrthoShower     +
    1*SinPhi[i]*CrossShower     ;
    G4ThreeVector InitialPositionShowerGlobal = AffTr->TransformPoint(Position);
    fpNavigator->LocateGlobalPointAndUpdateTouchableHandle(InitialPositionShowerGlobal, G4ThreeVector(0.,0.,0.), fTouchableHandle);
    if(fTouchableHandle->GetVolume()->GetLogicalVolume()->GetRegion()->GetName() == "EMCalo_parameterization_region")
      NlateralInside++;
  }
  if(NlateralInside<4) filter=false;
  if (filter == true)
  {
    if (TryToFillSpots(fastTrack) == false)
      filter=false;
  }
  return filter;
}

G4bool LKrEMShowers::TryToFillSpots(const G4FastTrack& fastTrack) {
  feSpotList.clear();
  tracksvec.clear();
  G4bool EverythingIsOk = true;
  G4ThreeVector DirectionShower =
    fastTrack.GetPrimaryTrack()->GetMomentumDirection();
  G4ThreeVector OrthoShower, CrossShower;
  OrthoShower = DirectionShower.orthogonal();
  CrossShower = DirectionShower.cross(OrthoShower);

  //G4VSolid *SolidCalo = fastTrack.GetEnvelopeSolid();
  G4ThreeVector pos   = fastTrack.GetPrimaryTrackLocalPosition();
  G4ThreeVector dir   = fastTrack.GetPrimaryTrackLocalDirection();
  //G4double Bound      = SolidCalo->DistanceToOut(pos,dir); 

  G4ThreeVector PositionShower  = fastTrack.GetPrimaryTrack()->GetPosition();
  globaltime = fastTrack.GetPrimaryTrack()->GetGlobalTime();

  double TrackEnergy = fastTrack.GetPrimaryTrack()->GetKineticEnergy();
  HitsTypeList hitsvec;
  int eneindex = 0;
  float dist = 10000000;
  
  for (int i = 0; i < countEnergies; i++)
  {
    if ( fabs(TrackEnergy - showersEnergy[i]) < dist )
    {
     eneindex = i;
     dist = fabs(TrackEnergy - showersEnergy[i]);
    }
  }

  enecorrection = showersEnergy[eneindex]/TrackEnergy;
  int randrow = -1;
  
  while (randrow == -1)
  {
    randrow = G4UniformRand() * (showersCount[eneindex] -1 ) + 1;
    double tryval = G4UniformRand() * maxweight[eneindex];
    if (tryval < showersweight[randrow-1][eneindex])
    {;}
    else
      randrow=-1;
  }

  char *sql = new char[255];
  sprintf(sql, "SELECT * FROM hits%i WHERE ID=%i;\n", showersEnergy[eneindex] ,randrow);
  sqlite3_stmt *stmt;
  /*int rc = */sqlite3_prepare_v2(db, sql, strlen(sql)+1, &stmt, 0);
  /*rc = */sqlite3_step(stmt);
  std::string hitsstrcomp;

  hitsstrcomp =  std::string(reinterpret_cast<const char*> ( sqlite3_column_blob (stmt, 1)));
  std::string trackstrcomp;
  trackstrcomp = std::string(reinterpret_cast<const char*> ( sqlite3_column_blob (stmt, 2)));
  std::string hitsstrdecoded = base64_decode(hitsstrcomp.c_str());
  std::string hitsstr = DecompressWithLzma(hitsstrdecoded);

  namespace bar = boost::archive;
  namespace bio = boost::iostreams;
  std::istringstream issh(hitsstr);
  bar::text_iarchive ih(issh);
  ih >> hitsvec;
  
  std::string tracksstrdecoded = base64_decode(trackstrcomp.c_str());
  std::string tracksstr = DecompressWithLzma(tracksstrdecoded);
  std::istringstream isst(tracksstr);
  bar::text_iarchive it(isst);
  it >> tracksvec;

  NewPositionShower    = PositionShower;   
  G4ThreeVector TrackDirectionoldUnit = DirectionShower.unit();
  TrackRotAxis = G4ThreeVector(-1*TrackDirectionoldUnit.y(), 
			        1*TrackDirectionoldUnit.x(), 
			        0.);

  alpha = acos( TrackDirectionoldUnit.z()*1.0/  sqrt( 1.0*1.0*( TrackDirectionoldUnit.x()*TrackDirectionoldUnit.x() + TrackDirectionoldUnit.y()*TrackDirectionoldUnit.y() + 
			    TrackDirectionoldUnit.z()*TrackDirectionoldUnit.z())));
  
  //G4double  Zmaxshowerlib     = 35*cm;
  //G4int totsteps = 300;
  //G4double shiftZ[301];
  //shiftZ[0] = 0;
  //G4int matatZ[301];
  //matatZ[0] = 0;
  //G4int nR = 19;
  //G4int nPhi = 50;
  //G4int nZ = 36;
  //G4int TotNumber= nR*nPhi*nZ;

  PhiRandomRot = G4UniformRand()*2.0*M_PI;

  //double TotalEnergy = 0;

  for (unsigned int i = 0; i < hitsvec.size(); i++) {
    HitsType currhit = hitsvec[i];
    int id = currhit.cellID;
    float Energy = currhit.Energy/enecorrection;
    
    G4int iZ= id / (50*19);
    G4int iR= (id - iZ*50*19) / 50;
    G4int iPhi= id - iZ*50*19 - iR*50 ;

    LKrEnergySpot Spot;
    Spot.SetEnergy( Energy );
    
 //   G4cout << ">>> Spot.SetEnergy( Energy ); " << Energy << " currhit.cellID " << currhit.cellID << G4endl;

    G4LogicalVolume* myVolume;
    G4String matname;
    G4int countIter = 0;
    G4double smearcoefficiency = 0;

 //   do
    {
    G4double PhiSpot = iPhi*2.0*M_PI/50.0 + G4UniformRand()*2.0*M_PI/50.0; // phi of spot
    G4double RSpot   = iR*1.0*cm + G4UniformRand()*1.0*cm;
    G4ThreeVector SpotPosition;
    SpotPosition.setRhoPhiZ( RSpot ,PhiSpot, iZ*2.0*cm + G4UniformRand()*2.0*cm);

    if (countIter > 100)
    {
      smearcoefficiency = 1 + (countIter/100.0);
      G4ThreeVector SmearVector (1.0*mm, 1.0*mm, 1.0*mm);
      SmearVector.setPhi(G4UniformRand()*2.0*M_PI);
      SmearVector.setTheta (G4UniformRand()*M_PI/2.0);
      SmearVector.setR(G4UniformRand()*smearcoefficiency*mm);
      SpotPosition += SmearVector;
    }
    SpotPosition.rotateZ(PhiRandomRot);
    SpotPosition.rotate(alpha,TrackRotAxis);
    countIter++;
    SpotPosition += NewPositionShower;
    if (countIter > 300)
    {
      sqlite3_finalize(stmt);
      feSpotList.clear();
      hitsvec.clear();
      tracksvec.clear();
      return false;
    }

    Spot.SetPosition(SpotPosition);
    if (i == 0)
       fpNavigator->LocateGlobalPointAndUpdateTouchableHandle(SpotPosition, G4ThreeVector(0.,0.,0.), fTouchableHandle, false);
    else
       fpNavigator->LocateGlobalPointAndUpdateTouchableHandle(SpotPosition, G4ThreeVector(0.,0.,0.), fTouchableHandle);
    myVolume = fTouchableHandle->GetVolume()->GetLogicalVolume();
    if (myVolume != NULL)
    matname = myVolume->GetMaterial()->GetName();    
    }
    feSpotList.push_back(Spot);
  }
  hitsvec.clear();
  sqlite3_finalize(stmt);
  return EverythingIsOk;
}

G4bool LKrEMShowers::ModelTrigger(const G4FastTrack& fastTrack) {
 if (issuewithfile) return false;
 if (lastEcount > 1000) {
   particleisinstuck = true;
   return false;
 }

 if ((lastE > fastTrack.GetPrimaryTrack()->GetKineticEnergy()* (1-0.03)) &&
    (lastE < fastTrack.GetPrimaryTrack()->GetKineticEnergy()* (1+0.03)) )
  lastEcount++;  
  else
  lastEcount = 0;
  lastE = fastTrack.GetPrimaryTrack()->GetKineticEnergy();
  if (fastTrack.GetPrimaryTrack()->GetKineticEnergy() < 0.3*GeV)
    return false;
  
  if (fastTrack.GetPrimaryTrack()->GetKineticEnergy() > 12.0*GeV)
    return false;
  if (CheckContainment(fastTrack) == false)
    return false;
  return true;
}

void LKrEMShowers::DoIt(const G4FastTrack& /*fastTrack*/,
		     G4FastStep& fastStep) {
  if (particleisinstuck == true) {
    G4ThreeVector position = NewPositionShower;
    position += G4ThreeVector(G4UniformRand()*mm-1.0*mm, G4UniformRand()*mm-1.0*mm, G4UniformRand()*mm-1.0*mm);
    fastStep.ProposePrimaryTrackFinalPosition(position, false);
    G4cout << "particleisinstuck" << G4endl;
    particleisinstuck = false;
    return;
  }
  
  lastEcount = 0;
  BuildDetectorResponse();
  G4ParticleTable *pt = G4ParticleTable::GetParticleTable();
  fastStep.KillPrimaryTrack();
  fastStep.SetPrimaryTrackPathLength(0.0);
  fastStep.SetNumberOfSecondaryTracks(tracksvec.size());
  for (unsigned int i = 0; i < tracksvec.size() ; i++) {
    TrackType currtrack =tracksvec[i];
    //float R = sqrt (currtrack.x*currtrack.x+currtrack.y*currtrack.y);
    //float Phi = atan2(currtrack.y, currtrack.x);
    G4ThreeVector TrackPosition (currtrack.x, currtrack.y, currtrack.z+1000-10);
    TrackPosition.rotateZ(PhiRandomRot);

    float TrackPartMass = pt->FindParticle(currtrack.PDGid)->GetPDGMass();
    float TrackP = sqrt(currtrack.px*currtrack.px + currtrack.py*currtrack.py + currtrack.pz*currtrack.pz);
    float addTrackEnergy = sqrt( TrackP*TrackP + TrackPartMass*TrackPartMass) ;
    float corrTrackEnergy = addTrackEnergy/enecorrection;
    float corrTrackP = sqrt(corrTrackEnergy*corrTrackEnergy - TrackPartMass*TrackPartMass);
    float TrackPcorr = corrTrackP/TrackP;
    G4ThreeVector TrackDirectionold(currtrack.px/TrackPcorr, currtrack.py/TrackPcorr, currtrack.pz/TrackPcorr);
    TrackDirectionold.rotateZ(PhiRandomRot);
    TrackPosition.rotate(alpha,TrackRotAxis);
    TrackPosition += NewPositionShower;
    TrackDirectionold.rotate(alpha,TrackRotAxis);
    G4DynamicParticle secondary(pt->FindParticle(currtrack.PDGid), TrackDirectionold);
    fastStep.CreateSecondaryTrack(secondary, TrackPosition, globaltime, false); 
  }  
  tracksvec.clear();
}

void LKrEMShowers::BuildDetectorResponse() {
  // Does the assignation of the energy spots to the sensitive volumes:
  for (size_t i = 0; i < feSpotList.size(); i++)
    {
      AssignSpotAndCallHit(feSpotList[i]);
    }
   feSpotList.clear();
}

void LKrEMShowers::AssignSpotAndCallHit(const LKrEnergySpot &eSpot) {
  //
  // "converts" the energy spot into the fake
  // G4Step to pass to sensitive detector:
  //
    
  FillFakeStep(eSpot);

  //
  // call sensitive part: taken/adapted from the stepping:
  // Send G4Step information to Hit/Dig if the volume is sensitive
  //
  G4VPhysicalVolume* pCurrentVolume =
    fFakeStep->GetPreStepPoint()->GetPhysicalVolume();

  if( pCurrentVolume != 0 )
    {
      G4VSensitiveDetector* pSensitive = pCurrentVolume->GetLogicalVolume()->
        GetSensitiveDetector();
      if( pSensitive != 0 )
        {
          pSensitive->Hit(fFakeStep);
        }
    }   
}

void LKrEMShowers::FillFakeStep(const LKrEnergySpot &eSpot)
{
  //-----------------------------------------------------------
  // find in which volume the spot is.
  //-----------------------------------------------------------

      
      fpNavigator->
        SetWorldVolume(G4TransportationManager::GetTransportationManager()->
                       GetNavigatorForTracking()->GetWorldVolume());
      fpNavigator->
        LocateGlobalPointAndUpdateTouchableHandle(eSpot.GetPosition(),
                                            G4ThreeVector(0.,0.,0.),
                                            fTouchableHandle,
                                            false);
      fNaviSetup = true;
      

  //--------------------------------------
  // Fills attribute of the G4Step needed
  // by our sensitive detector:
  //-------------------------------------
  // set touchable volume at PreStepPoint:
  fFakePreStepPoint->SetTouchableHandle(fTouchableHandle);
  fFakePreStepPoint->SetPosition(eSpot.GetPosition());
  
  fFakeStep->SetPreStepPoint(fFakePreStepPoint);
// set total energy deposit:
  fFakeStep->SetTotalEnergyDeposit(eSpot.GetEnergy());
}
