#include "MUV12Corrections.hh"
#include "TSystem.h"
#include "NA62Exceptions.hh"
#include "TRegexp.h"
#include "TString.h"
#include "TObjString.h"
#include "TObjArray.h"
#include "sqlite3.h"
#include "NA62ConditionsService.hh"

using namespace NA62Analysis;

/// \class MUV12Corrections
/// \Brief
/// Class to handle all MUV1&2 corrections for hit usage (signal attenuation and delay inside scintillators).
/// \EndBrief
/// \Detailed
///
/// The class provides an interface to all correction needed to use the MUV1&2 hit informations. It is used for example by SpectrometerCalorimetersAssociation for clusters contruction
/// and by MUV12SwapCheck to verify the presence of swaps inside the CREAMs. \n
/// An instance can be retrieved by calling
/// \code
/// MUV12Corrections::GetInstance(GetRunID(),GetStreamInfo()->GetRecoInfo().GetRevision(),GetWithMC())
/// \endcode
/// this will create a new element of the class (if needed) and update the corrections in case the run informations (Run ID, MC, reconstruction revision) changed.
/// The Run informations can be also updated by calling
/// \code
/// ((MUV12Correction*) instance) -> NewRunID(GetRunID(),GetStreamInfo()->GetRecoInfo().GetRevision(),GetWithMC())
/// \endcode
/// The methods GetMUV1(2)Charge(Time)Correction(ChannelID,x) provide the attenuation (delay) for hits with ChannelID=hit->GetChannelID() at x = ( hit->GetScintillatorOrientation() ? PosAtMUV1.X() : PosAtMUV1.Y ).
/// The obtained corrections must be multiplied in the Charge case or summed in the Time case. \n
/// A database MUV12ChargeEqualization.db placed in the analysis config folder, contains fine equalization parameters. An analyzer to fill the database information will be published later. \n
/// \n
/// The method GetMUV1(2)Channels(x,y) return a pair with the ChannelIDs of the scintillators crossing at the position (x,y). \n
/// \n
/// Functions to obtain the channel mapping inside the CREAMs are also provided: \n
/// GetMUV1(2)ChannelID(CREAMSlotID, CREAMChannelID): returns the ChannelID corresponding to the provided electronic channel \n
/// GetMUV1(2)CREAMID(ChannelID): returns a pair made of CREAM Slot ID (first) and CREAM Channel ID (second)
/// \EndDetailed



MUV12Corrections *Instance = nullptr;


MUV12Corrections::MUV12Corrections(){
  if (!Instance) Instance=this;

  fRunID=-1;
  fYear=-1;
  NewRunID(0,"r0000",false);

  fM1ScintillatorPositions[1] = -1278;
  fM1ScintillatorWidth[1] = 60.;

  for (int i=1; i<45; i++){

    fM1ScintillatorPositions[i]=-1278;

    if (i<21){ fM1ScintillatorPositions[i] += (i - 1)*60.; fM1ScintillatorWidth[i] = 60.; }
    else if (i>24){ fM1ScintillatorPositions[i] += (44 - i)*60.; fM1ScintillatorWidth[i] = 60.; }
    else if (i==21 || i==24) { fM1ScintillatorPositions[i] = -81; fM1ScintillatorWidth[i] = 54; }
    else { fM1ScintillatorPositions[i] = -27; fM1ScintillatorWidth[i] = 54; }

    if (i>22) fM1ScintillatorPositions[i] *= -1.;

  }


  fM2ScintillatorPositions[1] = -1238.5;
  fM2ScintillatorWidth[1] = 119.;
  for (int i=2; i<23; i++){
    if (i<11 || i>12){ fM2ScintillatorPositions[i] = fM2ScintillatorPositions[i-1] + 119.; fM2ScintillatorWidth[i]=119.; }
    else{ fM2ScintillatorPositions[i] = 54.*(i<12 ? -1. : 1.); fM2ScintillatorWidth[i] = 108.; }
  }

}

MUV12Corrections::MUV12Corrections(int RunID, TString rev, bool IsMC){

  if (!Instance) Instance=this;

  fRunID=-1;
  fYear=-1;
  NewRunID(RunID,rev,IsMC);

  fM1ScintillatorPositions[1] = -1278;
  fM1ScintillatorWidth[1] = 60.;

  for (int i=1; i<45; i++){

    fM1ScintillatorPositions[i]=-1278;

    if (i<21){ fM1ScintillatorPositions[i] += (i - 1)*60.; fM1ScintillatorWidth[i] = 60.; }
    else if (i>24){ fM1ScintillatorPositions[i] += (44 - i)*60.; fM1ScintillatorWidth[i] = 60.; }
    else if (i==21 || i==24) { fM1ScintillatorPositions[i] = -81; fM1ScintillatorWidth[i] = 54; }
    else { fM1ScintillatorPositions[i] = -27; fM1ScintillatorWidth[i] = 54; }

    if (i>22) fM1ScintillatorPositions[i] *= -1.;

  }


  fM2ScintillatorPositions[1] = -1238.5;
  fM2ScintillatorWidth[1] = 119.;
  for (int i=2; i<23; i++){
    if (i<11 || i>12){ fM2ScintillatorPositions[i] = fM2ScintillatorPositions[i-1] + 119.; fM2ScintillatorWidth[i]=119.; }
    else{ fM2ScintillatorPositions[i] = 54.*(i<12 ? -1. : 1.); fM2ScintillatorWidth[i] = 108.; }
  }

}

MUV12Corrections::~MUV12Corrections(){

}

MUV12Corrections* MUV12Corrections::GetInstance(){
  if (Instance)
    return Instance;
  else
    return new MUV12Corrections;
}

MUV12Corrections* MUV12Corrections::GetInstance(int RunID, TString rev, bool IsMC=false){
  if (Instance){
    Instance->NewRunID(RunID,rev,IsMC);
    return Instance;
  }
  else
    return new MUV12Corrections(RunID,rev,IsMC);
}


void MUV12Corrections::NewRunID(int ID, TString rev, bool IsMC){

  if (ID==fRunID) return;

  auto GetYear = [] (int RunID){
	  int year = 0;
	  if (RunID<=0) year=2016;
	  else if (RunID<2000) year=2014;
	  else if (RunID<5000) year=2015;
	  else if (RunID<7000) year=2016;
	  else if (RunID<8395) year=2017;
	  else year=2018;

	  return year;
  };
  fYear=GetYear(ID);

  fRevID = rev;
  fRevNum = atoi( TagConvert(rev)(TRegexp("[0-9]+")).Data() );
  fIsMC = IsMC;

  //cout <<"[MUV12Corrections] New run id "<<ID<<" with revision "<<fRevID<<"("<<fRevNum<<") reading corrections"<<endl;

  fRunID = ID;

  TString RepositoryPath=gSystem->Getenv("NA62RECOSOURCE");
  RepositoryPath.Append("/");
  TString MUV1ConfFile=RepositoryPath,MUV2ConfFile=RepositoryPath;
  TString MUV1QEqFile="", MUV2QEqFile="", MUV1TPosFile="", MUV2TPosFile="";

  MUV1ConfFile.Append("config/");
  MUV2ConfFile.Append("config/");

  MUV1ConfFile.Append("MUV1.conf");
  MUV2ConfFile.Append("MUV2.conf");


  ifstream MUV1File(MUV1ConfFile.Data());
  TString line;
  int NReadLines = 0;
  map<int,int> muv1slotmap;

  while (line.ReadLine(MUV1File)){

    NReadLines++;

    if (line.BeginsWith("QeqFile")){
  	TObjArray *arr = line.Tokenize(" ");
  	MUV1QEqFile = ((TObjString*)arr->At(1))->GetString();
  	arr->Delete();
  	continue;
    }
    else if (line.BeginsWith("TimeDepFile")){
  	TObjArray *arr = line.Tokenize(" ");
  	MUV1TPosFile = ((TObjString*)arr->At(1))->GetString();
  	arr->Delete();
  	continue;
    }
  }
  MUV1File.close();

  if (NReadLines<1){
    cout <<"[MUV12Corrections] Read "<<NReadLines<<" Lines of "<<MUV1ConfFile.Data()<<endl;
    throw Core::LogicException();
  }

  TString MUV1RawDecoderSettingsFile = "MUV1-RawDecoderSettings.dat";
  NA62ConditionsService::GetInstance()->Open(MUV1RawDecoderSettingsFile);
  NReadLines = 0;
  while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(MUV1RawDecoderSettingsFile))){
    NReadLines++;

    if (line.BeginsWith("#")) continue;
    else if (line.BeginsWith("SlRemap_0000")){
  	TObjArray *arr = line.Tokenize(" ");
  	for (int i=1; i<arr->GetEntries(); i++){
  	  int val = ((TObjString*)arr->At(i))->GetString().Atoi();
  	  if (val > -1){
  		muv1slotmap[val*2] = i-1;
  		muv1slotmap[val*2+1] = i-1;
  		//cout <<"slot "<<i<<" mapped with "<<val<<endl;
  	  }
  	}
  	arr->SetOwner();
  	arr->Delete();
  	continue;
    }
    else if (line.BeginsWith("ChRemap_")){
  	TObjArray *arr = line.Tokenize(" ");
  	pair<int,int> a;
  	int Slot = atoi(line(TRegexp("[0-9]+")).Data());
  	a.first = muv1slotmap[Slot];
  	bool IsEmpty = 1;
  	for (int i=1; i<arr->GetEntries(); i++){
  	  int val = ((TObjString*)arr->At(i))->GetString().Atoi();
  	  a.second = i-1;
  	  if (Slot%2) a.second += 16;
  	  fMUV1ChannelCREAMMap[val]=a;
  	  fMUV1CREAMChannelMap[a]=val;
  	  if (val>0) IsEmpty = 0;
  	  //cout <<"Remap for channel "<<i-1<<" on remap "<<Slot<<" in slot "<<slotmap[Slot]<<" is "<<val<<endl;
  	}
  	fMUV1CREAMEmptyHalfSlot[make_pair(a.first,Slot%2)] = IsEmpty;
  	arr->SetOwner();
  	arr->Delete();
  	continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(MUV1RawDecoderSettingsFile);


  NA62ConditionsService::GetInstance()->Open(MUV1QEqFile);
  //cout <<"[MUV12Corrections:] Reading "<<NA62ConditionsService::GetInstance()->GetFullPath(MUV1QEqFile)<<endl;
  NReadLines = 0;

  while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(MUV1QEqFile))){
    NReadLines++;

    if (line.BeginsWith("#")) continue;
    else if (line.BeginsWith("QPositionFunction")){
  		  TObjArray *arr = line.Tokenize(" ");
  		  fMUV1QPosCorr = TF1("MUV1QPfcn",((TObjString*)arr->At(1))->GetString(),-1300,1300);
  		  arr->Delete();
  		  continue;
    }

    TObjArray *arr = line.Tokenize(" ");

    int channel = ((TObjString*)arr->At(0))->GetString().Atoi();
    int side = (channel - 100)/50;
    int pos = channel%50 - 1;

    //cout <<"MUV1 Channel "<<channel<<" Side "<<side<<" pos "<<pos<<endl;
    fMUV1QEqValues[side][pos] = 1.;//((TObjString*)arr->At(1))->GetString().Atof();
    fMUV1QPosCorrParameters[side][pos].clear();
    for (int i=2; i<arr->GetEntries(); i++) fMUV1QPosCorrParameters[side][pos].push_back(((TObjString*)arr->At(i))->GetString().Atof());
    arr->Delete();

  }

  NA62ConditionsService::GetInstance()->Close(MUV1QEqFile);

  if (NReadLines<1){
    cout <<"[MUV12Corrections] Read "<<NReadLines<<" Lines of "<<MUV1QEqFile.Data()<<endl;
    throw Core::LogicException();;
  }

  NA62ConditionsService::GetInstance()->Open(MUV1TPosFile);
  NReadLines = 0;

  while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(MUV1TPosFile))){
    NReadLines++;

    if (line.BeginsWith("#")) continue;
    else if (line.BeginsWith("TimePositionFunction")){
  		  TObjArray *arr = line.Tokenize(" ");
  		  fMUV1TPosCorr = TF1("MUV1TPfcn",((TObjString*)arr->At(1))->GetString(),-1300,1300);
  		  arr->Delete();
  		  continue;
    }
    TObjArray *arr = line.Tokenize(" ");

    int channel = ((TObjString*)arr->At(0))->GetString().Atoi();
    int side = (channel - 100)/50;
    int pos = channel%50 - 1;

    //cout <<"MUV1 Channel "<<channel<<" Side "<<side<<" pos "<<pos<<endl;
    fMUV1TPosCorrParameters[side][pos].clear();
    for (int i=1; i<arr->GetEntries(); i++) fMUV1TPosCorrParameters[side][pos].push_back(((TObjString*)arr->At(i))->GetString().Atof());

    arr->Delete();
  }

  NA62ConditionsService::GetInstance()->Close(MUV1TPosFile);

  if (NReadLines<1){
    cout <<"[MUV12Corrections] Read "<<NReadLines<<" Lines of "<<MUV1TPosFile.Data()<<endl;
    throw Core::LogicException();;
  }

  ifstream MUV2File(MUV2ConfFile.Data());
  NReadLines = 0;
  map<int,int> muv2slotmap;

  while (line.ReadLine(MUV2File)){
    NReadLines++;

    if (line.BeginsWith("QeqFile")){
  	TObjArray *arr = line.Tokenize(" ");
  	MUV2QEqFile = ((TObjString*)arr->At(1))->GetString();
  	arr->Delete();
  	continue;
    }
    else if (line.BeginsWith("TimeDepFile")){
  	TObjArray *arr = line.Tokenize(" ");
  	MUV2TPosFile = ((TObjString*)arr->At(1))->GetString();
  	arr->Delete();
  	continue;
    }

  }

  MUV2File.close();

  if (NReadLines<1){
    cout <<"[MUV12Corrections] Read "<<NReadLines<<" Lines of "<<MUV2ConfFile.Data()<<endl;
    throw Core::LogicException();
  }

  TString MUV2RawDecoderSettingsFile = "MUV2-RawDecoderSettings.dat";
  NA62ConditionsService::GetInstance()->Open(MUV2RawDecoderSettingsFile);
  NReadLines = 0;
  while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(MUV2RawDecoderSettingsFile))){
    NReadLines++;

    if (line.BeginsWith("#")) continue;
    else if (line.BeginsWith("SlRemap_0000")){
  	TObjArray *arr = line.Tokenize(" ");
  	for (int i=1; i<arr->GetEntries(); i++){
  	  int val = ((TObjString*)arr->At(i))->GetString().Atoi();
  	  if (val > -1){
  		muv2slotmap[val*2] = i-1;
  		muv2slotmap[val*2+1] = i-1;
  		//cout <<"slot "<<i<<" mapped with "<<val<<endl;
  	  }
  	}
  	arr->SetOwner();
  	arr->Delete();
  	continue;
    }
    else if (line.BeginsWith("ChRemap_")){
  	TObjArray *arr = line.Tokenize(" ");
  	pair<int,int> a;
  	int Slot = atoi(line(TRegexp("[0-9]+")).Data());
  	a.first = muv2slotmap[Slot];
  	bool IsEmpty = 1;
  	for (int i=1; i<arr->GetEntries(); i++){
  	  int val = ((TObjString*)arr->At(i))->GetString().Atoi();
  	  a.second = i-1;
  	  if (Slot%2) a.second += 16;
  	  fMUV2ChannelCREAMMap[val]=a;
  	  fMUV2CREAMChannelMap[a]=val;
  	  if (val>0) IsEmpty = 0;
  	  //cout <<"Remap for channel "<<i-1<<" on remap "<<Slot<<" in slot "<<slotmap[Slot]<<" is "<<val<<endl;
  	}
  	fMUV2CREAMEmptyHalfSlot[make_pair(a.first,Slot%2)] = IsEmpty;
  	arr->SetOwner();
  	arr->Delete();
  	continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(MUV2RawDecoderSettingsFile);


  NA62ConditionsService::GetInstance()->Open(MUV2QEqFile);
  NReadLines = 0;

  while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(MUV2QEqFile))){
    NReadLines++;
    if (line.BeginsWith("#")) continue;
    else if (line.BeginsWith("QPositionFunction")){
  		  TObjArray *arr = line.Tokenize(" ");
  		  fMUV2QPosCorr = TF1("MUV2QPfcn",((TObjString*)arr->At(1))->GetString(),-1300,1300);
  		  arr->Delete();
  		  continue;
    }

    TObjArray *arr = line.Tokenize(" ");

    int channel = ((TObjString*)arr->At(0))->GetString().Atoi();
    int side = (channel - 100)/50;
    int pos = channel%50 - 1;

    fMUV2QEqValues[side][pos] = 1.;//((TObjString*)arr->At(1))->GetString().Atof();
    //cout <<"[MUV12Corrections] MUV2 Channel "<<channel<<" Side "<<side<<" pos "<<pos<<endl;
    fMUV2QPosCorrParameters[side][pos].clear();
    for (int i=2; i<arr->GetEntries(); i++) fMUV2QPosCorrParameters[side][pos].push_back(((TObjString*)arr->At(i))->GetString().Atof());

    arr->Delete();
  }

  NA62ConditionsService::GetInstance()->Close(MUV2QEqFile);

  if (NReadLines<1){
    cout <<"[MUV12Corrections] Read "<<NReadLines<<" Lines of "<<MUV2QEqFile.Data()<<endl;
    throw Core::LogicException();;
  }

  NA62ConditionsService::GetInstance()->Open(MUV2TPosFile);
  NReadLines = 0;

  while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(MUV2TPosFile))){
    NReadLines++;

    if (line.BeginsWith("#")) continue;
    else if (line.BeginsWith("TimePositionFunction")){
  		  TObjArray *arr = line.Tokenize(" ");
  		  fMUV2TPosCorr = TF1("MUV2TPfcn",((TObjString*)arr->At(1))->GetString(),-1300,1300);
  		  arr->Delete();
  		  continue;
    }

    TObjArray *arr = line.Tokenize(" ");

    int channel = ((TObjString*)arr->At(0))->GetString().Atoi();
    int side = (channel - 100)/50;
    int pos = channel%50 - 1;

    //cout <<"MUV2 Channel "<<channel<<" Side "<<side<<" pos "<<pos<<endl;
    fMUV2TPosCorrParameters[side][pos].clear();
    for (int i=1; i<arr->GetEntries(); i++) fMUV2TPosCorrParameters[side][pos].push_back(((TObjString*)arr->At(i))->GetString().Atof());

    arr->Delete();
  }

  NA62ConditionsService::GetInstance()->Close(MUV2TPosFile);

  if (NReadLines<1){
    cout <<"[MUV12Corrections] Read "<<NReadLines<<" Lines of "<<MUV2TPosFile.Data()<<endl;
    throw Core::LogicException();;
  }

  if (fIsMC || fRunID<=0) return;
  //cout <<"[MUV12Corrections] Fetching corrections from database "<<endl;

  int tagmin=0,tagmax=0;
  if (!TagMinMax(tagmin,tagmax)) return;

  TString DB_Path = "MUV12ChargeEqualization.db";
  if (NA62ConditionsService::GetInstance()->GetFullPath(DB_Path).Length()==0) {
    cout<<"[MUV12Corrections] Cannot find db file"<<endl;
    return;
  }
  sqlite3 *db = NULL;
  sqlite3_stmt *stmt;

  //  int RC = sqlite3_open_v2(DB_Path.Data(),&db,SQLITE_OPEN_READONLY,NULL);
  int RC = sqlite3_open_v2(NA62ConditionsService::GetInstance()->GetFullPath(DB_Path).Data(),&db,SQLITE_OPEN_READONLY,NULL);
  if (RC!=SQLITE_OK){
    cout<<"[MUV12Corrections] Cannot open db file"<<endl;
    cout<<sqlite3_errmsg(db)<<endl;
    return;
  }

  TString command="SELECT * FROM MUV1 WHERE abs(RUN-?)<1000 and (CAST (REPLACE(REV,'r','0') as INTEGER) > ?) and (CAST (REPLACE(REV,'r','0') as INTEGER) < ?) ORDER BY abs(RUN-?) ASC, (CAST (REPLACE(REV,'r','0') as INTEGER) - ?) ASC LIMIT ?";
  RC = sqlite3_prepare(db,command.Data(),-1,&stmt,NULL);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,1,fRunID);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,2,tagmin);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,3,tagmax);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,4,fRunID);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,5,fRevNum);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,6,1);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  if (RC == SQLITE_OK){

    RC = sqlite3_step(stmt);

    if (RC == SQLITE_ROW){

      for (int i=0; i<sqlite3_column_count(stmt); i++){

        TString Name = sqlite3_column_name(stmt,i);
        if (Name.BeginsWith("RUN")){
          cout <<"[MUV12Corrections] Found MUV1 corrections from nearest stored run "<<sqlite3_column_int(stmt,i)<<"...";

          ID = sqlite3_column_int(stmt,i);

          int year = GetYear(ID);

          if (year != fYear){
            cout <<"too far, skipping!"<<endl;
            break;
          }
          cout <<"loading from revision ";
        }
        if (Name.BeginsWith("REV")){
          cout <<sqlite3_column_text(stmt,i)<<endl;
        }
        if (!Name.BeginsWith("Ch")) continue;

        int ChID = TString(Name(TRegexp("[0-9]+"))).Atoi();

        int side = (ChID-100)/50;
        int ch = (ChID-100)%50 - 1;

        fMUV1QEqValues[side][ch] = sqlite3_column_double(stmt,i);
        //cout <<"MUV1 Channel "<<ChID<<"("<<side<<","<<ch<<") val "<<fMUV1QEqValues[side][ch]<<endl;
      }
    }
    else cout <<"[MUV12Corrections] Run not found in MUV1 DB"<<endl;

    sqlite3_finalize(stmt);
  }
  else cout <<"[MUV12Corrections] cannot fetch run corrections FOR MUV1! ("<<sqlite3_errmsg(db)<<")"<<endl;


  command="SELECT * FROM MUV2 WHERE abs(RUN-?)<1000 and (CAST (REPLACE(REV,'r','0') as INTEGER) > ?) and (CAST (REPLACE(REV,'r','0') as INTEGER) < ?) ORDER BY abs(RUN-?) ASC, (CAST (REPLACE(REV,'r','0') as INTEGER) - ?) ASC LIMIT ?";
  RC = sqlite3_prepare(db,command.Data(),-1,&stmt,NULL);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,1,fRunID);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,2,tagmin);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,3,tagmax);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,4,fRunID);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,5,fRevNum);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  sqlite3_bind_int(stmt,6,1);
  if (RC != SQLITE_OK) cout<<sqlite3_errmsg(db)<<endl;

  if (RC == SQLITE_OK){

    RC = sqlite3_step(stmt);

    if (RC == SQLITE_ROW){

      for (int i=0; i<sqlite3_column_count(stmt); i++){

        TString Name = sqlite3_column_name(stmt,i);
        if (Name.BeginsWith("RUN")){
          cout <<"[MUV12Corrections] Found MUV2 corrections from nearest stored run "<<sqlite3_column_int(stmt,i)<<"...";
          ID = sqlite3_column_int(stmt,i);

          int year = GetYear(ID);

          if (year != fYear){
            cout <<"too far, skipping!"<<endl;
            break;
          }
          cout <<"loading from revision ";
        }
        if (Name.BeginsWith("REV")){
          cout <<sqlite3_column_text(stmt,i)<<endl;
        }
        if (!Name.BeginsWith("Ch")) continue;

        int ChID = TString(Name(TRegexp("[0-9]+"))).Atoi();

        int side = (ChID-100)/50;
        int ch = (ChID-100)%50 - 1;

        fMUV2QEqValues[side][ch] = sqlite3_column_double(stmt,i);

        //cout <<"MUV2 Channel "<<ChID<<"("<<side<<","<<ch<<") val "<<fMUV2QEqValues[side][ch]<<endl;
      }
      sqlite3_finalize(stmt);
    }
    else cout <<"[MUV12Corrections] Run not found in MUV2 DB"<<endl;
  }
  else cout <<"[MUV12Corrections] cannot fetch run corrections FOR MUV2! ("<<sqlite3_errmsg(db)<<")"<<endl;

  sqlite3_close(db);
}

double MUV12Corrections::GetMUV1ChargeCorrection(int ChannelID, double position, bool WithEq){

  int side = (ChannelID-100)/50;
  int ch = ChannelID%50-1;

  fMUV1QPosCorr.SetParameters(fMUV1QPosCorrParameters[side][ch].data());

  double val = 1./fMUV1QPosCorr.Eval(position);

  if (fIsMC || !WithEq) return val;
  return val * fMUV1QEqValues[side][ch];

}
double MUV12Corrections::GetMUV2ChargeCorrection(int ChannelID, double position, bool WithEq){

  int side = (ChannelID-100)/50;
  int ch = ChannelID%50-1;

  fMUV2QPosCorr.SetParameters(fMUV2QPosCorrParameters[side][ch].data());
  double val = fMUV2QPosCorr.Eval(position);

  if (val!=0) val = 1./val;
  else val = 1.;

  if (fIsMC || !WithEq) return val;
  return val * fMUV2QEqValues[side][ch];

}

double MUV12Corrections::GetMUV1TimeCorrection(int ChannelID, double position){

  int side = (ChannelID-100)/50;
  int ch = ChannelID%50-1;

  fMUV1TPosCorr.SetParameters(fMUV1TPosCorrParameters[side][ch].data());

  double val = fMUV1TPosCorr.Eval(position);

  if (fIsMC && fRevNum<1651) return -val;
  return val;

}
double MUV12Corrections::GetMUV2TimeCorrection(int ChannelID, double position){

  int side = (ChannelID-100)/50;
  int ch = ChannelID%50-1;

  fMUV2TPosCorr.SetParameters(fMUV2TPosCorrParameters[side][ch].data());

  return fMUV2TPosCorr.Eval(position);

}

pair<int,int> MUV12Corrections::GetMUV1Channels(double x, double y){

  int Xch = (x<0 ? 1 : 21);
  int Ych = (y<0 ? 1 : 21);

  while (Xch<44 && x>fM1ScintillatorPositions[Xch]+0.5*fM1ScintillatorWidth[Xch]) Xch++;
  while (Ych<44 && y>fM1ScintillatorPositions[Ych]+0.5*fM1ScintillatorWidth[Ych]) Ych++;

  Ych += 50;

  if (x>0) Ych += 200;
  else Ych += 100;

  if (y>0) Xch += 200;
  else Xch += 100;

  return make_pair(Xch,Ych);
}
pair<int,int> MUV12Corrections::GetMUV2Channels(double x, double y){

  int Xch = (x<0 ? 1 : 10);
  int Ych = (y<0 ? 1 : 10);

  while (Xch<22 && x>fM2ScintillatorPositions[Xch]+0.5*fM2ScintillatorWidth[Xch]) Xch++;
  while (Ych<22 && y>fM2ScintillatorPositions[Ych]+0.5*fM2ScintillatorWidth[Ych]) Ych++;

  Ych += 50;

  if (x>0) Ych += 200;
  else Ych += 100;

  if (y>0) Xch += 200;
  else Xch += 100;

  return make_pair(Xch,Ych);
}

TString MUV12Corrections::TagConvert (TString tag){
  if (tag.BeginsWith("r")) return tag;
  if (tag.BeginsWith("v")){
		if (tag.EqualTo("v1.0.0")) return TString("r2390");
		else if (tag.EqualTo("v0.11.3")) return TString("r2282");
    else if (tag.EqualTo("v0.11.2")) return TString("r2216");
    else if (tag.EqualTo("v0.11.1")) return TString("r2026");
    else if (tag.EqualTo("v0.11.0")) return TString("r1819");
    else if (tag.EqualTo("v0.10.0")) return TString("r1569");
    else if (tag.EqualTo("v0.9.1")) return TString("r1068");
    else if (tag.EqualTo("v0.9.0")) return TString("r849");
  }
  return TString ("r9998");
}

bool MUV12Corrections::TagMinMax (int &tagmin, int &tagmax){
  if (fYear<2016) { tagmin=0; tagmax=9999; return true; }
  if (fYear==2016){
    if (fRevNum<=1034){ tagmin=0; tagmax=1034; return true; }
    else { tagmin=1035; tagmax=9999; return true; }
  }
  else if (fYear==2017){
    if (fRevNum<=1773){ tagmin=0; tagmax=1773; return true; }
    else { tagmin=1774; tagmax=9999; return true; }
  }
	else if (fYear==2018){
		if (fRevNum<=2279){ tagmin=0; tagmax=2279; return true; }
		else { tagmin=2280; tagmax=9999; return true; }
	}
  else { tagmin=0; tagmax=9999; return true; }
  tagmin=-1; tagmax=-1;
  return false;
}
