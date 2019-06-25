// ---------------------------------------------------------------
// History:
//
// Created by Dmitry Madigozhin (madigo@mail.cern.ch) 2016-11-24
//
// An option to correct only in-run jumps for exclusive runs given 
// by the list is added by D.Madigozhin 2016-12-20
//
// ---------------------------------------------------------------

#include "T0Jumps.hh"
#include "NA62ConditionsService.hh"

/// \class T0Jumps
/// \Brief
/// Implements the Spectrometer T0 25-ns jumps correction
/// \EndBrief
///
/// \Detailed
/// The correction is based on the graphs of the peak position of the raw digi time 
/// distribution vs the burst timestamp. The constructor of correction takes the 
/// file name of the jumps data base (currently it is a text file) and the starting
/// time (normally 0 that means a use of the base from the beginning).
///
/// If a method SetExclusive(const char *fname) is called, the corresponding list 
/// of runs to be corrected exclusively for their in-run jumps is taken into account.
/// Other jumps inside other runs as well as inter-run jumps are ignored. So it should 
/// be a complete list of good runs with a T0 jumps inside. In fact the time of run 
/// begin and the time of run end are used here. Starting time for the jumps accumulation
/// in this case is reset to begin of the run.
///
/// Then each call of GetJump(int i, int time) returns the sum of jumps accumulated 
/// from the starting time till the requested time for the given 
/// cover identifier = (<SRB number>*16 + <cover electronics number>).   
///
/// \EndDetailed

T0Jumps::T0Jumps(const char *fname, int from):fT0JumpFileName(fname){
  /// \MemberDescr
  /// \param fname : file name of the jumps text data base
  /// \param from : starting time (normally 0)
  ///
  /// \code
  fExclusive=false;
  fFromTime=from;
  ZeroStatus();
  NA62ConditionsService::GetInstance()->Open(fT0JumpFileName);
  /// \endcode
  /// \EndMemberDescr
}

T0Jumps::~T0Jumps(){
  NA62ConditionsService::GetInstance()->Close(fT0JumpFileName);
}

void T0Jumps::SetExclusive(const char *fname){
  /// \MemberDescr
  /// \param fname : file name of the runs list to be corrected exclusively
  ///
  /// \code

  int run,beg,end;

  fExclusiveRun = std::vector<int>();
  fExclusiveRunBegin = std::vector<int>();
  fExclusiveRunEnd = std::vector<int>();

  NA62ConditionsService::GetInstance()->Open(fname);
  while(NA62ConditionsService::GetInstance()->Get(fname) >> run >> beg >> end){
    fExclusiveRun.push_back(run);
    fExclusiveRunBegin.push_back(beg);
    fExclusiveRunEnd.push_back(end);
  } 
  fExclusive=true;
  NA62ConditionsService::GetInstance()->Close(fname);

  std::cout << "T0 jumps correction is applied only to the runs listed in " << fname << std::endl;

  //  for(unsigned int k=0; k<fExclusiveRun.size(); k++){  // control printout
  //    std::cout << k << " " << fExclusiveRun[k] << " " << fExclusiveRunBegin[k] << " " << fExclusiveRunEnd[k] << endl;
  //  }

  /// \endcode
  /// \EndMemberDescr
}

void T0Jumps::ZeroStatus(){
  /// \MemberDescr
  /// Resets the counters and jump summaries.\n
  /// \code

  fLastRequestedTime = 0;
  fLastTime=0;
  fLastCover=0;
  fLastJump=0;
  for(int i=0; i<512; i++)fJump[i]=0;
  return;

  /// \endcode
  /// \EndMemberDescr
}

int T0Jumps::ForTime(int time){
  /// \MemberDescr
  /// \param time : time for the jumps summaries
  ///
  /// Calculates the summary jumps for all the covers till the given time.
  /// If the requested time is earlier than the last requested one, the 
  /// database is scanned from the very beginning. The database file must 
  /// be time-ordered, but the requests may come in arbitrary time order.
  /// \code

  fLastRequestedTime = time;

  //------------ Exclusive runs to be corrected ------------------------
  if(fExclusive){
    bool dont = true;
    for(unsigned int k=0; k<fExclusiveRun.size(); k++){
      if((fExclusiveRunBegin[k] <= time) && (time <= fExclusiveRunEnd[k])){
        fFromTime=fExclusiveRunBegin[k]-1; // to avoid inter-run jumps that always have time = (RunBegin-10).
        dont = false;
        break;
      }
    }
    if(dont)return 0;
  }
  //--------------------------------------------------------------------

  if(fLastTime && (fLastTime <= time)){  // we have the results of the last reading
    if(fLastCover<0 || fLastCover>511){
      cout << " wrong cover " << endl;
      return 1;
    }else{
      if(fLastTime >= fFromTime)fJump[fLastCover] += fLastJump;
    }
    fLastTime=0;
  }

  if(time > fLastTime){

    while(! NA62ConditionsService::GetInstance()->Get(fT0JumpFileName).eof()){

      NA62ConditionsService::GetInstance()->Get(fT0JumpFileName) >> fLastTime >> fLastCover >> fLastJump;
      if(NA62ConditionsService::GetInstance()->Get(fT0JumpFileName).eof()){
        fLastTime=0; // to avoid the second usage of the last line
        break;
      }

      if(fLastTime > time){
        break;
      }

      if(fLastCover<0 || fLastCover>511){
        cout << " Wrong Cover !!! " << endl;
        return 1;
      }else{
        if(fLastTime >= fFromTime)fJump[fLastCover] += fLastJump;
      }

    }

  }

  return 0;

  /// \endcode
  /// \EndMemberDescr
}


int T0Jumps::GetJump(int i, int time){
  /// \MemberDescr
  /// \param i : cover, for wchich the jumps summary is requested
  /// \param time : time for the jumps summary
  ///
  /// Returns the summary jump for the requested cover and time.\n
  /// If the requested time differs from the last requested time,
  /// the database is scanned in order to set the correct summary 
  /// jumps for all the covers. 
  /// \code

  if(time > fLastRequestedTime){
    if(ForTime(time)){
      cout << "Error in the T0 jumps input file" << endl;
    }
  }else if(time < fLastRequestedTime){
    ZeroStatus();
    NA62ConditionsService::GetInstance()->Get(fT0JumpFileName).clear();
    NA62ConditionsService::GetInstance()->Get(fT0JumpFileName).seekg(0,ios::beg);
    if(ForTime(time)){
      cout << "Error in the T0 jumps input file" << endl;
    }
  }
  //  cout << "time " << time << " cover " << i << " jump: " << fJump[i] << endl;  

  return fJump[i];

  /// \endcode
  /// \EndMemberDescr
}
