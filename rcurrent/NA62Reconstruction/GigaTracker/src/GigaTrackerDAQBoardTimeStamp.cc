#include "GigaTrackerDAQBoardTimeStamp.hh" 
#include "RG_Utilities.hh"
#include <iomanip>
#include "TVEvent.hh" // To get the ClockPeriod
#include "GigaTrackerErrorsHandler.hh"

namespace GTK
{
  const int GigaTrackerDAQBoardTimeStamp::buffLength  ; 

  const u32 GigaTrackerDAQBoardTimeStamp::iFlag = 0;
  const u32 GigaTrackerDAQBoardTimeStamp::mFlag = 0xF0000000;
  const u32 GigaTrackerDAQBoardTimeStamp::sFlag = 28;
  const u32 GigaTrackerDAQBoardTimeStamp::rFlag = 0x0;

  const u32 GigaTrackerDAQBoardTimeStamp::iChipId = 0;
  const u32 GigaTrackerDAQBoardTimeStamp::mChipId = 0x0F000000;
  const u32 GigaTrackerDAQBoardTimeStamp::sChipId = 24;

  const u32 GigaTrackerDAQBoardTimeStamp::iStationId = 0;
  const u32 GigaTrackerDAQBoardTimeStamp::mStationId = 0x00C00000;
  const u32 GigaTrackerDAQBoardTimeStamp::sStationId = 22;

  const u32 GigaTrackerDAQBoardTimeStamp::iFrameCountLSB = 0;
  const u32 GigaTrackerDAQBoardTimeStamp::mFrameCountLSB = 0x003C0000;
  const u32 GigaTrackerDAQBoardTimeStamp::sFrameCountLSB = 18;

  const u32 GigaTrackerDAQBoardTimeStamp::iDataStatusSelector = 0;
  const u32 GigaTrackerDAQBoardTimeStamp::mDataStatusSelector = 0x00020000;
  const u32 GigaTrackerDAQBoardTimeStamp::sDataStatusSelector = 17;

  const u32 GigaTrackerDAQBoardTimeStamp::iPixelAddress = 0;
  const u32 GigaTrackerDAQBoardTimeStamp::mPixelAddress = 0x0001FC00;
  const u32 GigaTrackerDAQBoardTimeStamp::sPixelAddress = 10;

  const u32 GigaTrackerDAQBoardTimeStamp::iHitArbiterAddress = 0;
  const u32 GigaTrackerDAQBoardTimeStamp::mHitArbiterAddress = 0x000003E0;
  const u32 GigaTrackerDAQBoardTimeStamp::sHitArbiterAddress = 5;

  const u32 GigaTrackerDAQBoardTimeStamp::iPileUpAddress = 0;
  const u32 GigaTrackerDAQBoardTimeStamp::mPileUpAddress = 0x0000001F;
  const u32 GigaTrackerDAQBoardTimeStamp::sPileUpAddress = 0;

  const u32 GigaTrackerDAQBoardTimeStamp::iqChipId = 1;
  const u32 GigaTrackerDAQBoardTimeStamp::mqChipId = 0xC0000000;
  const u32 GigaTrackerDAQBoardTimeStamp::sqChipId = 30;

  const u32 GigaTrackerDAQBoardTimeStamp::iLeadingSelector = 1;
  const u32 GigaTrackerDAQBoardTimeStamp::mLeadingSelector = 0x20000000;
  const u32 GigaTrackerDAQBoardTimeStamp::sLeadingSelector = 29;

  const u32 GigaTrackerDAQBoardTimeStamp::iLeadingCoarse = 1;
  const u32 GigaTrackerDAQBoardTimeStamp::mLeadingCoarse = 0x1FFE0000;
  const u32 GigaTrackerDAQBoardTimeStamp::sLeadingCoarse = 17;

  const u32 GigaTrackerDAQBoardTimeStamp::iLeadingFine = 1;
  const u32 GigaTrackerDAQBoardTimeStamp::mLeadingFine = 0x0001F000;
  const u32 GigaTrackerDAQBoardTimeStamp::sLeadingFine = 12;

  const u32 GigaTrackerDAQBoardTimeStamp::iTotSelector = 1;
  const u32 GigaTrackerDAQBoardTimeStamp::mTotSelector = 0x00000800;
  const u32 GigaTrackerDAQBoardTimeStamp::sTotSelector = 11;

  const u32 GigaTrackerDAQBoardTimeStamp::iTotCoarse = 1;
  const u32 GigaTrackerDAQBoardTimeStamp::mTotCoarse = 0x000007E0;
  const u32 GigaTrackerDAQBoardTimeStamp::sTotCoarse = 5;

  const u32 GigaTrackerDAQBoardTimeStamp::iTotFine = 1;
  const u32 GigaTrackerDAQBoardTimeStamp::mTotFine = 0x0000001F;
  const u32 GigaTrackerDAQBoardTimeStamp::sTotFine = 0;

  u32 GigaTrackerDAQBoardTimeStamp::NbErrFrameLSB[]   = {0,0,0,0,0,0,0,0,0,0,
							 0,0,0,0,0,0,0,0,0,0,
							 0,0,0,0,0,0,0,0,0,0,
							 0,0,0,0,0,0,0,0,0,0,
							 0,0,0,0,0,0,0,0,0,0,
							 0,0,0,0,0,0,0,0,0,0};
  u32 GigaTrackerDAQBoardTimeStamp::NbErrStationId[]   = {0,0,0,0,0,0,0,0,0,0};

  u32 GigaTrackerDAQBoardTimeStamp::NbErrFrameLSBAll  = 0;
  u32 GigaTrackerDAQBoardTimeStamp::NbErrTag          = 0;



  // ==============================================  
  void  GigaTrackerDAQBoardTimeStamp::ResetError(){
    for(int i(0); i<60; i++ ) NbErrFrameLSB[i] = 0;
    for(int i(0); i<10; i++ ) NbErrStationId[i] = 0;
    GigaTrackerDAQBoardTimeStamp::NbErrTag = 0;
    GigaTrackerDAQBoardTimeStamp::NbErrFrameLSBAll  = 0;
  }
  
  // ==============================================  
  void GigaTrackerDAQBoardTimeStamp::PrintErrorSummary(){

    int w = 2*13+23-1;
    printf("*%s*\n",std::string(w,'*').c_str());
    printf("* %-46s *\n","GTK RAW DECODER ERROR SUMMARY");
    printf("*%s*\n",std::string(w,'*').c_str());
    printf("* %-20s * %23u *\n", "NbErrTag",GigaTrackerDAQBoardTimeStamp::NbErrTag);
    printf("*%s*\n",std::string(w,'*').c_str());
    printf("* %-20s * %10s * %10s *\n", "NbErrFrameLSB","Link 1","Link 2");
    printf("*%s*\n",std::string(w,'-').c_str());
    for(int i(0);i<10;i++) printf("* %17s %2d * %10u * %10u *\n","Board",i,GigaTrackerDAQBoardTimeStamp::NbErrFrameLSB[i],GigaTrackerDAQBoardTimeStamp::NbErrFrameLSB[i+30]);
    printf("*%s*\n",std::string(w,'-').c_str());
    for(int i(10);i<20;i++) printf("* %17s %2d * %10u * %10u *\n","Board",i,GigaTrackerDAQBoardTimeStamp::NbErrFrameLSB[i],GigaTrackerDAQBoardTimeStamp::NbErrFrameLSB[i+30]);
    printf("*%s*\n",std::string(w,'-').c_str());
    for(int i(20);i<30;i++) printf("* %17s %2d * %10u * %10u *\n","Board",i,GigaTrackerDAQBoardTimeStamp::NbErrFrameLSB[i],GigaTrackerDAQBoardTimeStamp::NbErrFrameLSB[i+30]);
    printf("*%s*\n",std::string(w,'*').c_str());

    printf("* %-20s *\n", "NbErrStationId");
    for(int i(0);i<10;i++) printf("* %17s %2d * %10u *\n","Board",i,GigaTrackerDAQBoardTimeStamp::NbErrStationId[i]);
    printf("*%s*\n",std::string(w,'-').c_str());

    GigaTrackerDAQBoardTimeStamp::ResetError();

  }


  // ==============================================  
  GigaTrackerDAQBoardTimeStamp::GigaTrackerDAQBoardTimeStamp()
  {
    fTrailerOne = NULL;
    fTrailerTwo = NULL;

    //register error messages
    fErrorFlag      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_ErrorFlag");
    fErrorChip      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_ErrorChip");
    fErrorqChip     = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_ErrorqChip");
    fErrorStation   = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_ErrorStation");
    fErrorIncFcL1   = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_ErrorIncFcL1");
    fErrorHitArbiter= GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_HitArbiter");
    fErrorPixAddress= GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_PixAddress");
    fErrorPUAddress = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_PUAddress");
    fErrorDataStatus = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTimeStamp_DataStatus");

  }

  // ==============================================  
  GigaTrackerDAQBoardTimeStamp::~GigaTrackerDAQBoardTimeStamp()
  {
   
  }

  //==============================================  
  int GigaTrackerDAQBoardTimeStamp::SetBuffer(u8* buf){
    RG_CHECK_PTR_RETURN_INT(buf,1);
    for(int i(0); i<buffLength; i++){
      fBuffer[i] = buf[i];
    }

    char errBuff[80];
    //CONSISTENCY CHECK - flag must be 0x0000
    if (this->GetFlag() != GigaTrackerDAQBoardTimeStamp::rFlag){
      GigaTrackerDAQBoardTimeStamp::NbErrTag ++;
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Not a TimeStamp Flag: %u  Ref: %u  Offending Word: %8.8x",this->GetFlag(),GigaTrackerDAQBoardTimeStamp::rFlag,Word[GigaTrackerDAQBoardTimeStamp::iFlag]);
      REPORT_NEW_ERROR_RETURN(fErrorFlag,errBuff);
    }

    //CONSISTENCY CHECK - chipId must be between 0 and 9
    if (/*this->GetChipId()<0 || unsigned value, always >0 by definition*/ this->GetChipId()>9 ){
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Invalid ChipId %d Offending Word: %8.8x",this->GetChipId(),Word[GigaTrackerDAQBoardTimeStamp::iChipId]);
      REPORT_NEW_ERROR_RETURN(fErrorChip,errBuff);
    }

    //CONSISTENCY CHECK - qchipId must be between 0 and 3
    if (/*this->GetqChipId()<0 || unsigned value, always >0 by definition*/ this->GetqChipId()>3 ){
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Invalid qChipId: %d Offending Word: %8.8x",this->GetqChipId(),Word[GigaTrackerDAQBoardTimeStamp::iqChipId]);
      REPORT_NEW_ERROR_RETURN(fErrorqChip,errBuff);
    }

    //CONSISTENCY CHECK - stationId must be between 1 and 3
    if (this->GetStationId()<1 || this->GetStationId()>3 ){
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Invalid StationId: %d Offending Word: %8.8x",this->GetStationId(),Word[GigaTrackerDAQBoardTimeStamp::iStationId]);
      REPORT_NEW_ERROR_RETURN(fErrorStation,errBuff);
    }


    //CONSISTENCY CHECK - StatusBit Must be 0
    if (this->GetDataStatusSelector()!=0 ){
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Invalid Status bis: %d Offending Word: %8.8x",this->GetDataStatusSelector(),Word[GigaTrackerDAQBoardTimeStamp::iDataStatusSelector]);
      REPORT_NEW_ERROR_RETURN(fErrorDataStatus,errBuff);
    }

    return 0;
  }


  //==============================================  
  int GigaTrackerDAQBoardTimeStamp::GetBufferLength(){
    return buffLength;
  }

  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetFlag()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iFlag],mFlag,sFlag, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetChipId()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iChipId],mChipId,sChipId, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetStationId()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iStationId],mStationId,sStationId, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetFrameCountLSB()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iFrameCountLSB],mFrameCountLSB,sFrameCountLSB, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetDataStatusSelector()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iDataStatusSelector],mDataStatusSelector,sDataStatusSelector, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetPixelAddress()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iPixelAddress],mPixelAddress,sPixelAddress, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetHitArbiterAddress()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iHitArbiterAddress],mHitArbiterAddress,sHitArbiterAddress, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  int GigaTrackerDAQBoardTimeStamp::GetNbHitArbiterAddress(){
    int n=0;
    u32 hit_arbiter_address = GetHitArbiterAddress();
    for (int b(0); b < 5; ++b) {
      if (! ( (hit_arbiter_address & int(pow(2,b))) >> b) ) continue;
      n++;
    }
    return n;
  }
  
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetPileUpAddress()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iPileUpAddress],mPileUpAddress,sPileUpAddress, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  int GigaTrackerDAQBoardTimeStamp::GetNbPileUpAddress(){
    int n=0;
    u32 hit_arbiter_address = GetPileUpAddress();
    for (int b(0); b < 5; ++b) {
      if (! ( (hit_arbiter_address & int(pow(2,b))) >> b) ) continue;
      n++;
    }
    return n;
  }

  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetqChipId()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iqChipId],mqChipId,sqChipId, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetLeadingSelector()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iLeadingSelector],mLeadingSelector,sLeadingSelector, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetLeadingCoarse()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iLeadingCoarse],mLeadingCoarse,sLeadingCoarse, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetLeadingFine()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iLeadingFine],mLeadingFine,sLeadingFine, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetTotSelector()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTotSelector],mTotSelector,sTotSelector, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetTotCoarse()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTotCoarse],mTotCoarse,sTotCoarse, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetTotFine()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTotFine],mTotFine,sTotFine, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetFrameCounter()
  {
    RG_CHECK_PTR_RETURN_INT(fTrailerOne,0xffffffff);
    u32 ts = fTrailerOne->GetL1ATimeStamp();
    u32 fc = ts/256;


    return fc;    
  }

  //==============================================
  u32 GigaTrackerDAQBoardTimeStamp::GetL1ATimeStamp()
  {
    RG_CHECK_PTR_RETURN_INT(fTrailerOne,0xffffffff);
    return fTrailerOne->GetL1ATimeStamp();
  }

  //==============================================
  int GigaTrackerDAQBoardTimeStamp::SetTrailerOne(GigaTrackerDAQBoardTrailerOne* t)
  {
    RG_CHECK_PTR_RETURN_INT(t,1);
    fTrailerOne = t;
    return 0;
  }
  //==============================================
  int GigaTrackerDAQBoardTimeStamp::SetTrailerTwo(GigaTrackerDAQBoardTrailerTwo* t)
  {
    RG_CHECK_PTR_RETURN_INT(t,1);
    fTrailerTwo = t;
    return 0;
  }
  //==============================================
  double GigaTrackerDAQBoardTimeStamp::GetLeadingTime()
  {
    u32 ft=gray_to_binary(this->GetLeadingFine()); 
    u32 ct=this->GetLeadingCoarse(); 
    u32 ct_bin=gray_to_binary(ct);

    // ct disambiguity mechanism:  
    if(ft>20){
      int par=parity(ct, 12); 
      int sel=this->GetLeadingSelector(); 
      if(par!=sel){                 // parity doesn't match selector: 
	ct_bin = (ct_bin-1)&0xfff;  // if ct == 0 => ct_bin = 0xfff
      }
    }

    double time=double( (ct_bin&0x7ff) )*ClockPeriod/8.0; //3.125;
    time+=(double(ft)+0.5)*ClockPeriod/256.0;    //  time+=double(ft)*0.09765625+0.048828125;
    unsigned int fc = this->GetFrameCounter();

    // fc disambiguity mechanism: 
    if( (fc&0x1) !=  (ct_bin&0x800)>>11) fc = fc-1;     
  
    time += double(fc)*ClockPeriod*256.0;//6.4e3; // ns
    time-= this->GetL1ATimeStamp()*ClockPeriod;
    return time;
  }

  //==============================================
  double GigaTrackerDAQBoardTimeStamp::GetTrailingTime()
  {
    // First find the frame to which the hit leadind time belong
    u32 ft_lead=gray_to_binary(this->GetLeadingFine()); 
    u32 ct_lead=this->GetLeadingCoarse(); 
    u32 ct_lead_bin=gray_to_binary(ct_lead);
    u32 lead_corr(0);

    // ct disambiguity mechanism:  
    if(ft_lead>20){
      int par=parity(ct_lead, 12); 
      int sel=this->GetLeadingSelector(); 
      if(par!=sel){                            // parity doesn't match selector: 
    	ct_lead_bin = (ct_lead_bin-1)&0xfff;   // if ct == 0 => ct_bin = 0xfff
	lead_corr = 1;
      }
    }
    unsigned int fc = this->GetFrameCounter();
    if( (fc&0x1) !=  (ct_lead_bin&0x800)>>11)  fc = fc-1;

    // The trailing coarse counter should counts from the begining of the
    // leading frame. If the trailing falls in the next frame then
    // the coarse time should count over 4095.
    // In particular if raw_cc = 0 and the disambiguation puts it to 4094
    // the trailing coarse time  should be 4094 + tot and not 0+tot.
    // So the following has to be done:
    ct_lead_bin = ct_lead_bin + lead_corr;

    // Second compute the coarse trailing time
    u32 raw_ct_lead_bin=gray_to_binary(ct_lead); //reset to original value
    u32 tot=this->GetTotCoarse(); 
    u32 ct_trail_bin=(tot+raw_ct_lead_bin)&0xfff;
    u32 ct_trail=binary_to_gray(ct_trail_bin);
    u32 ft_trail=gray_to_binary(this->GetTotFine()); 

    ct_trail_bin = ct_lead_bin+tot;
    if(ft_trail>20){
      int par=parity(ct_trail,12); 
      int sel=this->GetTotSelector();
      if(par!=sel){ 
	ct_trail_bin--;
      }
    }
    double time=double(ct_trail_bin)*ClockPeriod/8.0; //3.125 ns;
    time+=(double(ft_trail)+0.5)*ClockPeriod/256.0;   //time+=double(ft_trail)*0.09765625+0.048828125;
    time+= double(fc&0xfffffffe)*ClockPeriod*256.0;   //6.4e3 ns
    time-= this->GetL1ATimeStamp()*ClockPeriod;
    return time; 
  }

  //==============================================
  int GigaTrackerDAQBoardTimeStamp::GetPixelUID( int refiring, bool sensorUpstream ){
    //Chips in GTK are labelled as:
    //  [for Sensor Downstream] 
    //
    //       ┌─┬─┬─┬─┬─┐
    //       │4│3│2│1│0│
    //  Jura ├─┼─┼─┼─┼─┤ Saleve
    //       │9│8│7│6│5│
    //       └─┴─┴─┴─┴─┘
    //
    // Quarter Chip:
    //
    //       ┌─┬─┬─┬─┐
    //  Jura │3│2│1│0│ Saleve
    //       └─┴─┴─┴─┘
    //
    // Columns:
    //  Jura |39,38....,1,0| saleve
    //
    //
    //Within one qchip pixel are labelled by:
    // -hit arbiter (ha) an
    // -pixel group address pga
    //zzWhich define the pixel as:
    // Saleve                         Jura
    //    ┌─────┬─────┬───────────┬─────┐
    //    │ 8   │16   │           │ 89  │
    //    │ /   │ /   │           │  /  │   
    //    │ /   │ /   │           │  /  │   
    // S  │ 3  4│12  4│   . . .   │ 84 4│
    //    │ 2   │11   │           │ 83  │  
    // A  │ 1   │10   │           │ 82  │  
    //    │ 0   │ 9   │           │ 81  │
    // L  ├─────┴─────┴───────────┴─────┤  
    //    │                 .           │
    // E  │               . . .         │
    //    │                 .           │
    // V  ├─────┬─────┬───────────┬─────┤
    //    │ 8   │16   │           │ 89  │
    // E  │ /   │ /   │           │  /  │   
    //    │ /   │ /   │           │  /  │   
    //    │ 3  1│12  1│   . . .   │ 84 1│
    //    │ 2   │11   │           │ 83  │  
    //    │ 1   │10   │           │ 82  │  
    //    │ 0   │ 9   │           │ 81  │  
    //    ├─────┼─────┼───────────┼─────┤
    //    │ 8   │16   │           │ 89  │
    //    │ /   │ /   │           │  /  │   
    //    │ /   │ /   │           │  /  │   
    //    │ 3  0│12  0│   . . .   │ 84 0│
    //    │ 2   │11   │           │ 83  │  
    //    │ 1   │10   │           │ 82  │  
    //    │ 0   │ 9   │           │ 81  │  
    //    ├─────┼─────┼───────────┼─────┤
    //    │a  pg│h  pg│           │h  pg│
    //
 
    u32 qchip = this->GetqChipId();
    u32 pixel_group_address = this->GetPixelAddress();

    u32 hit_arbiter_address; // hit or pileup
    if (refiring < this->GetNbHitArbiterAddress()) hit_arbiter_address = this->GetHitArbiterAddress();
    else {
      hit_arbiter_address = this->GetPileUpAddress();
      refiring-= this->GetNbHitArbiterAddress();
    }
    
    u32 x = qchip*10 + (pixel_group_address/9); 

    u32 hit_arbiter_idx=0; 
    //Hit arbiter are coded as
    // 0b00001 -> 0
    // 0b00010 -> 1
    // 0b00100 -> 2
    // 0b01000 -> 3
    // 0b10000 -> 4
    // If N pixels fire at the same time you get N bits at 1
    int index(0);
    for(int b(0);b<5;b++){
      if (! ( (hit_arbiter_address & int(pow(2,b))) >> b) ) continue;
      if(index == refiring){
	hit_arbiter_idx = b;
	break;
      }
      index++;
    }
    
    u32 y = pixel_group_address%9 + hit_arbiter_idx*9;

    u32 chipid  = this->GetChipId() ;
    if(chipid>4){
      x = 39-x;
      y = 44-y;    
    }

    
    if (sensorUpstream) {
      chipid = (chipid+5)%10;
      y = 44-y; 
    }
    
    int UID = x+chipid%5*40 + y*200 + chipid/5 * 200*45;
    return UID;

  }

  //==============================================
  std::ostream& operator<< (std::ostream &out, GigaTrackerDAQBoardTimeStamp & ts){

    u32* pU32;
    pU32 = (u32*) &(ts.fBuffer[0]);
    for(int i(0); i<ts.GetBufferLength()/4;i++){
      out << "  0x"<< std::hex<<std::setfill('0')<<std::setw(8)<<pU32[i]<<"\n"<<std::dec;
     }
    out<<"   Flag               : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetFlag()<<"\n";
    out<<"   ChipId             : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetChipId()<<"\n";
    out<<"   StationId          : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetStationId()<<"\n";
    out<<"   FrameCountLSB      : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetFrameCountLSB()<<std::hex<<" (0x"<<ts.GetFrameCountLSB()<<")\n"<<std::dec;
    out<<"   DataStatusSelector : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetDataStatusSelector()<<"\n";
    out<<"   PixelAddress       : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetPixelAddress()<<"\n";
    out<<"   HitArbiterAddress  : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetHitArbiterAddress()<<"\n";
    out<<"   PileUpAddress      : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetPileUpAddress()<<"\n";
    out<<"   qChipId            : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetqChipId()<<"\n";

    out<<"   LeadingSelector    : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetLeadingSelector()<<"\n";
    out<<"   LeadingCoarse      : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetLeadingCoarse()<<std::hex<<" (0x"<<ts.GetLeadingCoarse()<<")\n"<<std::dec;
    out<<"   LeadingFine        : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetLeadingFine()<<std::hex<<" (0x"<<ts.GetLeadingFine()<<")\n"<<std::dec;
    out<<"   TotSelector        : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetTotSelector()<<std::hex<<" (0x"<<ts.GetTotSelector()<<")\n"<<std::dec;
    out<<"   TotCoarse          : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetTotCoarse()<<std::hex<<" (0x"<<ts.GetTotCoarse()<<")\n"<<std::dec;
    out<<"   TotFine            : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.GetTotFine()<<std::hex<<" (0x"<<ts.GetTotFine()<<")\n"<<std::dec;

    out<<"   TriggerTimeStamp   : "<<std::setfill(' ')<<std::setw(8)<<std::left<<ts.fTrailerOne->GetL1ATimeStamp()<<std::hex<<" (0x"<<ts.fTrailerOne->GetL1ATimeStamp()<<")"<<std::dec;
    //out<<"FrameCounter       : "<<std::setfill(' ')<<std::setw(8)<<std::left<<GetFrameCounter()<<"\n";
    return out;
  }

  //==============================================
  bool operator == (  GigaTrackerDAQBoardTimeStamp & ts1,  GigaTrackerDAQBoardTimeStamp & ts2){
    bool equal = true;
    for(int i(0);i<GigaTrackerDAQBoardTimeStamp::buffLength;i++){
      equal = equal && (ts1.fBuffer[i] == ts2.fBuffer[i]);
    }
    return equal;
  }

}//~namespace IImaS


