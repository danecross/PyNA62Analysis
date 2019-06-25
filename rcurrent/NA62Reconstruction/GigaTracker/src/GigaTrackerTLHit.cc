#include "GigaTrackerTLHit.hh" 
#include "GigaTrackerTLMasks.hh"
#include "RG_Utilities.hh"

namespace GTK
{

  GigaTrackerTLHit::GigaTrackerTLHit()
  {
    fDelay = 0;
    fSourceId = 0xffffffff;
    fStationId = 0xffffffff;
    fFrameCounter = 0xffffffff;
    fChipId = 0xffffffff;
    fqChipId = 0xffffffff;
    fNCollisions = 0xffffffff;
    
    fPixelAddress = 0xffffffff;
    fHitArbiterAddress = 0xffffffff;
    fPileUpAddress = 0xffffffff;
    
    fLeadingSelector = 0xffffffff;
    fLeadingCoarse = 0xffffffff;
    fLeadingFine = 0xffffffff;
    
    fTotSelector = 0xffffffff;
    fTotCoarse = 0xffffffff;
    fTotFine = 0xffffffff;

    fSensorUpstream = 0;

  }


  GigaTrackerTLHit::~GigaTrackerTLHit()
  {
   
  }


  void GigaTrackerTLHit::ReadFromBuffer(unsigned int* Buff, unsigned int& OffSet){

    fChipId = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_hit_ChipId,GigaTrackerTLMask::s_hit_ChipId);
    fqChipId = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_hit_qChipId,GigaTrackerTLMask::s_hit_qChipId);
    fNCollisions = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_hit_NCollisions,GigaTrackerTLMask::s_hit_NCollisions);
    fPixelAddress = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_hit_PixelAddress,GigaTrackerTLMask::s_hit_PixelAddress);
    fHitArbiterAddress = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_hit_HitArbiterAddress,GigaTrackerTLMask::s_hit_HitArbiterAddress);
    fPileUpAddress = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_hit_PileUpAddress,GigaTrackerTLMask::s_hit_PileUpAddress);

    fLeadingSelector = GigaTrackerTLMask::GetUInt(Buff[1+OffSet],GigaTrackerTLMask::m_hit_LeadingSelector,GigaTrackerTLMask::s_hit_LeadingSelector);
    fLeadingCoarse = GigaTrackerTLMask::GetUInt(Buff[1+OffSet],GigaTrackerTLMask::m_hit_LeadingCoarse,GigaTrackerTLMask::s_hit_LeadingCoarse);
    fLeadingFine = GigaTrackerTLMask::GetUInt(Buff[1+OffSet],GigaTrackerTLMask::m_hit_LeadingFine,GigaTrackerTLMask::s_hit_LeadingFine);
    fTotSelector = GigaTrackerTLMask::GetUInt(Buff[1+OffSet],GigaTrackerTLMask::m_hit_TotSelector,GigaTrackerTLMask::s_hit_TotSelector);
    fTotCoarse = GigaTrackerTLMask::GetUInt(Buff[1+OffSet],GigaTrackerTLMask::m_hit_TotCoarse,GigaTrackerTLMask::s_hit_TotCoarse);
    fTotFine = GigaTrackerTLMask::GetUInt(Buff[1+OffSet],GigaTrackerTLMask::m_hit_TotFine,GigaTrackerTLMask::s_hit_TotFine);


    DBG(printf("   Read:            \n"));
    DBG(printf(" >> 0x%08x\n",Buff[0+OffSet]));
    DBG(printf(" >> 0x%08x\n",Buff[1+OffSet]));
    
    DBG(printf("      ChipId            : %u\n",fChipId));
    DBG(printf("      QChipId           : %u\n",fqChipId));
    DBG(printf("      Ncollistions      : %u\n",fNCollisions));
    DBG(printf("      PixelAddress      : %u\n",fPixelAddress));
    DBG(printf("      HitArbiterAddress : %u\n",fHitArbiterAddress));
    DBG(printf("      PileUpAddres      : %u\n",fPileUpAddress));
    DBG(printf("      LeadingCoars      : %u\n",fLeadingCoarse));
    DBG(printf("      LeadingFine       : %u\n",fLeadingFine));
    DBG(printf("      TotCoarse         : %u\n",fTotCoarse));
    DBG(printf("      TotFin            : %u\n",fTotFine));
    DBG(printf("      -------------------------\n"));

    OffSet = OffSet+2;
  }


  void GigaTrackerTLHit::WriteToBuffer(unsigned int* Buff, unsigned int& OffSet){

    DBG(printf("   Write:            \n"));
    //DBG(printf("      Offset            : %u\n",OffSet ));
    DBG(printf("      ChipId            : %u\n",fChipId));
    DBG(printf("      QChipId           : %u\n",fqChipId));
    DBG(printf("      Ncollistions      : %u\n",fNCollisions));
    DBG(printf("      PixelAddress      : %u\n",fPixelAddress));
    DBG(printf("      HitArbiterAddress : %u\n",fHitArbiterAddress));
    DBG(printf("      PileUpAddres      : %u\n",fPileUpAddress));
    DBG(printf("      LeadingCoars      : %u\n",fLeadingCoarse));
    DBG(printf("      LeadingFine       : %u\n",fLeadingFine));
    DBG(printf("      TotCoarse         : %u\n",fTotCoarse));
    DBG(printf("      TotFin            : %u\n",fTotFine));


     GigaTrackerTLMask::SetUInt(Buff+OffSet,fChipId,GigaTrackerTLMask::m_hit_ChipId,GigaTrackerTLMask::s_hit_ChipId);
     GigaTrackerTLMask::SetUInt(Buff+OffSet,fqChipId,GigaTrackerTLMask::m_hit_qChipId,GigaTrackerTLMask::s_hit_qChipId);
     GigaTrackerTLMask::SetUInt(Buff+OffSet,fNCollisions,GigaTrackerTLMask::m_hit_NCollisions,GigaTrackerTLMask::s_hit_NCollisions);
     GigaTrackerTLMask::SetUInt(Buff+OffSet,fPixelAddress,GigaTrackerTLMask::m_hit_PixelAddress,GigaTrackerTLMask::s_hit_PixelAddress);
     GigaTrackerTLMask::SetUInt(Buff+OffSet,fHitArbiterAddress,GigaTrackerTLMask::m_hit_HitArbiterAddress,GigaTrackerTLMask::s_hit_HitArbiterAddress);
     GigaTrackerTLMask::SetUInt(Buff+OffSet,fPileUpAddress,GigaTrackerTLMask::m_hit_PileUpAddress,GigaTrackerTLMask::s_hit_PileUpAddress);

     GigaTrackerTLMask::SetUInt(Buff+OffSet+1,fLeadingSelector,GigaTrackerTLMask::m_hit_LeadingSelector,GigaTrackerTLMask::s_hit_LeadingSelector);
     GigaTrackerTLMask::SetUInt(Buff+OffSet+1,fLeadingCoarse,GigaTrackerTLMask::m_hit_LeadingCoarse,GigaTrackerTLMask::s_hit_LeadingCoarse);
     GigaTrackerTLMask::SetUInt(Buff+OffSet+1,fLeadingFine,GigaTrackerTLMask::m_hit_LeadingFine,GigaTrackerTLMask::s_hit_LeadingFine);

     GigaTrackerTLMask::SetUInt(Buff+OffSet+1,fTotSelector,GigaTrackerTLMask::m_hit_TotSelector,GigaTrackerTLMask::s_hit_TotSelector);
     GigaTrackerTLMask::SetUInt(Buff+OffSet+1,fTotCoarse,GigaTrackerTLMask::m_hit_TotCoarse,GigaTrackerTLMask::s_hit_TotCoarse);
     GigaTrackerTLMask::SetUInt(Buff+OffSet+1,fTotFine,GigaTrackerTLMask::m_hit_TotFine,GigaTrackerTLMask::s_hit_TotFine);

     DBG(printf(" >> 0x%08x\n",*(Buff+OffSet)));
     DBG(printf(" >> 0x%08x\n",*(Buff+OffSet+1)));
     DBG(printf("      -------------------------\n"));

     OffSet = OffSet+2;
     return;
    
  }


  

  void GigaTrackerTLHit::SetSourceId(unsigned int id){
    fSourceId=id;
  }
  
  unsigned int GigaTrackerTLHit::GetSourceId()const{
    return fSourceId;
  }
  

  void GigaTrackerTLHit::SetStationId(unsigned int id){
    fStationId = id;
  }

  unsigned int GigaTrackerTLHit::GetStationId()const{
    return fStationId;
  }


  void GigaTrackerTLHit::SetDelay(float d){
    fDelay = d;
  }

  float GigaTrackerTLHit::GetDelay()const{
    return fDelay;
  }


  void GigaTrackerTLHit::SetSensorUpstream(bool v){
    fSensorUpstream = v;
  }

  bool GigaTrackerTLHit::GetSensorUpstream()const{
    return fSensorUpstream;
  }


  void GigaTrackerTLHit::SetFrameCounter(unsigned int f){
    fFrameCounter = f;
  }

  unsigned int GigaTrackerTLHit::GetFrameCounter()const{
    return fFrameCounter;
  }


  unsigned int GigaTrackerTLHit::GetChipId()const{
    return fChipId;
  }

  void GigaTrackerTLHit::SetChipId(unsigned int id){
    fChipId = id;
  }


  unsigned int GigaTrackerTLHit::GetqChipId()const{
    return fqChipId;
  }

  void GigaTrackerTLHit::SetqChipId(unsigned int id){
    fqChipId = id;
  }


  unsigned int GigaTrackerTLHit::GetNCollisions()const{
    return fNCollisions;
  }

  void GigaTrackerTLHit::SetNCollisions(unsigned int n){
    fNCollisions = n;
  }


  unsigned int GigaTrackerTLHit::GetPixelAddress()const{
    return fPixelAddress;
  }

  void GigaTrackerTLHit::SetPixelAddress(unsigned int a){
    fPixelAddress = a;
  }


  unsigned int GigaTrackerTLHit::GetHitArbiterAddress()const{
    return fHitArbiterAddress;
  }

  void GigaTrackerTLHit::SetHitArbiterAddress(unsigned int a){
    fHitArbiterAddress = a;
  }


  unsigned int GigaTrackerTLHit::GetPileUpAddress()const{
    return fPileUpAddress;
  }

  void GigaTrackerTLHit::SetPileUpAddress(unsigned int a){
    fPileUpAddress = a;
  }


  unsigned int GigaTrackerTLHit::GetLeadingSelector()const{
    return fLeadingSelector;
  }

  void GigaTrackerTLHit::SetLeadingSelector(unsigned int t){
    fLeadingSelector = t;
  }


  unsigned int GigaTrackerTLHit::GetLeadingCoarse()const{
    return fLeadingCoarse;
  }

  void GigaTrackerTLHit::SetLeadingCoarse(unsigned int t){
    fLeadingCoarse = t;
  }


  unsigned int GigaTrackerTLHit::GetLeadingFine()const{
    return fLeadingFine;
  }

  void GigaTrackerTLHit::SetLeadingFine(unsigned int t){
    fLeadingFine = t;
  }


  unsigned int GigaTrackerTLHit::GetTotCoarse()const{
    return fTotCoarse;
  }

  void GigaTrackerTLHit::SetTotCoarse(unsigned int t){
    fTotCoarse = t;
  }


  unsigned int GigaTrackerTLHit::GetTotSelector()const{
    return fTotSelector;
  }

  void GigaTrackerTLHit::SetTotSelector(unsigned int t){
    fTotSelector = t;
  }


  unsigned int GigaTrackerTLHit::GetTotFine()const{
    return fTotFine;
  }

  void GigaTrackerTLHit::SetTotFine(unsigned int t){
    fTotFine = t;
  }


  double GigaTrackerTLHit::GetLeadingTime()const
  {
    unsigned int ft=gray_to_binary(this->GetLeadingFine()); 
    unsigned int ct=this->GetLeadingCoarse(); 
    unsigned int ct_bin=gray_to_binary(ct);
    if(ft>20){// && ft<30){// use disambiguity mechanism:  
      int par=parity(ct, 12); 
      int sel=this->GetLeadingSelector(); 
      if(par!=sel){// parity doesn't match selector: 
	ct_bin = (ct_bin-1)&0xfff;  //what to do it ct == 0 => ct_bin = 0xfff ?? BUGG in matt's?
      }
    }

    // basic calculation
    double time=double( (ct_bin&0x7ff) )*3125.;// ps/clk cycle 
    time+=double(ft)*97.65625+48.828125;//=97.65625/2.0;// bin centre: 

    unsigned int fc = this->GetFrameCounter();
    //  Due to queueing some time stamps cannot be sent in the frame they belong to
    //  and get sent only in the next frame, so the frame counter has to be corrected.
    if( (fc&0x1) !=   ((ct_bin&0x800)>>11)) {
      fc = fc-1;     
    }

    time += double(fc)*6.4e6; // ps/frame
    return time;
  } 


  
  double GigaTrackerTLHit::GetTrailingTime()const
  {
    // First find the frame to which the hit leadind time belong
    unsigned int ft_lead=gray_to_binary(this->GetLeadingFine()); 
    unsigned int ct_lead=this->GetLeadingCoarse(); 
    unsigned int ct_lead_bin=gray_to_binary(ct_lead);
    unsigned int lead_corr(0);

    if(ft_lead>20){// && ft<30){// use disambiguity mechanism:  
      int par=parity(ct_lead, 12); 
      int sel=this->GetLeadingSelector(); 
      if(par!=sel){// parity doesn't match selector: 
    	ct_lead_bin = (ct_lead_bin-1)&0xfff;
	lead_corr = 1;
      }
    }
    unsigned int fc = this->GetFrameCounter();
    if( ( (fc&0x1)) !=  ((ct_lead_bin&0x800)>>11) )  fc = fc-1;

    // The trailing coarse counter should counts from the begining of the
    // leading frame. If the trailing falls in the next frame then
    // the coarse time should count over 4095.
    // In particular if raw_cc = 0 and the disambiguation puts it to 4094
    // the trailing coarse time  should be 4094 + tot and not 0+tot.
    // So the following has to be done:
    ct_lead_bin = ct_lead_bin + lead_corr;

    // Second compute the coarse trailing time
    unsigned short raw_ct_lead_bin=gray_to_binary(ct_lead); //reset to original value
    unsigned short tot=this->GetTotCoarse(); 
    unsigned short ct_trail_bin=(tot+raw_ct_lead_bin)&0xfff;
    unsigned short ct_trail=binary_to_gray(ct_trail_bin);
    unsigned short ft_trail=gray_to_binary(this->GetTotFine()); 

    ct_trail_bin = ct_lead_bin+tot;
    if(ft_trail>20){
      int par=parity(ct_trail,12); 
      int sel=this->GetTotSelector();
      if(par!=sel){// parity doesn't match selector:
	ct_trail_bin--;
      }
    }
    double time=double(ct_trail_bin)*3125.;
    time+=double(ft_trail)*97.65625+48.828125;
    time += double(fc&0xfffffffe)*6.4e6;
    
    return time; 
  }



  int GigaTrackerTLHit::GetPixelUID()const{
    //Chips in GTK are labelled as:
    //  [for Sensor Upstream] 
    //      ┌─┬─┬─┬─┬─┐
    //      │5│6│7│8│9│
    // Jura ├─┼─┼─┼─┼─┤ Saleve
    //      │0│1│2│3│4│
    //      └─┴─┴─┴─┴─┘
    //

    unsigned int qchip = this->GetqChipId();
    unsigned int pixel_group_address = this->GetPixelAddress();
    unsigned int hit_arbiter_address = this->GetHitArbiterAddress();


    if(qchip == 0) qchip =2;
    else if(qchip == 1) qchip =0;
    else if(qchip == 2) qchip =1;
    else if(qchip == 3) qchip =3;
    unsigned int x = qchip*10+(9-pixel_group_address/9); 


    unsigned int hit_arbiter_idx=0; 
    switch(hit_arbiter_address){
    case 1:
      hit_arbiter_idx=0; 
      break;
    case 2:
      hit_arbiter_idx=1;
      break;
    case 4:
      hit_arbiter_idx=2;
      break;
    case 8:
      hit_arbiter_idx=3;
      break;
    case 16:
      hit_arbiter_idx=4;
      break;
    default:
      return -1; 
    };

    unsigned int y = pixel_group_address%9+hit_arbiter_idx*9;

    unsigned int chipid  = this->GetChipId();

    if(chipid<4){
      x = 39-x;
      y = 44-y;    
    }

    
    if (!this->GetSensorUpstream()) {
      chipid = (chipid+5)%10;
    }
    else  y = 44-y; 






    int UId = x+chipid%5*40 + y*200 + chipid/5 * 200*45;
    
    //    printf("Pixel UId: %d  x: %u y:%u  chipId: %u\n",UId,x,y,chipid);
    return UId;

  }

  std::ostream & operator<<(std::ostream & os, 
				   const GigaTrackerTLHit & hit){

    os<<"        ------------ Hit ------------\n";
    os<<"         Source Id            = "<< hit.GetSourceId() <<"\n";
    os<<"         Station Id           = "<< hit.GetStationId() <<"\n";
    os<<"         frame counter        = "<<hit.GetFrameCounter()<<"\n";
    os<<"         chip Id              = "<<hit.GetChipId()<<"\n"; 
    os<<"         qchip Id             = "<<hit.GetqChipId()<<"\n"; 
    os<<"         Sensor Upstream      = "<<hit.GetSensorUpstream()<<"\n"; 
    os<<"         nb Collision         = "<<hit.GetNCollisions()<<"\n"; 

    os<<"         pixel address         = "<<hit.GetPixelAddress()<<"\n"; 
    os<<"         hit arb. address      = "<<hit.GetHitArbiterAddress()<<"\n"; 
    os<<"         pile up address       = "<<hit.GetPileUpAddress()<<"\n"; 

    os<<"         leading ambiguity bit = "<<hit.GetLeadingSelector()<<"\n";        
    os<<"         leading fine time     = "<<hit.GetLeadingFine()<<"\n"; 
    os<<"         leading coarse time   = "<<hit.GetLeadingCoarse()<<"[g] -> "<<gray_to_binary(hit.GetLeadingCoarse())<<"[b]\n";  

    os<<"         tot ambiguity bit     = "<<hit.GetTotSelector()<<"\n";      
    os<<"         tot fine time         = "<<hit.GetTotFine()<<"\n"; 
    os<<"         tot coarse time       = "<<hit.GetTotCoarse()<<"\n";


    return os;

  }

  // std::ostream & operator<<(std::ostream & os, 
  // 			    const TDCPixTimeStamp & ts)
  // {
  //   int channel=ts.mBuf[TDCPIX_TS_BUFFER_SIZE-1];

  //   os<<"-----------------------------------------------\n";
  //   if(ts.IsTimeStamp()){
  //     // this is a real time stamp: 
  //     os<<"\n\n channel = "<<channel<<" : \n";
  //     os<<" time stamp data = ";
  //     os<<"0x";
  //     for(unsigned int i(0); i!=TDCPIX_TS_BUFFER_SIZE; i++){
  // 	int temp=ts.mBuf[TDCPIX_TS_BUFFER_SIZE-i-1];
  // 	os.width(2);os.fill('0');
  // 	os<<std::hex<<temp;
  //     }
  //     os<<std::dec; 
  //     os<<"\n";
  //     os<<"   qchip address         = "<<ts.GetQChip()<<"\n"; 
  //     os<<"   pixel address         = "<<ts.GetPixelGroupAddress()<<"\n"; 
   
  //     os<<"   hit address           = "<<ts.GetHitAddress()<<"\n"; 
  //     os<<"   pile up address       = "<<ts.GetPileUpAddress()<<"\n"; 

  //     os<<"   frame counter         = "<<ts.GetFrameCount()<<"\n";

  //     os<<"   leading fine time     = "<<ts.GetLeadingFineTime()<<"\n"; 
  //     os<<"   leading coarse time   = "<<ts.GetLeadingCoarseTime()<<"[g] -> "<<gray_to_binary(ts.GetLeadingCoarseTime())<<"[b]\n";  
  //     os<<"   leading ambiguity bit = "<<ts.GetLeadingCoarseTimeSelector()<<"\n";  
    
  //     os<<"   trailing fine time    = "<<ts.GetTrailingFineTime()<<"\n"; 
  //     os<<"   trailing coarse time  = "<<ts.GetTrailingCoarseTime()<<"\n";// -> "<<gray_to_binary(ts.GetTrailingCoarseTime())<<"\n"; 
  //     os<<"   trailing ambiguity bit= "<<ts.GetTrailingCoarseTimeSelector()<<"\n";  
    
  //     os<<"     row    = "<<ts.GetNaturalPixelIndex()<<"\n";
  //     os<<"     column = "<<ts.GetNaturalColumnIndex()<<"\n";

  //   }else{
  //     // this is a frame sync word or a test word: 
  //     //os<<"   Frame Count          = "<<std::dec<<ts.GetFrameCount()<<"\n";

  //     os<<"\n\n channel = "<<channel<<" : \n";
  //     os<<" frame sync word = ";
  //     os<<"  0x";
  //     for(unsigned int i(0); i!=TDCPIX_TS_BUFFER_SIZE; i++){
  // 	int temp=ts.mBuf[TDCPIX_TS_BUFFER_SIZE-i-1];
  // 	os.width(2); 
  // 	os.fill('0');
  // 	os<<std::hex<<temp<<' ';
  //     }
  //     os<<"\n";
  //     os<<"   hit count            = "<<std::dec<<ts.GetHitCount() <<"\n";
  //     os<<"   collision count      = "<<std::dec<<ts.GetQChipCollisionCount()<<"\n"; 
  //     os<<"   Frame Count          = "<<std::dec<<ts.GetFrameCount()<<"\n";

  //   }
  
  //   return os; 
  // }

}//~namespace IImaS


