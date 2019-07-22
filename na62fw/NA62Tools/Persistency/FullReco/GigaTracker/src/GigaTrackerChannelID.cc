// --------------------------------------------------------------
// History:
//
// Bob Velghe (bob.velghe@cern.ch) 2014-11-13
//  - Add UID field
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-10-26
//
// --------------------------------------------------------------
#include "GigaTrackerChannelID.hh"
#include "TMath.h"

ClassImp(GigaTrackerChannelID)


GigaTrackerChannelID::GigaTrackerChannelID() :
    fStationNo(-1),
    fChipPixelID(-1),
    fChipID(0)
{
}

GigaTrackerChannelID::GigaTrackerChannelID(const GigaTrackerChannelID & chid) :
    fStationNo(chid.fStationNo),
    fChipPixelID(chid.fChipPixelID),
    fChipID(chid.fChipID)
{
}

GigaTrackerChannelID::~GigaTrackerChannelID() {}

void GigaTrackerChannelID::Clear(Option_t* /*option*/) {
    fStationNo = -1;
    fChipPixelID = -1;
    fChipID = 0;
}

Int_t  GigaTrackerChannelID::GetStationNo() const { 
    /// \MemberDescr
    /// \return The Station Number between 0 and 2
    /// \EndMemberDescr
    return fStationNo;                    
}

void   GigaTrackerChannelID::SetStationNo(Int_t value) { 
    fStationNo = value;                   
}

Int_t  GigaTrackerChannelID::GetPixelID() const { 
    /// \MemberDescr
    /// \return The Pixel ID in the station (0 to 17999).
    ///
    /// The pixel with uid 0 is the bottom jura-side, then the numbering goes as:
    ///   ┌─────┬─────┬─────┬─────┬─────┐
    ///   │             ...         1799│
    /// J │                             │
    /// U │                             │
    /// R │                             │
    /// A │200          ...          399│
    ///   │0            ...          199│
    ///   └─────┴─────┴─────┴─────┴─────┘  
    ///
    /// In case of detector with upstream sensor, the UID is corrected. Hence the UID returned by the function
    /// can be treated as for downstream sensor stations.
    /// \EndMemberDescr
    Int_t row = fChipPixelID/40 + 45*(fChipID/5);
    Int_t column = 40*(fChipID%5) + fChipPixelID%40;
    return 200*row + column;
}

void   GigaTrackerChannelID::SetPixelID(Int_t value) { 
    Int_t row = value/200;
    Int_t column = value%200;
    fChipID = column/40 + 5*(row/45);
    fChipPixelID = column%40 + 40*(row%45);                     
}

UInt_t GigaTrackerChannelID::EncodeChannelID() {
    /// \MemberDescr
    /// \return The Pixel UID. The 5 first digits are the pixel ID in the station, while the 6th digit is the station ID: uid from 100000 to 117999 are GTK1 pixel, 200000 to 217999 GTK2 and, 300000 to 317999 GTK3.
    ///
    /// The pixel with uid 0 is the bottom jura-side, then the numbering goes as:
    /// ┌─────┬─────┬─────┬─────┬─────┐
    /// │                         1799│
    /// │                             │
    /// │                             │
    /// │200          ...          399│
    /// │0            ...          199│
    /// └─────┴─────┴─────┴─────┴─────┘  
    ///
    /// In case of detector with upstream sensor, the ID is corrected. Hence the ID returned by the function
    /// can be treated as for downstream sensor stations.
  
    return fStationNo*100000+fChipID*10000+fChipPixelID;
}

GigaTrackerChannelID::chIDDecoded GigaTrackerChannelID::DecodeChannelID_Static(Long_t UID) {
  chIDDecoded ret;
  ret.fStationNo = UID/100000;
  ret.fChipID = (UID/10000)%10;
  ret.fChipPixelID = UID%10000;
  return ret;
}

void GigaTrackerChannelID::DecodeChannelID(Long_t UID) {
  chIDDecoded ret = DecodeChannelID_Static(UID);
  fStationNo   = ret.fStationNo;
  fChipID      = ret.fChipID;
  fChipPixelID = ret.fChipPixelID;
}

Int_t GigaTrackerChannelID::GetChipID() const {
  /// \MemberDescr
  /// \return The Chip ID between 0 and 9
  /// \EndMemberDescr
  return  fChipID;
}

UInt_t GigaTrackerChannelID::GetqChipID(){
  /// \MemberDescr
  /// \return The QChip ID between 0 and 3
  /// \EndMemberDescr
  return  TMath::Abs(3*(Int_t)(fChipID/5) - (fChipPixelID%40)/10);
}

Double_t GigaTrackerChannelID::GetPixelXPosition() {
  // checked 24/11/2016 ok mpt
  //  nota: 12.2 is 61.0/5
  return fChipID%5 * 12.2 + fChipPixelID%40 * 0.3 + 0.2 + 0.05*(fChipPixelID%40 > 0) + 0.05*(fChipPixelID%40 > 38) + (fChipID%5 == 0 && fChipPixelID%40 ==0)*0.05  - (fChipID%5 == 4 && fChipPixelID%40 ==39)*0.05;
}

Double_t GigaTrackerChannelID::GetPixelYPosition() {
    return fChipID/5 * 13.5 + fChipPixelID/40 * 0.3 + 0.150;
}

TVector3 GigaTrackerChannelID::GetRawPosition() {
    return GetRawPosition(fStationNo, GetPixelXPosition(), GetPixelYPosition());
}

TVector3 GigaTrackerChannelID::GetRawPosition(Int_t stationNo, Double_t pixelX, Double_t pixelY) {
    Double_t X = pixelX - 30.5;
    Double_t Y = pixelY - 13.5;
    Double_t Z = 0.0;
    if(stationNo == 0) Z =  79.600 * 1.e3 - 0.5 * 200.e-6 * 1.e3;
    if(stationNo == 1) Z =  92.800 * 1.e3 - 0.5 * 200.e-6 * 1.e3;
    if(stationNo == 2) Z = 102.400 * 1.e3 - 0.5 * 200.e-6 * 1.e3;

    return TVector3(-X,-Y,Z);
}

UInt_t GigaTrackerChannelID::GetColumn(){
  /// \MemberDescr
  /// return the column index from 0 to 200
  return GetPixelID()%200;
  ///  \EndMemberDesc

}

UInt_t GigaTrackerChannelID::GetRow(){
  /// \MemberDescr
  /// return the row index from 0 to 90
  return GetPixelID()/200;
  ///  \EndMemberDesc

}

UInt_t GigaTrackerChannelID::GetColumn(Int_t ChipID, Int_t ChipPixelID) {
    return 40*(ChipID%5) + ChipPixelID%40;
}

UInt_t GigaTrackerChannelID::GetRow(Int_t ChipID, Int_t ChipPixelID) {
    return ChipPixelID/40 + 45*(ChipID/5);
}
