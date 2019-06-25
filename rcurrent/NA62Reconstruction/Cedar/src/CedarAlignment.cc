#include "CedarAlignment.hh"
#include "NA62ConditionsService.hh"
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <queue>
#include <stdexcept>
#include <algorithm>
#include "TFile.h"
#include "TObjArray.h"
#include "TKey.h"
#include "TSpectrum2.h"
#include "TRotation.h"
#include "TMath.h"

using namespace std;

namespace CedarAlignment{

  /// \class PMTProperties
  /// \Brief
  /// A map between PMT position IDs and hardware efficiencies
  /// \EndBrief
  ///
  /// \Detailed
  /// The data come from two files. One file is a mapping between position IDs 
  ///  (i.e 100*octant+10*row+colum), and hardware IDs (eg HB1234). The other file
  /// is a mapping between hardware IDs and measured efficiencies (normalized to 1.0).
  /// \n
  ///
  /// Usage: call GetPositionEfficiency( Int_t PosID) to get the efficiency for a 
  /// particular position.
  ///
  //  This class is used by the other alignment classes
  //  \EndDetailed

  PMTProperties::PMTProperties() : fLoaded(false ) {}

  PMTProperties::PMTProperties(TString PMTPositions, TString PMTEffs ) : fLoaded(false ) {
    LoadMaps( PMTPositions, PMTEffs );
  }

  void PMTProperties::LoadMaps(  TString PMTPositions, TString PMTEffs)
  {
    //buffers for streaming
    TString hardware_id;
    Int_t pmt_pos;
    double pmt_eff;

    //Load PMT hardware positions
    if (NA62ConditionsService::GetInstance()->Open(PMTPositions)!=kSuccess) {
      std::cout << "CedarAlignment: Couldn't open " << PMTPositions 
        << ". Not doing individual efficiency corrections\n";
      return;
    }

    for( TString buf;  buf.ReadLine( NA62ConditionsService::GetInstance()->Get(PMTPositions));  ) {
      std::stringstream ss( buf.Data() );
      ss >> pmt_pos >> hardware_id;
      fPosHardware.insert( make_pair( pmt_pos, hardware_id ) );
    }
    NA62ConditionsService::GetInstance()->Close(PMTPositions);

    //Load PMT efficiencies
    if (NA62ConditionsService::GetInstance()->Open(PMTEffs)!=kSuccess) {
      std::cout << "CedarAlignment: Couldn't open " << PMTEffs
        << ". Not doing invdividual efficiency corrections\n";
      return;
    }

    for( TString buf ; buf.ReadLine(NA62ConditionsService::GetInstance()->Get(PMTEffs)) ;  ) {
      std::stringstream ss( buf.Data() );
      ss >> hardware_id >> pmt_eff;

      //check for surprisingly low efficiencies
      if ( pmt_eff < 0.5 ) {
        std::cout << "CedarAlignment: Warning! Small PMT efficiency read: "
          << hardware_id  << " " << pmt_eff; }
      fHardwareEff.insert( make_pair( hardware_id, pmt_eff ) );
    }
    NA62ConditionsService::GetInstance()->Close(PMTEffs);

    //Map positions to efficiencies
    map<Int_t, TString>::iterator ipos;
    map<TString, double>::iterator ihardware;
    for ( ipos = fPosHardware.begin() ; ipos != fPosHardware.end() ; ++ ipos )
    {
      ihardware = fHardwareEff.find( ipos->second );

      if ( ihardware != fHardwareEff.end() )
      {
        fPosEff.insert( make_pair( ipos->first, ihardware->second ));
      }
      else
      {
        std::cout << "\nCedarAlignment: Missing efficiency for PMT"
          << ipos->second << std::endl;
      }
    }
    fLoaded = true;
  }

  // If we haven't loaded any data, assume 
  // relative eff = 1.0 for all PMTS
  double PMTProperties::GetPositionEfficiency( Int_t PosId )
  {
    if ( !fLoaded ) {
      return 1.0; }

    map<Int_t, double>::iterator ipos;
    ipos = fPosEff.find( PosId );

    if ( ipos != fPosEff.end() ) {
      return ipos->second;
    }
    else {
      std::cout << "CedarAlignment: No efficiency for " << PosId << std::endl;
      return 1.0;
    }
  }

  //--------------------------------------------------

  /// \class RootDataset
  /// \Brief
  /// A set of PMT hits corresponding to one measured (e.g. a single burst,
  /// or a simulation of a single misalignment.)
  /// \EndBrief
  ///
  /// \Detailed
  ///  The hits are stored in a 1D histogram with 900 bins so that the centre
  ///  of bin 150 is 150.0.
  ///  Calling Update(hits_histogram) replaces the current data with the 
  ///  contents of the new histogram.
  ///  Calling AddHits(hits_histogram, scale) scales the new hits and adds
  ///  them to the existing data.
  ///  ReWeightPMTs( pmt_data), applies the weights in pmt_data to the 
  ///  current dataset.
  ///  \EndDetailed

  //Contruct histogram to hold all channels
  DataSet::DataSet()
    :fHChannelHits( "hChannelHits", "Hits by position ID", 
        900, +0.5 , 900.5 )
  {
    fHChannelHits.SetDirectory(0);
  } 

  //Extract hits from histogram
  pair<double,double>
    DataSet::GetPMTHitsWithError( Int_t PosId ) const
    {
      double nhits = fHChannelHits.GetBinContent( PosId );
      return make_pair( nhits, sqrt( nhits ) );
    }

  //Add the hits from another histogram
  void DataSet::AddHits( TH1D * fHOtherHits, double scale )
  {
    fHChannelHits.Add( fHOtherHits, scale );
  }

  //Update histogram from single histogram of position IDs
  void DataSet::Update( TH1D * fHAlignmentHits )
  {
    Int_t nBins = fHAlignmentHits->GetNbinsX();
    for ( Int_t bin = 1; bin != nBins + 1 ; ++ bin ) {
      Int_t value = fHAlignmentHits->GetBinContent( bin );
      fHChannelHits.SetBinContent( bin, value );
    }
  }

  //Expose histogram ( in case we don't want to
  //use GetPMTHitsWithError method )
  TH1D& DataSet::GetRawHistogram() {
    return fHChannelHits;
  }

  void DataSet::ReWeightPMTs( const PMTProperties& PMTProp )
  {
    //Loop over all positions we have efficiencies for
    for( PMTProperties::const_iterator itprop = PMTProp.begin() ; 
        itprop != PMTProp.end() ; ++itprop )
    {
      Int_t pos_id = itprop->first;
      double eff = itprop->second;

      //Rescale value for this position
      fHChannelHits.SetBinContent( pos_id, 
          fHChannelHits.GetBinContent( pos_id ) * eff );
    }
  }
  //--------------------------------------------------

  /// \class DataParamters
  /// \Brief
  ///  3 numbers describing a MC simulation: misalignment x,y 
  //   and diaphragm aperture, r. All in micrometres to avoid decimal points.
  /// 
  /// \EndBrief

  //comparision operator so DataParameters can
  //be the key in a map
  Bool_t operator< ( const DataParameters & lhs, 
      const DataParameters & rhs )
  {
    //do we support std::tie yet?
    if ( lhs.x != rhs.x )
    { return lhs.x < rhs.x ; }

    if ( lhs.y != rhs.y )
    { return lhs.y < rhs.y ; }

    return lhs.r < rhs.r;
  }

  //--------------------------------------------------

  DataParameters Root2Params( TString s )
  {
    TObjArray * tokens = s.Tokenize("xyd_");
    DataParameters result;
    result.x = TString2Param( static_cast<TObjString*>(tokens->At(0)) );
    result.y = TString2Param( static_cast<TObjString*>(tokens->At(1)) );
    result.r = TString2Param( static_cast<TObjString*>(tokens->At(2)) );
    delete tokens;
    return result;
  }

  Int_t TString2Param( TObjString * s )
  {
    s->String().ReplaceAll( 'm', '-' );
    s->String().ReplaceAll( 'p', '+' );
    return  s->String().Atoi();
  }

  //--------------------------------------------------

  //CEDAR ALIGNMENT MULTI DATA SET
  /// \class MultiDataSet
  /// \Brief
  ///  A collection of DataSets
  /// \EndBrief
  ///
  /// \Detailed
  /// Each data set is indexed by a DataParameters class.
  /// We also provided container functions ( begin(), end() ),
  /// so we can loop over all the data sets.
  ///
  /// A MultiDataSet can be intialized with a single ROOT file,
  /// containing histograms with all the simulations at a certain
  /// diaphragm aperture, following the naming convention ...
  ///
  /// Alternatively, we can store a list of filenames, where the 
  /// filenames contain the diaphragm aperture so the most appropriate
  /// one can be picked automatically.

  //constructor
  MultiDataSet::MultiDataSet
    ( PMTProperties& PMTProperties)
    :fPMTProperties( PMTProperties )
    {}

  //Store a template filename to be read from later
  void MultiDataSet::AddTemplateFile( TString path, double weight )
  {
    //cout << "CedarAlignment: Adding template " << path << std::endl;
    fTemplateFileNames.push_back( make_pair( path, weight ) );
  }

  void MultiDataSet::Reset(){
    map<DataParameters, DataSet *>::iterator iDS = fDataSets.begin();
    while ( iDS != fDataSets.end() ) {
      delete iDS->second;
      ++iDS;
    }

    fDataSets.clear();
    fParameters.clear();
    fTemplateFileNames.clear();
  }

  //Load the data from all the filenames which have been supplied
  void MultiDataSet::LoadTemplates()
  {
    if ( fTemplateFileNames.size() < 1 )
    {
      //cout << "[CedarAlignment]     WARNING: No templates for CEDAR alignment!!" << std::endl;
      return;
    }

    //Open all the files
    std::vector<pair<TFile*, double> > fTemplateFiles;

    std::vector<pair<TString, double> >::iterator iFName;
    for ( iFName = fTemplateFileNames.begin() ;
        iFName != fTemplateFileNames.end(); ++ iFName )
    {
      TString filename = iFName->first;

      //Check for eos
      if ( filename.Contains( "/eos/" ) ){
        filename = "root://eosna62.cern.ch//" + filename;
      }

      //Check for  castor
      if ( filename.Contains( "/castor/" ) ){
        filename = "root://castorpublic.cern.ch//" + filename;
      }

      TFile * tfile = TFile::Open( filename.Data() );

      if ( (!tfile || !tfile->IsOpen() || tfile->IsZombie() ) )
      {
        std::cout << "CedarAlignment: Problem opening " 
          << iFName->first  << " ... skipping" << std::endl;
        std::cout << "CedarAlignment: [ Tried to open it as " 
          << filename <<  " ]" << std::endl;
        continue;
      }

      double weight = iFName->second;
      fTemplateFiles.push_back( make_pair( tfile, weight) );
    }

    //No point continuing if we couldn't open any files
    if ( fTemplateFiles.size() < 1 )
    {
      std::cout << "CedarAlignment: Couldn't load any templates!"
        << std::endl;
      return;
    }

    //Read keys from first file
    std::vector<pair<TFile*, double> >::iterator iFile, iFend, iFbegin;

    iFile = fTemplateFiles.begin();
    TIter nextkey( iFile->first->GetListOfKeys() );
    TKey * key = static_cast<TKey*>( nextkey() );

    iFbegin = fTemplateFiles.begin();
    iFend = fTemplateFiles.end();

    Int_t counter = 0;

    //cout << " CedarAlignment: Loading MCTemplates" << flush;
    //Loop over keys
    for( ; key ; key = static_cast<TKey*>( nextkey() ) )
    {
      TString hname = key->ReadObj()->GetName();
      DataParameters dp = Root2Params( hname );

      DataSet * hds = new DataSet();

      if ( ++counter  % 10 == 0 ) { 
        //cout << "." << flush;
      }

      //Loop over files
      for ( iFile = iFbegin ; iFile != iFend ; ++iFile )
      {
        TH1D * hHits(0);
        TFile * tf = iFile->first;
        double weight = iFile->second;
        tf->GetObject( hname.Data() , hHits);
        if ( !hHits )
        {
          std::cout << "\nCedarAlignment: Missing " << hname  << " in " <<
            fTemplateFileNames[ distance( iFbegin, iFile)].first
            << std::endl;
          continue;
        }
        //Load template histograms into correct axes
        TH1D fHCorrectedHits( "hCorrectedHits", "Hits by position ID", 
            900, +0.5 , 900.5 );
        fHCorrectedHits.SetDirectory(0);

        Int_t nBins = fHCorrectedHits.GetNbinsX();
        for ( Int_t i = 1 ; i != nBins +1 ; ++i )
        {
          fHCorrectedHits.SetBinContent( i, hHits->GetBinContent(i) );
        }
        hds->AddHits( &fHCorrectedHits, weight);
      }

      hds->ReWeightPMTs( fPMTProperties );
      fDataSets.insert( make_pair( dp, hds ) );
      fParameters.push_back( dp );
    }

    //Close files
    for ( iFile = iFbegin ; iFile != iFend ; ++iFile ) {
      if ( iFile->first ) {
        delete iFile->first;
      }
    }
  }

  //--------------------------------------------------

  //Helper functions used by GroupAligner
  Bool_t chi2_result_comp::operator() ( const pair<DataParameters, double>& lhs,
      const pair<DataParameters, double>& rhs )
  {
    return lhs.second > rhs.second;
  }

  pair<double, double> 
    GroupTotalError( std::vector<Int_t>& group, const DataSet * ds ) 
    {
      double total = 0 ;
      double squared_error = 0;

      //Loop over all PMTs in group
      for ( std::vector<Int_t>::iterator ipmt =  group.begin();
          ipmt !=group.end(); ++ipmt ) { 
        Int_t pmt_id = *ipmt;
        pair<double, double> info = ds->GetPMTHitsWithError( pmt_id );
        total += info.first;

        //Add errors in quadrature
        squared_error += pow( info.second, 2 );
      }
      return make_pair( total, sqrt( squared_error ) );
    }

  //--------------------------------------------------

  /// \class GroupAligner
  /// \Brief
  /// Determine CEDAR alignment by looking at chi2 fits between measured
  //  data and MC templates
  /// \EndBrief
  ///
  /// \Detailed
  //  \EndDetailed

  GroupAligner::GroupAligner( TString name, TString title, int nBinsX, Double_t xMin, Double_t xMax, 
      int nBinsY, Double_t /*yMin*/, Double_t /*yMax*/ )
    :fDebug( kFALSE ), 
    fHVis( name, title,
        nBinsX, xMin , xMax, nBinsY, xMin , xMax ),
        fTemplateDataPtr(nullptr),
        fChi2X(0.),
        fChi2Y(0.)
  {
    fHVis.SetDirectory(0);
    fHVis.GetXaxis()->SetTitle( "X (um)" );
    fHVis.GetYaxis()->SetTitle( "Y (um)" );
    fHVis.SetStats(0);
  }

  void GroupAligner::Init()
  {}

  void GroupAligner::ComputeAlignment( DataSet& ds )
  {
    fHVis.Reset();

    const MultiDataSet& fTemplateData = *fTemplateDataPtr;

    typedef pair<DataParameters, double> chi2_result;

    //store chi2 results in an ordered queue
    priority_queue<chi2_result,
      std::vector<chi2_result>, chi2_result_comp> chi2_queue;

    //do we have any templates
    if ( fTemplateData.size() < 1 )
    {
      fHVis.Reset("M");
      Int_t nBins = fHVis.GetNbinsX();

      //set a diagonal stripe to indicate no templates
      for ( Int_t bin = 1; bin != nBins + 1 ; ++ bin ) {
        fHVis.SetBinContent( bin, bin, 1 );
        fHVis.SetBinContent( bin + 1, bin, 1 );
        fHVis.SetBinContent( bin + 1, bin + 1, 1 );
      }

      return;
    }

    //Loop over all templates
    for ( MultiDataSet::const_iterator dsit = fTemplateData.begin() ;
        dsit != fTemplateData.end() ; ++dsit )
    {
      DataParameters params = dsit->first;
      DataSet * model_data = dsit->second;

      double chi2 = ComputeChi2( &ds, model_data );
      chi2_queue.push( make_pair( params, chi2 ) );

      //store in plot
      Int_t xbin = fHVis.GetXaxis()->FindBin( params.x);
      Int_t ybin = fHVis.GetYaxis()->FindBin( params.y);
      fHVis.SetBinContent( xbin, ybin, log( chi2 ) );
    }

    //Take best results from top of queue
    fChi2X = chi2_queue.top().first.x;
    fChi2Y = chi2_queue.top().first.y;
  }

  double GroupAligner::ComputeChi2( const DataSet* data,
      const DataSet * model )
  {
    double chi2 = 0;

    //vectors to hold the photon counts and errors for each group
    std::vector<pair<double,double> > data_groups;
    std::vector<pair<double,double> > model_groups;

    //Totals for normalisation
    double data_total = 0;
    double model_total = 0;

    //Extract group photon counts and errors
    for ( std::vector<vector<Int_t> >::iterator
        igroup = fChi2Groups.begin();
        igroup != fChi2Groups.end() ; ++ igroup )
    {
      data_groups.push_back(  GroupTotalError( *igroup, data ) );
      model_groups.push_back(  GroupTotalError( *igroup, model ) );

      data_total += data_groups.back().first;
      model_total += model_groups.back().first;
    }

    //check we have some hits
    if ( model_total <  2 ) {
      return std::numeric_limits<Double_t>::max();}

    std::vector<pair<double,double> >::iterator itdata = data_groups.begin();
    std::vector<pair<double,double> >::iterator itmodel = model_groups.begin();

    //Loop over groups and extract chi2
    while ( itdata != data_groups.end() )
    {
      double& dtval = itdata->first;
      double& dterr = itdata->second;

      double& mdval = itmodel->first;
      double& mderr = itmodel->second;

      double numerator = pow( model_total * dtval - data_total * mdval, 2 ) ;
      double denominator = ( dtval + mdval  );

      if ( denominator > 0 )
      {
        chi2 += ( numerator / denominator / model_total / data_total) ;
      }

      if ( fDebug )
      {
        cerr 
          << setw( 12) << dtval << setw( 12) << dterr
          << setw( 12) << mdval << setw( 12) << mderr
          << setw( 12) << numerator << setw( 12) << denominator 
          << setw( 12) << model_total << setw( 12) << data_total 
          << setw( 12) << chi2 << std::endl;
      }

      if ( chi2 != chi2 )
      {
        return std::numeric_limits<Double_t>::max();
      }

      ++itdata;
      ++itmodel;
    }

    return chi2;
  }

  void GroupAligner::SetGroups( std::vector<vector<Int_t> > &Chi2Groups )
  {
    fChi2Groups = Chi2Groups;
  }

  void GroupAligner::SetTemplateData( MultiDataSet * mds )
  {
    fTemplateDataPtr = mds ;
  }

  //--------------------------------------------------

  /// \class RatioAligner
  /// \Brief
  /// Produce assymetries for alignment
  /// \EndBrief
  ///
  /// \Detailed
  //  \EndDetailed
  RatioAligner::RatioAligner() :
    fSectorTotals(8, 0 ),
    fSectorErrors(8, 0 ),
    f2012Mode(false)
  {
  }


  void RatioAligner::SetSectorGroups
    ( std::vector<vector<Int_t> > &SectorGroups )
    {
      fSectorGroups = SectorGroups;
    }

  void RatioAligner::ComputeAlignment( DataSet& ds ){
    if (fSectorGroups.size() < 8 )
    { std::cout << "CEDAR: RatioAligner is missing some sectors!\n" ; }

    for ( Int_t i = 0 ; i != 8 ; ++ i )
    {
      pair<double,double> total_error 
        = GroupTotalError( fSectorGroups[i], &ds );

      fSectorTotals[i] = total_error.first;
      fSectorErrors[i] = total_error.second;
    }

    //-----------------------------------------------------------------------------
    // Sectors are numbered 1-8, ordered clockwise looking in the K beam direction.
    // The Saleve (Jura) sectors are numbered 1-4 (5-8).
    // Sector 1 is top Saleve (=top right looking along the K beam).
    // The X axis is directed to the Jura (=left looking along the K beam).
    // Sector locations:
    // 1,2: x<0, y>0      3,4: x<0, y<0
    // 5,6: x>0, y<0      7,8: x>0, y>0
    // Reference frame of the sector: the sector centre of symmetry axis is Y.
    //-----------------------------------------------------------------------------

    double U, D, S, J;

    //Calculate U, D, L, R ( -1's for zero indexed vector )
    if ( f2012Mode )
    {
      U = fSectorTotals[7-1] + fSectorTotals[1-1];
      D = fSectorTotals[4-1] + fSectorTotals[6-1];
      J = fSectorTotals[6-1] + fSectorTotals[7-1];
      S = fSectorTotals[4-1] + fSectorTotals[1-1];
    }
    else
    {
      U = fSectorTotals[8-1] + fSectorTotals[1-1];
      D = fSectorTotals[4-1] + fSectorTotals[5-1];
      J = fSectorTotals[6-1] + fSectorTotals[7-1];
      S = fSectorTotals[2-1] + fSectorTotals[3-1];
    }

    fAsymUpDown   = ( U - D ) / ( U + D );
    fAsymSalvJura = ( S - J ) / ( S + J );
  }

  //--------------------------------------------------

  //SMOOTHING

  /// \class Mat2D
  /// \Brief
  //  A wrapper for a 2D array of Float_ts
  /// \EndBrief
  ///
  /// \Detailed
  //  \EndDetailed
  Mat2D::Mat2D( Int_t dimx, Int_t dimy )
    :fDimX(dimx), fDimY(dimy), fData( new Float_t * [fDimX] )
  {
    for ( Int_t i = 0 ; i != fDimX ; ++ i )
    {
      fData[i] = new Float_t[fDimY];
    }
  }

  Mat2D::Mat2D( const Mat2D& other)
    :fDimX( other.fDimX), fDimY( other.fDimY), fData( new Float_t * [fDimX] )
  {
    for ( Int_t i = 0 ; i != fDimX ; ++ i )
    {
      fData[i] = new Float_t[fDimY];

      for( Int_t j = 0 ; j != fDimY ; ++ j )
      {
        fData[i][j] = other( i, j );
      }
    }
  }

  Mat2D& Mat2D::operator=( Mat2D other)
  {
    swap( *this, other );
    return *this;
  }

  void swap( Mat2D& first, Mat2D& second )
  {
    using std::swap;
    swap( first.fDimX, second.fDimX );
    swap( first.fDimY, second.fDimY );
    swap( first.fData, second.fData );
  }

  void Mat2D::FillFromHist( const TH2F&  h )
  {
    for ( int ix = 0; ix != fDimX ; ++ix ) {
      for ( int iy = 0; iy != fDimY ; ++iy ) {
        (*this)( ix, iy ) = h.GetBinContent( ix + 1 , iy+ 1 );
      }
    }
  }

  void Mat2D::WriteToHist( TH2F&  h ) const
  {
    for ( int ix = 0; ix != fDimX ; ++ix ) {
      for ( int iy = 0; iy != fDimY ; ++iy ) {
        h.SetBinContent( ix + 1, iy + 1 , (*this)(ix, iy) );
      }
    }
  }

  Float_t& Mat2D::operator()( Int_t x, Int_t y ){ return fData[x][y]; }
  Float_t Mat2D::operator()( Int_t x, Int_t y ) const{ return fData[x][y]; }

  Float_t ** Mat2D::Data(){ return fData;}

  Mat2D::~Mat2D()
  {
    for ( Int_t i = 0 ; i != fDimX ; ++ i )
    {
      delete[] fData[i];
    }
    delete[] fData;
  }

  /// \fn fill_holes
  /// \Brief
  /// Fill holes in chi2 distribution with nearby values
  /// \EndBrief
  void fill_holes( Mat2D & m )
  {
    int nBinsX = m.GetDimX();
    int nBinsY = m.GetDimY();

    for( int ix = 0 ; ix != nBinsX ; ++ix )
    {
      for( int iy = 0 ; iy != nBinsY ; ++iy )
      {
        if ( m( ix, iy ) < 1.0 )
        {
          Float_t total = 0;
          int count = 0;

          for ( int ii = ix-1 ; ii != ix + 2; ++ii )
          {
            for ( int jj = iy-1 ; jj != iy + 2; ++jj )
            {
              if ( (ii!=ix) && (jj!=ix )
                  && (ii > -1) && ( ii < nBinsX ) 
                  && (jj > -1) && ( jj < nBinsY) )
              {
                if ( m( ii, jj ) > 1 )
                {
                  total += m(ii, jj);
                  ++count;
                }
              }
            }
          }
          m(ix,iy)  = total / Float_t(count);
        }
      }
    }
  }

  /// \fn negate
  /// \Brief
  /// Send m(a,b) -> -m(a,b)
  /// \EndBrief
  void negate_mat( Mat2D &m )
  {
    int nBinsX = m.GetDimX();
    int nBinsY = m.GetDimY();

    for( int i = 0 ; i != nBinsX ; ++i )
    {
      for( int j = 0 ; j != nBinsY ; ++j )
      {
        m(i,j)= - m(i,j);
      }
    }
  }

  //--------------------------------------------------

  /// \class PeakFinder
  /// \Brief
  //  Takes a histogram of chi2 vs position and finds the minima
  /// \EndBrief
  ///
  /// \Detailed
  //  \EndDetailed
  PeakFinder::PeakFinder( TH2F &h )
    :fChi2Hist( h ), fSmoothedHist(h),
    fChi2Array( h.GetNbinsX(), h.GetNbinsY() ),
    fSmoothedArray( h.GetNbinsX(), h.GetNbinsY() )
  {
    fSmoothedHist.SetTitle( TString( fChi2Hist.GetName() ) + " (smoothed}" );
    fSmoothedHist.SetDirectory(0);

    fPeakSigma = 6;
    fPeakThreshold = 80;
    fRemoveBackground = kTRUE;
    fDeconIterations = 3;
    fDoMarkov = kFALSE;
    fMarkovWindow = 3;
  }

  void PeakFinder::FindPeaks()
  {
    fPeaks.clear();

    fChi2Array.FillFromHist( fChi2Hist );
    fill_holes( fChi2Array );
    fChi2Array.WriteToHist( fChi2Hist );
    negate_mat( fChi2Array );
    TSpectrum2 s;

    Int_t nPeaks = s.SearchHighRes( (Double_t **)fChi2Array.Data(), (Double_t **)fSmoothedArray.Data(), 
        fChi2Array.GetDimX(), fChi2Array.GetDimY(),
        fPeakSigma, fPeakThreshold, fRemoveBackground,
        fDeconIterations, fDoMarkov, fMarkovWindow );

    negate_mat( fSmoothedArray );
    fSmoothedArray.WriteToHist( fSmoothedHist );

    Double_t * x = s.GetPositionX();
    Double_t * y = s.GetPositionY();

    TAxis * xAxis = fChi2Hist.GetXaxis();
    TAxis * yAxis = fChi2Hist.GetYaxis();

    for( Int_t iPeak = 0 ; iPeak != nPeaks ; ++iPeak )
    {
      //peak in matrix coordinates
      Float_t xm = x[iPeak];
      Float_t ym = y[iPeak];

      //Convert to alignment coordinates
      Float_t wholex;
      Float_t fractx = std::modf(xm,  &wholex);
      Float_t alignx = xAxis->GetBinCenter( int(wholex) ) 
        + xAxis->GetBinWidth( int(wholex) ) * ( fractx );

      Float_t wholey;
      Float_t fracty = std::modf(ym, &wholey);
      Float_t aligny = yAxis->GetBinCenter( int(wholey) ) 
        + yAxis->GetBinWidth( int(wholey) ) * ( fracty );

      Float_t lnchi2 = fSmoothedArray( xm, ym );

      Peak result = {  alignx, aligny, lnchi2  };
      fPeaks.push_back( result );
    }

    std::sort( fPeaks.begin(), fPeaks.end() );
  }

  PeakFinder::Peak PeakFinder::get_best_peak() const
  { 
    if ( fPeaks.size() > 0 )
    {
      return fPeaks[0];
    }
    else
    {
      Peak result  = { 999, 999, 999 };
      return result;
    }
  }

  bool operator< (const PeakFinder::Peak& lhs, const PeakFinder::Peak& rhs)
  {
    return ( lhs.lnchi2 < rhs.lnchi2 );
  }

  //--------------------------------------------------

  /// \class HitsVis
  /// \Brief
  //  Creates histograms to visualize hit positions
  /// \EndBrief
  ///
  /// \Detailed
  //  \EndDetailed
  HitVis::HitVis( TString PMT3DPositionsFile, std::vector<vector<Int_t> >& SectorGroups)
    :fHOctantVis( "hSectorsVis", "Hits in sectors", 12, -1.2, 1.2, 12, -1.2, 1.2 ),
    fHPMTVis( "hPMTVis", "Hits in PMTs", 200, -450, 450, 200, -450, 450 ),
    fSectorGroups( SectorGroups )
  {
    fHOctantVis.SetDirectory( 0 );
    fHOctantVis.SetStats( 0 );
    fHPMTVis.SetDirectory( 0 );
    fHPMTVis.SetStats( 0 );

    //Load position map
    NA62ConditionsService::GetInstance()->Open(PMT3DPositionsFile);
    for( TString buffer ; buffer.ReadLine(NA62ConditionsService::GetInstance()->Get(PMT3DPositionsFile));  ) {
      //load position of a PMT
      Int_t pos_id;
      double x, y, z;
      std::stringstream ss( buffer.Data() );
      ss >> pos_id >> x >> y >> z ;
      TVector3 pmt_3d_pos( x, y, z );

      //Project in 2d plane
      TVector2 pmt_2d_proj = ProjectPMT( pos_id, pmt_3d_pos );
      fPositionMap.insert( make_pair( pos_id, pmt_2d_proj ) );
    }
    NA62ConditionsService::GetInstance()->Close(PMT3DPositionsFile);
  }

  void HitVis::Update( DataSet & ds)
  {
    fHPMTVis.Reset();
    for ( map<Int_t, TVector2>::iterator itpmt = fPositionMap.begin() ;
        itpmt != fPositionMap.end() ; ++itpmt )
    {
      Int_t pos_id = itpmt->first;
      const TVector2& pmt_pos = itpmt->second;
      double hits = ds.GetPMTHitsWithError( pos_id).first;
      fHPMTVis.Fill( pmt_pos.X(), pmt_pos.Y(), hits );
    }

    fHOctantVis.Reset();
    for ( Int_t isec = 0; isec != 8 ; ++isec )
    {
      Double_t total = GroupTotalError( fSectorGroups[isec], &ds ).first;
      Double_t xSec      = sin(0.25*TMath::Pi()*(isec+0.5));
      Double_t ySec      = cos(0.25*TMath::Pi()*(isec+0.5));
      fHOctantVis.Fill( xSec, ySec, total );
    }
  }

  TVector2 ProjectPMT( Int_t pos_id, TVector3 pmt_3d_pos )
  {
    TRotation r, r1;
    TVector2 result;

    //Shift origin in z
    pmt_3d_pos.SetZ( pmt_3d_pos.Z() - 69979 );

    //Loop over all sectors to find our PMT
    for ( Int_t sector = 0 ; sector < 8 ; ++sector )
    {
      if ( pos_id > 114 && pos_id < 888 )
        if ( pos_id > ( sector+1 )*100 && pos_id < (sector +2)*100 )
        {
          //We have found the correct sector - setup rotations

          //Rotate all sectors into the same position
          r.RotateZ( -TMath::Pi() / 8 - sector*TMath::Pi() / 4 );
          pmt_3d_pos.Transform( r );

          //Rearrange coordinates
          TVector3 hit_position_rot
            ( pmt_3d_pos.X(), pmt_3d_pos.Z() + 352, pmt_3d_pos.Y() );

          //Repeat rotation
          r1.RotateZ( -TMath::Pi()/ 8 - sector*TMath::Pi()/4 );
          hit_position_rot.Transform( r1 );

          result.Set( hit_position_rot.X(), hit_position_rot.Y() );
        }
    }
    return result;
  }


  //--------------------------------------------------

  /// \class CedarAlign
  /// \Brief
  //  Class to hold all alignment methods
  /// \EndBrief
  ///
  /// \Detailed
  //  \EndDetailed
  CedarAlign::CedarAlign()
    :fGroupAlignerOctants( "hSelectedOctants", "Selected Octants #chi^{2}", 40, -1025, 975, 40, -1025, 975),
    fGroupAlignerSixths( "hSelectedSixths", "Selected Sixths #chi^{2}", 40, -1025, 975, 40, -1025, 975),
    fGroupAlignerCoarseSixths( "hRawSixths", "Raw Sixths #chi^{2}", 40, -4100, 3900, 40, -4100, 3900),
    fPeakFinderOctants( *static_cast<TH2F*>( fGroupAlignerOctants.GetHVis() ) ),
    fPeakFinderSixths( *static_cast<TH2F*>( fGroupAlignerSixths.GetHVis() ) ),
    fRatioAligner(),
    fHAllHits( "hAllChannelHits", "Hits by position ID", 900, +0.5 , 900.5 ),
    fHSelectedHits( "hSelectedChannelHits", "Hits by position ID", 900, +0.5 , 900.5 ),
    fPMTProperties(),
    fAllHitTemplates( fPMTProperties ),
    fSelectedHitTemplates( fPMTProperties ),
    fHitVis("Cedar-PMT3DPositions.dat", fSectorGroups ),
    fPMTPosFile(), fPMTEffFile(),
    fMotorPosX(0), fMotorPosY(0),
    fHAsymUD( "hAsymUD", "Asymetry Up Down", 100, -0.2, 0.2 ),
    fHAsymSJ( "hAsymSJ", "Asymetry Saleve Jura", 100, -0.2, 0.2 ),
    fNEvents(0)
    {
      fGSugXYTrend.SetPoint( fGSugXYTrend.GetN(), 0, 0 );
      fGSugXYTrend.SetTitle( "Estimated true alignment history" );

      fHSelectedHits.SetDirectory(0);
      fHAllHits.SetDirectory(0);
      fHAsymUD.SetDirectory(0);
      fHAsymSJ.SetDirectory(0);
      fHAsymUD.GetXaxis()->SetTitle("(Up-Down)/(Up+Down)");
      fHAsymUD.GetYaxis()->SetTitle("Bursts");
      fHAsymSJ.GetXaxis()->SetTitle("(Saleve-Jura)/(Saleve+Jura)");
      fHAsymSJ.GetYaxis()->SetTitle("Bursts");


      //A list of the position IDs for one sector
      TString sector_positions_string( 
          "15 22 23 24 25 26 27 32 33 34 35 36 "
          "37 38 41 42 43 44 45 46 47 48 51 52 "
          "53 54 55 56 57 58 59 61 62 63 64 65 "
          "66 67 68 72 73 74 75 76 77 78 82 87\n"
          );

      //Duplicate across all sectors
      std::vector<vector<Int_t> > sector_positions(1);
      TString_to_group( sector_positions_string, sector_positions[0] );
      duplicate_sectors( sector_positions, fSectorGroups );

      //Grouping of PMTs in sixths
      TString sixth_positions_string( 
          "32 33 41 42 43 51 22 23\n"
          "72 73 82 52 53 61 62 63\n"
          "34 35 36 44 45 15 24 25\n"
          "64 65 74 75 76 54 55 56\n"
          "27 37 38 46 47 48 26 59\n"
          "66 67 68 77 78 87 57 58\n"
          );

      //Duplicate across all sectors
      std::stringstream sixths_stream( sixth_positions_string.Data() );
      std::vector<vector<Int_t> > sixth_positions;
      load_groups( sixths_stream, sixth_positions);
      duplicate_sectors( sixth_positions, fSixthGroups );

      fGroupAlignerOctants.SetGroups( fSectorGroups );
      fGroupAlignerSixths.SetGroups( fSixthGroups );
      fGroupAlignerCoarseSixths.SetGroups( fSixthGroups ) ;
      fRatioAligner.SetSectorGroups( fSectorGroups );
    }

  void CedarAlign::SetAllTemplateFile( TString allTemplate ) {
    fAllHitTemplates.Reset();
    fAllHitTemplates.AddTemplateFile( allTemplate, 1.0 );
  }

  void CedarAlign::SetSelectedTemplateFile( TString selectedTemplate ) {
    fSelectedHitTemplates.Reset();
    fSelectedHitTemplates.AddTemplateFile( selectedTemplate, 1.0 );
  }

  void CedarAlign::LoadTemplates(){
    fAllHitTemplates.LoadTemplates();
    fSelectedHitTemplates.LoadTemplates();

    fGroupAlignerOctants.SetTemplateData( &fSelectedHitTemplates );
    fGroupAlignerSixths.SetTemplateData( &fSelectedHitTemplates );
    fGroupAlignerCoarseSixths.SetTemplateData( &fAllHitTemplates );
  }

  void CedarAlign::Init()
  {
    if ( fPMTPosFile.Length() > 0 && fPMTEffFile.Length() > 0 )
    {
      fPMTProperties.LoadMaps( fPMTPosFile, fPMTEffFile );
    }

    LoadTemplates();
  }

  void CedarAlign::SetMotorPosXY( Float_t PosX, Float_t PosY)
  {
    fMotorPosX = PosX;
    fMotorPosY = PosY;
  }

  void CedarAlign::Fill( TRecoCedarEvent& fCedarEvent )
  {
    Int_t nCand = fCedarEvent.GetNCandidates();
    for ( Int_t iCand = 0 ; iCand != nCand ; ++iCand ) {

      TRecoCedarCandidate * fCandidate =
        static_cast<TRecoCedarCandidate*>(fCedarEvent.GetCandidate(iCand) );

      Int_t nHit = fCandidate->GetNHits() ;

      bool Selected = fCandidate->GetNSectors() > 4;

      for (Int_t iHit=0; iHit != nHit; iHit++) {
        TRecoCedarHit * Hit = static_cast<TRecoCedarHit*>(fCandidate->GetHit(iHit));
        Int_t PositionID = Hit->GetSectorID()*100 + Hit->GetRowID()*10 + Hit->GetConeID();

        fHAllHits.Fill( PositionID );

        if ( Selected )
        {
          fHSelectedHits.Fill( PositionID );
        }
      }
    }

    ++fNEvents;
  }

  void CedarAlign::ResetHits()
  {
    //std::cerr << "RESETALIGNMENT" << std::endl;
    fHAllHits.Reset();
    fHSelectedHits.Reset();
    fNEvents = 0;
    //Save results
    fHAsymUD.Fill( fRatioAligner.GetAsymUpDown() );
    fHAsymSJ.Fill( fRatioAligner.GetAsymSalvJura() );

    fGSugXYTrend.SetPoint( fGSugXYTrend.GetN(), 
        fMotorPosX - fPeakFinderOctants.get_best_peak().x / 1000.0,
        fMotorPosY - fPeakFinderOctants.get_best_peak().y / 1000.0 );

    fGSugXYTrend.GetXaxis()->SetLimits( -1.5, 1.5 );
    fGSugXYTrend.GetHistogram()->SetMaximum(  1.5 );
    fGSugXYTrend.GetHistogram()->SetMinimum( -1.5 );
  }

  void CedarAlign::Update( TH1D & hAllHits, TH1D &hSelectedHits )
  {
    fHAllHits = hAllHits;
    fDsAllHits.Update( &fHAllHits );

    fHSelectedHits = hSelectedHits;
    fDsSelectedHits.Update( &fHAllHits );
  }

  void CedarAlign::Update()
  {
    fDsAllHits.Update( &fHAllHits );
    fDsSelectedHits.Update( &fHAllHits );
  }

  void CedarAlign::ComputeAlignment()
  {
    Update();
    //std::cerr << "COMPUTEALIGNMENT" << std::endl;

    //Do computation
    fGroupAlignerOctants.ComputeAlignment( fDsSelectedHits );
    fPeakFinderOctants.FindPeaks();

    fGroupAlignerSixths.ComputeAlignment( fDsSelectedHits );
    fPeakFinderSixths.FindPeaks();

    fGroupAlignerCoarseSixths.ComputeAlignment( fDsAllHits );
    fRatioAligner.ComputeAlignment( fDsAllHits );
    fHitVis.Update( fDsSelectedHits );

  }

  void CedarAlign::PrintStats( TPaveText * Pave )
  {
    Pave->GetListOfLines()->Delete();

    Pave->SetTextFont( 102);

    Pave->AddText( Form("#splitline{Sixths:}{[ %+5.3f, %+5.3f]}",
          fGroupAlignerSixths.GetPositionX()/1000.0,
          fGroupAlignerSixths.GetPositionY()/1000.0 ) ) ;

    Pave->AddText( Form("#splitline{Sixths Fit:}{[ %+5.3f, %+5.3f]}",
          fPeakFinderSixths.get_best_peak().x/1000.0,
          fPeakFinderSixths.get_best_peak().y/1000.0 ) ) ;

    Pave->AddText( Form("#splitline{Octants:}{[ %+5.3f, %+5.3f]}",
          fGroupAlignerOctants.GetPositionX()/1000.0,
          fGroupAlignerOctants.GetPositionY()/1000.0 ) ) ;

    Pave->AddText( Form("#splitline{Octants Fit:}{[ %+5.3f, %+5.3f]}",
          fPeakFinderOctants.get_best_peak().x/1000.0,
          fPeakFinderOctants.get_best_peak().y/1000.0 ) ) ;

    Pave->AddText( Form("#splitline{Raw Sixths:}{[ %+5.3f, %+5.3f]}",
          fGroupAlignerCoarseSixths.GetPositionX()/1000.0,
          fGroupAlignerCoarseSixths.GetPositionY()/1000.0 ) ) ;
  }

  void CedarAlign::PrintAsym( TPaveText * Pave )
  {
    Pave->GetListOfLines()->Delete();
    Pave->SetTextFont( 102);

    Pave->AddText( "ASYM" );
    Pave->AddText( Form("#splitline{UD: %+5.3f}{SJ: %+5.3f}",
          fRatioAligner.GetAsymUpDown(),
          fRatioAligner.GetAsymSalvJura()) ) ;
  }
}
