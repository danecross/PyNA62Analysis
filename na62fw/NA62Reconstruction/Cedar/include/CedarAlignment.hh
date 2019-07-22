#ifndef CedarAlignment_H
#define CedarAlignment_H 1

#include "TString.h"
#include "TObjString.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TVector2.h"
#include "TVector3.h"
#include "TPaveText.h"
#include "TGraph.h"

#include <map>
#include <sstream>

#include "TRecoCedarEvent.hh"

using namespace std;

namespace CedarAlignment
{
  //CEDARPMTPROPERTIES
  //Database of PMT efficiencies
  class PMTProperties
  {
    public:
      //Constructors
      PMTProperties(TString PMTPositions, TString PMTEffs);
      PMTProperties();

      //Load data
      void LoadMaps(  TString PMTPositions, TString PMTEffs);

      //Retrieve data
      Double_t GetPositionEfficiency( Int_t PosId );

      //Container Interface
      typedef map<Int_t, Double_t>::const_iterator const_iterator;
      typedef map<Int_t, Double_t>::size_type size_type;
      const_iterator begin() const    { return fPosEff.begin() ;}
      const_iterator end() const      { return fPosEff.end()   ;}
      size_type size() const          { return fPosEff.size()  ;}

    private:
      Bool_t fLoaded;
      map<Int_t, Double_t> fPosEff;
      map<TString, Double_t> fHardwareEff;
      map<Int_t, TString> fPosHardware;
  };

  //--------------------------------------------------

  //CEDAR ALIGNMENT ROOT DATASET
  //A class to store PMT hits in a histogram
  class DataSet {
    public:
      DataSet();

      //Replace current data with supplied data
      void Update(  TH1D  * fHAlignmentHits );

      //Add supplied data to current data
      void AddHits( TH1D  * fHOtherHits, Double_t scale );

      //Retrieve data
      pair<Double_t, Double_t>
        GetPMTHitsWithError( Int_t PosId ) const;

      //Apply weightings to current data
      void ReWeightPMTs( const PMTProperties& PMTProp );

      //Access to raw histogram ( ideally avoid )
      TH1D& GetRawHistogram();

    private:
      TH1D fHChannelHits;
  };

  //--------------------------------------------------

  //MULTI DATA SET

  //Uniquely identify a data set
  struct DataParameters {
    //Misalignment
    Int_t x; //micrometres
    Int_t y; //micrometres

    //Diaphragm opening
    Int_t r; //micrometres
  };

  Bool_t operator< ( const DataParameters & lhs, 
      const DataParameters & rhs );

  //--------------------------------------------------

  //Handy functions for converting the strings in histogram
  //titles into alignment data parameters
  DataParameters Root2Params( TString s );
  Int_t TString2Param( TObjString * s );

  //--------------------------------------------------

  //CEDAR ALIGNMENT MULTI DATA SET
  //A class to hold lots of DataSets and provide iterators so we can
  //loop over a set of MC templates
  class MultiDataSet
  {
    public:

      explicit MultiDataSet( PMTProperties& PMTProperties );
      void Reset();

      void AddTemplateFile( TString path, Double_t weight );
      void LoadTemplates();

      //Container Int_terface
      typedef map<DataParameters, DataSet * >::iterator iterator;
      typedef map< DataParameters, DataSet *>::const_iterator 
        const_iterator;

      typedef map< DataParameters, DataSet *>::size_type 

        size_type;
      iterator begin()                { return fDataSets.begin() ;}
      iterator end()                  { return fDataSets.end()   ;}

      const_iterator begin() const    { return fDataSets.begin() ;}
      const_iterator end() const      { return fDataSets.end()   ;}
      size_type size() const          { return fDataSets.size()  ;}

      ~MultiDataSet(){};

    private:
      PMTProperties& fPMTProperties;
      map<DataParameters, DataSet *> fDataSets;
      std::vector<DataParameters> fParameters;
      std::vector<pair<TString, Double_t> > fTemplateFileNames;
  };

  //--------------------------------------------------

  //Helper function for adding up pmt groups
  struct chi2_result_comp
  {
    Bool_t operator() ( const pair<DataParameters, Double_t>& lhs,
        const pair<DataParameters, Double_t>& rhs );
  };

  pair<Double_t, Double_t> 
    GroupTotalError( std::vector<Int_t>& group, 
        const DataSet * ds ) ;

  //--------------------------------------------------

  class GroupAligner {
    public:
      GroupAligner( TString name, TString title,
          int nBinsX, Double_t xMin, Double_t xMax, 
          int nBinsY, Double_t yMin, Double_t yMax );

      void SetTemplateData( MultiDataSet * mds );
      void SetGroups( std::vector<vector<Int_t> > &Chi2Groups );
      void Init();

      void ComputeAlignment( DataSet& ds );
      Double_t GetPositionX()   { return fChi2X; }
      Double_t GetPositionY()   { return fChi2Y; }
      TH1 * GetHVis()           { return &fHVis;  }

      Double_t ComputeChi2( const DataSet* data,
          const DataSet * model );
    private:
      Bool_t fDebug;
      TH2F fHVis;
      MultiDataSet * fTemplateDataPtr;
      std::vector<vector<Int_t> > fChi2Groups;
      Double_t fChi2X;
      Double_t fChi2Y;
  };

  //--------------------------------------------------


  class RatioAligner {
    public:
      RatioAligner();
      void Init(){}
      void ComputeAlignment( DataSet& ds );
      void SetSectorGroups
        ( std::vector<vector<Int_t> > &SectorGroups );

      Double_t GetAsymUpDown()   { return fAsymUpDown; }
      Double_t GetAsymSalvJura() { return fAsymSalvJura; }

    private:
      std::vector<vector<Int_t> > fSectorGroups;
      std::vector<Double_t> fSectorTotals;
      std::vector<Double_t> fSectorErrors;

      Double_t fAsymUpDown;
      Double_t fAsymSalvJura;
      Bool_t f2012Mode;
  };

  //--------------------------------------------------


  //CONVENIENCE FUNCTIONS

  template<class container>
    void load_groups( istream& is, container& c ) {
      TString buffer;
      while ( buffer.ReadLine( is ) ) {
        typename container::value_type group;
        std::stringstream ss( buffer.Data() );
        Int_t id;
        while ( ss >> id )
        { group.push_back( id ); }
        c.push_back( group );
      }
    }

  //convert a TString to a PMT group
  template <class container>
    void TString_to_group( TString s, container& c ) {
      //Convert to vector of Int_ts
      std::stringstream ss( s.Data() );
      Int_t id;
      while ( ss >> id ) { 
        c.push_back( id );
      }
    }

  //duplicate a set of position IDs across 8 sectors
  template <class container_set>
    void  duplicate_sectors( container_set groups, 
        container_set& all_sectors)
    {
      for ( Int_t sector = 100 ; sector != 900 ; sector += 100 ) {
        for ( typename container_set::iterator igroup = groups.begin() ;
            igroup != groups.end() ; ++igroup ) {

          typename container_set::value_type group;

          for( typename container_set::value_type::iterator ipmt
              = igroup->begin();
              ipmt != igroup->end() ; ++ipmt ) {
            group.push_back( *ipmt + sector );
          }
          all_sectors.push_back( group );
        }
      }
    }

  //--------------------------------------------------

  //SMOOTHING
  class Mat2D
  {
    public:
      Mat2D( Int_t dimx, Int_t dimy );
      Mat2D( const Mat2D& other);
      Mat2D& operator=( Mat2D other);

      friend void swap( Mat2D& first, Mat2D& second );

      void FillFromHist( const TH2F&  h );
      void WriteToHist( TH2F&  h ) const;

      Float_t ** Data();
      Float_t& operator()( Int_t x, Int_t y );
      Float_t operator()( Int_t x, Int_t y ) const;
      Int_t GetDimX(){ return fDimX;}
      Int_t GetDimY(){ return fDimY;}

      ~Mat2D();

    private:
      Int_t fDimX;
      Int_t fDimY;
      Float_t ** fData;
  };

  void fill_holes( Mat2D & m );

  void negate_mat( Mat2D &m );

  class PeakFinder
  {
    public:
      explicit PeakFinder( TH2F &h );
      void FindPeaks();

      struct Peak
      {
        Float_t x;
        Float_t y;
        Float_t lnchi2;
      };

      typedef std::vector<Peak>::const_iterator const_iterator;
      typedef std::vector<Peak>::iterator iterator;
      typedef std::vector<Peak>::size_type size_type;

      const_iterator begin() const{ return fPeaks.begin(); }
      iterator begin()  { return fPeaks.begin(); }
      const_iterator end() const { return fPeaks.end(); }
      iterator end() { return fPeaks.end() ; }
      size_type size(){ return fPeaks.size(); }

      Peak get_best_peak() const;
      TH2F * GetPeaksHist(){ return &fSmoothedHist; }

    private:
      TH2F& fChi2Hist;
      TH2F fSmoothedHist;
      Mat2D fChi2Array;
      Mat2D fSmoothedArray;

      Double_t fPeakSigma;
      Double_t fPeakThreshold;
      Bool_t fRemoveBackground;
      Int_t fDeconIterations;
      Bool_t fDoMarkov;
      Int_t fMarkovWindow;

      std::vector<Peak> fPeaks;
  };

  bool operator< (const PeakFinder::Peak& lhs, const PeakFinder::Peak& rhs);

  //--------------------------------------------------

  class HitVis
  {
    public:
      HitVis(TString PMT3DPositionsFile, std::vector<vector<Int_t> >& SectorGroups );
      void Update( DataSet & ds);
      TH1 * GetOctantVis() { return &fHOctantVis; }
      TH1 * GetPMTVis() { return  &fHPMTVis; }

    private:
      TH2F fHOctantVis;
      TH2F fHPMTVis;
      std::map<Int_t, TVector2> fPositionMap;
      std::vector<vector<Int_t> >& fSectorGroups;
  };

  TVector2 ProjectPMT( Int_t pos_id, TVector3 pmt_3d_pos );

  //--------------------------------------------------

  class CedarAlign
  {
    public:
      CedarAlign();

      void SetAllTemplateFile( TString allTemplate );
      void SetSelectedTemplateFile( TString selectedTemplate );
      void LoadTemplates();

      void Init();


      void SetPMTPositionMap( TString PMTPosFile ){ fPMTPosFile = PMTPosFile ; }
      void SetPMTEffMap( TString PMTEffFile ){ fPMTEffFile = PMTEffFile ; }

      void Fill( TRecoCedarEvent& fCedarEvent );
      void ResetHits();
      void Update( TH1D & hAllHits, TH1D &hSelectedHits );
      void Update();
      void ComputeAlignment();

      TH1 * GetOctantHitVis()   { return fHitVis.GetOctantVis();              }
      TH1 * GetPMTVis()         { return fHitVis.GetPMTVis();                 }

      TH1 * GetSixthsVis()      { return fGroupAlignerSixths.GetHVis();       }
      TH1 * GetSixthsFitVis()   { return fPeakFinderOctants.GetPeaksHist();   }

      TH1 * GetOctantsVis()     { return fGroupAlignerOctants.GetHVis();      }
      TH1 * GetOctantsFitVis()  { return fPeakFinderSixths.GetPeaksHist();    }

      TH1 * GetCoarseVis()      { return fGroupAlignerCoarseSixths.GetHVis(); }

      TH1 * GetHAsymUD()        { return &fHAsymUD;                           }
      TH1 * GetHAsymSJ()        { return &fHAsymSJ;                           }
      TGraph * GetGSugXYTrend() { return &fGSugXYTrend;                       }

      Float_t GetBestX()        { return fPeakFinderOctants.get_best_peak().x
        / 1000.0; }

      Float_t GetBestY()        { return fPeakFinderOctants.get_best_peak().y
        / 1000.0; }

      Long64_t GetNEvents()     { return fNEvents;                            }

      void SetMotorPosXY( Float_t PosX, Float_t PosY);


      void PrintStats( TPaveText * Pave );
      void PrintAsym( TPaveText * Pave );


    private:

      //Chi2 aligners
      GroupAligner fGroupAlignerOctants;
      GroupAligner fGroupAlignerSixths;
      GroupAligner fGroupAlignerCoarseSixths;

      //Peak finders
      PeakFinder fPeakFinderOctants;
      PeakFinder fPeakFinderSixths;

      //Assymetries
      RatioAligner fRatioAligner;

      //Data from reconstruction
      TH1D fHAllHits;
      DataSet fDsAllHits;

      TH1D fHSelectedHits;
      DataSet fDsSelectedHits;

      //MC data
      PMTProperties fPMTProperties;
      MultiDataSet fAllHitTemplates;
      MultiDataSet fSelectedHitTemplates;

      //Groups for chi2
      std::vector<vector<Int_t> > fSectorGroups;
      std::vector<vector<Int_t> > fSixthGroups;

      //Visualisation
      HitVis fHitVis;

      TString fPMTPosFile;
      TString fPMTEffFile;

      //Trending
      TGraph fGSugXYTrend;

      //Motor Positions
      Float_t fMotorPosX;
      Float_t fMotorPosY;

      TH1D fHAsymUD;
      TH1D fHAsymSJ;
      Long64_t fNEvents;
  };
}

using CedarAlignment::CedarAlign;
#endif
