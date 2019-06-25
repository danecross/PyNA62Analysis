#ifndef SPECTROMETERDATAQUALITYMONITOR_HH
#define SPECTROMETERDATAQUALITYMONITOR_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include <memory>
#include <fstream>

class TH1I;
class TH2F;
class TH2F;
class TGraph;
class TTree;

class SpectrometerDataQualityMonitor : public NA62Analysis::Analyzer
{
public:
    explicit SpectrometerDataQualityMonitor(NA62Analysis::Core::BaseAnalysis *ba);
    ~SpectrometerDataQualityMonitor(){};
    // analyzer functions not used in the data quality defined here as void
    void DefineMCSimple(){};
    void Process(int){};
    void PostProcess(){};
    void DrawPlot(){};
    void StartOfRunUser() {};
    void EndOfBurstUser() {};
    void EndOfRunUser(){};
    // functions defined in the .cc file:
    void InitHist();
    void InitOutput();
    void StartOfBurstUser();
    void EndOfJobUser();
    enum kBurstQualityFlags {
        kHistoMissing = 0x1,    ///< Cannot get DigiTimeRawFine or DigiTimeRawFineVsROChannel
        kEmptyBurst = 0x2,      ///< Number of straw hits in the burst is less than kMinimumNHits
        kCoverMissing = 0x3,    ///< At least one bad cover detected
        kStrawsMissing = 0x4    ///< At least three non-masked straws detected
    };
private:
    void PrintSpectrometerDQBadBurstList();
    std::unique_ptr<TCanvas> GetChannelActivityCanvas();
    std::unique_ptr<TCanvas> GetTotalTimingCanvas();
    std::unique_ptr<TCanvas> GetCoverTimingCanvas();
    std::unique_ptr<TCanvas> GetCoverAsymmetryCanvas();
    std::unique_ptr<TCanvas> GetDecoderErrorCanvas();
    std::unique_ptr<TCanvas> GetHitCanvas(int ichamber);
    std::unique_ptr<TCanvas> GetResidualsCanvas(TH1F* hOffset = nullptr, int ichamber = -1);
    const std::array<std::vector<double>, 4> ComputeAsymmetries();
    // Analyzer output (per burst)
    UInt_t fBurstQuality;
    std::vector<Int_t> fMissingCoverIDs;
    std::vector<Int_t> fMissingStrawIDs;
    // Analyzer parameters
    double fMagicT0;
    TString fOutDirName;
    TString fBadBurstFileName;
    TString fOutT0Name;
    TString fOutPdfName;
    TString fOutResidualsName;
    bool fPrintRecomputedT0s;
    bool fPrintResiduals;
    // private variables
    TString fRecoSubDirName;
    double fCoverT0s[32][16];

    // Reco histograms
    TH2F *fDigiCover, *fDigiStraw;
    TH1F *fHChannelActivity;
    TH1F *fHLeadingTotal, *fHTrailingTotal, *fHRadiusTotal, *fHStaggeringTotal;
    TH2F *fHDecoderErrors, *fHResidual;
    TH2F *fHRecoChamberHit4Total[4];
    TH2F *fHRecoChamberHit3Total[4];
    TH2F *fHRecoChamberHit2Total[4];
    TH2F *fHProjectedMissedHit[4];
    TH2F *fHCoverIDvsAllLeadingTimeTotal, *fHCoverIDvsAllLeadingTimeBurst;
    // Current burst info
    UInt_t fBurstID, fNProcessedBursts;
    // Graphs storing burst quality , missing covers and straws per burst
    TGraph *fGBurstQuality, *fGMissingCovers, *fGMissingStraws;
    // Constants
    const int kMinimumNHits = 1e6;
    // asymmetry correction due to different number of equiped straws in neighboring covers
    const std::map<int, double> kAsymmetryCorrection = {
        {67, 8./7}, {71, 7./8},
        {259, 12./11}, {263, 11./12},
        {272, 11./12}, {276, 12./11},
        {304, 11./10}, {308, 10./11},
        {323, 9./8}, {327, 8./9},
        {400, 7./8}, {404, 8./7},
        {419, 14./15},{423, 15./14},
        {432, 8./7}, {436, 7./8},
        {496, 5./4}, {500, 4./5}
    };

    const std::set<int> kCoverKnownIssue {416};
    const std::set<int> kStrawNonexist { ///< list of straw channel readout IDs in that are equipped with a straw (chamber edges, beam hole, leaking straws)
        0,1,2,3,59,60,61,62,63,76,77,78,79,112,113,114,
        115,116,256,257,258,259,260,331,332,333,334,335,
        446,447,496,497,524,525,526,527,560,561,562,563,
        564,576,577,578,579,635,636,637,638,639,779,780,
        781,782,783,832,833,834,835,836,944,945,1022,1023,
        1024,1025,1026,1027,1036,1079,1080,1081,1082,1083,
        1084,1085,1086,1087,1100,1101,1102,1103,1136,1137,
        1138,1139,1140,1141,1142,1143,1280,1358,1359,1470,
        1471,1520,1521,1585,1662,1797,1798,1799,1800,1801,
        1802,1803,1804,1805,1806,1807,1856,1857,1858,1859,
        1860,1861,1862,1863,1864,1865,1866,1968,1969,1970,
        1971,2044,2045,2046,2047,2048,2049,2050,2051,2106,
        2107,2108,2109,2110,2111,2124,2125,2126,2127,2160,
        2161,2162,2163,2164,2165,2304,2305,2306,2307,2380,
        2381,2382,2383,2494,2495,2544,2545,2572,2573,2574,
        2575,2608,2609,2610,2611,2612,2613,2624,2625,2626,
        2627,2682,2683,2684,2685,2686,2687,2828,2829,2830,
        2831,2880,2881,2882,2883,2992,2993,3070,3071,3072,
        3073,3074,3075,3126,3127,3128,3129,3130,3131,3132,
        3133,3134,3135,3148,3149,3150,3151,3184,3185,3186,
        3187,3188,3189,3190,3191,3192,3193,3518,3519,3568,
        3569,3633,3710,3845,3846,3847,3848,3849,3850,3851,
        3852,3853,3854,3855,3904,3905,3906,3907,3908,3909,
        3910,3911,3912,3913,3914,4016,4017,4018,4019,4092,
        4093,4094,4095,4096,4097,4098,4099,4155,4156,4157,
        4158,4159,4172,4173,4174,4175,4208,4209,4210,4211,
        4352,4353,4354,4355,4356,4426,4427,4428,4429,4430,
        4431,4542,4543,4592,4593,4620,4621,4622,4623,4656,
        4657,4658,4659,4672,4673,4674,4675,4731,4732,4733,
        4734,4735,4874,4875,4876,4877,4878,4879,4928,4929,
        4930,4931,4932,5040,5041,5118,5119,5120,5121,5122,
        5123,5176,5177,5178,5179,5180,5181,5182,5183,5196,
        5197,5198,5199,5232,5233,5234,5235,5236,5237,5238,
        5376,5377,5453,5454,5455,5566,5567,5616,5617,5681,
        5758,5893,5894,5895,5896,5897,5898,5899,5900,5901,
        5902,5903,5952,5953,5954,5955,5956,5957,5958,5959,
        5960,5961,5962,6064,6065,6066,6067,6140,6141,6142,
        6143,6144,6145,6146,6147,6206,6207,6220,6221,6222,
        6223,6257,6400,6401,6402,6403,6404,6405,6406,6407,
        6471,6472,6473,6474,6475,6476,6477,6478,6479,6590,
        6591,6640,6641,6668,6669,6670,6671,6705,6720,6721,
        6722,6723,6782,6783,6919,6920,6921,6922,6923,6924,
        6925,6926,6927,6976,6977,6978,6979,6980,6981,6982,
        6983,7088,7089,7166,7167,7168,7169,7170,7171,7229,
        7230,7231,7244,7245,7246,7247,7280,7281,7282,7424,
        7425,7426,7427,7428,7429,7430,7497,7498,7499,7500,
        7501,7502,7503,7614,7615,7664,7665,7729,7806,7939,
        7941,7942,7943,7944,7945,7946,7947,7948,7949,7950,
        7951,8000,8001,8002,8003,8004,8005,8006,8007,8008,
        8009,8010,8112,8113,8114,8115,8188,8189,8190,8191
    };
};

#endif
