#include "TELL1RawDecoder.hh"
#include "TELL1Buffer.h"
#include "TELL1BufferProto.h"
#include <stdlib.h>

#include "Riostream.h"

#define printf_raw(x) if((x)) printf

//#define debug_cout(x) std::cout << x << std::endl
#define debug_cout(x)

TELL1RawDecoder::TELL1RawDecoder(Bool_t TriggerLess, Bool_t BothEdges) :
    NA62VRawDecoder(0, "TELL1"),
    fTriggerLess(TriggerLess),
    fBothEdges(BothEdges),
    fCurrentEventChanged(kFALSE),
    fCurrentEventFlushed(kFALSE),
    fNTrig(-1),
    fpDataBuffer(nullptr),
    fNWords(0),
    fNMEPs(0),
    fLastiTimeStamp(0)
{
    fTdcEvents = new TClonesArray("TDCEvent",20);
    for(Int_t iCh = 0; iCh < 512; iCh++)
        fChannelRemap[iCh] = iCh;
    //for(Int_t iCh = 368; iCh < 384; iCh++){
    //  fChannelRemap[iCh] = iCh + 47;
    //  fChannelRemap[iCh + 47] = iCh;
    //}
    fChannelRemap[0] = 0;
    fChannelRemap[1] = 1;
    fChannelRemap[2] = 2;
    fChannelRemap[3] = 3;
    fChannelRemap[4] = 4;
    fChannelRemap[5] = 5;
    fChannelRemap[6] = 6;
    fChannelRemap[7] = 7;
    fChannelRemap[8] = 8;
    fChannelRemap[9] = 9;
    fChannelRemap[10] = 10;
    fChannelRemap[11] = 11;
    fChannelRemap[12] = 12;
    fChannelRemap[13] = 13;
    fChannelRemap[14] = 14;
    fChannelRemap[15] = 15;
    fChannelRemap[16] = 31;
    fChannelRemap[17] = 30;
    fChannelRemap[18] = 29;
    fChannelRemap[19] = 28;
    fChannelRemap[20] = 27;
    fChannelRemap[21] = 26;
    fChannelRemap[22] = 25;
    fChannelRemap[23] = 24;
    fChannelRemap[24] = 23;
    fChannelRemap[25] = 22;
    fChannelRemap[26] = 21;
    fChannelRemap[27] = 20;
    fChannelRemap[28] = 19;
    fChannelRemap[29] = 18;
    fChannelRemap[30] = 17;
    fChannelRemap[31] = 16;
    fChannelRemap[32] = 32;
    fChannelRemap[33] = 33;
    fChannelRemap[34] = 34;
    fChannelRemap[35] = 35;
    fChannelRemap[36] = 36;
    fChannelRemap[37] = 37;
    fChannelRemap[38] = 38;
    fChannelRemap[39] = 39;
    fChannelRemap[40] = 40;
    fChannelRemap[41] = 41;
    fChannelRemap[42] = 42;
    fChannelRemap[43] = 43;
    fChannelRemap[44] = 44;
    fChannelRemap[45] = 45;
    fChannelRemap[46] = 46;
    fChannelRemap[47] = 47;
    fChannelRemap[48] = 63;
    fChannelRemap[49] = 62;
    fChannelRemap[50] = 61;
    fChannelRemap[51] = 60;
    fChannelRemap[52] = 59;
    fChannelRemap[53] = 58;
    fChannelRemap[54] = 57;
    fChannelRemap[55] = 56;
    fChannelRemap[56] = 55;
    fChannelRemap[57] = 54;
    fChannelRemap[58] = 53;
    fChannelRemap[59] = 52;
    fChannelRemap[60] = 51;
    fChannelRemap[61] = 50;
    fChannelRemap[62] = 49;
    fChannelRemap[63] = 48;
    fChannelRemap[64] = 64;
    fChannelRemap[65] = 65;
    fChannelRemap[66] = 66;
    fChannelRemap[67] = 67;
    fChannelRemap[68] = 68;
    fChannelRemap[69] = 69;
    fChannelRemap[70] = 70;
    fChannelRemap[71] = 71;
    fChannelRemap[72] = 72;
    fChannelRemap[73] = 73;
    fChannelRemap[74] = 74;
    fChannelRemap[75] = 75;
    fChannelRemap[76] = 76;
    fChannelRemap[77] = 77;
    fChannelRemap[78] = 78;
    fChannelRemap[79] = 79;
    fChannelRemap[80] = 95;
    fChannelRemap[81] = 94;
    fChannelRemap[82] = 93;
    fChannelRemap[83] = 92;
    fChannelRemap[84] = 91;
    fChannelRemap[85] = 90;
    fChannelRemap[86] = 89;
    fChannelRemap[87] = 88;
    fChannelRemap[88] = 87;
    fChannelRemap[89] = 86;
    fChannelRemap[90] = 85;
    fChannelRemap[91] = 84;
    fChannelRemap[92] = 83;
    fChannelRemap[93] = 82;
    fChannelRemap[94] = 81;
    fChannelRemap[95] = 80;
    fChannelRemap[96] = 511;
    fChannelRemap[97] = 96;
    fChannelRemap[98] = 97;
    fChannelRemap[99] = 98;
    fChannelRemap[100] = 99;
    fChannelRemap[101] = 100;
    fChannelRemap[102] = 101;
    fChannelRemap[103] = 102;
    fChannelRemap[104] = 103;
    fChannelRemap[105] = 104;
    fChannelRemap[106] = 105;
    fChannelRemap[107] = 106;
    fChannelRemap[108] = 107;
    fChannelRemap[109] = 108;
    fChannelRemap[110] = 109;
    fChannelRemap[111] = 110;
    fChannelRemap[112] = 127;
    fChannelRemap[113] = 126;
    fChannelRemap[114] = 125;
    fChannelRemap[115] = 124;
    fChannelRemap[116] = 123;
    fChannelRemap[117] = 122;
    fChannelRemap[118] = 121;
    fChannelRemap[119] = 120;
    fChannelRemap[120] = 118;
    fChannelRemap[121] = 117;
    fChannelRemap[122] = 116;
    fChannelRemap[123] = 115;
    fChannelRemap[124] = 114;
    fChannelRemap[125] = 113;
    fChannelRemap[126] = 112;
    fChannelRemap[127] = 111;
    fChannelRemap[128] = 128;
    fChannelRemap[129] = 129;
    fChannelRemap[130] = 130;
    fChannelRemap[131] = 131;
    fChannelRemap[132] = 132;
    fChannelRemap[133] = 133;
    fChannelRemap[134] = 134;
    fChannelRemap[135] = 135;
    fChannelRemap[136] = 136;
    fChannelRemap[137] = 137;
    fChannelRemap[138] = 138;
    fChannelRemap[139] = 139;
    fChannelRemap[140] = 140;
    fChannelRemap[141] = 141;
    fChannelRemap[142] = 142;
    fChannelRemap[143] = 119;
    fChannelRemap[144] = 159;
    fChannelRemap[145] = 158;
    fChannelRemap[146] = 157;
    fChannelRemap[147] = 156;
    fChannelRemap[148] = 155;
    fChannelRemap[149] = 154;
    fChannelRemap[150] = 153;
    fChannelRemap[151] = 152;
    fChannelRemap[152] = 151;
    fChannelRemap[153] = 150;
    fChannelRemap[154] = 149;
    fChannelRemap[155] = 148;
    fChannelRemap[156] = 147;
    fChannelRemap[157] = 146;
    fChannelRemap[158] = 145;
    fChannelRemap[159] = 144;
    fChannelRemap[160] = 160;
    fChannelRemap[161] = 161;
    fChannelRemap[162] = 162;
    fChannelRemap[163] = 163;
    fChannelRemap[164] = 164;
    fChannelRemap[165] = 165;
    fChannelRemap[166] = 166;
    fChannelRemap[167] = 143;
    fChannelRemap[168] = 168;
    fChannelRemap[169] = 169;
    fChannelRemap[170] = 170;
    fChannelRemap[171] = 171;
    fChannelRemap[172] = 172;
    fChannelRemap[173] = 173;
    fChannelRemap[174] = 174;
    fChannelRemap[175] = 175;
    fChannelRemap[176] = 167;
    fChannelRemap[177] = 190;
    fChannelRemap[178] = 189;
    fChannelRemap[179] = 188;
    fChannelRemap[180] = 187;
    fChannelRemap[181] = 186;
    fChannelRemap[182] = 185;
    fChannelRemap[183] = 184;
    fChannelRemap[184] = 183;
    fChannelRemap[185] = 182;
    fChannelRemap[186] = 181;
    fChannelRemap[187] = 180;
    fChannelRemap[188] = 179;
    fChannelRemap[189] = 178;
    fChannelRemap[190] = 177;
    fChannelRemap[191] = 176;
    fChannelRemap[192] = 192;
    fChannelRemap[193] = 193;
    fChannelRemap[194] = 194;
    fChannelRemap[195] = 195;
    fChannelRemap[196] = 196;
    fChannelRemap[197] = 197;
    fChannelRemap[198] = 198;
    fChannelRemap[199] = 199;
    fChannelRemap[200] = 200;
    fChannelRemap[201] = 201;
    fChannelRemap[202] = 202;
    fChannelRemap[203] = 203;
    fChannelRemap[204] = 204;
    fChannelRemap[205] = 205;
    fChannelRemap[206] = 206;
    fChannelRemap[207] = 511;
    fChannelRemap[208] = 222;
    fChannelRemap[209] = 221;
    fChannelRemap[210] = 511;
    fChannelRemap[211] = 220;
    fChannelRemap[212] = 219;
    fChannelRemap[213] = 218;
    fChannelRemap[214] = 217;
    fChannelRemap[215] = 216;
    fChannelRemap[216] = 215;
    fChannelRemap[217] = 214;
    fChannelRemap[218] = 213;
    fChannelRemap[219] = 212;
    fChannelRemap[220] = 211;
    fChannelRemap[221] = 210;
    fChannelRemap[222] = 209;
    fChannelRemap[223] = 208;
    fChannelRemap[224] = 223;
    fChannelRemap[225] = 224;
    fChannelRemap[226] = 225;
    fChannelRemap[227] = 226;
    fChannelRemap[228] = 227;
    fChannelRemap[229] = 228;
    fChannelRemap[230] = 229;
    fChannelRemap[231] = 230;
    fChannelRemap[232] = 232;
    fChannelRemap[233] = 233;
    fChannelRemap[234] = 234;
    fChannelRemap[235] = 235;
    fChannelRemap[236] = 236;
    fChannelRemap[237] = 237;
    fChannelRemap[238] = 238;
    fChannelRemap[239] = 239;
    fChannelRemap[240] = 231;
    fChannelRemap[241] = 511;
    fChannelRemap[242] = 253;
    fChannelRemap[243] = 252;
    fChannelRemap[244] = 251;
    fChannelRemap[245] = 250;
    fChannelRemap[246] = 249;
    fChannelRemap[247] = 248;
    fChannelRemap[248] = 247;
    fChannelRemap[249] = 246;
    fChannelRemap[250] = 245;
    fChannelRemap[251] = 244;
    fChannelRemap[252] = 243;
    fChannelRemap[253] = 242;
    fChannelRemap[254] = 241;
    fChannelRemap[255] = 240;
    fChannelRemap[256] = 256;
    fChannelRemap[257] = 511;
    fChannelRemap[258] = 257;
    fChannelRemap[259] = 258;
    fChannelRemap[260] = 259;
    fChannelRemap[261] = 260;
    fChannelRemap[262] = 261;
    fChannelRemap[263] = 262;
    fChannelRemap[264] = 263;
    fChannelRemap[265] = 264;
    fChannelRemap[266] = 265;
    fChannelRemap[267] = 266;
    fChannelRemap[268] = 267;
    fChannelRemap[269] = 268;
    fChannelRemap[270] = 269;
    fChannelRemap[271] = 270;
    fChannelRemap[272] = 291;
    fChannelRemap[273] = 290;
    fChannelRemap[274] = 289;
    fChannelRemap[275] = 288;
    fChannelRemap[276] = 287;
    fChannelRemap[277] = 286;
    fChannelRemap[278] = 511;
    fChannelRemap[279] = 285;
    fChannelRemap[280] = 255;
    fChannelRemap[281] = 254;
    fChannelRemap[282] = 275;
    fChannelRemap[283] = 511;
    fChannelRemap[284] = 274;
    fChannelRemap[285] = 273;
    fChannelRemap[286] = 272;
    fChannelRemap[287] = 271;
    fChannelRemap[288] = 292;
    fChannelRemap[289] = 293;
    fChannelRemap[290] = 294;
    fChannelRemap[291] = 295;
    fChannelRemap[292] = 296;
    fChannelRemap[293] = 297;
    fChannelRemap[294] = 298;
    fChannelRemap[295] = 299;
    fChannelRemap[296] = 300;
    fChannelRemap[297] = 301;
    fChannelRemap[298] = 302;
    fChannelRemap[299] = 303;
    fChannelRemap[300] = 276;
    fChannelRemap[301] = 277;
    fChannelRemap[302] = 278;
    fChannelRemap[303] = 279;
    fChannelRemap[304] = 317;
    fChannelRemap[305] = 316;
    fChannelRemap[306] = 511;
    fChannelRemap[307] = 315;
    fChannelRemap[308] = 314;
    fChannelRemap[309] = 313;
    fChannelRemap[310] = 511;
    fChannelRemap[311] = 312;
    fChannelRemap[312] = 311;
    fChannelRemap[313] = 310;
    fChannelRemap[314] = 309;
    fChannelRemap[315] = 308;
    fChannelRemap[316] = 307;
    fChannelRemap[317] = 306;
    fChannelRemap[318] = 305;
    fChannelRemap[319] = 304;
    fChannelRemap[320] = 318;
    fChannelRemap[321] = 319;
    fChannelRemap[322] = 320;
    fChannelRemap[323] = 280;
    fChannelRemap[324] = 281;
    fChannelRemap[325] = 282;
    fChannelRemap[326] = 283;
    fChannelRemap[327] = 284;
    fChannelRemap[328] = 328;
    fChannelRemap[329] = 329;
    fChannelRemap[330] = 330;
    fChannelRemap[331] = 331;
    fChannelRemap[332] = 332;
    fChannelRemap[333] = 333;
    fChannelRemap[334] = 334;
    fChannelRemap[335] = 335;
    fChannelRemap[336] = 327;
    fChannelRemap[337] = 326;
    fChannelRemap[338] = 325;
    fChannelRemap[339] = 324;
    fChannelRemap[340] = 323;
    fChannelRemap[341] = 322;
    fChannelRemap[342] = 321;
    fChannelRemap[343] = 344;
    fChannelRemap[344] = 343;
    fChannelRemap[345] = 342;
    fChannelRemap[346] = 341;
    fChannelRemap[347] = 340;
    fChannelRemap[348] = 339;
    fChannelRemap[349] = 338;
    fChannelRemap[350] = 337;
    fChannelRemap[351] = 336;
    fChannelRemap[352] = 352;
    fChannelRemap[353] = 353;
    fChannelRemap[354] = 354;
    fChannelRemap[355] = 355;
    fChannelRemap[356] = 356;
    fChannelRemap[357] = 357;
    fChannelRemap[358] = 358;
    fChannelRemap[359] = 359;
    fChannelRemap[360] = 360;
    fChannelRemap[361] = 361;
    fChannelRemap[362] = 362;
    fChannelRemap[363] = 363;
    fChannelRemap[364] = 364;
    fChannelRemap[365] = 365;
    fChannelRemap[366] = 366;
    fChannelRemap[367] = 367;
    fChannelRemap[368] = 383;
    fChannelRemap[369] = 382;
    fChannelRemap[370] = 381;
    fChannelRemap[371] = 380;
    fChannelRemap[372] = 379;
    fChannelRemap[373] = 378;
    fChannelRemap[374] = 377;
    fChannelRemap[375] = 376;
    fChannelRemap[376] = 351;
    fChannelRemap[377] = 350;
    fChannelRemap[378] = 349;
    fChannelRemap[379] = 348;
    fChannelRemap[380] = 347;
    fChannelRemap[381] = 346;
    fChannelRemap[382] = 345;
    fChannelRemap[383] = 368;
    fChannelRemap[384] = 384;
    fChannelRemap[385] = 385;
    fChannelRemap[386] = 386;
    fChannelRemap[387] = 511;
    fChannelRemap[388] = 388;
    fChannelRemap[389] = 389;
    fChannelRemap[390] = 390;
    fChannelRemap[391] = 391;
    fChannelRemap[392] = 511;
    fChannelRemap[393] = 369;
    fChannelRemap[394] = 370;
    fChannelRemap[395] = 371;
    fChannelRemap[396] = 372;
    fChannelRemap[397] = 373;
    fChannelRemap[398] = 374;
    fChannelRemap[399] = 375;
    fChannelRemap[400] = 431;
    fChannelRemap[401] = 430;
    fChannelRemap[402] = 429;
    fChannelRemap[403] = 428;
    fChannelRemap[404] = 427;
    fChannelRemap[405] = 426;
    fChannelRemap[406] = 425;
    fChannelRemap[407] = 424;
    fChannelRemap[408] = 423;
    fChannelRemap[409] = 422;
    fChannelRemap[410] = 421;
    fChannelRemap[411] = 420;
    fChannelRemap[412] = 419;
    fChannelRemap[413] = 418;
    fChannelRemap[414] = 417;
    fChannelRemap[415] = 416;
    fChannelRemap[416] = 400;
    fChannelRemap[417] = 401;
    fChannelRemap[418] = 402;
    fChannelRemap[419] = 403;
    fChannelRemap[420] = 387;
    fChannelRemap[421] = 404;
    fChannelRemap[422] = 405;
    fChannelRemap[423] = 406;
    fChannelRemap[424] = 407;
    fChannelRemap[425] = 408;
    fChannelRemap[426] = 409;
    fChannelRemap[427] = 410;
    fChannelRemap[428] = 411;
    fChannelRemap[429] = 412;
    fChannelRemap[430] = 413;
    fChannelRemap[431] = 414;
    fChannelRemap[432] = 447;
    fChannelRemap[433] = 446;
    fChannelRemap[434] = 445;
    fChannelRemap[435] = 444;
    fChannelRemap[436] = 443;
    fChannelRemap[437] = 442;
    fChannelRemap[438] = 441;
    fChannelRemap[439] = 440;
    fChannelRemap[440] = 439;
    fChannelRemap[441] = 438;
    fChannelRemap[442] = 437;
    fChannelRemap[443] = 436;
    fChannelRemap[444] = 435;
    fChannelRemap[445] = 434;
    fChannelRemap[446] = 433;
    fChannelRemap[447] = 432;
    fChannelRemap[448] = 392;
    fChannelRemap[449] = 393;
    fChannelRemap[450] = 394;
    fChannelRemap[451] = 395;
    fChannelRemap[452] = 396;
    fChannelRemap[453] = 397;
    fChannelRemap[454] = 398;
    fChannelRemap[455] = 399;
    fChannelRemap[456] = 456;
    fChannelRemap[457] = 457;
    fChannelRemap[458] = 458;
    fChannelRemap[459] = 459;
    fChannelRemap[460] = 460;
    fChannelRemap[461] = 461;
    fChannelRemap[462] = 462;
    fChannelRemap[463] = 463;
    fChannelRemap[464] = 479;
    fChannelRemap[465] = 478;
    fChannelRemap[466] = 477;
    fChannelRemap[467] = 476;
    fChannelRemap[468] = 475;
    fChannelRemap[469] = 474;
    fChannelRemap[470] = 473;
    fChannelRemap[471] = 472;
    fChannelRemap[472] = 471;
    fChannelRemap[473] = 470;
    fChannelRemap[474] = 469;
    fChannelRemap[475] = 468;
    fChannelRemap[476] = 467;
    fChannelRemap[477] = 466;
    fChannelRemap[478] = 465;
    fChannelRemap[479] = 464;
    fChannelRemap[480] = 480;
    fChannelRemap[481] = 481;
    fChannelRemap[482] = 482;
    fChannelRemap[483] = 483;
    fChannelRemap[484] = 484;
    fChannelRemap[485] = 485;
    fChannelRemap[486] = 486;
    fChannelRemap[487] = 191;
    fChannelRemap[488] = 488;
    fChannelRemap[489] = 489;
    fChannelRemap[490] = 490;
    fChannelRemap[491] = 491;
    fChannelRemap[492] = 492;
    fChannelRemap[493] = 493;
    fChannelRemap[494] = 494;
    fChannelRemap[495] = 495;
    fChannelRemap[496] = 496;
    fChannelRemap[497] = 510;
    fChannelRemap[498] = 509;
    fChannelRemap[499] = 508;
    fChannelRemap[500] = 507;
    fChannelRemap[501] = 506;
    fChannelRemap[502] = 505;
    fChannelRemap[503] = 504;
    fChannelRemap[504] = 503;
    fChannelRemap[505] = 502;
    fChannelRemap[506] = 501;
    fChannelRemap[507] = 500;
    fChannelRemap[508] = 499;
    fChannelRemap[509] = 498;
    fChannelRemap[510] = 497;
    for(Int_t iCh = 0; iCh < 512; iCh++)
        fChannelInvRemap[fChannelRemap[iCh]] = iCh;
    //for(Int_t iCh = 0; iCh < 512; iCh++)
    //  fChannelRemap[iCh] = iCh;
}

TELL1RawDecoder::~TELL1RawDecoder(){}

void TELL1RawDecoder::Reset(UInt_t * pDataBuffer, Int_t NWords)
{
    fpDataBuffer = pDataBuffer;
    fNWords = NWords;
    fNMEPs = 0;
    for(Int_t iBoard = 0; iBoard < 4; iBoard++){
        ftTime[iBoard]=0;
        fiTime[iBoard]=0;
    }
    fTdcEvents->Clear("C");
    fLastiTimeStamp = -1;
    fTdcEventStatus.clear();
}

TDCEvent* TELL1RawDecoder::GetDecodedEvent()
{
    debug_cout("TELL1RawDecoder::GetDecodedEvent: fCurrentEventChanged(" << fCurrentEventChanged << ") " << fDigiEvent 
            << " is a " << (fDigiEvent ? fDigiEvent->IsA()->GetName() : "0" ));
    if(fCurrentEventChanged || PollEventBuffer()){
        fCurrentEventChanged = kFALSE;
        fCurrentEventFlushed = kTRUE;
        return static_cast<TDCEvent*>(fDigiEvent);
    }else
        return nullptr;
}

Bool_t TELL1RawDecoder::PollEventBuffer()
{
    Bool_t en = 0;
    fCurrentEventChanged = kFALSE;
    if(fCurrentEventFlushed){
        fCurrentEventFlushed = kFALSE;
        fTdcEvents->Remove(fDigiEvent);
        fTdcEvents->Compress();
    }
    if(fTriggerLess){
        for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t)(fTdcEvents->GetEntries()); iTimeStamp++){
            if(en) std::cout << "PollEventBuffer: Status " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
                << static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " "
                    << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
        }
        for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t)(fTdcEvents->GetEntries() - 2); iTimeStamp++){
            if(en) std::cout << "PollEventBuffer: Scanning  " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
                << static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " "
                    << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
            if(fTdcEventStatus[iTimeStamp + 2] == 0xf){
                Int_t nhits=(static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->GetNHits());
                if(nhits>0)
                    for(Int_t iHit = 0; iHit < nhits; iHit++){ // loop on hits for current channel
                        TDCVHit * TdcHitOld=static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->GetHit(iHit);
                        if((( (TdcHitOld->GetLeadingEdge() > 40000) | ((TdcHitOld->GetTrailingEdge() > 40000)<<1) ) &  
                                    TdcHitOld->GetDetectedEdge()
                           ) == TdcHitOld->GetDetectedEdge() &&
                                TdcHitOld->GetMCTrackID() <= 1){
                            printf_raw(en)("TELL1Header: moving measurement nChannel %d(%d), %7.2f - %7.2f from TimeStamp %ld to %ld\n",
                                    TdcHitOld->GetChannelID(),TdcHitOld->GetDetectedEdge(),TdcHitOld->GetLeadingEdge(),
                                    TdcHitOld->GetTrailingEdge(),
                                    static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->GetTimeStamp(),static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp());
                            //printf_raw(en)("---- %f %f %d %d\n",TdcHitOld->GetLeadingEdge(),TdcHitOld->GetTrailingEdge(),
                            //        TdcHitOld->GetDetectedEdge(),TdcHitOld->GetMCTrackID());

                            TDCVHit * TdcHit = static_cast<TDCVHit*>(static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->AddHit(TdcHitOld->GetChannelID()));
                            TdcHit->SetDetectedEdge(TdcHitOld->GetDetectedEdge());
                            TdcHit->SetLeadingEdge(TdcHitOld->GetLeadingEdge());
                            TdcHit->SetTrailingEdge(TdcHitOld->GetTrailingEdge());
                            TdcHit->SetMCTrackID(4);
                            static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->RemoveHit(iHit);
                            //------------delete TdcHitOld;
                            nhits=static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp + 1])->GetNHits();
                            iHit--;
                            if(nhits == 0) break;
                        }
                    }
                if(static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp() != 0){
                    if(fTdcEventStatus[iTimeStamp] == 0xf && static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetNHits() > 0){
                        fDigiEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp]);
                        debug_cout("TELL1RawDecoder::PollEventBuffer: " << fDigiEvent << " is a " 
                                << (fDigiEvent ? fDigiEvent->IsA()->GetName() : "0" ));
                            fCurrentEventChanged = kTRUE;
                        printf_raw(en) ("TELL1Header: # of TimeStamps %d; saving %dth TimeStamp (%ld), with %d hits\n",
                                (int)fTdcEvents->GetEntries(),iTimeStamp,static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp(),fDigiEvent->GetNHits());
                        if(en){
                            //if(fDigiEvent->GetID()%1000 == 0)
                            //    std::cout << "Event " << fDigiEvent->GetID()  << " " << std::endl;
                            Int_t nhits=fDigiEvent->GetNHits();
                            if(nhits>0)
                                for(Int_t iHit = 0; iHit < nhits; iHit++) {
                                    printf_raw(en) ("TELL1Header: Ch %d Leading %7.2f Trailing %7.2f on %dth TimeStamp\n",
                                            static_cast<TDCVHit*>(fDigiEvent->GetHit(iHit))->GetChannelID(),
                                            static_cast<TDCVHit*>(fDigiEvent->GetHit(iHit))->GetLeadingEdge(),
                                            static_cast<TDCVHit*>(fDigiEvent->GetHit(iHit))->GetTrailingEdge(),
                                            static_cast<TDCVHit*>(fDigiEvent->GetHit(iHit))->GetMCTrackID());
                                }
                            //if(fDigiEvent->GetTimeStamp()%2==1 && fDigiEvent->GetNHits()>10)
                            //cout << "+++++++++++ " << fDigiEvent->GetTimeStamp() << "+++++++++++ " << fDigiEvent->GetNHits() << std::endl;
                        }
                    }else{
                        fDigiEvent = 0;
                        fNTrig--;
                    }
                }
                printf_raw(en) ("TELL1Header: # of TimeStamps %d; erasing %dth TimeStamp (%ld)\n",
                        (int)fTdcEvents->GetEntries(),iTimeStamp,static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp])->GetTimeStamp());
            //if(en) std::cout << "YYYY1 " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
            //    << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " " 
            //        << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
                if(!fCurrentEventChanged){
                    fTdcEvents->RemoveAt(iTimeStamp);
                    fTdcEvents->Compress();
                }else{
                    fLastiTimeStamp = iTimeStamp;
                }
                fTdcEventStatus.erase(fTdcEventStatus.begin()+iTimeStamp);
            //if(en) std::cout << "YYYY2 " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
            //    << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " " 
            //        << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
                iTimeStamp--;
                if(fCurrentEventChanged)
                    break;
            }
        }
    }
        for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t)(fTdcEvents->GetEntries()); iTimeStamp++){
            if(en) std::cout << "PollEventBuffer: Status2  " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
                << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetHits()->GetEntries() << " " 
                    << (*fTdcEvents)[iTimeStamp] << " " << fTdcEventStatus[iTimeStamp] << std::endl;
        }
    if(en) std::cout << "PollEventBuffer: CurrentEvent  " << fDigiEvent << " "<< fCurrentEventChanged << std::endl;    
    return fCurrentEventChanged;
}

Bool_t TELL1RawDecoder::DecodeNextEvent(Long_t * CurrentWord, Int_t iBurst) {
    UInt_t nReadEvent = 0;
    UInt_t datum;
    UInt_t nChannel,EventSize=0,tBoard=0;
    Long_t nValue;
    Bool_t en = 0;

    TDCEvent* TdcEvent = 0;
    fCurrentEventFlushed = kFALSE;

    printf_raw(en) ("DecodeTELL1Buffer ---- start -------- \n");

    for(; *CurrentWord < fNWords; (*CurrentWord)++) {
        //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0============== " << TdcEvent->GetID() << " " << *CurrentWord << std::endl;
        //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
        datum = *fpDataBuffer;
        printf_raw(en) (" DATUM: %ld 0x%08lx %d\n",(long)*CurrentWord,(unsigned long)datum,nReadEvent);
        if(isTELL1Header(datum)) {
            if(nReadEvent*4 != EventSize) std::cout << "WARNING: Event Size mismatch (" 
                << nReadEvent << " bytes instead of " << EventSize/4 << " bytes)" << std::endl;
                 //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0.1============== " << TdcEvent->GetID() << std::endl;
            //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0.1============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
            if(PollEventBuffer())
                return kTRUE;
            fNMEPs++;
            TdcEvent = 0;
            //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0.2============== " << TdcEvent->GetID() << std::endl;
            //if(en && TdcEvent) std::cout << TdcEvent << " $$$$$$$$$0.2============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
            nReadEvent = 1;
            EventSize = TELL1EventSize(datum);
            printf_raw(en) ("TELL1Header: EventSize %d, NTrig %d\n",EventSize,fNTrig);
            if(fTriggerLess){
                if(fNTrig == -1 || fTdcEvents->GetEntries() == 0){
                    TdcEvent=static_cast<TDCEvent*>( new ((*fTdcEvents)[0]) TDCEvent(TDCVHit::Class()));
                    fTdcEventStatus.push_back(0xf);
                    TdcEvent->SetTimeStamp(0);
                    TdcEvent->SetID(fNTrig);
                    TdcEvent->SetBurstID(iBurst);
                    TdcEvent->Clear();
                    fNTrig++;
                    if(en) std::cout << "TELL1Header: EventBufferSize " << fTdcEvents->GetEntries() << ", Event ID "  
                        << TdcEvent->GetID() << ", TimeStamp " 
                        << TdcEvent->GetTimeStamp() << ", " 
                        << TdcEvent << " "
                        << std::endl;
                }else if(TdcEvent == 0){
                    TdcEvent=static_cast<TDCEvent*>(fTdcEvents->Last());
                }
            }else{
                TdcEvent->Clear();
                TdcEvent->SetID(fNTrig);
                fNTrig++;
            }
            //if(en) std::cout << TdcEvent << " $$$$$$$$$1============== " << TdcEvent->GetID() << std::endl;
            //if(en) std::cout << TdcEvent << " $$$$$$$$$1============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
        }
        else if(isTELL1Error(datum)) { 
            nReadEvent++;
            printf_raw(en) ("TELL1Error \n");
        }
        else if(isTELL1Measurement(datum)) {
            //if(en) std::cout << TdcEvent << " $$$$$$$$$3============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
            nReadEvent++;
            nChannel = TELL1ChannelNumber(datum); 
            nChannel = fChannelRemap[nChannel];
            nValue = TELL1ChannelValue(datum);
            Int_t iTimeStamp = 0;
            if(fTriggerLess){
                for(iTimeStamp = 0; iTimeStamp < (Int_t) fTdcEvents->GetEntries(); iTimeStamp++){
                    if(en) std::cout << "TELL1Measurement: EventBufferScan: " << iTimeStamp << " "<< fTdcEvents->GetEntries() << " "  
                        << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetID() << " " 
                        << ((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() << " " 
                        << (*fTdcEvents)[iTimeStamp] << " " << ftTime[(Int_t)(fChannelInvRemap[nChannel]/128)] << " "
                        << fTdcEventStatus[iTimeStamp] << std::endl;
                    if(((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() == (ULong_t)ftTime[(Int_t)(fChannelInvRemap[nChannel]/128)]){
                        TdcEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp]);
                        //if(en) std::cout << "///////////||||||||| " << TdcEvent << std::endl;
                        //                fTdcEventStatus[iTimeStamp]|=(1<<tBoard);
                        break;
                    }
                }
            }
            //if(en) std::cout << TdcEvent << " $$$$$$$$$4============== " << std::endl;
            //if(en) std::cout << TdcEvent << " $$$$$$$$$4============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
            TDCVHit * LastHit = static_cast<TDCVHit*>( TdcEvent->GetLastHitOnChannel(nChannel));
            if(!(fBothEdges && LastHit) ||
                    (!isTELL1Trailing(datum) && LastHit->GetDetectedEdge() & 1) ||
                    (LastHit->GetDetectedEdge() & 2)
              ){
                if(iTimeStamp > 0){
                    TDCVHit * LastHitInPrevTimeStamp = static_cast<TDCVHit*>( static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp-1])->GetLastHitOnChannel(nChannel));
                    if((fBothEdges && LastHitInPrevTimeStamp) &&
                            LastHitInPrevTimeStamp->GetLeadingEdge() > 40000 && 
                            (isTELL1Trailing(datum) && LastHitInPrevTimeStamp->GetDetectedEdge() == 1)
                      ){
                        TdcEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp-1]);
                    }else{
                        TDCVHit * Hit = static_cast<TDCVHit*>(TdcEvent->AddHit(nChannel));
                        Hit->SetMCTrackID(fiTime[(Int_t)(fChannelInvRemap[nChannel]/128)]);
                    }
                }else{
                    TDCVHit * Hit = static_cast<TDCVHit*>(TdcEvent->AddHit(nChannel));
                    Hit->SetMCTrackID(fiTime[(Int_t)(fChannelInvRemap[nChannel]/128)]);
                }
            }
            TDCVHit * TdcHit = static_cast<TDCVHit*>(TdcEvent->GetLastHitOnChannel(nChannel));
            if(!isTELL1Trailing(datum)){
                TdcHit->SetLeadingEdge(nValue*TdcCalib);
                TdcHit->UpdateDetectedEdge(1);
            }else{
                if(nValue*TdcCalib - TdcHit->GetLeadingEdge() < -10000)
                    TdcHit->SetTrailingEdge((nValue+524287)*TdcCalib);
                else
                    TdcHit->SetTrailingEdge(nValue*TdcCalib);
                TdcHit->UpdateDetectedEdge(2);
            }
            printf_raw(en) ("Measurement(%d,%d): nChannel %d(%d), nValue(%ld) %10.3f TimeStamp %ld\n",fNTrig,TdcEvent->GetNHits(),
                    nChannel,TdcHit->GetDetectedEdge(),nValue,nValue*TdcCalib,TdcEvent->GetTimeStamp());
            //if(en) std::cout << TdcEvent << " $$$$$$$$$5============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
        }
        else if(isTELL1TimeStamp(datum)) {
            nReadEvent++;
            tBoard = TELL1TimeStampBoard(datum); 
            Int_t TimeStampDiff=TELL1TimeStamp(datum) - ftTime[tBoard];
            fiTime[tBoard]++;
            printf_raw(en)("TELL1TimeStamp: # of TimeStamps %d DeltaTimeStamp %d for board %d\n",
                    (int)fTdcEvents->GetEntries(),TimeStampDiff,tBoard);
            if(fTriggerLess && TimeStampDiff >=1){
                TdcEvent = 0;
                for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t) fTdcEvents->GetEntries(); iTimeStamp++){
                    if(((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() == TELL1TimeStamp(datum)){
                        TdcEvent = static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp]);
                        //cout << "/////////// " << TdcEvent << std::endl;
                        fTdcEventStatus[iTimeStamp]|=(1<<tBoard);
                        break;
                    }   
                }
                if(TdcEvent == 0){
                    for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t) fTdcEvents->GetEntries(); iTimeStamp++)
                        if(((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() > TELL1TimeStamp(datum)){
                            TdcEvent = static_cast<TDCEvent*>( new ((*fTdcEvents)[fTdcEvents->GetEntries()]) TDCEvent(TDCVHit::Class()));
                            TdcEvent->SetTimeStamp(TELL1TimeStamp(datum));
                            fTdcEvents->Sort();
                            fTdcEventStatus.insert(fTdcEventStatus.begin()+iTimeStamp, 1<<tBoard);
		            if(TdcEvent != (TDCEvent*)(*fTdcEvents)[iTimeStamp]){
                                std::cout << "Sort error" << std::endl;
                                exit(kGenericError);
                            }
                            break;
                        }
                    if(TdcEvent == 0){
                        TdcEvent=static_cast<TDCEvent*>( new ((*fTdcEvents)[fTdcEvents->GetEntries()]) TDCEvent(TDCVHit::Class()));
                        fTdcEventStatus.push_back(1<<tBoard);
                        TdcEvent->SetTimeStamp(TELL1TimeStamp(datum));
                    }
                    TdcEvent->SetID(fNTrig);
                    TdcEvent->SetBurstID(iBurst);
                    TdcEvent->Clear();
                    fNTrig++;
                    if(en) std::cout << "TELL1TimeStamp: Adding event " << fTdcEvents->GetEntries() << " "  
                        << TdcEvent->GetID() << " " 
                        << TdcEvent->GetTimeStamp() << " " 
                        << TdcEvent << " "
                        << std::endl;
                }
            //if(en) std::cout << TdcEvent << " $$$$$$$$$6============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
                fiTime[tBoard] = 0;
                for(Int_t iTimeStamp = 0; iTimeStamp < (Int_t) fTdcEvents->GetEntries(); iTimeStamp++){
                    if(((TDCEvent*)(*fTdcEvents)[iTimeStamp])->GetTimeStamp() == (ULong_t)ftTime[tBoard]){
                        TDCEvent * TdcEventOld=static_cast<TDCEvent*>((*fTdcEvents)[iTimeStamp]);
                            Int_t nhits = TdcEventOld->GetNHits();
                            if(nhits>0)
                                for(Int_t iHit = 0; iHit < nhits; iHit++){ // loop on hits for current channel
                                    TDCVHit * TdcHitOld = TdcEventOld->GetHit(iHit);
                                    if(fChannelInvRemap[TdcHitOld->GetChannelID()] < (Int_t)tBoard*128 || 
                                            fChannelInvRemap[TdcHitOld->GetChannelID()] >= (Int_t)(127 + tBoard*128))
                                        continue;
                                    if(ftTime[tBoard] == 0 ||  
                                                ((( (TdcHitOld->GetLeadingEdge() < 20000) | ((TdcHitOld->GetTrailingEdge() < 20000)<<1) ) &  
                                                  TdcHitOld->GetDetectedEdge()
                                                 ) == TdcHitOld->GetDetectedEdge() &&
//         (TdcHitOld->GetLeadingEdge()<20000 && TdcHitOld->GetDetectedEdge() & 1) ||
//                                                (TdcHitOld->GetTrailingEdge()<20000 && TdcHitOld->GetDetectedEdge() & 2)) &&
                                                TdcHitOld->GetMCTrackID() >= 4)
                                        ){
                                        printf_raw(en)("TELL1TimeStamp: moving measurement nChannel %d(%d), %7.2f - %7.2f from TimeStamp %ld to %ld\n",
                                                TdcHitOld->GetChannelID(),TdcHitOld->GetDetectedEdge(),TdcHitOld->GetLeadingEdge(),
                                                TdcHitOld->GetTrailingEdge(),ftTime[tBoard],TELL1TimeStamp(datum));
                                        
                                        TDCVHit * TdcHit = static_cast<TDCVHit*>(TdcEvent->AddHit(TdcHitOld->GetChannelID()));
                                        TdcHit->SetDetectedEdge(TdcHitOld->GetDetectedEdge());
                                        TdcHit->SetLeadingEdge(TdcHitOld->GetLeadingEdge());
                                        TdcHit->SetTrailingEdge(TdcHitOld->GetTrailingEdge());
                                        TdcHit->SetMCTrackID(0);
                                        TdcEventOld->RemoveHit(iHit);
                                        //------------------------------delete TdcHitOld;
                                        nhits=TdcEventOld->GetNHits();
                                        iHit--;
                                        if(nhits == 0) break;
                                    }
                                }
                    }
                }
            }
            ftTime[tBoard] = TELL1TimeStamp(datum); 
            printf_raw(en)("TELL1TimeStamp: tTime %ld(%d) for board %d\n",ftTime[tBoard],fiTime[tBoard],tBoard);
            //if(en) std::cout << TdcEvent << " $$$$$$$$$7============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
        }else if(isTELL1DummyWord(datum)) { 
            nReadEvent++;
            printf_raw(en) ("DummyWord \n");
        }else{
            nReadEvent++;
            printf_raw(en) ("Unknown Word \n");
        }
        fpDataBuffer++;      
        //if(en) std::cout << TdcEvent << " $$$$$$$$$8============== " << TdcEvent->GetID() << std::endl;
        //if(en) std::cout << TdcEvent << " $$$$$$$$$8============== " << TdcEvent->GetHits()->GetEntries() << std::endl;
    } 

    printf_raw(en) ("DecodeTELL1Buffer ---- end ---------- \n");

    return (*CurrentWord < fNWords ? kTRUE : kFALSE);

}
