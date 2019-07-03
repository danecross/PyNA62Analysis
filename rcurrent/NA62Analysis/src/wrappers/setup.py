
#from ROOT import TCanvas, TFile
from distutils.core import setup, Extension

#importing  C++ ROOT
#environ['CPPFLAGS'] = '-I/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.16.00/x86_64-centos7-gcc48-opt/'
#environ['LDFLAGS'] = '-L/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.16.00/x86_64-centos7-gcc48-opt/lib'

import os 
dir_path = os.path.dirname(os.path.realpath(__file__))
start = dir_path.find("/SummerProject", 0, len(dir_path)-1)
user_path = dir_path[:start]



UM_module = Extension('UserMethods', sources=['UserMethodsModule.cpp'], language='C++', 
			include_dirs=[ "/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.16.00/x86_64-centos7-gcc48-opt/include/", 
					user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/",
					user_path + "/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/NA62/include/",  
					user_path + "/SummerProject/rcurrent/NA62Analysis/ToolsLib/include/", 
					user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/GigaTracker/", 
					user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/IRC/", 
					user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/LAV", 
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/LKr/",
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/MUV0",
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/MUV1/",
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/MUV2",
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/MUV3", 
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/NewCHOD/",
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/RICH", 
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/SAC",
                                        user_path + "/SummerProject/rcurrent/NA62MC/Persistency-headers/Spectrometer/",
					user_path + "/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include/", 
					user_path + "/SummerProject/rcurrent/NA62Analysis/include/",  
                                        user_path + "/SummerProject/rcurrent/NA62Tools/include/", 
					"/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.16.00/x86_64-centos7-gcc48-opt/include/" ],
			extra_link_args=['/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib/libCore.so', '-Wl,--no-undefined'], 
			extra_compile_args=['-std=c++11'],
			libraries=['stdc++'],)

setup(name='UserMethods',
      version='1.0',
      ext_modules=[UM_module],
      )