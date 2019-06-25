cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

project(NA62Analysis)

set(NA62ANALYSIS ${PROJECT_SOURCE_DIR})
set(NA62FW 1)
set(INSTALLEXTENSION "-$ENV{SYSTEMINSTALL}")
set(BINDIR "bin-$ENV{SYSTEMINSTALL}")

include(scripts/NA62Analysis.cmake)

#Get all FW source files
#List all classes with both header and sources            
set(CLASSES Algorithm            
            Analyzer
            BadBursts 
            BaseAnalysis 
            CanvasOrganizer 
            ConfigAnalyzer 
            ConfigParser 
            ConfigSettings
            CounterHandler
            EventChecker
            EventFraction
            HistoHandler
            InputHandler
            InputHisto
            IOPrimitive
            InputTree
            MCSimple
            Misc
            NeuralNetwork
            OMAnalyzerWindow
            OMMainWindow
            OutputHandler
            ParticleInterface
            ParticleTree
            PrimitiveReader
            StringBalancedTable
            StringTable
            TermManip
            UserMethods
            Verbose
   )
#Additional headers
set(MAINHEADERS ${CLASSES}
            containers
            Indexer
            FWEnums
            NA62Exceptions
            Persistency
            SlimPersistency
            TimeCounter
   )
#Additional sources
set(MAINSRC ${CLASSES}
            HistoHandlerIterator
   )

SANDWICH_LIST(MAINHEADERS "include/" ".hh")
SANDWICH_LIST(MAINSRC "src/" ".cc")

include_directories(.)

add_library(AnalysisFW${LIBTYPEPOSTFIX} ${LIBTYPE} ${MAINSRC} src/TimeCounter.tcc ${MAINHEADERS})
add_dependencies(NA62Core AnalysisFW${LIBTYPEPOSTFIX})

add_subdirectory(Examples)
add_subdirectory(Analyzers)
add_subdirectory(Algorithms)
add_subdirectory(ToolsLib)
add_subdirectory(Tools)

if(COMPILE_STD_AN)
	add_subdirectory(StdConfigs)
endif()