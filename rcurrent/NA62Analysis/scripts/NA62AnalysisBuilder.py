#!/usr/bin/env python

import ConfigParser
import os
import re
import shutil
import subprocess
import sys
import hashlib
import time

from scripts import dependencyGraph
import scripts.SimpleConfigParser as SimpleConfigParser

try:
    from argparse import ArgumentParser, RawDescriptionHelpFormatter
except ImportError:
    from scripts.argparse import ArgumentParser, RawDescriptionHelpFormatter

__rev__ = None
__descr__ = ("""
   Use this script when working with NA62Analysis. The script takes care of
   operations like preparing the environment, creating, renaming and cleaning
   analyzers.
   It also takes care of parsing the configuration file and generating all necessary
   information to build the framework with the desired analyzers.
""")


# ----- Version handling -----
# Version Check
# Version Update
def getUserVersion(UserPath):
    version = ""
    # Get the user version from the hidden file in the user directory
    if os.path.exists("%s/.version" % UserPath):
        f = open("%s/.version" % UserPath, 'r')
        version = f.read()
        f.close()

    rev = 0
    # Try to parse it as integer
    try:
        rev = int(version)
    finally:
        return rev


def writeUserVersion(UserPath):
    global __rev__
    f = open("%s/.version" % UserPath, 'w')
    f.write(str(__rev__))
    f.close()


def getFWWorkspaceVersion(FWPath):
    global __rev__
    if __rev__ is not None:
        return True
    (__rev__, _) = subprocess.Popen("cd {0}; git log -n1 --pretty=format:%h -- Templates/* | xargs git describe --tags".format(FWPath), shell=True, stdout=subprocess.PIPE).communicate()
    if "r" in __rev__:
        __rev__ = int(__rev__[1:5])
        return True
    else:
        print "\033[33;1mUnable to fetch FW workspace revision\033[0m\n"
        return False


def updateOld(UserPath, FWPath):
    # Verify the Analyzer folders have been renamed Analyzers
    if os.path.exists("%s/Analyzer" % UserPath):
        if os.path.exists("%s/Analyzers" % UserPath):
            print ("""
   \033[33;1m WARNING: You have a "Analyzer" folder in you user directory.
   This folder has been renamed "Analyzers" but it seems to still exist.
   Please move all your analyzers still inside to the correct "Analyzers" folder.
   Warning : When moving your analyzers in the new folder, they may already exist
   from the automatic update of the user folder. Take care of not overwriting
   you previous work.\033[0m
""")
            sys.exit(0)
        else:
            os.rename("%s/Analyzer" % UserPath, "%s/Analyzers" % UserPath)

    # Verify that the PhysicsObjects folder has been created
    if not os.path.exists("%s/PhysicsObjects" % UserPath):
        os.mkdir("%s/PhysicsObjects" % UserPath)
    if not os.path.exists("%s/PhysicsObjects/include" % UserPath):
        os.mkdir("%s/PhysicsObjects/include" % UserPath)
    if not os.path.exists("%s/PhysicsObjects/src" % UserPath):
        os.mkdir("%s/PhysicsObjects/src" % UserPath)
    if FWPath != -1:
        if not os.path.exists("%s/PhysicsObjects/CMakeLists.txt" % UserPath):
            shutil.copyfile("%s/Templates/CMakeLists_PO.txt" % FWPath, "%s/PhysicsObjects/CMakeLists.txt" % UserPath)
        if not os.path.exists("%s/CMakeLists.txt" % UserPath):
            shutil.copyfile("%s/Templates/CMakeLists.txt" % FWPath, "%s/CMakeLists.txt" % UserPath)


def updateHeaderSignature(UserPath):
    print ("""\033[31m
    The signature of the Process and DefineMCSimple method of the analyzer have changed. This requires
    to change the method signature in every analyzer header in Analyzers/include.
    Replace both
    \t\033[32mvoid Process(int i, MCSimple &fMCSimple, Event* MCTruthEvent);\033[31m
    \t\033[32mvoid DefineMCSimple(MCSimple *fMCSimple);\033[31m
    with
    \t\033[32mvoid Process(int i);\033[31m
    \t\033[32mvoid DefineMCSimple();\033[31m
    It also requires to change the source file of every analyzer.
    Replace both
    \t\033[32mvoid clusterNN::Process(int iEvent, MCSimple &fMCSimple, Event* MCTruthEvent){\033[31m
    \t\033[32mvoid clusterNN::DefineMCSimple(MCSimple *fMCSimple){\033[31m
    with
    \t\033[32mvoid clusterNN::Process(int iEvent){\033[31m
    \t\033[32mvoid clusterNN::DefineMCSimple(){\033[31m
    and if you want access to MCTruthEvent you can now use
    \t\033[32mEvent* MCTruthEvent = GetMCEvent();\033[31m
    You can either allow this script to attempt to automatically change all your
    analyzers, or do it manually. In case you allow this script to automatically
    do the changes, a backup copy of your analyzers will be created in Analyzers/bckp.
\033[0m""")

    answer = raw_input("\tDo you want this script to automatically modify your analyzers [Y/N]?")
    if answer.lower() != "y":
        return

    if not os.path.exists("%s/Analyzers/bckp" % (UserPath)):
        os.mkdir("%s/Analyzers/bckp" % (UserPath))
    if not os.path.exists("%s/Analyzers/bckp/include" % (UserPath)):
        os.mkdir("%s/Analyzers/bckp/include" % (UserPath))
    if not os.path.exists("%s/Analyzers/bckp/src" % (UserPath)):
        os.mkdir("%s/Analyzers/bckp/src" % (UserPath))

    # Read user Analyzers include dir
    anList = os.listdir("%s/Analyzers/include" % UserPath)
    for header in anList:
        if (os.path.isdir(header)):
            continue
        # For each file, search "void Process(int xxx, MCSimple &xxx, Event* xxx);" and replace with "void Process(int iEvent);"
        original = "%s/Analyzers/include/%s" % (UserPath, header)
        backup = "%s/Analyzers/bckp/include/%s.bak" % (UserPath, header)

        shutil.copy2(original, backup)
        if not os.path.exists(backup):
            print "\033[33;1m WARNING: Unable to create backup copy of %s in %s\033[0m" % (original, backup)
            continue
        with open(backup, 'r') as f1:
            with open(original, 'w') as f2:
                print "Updating " + original
                for line in f1:
                    # search
                    m1 = re.findall(r"(.*)void Process\(int(.*),.*MCSimple.*&.*,.*Event.*\*.*\);", line)
                    m2 = re.findall(r"(.*)void DefineMCSimple\(.*MCSimple.*\*.*\);", line)
                    if m1:
                        print "\033[31m- " + line.replace("\n", "") + "\033[0m"
                        print "\033[32m+ " + m1[0][0] + "void Process(int" + m1[0][1] + ");\n\033[0m",
                        f2.write(m1[0][0] + "void Process(int" + m1[0][1] + ");\n")
                    elif m2:
                        print "\033[31m- " + line.replace("\n", "") + "\033[0m"
                        print "\033[32m+ " + m2[0] + "void DefineMCSimple();\n\033[0m",
                        f2.write(m2[0] + "void DefineMCSimple();\n")
                    else:
                        f2.write(line)

    # Read user Analyzers src dir
    anList = os.listdir("%s/Analyzers/src" % UserPath)
    for source in anList:
        if(os.path.isdir(source)):
            continue
        # For each file, search "void clusterNN::Process(int ${1}, MCSimple &xxx, Event* ${2}){"
        # and replace with
        #     void clusterNN::Process(int ${1}){
        #         Event* ${2}
        #         if(GetWithMC()) ${2}= GetMCEvent()
        original = "%s/Analyzers/src/%s" % (UserPath, source)
        backup = "%s/Analyzers/bckp/src/%s.bak" % (UserPath, source)

        shutil.copy2(original, backup)
        if not os.path.exists(backup):
            print "\033[33;1m WARNING: Unable to create backup copy of %s in %s\033[0m" % (original, backup)
            continue

        with open(backup, 'r') as f1:
            with open(original, 'w') as f2:
                print "Updating " + original
                for line in f1:
                    # search
                    m1 = re.findall(r"(.*)void (.*)::Process\(int(.*),.*MCSimple.*&.*,.*Event.*\*(.*)\)(.*)", line)
                    m2 = re.findall(r"(.*)void (.*)::DefineMCSimple\(.*MCSimple.*\*.*\)(.*)", line)
                    m3 = re.findall(r"(.*)fMCSimple->(.*)", line)
                    if m1:
                        print "\033[31m- " + line.replace("\n", "") + "\033[0m"
                        print "\033[32m+ " + m1[0][0] + "void " + m1[0][1] + "::Process(int" + m1[0][2] + ")" + m1[0][4] + "\033[0m\n",
                        print "\033[32m+ " + m1[0][0] + "\tEvent* " + m1[0][3] + " = NULL;\033[0m\n",
                        print "\033[32m+ " + m1[0][0] + "\tif(GetWithMC()) " + m1[0][3] + "= GetMCEvent();\033[0m\n",
                        f2.write(m1[0][0] + "void " + m1[0][1] + "::Process(int" + m1[0][2] + ")" + m1[0][4] + "\n")
                        f2.write(m1[0][0] + "\tEvent* " + m1[0][3] + " = NULL;\n")
                        f2.write(m1[0][0] + "\tif(GetWithMC()) " + m1[0][3] + "= GetMCEvent();\n")
                    elif m2:
                        print "\033[31m- " + line.replace("\n", "") + "\033[0m"
                        print "\033[32m+ " + m2[0][0] + "void " + m2[0][1] + "::DefineMCSimple()" + m2[0][2] + "\033[0m\n",
                        f2.write(m2[0][0] + "void " + m2[0][1] + "::DefineMCSimple()" + m2[0][2] + "\n")
                    elif m3:
                        print "\033[31m- " + line.replace("\n", "") + "\033[0m"
                        print "\033[32m+ " + m3[0][0] + "fMCSimple." + m3[0][1] + "\033[0m\n",
                        f2.write(m3[0][0] + "fMCSimple." + m3[0][1] + "\n")
                    else:
                        f2.write(line)


def updateAnalyzerUUID(UserPath):
    print ("""\033[31m
    A unique ID must be generated for every analyzer. The unique ID must be set in the
    constructor of the analyzer. This script will attempt to modify your existing
    analyzers.
    The constructors
    \t\033[32mMyAnalyzer::MyAnalyzer(Core::BaseAnalysis *ba) : Analyzer(ba, "MyAnalyzer")\033[31m
    must be replaced with
    \t\033[32mMyAnalyzer::MyAnalyzer(Core::BaseAnalysis *ba) : Analyzer(ba, "MyAnalyzer", "UniquelyGeneratedID")\033[31m
    You can either allow this script to attempt to automatically change all your
    analyzers, or do it manually. In case you allow this script to automatically
    do the changes, a backup copy of your analyzers will be created in Analyzers/bckp.
    Else this script will generate unique ID for every of your analyzer, please copy-paste these
    unique ID in the corresponding analyzer.
\033[0m""")

    answer = raw_input("\tDo you want this script to automatically modify your analyzers [Y/N]?")

    doIt = True
    if answer.lower() != "y":
        doIt = False

    if doIt:
        if not os.path.exists("%s/Analyzers/bckp" % (UserPath)):
            os.mkdir("%s/Analyzers/bckp" % (UserPath))
        if not os.path.exists("%s/Analyzers/bckp/src" % (UserPath)):
            os.mkdir("%s/Analyzers/bckp/src" % (UserPath))

    # Read user Analyzers src dir
    anList = os.listdir("%s/Analyzers/src" % UserPath)
    for src in anList:
        if (os.path.isdir("%s/Analyzers/src/%s" % (UserPath, src))):
            continue

        if doIt:
            # For each file, search "test::test(Core::BaseAnalysis *ba) : Analyzer(ba, "test")" and replace with
            # "test::test(Core::BaseAnalysis *ba) : Analyzer(ba, "test", "0705bb9f1e7e22e84360da41d42f07a6")"
            original = "%s/Analyzers/src/%s" % (UserPath, src)
            backup = "%s/Analyzers/bckp/src/%s.bak" % (UserPath, src)

            shutil.copy2(original, backup)
            if not os.path.exists(backup):
                print "\033[33;1m WARNING: Unable to create backup copy of %s in %s\033[0m" % (original, backup)
                continue
        else:
            backup = "%s/Analyzers/src/%s" % (UserPath, src)

        with open(backup, 'r') as f1:
            fullLine = ""
            if doIt:
                with open(original, 'w') as f2:
                    print "Updating " + original
                    for line in f1:
                        fullLine += line
                        if line.rstrip().endswith(':'):
                            continue
                        # search
                        m1 = re.findall(r"(?P<name>.*)::(?P=name)\((.*)\)\s:\s(.*)\(([^,]*), \"([^,]*)\"(?!,.*)\)(.*)", fullLine)
                        if m1:
                            print "\033[31m- " + fullLine.replace("\n", "") + "\033[0m"
                            print "\033[32m+ " + m1[0][0] + "::" + m1[0][0] + "(" + m1[0][1] + ") : " + m1[0][2] + "(" + m1[0][3] + ", \"" + m1[0][4] + "\", \"" + generateMD5(m1[0][0]) + "\")" + m1[0][5] + "\n\033[0m",
                            f2.write(m1[0][0] + "::" + m1[0][0] + "(" + m1[0][1] + ") : " + m1[0][2] + "(" + m1[0][3] + ", \"" + m1[0][4] + "\", \"" + generateMD5(m1[0][0]) + "\")" + m1[0][5] + "\n")
                        else:
                            f2.write(fullLine)

                        fullLine = ""
            else:
                for line in f1:
                    fullLine += line
                    if line.rstrip().endswith(':'):
                        continue
                    # search
                    m1 = re.findall(r"(?P<name>.*)::(?P=name)\((.*)\)\s:\s(.*)\(([^,]*), \"([^,]*)\"(?!,.*)\)(.*)", fullLine)
                    if m1:
                        print m1
                        print ("\033[31m For analyzer " + m1[0][0]).ljust(50) + " use " + generateMD5(m1[0][0]) + "\033[0m"
                    else:
                        pass

                    fullLine = ""


def updateOfficialUUID(FWPath, subPath):
    print ("""\033[31m
    A unique ID must be generated for every analyzer. The unique ID must be set in the
    constructor of the analyzer. This script will attempt to modify your existing
    analyzers.
    The constructors
    \t\033[32mMyAnalyzer::MyAnalyzer(Core::BaseAnalysis *ba) : Analyzer(ba, "MyAnalyzer")\033[31m
    must be replaced with
    \t\033[32mMyAnalyzer::MyAnalyzer(Core::BaseAnalysis *ba) : Analyzer(ba, "MyAnalyzer", "UniquelyGeneratedID")\033[31m
    A backup copy of the analyzers will be created in {0}/bckp.\033[0m""".format(subPath))

    if not os.path.exists("%s/%s/bckp" % (FWPath, subPath)):
        os.mkdir("%s/%s/bckp" % (FWPath, subPath))
    if not os.path.exists("%s/%s/bckp/src" % (FWPath, subPath)):
        os.mkdir("%s/%s/bckp/src" % (FWPath, subPath))

    # Read user Analyzers src dir
    anList = os.listdir("%s/%s/src" % (FWPath, subPath))
    for src in anList:
        if (os.path.isdir("%s/%s/src/%s" % (FWPath, subPath, src))):
            continue

        # For each file, search "test::test(Core::BaseAnalysis *ba) : Analyzer(ba, "test")" and replace with
        # "test::test(Core::BaseAnalysis *ba) : Analyzer(ba, "test", "0705bb9f1e7e22e84360da41d42f07a6")"
        original = "%s/%s/src/%s" % (FWPath, subPath, src)
        backup = "%s/%s/bckp/src/%s.bak" % (FWPath, subPath, src)

        shutil.copy2(original, backup)
        if not os.path.exists(backup):
            print "\033[33;1m WARNING: Unable to create backup copy of %s in %s\033[0m" % (original, backup)
            continue

        with open(backup, 'r') as f1:
            fullLine = ""
            with open(original, 'w') as f2:
                print "Updating " + original
                for line in f1:
                    fullLine += line
                    if line.rstrip().endswith(':'):
                        continue
                    # search
                    m1 = re.findall(r"(?P<name>.*)::(?P=name)\((.*)\)\s:\s(.*)\(([^,]*), \"([^,]*)\"(?!,.*)\)(.*)", fullLine)
                    if m1:
                        print "\033[31m- " + fullLine.replace("\n", "") + "\033[0m"
                        print "\033[32m+ " + m1[0][0] + "::" + m1[0][0] + "(" + m1[0][1] + ") : " + m1[0][2] + "(" + m1[0][3] + ", \"" + m1[0][4] + "\", \"" + generateMD5(m1[0][0]) + "\")" + m1[0][5] + "\n\033[0m",
                        f2.write(m1[0][0] + "::" + m1[0][0] + "(" + m1[0][1] + ") : " + m1[0][2] + "(" + m1[0][3] + ", \"" + m1[0][4] + "\", \"" + generateMD5(m1[0][0]) + "\")" + m1[0][5] + "\n")
                    else:
                        f2.write(fullLine)

                    fullLine = ""


def updateSettings(UserPath, FwPath, old_rev, new_rev):
    p = ConfigParser.RawConfigParser()
    p.add_section("Global")
    p.read("%s/Templates/settingsna62" % FwPath)
    updated = ConfigParser.RawConfigParser()
    updated.add_section("Global")
    updated.read("%s/Templates/settingsna62" % FwPath)
    p.read("%s/.settingsna62" % UserPath)

    if p.has_option("Global", "badburstdbpath"):
        p.set("Global", "badburstdb", p.get("Global", "badburstdbpath"))
        p.remove_option("Global", "badburstdbpath")

    if p.has_option("Global", "metadatapath") and not p.has_option("Global", "badburstdb"):
        p.set("Global", "badburstdb", "{0}/BadBursts.txt".format(p.get("Global", "metadatapath")))

    if p.has_section("Force"):
        revisionJumps = p.options("Force")
        for revJump in revisionJumps:
            if old_rev < int(revJump) and new_rev >= int(revJump):
                # must force parameter update
                paramsToUpdate = p.get("Force", revJump).split(",")
                for param in paramsToUpdate:
                    if p.has_option("Global", param) and updated.has_option("Global", param):
                        p.set("Global", param, updated.get("Global", param))

    p.remove_section("Force")
    with open("%s/.settingsna62" % UserPath, "wb") as configFile:
        p.write(configFile)

    with open("%s/.settingsna62" % UserPath, "r") as configFile:
        lines = configFile.readlines()
        for i, line in enumerate(lines):
            if line == "[Global]\n":
                lines.insert(i + 1, "; UseColors : If set to true NA62Analysis can use colors in the standard and error output. (default: true)\n")
                lines.insert(i + 2, "; ProcessOutputNewLine : NA62Analysis regularly outputs the number of events already read. (default: true)\n")
                lines.insert(i + 3, ";  - If set to true this output is printed on a new line every time.\n")
                lines.insert(i + 4, ";  - If set to false, a carriage return is used instead of a new line and the line is replaced.\n")
                lines.insert(i + 5, "; The two memory monitor variables (MemoryReportEventInterval and MemoryReportTimeInterval) have special \n")
                lines.insert(i + 6, ";  values 0 and -1. (default:-1  for MemoryReportEventInterval and 0 for MemoryReportTimeInterval)\n")
                lines.insert(i + 7, ";  -  0: no monitoring\n")
                lines.insert(i + 8, ";  - -1: monitoring at the same rate as the processing status printing (only for event based monitoring)\n")
                lines.insert(i + 9, "; SkippedName: Name of the file containing skipped input files. (default: NA62Analysis.skipped)\n")
                lines.insert(i + 10, "; SVCClass: castor/eos service class. (default: na62)\n")
                lines.insert(i + 11, "; MetadataPath: Path to the metadata directory. (default: /afs/cern.ch/na62/offline/metadata/)\n")
                lines.insert(i + 12, "; DefaultAutoupdateRate: Refresh rate of histograms marked as autoupdate in units of processed events. (default: 1000)\n")
                lines.insert(i + 13, "; DateTimeFormat: Format of the date and time display in the output. (default: %d/%m/%y %H:%M:%S)\n")
                lines.insert(i + 14, "; OutputPrefixFormat: Format of the verbosity output message. %d: date (according to datatimeformat), %l verbosity level, %n module name (default: [%n]), previous default: [%d] %l - [%n]")
                lines.insert(i + 15, "; BasketSize: Size of the output tree basket (see ROOT documentation). (default: 0)\n")
                lines.insert(i + 16, "; AutoFlush: AutoFlush value of the output tree (see ROOT documentation). (default: -10000)\n")
                lines.insert(i + 17, "; BadBurstPath: Path to the directory containing the bad burst list. (default: /afs/cern.ch/na62/offline/metadata/)\n")
                lines.insert(i + 18, "; NonExistingWarning: Turn on/off warning when trying to access non existing objects in the framework (histograms, counters, branches). (default:true)\n")
                lines.insert(i + 19, "; Verbosity: Global default verbosity level of the framework (overriden by -v command line option). (default: kNormal)\n")
                break

    with open("%s/.settingsna62" % UserPath, "wb") as configFile:
        configFile.writelines(lines)


def copyTemplates(FWPath, UserPath, NA62RECOSOURCE):
    # Always replace the CMakeLists.txt in case it changed

    shutil.copyfile("%s/Templates/CMakeLists.txt" % FWPath, "%s/CMakeLists.txt" % UserPath)
    shutil.copyfile("%s/Templates/CMakeLists_PO.txt" % FWPath, "%s/PhysicsObjects/CMakeLists.txt" % UserPath)
    shutil.copyfile("%s/Templates/CMakeLists_Algos.txt" % FWPath, "%s/Algorithms/CMakeLists.txt" % UserPath)

    readAndReplace("%s/Templates/env.sh" % FWPath, "%s/scripts/env.sh" % UserPath, {"$$ANALYSISFW$$": FWPath, "$$USERDIR$$": UserPath, "$$NA62RECOSOURCE$$": NA62RECOSOURCE})

    shutil.copyfile("%s/Templates/rootlogon.C" % FWPath, "%s/.rootlogon.C" % UserPath)
    shutil.copyfile("%s/Templates/rootlogon.C" % FWPath, "%s/../NA62MC/.rootlogon.C" % FWPath)
    shutil.copyfile("%s/Templates/rootlogon.C" % FWPath, "%s/../NA62Reconstruction/.rootlogon.C" % FWPath)


def checkUpdate():
    global __rev__
    UserPath = getVar("ANALYSISFW_USERDIR", -1)
    FWPath = getVar("ANALYSISFW_PATH", -1)
    NA62RECOSOURCE = getCheckVar("NA62RECOSOURCE")

    if(not getFWWorkspaceVersion(FWPath)):
        print "\033[94mAborting update userspace version check\033[0m"
        print "\033[94mForcing template update without updating user revision\033[0m"
        print("\n\nUserPath: %s\n\n" % UserPath)
        copyTemplates(FWPath, UserPath, NA62RECOSOURCE)
        return

    if UserPath != -1:
        if not os.path.exists(UserPath) or not os.path.exists("%s/Analyzers" % UserPath):
            return
        # check version number
        version = getUserVersion(UserPath)
        print "\033[94mUser workspace version:%s, FW workspace version:%s \033[0m" % (version, __rev__)

        # Old versions with no version number
        updateOld(UserPath, FWPath)

        # Newer versions
        if int(version) < __rev__:
            print "\033[94mUpdating user directory from revision %s to revision %s \033[0m\n" % (version, __rev__)
            if(int(version)) <= 385:
                updateHeaderSignature(UserPath)
            # if(int(version))<=835:
                # updateAnalyzerUUID(UserPath)
                # updateOfficialUUID(FWPath, "Analyzers/CalibrationTools")
                # updateOfficialUUID(FWPath, "Analyzers/MonitoringTools")
                # updateOfficialUUID(FWPath, "Analyzers/Physics")
                # updateOfficialUUID(FWPath, "Analyzers/PhysicsTools")
                # updateOfficialUUID(FWPath, "Analyzers/TestTools")
                # updateOfficialUUID(FWPath, "Examples")

            updateSettings(UserPath, FWPath, version, __rev__)

            if not os.path.exists("%s/Algorithms" % UserPath):
                os.mkdir("%s/Algorithms" % UserPath)
            if not os.path.exists("%s/Algorithms/include" % UserPath):
                os.mkdir("%s/Algorithms/include" % UserPath)
            if not os.path.exists("%s/Algorithms/src" % UserPath):
                os.mkdir("%s/Algorithms/src" % UserPath)

            copyTemplates(FWPath, UserPath, NA62RECOSOURCE)

            # write the new version file
            writeUserVersion(UserPath)


# ----- Utility functions -----
# Directory listing with filtering
def listDirClean(path):
    list = os.listdir(path)
    list = [x for x in list if ".svn" not in x]
    list = [x for x in list if os.path.isdir("%s/%s" % (path, x))]
    return list


def listFileClean(path):
    list = os.listdir(path)
    list = [x for x in list if ".svn" not in x]
    list = [x for x in list if not os.path.isdir("%s/%s" % (path, x))]
    return list


# Execute a command in suitable shell
def bash_command(cmd):
    p = subprocess.Popen(cmd, shell=True, executable='/bin/bash')
    return p.wait() == 0


# Check existence of environment variable an get it
def getCheckVar(name):
    var = os.getenv(name, -1)
    if var == -1:
        print "\033[33;1m WARNING: Environment variable %s was not found.\033[0m" % name
        sys.exit(0)
    return var


# Get environment variable and set to default if not existing
def getVar(name, default):
    var = os.getenv(name, -1)
    if var == -1:
        var = default
    return var


# Read a template file and copy it to oPath, replacing every
# matching string found in the searchMap by its value
def readAndReplace(iPath, oPath, searchMap, skipComments=True):
    inComment = False

    with open(iPath, 'r') as f1:
        with open(oPath, 'w') as f2:
            for line in f1:
                if skipComments:
                    # Skip comment lines (but not doxygen doc)
                    if "//" in line and "///" not in line:
                        continue
                    # Skip comment blocks
                    if "/**" in line:
                        inComment = True
                    if inComment and ("*/" in line):
                        inComment = False
                        continue
                    if inComment:
                        continue
                for old in searchMap:
                    line = line.replace(old, searchMap[old])
                f2.write(line)


# Test if a file is binary or text
def is_binary(filename):
    """Return true if the given filename is binary.
    @raise EnvironmentError: if the file does not exist or cannot be accessed.
    @attention: found @ http://bytes.com/topic/python/answers/21222-determine-file-type-binary-text on 6/08/2010
    @author: Trent Mick <TrentM@ActiveState.com>
    @author: Jorge Orpinel <jorge@orpinel.com>"""
    fin = open(filename, 'rb')
    try:
        CHUNKSIZE = 1024
        while 1:
            chunk = fin.read(CHUNKSIZE)
            if '\0' in chunk:  # found null byte
                return True
            if len(chunk) < CHUNKSIZE:
                break  # done
    finally:
        fin.close()

    return False


# Generate a MD5 from name+timestamp
def generateMD5(name):
    md5word = hashlib.md5()
    md5word.update(name)
    md5word.update("%i" % time.time())
    return md5word.hexdigest()


# ----- Analyzer handling functions -----
# Check histogram use and booking coherence
def check_histo(an, iPath):
    with open("%s/src/%s.cc" % (iPath, an), 'r') as f1:
        inComment = False

        uses = []
        defs = set()
        error = False

        for i, line in enumerate(f1):
            # Skip comment blocks
            if line.strip().startswith("/*"):
                inComment = True
            if inComment and ("*/" in line.strip()[2:]):
                inComment = False
                continue
            if line.strip().startswith("//"):
                continue
            if inComment:
                continue

            # List booked histograms in analyzer
            m = re.findall(r"(fHisto|fHisto2|fGraph)\[\"(.*?)\"\]", line)
            for e in m:
                uses.append((e[1], i))

            # List used histograms in analyzer
            m = re.findall(r"FillHisto\(\"(.*?)\"", line)
            for e in m:
                uses.append((e, i))

            m = re.findall(r"BookHisto\(\"(.*?)\"", line)
            for e in m:
                defs.add(e)

        # Check all used histograms have been booked
        for e in uses:
            if not e[0] in defs:
                print "\033[31;1mError in analyzer %s : Histogram %s not defined at line %s\033[0m" % (an, e[0], e[1])
                error = True

    return error


# Check analyzer dependencies with dependencyGraph
def checkDependence(depsGraph, name, prefix):
    inComment = False
    with open("%s/src/%s.cc" % (prefix, name), 'r') as f:
        for line in f:
            # skip comment blocks
            if line.find("/*") >= 0 and line.find("//*") < 0:
                inComment = True
            if inComment:
                if line.find("*/") >= 0:
                    inComment = False
                    if line.find("*/") == len(line.rstrip()) - 2:
                        continue
                else:
                    continue

            # search for added preanalyzers, otherwise skip commented lines
            if line.strip().startswith("//"):
                if "add_preanalyzer" not in line:
                    continue
                line = line.strip(" /").replace("=", " ").replace(",", " ")
                parts = line.split()
                for an in parts[parts.index("add_preanalyzer") + 1:]:
                    if "//" in an:
                        break
                    depsGraph.addDependency(name, an)
                continue

            # Regex matching analyzer call introducing dependency
            m = re.search(r"(?:[^/+].*)?GetOutput(?:<.*>)?\(\"(.*)\..*\".*\);", line)
            if m:
                depsGraph.addDependency(name, m.group(1))


def checkAnalyzerNameConsistency(name, dir):
    inComment = False
    isMatching = False
    foundNames = []
    with open("%s/include/%s.hh" % (dir, name), 'r') as f:
        for line in f:
            # skip comment blocks
            if line.find("/**") >= 0 and line.find("//**") < 0:
                inComment = True
            if inComment:
                if line.find("*/") >= 0:
                    inComment = False
                else:
                    continue
            # skip comment lines
            if line.strip().startswith("//"):
                continue

            # Regex matching analyzer call introducing dependency
            m = re.search(r"class\s*(.*?)\s*:\s*public.*", line)
            if m:
                foundNames.append(m.group(1))
                if m.group(1) == name:
                    isMatching = True
    return isMatching, foundNames


# Check analyzer existence and return to which hierarchy it belongs to
def checkAnalyzerExists(an, FWPath, userPath):
    anType = []
    anDir = []
    anDisplayDir = []
    for dir in listDirClean("%s/Analyzers" % (FWPath)):
        if os.path.exists("%s/Analyzers/%s/include/%s.hh" % (FWPath, dir, an)):
            anType.append(1)
            anDir.append(dir)
            anDisplayDir.append("%s/Analyzers/%s" % (FWPath, dir))
    if os.path.exists("%s/Analyzers/include/%s.hh" % (userPath, an)):
        anType.append(2)
        anDir.append("")
        anDisplayDir.append("%s/Analyzers" % (userPath))
    elif os.path.exists("%s/Examples/include/%s.hh" % (FWPath, an)):
        anType.append(3)
        anDir.append("")
        anDisplayDir.append("%s/Examples" % (FWPath))

    if len(anType) == 1:
        isMatching, names = checkAnalyzerNameConsistency(an, anDisplayDir[0])
        if not isMatching:
            print ("\033[31;1mError: In %s does not have a consistent naming. The class name (%s) and files name (%s.cc,%s.hh) must be identical\033[0m") % (anDisplayDir[0], ", ".join(names), an, an)
            return [-1, ""]
        return [anType[0], anDir[0]]
    elif len(anType) > 0:
        print ("\033[31;1mError: Conflict for Analyzer %s. Multiple occurences found in:\n  %s\033[0m") % (an, "\n  ".join(anDisplayDir))
        return [-1, ""]
    else:
        return [0, ""]


def createAnalyzerGraph(analyzers, FWPath, UserPath, preAnalyzers=None):
    usrAnList = ""
    subFolder = ""
    missing = False
    wrongList = False
    depsGraph = dependencyGraph.DependencyGraph()

    for anDef in analyzers:
        an = parseAnalyzerDef(anDef)[0]
        [anType, subFolder] = checkAnalyzerExists(an, FWPath, UserPath)
        if anType == -1:
            missing = True
        elif anType == 0:
            answer = raw_input("Analyzer %s does not exist. Do you want to create it [Y/N]" % (an))
            if answer.lower() == "y":

                class temp():
                    AnalyzerName = [anDef]

                createAnalyzer(temp())
            else:
                missing = True
        else:
            if anType == 1:
                prefix = "%s/Analyzers/%s" % (FWPath, subFolder)
            elif anType == 2:
                prefix = "%s/Analyzers" % UserPath
                usrAnList += "%s " % an
            elif anType == 3:
                prefix = "%s/Examples" % FWPath
            depsGraph.addNode(an)
            checkDependence(depsGraph, an, prefix)
            wrongList = wrongList or preProcessAnalyzer(an, prefix, preAnalyzers is None)
            depList = list(depsGraph.getDependencies(an))
            for d in depList:
                if d not in analyzers:
                    if preAnalyzers is None or d not in preAnalyzers:
                        if isPreAnalyzer(d, FWPath, UserPath) and preAnalyzers is not None:
                            print "\033[33;1mWARNING: Missing dependence %s for %s. Adding it to the list of pre-analyzers\033[0m" % (d, an)
                            preAnalyzers.append(d)
                            depsGraph.removeDependency(an, d)
                        else:
                            print "\033[33;1mWARNING: Missing dependence %s for %s. Adding it to the list of analyzers\033[0m" % (d, an)
                            analyzers.append(d)
                    else:
                        depsGraph.removeDependency(an, d)

    return [missing, wrongList, depsGraph, usrAnList]


def isPreAnalyzer(name, FWPath, UserPath):
    [anType, subFolder] = checkAnalyzerExists(name, FWPath, UserPath)
    if anType == 1:
        prefix = "%s/Analyzers/%s" % (FWPath, subFolder)
    elif anType == 2:
        prefix = "%s/Analyzers" % UserPath
    elif anType == 3:
        prefix = "%s/Examples" % FWPath
    else:
        return False

    with open("%s/src/%s.cc" % (prefix, name), 'r') as f:
        for line in f:
            if line.lower().find("#pragma") >= 0:
                if line.lower().find("pre-analyzer") >= 0:
                    return True

    with open("%s/include/%s.hh" % (prefix, name), 'r') as f:
        for line in f:
            if line.lower().find("#pragma") >= 0:
                if line.lower().find("pre-analyzer") >= 0:
                    return True

    return False


def preProcessAnalyzer(name, prefix, isPreAnalyzer):
    returnVal = False
    with open("%s/src/%s.cc" % (prefix, name), 'r') as f:
        for line in f:
            if line.lower().find("#pragma") >= 0:
                if line.lower().find("pre-analyzer") >= 0 and not isPreAnalyzer:
                    print ("\033[31;1mError: Analyzer %s is a pre-analyzer. It cannot be used in the analyzers list. "
                           "Please move it to preanalyzers in the build configuration file. \033[0m"
                           ) % (name)
                    returnVal = True

    with open("%s/include/%s.hh" % (prefix, name), 'r') as f:
        for line in f:
            if line.lower().find("#pragma") >= 0:
                if line.lower().find("pre-analyzer") >= 0 and not isPreAnalyzer:
                    print ("\033[31;1mError: Analyzer %s is a pre-analyzer. It cannot be used in the analyzers list. "
                           "Please move it to preanalyzers in the build configuration file. \033[0m"
                           ) % (name)
                    returnVal = True

    return returnVal


# ----- Building functions -----
def isPrecompiled(FWPath):
    if "cvmfs/na62.cern.ch" not in FWPath:
        return False

    return True


def buildFW(FWPath, defines, jobs, core_only):
    if isPrecompiled(FWPath):
        return

    shell = getVar("FWSHELL", "sh")
    buildType = getVar("SYSTEMINSTALL", "")
    if not os.path.exists("%s/build-%s" % (FWPath, buildType)):
        command = ["cd %s && source ./scripts/env.%s && cmake -H. -Bbuild-%s" % (FWPath, shell, buildType)]
        for d in defines or []:
            command.append(" -D%s=1" % d)
        bash_command(''.join(command))
    target = ""
    if core_only:
        target = "NA62Core NA62Tools"
    success = bash_command("cd %s/build-%s && source ../scripts/env.%s && make -j%s %s" % (FWPath, buildType, shell, jobs, target))
    return not success


def buildUser(defines, jobs):
    shell = getVar("FWSHELL", "sh")
    buildType = getVar("SYSTEMINSTALL", "")
    if not os.path.exists("build-%s" % buildType):
        command = ["source ./scripts/env.%s && cmake -H. -Bbuild-%s" % (shell, buildType)]
        for d in defines or []:
            command.append(" -D%s=1" % d)
        bash_command(''.join(command))
    bash_command("cd build-%s && source ../scripts/env.%s && make -j%s" % (buildType, shell, jobs))


# Parse analyzer definition from config file (or command line)
# Matching analyzerName(InputType InputName, ...)
def parseAnalyzerDef(name):
    m = re.findall(r"(.*?)\((.*)\)", name)
    if not m:
        return [name, 0]
    else:
        anName = m[0][0]
        inputs = m[0][1].split(",")

    return [anName, inputs]


# -----------------------------
# ----- Command functions -----
# -----------------------------
# ----- Clean functions -------
def cleanUser(args):
    UserPath = getCheckVar("ANALYSISFW_USERDIR")
    buildType = getVar("SYSTEMINSTALL", "")

    print "cd %s" % UserPath
    os.chdir(UserPath)
    print "rm -rf obj-%s" % buildType
    print "rm -rf lib-%s" % buildType
    print "rm -rf bin-%s" % buildType
    print "rm -rf build-%s" % buildType
    if os.path.exists("obj-%s" % buildType):
        shutil.rmtree("obj-%s" % buildType, True)
    if os.path.exists("lib-%s" % buildType):
        shutil.rmtree("lib-%s" % buildType, True)
    if os.path.exists("bin-%s" % buildType):
        shutil.rmtree("bin-%s" % buildType, True)
    if(os.path.exists("build-%s" % buildType)):
        shutil.rmtree("build-%s" % buildType, True)

    print "rm outfile.root"
    if(os.path.exists("outfile.root")):
        os.remove("outfile.root")

    print "rm main.cc"
    if(os.path.exists("main.cc")):
        os.remove("main.cc")
    print "rm Makefile"
    if(os.path.exists("Makefile")):
        os.remove("Makefile")
    print "rm CMakeLists.txt"
    if(os.path.exists("CMakeLists.txt")):
        os.remove("CMakeLists.txt")
    print "rm analyzers.cmake"
    if(os.path.exists("analyzers.cmake")):
        os.remove("analyzers.cmake")


def cleanFW(args):
    FWPath = getVar("ANALYSISFW_PATH", ".")
    buildType = getVar("SYSTEMINSTALL", "")

    print "cd %s" % FWPath
    os.chdir(FWPath)
    print "rm -rf obj-%s" % buildType
    print "rm -rf lib-%s" % buildType
    print "rm -rf bin-%s" % buildType
    print "rm -rf build-%s" % buildType
    if os.path.exists("obj-%s" % buildType):
        shutil.rmtree("obj-%s" % buildType, True)
    if os.path.exists("lib-%s" % buildType):
        shutil.rmtree("lib-%s" % buildType, True)
    if os.path.exists("bin-%s" % buildType):
        shutil.rmtree("bin-%s" % buildType, True)
    if(os.path.exists("build-%s" % buildType)):
        shutil.rmtree("build-%s" % buildType, True)
    if os.path.exists("ROOTCompare"):
        print "rm ROOTCompare"
        os.remove("ROOTCompare")
    print "rm *.pyc"
    for f in os.listdir("%s" % FWPath):
        if ".pyc" in f:
            os.remove(f)
    print "rm scripts/*.pyc"
    for f in os.listdir("%s/scripts" % FWPath):
        if ".pyc" in f:
            os.remove("scripts/%s" % (f))


def cleanStd(args):
    FWPath = getVar("ANALYSISFW_PATH", ".")
    buildType = getVar("SYSTEMINSTALL", "")

    print "cd %s" % FWPath
    os.chdir(FWPath)
    print "rm -r build-%s/StdConfigs" % buildType
    if(os.path.exists("build-%s/StdConfigs" % buildType)):
        shutil.rmtree("build-%s/StdConfigs" % buildType)


def cleanAll(args):
    cleanUser(args)
    cleanFW(args)


# List available analyzers ( in FW and User directories)
def available(args):
    FWPath = getCheckVar("ANALYSISFW_PATH")
    UserPath = getCheckVar("ANALYSISFW_USERDIR")

    print "FW Analyzers : "
    for dir in listDirClean("%s/Analyzers" % FWPath):
        if os.path.exists("%s/Analyzers/%s/include" % (FWPath, dir)):
            for el in listFileClean("%s/Analyzers/%s/include" % (FWPath, dir)):
                print "\t%s" % el.replace(".hh", "")
    print "Examples Analyzers : "
    for el in listFileClean("%s/Examples/include" % FWPath):
        print "\t%s" % el.replace(".hh", "")
    print "User Analyzers : "
    for el in listFileClean("%s/Analyzers/include" % UserPath):
        print "\t%s" % el.replace(".hh", "")


# Create new analyzer
def generateNewAnalyzer(name, FWPath, UserPath, inputs):

    if name[-3:]=='.py':
        extension = 'py'
    else:
        extension = 'hh'

    if os.path.exists("%s/Analyzers/include/%s.%s" % (UserPath, name, extension)):
        answer = raw_input("This analyzer already exists. Do you want to overwrite it [Y/N] ? ")
        if answer.lower() == 'y':
            os.remove("%s/Analyzers/include/%s.%s" % (UserPath, name, extension))
	    if ext == 'hh':
                os.remove("%s/Analyzers/src/%s.cc" % (UserPath, name))
        else:
            print "Please choose a different name."
            return
    if os.path.exists("%s/Analyzers/include/%s.%s" % (FWPath, name, extension)):
        print "This analyzer already exists. Please choose a different name."
        return
    if os.path.exists("%s/Examples/include/%s.cc" % (FWPath, name)) and extension == 'hh':
        print "This analyzer already exists. Please choose a different name."
        return

    if extension == 'py':
	readAndReplace("%s/Templates/templateAnalyzer.py" % FWPath, "%s/Analyzers/src/%s.py" %
                       (UserPath, name[:-3]), {'templateAnalyzer': name[:-3], 'TEMPLATEANALYZER': name.upper()[:-3]})
    else: 
        readAndReplace("%s/Templates/templateAnalyzer.hh" % FWPath, "%s/Analyzers/include/%s.hh" %
                       (UserPath, name), {'templateAnalyzer': name, 'TEMPLATEANALYZER': name.upper()})
        readAndReplace("%s/Templates/templateAnalyzer.cc" % FWPath, "%s/Analyzers/src/%s.cc" %
                       (UserPath, name), {'templateAnalyzer': name, '/*$$TREEREQUEST$$*/': inputs[0],
                                          '/*$$GETEVENTS$$*/': inputs[1]})


# def createAnalyzer(name, FWPath, UserPath):
def createAnalyzer(args):
    [name] = args.AnalyzerName
    FWPath = getCheckVar("ANALYSISFW_PATH")
    UserPath = getCheckVar("ANALYSISFW_USERDIR")

    [anName, inputs] = parseAnalyzerDef(name)

    if inputs == 0:
        # No analyzer inputs specified. Old style new.
        generateNewAnalyzer(name, FWPath, UserPath, ['', '', ''])
        return

    treeRequest = ""
    getEvents = ""
    for i in inputs:
        i = i.strip()
        if " " not in i:
            print "\033[31;1mError while creating new analyzer %s: input type not specified for %s. Please add 'MC' or 'Reco' identifier before tree name.\033[0m" % (anName, i)
            return
        [type, tree] = i.split(" ")
        if type == "MC":
            className = "T%sEvent" % (tree)
        else:
            className = "TReco%sEvent" % (tree)

        # treeInclude = "%s#include \"%s.hh\"\n" % (treeInclude,className)
        treeRequest = "%s\tRequestTree(\"%s\",new %s);\n" % (treeRequest, tree, className)
        getEvents = "%s\t%s *%sEvent = (%s*)GetEvent(\"%s\");\n" % (getEvents, className, tree, className, tree)

    generateNewAnalyzer(anName, FWPath, UserPath, [treeRequest, getEvents])


# Rename a user analyzer
def renameAnalyzer(args):
    FWPath = getCheckVar("ANALYSISFW_PATH")
    UserPath = getCheckVar("ANALYSISFW_USERDIR")

    [oldName] = args.oldName
    [newName] = args.newName

    if not os.path.exists("%s/Analyzers/include/%s.hh" % (UserPath, oldName)):
        print "\033[31;1mThis analyzer does not exist (%s). It cannot be renamed.\033[0m" % oldName
        return

    if os.path.exists("%s/Analyzers/include/%s.hh" % (UserPath, newName)):
        answer = raw_input("This analyzer (%) already exists. Do you want to overwrite it [Y/N] ? " % newName)
        if answer.lower() == 'y':
            os.remove("%s/Analyzers/include/%s.hh" % (UserPath, newName))
            os.remove("%s/Analyzers/src/%s.cc" % (UserPath, newName))
        else:
            print "Please choose a different name."
            return
    if os.path.exists("%s/Analyzers/include/%s.hh" % (FWPath, newName)):
        print "This analyzer already exists. Please choose a different name."
        return
    if os.path.exists("%s/Examples/include/%s.hh" % (FWPath, newName)):
        print "This analyzer already exists. Please choose a different name."
        return

    readAndReplace("%s/Analyzers/include/%s.hh" % (UserPath, oldName), "%s/Analyzers/include/%s.hh" % (UserPath, newName), {oldName: newName, oldName.upper(): newName.upper()}, skipComments=False)
    readAndReplace("%s/Analyzers/src/%s.cc" % (UserPath, oldName), "%s/Analyzers/src/%s.cc" % (UserPath, newName), {oldName: newName}, skipComments=False)

    os.remove("%s/Analyzers/include/%s.hh" % (UserPath, oldName))
    os.remove("%s/Analyzers/src/%s.cc" % (UserPath, oldName))


# Clone a user analyzer
def cloneAnalyzer(args):
    FWPath = getCheckVar("ANALYSISFW_PATH")
    UserPath = getCheckVar("ANALYSISFW_USERDIR")

    [origin] = args.origin
    [dest] = args.dest

    if not os.path.exists("%s/Analyzers/include/%s.hh" % (UserPath, origin)):
        print "\033[31;1mThis analyzer does not exist (%s). It cannot be cloned.\033[0m" % origin
        return

    if os.path.exists("%s/Analyzers/include/%s.hh" % (UserPath, dest)):
        answer = raw_input("This analyzer (%) already exists. Do you want to overwrite it [Y/N] ? " % dest)
        if answer.lower() == 'y':
            os.remove("%s/Analyzers/include/%s.hh" % (UserPath, dest))
            os.remove("%s/Analyzers/src/%s.cc" % (UserPath, dest))
        else:
            print "Please choose a different name."
            return
    if os.path.exists("%s/Analyzers/include/%s.hh" % (FWPath, dest)):
        print "This analyzer already exists. Please choose a different name."
        return
    if os.path.exists("%s/Examples/include/%s.hh" % (FWPath, dest)):
        print "This analyzer already exists. Please choose a different name."
        return

    readAndReplace("%s/Analyzers/include/%s.hh" % (UserPath, origin), "%s/Analyzers/include/%s.hh" % (UserPath, dest), {origin: dest, origin.upper(): dest.upper()}, skipComments=False)
    readAndReplace("%s/Analyzers/src/%s.cc" % (UserPath, origin), "%s/Analyzers/src/%s.cc" % (UserPath, dest), {origin: dest}, skipComments=False)


def readBuildConfigFile(filename):

    print("filename: %s\n" % filename)

    if not os.path.exists(filename):
        print "The config file %s does not exist" % filename
        return None

    cp = SimpleConfigParser.SimpleConfigParser()
    if is_binary(filename):
        print "\033[31;1mError reading the configuration file. It seems to be a binary file.\033[0m"
        return None
    cp.read(filename)

    class ConfigFileParsed:
        analyzersList = None
        preAnalyzersList = None
        executable = None
        extralibs = []
        extralibsdirs = []
        extraincludedirs = []
        noDetCheckBadBurst = []
        noDetCheckBadEvent = []
        parameters = None

    configFileParsed = ConfigFileParsed()

    # parse options
    # Start with reading analyzer list and executable name. They are mandatory
    noAnalyzer = False
    noExecutable = False
    if not cp.hasoption('analyzers'):
        noAnalyzer = True
    else:
        configFileParsed.analyzersList = cp.getoption("analyzers")
    if cp.hasoption('preanalyzers'):
        configFileParsed.preAnalyzersList = cp.getoption("preanalyzers")
    else:
        configFileParsed.preAnalyzersList = ""

    if not cp.hasoption('exec'):
        noExecutable = True
    else:
        configFileParsed.executable = cp.getoption("exec")

    if len(configFileParsed.analyzersList) == 0:
        noAnalyzer = True
    if len(configFileParsed.executable) == 0:
        noExecutable = True

    if noAnalyzer:
        print "\033[31;1mNo analyzers found in config file\033[0m"
    if noExecutable:
        print "\033[31;1mNo executable name in config file\033[0m"

    if noAnalyzer or noExecutable:
        return None

    # Continue with checking the additional compiling info
    if cp.hasoption("libs"):
        configFileParsed.extralibs = cp.getoption("libs").split()

    if cp.hasoption("libsdirs"):
        configFileParsed.extralibsdirs = cp.getoption("libsdirs").split()

    if cp.hasoption("includedirs"):
        configFileParsed.extraincludedirs = cp.getoption("includedirs").split()

    # Check hard-coded parameters
    if cp.hasoption("parameters"):
        configFileParsed.parameters = cp.getoption("parameters").strip()

    # Check event and burst error checking
    if cp.hasoption("SystemsToIgnore") or cp.hasoption("SystemsToCheck"):
        print "\033[31;1mError reading the configuration file. SystemsToCheck and SystemsToIgnore are no longer valid identifier.\033[0m"
        print "\033[31;1mThey have been replaced by the separate BadBurstSystemsTo... and BadEventSystemsTo... identifiers.\033[0m"
        print "\033[31;1mPlease update your config file.\033[0m"
        return None

    noDetCheckBadBurst = []
    detCheckBadBurst = []
    if cp.hasoption("BadBurstSystemsToIgnore"):
        noDetCheckBadBurst = cp.getoption("BadBurstSystemsToIgnore").split()
    if cp.hasoption("BadBurstSystemsToCheck"):
        detCheckBadBurst = cp.getoption("BadBurstSystemsToCheck").split()

    noDetCheckBadEvent = []
    detCheckBadEvent = []
    if cp.hasoption("BadEventSystemsToIgnore"):
        noDetCheckBadEvent = cp.getoption("BadEventSystemsToIgnore").split()
    if cp.hasoption("BadEventSystemsToCheck"):
        detCheckBadEvent = cp.getoption("BadEventSystemsToCheck").split()

    # They are mutually exclusive two by two. So verify
    if len(noDetCheckBadBurst) > 0 and len(detCheckBadBurst) > 0:
        print "\033[31;1mError reading the configuration file. BadBurstSystemsToCheck and BadBurstSystemsToIgnore cannot be used together.\033[0m"
        return None
    if len(noDetCheckBadEvent) > 0 and len(detCheckBadEvent) > 0:
        print "\033[31;1mError reading the configuration file. BadEventSystemsToCheck and BadEventSystemsToIgnore cannot be used together.\033[0m"
        return None

    # All is a special case and will be dealt with later
    # In other cases, we always want a list of nochecks. So if check was specified, just take the whole list
    # of possible values to "not check" and remove those for which we asked to check
    if len(detCheckBadBurst) > 0 and "all" not in detCheckBadBurst:
        noDetCheckBadBurst = set(["Cedar", "CHANTI", "CHOD", "DIM", "GTK", "HAC", "IRC", "L0Calo", "L0CHOD", "L0LAV", "L0MUV3", "L0NewCHOD", "L0RICH", "L0TALK", "L0TP", "L1TP", "L2EB", "LAV", "LKr", "MUV0", "MUV1", "MUV2", "MUV3", "NewCHOD", "Processing", "RICH", "SAC", "SAV", "Straw"])
        noDetCheckBadBurst = noDetCheckBadBurst - set(detCheckBadBurst)

    if len(detCheckBadEvent) > 0 and "all" not in detCheckBadEvent:
        noDetCheckBadEvent = set(["Cedar", "CHANTI", "CHOD", "GTK", "HAC", "IRC", "L0", "L1", "LAV", "LKr", "MUV0", "MUV1", "MUV2", "MUV3", "NewCHOD", "RICH", "SAC", "SAV", "Straw"])
        noDetCheckBadEvent = noDetCheckBadEvent - set(detCheckBadEvent)

    configFileParsed.noDetCheckBadBurst = noDetCheckBadBurst
    configFileParsed.noDetCheckBadEvent = noDetCheckBadEvent

    return configFileParsed


def buildOrderedListOfAnalyzers(analyzersString, fwPath, userPath, useStrictOrder, alreadyOrderedPreAnalyzers):
    analyzersList = [x.strip() for x in re.findall(r" ?(.+?(?:\(.+?\)|[ ]|$)) ?", analyzersString)]
    analyzersList.reverse()
    orderedList = []
    [missing, wrongList, depsGraph, usrAnList] = createAnalyzerGraph(analyzersList, fwPath, userPath, alreadyOrderedPreAnalyzers)

    if missing or wrongList:
        return None, None, None, None

    if not useStrictOrder:
        # Use dependency graph to solve the order and dependencies between analyzers
        p = depsGraph.getNextPath()
        while len(p) > 0:
            if p[0] == -1:
                exit(0)
            for node in p:
                orderedList.append(node)
            p = depsGraph.getNextPath()
    else:
        # Just do it in the order the user specified
        print """\033[33;1mWARNING: Using strict analyzer order. Dependencies between analyzers are not automatically resolved...
Re-run without --strict-order if you want dependencies to be automatically solved.\033[0m"""
        orderedList = analyzersList[:]

    return orderedList, usrAnList


# Build the framework against the provided config file
def build(args):
    [filename] = args.configFileName
    FWPath = getCheckVar("ANALYSISFW_PATH")
    UserPath = getCheckVar("ANALYSISFW_USERDIR")
    generateCode(filename, FWPath, UserPath, args.strict_order)

    # Check if FW needs to be recompiled
    buildErr = True
    if not args.build_user_only:
        buildErr = buildFW(FWPath, args.defines, args.jobs, args.core_only)
    if not buildErr or args.build_user_only:
        buildUser(args.defines, args.jobs)


def buildStdAnalyzers(args):
    sys.stdout = open(os.devnull, 'w')
    [filename] = args.configFileName
    FWPath = getCheckVar("ANALYSISFW_PATH")

    generateStandardMain(filename, FWPath)


def generateCode(configFile, FWPath, UserPath, strictOrder):
    configFileParsed = readBuildConfigFile(configFile)
    # There was an error while parsing the config file
    if configFileParsed is None:
        return

    orderedPreAnalyzers, usrPreAnList = buildOrderedListOfAnalyzers(configFileParsed.preAnalyzersList, FWPath, UserPath, strictOrder, None)
    orderedAnalyzers, usrAnList = buildOrderedListOfAnalyzers(configFileParsed.analyzersList, FWPath, UserPath, strictOrder, orderedPreAnalyzers)

    if any([orderedAnalyzers is None, usrAnList is None]):
        sys.exit()

    usrAnMergedList = usrAnList + usrPreAnList

    generateCMake(FWPath, UserPath, orderedPreAnalyzers + orderedAnalyzers, usrAnMergedList, configFileParsed.executable,
                  configFileParsed.extralibs, configFileParsed.extralibsdirs, configFileParsed.extraincludedirs)
    generateMain(FWPath, UserPath, orderedPreAnalyzers, orderedAnalyzers, configFileParsed.extralibs, configFileParsed.extralibsdirs,
                 configFileParsed.extraincludedirs, configFileParsed.noDetCheckBadBurst, configFileParsed.noDetCheckBadEvent,
                 configFileParsed.parameters, "main")


def generateStandardMain(configFile, FWPath):
    configFileParsed = readBuildConfigFile(configFile)
    # There was an error while parsing the config file
    if configFileParsed is None:
        return
    system = getCheckVar("SYSTEMINSTALL")

    orderedPreAnalyzers, _ = buildOrderedListOfAnalyzers(configFileParsed.preAnalyzersList, FWPath, FWPath, False, None)
    orderedAnalyzers, _ = buildOrderedListOfAnalyzers(configFileParsed.analyzersList, FWPath, FWPath, False, orderedPreAnalyzers)

    generateMain(FWPath, "%s/build-%s/StdConfigs/" % (FWPath, system), orderedPreAnalyzers, orderedAnalyzers,
                 configFileParsed.extralibs, configFileParsed.extralibsdirs, configFileParsed.extraincludedirs,
                 configFileParsed.noDetCheckBadBurst, configFileParsed.noDetCheckBadEvent, configFileParsed.parameters,
                 "main_%s" % os.path.basename(configFile))

    # with open("%s/build-%s/StdConfigs/depends_%s" % (FWPath, system, os.path.basename(configFile)), "w") as fd:
    #     fd.write("SET(dependencies_%s %s)" % (os.path.basename(configFile), ";".join(fwAnMergedList)))
    # print "Done"


def generateCMake(FWPath, UserPath, analyzersList, usrAnMergedList, executable, extralibs, extralibsdirs, extraincludedirs):
    # Make string for extrac compilation options
    strExtralibs_CMake = ""
    for lib in extralibs:
        strExtralibs_CMake += "%s " % (lib)

    strExtralibsdirs_CMake = ""
    for dir in extralibsdirs:
        strExtralibsdirs_CMake += "%s " % (dir)

    strExtraIncdirs_CMake = ""
    for dir in extraincludedirs:
        strExtraIncdirs_CMake += "%s " % (dir)

    # Create cmake file
    makefileDict = {"$$FWPath$$": FWPath, "$$USERCODEPATH$$": UserPath, "$$ANALYZERSHH$$": " ".join(analyzersList), "$$USRHH$$": usrAnMergedList,
                    "$$EXEC$$": executable, "$$EXTRALIBS$$": strExtralibs_CMake,
                    "$$EXTRALIBSDIRS$$": strExtralibsdirs_CMake, "$$EXTRAINCLUDEDIRS$$": strExtraIncdirs_CMake}
    readAndReplace("%s/Templates/analyzers.cmake" % FWPath, "%s/analyzers.cmake" % UserPath, makefileDict)


def generateMain(FWPath, UserPath, orderedPreAnalyzers, orderedAnalyzers, extralibs, extralibsdirs,
                 extraincludedirs, noDetCheckBadBurst, noDetCheckBadEvent, hcParameters, mainName):
    # Make string for extrac compilation options
    strExtralibs = ""
    for lib in extralibs:
        strExtralibs += "-l%s " % (lib)

    strExtralibsdirs = ""
    for dir in extralibsdirs:
        strExtralibsdirs += "-L%s " % (dir)

    strExtraIncdirs = ""
    for dir in extraincludedirs:
        strExtraIncdirs += "-I%s " % (dir)

    # Create main.cc file
    # First create and add the analyzers
    includesList = ""
    instancesAnalyzer = ""
    helpAnalyzer = ""
    deleteAnalyzer = ""
    for an in orderedPreAnalyzers:
        includesList += """#include "%s.hh"\n""" % an
        instancesAnalyzer += "\t%s *an_%s = new %s(ban);\n" % (an, an, an)
        instancesAnalyzer += "\tban->AddAnalyzer(an_%s);\n" % an
        helpAnalyzer += "\t%s *an_%s = new %s(nullptr);\n" % (an, an, an)
        helpAnalyzer += "\tan_%s->PrintParameters(true);\n" % an
        deleteAnalyzer += "\tdelete an_%s;\n" % an

    # instancesAnalyzer += "\tban->StartAnalyzers();\n"
    for an in orderedAnalyzers:
        includesList += """#include "%s.hh"\n""" % an
        instancesAnalyzer += "\t%s *an_%s = new %s(ban);\n" % (an, an, an)
        instancesAnalyzer += "\tban->AddAnalyzer(an_%s);\n" % an
        helpAnalyzer += "\t%s *an_%s = new %s(nullptr);\n" % (an, an, an)
        helpAnalyzer += "\tan_%s->PrintParameters(true);\n" % an
        deleteAnalyzer += "\tdelete an_%s;\n" % an

    # Then set the flags for the no check events/bad bursts
    noDetStringBadBurst = ""
    for det in noDetCheckBadBurst:
        if det == "all":
            noDetStringBadBurst += "\tConfigSettings::CLI::fNoSkipBadBurst = true;\n"
        else:
            noDetStringBadBurst += "\tConfigSettings::CLI::fNoBadBurstSystems.insert(\"%s\");\n" % (det)

    noDetStringBadEvent = ""
    for det in noDetCheckBadEvent:
        if det == "all":
            noDetStringBadEvent += "\tConfigSettings::CLI::fNoCheckEvents = true;\n"
        else:
            noDetStringBadEvent += "\tConfigSettings::CLI::fNoCheckSystems.insert(\"%s\");\n" % (det)

    hcParametersString = ""
    if hcParameters is not None and len(hcParameters) > 0:
        hcParametersString = "\tConfigSettings::CLI::fParameters += \"&{0}\";".format(hcParameters)
    # Write the file
    readAndReplace("%s/Templates/main.cc" % FWPath,
                   "%s/%s.cc" % (UserPath, mainName),
                   {"$$ANALYZERSINCLUDE$$": includesList,
                    "/*$$ANALYZERSNEW$$*/": instancesAnalyzer,
                    "/*$$ANALYZERSHELP$$*/": helpAnalyzer,
                    "/*$$ANALYZERSDELETE$$*/": deleteAnalyzer,
                    "/*$$FORCENOBADBURSTDETECTORS$$*/": noDetStringBadBurst,
                    "/*$$FORCENOCHECKDETECTORS$$*/": noDetStringBadEvent,
                    "/*$$FORCEPARAMETERS$$*/": hcParametersString})


# Prepare user folder
def prepareUserFolder(args):
    global __rev__

    print("\n\n!!here!!\n\n")

    [path] = args.UserDirectory
    path = path.rstrip("/")
    path = os.path.abspath(path)
    FWPath = getVar("ANALYSISFW_PATH", os.path.dirname(os.path.realpath(__file__)) + "/../")
    NA62RECOSOURCE = getCheckVar("NA62RECOSOURCE")

    getFWWorkspaceVersion(FWPath)

    if not os.path.exists(path):
        os.makedirs(path)
    else:
        if len(os.listdir(path)) > 0:
            print (
                """The destination path is not empty. If you continue, the folder structure will be checked and eventually updated.
                The env.(c)sh and config files will be regenerated."""
            )
            answer = raw_input("Are you sure you want to continue [Y/N]?")
            if answer.lower() != "y":
                return

    if not args.build_user_only:
        buildFW(FWPath, args.defines, args.jobs, args.core_only)

    if not os.path.exists("%s/Analyzers" % path):
        os.mkdir("%s/Analyzers" % path)
    if not os.path.exists("%s/Analyzers/include" % path):
        os.mkdir("%s/Analyzers/include" % path)
    if not os.path.exists("%s/Analyzers/src" % path):
        os.mkdir("%s/Analyzers/src" % path)
    if not os.path.exists("%s/PhysicsObjects" % path):
        os.mkdir("%s/PhysicsObjects" % path)
    if not os.path.exists("%s/PhysicsObjects/include" % path):
        os.mkdir("%s/PhysicsObjects/include" % path)
    if not os.path.exists("%s/PhysicsObjects/src" % path):
        os.mkdir("%s/PhysicsObjects/src" % path)
    if not os.path.exists("%s/Algorithms" % path):
        os.mkdir("%s/Algorithms" % path)
    if not os.path.exists("%s/Algorithms/include" % path):
        os.mkdir("%s/Algorithms/include" % path)
    if not os.path.exists("%s/Algorithms/src" % path):
        os.mkdir("%s/Algorithms/src" % path)
    if not os.path.exists("%s/obj" % path):
        os.mkdir("%s/obj" % path)
    if not os.path.exists("%s/lib" % path):
        os.mkdir("%s/lib" % path)
    if not os.path.exists("%s/scripts" % path):
        os.mkdir("%s/scripts" % path)

    readAndReplace("%s/Templates/env.sh" % FWPath, "%s/scripts/env.sh" % path, {"$$ANALYSISFW$$": FWPath, "$$USERDIR$$": path, "$$NA62RECOSOURCE$$": NA62RECOSOURCE})
    readAndReplace("%s/Templates/config" % FWPath, "%s/config" % path, {})
    shutil.copyfile("%s/Templates/CMakeLists.txt" % FWPath, "%s/CMakeLists.txt" % path)
    shutil.copyfile("%s/Templates/CMakeLists_PO.txt" % FWPath, "%s/PhysicsObjects/CMakeLists.txt" % path)
    shutil.copyfile("%s/Templates/CMakeLists_Algos.txt" % FWPath, "%s/Algorithms/CMakeLists.txt" % path)

    version = getUserVersion(path)
    updateSettings(path, FWPath, version, __rev__)
    writeUserVersion(path)

    print "\nYour new user directory has been created. \nTo continue, go in %s, edit your config file, verify and source scripts/env.sh, and run \nNA62AnalysisBuilder.py config" % path


# Build example analyzers and copy config files in user dir
def buildExample(args):
    FWPath = getCheckVar("ANALYSISFW_PATH")
    UserPath = getCheckVar("ANALYSISFW_USERDIR")

    shutil.copyfile("%s/Examples/examplePi0Config" % FWPath, "%s/examplePi0Config" % UserPath)
    shutil.copyfile("%s/Examples/exampleSkimmingConfig" % FWPath, "%s/exampleSkimmingConfig" % UserPath)
    readAndReplace("%s/Examples/exampleExportTreesConfig" % FWPath, "%s/exampleExportTreesConfig" % UserPath, {"$$FWPATH$$": FWPath})


def printHelp(args):
    args.parserRef.print_help()


# Command line argument parser
def parseArgs():
    global __rev__, __descr__

    program_version_message = "rev %s." % __rev__
    program_short_description = __descr__

    # Prepend the build command if the first positional argument is a file path (for compatibility with previous syntax)
    # Plus lowercase the first positional argument (supposed to be the command then)
    if len(sys.argv) >= 1:
        try:
            index, positional = next((i, x) for (i, x) in enumerate(sys.argv[1:]) if not x.startswith('-'))
        except StopIteration:
            pass
        else:
            if os.path.exists(positional):
                if os.path.isfile(positional):
                    sys.argv.insert(1, 'build')
                    index = 0
            sys.argv[index + 1] = sys.argv[index + 1].lower()

    # Setup argument parser
    common_flags = ArgumentParser(add_help=False)
    common_flags.add_argument('-j', '--jobs', default=1, type=int, help="Number of processes to use for building (same as make -j)")
    common_flags.add_argument('--strict-order', default=False, action="store_true", help="""Use the analyzer in the same order as defined
                                in the configuration file (beware of dependencies between analyzers)""")
    common_flags.add_argument('--build-user-only', dest="build_user_only", default=False, action="store_true", help="Compiles only the user directory. Does not check the FW state")
    common_flags.add_argument('--core-only', dest="core_only", default=False, action="store_true", help="In NA62Analysis, compile only the core necessary to run user analyzes. Do not build FW analyzers.")
    clean_group = common_flags.add_argument_group(title="Build options",
                                                  description=("The following options require a cleanAll to take effect\n"
                                                               "if the framework was already compiled without the option"))
    clean_group.add_argument('-d', '--no-debug', action="store_true", default=False,
                             dest="nodebug", help="Compile the framework and user directories without debugging informations (release mode)")
    clean_group.add_argument('--no-c++11', action="store_false", default=True,
                             dest="c11", help="""Compile the framework and user directories without c++11 support
                            (automatically disabled if compiler does not support c++11)""")
    clean_group.add_argument('--full-warning', action="append_const", const="FULL_WARNING",
                             dest="defines", help="Compile the framework and user directories with all the warning flags")
    clean_group.add_argument('--shared', action="append_const", const="SHARED_LIB",
                             dest="defines", help="Use shared libraries rather than static libraries")
    clean_group.add_argument('--grid', action="append_const", const="NA62_NOGEANT4",
                             dest="defines", help="Do not link Geant4.")
    clean_group.add_argument('--build-std', action="append_const", const="COMPILE_STD_AN",
                             dest="defines", help="Build standard analyzers.")
    clean_group.add_argument('--old-specialtrigger', action="append_const", const="OLD_SPECIALTRIGGER",
                             dest="defines", help="Build the framework for old SpecialTrigger persistency")

    parser = ArgumentParser(description=program_short_description, formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('-V', '--version', action='version', version=program_version_message)
    subparsers = parser.add_subparsers(metavar="commandName", title="Available commands",
                                       description="'NA62AnalysisBulder.py commandName -h' for help about specific command")

    ''' Help command'''
    parser_help = subparsers.add_parser('help', help='Show this help message and exit', description='Show this help message and exit')
    parser_help.set_defaults(func=printHelp, parserRef=parser)

    ''' Build command'''
    parser_build = subparsers.add_parser('build', help='Build the FW using the configuration file', parents=[common_flags],
                                         description="Build the FW using the configuration file configFileName")
    parser_build.set_defaults(func=build)
    parser_build.add_argument('configFileName', type=str, nargs=1, help="Path to the config file")

    ''' Build standard command'''
    parser_build = subparsers.add_parser('build-std', help='Build the FW standard analyzer using the configuration file', parents=[common_flags],
                                         description="Build the FW standard analyzer using the configuration file configFileName")
    parser_build.set_defaults(func=buildStdAnalyzers)
    parser_build.add_argument('configFileName', type=str, nargs=1, help="Path to the config file")

    ''' Rename command'''
    parser_rename = subparsers.add_parser('rename', help='Rename a user analyzer',
                                          description="Rename a user analyzer")
    parser_rename.set_defaults(func=renameAnalyzer)
    parser_rename.add_argument('oldName', type=str, nargs=1, help="Old name of the analyzer")
    parser_rename.add_argument('newName', type=str, nargs=1, help="New name of the analyzer")

    ''' Clone command'''
    parser_rename = subparsers.add_parser('clone', help='Clone a user analyzer with a new name',
                                          description="Clone a user analyzer with a new name")
    parser_rename.set_defaults(func=cloneAnalyzer)
    parser_rename.add_argument('origin', type=str, nargs=1, help="Origin analyzer")
    parser_rename.add_argument('dest', type=str, nargs=1, help="Destination analyzer")

    ''' new command'''
    parser_new = subparsers.add_parser('new', help='Create a new analyzer in the user directory',
                                       description="""
   Create a new analyzer with name AnalyzerName in the user directory
   Alternatively AnalyzerName can be replaced by the following syntax:
      AnalyzerName(InputType TreeName, ...)
   This will already include the correct persistency headers, request the input tree and
   retrieve the input events.
   Example: Creating an analyzer named toto using as input the GigaTracker MonteCarlo and
   LKr and Spectrometer RECO
      NA62AnalysisBuilder.py new "toto(MC GigaTracker, Reco LKr, Reco Spectrometer)""",
                                       formatter_class=RawDescriptionHelpFormatter)
    parser_new.set_defaults(func=createAnalyzer)
    parser_new.add_argument('AnalyzerName', type=str, nargs=1, help="Name of the analyzer to create")

    ''' prepare command'''

    parser_prepare = subparsers.add_parser('prepare', help='Prepare a new user directory at the specified path', parents=[common_flags],
                                           description="Prepare a user directory at the specified path")
    parser_prepare.set_defaults(func=prepareUserFolder)
    parser_prepare.add_argument('UserDirectory', type=str, nargs=1, help="Path to the user directory")

    ''' cleanUser command'''
    parser_cleanUser = subparsers.add_parser('cleanuser', help='Remove all files generated by the build in the user directory',
                                             description="Remove all files generated by the build in the user directory")
    parser_cleanUser.set_defaults(func=cleanUser)

    ''' cleanFW command'''
    parser_cleanFW = subparsers.add_parser('cleanfw', help='Remove all files generated by the build in the FW directory',
                                           description="Remove all files generated by the build in the FW directory")
    parser_cleanFW.set_defaults(func=cleanFW)

    ''' cleanStd command'''
    parser_cleanStd = subparsers.add_parser('cleanstd', help='Remove all build files related to the Standard analyzers',
                                            description="Remove all build files related to the Standard analyzers")
    parser_cleanStd.set_defaults(func=cleanStd)

    ''' cleanAll command'''
    parser_cleanAll = subparsers.add_parser('cleanall', help='Remove all files generated by the build in the User and FW directory',
                                            description="Remove all files generated by the build in the FW directory and user directory")
    parser_cleanAll.set_defaults(func=cleanAll)

    ''' available command'''
    parser_available = subparsers.add_parser('available', help='Display the list of available analyzers',
                                             description="Print the list of available Analyzers")
    parser_available.set_defaults(func=available)

    ''' examples command'''
    parser_examples = subparsers.add_parser('examples', help='Build the libraries for the examples and copy the config files into the user directory',
                                            description="Build the libraries for examples and import the configuration files in user directory")
    parser_examples.set_defaults(func=buildExample)

    # Process arguments
    args, unknown = parser.parse_known_args()

    # NARKD-1069
    # if "c11" in args and args.c11:
    #    if args.defines is None:
    #        args.defines = []
    #    args.defines.append("C++11_COMPAT")

    if "nodebug" in args and not args.nodebug:
        if args.defines is None:
            args.defines = []
        args.defines.append("NA62_DEBUG")

    args.func(args)


def main():

    print("\n\n\n\nYOU ARE RUNNING THE ALTERED SCRIPT NOT THE OFFICIAL ONE\n\n\n\n\n")

    if False and getVar("ANALYSISFW_USERDIR", -1) != -1:
	print("\n\n this: %s \n\n" % getVar("ANALYSISFW_USERDIR", -1))
        checkUpdate()

    parseArgs()
    sys.exit(0)


if __name__ == '__main__':
    main()
