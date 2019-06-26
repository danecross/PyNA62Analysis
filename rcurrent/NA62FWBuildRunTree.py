#!/usr/bin/env python

from argparse import RawTextHelpFormatter
import sys
import argparse
import os
import fnmatch
import shutil

# # Setup options to the code
descText = """Install local files required to use the CVMFS or AFS copies of the NA62 code"""
exampleText = """
Examples of use:

Normal use:
/cvmfs/na62.cern.ch/offline/NA62FW/prod/v0.9.0/NA62FWBuildRunTree.py -v v0.9.0 -w ~/NA62/v0.9.0

List all available versions on CVMFS and AFS:
NA62FWBuildRunTree.py -l

Using a copy from an non-standard location:
/localCopy/na62fw/NA62FWBuildRunTree.py --repository /localCopy/na62fw -w ~/na62LocalCopy
"""
parser = argparse.ArgumentParser(description=descText, epilog=exampleText,
                                 formatter_class=RawTextHelpFormatter)
parser.add_argument("-l", "--list",
                    help="List available version of the code",
                    action="store_true")
parser.add_argument("-v", "--version",
                    help="Version of code to setup in local directory")
parser.add_argument("-w", "--workdir",
                    help="Directory to use for local files, default is ~/NA62FWr123 for version r123")
parser.add_argument("--verbose",
                    help="Give versbose output from this script",
                    action="store_true")
parser.add_argument("--repository",
                    help="Use an explicit location for the code")
args = parser.parse_args()

usePath = args.repository
if(args.repository is not None and args.version):
    print "You can either set the searchPath or the version but not both"

# # list of locations to look for versions in order
# # 1st element in the name of the location, second the path to it
# # expect that some of the paths are not valid
locations = [("Dane's Home Directory", "/afs/cern.ch/user/d/dacross/SummerProject"),
	     ("Amanda's Home Directory", "/afs/cern.ch/user/a/ahoebel/SummerProject"),
	     ("CVMFS production", "/cvmfs/na62.cern.ch/offline/NA62FW/prod"),
             ("CVMFS development", "/cvmfs/na62.cern.ch/offline/NA62FW/dev"),
             ("EOS production", "/eos/experiment/na62/data/offline/NA62FW/prod"),
             ("EOS development", "/eos/experiment/na62/data/offline/NA62FW/dev"),
             ("AFS production", "/afs/cern.ch/na62/offline/NA62FW/prod"),
             ("AFS development", "/afs/cern.ch/na62/offline/NA62FW/dev")]

########## no version defined, list and quit
if(args.version is None and args.repository is None and not args.list):
    parser.print_help()
    args.list = True

########## list the available versions of the code
if(args.list):
    # directories may not be available...
    for l in locations:
        try:
            versions = os.listdir(l[1])
        except OSError as e:
            versions = None
        if(versions is not None):
            print "\n{0} versions available on {1}:".format(l[0], l[1])
            for v in versions:
                print v
    sys.exit(0)

######### try and find the named version
if(usePath is None):
    for l in locations:
        try:
            usePath = os.path.join(l[1], args.version)
            if(True):
                print "Trying {0}".format(usePath)
            os.listdir(usePath)
        except OSError as e:
            if(args.verbose):
                print "Failed to access {0}".format(usePath)
            usePath = None
        if(usePath is not None):
            break

if(usePath is None):  # OK so that version is not available anywhere
    print "\nVersion {0} is not available, try -l to list available versions".format(args.version)
    sys.exit(0)

if(True):
    print "Using {0}".format(usePath)

# # set variables
NA62MCDir = os.path.join(usePath, "NA62MC")
NA62RecoDir = os.path.join(usePath, "NA62Reconstruction")
NA62AnalysisDir = os.path.join(usePath, "NA62Analysis")

########## no workdir defined make a workdir name automatically
if(args.workdir is None):
    args.workdir = os.join(os.getenv("HOME"), "NA62FW" + args.version)
else:
    args.workdir = os.path.abspath(args.workdir)

if(args.verbose):
    print "Setup NA62 softare in Workdir: {0}".format(args.workdir)


def cdAndMake(localDir):
    """Go to local directory, making it if required"""
    try:
        os.chdir(localDir)
    except OSError:
        if(args.verbose):
            print "Need to make directory {0}".format(localDir)
        os.makedirs(localDir, mode=0755)  # if not make it now
        os.chdir(localDir)


# ## make the required directories for the files
cdAndMake(args.workdir)
cdAndMake("NA62MC")
os.chdir(args.workdir)
cdAndMake("NA62Reconstruction")
os.chdir(args.workdir)

# ## make the setup script
envScript = """#!/bin/sh
export NA62MCSOURCE={0}
export NA62RECOSOURCE={1}
olddir=`pwd`
cd $NA62RECOSOURCE
source $NA62RECOSOURCE/scripts/env.sh
cd $olddir
export ANALYSISFW_PATH={2}
export PATH=$ANALYSISFW_PATH:$PATH
""".format(NA62MCDir, NA62RecoDir, NA62AnalysisDir)
versionName = os.path.basename(args.workdir)
envNameOut = "env{0}.sh".format(versionName)
envOut = open(envNameOut, "w")
envOut.write(envScript)
envOut.close()

######## copy files, whole directories and make symbolic links to local area
toCopyDirs = [os.path.join(NA62MCDir, "config"),
              os.path.join(NA62MCDir, "scripts"),
              os.path.join(NA62MCDir, "macros"),
              os.path.join(NA62RecoDir, "config"),
              os.path.join(NA62RecoDir, "scripts")]
toCopyFiles = [os.path.join(NA62MCDir, "LKr*txt"),
               os.path.join(NA62MCDir, "LAV*txt"),
               os.path.join(NA62RecoDir, "Spectrometer/*.txt"),
               os.path.join(NA62RecoDir, "LKr/*.txt")]
toCopysymbolicLinks = [os.path.join(NA62MCDir, "bin/NA62MC"),
                       os.path.join(NA62RecoDir, "bin/NA62Reco"),
                       os.path.join(NA62RecoDir, "bin/NA62EventDisplay"),
                       os.path.join(NA62RecoDir, "NA62.root")]


# ## functions to do the copying
def copyFiles(remoteFile, localDir):
    """Copy remote file to the local directory"""
    cdAndMake(localDir)
    toFile = os.path.join(localDir, os.path.basename(remoteFile))
    if(args.verbose):
        print "Copying from {0} to {1}".format(remoteFile, toFile)
    shutil.copy2(remoteFile, toFile)


def copyFilesPattern(remotePattern, localDir):
    """Copy files matching a pattern to the local directory"""
    remoteDir = os.path.dirname(remotePattern)
    filePattern = os.path.basename(remotePattern)
    for f in os.listdir(remoteDir):
        if fnmatch.fnmatch(f, filePattern):
            fromFile = os.path.join(remoteDir, f)
            copyFiles(fromFile, localDir)


def copyDirs(remoteDir, localDir, exclude=[".svn", ".git"]):
    """Copy directorys and optionally exclude parts"""
    for root, dirs, files in os.walk(remoteDir):
        for excl in exclude:
            if (excl in dirs):  # check if directory matches exlcude pattern
                dirs.remove(excl)  # kill directory in walk
        for f in files:
            # note root is remoteDir/dirA/dirB/ so strip remoteDir/ from it
            # and append to localDir for toDir
            toDir = os.path.join(localDir, root[len(remoteDir) + 1:])
            copyFiles(os.path.join(root, f), toDir)


def makeSymbolicLink(remoteFile, localDir):
    cdAndMake(localDir)
    if(args.verbose):
        print "Sym linking from {0} to {1} in {2}".format(remoteFile, os.path.basename(remoteFile), localDir)
    try:
        os.symlink(remoteFile, os.path.basename(remoteFile))
    except OSError:
        pass  # assume link already existed


os.chdir(usePath)

# bulk copy directories
for cd in toCopyDirs:
    # note cd is remoteDir/dirA/dirB/ so strip remoteDir/ from it
    toDir = os.path.join(args.workdir, cd[len(usePath) + 1:])
    copyDirs(cd, toDir)

# copy files matching a pattern
for cf in toCopyFiles:
    remoteDir = os.path.dirname(cf)
    # strip local root dir
    toDir = os.path.join(args.workdir, remoteDir[len(usePath) + 1:])
    copyFilesPattern(cf, toDir)

# make sym links (not to be done any longer. Use proper PATH instead)
# for sym in toCopysymbolicLinks:
    # remoteDir = os.path.dirname(sym)
    # strip local root dir
    # toDir = os.path.join(args.workdir,remoteDir[len(usePath)+1:])
    # if toDir.split("/")[-1]=="bin":
    #    toDir = "/".join(toDir.split("/")[:-1])
    # makeSymbolicLink(sym,toDir)

print """Source the {0} script in the {1} directory.
NA62MC and NA62AnalysisBuilder.py will be added to your PATH and NA62Reco can be run from the NA62Reconstruction directory""".format(envNameOut, args.workdir)
