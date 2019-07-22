# A script to convert NA62MC geometry into the root format.
# This script creates the files NA62.gdml and NA62.root.
# The geometry can be then visualized as follows:
# root -l -x macros/CheckGeom.C

rm -f NA62.gdml NA62.root
NA62MC macros/GenerateGDML.mac 6610
root -l -q macros/GDML2Root.cc
