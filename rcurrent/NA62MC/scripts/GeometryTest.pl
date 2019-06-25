#!/bin/perl
#
# Check for NA62MC geometry conflicts.
# If no parameters are passed, macros/GeometryTest.mac is used.
# Otherwise, the first parameter is used as the detector to be checked.
# Example of use: scripts/GeometryTest.pl Cedar
#
# E Goudzovski, 23/10/2018

$run_number = 6610;

# No parameters are specified: use the standard macro
if ($#ARGV == -1) {
    system("NA62MC macros/GeometryTest.mac $run_number");
    exit;
}

# If a parameter is passed, test the detector specified
open(my $file, '>', 'tmp.mac');
print $file "/run/initialize\n";
print $file "/Detector/PathToLKrShowersDb /cvmfs/na62.cern.ch/offline/NA62FW/showers.db\n";
print $file "/Detector/EnableSubDetector ", shift, "\n";
print $file "/Detector/LKr/GeVtoCurrent1 LKrGeVtoCurr1.dat\n";
print $file "/Detector/LKr/GeVtoCurrent2 LKrGeVtoCurr2.dat\n";
print $file "/Detector/LKr/GeVtoCurrent3 LKrGeVtoCurr3.dat\n";
print $file "/Detector/UpdateGeometry\n";
print $file "/geometry/test/run true\n";
close $file;
system("NA62MC tmp.mac $run_number");
system("rm tmp.mac");
