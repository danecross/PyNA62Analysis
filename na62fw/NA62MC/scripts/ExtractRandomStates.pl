#!/usr/bin/perl

use strict;
use Getopt::Std;
use File::Basename;


my %Opts;
my $dirname = dirname(__FILE__);
my $cmd;

getopts('i:l:r:',\%Opts);

die "ExtractRandomStates.pl -i MCFile.root -r FirstEvent-LastEvent -l \"EventNumber1 EventNumber2 ...\"" 
if(!defined $Opts{'i'} || ((!defined $Opts{'r'} || !$Opts{'r'}=~/\d+-\d+/) && 
        (!defined $Opts{'l'} || !$Opts{'l'}=~/^\d+/)));

my($MCFile) = $Opts{'i'};
open EVLIST, ">evlist.tmp";
if(defined $Opts{'r'}){
    $Opts{'r'} =~ /(\d+)-(\d+)/;
    my ($FirstEvent, $LastEvent) = ($1, $2);
    while($FirstEvent <= $LastEvent){
        print EVLIST "$FirstEvent\n";
        $FirstEvent++;
    }
}
if(defined $Opts{'l'}){
    my @EventList = split/\s+/,$Opts{'l'};
    foreach (@EventList){
        print EVLIST "$_\n";
    }
}
close EVLIST;

$cmd = "root -b -q \"".$dirname."/../macros/ExtractRandomStates.C\(\\\"$MCFile\\\", \\\"".$dirname."/../\\\"\)\"";
system($cmd);
unlink "evlist.tmp";

