#!/bin/perl
#
# Count the number of bad bursts per system in the database
# Usage: BadBurstSummary.pl [revision] [first_run] [last_run]
# EG, 20 Sep 2017
#

$revision  = shift;
$first_run = shift;
$last_run  = shift;

if (!defined $first_run) {
    print "Usage: BadBurstSummary.pl <Revision> <FirstRun> <LastRun> OR\n";
    print "       BadBurstSummary.pl <Revision> <Sample>\n";
    print "   Data samples (run ranges):\n";
    print "   16B=(5435-6252), 16A=(6278-6726)\n";
    print "   17D=(7615-7721), 17C=(7747-7873)\n";
    print "   17B=(7876-8107), 17A=(8134-8282)\n";
    print "   18A=(8518-8535), 18B=(8548-8740), 18C=(8777-8799), 18D=(8802-8967)\n";
    print "   18E=(8968-9040), 18F=(9047-9134), 18G=(9147-9271), 18H=(9306-9462)\n";
    exit;
}

if (!defined $last_run) {
    if ($first_run eq "16B") {
	$first_run=5435; $last_run=6252;
    }
    if ($first_run eq "16A") {
	$first_run=6278; $last_run=6726;
    }
    if ($first_run eq "17D") {
	$first_run=7615; $last_run=7721;
    }
    if ($first_run eq "17C") {
	$first_run=7747; $last_run=7873;
    }
    if ($first_run eq "17B") {
        $first_run=7876; $last_run=8107;
    }
    if ($first_run eq "17A") {
        $first_run=8134; $last_run=8282;
    }
    if ($first_run eq "18A") {
	$first_run=8518; $last_run=8535;
    }
    if ($first_run eq "18B") {
        $first_run=8548; $last_run=8740;
    }
    if ($first_run eq "18C") {
        $first_run=8777; $last_run=8799;
    }
    if ($first_run eq "18D") {
        $first_run=8802; $last_run=8967;
    }
    if ($first_run eq "18E") {
        $first_run=8968; $last_run=9040;
    }
    if ($first_run eq "18F") {
        $first_run=9047; $last_run=9134;
    }
    if ($first_run eq "18G") {
        $first_run=9147; $last_run=9271;
    }
    if ($first_run eq "18H") {
        $first_run=9306; $last_run=9462;
    }
}

# List of input files to consider
$badburstdir = "/cvmfs/na62.cern.ch/offline/CDB/$revision";
if ($revision=~/^r.*/ or $revision=~/^t.*/) {
  $badburstdir = "/afs/cern.ch/work/n/na62prod/public/CDB/*-$revision";
}
@files = qx{ls $badburstdir | awk '(\$1>=$first_run && \$1<=$last_run){print "$badburstdir/"\$0"/BadBursts.run"\$1"_0000-run"\$1"_9999.dat"}'};

# Read these files
my @input;
foreach $filename (@files) {
    chomp $filename;
    my $nslashes = () = $filename =~ /\//g;
    @c = split /\//, $filename;
    $runno = $c[$nslashes-1];
    chomp $runno;
    @read = qx{cat $filename | grep -v '#' | grep $runno};
    push @input, @read;
}

# Build a list of systems
my @systems;
foreach $line (@input) {
    chomp $line;
    my @components = split / /,$line;
    $offset=3;
    $offset=4 if $line =~/^B/;
    for ($i=$offset; $i<@components; $i++) {
	$det = $components[$i];
	push @systems, $det if (!grep {$_ eq $det} @systems);
    }
}
@sorted_systems = sort {lc($a) cmp lc($b)} @systems;

# Count bad bursts per system
print "# Bad bursts per system: all, exclusive\n";
print "# Revision $revision; run range: $first_run-$last_run\n";
print "# Input files found: ", @files+0, "\n#\n";
foreach $system (@sorted_systems) {
    $system1 = " " . $system; # to resolve ambiguities like CHOD/NewCHOD
    $Nbadbursts = 0;
    $Nbadbursts_exclusive = 0;
    foreach $line (@input) {
	if (index($line, $system1) != -1) {
	    $Nbadbursts++;
	    my @components = split / /,$line;
            $offset=3;
            $offset=4 if $line =~/^B/;
	    $Nbadbursts_exclusive++ if (@components-$offset==1);
	}
    }
    printf("%-12s %5d %5d\n", $system, $Nbadbursts, $Nbadbursts_exclusive);
}

print "------------------------\n";
printf("%-12s %5d\n", "Any", @input+0);
