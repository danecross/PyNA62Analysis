#!/bin/perl
#
# Count the number of bad bursts per run in the database
# Usage: BadBurstSummaryPerRun.pl [Revision] [FirstRun] [LastRun]
# KM, 12 Dec 2018
#

$revision  = shift;
$first_run = shift;
$last_run  = shift;

if (!defined $first_run) {
    print "Usage: BadBurstSummaryPerRun.pl <Revision> <FirstRun> <LastRun> OR\n";
    print "       BadBurstSummaryPerRun.pl <Revision> <Sample>\n";
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

$badburstdir = "/cvmfs/na62.cern.ch/offline/CDB/$revision";
if($revision=~/^r.*/ or $revision=~/^t.*/){
  $badburstdir = "/afs/cern.ch/work/n/na62prod/public/CDB/*-$revision";
}

$afslistdir = "/afs/cern.ch/na62/offline/lists";

$NBadBurstsTotal=0;
$NBurstsTotal=0;
$NCheckedRuns=0;

# Colours
$ESC_SEQ="\x1b[";
$COL_RESET=$ESC_SEQ."39;49;00m";
$COL_RED=$ESC_SEQ."31;01m";
$COL_GREEN=$ESC_SEQ."32;01m";
$COL_YELLOW=$ESC_SEQ."33;01m";
$COL_BLUE=$ESC_SEQ."34;01m";
$COL_MAGENTA=$ESC_SEQ."35;01m";
$COL_CYAN=$ESC_SEQ."36;01m";

@RunLists = sort qx(find $afslistdir/Data/ -name Run??????.list);
foreach $RunList (@RunLists){
  chomp ($RunList);
  if($RunList=~/.*Run(\d+)\.list/){
    $RunID=$1;
    next if ($RunID<$first_run or $RunID>$last_run);
    $BBListFound = qx(ls -1 $badburstdir/$RunID/BadBursts.*dat 2>/dev/null); chomp($BBListFound);
    if($BBListFound){
      $NBadBursts = @BadBursts = qx(cat $badburstdir/$RunID/BadBursts.*dat 2>/dev/null | grep -v '#' | grep -v '^\$'); chomp($NBadBursts);
      $NBursts = qx(cat $RunList 2>/dev/null | grep -v '#' | wc -l); chomp($NBursts);
      $BBPercent = $NBadBursts*100./$NBursts;
      $Color = ${COL_RESET};
      $Color = ${COL_GREEN}  if($BBPercent< 3.);
      $Color = ${COL_YELLOW} if($BBPercent>10.);
      $Color = ${COL_RED}    if($BBPercent>50.);
      print "Run $RunID: ",sprintf("%4d",$NBadBursts),"/",sprintf("%4d",$NBursts)," bad bursts found (= ",$Color,sprintf("%2.2f\%",$BBPercent),${COL_RESET},")";
      # print main bad subsystems
      my %NBadBurstsPerSubsystem;
      foreach $BadBurst (@BadBursts) {
        chomp $BadBurst;
        my @components = split / /,$BadBurst;
        for ($i=4; $i<@components; $i++) {
          $NBadBurstsPerSubsystem{$components[$i]}++;
        }
        $NBadBurstsTotal++;
      }
      foreach my $key (sort { $NBadBurstsPerSubsystem{$b} <=> $NBadBurstsPerSubsystem{$a} } keys %NBadBurstsPerSubsystem) {
        $BBPercentPerSubsystem = $NBadBurstsPerSubsystem{$key}*100./$NBursts;
        next if ($BBPercentPerSubsystem<5.);
        $Color = ${COL_RESET};
        $Color = ${COL_GREEN}  if($BBPercentPerSubsystem< 3.);
        $Color = ${COL_YELLOW} if($BBPercentPerSubsystem>10.);
        $Color = ${COL_RED}    if($BBPercentPerSubsystem>50.);
        print " $key (",$Color,sprintf("%2.2f\%",$BBPercentPerSubsystem),${COL_RESET},")";
      }
      print "\n";
      $NBurstsTotal+=$NBursts;
      $NCheckedRuns++;
    }
    #else{
    #  print "Run $RunID: Bad burst list not found\n";
    #}
  }
  else{
    print "Invalid run list format: ",$RunList,"\n";
  }
}
$BBPercentTotal = 0.;
$BBPercentTotal = $NBadBurstsTotal*100./$NBurstsTotal if($NBurstsTotal>0);
print "$NBadBurstsTotal/$NBurstsTotal (= ",sprintf("%2.2f\%",$BBPercentTotal),") bad bursts found in range [$first_run,$last_run] (= $NCheckedRuns runs)\n";
