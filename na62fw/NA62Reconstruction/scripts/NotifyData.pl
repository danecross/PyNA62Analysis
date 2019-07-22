#!/usr/bin/perl
## Updates the list of files to be processed by the online monitor.
## Authors: Karim Massri (karim.massri@cern.ch), Antonino Sergi (antonino.sergi@cern.ch), October 2011.

$hostname = $ENV{'HOSTNAME'};
$listfile = "/mnt/sw/NA62Reconstruction/output.list";
$last_ctime = time(); 

$already_running = qx(ps -A | grep NotifyData.pl | wc -l); chomp($already_running);
if($already_running>1){
  print "NotifyData.pl is already running!! Nothing to be done..\n";
  exit;
}

while (1) {

  for($mID=1;$mID<=3;$mID++){
#    next if($mID eq 2); #merger 2 disabled
    if($hostname ne "na62merger".$mID){
      $mergerdir = "/mnt/merger".$mID;
      system("ls $mergerdir > /dev/null") #automount
    }
    else { #local machine
      $mergerdir = "/merger";
    }
    $datadir = $mergerdir."/cdr";
    $tagdir = $mergerdir."/bkm/OnlineDataComplete";
    opendir (TAGDIR, $tagdir);
    @files = qx(ls -1 $tagdir | tail -n 10);
    foreach $file (@files){
      chomp($file);
      if ($file =~ /na62raw_\d+-\d+-\d+-\d+(_\d+)?\.dat$/) {
#      if ($file =~ /cdr0\d+-\d+(_\d+)*\.dat(\.\d*)?$/) {
        @status = stat("$tagdir/$file");
        $ctime = @status[9];
        if ($ctime > $last_ctime) {
          if (!exists($used{$file})) {
            open LIST, ">$listfile";
            print LIST "$datadir/$file\n";
            print  "$datadir/$file\n";
            close LIST;
            $used{$file} = 1;
          }
        }
      }
    }
    close TAGDIR;
  }

  sleep(1);

}
