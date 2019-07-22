#!/usr/bin/perl -w
# KM 2019.08.17

$|++;

$RESULT=0;
$ConditionsDir = $ENV{'NA62TOOLSSOURCE'}."/Conditions";
@ConditionFiles = qx(find $ConditionsDir -type f);

$i=0;
my @NamePrefixes;
foreach $File (@ConditionFiles){
  chomp($File);
  if($File=~/^.*\/([\w\-.]+?)(\.run\d+_\d{4}-run\d+_\d{4})?\.(dat|db|csv|root|xml|gdml)$/){
    $NamePrefix = $1;
    $Found = grep(/^$NamePrefix\b/x, @NamePrefixes); chomp($Found);
    next if ($Found);
    $NamePrefixes[$i] = $NamePrefix;
    $i++;
  }
  else{
    print $File,": Wrong format!!\n";
    $RESULT++;
  }
}
my @Runs = ( 1000 .. 9999 );

foreach $NamePrefix (@NamePrefixes){
  chomp($NamePrefix);
  $NoValidFound = 0;
  $E1ValidFound = 0;
  $M1ValidFound = 0;
  $WrongFormat = 0;
  $NRuns= @Runs;
  $NFiles=0;
  $NFilesWithValidityRange=0;
  foreach $CurrentRun (@Runs){
    chomp($CurrentRun);
    $CurrentBurst = 1;
    $NFiles += @Files = grep(/\/$NamePrefix\./, @ConditionFiles);
    $ValidFoundPerRun=0;
    $ValidFoundNoRange=0;
    $NFilesFoundPerRun=0;
    foreach $File (@Files){
      chomp($File);
      if($File=~/.*\/$NamePrefix\..*/){
        if($File=~/.*\/$NamePrefix\.run(\d+)_(\d+)-run(\d+)_(\d+)\..*/){
          $RunMin     = $1;
          $BurstMin   = $2;
          $RunMax     = $3;
          $BurstMax   = $4;
          if(($RunMin<$CurrentRun && $CurrentRun<$RunMax) ||
            ($RunMin==$CurrentRun && $BurstMin<=$CurrentBurst && $CurrentRun<$RunMax) ||
            ($RunMax==$CurrentRun && $CurrentBurst<=$BurstMax && $RunMin<$CurrentRun) ||
            ($RunMin==$CurrentRun && $BurstMin<=$CurrentBurst && $RunMax==$CurrentRun && $CurrentBurst<=$BurstMax)){
            $ValidFoundPerRun++;
          }
          $NFilesFoundPerRun++ if($RunMin<=$CurrentRun && $CurrentRun<=$RunMax);
          $NFilesWithValidityRange++;
        }
        elsif(not $File=~/.*run\d+.*/){ #no validity range specified = always valid
          $ValidFoundNoRange++;
        }
        else { 
          $WrongFormat++;
        }
      }
    }
    $ValidFoundPerRun++ if(not $ValidFoundPerRun and $ValidFoundNoRange>0);
    $NoValidFound++ if($ValidFoundPerRun==0);
    $E1ValidFound++ if($ValidFoundPerRun==1);
    $M1ValidFound++ if($ValidFoundPerRun>1);
    #print $NamePrefix,": No valid files found for run ",$CurrentRun,"\n" if($ValidFoundPerRun==0);
    print $NamePrefix,": More than one file found for run ",$CurrentRun,"\n" if($ValidFoundPerRun>1);
  }
  print $NamePrefix,": Wrong format!\n" if($WrongFormat>0);
  print $NamePrefix,": No valid files found for ",$NoValidFound,"/",$NRuns," runs!\n" if($NoValidFound>0);
  print $NamePrefix,": >1 valid files found for ",$M1ValidFound,"/",$NRuns," runs!\n" if($M1ValidFound>0);
  print $NamePrefix,": OK!\n" if($E1ValidFound==$NRuns);
  # Ignore known issues
  $Ignore = 0;
  $Ignore = 1 if($NamePrefix eq "T10" and $NoValidFound<=6400);
  $Ignore = 1 if($NamePrefix=~/^Spectrometer-T0-InGoodRun.*/ and $NoValidFound<=6400);
  $Ignore = 1 if($NamePrefix=~/^Spectrometer-T0-jumps.*/ and $NoValidFound<=6400);
  if (not $Ignore and ($WrongFormat>0 or $NoValidFound>0 or $M1ValidFound>0)) {
      print "Invalid entry (not in ignore list): ",$NamePrefix,"\n";
      $RESULT++;
  }
}
exit $RESULT;
