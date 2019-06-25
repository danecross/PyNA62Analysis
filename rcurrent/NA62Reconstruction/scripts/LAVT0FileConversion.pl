#!/usr/bin/perl -w

if ($#ARGV <0 || $#ARGV > 1) {
    die "LAVT0FileConversion >> Usage Error: ./LAVT0FileConversion inputT0File [year]\n $#ARGV \n";
}

$fileName = $ARGV[0];
open (FILIN, $fileName) or die "LAVT0FileConversion >> cannot open input file $fileName\n";
$fileNameOut = "LAV-T0_CustomFormat.dat";
open (FILOUT, ">$fileNameOut") or die "LAVT0FileConversion >> cannot open output file $fileNameOut\n";


print FILOUT "Block\n";
$lastBananaAverage = 0;

while (<FILIN>) {
    
    @f = split;
    if ((!(/^#/)) && ($#f == 2)) {
	if ($f[1] =~ /^(\d+)$/) {
	    $blockID = $f[1];
	    $t0Value = $f[2];
	    
	    if ($blockID >= 10000 && $blockID <= 120000+3*1000+16*10) {
		$stationID = int($f[1]/10000); 
		$layerID = int(($blockID-$stationID*10000)/1000);
		$bananaID = int(($blockID-$stationID*10000-$layerID*1000)/10);
		$bID = $blockID-$stationID*10000-$layerID*1000-10*$bananaID;
		$blocksID[$bID] = $blockID;
		$blocksT0[$bID] = $t0Value;
		if ($bID==0) {
		    $bananaAverage = 0;
		    $nAveragedBlocks = 0;
		}
		if ($t0Value > -999 && $t0Value < 999) {
		    $bananaAverage += $t0Value;
		    $nAveragedBlocks++;
		}
		if ($bID == 3) {
		    if ($nAveragedBlocks > 0) {
			$bananaAverage /= $nAveragedBlocks;
			$lastBananaAverage = $bananaAverage;
		    }
		    else {
			$bananaAverage = $lastBananaAverage;
		    }
		    for ($ib = 0; $ib < 4; $ib++) {
			if ($blocksT0[$ib] > -999 && $blocksT0[$ib] < 999) {			    
			    print FILOUT "$blocksID[$ib] $blocksT0[$ib]\n";
			}
			else {
			    print FILOUT "$blocksID[$ib] $bananaAverage\n";
			}
		    }
		}
	    }
	}
    }
}
close FILIN;
close FILOUT; 
