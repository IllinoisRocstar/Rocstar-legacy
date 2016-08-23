#The ElmerProcessor package inherits from ModuleProcessor.pm. 

package ElmerProcessor;

require "ModuleProcessor.pm";
@ElmerProcessor::ISA = qw(ModuleProcessor); #Inherits from ModuleProcessor

use strict;

my (%NDAfiles);

#************************************************************************************
#************************************************************************************
sub getModuleName {
	return "ElmerProcessor";
}

#************************************************************************************
#************************************************************************************
sub getRocstarModuleName {
	return "Elmer";
}

#************************************************************************************
#************************************************************************************
sub checkNDA {
    my ($props, $log, $badFileArray, $numBadFiles, $filename);
    my ($datadir, $griddir, $tempName, $i);
    my($casename, $form1Name, $form2Name, $suffix, $file);
    $props = ModuleProcessor::getProps();
    $log   = ModuleProcessor::getLog  ();

    $datadir = $props->getSOURCEDIR()."Elmer/".$props->getDATADIR("ELMER")."/";
    $griddir = $props->getSOURCEDIR()."Elmer/".$props->getGRIDDIR("ELMER")."/";

    #check to see if the Data and Grid directories exist before we make the file list
    #if not, then return an empty array of filenames.  

    if (!ModuleProcessor::checkDataGridExists($datadir, $griddir)) {
	return(1);
    }
    getNDAFiles($datadir, $griddir);
    my (@tempArray) = keys(%NDAfiles);
    $badFileArray = ModuleProcessor::checkFilesExist(\@tempArray);
    $numBadFiles = @$badFileArray;

    #The following is the expected behavior.  Since there can be one of four filenames
    #for the grid file, and we're checking for all of them, it is expected that
    #three of the four won't exist.  If numBadFiles = 4 or 5, then none were found, and an
    #error should occur. 

    if ($numBadFiles == 3) {
	delete $NDAfiles{@$badFileArray[0]};       
        delete $NDAfiles{@$badFileArray[1]};
        delete $NDAfiles{@$badFileArray[2]};

	$numBadFiles=$numBadFiles-3;
    }
    
    if ($numBadFiles) {
	foreach $filename (@$badFileArray) {
	    $log->processErrorCode(23, $filename);
	}
	return(1);
    }

    $casename = getCaseName($datadir);
    if ($casename eq "NOT FOUND") {
        print "COULD NOT FIND CASE NAME IN ElmerProcessor.checkNDA!";
	return;
    }

    $form1Name = $griddir.$casename.'.sif';
    $form2Name = 'ELMERSOLVER_STARTINFO';
    $suffix = "";

    foreach $file (keys(%NDAfiles)) {
        #look for either of the form1Name or form2Name.
        #should never actually see both... 
        if ($file =~ m/$form1Name/) {$suffix = ".sif";}
	if ($file =~ m/$form2Name/) {$suffix = "";} 
    }

    #Non-empty strings are true in Perl...
    if ($suffix) {
       #if suffix is non-empty, assume we have to find more parts of the
       #grid. Otherwise, assume there's a single part with no .1 or .2, etc.
   
       $log->printMessage("Ready to look for more grid parts. Multipart Elmer Grid format found.");

       #addGridPartsToNDAFiles($griddir.$casename, $suffix);
    }

    return(0);
}



#************************************************************************************
#
#************************************************************************************
sub addGridPartsToNDAFiles {
    my ($prefix,$suffix) = @_;
    my ($testFile, $fileNumber,$props, $log);

   
    $props = ModuleProcessor::getProps();
    $log = ModuleProcessor::getLog();
    $fileNumber=2;

    $log->printMessage("prefix = $prefix, and suffix = $suffix");    

    while(1) {
        $testFile = $prefix.'.'.$fileNumber.$suffix;
        if (-e $testFile) {
	    $log->printMessage("Found Grid filename: $testFile");
            $NDAfiles{$testFile} = $props->getTARGETDIR()."Elmer/Modin/";
	    $fileNumber++;
        }
        else {
            $log->printMessage("Assume that all parts of the Elmer Grid have been found. There is no way to validate this.\n");
            $fileNumber++;
            last;
        }
        #Safety for coding screwups: 
        if ($fileNumber > 50) {
           $log->printMessage("Coding error in ElmerProcessor.addGridPartsToNDAFiles:more than 50 files tried!");
           last;
        }
    }
}


#************************************************************************************
#
#************************************************************************************
sub extract {
    my ($props);
    $props = ModuleProcessor::getProps();

    return(ModuleProcessor::performExtract(\%NDAfiles));
}

#************************************************************************************
#************************************************************************************
sub preProcess {
    my ($log, $props, $targetDir,$currentDir, $errorCode, $caseName, $numProcs);
    my ($elmerprep1, $elmerprep2, $binDir, $workingDir, $units, $unitflag);
    my ($cmd);
    my ($elmerGridFileName);
    $log = ModuleProcessor::getLog();
    $props=ModuleProcessor::getProps();

    $targetDir = $props->getTARGETDIR();
    $workingDir = $targetDir."Elmer/";
    $binDir = $props->getBINDIR();
    $caseName = getCaseName($workingDir);
    $currentDir = `pwd`;
    chomp($currentDir);
    $numProcs = $props->getNumProcs();
    if ($props->propExists("ELMERUNITS")) {
       $units = $props->getElmerUnits();
       $unitflag = "-un $units";
    }
   
    # Elmer file/folder structure preprocessing
    # creating simlink to caseName.sif
    chdir($targetDir);
    $cmd = "ln -s Elmer/Modin/$caseName.sif . > $targetDir/elmerFilePrep.log 2>&1";
    $errorCode = system($cmd);
    if ($errorCode) {
    	$log->processErrorCode(11, $elmerprep1);
    	return(1);
    }
    # creating simlink to ELMERSOLVER_STARTINFO
    chdir($targetDir);
    $cmd = "ln -s Elmer/Modin/ELMERSOLVER_STARTINFO . > $targetDir/elmerFilePrep.log 2>&1";
    $errorCode = system($cmd);
    if ($errorCode) {
    	$log->processErrorCode(11, $elmerprep1);
    	return(1);
    }
    # creating Elmer output folder
    chdir($targetDir);
    $cmd = "mkdir Elmer/Modout/$caseName > $targetDir/elmerFilePrep.log 2>&1";
    $errorCode = system($cmd);
    if ($errorCode) {
    	$log->processErrorCode(11, $elmerprep1);
    	return(1);
    }
    
    # generating mesh   
    $workingDir = $workingDir."Modin/"; 
    chdir($workingDir);
    $elmerGridFileName = $caseName.".grd";
    $elmerprep1 = $binDir."ElmerGrid 1 2 $elmerGridFileName > $targetDir/elmerprep.log 2>&1";
    $errorCode = system($elmerprep1);
    if ($errorCode) {
	$log->processErrorCode(11, $elmerprep1);
	return(1);
    }
  
    # running elmerprep 
    chdir($targetDir);
    $elmerprep2 = "mpirun -np $numProcs $binDir"."elmerprep -com-mpi -fsi -prefix $caseName > $targetDir/elmerprep2.log 2>&1";
    $errorCode = system($elmerprep2);
    if ($errorCode) {
	$log->processErrorCode(11, $elmerprep1);
	return(1);
    }
    chdir($currentDir);
    
    return(0);
}

#************************************************************************************
#************************************************************************************
sub checkResults {
    my ($log, $props, $badFileArray, $numBadFiles, $filename);
    $log = ModuleProcessor::getLog();
    $props = ModuleProcessor::getProps();

    my (@tempArray) = getRuntimeFiles();
    $badFileArray = ModuleProcessor::checkFilesExist(\@tempArray);
    $numBadFiles = @$badFileArray;

    if ($numBadFiles) {
	foreach $filename (@$badFileArray) {
	    $log->processErrorCode(23, $filename);
	}
	return(1);
    }

    return(0);
}

#************************************************************************************
#  Subroutine that develops a list of files that should be in the NDA given user input
#  of a specific Data and Grid combination. 
#************************************************************************************
sub getNDAFiles {
    my ($datadir,$griddir) = @_;
    my ($props, $log, $casename);
    $log = ModuleProcessor::getLog();
    $props = ModuleProcessor::getProps();

    $casename = getCaseName($datadir);

    if ($casename eq "NOT FOUND") {return;}

    #Note: The grid file will one of the following four, but we don't know which
    #Also note that the ones with .1.pat or .1.out will also be followed by
    #some unknown number of .2.out, .3.out, etc. files. We won't check
    #those.

    $NDAfiles{$datadir."ElmerControl.txt"} = $props->getTARGETDIR()."Elmer/" ;
    $NDAfiles{$datadir.$casename.'.sif'} =  $props->getTARGETDIR()."Elmer/Modin/";
    $NDAfiles{$datadir.'ELMERSOLVER_STARTINFO'} =  $props->getTARGETDIR()."Elmer/Modin/";
    $NDAfiles{$griddir.$casename.'.grd'} =  $props->getTARGETDIR()."Elmer/Modin/";
}

#************************************************************************************
#  Subroutine that develops a list of files and directories that should be in the
#  dataset so that it is considered ready to run. 
#************************************************************************************
sub getRuntimeFiles {
    my ($props);
    my ($targetDir, @runtimeFiles);
    $props = ModuleProcessor::getProps();
    $targetDir = $props->getTARGETDIR()."Elmer/";

    push(@runtimeFiles, $targetDir."ElmerControl.txt");

    #parse volume Rocin Control file
    push(@runtimeFiles,ModuleProcessor::getRocinFiles($targetDir."Rocin/solid_in_00.000000.txt"));

    #parse surface Rocin Control file
    push(@runtimeFiles,ModuleProcessor::getRocinFiles($targetDir."Rocin/isolid_in_00.000000.txt"));

    return(@runtimeFiles);
}


#************************************************************************************
#
#************************************************************************************
sub getCaseName {
    my ($dataDir) = shift;
    my ($controlFile, $log, $caseName);
    $controlFile = "ElmerControl.txt";
    $controlFile = $dataDir.$controlFile;

    $log = ModuleProcessor::getLog();

    if (!open(CONTROLFILE, $controlFile))
        {
	    $log->processErrorCode(23, $controlFile);
	    $log->printMessage("$controlFile must exist to supply casename for grid file.");
	    return("NOT FOUND");
	}

    while(<CONTROLFILE>) {
        chomp;
	if ($_ =~ /^\*PREFIX/) {
	    $caseName = <CONTROLFILE>;
	    last;
	}
    }

    close (CONTROLFILE);

    if (($caseName eq undef)||($caseName eq "")) {
	    $log->processErrorCode(24, "In file: ".$controlFile.", can't read case name.");
	    return("NOT FOUND");
    }
    
    chomp($caseName);
    
    #the following strips spaces from the beginning and end of the casename. 
    for ($caseName) {
	s/^\s+//;
	s/\s+$//;
    }  


    return ($caseName);
}
    

#************************************************************************************
#   Returns Boolean whether the module should be processed or not. 
#************************************************************************************
sub isActive {
    my ($props);
    $props = ModuleProcessor::getProps();
    return($props->processModule("ELMER"));
}

1;
