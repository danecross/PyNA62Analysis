# Tests for gitlab-ci (Continuous Integration)
The CI mechanism in gitlab allows to automatically test any merge request to develop.
The tests are done in multiple steps (called pipeline), and the pipeline fails as soon as one step fails (and cannot therefore be merged until the problem is solved).
When a new push is done on the merge request, any existing running pipeline is cancelled and a new pipeline is created, restarting from the first step.

This feature can make development of new tests a bit tedious as your new test is likely towards the end of the pipeline, after the compilation of the framework. Any tiny change you make (typo, missing command, ...) will take a long time before before you can verify that your change has been effective and that you made no further mistake (~30 minutes).

## How to add new tests
Code for the tests are located in two places, at the root of the git repository (na62fw/):
 1. In the **CITests** directory, containing the actual scripts that run the test
 1. In the **.gitlab-ci.yml** file, containing the definitions of the tests
 
 
The first step is to determine if your test is part of an existing test or is independent (most likely the second option). Tests should be kept as independent as possible, as determining which test fails will help understand what is the problem.
 - If the test is part of another one, simply modify the corresponding script in the **CITests** directory, and move to [How to test the test](#how-to-test-the-test)
 - If the test is independent, you will need to add a new script in **CITests**. Use the provided template.sh, which already contains the definition of some useful variables.
   - **BUILDDIR**: location of the root of the git repository (na62fw/)
   - **TESTDIR**: location of a test directory where the test should be run and any output of your test should be produced. The content of this directory will be archived, saved and downloadable for inspection from the gitlab website (jargon: the content of this directory is a '*CI artifact*') 
   - **DATADIR**: location of a data directory where you can download/find raw/processed data files to be used for your test. This folder is persistent across pipelines and its content will almost never be removed. If you have to download/copy any data file in this folder, make sure it is not already present. You can add the copy/download operation in the **CITests/get_data_files.sh** script and assume in your test that the file will be present at this location.
   - **FWDIR**: location of NA62Reconstruction
 - You must add a job definition in **.gitlab-ci.yml**. The job definition usually contains 3 elements: a **template** (describing the scripts to run, the stage and the artifacts), and two **system-dependent jobs** (SLC6 - CC7). You need to determine if the job will belong to the *generation* or the *test* stage.
   - *generation* (*gen-&ast; stage*): All jobs for which raw/root data are processed, generating an output root file that could be used in the *test* stage. In this stage, the generation/processing of the file itself is tested. The structure of a gen-* job looks like:

     ```
     # -- Template
     .gen_name: &gen_name  # This is the name of the template used for the job. Note that it starts necessarily with a . (dot). 
       stage: gen-mc_reco_ana  # Stage in which the job will run
       script: # Commands that the jobs will run (typically just run the shell script you created in CITests)
         - $CI_PROJECT_DIR/CITests/your_script.sh
       artifacts:
         <<: *gen_artifacts
         name: artifacts_name # Name of the artifact generated by the job (name of the archive containing the output files generated)
     
     # -- Jobs
     gen_name_cc7:              # cc7 job
       <<: *gen_jobs_cc7        # Standard gen-* job definition for cc7
       <<: *gen_name            # Name of the template defined above
       <<: *gen_build_deps_cc7  # Dependencies on the build-*_cc7 jobs (ensure the libraries/binaries compiled in the build-* jobs are extracted) 
         
     gen_mc_reco_ana_slc6:      # slc6 job                                                                                                    
       <<: *gen_jobs_slc6       # Standard gen-* job definition for slc6
       <<: *gen_mc_reco_ana     # Name of the template defined above
       <<: *gen_build_deps_slc6 # Dependencies on the build-*_slc6 jobs (ensure the libraries/binaries compiled in the build-* jobs are extracted)
     ```

   - *test* (*test-&ast; stage*): In this stage, we are possibly using the root output of the previous stage. The actual content of the files is tested in this stage. 
   The structure of a test-* job looks like:
 
     ```
     # -- Template
     .test_name: &test_name  # This is the name of the template used for the job. Note that it starts necessarily with a . (dot).
       stage: test-mc_reco_ana  #Stage in which the job will run
       script:  # Commands that the jobs will run (typically just run the shell script you created in CITests)
         - $CI_PROJECT_DIR/CITests/your_script.sh
     
     # -- Jobs
     test_name_cc7:               # cc7 job
       <<: *test_jobs_cc7         # Standard test-* job definition for cc7
       <<: *test_name             # Name of the template defined above
       dependencies:              # List of dependencies from previous stages
         - build_mc_cc7           # Mandatory
         - build_reco_cc7         # Mandatory
         - build_analysis_fw_cc7  # Mandatory
         - gen_name1_cc7          # List of cc7 gen-* jobs whose output are required for this job
         - gen_name2_cc7          # List of cc7 gen-* jobs whose output are required for this job
     
     memory_usage_slc6:           # slc6 job
       <<: *test_jobs_slc6        # Standard test-* job definition for slc6
       <<: *test_name             # Name of the template defined above
       dependencies:              # List of dependencies from previous stages
         - build_mc_slc6          # Mandatory
         - build_reco_slc6        # Mandatory
         - build_analysis_fw_slc6 # Mandatory
         - test_name1_slc6        # List of slc6 gen-* jobs whose output are required for this job
         - test_name2_slc6        # List of slc6 gen-* jobs whose output are required for this job
     ```
     
     In summary, the tests at the gen-&ast; stage verify the processing itself of input file and generation of output file. 
     The tests in the test-&ast; stage verify the content of the output files created in the gen-&ast; stage.
## How to test the test
Start by creating a new branch with the format **gitlabci/some_name**. Commit your tests and push the branch to the gitlab repository.

You first should check that the changes you made to the **.gitlab-ci.yml** file are valid. You can do this by simply navigating to 
the **.gitlab-ci.yml** file of your branch (https://gitlab.cern.ch/NA62FW/na62fw/blob/gitlabci/some_name/.gitlab-ci.yml). If the file is not valid, a message will warn
you and you should fix it. Once the file is valid, this will trigger a gitlab-ci pipeline running the tests.

The difference with respect to the standard pipelines is that a docker image is going to be published in the registry for each successful build-step. 
At this point, two outcome are possible:
  - Your tests are correctly configured, and the pipeline will run without problem to the end. You are therefore ready to create a merge request to integrate your
    test in the develop branch. Please have a look at the [Cleanup](#cleanup) section below
  - There are some problems with your test and it will fail. This is when the docker images are becoming useful. Rather committing a fix for the issue, 
    wait for the whole pipeline to run again and check that your fix was efficient, you can simply use the script **CITests/docker_test.py** provided to test
    only the stage that failed. This script will run on a system where docker is installed (and where the user is allowed to run docker without sudo privileges).
    In case you do not have access to such a system, you can login on the following CERN VM **na62fw-dev.cern.ch** (accessible to na62fw-dev egroup members).

    The script requires few arguments:
    - --stage: Name of the stage from which you want to start your test. This is likely to be the last successful stage. Example: you are adding 
     a new test at the gen-\* stage for NA62Reconstruction, and your test fails. So you want to run your test from a successful build of NA62Reconstruction. 
     You will then use *'--stage reco'* 
    - --branch: This is the name of you branch, without the gitlabci/ prefix. If your branch is 'gitlabci/new_amazing_test', you will pass
     *'--branch new_amazing_test'*
    - --system: Which system you want to test (*slc6* or *cc7*). The default is *cc7*
    - --local: Location of your local **CITests** directory (will be mounted as **na62fw/CITests_Fixed** in the container)
    - --datadir: Location of a data directory containing data files eventually needed by your test (the content will appear in **/data**).
    - --cleanup: clean whatever can be cleaned (please do it once all your tests are successful and you are ready to merge), and see [Cleanup](#cleanup) below.
    
  - Before running the script, if your job uses an external data file copied with **get_data_files.sh**, please copy it into a local directory. The directory has
    to be readable/writable by docker (an AFS directory is not!). If you use **na62fw-dev.cern.ch**, you can use **/home/data**. Then use the *'--datadir'* 
    option to indicate the script where that directory is located.
  - When running the script, you will be logged into an independent container (can be thought of as a virtual machine) and redirected in the
    **CITests** directory of your branch. This container is exactly the environment in which the real CI tests will take place on gitlab.
    
    
  1. Run the script for your test. It should fail in the same way it fails on the CI server.
  1. Exit the container (```exit```)
  1. Fix your code locally (do not commit/push yet)
  1. Run the script again, from the **CITests_Fixed** directory (this should contain the fix you just made) and verify that the test now works. 
     If not, restart at 1) until the test is successful.
  1. Commit your fix and push, wait for the pipeline to be successful then create a merge request.
  1. Good job! 

## Cleanup  
Note: Because of technical difficulties (unable to login to the gitlab-registry.cern.ch REST API, if you know how to do it, drop me a line!),
the script is not able to automatically cleanup and remove the images created during the test process, once everything is working.
So please, cleanup after yourself by visiting the page at https://gitlab.cern.ch/NA62FW/na62fw/container_registry and expand the registry by clicking
*na62fw/na62fw*, and delete all the images related to your branch (contain the name of your branch) by clicking the trash bin. 

  