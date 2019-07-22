#!/usr/bin/env python
'''
This script helps loading docker images to test gitlab-CI tests for na62fw
@author:     nlurkin
'''

import sys
import os

from argparse import ArgumentParser
from argparse import RawDescriptionHelpFormatter


def login():
    loggedin = False
    if os.path.exists("{HOME}/.docker/config.json".format(HOME=os.getenv("HOME", "."))):
        if 'gitlab-registry.cern.ch' in open("{HOME}/.docker/config.json".format(HOME=os.getenv("HOME", "."))).read():
            loggedin = True
    if not loggedin:
        os.system("docker login gitlab-registry.cern.ch")


def run(args):
    print "-- Login to gitlab registry. Please provide your NICE credentials, if asked"
    login()

    value_dic = {}
    value_dic["docker_registry"] = "gitlab-registry.cern.ch/na62fw/na62fw"
    value_dic["image_name"] = "gitlabci-{args.branch}-{args.stage}_{args.system}".format(args=args)
    value_dic["CI_PROJECT_DIR"] = "/builds/NA62FW/na62fw"
    value_dic["local"] = os.path.abspath(args.local)
    value_dic["data_share"] = ""
    if args.datadir is not None:
        value_dic["data_share"] = "-v {0}:/data:shared".format(args.datadir)

    if args.cleanup:
        print "-- Cleaning system. Please answer yes if prompted."
        os.system("docker system prune -a")
        print
        print "I am currently unable to implement the remaining of this feature due to technical difficulties..."
        print "Please would you be so kind to go to the following address"
        print "https://gitlab.cern.ch/NA62FW/na62fw/container_registry , click on na62fw/na62fw"
        print "and click on the bin button beside all the tags that have the name of your branch in the name"
        print
        print "Thanks a lot for tidying up behind you!"
        print
        return

    print "-- Retrieving latest image from registry for {image_name}".format(**value_dic)
    os.system("docker pull {docker_registry}:{image_name}".format(**value_dic))

    print "-- Syncing CITests directory"
    os.system("rsync -a {local}/* /home/public".format(**value_dic))

    print "-- Running container"
    command = "docker run -w {CI_PROJECT_DIR}/CITests --rm -ti \
            -v /cvmfs:/cvmfs:shared -v /afs:/afs:shared -v /home/public:{CI_PROJECT_DIR}/CITests_Fixed:ro {data_share} \
            {docker_registry}:{image_name} \
            /bin/bash -c 'export CI_PROJECT_DIR=/{CI_PROJECT_DIR}; export NA62MCSOURCE=$CI_PROJECT_DIR/NA62MC; \
            /bin/bash'".format(**value_dic)
    os.system(command)


def main(argv=None):
    '''Command line options.'''

    if argv is None:
        argv = sys.argv
    else:
        sys.argv.extend(argv)

    # Setup argument parser
    parser = ArgumentParser(description="This script will helps loading docker images to test gitlab-CI tests for na62fw", formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument("--stage", choices=["mc", "reco", "analysis", "gen"], help="Stage to load (you most likely want the last successful stage?)", required=True)
    parser.add_argument("--branch", help="Name of the tested branch (omit the 'gitlabci/' prefix)", required=True)
    parser.add_argument("--system", choices=["slc6", "cc7"], default="cc7", help="Target system to retrieve [default: %(default)s]")
    parser.add_argument("--cleanup", action="store_true", default=False, help="Cleanup registry (this should be done when everything is successful as it removes all images for this branch in the registry")
    parser.add_argument("--datadir", help="Location of a directory containing data files")
    parser.add_argument("--local", help="Location of your local CITests directory (where the non-committed fix should be located)", required=True)

    # Process arguments
    args = parser.parse_args()
    args.branch = args.branch.replace("_", "-").lower()

    run(args)


if __name__ == "__main__":
    sys.exit(main())
