#!/usr/bin/env python
# coding=utf-8

##### Merge binary and header files from different detectors in TEL62 ####
##### Author: Lorenza Iacobuzio, June 2016 ####

import sys
import optparse
import argparse
import re
import struct
from collections import OrderedDict

usage = "usage: python %prog path_to_files path_to_output [detectors]"
parser = optparse.OptionParser(usage)
arg = argparse.ArgumentParser(usage)

parser.add_option("-I", "--IRC", dest="IRC", action="store_true", default=False, help="IRC")
parser.add_option("-S", "--SAC", dest="SAC", action="store_true", default=False, help="SAC")
parser.add_option("-N", "--NewCHOD", dest="NewCHOD", action="store_true", default=False, help="NewCHOD")
parser.add_option("-M", "--MUV0", dest="MUV0", action="store_true", default=False, help="MUV0")

(options, args) = parser.parse_args()
if len(sys.argv) < 2:
    arg.error("Incorrect number of arguments! Please state path to files")
arg.add_argument('args', nargs='*')
path = sys.argv[1]
output = sys.argv[2]
if path.endswith("/"):
    path = path
else:
    path = str(path + "/")

if output.endswith("/"):
    output = output
else:
    output = str(output + "/")

detector = []
IRC = options.IRC
SAC = options.SAC
NewCHOD = options.NewCHOD
MUV0 = options.MUV0

if IRC:
    detector.append("IRC")
if SAC:
    detector.append("SAC")
if NewCHOD:
    detector.append("NewCHOD")
if MUV0:
    detector.append("MUV0")

nbytes = 4
regex = r"^(\d+),(0x\d*\w*),(\d+):(\d+)$"
regex1 = r"^.*?:.*?:(.*)$"

counter_ev = dict()
Buffer = [OrderedDict() for x in range(4)]

binary = []
header = []
fpgaID = []

for i in range(4):
    fpgaID.append(0)

for i in range(len(detector)):
    binary.append([])
    header.append([])

line_counter = 0
n_ev = 0
nwords = 0
ev_header = 0
counter_event = 0
TS = 0
FT = 0
name = ""


def pack_data(data):
    return struct.pack("I", int(data[0:8], 16))

########### Unpacking 32-bit words from binary file for each detector ################


for i_det in range(len(detector)):
    with open(str(path + detector[i_det] + ".dat"), "rb") as f:
        byte = f.read(nbytes)
        while byte != "":
            unpacked_data = struct.unpack("I", byte)
            unpacked_hex = format(unpacked_data[0], "x").rstrip()
            if len(unpacked_hex) % 8 != 0 and len(unpacked_hex) != 0:
                unpacked_hex = unpacked_hex.zfill(8)
            binary[i_det].append(unpacked_hex)
            byte = f.read(nbytes)

########## Unpacking header file for each detector ################

for i_det in range(len(detector)):
    with open(str(path + detector[i_det] + "_header.txt"), "rb") as f:
        line_counter = 0
        content = f.readlines()
        for line in content:
            line = line.strip()
            reg = re.match(regex, line)
            if reg:
                header[i_det].append([reg.group(1), reg.group(2), reg.group(3), reg.group(4)])
                line_counter += 1

n_ev = line_counter
print(n_ev)
for i_det in range(len(detector)):
    counter_ev[i_det] = 0

for item in detector:
    name += item

########### Producing output binary file, merged, and header file, merged ################

with open(str(output + name + ".dat"), "w+b") as f:
    with open(str(output + name + "_header.txt"), "w+") as g:
        g.write(str(name + ".dat" + "\n"))
        g.write("0x20:1:" + str(n_ev) + "\n")
        for i_ev in range(n_ev):
            if i_ev % 1000 == 0:
                print("Processing event n." + str(i_ev))
            ev_header = 0
            for i_det in range(len(detector)):
                nwords = int(header[i_det][i_ev][0]) + counter_ev[i_det]
                TS = header[i_det][i_ev][1]
                FT = header[i_det][i_ev][2]
                if ev_header == 0 or ev_header == binary[i_det][counter_ev[i_det]]:
                    ev_header = binary[i_det][counter_ev[i_det]]
                    counter_ev[i_det] += 1
                else:
                    print("WARNING: Event header with unexpected format")
                for i_fpga in range(4):
                    if binary[i_det][counter_ev[i_det]][4:6] != "ff":
                        print("WARNING: FPGA header with unexpected format")
                    fpgaID[i_fpga] = binary[i_det][counter_ev[i_det]][0:6]
                    nslots = int(binary[i_det][counter_ev[i_det]][6:8], 16)
                    counter_ev[i_det] += 1
                    for i_slot in range(nslots):
                        nslotwords = int(binary[i_det][counter_ev[i_det]][0:4], 16)
                        timestamp = binary[i_det][counter_ev[i_det]][4:8]
                        counter_ev[i_det] += 1
                        for i_hit in range(nslotwords - 1):
                            if timestamp in Buffer[i_fpga].keys():
                                Buffer[i_fpga][timestamp].append(binary[i_det][counter_ev[i_det]])
                            else:
                                Buffer[i_fpga][timestamp] = []
                                Buffer[i_fpga][timestamp].append(binary[i_det][counter_ev[i_det]])
                            counter_ev[i_det] += 1
            f.write(pack_data(ev_header))
            counter_event += 1
            for i_fpga in range(4):
                nslots = format(len(Buffer[i_fpga].keys()), "x")
                f.write(pack_data(fpgaID[i_fpga] + str(nslots).zfill(2)))
                counter_event += 1
                for key in Buffer[i_fpga].keys():
                    nslotwords = len(Buffer[i_fpga][key]) + 1
                    f.write(pack_data(str(format(nslotwords, "x")).zfill(4) + key))
                    counter_event += 1
                    for i_hit in range(nslotwords - 1):
                        f.write(pack_data(Buffer[i_fpga][key][i_hit]))
                        counter_event += 1
                Buffer[i_fpga].clear()
                if nwords != counter_ev[i_det]:
                    print("WARNING: Binary file not consistent with header file")
            g.write(str(counter_event) + "," + str(TS) + "," + str(FT) + ":" + str(counter_event) + "\n")
            counter_event = 0
