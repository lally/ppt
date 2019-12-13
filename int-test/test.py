#!/usr/bin/python3

import subprocess
import os
from time import sleep

# Plan:
# - Generate the local spec into each language.
# - Build each language's output (I can use a local Makefile to hold those details)
# - Run each executable.  Make can output the PID.
# - Attach with a timeout.
# - SIGKINT ppt, then read convert the output.
# - Verify that some properties of the spec are valid for each.
#

# emit C++;
# buffer Minimal 512;
# option runtime multithread true;
# option time timespec realtime;
# option debug true;
# frame first {
#    int a, b, c;
#    interval time duration;
#    interval counter events;
# }
# frame second {
#    interval counter foos;
#    int d,e,f;
# }


def buildCpp():
  subprocess.call(["make", "cpptest"])

def runCppExperiment():
  with NamedTemporaryFile as tmpfile
    with Popen(["cpptest"]) as instrumented:
      sleep(1)
      ppt = Popen(["stack", "exec", "ppt", "attach", "-p", str(instrumented.pid), "-b", "Minimal", "-o", tmpfile.name])
      sleep(5)
      ppt.send_signal(SIGINT)
      instrumented.kill()
      # Note: this probably requires a flag to stack to indicate where
      # the project is, and a prior cd() to the temp dir to convert it.
      subprocess.call(["stack", "exec", "ppt", "retrieve", tmpfile.name])

buildCpp()
runCppExperiment()
