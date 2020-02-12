#!/usr/bin/python3

import os
import tempfile
from time import sleep
from subprocess import call, Popen
from signal import SIGKILL
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

class InTempDir:
  def __enter__(self):
    self.dir = tempfile.mkdtemp()
    self.passing = True
    print(f"Temp dir is {self.dir}")
    return self
  def __exit__(self, type, value, traceback):
    if self.passing:
      os.rmdir(self.dir)


def buildCpp():
  call(["make", "cpptest"])

def runCppExperiment(extraFlags=[]):
  with InTempDir() as tmpdir:
    call(["rm", "-f", "./last"])
    call(["ln", "-s", tmpdir.dir, "./last"])
    with tempfile.NamedTemporaryFile(dir=tmpdir.dir, delete=False) as tmpfile:
      print("Running cpptest")
      with Popen(["./cpptest"]) as instrumented:
        print("  sleeping 1 sec")
        sleep(1)
        attach_args = ["stack", "exec", "ppt", "--", "attach", "-p", str(instrumented.pid),
                       "-b", "Minimal", "-o", tmpfile.name] + extraFlags
        print(f"Attaching via ppt with filename {tmpfile.name}: " + ' '.join(attach_args))
        ppt = Popen(attach_args)
        print(f"  pid is {ppt.pid}, sleeping 5 sec")
        sleep(5)
        print( "  killing ppt")
        ppt.kill()
        print( "  killing cpptest")
        instrumented.kill()
        # Note: this probably requires a flag to stack to indicate where
        # the project is, and a prior cd() to the temp dir to convert it.
        decode_args = ["stack", "exec", "--cwd", tmpdir.dir, "ppt", "decode", tmpfile.name]
        if True:
           print( "Decoding via " + ' '.join(decode_args))
           with open(os.path.join(tmpdir.dir, "decode.log"), "w") as output:
             call(decode_args, stdout=output)
        else:
            print(" Skipping decode.")
        tmpdir.passing = False

if __name__ == '__main__':
  buildCpp()
  runCppExperiment(["-c", "LLC_MISSES,INSTRUCTIONS_RETIRED,UNHALTED_CORE_CYCLES"])
#  runCppExperiment()

