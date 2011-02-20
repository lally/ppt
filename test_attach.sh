#!/bin/bash
#gconftool-2 --type bool --set /apps/gnome-power-manager/general/use_time_for_policy false
#Found symbol _ppt_frame_beta2
#Found symbol _ppt_version_beta2
#Found symbol _Z21ppt_write_beta2_framev
#Found symbol _ppt_hmem_beta2
#Found symbol _ppt_hsize_beta2
PROC_PID=`pgrep test_writer`
echo "Attempting to attach to $PROC_PID..."
./ppt-agent attach -p $PROC_PID -s _ppt_hmem_beta2 -v _ppt_version_beta2 -N 0xa050db9e -i "./beta2_listen out.buf %d 16384" -n 64 "$@"

