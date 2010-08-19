#!/bin/bash

COUNT=$(( `ipcs -m | grep '^m' | awk '{print $2}' | wc -l` ))
echo -n "Cleaning up $COUNT segments..."

ipcs -m | grep '^m' | awk '{print $2}' | xargs -n 1 ipcrm -m
echo "done"

