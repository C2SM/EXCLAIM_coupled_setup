#!/bin/bash

mkdir /dev/shm/"$USER"
ldd ./icon_gpu | sed 's/\t/ /g' > temp.tmp
cat temp.tmp | sed 's/.*=> //' | sed 's/ (.*//' | sed 's/^ *//' > temp2.tmp
cat temp.tmp | sed 's/ =>.*//' | sed 's/.*\///' | sed "s/^[ \t]*/\/dev\/shm\/$USER\//" | sed 's/ *(.*//' > temp3.tmp
paste temp2.tmp temp3.tmp > bcast_files.sh
