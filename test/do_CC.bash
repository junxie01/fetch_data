#!/bin/bash
# do Cross correlation
# example 
# bash do_CC.bash station.lst 2009 1 2009 2
# files needed:
# 1, for_cc see example
# 2, station.lst see example
if [ $# -ne 5 ];then
	echo usage: do_CC.bash station.lst year_begin day_begin year_end day_end
	echo        file for_cc and station list is needed, see example.
	exit
fi
lst=$1
year_b=$2
day_b=$3
year_e=$4
day_e=$5
# download miniseed
~/Dropbox/progs/jun_CC/bin/fetch_data        $lst $year_b $day_b $year_e $day_e
# extract data
~/Dropbox/progs/jun_CC/bin/extract_seed_sec  $lst $year_b $day_b $year_e $day_e
# do preprocessing
~/Dropbox/progs/jun_CC/bin/pre_processing    $lst $year_b $day_b $year_e $day_e
# do cross correlation
~/Dropbox/progs/jun_CC/bin/do_3COM_CC_1list  $lst $year_b $day_b $year_e $day_e
# do stack
~/Dropbox/progs/jun_CC/bin/do_stack_1list    $lst $year_b $day_b $year_e $day_e
