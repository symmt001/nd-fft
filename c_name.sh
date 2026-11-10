#!/usr/bin/sh

OS_TYPE=`uname -s`

case $OS_TYPE in 
HP-UX|AIX) cat - <<  | ed -s fftnd_fftw.c
1,$ s/\(fftnd\)_*/\1/
1,$ s/\(fftprep1\)_*/\1/
w
q

cat - <<  | ed -s usrtime.c
1,$ s/\(usrtime\)_*/\1/
w
q

;;

HI-UX*|HI-OSF*) cat - <<  | ed -s fftnd_fftw.c
1,$ s/fftnd_*/FFTND/
1,$ s/fftprep1_*/FFTPREP1/
w
q

cat - <<  | ed -s usrtime.c
1,$ s/usrtime_*/USRTIME/
w
q

;;

*) cat - <<  | ed -s fftnd_fftw.c
1,$ s/\(fftnd\)_*/\1_/
1,$ s/\(fftprep1\)_*/\1_/
w
q

cat - <<  | ed -s usrtime.c
1,$ s/\(usrtime\)_*/\1_/
w
q

;;

esac
