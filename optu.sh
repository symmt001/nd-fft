#!/usr/bin/sh

LIST_U=`grep -l nunroll= *.f`

OS_TYPE=`uname -s`

case $OS_TYPE in 
FreeBSD) for i in $LIST_U
do cat - <<  | ed -s $i
1,$ s/\(nunroll=2\*\*\)5/\12/
w
q

done
;;

OSF1) for i in $LIST_U
do cat - <<  | ed -s $i
1,$ s/\(nunroll=2\*\*\)5/\13/
w
q

done
;;

*) for i in $LIST_U
do echo $i
done
;;

esac
