#!/usr/bin/ksh
for i in *.f
do  if [ -f ${i%.f}.diag ]
    then loggen $i
    elif [ -f ${i%.f}.par ]
    then loggen $i
    fi
done
