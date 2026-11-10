#! /usr/bin/sh
out=test.out
err=test.err

rm -f $out
rm -f $err

for i in nd? nd?? ndgpfa ndgpfa2 ndgpfas
do (echo "$i " | tee -a $out )   >> $err 2>&1
   (time ./$i  | tee -a $out )   >> $err 2>&1 
done

cat - <<  | ed -s $out
1,$ g/Test/d
1,$ g/^nd/j
w
q

