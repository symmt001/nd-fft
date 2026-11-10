#! /bin/sh

for i in nd? nd?? ndfftw ndzfft1d
do echo $i
   time ./$i 
done
