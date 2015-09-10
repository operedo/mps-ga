
tail -n 110 $1 > tmp
perl 2dto1d.pl tmp 
rm tmp
