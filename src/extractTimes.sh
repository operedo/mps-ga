tem=$1
size=$2
proc=$3

ls -1 evol_${tem}_${size}_A_100x100_${proc}_* | sed 's/.*_\([0-9]\+\)\.txt/err_\1_mps-ga/' > tmp 
pal=" "

for i in `cat tmp`; 
do 
	pal="${pal} $i"
done; 

#tail $pal | grep 'real\t\+[0-9]\+m[0-9]\+\.[0-9]\+s'
#tail $pal | grep 'real' | sed 's/real\t\+\([0-9]\+\)m\([0-9]+\)\.\([0-9]\+\)s/\1 \2 \3/'
tail $pal | grep 'real' | sed 's/real\t\+\([0-9]\+\)m\([0-9]\+\)\.\([0-9]\+\)s/=60\*\1+\2\.\3/'
