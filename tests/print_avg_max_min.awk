#get-mean-min-max.awk
#You must define COL with option -v of awk
{
if(min=="")
{
min=max=$COL
};
if($COL>max)
{
max=$COL
maxIdx=NR
};
if($COL< min)
{
min=$COL
minIdx=NR
};
total+=$COL;
count+=1
}
#END {print total/count "\n" min " -- " minIdx "\n" max " -- " maxIdx}
END {print total/count}
#END {print min}
