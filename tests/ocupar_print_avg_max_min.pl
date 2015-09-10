
$p=$ARGV[0];

$n=7;
@tags=();
@vals=();

push(@tags,"1");
push(@tags,"2");
push(@tags,"4");
push(@tags,"8");
push(@tags,"16");
push(@tags,"32");
push(@tags,"64");

#push(@tags,"1");
#push(@tags,"5");
#push(@tags,"10");
#push(@tags,"15");
#push(@tags,"20");
#push(@tags,"25");
#push(@tags,"30");


$sum=0;
for($i=0;$i<=$#tags;$i++){
        $tmp=$tags[$i];
	$r=`cat $p |grep ' $tmp .*CLOCK' | awk -v COL=3 -W exec=print_avg_max_min.awk`;
        $sum=$sum+$r;
        push(@vals,$r);
        #print $tags[$i],"\t\t",$vals[$i];
        print $vals[$i];
}
print "total:\t\t",$sum,"\n";

#$r=`cat $p |grep 'iter.*$r2tag ' | awk -v COL=6 -W exec=print_avg_max_min.awk`;
#$r=`cat $p |grep 'iter.*$r3tag ' | awk -v COL=6 -W exec=print_avg_max_min.awk`;
#$r=`cat $p |grep 'iter.*$r4tag ' | awk -v COL=6 -W exec=print_avg_max_min.awk`;
#$r=`cat $p |grep 'iter.*$r5tag ' | awk -v COL=6 -W exec=print_avg_max_min.awk`;
#$r=`cat $p |grep 'iter.*$r6tag ' | awk -v COL=6 -W exec=print_avg_max_min.awk`;
#$r=`cat $p |grep 'iter.*$r7tag ' | awk -v COL=6 -W exec=print_avg_max_min.awk`;


#print $r1tag,"\t",$r1,"\t",$r2tag,"\t",$r2,"\t",$r3tag,"\t",$r3,"\t",$r4tag,"\t",$r4,"\t",$r5tag,"\t",$r5,"\t",$r6tag,"\t",$r6,"\t",$r7tag,"\t",$r7,"\n";

