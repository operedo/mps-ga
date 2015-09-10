open(IN,"<$ARGV[0]");
%array=();
$counter=1;
while(<IN>){
	@line=split(/\s+/,$_);
        if($counter>5 && $counter<106){
		@{ $array{$counter} }=@line;
		#for($i=6;$i<=$#line-5;$i++){
		#	#print $line[$i],"\n";
		#	$array[$counter-1][$i-6]=$line[$i];
		#}
	}
	$counter=$counter+1;
}


for($i=105;$i>=6;$i--){
#	for($j=6;$j<=105;$j++){
#		print ${$array{$i}}[$j],"\n";
#	}
	print @{$array{$i}},"\n";
}
