
fileName=$1
genNum=$2
size=$3

pattern="Generation=[[:space:]]*${genNum}$"

comm=`grep -n "${pattern}" ${fileName} | cut -f1 -d:`





