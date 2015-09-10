tail -n 105 $1 | head -n 100 | sed 's/ //g' | cut -c6-105
#tail -n 205 $1 | head -n 200 | sed 's/ //g' | cut -c6-205
