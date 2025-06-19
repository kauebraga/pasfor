

gawk -F, 'NR==1{h=$0;next}{d=substr($4,2,8); if(!(d in f)){print h > "daily_csv/gps_"d".csv"; f[d]=1} print >> "daily_csv/gps_"d".csv"}' PAITT_202412_281.csv