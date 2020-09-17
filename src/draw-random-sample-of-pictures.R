# select a random sample of pictures to recover temperature data

# run this after filtering non-independent events in Part-1!!!

# check how many records of bandeira and canastra we have
bandeira <- subset(gurupi2016, bin=="Myrmecophaga tridactyla"); nrow(bandeira)
canastra <- subset(gurupi2016, bin=="Priodontes maximus"); nrow(canastra)

# how many total records
nrow(gurupi2016)

# draw a random sample of 100 rows from gurupi2016
randomSample <- gurupi2016[sample(nrow(gurupi2016), 100), ]
nrow(randomSample)
