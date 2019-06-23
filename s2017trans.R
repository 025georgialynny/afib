sampspath = "/home/georgia/afib/samplesout/sampsnotrans/"

samps = list.files(sampspath, pattern = "*.csv")

for(i in 1:length(samps)){
  finds = paste(sampspath, samps[i], sep = "")
  assign(samps[i], read.csv(finds))
  assign(samps[i], get(samps[i])[,-1])
}



library(e1071)
aft = NULL
svms = NULL
aff = NULL
for(i in 1:length(samps)){
  svms = c(svms, paste("svms", substr(samps[i], 6, 10), sep = ""))
  aft = c(aft, paste("aft", substr(samps[i], 6, 10), sep = ""))
  aff = c(aff, paste("aff", substr(samps[i], 6, 10), sep = ""))
}


for(i in 1:length(samps)){
  assign(svms[i], get(samps[i])[,c(1, 6:15)])
}

for(i in 1:length(samps)){
  s1 = get(svms[i])
  t1 = subset(s1, afib==T)
  f1 = subset(s1, afib == F)
  assign(aft[i], t1)
  assign(aff[i], f1)
}


allt = NULL
allf = NULL

allt = get(aft[1])
allf = get(aff[1])
for(i in 2:length(samps)){
  allt = rbind(allt, get(aft[i]))
  allf = rbind(allf, get(aff[i]))
}

atshuff = sample(nrow(allt), nrow(allt), replace = F)
afshuff = sample(nrow(allf), nrow(allf), replace = F)


allt = allt[atshuff,]
allf = allf[afshuff,]

tranat = allt[1:5000,]
tranaf = allf[1:5000,]

train = rbind(tranat, tranaf)
t2 = train[,-1]
s04015 = svm(afib~., t2, kernel= "radial", cost = 1000, gamma = (1/27), 
             epsilon = .000001, type = 'C', fitted = F, shrinking = T, tolerance = .00001)

set.seed(3)
meanz = NULL
for(i in 1:length(svms)){
  a = predict(s04015, get(svms[i])[,3:11])
  meanz = c(meanz, mean(a == get(svms[i])[,2]))
  print(svms[i])
  print(table(a, !get(svms[i])[,2]))
}
mean(meanz)
