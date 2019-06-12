sampspath = "/home/georgia/afib/samplesout/samps/"
mergepath = "/home/georgia/afib/samplesout/merge/"


merge = list.files(mergepath, pattern = "*.csv") 
samps = list.files(sampspath, pattern = "*.csv")


for(i in 1:length(merge)){
  findm = paste(mergepath, merge[i], sep = "")
  finds = paste(sampspath, samps[i], sep = "")
  assign(merge[i], read.csv(findm))
  assign(samps[i], read.csv(finds))
  assign(samps[i], get(samps[i])[,-1])
  assign(merge[i], get(merge[i])[,-1])
}



afib = function(files, st, en){
  sub1 = which(files[,2] == st)
  sub2 = which(files[,2] == en)
  fs = files[sub1:sub2,]
  af = length(which(fs[,3]=="B"))
  non = length(which(fs[,3]!="B"))
  bool = NULL
  if(af!=0 & non!=0){
    bool = FALSE
  }
  else{
    bool = TRUE
  }
  bool
}
saml = NULL
for(i in 1:length(samps)){
  saml = c(saml, nrow(get(samps[i])))
}

runafib = function(files, samps, j = nrow(samps)){
  thrw = NULL
  for(i in 1:j){
    b = afib(files, samps[i,2], samps[i, 3])
    if(!b){
      thrw=c(thrw, i)
      print(i)
    }
  }
  if(!is.null(thrw)){samps = samps[-thrw,]; print(thrw)}
  samps
}
for(i in 1:length(samps)){
  assign(samps[i], runafib(get(merge[i]), get(samps[i])))
}

sam2 = NULL
for(i in 1:length(samps)){
  sam2 = c(sam2, nrow(get(samps[i])))
}

path = "/home/georgia/afib/samplesout/sampsnotrans/"


for(i in 1:length(samps)){
  write.csv(get(samps[i]), paste(path, samps[i], sep = ""))
}
