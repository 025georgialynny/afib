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

library(colorspace)
cols = sequential_hcl(6, "blue")[c(-4,-5, -6)]
cols2 = sequential_hcl(6, "red")[c(-4,-5, -6)]
cols3 = sequential_hcl(6, "green")[c(-4,-5, -6)]
cols = c(cols, cols2, cols3)

barplot(1:9, col = cols)
hist(samps04015.csv[,11], border = cols2[11%%3+1], breaks = 25, col = cols2[11%%3+1])
for(i in c(12, 10)){
  hist(samps04015.csv[,i], border = cols2[i%%3+1], breaks = 11, col = cols2[i%%3+1], add = T)
}
for(i in 7:9){
  hist(samps04015.csv[,i], border = cols[i%%3+1], breaks = 11, col = cols[i%%3+1], add = T)
}

for(i in 13:15){
  hist(samps04015.csv[,i], border = cols3[i%%3+1], breaks = 8, col = cols3[i%%3+1], add = T)
}

findm = function(files){
  maxi = 0
  for (i in 7:15){
    if(max(density(files[,i])$y)>maxi){
      maxi = max(density(files[,i])$y)
    }
  }
  maxi
}

barprobs = function(files){
  par(mar = c(5,5,2,2), family = "serif", usr = c(0, 1, 0, 70), mfrow = c(1,3))
  cols = sequential_hcl(6, "ag_sunset")[c(-1, -3, -6)]
  cols2 = cols
  cols3 = cols
  
  plot(1, type="n", xlab="Transisition Proportion From Short", ylab="Density", xlim=c(0, 1), ylim=c(0, findm(files)), cex.lab = 3, cex.axis = 2.5)
  
  for(i in 7:9){
    points(density(files[,i]), lwd = 5, col = cols[i%%3+1], type = "l")
  }
  
  plot(1, type="n", xlab="Transisition Proportion From Regular", ylab="Density", xlim=c(0, 1), ylim=c(0, findm(files)), cex.lab = 3, cex.axis = 2.5)
  
  for(i in 10:12){
    points(density(files[,i]), lwd = 5, col = cols2[i%%3+1], type = "l")
  }
  legend("top", title = "To", legend = c("Short", "Regular", "Long"), cex = 3, pch=15, pt.cex=5, col = cols)
  
  plot(1, type="n", xlab="Transisition Proportion From Long", ylab="Density", xlim=c(0, 1), ylim=c(0, findm(files)), cex.lab = 3, cex.axis = 2.5)
  
  for(i in 13:15){
    points(density(files[,i]), lwd = 5, col = cols3[i%%3+1], type = "l")
  }
  
}
path = "/home/georgia/afib/propsplots/"
barprobs(samps04015.csv)
for(i in 1:length(samps)){
  jpeg(filename = paste(path, "probsplot", substr(samps[i], 6, 10), ".jpeg", sep = ""), width = 900*3, height = 700)
  barprobs(get(samps[i]))
  dev.off()
}
  
library(diagram)
library(markovchain)

transmat = function(file, mat){###############creates transition matrix based on classify function additions above
  for(i in 1:(nrow(file)-1)){
    from = file[i,6]
    to = file[i+1, 6]
    mat[to, from] = mat[to, from] + 1
  }
  mat
}
rowprop = function(mat){
  newmat = matrix(rep(0,9), ncol = 3)
  for(i in 1:3){
    for(j in 1:3){
      newmat[j, i] = mat[j, i]/sum(unlist(list(mat[,i])))
    }
  }
  newmat
}
t = matrix(rep(0, 9), ncol = 3)
t = transmat(merge04015.csv[1:50,], t)
t = rowprop(t)
st = c("S", "R", "L")
dimnames(t) = list(st)
colnames(t) = st
s1 = new("markovchain", states = st, transitionMatrix = t, byrow = F)

par(mar = c(1,1,3,1), xpd = NA, family = 'serif')
dcol = sequential_hcl(9, "ag_sunset")
dcol = matrix(dcol, ncol = 3)
markdia = function(t){
  par(mar = c(1,1,3,1), xpd = NA, family = 'serif')
  plotmat(t, pos = c(1,2),lwd = 4, lcol = "black", box.col = dcol[2, 1:3], arr.lcol = dcol,
        arr.col = t(dcol), arr.width = .4, arr.length = .4, self.lwd = 4, self.shiftx = c(0, -.15, .15),
        self.shifty = c(.12, -.01, -.01), cex.txt = 1.5, box.cex = 3, dtext = -.6, 
        self.arrpos = pi/2, arr.pos = .6)
}



transpath = "/home/georgia/afib/samplesout/trans/"


trans = list.files(transpath, pattern = "*.csv") 

for(i in 1:length(merge)){
  findm = paste(transpath, trans[i], sep = "")
  assign(trans[i], read.csv(findm))
  assign(trans[i], get(trans[i])[,-1])
}

path = "/home/georgia/afib/samplesout/tdiagram/"

for(i in 1:length(trans)){
  tr = rowprop(get(trans[i]))
  jpeg(filename = paste(path, "tdiagram", substr(trans[i], 6, 10), ".jpeg", sep = ""), width = 900, height = 700)
  st = c("S", "R", "L")
  dimnames(tr) = list(st)
  colnames(tr) = st
  markdia(round(tr, 3))
  dev.off()
}




rowprop = function(mat){
  newmat = matrix(rep(0,9), ncol = 3)
  for(i in 1:3){
    for(j in 1:3){
      if(sum(unlist(mat[,i]))!=0){newmat[j, i] = unlist(mat[j, i])/sum(unlist(mat[,i]))}
      else{mat[j,i]=0}
    }
  }
  newmat
}
samptotrans = function(samp){
  mat = matrix(rep(samp[4], 9)*samp[7:15], ncol = 3)
  mat = rowprop(mat)
  mat
}

avgasamp = function(samp){
  afib = NULL; non = NULL;
  for(i in 1:nrow(samp)){
    ind = samptotrans(samp[i,])
    if(samp[i,6]==TRUE){
      if(is.null(afib)){
        afib = ind
      }
      else{
        afib = (afib*.75)+(ind*.25)
      }
    }
    else{
      if(is.null(non)){
        non = ind
      }
      else{
        non = (non*.75)+(non*.25)
      }
    }
  }
  if(dim(table(samp[,6]))!=2){
    if(is.null(non)){
      non = matrix(rep(0, 9), ncol = 3)
    }
    else{
      afib = matrix(rep(0,9), ncol = 3)
    }
  }
  list(afib, non)
}

avgtafib = NULL
avgtnon = NULL
for(i in 1:length(samps)){
  avgtafib = c(avgtafib, paste("avgtafib", substr(samps[i], 6, 10), sep = ""))
  avgtnon = c(avgtnon, paste("avgtnon", substr(samps[i], 6, 10), sep = ""))
}

for(i in 1:length(samps)){
  a = avgasamp(get(samps[i]))
  assign(avgtafib[i], a[[1]])
  assign(avgtnon[i], a[[2]])
}


path = "/home/georgia/afib/samplesout/tdiagram/"



for(i in 1:length(trans)){
  print(i)
  tr = get(avgtafib[i])
  tr = matrix(tr, ncol = 3)
  jpeg(filename = paste(path, avgtafib[i], ".jpeg", sep = ""), width = 900, height = 700)
  st = c("S", "R", "L")
  dimnames(tr) = list(st)
  colnames(tr) = st
  markdia(round(tr, 3))
  dev.off()
  tr = get(avgtnon[i])
  print(tr)
  tr = matrix(tr, ncol = 3)
  jpeg(filename = paste(path, avgtnon[i], ".jpeg", sep = ""), width = 900, height = 700)
  st = c("S", "R", "L")
  dimnames(tr) = list(st)
  colnames(tr) = st
  markdia(round(tr, 3))
  dev.off()
}
