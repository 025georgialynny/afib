

path = "/home/georgia/afib/files/csv/"
library(dplyr)
files = list.files(path, pattern = "*.csv")
for(i in 1:length(files)){
  readfile = paste(path, files[i], sep="")
  assign(files[i], read.csv(readfile))
}

head(merge00735.csv)
      

table(merge00735.csv[,3])

afibmean = function(file, i = nrow(file)){
  mean = file[1,1]
  tmean = NULL
  for( j in 1:i){
      mean = .75*mean + .25*file[j, 1]
      tmean = c(tmean, mean)
  }
  tmean
}

secs = function(file, i = nrow(file)){
  sec = 0
  sex = NULL
  for (j in 1:i){
    sec = sec + file[j, 1]
    sex = c(sex, sec)
  }
  sex
}

for(i in 1:length(files)){
  me = afibmean(get(files[i]))
  assign(files[i], cbind(get(files[i]), me))
}

for(i in 1:length(files)){
  sec = secs(get(files[i]))
  assign(files[i], cbind(get(files[i]), sec))
}
################## rr, samp, n, rmean, rsec
rmeanplot = function(fullfile){
  par(mfrow = c(2, 1), mar = c(5,5, 1,1))
  colnames(fullfile) = c("rr", "samp", "afib", "rmean", "rsec")
  plot(fullfile[,5], fullfile[,4], type = "l", lwd = 0, col = "red4",
       xlab = "Seconds", ylab = "Running Mean")
  fib = which(fullfile[,3] =="B")
  if(length(fib) >0){
    points(fullfile[fib, 5], rep(4, length(fib)), cex = .5, col = "darkolivegreen3", type = "h")}
  points(fullfile[,5], fullfile[,4], type = "l", lwd = .5, col = "red4")
  
  plot(fullfile[,5], fullfile[,1], type = "l", lwd = 0, col = "dodgerblue4",
       xlab = "Seconds", ylab = "RR-Interval")
  if(length(fib) >0){
    points(fullfile[fib, 5], rep(4, length(fib)), cex = .5, col = "darkolivegreen3", type = "h")}
  points(fullfile[,5], fullfile[,1], type = "l", lwd = .5, col = "dodgerblue4")
}
rmeanplot(merge04015.csv)  
rmeanplot(merge04043.csv)
rmeanplot(merge04048.csv)  
rmeanplot(merge04126.csv)  
rmeanplot(merge04746.csv) 
rmeanplot(merge04908.csv)
rmeanplot(merge04936.csv)
rmeanplot(merge05091.csv)  
rmeanplot(merge05121.csv)  
rmeanplot(merge05261.csv)  
rmeanplot(merge06426.csv)  
rmeanplot(merge06453.csv)  
rmeanplot(merge06995.csv)  
rmeanplot(merge07162.csv)  
rmeanplot(merge07859.csv)  
rmeanplot(merge07879.csv)
rmeanplot(merge07910.csv)
rmeanplot(merge08215.csv)
rmeanplot(merge08219.csv)
rmeanplot(merge08378.csv)
rmeanplot(merge08405.csv)
rmeanplot(merge08434.csv)
rmeanplot(merge08455.csv)


################## rr, samp, n, rmean, rsec

classify= function(file, i = nrow(file)){
  tran = NULL
  for(j in 1:i){
    if(file[j,1]>(1.15*file[j,4])){
      tran = c(tran, 1) ######################                     1 = L
    }else if(file[j,1]<(.85*file[j,4])){
      tran = c(tran, 3) ####################                        2 = S
    }else{
      tran = c(tran, 2)  ###################                        3 = R
    }
  }
  tran
}

for(i in 3:length(files)){
  trans = classify(get(files[i]))
  assign(files[i], cbind(get(files[i])[,-6], trans))
}


transmat = function(file, mat){
  for(i in 1:(nrow(file)-1)){
    from = file[i,6]
    to = file[i+1, 6]
    mat[to, from] = mat[to, from] + 1
  }
  mat
}


trans = NULL
samps = NULL
for (i in 1:length(files)){
  trans = c(trans, paste("trans", substr(files[i], 6, 10), sep = ""))
  samps = c(samps, paste("samps", substr(files[i], 6, 10), sep = ""))
}


for(i in 3:length(files)){
  mat = matrix(rep(0, 9), ncol = 3)
  assign(trans[i], transmat(get(files[i]), mat))
}

for(i in 3:length(trans)){
  print(trans[i])
  print(get(trans[i]))
}

props = function(mat){
  summy = sum(mat)
  mat = mat/summy
  unlist(list(mat))
}

sampfib = function(file, i = nrow(file)){
  afib = length(which(file[,3]=="B"))
  non = length(which(file[,3]!="B"))
  afib>non
}

sub = function(file, i = nrow(file), id){
  subs = NULL
  sub1 = NULL
  sums = 0
  s = 1
  c = 0
  for(j in 1:i){
    sums = sums+file[j,1]
    if(sums>=45){
      mat = matrix(rep(0, 9), ncol = 3)
      transsub = transmat(file[s:j,], mat)
      translist = props(transsub)
      sub1 = c(id, file[s,2], file[j,2], j-s, sums, sampfib(file[s:j,]), translist)
      subs = c(subs, sub1)
      sums = 0
      s = j
      c = c+1
    }
    else if(j == i){
      mat = matrix(rep(0, 9), ncol = 3)
      transsub = transmat(file[s:j,], mat)
      translist = props(transsub)
      sub1 = c(id, file[s,2], file[j,2], j-s, sums, sampfib(file[s:j,]), translist)
      subs = c(subs, sub1)
      sums = 0
      s = j
      c = c+1
    }
  }
  subs = t(matrix(subs, ncol = c))
  print(head(subs))
  subs
}


for(j in 3:length(files)){
  assign(samps[j], sub(get(files[j]), id = substr(files[j], 6, 10)))
}


for( i in 3:length(files)){
  s = get(samps[i])
  f = get(files[i])
  t = get(trans[i])
  colnames(t) = c("S", "R", "L")
  rownames(t) = c("S", "R", "L")
  colnames(f) = c("RR", "samp", "afib", "rmean", "rsec", "trans")
  colnames(s) = c("id", "stsamp", "endsamp", "beats", "secs", "afib","ss", "sr", "sl", "rs", 
                  "rr", "rl", "ls", "lr", "ll")
  assign(samps[i], s)
  assign(trans[i], t)
  assign(files[i], f)
}

path = "/home/georgia/afib/samplesout/"

for(i in 3:length(files)){
  write.csv(get(samps[i]), paste(path, samps[i], ".csv", sep = ""))
  
  write.csv(get(trans[i]), paste(path, trans[i], ".csv", sep = ""))
  
  write.csv(get(files[i]), paste(path, files[i], sep = ""))
}
