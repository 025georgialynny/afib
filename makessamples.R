

path = "/home/georgia/afib/files/csv/" #############path for files - change to your directory
library(dplyr)
files = list.files(path, pattern = "*.csv") ############pattern of merge files in afib/oldmerge
for(i in 1:length(files)){
  readfile = paste(path, files[i], sep="")
  assign(files[i], read.csv(readfile))
}#################read in



rmlong = function(file, i = nrow(file)){###########################remove any readings of rr intervals greater than 1.5 seconds
  rm = NULL
  for(j in 1:i){
    if(file[j,1]>=1.5){
      rm = c(rm, j)
    }
  }
  if(!is.null(rm)){
    file = file[-rm,]}
  file
}

for(i in 3:length(files)){##################removes for all files
  f = rmlong(get(files[i]))
  assign(files[i], f)
}


afibmean = function(file, i = nrow(file)){####################returns running mean for files passed in corresponding to each row entry
  mean = file[1,1]
  tmean = NULL
  for( j in 1:i){
      mean = .75*mean + .25*file[j, 1]
      tmean = c(tmean, mean)
  }
  tmean
}

secs = function(file, i = nrow(file)){########################returns running seconds for files passed in corresponding to each row entry
  sec = 0
  sex = NULL
  for (j in 1:i){
    sec = sec + file[j, 1]
    sex = c(sex, sec)
  }
  sex
}

for(i in 1:length(files)){###############running mean for each file
  me = afibmean(get(files[i]))
  assign(files[i], cbind(get(files[i]), me))
}

for(i in 1:length(files)){################running seconds for each entry
  sec = secs(get(files[i]))
  assign(files[i], cbind(get(files[i]), sec))
}



################## rr, samp, n, rmean, rsec

#################################plotting

par(mfrow = c(2, 1), mar = c(5,5, 1,1), family = "serif")
rmeanplot = function(fullfile){
  par(mfrow = c(2, 1), mar = c(5,5, 1,1), family = "serif")
  colnames(fullfile) = c("rr", "samp", "afib", "rmean", "rsec")
  plot(fullfile[,5], fullfile[,4], type = "l", lwd = 0, col = "red4",
       xlab = "Seconds", ylab = "Running Mean", cex.lab = 2, cex.axis = 1.5)
  fib = which(fullfile[,3] =="B")
  if(length(fib) >0){
    points(fullfile[fib, 5], rep(4, length(fib)), cex = .5, col = "darkolivegreen3", type = "h")}
  points(fullfile[,5], fullfile[,4], type = "l", lwd = .5, col = "red4")
  
  plot(fullfile[,5], fullfile[,1], type = "l", lwd = 0, col = "dodgerblue4",
       xlab = "Seconds", ylab = "RR-Interval", cex.lab = 1.75, cex.axis = 1.5)
  if(length(fib) >0){
    points(fullfile[fib, 5], rep(4, length(fib)), cex = .5, col = "darkolivegreen3", type = "h")}
  points(fullfile[,5], fullfile[,1], type = "l", lwd = .5, col = "dodgerblue4")
}

path = "/home/georgia/afib/mergeplots/" ################change this path for your plots               


################3unmask these and change path to get plots
#for(i in 3:length(files)){
 # jpeg(paste(path, "plot", substr(files[i], 6, 10), ".jpeg", sep = ""), width = 1533, height = 792)
  # 2. Create the plot
  #rmeanplot(get(files[i]))
  # 3. Close the file
#  dev.off()
#}


################## rr, samp, n, rmean, rsec

classify= function(file, i = nrow(file)){################################add a column categorizing each rr interval as long short or regular
  tran = NULL
  for(j in 1:i){
    if(file[j,1]>(1.15*file[j,4])){
      tran = c(tran, 3) ######################                     3 = L
    }else if(file[j,1]<(.85*file[j,4])){
      tran = c(tran, 1) ####################                        1 = S
    }else{
      tran = c(tran, 2)  ###################                        2 = R
    }
  }
  tran
}

for(i in 3:length(files)){############classify each rr interval for each file
  trans = classify(get(files[i]))
  assign(files[i], cbind(get(files[i])[,-6], trans))
}


transmat = function(file, mat){###############creates transition matrix based on classify function additions above
  for(i in 1:(nrow(file)-1)){
    from = file[i,6]
    to = file[i+1, 6]
    mat[to, from] = mat[to, from] + 1
  }
  mat
}


trans = NULL
samps = NULL
for (i in 1:length(files)){##########creates trans and samps lists for easy access to trans and samps data
  trans = c(trans, paste("trans", substr(files[i], 6, 10), sep = ""))
  samps = c(samps, paste("samps", substr(files[i], 6, 10), sep = ""))
}


for(i in 3:length(files)){#################makes transition matrices
  mat = matrix(rep(0, 9), ncol = 3)
  assign(trans[i], transmat(get(files[i]), mat))
}

for(i in 3:length(trans)){#################prints transition matrices
  print(trans[i])
  print(get(trans[i]))
}

props = function(mat){#####################function to turn transition matrices into proportions instead of tallies of individual beats
  summy = sum(mat)
  mat = mat/summy
  unlist(list(mat))
}

sampfib = function(file, i = nrow(file)){######################function determine if a sample group should be classified as afib or not
  afib = length(which(file[,3]=="B"))
  non = length(which(file[,3]!="B"))
  afib>non
}

sub = function(file, i = nrow(file), id){#######################3function to split individual data frames into samples of 45 secons each
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


for(j in 3:length(files)){###################makes each file into sample
  assign(samps[j], sub(get(files[j]), id = substr(files[j], 6, 10)))
}


for( i in 3:length(files)){################################give meaningful col names to each file used (trans, files, and samps)
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

path = "/home/georgia/afib/samplesout/"#########################33change path for yourself

for(i in 3:length(files)){###############################################3write out all new data
  write.csv(get(samps[i]), paste(path, "samps/", samps[i], ".csv", sep = ""))
  
  write.csv(get(trans[i]), paste(path, "trans/",trans[i], ".csv", sep = ""))
  
  write.csv(get(files[i]), paste( path,"merge/", files[i], sep = ""))
}
