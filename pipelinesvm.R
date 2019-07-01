



library(e1071)
path = "/home/georgia/afib/"


traintrans = read.csv(paste(path, "train2017morefeatstrans.csv", sep = ""))[,-1]
traintru = read.csv(paste(path, "REFERENCEt2017.csv", sep = ""), header = F)
removed = which(traintrans$rr==-99)
traintrans = traintrans[-removed, ]
traintru = traintru[-removed,]

getsamp = function(){
  samp2017 = read.csv("/home/georgia/afib/samp2017morefeatstrans.csv")    
  samp2017= samp2017[,-1] 
  return(samp2017[,c(7,11,12)])
}
getans2017 = function(){
  ans2017 = read.csv("/home/georgia/afib/sample2017/validation/REFERENCE.csv", header = F)
  ans2017 = ans2017[-288,]
  return(ans2017[,2])
}
getansid = function(){
  ans2017 = read.csv("/home/georgia/afib/sample2017/validation/REFERENCE.csv", header = F)
  ans2017 = ans2017[-288,]
  return(ans2017[,1])
}

train2017 = cbind.data.frame(traintrans$ID, traintru[,2], traintrans[,2:13])

colnames(train2017) = c("id", "hb", names(traintrans[2:13]))


three = c(2, 8, 12, 13)

maintrain = train2017[,three]

test = getsamp()
testans = getans2017()

pipe = function(classifier, co, gam, sub = NULL, tsub = NULL, asub = NULL){
  nextpipe = NULL
  if(is.null(sub)){sub = maintrain}
  norm = which(sub$hb == classifier)
  normdone = rep(F, nrow(sub))
  normdone[norm] = T
  normdone = data.frame(normdone)
  normdone = cbind.data.frame(normdone, sub[,-1])
  names(normdone) = c("norm", "rl", "RRVAR", "RR200")
  wts = table(normdone$norm)/nrow(normdone)
  norm.fit =svm(norm~., normdone, kernel= "radial", cost = co, gamma = gam, scale=T,
                type = 'C', fitted = T, shrinking = T, tolerance = .00001,
                epsilon = .0000001)
  

  if(is.null(asub)){
    tsub = test; asub = testans}
  
  normpred = predict(norm.fit, tsub)
  
  sans = rep(F, length(asub))
  nsamp = which(asub == classifier)
  sans[nsamp] = T
  
  a = sans == normpred
  print(classifier)
  print(mean(a))
  print(table(sans, normpred))
  return(normpred)
}
#pipe("N", 10000, .5)
#pipe("O", 100, 1/3)
#pipe("A", 10, 1/3)
id = getansid()



noise = pipe("~", 10, 35)#10, 35(1)



minusnoise = testans[-which(noise == T)]
minusnoisetrain = maintrain[-which(maintrain$hb=="~"),]
minusnoisetest = test[-which(noise==T),]
noise = cbind.data.frame(id,noise)
colnames(noise) = c("id", "noise")

norm = pipe("N", 500, 50, minusnoisetrain, minusnoisetest, minusnoise)#20,35(1)#100,25 .9191854(2), 1000,25 .9435152 (MAXIT)  (3);
######################################                                # 500,25 0.938171(4), 500,50 .953845

minusnorm = minusnoise[-which(norm==T)]
minusnormtrain = minusnoisetrain[-which(minusnoisetrain$hb=="N"),]
minusnormtest = minusnoisetest[-which(norm==T),]
norm = data.frame(norm)
norm = cbind(noise[-which(noise[,2]==T),1], norm)
colnames(norm) = c("id", "norm")


af = pipe("A", 1000, 25, minusnormtrain, minusnormtest, minusnorm)#20,35(1) 

af = data.frame(af)
af = cbind(norm[-which(norm[,2]==T),1], af) 
colnames(af) = c('id', "af")

ans = merge(noise, norm, all = T, by = "id")
ans = merge(ans, af, all = T, by = "id")

ans[is.na(ans)] <- F


#((28/29)+(142/(142+20+8))+(36/50)+(70/84))/4

predans = rep("p", 299)
for(i in 1:nrow(ans)){
  if(ans[i,2]==T){
    predans[i] = "~"
  }
  else if (ans[i,3]==T){
    predans[i] = "N"
  }else if(ans[i,4]==T){
    predans[i] = "A"
  }else{
    predans[i] = "O"
  }
}

calf = matrix(rep(0, 16), ncol = 4)
testans = getans2017()
for(i in 1:length(testans)){
  if(testans[i]=="N"){
    if(predans[i] == "N"){
      calf[1,1] = calf[1,1]+1
    }else if(predans[i]=="A"){
      calf[1,2] = calf[1,2]+1
    }else if(predans[i]=="O"){
      calf[1,3] = calf[1,3]+1
    }else{
      calf[1,4] = calf[1,4]+1
    }
  }else if(testans[i]== "A"){
    if(predans[i] == "N"){
      calf[2,1] = calf[2,1]+1
    }else if(predans[i]=="A"){
      calf[2,2] = calf[2,2]+1
    }else if(predans[i]=="O"){
      calf[2,3] = calf[2,3]+1
    }else{
      calf[2,4] = calf[2,4]+1
    }
  }else if(testans[i]== "O"){
    if(predans[i] == "N"){
      calf[3,1] = calf[3,1]+1
    }else if(predans[i]=="A"){
      calf[3,2] = calf[3,2]+1
    }else if(predans[i]=="O"){
      calf[3,3] = calf[3,3]+1
    }else{
      calf[3,4] = calf[3,4]+1
    }
  }else{
    if(predans[i] == "N"){
      calf[4,1] = calf[4,1]+1
    }else if(predans[i]=="A"){
      calf[4,2] = calf[4,2]+1
    }else if(predans[i]=="O"){
      calf[4,3] = calf[4,3]+1
    }else{
      calf[4,4] = calf[4,4]+1
    }
  }
}

f1 = (2*calf[1,1]/(sum(calf[,1])+sum(calf[1,]))+
        2*calf[2,2]/(sum(calf[,2])+sum(calf[2,]))+
        2*calf[3,3]/(sum(calf[,3])+sum(calf[3,])))/3

f1
