library(R.matlab);library(forecast);
library(plotly)
library(pracma)

library(signal)


file.dir<-'/home/georgia/afib/oldrez/training2017'

filelist<-list.files(path = file.dir, recursive = TRUE, pattern = '*.mat',full.names = TRUE)

ref.dir = "/home/georgia/afib/oldrez"
ref<-read.csv(paste(ref.dir,"REFERENCEt2017.csv",sep="/"),header=FALSE);



datalist = lapply(filelist, function(x) R.matlab::readMat(x)$val)

n.list<-length(datalist);



l.secs = NULL
for(i in 1:n.list){
  l.secs = c(l.secs, length(datalist[[i]])/300)
  datalist[[i]] = t(datalist[[i]])
}
#hist(l.secs, breaks = 15, main = "Histogram of 2017 Challenge Sample Lengths", xlab = "Time in Seconds", ylab = "# of Samples")

l.samps = NULL
for(i in 1:n.list){
  l.samps = c(l.samps, nrow(datalist[[i]]))
}

l.mean = NULL
for(i in 1:n.list){
  l.mean = c(l.mean, mean(datalist[[i]]))
}


over = which(l.mean>(mean(l.mean+2*sd(l.mean))))
under = which(l.mean<(mean(l.mean-2*sd(l.mean))))
wayover =which(l.mean>(mean(l.mean+4*sd(l.mean))))



plotdat.ly = function(index){
  plot.dat = cbind.data.frame(datalist[[index]], c(1:nrow(datalist[[index]])))
  colnames(plot.dat) = c("amp", "Sample")
  plot_ly(plot.dat, x = ~Sample, y = ~amp, type = "scatter", mode = "lines") %>% 
    layout(title = paste(ref[index,1], ref[index, 2], sep = ":"))
}


filt = function(index, hz = .08, but = 3, t = "high", data = datalist[[index]]){
  
  bf = butter(but, hz, type = t)
  Fs = 300
  z = filtfilt(bf, data)
  return(z)
}


plotfilt = function(index, z = filt(index)){
  t = c(1:nrow(datalist[[index]]))
  zmark= cbind(z, t)
  fp = findpeaks(z, minpeakheight = (mean(z)+sd(z)), threshold = l.secs[index])
  filtplot = cbind.data.frame(t, datalist[[index]])
  colnames(filtplot) = c("Sample(Hz)", "Amplitude")
  colnames (zmark) = c("Sample(Hz)", "Amplitude")
  plot_ly(data = filtplot, x = ~`Sample(Hz)`, y = ~Amplitude, type = "scatter", mode = "lines") %>%
    add_lines(x = t, y = z) %>% 
    add_markers(x = fp[,2], y = fp[,1], size = 3) %>%
    layout(title = paste(ref[index,1], ref[index, 2], sep = ":"))
}

extrr = function(index, z = filt(index)){
  fp = findpeaks(z, minpeakheight = (mean(z)+sd(z)), minpeakdistance = 100)
  sn = sort(fp[,2])
  rrsamp = NULL
  for(i in 1:length(sn)){
    if(i == 1){rrsamp = sn[1]}else{
      rrsamp = c(rrsamp, (sn[i]- sn[i-1]))
    }
  }
  return(rrsamp/300)
}


meandrr = function(index){
  rr = extrr(index)
  drr = NULL
  for(i in 2:length(rr)){
    drr = c(drr, abs(rr[i]-rr[i-1]))
  }
  return(mean(drr))
}

wrong = NULL
for(i in 1:n.list){
  md = meandrr(i)
  if(md>.9){
    wrong = c(wrong, i)
  }
}


plotsect = function(index, z = filt(index), where = c(1:length(z)), min = 100, minh = (mean(z[where])+sd(z[where]))){
  t = c(1:length(z))
  t = t[where]
  z = z[where]
  t = t-(where[1]-1)
  if(min == -1){
    fp = findpeaks(z, minpeakheight = (mean(z)+sd(z)))}else{
      fp = findpeaks(z, minpeakheight = minh, minpeakdistance = min)
    }
  filtplot = cbind.data.frame(t, datalist[[index]][where,])
  colnames(filtplot) = c("Sample(Hz)", "Amplitude")
  print(length(z)); print(length(where))
  if(length(z)<length(where)){
    fpw = which(fp[,2]<where[1])
    fp = fp[-fpw,]
  }
  plot_ly(data = filtplot, x = ~`Sample(Hz)`, y = ~Amplitude, type = "scatter", mode = "lines", name = "Given Data") %>%
    add_lines(x = t, y = z, name = "Filtered Data") %>% 
    add_markers(x = fp[,2], y = fp[,1], size = 3, name = "R-Peaks") %>%
    layout(title = paste(ref[index,1], ref[index, 2], sep = ":"))
}


extrr2 = function(index, z = filt(index), where = c(1:length(z))){
  fp = findpeaks(z[where], minpeakheight = (mean(z[where])+sd(z[where])))
  sn = sort(fp[,2])
  rrsamp = NULL
  for(i in 1:length(sn)){
    if(i == 1){rrsamp = sn[1]}else{
      rrsamp = c(rrsamp, (sn[i]- sn[i-1]))
    }
  }
  return(list(rrsamp/300, fp[,2]))
}

extrr3 = function(index, z = filt(index), where = c(1:length(z)), min = 150, minh = (mean(z[where])+sd(z[where])) ){
  fp = findpeaks(z[where], minpeakheight = minh, minpeakdistance = min)
  sn = sort(fp[,2])
  rrsamp = NULL
  for(i in 1:length(sn)){
    if(i == 1){rrsamp = sn[1]}else{
      rrsamp = c(rrsamp, (sn[i]- sn[i-1]))
    }
  }
  return(list(rrsamp/300, fp[,2]))
}




getrid = function(index){
  d = extrr2(index)
  d2 = which(d[[1]] >1)
  d3 = which(d[[1]]<.3)
  d3diff = 0
  for(i in 2:length(d3)){
    d3diff = c(d3diff, abs(d[[2]][d3[i]]-d[[2]][d3[i-1]]))
  }
  maxdiff = which(d3diff == max(d3diff))
  while(max(d3diff) > 500){
    d3diff = d3diff[-maxdiff]
    d3 = d3[-maxdiff]
    maxdiff = which(d3diff == max(d3diff))
  }
  
  if(length(d2)>1){
    d4 = which(d3<d2[length(d2)])
    d5 = which(d3>d2[length(d2)])}else{
      d4 = which(d3<d2)
      d5 = which(d3>d2)}
  
  
  d3a = d3[d4]
  d3b = d3[d5]
  d5 = d[[2]][d3a[length(d3a)]]
  d6 = d[[2]][d3b[1]]
  if(is.na(d6)){d6 = length(datalist[[index]])}
  z = filt(index, .05, 3, t = "low")
  z = filt(index, .02, 3, t = "high", data = z)
  
  where = c(d5:d6)
  zw = abs(z[where])
  zw = zw[-which(zw>(mean(zw)+(2*sd(zw))))]
  h = mean(zw)+sd(zw)
  
  plotsect(index,z, min = -1)
  plotsect(index, min = 50, z = z)
  plotsect(index, z = z, where = where, min = 100, minh = h)
      extrr3(index, z, where =where, min = 100, minh = h)
}
























