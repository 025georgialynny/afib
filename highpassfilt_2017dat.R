library(R.matlab);library(forecast);
library(plotly)
library(signal)
library(pracma)


#fn<-file.choose();
file.dir<-'/home/georgia/afib/oldrez/training2017'

filelist<-list.files(path = file.dir, recursive = TRUE, pattern = '*.mat',full.names = TRUE)


ref<-read.csv(paste(file.dir,"REFERENCE-original.csv",sep="/"),header=FALSE);

table(ref[,2]);
#assuming tab separated values with a header
datalist = lapply(filelist, function(x) R.matlab::readMat(x)$val)

n.list<-length(datalist);


l.secs = NULL
for(i in 1:n.list){
  l.secs = c(l.secs, length(datalist[[i]])/300)
  datalist[[i]] = t(datalist[[i]])
}



plotdat = function(index){
  plot(datalist[[index]], type = "l", main = paste(ref[index, 1], ref[index, 2], sep = ":"), ylab = "Amplitude (mV)", xlab = "Time Passed (300 Hz/sec)")
}


plotdat.ly = function(index){
  plot.dat = cbind.data.frame(datalist[[index]], c(1:nrow(datalist[[index]])))
  print(head(plot.dat))
  fit.pl = lm.fit(index)
  plot.lm = predict(fit.pl)
  meet = NA
  pl = .001
  while(is.na(meet) & pl< 1){
    meet = which(plot.lm >(l.mean[index]-pl)&plot.lm <(l.mean[index]+pl))[1]
    pl = pl+.001}
  print(meet)
  print(head(plot.lm))
  colnames(plot.dat) = c("amp", "Sample")
  plot_ly(plot.dat, x = ~Sample, y = ~amp, type = "scatter", mode = "lines") %>%
    add_lines(y = l.mean[index]) %>% add_lines(x = c(1:nrow(datalist[[index]])), y = plot.lm)%>% 
    add_lines(x = meet) %>% layout(title = paste(ref[index,1], ref[index, 2], sep = ":"), xaxis = list(title("SAMPLE(HZ)")),
                                   yaxis = list(title("Amplitude")))
}

l.samps = NULL
for(i in 1:n.list){
  l.samps = c(l.samps, nrow(datalist[[i]]))
}

l.mean = NULL
for(i in 1:n.list){
  l.mean = c(l.mean, mean(datalist[[i]]))
}


lm.fit = function(index){
  d = cbind.data.frame(datalist[[index]], c(1:nrow(datalist[[index]])))
  colnames(d) = c("y", "x")
  return(lm(y~x, data = d))
}
addline = function(fit){
  abline(fit, col = "red", lwd = 2)
}

######################################FILTER AND EXTRACT BELOW
filt = function(index, hz = .08){##########High pass filter at 8hz, filtfilt function from signal lib
  
  bf = butter(3, hz, type = "high")
  Fs = 300
  z = filtfilt(bf, datalist[[index]])
  return(z)
}

plotfilt = function(index){ #----------------Plots filtered data over reg data with R-peaks 
  z = filt(index)
  t = c(1:nrow(datalist[[index]]))
  zmark= cbind(z, t)
  fp = findpeaks(z, minpeakheight = (mean(z)+sd(z)), minpeakdistance = 100)
  filtplot = cbind.data.frame(t, datalist[[index]])
  colnames(filtplot) = c("Sample(Hz)", "Amplitude")
  colnames (zmark) = c("Sample(Hz)", "Amplitude")
  plot_ly(data = filtplot, x = ~`Sample(Hz)`, y = ~Amplitude, type = "scatter", mode = "lines") %>%
    add_lines(x = t, y = z) %>% 
    add_markers(x = fp[,2], y = fp[,1]) %>%
    layout(title = paste(ref[index,1], ref[index, 2], sep = ":"))
}
extrr = function(index){#-----------------------extracts RR intervals from filtered data
  z = filt(index)
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


filt.datalist = lapply(c(1:n.list),extrr)

for(i in 1:n.list){
  name = paste("/home/georgia/afib/filt2017/", ref[i,1], ".csv", sep = "")
  write.csv(filt.datalist[[i]], name, row.names = F)
}

