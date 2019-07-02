library(R.matlab);library(forecast);


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


hist(l.secs, breaks = 15, main = "Histogram of 2017 Challenge Sample Lengths", xlab = "Time in Seconds", ylab = "# of Samples")


plot(datalist[[1]], type = "l")
plot(datalist[[210]], type = "l")

plotdat = function(index){
  plot(datalist[[index]], type = "l", main = paste(ref[index, 1], ref[index, 2], sep = ":"), ylab = "Amplitude (mV)", xlab = "Time Passed (300 Hz/sec)")
}
plotdat(1)
plotdat(210)
plotdat(100)
plotdat(2012)
plotdat(123)
af = which(ref[,2]=="A")
plotdat(af[1])
plotdat(af[33])
plotdat(af[2])
plotdat(af[770])
plotdat(af[210])
oth = which(ref[,2]=="O")
plotdat(oth[23])
plotdat(oth[291])
noise = which(ref[,2]=="~")
plotdat(noise[1])
plotdat(noise[2])
plotdat(noise[33])
norm = which(ref[,2] == "N")
plotdat(norm[2])

l.samps = NULL
for(i in 1:n.list){
  l.samps = c(l.samps, nrow(datalist[[i]]))
}
plotdat(210)

l.mean = NULL
for(i in 1:n.list){
  l.mean = c(l.mean, mean(datalist[[i]]))
}



over = which(l.mean>(mean(l.mean+2*sd(l.mean))))
under = which(l.mean<(mean(l.mean-2*sd(l.mean))))
wayover =which(l.mean>(mean(l.mean+4*sd(l.mean))))

plotdat(over[3])
plotdat(under[80])

table(ref[c(over,under), 2])



lm.fit = function(index){
  d = cbind.data.frame(datalist[[index]], c(1:nrow(datalist[[index]])))
  colnames(d) = c("y", "x")
  return(lm(y~x, data = d))
}
addline = function(fit){
  abline(fit, col = "red", lwd = 2)
}

fit.210 = lm.fit(210)
plotdat(210)
abline(fit.210, col = "red", lwd = 2)

fit.432 = lm.fit(432)
plotdat(432)
abline(fit.432, col = "red", lwd = 2)


fit.823 = lm.fit(823)
plotdat(823)
addline(fit.823)
fit.823

fit.437 = lm.fit(437)
plotdat(437)
addline(fit.437)
fit.437

fit.4832 = lm.fit(4832)
plotdat(4832)
addline(fit.4832)
fit.4832


slope = NULL
for(i in 1:n.list){
  fit.a = lm.fit(i)
  slope = c(slope, fit.a$coefficients[2])
}



slope.over = which(slope>(2*sd(slope)+mean(slope)))
length(slope[slope.over])
slope.under = which(slope<(mean(slope)-2*sd(slope)))
length(slope[slope.under])



fit.2740 = lm.fit(2740)
plotdat(2740)
addline(fit.2740)
abline(h = l.mean[2740], col = "darkgreen", lwd = 2)


fit.7250 = lm.fit(7250)
plotdat(7250)
addline(fit.7250)
abline(h = l.mean[7250], col = "darkgreen", lwd = 2)

fit.504 = lm.fit(504)
plotdat(504)
addline(fit.504)
abline(h = l.mean[504], col = "darkgreen", lwd = 2)


fit.3203 = lm.fit(3203)
plotdat(3203)
addline(fit.3203)
abline(h = l.mean[3203], col = "darkgreen", lwd = 2)





fit.2390 = lm.fit(2390)
plotdat(2390)
addline(fit.2390)
abline(h = l.mean[2390], col = "darkgreen", lwd = 2)


library(plotly)

plot.ly = cbind.data.frame(datalist[[2390]], c(1:nrow(datalist[[2390]])))
plot.lm = predict(fit.2390)
colnames(plot.ly) = c("amp", "index")
plot_ly(plot.ly, x = ~index, y = ~amp, type = "scatter", mode = "lines") %>%
  add_lines(y = l.mean[2390]) %>% add_lines(x = c(1:nrow(datalist[[2390]])), y = plot.lm) 

which(plot.lm >(l.mean[2390]-.009)&plot.lm <(l.mean[2390]+.009))[1]



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
plotdat.ly(2390)
plotdat.ly(324)
plotdat.ly(2938)
plotdat.ly(3203)
plotdat.ly(234)
plotdat.ly(3240)
plotdat.ly(100)
plotdat.ly(2740)



library(signal)


bf = butter(3, .08, type = "high")
Fs = 300
t = seq(0, 10.74667, len = nrow(datalist[[2740]]))

plot(t, datalist[[2740]], type = "l")
z = filtfilt(bf, datalist[[2740]])
lines(t, z, col = "red")


filtplot = cbind.data.frame(t, datalist[[2740]])
colnames(filtplot) = c("time", "dat")
plot_ly(data = filtplot, x = ~time, y = ~dat, type = "scatter", mode = "lines") %>%
  add_lines(x = t, y = z)


filt = function(index, hz = .08, but = 3){
  
  bf = butter(but, hz, type = "high")
  Fs = 300
  z = filtfilt(bf, datalist[[index]])
  return(z)
}
library(pracma)
library(peakPick)
plotfilt = function(index, z = filt(index)){
  t = c(1:nrow(datalist[[index]]))
  zmark= cbind(z, t)
  fp = findpeaks(z, minpeakheight = (mean(z)+sd(z)), threshold = l.secs[index])
  fp2 = peakpick(z, neighlim = 150, deriv.lim = 10, peak.min.sd = .5)
  filtplot = cbind.data.frame(t, datalist[[index]])
  colnames(filtplot) = c("Sample(Hz)", "Amplitude")
  colnames (zmark) = c("Sample(Hz)", "Amplitude")
  fp2 = which(fp2 ==T)
  plot_ly(data = filtplot, x = ~`Sample(Hz)`, y = ~Amplitude, type = "scatter", mode = "lines") %>%
    add_lines(x = t, y = z) %>% 
    add_markers(x = fp[,2], y = fp[,1], size = 3) %>%
    layout(title = paste(ref[index,1], ref[index, 2], sep = ":"))
   if(F){ plot_ly(data = filtplot, x = ~`Sample(Hz)`, y = ~Amplitude, type = "scatter", mode = "lines") %>%
    add_lines(x = t, y = z) %>% 
    add_markers(x = t[fp2], y = z[fp2], size = 3)  %>%
    layout(title = paste(ref[index,1], ref[index, 2], sep = ":"))}
}

z = filt(wrong[12], .15, 2)
plotfilt(wrong[12], z)
plotfilt(8)
d = extrr(wrong[2], z)

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


filt.datalist = lapply(c(1:n.list),extrr)

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
  if(md>1){
    print(i)
    print(md)
    wrong = c(wrong, i)
  }
}






plotsect = function(index, z = filt(index), where = c(1:length(z)), min = 150, minh = (mean(z[where])+sd(z[where]))){
  t = c(1:length(z))
  t = t[where]
  z = z[where]
  t = t-(where[1]-1)
  #zmark= cbind(z, t)
  if(min == -1){
    fp = findpeaks(z, minpeakheight = (mean(z)+sd(z)))}else{
      fp = findpeaks(z, minpeakheight = minh, minpeakdistance = min)
    }
  filtplot = cbind.data.frame(t, datalist[[index]][where,])
  colnames(filtplot) = c("Sample(Hz)", "Amplitude")
  colnames (zmark) = c("Sample(Hz)", "Amplitude")
  print(length(z)); print(length(where))
  if(length(z)<length(where)){
   fpw = which(fp[,2]<where[1])
    fp = fp[-fpw,]
  }
  plot_ly(data = filtplot, x = ~`Sample(Hz)`, y = ~Amplitude, type = "scatter", mode = "lines") %>%
    add_lines(x = t, y = z) %>% 
    add_markers(x = fp[,2], y = fp[,1], size = 3) %>%
    layout(title = paste(ref[index,1], ref[index, 2], sep = ":"))
}

plotsect(wrong[1])
d = extrr2(wrong[1])

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

d = extrr2(wrong[1])
d2 = which(d[[1]] >1)
d3 = which(d[[1]]<.1)
d4 = which(d3<d2[1])
d3 = d3[d4]
d = d[[2]][d3[length(d3)]]
 
plotsect(wrong[1])
z = filt(wrong[1], .09, 4)
plotsect(wrong[1], z, where = c(d:length(z)))

extrr3(wrong[1], z, where = c(d:length(z)))

getrid = function(index){
  d = extrr2(index)
  d2 = which(d[[1]] >1)
  d3 = which(d[[1]]<.5)
  if(length(d2)>1){
  d4 = which(d3<d2[2])}else{d4 = which(d3<d2)}
  d5 = which(d3>d2[length(d2)])
  d3a = d3[d4]
  d3b = d3[d5]
  d5 = d[[2]][d3a[length(d3a)]]
  d6 = d[[2]][d3b[1]]
  if(is.na(d6)){d6 = length(datalist[[index]])}
  z = filt(index, .08, 4)
  
  where = c(d5:d6)
  zw = z[where]
  zw = zw[-which(zw>(mean(zw)+sd(zw)))]
  h = mean(zw)+sd(zw)
  
  plotsect(index,z, min = -1)
  plotsect(index, z = z, where = where, min = 100, minh = h)
   extrr3(index, z, where =where, min = 100, minh = h)
}

getrid(wrong[4])
wrongrep = NULL
wrongrep = lapply(1:length(wrong), function(x){ print(x); getrid(wrong[x])[[1]]})

filt.datalist = lapply(1:n.list, function(x) extrr(x))

for(i in 1:length(wrong)){
  filt.datalist[[wrong[i]]] = wrongrep[[i]]
}

for(i in 1:n.list){
  name = paste("/home/georgia/afib/filt2017/", ref[i,1], ".csv", sep = "")
  write.csv(filt.datalist[[i]], name, row.names = F)
}
