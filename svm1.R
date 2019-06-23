path = "/home/georgia/afib/samplesout/sampsnotrans/"
library(e1071)
library(MASS)
allsamps = list.files(path, "*.csv")

for(i in 1:length(allsamps)){
  p = paste(path, allsamps[i], sep = "")
  assign(allsamps[i], read.csv(p)[,-1])
}



set.seed(3)
normal = samps04015.csv[1:2,]
AF = samps04015.csv[1:2,]
for(i in 1:length(allsamps)){
  s1 = get(allsamps[i])
  non = subset(s1, afib == F)
  afibr = subset(s1, afib == T)
  normal = rbind(normal, non)
  AF = rbind(AF, afibr)
}

normal = normal [c(-1,-2),]
AF = AF[c(-1,-2),]
all = rbind(normal, AF)
foldn = sample(nrow(normal), nrow(normal), replace = F)
normal = normal[foldn,]
foldn = sample(nrow(AF), nrow(AF), replace = F)
AF = AF[foldn,]
all = rbind.data.frame(normal[1:5000,], AF[1:5000,])
all = all[, 6:15]

set.seed(1)


    svm.fit =svm(afib~., all, kernel= "radial", cost = 10, gamma = 1/18, scale=T,
                                     type = 'C', fitted = F, shrinking = T, tolerance = .01,
                                     epsilon = .0000001)


  
  
  
  
  
  
  samp2017 = read.csv("/home/georgia/afib/samp2017wqrstrans.csv")    
  samp2017= samp2017[,-1]                          
  
  ans2017 = read.csv("/home/georgia/afib/sample2017/validation/REFERENCE.csv", header = F)
  ans2017 = ans2017[-288,]
  
  ans = NULL
  for(i in 1:nrow(ans2017)){
    if(ans2017[i,2]=="A"){ans=c(ans, T)}
    else(ans = c(ans, F))
  }
  
  
pred = predict(svm.fit, rbind(normal, AF)[,7:15])
mean(pred == rbind(normal, AF)[,6])
s2017 = predict(svm.fit, samp2017)
a = ans==s2017
mean(a)

table(ans, s2017)



############################3  ------------kpca and pca models

##kpca
library(colorspace)
cols = sequential_hcl(10, "bupu")
cols = cols[c(1,5)]
library(kernlab)
library(plotly)

normalc = rep(cols[1], nrow(normal))
afc = rep(cols[2], nrow(AF))


allfeatsMIT = rbind(normal[1:5000,], AF[1:5000,])[6:15]

fold = sample(rep(1:10), nrow(all), replace = T)
all = all[which(fold == 2),]
cols = c(normalc[1:5000], afc[1:5000])
cols = cols[which(fold==2)]
m = ifelse(cols=="#540046", "AFIB", "NONAFIB")
cols = cbind(cols, m)
pca.re<-kpca(~. , all[,-1],kernel="rbfdot",
             kpar=list(sigma=.1),features=3)
  kpcare = (rotated(pca.re))
colnames(kpcare) = c("kp1", "kp2", "kp3")
kpcare = data.frame(kpcare)


l <- list(
  font = list(
    family = "sans-serif",
    size = 18,
    color = "#000"))
p <- plot_ly(data = kpcare,x = ~kp1, y = ~kp2, z = ~kp3,
             colors = cols[,1],
             color = m,
             type = "scatter3d",
             mode = 'markers',
             showlegend = F,
             marker = list(size = 2.5,
                           width = 5)) %>%
  layout(legend = l)
ax = list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)

p.legend = plot_ly(x = 0, y = c(1.05,1.06),marker=list(size = 20), showlegend = F, colors = names(table(cols[,1])), 
                   color= names(table(m)), text = c("AFIB", "NONAFIB"))  %>% add_markers() %>%
  add_text(textposition = "centerright", size = 10, mode = "markers") %>%
  layout( yaxis = ax, xaxis = ax)
p
p.legend

subplot(p, p.legend, widths = c(.5, .5))

all = rbind.data.frame(normal, AF)
all = data.frame(all)

set.seed(3)
kda1 = kda(all, x.group = names(table(m)))
all = rbind.data.frame(normal[1:5000,], AF[1:5000,])
all = all[, 6:15]