


import glob

qrsreadin = (glob.glob("/home/georgia/afib/files/csv/qrs*.txt"))

qrsnames = []
qrsnewnames = []
for i in range(len(qrsreadin)):
    qrsnames.append(qrsreadin[i].rsplit("/csv/")[1])
    qrsnames[i] = qrsnames[i][:-4]
    qrsnewnames.append(qrsnames[i]+"Mat")
    

for i in range(len(qrsreadin)):
    globals()[qrsnames[i]] = open(qrsreadin[i], "r")  


for i in range(len(qrsnames)):
    globals()[qrsnewnames[i]]=[]
    for line in globals()[qrsnames[i]]:
        globals()[qrsnewnames[i]].append(line.split("\t"))
        
for i in range(len(qrsnames)):
    for j in range(len(globals()[qrsnewnames[i]])):
        globals()[qrsnewnames[i]][j][2] = globals()[qrsnewnames[i]][j][2][:-1]
        for k in range(3):
            globals()[qrsnewnames[i]][j][k] = float(globals()[qrsnewnames[i]][j][k])
            
            
            
            
atrreadin = (glob.glob("/home/georgia/afib/files/csv/atr*.txt"))

atrnames = []
atrnewnames = []
mergenam = []
for i in range(len(atrreadin)):
    atrnames.append(atrreadin[i].rsplit("/csv/")[1])
    atrnames[i] = atrnames[i][:-4]
    atrnewnames.append(atrnames[i]+"Mat")
    mergenam.append("merge"+atrnames[i][-5:])
    

for i in range(len(atrreadin)):
    globals()[atrnames[i]] = open(atrreadin[i], "r")  


for i in range(len(atrnames)):
    globals()[atrnewnames[i]]=[]
    for line in globals()[atrnames[i]]:
        globals()[atrnewnames[i]].append(line.split(","))
        
        

for i in range(len(atrnames)):
    for j in range(len(globals()[atrnewnames[i]])):
        globals()[atrnewnames[i]][j][0] = ' '.join(globals()[atrnewnames[i]][j][0].split())
        if(j == 0):
            globals()[atrnewnames[i]][j][0]  = globals()[atrnewnames[i]][j][0].replace("d ", "d_")
            globals()[atrnewnames[i]][j][0]  = globals()[atrnewnames[i]][j][0].replace("n ", "n_")
        globals()[atrnewnames[i]][j][0] = globals()[atrnewnames[i]][j][0].split(" ")
        globals()[atrnewnames[i]][j][0].pop(2)
        globals()[atrnewnames[i]][j][0].pop(2)
        globals()[atrnewnames[i]][j][0].pop(2)
        globals()[atrnewnames[i]][j][0].pop(2)
        globals()[atrnewnames[i]][j][0].pop(0)
        if(j!=0):
            globals()[atrnewnames[i]][j][0][0] = float( globals()[atrnewnames[i]][j][0][0])
            
            
k = 1
for i in range(len(qrsnewnames)):
    globals()[mergenam[i]] = []
    k = 1
    for j in range(len(globals()[qrsnewnames[i]])):
        add = []
        if k+1 < len(globals()[atrnewnames[i]]):
            if globals()[qrsnewnames[i]][j][2] <= globals()[atrnewnames[i]][k+1][0][0]:
                add = [globals()[qrsnewnames[i]][j][1], globals()[qrsnewnames[i]][j][2],
                        globals()[atrnewnames[i]][k][0][1][-1:]]
                globals()[mergenam[i]].append(add)
            else:
                k = k+1
                add = [globals()[qrsnewnames[i]][j][1], globals()[qrsnewnames[i]][j][2],
                        globals()[atrnewnames[i]][k][0][1][-1:]]
                globals()[mergenam[i]].append(add)
        else:
            add = [globals()[qrsnewnames[i]][j][1], globals()[qrsnewnames[i]][j][2],
                        globals()[atrnewnames[i]][k][0][1][-1:]]
            globals()[mergenam[i]].append(add)
            
import csv         
path = "/home/georgia/afib/files/csv/"
for i in range(len(atrnames)):
    with open(path+mergenam[i]+".csv", mode = 'w') as writec:
        writer = csv.writer(writec, delimiter = ",", quotechar = "'", quoting = csv.QUOTE_MINIMAL)
        for k in range(len(globals()[mergenam[i]])):
            writer.writerow(globals()[mergenam[i]][k])