#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 17 17:29:48 2019

@author: georgia
"""

import glob

qrsreadin = (glob.glob("/home/georgia/afib/sample2017/training2017RR/*.txt"))

rr = []
rrout = []
rr2 = []

for i in range(len(qrsreadin)):
    rr.append(qrsreadin[i].rsplit("/training2017RR/")[1])
    rr[i] = rr[i][:-4]
    rrout.append(rr[i]+".csv")
    rr2.append(rr[i]+"out")
    
    
for i in range(len(qrsreadin)):
    globals()[rr[i]] = open(qrsreadin[i], "r")
    globals()[rr2[i]]=[]
    for line in globals()[rr[i]]:
        globals()[rr2[i]].append(line.split('\n')[0])
    for j in range(len(globals()[rr2[i]])):
        globals()[rr2[i]][j] = float(globals()[rr2[i]][j])
    import csv         
    path = "/home/georgia/afib/training2017RR/"
    with open(path+rrout[i], mode = 'w') as writec:
        writer = csv.writer(writec, delimiter = ",", quotechar = "'", quoting = csv.QUOTE_MINIMAL)
        writer.writerow(globals()[rr2[i]])
    globals()[rr[i]].close()