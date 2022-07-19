# -*- coding: utf-8 -*-
"""
Created on Wed Oct 12 03:05:22 2016

@author: Naver
"""

from scipy import signal
import matplotlib.pyplot as plt

##1. read original ecg
train = []
fp = open("ecg.txt","r")
for line in fp:
    line = line.strip("\r\n")
    psd = line.split("\t")
    val = float(psd[1])
    train.append(val)
plt.plot(train)
plt.show()
##2. transform test    
wd = train[0:400]
f, Pxx_den = signal.welch(wd, 40) #100
plt.plot(f,Pxx_den)
plt.show()
##3. do slotting
tmp = {}
p=0
psum=0
for fd in f:
    binpos = int(fd)
    if not binpos in tmp:
        tmp[binpos]=0
    tmp[binpos] += Pxx_den[p]
    psum += Pxx_den[p]
    p += 1
p=0
for binpos in tmp:
    pavg = tmp[binpos] / psum
    if p > 10: break
    p += 1
    plt.bar(binpos,pavg)
plt.show()
