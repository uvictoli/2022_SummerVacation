# -*- coding: utf-8 -*-
"""
Created on Wed Oct 12 03:05:22 2016

@author: Naver
"""

from scipy import signal
import matplotlib.pyplot as plt

## window size set
window=400
dimension=10

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

##2. compute spectral power
fout = open("p.train", "w")
for i in range(0,len(train)-window,window):
    wd = train[i:i+window]
    f, Pxx_den = signal.welch(wd, window/10.0) #100
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
        #print binpos, pavg
        if p != 0: fout.write(",")
        fout.write("%.3f" % pavg)
        if p > dimension: break
        p += 1
        if i == 0: plt.bar(binpos,pavg)
    fout.write("\n")
fout.close()

plt.show()
##3. draw graph
plt.semilogy(f, Pxx_den)
plt.xlim([0,20])
plt.xlabel('frequency [Hz]')
plt.ylabel('PSD [V**2/Hz]')
plt.show()
