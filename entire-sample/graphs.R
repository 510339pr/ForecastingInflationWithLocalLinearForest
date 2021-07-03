# # # build graphs. 

# # Horizon 10 # #
pred.rf = matrix(0,length(sample1.rf10p$pred)+length(sample2.rf10p$pred),1)
pred.llf = matrix(0,length(sample1.rf10p$pred)+length(sample2.rf10p$pred),1)
pred.rw = matrix(0,length(sample1.rf10p$pred)+length(sample2.rf10p$pred),1)
real = matrix(0,length(sample1.rf10p$pred)+length(sample2.rf10p$pred),1)

pred.rf[1:132,1] = sample1.rf10c$pred
pred.rf[133:312,1] = sample2.rf10c$pred

pred.llf[1:132,1] = sample1.llf10c$pred
pred.llf[133:312,1] = sample2.llf10c$pred

real[1:132,1] = sample1.rf10c$real
real[133:312,1] = sample2.rf10c$real

pred.rf.ts = ts(pred.rf,start = 1990,frequency = 12)
pred.llf.ts = ts(pred.llf,start = 1990,frequency = 12)
real.ts = ts(real,start = 1990,frequency = 12)

plot 
plot(real.ts,type="l", ylab="inflation", xlab="time")
lines(pred.rf.ts,col="brown")
lines(pred.llf.ts,col="forestgreen")
legend("bottomleft",
       c("True Inflation","RF", "LLF"),
       fill=c("black","brown", "forestgreen")
)


# # Horizon 1 # #
pred.rf = matrix(0,length(sample1.rf1p$pred)+length(sample2.rf1p$pred),1)
pred.llf = matrix(0,length(sample1.rf1p$pred)+length(sample2.rf1p$pred),1)
pred.rw = matrix(0,length(sample1.rf1p$pred)+length(sample2.rf1p$pred),1)
real = matrix(0,length(sample1.rf10p$pred)+length(sample2.rf1p$pred),1)

pred.rf[1:132,1] = sample1.rf1p$pred
pred.rf[133:312,1] = sample2.rf1p$pred

pred.llf[1:132,1] = sample1.llf1p$pred
pred.llf[133:312,1] = sample2.llf1p$pred

real[1:132,1] = sample1.rf1p$real
real[133:312,1] = sample2.rf1p$real

pred.rf.ts = ts(pred.rf,start = 1990,frequency = 12)
pred.llf.ts = ts(pred.llf,start = 1990,frequency = 12)
real.ts = ts(real,start = 1990,frequency = 12)

plot 
plot(real.ts,type="l", ylab="inflation", xlab="time")
lines(pred.rf.ts,col="brown")
lines(pred.llf.ts,col="forestgreen")
legend("bottomleft",
       c("True Inflation","RF", "LLF"),
       fill=c("black","brown", "forestgreen")
)



