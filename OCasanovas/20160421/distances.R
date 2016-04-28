###########################################
#Distances between the six individuals
# Version 0.1 - Luis Palomero - 2016 04 21
###########################################

#Dependencies

#Data loading
mRX03 <- read.table('mRX03.txt', header=T)
hRX03 <- read.table('hRX03.txt', header=T) 

#Functions

#Code
#In Mice
mRX03.t = t(as.matrix(mRX03[,-1:-2]))
mRX03.t.scale = scale(mRX03.t)
mRX03.t.scale.cmd = cmdscale(dist(mRX03.t.scale), k=2)
jpeg('cmd_mouse.jpg')
plot(mRX03.t.scale.cmd, main="CMDScale from mouse samples")
text(mRX03.t.scale.cmd, labels = row.names(mRX03.t.scale), pos = 4)
dev.off()
#In Humans
hRX03.t = t(as.matrix(hRX03[,-1:-2]))
hRX03.t.scale = scale(hRX03.t)
hRX03.t.scale.cmd = cmdscale(dist(hRX03.t.scale), k=2)
jpeg('cmd_human.jpg')
plot(hRX03.t.scale.cmd, main="CMDScale from human samples")
text(hRX03.t.scale.cmd, labels = row.names(hRX03.t.scale), pos = 4)
dev.off()
