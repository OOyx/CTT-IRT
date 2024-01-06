library("equate")
library("hemp")
a = table(hcre$score, hcre$form)
hcre$score
hcre$form
hcre_data = as.data.frame(table(hcre$score, hcre$form))
ACTmath
act.x = as.freqtab(ACTmath[,1:2])
act.y = as.freqtab(ACTmath[,c(1,3)])
act.x
act.y

rbind(x = summary(act.x), y = summary(act.y))                   

b = KBneat$x
neat.x = freqtab(KBneat$x, scales = list(0:36, 0:12))
neat.x
neat.y = freqtab(KBneat$y, scales = list(0:36, 0:12))

plot(x=act.x,lwd=2,xlab ="Score",ylab= "Count")
plot(neat.x)

presmoothing(~ poly(total, 3, raw = T) + 
               poly(anchor, 3, raw = T) + 
               total:anchor, data = neat.x)
# ~ 用于分割自变量和因变量，前面为因变量，后者为自变量
neat.xsf <- with(as.data.frame(neat.x), 
                 cbind(total, total^2, total^3, 
                       anchor, anchor^2, anchor^3, total*anchor)) 
presmoothing(neat.x, smooth = "loglinear", 
             scorefun = neat.xsf)
neat.x
neat.xs <- presmoothing(neat.x, smooth = "log", 
                        degrees = list(3, 1))
neat.xs
