d = read.table('hw1.txt') 
gen1 = d[,1] # "CPY14" 
gen2 = d[,2] # "RMD4" 
gen3 = d[,3] # "htVWQ"
rm(d)
gc()
# 1
cor.test(gen1, gen2)
cor.test(gen1, gen3)
cor.test(gen2, gen3)
# 2
t.test(gen1, gen2)
t.test(gen1, gen3)
t.test(gen2, gen3)
# 3
par(mfrow=c(1, 3))
plot(density(gen1), ylim = c(0,0.0065), col = 'red', main = 'Density', xlab = 
       'Expression')
lines(density(gen2), col = 'blue')
lines(density(gen3), col = 'green')
boxplot(gen1, gen2, gen3, main = "Boxplot", xlab = 'x label', ylab = 'y label')
plot(gen1, gen2, main = 'plot')
m = lm(gen2 ~ gen1) # сред квад отклонение второй колонки от первой
abline(m, col ='red', lwd = 3) # провести от a  до b, lwd - толщина

# 1. Dependence between expressions
# significant dependence: [ gen1("CPY14" ), gen2("RMD4") ], p-value < 2.2e-16



# 2.Dependence between average values
# significant dependence: [ gen1("CPY14" ), gen2("RMD4") ], p-value = 0.001649



