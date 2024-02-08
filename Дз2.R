# task 1
x = runif(10000, -1, 1)
y = runif(10000, -1, 1)
r = x^2 + y^2
p = length(r[r <= 1]) / length(r)
p

# task 2
my.pi = p * 4
my.pi

# task 3
r[r <= 1] = 1
r[r > 1] = 0
r.cumsum = cumsum(r)
d = 1:10000
mypi = (r.cumsum / d) * 4
plot(mypi, type = 'l', xlab = 'number of throws', ylab = 'number pi',
     main = 'The dependence of the estimate of pi on throws', col = 'blue', 
     ylim = c(2.5, 4))
abline(h = pi, col = 'red')
