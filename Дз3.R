# task 1
typeof(mtcars)
typeof(mtcars[,2])
mtcars['Fiat 128', 'cyl']
rownames(mtcars[mtcars[,'cyl'] == mtcars['Fiat 128', 'cyl'],])
min(mtcars[,'cyl'])
rownames(mtcars[mtcars[,'cyl'] == min(mtcars[,'cyl']),])
cmt = cor(mtcars)
cmt
typeof(cmt)
names(cmt[1,cmt[,1] < -0.7])

# task 2
x = rnorm(n=100, mean = 40, sd = sqrt(10)) 
x[seq(3, 100, by=3)]
x[seq(-5, -100, by= -5)]
x[as.integer(x) %% 2 == 0]

# task 3
tree = list(list('a', list('b', 'c')), list('d', 'e'))
unlist(tree)
unlist(tree[1])
unlist(tree[[1]][2])
