# task 1

# creating a function that takes two parameters as input:vector and n to return
# element number n in sorted vector
n.fun = function(vec, n) { 
  return(sort(vec, decreasing = TRUE)[n]) # sorting and then return element n
}

# lets check
set.seed(123)
arr = floor(runif(10, 1, 20))
print(arr)
n.fun(arr, 9)

# task 2

# creating a function that takes two parameters as input for x and y matrix and
# ploting Mandelbrot set
mandel = function(xmin, xmax, ymin, ymax){
  N = 1000
  x0 = matrix(rep(seq(xmin, xmax, length.out=N), each=N), ncol=N)
# repeat values for each columns
  y0 = matrix(rep(seq(ymin, ymax, length.out=N), times=N), ncol=N) 
# repeat values for each rows
  x = x0 
  y = y0
  for (i in 1:20) { # formula
    x_old = x
    x = x^2 - y^2 + x0
    y = 2*x_old*y + y0
  }
  z = t(abs(x^2 + y^2)) # transpose matrix
  z[!is.na(z)] = rank(z[!is.na(z)])
  image(z^3,col=rev(terrain.colors(1000))) # ploting Mandelbrot set
}

# lets check
mandel(-2,1,-1,1)
