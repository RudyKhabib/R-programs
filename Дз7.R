# задание 1
# создаем функцию со всеми аргументами
my.barplot = function(d, t, e, col = 'grey', border = NA) { 
  # barplot с ограничениями по у, с цветом и границами
  b = barplot(d, ylim = c(0, max(d) + 1), col = col, border = border) 
  # буковки по центру колонок и под верхним пределом
  text(b, d, t, adj = c(0.4, 1))
  # цикл на линии у которых х это цетр а у это от высоты до высоты + е
  for (i in 1:length(d)) {
    lines(c(b[i], b[i]), c(d[i], d[i] + e[i]))
  }
}
# проверка
my.barplot(d = 1:5, t = LETTERS[1:5], e = runif(5, max = 3), col = 'red', 
           border = NA)

# задание 2
# создаем функцию со всеми аргументами
imageWithText = function(d, t, xlab = 'Cols', ylab = 'rows', main = NULL, col = NULL) {
  # используем image и все атрибуты которые решают нашу задачу, не задаем оси
  image(d, xlab = xlab, ylab = ylab, main = main, col = col,  xaxt='n', yaxt='n')
  # вычисляем координаты серединок по колонкам
  y = seq(-0.5, 1.5, by = 2 / (ncol(d) * 2))
  y = y[seq(2, length(y), by=2)]
  # вычисляем координаты серединок по строкам
  x = seq(-0.2, 1.2, by = 1.4 / (nrow(d) * 2))
  x = x[seq(2, length(x), by=2)]
  # печатаем текст, делаем это немножко хитро, чтобы y и х чередовались
  text(rep(x, times = ncol(d)), rep(y, each = nrow(d)), t)
  # опять хитро оставляем только нечетные имена строк
  rx = rownames(d)
  rx[seq(2, length(x), by=2)] = NA
  # оси х и у соответственно
  axis(1, x, rx)
  axis(2, y, colnames(d))
}

# проверка
d = matrix (1:8,ncol=2) 
colnames(d) = c('col1', 'col2')
rownames(d) = paste0('row', 1:4)
par(mfrow=c(1,2))
image(d,col=terrain.colors (100))
imageWithText(d, t = paste0('x=',d), xlab = 'Cols', ylab = 'rows',
              main = 'table', col=terrain.colors(100))
