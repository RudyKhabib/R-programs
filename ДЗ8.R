# 1 задание
set.seed(123) # зафиксируем для 9го задания
m1 = matrix(rnorm(50000, mean = 0, sd = 1), ncol = 5) # матрица, 5 колонок
m1

# 2 задание 

# соединяем в один вектор наши забавы, 5 колонок, но заполняем построчно, чтобы
# именно первые 500 генов были такими 
set.seed(123)
m2 = matrix(c(rnorm(2500, 3), rnorm(47500, 0)), ncol = 5, byrow = TRUE)
m2

# 3 задание 

# обьеденим, получим первые 5 столбцов здоровых, последующие 5 - больные (пусть выздоравливают)
ob.m = cbind(m1, m2)
ob.m

pca = prcomp(ob.m) # построили pca
pca

cr = cor(ob.m, use='pair', method='sp')
heatmap(1  - cr, symm=T) # корреляционный хитмап

heatmap(pca$x) # некорреляционный хитмап

mds = cmdscale(1-cr, k=2) # mds
col = rep(c('orange', 'green'), each = 5) # больных и здоровых по 5
plot(mds, pch=19, col=col) # рисуем
title('MDS') # ну и хватит красоты наводить, в задании не указано :)

# 4 задание

t.test(m1, m2) # t-test сравнение больных и здоровых
pv = t.test(m1, m2)$p.value # вытащим р-валуе
pv
# для каждого гена
pv = sapply(1:10000, function(i) {t.test(m1[i,], m2[i,])$p.value})
pv

# 5 задание

table(pv < 0.05) 
num_sign = as.numeric(table(pv < 0.05)["TRUE"]) # вот столько генов значимо
 
fdr = sum(p.adjust(pv[pv < 0.05], method = "fdr"))/num_sign # доля ложно-положительных 
fdr

# 6 задание

num_sign_bh = sum(p.adjust(pv, method = "BH") < 0.05) # вот столько генов значимо
num_sign_bh
fdr_r_bh = (sum(p.adjust(pv, method = "BH")[501:10000] < 0.05) / num_sign_bh) # рил фдр
fdr_r_bh

# 7 задание 

# control+c, control+v
num_sign_bf = sum(p.adjust(pv, method = "bonferroni") < 0.05) # вот столько генов значимо
num_sign_bf
fdr_r_bf = (sum(p.adjust(pv, method = "bonferroni")[501:10000] < 0.05) / num_sign_bf) # рил фдр
fdr_r_bf
 
# 8 задание

# среднее по каждому гену из 6го для здоровых
health = sapply(m1[p.adjust(pv, method = "BH") < 0.05,], mean)
# среднее по каждому гену из 6го для больных (пусть выздоравливают)
ill = sapply(m2[p.adjust(pv, method = "BH") < 0.05,], mean)
hist(ill - health, main = 'Распределение размера эффекта', 
     xlab = 'Разница средних между больными и здоровыми')

# 9 задания файнали 

# начнем жесткое ctrl+c, ctrl+v

# 1 
set.seed(23) 
m11 = matrix(rnorm(50000, mean = 0, sd = 1), ncol = 5) 
m11

# 2 
set.seed(23)
m21 = matrix(c(rnorm(2500, 3), rnorm(47500, 0)), ncol = 5, byrow = TRUE)
m21

# 4
t.test(m11, m21) 
pv1 = t.test(m11, m21)$p.value 
pv1 = sapply(1:10000, function(i) {t.test(m11[i,], m21[i,])$p.value})
pv1

# 6 
num_sign_bh1 = sum(p.adjust(pv1, method = "BH") < 0.05) # вот столько генов значимо
num_sign_bh1
fdr_r_bh1 = (sum(p.adjust(pv1, method = "BH")[501:10000] < 0.05) / num_sign_bh1) # рил фдр
fdr_r_bh1

num_sign_bh # кол-во значимых в 1ом эксперименте
num_sign_bh1 # кол-во значимых в 2ом эксперименте
# пересечение значимых : взяли индексы генов из 2го, которые были значимы в 1ом 
# и проверяли условие для 2го
num_sign_in = sum((p.adjust(pv1, method = "BH")[p.adjust(pv, method = "BH") < 0.05]) < 0.05)

res = num_sign_in / num_sign_bh # доля генов знач 1го, воспроизведенных во 2ом
res
