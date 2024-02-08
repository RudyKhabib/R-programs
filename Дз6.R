# пункт 1
library(plyr)
baseball
teams = names(table(baseball$team)[table(baseball$team) >= 200])
teams # список команд удовлетворяющих условию

# пункт 2

n = length(teams) # количество команд
# создаем нулевую матрицу нужного размера
transitions = matrix(0, nrow=n, ncol=n, dimnames=list(teams, teams)) 
for (i in 1:(n-1)) { 
  for (j in (i+1):n) { # матрица симметричная, поэтому так, чтобы не шагать за зря
    nt = c(teams[i], teams[j]) # список команд для выборки
    # выбираем игроков, игравших в обеих командах
    players1 = subset(baseball, is.element(baseball$team, nt[1]))$id 
    players2 = subset(baseball, is.element(baseball$team, nt[2]))$id 
    l.players = length(unique(players1[is.element(players1, players2)]))
    transitions[i,j] = l.players # записываем количество игроков в матрицу
    # так как матрица симметрична, то записываем значение и для другой пары
    transitions[j,i] = transitions[i,j] 
  }
}
transitions # смотрим на матрицу

# пункт 3

trans1 = transitions # копируем матрицу
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    nt = c(teams[i], teams[j]) 
    n.min = max(length(unique(baseball[baseball$team == nt[1],]$id)), # минимум из 2 команд
                length(unique(baseball[baseball$team == nt[1],]$id)))
    trans1[i,j] = trans1[i, j] / n.min 
    trans1[j,i] = trans1[i,j]
  }
}
trans1 # смотрим на матрицу

# пункт 4

distan = 1 - trans1 # вот и всё задание
distan

# пункт 5
mds = cmdscale(distan,k=2)
lst.cex = rep(0, each = length(rownames(mds))) # вектор нужным размером
for (i in 1:length(rownames(mds))){
  # заполняем вектор размером каждой команды
  lst.cex[i] = length(unique(baseball[baseball$team == rownames(mds)[i],]$id))
}
# нормируем по максимальному и умножаем на 2 чтобы лучше было видно
lst.cex = lst.cex / max(lst.cex) * 2
lst.cex # смотрим на вектор
# дальше твой код
mds = cmdscale(distan,k=2)
plot(mds,pch=19, cex = lst.cex)
text(mds,rownames(mds),adj=c(1.2,1.2),col='red')

