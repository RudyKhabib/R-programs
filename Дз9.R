# Прочтем данные из csv файла
mutants = read.csv('CaseStudy2_Bean_MutantProteinContent_Data.csv')
mutants # посмотрим на них
# разделим по мутантам (оставляем только значения, поэтому запятая)
naz = mutants[mutants['Mutant'] == 'Naz',][,'ProteinContent']
m692 = mutants[mutants['Mutant'] == 'M6-92',][,'ProteinContent']
m6125 = mutants[mutants['Mutant'] == 'M6-125',][,'ProteinContent']
naz
m692
m6125

mutants.matrix = cbind(naz, m692, m6125) # cделаем матрицу
mutants.matrix

# я не стал париться и хитрым образом получил средние вот так
prot.mean = c(mean(naz), mean(m692), mean(m6125))
prot.mean
all.mean = mean(mutants.matrix) # общее среднее
all.mean

# f статистика без анова
total = sum((mutants.matrix - all.mean)^2)
total
resid = sum((t(mutants.matrix) - prot.mean)^2)
resid
explained = total - resid
f.stat1 = (explained / 2) / (resid / (length(mutants.matrix) - 3))
f.stat1



