library(arules)
library(arulesViz)

data()

movi <- read.transactions(file.choose())
summary(movi)

inspect(movi)
class(movi)


#########for support = 0.002,confidence = 0.5,minlen = 2
movii <- apriori(movi,parameter = list(support=0.002,confidence = 0.5,minlen=2))
#The Apriori Algorithm is an influential algorithm for mining 
#frequent itemsets for boolean association rules.
movii  
#for the above performance measures will get 245 rules

inspect(movii,by ='lift')
inspect(head(sort(movii, by='lift')))

plot(movii)
head(quality(movii))

plot(movii,method = 'group')


#########for support = 0.003,confidence = 0.7,minlen = 3
movii <- apriori(movi,parameter = list(support=0.003,confidence = 0.7,minlen=3))
movii  
#for the above performance measures will get 181 rules

inspect(movii,by ='lift')
inspect(head(sort(movii, by='lift')))

plot(movii)
head(quality(movii))

plot(movii,method = 'group')


#########for support = 0.03,confidence = 0.75,minlen = 4

movii <- apriori(movi,parameter = list(support=0.03,confidence = 0.75,minlen=4))
movii  
#for the above performance measures will get 100 rules

inspect(movii,by ='lift')
inspect(head(sort(movii, by='lift')))

plot(movii)
head(quality(movii))

plot(movii,method = 'group')


#########for support = 0.003,confidence = 0.7,minlen = 1
movii <- apriori(movi,parameter = list(support=0.3,confidence = 0.8,minlen=1))
movii  
#for the above performance measures will get 181 rules

inspect(movii,by ='lift')
inspect(head(sort(movii, by='lift')))

plot(movii)
head(quality(movii))

plot(movii,method = 'group')
