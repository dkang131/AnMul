#correspondence analysis using packages -> ca
install.packages('ca')
library("ca")
data("author")
ca(author)
plot(ca(author))


#table method
haireye <- margin.table(HairEyeColor, 1:2)
haireye.ca <- ca(haireye)
haireye.ca
plot(haireye.ca)

#some plot options
plot(haireye.ca, lines = T)
plot(haireye.ca, arrows = c(T, F))
