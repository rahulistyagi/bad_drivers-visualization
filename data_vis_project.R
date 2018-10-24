## data visualization project 


# Library
library(plotly)
library(ggplot2)



# static graph 1

graph1=plot_ly(y =bad.drivers$State,x=bad.drivers$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, type = "histogram")
graph1

index<-seq(from=1,to=51,by=1)

bad.driverssorted<-cbind(index,bad.driverssorted)

View(bad.driverssorted)

Graph1<-ggplot(bad.driverssorted, aes(x=bad.driverssorted$State,y=bad.driverssorted$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles,stat = "identity")) + 
  geom_histogram(stat = "identity",fill = "#FF6666")

Graph2<-Graph1+theme(axis.text.x = element_text(face="bold",  size=10, angle=360,color = "#993333"))+geom_text(aes(label=bad.driverssorted$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles), vjust=0.3, hjust=1, color="black", size=4)
            
Graph2<-Graph2+labs(title = "DRIVERS INVOLVED IN FATAL COLLISIONS") + ylab("TOTAL FATAL COLLISIONS (BILLION/MILES) ") + xlab("STATES")

Graph2+coord_flip()

#calculating the numbers of alcohol impaired drivers 
number_of_drivers_alcohol_impaired<-((bad.driverssorted$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired)/100)*(bad.driverssorted$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)

bad.driverssorted<-cbind(number_of_drivers_alcohol_impaired,bad.driverssorted) # adding them to the dataset

View(bad.driverssorted)



# generating the susbset of top 5 cities with highest collision number for comparision 

compare<-bad.driverssorted[47,]
comapre2<-bad.driverssorted[48,]
comapre3<-bad.driverssorted[49,]
comapre4<-bad.driverssorted[50,]
comapre5<-bad.driverssorted[51,]

compare1<-rbind(comapre2,compare,comapre3,comapre4,comapre5)

compare1<-as.data.frame(compare1)


#interactive graph 1

plot_ly(compare1, labels = compare1$State, values = compare1$number_of_drivers_alcohol_impaired, type = 'pie') %>% 
  
layout(title = 'Percentage of drivers who were drunk while driving in 
         top 5 cities with most collisions')
    


# static graph 2 

number_of_drivers_speeding<-((bad.driverssorted$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)/100)*(bad.driverssorted$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)

bad.driverssorted<-cbind(number_of_drivers_speeding,bad.driverssorted)

#calculating number of speedy drivers
no_of_speeding<-((compare1$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding)/100)*(compare1$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)


compare1<-cbind(no_of_speeding,compare1)

View(compare1)

compare1<-compare1[,-c(2,3)] # removing duplicate elements

#static graph 3 
graph_static2<-ggplot(compare1, aes(compare1$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, compare1$no_of_speeding)) + 
  geom_bar(aes(fill = compare1$State), stat = "identity", position = "dodge")

graph_static2<-graph_static2+labs(title = "SPEEDING DRIVERS VS TOTAL COLLISIONS ") + ylab("Number of drivers who were speeding") + xlab("Total no of collisions")


graph_static2<-graph_static2 + theme_grey(base_size = 22)

graph_static2 + scale_fill_discrete(name="States")

#static graph 4
ggplot(CarDeath,aes(x = Losses, y = Premiums))+
  geom_point(aes(size = DR, color = DR), alpha = 0.7)+
  geom_smooth(aes(size = DR), method = lm, size = 1,color = "black")+
  scale_colour_gradient(low = "grey", high = "black", guide = "none")+
  scale_size_area("Drivers involved in fatal collisions\n(per billion miles)")+
  theme_light()





# interactive graph 2

#generating subset of states with least collisions
compare6<-bad.driverssorted[1,]
compare7<-bad.driverssorted[2,]
compare8<-bad.driverssorted[3,]
compare9<-bad.driverssorted[4,]
compare10<-bad.driverssorted[5,]


compare_insurance<-rbind(compare6,compare7,compare8,compare9,compare10)
View(compare_insurance)

compare_insurance<-as.data.frame(compare_insurance)

interactive_2<- plot_ly(bad.driverssorted, x =bad.driverssorted$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver...., y =bad.driverssorted$State, name = "losses", type = 'scatter',
             mode = "markers", marker = list(color = "red"))%>%
  add_trace(x =bad.driverssorted$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles, y =bad.driverssorted$State, name = "total collisions",type = 'scatter',
            mode = "markers", marker = list(color = "blue")) %>%
  layout(
    title = "Loss suffered by insurance companies",
    xaxis = list(title = "value"),
    margin = list(l = 100)
  )

interactive_2

#animated graphs

animated_1<-ggplot(CarDeath3, aes(y=State, x= Prc, frame = type )) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  theme_light()+
  labs(title = 'Top ten states with the highest fatal collisions per billion miles', x = 'Precentage', y ='')
animated_1<-ggplotly(animated_1)
animated_1


animated_2<-ggplot(CarDeath2, aes(y= Prc, frame = type )) +
  geom_boxplot(show.legend = FALSE) +
  theme_light()+
  labs(title = 'Distrabution of three attributes among states', x = '', y ='Precentage')
animated_2<-ggplotly(animated_2)
animated_2

animated_3<-ggplot(CarDeath, aes(x = Losses, y = Premiums, frame = Region )) +
  geom_point(show.legend = FALSE) +
  theme_light()+
  labs(title = 'Premium VS. Loss', x = 'Loss($)', y ='Premiums($)')
animated_3<-ggplotly(animated_3)
animated_3













