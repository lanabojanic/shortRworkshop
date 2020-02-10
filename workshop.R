#Let's get started!
#workshop

#check working directory
getwd()
#set working directory
setwd()

#read data
data <- read.csv(file= "countries.csv", header = TRUE)
summary(data)               #gives median, mean, min & max

#variance, standard deviation
install.packages("pastecs")
library(pastecs)
res <- stat.desc(data[,-1]) #stat.desc cannot handle catergorical data, we removed var Country (first column)
round(res,2)                #rounded output to two decimals

#install.packages("tidyverse")
library(tidyverse)

#count
data %>% count(Year, wt=Deaths)
data %>% count(Year, Country, wt=Deaths)

#calculate rates
data %>% mutate(Rate= Deaths/Population * 10000)

#save your result
data <- data %>% mutate(Rate= Deaths/Population * 10000)

#do countries A & B differ in numbers of deaths?
#subset
data_ab <- data[which(data$Country=='A' | data$Country=='B'),]  #select observations in column Country that are either A or B
#chi
chi <- chisq.test(data_ab$Deaths)
             
# Visualise changes in rates over time
library(ggplot2)

pl <- ggplot(data, aes(Year, Rate)) + 
  geom_line(aes(group = Country), colour = "grey50") + 
  geom_point(aes(colour = Country)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 
                                2015, 2016, 2017, 2018)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black"), 
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

#export data
tiff("pl.tiff", units="in", width=7, height=5, res=400)
plot(pl)
dev.off()

#animations
#install.packages("gganimate")
#install.packages("gifski")
library(gganimate)
library(gifski)

pl2 <- pl + transition_reveal(Year)
pl2 <- animate(pl2, duration = 5, fps = 20, width = 700, height = 500, renderer = gifski_renderer())
anim_save("output.gif", animation = pl2)
