#============================================================================
# Program: Y18_gvang_nowovi.R
#
#============================================================================

library(tibble)
library(lubridate)
library(FSA)
#----------------------------------------------------------------------------
# 1 Import data, clean, characterize

Eggs <- as_tibble(read.csv("./Data/Y18_nowovi_06_13_data.csv"))
View(Eggs)
str(Eggs)

Eggs$FemEclosion <- as.Date(mdy(Eggs$FemEclosion))
Eggs$PaperStrt <- as.Date(mdy(Eggs$PaperStrt))
Eggs$PaperStop <- as.Date(mdy(Eggs$PaperStop))
str(Eggs)

## Generate female ages as opposed to calendar day data were collected
Eggs$StopDay <- as.integer(Eggs$PaperStop - Eggs$FemEclosion)

## Pool red and white eggs to get all eggs
Eggs$EggsAll <- Eggs$EggsRed + Eggs$EggsWhite

library(DescTools)  
out <-  Desc(EggsAll ~ Mated, 
             data = Eggs,
             plotit = FALSE)     

out

#----------------------------------------------------------------------------
# 2 Visually examine frequency distribution by mating status and age category

#Jitter plot red egg distribution frequency in mated female NOW vs PaperCode
#Unexpected end of document, will figure out on Monday.
library(ggplot2)

p <- ggplot(Eggs, aes(y = EggsAll, x = Mated, group = Mated)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(w=0.1, h=0), 
              shape = 21, size = 3) +
  theme_bw() +
  facet_grid( ~ StopDay) 

p

P <- p+ xlab("PaperCode") + ylab("EggsRed") +
  scale_x_discrete(breaks = c(1, 2),
                   labels = c("PaperCode", "EggsRed")) +
  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12))
p

MatedFems <- Eggs[Eggs$Mated == "true", ]
MatedFems$StopDay <- as.factor(MatedFems$StopDay)

p2 <- ggplot(MatedFems, aes(y = EggsAll, x = StopDay)) +
  geom_boxplot() +
  theme_bw()

p2

NotMated <- Eggs[Eggs$Mated == "false", ]
NotMated$StopDay <- as.factor(NotMated$StopDay)

p3 <- ggplot(NotMated, aes(y = EggsAll, x = StopDay)) +
  geom_boxplot() +
  theme_bw()

p3


#basic plot of JarID and eggs laid
p <-  plot(Y18_nowovi_06_13_data$JarID, Y18_nowovi_06_13_data$EggsWhite)
plot(Y18_nowovi_06_13_data$JarID, Y18_nowovi_06_13_data$EggsRed)
p

#Summarizes data.
#Gives count, mean 
library(dplyr)

EggsAllSummary <- Eggs %>% group_by(Mated, StopDay) %>%
  summarise(nObs = sum(!is.na(EggsRed)), 
            mn = mean(EggsRed), 
            se = sd(EggsRed)/
              sqrt(sum(!is.na(EggsRed)))
  )

EggsAllSummary

EggsWhite <- Y18_nowovi_06_13_data%>%
  group_by(Y18_nowovi_06_13_data$Mated) %>%
  summarise(nObs = sum(!is.na(Y18_nowovi_06_13_data$EggsWhite)), 
            mn = mean(Y18_nowovi_06_13_data$EggsWhite), 
            se = sd(Y18_nowovi_06_13_data$EggsWhite)/
              sqrt(sum(!is.na(Y18_nowovi_06_13_data$EggsWhite)))
  )

EggsWhite

#Average number of red eggs laid = 13.9
#Average number of white eggs laid = 9.11
#Standard deviation for red eggs = 37.4
#Standard deviation for white eggs = 20.8

#EggsRed
# A tibble: 2 x 4
#`Y18_nowovi_06_13_data$Mated`  nObs    mn    se
#<chr>                         <int> <dbl> <dbl>
#  1 No                              140  13.9  3.16
#  2 Yes                             140  13.9  3.16

#EggsWhite
#A tibble: 2 x 4
#`Y18_nowovi_06_13_data$Mated`  nObs    mn    se
#<chr>                         <int> <dbl> <dbl>
#  1 No                              140  9.11  1.75
#  2 Yes                             140  9.11  1.75
