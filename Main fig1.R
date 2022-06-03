
###
### PNAS fig v3
###

library(dplyr)
library(ggplot2)
library(ggpubr)
library(ungroup)

data <- read.table("data/Lifetables by edu.csv",sep = ",",header=TRUE)

class(data$sex)
class(data$educ5)

edu_category <- unique(data$educ5)
sex <- unique(data$sex)

age_x <- unique(data$agegp5)


male_BH    <- data[data$sex==unique(data$sex)[1] & data$educ5==unique(data$educ5)[1],]
female_BH  <- data[data$sex==unique(data$sex)[2] & data$educ5==unique(data$educ5)[1],]

pop_ml_BH <- male_BH$pop_edu
pop_ml_BH <- pclm(age_x, pop_ml_BH, nlast = 5)
pop_ml_BH <- fitted(pop_ml_BH)
pop_fl_BH <- female_BH$pop_edu
pop_fl_BH <- pclm(age_x, pop_fl_BH, nlast = 5)
pop_fl_BH <- fitted(pop_fl_BH)

male_PH    <- data[data$sex==unique(data$sex)[1] & data$educ5==unique(data$educ5)[2],]
female_PH    <- data[data$sex==unique(data$sex)[2] & data$educ5==unique(data$educ5)[2],]

pop_ml_PH <- male_PH$pop_edu
pop_ml_PH <- pclm(age_x, pop_ml_PH, nlast = 5)
pop_ml_PH <- fitted(pop_ml_PH)
pop_fl_PH <- female_PH$pop_edu
pop_fl_PH <- pclm(age_x, pop_fl_PH, nlast = 5)
pop_fl_PH <- fitted(pop_fl_PH)

male_PNH    <- data[data$sex==unique(data$sex)[1] & data$educ5==unique(data$educ5)[3],]
female_PNH  <- data[data$sex==unique(data$sex)[2] & data$educ5==unique(data$educ5)[3],]

pop_ml_PNH <- male_PNH$pop_edu
pop_ml_PNH <- pclm(age_x, pop_ml_PNH, nlast = 5)
pop_ml_PNH <- fitted(pop_ml_PNH)
pop_fl_PNH <- female_PNH$pop_edu
pop_fl_PNH <- pclm(age_x, pop_fl_PNH, nlast = 5)
pop_fl_PNH <- fitted(pop_fl_PNH)

male_NPH    <- data[data$sex==unique(data$sex)[1] & data$educ5==unique(data$educ5)[4],]
female_NPH    <- data[data$sex==unique(data$sex)[2] & data$educ5==unique(data$educ5)[4],]

pop_ml_NPH <- male_NPH$pop_edu
pop_ml_NPH <- pclm(age_x, pop_ml_NPH, nlast = 5)
pop_ml_NPH <- fitted(pop_ml_NPH)
pop_fl_NPH <- female_NPH$pop_edu
pop_fl_NPH <- pclm(age_x, pop_fl_NPH, nlast = 5)
pop_fl_NPH <- fitted(pop_fl_NPH)

male_NPNH <- data[data$sex==unique(data$sex)[1] & data$educ5==unique(data$educ5)[5],]
female_NPNH  <- data[data$sex==unique(data$sex)[2] & data$educ5==unique(data$educ5)[5],]

pop_ml_NPNH <- male_NPNH$pop_edu
pop_ml_NPNH <- pclm(age_x, pop_ml_NPNH, nlast = 5)
pop_ml_NPNH <- fitted(pop_ml_NPNH)
pop_fl_NPNH <- female_NPNH$pop_edu
pop_fl_NPNH <- pclm(age_x, pop_fl_NPNH, nlast = 5)
pop_fl_NPNH <- fitted(pop_fl_NPNH)


dt_popP <- data.table::data.table(
  edu =
    rep(c(rep(edu_category[1],80), rep(edu_category[2],80), rep(edu_category[3],80),
          rep(edu_category[4],80), rep(edu_category[5],80)),2),
  age =
    rep(c(25:104),10),
  sex =
    c(rep("Male", 400),rep("Female", 400)),
  pop =
    c(round(c(pop_ml_BH, pop_ml_PH, pop_ml_PNH, pop_ml_NPH, pop_ml_NPNH),1),
      round(c(pop_fl_BH, pop_fl_PH, pop_fl_PNH, pop_fl_NPH, pop_fl_NPNH),1))
)


dt_popP$edu <- factor(dt_popP$edu,levels = c(
  "Bachelor+ & With/Without/Missing Year 12",
  "Other postsecondary qual & Year 12",
  "Other postsecondary qual & No Year 12",
  "No postsecondary qual & Year 12",
  "No postsecondary qual & No Year 12"),
  labels = c("University Education", "Post-secondary & Secondary", 
             "Post-secondary", "Secondary", "Lower than secondary"))


dt_popF <- dt_popP %>% arrange(age) %>% mutate(total = sum(pop)) %>% 
  filter(sex == "Female") %>% mutate(prop = pop/total*-100) 

dt_popM <- dt_popP %>% arrange(age) %>% mutate(total = sum(pop)) %>% 
  filter(sex == "Male") %>% mutate(prop = pop/total*100)

dt_popP2 <- rbind(dt_popF, dt_popM)


### annotation ####
min40F <- dt_popP2 %>% filter(age<40 & edu != "University Education" & sex == "Female") %>%
  group_by(age) %>% summarise(mark = sum(prop))
max40F <- dt_popP2 %>% filter(age<40 & sex == "Female") %>%
  group_by(age) %>% summarise(mark = sum(prop))
min40M <- dt_popP2 %>% filter(age<40 & edu != "University Education" & sex == "Male") %>%
  group_by(age) %>% summarise(mark = sum(prop))
max40M <- dt_popP2 %>% filter(age<40 & sex == "Male") %>%
  group_by(age) %>% summarise(mark = sum(prop))

min40Fd <- rbind(min40F[1,], max40F[1,])
min40Fu <- rbind(min40F[15,], max40F[15,])
min40Md <- rbind(min40M[1,], max40M[1,])
min40Mu <- rbind(min40M[15,], max40M[15,])

max50F <- dt_popP2 %>% 
  filter(age>=40 & edu != "University Education" & edu != "Post-secondary & Secondary" & sex == "Female") %>%
  group_by(age) %>% summarise(mark = sum(prop))

min50F <- dt_popP2 %>% 
  filter(age>=40 &
           edu!="University Education" & edu!="Post-secondary & Secondary" & edu!="Post-secondary" &
           sex=="Female") %>%
  group_by(age) %>% summarise(mark = sum(prop))

max50M <- dt_popP2 %>% 
  filter(age>=40 & edu != "University Education" & edu != "Post-secondary & Secondary" & sex == "Male") %>%
  group_by(age) %>% summarise(mark = sum(prop))

min50M <- dt_popP2 %>% 
  filter(age>=40 &
           edu!="University Education" & edu!="Post-secondary & Secondary" & edu!="Post-secondary" &
           sex=="Male") %>%
  group_by(age) %>% summarise(mark = sum(prop))

lth <- dim(min50F)[1]

mid50Fd <- rbind(min50F[1,], max50F[1,])
mid50Fu <- rbind(min50F[lth,], max50F[lth,])
mid50Md <- rbind(min50M[1,], max50M[1,])
mid50Mu <- rbind(min50M[lth,], max50M[lth,])

max60F <- dt_popP2 %>% filter(age>=60 & edu == "Lower than secondary" & sex == "Female") %>%
  group_by(age) %>% summarise(mark = sum(prop))
max60M <- dt_popP2 %>% filter(age>=60 & edu == "Lower than secondary" & sex == "Male") %>%
  group_by(age) %>% summarise(mark = sum(prop))

max60Fd <- rbind(max60F[1,],max60F[1,])
max60Fd[1,2] <- 0
max60Md <- rbind(max60M[1,],max60M[1,])
max60Md[1,2] <- 0


#### figure ####

ggplot(data = dt_popP2, aes(x = age, y = prop))+
  geom_bar(aes(fill = edu), stat = "identity")+
  geom_hline(yintercept = 0, color = "white")+
  # geom_vline(xintercept = 40, linetype = 2)+
  # geom_vline(xintercept = 60, linetype = 2)+
  scale_fill_brewer(type = "div", palette = 7)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), labels = function(x)paste0(abs(x), "%"))+
  coord_flip()+
  theme_classic()+
  theme(strip.text.x = element_text(size = 12),
        plot.margin = margin(1,1,1,1,"cm"),
        legend.position = "bottom")+
  guides(fill = guide_legend(title = "Education", position = "bottom"))+
  labs(title = "",
       x = "Age", y = "Proportions respect to the total population age 25 and above")+
  annotate("text", x = 100, y = -1.0, label = "Female", size = 6)+
  annotate("text", x = 100, y = 1.0, label = "Male", size = 6)+

#below 40
  geom_line(data = min40F, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = max40F, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = min40Fd, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = min40Fu, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  
  geom_line(data = min40M, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = max40M, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = min40Md, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = min40Mu, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
#around 50 
  geom_line(data = min50F, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = max50F, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = mid50Fd, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = mid50Fu, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  
  geom_line(data = min50M, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = max50M, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = mid50Md, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = mid50Mu, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  # higher than 60 
  geom_line(data = max60F, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = max60Fd, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  
  geom_line(data = max60M, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  geom_line(data = max60Md, aes(age, mark), linetype = 2, size = 1.2, color = "dimgrey")+
  
  annotate("segment", x = 60, xend = 100, y =0, yend = 0, linetype = 2, size = 1.2, color = "dimgrey") +
  
#below 40
  annotate("text", x = 30, y = -0.9, 
           label = paste(format(round(
             summarise(filter(dt_popP2, sex == "Female" & age<40 & edu == "University Education"), 
                       sum = sum(prop))*-1,1), nsmall=1),
             "%", sep = ""), 
           colour = "black", size = 6)+
  annotate("text", x = 30, y = 0.9, 
           label = paste(round(
             summarise(filter(dt_popP2, sex == "Male" & age<40 & edu == "University Education"), 
                       sum = sum(prop)),1),
             "%", sep = ""), 
           colour = "black", size = 6)+
#around 50 
  annotate("text", x = 50, y = -0.45, 
           label = paste(round(
             summarise(filter(dt_popP2, sex == "Female" & age>=40 & edu == "Post-secondary"), 
                       sum = sum(prop))*-1,1),
             "%", sep = ""), 
           colour = "black", size = 6)+
  annotate("text", x = 50, y = 0.44, 
           label = paste(round(
             summarise(filter(dt_popP2, sex == "Male" & age>=40 & edu == "Post-secondary"), 
                       sum = sum(prop)),1),
             "%", sep = ""), 
           colour = "black", size = 6)+
#higher than 60
  annotate("text", x = 70, y = -0.15, 
           label = paste(format(round(
             summarise(filter(dt_popP2, sex == "Female" & age>=60 & edu == "Lower than secondary"), 
                       sum = sum(prop))*-1,1) ,nsmall=1),
             "%", sep = ""), 
           colour = "black", size = 6)+
  annotate("text", x = 70, y = 0.1, 
           label = paste(format(round(
             summarise(filter(dt_popP2, sex == "Male" & age>=60 & edu == "Lower than secondary"), 
                       sum = sum(prop)),1), nsmall = 1),
             "%", sep = ""), 
           colour = "black", size = 6)

ggsave("reports/PNAS/PNAS.png", height = 6, width = 9)


#### table ####
# dt_popP3 <- dt_popP2 %>% mutate(age = case_when(
#   age<=40 ~ "25~39",
#   age>40 & age <60 ~ "40~59",
#   age>=60 ~ "60+",
#   T ~ as.character(age))) %>%
#   group_by(age,edu,sex) %>%
#   summarise(pct = sum(prop)) %>%
#   mutate(pct = paste0(round(abs(pct),1),"%"))
# ggtexttable(dt_popP3)
# ggsave("reports/PNAS table.pdf", height = 9)
