
###
### PNAS figs v5
###
### Mortality section


library(tidyverse)
library(ggthemes)


#### preparation ####
dta <- read.table("data/Lifetables by edu.csv",sep = ",",header=TRUE)

names(dta)[2] <- "educ5"

class(dta$educ5)
edu_cats <- unique(dta$educ5)

if(class(dta$educ5) == "integer"){
  dta$educ5 <- as.character(dta$educ5)
  edu_cats <- rev(unique(dta$educ5))
}

age_x <- unique(dta$agegp5)



#### Decomposition data preparation ####

library(ungroup)

age_mat <- c(25:104)

#male
MD_BH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[1] & dta$sex == "male", 6], nlast = 5)
MN_BH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[1] & dta$sex == "male", 5], nlast = 5)
mD_BH_m <- fitted(MD_BH_m)
mN_BH_m <- fitted(MN_BH_m)
Mx_BH_m <- fitted(MD_BH_m)/fitted(MN_BH_m)


MD_PH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[2] & dta$sex == "male", 6], nlast = 5)
MN_PH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[2] & dta$sex == "male", 5], nlast = 5)
mD_PH_m <- fitted(MD_PH_m)
mN_PH_m <- fitted(MN_PH_m)
Mx_PH_m <- fitted(MD_PH_m)/fitted(MN_PH_m)


MD_PNH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[3] & dta$sex == "male", 6], nlast = 5)
MN_PNH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[3] & dta$sex == "male", 5], nlast = 5)
mD_PNH_m <- fitted(MD_PNH_m)
mN_PNH_m <- fitted(MN_PNH_m)
Mx_PNH_m <- fitted(MD_PNH_m)/fitted(MN_PNH_m)


MD_NPH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[4] & dta$sex == "male", 6], nlast = 5)
MN_NPH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[4] & dta$sex == "male", 5], nlast = 5)
mD_NPH_m <- fitted(MD_NPH_m)
mN_NPH_m <- fitted(MN_NPH_m)
Mx_NPH_m <- fitted(MD_NPH_m)/fitted(MN_NPH_m)


MD_NPNH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[5] & dta$sex == "male", 6], nlast = 5)
MN_NPNH_m <- pclm(age_x, dta[dta$educ5 == edu_cats[5] & dta$sex == "male", 5], nlast = 5)
mD_NPNH_m <- fitted(MD_NPNH_m)
mN_NPNH_m <- fitted(MN_NPNH_m)
Mx_NPNH_m <- fitted(MD_NPNH_m)/fitted(MN_NPNH_m)

#female
MD_BH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[1] & dta$sex == "female", 6], nlast = 5)
MN_BH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[1] & dta$sex == "female", 5], nlast = 5)
mD_BH_f <- fitted(MD_BH_f)
mN_BH_f <- fitted(MN_BH_f)
Mx_BH_f <- fitted(MD_BH_f)/fitted(MN_BH_f)


MD_PH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[2] & dta$sex == "female", 6], nlast = 5)
MN_PH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[2] & dta$sex == "female", 5], nlast = 5)
mD_PH_f <- fitted(MD_PH_f)
mN_PH_f <- fitted(MN_PH_f)
Mx_PH_f <- fitted(MD_PH_f)/fitted(MN_PH_f)


MD_PNH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[3] & dta$sex == "female", 6], nlast = 5)
MN_PNH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[3] & dta$sex == "female", 5], nlast = 5)
mD_PNH_f <- fitted(MD_PNH_f)
mN_PNH_f <- fitted(MN_PNH_f)
Mx_PNH_f <- fitted(MD_PNH_f)/fitted(MN_PNH_f)


MD_NPH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[4] & dta$sex == "female", 6], nlast = 5)
MN_NPH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[4] & dta$sex == "female", 5], nlast = 5)
mD_NPH_f <- fitted(MD_NPH_f)
mN_NPH_f <- fitted(MN_NPH_f)
Mx_NPH_f <- fitted(MD_NPH_f)/fitted(MN_NPH_f)


MD_NPNH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[5] & dta$sex == "female", 6], nlast = 5)
MN_NPNH_f <- pclm(age_x, dta[dta$educ5 == edu_cats[5] & dta$sex == "female", 5], nlast = 5)
mD_NPNH_f <- fitted(MD_NPNH_f)
mN_NPNH_f <- fitted(MN_NPNH_f)
Mx_NPNH_f <- fitted(MD_NPNH_f)/fitted(MN_NPNH_f)

# we need to have a total
Nx_TOT_M <- mN_BH_m + mN_PH_m + mN_PNH_m + mN_NPH_m + mN_NPNH_m
Dx_TOT_M <- mD_BH_m + mD_PH_m + mD_PNH_m + mD_NPH_m + mD_NPNH_m

Mx_TOT_M <- Dx_TOT_M/Nx_TOT_M

Nx_TOT_F <- mN_BH_f + mN_PH_f + mN_PNH_f + mN_NPH_f + mN_NPNH_f
Dx_TOT_F <- mD_BH_f + mD_PH_f + mD_PNH_f + mD_NPH_f + mD_NPNH_f

Mx_TOT_F <- Dx_TOT_F/Nx_TOT_F


##### The proportion of Death ######
PropD_BH_m <- mD_BH_m/Dx_TOT_M
PropD_PH_m <- mD_PH_m/Dx_TOT_M
PropD_PNH_m <- mD_PNH_m/Dx_TOT_M
PropD_NPH_m <- mD_NPH_m/Dx_TOT_M
PropD_NPNH_m <- mD_NPNH_m/Dx_TOT_M

PropD_BH_f <- mD_BH_f/Dx_TOT_F
PropD_PH_f <- mD_PH_f/Dx_TOT_F
PropD_PNH_f <- mD_PNH_f/Dx_TOT_F
PropD_NPH_f <- mD_NPH_f/Dx_TOT_F
PropD_NPNH_f <- mD_NPNH_f/Dx_TOT_F

#### proportion of Population #####

PropN_BH_m <- mN_BH_m/Nx_TOT_M
PropN_PH_m <- mN_PH_m/Nx_TOT_M
PropN_PNH_m <- mN_PNH_m/Nx_TOT_M
PropN_NPH_m <- mN_NPH_m/Nx_TOT_M
PropN_NPNH_m <- mN_NPNH_m/Nx_TOT_M

PropN_BH_f <- mN_BH_f/Nx_TOT_F
PropN_PH_f <- mN_PH_f/Nx_TOT_F
PropN_PNH_f <- mN_PNH_f/Nx_TOT_F
PropN_NPH_f <- mN_NPH_f/Nx_TOT_F
PropN_NPNH_f <- mN_NPNH_f/Nx_TOT_F

table_Ux <- tibble(
  age = c(rep(25:104,12)),
  sex = c(rep("Male", 400), rep("Female", 400), rep("Male", 80), rep("Female", 80)),
  edu = c(rep(c(rep("University Education", 80), rep("Post-secondary & Secondary", 80), 
                rep("Post-secondary", 80), rep("Secondary", 80), 
                rep("Lower than secondary",80)),2),
          rep("Total",160)),
  Ux  = c(Mx_BH_m*PropN_BH_m, Mx_PH_m*PropN_PH_m, Mx_PNH_m*PropN_PNH_m, 
          Mx_NPH_m*PropN_NPH_m, Mx_NPNH_m*PropN_NPNH_m,
          Mx_BH_f*PropN_BH_f, Mx_PH_f*PropN_PH_f, Mx_PNH_f*PropN_PNH_f, 
          Mx_NPH_f*PropN_NPH_f, Mx_NPNH_f*PropN_NPNH_f,
          Mx_TOT_M, Mx_TOT_F),
)

table_Ux <- table_Ux %>% mutate(
  age2 = case_when(
    age < 40 ~ "25~39",
    age >= 40 & age < 60 ~ "40~59",
    age >= 60 & age < 80 ~ "60~79",
    age >= 80 & age < 100 ~ "80~99",
    age >= 100 ~"100+",
    TRUE ~ as.character(age)
  ))

table_Ux$age2 <- factor(table_Ux$age2, 
                        levels = rev(c(
                          "25~39", "40~59", "60~79", "80~99", "100+"
                        )))

table_Ux$edu <- factor(table_Ux$edu,
                       levels = c("University Education", "Post-secondary & Secondary", 
                                  "Post-secondary", "Secondary", "Lower than secondary"))

ggplot(data = filter(table_Ux, edu != "Total"), aes(x = age, y = Ux))+
  geom_col(aes(fill = edu), size = 1.2)+
  facet_grid(age2~sex, scales = "free")+
  scale_fill_brewer(type = "div", palette = 7)+
  scale_y_continuous(expand = c(0.1,0), n.breaks = 3, labels = function(x)format(x,nsmall=4,scientific = F))+
  theme_classic()+
  labs(title = "", 
       x = "Age", 
       y = "Age-specific death rates and the contribution by education",
       caption = "Note: the national age-specific death rate is the sum over all education groups of age- and education-specific death rate by the age- and education-specific proportions.")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 8),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "grey90"),
        text = element_text(size = 8),
        plot.margin = margin(2,2,4,2))+
  guides(fill = guide_legend(title = "Education"))
ggsave("reports/PNAS/PNAS2.png", height = 6,width = 9)

# ##### deaths counts #####
# 
# lifetable <- function(x, Nx, Dx, sex="M", ax=NULL){
#   m <- length(x)
#   mx  <- Dx/Nx
#   n <- c(diff(x), NA)
#   if(is.null(ax)){
#     ax <- rep(0,m)
#     if(x[1]!=0 | x[2]!=1){
#       ax <- n/2
#       ax[m] <- 1 / mx[m]
#     }else{    
#       if(sex=="F"){
#         if(mx[1]>=0.107){
#           ax[1] <- 0.350
#         }else{
#           ax[1] <- 0.053 + 2.800*mx[1]
#         }
#       }
#       if(sex=="M"){
#         if(mx[1]>=0.107){
#           ax[1] <- 0.330
#         }else{
#           ax[1] <- 0.045 + 2.684*mx[1]
#         }
#       }
#       ax[-1] <- n[-1]/2
#       ax[m] <- 1 / mx[m]
#     }
#   }
#   qx  <- n*mx / (1 + (n - ax) * mx)
#   qx[m] <- 1
#   px  <- 1-qx
#   lx  <- cumprod(c(1,px))*100000
#   dx  <- -diff(lx)
#   Lx  <- n*lx[-1] + ax*dx
#   lx <- lx[-(m+1)]
#   Lx[m] <- lx[m]/mx[m]
#   Lx[is.na(Lx)] <- 0 ## in case of NA values
#   Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
#   Tx  <- rev(cumsum(rev(Lx)))
#   ex  <- Tx/lx
#   return.df <- data.frame(x, n, Nx, Dx, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
#   return(return.df)
# }
# 
# CIdx <- function(x, Nx, Dx, sex = "M", ax = NULL, ns=1000, level=0.95){
#   ## point-estimated lifetable
#   LT <- lifetable(x, Nx, Dx, sex = sex, ax = ax)
#   ## number of ages
#   m <- nrow(LT)
#   ## estimated probs # put mx over here
#   qx <- LT$qx
#   ## trials for binomial, rounded
#   Ntil <- round(Dx/qx)
#   ## simulated death counts
#   set.seed(2718)
#   ## from Binomial distribution
#   Y <- suppressWarnings(matrix(rbinom(m*ns,
#                                       Ntil,
#                                       qx),
#                                m,ns))
#   return(Y)
# }
# 
# Death.cnt_m <- CIdx(age_mat, Nx_TOT_M, Dx_TOT_M, sex = "M")
# 
# Death.cnt_f <- CIdx(age_mat, Nx_TOT_F, Dx_TOT_F, sex = "F")
# 
# #### bootstrap mortality ##### 
# 
# px_func <- function(Mx){
#   px <- c()
#   for (i in 1:length(Mx)) {
#     px2 <- exp(-Mx[i])
#     px <- c(px, px2)
#   }
#   return(px)
# }
# 
# gap <- c()
# e0m <- c()
# e0f <- c()
# 
# Ux_BH_m <- c()
# Ux_PH_m <- c()
# Ux_PNH_m <- c()
# Ux_NPH_m <- c()
# Ux_NPNH_m <- c()
# 
# mx_BH_m <- c()
# mx_PH_m <- c()
# mx_PNH_m <- c()
# mx_NPH_m <- c()
# mx_NPNH_m <- c()
# 
# Ux_BH_f <- c()
# Ux_PH_f <- c()
# Ux_PNH_f <- c()
# Ux_NPH_f <- c()
# Ux_NPNH_f <- c()
# 
# mx_BH_f <- c()
# mx_PH_f <- c()
# mx_PNH_f <- c()
# mx_NPH_f <- c()
# mx_NPNH_f <- c()
# 
# 
# for (i in 1:1000) {
#   death.male <- Death.cnt_m[,i]
#   death.female <- Death.cnt_f[,i]
#   
#   Mx_tot_m <- death.male/Nx_TOT_M
#   Mx_tot_f <- death.female/Nx_TOT_F
#   
#   lx_male <- cumprod(px_func(Mx_tot_m))
#   lx_female <- cumprod(px_func(Mx_tot_f))
#   lx_avg <- (lx_male + lx_female)/2
#   
#   gap <- c(gap, sum(lx_female) - sum(lx_male))
#   e0m <- c(e0m, sum(lx_male))
#   e0f <- c(e0f, sum(lx_female))
#   
#   dx_bhm <- PropD_BH_m * death.male
#   dx_phm <- PropD_PH_m * death.male
#   dx_pnhm <- PropD_PNH_m * death.male
#   dx_nphm <- PropD_NPH_m * death.male
#   dx_npnhm <- PropD_NPNH_m * death.male
#   
#   dx_bhf <- PropD_BH_f * death.female
#   dx_phf <- PropD_PH_f * death.female
#   dx_pnhf <- PropD_PNH_f * death.female
#   dx_nphf <- PropD_NPH_f * death.female
#   dx_npnhf <- PropD_NPNH_f * death.female
#   
#   Mx_BH_m <- dx_bhm/mN_BH_m
#   Mx_PH_m <- dx_phm/mN_PH_m
#   Mx_PNH_m <- dx_pnhm/mN_PNH_m
#   Mx_NPH_m <- dx_nphm/mN_NPH_m
#   Mx_NPNH_m <- dx_npnhm/mN_NPNH_m
#   
#   Mx_BH_f <- dx_bhf/mN_BH_f
#   Mx_PH_f <- dx_phf/mN_PH_f
#   Mx_PNH_f <- dx_pnhf/mN_PNH_f
#   Mx_NPH_f <- dx_nphf/mN_NPH_f
#   Mx_NPNH_f <- dx_npnhf/mN_NPNH_f
#   
#   mx_BH_m <- rbind(mx_BH_m, Mx_BH_m)
#   mx_PH_m <- rbind(mx_PH_m, Mx_PH_m)
#   mx_PNH_m <- rbind(mx_PNH_m, Mx_PNH_m)
#   mx_NPH_m <- rbind(mx_NPH_m, Mx_NPH_m)
#   mx_NPNH_m <- rbind(mx_NPNH_m, Mx_NPNH_m)
#   
#   mx_BH_f <- rbind(mx_BH_f, Mx_BH_f)
#   mx_PH_f <- rbind(mx_PH_f, Mx_PH_f)
#   mx_PNH_f <- rbind(mx_PNH_f, Mx_PNH_f)
#   mx_NPH_f <- rbind(mx_NPH_f, Mx_NPH_f)
#   mx_NPNH_f <- rbind(mx_NPNH_f, Mx_NPNH_f)
# 
#   Ux_BH_m <- rbind(Ux_BH_m, Mx_BH_m*PropN_BH_m)
#   Ux_PH_m <- rbind(Ux_PH_m, Mx_PH_m*PropN_PH_m)
#   Ux_PNH_m <- rbind(Ux_PNH_m, Mx_PNH_m*PropN_PNH_m)
#   Ux_NPH_m <- rbind(Ux_NPH_m, Mx_NPH_m*PropN_NPH_m)
#   Ux_NPNH_m <- rbind(Ux_NPNH_m, Mx_NPNH_m*PropN_NPNH_m)
#   
#   Ux_BH_f <- rbind(Ux_BH_f, Mx_BH_f*PropN_BH_f)
#   Ux_PH_f <- rbind(Ux_PH_f, Mx_PH_f*PropN_PH_f)
#   Ux_PNH_f <- rbind(Ux_PNH_f, Mx_PNH_f*PropN_PNH_f)
#   Ux_NPH_f <- rbind(Ux_NPH_f, Mx_NPH_f*PropN_NPH_f)
#   Ux_NPNH_f <- rbind(Ux_NPNH_f, Mx_NPNH_f*PropN_NPNH_f)
#   
# }
# 
# mean_qtl <- function(x){
#   mean <- mean(x)
#   qtl <- quantile(x, probs = c((1-0.95)/2, 1 - (1-0.95)/2))
#   return(c(mean,qtl))
# }
# 
# mx_BH_m <- apply(mx_BH_m, 2, mean_qtl)
# mx_PH_m <- apply(mx_PH_m, 2, mean_qtl)
# mx_PNH_m <- apply(mx_PNH_m, 2, mean_qtl)
# mx_NPH_m <- apply(mx_NPH_m, 2, mean_qtl)
# mx_NPNH_m <- apply(mx_NPNH_m, 2, mean_qtl)
# 
# mx_BH_f <- apply(mx_BH_f, 2, mean_qtl)
# mx_PH_f <- apply(mx_PH_f, 2, mean_qtl)
# mx_PNH_f <- apply(mx_PNH_f, 2, mean_qtl)
# mx_NPH_f <- apply(mx_NPH_f, 2, mean_qtl)
# mx_NPNH_f <- apply(mx_NPNH_f, 2, mean_qtl)
# 
# Ux_BH_m <- apply(Ux_BH_m, 2, mean_qtl)
# Ux_PH_m <- apply(Ux_PH_m, 2, mean_qtl)
# Ux_PNH_m <- apply(Ux_PNH_m, 2, mean_qtl)
# Ux_NPH_m <- apply(Ux_NPH_m, 2, mean_qtl)
# Ux_NPNH_m <- apply(Ux_NPNH_m, 2, mean_qtl)
# 
# mx_tot_m <- Ux_BH_m + Ux_PH_m + Ux_PNH_m + Ux_NPH_m +Ux_NPNH_m
# 
# Ux_BH_f <- apply(Ux_BH_f, 2, mean_qtl)
# Ux_PH_f <- apply(Ux_PH_f, 2, mean_qtl)
# Ux_PNH_f <- apply(Ux_PNH_f, 2, mean_qtl)
# Ux_NPH_f <- apply(Ux_NPH_f, 2, mean_qtl)
# Ux_NPNH_f <- apply(Ux_NPNH_f, 2, mean_qtl)
# 
# mx_tot_f <- Ux_BH_f + Ux_PH_f + Ux_PNH_f + Ux_NPH_f +Ux_NPNH_f
# 
# # table_mx <- tibble(
# #   age = c(rep(25:104,12)),
# #   sex = c(rep("Male", 400), rep("Female", 400), rep("Male", 80), rep("Female", 80)),
# #   edu = c(rep(c(rep("High", 80), rep("MH", 80), rep("Medium", 80), rep("ML", 80), rep("Low",80)),2),
# #           rep("Total",160)),
# #   mx  = c(mx_BH_m[1,], mx_PH_m[1,], mx_PNH_m[1,], mx_NPH_m[1,], mx_NPNH_m[1,],
# #           mx_BH_f[1,], mx_PH_f[1,], mx_PNH_f[1,], mx_NPH_f[1,], mx_NPNH_f[1,],
# #           mx_tot_m[1,], mx_tot_f[1,]),
# #   mx_lower = c(mx_BH_m[2,], mx_PH_m[2,], mx_PNH_m[2,], mx_NPH_m[2,], mx_NPNH_m[2,],
# #                mx_BH_f[2,], mx_PH_f[2,], mx_PNH_f[2,], mx_NPH_f[2,], mx_NPNH_f[2,],
# #                mx_tot_m[2,], mx_tot_f[2,]),
# #   mx_upper = c(mx_BH_m[3,], mx_PH_m[3,], mx_PNH_m[3,], mx_NPH_m[3,], mx_NPNH_m[3,],
# #                mx_BH_f[3,], mx_PH_f[3,], mx_PNH_f[3,], mx_NPH_f[3,], mx_NPNH_f[3,],
# #                mx_tot_m[3,], mx_tot_f[3,])
# # )
# # 
# # color <- RColorBrewer::brewer.pal(5,"RdYlBu")
# # 
# # ggplot(
# #   data = filter(table_mx, edu %in% c("High", "Low", "Total")),
# #   aes(x = age, y = mx)
# # )+
# #   geom_line(aes(color = edu), size = 1)+
# #   scale_y_continuous(labels = c(), trans = "log10")+
# #   geom_ribbon(aes(ymin = mx_lower, ymax = mx_upper, fill = edu), 
# #               alpha  = 0.3)+
# #   facet_wrap(~sex)+
# #   theme_bw()+
# #   scale_fill_manual(values = c(color[c(1,5)], "Brown"))+
# #   scale_color_manual(values = c(color[c(1,5)], "Brown"))+
# #   guides(fill = guide_legend(title = "Education level"),
# #          color = guide_legend(title = "Education level"))+
# #   labs(title = "Mortality comparison, Australia 2016",
# #        x = "Age", y = "Age-specific mortality rate (log10 scale)")
# 
# 
# 
# table_Ux <- tibble(
#   age = c(rep(25:104,12)),
#   sex = c(rep("Male", 400), rep("Female", 400), rep("Male", 80), rep("Female", 80)),
#   edu = c(rep(c(rep("University Education", 80), rep("Post-Secondary & Secondary", 80), 
#                 rep("Post-Secondary", 80), rep("Secondary", 80), 
#                 rep("Lower than Secondary",80)),2),
#           rep("Total",160)),
#   Ux  = c(Ux_BH_m[1,], Ux_PH_m[1,], Ux_PNH_m[1,], Ux_NPH_m[1,], Ux_NPNH_m[1,],
#           Ux_BH_f[1,], Ux_PH_f[1,], Ux_PNH_f[1,], Ux_NPH_f[1,], Ux_NPNH_f[1,],
#           mx_tot_m[1,], mx_tot_f[1,]),
#   # Ux_lower = c(Ux_BH_m[2,], Ux_PH_m[2,], Ux_PNH_m[2,], Ux_NPH_m[2,], Ux_NPNH_m[2,],
#   #              Ux_BH_f[2,], Ux_PH_f[2,], Ux_PNH_f[2,], Ux_NPH_f[2,], Ux_NPNH_f[2,],
#   #              mx_tot_m[2,], mx_tot_f[2,]),
#   # Ux_upper = c(Ux_BH_m[3,], Ux_PH_m[3,], Ux_PNH_m[3,], Ux_NPH_m[3,], Ux_NPNH_m[3,],
#   #              Ux_BH_f[3,], Ux_PH_f[3,], Ux_PNH_f[3,], Ux_NPH_f[3,], Ux_NPNH_f[3,],
#   #              mx_tot_m[3,], mx_tot_f[3,])
#   Ux_lower = c(rep(c(mx_tot_m[2,], mx_tot_f[2,]),6)),
#   Ux_upper = c(rep(c(mx_tot_m[3,], mx_tot_f[3,]),6))
# )
# 
# table_Ux <- table_Ux %>% mutate(
#   age2 = case_when(
#   age < 40 ~ "25~39",
#   age >= 40 & age < 60 ~ "40~59",
#   age >= 60 & age < 80 ~ "60~79",
#   age >= 80 & age < 100 ~ "80~99",
#   age >= 100 ~"100+",
#   TRUE ~ as.character(age)
# ))
# table_Ux$age2 <- factor(table_Ux$age2, 
#                         levels = c(
#                           "25~39", "40~59", "60~79", "80~99", "100+"
#                         ))
# table_Ux$edu <- factor(table_Ux$edu,
#                        levels = c("University Education", "Post-Secondary & Secondary", 
#                                   "Post-Secondary", "Secondary", "Lower than Secondary"))
# 
# table_Ux2 <- table_Ux %>% select(-edu)
# 
# ggplot(data = filter(table_Ux, edu != "Total"), aes(x = age, y = Ux))+
#   geom_col(aes(fill = edu), size = 1.2)+
#   facet_grid(age2~sex, scales = "free")+
#   geom_errorbar(data = table_Ux2, 
#                 aes(x = age, y = Ux, ymin = Ux_lower, ymax = Ux_upper), size = 0.5,
#                 stat = "identity")+
#   scale_fill_brewer(type = "div", palette = 7)+
#   scale_y_continuous(expand = c(0.1,0), n.breaks = 3,
#                      labels = function(x)paste0(format(x, scientific = F)))+
#   theme_bw()+
#   labs(title = "Figure 2. Mortality contribution by education levels to the national age-specific death rates, Australia 2016", 
#        x = "Age", 
#        y = "Age-specific death rates and the contribution by education",
#        caption = "Note: the total age-specific death rate is the sum over all education groups of age- and education-specific death rate by the age- and education-specific proportions.")+
#   theme(plot.caption.position = "plot",
#         plot.caption = element_text(size = 8))+
#   guides(fill = guide_legend(title = "Education"))
# # ggsave("reports/PNAS2.pdf")
#   
# table_Ux_cont <- table_Ux %>% filter(age2 != "25~39") %>% 
#   group_by(sex,edu) %>% summarise(prop = sum(Ux))
# #female
# table_Ux_cont$prop[5]/table_Ux_cont$prop[6]
# #male
# table_Ux_cont$prop[11]/table_Ux_cont$prop[12]
