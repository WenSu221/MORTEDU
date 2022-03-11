
###
### Preparation for the CI
###

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

#### Now we should specify the counts for population & deaths ####

# start with the highest educated level
Nx_BH_M <- dta[dta$educ5 == edu_cats[1] & dta$sex == "male", 5]
Nx_BH_F <- dta[dta$educ5 == edu_cats[1] & dta$sex == "female", 5]

Dx_BH_M <- dta[dta$educ5 == edu_cats[1] & dta$sex == "male", 6]
Dx_BH_F <- dta[dta$educ5 == edu_cats[1] & dta$sex == "female", 6]

# then second highest
Nx_PH_M <- dta[dta$educ5 == edu_cats[2] & dta$sex == "male", 5]
Nx_PH_F <- dta[dta$educ5 == edu_cats[2] & dta$sex == "female", 5]

Dx_PH_M <- dta[dta$educ5 == edu_cats[2] & dta$sex == "male", 6]
Dx_PH_F <- dta[dta$educ5 == edu_cats[2] & dta$sex == "female", 6]

# the middle one
Nx_PNH_M <- dta[dta$educ5 == edu_cats[3] & dta$sex == "male", 5]
Nx_PNH_F <- dta[dta$educ5 == edu_cats[3] & dta$sex == "female", 5]

Dx_PNH_M <- dta[dta$educ5 == edu_cats[3] & dta$sex == "male", 6]
Dx_PNH_F <- dta[dta$educ5 == edu_cats[3] & dta$sex == "female", 6]

# the second last one
Nx_NPH_M <- dta[dta$educ5 == edu_cats[4] & dta$sex == "male", 5]
Nx_NPH_F <- dta[dta$educ5 == edu_cats[4] & dta$sex == "female", 5]

Dx_NPH_M <- dta[dta$educ5 == edu_cats[4] & dta$sex == "male", 6]
Dx_NPH_F <- dta[dta$educ5 == edu_cats[4] & dta$sex == "female", 6]

# the most unfortunate one
Nx_NPNH_M <- dta[dta$educ5 == edu_cats[5] & dta$sex == "male", 5]
Nx_NPNH_F <- dta[dta$educ5 == edu_cats[5] & dta$sex == "female", 5]

Dx_NPNH_M <- dta[dta$educ5 == edu_cats[5] & dta$sex == "male", 6]
Dx_NPNH_F <- dta[dta$educ5 == edu_cats[5] & dta$sex == "female", 6]

#### Decomposition data preparation ####

library(ungroup)

age_mat <- c(25:104)

#male
MD_BH_m <- pclm(age_x, Dx_BH_M, nlast = 5)
MN_BH_m <- pclm(age_x, Nx_BH_M, nlast = 5)
mD_BH_m <- fitted(MD_BH_m)
mN_BH_m <- fitted(MN_BH_m)
Mx_BH_m <- fitted(MD_BH_m)/fitted(MN_BH_m)


MD_PH_m <- pclm(age_x, Dx_PH_M, nlast = 5)
MN_PH_m <- pclm(age_x, Nx_PH_M, nlast = 5)
mD_PH_m <- fitted(MD_PH_m)
mN_PH_m <- fitted(MN_PH_m)
Mx_PH_m <- fitted(MD_PH_m)/fitted(MN_PH_m)


MD_PNH_m <- pclm(age_x, Dx_PNH_M, nlast = 5)
MN_PNH_m <- pclm(age_x, Nx_PNH_M, nlast = 5)
mD_PNH_m <- fitted(MD_PNH_m)
mN_PNH_m <- fitted(MN_PNH_m)
Mx_PNH_m <- fitted(MD_PNH_m)/fitted(MN_PNH_m)


MD_NPH_m <- pclm(age_x, Dx_NPH_M, nlast = 5)
MN_NPH_m <- pclm(age_x, Nx_NPH_M, nlast = 5)
mD_NPH_m <- fitted(MD_NPH_m)
mN_NPH_m <- fitted(MN_NPH_m)
Mx_NPH_m <- fitted(MD_NPH_m)/fitted(MN_NPH_m)


MD_NPNH_m <- pclm(age_x, Dx_NPNH_M, nlast = 5)
MN_NPNH_m <- pclm(age_x, Nx_NPNH_M, nlast = 5)
mD_NPNH_m <- fitted(MD_NPNH_m)
mN_NPNH_m <- fitted(MN_NPNH_m)
Mx_NPNH_m <- fitted(MD_NPNH_m)/fitted(MN_NPNH_m)

#female
MD_BH_f <- pclm(age_x, Dx_BH_F, nlast = 5)
MN_BH_f <- pclm(age_x, Nx_BH_F, nlast = 5)
mD_BH_f <- fitted(MD_BH_f)
mN_BH_f <- fitted(MN_BH_f)
Mx_BH_f <- fitted(MD_BH_f)/fitted(MN_BH_f)


MD_PH_f <- pclm(age_x, Dx_PH_F, nlast = 5)
MN_PH_f <- pclm(age_x, Nx_PH_F, nlast = 5)
mD_PH_f <- fitted(MD_PH_f)
mN_PH_f <- fitted(MN_PH_f)
Mx_PH_f <- fitted(MD_PH_f)/fitted(MN_PH_f)


MD_PNH_f <- pclm(age_x, Dx_PNH_F, nlast = 5)
MN_PNH_f <- pclm(age_x, Nx_PNH_F, nlast = 5)
mD_PNH_f <- fitted(MD_PNH_f)
mN_PNH_f <- fitted(MN_PNH_f)
Mx_PNH_f <- fitted(MD_PNH_f)/fitted(MN_PNH_f)


MD_NPH_f <- pclm(age_x, Dx_NPH_F, nlast = 5)
MN_NPH_f <- pclm(age_x, Nx_NPH_F, nlast = 5)
mD_NPH_f <- fitted(MD_NPH_f)
mN_NPH_f <- fitted(MN_NPH_f)
Mx_NPH_f <- fitted(MD_NPH_f)/fitted(MN_NPH_f)


MD_NPNH_f <- pclm(age_x, Dx_NPNH_F, nlast = 5)
MN_NPNH_f <- pclm(age_x, Nx_NPNH_F, nlast = 5)
mD_NPNH_f <- fitted(MD_NPNH_f)
mN_NPNH_f <- fitted(MN_NPNH_f)
Mx_NPNH_f <- fitted(MD_NPNH_f)/fitted(MN_NPNH_f)


# we need to have a total
Nx_TOT_M <- mN_BH_m + mN_PH_m + mN_PNH_m + mN_NPH_m + mN_NPNH_m
Dx_TOT_M <- mD_BH_m + mD_PH_m + mD_PNH_m + mD_NPH_m + mD_NPNH_m

Nx_TOT_F <- mN_BH_f + mN_PH_f + mN_PNH_f + mN_NPH_f + mN_NPNH_f
Dx_TOT_F <- mD_BH_f + mD_PH_f + mD_PNH_f + mD_NPH_f + mD_NPNH_f

##### The proportion of Death
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

lifetable <- function(x, Nx, Dx, sex="M", ax=NULL){
  m <- length(x)
  mx  <- Dx/Nx
  n <- c(diff(x), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- n/2
      ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="F"){
        if(mx[1]>=0.107){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.053 + 2.800*mx[1]
        }
      }
      if(sex=="M"){
        if(mx[1]>=0.107){
          ax[1] <- 0.330
        }else{
          ax[1] <- 0.045 + 2.684*mx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- 1 / mx[m]
    }
  }
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, Nx, Dx, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

CIdx <- function(x, Nx, Dx, sex = "M", ax = NULL, ns=1000, level=0.95){
  ## point-estimated lifetable
  LT <- lifetable(x, Nx, Dx, sex = sex, ax = ax)
  ## number of ages
  m <- nrow(LT)
  ## estimated probs # put mx over here
  qx <- LT$qx
  ## trials for binomial, rounded
  Ntil <- round(Dx/qx)
  ## simulated death counts
  set.seed(2718)
  ## from Binomial distribution
  Y <- suppressWarnings(matrix(rbinom(m*ns,
                                      Ntil,
                                      qx),
                               m,ns))
  return(Y)
}

Death.cnt_m <- CIdx(age_mat, Nx_TOT_M, Dx_TOT_M, sex = "M")

Death.cnt_f <- CIdx(age_mat, Nx_TOT_F, Dx_TOT_F, sex = "F")

#### Construct average lx ####

# M1_D <- pclm(age_mat, Dx_TOT_M, nlast = 5)
# M1_N <- pclm(age_mat, Nx_TOT_M, nlast = 5)
# plot(M1_D)
# plot(M1_N)
# Dx_m <- fitted(M1_D)
# Nx_m <- fitted(M1_N)
# Mx_m <- Dx_m/Nx_m
# 
# M2_D <- pclm(age_mat, Dx_TOT_F, nlast = 5)
# M2_N <- pclm(age_mat, Nx_TOT_F, nlast = 5)
# plot(M2_D)
# plot(M2_N)
# Dx_f <- fitted(M2_D)
# Nx_f <- fitted(M2_N)
# Mx_f <- Dx_f/Nx_f

# px_func <- function(Mx){
#   px <- c()
#   for (i in 1:length(Mx)) {
#     px2 <- exp(-Mx[i])
#     px <- c(px, px2)
#   }
#   return(px)
# }
# 
# lx_male <- cumprod(px_func(Mx_m))
# 
# lx_female <- cumprod(px_func(Mx_f))
# 
# sum(lx_male)
# sum(lx_female)
# 
# sum(lx_female) - sum(lx_male)

#### bootstrap function ####

px_func <- function(Mx){
  px <- c()
  for (i in 1:length(Mx)) {
    px2 <- exp(-Mx[i])
    px <- c(px, px2)
  }
  return(px)
}

dydx_decomp <- function(rate1, rate2){
  rate_speed <- log(rate2/rate1)
  rate_middle <- (rate1)*exp(0.5 * rate_speed)
  rate_change <- rate_speed * rate_middle
  return(rate_change)
}

gap <- c()
e0m <- c()
e0f <- c()

gap2 <- c()
comp <- c()
mort <- c()

levels <- c(0,0,0,0,0)
mort_lvls <- c(0,0,0,0,0)
comp_lvls <- c(0,0,0,0,0)

agecon40 <- c(rep(0,5))
agecon50 <- c(rep(0,5))
agecon60 <- c(rep(0,5))

for (i in 1:1000) {
  death.male <- Death.cnt_m[,i]
  death.female <- Death.cnt_f[,i]
  
  Mx_tot_m <- death.male/Nx_TOT_M
  Mx_tot_f <- death.female/Nx_TOT_F
  
  lx_male <- cumprod(px_func(Mx_tot_m))
  lx_female <- cumprod(px_func(Mx_tot_f))
  lx_avg <- (lx_male + lx_female)/2
  
  gap <- c(gap, sum(lx_female) - sum(lx_male))
  e0m <- c(e0m, sum(lx_male))
  e0f <- c(e0f, sum(lx_female))
  
  dx_bhm <- PropD_BH_m * death.male
  dx_phm <- PropD_PH_m * death.male
  dx_pnhm <- PropD_PNH_m * death.male
  dx_nphm <- PropD_NPH_m * death.male
  dx_npnhm <- PropD_NPNH_m * death.male
  
  dx_bhf <- PropD_BH_f * death.female
  dx_phf <- PropD_PH_f * death.female
  dx_pnhf <- PropD_PNH_f * death.female
  dx_nphf <- PropD_NPH_f * death.female
  dx_npnhf <- PropD_NPNH_f * death.female
  
  Mx_BH_m <- dx_bhm/mN_BH_m
  Mx_PH_m <- dx_phm/mN_PH_m
  Mx_PNH_m <- dx_pnhm/mN_PNH_m
  Mx_NPH_m <- dx_nphm/mN_NPH_m
  Mx_NPNH_m <- dx_npnhm/mN_NPNH_m
  
  Mx_BH_f <- dx_bhf/mN_BH_f
  Mx_PH_f <- dx_phf/mN_PH_f
  Mx_PNH_f <- dx_pnhf/mN_PNH_f
  Mx_NPH_f <- dx_nphf/mN_NPH_f
  Mx_NPNH_f <- dx_npnhf/mN_NPNH_f
  
  dMx_BH <- dydx_decomp(Mx_BH_m, Mx_BH_f)
  dMx_PH <- dydx_decomp(Mx_PH_m, Mx_PH_f)
  dMx_PNH <- dydx_decomp(Mx_PNH_m, Mx_PNH_f)
  dMx_NPH <- dydx_decomp(Mx_NPH_m, Mx_NPH_f)
  dMx_NPNH <- dydx_decomp(Mx_NPNH_m, Mx_NPNH_f)
  
  dCx_BH <- dydx_decomp(PropN_BH_m, PropN_BH_f)
  dCx_PH <- dydx_decomp(PropN_PH_m, PropN_PH_f)
  dCx_PNH <- dydx_decomp(PropN_PNH_m, PropN_PNH_f)
  dCx_NPH <- dydx_decomp(PropN_NPH_m, PropN_NPH_f)
  dCx_NPNH <- dydx_decomp(PropN_NPNH_m, PropN_NPNH_f)
  
  mMx_BH <- (Mx_BH_m + Mx_BH_f)/2
  mMx_PH <- (Mx_PH_m + Mx_PH_f)/2
  mMx_PNH <- (Mx_PNH_m + Mx_PNH_f)/2
  mMx_NPH <- (Mx_NPH_m + Mx_NPH_f)/2
  mMx_NPNH <- (Mx_NPNH_m + Mx_NPNH_f)/2
  
  mCx_BH <- (PropN_BH_m + PropN_BH_f)/2
  mCx_PH <- (PropN_PH_m + PropN_PH_f)/2
  mCx_PNH <- (PropN_PNH_m + PropN_PNH_f)/2
  mCx_NPH <- (PropN_NPH_m + PropN_NPH_f)/2
  mCx_NPNH <- (PropN_NPNH_m + PropN_NPNH_f)/2
  
  mat_dMx <- cbind(dMx_BH, dMx_PH, dMx_PNH, dMx_NPH, dMx_NPNH)
  mat_mMx <- cbind(mMx_BH, mMx_PH, mMx_PNH, mMx_NPH, mMx_NPNH)
  
  mat_dCx <-  cbind(dCx_BH, dCx_PH, dCx_PNH, dCx_NPH, dCx_NPNH)
  mat_mCx <-  cbind(mCx_BH, mCx_PH, mCx_PNH, mCx_NPH, mCx_NPNH)
  
  # total contribution
  mat_A <- (mat_dMx * mat_mCx) + (mat_dCx * mat_mMx)
  
  mat_B <- matrix(0,80,ncol=5)
  for (i in 1:5) {
    mat_B[,i] <- cumsum(mat_A[,i])
  }
  
  mat_BS <- mat_B * lx_avg
  mat_C <- rowSums(mat_BS)* -1
  
  # mortality contribution
  mat_A1 <- (mat_dMx * mat_mCx)
  
  mat_B1 <- matrix(0,80,ncol=5)
  for (i in 1:5) {
    mat_B1[,i] <- cumsum(mat_A1[,i])
  }
  B1 <- mat_B1 * lx_avg
  C1 <- rowSums(B1)* -1
  
  # composition contribution
  mat_A2 <- (mat_dCx * mat_mMx)
  
  mat_B2 <- matrix(0,80,ncol=5)
  for (i in 1:5) {
    mat_B2[,i] <- cumsum(mat_A2[,i])
  }
  B2 <- mat_B2 * lx_avg
  C2 <- rowSums(B2)* -1
  
  # ### results 
  # # mortality contribution
  # sum(C1)
  # colSums(B1)* -1 # sub-population contribution
  # # population composition contribution
  # sum(C2)
  # colSums(B2)* -1 # sub-population contribution
  # # total contribution
  # sum(mat_C)
  # # barplot(mat_C)
  
  agecon40 <- rbind(agecon40, colSums(mat_BS[1:15,])*-1)
  agecon50 <- rbind(agecon50, colSums(mat_BS[16:35,])*-1)
  agecon60 <- rbind(agecon60, colSums(mat_BS[36:80,])*-1)
  
  gap2 <- c(gap2, sum(mat_C))
  levels <- rbind(levels, colSums(mat_BS)* -1)
  mort <- c(mort, sum(C1))
  comp <- c(comp, sum(C2))
  mort_lvls <- rbind(mort_lvls, colSums(B1)*-1)
  comp_lvls <- rbind(comp_lvls, colSums(B2)*-1)
}

CI.e0m <- quantile(e0m, probs = c((1-0.95)/2, 1 - (1-0.95)/2))
mean.e0m <- mean(e0m)
CI.e0f <- quantile(e0f, probs = c((1-0.95)/2, 1 - (1-0.95)/2))
mean.e0f <- mean(e0f)

CI.gap2 <- quantile(gap2, probs = c((1-0.95)/2, 1 - (1-0.95)/2))
mean.gap2 <- mean(gap2)
CI.mort <- quantile(mort, probs = c((1-0.95)/2, 1 - (1-0.95)/2))
mean.mort <- mean(mort)
CI.comp <- quantile(comp, probs = c((1-0.95)/2, 1 - (1-0.95)/2))
mean.comp <- mean(comp)

qtl.fun <- function(x){quantile(x, probs = c((1-0.95)/2, 1 - (1-0.95)/2))}

CI.levels <- apply(levels[-1,], 2, qtl.fun)
mean.levels <- colMeans(levels[-1,])
CI.mort_lvls <- apply(mort_lvls[-1,], 2, qtl.fun)
mean.mort_lvls <- colMeans(mort_lvls[-1,])
CI.comp_lvls <- apply(comp_lvls[-1,], 2, qtl.fun)
mean.comp_lvls <- colMeans(comp_lvls[-1,])

### LE <- c(2.56,3.81,4.51,4.3,6.09)

data.table::data.table(
  mean = c(round(mean.gap2,2), round(mean.mort,2), round(mean.comp,2)),
  upper = c(round(CI.gap2[2],2), round(CI.mort[2],2), round(CI.comp[2],2)),
  lower = c(round(CI.gap2[1],2), round(CI.mort[1],2), round(CI.comp[1],2))
)

tbl_lvls <- 
data.table::data.table(
  mort.mean = mean.mort_lvls,
  comp.mean = mean.comp_lvls,
  levels.mean = mean.levels
)
tbl_lvls
tbl_lvls <- apply(tbl_lvls, 2, abs)
tbl_lvls_csum <- colSums(tbl_lvls)
tbl_lvls[,1]/tbl_lvls_csum[1]
tbl_lvls[,2]/tbl_lvls_csum[2]


mean.agecon40 <- colMeans(agecon40)
CI.agecon40 <- apply(agecon40, 2, qtl.fun)
mean.agecon50 <- colMeans(agecon50)
CI.agecon50 <- apply(agecon50, 2, qtl.fun)
mean.agecon60 <- colMeans(agecon60)
CI.agecon60 <- apply(agecon60, 2, qtl.fun)


library(dplyr)
library(ggplot2)

age_con <- 
data.table::data.table(
  age = c(rep("25~39",5), rep("40~59",5), rep("60+",5)),
  edu = c(rep(c("High", "MH","Medium", "ML", "Low"),3)),
  cont = c(mean.agecon40, mean.agecon50, mean.agecon60),
  upper = c(rep(CI.agecon40[2,] + CI.agecon50[2,] + CI.agecon60[2,],3)),
  lower = c(rep(CI.agecon40[1,] + CI.agecon50[1,] + CI.agecon60[1,],3))
)
age_con$edu <- factor(age_con$edu, levels = c("High", "MH","Medium", "ML", "Low"))


age_con <- age_con %>% bind_rows(
  age_con %>% group_by(edu) %>% summarise(labl = sum(cont))
)

prop <- age_con %>% group_by(age) %>% summarise(tot = sum(cont))
prop$tot[-4]/sum(prop$tot[-4])

p1 <- 
ggplot(age_con, aes(edu,cont, fill = age))+
  geom_bar(stat = "identity", width = 0.95)+
  geom_text(aes(y = labl,label = paste0(round(labl,2))), 
            position = position_dodge(0.5), vjust = -1.5)+
  geom_errorbar(aes(ymax = upper, ymin = lower), 
                linetype = 2, width = 0.4, size = 1.2)+
  coord_cartesian(ylim = c(0,2))+
  labs(subtitle = "Age-group & Education levels contribution, AUS 2016",
       x = "Education levels", y = "Contribution")+
  guides(fill = guide_legend("Age group"))+
  theme_bw()
ggsave("reports/PNAS-age.pdf")
