remove(list=ls())

setwd("C:/Users/Caroline/Documents/PhD/2MCFG")
source("DP_SMC3.R")

##sentence abc
p1_e3<- (1-proba_emission)*dpois(1, gamma)*(proba_emission*proba_epsilon)^3
p1_e2<- (1-proba_emission)*dpois(0,gamma)*proba_emission*(1-proba_epsilon)*proba_emission*proba_epsilon*2
p2_e3<- ((1-proba_emission)*dpois(0,gamma))^2*(proba_emission*proba_epsilon/2)^3*(2^4+2^5)/3
total<- p1_e3+p1_e2+p2_e3
p1_e3<- p1_e3/total
p1_e2<- p1_e2/total
p2_e3<- p2_e3/total

p1_e3_est<- length(which(emission_count==3 & production_count ==1))/M
p1_e2_est<- length(which(emission_count==2))/M
p2_e3_est<- length(which(production_count==2))/M
total_est<- p1_e3_est+p1_e2_est+p2_e3_est

p1_e3
p1_e3_est
p1_e2
p1_e2_est
p2_e3
p2_e3_est

s3<- p1_e3
s3_est<- p1_e3_est
s2<- p1_e2 + p2_e3
s2_est<- p1_e2_est + p2_e3_est

s3
s3_est
s2
s2_est


##sentence ab

p1_e2<- (1-proba_emission)*dpois(0,gamma)*(proba_emission*proba_epsilon)^2
p0_e1<- proba_emission*(1-proba_epsilon)
total<- p1_e2 + p0_e1
p1_e2<- p1_e2/total
p0_e1<- p0_e1/total
p1_e2_est<- length(which(emission_count==2 & production_count==1))/M
p0_e1_est<- length(which(production_count==0))/M
p1_e2_est + p0_e1_est

p1_e2
p1_e2_est
p0_e1
p0_e1_est

nterminals<- list_nonterminals_long[which(production_count==1)]
count<- 0
for(i in 1:length(nterminals)){
pair<- nterminals[[i]]
if(pair[2]==1){
count<- count +1
}
}
count/length(nterminals)
