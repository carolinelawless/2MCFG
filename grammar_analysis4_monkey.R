remove(list=ls())
source("functions.R")
library(tictoc)
library(seqinr)
library(LaplacesDemon)

start<- Sys.time()
tic()

number_initiations<- 3
g<- "monkey"
M<- 5000
len<- 30
alpha1 <- 0.5 #scaling parameter for DP over nonterminals
alpha2 <- 0.5 #scaling parameter for DP over rules
b1<- 10 #Beta parameters for type = emission
b2<- 10
c1<- 1 #Beta parameters for epsilon
c2<- 10
permutations_param<- 0.01


list_number_nonterminals<- list()
list_number_p_rules<- list()
list_number_e_rules<- list()
list_number_nonterminals_3<- list()
list_number_p_rules_3<- list()
list_number_e_rules_3<- list()
list_number_nonterminals_10<- list()
list_number_p_rules_10<- list()
list_number_e_rules_10<- list()
list_number_permutations<- list()
list_number_permutations_3<- list()
list_number_permutations_10<- list()

list_nonterminals_vec_long<- list()
list_nonterminals_vec_short<- list()
list_p_rules<- list()
list_p_rules_short<- list()
list_e_rules<- list()
list_type_matrix<- list()
list_epsilon_matrix<- list()
list_terminals_matrix<- list()
list_permutations_vec<- list()

sentences<- list()


data1<- "data_rp.txt"
description<- paste0("G=",g,"_M=",M,"_alpha1=",alpha1,"_alpha2=",alpha2,"_b1=",b1,"_c2=",c2,"_P=",permutations_param,"_data=",data1, "_init=",number_initiations)
print(description)
terminals<- c("X","r","p")
data<- read.table(data1, header = TRUE)
data<- as.matrix(data)
data<- as.vector(data)
number_sentences<- length(data)
ESS<- vector(length = length(sentences))
number_sentences1<- round(number_sentences/3)
number_sentences2<- number_sentences - number_sentences1
for(i in 1:number_sentences1){
  sentences[[length(sentences)+1]]<- s2c(data[i])
}

list_nonterminals_vec_long_init<- list()
list_nonterminals_vec_short_init<- list()
list_p_rules_init<- list()
list_p_rules_short_init<- list()
list_e_rules_init<- list()
list_type_matrix_init<- list()
list_epsilon_matrix_init<- list()
list_terminals_matrix_init<- list()
list_permutations_vec_init<- list()

for(j in 1:number_initiations){
  
  source("SMC.R")
  
  list_nonterminals_vec_long_init<- c(list_nonterminals_vec_long,list_nonterminals_vec_long_init)
  list_nonterminals_vec_short_init<- c(list_nonterminals_vec_short,list_nonterminals_vec_short_init)
  list_p_rules_init<- c(list_p_rules,list_p_rules_init)
  list_p_rules_short_init<- c(list_p_rules_short,list_p_rules_short_init)
  list_e_rules_init<- c(list_e_rules,list_e_rules_init)
  list_type_matrix_init<- c(list_type_matrix,list_type_matrix_init)
  list_epsilon_matrix_init<- c(list_epsilon_matrix,list_epsilon_matrix_init)
  list_terminals_matrix_init<- c(list_terminals_matrix,list_terminals_matrix_init)
  list_permutations_vec_init<- c(list_permutations_vec,list_permutations_vec_init)
  
}

sentences<- list()
for(i in 1:number_sentences2){
  sentences[[length(sentences)+1]]<- s2c(data[number_sentences1 + i])
}


M<- 2*M

list_nonterminals_vec_long<- list_nonterminals_vec_long_init
list_nonterminals_vec_short<- list_nonterminals_vec_short_init
list_p_rules<- list_p_rules_init
list_p_rules_short<- list_p_rules_short_init
list_e_rules<- list_e_rules_init
list_type_matrix<- list_type_matrix_init
list_epsilon_matrix<- list_epsilon_matrix_init
list_terminals_matrix<- list_terminals_matrix_init
list_permutations_vec<- list_permutations_vec_init

source("SMC4.R")

list_grammars_all<- list()
for(i in 1:M){
  list_grammars_all[[i]]<- list(list_e_rules[[i]],list_p_rules[[i]], list_nonterminals_vec_short[[i]],list_nonterminals_vec_long[[i]],list_type_matrix[[i]],list_epsilon_matrix[[i]],list_terminals_matrix[[i]],list_permutations_vec[[i]]) 
}

ug<- unique_ordered(list_grammars_all)
ug_frequencies<- unique_frequencies(list_grammars_all)

grammar<- ug[[1]]
e_rules<- grammar[[1]]
p_rules<- grammar[[2]]
nonterminals_vec_short<- grammar[[3]]
nonterminals_vec_long<- grammar[[4]]
type_matrix<- grammar[[5]]
epsilon_matrix<- grammar[[6]]
terminals_matrix<- grammar[[7]]
permutations_vec<- grammar[[8]]
p_rules_short<- list()
for(i in 1:length(p_rules)){
  p_rules_short[[length(p_rules_short)+1]]<- list(p_rules[[i]][[1]],p_rules[[i]][[2]],p_rules[[i]][[3]],p_rules[[i]][[4]],p_rules[[i]][[6]])
}
e_rules_ordered<- unique_ordered(e_rules)
e_rules_frequencies<- unique_frequencies(e_rules)
p_rules_ordered<- unique_ordered(p_rules_short)
p_rules_frequencies<- unique_frequencies(p_rules_short)
p_rules_ordered[[1]]
e_rules_ordered[[1]]
e_rules_ordered[[2]]
e_rules_ordered[[3]]

duration<- Sys.time() - start

source("simulate_sentences.R")

r_object2<-list()
r_object2[[1]]<- description
#r_object2[[2]]<- ug #too long to load
r_object2[[3]]<- ug_frequencies 
#r_object2[[4]]<- e_rules #too long to load
r_object2[[2]]<- sentences
r_object2[[4]]<- duration
r_object2[[5]]<- e_rules_ordered
r_object2[[6]]<- e_rules_frequencies
r_object2[[7]]<- p_rules_ordered
r_object2[[8]]<- p_rules_frequencies
r_object2[[9]]<- nonterminals_vec_long
r_object2[[10]]<- nonterminals_vec_short
r_object2[[11]]<- type_matrix
r_object2[[12]]<- epsilon_matrix
r_object2[[13]]<- terminals_matrix
r_object2[[14]]<- permutations_vec
r_object2[[15]]<- sim_sentences
r_object2[[16]]<- sentence_length
r_object2[[17]]<- NA
r_object2[[18]]<- qq/Q
r_object2[[19]]<- ESS
r_object2[[20]]<- list_number_p_rules
r_object2[[21]]<- list_number_e_rules
r_object2[[22]]<- list_number_nonterminals
r_object2[[23]]<- list_number_p_rules_3
r_object2[[24]]<- list_number_e_rules_3
r_object2[[25]]<- list_number_nonterminals_3
r_object2[[26]]<- list_number_p_rules_10
r_object2[[27]]<- list_number_e_rules_10
r_object2[[28]]<- list_number_nonterminals_10
r_object2[[29]]<- list_number_permutations
r_object2[[30]]<- list_number_permutations_3
r_object2[[31]]<- list_number_permutations_10

print(description)

toc()

filename<- paste0(Sys.Date(),"_",description,"_analysis")
save(r_object2,file=filename)
