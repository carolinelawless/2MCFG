remove(list=ls())
source("functions2.R")
library(seqinr)
library(LaplacesDemon)

#load("2023-07-02-doubles-s200-S100-m1000-sent_len_range4to12") 
load("2023-07-05-doubles-s100-S49-m1000-sent_len_range4to10") 
#load("2023-07-03-copy-s200-S100-m10000-sent_len_range4to12")
#load("2023-07-03-doubles-s200-S100-m5000-sent_len_range4to12")

g<- "doubles"
number_particles<- 1000
number_sentences<- 100

terminals<- c("a","b","c")

C_rules<- 0 #factor to add to each of the observed rules
C_nonterminals<- 0 #factor to add to each of the observed nonterminals
alpha1 <- 50 #scaling parameter for DP over nonterminals
alpha2 <- 50 #scaling parameter for DP over rules
a1<- 1 #Gamma parameters for poisson
a2<- 1
b1<- 1000 #Beta parameters for type = emission
b2<- 1
c1<- 1 #Beta parameters for epsilon
c2<- 1000

list_nonterminals_vec_long<- r_object[[1]]
list_nonterminals_vec_short<- r_object[[2]]
list_p_rules<- r_object[[3]]
list_e_rules<- r_object[[4]]
list_gamma_matrix<- r_object[[5]]
list_type_matrix<- r_object[[6]]
list_epsilon_matrix<- r_object[[7]]
list_terminals_matrix<- r_object[[8]]
list_tree_matrix<- r_object[[9]]
list_left_functions<- r_object[[10]]
list_right_functions<- r_object[[11]]
list_rows<- r_object[[12]]
list_sides<- r_object[[13]]
list_numbers<- r_object[[14]]
description<- r_object[[15]]
sentences<- r_object[[16]]
permutation_parameters<- r_object[[17]]

list_grammars_all<- list()
M<- length(list_tree_matrix)
for(i in 1:M){
  list_grammars_all[[i]]<- list(list_e_rules[[i]],list_p_rules[[i]], list_nonterminals_vec_short[[i]],list_nonterminals_vec_long[[i]],list_gamma_matrix[[i]],list_type_matrix[[i]],list_epsilon_matrix[[i]],list_terminals_matrix[[i]]) 
}
names(list_grammars_all)<- c("emissions","productions","nt_short","nt_long","gamma","type","epsilon","terminals")

ug<- unique_ordered(list_grammars_all)
ug_frequencies<- unique_frequencies(list_grammars_all)

#### construct a tree

grammar<- ug[[1]]
e_rules<- grammar[[1]]
p_rules<- grammar[[2]]
nonterminals_vec_short<- grammar[[3]]
nonterminals_vec_long<- grammar[[4]]
gamma_matrix<- grammar[[5]]
type_matrix<- grammar[[6]]
epsilon_matrix<- grammar[[7]]
terminals_matrix<- grammar[[8]]
e_rules_ordered<- unique_ordered(e_rules)
e_rules_frequencies<- unique_frequencies(e_rules)


####
p_rule_frequencies<- vector(length = length(p_rules))
for(i in 1:length(p_rules)){
p_rule_frequencies[i]<- p_rules[[i]][[6]]
}
p_rule_mode<- p_rules[[which.max(p_rule_frequencies)]]
####

Q<- 1000
sentence_length<- vector(length=Q)
qq<- 0
for(QQ in 1:Q){

stop<- TRUE
while(stop == TRUE){

stop<- FALSE
tree_matrix<- matrix(ncol=8)
colnames(tree_matrix)<- c("ind_1","ind_2","n_children","type","min","max","B","nrows")
tree_matrix[1,2]<- 1
tree_matrix[1, 5]<- 3
tree_matrix[1, 6]<- 20
tree_matrix[1, 7]<- 1
tree_matrix[1,8]<- 1
left_functions<- list()
right_functions<- list()

row_vec<- tree_matrix[,3]
while(sum(is.na(row_vec))>0 & stop == FALSE){
row<- which(is.na(row_vec))[1]
rows<- row
row1<- row
while(is.na(tree_matrix[row1,1])==FALSE){
row1<- tree_matrix[row1,1]
rows<- c(row1,rows)
}
nonterminal<- tree_matrix[row,7]
minimum<- tree_matrix[row,5]
maximum<- tree_matrix[row,6]

if(length(which(gamma_matrix[,1] == nonterminal))==0){
  gamma_matrix<- rbind(gamma_matrix,c(nonterminal,a1,a2))
  type_matrix<- rbind(type_matrix,c(nonterminal,b1,b2))
  epsilon_matrix<- rbind(epsilon_matrix,c(nonterminal,c1,c2))
}
if(length(which(terminals_matrix[,1] == nonterminal))==0){
  terminals_matrix<- rbind(terminals_matrix,c(nonterminal,rep(1,length(terminals))))
}

rule_all<- dp_random(nonterminal, minimum, maximum)
rule<- rule_all[[1]]
stop<- rule_all[[2]]
draws<- rule_all[[3]]

if(draws[2]==0){#production
  tree_matrix[row, 3]<- 2
  left_functions[[row]]<- as.list(rule[[3]])
  right_functions[[row]]<- as.list(rule[[4]])
  nc <- 2 #number of children
  nr <- nrow(tree_matrix) #number of rows
  for(cc in 1:nc){
    tree_matrix <-rbind(tree_matrix, c(row, nr + cc, rep(NA, 2), 1, maximum - nc + 1, rule[[2]][cc],nr+nc))
    left_functions[[nr + cc]] <- list()
    right_functions[[nr + cc]] <- list()
  }
  minimum<- max(tree_matrix[row,5],nc)
  tree_matrix[row,5]<- minimum
  tree_matrix<- update("min")
}else if(draws[2]==1){#emission

  tree_matrix[row, 3]<- 0
  left_functions[[row]]<- rule[2]
  right_functions[[row]]<- rule[3]
  if(rule[2]==""|rule[3]==""){
    maximum<- 1
    tree_matrix[row,6]<- maximum
    tree_matrix<- update("max")
  }else{
    maximum<- 2
    tree_matrix[row,6]<- maximum
    tree_matrix<- update("max")
    minimum<- 2
    tree_matrix[row,5]<- minimum
    tree_matrix<- update("min")
  }
}

row_vec<- tree_matrix[,3]
}
}
stop
tree_matrix


###evaluate the tree

row<- 1
side<- "left"
number<- 1
rows<- row
sides<- side
numbers<- number
string<- left_functions[[row]]

count<- 0
if(length(left_functions[[1]])>0){
  for(j in 1:length(left_functions[[1]])){
    count<- count+ is.numeric(left_functions[[1]][[j]])}
}
if(length(right_functions[[1]])>0){
  for(j in 1:length(right_functions[[1]])){
    count<- count+ is.numeric(right_functions[[1]][[j]])}
}


while(count > 0){
  
  if(tree_matrix[row,3]==2){
    
    if(side=="left"){
      string<- left_functions[[row]]
    }else if(side=="right"){
      string<- right_functions[[row]]
    }
    vec<- vector(length=length(string))
    for(k in 1:length(vec)){vec[k]<- is.numeric(string[[k]])}
    
    if(row==1 & side == "left" & sum(vec) == 0 ){
      sides<- "right"
      side<- "right"
    }else{
      
      if(sum(vec)==0){
        if(length(string)==1){
          if(is.na(string)==TRUE){
            x<- ""
          }else{x<- string}
        }else{
          x<- paste(string,collapse="")
        }
        if(side=="left"){
          left_functions[[row]]<- x
        }else if(side=="right"){
          right_functions[[row]]<- x
        }
        row <- rows[(length(rows) - 1)]
        side <- sides[(length(sides) - 1)]
        numb<- number
        number<- numbers[(length(numbers)-1)]
        rows <- rows[1:(length(rows) - 1)]
        sides <- sides[1:(length(sides) - 1)]
        numbers<- numbers[1:(length(numbers)-1)]
        if (side == "left") {
          string1 <- left_functions[[row]]
          left_functions[[row]] <- evaluate(string1, x, numb)
        } else if (side == "right") {
          string1 <- right_functions[[row]]
          right_functions[[row]] <- evaluate(string1, x, numb)
        }
        
      }else{
        
        ii<- 1
        while(is.numeric(string[[ii]])==FALSE){ii<- ii+1}
        
        number <- string[[ii]]
        if (ceiling(number / 2) == number / 2) {
          side <- "right"
        } else if (ceiling(number / 2) != number / 2) {
          side <- "left"
        }
        row <- tree_matrix[row,8] + ceiling(number / 2)
        sides <- c(sides, side)
        rows <- c(rows, row)
        numbers<- c(numbers, number)
        
        
        
      }
    }
  }else if(tree_matrix[row,3]==0){
    
    if(side == "left"){
      x<- left_functions[[row]]
    }else{
      x<- right_functions[[row]]
    }
    
    if(length(rows)!=1){
      row <- rows[(length(rows) - 1)]
      side <- sides[(length(sides) - 1)]
      numb<- number
      number<- numbers[(length(numbers) -1)]
      rows <- rows[1:(length(rows) - 1)]
      sides <- sides[1:(length(sides) - 1)]
      numbers<- numbers[1:(length(numbers) -1)]
      
      if (side == "left") {
        string1 <- left_functions[[row]]
        left_functions[[row]] <- evaluate(string1,x,numb)
      } else if (side == "right") {
        string1 <- right_functions[[row]]
        right_functions[[row]] <- evaluate(string1, x, numb)
      }
    }
  }
  
  count1<- 0
  if(length(left_functions[[1]])>0){
    for(j in 1:length(left_functions[[1]])){
      count1<- count1+ is.numeric(left_functions[[1]][[j]])}
  }
  if(length(right_functions[[1]])>0){
    for(j in 1:length(right_functions[[1]])){
      count1<- count1+ is.numeric(right_functions[[1]][[j]])}
  }
  count<- count1
}

simulated_sentence<- vector(length = 0)

for(i in 1:length(left_functions[[1]])){
words<- left_functions[[1]][[i]]
words_sep<- s2c(words)
simulated_sentence<- c(simulated_sentence,words_sep)
}
for(i in 1:length(right_functions[[1]])){
  words<- right_functions[[1]][[i]]
  words_sep<- s2c(words)
  simulated_sentence<- c(simulated_sentence,words_sep)
}
simulated_sentence
if(length(which(simulated_sentence == ""))>0){
simulated_sentence<- simulated_sentence[-(which(simulated_sentence == ""))]
}
if(length(which(is.na(simulated_sentence)))>0){
  simulated_sentence<- simulated_sentence[-(which(is.na(simulated_sentence)))]
}
simulated_sentence

if(g=="copy"){
cut<- round(length(simulated_sentence)/2)
s1<- simulated_sentence[1:cut]
s2<- simulated_sentence[(cut+1):length(simulated_sentence)]
}else if(g=="doubles"){
s1<- simulated_sentence[seq(1,length(simulated_sentence),2)]
s2<- simulated_sentence[seq(2,length(simulated_sentence),2)]
}
s1<- paste0(s1,collapse = "")
s2<- paste0(s2,collapse = "")
sentence_length[QQ]<- length(simulated_sentence)
if(s1 == s2){qq<- qq+1}
}
print(paste0("grammar:",g," particles:",number_particles," sentences:",number_sentences))
print("proportion of correct estimates:")
print(qq/Q)