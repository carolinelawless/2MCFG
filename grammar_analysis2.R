remove(list=ls())
source("functions.R")
library(tictoc)
library(seqinr)
library(LaplacesDemon)

tic()

g<- "doubles"
M<- 100000
number_sentences<- 10
len<- 10
alpha1 <- 0.00001 #scaling parameter for DP over nonterminals
alpha2 <- 0.00001 #scaling parameter for DP over rules
b1<- 10 #Beta parameters for type = emission
b2<- 10
c1<- 1 #Beta parameters for epsilon
c2<- 100000
permutations_param<- 0.05

description<- paste0("G=",g,"_M=",M,"_S=",number_sentences,"_alpha1=",alpha1,"_alpha2=",alpha2,"_b1=",b1,"_c2=",c2,"_len=",len,"_P=",permutations_param)
print(description)

terminals<- c("a","b","c")



sentences<- list()

for(i in 1:number_sentences){
  sent_short<- sample(terminals,len/2,replace = TRUE)
  if(g=="copy"){
    sentences[[length(sentences)+1]]<- rep(sent_short,2)
  }else if(g=="doubles"){
    sentences[[length(sentences)+1]]<- rep(sent_short,each=2)
  }
}



source("SMC.R")
source("functions2.R")

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
(p_rules_ordered[[1]][[5]]-1)%/%24
p_rules_ordered[[1]][[5]]%%24

Q<- 1000 #number of sentences to generate
sim_sentences<- list()
sentence_length<- vector(length=Q)
edit_distance<- vector(length = Q)
qq<- 0
for(QQ in 1:Q){
  print(paste0("QQ=",QQ))
  #### construct a tree
  
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
      
      if(length(which(type_matrix[,1] == nonterminal))==0){
        type_matrix<- rbind(type_matrix,c(nonterminal,b1,b2))
        epsilon_matrix<- rbind(epsilon_matrix,c(nonterminal,c1,c2))
      }
      if(length(which(terminals_matrix[,1] == nonterminal))==0){
        terminals_matrix<- rbind(terminals_matrix,c(nonterminal,rep(1,length(terminals))))
      }
      
      rule_all<- dp_random2(nonterminal, minimum, maximum)
      rule<- rule_all[[1]]
      stop<- rule_all[[2]]
      draws<- rule_all[[3]]
      permutation1<- rule_all[[4]]
      rule_index<- rule_all[[5]]
      
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
      tree_matrix
      
    }
  }
  
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
  if(length(which(simulated_sentence == ""))>0){
    simulated_sentence<- simulated_sentence[-(which(simulated_sentence == ""))]
  }
  if(length(which(is.na(simulated_sentence)))>0){
    simulated_sentence<- simulated_sentence[-(which(is.na(simulated_sentence)))]
  }
  
  if(g=="copy"){
    cut<- round(length(simulated_sentence)/2)
    s1<- simulated_sentence[1:cut]
    s2<- simulated_sentence[(cut+1):length(simulated_sentence)]
    s1<- paste0(s1,collapse = "")
    s2<- paste0(s2,collapse = "")
    edit_distance[QQ]<- adist(s1,s2)
  }else if(g=="doubles"){
    s1<- simulated_sentence[seq(1,length(simulated_sentence),2)]
    s2<- simulated_sentence[seq(2,length(simulated_sentence),2)]
    z = rle(rle(simulated_sentence)$lengths %% 2)
    edit_distance[[QQ]]<- sum(ceiling(z$lengths[z$values == 1] / 2))
  }
  s1<- paste0(s1,collapse = "")
  s2<- paste0(s2,collapse = "")
  sentence_length[QQ]<- length(simulated_sentence)
  sim_sentences[[length(sim_sentences)+1]]<- simulated_sentence
  if(s1 == s2){qq<- qq+1}
}
print(description)
print("proportion of correct estimates:")
print(qq/Q)

#e_rules<- unique_ordered(e_rules)
e_rule_frequencies<- unique_frequencies(e_rules)

r_object2<-list()
r_object2[[1]]<- description
r_object2[[2]]<- ug
r_object2[[3]]<- ug_frequencies
r_object2[[4]]<- e_rules
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
r_object2[[17]]<- edit_distance
r_object2[[18]]<- qq/Q

toc()

filename<- paste0(Sys.Date(),"_",description,"_analysis")
save(r_object2,file=filename)
