#library(LaplacesDemon)
##Draw from the DP of nonterminals ##returns a vector of length n of nonterminals
draw_nt<- function(n){
  w<- 1
  nt<- vector(length=n)
  ntv<-nonterminals_vec_long
  for(i in 1:n){
    if(length(ntv)==0){
      nt[i]<- 1
    }else{
      draw<- sample(c("new","old"),1,prob=c(alpha1,length(ntv)))
      if(draw=="old"){
        tab<- table(ntv)
        s<- sample(1:length(tab), 1, replace= TRUE, prob = (tab + C_nonterminals))
        nt[i]<- s
        w<- w*(tab[s]/sum(tab)/((tab[s]+C_nonterminals)/sum(tab + C_nonterminals)))
      }else if(draw=="new"){
        nt[i]<- max(ntv)+1
      }
    }
    ntv<- c(ntv,nt[i])
  }
  return(list(nt,w))
}

##Random base production rule

base_production_random<- function(nonterminal){
  weight<- 1
  permutation_weights<- rdirichlet(1,permutations_vec)
  permutation<- sample(factorial(5),1,prob=permutation_weights)
  if((permutation-1) %/% 24 == 0){
    cut<- 1
  }else if((permutation-1) %/% 24 ==1){
    cut<- 2
  }else if((permutation-1) %/% 24 == 2){
    cut<- 3
  }else if((permutation-1) %/% 24 == 3){
    cut<- 4
  }else if((permutation-1) %/% 24 == 4){
    cut<- 5
  }
  if(permutation%%24 == 1){
    sigma<- c(1,2,3,4)
  }else if(permutation%%24 == 2){
    sigma<- c(1,2,4,3)
  }else if(permutation%%24 == 3){
    sigma<- c(1,3,2,4)
  }else if(permutation%%24 == 4){
    sigma<- c(1,3,4,2)
  }else if(permutation%%24 == 5){
    sigma<- c(1,4,2,3)
  }else if(permutation%%24 ==6){
    sigma<- c(1,4,3,2)
  }else if(permutation%%24 == 7){
    sigma<- c(2,1,3,4)
  }else if(permutation%%24 == 8){
    sigma<- c(2,1,4,3)
  }else if(permutation%%24 == 9){
    sigma<- c(2,3,1,4)
  }else if(permutation%%24 == 10){
    sigma<- c(2,3,4,1) 
  }else if(permutation%%24 == 11){
    sigma<- c(2,4,1,3)
  }else if(permutation%%24 == 12){
    sigma<- c(2,4,3,1) 
  }else if(permutation%%24 == 13){
    sigma<- c(3,1,2,4)
  }else if(permutation%%24 == 14){
    sigma<- c(3,1,4,2)
  }else if(permutation%%24 == 15){
    sigma<- c(3,2,1,4)
  }else if(permutation%%24 == 16){
    sigma<- c(3,2,4,1)
  }else if(permutation%%24 == 17){
    sigma<- c(3,4,1,2)
  }else if(permutation%%24 ==18){
    sigma<- c(3,4,2,1)
  }else if(permutation%%24 == 19){
    sigma<- c(4,1,2,3)
  }else if(permutation%%24 == 20){
    sigma<- c(4,1,3,2)
  }else if(permutation%%24 == 21){
    sigma<- c(4,2,1,3)
  }else if(permutation%%24 == 22){
    sigma<- c(4,2,3,1) 
  }else if(permutation%%24 == 23){
    sigma<- c(4,3,1,2)
  }else if(permutation%%24 == 0){
    sigma<- c(4,3,2,1) 
  }
  
  N<- 2
  if(cut==1){
    string1<- NA
    string2<- sigma
  }else if(cut==(2*N+1)){
    string1<- sigma
    string2<- NA
  }else{
    string1<- sigma[1:(cut-1)]
    string2<- sigma[cut:(2*N)]
  }
  nonterminals_with_weights<- draw_nt(N)
  nonterminals<- nonterminals_with_weights[[1]]
  weight<- weight*nonterminals_with_weights[[2]]
  
  
  r<- list(nonterminal, nonterminals, string1, string2, N,0,weight,permutation)
  return(r)
}

dp_random<- function(nonterminal,minimum,maximum){
draws<- vector()
stop<- FALSE
p_rules_star<- list()
e_rules_star<- list()
for(i in 1:length(p_rules)){
if(p_rules[[i]][[1]]==nonterminal){p_rules_star[[length(p_rules_star)+1]]<- p_rules[[i]]}
}

for(i in 1:length(e_rules)){
if(e_rules[[i]][1]==nonterminal){e_rules_star[[length(e_rules_star)+1]]<- e_rules[[i]]}
}

if(length(p_rules_star) > 0){
p_frequencies<- vector(length = length(p_rules_star))
for(i in 1:length(p_rules_star)){
p_frequencies[i]<- p_rules_star[[i]][[6]]
}
}else{
p_frequencies <- 0
}
p_freq_total<- sum(p_frequencies)

old_rule_total<- p_freq_total + length(e_rules_star)

draw1<- sample(0:1,1,prob=c(alpha2,old_rule_total))
draws<- c(draws,draw1)
if(draw1==0){#new rule
index1<- which(type_matrix[,1]==nonterminal)
proba_emission<- rbeta(1,type_matrix[index1,2],type_matrix[index1,3])
proba_epsilon<- rbeta(1,epsilon_matrix[index1,2],epsilon_matrix[index1,3])

draw2<- sample(0:1,1,prob=c(1-proba_emission,proba_emission))
draws<- c(draws,draw2)
if(draw2==0){#new production
if(maximum == 1){stop<- TRUE}
rule<- base_production_random(nonterminal)
}else{#new emission
if(minimum > 2){stop<- TRUE}
index2<- which(terminals_matrix[,1]==nonterminal)
emission_parameters<- terminals_matrix[index2,2:ncol(terminals_matrix)]
emission_probas<- rdirichlet(1,emission_parameters)
draw3<- sample(0:2,1,prob = c(1 - proba_epsilon,proba_epsilon/2,proba_epsilon/2))
draws<- c(draws,draw3)
if(draw3 == 0){
if(maximum == 1){stop<- TRUE}
x<- sample(terminals,1,prob = emission_probas)
y<- sample(terminals,1,prob = emission_probas)
}else if(draw3 ==1){
if(minimum ==2){stop<- TRUE}
x<- sample(terminals,1,prob=emission_probas)
y<- ""
}else if(draw3 ==2){
if(minimum == 2){stop<- TRUE}
x<- ""
y<- sample(terminals,1, prob=emission_probas)
}
rule<- c(nonterminal,x,y)
}
}else{#old rule
draw2<- sample(0:1,1,prob=c(p_freq_total,length(e_rules_star)))
draws<- c(draws,draw2)
if(draw2 == 0){#old production
if(maximum == 1){stop<- TRUE}
samp<- sample(1:length(p_rules_star),1, prob = p_frequencies)
rule<- p_rules_star[[samp]]
}else{#old emission
samp<- sample(1:length(e_rules_star),1)
rule<- e_rules_star[[samp]]
if(rule[2]=="" | rule[[3]]==""){
if (minimum >1 ){stop<- TRUE}
}else{
if(minimum > 2){stop<- TRUE}
}
}
}
return(list(rule,stop,draws))
}

evaluate<- function(string1,x,numb){
  w<- which(string1==numb)
  s<- append(string1,x,after=w)
  s<- s[-w]
  return(s)
}

update<- function(min_or_max){
  if(row !=1){
  tm<- tree_matrix
  l<- length(rows)
  if(min_or_max == "min"){
    for(i in 1:(l-1)){
      par<- rows[l-i]
      ch<- which(tm[,1] == par)
      tm[par,5]<- max(tm[par,5], sum(tm[ch,5]))
    }
    for(node in 2:nrow(tm)){
      par<- tm[node,1]
      ch<- which(tm[,1]==par)
      update<- tm[par,6] - sum(tm[ch,5]) + tm[node,5]
      tm[node,6]<- min(tm[node,6], update)
    }
  }else if(min_or_max== "max"){
    for(i in 1:(l-1)){
      par<- rows[l-i]
      ch<- which(tm[,1] == par)
      tm[par,6]<- min(tm[par,6], sum(tm[ch,6]))
    }
    for(node in 2:nrow(tm)){
      par<- tm[node,1]
      ch<- which(tm[,1]==par)
      update<- tm[par,5] - sum(tm[ch,6]) + tm[node,6]
      tm[node,5]<- max(tm[node,5], update)
    }
  }
  return(tm)
  }else{return(tree_matrix)}
}

weight_e2<- function(nonterminal){
  w<- 1
  
  nt_ind<- which(terminals_matrix[,1]==nonterminal)
  if(x!=""){
    x_ind<- which(terminals == x)
    d1<- terminals_matrix[nt_ind,(x_ind+1)]/(sum(terminals_matrix[nt_ind,2:(length(terminals)+1)]))
    w<- w*d1
  }
  if(y!=""){
    y_ind<- which(terminals == y)
    d2<- terminals_matrix[nt_ind,(y_ind+1)]/(sum(terminals_matrix[nt_ind,2:(length(terminals)+1)]))
    w<- w*d2
  }
  
  freq<- 0
  if(side=="left"){
    for(i in 1:length(e_rules)){
      if(e_rules[[i]][[1]]==nonterminal & e_rules[[i]][[2]]==x & e_rules[[i]][[3]]==y){
        freq<- freq+1
      }
    }
  }else if(side=="right"){
    for(i in 1:length(e_rules)){
      if(e_rules[[i]][[1]]==nonterminal & e_rules[[i]][[2]]==y & e_rules[[i]][[3]]==x){
        freq<- freq+1
      }
    }
  }
  ind_emission<- which(type_matrix[,1]==nonterminal)
  ind_epsilon<- which(epsilon_matrix[,1]== nonterminal)
  proba_epsilon_expected<- epsilon_matrix[ind_epsilon,2]/(epsilon_matrix[ind_epsilon,2] + epsilon_matrix[ind_epsilon,3])
  proba_emission_expected<- type_matrix[ind_emission,2]/(type_matrix[ind_emission,2]+ type_matrix[ind_emission,3])
  if(x!="" & y!= ""){
    w<- w + freq/(proba_emission_expected*(1-proba_epsilon_expected))
  }else{
    w<- w + freq/(proba_emission_expected*proba_epsilon_expected/2)
  }
  
  return(w)
}


unique_ordered <- function(x) {
  ux <- unique(x)
  tab<- tabulate(match(x,ux))
  index_ux<-order(tab,decreasing=TRUE)
  ux_ordered<- ux[index_ux]
  frequencies<- sort(tab,decreasing = TRUE)
  return(ux_ordered)
}

unique_frequencies<- function(x){
  ux <- unique(x)
  tab<- tabulate(match(x,ux))
  frequencies<- sort(tab,decreasing = TRUE)
  return(frequencies)
}


