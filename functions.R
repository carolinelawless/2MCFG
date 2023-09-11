library(LaplacesDemon)
##Draw from the DP of nonterminals ##returns a vector of length n of nonterminals
draw_nt<- function(n){
  w<- 1
  nt<- vector(length=n)
  ntv<-nonterminals_vec_long
  for(i in 1:n){
    if(length(ntv)==0){
      nt[i]<- 2
    }else{
      draw<- sample(c("new","old"),1,prob=c(alpha1,length(ntv)))
      if(draw=="old"){
        tab<- table(ntv)
        if(length(tab)==1){
          s<- 2
        }else{
          s<- sample(2:(length(tab)+1), 1, replace= TRUE, prob = (tab + C_nonterminals))
        }
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
  
  permutation_density<- permutation_weights[permutation]/sum(permutation_weights)
  r<- list(nonterminal, nonterminals, string1, string2,permutation_density,permutation)
  return(r)
}

kernel_parameters_function<- function(nonterminal,minimum,maximum){
  index1<- which(type_matrix[,1]==nonterminal)
  proba_emission<- rbeta(1,type_matrix[index1,2],type_matrix[index1,3])
  proba_epsilon<- rbeta(1,epsilon_matrix[index1,2],epsilon_matrix[index1,3])
  q<- 0
  q = length(which(nonterminals_vec_short == nonterminal)) #number of p_rules
  if(length(e_rules)>0){
    for(i in 1:length(e_rules)){
      e_rule<- e_rules[[i]]
      if(e_rule[1]== nonterminal){q<- q+1} #+ number of e_rules
    }
  }
  
  if(minimum == 1 & maximum > 2){
    proba_emission_star<- proba_emission
    alpha_star<- alpha2
  }
  if(minimum == 1 & maximum == 2){
    proba_emission_star<- 1
    alpha_star<- alpha2*proba_emission
  }
  if(minimum == 1 & maximum == 1){
    proba_emission_star<- 1
    alpha_star<- alpha2*proba_emission*proba_epsilon
  }
  if(minimum == 2 & maximum ==2){
    proba_emission_star<- 1
    alpha_star<- alpha2*proba_emission*(1-proba_epsilon)
  }
  if(minimum == 2 & maximum > 2){
    proba_emission_star<- (1-proba_epsilon)*proba_emission/(1-proba_emission+(1-proba_epsilon)*proba_emission)
    alpha_star<- alpha2*(1-proba_emission+(1-proba_epsilon)*proba_emission)
  }
  if(minimum > 2){
    proba_emission_star<- 0
    alpha_star<- alpha2*(1-proba_emission)
  }
  parameter_list<- list(proba_emission,proba_epsilon,proba_emission_star,q,alpha_star)
  return(parameter_list)
}

dp_random<- function(nonterminal,minimum,maximum){
  permutation<- 0
  weight<- 1
  ww<- 1
  kernel_params<- kernel_parameters_function(nonterminal,minimum,maximum)
  proba_emission<- kernel_params[[1]]
  proba_epsilon<- kernel_params[[2]]
  proba_emission_star<- kernel_params[[3]]
  q<- kernel_params[[4]]
  alpha_star<- kernel_params[[5]]
  
  
  draw1<- sample(0:1,1,prob=c((1-proba_emission_star),proba_emission_star))
  if(draw1==0){#new production rule
    rule <- base_production_random(nonterminal)
    rule_short<- c(rule[[1]],rule[[2]],rule[[6]])
    type<- 0
    if(length(p_rules_short)==0){
      freq<- 0
    }else{
      freq<- sum(sapply(p_rules_short, identical, rule_short))
    }
    permutation_density<- rule[[5]]
    weight<- alpha_star + freq/((1-proba_emission)*permutation_density)
  }else if(draw1==1){#new (partial) emission rule
    weight<- alpha_star
    if(minimum==1 & maximum >1){
      draw2<- sample(0:2,1,prob=c(1-proba_epsilon,proba_epsilon/2,proba_epsilon/2))
      if(draw2==0){
        x<- sentence[tt] #### the index here is tt?
        type<- 2 #(y<- "symbol")
        #ww<- alpha2*proba_emission*(1-proba_epsilon)
        ww<- proba_emission*(1-proba_epsilon)
      }else if(draw2==1){
        x<- sentence[tt]
        type<- 3
        #ww<- alpha2*proba_emission*proba_epsilon/2
        ww<- proba_emission*proba_epsilon/2
      }else if(draw2==2){
        x<- ""
        type<- 2 #(y<- "symbol")
        #ww<- alpha2*proba_emission*proba_epsilon/2
        ww<- proba_emission*proba_epsilon/2
      }
    }else if(minimum==2){
      x<- sentence[tt]
      type<- 2 #(y<- "symbol")
    }else if(maximum==1){
      draw2<- sample(0:1,1)
      if(draw2 == 0){
        x<- sentence[tt]
        y<- ""
        type<- 3
      }else if(draw2 == 1){
        x<- ""
        type<- 2 #(y<- "symbol")
      }
    }
    rule<- x
  }
  
  weight<- weight/(alpha2 + q)
  output<- list(rule,type,weight,ww)
  return(output)
}

evaluate<- function(string1,x,numb){
  w<- which(string1==numb)
  s<- append(string1,x,after=w)
  s<- s[-w]
  return(s)
}

update<- function(min_or_max){
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
}

weight_e2<- function(nonterminal){
  ww<- tree_matrix[row,3]
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
  freq1<- 0
  freq2<- 0
  if(length(e_rules)>0){
    if(side=="left"){
      for(i in 1:length(e_rules)){
        if(e_rules[[i]][[1]]==nonterminal & e_rules[[i]][[2]]==x & e_rules[[i]][[3]]==y){
          freq1<- freq1+1
        }
      }
    }else if(side=="right"){
      for(i in 1:length(e_rules)){
        if(e_rules[[i]][[1]]==nonterminal & e_rules[[i]][[2]]==y & e_rules[[i]][[3]]==x){
          freq1<- freq1+1
        }
      }
    }
    for(i in 1:length(e_rules)){
      if(e_rules[[i]][[1]]==nonterminal){
        freq2<- freq2 + 1
      }
    }
    for(i in 1:length(p_rules)){
      if(p_rules[[i]][[1]]==nonterminal){
        freq2<- freq2 + p_rules[[i]][[6]]
      }
    }
  }
  #w <- w + freq1/ww/(alpha2 + freq2) ########
  w<- w + freq1/ww
  
  return(w)
}





unique_ordered <- function(x) {
  ux <- unique(x)
  tab<- tabulate(match(x,ux))
  index_ux<-order(tab,decreasing=TRUE)
  ux_ordered<- ux[index_ux]
  return(ux_ordered)
}

unique_frequencies<- function(x){
  ux <- unique(x)
  tab<- tabulate(match(x,ux))
  frequencies<- sort(tab,decreasing = TRUE)
  return(frequencies)
}
