dp_random2<- function(nonterminal,minimum,maximum){
  draws<- vector()
  #stop<- FALSE
  permutation1<- 0
  p_rules_star<- list()
  p_rules_star_indices<- vector()
  p_rules_frequencies_star<- vector()
  rule_index<- 0
  e_rules_star<- list()
  e_rules_star_indices<- vector()
  e_rules_frequencies_star<- vector()
  for(i in 1:length(p_rules_ordered)){
    if(p_rules_ordered[[i]][[1]]==nonterminal){
      p_rules_star[[length(p_rules_star)+1]]<- p_rules_ordered[[i]]
      p_rules_star_indices<- c(p_rules_star_indices,i)
      p_rules_frequencies_star[[length(p_rules_frequencies_star)+1]]<- p_rules_frequencies[[i]]
    }
  }
  
  for(i in 1:length(e_rules_ordered)){
    if(e_rules_ordered[[i]][1]==nonterminal){
      e_rules_star[[length(e_rules_star)+1]]<- e_rules_ordered[[i]]
      e_rules_star_indices<- c(e_rules_star_indices,i)
      e_rules_frequencies_star[[length(e_rules_frequencies_star)+1]]<- e_rules_frequencies[[i]]
    }
  }
  
  
  old_rule_total<- sum(p_rules_frequencies_star) + sum(e_rules_frequencies_star)
  
  draw1<- sample(0:1,1,prob=c(alpha2,old_rule_total))
  draws<- c(draws,draw1)
  if(draw1==0){#new rule
    index1<- which(type_matrix[,1]==nonterminal)
    proba_emission<- rbeta(1,type_matrix[index1,2],type_matrix[index1,3])
    proba_epsilon<- rbeta(1,epsilon_matrix[index1,2],epsilon_matrix[index1,3])
    
    draw2<- sample(0:1,1,prob=c((1-proba_emission),proba_emission))
    if(tree_matrix[row,5]>2){draw2<- 0}
    draws<- c(draws,draw2)
    if(draw2==0){#new production
      if(maximum == 1){stop<- TRUE}
      rule<- base_production_random(nonterminal)
      permutation1<- rule[[6]]
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
    draw2<- sample(0:1,1,prob=c(sum(p_rules_frequencies_star),sum(e_rules_frequencies_star)))
    draws<- c(draws,draw2)
    if(draw2 == 0){#old production
      if(maximum == 1){stop<- TRUE}
      samp<- sample(1:length(p_rules_frequencies_star),1, prob = p_rules_frequencies_star)
      rule<- p_rules_star[[samp]]
      rule_index<- p_rules_star_indices[samp]
    }else{#old emission
      samp<- sample(1:length(e_rules_frequencies_star),1, prob = e_rules_frequencies_star)
      rule<- e_rules_star[[samp]]
      rule_index<- e_rules_star_indices[samp]
      if(rule[2]=="" | rule[[3]]==""){
        if (minimum >1 ){stop<- TRUE}
      }else{
        if(minimum > 2){stop<- TRUE}
      }
    }
  }
  return(list(rule,stop,draws,permutation1,rule_index))
}
