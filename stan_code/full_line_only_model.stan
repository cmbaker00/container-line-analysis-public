data {
  int num_rows;
  
  int Num_item_classes;
  int Item_class[num_rows];
  
  int Document[num_rows];
  
  int num_unique_Entry;
  int Entry[num_rows];
  
  int num_countries;
  int country[num_rows];
  
  int Record_intercept[num_rows];
}


parameters {
  vector[Num_item_classes] p_intercept; // item probability
  vector[num_countries] country_effect;
  real beta_doc; // documentation probability
  real sigma_entry; // random effect standard deviation
  vector[num_unique_Entry] entry_effect; // random effect of entry
}


model {
  vector[num_rows] pvec;
  int country_effect_flag;
  int entry_effect_flag;
  
  p_intercept ~ normal(-1, 2);
  beta_doc ~ normal(0, .5);
  country_effect ~ normal(0, .5);
  sigma_entry ~ uniform(0,0.5);
  entry_effect ~ normal(0, sigma_entry);

  country_effect_flag = 1;
  if (num_countries == 1){country_effect_flag = 0;};
  entry_effect_flag = 1;
  if (num_unique_Entry == 1){entry_effect_flag = 0;};
  
  for (i in 1:num_rows){
    pvec[i] = p_intercept[Item_class[i]] + country_effect_flag*country_effect[country[i]] + beta_doc*Document[i] + entry_effect_flag*entry_effect[Entry[i]];
  }
  
  Record_intercept ~ bernoulli_logit(pvec);
}


