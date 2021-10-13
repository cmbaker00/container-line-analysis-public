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
  
  p_intercept ~ normal(0, 2);
  // beta_doc ~ uniform(-2, 2);
  beta_doc ~ normal(0, .5);
  country_effect ~ normal(0, .5);
  sigma_entry ~ uniform(0,0.5);
  entry_effect ~ normal(0, sigma_entry);
  
  for (i in 1:num_rows){
    pvec[i] = p_intercept[Item_class[i]] + country_effect[country[i]] + beta_doc*Document[i] + entry_effect[Entry[i]];
  }
  
  Record_intercept ~ bernoulli_logit(pvec);
}


