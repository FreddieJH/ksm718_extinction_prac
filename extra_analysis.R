
out <- tibble()

for(spp in unique(dat_no_na$species_name)){
  
  # 980 rows
  dat_one_spp <- 
    dat_no_na |> 
    filter(species_name == spp) |> 
    mutate(log_count = log(count + 1))
  
  
  dat_one_spp_mean <- 
    dat_one_spp |> 
    group_by(year) |> 
    summarise(mean_log_count = mean(log_count))
  
  lm1 <- lm(mean_log_count ~ year, data = dat_one_spp_mean)
  
  lmer1 <- lmerTest::lmer(log_count ~ year + (1|site_code), data = dat_one_spp) 
  
  # Extracting the slope and intercept from the lmer
  intercept_value <- coef(summary(lmer1))[1, 1]
  slope_value <- coef(summary(lmer1))[2, 1]
  
    
  out <- 
    out |> 
    bind_rows(tibble(species_name = spp, 
              int = intercept_value, 
              slope = slope_value, 
              int_lm = lm1$coefficients[1], 
              slope_lm = lm1$coefficients[2]))
  
}

out |> 
  ggplot(aes(x = slope)) + 
  geom_density()


out$slope |> mean()


out |> 
  ggplot(aes(x = slope, 
             y = slope_lm)) + 
  geom_point() +
  geom_abline(slope = 1, lty = 2)
