

eiu_joint <- normfuncpos(eiu_joint, 70, 0, "M_EIU_Score")
eiu_joint <- normfuncpos(eiu_joint, 2, -2, "M_EIU_12m_change")
eiu_joint <- normfuncpos(eiu_joint, 70, 0, "M_EIU_Score_12m")

test <- eiu_joint %>%
  rowwise() %>%
  mutate(alert_level = geometricmean(c(M_EIU_12m_change_norm, M_EIU_Score_12m_norm), na.rm = T))

test <- test %>% dplyr::select(Country, M_EIU_Score_12m_norm, M_EIU_12m_change_norm, alert_level) %>%
  rename(underlying_vul = M_EIU_Score_12m_norm, emerging_threats = M_EIU_12m_change_norm)
write.csv(test, "macro_test.csv")

