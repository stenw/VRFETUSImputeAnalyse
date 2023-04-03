library(foreign)
library(mice)
library(dplyr)
library(forcats)
library(tidyr)

d <- read.spss('../data/VRFETUS_Dataset_23-03-23_anom.sav',
               to.data.frame = TRUE)
# NB: There are warnings that should be checked!!
# CP 2-4: checked. Only long string variables cannot be changed, and will continue to give warnings.


# CP 2-4: 'STOP' = STOP var, 1 = stop, 6 = continued participation.
d1 <-
d %>% mutate(heeftT1= !is.na(HADS_scoreffDepressie) &
               V_Round=='T=1 (after 3D VR or around 13 wks)',
             heeftT3=!is.na(HADS_scoreffDepressie) &
               V_Round=='T=3 (after STAS)') %>%
  filter(heeftT1 | heeftT3)

intersect(d1$VRFETUSnr[d1$heeftT1], d1$VRFETUSnr[d1$heeftT3])

data_to_impute <- d %>%
  filter(V_Round != "T=2") %>%
  mutate(V_Round=fct_recode(V_Round,
                            T0="T=0 (around inclusion)",
                            T1="T=1 (after 3D VR or around 13 wks)",
                            T3="T=3 (after STAS)",
                            T4="T=4 (around 32 wks)",
                            T5="T=5 (6 wks after EDD)"),
         Q_Outcome_recode=fct_recode(Q_Outcome_recode,
                                     "Alive"="Alive",
                                     "IUFD"="IUFD",
                                     "IUFD"="Deceased during delivery",
                                     "IUFD"="Immature delivery",
                                     "IUFD"="Died during neonatal period",
                                     "Miscarriage"="Miscarriage",
                                     "Miscarriage"="Termination of pregnancy",
                                     "Miscarriage"="Selective termination of fetus"),
         rtMCQ15_med_total=sqrt(MCQ15_med_total),
         rtMCQ_1elijn_EUR=sqrt(MCQ_1elijn_EUR),
         rtMCQ01a_EUR=sqrt(MCQ01a_EUR),
         rtMCQ_zorgthuis_EUR=sqrt(MCQ_zorgthuis_EUR),
         rtMCQ_spoedzorg_EUR=sqrt(MCQ_spoedzorg_EUR),
         rtMCQ_2elijnconsulten_EUR=sqrt(MCQ_2elijnconsulten_EUR),
         rtMCQ19a_SQ001_EUR=sqrt(MCQ19a_SQ001_EUR),
         rtMCQ28_EUR=sqrt(MCQ28_EUR),
         rtMCQ_Opnames_overig=sqrt(MCQ_Opnames_overig)) %>%
  mutate(Q_Outcome_recode=droplevels(Q_Outcome_recode)) %>%
  select(VRFETUSnr,
         V_Round,
         organizationid,
         Etn_west,  #1m
         Preg_Drugs,
         Preg_Depr_dicho,
         MCQA4_rec,  # 3, 4, 5
         Preg_ETScat_dicho,
         Q_Outcome_recode,
         Q_Del_mode,
         Q_del_GA_days,
         Q_Birthweight,
         SF36_pain, # 0,1,3,5
         SF36_healtran,
         SF36_phys_fun,
         SF36_lim_phys,
         SF36_lim_emot,
         SF36_energy,
         SF36_emot_wb,
         SF36_soc_fun,
         SF36_gen_heal,
         HADS_scoreffAngst, # 0,1,3,5
         HADS_scoreffDepressie,
         STAI_StateAnxiety1, # 0,1,3,5
         STAI_TraitAnxiety, # 0
         VAS_Therm_th1, # 0,1,3,5
         rtMCQ15_med_total, # 3,4,5
         rtMCQ_1elijn_EUR,
         rtMCQ01a_EUR,
         rtMCQ_zorgthuis_EUR,
         rtMCQ_spoedzorg_EUR,
         rtMCQ_2elijnconsulten_EUR,
         rtMCQ19a_SQ001_EUR,
         rtMCQ28_EUR,
         rtMCQ_Opnames_overig,
         FU_US_exams, # 0
         FU_consult,
         FU_Radiology,
         FU_Admitted,
         FU_part_mod_EUR,
         FU_Neo_KA_EUR,
         FU_Child,
         Allocation, # 0
         STOP,
         Age,
         Preg_nulli,
         Preg_Sig_dicho,
         Preg_FzPreCon,
         Preg_ETScat_dicho,
         Preg_Depr_dicho,
         US_invasive,
         Anom_rec,
         Marker_rec,
         Q_del_GA_before168,
         Q_del_GA_168_259) %>%
  pivot_wider(id_cols = c(VRFETUSnr, organizationid, Allocation, STOP, Preg_nulli, Age,
                          Preg_Sig_dicho,
                          Preg_FzPreCon,
                          Preg_ETScat_dicho,
                          Preg_Depr_dicho,
                          US_invasive,
                          Anom_rec,
                          Marker_rec,
                          Q_del_GA_before168,
                          Q_del_GA_168_259),
              names_from =V_Round,
              values_from=c(SF36_pain,
                            SF36_healtran,
                            SF36_phys_fun,
                            SF36_lim_phys,
                            SF36_lim_emot,
                            SF36_energy,
                            SF36_emot_wb,
                            SF36_soc_fun,
                            SF36_gen_heal,
                            HADS_scoreffAngst,
                            HADS_scoreffDepressie,
                            STAI_StateAnxiety1,
                            VAS_Therm_th1,
                            rtMCQ15_med_total, # 3,4,5
                            rtMCQ_1elijn_EUR,
                            rtMCQ01a_EUR,
                            rtMCQ_zorgthuis_EUR,
                            rtMCQ_spoedzorg_EUR,
                            rtMCQ_2elijnconsulten_EUR,
                            rtMCQ19a_SQ001_EUR,
                            rtMCQ28_EUR,
                            rtMCQ_Opnames_overig)) %>%
  select(!c(VRFETUSnr,
            SF36_pain_T4, SF36_healtran_T4, SF36_phys_fun_T4,
            SF36_lim_phys_T4, SF36_lim_emot_T4, SF36_energy_T4,
            SF36_emot_wb_T4, SF36_soc_fun_T4, SF36_gen_heal_T4,
            HADS_scoreffAngst_T4, HADS_scoreffDepressie_T4,
            STAI_StateAnxiety1_T4,
            VAS_Therm_th1_T4,
            rtMCQ15_med_total_T0, rtMCQ_1elijn_EUR_T0, rtMCQ01a_EUR_T0,
            rtMCQ_zorgthuis_EUR_T0, rtMCQ_spoedzorg_EUR_T0,
            rtMCQ_2elijnconsulten_EUR_T0, rtMCQ19a_SQ001_EUR_T0,
            rtMCQ_Opnames_overig_T0, rtMCQ28_EUR_T0,
            rtMCQ15_med_total_T1, rtMCQ_1elijn_EUR_T1,
            rtMCQ01a_EUR_T1, rtMCQ_zorgthuis_EUR_T1,
            rtMCQ_spoedzorg_EUR_T1, rtMCQ_2elijnconsulten_EUR_T1,
            rtMCQ19a_SQ001_EUR_T1, rtMCQ28_EUR_t1,
            rtMCQ_Opnames_overig_T1))


# rtMCQ_Opnames_overig_T4 is always zero so we also remove it
  # CP2-4: rtMCQ_Opnames_overig_T5 (note: T5) is not always zero, but is removed; adjusted this in the syntax.
# CP 2-4-23: These MCQ data may be used wide, because they have to be summed (T3+T4+T5) as a total costs variable for the pregnancy. i.e., not measured as repeated measures.
impimp <- mice(data_to_impute,m = 6, maxit = 6)
cnv <- convergence(imp)
imputed_d_wide <- complete(imp, action='long', include=TRUE) %>% 
  mutate(allcosts=rtMCQ15_med_total_T3^2+ rtMCQ_1elijn_EUR_T3^2, rtMCQ01a_EUR_T3^2+
         rtMCQ_zorgthuis_EUR_T3^2+ rtMCQ_spoedzorg_EUR_T3^2+
         rtMCQ_2elijnconsulten_EUR_T3^2+ rtMCQ19a_SQ001_EUR_T3^2+
         rtMCQ_Opnames_overig_T3^2 + rtMCQ28_EUR_T3^2,
           rtMCQ15_med_total_T4^2+ rtMCQ_1elijn_EUR_T4^2, rtMCQ01a_EUR_T4^2+
           rtMCQ_zorgthuis_EUR_T4^2+ rtMCQ_spoedzorg_EUR_T4^2+
         rtMCQ_2elijnconsulten_EUR_T4^2+ rtMCQ19a_SQ001_EUR_T4^2+
         rtMCQ28_EUR_T4^2 +
           rtMCQ15_med_total_T5^2+ rtMCQ_1elijn_EUR_T5^2, rtMCQ01a_EUR_T5^2+
           rtMCQ_zorgthuis_EUR_T5^2+ rtMCQ_spoedzorg_EUR_T5^2+
         rtMCQ_2elijnconsulten_EUR_T5^2+ rtMCQ19a_SQ001_EUR_T5^2 +
         rtMCQ28_EUR_T5^2+ rtMCQ_Opnames_overig_T5^2)

saveRDS(imputed_d_wide, file = 'imputed_d_wide.Rds')
mids_wide <- as.mids(imputed_d_wide)


# add missing values for items where question was not asked in a certain round
setNA <- function(data, vars){
  for(i in seq_along(vars)){
    data[[ vars[i] ]] <- NA
  }
  data
}

imputed_d_wide <- setNA(imputed_d_wide,
                        vars = c("SF36_pain_T4", 
                          "SF36_healtran_T4", "SF36_phys_fun_T4",
                          "SF36_lim_phys_T4", "SF36_lim_emot_T4", "SF36_energy_T4",
                          "SF36_emot_wb_T4", "SF36_soc_fun_T4", "SF36_gen_heal_T4",
                          "HADS_scoreffAngst_T4", "HADS_scoreffDepressie_T4",
                          "STAI_StateAnxiety1_T4",
                          "VAS_Therm_th1_T4",
                          "rtMCQ15_med_total_T0", "rtMCQ_1elijn_EUR_T0", "rtMCQ01a_EUR_T0",
                          "rtMCQ_zorgthuis_EUR_T0", "rtMCQ_spoedzorg_EUR_T0",
                          "rtMCQ_2elijnconsulten_EUR_T0", "rtMCQ19a_SQ001_EUR_T0",
                          "rtMCQ_Opnames_overig_T0", "rtMCQ28_EUR_T0"
                          "rtMCQ15_med_total_T1", "rtMCQ_1elijn_EUR_T1",
                          "rtMCQ01a_EUR_T1", "rtMCQ_zorgthuis_EUR_T1",
                          "rtMCQ_spoedzorg_EUR_T1", "rtMCQ_2elijnconsulten_EUR_T1",
                          "rtMCQ19a_SQ001_EUR_T1", "rtMCQ_Opnames_overig_T1",
                          "rtMCQ_Opnames_overig_T1", "rtMCQ28_EUR_T1"))

tvcolnames <- c("SF36_pain",
                "SF36_healtran",
                "SF36_phys_fun",
                "SF36_lim_phys",
                "SF36_lim_emot",
                "SF36_energy",
                "SF36_emot_wb",
                "SF36_soc_fun",
                "SF36_gen_heal",
                "HADS_scoreffAngst",
                "HADS_scoreffDepressie",
                "STAI_StateAnxiety1",
                "VAS_Therm_th1",
                "rtMCQ15_med_total",
                "rtMCQ_1elijn_EUR",
                "rtMCQ01a_EUR",
                "rtMCQ_zorgthuis_EUR",
                "rtMCQ_spoedzorg_EUR",
                "rtMCQ_2elijnconsulten_EUR",
                "rtMCQ19a_SQ001_EUR",
                "rtMCQ28_EUR"
                "rtMCQ_Opnames_overig")

tvcolnames <- paste0(rep(tvcolnames, each=5),
                     c('_T0', '_T1', '_T3', '_T4', '_T5'))

#CP 2-4: rtMCQ_med_total en rt_1elijn_EUR staan niet in de onderstaande syntax, is dat opzettelijk?

imputed_d_long <- pivot_longer(imputed_d_wide,
             cols = tvcolnames,
             names_to = c(".value", "V_Round"),
             names_pattern = "(.*)_T(.)") %>%
  mutate(MCQ15_med_total=rtMCQ15_med_total^2,
         MCQ_1elijn_EUR=rtMCQ_1elijn_EUR^2,
         MCQ01a_EUR=rtMCQ01a_EUR^2,
         MCQ_zorgthuis_EUR=rtMCQ_zorgthuis_EUR^2,
         MCQ_spoedzorg_EUR=rtMCQ_spoedzorg_EUR^2,
         MCQ_2elijnconsulten_EUR=rtMCQ_2elijnconsulten_EUR^2,
         MCQ19a_SQ001_EUR=rtMCQ19a_SQ001_EUR^2,
         MCQ28_EUR=rtMCQ28_EUR^2,
         MCQ_Opnames_overig=rtMCQ_Opnames_overig^2) %>% 
  mutate(Arm=Allocation,
         BL=ifelse(V_Round=='0',1,0),
         Control1=ifelse(V_Round=='1' & Allocation=='Control' ,1,0),
         Control3=ifelse(V_Round=='3' & Allocation=='Control' ,1,0),
         Control4=ifelse(V_Round=='4' & Allocation=='Control' ,1,0),
         Control5=ifelse(V_Round=='5' & Allocation=='Control' ,1,0),
         Intervention1=ifelse(V_Round=='1' & Allocation=='Intervention' ,1,0),
         Intervention3=ifelse(V_Round=='3' & Allocation=='Intervention' ,1,0),
         Intervention4=ifelse(V_Round=='4' & Allocation=='Intervention' ,1,0),
         Intervention5=ifelse(V_Round=='5' & Allocation=='Intervention' ,1,0),
         Round1=ifelse(V_Round=='1' ,1,0),
         Round3=ifelse(V_Round=='3' ,1,0),
         Round4=ifelse(V_Round=='4' ,1,0),
         Round5=ifelse(V_Round=='5' ,1,0),
         obstime0135=case_when(V_Round==0 ~ 1L,
                               V_Round==1 ~ 2L,
                               V_Round==3 ~ 3L,
                               V_Round==5 ~ 4L,
                               TRUE ~ NA_integer_)
  )
imputed_d_long$Arm[imputed_d_long$V_Round=='0']='Control'

imputed_d_long <- imputed_d_long %>% 
  mutate(grp=factor(paste0(as.character(Arm), V_Round))) %>% 
  mutate(grpSF36=grp) %>%
  mutate(.id2=.id,
         .id=.id+as.numeric(V_Round)*10000) %>% 
  as.data.frame

#imputed_d_long$grpSF36[!(imputed_d_long$grp %in% c('Control0', 'Control1', 'Control3', 'Control5',
#                                 'Intervention1', 'Intervention3', 'Intervention5'))] <- NA
#imputed_d_long$grpSF36 <- droplevels(imputed_d_long$grpSF36)
                       

rownames(imputed_d_long) <- with(imputed_d_long, paste(.id2, V_Round,.imp, sep = '_'))

saveRDS(imputed_d_long, file = 'imputed_d_long.Rds')
mids_long <- as.mids(imputed_d_long)


# transform wide to long

#CP 2-4: Sten, zijn dit de correcte formuleringen voor berekeningen in RStudio?
#Calculate new Z-scores SF36:
SF36_pf_z0 <- (SF36_phys_fun - 80.4) / 24.2
SF36_rp_z0 <- (SF36_lim_phys - 73.8) / 38.5
SF36_bp_z0 <- (SF36_pain - 71.9) / 23.8
SF36_gh_z0 <- (SF36_gen_heal - 69.9) / 20.6
SF36_vt_z0 <- (SF36_energy   - 64.3) / 19.7
SF36_sf_z0 <- (SF36_soc_fun  - 82.0) / 23.5
SF36_re_z0 <- (SF36_lim_emot - 78.5) / 35.7
SF36_mh_z0 <- (SF36_emot_wb  - 73.7) / 18.2 

SF36_pcs <- SF36_pf_z0* .42402 + SF36_rp_z0* .35119 + SF36_bp_z0* .31754 + 
SF36_gh_z0* .24954 + SF36_vt_z0*.02877 + SF36_sf_z0*-.00753 + 
SF36_re_z0*-.19206 + SF36_mh_z0*-.22069 
SF36_mcs <- SF36_pf_z0*-.22999 + SF36_rp_z0*-.12329 + SF36_bp_z0*-.09731 + 
SF36_gh_z0*-.01571 + SF36_vt_z0*.23534 + SF36_sf_z0* .26876 + 
F36_re_z0* .43407 + SF36_mh_z0* .48581 

SF36_t_pcs <- 50 + SF36_pcs * 10 
SF36_t_mcs <- 50 + SF36_mcs * 10 

#Calculate QALY based on: https://doi.org/10.1111/j.1524-4733.2008.00352.x
#Scores are expressed 0 - 1.0.
SF36_EQ5D <- 0.03256 + 0.0037* SF36_phys_fun + 0.0011* SF36_soc_fun 
  - 0.00024 * SF36_lim_phys + 0.00024 * SF36_lim_emot 
  + 0.00256 * SF36_emot_wb - 0.00063 * SF36_energy 
  +0.00286 * SF36_pain + 0.00052 * SF36_gen_heal

library(lme4)
library(geepack)
library(broom.mixed)
# Health related quality of life effects (terms of QALY’s)  

gee_pain <- with(mids_long, geeglm(SF36_pain ~ Round1+Round3 + Round5 +
                                     Intervention1+Intervention3 + Intervention5+organizationid,
                                                family = "gaussian", id = .id2,
                                   corstr = "exchangeable"))

gee_pain0 <- with(mids_long, geeglm(SF36_pain  ~ Round1+Round3 + Round5+organizationid,
                                                   family = "gaussian", id = .id2,
                                                   corstr = "exchangeable"))



pooled <- pool(gee_pain)
summary(pooled, conf.int=TRUE)
D1(gee_pain, gee_pain0)


lmer_pain <- with(mids_long, lmer(SF36_pain ~ Round1+Round3 + Round5 +organizationid+
                                     Intervention1+Intervention3 + 
                                   Intervention5+(1|.id2), REML=FALSE))

lmer_pain0 <- with(mids_long, lmer(SF36_pain ~ Round1+Round3 + Round5 +
                                    (1|.id2)+organizationid, REML=FALSE))

pooled <- pool(lmer_pain)
summary(pooled, conf.int=TRUE)

D1(lmer_pain, lmer_pain0)



gls_pain <- with(mids_long, gls(SF36_pain ~ Round1+Round3 + Round5 +
                                  Intervention1+Intervention3 + Intervention5+organizationid,
                                na.action=na.omit,
                                correlation = corSymm(form = ~ obstime0135 | .id2)))
gls_pain0 <- with(mids_long, gls(SF36_pain ~ Round1+Round3 + Round5+organizationid, na.action=na.omit,
                                 correlation = corSymm(form = ~ obstime0135 | .id2)))

pooled <- pool(gls_pain)
summary(pooled, conf.int=TRUE)

#CP 2-4: Stratificatiefactor 'organizationid' toegevoegd in de syntax. 
#CP 2-4: Extra analyses voor samenvattende scores en EQ5D toegevoegd.
D1(gls_pain, gls_pain0)
gls0135 <- function(Y){
  frm0 <- as.formula(paste(Y, '~ Round1+Round3 + Round5+organizationid'))
  frm1 <- as.formula(paste(Y, '~ Round1+Round3 + Round5+Intervention1+Intervention3 + Intervention5+organizationid'))
  gls_pain <- with(mids_long, gls(frm1,
                                  na.action=na.omit,
                                  correlation = corSymm(form = ~ obstime0135 | .id2)))
  gls_pain0 <- with(mids_long, gls(frm0,
                                   na.action=na.omit,
                                   correlation = corSymm(form = ~ obstime0135 | .id2)))
  
  pooled <- pool(gls_pain)
  print(summary(pooled, conf.int=TRUE))
  print(D1(gls_pain, gls_pain0))
}
gls0135('SF36_pain')
gls0135("SF36_healtran")
gls0135("SF36_phys_fun")
gls0135("SF36_lim_phys")
gls0135("SF36_lim_emot")
gls0135("SF36_energy")
gls0135("SF36_emot_wb")
gls0135("SF36_soc_fun")
gls0135("SF36_gen_heal")
gls0135("SF36_t_pcs")
gls0135("SF36_t_mcs")
gls0135("SF36_EQ5D")

gls0135("HADS_scoreffAngst")
gls0135("HADS_scoreffDepressie_T4")

gls0135("STAI_StateAnxiety1")
gls0135("VAS_Therm_th1")

# NB Assumes equal variance!!
costs <- with(mids_wide, lm(allcosts~Allocation))
summary(pool(costs),  conf.int=TRUE)

#CP 2-4-23: Sten wil je aub helpen: hoe kan ik de volgende variabelen bij elkaar optellen, sum / c()? ik snap het niet. Let op: er kunnen lege/missing waarden bij zitten.
#Som van kosten:
MCQ_totaal <- (MCQ15_med_total, MCQ01a_EUR,MCQ_1elijn_EUR,MCQ_zorgthuis_EUR,MCQ_spoedzorg_EUR, MCQ19a_SQ001_EUR,MCQ_2elijnconsulten_EUR, MCQ28_EUR,MCQ_Opnames_overig,MCQ35_SQ001_EUR)
MCQ_tot_excl_mantel_vlkgyn_opname <- (MCQ15_med_total, MCQ_1elijn_EUR,MCQ_spoedzorg_EUR,MCQ_2elijnconsulten_EUR)

#CP 2-4: tenslotte optelsom van de meest betrouwbare variabelen uit MCQ samen met follow-up uit dossieronderzoek:
Costs_total_incl_child <- (FU_US_exams,FU_consult,FU_Radiology,FU_Admitted,FU_Child,FU_part_mod_EUR,MCQ15_med_total, MCQ_1elijn_EUR, MCQ_spoedzorg_EUR,MCQ_2elijnconsulten_EUR)
Costs_total_mother <- (FU_US_exams,FU_consult,FU_Radiology,FU_Admitted,FU_part_mod_EUR,MCQ15_med_total, MCQ_1elijn_EUR, MCQ_spoedzorg_EUR,MCQ_2elijnconsulten_EUR)

#CP 2-4-2023 to do:
    #1. MICE imputation naar 40-50 imputatierondes. 
    #2. Ongepaarde T-test kosten: 
      #MCQ01a_EUR; MCQ19a_SQ001_EUR;
      #MCQ_totaal; MCQ_tot_excl_mantel_vlkgyn_opname;
      #FU_consult; FU_US_exams;
      #FU_Total; FU_Total_mother;
      #Costs_total_incl_child; Costs_total_mother
    #3. Sensitiviteitsanalyses.