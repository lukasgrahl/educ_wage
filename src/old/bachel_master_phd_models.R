## BA
## LM
lm1 <- lm(log_wage ~
            is_bachel,
          # age +
          # male +
          # famsize +
          # sing_par +
          # immig_first +
          # immig_secn +
          # is_hispan +
          # is_negro +
          # is_asian +
          # is_nativamec +
          # firmsize2 +
          # firmsize5 +
          # firmsize7 +
          # firmsize8 +
          # firmsize9 +
          # pension_inc +
          # pension_exl +
          # union_mem +
          # union_mem_incl +
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm2 <- lm(log_wage ~
            is_bachel +
            age +
            male +
            # famsize +
            # sing_par +
            immig_first +
            immig_secn,
          # is_hispan +
          # is_negro +
          # is_asian +
          # is_nativamec +
          # firmsize2 +
          # firmsize5 +
          # firmsize7 +
          # firmsize8 +
          # firmsize9 +
          # pension_inc +
          # pension_exl +
          # union_mem +
          # union_mem_incl +
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm3 <- lm(log_wage ~
            is_bachel +
            age +
            male +
            famsize +
            sing_par +
            immig_first +
            # immig_secn +
            is_hispan +
            is_negro +
            is_asian +
            is_nativamec, 
          # firmsize2 +
          # firmsize5 +
          # firmsize7 +
          # firmsize8 +
          # firmsize9 +
          # pension_inc +
          # pension_exl +
          # union_mem +
          # union_mem_incl +
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)
summary(lm3)

lm4 <- lm(log_wage ~
            is_bachel +
            age +
            male +
            famsize +
            # sing_par +
            immig_first +
            # immig_secn +
            is_hispan +
            is_negro +
            # is_asian +
            is_nativamec +
            firmsize2 +
            firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            pension_exl,
          # union_mem +
          # union_mem_incl +
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm5 <- lm(log_wage ~
            is_bachel +
            age +
            male +
            famsize +
            # sing_par +
            immig_first +
            # immig_secn +
            is_hispan +
            is_negro +
            # is_asian +
            is_nativamec +
            firmsize2 +
            firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            # pension_exl +
            union_mem +
            union_mem_incl +
            is_poorheal +
            is_paidhour +
            is_disabled,
          data = df_lm
)

lm6 <- lm(log_wage ~
            is_bachel +
            age +
            male +
            # famsize +
            # sing_par +
            # immig_first +
            # immig_secn +
            is_hispan +
            is_negro +
            # is_asian +
            # is_nativamec +
            firmsize2 +
            firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            # pension_exl +
            union_mem +
            # union_mem_incl +
            # is_poorheal +
            is_paidhour +
            is_disabled,
          data = df_lm
)

# summary(lm6)
# plot(lm1)


## TABLE
models <- list(
  "(1)" = lm1,
  "(2)" = lm2,
  "(3)" = lm3,
  "(4)" = lm4,
  "(5)" = lm5,
  "(6)" = lm6
)

# MASTER

## LM
lm1 <- lm(log_wage ~
            is_master,
          # age +
          # male +
          # famsize +
          # sing_par +
          # immig_first +
          # immig_secn,
          # is_hispan +
          # is_negro +
          # is_asian +
          # is_nativamec +
          # firmsize2 +
          # firmsize5 +
          # firmsize7 +
          # firmsize8 +
          # firmsize9 +
          # pension_inc +
          # pension_exl +
          # union_mem +
          # union_mem_incl +
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm2 <- lm(log_wage ~
            is_master +
            age +
            male +
            famsize +
            sing_par +
            immig_first +
            immig_secn,
          # is_hispan +
          # is_negro +
          # is_asian +
          # is_nativamec +
          # firmsize2 +
          # firmsize5 +
          # firmsize7 +
          # firmsize8 +
          # firmsize9 +
          # pension_inc +
          # pension_exl +
          # union_mem +
          # union_mem_incl +
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm3 <- lm(log_wage ~
            is_master +
            age +
            male +
            famsize +
            # sing_par +
            immig_first +
            # immig_secn +
            is_hispan +
            is_negro +
            is_asian +
            is_nativamec,
          # firmsize2 +
          # firmsize5 +
          # firmsize7 +
          # firmsize8 +
          # firmsize9 +
          # pension_inc +
          # pension_exl +
          # union_mem +
          # union_mem_incl +
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm4 <- lm(log_wage ~
            is_master +
            age +
            male +
            famsize +
            # sing_par +
            # immig_first +
            # immig_secn +
            is_hispan +
            # is_negro +
            # is_asian +
            is_nativamec +
            firmsize2 +
            firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            pension_exl +
            union_mem +
            union_mem_incl, 
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm5 <- lm(log_wage ~
            is_master +
            age +
            male +
            famsize +
            # sing_par +
            # immig_first +
            # immig_secn +
            is_hispan +
            # is_negro +
            # is_asian +
            # is_nativamec +
            firmsize2 +
            firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            pension_exl +
            # union_mem +
            # union_mem_incl +
            is_poorheal +
            is_paidhour +
            is_disabled,
          data = df_lm
)

lm6 <- lm(log_wage ~
            is_master +
            age +
            male +
            # famsize +
            # sing_par +
            # immig_first +
            # immig_secn +
            # is_hispan +
            # is_negro +
            # is_asian +
            # is_nativamec +
            # firmsize2 +
            # firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            # pension_inc +
            # pension_exl +
            # union_mem +
            # union_mem_incl +
            # is_poorheal +
            is_paidhour,
          # is_disabled,
          data = df_lm
)

# PHD

lm1 <- lm(log_wage ~
            is_phd +
            age +
            male +
            famsize +
            # sing_par +
            # immig_first +
            # immig_secn,
            is_hispan +
            is_negro +
            is_asian +
            is_nativamec +
            firmsize2 +
            firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            pension_exl +
            union_mem +
            union_mem_incl +
            is_poorheal +
            is_paidhour +
            is_disabled,
          data = df_lm
)

lm2 <- lm(log_wage ~
            is_phd +
            age +
            male +
            famsize +
            # sing_par +
            # immig_first +
            # immig_secn,
            # is_hispan +
            # is_negro +
            # is_asian +
            # is_nativamec +
            # firmsize2 +
            # firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            # pension_inc +
            # pension_exl +
            # union_mem +
            # union_mem_incl +
            # is_poorheal +
            is_paidhour,
          # is_disabled
          data = df_lm
)