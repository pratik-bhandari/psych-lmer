
# Sample size estimation for an effect size of interest in GLMM -----------
# Pratik Bhandari
# 2022-02-04

dat <- expand_grid(
  participants = paste(letters, 1:26, sep = '_'),
  noise = c('mild', 'severe'),
  cloze = c('high', 'low'),
  item = paste('item', 1:10, sep = '_'))
dat$acc <- sample(c(0,1), nrow(dat), replace = T)

# Assign class to the variables of interest:
dat$noise <- as.factor(dat$noise)
dat$cloze <- as.factor(dat$cloze)
dat$acc <- as.integer(dat$acc)

contrasts(dat$cloze) <- contr.treatment(2) # High cloze is the baseline, i.e., at the intercept term
contrasts(dat$noise) <- contr.treatment(2) # Mild noise is the baseline

m0 <- glmer(acc ~ 1 + (cloze + noise + cloze:noise) +
              (1 | participants) + (1 | item), 
            data=dat, family = "binomial", 
            control = glmerControl(calc.derivs = FALSE, optimizer="bobyqa", optCtrl = list(maxfun=1e6)),
            nAGQ = 0, na.action = na.exclude)

summary(m0)

fixef(m0)['noise2'] #fixef(m0)[3]
fixef(m0)['noise2'] <- 0.05 #changed from 0.1847905 to 0.55

N_tar_grid    <- seq(32, 160, by = 32) # five different sample sizes

fit_ext_simr0  <- simr::extend(m0, #increases sample size for 'm1'
                               along = "participants",
                               n = max(N_tar_grid)
)

B_boot <- 100 # number of boot repetitions within one experiment, one setup; at least ONE THOUSAND

noise_power0 <- simr::powerSim(fit_ext_simr0,
                               nsim = B_boot,
                               progress = TRUE,
                               test = fixed('noise2', 'z'))

noise_power0

noise_powercurve0 <- simr::powerCurve(fit_ext_simr0,
                                      along = 'participants',
                                      nsim = B_boot,
                                      breaks = N_tar_grid, #seq(32, 160, by = 32)
                                      progress = TRUE, 
                                      test = fixed('noise2', 'z'))

noise_powercurve0

plot(noise_powercurve0) #Doesn't work. :/ Increase the number of simulations.


