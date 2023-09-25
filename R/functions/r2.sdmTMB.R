# library(sdmTMB)
# library(tidyr)
# library(lme4)
# library(assertthat)
# 
# set.seed(1)
# 
# predictor_dat <- data.frame(
#   X = runif(300), Y = runif(300),
#   b1 = rnorm(300), b2 = rnorm(300),
#   year = rep(1:6, each = 50),
#   f_year = as.factor(rep(1:6, each = 50)) # random intercept
# )
# 
# mesh <- make_mesh(predictor_dat, xy_cols = c("X", "Y"), cutoff = 0.1)
# 
# sim_dat <- sdmTMB_simulate(
#   formula = ~ 1 + b1 + b2 + (1|f_year),
#   data = predictor_dat,
#   time = "year",
#   mesh = mesh,
#   family = gaussian(),
#   range = 0.5,
#   sigma_E = 0.2,
#   sigma_O = 0.7,
#   phi = 0.9,
#   seed = 123,
#   B = c(0.2, -0.4, 0.3)
# )
# sim_dat$f_year <- as.factor(sim_dat$year) # ml+fm: for testing random effects
# 
# sim_dat_binom <- sdmTMB_simulate(
#   formula = ~ 1 + b1 + b2 + (1|f_year),
#   data = predictor_dat,
#   time = "year",
#   mesh = mesh,
#   family = binomial(),
#   range = 0.5,
#   sigma_E = 0.2,
#   sigma_O = 0.7,
#   phi = 0.9,
#   seed = 123,
#   B = c(0.2, -0.4, 0.3)
# )
# sim_dat_binom$f_year <- as.factor(sim_dat_binom$year) # ml+fm: for testing random effects
# 
# sim_dat_tweedie <- sdmTMB_simulate(
#   formula = ~ 1 + b1 + b2 + (1|f_year),
#   data = predictor_dat,
#   time = "year",
#   mesh = mesh,
#   family = tweedie(),
#   range = 0.5,
#   sigma_E = 0.2,
#   sigma_O = 0.7,
#   phi = 0.9,
#   seed = 123,
#   B = c(0.2, -0.4, 0.3)
# )
# sim_dat_tweedie$f_year <- as.factor(sim_dat_tweedie$year) # ml+fm: for testing random effects
# 
# sim_dat_pois <- sdmTMB_simulate(
#   formula = ~ 1 + b1 + b2 + (1|f_year),
#   data = predictor_dat,
#   time = "year",
#   mesh = mesh,
#   family = poisson(),
#   range = 0.5,
#   sigma_E = 0.2,
#   sigma_O = 0.7,
#   phi = 0.9,
#   seed = 123,
#   B = c(0.2, -0.4, 0.3)
# )
# sim_dat_pois$f_year <- as.factor(sim_dat_pois$year) # ml+fm: for testing random effects
# 
# # Fit some example models so that we can test
# fit <- sdmTMB(observed ~ 1 + b1 + b2, data = sim_dat, mesh = mesh, time = "year")
# fit_re <- sdmTMB(observed ~ 1 + b1 + b2 + (1|f_year), data = sim_dat, mesh = mesh, time = "year") # ml+fm: test random year
# fit_bi <- sdmTMB(observed ~ 1 + b1 + b2 + (1|f_year), data = sim_dat_binom, mesh = mesh, time = "year", family = binomial()) # ml+fm: binomial
# fit_tweedie <- sdmTMB(observed ~ 1 + b1 + b2 + (1|f_year), data = sim_dat_tweedie, mesh = mesh, time = "year", family = tweedie()) # ml+fm: tweedie
# fit_delta <- sdmTMB(observed ~ 1 + b1 + b2 + (1|f_year), data = sim_dat_tweedie, mesh = mesh, time = "year", family = delta_gamma()) # ml+fm: tweedie
# fit_pois <- sdmTMB(observed ~ 1 + b1 + b2 + (1|f_year), data = sim_dat_pois, mesh = mesh, time = "year", family = poisson()) # ml+fm: tweedie
# fit_st <- sdmTMB(observed ~ 1 + b1 + b2 + (1|f_year), data = sim_dat, mesh = mesh, time = "year", family = student()) # ml+fm: tweedie
# 
# 
# 
# # testing varCorr is the same as tidy
# t1 <- sdmTMB(observed ~ 1 + b1 + b2 + (1|f_year), data = sim_dat_binom, mesh = mesh, time = "year", family = binomial(), spatial = FALSE, spatiotemporal = FALSE)
# t2 <- lme4::glmer(observed ~ 1 + b1 + b2 + (1|f_year), data = sim_dat_binom, family = "binomial")
# 
# tidy(t1, "ran_par") # this gives stdev (as it says)
# lme4::VarCorr(t2) # this also prints stdev, BUT, in Nakagaw they either use as.numeric(VarCorr()) OR VarCorr(model)$random[1], which both return the VARIANCE, which is why they don't square it!
# as.numeric(VarCorr(t2)) # variance!
# VarCorr(t2)$f_year[1] # variance!

# ml+fm: ... change from x to x ... ?
fixef <- function(x) {
  b <- tidy(x)
  stats::setNames(b$estimate, b$term)
}

# fixef(x)
# fe <- fixef(x)
# X <- as.data.frame(x$tmb_data$X_ij) # ml+fm: delta model update; make it available to both components? currently first model; fm: won't work for bp models currently
# VarF <- var(as.vector(fixef(x) %*% t(X))) # variance from fixed-effects
# b <- tidy(x, "ran_par")
# sigma_O <- b$estimate[b$term == "sigma_O"] # spatial standard deviation
# sigma_E <- b$estimate[b$term == "sigma_E"] # spatiotemporal standard deviation
# VarO <- sigma_O^2
# VarE <- sigma_E^2
# VarR <- var(as.vector(residuals(x))) # residual variance
# VarF/(VarF + VarO + VarE + VarR)
# VarO/(VarF + VarO + VarE + VarR)
# VarE/(VarF + VarO + VarE + VarR)

# https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/inst/misc/rsqglmm.R
# ml+fm... change from x to x ... ?
r2.sdmTMB <- function(x, which_fixef = NULL, method = NULL) {
  
  if (!is(x, "sdmTMB")) {
    stop("x must be a model of class sdmTMB.", call. = FALSE)
  }
  if (length(x$family$family) > 1) { 
    stop("r2.sdmTMB() does not work for delta (hurdle) models.", call. = FALSE)
  }
  if (x$family$family == "student") { 
    warning("Warning: family is student, but the variance does not account for the degrees of freedom.")
  }
  if (x$family$family == "binomial" & is.null(method)) { 
    message("`method` not specified, using the theoretical approximation.") # ml+ fm: not sure this is the place for this message
  }
  if (!x$family$family %in% c("student", "gaussian", "binomial", "poisson", "tweedie"))
    stop("r2.sdmTMB() currently only works for Gaussian, binomial, Poisson and Tweedie models.", call. = FALSE)
  if (!is.null(x$spatial_varying)) {
    stop("r2.sdmTMB() currently does not work with spatially varying coefficient models.", call. = FALSE)
  }
  
  fe <- fixef(x)
  X <- as.data.frame(x$tmb_data$X_ij)  # ml+fm: delta model update; make it available to both components? currently first model; fm: won't work for bp models currently
  varF_all <- var(as.vector(fe %*% t(X))) # variance from fixed-effects
  if (!is.null(which_fixef)) {
    assert_that(max(which_fixef) <= length(fe))
    assert_that(min(which_fixef) >= 1)
    assert_that(is.numeric(which_fixef))
    assert_that(all(which_fixef %in% seq_along(fe)))
    message("Including fixed effects: ", paste(names(fe)[which_fixef], collapse = ", "))
    fe <- fe[which_fixef]
    X <- X[,which_fixef,drop=FALSE]
  }
  varF <- var(as.vector(fe %*% t(X))) # variance from fixed-effects; ml+fm: here we could make it different from varF_all, e.g., selecting certain predictors
  
  b <- tidy(x, "ran_par")
  
  if (x$tmb_data$include_spatial == 1L) {
    varO <- b$estimate[b$term == "sigma_O"]^2 # spatial variance
  } else {
    varO <- 0
  }
  
  if (x$tmb_data$spatial_only == 0L) {
    varE <- b$estimate[b$term == "sigma_E"]^2 # spatiotemporal variance
  } else {
    varE <- 0
  }
  
  if (x$tmb_data$random_walk == 1L) {
    if (!identical(x$time_varying, ~ 1))
      stop("r2.sdmTMB() currently only works with time-varying intercepts.", call. = FALSE)
    varV <- b$estimate[b$term == "sigma_V"]^2 # time-varying variance
  } else {
    varV <- 0
  }
  
  # ml+fm: random interecept
  if (x$tmb_data$nobs_RE > 0) {
    varG <- b$estimate[b$term == "sigma_G"]^2 # random effect variance
  } else {
    varG <- 0
  }
  
  #varR <- suppressMessages(var(as.vector(residuals(x)))) # residual variance
  varR <- suppressMessages(as.vector(var(x$tmb_data$y_i - predict(x)$est))) # variance of raw residuals; cannot use residuals.sdmTMB

  #if (varO != 0) { # ml+fm: previously it was: "if (varO != 0) {", but we want to make it work non spatial models. We replaced with if for family
  if (x$family$family %in% c("student", "gaussian")) {
    denominator <- varF_all + varO + varE + varR + varV + varG
  } else {
     
    if (x$family$family == "binomial") { # theoretical is default
      denominator <- varF_all + varO + varE + varV + varG + pi^2/3 # remove varR, add constant, from Nakagawa supp. row 115 (#A general and simple method for obtaining R2 from generalized linear mixed-effects models)
    } else {

    # #  
    # if (x$family$family == "binomial" & method == "observation-level") {
    #   Vt <- varO + varE + varV + varG
    # 
    #   if (x$tmb_data$nobs_RE > 0) {
    #     # need to fit an intercept only model but keep the random effect!
    #     re <- x$split_formula[[1]][[2]][[1]] # extract the random part so that we update the formula to remove all terms except the random effect
    #     m0 <- update(x, formula = as.formula(paste0(". ~ + 1 + ", deparse(re)))) # ml+fm: add some test here to check there are no fixed effects?
    #   } else {
    #     m0 <- update(x, formula = .~ +1) # need to fit an intercept only model
    #   }
    #   pmean <- plogis(as.numeric(fixef(m0)) -0.5 * Vt * tanh(as.numeric(fixef(m0)) * (1 + 2*exp(-0.5*Vt))/6)) # ml+fe: Nakagawa 2017 supp
    #   VarOL <- 1/(pmean * (1 - pmean))
    #   denominator <- varF_all + varO + varE + varV + varG + VarOL
    # } else {

      if (x$family$family == "poisson") {
        re <- x$split_formula[[1]][[2]][[1]]
        m0 <- update(x, formula = as.formula(paste0(". ~ + 1 + ", deparse(re)))) 
        denominator <- varF_all + varO + varE + varR + varV + varG + log(1 + 1/exp(as.numeric(fixef(m0))))
        } else {
          
          if (x$family$family == "tweedie") {
            phiN <- b$estimate[b$term == "phi"]
            pN <- b$estimate[b$term == "tweedie_p"]
            mu <- mean(x$tmb_data$y_i)
            VarOlN <- log(1 + (phiN * mu^(pN - 2))) # log-normal approximation
            denominator <- varF_all + varE + VarOlN + varV + varG # remove varR
          }
        }
      }
    }
#  }
  
  marg <- varF/denominator
  
  if (varO != 0) {
    cond_rf_sp <- (varO)/denominator
  } else {
    cond_rf_sp <- NULL
  }
  if (varE != 0) {
    cond_rf_spt <- (varE)/denominator
  } else {
    cond_rf_sp <- NULL
  }
  if (varV != 0) {
    cond_tv <- (varV)/denominator
  } else {
    cond_tv <- NULL
  }
  if (varG != 0) {
    cond_re <- (varG)/denominator
  } else {
    cond_re <- NULL
  }
  if (varE != 0 || varO != 0 || varV != 0 || varG != 0) {
    cond_all <- (varF + varO + varE + varV + varG)/denominator
  } else {
    cond_all <- NULL
  }
  
  out <- list(
    marginal = marg,
    partial_time_varying = cond_tv,
    partial_spatial = cond_rf_sp,
    partial_spatiotemporal = cond_rf_spt,
    partial_random = cond_re,
    conditional = cond_all
  )
  out[vapply(out, is.null, logical(1L))] <- NULL
  out
  
  # ml+fm: ... nicer output?
  as.data.frame(out) |> pivot_longer(everything(), names_to = "component", values_to = "R2")
  
}
 
# r2.sdmTMB(fit)
# r2.sdmTMB(fit_re)
# 
# r2.sdmTMB(fit_bi, method = "observation-level")
# r2.sdmTMB(fit_bi)
# r2.sdmTMB(fit_pois) # gives binomial error!!! wrong nesting if else?? it works if bimom hashtagged!
# r2.sdmTMB(fit_tweedie)
# 
# r2.sdmTMB(fit_st)
# r2.sdmTMB(fit_delta)
# 
# # Partition fixed effects R2? An attempt:
# dl <- list()
# 
# x <- fit_tweedie
# which_fixef <- nrow(tidy(x))
# for(i in 1:which_fixef) {
#   temp <- r2.sdmTMB(x, which_fixef = i) |>
#     filter(component == "marginal") |>
#     mutate(variable = tidy(x)$term[i])
# 
#     dl[[i]] <- temp
# 
#     }
#   
# partial_R2 <- bind_rows(dl)
#   
# out <- r2.sdmTMB(fit_tweedie) |>
#   mutate(variable = ifelse(!component == "marginal", component, "marginal"))
#   
# out <- out |> full_join(partial_R2)
# 
# library(ggplot2)
# # how to deal with intercept?
# ggplot(out |> filter(!variable %in% c("conditional", "marginal")), aes(1, R2, fill = variable)) + 
#   geom_bar(stat = "identity")

# r2.sdmTMB(fit_tweedie, which_fixef = 1)
# r2.sdmTMB(fit_tweedie, which_fixef = c(1, 2))
# r2.sdmTMB(fit_tweedie, which_fixef = c(1, 2, 3))
# r2.sdmTMB(fit_tweedie, which_fixef = c(2, 3))
# r2.sdmTMB(fit_tweedie, which_fixef = NULL)

# TODO:
# add method: c("lognormal approximation", "delta method", "theoretical distribution-specific", "observation-level")
# e-mail Sean?

# Next steps?
# logistic function / breakpoint compatability, not in X_ij
# In theory we could perhaps add some test/comparisons with Nakagawa's data, but they have multiple random effects... Test with glmMTMB??
# Splitting the covariates? varF
# Binomial second way: Nakagawa 2017 (https://royalsocietypublishing.org/doi/full/10.1098/rsif.2017.0213) supporting information
# Tweedie, different approach: Nakagawa 2017 (https://royalsocietypublishing.org/doi/full/10.1098/rsif.2017.0213) supporting information
# AUC in the binomial?
