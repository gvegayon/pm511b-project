library(magrittr)
library(dplyr)
library(tidyr)

# Reading the data in
dat <- haven::read_dta("data-raw/project_spring2019(1).dta")

# Processing the data (writing the right types)
dat$sid  <- as.integer(dat$sid)
dat$died <- as.integer(dat$died)
dat$age  <- as.integer(dat$age)
dat$male <- as.integer(dat$male)
dat$race <- as.integer(dat$race)

dat$val   <- as.integer(dat$val)
dat$asaps <- as.integer(dat$asaps)

# Race -------------------------------------------------------------------------
race <- c("Asian", "African American", "Non-Hispanic white", "Hispanic white")
dat$race <- race[dat$race]
dat$race2 <- factor(dat$race, levels = race)

dat <- model.matrix(~ race2, data = dat) %>%
  as_tibble() %>%
  select(-1) %>%
  set_colnames(tolower(colnames(.))) %>%
  set_colnames(gsub("^race2", "race_", colnames(.))) %>%
  set_colnames(gsub("([-]|\\s+)", "_", colnames(.))) %>%
  bind_cols(dat, .) %>%
  select(-race2) %>%
  mutate_at(vars(starts_with("race_")), as.integer)

# Recoding gsc
dat %>%
  mutate(
    gcs_cont = if_else(gcs == 1L)
  )

# Squares, cubes ---------------------------------------------------------------
for (v in c("age", "sbp", "rr", "gcs")) {
  
  # Roots, Squares and cubes
  dat[[paste0(v, "_over")]] <- 1/dat[[v]]
  dat[[paste0(v, "_2")]] <- dat[[v]]^2L
  # dat[[paste0(v, "_3")]] <- dat[[v]]^3L
  dat[[paste0(v, "_sqrt")]] <- sqrt(dat[[v]])
  
}

# Interactions -----------------------------------------------------------------
vars <- colnames(dat)
vars <- setdiff(vars, c("died", "sid", "race", "val", "asaps"))
vpairs <- combn(vars, 2, simplify = FALSE)
for (v in vpairs) {
  
  i <- v[1L]
  j <- v[2L]
  
  # No duplicates
  if (any(grepl("[_]([0-9]|over|sqrt)$", v))) {
    
    vars_tmp <- gsub("[_]([0-9]|over|sqrt)$", "", v)
    
    if (vars_tmp[1] == vars_tmp[2])
      next
    
  }
  
  # Creating the interaction between and j
  dat[[paste0(i, "__", j)]] <- dat[[i]] * dat[[j]]
  
}

saveRDS(dat, "model_data.rds")
