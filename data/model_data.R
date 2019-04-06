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

saveRDS(dat, "data/model_data.rds")
