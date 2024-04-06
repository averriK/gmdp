devtools::load_all()

source("inst/dsra_helpers.R")

NITER <- 1:50
DT <- data.table()
Hmin <- 14
Hmax <- 14
Hs_SET=seq(from=Hmin,to=Hmax)
PATH <- "data-raw/dsra"
POPmin <- 50
POPmax <- 1000
# POP_SET=c(50,75,100,150,200,300,400,500,750,1000)
POP_SET=c(600,700,800,900,1100,1200,1300,1400,1500)


# * RandomProfiles ----
USCS_TARGET <- c(ValidSands,ValidFines,ValidGravels)
FILE <- file.path(PATH,"RandomProfiles.csv")
.runUSCS(FILE=FILE,USCS_TARGET=USCS_TARGET,Hs_SET=Hs_SET,POP_SET=POP_SET)


# * UniformProfiles ----
FILE <- file.path(PATH,"UniformProfiles.csv")
Group_SET <- c("Gravels","Sands","Clays","Silts")
.runGROUP(FILE= FILE,Group_SET=Group_SET,Hs_SET=Hs_SET,POP_SET=POP_SET)
# * MixedGravelsSands----
USCS_TARGET <- c(ValidSands,ValidGravels)
FILE <- file.path(PATH,"MixedGravelsSands.csv")
.runUSCS(FILE=FILE,USCS_TARGET=USCS_TARGET,Hs_SET=Hs_SET,POP_SET=POP_SET)



# * MixedGravelsClays ----
USCS_TARGET <- c(ValidGravels,ValidClays)
FILE <- file.path(PATH,"MixedGravelsClays.csv")
.runUSCS(FILE=FILE,USCS_TARGET=USCS_TARGET,Hs_SET=Hs_SET,POP_SET=POP_SET)
##

# * MixedGravelsSilts ----
USCS_TARGET <- c(ValidGravels,ValidSilts)
FILE <- file.path(PATH,"MixedGravelsSilts.csv")
.runUSCS(FILE=FILE,USCS_TARGET=USCS_TARGET,Hs_SET=Hs_SET,POP_SET=POP_SET)

# * MixedFines ----
USCS_TARGET <- c(ValidFines)
FILE <- file.path(PATH,"MixedFines.csv")
.runUSCS(FILE=FILE,USCS_TARGET=USCS_TARGET,Hs_SET=Hs_SET,POP_SET=POP_SET)


# * MixedGravelsFines ----
USCS_TARGET <- c(ValidGravels,ValidFines)
FILE <- file.path(PATH,"MixedGravelsFines.csv")
.runUSCS(FILE=FILE,USCS_TARGET=USCS_TARGET,Hs_SET=Hs_SET,POP_SET=POP_SET)

# * MixedSandsFines----
USCS_TARGET <- c(ValidSands,ValidFines)
FILE <- file.path(PATH,"MixedSandsFines.csv")
.runUSCS(FILE=FILE,USCS_TARGET=USCS_TARGET,Hs_SET=Hs_SET,POP_SET=POP_SET)

# * MixedSandsSilts ----
USCS_TARGET <- c(ValidSands,ValidSilts)
FILE <- file.path(PATH,"MixedSandsSilts.csv")
.runUSCS(FILE=FILE,USCS_TARGET=USCS_TARGET,Hs_SET=Hs_SET,POP_SET=POP_SET)




