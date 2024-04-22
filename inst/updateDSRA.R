devtools::load_all()
DT <- data.table()
NITER <- 50
Hmin <- 10
Hmax <- 25
Hs=seq(from=Hmin,to=Hmax)
PATH <- "data-raw/dsra"
W=c(0,0.25,0.50,1.0) #%
# POP=c(50,75,100,150,200,300,400,500,750,1000)
POP=c(100,200,300,600,800,1000,1200,1500,2000,2500)


# * RandomProfiles ----
USCS <- c(ValidSands,ValidFines,ValidGravels)
FILE <- file.path(PATH,"RandomProfiles.csv")
.runUSCS(FILE=FILE,USCS=USCS,Hs=Hs,POP=POP,NITER=NITER,W=W)


# * UniformProfiles ----
FILE <- file.path(PATH,"UniformProfiles.csv")
Group <- c("Gravels","Sands","Clays","Silts")
.runGROUP(FILE= FILE,Group=Group,Hs=Hs,POP=POP,NITER=NITER,W=W)
# * MixedGravelsSands----
USCS <- c(ValidSands,ValidGravels)
FILE <- file.path(PATH,"MixedGravelsSands.csv")
.runUSCS(FILE=FILE,USCS=USCS,Hs=Hs,POP=POP,NITER=NITER,W=W)



# * MixedGravelsClays ----
USCS <- c(ValidGravels,ValidClays)
FILE <- file.path(PATH,"MixedGravelsClays.csv")
.runUSCS(FILE=FILE,USCS=USCS,Hs=Hs,POP=POP,NITER=NITER,W=W)
##

# * MixedGravelsSilts ----
USCS <- c(ValidGravels,ValidSilts)
FILE <- file.path(PATH,"MixedGravelsSilts.csv")
.runUSCS(FILE=FILE,USCS=USCS,Hs=Hs,POP=POP,NITER=NITER,W=W)

# * MixedFines ----
USCS <- c(ValidFines)
FILE <- file.path(PATH,"MixedFines.csv")
.runUSCS(FILE=FILE,USCS=USCS,Hs=Hs,POP=POP,NITER=NITER,W=W)


# * MixedGravelsFines ----
USCS <- c(ValidGravels,ValidFines)
FILE <- file.path(PATH,"MixedGravelsFines.csv")
.runUSCS(FILE=FILE,USCS=USCS,Hs=Hs,POP=POP,NITER=NITER,W=W)

# * MixedSandsFines----
USCS <- c(ValidSands,ValidFines)
FILE <- file.path(PATH,"MixedSandsFines.csv")
.runUSCS(FILE=FILE,USCS=USCS,Hs=Hs,POP=POP,NITER=NITER,W=W)

# * MixedSandsSilts ----
USCS <- c(ValidSands,ValidSilts)
FILE <- file.path(PATH,"MixedSandsSilts.csv")
.runUSCS(FILE=FILE,USCS=USCS,Hs=Hs,POP=POP,NITER=NITER,W=W)




