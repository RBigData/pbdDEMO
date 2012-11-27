library(pbdDEMO, quiet=T)

# Initialize MPI
library(pbdDEMO, quiet = TRUE)
init.grid()

# Read in example csv file
dx <- read.sql.ddmatrix(dbname="../extra/data/data", table="tabx", 
                        bldim=4, num.rdrs=1, ICTXT=0)

print(dx)

# Recombine on process 0 and print to show that everything worked.
x <- as.matrix(dx, proc.dest=0)
comm.print(x)

finalize()
