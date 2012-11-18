library(pbdDEMO, quiet=T)

# Initialize MPI
library(pbdDEMO, quiet = TRUE)
init.grid()

# Read in example csv file
dx <- read.csv.ddmatrix("../inst/data/x.csv", 
                        sep=",", nrows=10, ncols=10, header=TRUE,
                        bldim=4, num.rdrs=2, ICTXT=0)

print(dx)

# Recombine on process 0 and print to show that everything worked.
x <- as.matrix(dx, proc.dest=0)
comm.print(x)

finalize()
