library(pbdDEMO, quietly = TRUE)

# Initialize MPI
init.grid()

# Read in example csv file
dir.path <- system.file("extra/data", package = "pbdDEMO")
dx <- read.sql.ddmatrix(dbname=dir.path, table="tabx", 
                        bldim=4, num.rdrs=1, ICTXT=0)

print(dx)

# Recombine on process 0 and print to show that everything worked.
x <- as.matrix(dx, proc.dest=0)
comm.print(x)

finalize()
