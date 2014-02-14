library(pbdDEMO, quietly = TRUE)

if(comm.size() != 4){
  comm.stop("This example requries 4 processors.")
}

init.grid()


# Read in example csv file
file.path <- system.file("extra/data/x.csv", package = "pbdDEMO")
dx <- read.csv.ddmatrix(file.path,
                        sep=",", nrows=10, ncols=10, header=TRUE,
                        bldim=4, num.rdrs=2, ICTXT=0)

print(dx)

# Recombine on process 0 and print to show that everything worked.
x <- as.matrix(dx, proc.dest=0)
comm.print(x)

finalize()
