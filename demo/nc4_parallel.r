library(pbdDEMO, quiet = TRUE)

# -------------------------------------
# Parallel write and read NetCDF4 file
# -------------------------------------

### default of pbdMPI
rank <- comm.rank()
size <- comm.size()

### divide data into pieces by rank
x <- TREFHT$data
ncol <- ncol(x)
nrow <- nrow(x)

ncol.per.rank <- ceiling(ncol / size)
st <- c(1, 1 + ncol.per.rank * rank)
co <- c(nrow, ncol.per.rank)

### take care process overflows
if(st[2] + co[2] > ncol){
  if(st[2] <= ncol){  # fill the last piece
    co <- c(nrow, ncol - st[2] + 1)
  } else{             # empty matrix (rank > ncol)
    st <- c(1, 1)
    co <- c(0, 0)
  }
}
if(co[2] != 0){
  x.by.rank <- x[, st[2] - 1 + (1:co[2])]
} else{
  x.by.rank <- NULL
}

### define dimension and variable
lon <- ncdim_def("lon", "degree_east", vals = TREFHT$def$dim[[1]]$vals)
lat <- ncdim_def("lat", "degree_north", vals = TREFHT$def$dim[[2]]$vals)
var.def <- ncvar_def("TREFHT", "K", list(lon = lon, lat = lat), NULL)

### parallel write
file.name <- "nc4_parallel.nc"
nc <- nc_create_par(file.name, var.def)
nc_var_par_access(nc, "TREFHT")
ncvar_put(nc, "TREFHT", x.by.rank, start = st, count = co)
nc_close(nc)
if(rank == 0){
  ncdump(file.name)
}

### parallel read (everyone owns a portion)
nc <- nc_open_par(file.name)
nc_var_par_access(nc, "TREFHT")
x.by.rank.new <- ncvar_get(nc, "TREFHT", start = st, count = co)
nc_close(nc)

finalize()
