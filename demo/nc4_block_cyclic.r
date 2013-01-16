library(pbdDEMO, quiet = TRUE)

# -------------------------------------
# Parallel write and read NetCDF4 file
# -------------------------------------

### default of pbdMPI
rank <- comm.rank()
size <- comm.size()

### divide data into ddmatrix
x <- TREFHT$data
dx <- as.ddmatrix(x)

# define dimension and variable
lon <- ncdim_def("lon", "degree_east", vals = TREFHT$def$dim[[1]]$vals)
lat <- ncdim_def("lat", "degree_north", vals = TREFHT$def$dim[[2]]$vals)
var.def <- ncvar_def("TREFHT", "K", list(lon = lon, lat = lat), NULL)

### parallel write
file.name <- "nc4_block_cyclic.nc"
nc <- nc_create_par(file.name, var.def)
nc_var_par_access(nc, "TREFHT")
ncvar_put_dmat(nc, "TREFHT", dx)
nc_close(nc)
if(rank == 0){
  ncdump(file.name)
}

### parallel read (everyone owns a portion)
nc <- nc_open_par(file.name)
nc_var_par_access(nc, "TREFHT")
dx.new <- ncvar_get_dmat(nc, "TREFHT")
nc_close(nc)

finalize()
