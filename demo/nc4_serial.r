library(pbdDEMO, quiet = TRUE)

# -------------------------------------
# Serial write and read NCDF4 file
# -------------------------------------

### default of pbdMPI
rank <- comm.rank()
size <- comm.size()

### prepare data
x <- TREFHT$data

### define dimension and variable
lon <- ncdim_def("lon", "degree_east", vals = TREFHT$def$dim[[1]]$vals)
lat <- ncdim_def("lat", "degree_north", vals = TREFHT$def$dim[[2]]$vals)
var.def <- ncvar_def("TREFHT", "K", list(lon = lon, lat = lat), NULL)

### serial write
file.name <- "nc4_serial.nc"
if(rank == 0){
  nc <- nc_create(file.name, var.def)
  ncvar_put(nc, "TREFHT", x)
  nc_close(nc)
  ncdump(file.name)
}
barrier()

### serial read (everyone owns the same copy)
nc <- nc_open(file.name)
x.new <- ncvar_get(nc, "TREFHT")
nc_close(nc)

finalize()
