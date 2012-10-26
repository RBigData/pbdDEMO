# Data Filtering

# Intuitively, we want to read the data into blocks of contiguous 
  # rows, like how it might come out of the data reader. However,
  # doing so produces a data distribution which is inconvenient for 
  # actually dropping rows, because doing so will, in general, 
  # destroy block-cyclicality. So instead, we read into contiguous
  # columns, with each processor owning some subset of columns and
  # coordinate which rows to drop.

filter <- function(dx, ..., ICTXT, bldim)
{
  # setup
  oldctxt <- dx@CTXT
  if (missing(bldim))
    oldbldim <- dx@bldim
  else 
    oldbldim <- bldim
  
  if (dx@CTXT != 1) # move into column blocks (all rows owned)
    dx <- redistribute(dx=dx, bldim=dx@bldim/2, ICTXT=1)
  
  
  
  
  # filtering
  
  
  
  
  
  
  
  # wrangling the return distribution
  if (!missing(ICTXT))
    if (dx@CTXT != ICTXT)
      dx <- redistribute(dx=dx, bldim=oldbldim, ICTXT=2)
  else
    dx <- redistribute(dx=dx, bldim=oldbldim, ICTXT=0)
  
  return( dx )
}
