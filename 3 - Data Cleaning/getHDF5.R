# ########################################################################################
# Get data from HDF5
# 
# ########################################################################################


# ########################################################################################
# Import
# ########################################################################################

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)

###########################################################################################
# Create HDF5 file

#Create emtpy file
myHDF5 <- h5createFile("example.h5")

#Create groups
myHDF5 <- h5createGroup("example.h5","foo")
myHDF5 <- h5createGroup("example.h5","baa")

#Create subgroups
myHDF5 <- h5createGroup("example.h5","foo/foobaa")

#Display groups
h5ls("example.h5")

#Insert data
A=matrix(1:10,nrow=5,ncol=2)
h5write(A,"example.h5","foo/A")
h5write(c(12,13,14),"example.h5","foo/A",index=list(1:3,1))
df=data.frame(1:5,seq(0,1,length.out = 5))
h5write(A,"example.h5","df")

#Read data
readA= h5read("example.h5","foo/A")





