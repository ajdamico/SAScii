\name{SAScii-package}
\alias{SAScii-package}
\alias{SAScii}
\docType{package}
\title{
Import ASCII files directly into R using only a SAS input script
}
\description{
Using importation code designed for SAS users to read ASCII files into sas7bdat files, the SAScii package parses through the INPUT block of a (.sas) syntax file to design the parameters needed for a read.fwf() function call.  This allows the user to specify the location of the ASCII (often a .dat) file and the location of the .sas syntax file, and then load the data frame directly into R in just one step.
}
\details{
\tabular{ll}{
Package: \tab SAScii\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-08-18\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab Yes\cr
}
}
\author{
Anthony Joseph Damico

Maintainer: Anthony Joseph Damico <ajdamico@gmail.com>
}
\examples{
\dontrun{

##Load the 2009 Medical Expenditure Panel Survey Emergency Room Visits file as an R data frame

#Location of the ASCII 2009 Medical Expenditure Panel Survey Emergency Room Visits File
MEPS.09.ER.visit.file.location <- 
	"http://meps.ahrq.gov/mepsweb/data_files/pufs/h126edat.exe"

#Location of the SAS import instructions for the
#2009 Medical Expenditure Panel Survey Emergency Room Visits File
MEPS.09.ER.visit.SAS.read.in.instructions <- 
	"http://meps.ahrq.gov/mepsweb/data_stats/download_data/pufs/h126e/h126esu.txt"

#Load the 2009 Medical Expenditure Panel Survey Emergency Room Visits File
#NOTE: The SAS INPUT command occurs at line 273.
MEPS.09.ER.visit.df <- 
	read.SAScii ( MEPS.09.ER.visit.file.location , 
					MEPS.09.ER.visit.SAS.read.in.instructions , 
					zipped = T , 
					beginline = 273 )

#save the data frame now for instantaneous loading later
save( MEPS.09.ER.visit.df , file = "MEPS.09.ER.visit.data.rda" )

##Load the 2011 National Health Interview Survey Persons file as an R data frame

NHIS.11.personsx.SAS.read.in.instructions <- 
	"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/personsx.sas"
NHIS.11.personsx.file.location <- 
	"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2011/personsx.zip"

#store the NHIS file as an R data frame
NHIS.11.personsx.df <- 
	read.SAScii ( 
		NHIS.11.personsx.file.location , 
		NHIS.11.personsx.SAS.read.in.instructions , 
		zipped = T )

#or store the NHIS SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
NHIS.11.personsx.sas <- 
	parse.SAScii( NHIS.11.personsx.SAS.read.in.instructions )

#save the data frame now for instantaneous loading later
save( NHIS.11.personsx.df , file = "NHIS.11.personsx.data.rda" )


##Load the 2011 National Health Interview Survey Sample Adult file as an R data frame

NHIS.11.samadult.SAS.read.in.instructions <- 
	"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/samadult.sas"
NHIS.11.samadult.file.location <- 
	"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2011/samadult.zip"

#store the NHIS file as an R data frame!
NHIS.11.samadult.df <- 
	read.SAScii ( 
		NHIS.11.samadult.file.location , 
		NHIS.11.samadult.SAS.read.in.instructions , 
		zipped = T )

#or store the NHIS SAS import instructions for use in a
#read.fwf function call outside of the read.SAScii function
NHIS.11.samadult.sas <- 
	parse.SAScii( NHIS.11.samadult.SAS.read.in.instructions )

#save the data frame now for instantaneous loading later
save( NHIS.11.samadult.df , file = "NHIS.11.samadult.data.rda" )
}
}
