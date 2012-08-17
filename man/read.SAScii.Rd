\name{read.SAScii}
\alias{read.SAScii}
\alias{withWarnings}
\title{
Create an R data frame by reading in an ASCII file and SAS import instructions
}
\description{
Using importation code designed for SAS users to read ASCII files into sas7bdat files, the read.SAScii function parses through the INPUT block of a (.sas) syntax file to design the parameters needed for a read.fwf function call, and then runs that command.  This allows the user to specify the location of the ASCII (often a .dat) file and the location of the .sas syntax file, and then load the data frame directly into R in just one step.
}
\usage{
read.SAScii( fn, sas_ri, beginline = 1, buffersize = 50, zipped = F , n = -1 , intervals.to.print = 1000 , lrecl = NULL , skip.decimal.division = NULL )
}
\arguments{
  \item{fn}{
   Character string containing location of ASCII filename (or if zipped = T, a filename ending in .zip).
}
  \item{sas_ri}{
   Character string containing location of SAS import instructions.
}
  \item{beginline}{
   Line number in SAS import instructions where the INPUT statement begins.  If the word INPUT appears before the actual INPUT block, the function will return an error.
}
  \item{buffersize}{
   Maximum number of lines to read at one time, passed to read.fwf().
}
  \item{zipped}{
   Flag noting if ASCII file should be unzipped / decompressed before loading.  Useful when downloading larger data sets directly from a website.
}
  \item{n}{
   the maximum number of records (lines) to be passed to read.fwf(), defaulting to no limit.
}
  \item{intervals.to.print}{
   the number of records to wait before printing current progress to the screen.
}
  \item{lrecl}{
   LRECL option from SAS code.  Only necessary if the width of the ASCII file is longer than the actual columns containing data (if the file contains empty space on the right side)
}
  \item{skip.decimal.division}{
   whether numeric columns should be divided based on how many decimal places are specified by the SAS import instructions.
   
   recommended: ignore this parameter (or set it to NULL) to let the function attempt to determine whether numeric columns have already been divided to hit the appropriate number of decimal places or not.
   
   TRUE tells read.SAScii to not perform any decimal-related division of numeric columns.
   
   FALSE tells read.SAScii to perform decimal-related division according to the SAS import instructions, ignoring the presence of numeric fields that already contain decimals.
}
}
\details{
This function cannot handle overlapping columns.  For example, in the 2009 National Ambulatory Medical Care Survey (NAMCS) SAS import instructions, columns DIAG1 and DIAG13D will create an error because both start at space 55.
\cr
ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/sas/nam09inp.txt.
}
\value{
A data.frame as produced by read.fwf() which is called internally.
}
\author{
Anthony Joseph Damico
}
\note{
Some of the commands below take days to run, depending on your machine.  If you need the Survey of Income and Program Participation, start the program before you quit working for the weekend.
}
\examples{

###########
#Some Data#
###########

#write an example ASCII data set
some.data <- "0154hello2304coolgreatZZ\n2034puppy0023nicesweetok\n9900buddy4495    swell!!"

#create temporary ASCII file
some.data.tf <- tempfile()
#write the sas code above to that temporary file
writeLines ( some.data , con = some.data.tf )

#write an example SAS import script using the at method
sas.import.with.at.signs <-
	"INPUT
		@1 NUMBERS1 4.2
		@5 WORDS1 $ 5.
		@10 NUMBERS2 2.0
		@12 NUMBERS3 2.0
		@14 WORDS2 $4.
		@18 WORDS3 $5
		@23 WORDS4 $ 1
		@24 WORDS5 $ 1
	;"
	

#create a temporary file
sas.import.with.at.signs.tf <- tempfile()
#write the sas code above to that temporary file
writeLines ( sas.import.with.at.signs , con = sas.import.with.at.signs.tf )

parse.SAScii( sas.import.with.at.signs.tf )

#using at signs sas script
read.SAScii( some.data.tf , sas.import.with.at.signs.tf )


#write an example SAS import script using the dash method
sas.import.with.lengths <-
	"INPUT
		NUMBERS1 1 - 4 .2
		WORDS1 $ 5-9
		NUMBERS2 10 -11
		NUMBERS3 12- 13 .0
		WORDS2 $14-17
		WORDS3$ 18-22
		WORDS4   $   23-23
		WORDS5 $24
	;"
	
#create a temporary file
sas.import.with.lengths.tf <- tempfile()
#write the sas code above to that temporary file
writeLines ( sas.import.with.lengths , con = sas.import.with.lengths.tf )

parse.SAScii( sas.import.with.lengths.tf )

#using dash method sas script
read.SAScii( some.data.tf , sas.import.with.lengths.tf )

\dontrun{


#########################################################################################
#Load the 2009 Medical Expenditure Panel Survey Emergency Room Visits file as an R data frame

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
	read.SAScii ( 
		MEPS.09.ER.visit.file.location , 
		MEPS.09.ER.visit.SAS.read.in.instructions , 
		zipped = T , 
		beginline = 273 )

#save the data frame now for instantaneous loading later
save( MEPS.09.ER.visit.df , file = "MEPS.09.ER.visit.data.rda" )


#########################################################################################
#Load the 2011 National Health Interview Survey Persons file as an R data frame

NHIS.11.personsx.SAS.read.in.instructions <- 
	"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/personsx.sas"
NHIS.11.personsx.file.location <- 
	"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2011/personsx.zip"

#store the NHIS file as an R data frame!
NHIS.11.personsx.df <- 
	read.SAScii ( 
		NHIS.11.personsx.file.location , 
		NHIS.11.personsx.SAS.read.in.instructions , 
		zipped = T )

#or store the NHIS SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
NHIS.11.personsx.sas <- parse.SAScii( NHIS.11.personsx.SAS.read.in.instructions )

#save the data frame now for instantaneous loading later
save( NHIS.11.personsx.df , file = "NHIS.11.personsx.data.rda" )


#########################################################################################
#Load the 2011 National Health Interview Survey Sample Adult file as an R data frame

NHIS.11.samadult.SAS.read.in.instructions <- 
	"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/SAMADULT.sas"
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
NHIS.11.samadult.sas <- parse.SAScii( NHIS.11.samadult.SAS.read.in.instructions )

#save the data frame now for instantaneous loading later
save( NHIS.11.samadult.df , file = "NHIS.11.samadult.data.rda" )


#########################################################################################
#Load an IPUMS - American Community Survey Extract into R

#DOES NOT RUN without downloading ACS ASCII files to
#your local drive from http://www.ipums.org/

#MINNESOTA POPULATION CENTER - IPUMS ASCII EXTRACTS & SAS import instructions
IPUMS.file.location <- "./IPUMS/usa_00001.dat"
IPUMS.SAS.read.in.instructions <- "./IPUMS/usa_00001.sas"

#store the IPUMS extract as an R data frame!
IPUMS.df <- 
	read.SAScii ( 
		IPUMS.file.location , 
		IPUMS.SAS.read.in.instructions , 
		zipped = F )

#or store the IPUMS extract SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
IPUMS.sas <- parse.SAScii( IPUMS.SAS.read.in.instructions )


#########################################################################################
#Load the Current Population Survey - 
#Annual Social and Economic Supplement - March 2011 as an R data frame

#census.gov website containing the current population survey's main file
CPS.ASEC.mar11.file.location <- 
	"http://smpbff2.dsd.census.gov/pub/cps/march/asec2011_pubuse.zip"
CPS.ASEC.mar11.SAS.read.in.instructions <- 
	"http://www.nber.org/data/progs/cps/cpsmar11.sas"

#create a temporary file and a temporary directory..
tf <- tempfile() ; td <- tempdir()
#download the CPS repwgts zipped file
download.file( CPS.ASEC.mar11.file.location , tf , mode = "wb" )
#unzip the file's contents and store the file name within the temporary directory
fn <- unzip( tf , exdir = td , overwrite = T )

#the CPS March Supplement ASCII/FWF contains household-, family-, and person-level records.
#throw out records that are not person-level.
#according to the SAS import instructions, person-level record lines begin with a "3"

#create a second temporary file
tf.sub <- tempfile()

input <- fn
output <- tf.sub

incon <- file(input, "r") 
outcon <- file(output, "w") 

#cycle through every line in the downloaded CPS file..
while(length(line <- readLines(incon, 1))>0){
	#and if the first letter is a 3, add it to the new person-only CPS file.
	if ( substr( line , 1 , 1 ) == "3" ){
		writeLines(line,outcon)
	}
}
close(outcon)
close(incon , add = T)

#the SAS file produced by the National Bureau of Economic Research (NBER)
#begins the person-level INPUT after line 1209, 
#so skip SAS import instruction lines before that.
#NOTE that the beginline of 1209 will change for different years.

#store the CPS ASEC March 2011 file as an R data frame!
cps.asec.mar11.df <- 
	read.SAScii ( 
		tf.sub , 
		CPS.ASEC.mar11.SAS.read.in.instructions , 
		beginline = 1209 , 
		zipped = F )

#or store the CPS ASEC March 2011 SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
cps.asec.mar11.sas <- 
	parse.SAScii( CPS.ASEC.mar11.SAS.read.in.instructions , beginline = 1209 )


#########################################################################################
#Load the Replicate Weights file of the Current Population Survey 
#March 2011 as an R data frame

#census.gov website containing the current population survey's replicate weights file
CPS.replicate.weight.file.location <- 
	"http://smpbff2.dsd.census.gov/pub/cps/march/CPS_ASEC_ASCII_REPWGT_2011.zip"
CPS.replicate.weight.SAS.read.in.instructions <- 
	"http://smpbff2.dsd.census.gov/pub/cps/march/CPS_ASEC_ASCII_REPWGT_2011.SAS"

#store the CPS repwgt file as an R data frame!
cps.repwgt.df <- 
	read.SAScii ( 
		CPS.replicate.weight.file.location , 
		CPS.replicate.weight.SAS.read.in.instructions , 
		zipped = T )

#or store the CPS repwgt SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
cps.repwgt.sas <- parse.SAScii( CPS.replicate.weight.SAS.read.in.instructions )

	
#########################################################################################
#Load the 2008 Survey of Income and Program Participation Wave 1 as an R data frame
SIPP.08w1.SAS.read.in.instructions <- 
	"http://smpbff2.dsd.census.gov/pub/sipp/2008/l08puw1.sas"
SIPP.08w1.file.location <- 
	"http://smpbff2.dsd.census.gov/pub/sipp/2008/l08puw1.zip"

#store the SIPP file as an R data frame

#note the text "INPUT" appears before the actual INPUT block of the SAS code
#so the parsing of the SAS instructions will fail without a beginline parameter specifying
#where the appropriate INPUT block occurs

SIPP.08w1.df <- 
	read.SAScii ( 
		SIPP.08w1.file.location , 
		SIPP.08w1.SAS.read.in.instructions , 
		beginline = 5 , 
		buffersize = 10 , 
		zipped = T )

#or store the SIPP SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
SIPP.08w1.sas <- parse.SAScii( SIPP.08w1.SAS.read.in.instructions , beginline = 5 )


#########################################################################################
#Load the Replicate Weights file of the 
#2008 Survey of Income and Program Participation Wave 1 as an R data frame
SIPP.repwgt.08w1.SAS.read.in.instructions <- 
	"http://smpbff2.dsd.census.gov/pub/sipp/2008/rw08wx.sas"
SIPP.repwgt.08w1.file.location <- 
	"http://smpbff2.dsd.census.gov/pub/sipp/2008/rw08w1.zip"

#store the SIPP file as an R data frame

#note the text "INPUT" appears before the actual INPUT block of the SAS code
#so the parsing of the SAS instructions will fail without a beginline parameter specifying
#where the appropriate INPUT block occurs

SIPP.repwgt.08w1.df <- 
	read.SAScii ( 
		SIPP.repwgt.08w1.file.location , 
		SIPP.repwgt.08w1.SAS.read.in.instructions , 
		beginline = 5 , 
		zipped = T )

#store the SIPP SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
SIPP.repwgt.08w1.sas <- 
	parse.SAScii( 
		SIPP.repwgt.08w1.SAS.read.in.instructions , 
		beginline = 5 )


#########################################################################################
#Load all twelve waves of the 2004 Survey of Income and Program Participation as R data frames
	
SIPP.04w1.SAS.read.in.instructions <- 
	"http://smpbff2.dsd.census.gov/pub/sipp/2004/l04puw1.sas"

#store the SIPP SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
SIPP.04w1.sas <- parse.SAScii( SIPP.04w1.SAS.read.in.instructions , beginline = 5 )

#note the text "INPUT" appears before the actual INPUT block of the SAS code
#so the parsing of the SAS instructions will fail without a beginline parameter specifying
#where the appropriate INPUT block occurs

#loop through all 12 waves of SIPP 2004
for ( i in 1:12 ){
	
	SIPP.04wX.file.location <- 
		paste( "http://smpbff2.dsd.census.gov/pub/sipp/2004/l04puw" , i , ".zip" , sep = "" )

	#name the data frame based on the current wave
	df.name <- paste( "SIPP.04w" , i , ".df" , sep = "" )
	
	#store the SIPP file as an R data frame!
	assign( 
	
		df.name , 
		
		read.SAScii ( 
			SIPP.04wX.file.location , 
			SIPP.04w1.SAS.read.in.instructions , 
			beginline = 5 , 
			buffersize = 5 , 
			zipped = T )
	)

}
}
}
