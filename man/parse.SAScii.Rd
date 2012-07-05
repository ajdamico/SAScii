\name{parse.SAScii}
\alias{parse.SAScii}
\alias{SAS.uncomment}
\title{
Convert SAS import instructions into the arguments for a read.fwf function call.
}
\description{
Re-configure the INPUT block of a (.sas) syntax file into the arguments necessary to run the read.fwf function on an ASCII data set.
}
\usage{
parse.SAScii( sas_ri, beginline = 1 )
}
\arguments{
  \item{sas_ri}{
   Character string containing location of SAS import instructions.
}
  \item{beginline}{
   Line number in SAS import instructions where the INPUT statement begins.  If the word INPUT appears before the actual INPUT block, the function will return an error.
}
  \item{lrecl}{
   LRECL option from SAS code.  Only necessary if the width of the ASCII file is longer than the actual columns containing data (if the file contains empty space on the right side)
}
}
\details{
This function cannot handle overlapping columns.  For example, in the 2009 National Ambulatory Medical Care Survey (NAMCS) SAS import instructions, columns DIAG1 and DIAG13D will create an error because both start at space 55.
\cr
ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/dataset_documentation/namcs/sas/nam09inp.txt.
}
\value{
An R data frame containing:
\item{varname }{The name of the variable field}
\item{width }{The width of the field}
\item{char}{A logical flag indicating a character field if T and numeric if F}
\item{divisor }{A fraction to later be multiplied by numeric fields containing decimal points}
}
\author{
Anthony Joseph Damico
}
\examples{


###########
#Some Data#
###########

#write an example ASCII data set
some.data <- 
	"0154hello2304coolgreatZZ
	2034puppy0023nicesweetok
	9900buddy4495    swell!!"

#create temporary ASCII file
some.data.tf <- tempfile()
#write the sas code above to that temporary file
writeLines ( some.data , con = some.data.tf )

#write an example SAS import script using the @ method
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


############################################
#Survey of Income and Program Participation#
############################################

#first fourteen lines pulled from the Survey of Income and Program Participation
#http://smpbff2.dsd.census.gov/pub/sipp/2008/l08puw1.sas

sipp.sas <- 
	"LIBNAME sas8 v8 'current directory';
	FILENAME INPUTDAT 'l08puw1.dat' ;
	DATA sas8.l08puw1 ;
	INFILE INPUTDAT PAD LRECL=2341 ;
	INPUT
	 SSUSEQ 1- 5
	 SSUID $ 6- 17
	 SPANEL 18- 21
	 SWAVE 22- 23
	 SROTATON 24- 24
	 SREFMON 25- 25
	 RHCALMN 26- 27
	 RHCALYR 28- 31
	 SHHADID 32- 34
	 GVARSTR 35- 37
	 GHLFSAM 38- 38
	 GRGC $ 39- 41
	 TFIPSST 42- 43
	 TMOVRFLG 44- 45
			; RUN;"

#create a temporary file
sipp.tf <- tempfile()
#write the sas code above to that temporary file
writeLines ( sipp.sas , con = sipp.tf )
#parse that temporary file
sipp.fwf.parameters <- parse.SAScii( sipp.tf , beginline = 5 )
#print the results to the screen
sipp.fwf.parameters

###################################################
#NATIONAL HEALTH INTERVIEW SURVEY - IMPUTED INCOME#
###################################################

#INPUT lines pulled from the 2011 National Health Interview Survey's Imputed Income file
#ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2011_imputed_income/incmimp.sas

nhis.incmimp.sas <- 
	"   * INPUT ALL VARIABLES;

	   INPUT
	  RECTYPE  1-2   SRVY_YR  3-6  
	  HHX   7-12  FMX   13-14
	  FPX   15-16  IMPNUM   17   
	  FAMINCF2 18   TCINCM_F  19
	  FAMINCI2  20-25  POVRATI3  26-34
	  EMPLOY_F  35    EMPLOY_I  36    
	  ERNYR_F  37    TCEARN_F  38
	  ERNYR_I2  39-44
	  ;

	   * DEFINE VARIABLE LABELS;"

#create a temporary file
nhis.incmimp.tf <- tempfile()
#write the sas code above to that temporary file
writeLines ( nhis.incmimp.sas , con = nhis.incmimp.tf )
#parse that temporary file
nhis.incmimp.fwf.parameters <- parse.SAScii( nhis.incmimp.tf )
#print the results to the screen
nhis.incmimp.fwf.parameters


###################################################
#NATIONAL HEALTH INTERVIEW SURVEY - IMPUTED INCOME#
###################################################

#INPUT lines pulled from the 2011 National Health Interview Survey's Sample Adult file
#ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/samadult.sas

nhis.samadult.sas <- 
	"   * INPUT ALL VARIABLES;

	   INPUT

		  /* IDN LOCATIONS */

		  RECTYPE       1 -   2    SRVY_YR       3 -   6
		  HHX      $    7 -  12    INTV_QRT     13 -  13
		  ASSIGNWK     14 -  15    FMX      $   16 -  17
		  FPX      $   18 -  19    WTIA_SA      20 -  26 .1
		  WTFA_SA      27 -  32

		  /* UCF LOCATIONS */

		  REGION       33 -  33    STRAT_P      34 -  36
		  PSU_P        37 -  38
		;"

#create a temporary file
nhis.samadult.tf <- tempfile()
#write the sas code above to that temporary file
writeLines ( nhis.samadult.sas , con = nhis.samadult.tf )
#parse that temporary file
nhis.samadult.fwf.parameters <- parse.SAScii( nhis.samadult.tf )
#print the results to the screen
nhis.samadult.fwf.parameters


\dontrun{

#########################################################################################
#Create the read.fwf parameters required to load the
#2009 Medical Expenditure Panel Survey Emergency Room Visits file

#Location of the SAS import instructions for the
#2009 Medical Expenditure Panel Survey Emergency Room Visits File
MEPS.09.ER.visit.SAS.read.in.instructions <- 
	"http://meps.ahrq.gov/mepsweb/data_stats/download_data/pufs/h126e/h126esu.txt"

#Load the 2009 Medical Expenditure Panel Survey Emergency Room Visits File
#NOTE: The SAS INPUT command occurs at line 273.
MEPS.09.ER.visit.sas <- 
	parse.SAScii ( MEPS.09.ER.visit.SAS.read.in.instructions , beginline = 273 )


#########################################################################################
#Create the read.fwf parameters required to load the
#2011 National Health Interview Survey Persons file

NHIS.11.personsx.SAS.read.in.instructions <- 
	"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/personsx.sas"

#store the NHIS SAS import instructions for use in a 
#read.fwf function call outside of the read.SAScii function
NHIS.11.personsx.sas <- 
	parse.SAScii( NHIS.11.personsx.SAS.read.in.instructions )
}
}
