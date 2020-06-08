\name{hs99_d00}
\alias{hs99_d00}
\docType{data}
\title{Information table for the U.S. Health Service Areas Links the HSA 3 digit numbers
   to Registries and states.}
\description{
   This dataset contains a data.frame table used to provide the health service
   area (HSA) level information for all of the HSAs in the U. S. 
   to the \var{SeerMapper} package.  
   It provides the mapping of the HSA 3 digit number to the state 
   2 digit FIPS code, the associated Seer Registry abbreviation, 
   the HSA name, and statistics on the number of census tracts and counties 
   in each HSA for the 2000 and 2010 census years.
   }
\usage{data(hs99_d00)}
\format{
  The dataset contains a single data.frame named \var{co99_d00} that supports both the 
  2000 and 2010 census years. 
  
  Since a registry area may contain multiple HSAs, this data.frame is used
  to mark each HSA with the Seer Registry abbrevation it is a member.
  The 20 U. S. Seer registry areas are identify by the following character abbreviations (\var{saID}):
 
  \tabular{lll}{
      saID    \tab stID \tab      Name\cr
      AK-NAT\tab	02\tab	Alaska Natives\cr
      AZ-NAT\tab	04\tab	Arizona Indians\cr
      CA-OTH\tab	06\tab	California Other (not SF-Oakland, SJ-Monterey, LA areas)\cr
      CA-SF\tab	06\tab	California San Fran/Oakland\cr
      CA-SJ\tab	06\tab	California San Jose/Monterey\cr
      CA-LA\tab	06\tab	California Los Angeles\cr
      CT\tab 09\tab	Connecticut\cr
      GA-OTH\tab	13\tab	Georgia Other (not Atlanta or Rural areas)\cr
      GA-ATL\tab	13\tab	Georgia Atlanta\cr
      GA-RUR\tab	13\tab	Georgia Rural\cr
      HI\tab	15\tab	Hawaii\cr
      IA\tab	19\tab	Iowa	\cr
      KY\tab        21\tab	Kentucky\cr
      LA\tab	22\tab	Louisiana\cr
      MI-DET\tab	26\tab	Michigan Detroit\cr
      NJ\tab	34\tab	New Jersery\cr
      NM\tab	35\tab	New Mexico\cr
      OK-CHE\tab    40\tab	Oklahoma Cherokee Nation\cr
      UT\tab	49\tab	Utah\cr
      WA-SEA\tab	53\tab	Washington Seattle-Puget Sound\cr
    }    
The above table also contains the Registry's full name and the state's 2 digit FIPS code where the 
registry resides.  For more information on the NCI Seer Registry boundary data and information, refer 
to the document section on the \var{sa99_d00} dataset.
}
\details{
The \var{hs99_d00} dataset contains the following fields (columns):
 \describe{
   \item{row.names}{ = a character vector equal to the 3 digit U. S. 
     HSA number.
     }
   \item{HSAID}{ = a charactor vector of the 3 digit HSA number. 
     Same as the row.names.
    }
   \item{HSA}{ = a numeric value of the HSA number. 
     Same as the row.names.
    }
   \item{HSA_Name}{ - a character vector containing the assigned name of the 
     health service area.
    }
   \item{stID}{the state 2 digit FIPS code containing the HSA.
    }
   \item{y}{ = integer indicating which census years the HSA was active during.
    1=2000, 2=2010, 3=both.
    }
   \item{Chg10}{ = logical flag indicating the HSA changed in the 2010 census
    and to use the _d10.rda files for to get boundaries for this HSA.
    }
   \item{saID}{ = a character vector identifying the Seer Registry Abbreviation
     containing the county.  If the county is not a member of a registry, then the 
     field is set to NA.
    }
   \item{c_X_00}{ = A numeric value of the X coordinate of the centroid of the county in 
     the 2000 census.
    }
   \item{c_Y_00}{ = A numeric value of the Y coordinate of the centroid of the county in
     the 2000 census.
    }
   \item{c_X_10}{ = A numeric value of the X coordinate of the centroid of the county in 
     the 2010 census.
    }
   \item{c_Y_10}{ = A numeric value of the Y coordinate of the centroid of the county in 
     the 2010 census.
    }
   \item{county00}{ = An integer value of the number of 2000 counties are
      contained in the HSA.
    }
   \item{county10}{ = An integer value of the number of 2010 counties are  
      contained in the HSA.
    }
   \item{tracts00}{ = An integer value of the number of 2000 census tracts 
      are contained in the HSA.
    }
   \item{tracts10}{ = An integer value of the number of 2010 census tracts 
      are contained in the HSA.
    }
  }
}
\keyword{Census2000}
\keyword{Census2010}
\keyword{HSA}
\keyword{U. S.}
\keyword{Health Service Areas}
\keyword{datasets}
\keyword{SeerMapper}

