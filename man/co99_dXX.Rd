\name{co99_dXX}
\alias{co99_dXX}
\alias{co99_d00}
\docType{data}
\title{Information table for the counties in the US. Links the county FIPS code to Registries.}
\description{
   This dataset contains a data.frame table used to provide the county level information
   for all of the counties in the U. S. to the \var{SeerMapper} package.  
   It provides the mapping of the county 5 digit FIPS code to the state 2 digit FIPS code,
   the associated Seer Registry abbreviation, the county name, and statistics on the 
   number of census tracts in each county for the 2000 and 2010 census years.
   The information was originally stored in the Seer_stcoIDtosaID dataset and 
   the coxx_dxx datasets for each county and was moved to this data.frame when the
   2000 and 2010 census year county information was consolidated.
   }
\usage{data(co99_d00)}
\format{
  The dataset contains a single data.frame named \var{co99_d00} that supports both the 
  2000 and 2010 census years. 
  
  Since a registry area may contain multiple counties, this data.frame is used
  to mark each county with the Seer Registry abbreviation it is a member.
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
      NJ\tab	34\tab	New Jersey\cr
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
The co99_d00 dataset contains the following fields (columns):
 \describe{
   \item{row.names}{ = a character vector equal to the 5 digit U. S. 
     state and county FIPS code.
     }
   \item{ID}{ = a character vector of the 5 digit FIPS code for the 
     state and county. Same as the row.names.
    }
   \item{stID}{the state 2 digit FIPS code.
    }
   \item{stAbbr}{ = A character vector of the 2 letter state abbreviation.
    }
   \item{stName}{ = A character vector of the state full name.
    }
   \item{stcoID}{ = a character vector of the 5 digit FIPS code for the 
     state and county.
    }
   \item{coID}{ = a character vector of the 3 digit county FIPS code.
    }
   \item{coName}{ = a character vector of the county's name.
    }
   \item{stcotrID}{ = a character vector of the 11 digit FIPS code for the 
     state and county and tract (Set to NA in the co99_d00 dataset).
    }
   \item{saID}{ = a character vector identifying the Seer Registry Abbreviation
     containing the county.  If the county is not a member of a registry, then the 
     field is set to NA.
    }
   \item{c_X}{ = A numeric value of the X coordinate of the centroid of the county in 
     the 2000 census.
    }
   \item{c_Y}{ = A numeric value of the Y coordinate of the centroid of the county in
     the 2000 census.
    }
   \item{c_X10}{ = A numeric value of the X coordinate of the centroid of the county in 
     the 2010 census.
    }
   \item{c_Y10}{ = A numeric value of the Y coordinate of the centroid of the county in 
     the 2010 census.
    }
   \item{tracts00}{ = A integer value of the number of 2000 census tracts 
      contained in the county.
    }
   \item{tracts10}{ = A integer value of the number of 2010 census tracts 
      contained in the county.
    }
   \item{Yr}{ = A character value of the census year the county record was first used. 
     "00" = 2000, "10" = 2010.
    }
  }
}
\author{
Jim Pearson \email{jpearson@statnetconsulting.com}\cr 
Maintainer: "Jim Pearson" \email{jpearson@statnetconsulting.com}\cr 
}
\keyword{Census2000}
\keyword{Census2010}
\keyword{Registries}
\keyword{Seer}
\keyword{datasets}
\keyword{SeerMapper}

