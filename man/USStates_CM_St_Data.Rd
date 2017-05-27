\name{USStates_CM_St_Data}
\alias{USStates_CM_St_Data}
\docType{data}
\title{State Level Cancer Mortality Rates for U. S. for 2009 to 2013}
\description{
   This dataset containing the cancer mortality
   rates from the State Cancer Profile web site at the state level 
   for United States for the period of 2009 to 2013.  
   The dataset is used to support examples demonstrating 
   difference features of the \var{SeerMapper} and \var{Seer2010Mapper} packages.
   It was obtained through the State Cancer Profiles web site 
   at url{www.statecancerprofile.gov}.  This data represents
   all races and sexes for all sites of cancer.  The data was edited to 
   remove the recent trend data and to add a pseudo-pValue based on the 
   US age adjusted rate (AA_Rate = 168.5) and the abbreviation of any associated 
   Seer Registry (if any).  The pValue of the state was set to 0.02 unless
   it's rate was between 168.2 and 171.3.  When a state contains multiple Seer Registries, the 
   Abbreviation of "xx-MIX" is used where xx is the 2 character state abbreviation.
   }
\usage{data(USStates_CM_St_Data)}
\format{
  All of the state Cancer Mortality Rate datasets are at the county level and 
  contains 10 columns: 
  \describe{
   \item{stcoID}{is a numeric value for the state and county U. S. FIPS codes.  This is 
     used to link the data to the county and state boundary data.
     It must be a 4 or 5 character string.  For the US States data, the 5 digit
     FIPS code contains 2 characters for the state FIPS and three "0" for the county.
    }
   \item{State}{is a character vector state name provided by State Cancer Profiles.
    }
   \item{stID}{is a character vector containing the 2 digit state FIPS code.
    }
   \item{AA_Rate}{is a numeric value of the age-adjust rate for the county.
    }
   \item{Lower_CI}{is a numeric value for the lower confidence interval 
     value for the age-adjusted rate.
    }
   \item{Upper_CI}{is a numeric value for the upper confidence interval 
     value for the age-adjusted rate.
    }
   \item{Avg_DpY}{is a numeric value for the average number of deaths per year.
     This variable is not used in this package examples.
    }
   \item{saID}{is a character factor vector of the abbreviation of the Seer Registry
      the tract is a member.  The Seer Registry abbreviations match the 
      abbreviations used in the package boundaries datasets for each Seer Registry 
      state.  For Georgia the three Seer Registries are: GA-ATL, GA-RUR, and GA-OTH.
    }
   \item{pValue}{is a numeric value of 0.02 or 0.2. The pValue is set to 0.02 if the U. S. rate
      is not within the state's confidence interval range.  It is set to 0.2 if the U. S.
      rate is within the interval.  0.02 indicates the state's rate is significant, 
      while 0.2 indicates the rate is not significant.
    }
   \item{pV}{is a logical value indicating whether the county is significant or not.
    }
   \item{FIPS}{is a numeric value for the state and county U. S. FIPS codes.  This is 
     used to link the data to the county and state boundary data.
     It must be a 4 or 5 character string.  For the US States data, the 5 digit
     FIPS code contains 2 characters for the state FIPS and three "0" for the county.
     This is a duplicate column of the stcoID column for use in demonstrating using
     the default location ID column name.
    }
   \item{Rate}{is a numeric value of the age-adjust rate for the county.
     This is duplicate column to the AA_Rate column for use in demonstrating using
     the default dataCol value of "Rate".
    }

  }
  The first two rows of the data.frame represent the rate information 
  for the state and the USA.  These rows MUST be deleted before using the data.frame
  with the package.
}
\source{
 The US Cancer Mortality data for the years 2009 to 2013 for all races and sexes
 for each state for all cancer sites was exported from the State Cancer Profile web site
 \url{www.statecancerprofiles.gov} on September 2, 2016.  The data was modified to include
 the pValue, state's Seer Registry abbreviation (if associated), and a validated state name.  
}
\keyword{datasets}
\keyword{SeerMapper}
