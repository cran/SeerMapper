\name{messages}
\alias{messages}
\docType{data}
\title{SeerMapper Generated Warning and Error Messages}

\description{
  \var{SeerMapper} package tries to anticipates error and problems in 
  the R environment and generate more specific warnings and error 
  messages to help the user understand the problems and effect a solution
  and avoid as many cryptic R type messages. 
  
  Each message, warning and error message is documented in this section with the general form 
  of the message and a friendly explanation and advice on what may be wrong and 
  how to fix the issue identified.  

  All \var{SeerMapper} messages all start with "***" to help quickly find them 
  in the warnings() logs and general output.

  The general format of the messages is:
  \preformatted{

    ***XXX text of message
  
   }
  where the XXX is a unique numeric identifier for the message.
  
  When requesting support, it is recommended to include a copy of the 
  data and r script used to call the package and a copy of the output 
  log of the preparation and execution of \var{SeerMapper}.
}
\details{
  
  \bold{Conventions:}
  The message may contain dynamic values based on information and 
  variables related to the problem and provided by the caller.  These values appear 
  in message below as <nnnnn> strings. The following is a list of the 
  possible <nnnnn> strings the message may contain and their definition.
  
  The <nnnnn> string will be replaced with a value relevant to the \var{SeerMapper}
  function call being serviced. 
  \describe{
    \item{<badColors>}{a list of the bad colors found in the \var{dataCol} when 
        \var{categ} was set to "COLORS".  
     }
    \item{<badIDs>}{A list of invalid location IDs found in the \var{idCol} column
        of the \var{<ndfName>} data.frame.
     }
    \item{<badOptions>}{a list of invalid options in either the \var{hatch} or the 
        \var{mLegend} call parameter lists.
     }
    \item{<badValues>}{a list of bad data values found in the \var{<dataCol>} in the 
       \var{<ndfName>} data.frame.
     }
    \item{<catMaxNum>}{the maximum number of categories the package will support.
     }
    \item{<censusYear>}{A character vector used to contain the 4 digit census year of the 
        boundary data files to use with the data location IDs.  The values of "2000" and "2010" 
        are supported.
     }
    \item{<color>}{A character vector representating the name of a colors from the \var{colors()} 
       function or a 6 or 8 digit hexadecimal value (e.g., "#rrggbb" or "#rrggbbaa"). In the
       warning/error message this string is a color value that is not valid.
     }
    \item{<dataBCol>}{A character vector defining the color to be used as the borders 
        on the sub-areas containing data. This may be the state, county or tract boundaries 
        depending on what is being mapped.   
     }
    \item{<dataCol>}{a column name or number provided in the \var{dataCol} call parameter.
        It identifies the column in the \var{<ndfName>} data.frame, that contains the data to be 
        categorized, used a the integer category value, or the fill color for the sub-area.  
     }
    \item{<default>}{the default value for the specific parameter.
     }
    \item{<hatch:dataCol>}{a column name or number provided in the hatching \var{dataCol} option.
        It identifies the column in the \var{<ndfName>} data.frame, that contains the data to be 
        used to determine if hatching should be done over a sub-area.  
     }
    \item{<high value>}{the numerical value. Represents the high end of a range. 
     }
    \item{<idCol>}{The value of the \option{idCol} call parameter.  It is the 
       location id column name or number in the \var{<ndfName>} data.frame
       provided as the first call parameter.
     }
    \item{<idList>}{A list of location IDs referred to my the message.
     }
    \item{<low value>}{the numerical value. Represents the low end of a range. 
     }
    \item{<maxColumnN>}{The number of columns in the \var{<ndfName>} data.frame supplied by the caller.
     }
    \item{<missingDataSets>}{A list of boundary datasets required for the Location IDs
        found in the \var{<ndfName>} data.frame that cannot be found in the list of loaded 
        data() collection under R.       
     }
    \item{<missingPackages>}{A list of \var{SeerMapper} family packages that must 
        be installed to be able to support the Location IDs in the \var{<ndrName>}
        data.frame and the census year call parameter.  The packages include: 
        \var{SeerMapperEast}, \var{SeerMapperWest}, \var{SeerMapper2010}, 
        \var{SeerMapper2010East}, and \var{SeerMapper2010West}.
     }
    \item{<ndfName>}{The name of the statistics data.frame provided as the 
       first call parameter in the \var{SeerMapper} function call.  
       The data.frame contains the 
       location id of the data and the data to be categorized and mapped.
     }
    \item{<number>}{The number that may be the value of a parameter or a range limit of a 
        call parameter.  
     }
    \item{<packageName}{A character vector of a R package name.  This is generally
        one of the supplement census tract packages:  
        \var{SeerMapperEast}, \var{SeerMapperWest}, \var{SeerMapper2010}, 
        \var{SeerMapper2010East}, and \var{SeerMapper2010West}.
     }
    \item{<palColors>}{A character vector containing a character vector assigned to 
        the palColors call parameter.
     }
    \item{<palColorMaxNum>}{Based on the value of the \var{palColors} call parameter 
        specified, the package obtains the maximum number of colors it can 
        obtains from the RColorBrewer for palette selected.  \var{palColorsMaxNum}
        is that value.  
     }
    \item{<row numbers>}{One or more logical row numbers in the <ndfName> data.frame.  The 
        package may use the sequence row number of a row to help the user identify
        the data.frame row being referred to in the message. The first row is 1.
     }
    \item{<string>}{The value assigned to the call parameter mentioned in the message.
     }
    \item{<typeof>}{The R variable type of the value provided in a call parameters. 
     }
    \item{<value>}{the numerical value of the call parameter in error. 
     }
    \item{<year>}{The last two digits of the census year: "00" or "10".
     }
  }
  
  \bold{Messages:}
   
  The following is a listing of all \var{SeerMapper} generated messages and 
  a description of possible causes and solutions.  Each message contain specific 
  variable information to help quickly identify the cause of the problems 
  and allow the user to fix it and re-run the package.
  
  The error and warning message generated by the \var{SeerMapper} and \var{SeerMapper2010} 
  packages are listed below in numerical order:

   \describe{   
     \item{010}{\bold{The censusYear parameter is set to <censusYear> and is invalid. 
         It must be '2000' or '2010'.}
         
         The call parameter \option{censusYear} is an internal parameter and is 
         not generally used by the user.  It's only allowed census years are 2000 and 2010.  
         If the value of <censusYear> call parameter is any other value text, 
         the package terminate execution.  It is recommended the user use the \var{SeerMapper}
         and \var{SeerMapper2010} function calls without this call parameter
      }
     \item{011}{\bold{The Location IDs in column \var{idCol} in the \var{<ndfName>} data.frame are a mix of numbers 
         and characters.  They must be all digits for FIPS codes or characters 
         for Seer Area Abbreviations.} 
         
         The location ids supplied in the idCol column are not a valid type of variable. 
         They must be either digits or character strings.  Correct the location ids in the data.frame 
         and re-run.  An alternate cause may be the specification of the wrong column in the data.frame.
      }
     \item{015}{\bold{The palColors parameter value of <palColors> is not valid in 
         the RColorBrewer package. The default of 'RdYlBu' will be used.} 
         
         The value of the \option{palColors} call parameter must be acceptable to the RColorBrewer functions.
         
         The acceptable values are:
         'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 
         'Purples', 'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn', 'PuOr', 
         'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 
         'Set1', 'Set2', and 'Set3'         
                           
       Correct the call parameter and re-run.         
      }
     \item{020}{\bold{us48Only parameter is not a logical value of TRUE or FALSE. 
         The default of TRUE will be used.}
         
         The \option{us48Only} call parameter must be a logical value of TRUE or FALSE. 
      }
     \item{022}{\bold{includePR parameter is not a logical value of TRUE or FALSE. 
         The default of FALSE will be used.}
         
         The \option{includePR} must be a logical TRUE or FALSE.
      }
     \item{030}{\bold{The first parameter should be the statistics data.frame, 
         but is missing or NULL.}
     
         The first call parameter is not a data.frame or is missing.  Verify the 
         \var{<ndfName>} variable exists and is a data.frame with the correct structure.
         The '\code{str(<ndrName>)} R command can be used to help.
      }
     \item{031}{\bold{\var{<ndfName>} parameter is not a correctly formed data.frame.}
     
         The first call parameter is not a properly formed data.frame. It must have
         at least one row and one column.  Inspect the variable using 
         \code{str(\var{<ndfName>})} and correct the problem.
      }
     \item{032}{\bold{<ndfName> data.frame must have at least one column for Data.}

         The \var{<ndfName>} data.frame is using the row.names of the data.frame for the 
         location id. One column must be present to provide the data for the categorization
         or color for the sub-area.  Inspect the data.frame using \code{str(\var{<ndfName>})}
         and correct the problem.
      }
     \item{033}{\bold{<ndfName> data.frame must have at least two columns for ID and Data.}

         The \var{<ndfName>} data.frame must have at least two columns. One for the location ID and
         one for the data. Inspect the data.frame using \code{str(\var{<ndfName>})}
         and correct the problem.
      }
     \item{034}{\bold{<ndfName> data.frame does not have any data rows. 
         Must have at least one row of data.}
     
         Check the \var{<ndfName>} data.frame, correct to provide Location ID column and 
         data column and re-run.
      }
     \item{040}{\bold{idCol parameter is out of range. It must be a column number 
         between 1 and <maxColumnN>.}
          
         The \option{idCol} call parameter specifies a column number in the <ndfName> data.frame.  
         However, the number is less that 1 or greater than the number of columns in 
         the data.frame (<maxColumnN>).  Correct the value assigned to the \option{idCol} parameter.
      }
     \item{041}{\bold{idCol parameter is not a valid column name (<idCol>) in 
         the <ndfName> data.frame.}
       
         The \option{idCol} call parameter specifies a column name in the \var{<ndfName>} data.frame.  
         However, there is no column with the specified name (<idCol>).  
         Correct the value assigned to the \option{idCol} parameter.
      }
     \item{042}{\bold{idCol parameter is not the correct data type (<typeof>). 
         Must be numeric or character.}

         The \option{idCol} call parameter must be a numeric(integer) or character vector 
         with length of 1.  Verify and correct the value assigned to the \option{idCol} parameter.
      }
     \item{043}{\bold{idCol parameter is a character string, but is empty.}

         The \option{idCol} call parameter is a character vector, but is empty ("").
         It must point to a column in the \var{<ndfName>} data.frame. 
         Correct the value assigned to the \option{idCol} parameter.
      }
     \item{046}{\bold{The following ID values are not valid FIPS codes. 
         Check the first two digits for proper state codes: <badIDs>}
      
          Before loading the county and tract boundary data, the ID is check to see if 
          all of the state fips codes are correct.  This message identifies 
          the row entry(ies) where the first to digits of the FIPS code is not a 
          valid state Fips code.  Review the IDs on the supplied data and 
          correct any invalid ID codes.
      }
     \item{050}{\bold{\var{dataCol} parameter is out of range. It must be a column 
          number between 1 and <maxColumnN>.}

          The \var{dataCol} call parameter is specified as the column number in 
          the \var{<ndfName>} data.frame.  However, the value is out of range (< 1 or > the 
          number of columns in the data.frame <maxColumnN>.
          Correct the \var{dataCol} call parameter and rerun.
      }
     \item{051}{\bold{\var{dataCol} parameter is not a valid column name 
          (<dataCol>) in the <ndfName> data.frame.}
     
          The \var{dataCol} call parameter is specified as the column name in 
          the \var{<ndfName>} data.frame.  However, the column name does not exist.
          Correct the \var{dataCol} call parameter and rerun.
      }
     \item{052}{\bold{\var{dataCol} parameter is not the correct data type (<typeof>). 
          Must be numeric or character.}
      
          The \var{dataCol} call parameter is not the correct data type <typeof>.
          It must be a numeric or character vector.
          Correct the \var{dataCol} call parameter and rerun.
      }
     \item{053}{\bold{\var{dataCol} parameter is a character string, but is empty.}
      
          The \var{dataCol} call parameter is a character vector (string), but 
          is empty ("").  
          Correct the \var{dataCol} call parameter and rerun.
      }
     \item{055}{\bold{The \var{dataCol} parameter is set to NULL or NA, The default 
          column name of '<default>' will be used.}
         
          The \var{dataCol} call parameter is missing, NULL or set to NA.
          The default of <default> will be used.  The default is specified by <default> and
          is the column name of 'Rate'.  If \var{dataCol} is not specified, make
          sure the data is placed in the \var{<ndfName>} data.frame column named 'RATE'.
      }
     \item{060}{\bold{The following data rows in column <dataCol> of the <ndfName> data.frame contains
         missing (NA) values and will not be mapped. Location IDs: <idList>}
         
         The data in the column identified by <dataCol> or "Rate" contains one or more missing
         values (NA).  The list of location IDs with missing values is provided in the <idList> section
         of the message.  Any location with missing data will not be categorized or sub-area colored.
         It's boundary will be drawn, but colored "white". 
         Correct any locations with missing values and re-run.
       }
     \item{061}{\bold{Some of the data values in the <dataCol> column in the <ndfName>
         data.frame are not numeric values. Sub-areas will not be mapped. Location IDs: <idList>}
      
         The data in column <dataCol> of the data.frame was a factor or character type. The values provided
         could not be converted to numeric.  The <idList> provide the list of location ids with the 
         bad numeric data values.  Review and correct these values and re-run.
      }
     \item{062}{\bold{Some of values in data column <dataCol> are not 
         numeric values. Value set to NA.}
         
         After initial validation of the data in column \var{dataCol}, when 
         categorization is requested, one or more of the values were found to be NA (missing)
         The data row in the \var{<ndfName>} data.frame are ignored and the sub-area is not categorized
         mapping area filled with any color.
      }
     \item{063}{\bold{    Bad value(s): <badValues>}
      
         In association with message 062, <badValues> is a the rows in the \var{<ndfName>} data.frame
         that contain bad data. The rows are dropped and not mapped.
      }
     \item{064}{\bold{The data column \var{<dataCol>} in \var{<ndfName>} data.frame 
         is not numeric or character type data. Processing terminated.}
         
         The value provided at the \var{dataCol} call parameter is not a numeric 
         or character value. It may be a 'logical' or other type of value
         that cannot be used to identify the data column in the 
         \var{<ndrName>} data.frame  Execution is stopped.
      }
     \item{065}{\bold{Data in column (<dataCol>) in \var{<ndfName>} data.frame 
         contains one or more values that are not a color and will be ignored.}
     
        The \var{categ} is set to "COLORS".  The package will use the 
        colors in the column as the fill colors for the sub-areas.  However,
        one or more of the values in the \var{<dataCol>} column are not 
        valid colors.  The data must be character vectors and either the
        name of a color that matches one returned by the \var{colors()} function
        or a 6 or 8 digit hexadecimal character vector (e.g., "#rrggbb" or "#rrggbbaa")
      
        The sub-areas with invalided colors will be drawn filled with "white".
      }
     \item{066}{\bold{  Bad Color values: <badColors> }
      
        <badColors> is a list of bad colors found in the <dataCol> 
        column related to message 095 when categ="COLORS".
        Sub-areas with these values will be drawn but filled with "white".
      }
     \item{067}{\bold{The dataCol (<dataCol>) contains <number> colors. It is recommended 
         to limit the number of colors to 11.}

        Any number of colors can be used to create a map.  However, is a 
        legend is to be created, it's recommended 11 colors or less be sued.
        The package found <number> of colors present in the data.  Complications may 
        occur when the legend is drawn for the map.
      }
     \item{080}{\bold{The regionB call parameter is <string> and must be 'NONE', 
         'DATA' or 'ALL'. The default of '<default>' will be used.}
         
         The boundary control \option{regionB} must be a character vector and have
         a value of "NONE", "DATA", or "ALL".  It is set to the default based on the 
         data being mapped. <default> indicates the default being used, either "NONE" or "DATA".
      }
     \item{082}{\bold{The stateB call parameter is <string> and must be 'NONE', 
         'DATA' or 'ALL'. The default of '<default>' will be used.}
         
         The boundary control \option{stateB} must be a character vector and have
         a value of "NONE", "DATA", or "ALL".  It is set to the default based on the 
         data being mapped. <default> indicates the default being used, either "NONE" or "DATA".
      }
     \item{084}{\bold{The seerB call parameter is <string> and must be 'NONE', 
         'DATA', 'STATE', or 'ALL'. The default of '<default>' will be used.}
         
         The boundary control \option{seerB} must be a character vector and have
         a value of "NONE", "DATA", "STATE" or "ALL".  It is set to the default based on the 
         data being mapped. <default> indicates the default being used, either "NONE" or "DATA".
      }
     \item{086}{\bold{The countyB call parameter is <string> and must 
         be 'NONE', 'DATA', 'SEER', or 'STATE'. The default of '<default>' will be used.}
         
         The boundary control \option{countyB} must be a character vector and have
         a value of "NONE", "DATA", "SEER", or "STATE".  It is set to the default based on the 
         data being mapped. <default> indicates the default being used, either "NONE" or "DATA".
      }
     \item{089}{\bold{tractB call parameter is <string> and must be 
         'NONE', 'DATA', 'COUNTY', 'SEER' or 'STATE'. The default of '<default>' will be used.}
         
         The boundary control \option{tractB} must be a character vector and have
         a value of "NONE", "DATA", "COUNTY", "SEER", or "STATE".  It is set to the default based on the 
         data being mapped. <default> indicates the default being used, either "NONE" or "DATA".
      }
     \item{094}{\bold{The clipTo call parameter is <string> and must be 'NONE', 'DATA', 
         'SEER', 'STATE' or 'TRUE'/'FALSE'. The default of 'NONE' will be used."}
         
         The boundary control \option{clipTo} must be a character vector and have
         a value of "NONE", "DATA", "SEER", or "STATE" or a logical value of 
         \var{TRUE} or \var{FALSE}.  It is set to the default of "NONE",
      }
     \item{100}{\bold{The dataBCol call parameter is not a valid color: <dataBCol>.  
         The default of 'black' will be used.}

        The \var{dataBCol} call parameter value is not a valid color.  The parameter
        was set to "<dataBCol>".  The value must be the name of a color from the set of 
        names returned by the \var{colors()} function or a 6 or 8 digit hexadecimal 
        character string (e.g., "#rrggbb" or "#rrggbbaa").  The default color of 'black'
        is will be used.
     }
     \item{103}{\bold{The brkPtDigits call parameter must be greater than 0, no greater than 5 
         and not NA. Set to a value of 2.}

        The \var{brkPtDigits} call parameter is out of range.  It must be 
        > 0 and <= 5 digits.  The calculated values for the break point when \var{categ} is 
        set to a number of categories, will be rounded to the default value of 2 decimal
        digits.  Correct or remove the call parameter for future maps.
     }
     \item{105}{\bold{The 'mTitle' option can only contain one or two strings.
         Only the first two will be used.}
      
        The \var{mTitle} option can consist of one or two character vectors (e.g.
        mTitle=c("Title Line 1","Sub-title line 2").  However, when checking the 
        \var{mTitle} option it's length is > 2.  
        Only the first two title lines will be used.  For future maps, reduce the 
        number of title lines to 2 or less.
      }
     \item{106}{\bold{The 'mTitle' option is not a character vector. 
         'mTitle' parameter is ignored.}

        The \var{mTitle} option is not a character vector.  Only character
        vectors are supported for titles of maps.  The \var{mTitle} option
        is ignored.  Correct to get titles on future maps.
     }
     \item{108}{\bold{The 'mTitle.cex' call parameter is out of range (<=0 or >4). 
         'mTitle.cex' is set to the default of 1.}

        The \var{mTitle.cex} call parameter specifies the size multiplier (cex) 
        to be used when creating the title lines on the map. 
        The value of \var{mTitle.cex} was found out of the range for the 
        parameter (<= 0 or >4).  \var{mTitle.cex} was set to the default 
        value of 1.   
        Correct or remove the call parameter for future maps.
     }
     \item{109}{\bold{The 'mTitle.cex' option is not a numeric value. 'mTitle,cex' 
         parameter is set to the default of 1.}

        The \var{mTitle.cex} call parameter is not a numeric value.  
        \var{mTitle.cex} was set to the default value of 1.   
        Correct or remove the call parameter for future maps.
     }
     \item{110}{\bold{hatching:dataCol parameter is out of range. It must be a column 
          number between 1 and <maxColumnN>.}

          The \option{hatching:dataCol} call parameter is specified as the column number in 
          the \var{<ndfName>} data.frame.  However, the row number value is out of range 
          (< 1 or > the number of columns in the data.frame <maxColumnN>.)  The hatching 
          data for the comparison cannot be found.  Hatching is disabled.
          Correct the \option{hatching:dataCol} call parameter and rerun.
      }
     \item{111}{\bold{hatching:dataCol parameter is not a valid column name 
          (<dataCol>) in the <ndfName> data.frame.}
     
          The \option{hatching:dataCol} hatch option is specified as the column name in 
          the \var{<ndfName>} data.frame.  However, the column name provided does not exist in 
          the \var{<ndfName>} data.frame.  The hatching data for the comparison cannot be found.  
          Hatching is disabled.
          Correct the \option{hatching:dataCol} call parameter and rerun.
      }
     \item{112}{\bold{hatching:dataCol parameter is not the correct data type (<typeof>). 
          Must be numeric or character.}
      
          The data type of the \option{hatching:dataCol} hatch option was found to be <typeof>.
          It must be a numeric or character value to be able to reference the hatch data column
          in the \var{<ndfName>} data.frame.  Since the hatch data column cannot be found, 
          hatching is disabled.
          Correct the \option{hatching:dataCol} call parameter and rerun.
      }
     \item{113}{\bold{hatching:dataCol parameter is a character string, but is empty.}
      
          The \option{hatching:dataCol} call parameter is a character vector (string), but 
          is empty ("").  The hatch data column in the \var{<ndfName>} data.frame cannot be found. 
          Hatching is disabled.
          Correct the \option{hatching:dataCol} call parameter and rerun.
      }
     \item{115}{\bold{The following hatch settings are not valid 
          and will be ignored: <badOptions>}
        
          When validating the list of options in the \var{hatch} call parameter list, several
          invalid options were found.  The '<badOptions>' contains a list of the 
          invalid variables.  These options will be ignored.  Remove these items from the 
          \var{hatch} call parameter named list or correct the entries.
          The list of valid \var{hatch} options are: dataCol, ops, value, col, lty, lwd, den, 
          angle, and range.  
       }
     \item{117}{\bold{Hatching range values out of order.  First value 
          must be less than second value. Reversed.}
           
          The low and high values in the hatch range option are out of order.  There have been 
          sorted.
      }
     \item{118}{\bold{The hatching range option is not valid. It must be NA or a
         vector containing 2 numeric values (low and high limits) for the range.  
         Range checking is disabled.}

         The hatching range checking parameter must be "NA" to disable range changing
         or a vector of two numeric values representing the low and high limits
         of the range.  If the values are out of order, they will be reversed.
         If either value is an NA, range checking will be disabled.  An example of
         the use of the hatching option is:  range=c(0,20)  for a range change
         of 0 to 20.
      }
     \item{119}{\bold{Hatching dataCol is not a character vector or numeric. 
         The default value of 'pValue' will be used.}
         
         The name or number of the \var{hatch} dataCol, is not a valid column name or number.
         The default value of "pValue" as the column name will be used.
      }
     \item{120}{\bold{The hatch logical operator provided in the \option{ops} option -
          <string> - is not valid. Hatching disabled.}
         
          The \var{hatch} call parameter contains a \option{ops} options of <string> that is 
          not valid.  
         
          The allowed hatch \option{ops} is not a valid operation.  The allowed 
          comparision operation values are: "eq","==", "=", "ne", "<>", "!=", "lt", "<", 
          "le", "<=", "=<", "gt", ">", "ge", ">=", and "=>".  The comparison is 
          <hatch:dataCol-value> <ops> <hatch:value>.
         
      }
     \item{123}{\bold{The hatch col setting is not a valid color : <color>. 
        The default will be used.}
       
        The \var{hatch} \option{col} does not specify a valid color.  It must be 
        a name that matches the color names in \var{colors()} or a 6 or 8 digit 
        hexadecimal color value
        (e.g., "#rrggbb" or "#rrggbbaa".)  The default hatch color value 
        of "grey(0.66)" will be used.
      }
     \item{126}{\bold{The hatch lwd setting is not numeric - <value> The default 
         will be used.}
        
         The \var{hatch} call parameter \option{lwd} option value is not a numeric value.
         The line weight values for the hatch lines must a numeric value > 0.  The 
         default value of 0.85 is used.
      }      
     \item{128}{\bold{The hatch lwd setting is out of the range of >0 to <= 5. 
         The default will be used.}
       
         The \var{hatch} call parameter \option{lwd} option value is out of range. 
         It must be in a range > 0 and <= 5. Correct the call and re-run.  The 
         default value of 0.85 will be is used.
      }
     \item{130}{\bold{The hatch lty setting is not valid. Check the 
         par(lty) variable for acceptable values. The default will be used.}
       
         The \var{hatch} call parameter \option{lty} option value is not a valid line type 
         name.  It must be a numeric value or character vector value.
         If numeric, it must be in the range 0 to 6 or the type name: "blank", "solid" (default),
         "dashed", "dotted", "dotdash", "longdash", and "twodash".  
         The default value of 1 is used.
      }
      \item{131}{\bold{&The hatch lty setting is not numeric. The default will be used.}
      
          The \var{hatch} call parameter \option{lty} option is not a numeric value.
          It must be a integer value from 0 to 6.  The default value of 1 will be used.
          Valid values are: 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 
          5=longdash, and 6=twodash.
       }
      \item{133}{\bold{The hatch lty setting is out of the range of 0 to 6. 
          The default value of 'solid' will be used.}
          
          The \var{hatch} call parameter \option{lty} option is numeric, but out of range.
          It must be between 0 and 6.  Correct the option value and re-run.
          The default value of 1 is used.
       }
      \item{135}{\bold{The hatch density setting is not numeric - <value>  
          The default value of 25 will be used.}
      
          The \var{hatch} call parameter \option{den} option is not a numeric value. 
          It was set to <value>.  The option specifies the density of the 
          hatching in number of lines per inch.  The default is 25 lines per inch.
          The density may need to be adjusted if the size of the graphic space is not 7.5 x 10 inches.
       }
      \item{136}{\bold{The hatch density setting is <value> and is out of 
          the range of > 4 to <= 64 lines per inch.  The default value of 25 will be used.}
      
          The \var{hatch} call parameter \option{den} option is out of range.  It must be
          a numeric value > 4 and <= 64 lines per inch.  The default value of 25 lines
          per inch will be used.
       }
      \item{137}{\bold{The hatch angle setting is not numeric. The default will be used.}
      
          The \var{hatch} call parameter \option{angle} option is not a numeric value.
          The value is the degrees the hatching lines will be draw over the sub-area.
          The default value of 45 degrees will be used.
       }
      \item{138}{\bold{The hatch angle setting is out of the range 
          of => -360 to <= 360 degrees. The default will be used.}
      
          The \var{hatch} call parameter \option{angle} option is out of range.  It must be
          a numeric value between -360 and 360 degrees.  The default value of 45 degrees
          will be used.
       }
      \item{139}{\bold{Hatching parameter error detected. Hatching disable. 
          See previous messages for details.}
      
          Previous message detail the errors detected in processing the \var{hatch} call
          parameter options list.  Since errors were detected, hatching will be disabled.
          Correct the identified problems and re-run.
       }
      \item{140}{\bold{The hatch call parameter must be a logical value (T/F) 
           or a list of options. Parameter is ignored.}
           
          The \var{hatch} call parameter can be a logical TRUE/FALSE value of a named list
          of hatch options.  The \var{hatch} call parameter was neither a logical variable or a list.
          Hatching is disabled.  
       }
      \item{144}{\bold{The hatch data column <hatch:dataCol> does not contain numbers. 
          Hatching disabled.}
          
          The column identified to be used for the hatch data comparison, <hatch:dataCol>,
          does not contain numbers.  Only numerical comparisons are supported.
          Hatching is disabled.
       }
      \item{146}{\bold{Hatching data provided is not within the allowed range : 
          <low value> to <high value>. Hatching disabled.}

          Range checking of the hatch data is enabled.  The hatch data in the <hatch:dataCol> 
          column is not within the range of <low value> to <high value>.  Without all 
          of the data within the specified range, hatching will be disabled.
       }
      \item{150}{\bold{The 'mLegend' parameter is set to NA.  
          Set to the default of TRUE.}

          The \var{mLegend} call parameter is set to NA.  The value must be a logical (TRUE/FALSE)
          or a named list of options.  The default of TRUE will be used.
       }
      \item{152}{\bold{The 'mLegend' call parameter must be a list or logical value. 
          Parameter Ignored.}

          The \var{mLegend} call parameter is not a logical variable or named list.
          The call parameter is ignored and the default of TRUE will be used.
          Check the function all and correct the mLegend=XXXX for the cause of the error.
       }
      \item{153}{\bold{The mLegend options list does not names for each list entry.  
          The format must be mLegend=list(pos='left',numCols=4). Defaults used.}
 
          The \var{mLegend} call parameter is set to NA.  The value must be a logical (TRUE/FALSE)
          or a named list of options.  The default of TRUE will be used.
       }
      \item{154}{\bold{The following options in the Legend parameter are not valid 
          and will be ignored: <badOptions>.}

          The \var{mLegend} call parameter is a named list, but one or more of the variables 
          in the list are not valid. They are listed in the <badOptions> in the message.
          Valid variables are: counts, size, numCols, pos, noValue, pch, or labels.
          The invalid options are ignored.
       }
      \item{156}{\bold{The 'mLegend' parameter 'counts' must a a logical variable. 
          Set to FALSE.}

          The \var{mLegend} call parameter \option{counts} option was specified, the variable
          is not a logical TRUE or FALSE value.  Correct the value to TRUE or FALSE. 
          The default value of FALSE will be used.
       }
      \item{158}{\bold{The mLegend parameter 'size' option must be a numeric value.
          The default is used.}

          The \var{mLegend} call parameter \option{size} option was specified, the variable
          is not a numeric value setting the font size multiplier (cex).  The
          default value of 0.85 will be used.  Correct the value to a numeric and re-run.
       }
      \item{160}{\bold{The mLegend parameter 'size' option must be in the range 
          from 0 to 5. Set to 0.85.}
      
          The \var{mLegend} call parameter \option{size} option was specified, the variable
          is not in the range of 0 to 5.  The default of 0.85 is used as the font size multiplier.
          Correct the value to a value between 0 and 5 and re-run. 
       }
      \item{162}{\bold{The mLegend parameter option 'numColumns' must be numeric 
          and between 1 and 8. Set to 3.}
      
          The \var{mLegend} call parameter \option{numColumns} option was specified, the variable
          is not a numeric setting the number of columns in the legend. The default value of 
          3 columns will be used.  Correct the value and re-run. 
       }
      \item{164}{\bold{The legendPos parameter is not "left", "center", or "right".           
          Set to "left".}
      
          The \var{mLegend} call parameter \option{pos} option was specified, the variable
          sets the position of the legend on the bottom of the map.  The value is 
          not valid.  It must be 'left", "center" or "right".  The default of "left" will 
          be used.   Correct the option and re-run.
       }
      \item{166}{\bold{The pch parameter must be a numeric value. Set to 19.}

          The \var{mLegend} call parameter \option{pch} option was specified, the variable
          sets symbol used in the legend.  The value provided is not valid. The pch value 
          must be a value from 19 to 25 (filled symbols).  The default symbol value of 22 
          will be used (a square). Correct the option and re-run.
       }
      \item{167}{\bold{The pch parameter must be a value between 
          19 and 25. Set to 19.}
       
          The \var{mLegend} call parameter \option{pch} option was specified, the variable
          sets symbol used in the legend.  The value provided is out of range. The pch value 
          must be a value from 19 to 25 (filled symbols).  The default symbol value of 22 
          will be used (a square). Correct the option and re-run.
       }
      \item{168}{\bold{The noValue parameter must be a logical 
          (TRUE or FALSE) value. Set to FALSE.}
       
          The \var{mLegend} call parameter \option{noValue} option was specified, the variable
          requests legend entries with no sub-areas be listed in the legend. The value is not a 
          logical variable.  The value must be a logical variable set to TRUE or FALSE. 
          The default of TRUE will be used.
       }
      \item{170}{\bold{The labels parameter must be a vector of character 
          strings. Set to an empty string.}
 
          The \var{mLegend} call parameter \option{labels} option was specified, the variable
          is not a vector of replacement labels for the legend.  The labels option was found to 
          be empty (set to "").  The labels option is disabled.  Correct the value assigned to 
          the labels option and re-run.
       }
      \item{172}{\bold{The mLegends parameter is an empty list.  
          The legend will be drawn using default values.}
 
          The \var{mLegend} call parameter named list of options was found to be empty.
          No options were specified.  The legend will be drawn using the defaults for 
          each option.
       }
      \item{194}{\bold{The regions parameter is not a logical value of TRUE or FALSE. 
          The default of FALSE will be used.}
          
          Retired message
          
          The \var{regions} call parameter must be a logical type variable with a value
          of TRUE or FALSE.  Since it is not, the default value of FALSE will be used.  
       }
      \item{195}{\bold{Package: <packageName> failed to load into the system. 
          Verify package is install and try manually executing a 'library(<packageName>)' command.}
          
          To be able to map census tract boundaries, supplemental data packages must be installed
          on the local machine and available to be loaded.  The package tries to dynamically load
          the needed package(s) when it detects census tracts are being mapped and which states
          are involved.  If this process fails, this message will appear.  Usually a manual
          reinstall of the supplemental data package will fix the problem.  Another work
          around is to execute the library function with the name of the package to ensure
          it is loaded prior to calling \var{SeerMapper}.
          
       }
     \item{196}{\bold{The following boundary datasets are missing.  
         Make sure the appropriate SeerMapper supplement packages have been installed and loaded.}
         
         \var{SeerMapper} has determined it requires census tract boundary data for states
         with Seer Registries.  These boundary datasets are contained in the \var{SeerMapperEast} and 
         \var{SeerMapperWest} packages for the 2000 census and in the \var{SeerMapper2010East} and 
         \var{SeerMapper2010West} packages for the 2010 census.  Message 197 lists the 
         missing datasets.
         
         Install these packages and load them via 'library' commands when mapping census tracts for 
         states east and west of the Mississippi that do not contain Seer Registries.
      }
     \item{197}{\bold{Missing: <missingDataSets> }
     
         The boundary datasets (xxx.rda files) listed in <missingDataSets> are needed to map the 
         census tracts found in the call.  This should only occur when doing census tract
         maps in states without Seer Registries.  In this case the \var{SeerMapperEast} and/or 
         \var{SeerMapperWest} (or \var{SeerMapper2010East} and/or var{SeerMapper2010West} ) are be required.  
         Install and load via the 'library' command
         the packages in this list and re-run the mapping request.
      }
     \item{198}{\bold{The following supplemental SeerMapper Census Tract boundary packages are missing and must be installed:}
         
         \var{SeerMapper} requires supplemental census tract boundary packages to map data at the census tract level.
         The package has determined one or more of the three supplemental packages for the census were 
         not installed when the \var{SeerMapperr} package was install.  This should not happen. 
         Each package can be installed on it's own, if this occurs.  There are three census tract boundary 
         packages for each census year (2000 and 2010).  For 2000, the packages are: \var{SeerMapperRegs}, 
         \var{SeerMapperEast} and \var{SeerMapperWest}.  For 2010, the packages are: \var{SeerMapper2010Regs},
         \var{SeerMapper2010East} and \var{SeerMapper2010West}.  If the packages are installed, 
         \var{SeerMapper} will dynamically load the required supplemental package and read in the needed
         census tract boundary datasets.
         
         Install these packages and load them via 'library' commands when mapping census tracts for 
         states east and west of the Mississippi that do not contain Seer Registries.
      }
     \item{199}{\bold{Missing: <missingPackages> }
     
         The message provides a list of the SeerMapper packages with supplemental census tract
         boundary data that must be installed from CRAN to be able to map the census tracts in 
         the data.  The supplemental packages are \var{SeerMapperEast}, \var{SeerMapperWest}, 
         \var{SeerMapper2010East}, and var{SeerMapper2010West}.  
      }
     \item{200}{\bold{After cleaning up <ndfName> data.frame to remove detected errors,  
         there are no rows to process.}
         
         After all of the rows with detected errors were removed from the \var{<ndfName>} data.frame, 
         the data.frame was found to be empty - no rows remaining.  
         Resolve the error documented above and re-run.
      }
     \item{202}{\bold{The <ndfName> data.frame has duplicate location IDs. The duplicate rows will be removed.}
     
         The \var{idCol} column in the \var{<ndfName>} data.frame has rows with duplicate location IDs.  There
         should be only one row of data per sub-area location ID.  The duplicate row is removed.
         Message 204 contains a list of the duplicate IDs removed and their relative row number
         in the data.frame.
      }
     \item{204}{\bold{The duplicate IDs are: <badIDs>}
        
         This message provides a two column list of the duplicate IDs and their relative row numbers that
         were removed.
         \preformatted{
           row#    ID
              2    01
         } 
         Review and correct the contains of the data.frame and re-run.
      }

     \item{220}{\bold{Some of the data rows in the <ndfName> data.frame have location IDs with missing values (NA). These rows will be removed. Correct and rerun.}
     
         The location IDs in the \var{idCol} column of the \var{ndfName} data.frame
         contain some (one or more) missing values (NA).
         These rows cannot be linked to a geographic sub-area and will not be mapped.
         Message number 222 contains a list of the row numbers found to have the 
         invalid location IDs in the \var{<ndfName>} data.frame.  Correct the location IDs in the data.frame and re-run.
      }
     \item{222}{\bold{The following rows will not be mapped: <row numbers>}
     
         The <row numbers> list contains rows in the \var{ndrName} data.frame that cannot be
         mapped.  This could because the location ID is missing, invalid, or incorrect or the 
         data to be categorized is missing (NA) or not a valid number.  The <row numbers> list starts
         with 1 as the first row and increments by 1 for each row.
      }
     \item{250}{\bold{The categ call parameter is missing, empty or contains NAs.  The default value of 5 will be used.}
     
         The \var{categ} call parameter is NULL or NA (missing.)  This is an invalid seting and 
         the parameter is ignored.  The default of \var{categ} <- 5 will be used.
      }
     \item{252}{\bold{The value entered for categ parameter is not valid : <string>.
         The default of categ=5 is used.}
     
         The value entered for the \var{categ} call parameter was a character vector.  It was not 
         set to "DATA" or "COLORS" and could not be converted to a numeric or vector of numerics (break point list.)
         The default value of 5 will be used.
      }
     \item{253}{\bold{The categ parameter provided does not contain any value. 
         The default of categ=5 is used.}
     
         The \var{categ} call parameter was set to "".  The default of 5 will be used.
      }
     \item{256}{\bold{The categ call parameter is not 'DATA', 'COLORS', or a valid 
         single value numeric value.  The default of categ=5 will be used.}
     
         The \var{categ} call parameter is not set to a valid value. The assigned value 
         has a length of 1, so it cannot be a break point vector list.  As a single
         value, it must be set to a number, 'DATA', or 'COLORS'. The default value 
         of 5 is used.  Correct the call parameter and re-run.
      }
     \item{258}{\bold{The categ call parameter has a single value of <string> 
         and must be => 3 as a minimum.  The default of 5 will be used.}
      
         When the \var{categ} call parameter is the number of categories to be 
         calculated, the number of categories must be => 3.  If less then < 3
         the default value of 5 will be used.
      }
     \item{259}{\bold{The categ call parameter value has a single value of <string> and it is > than 
         <catMaxNum>. The categ will be set to <catMaxNum>.}
      
         When the \var{categ} call parameter is a single value of the number
         of categories to calculate the break points.  The maximum value is limited 
         by the \var{palColors} call parameter used.  With the default \var{palColors} of "RdYlBu",
         the maximum value is 10.  The <catMaxNum> value in the message indicates
         the maximum for the active \var{palColors} parameter.
         Only the first <catMaxNum> values in the vector will be used.
         
         Correct the break point vector and rerun.
      }
     \item{260}{\bold{The categ call parameter break point list has a 
         length < 3 items. The default of \var{categ} = 5 will be used.}
      
         When \var{categ} call parameter specifies a break point list (vector), it must have 
         a length equal to or greater than 3.  The break point list cannot be used.  The default
         of \var{categ} = 5 will be used.
      }
      \item{261}{\bold{The categ call parameter break point list has a length > <catMaxNum> items. 
         Only the first <catMaxNum> values will be used.}
      
         When \var{categ} call parameter specifies a break point list (vector) with a 
         length > <catMaxNum>.  The package does not support more then <catMaxNum> categories.
         Only the first <catMaxNum> break points will be used.
      }
     \item{262}{\bold{The categ call parameter contains non-numeric value in
         the break point vector: <string> . The default of categ=5 is used.}
      
         The \var{categ} call parameter is a character vector (<string>) with length > 1.  
         It appears to be a break point vector, but the values could not be 
         converted to a numeric vector.  The default value of 5 will be used.
      }
     \item{263}{\bold{One or more values in the categ breakpoint list is not a number. 
         The default of categ=5 will be used.}
      
         The \var{categ} call parameter appears to be a break point list.  One or more
         of the values in the vector are not numeric.  The break point vector cannot
         be used.  The default value of 5 will be used.
      }
     \item{264}{\bold{The categ call parameter breakpoint vector is not
         in order (low to high). Breakpoint vector has been sorted.}
      
         The \var{categ} call parameter is a break point numeric vector. 
         However, the break point values are not in numerical order from low to high.
         The vector was sorted to compensate.
      }
     \item{265}{\bold{The categ call parameter contains a break point vector with 
         duplicate values.  The duplicate values will be removed.}
      
         When a vector of break point values is provided via the \var{categ} call parameter, 
         each value must be unique and in order from low to high.  If duplicate values are
         found, the duplicate value is removed and the number of break points used reduce by one.
         If the number of break point falls below 3, an additional warning will be generated.
      }
     \item{266}{\bold{The number of points in the categ call parameter 
         list is out of range. It must be between 3 and <catMaxNum>.  
         The default of categ=5 will be used.}
      
         The minimum number of values in a \var{categ} break point vector is 3.  The maximum
         number of values is set by the \var{palColors} value used.  The default maximum is 
         10 when \var{palColors} is equal to "RdYlBu".  See the section on the \var{palColors}
         call parameter.
      }
     \item{267}{\bold{The <dataCol> column data are integer category values.  The range of 
         the values is greater than the maximum of <catMaxNum>. Reduce number of categories 
         or select different palColors value.}
      
         When \var{categ} is set to "DATA", the data column (<dataCol> in the 
         \var{<ndfName>} data.frame is 
         used directly as the category index.  The index should be from 1 to "n", but 
         can be any set of integers. However, the total number of sequential 
         categories from the lowest value to highest value must be <= <catMaxNum> to
         be able to assign colors in the selected RColorBrewer palette to the sub-areas.
         Recommend reducing the number of categories, close up any gaps in the 
         category range, or select a RColorBrewer palette that can provide more colors.
         
         Execution is terminated.
      }
     \item{268}{\bold{The categ call parameter contains non-numeric value in
         the break point vector: <string>. The default of categ=5 is used.}
      
         The \var{categ} call parameter is a break point vector (<string>), but
         contains non-numeric values.  It cannot be used to classify the data.
         The default value of 5 will be used.
      }
     \item{270}{\bold{The categ call parameter is 'DATA'. The <dataCol> column 
         does not appear to contain integer values.}
      
         When \var{categ} is set to "DATA", the values in the data column of the data.frame
         are categories values and must be integers.  The data.frame column is not integer
         values. 
         
         Execution is terminated.
      }
     \item{280}{\bold{The labels parameter must have one entry for each 
         categories. Set to an empty string.}
      
         The \option{labels} legend option allows you to replace the calculated labels 
         in the legend for each category.  There was not one vector entry for each 
         category to be included in the legend.  Correct the \option{labels} option to 
         have one entry for each legend category.  The \option{labels} option is disabled.
      }
     \item{290}{\bold{The following area(s) in the data do not match the list of boundaries:}
     
         When the package compared the location IDs supplied with the data in the \var{<ndfName>} data.frame with 
         the boundary information contained in the package, it was not able to match some of the 
         data rows with the boundary information.   Any data row that does not match the 
         boundary information will not be mapped.  This set of messages document which
         location IDs have been dropped.
      }
     \item{291}{\bold{  > <badIDs>}
     
         The message contains a list (<badIDs>) of  
         location IDs in the data that are not in the boundary dataset.
      }
     \item{292}{\bold{Please check to make sure your data matches the 20<year> census 
         area identifiers and boundaries.}
         
         One of the reasons the data location IDs may not match the boundary information is the wrong 
         census year boundary data is being used.  Check to determine if the data and location IDs
         are build from the 2000 or 2010 U. S. Census data.  Make sure the correct SeerMapper version
         is being used.
         
         Check to see to see which census year the location IDs should match and re-run under the appropriate 
         \var{SeerMapper}:  \var{SeerMapper} for 2000, and \var{SeerMapper2010} for 2010.
         Also check the location IDs in the \var{<ndfName>} data.frame to make sure they are correct.
      }
     \item{295}{\bold{Number of locations found in the Data Table with borders: <number>}
         
         This is an informative message when \option{debug} is \var{TRUE} to indicate the 
         number of rows of data in the supplied data.frame.
      }
     \item{300}{\bold{The following Seer Registry identifiers do not match 
         the abbreviations or aliases:}
         
         Messages 300, 301, and 302 are components of one message on three lines.
         The package has determined Seer Registry level data is being mapped.  That is 
         it is not U. S. Fips codes.  The location ID column does not contain Registry 
         abbreviations or character strings that would match the alias for each registry.
         Message 301 list the location IDs that could not be matched to the boundary information.
         To allow the package to continue running, any row without a valid location ID is 
         ignored and not mapped.
      }
     \item{301}{\bold{   <badIDs>}
     
         See message 300 for details.  <badIDs> is the list of Seer Registry location IDs that 
         do not match the Registry boundary information.
      }
     \item{302}{\bold{   These data rows will be ignored in the mapping.}
     
         See message 300 for details.
      }
     \item{380}{\bold{Internal Error - dataPList contains an NA in list: <idList>}
     
         The dataPList vector of location IDs should not contain a missing value.  
         <idList> is the contains of the dataPList vector.
         The execution was stopped.
      }
     \item{381}{\bold{Internal Error - dataPList does not exist.}
     
         the dataPList vector of Location IDs should not be missing.
         The execution was stopped.
      }
     \item{382}{\bold{Internal Error - regionPList contains a NA in list: <idList>.}
     
         The regionPList of regional IDs should not contain any missing values.
         <idList> is the contains of the regionPList vector.
         The execution was stopped.
      }
     \item{384}{\bold{Internal Error - statePList contains a NA in list: <idList>.}
     
         The statePList vector of Location IDs should not contain any missing values.
         <idList> is the contains of the statePList vector.
         The execution was stopped.
      }
     \item{386}{\bold{Internal Error - seerPList contains a NA in list: <idList>.}
     
         The seerPList vector of Location IDs should not contain any missing values.
         <idList> is the contains of the seerPList vector.
         The execution was stopped.
      }
     \item{387}{\bold{Internal Error - countyPList contains a NA in list: <idList>.}
     
         The countyPList vector of Location IDs should not contain any missing values.
         <idList> is the contains of the countyPList vector.
         The execution was stopped.
      }
     \item{388}{\bold{Internal Error - tractPList contains a NA in list: <idList>.}
     
         The tractPList vector of Location IDs should not contain any missing values.
         <idList> is the contains of the tractPList vector.
         The execution was stopped.
      }
     \item{980}{\bold{ Package: <packageName> failed to load into the system. Verify package 
         was installed and try manually executing a 'library(<packageName>)' command.}
     
         During the mapping of census tract data, \var{SeerMapper} attempted to have
         the system load the boundary datasets for the sub-areas identified in the
         data. The package named <packageName> failed to load.  The can be caused by 
         a corrupted package, package is not installed, the package was not installed in the 
         standard R library, permission prohibit access to the package, or the package has been 
         deleted.  Re-install the package from CRAN.  The execution was stopped.
      }
     \item{990}{\bold{ERRORS found in statistic data.frame checking - Run Terminated.}
     
         During the validation of the first parameter data.frame, errors have been 
         identified and warning messages generated.  The package cannot continue
         processing the mapping request. The execution was stopped.
      }
     \item{991}{\bold{The location ID column could not be found. Run Terminated.}
     
         Previous messages identify the reason the location ID column in the \var{<ndfName>} data.frame
         was not found, was in error, or could not be found.  If the location IDs are held in the
         row names attached to the data.frame, use \var{idCol} = "row.names". 
         The \var{idCol} call parameter can be used to specify the column name or number
         in the \var{<ndfName>} data.frame.  The default name of the location ID column is "FIPS".  
         The execution was stopped.
      }
     \item{992}{\bold{The data column could not be found. Run Terminated.}
     
         Previous messages identify the reason the data column in the \var{<ndfName>} data.frame
         was not found, was in error, or could not be found.  
         The \var{dataCol} call parameter can be used to specify the column name or number
         in the \var{<ndfName>} data.frame.  The default name of the data column is "Rate".  
         The execution was stopped.
      }
     \item{993}{\bold{The data column for the hatching comparison could not be found. 
          Hatching has been disabled.}
     
         Previous messages identify the reason the hatch data column in the \var{<ndfName>} data.frame
         was not found, was in error, or could not be found.  
         The \var{hatch} call parameter and \option{dataCol} option can be used to 
         specify the column name or number in the \var{<ndfName>} data.frame.  
         The default name of the \var{hatch} \option{dataCol} column is "pValue".
         Since the data column cannot be found, hatching is disabled.
      }
   }
 
 End of Messages.
 }
\author{
Jim Pearson \email{jpearson@statnetconsulting.com}\cr 
Maintainer: "Jim Pearson" \email{jpearson@statnetconsulting.com}\cr 
}
\keyword{datasets}

