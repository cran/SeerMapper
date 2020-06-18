\name{SeerMapper}
\alias{SeerMapper}
\title{Quick Data Mapper at State, Health Service Areas, State/County, 
  State/County/Census Tract, or Seer Registry Area Levels}
\description{
Provides a easy and quick means of creating U.S. maps of rates or data at the 
U.S. State, Health Service Areas (HSA), State/County, 
State/County/Census Tract or U.S. Seer Registry 
area levels. Send data is provide at the state level, the outlines of 
all states are overlaid, but only the states with data are categorized 
and colored.  If the data is provided at the HSA, state/county or 
state/county/census tract level, only the states involved are 
outlined and only the registry areas with data are categorized and mapped.  
If the data is provided at the Seer Registry area level, all of the 
states are outlined, but only the Seer Registry areas with data are 
categorized and mapped. Each row in the statistics data.frame 
represents one area (a state, state/county, state/county/census tract,
or Seer registry area.)  Either the U.S. FIPS code, the HSA number
or the Seer Registry area abbreviation is used to tie the data 
with the associated geographic area boundary for mapping. 
The package supports mapping with either the 2000 or 2010 U. S. Census 
Bureau boundaries at the state (district, territory),
HSA, and county levels using a modified version of the census boundary data.
The package also supports mapping of the NCI Seer Registries and/or counties 
or census tracts within the Seer Registries. The \var{SeerMapper} function and 
all of the supporting boundary data and information, even when compressed, 
could not fit within CRAN's 5 megabyte package size limitation.  
Therefore, the package and the boundary information for the 2000 and 2010 
census tracts are distributed as a set six (6) boundary dataset 
data only packages; the \var{SeerMapperRegs}, \var{SeerMapperEast} 
and \var{SeerMapperWest} packages provide the 2000 census tract 
boundaries and the \var{SeerMapper2010Regs}, \var{SeerMapper2010East} 
and \var{SeerMapper2010West} packages provide the 2010 
census tract boundaries. When \var{SeerMapper} is installed, its dependencies 
on the other six (6) packages will automatically installs them.  
When \var{SeerMapper} is called and census tract boundaries are 
required for a specific census year and area, the package ensures 
the appropriate supplement packages are loaded and the 
census tract boundary data and information are available.  
The caller does not have to install or load the six(6) packages.
\var{SeerMapper} handles management of these extra packages. 
All of the boundary data and information is intended for 
mapping purposes and should not be used for locating 
geographical points or area estimating. The boundary data 
has been simplified to speed up the mapping process
and reduce the size of the boundary distributed with the package.  
The purpose of this package is to be fully self-contained and 
provide quick and easy method of generating maps from data.  
The boundary data and information used by the package 
is distributed amoung the packages as follows:
\enumerate{
  \item main package - Code and all state, regional, Seer Registry, HSA, and 
     county boundaries for census years 2000 and 2010.
  \item Census Year 2000 census tracts boundary packages
    \enumerate{
      \item \var{SeerMapperRegs} -the census tract boundaries 
        for 19 states containing Seer Registries (Regs);
      \item \var{SeerMapperEast} -the census tract boundaries
        for 20 states not containing Seer Registries east of the Mississippi river 
        and DC and Puerto Rico;
      \item \var{SeerMapperWest} -the census tract boundaries for 13 states
        not containing Seer Registries west of the Mississippi river;
    }
   \item Census Year 2010 census tracts boundary packages
    \enumerate{
      \item \var{SeerMapper2010Regs} -the census tract boundaries 
        for 19 states containing Seer Registries (Regs);
      \item \var{SeerMapper2010East} -the census tract boundaries
        for 20 states not containing Seer Registries east of the Mississippi river
        and DC and Puerto Rico;
      \item \var{SeerMapper2010West} -the census tract boundaries for 13 states
       not containing Seer Registries west of the Mississippi river;
    }
 }
These packages are automatically installed when \var{SeerMapper} is  
installed and are loaded as required by \var{SeerMapper}.

}
\usage{SeerMapper( ndf,                                  
	censusYear     = NULL,    # default: 2000 census
	proj4          = NULL,    # default: NULL - no transformation, original boundary
	                          #                 projection is used to draw maps.
	idCol          = NULL,    # default: "FIPS"
	dataCol        = NULL,    # default: "Rate"
	categ          = NULL,    # default, "5" 
	mTitle         = NULL,    
	mTitle.cex     = NULL,    # default: 1
	us48Only       = NULL,    # default: TRUE
	includePR      = NULL,    # default: FALSE
	regionB        = NULL,    # default: "NONE" *
	stateB         = NULL,    # default: "NONE" *
	seerB          = NULL,    # default: "NONE" *
	hsaB           = NULL,    # default: "NONE" *
	countyB        = NULL,    # default: "NONE" *
	tractB         = NULL,    # default: "NONE" *
	dataBCol       = NULL,    # default: 'black'
	fillTo         = NULL,    # default: "NONE"
	clipTo         = NULL,    # default: "NONE"
	hatch          = NULL,    # T/F or List of hatching options
	hatch2         = NULL,    # default: NULL (empty)
	mLegend        = NULL,    # T/F or list of legend options.
	brkPtDigits    = NULL,    # default: 2
	palColors      = NULL,    # default: "RdYlBu" w/11 cat. colors
	debug          = NULL     # default: FALSE
	)	
	
}
\arguments{
  \item{ndf}{a data.frame containing identifier, data and pValue 
    for each area to map. The \var{ndf} must contain the 
    \var{id} and \var{data} column providing the geographical link to the 
    boundary data and the data to be classified and colored by the package.
    See the \option{idCol} and \option{dataCol} parameters below for 
    more details.  Based on the type of area identifer used, the package 
    can produces maps at the state, state/county, state/county/census tract, 
    Seer Registry area, state/seer/county or state/seer/county/tract levels.
   }
  \item{censusYear}{is a numeric or character value of the the census year 
    boundary data to use in the validation of the data and mapping of the areas.  
    The value must be \var{2000} or \var{2010}. The default is \var{2000} 
    in the \var{SeerMapper} call.  Using the \var{SeerMapper2010} function
    call, the censusYear call parameter is set to "2010".
   }
  \item{proj4}{is a character vector specifying the proj4 parameters for the 
    projection transformation of the map and hatching prior to drawning. 
    By default the map projection is \cr
    CRS("+proj=aea +lat_1=33 +lat_2=49 +lat_0=39 +lon_0=96W +ellps=WGS84")\cr
    The \option{proj4} parameter would be set as follows:\cr
    proj4 = "+proj=utm +zone=15"
   }
  \item{idCol}{a character vector specifying the name of the column 
    in the \var{ndf} data.frame to use for the rate data to categorize 
    and mapped.  The default value is \var{FIPS}.
   }
  \item{dataCol}{a character vector specifying the name of the column 
    in the \var{ndf} data.frame to use for the rate data to categorize 
    and mapped. The default value is \var{Rate}.  
    If \option{categ} is set to "data", then the \var{dataCol}
    column contains the integer category numbers instead of rates.
   }
  \item{categ}{a chacacter or numeric vector specifying a single numeric value (the 
    number of calculated categories), a numeric vector of multiple values (to be used 
    as the breakpoints in the data categorization), "data", or "colors".
    \preformatted{
         categ=5
           - or - 
         categ=c(0.6, 0.8, 1.0, 1.2, 1.4)
           - or - 
         categ="data" or "colors"
    }
    The default value is \var{5}.  This default allows mapping of any 
    range of data quickly.  If more categories or specific categories 
    are then required, the \option{categ} parameter can be used to adjust 
    the categorization.
    
    When the number of categories is specified as a single value, 
    the number of categories must be an integer and within the 
    range of 3 to 11 and less than or equal the maximum number of 
    categories supported by the color palette selected with 
    the \option{palColors} parameter. For example:  \option{categ} = 5.
    (See the \option{palColors} section for more detail.)
    
    When the actual break points are provided, the vector must have 
    between 3 and the maximum number of categories 
    supported by the color palette MINUS one. 
    For example, if \option{palColors}="Spectral",
    the maximum number of categories is 11.  Therefore, the maximum 
    number of breakpoints that can be provided in the \option{categ} 
    list is 10.
    For example: \option{categ} = c(0.6, 0.8, 1.0, 1.2, 1.4).
    
    If any map has the same color for all areas with data and  
    a breakpoint list is used, check the range of the data and 
    the breakpoint list to determine why all of the area were categorized 
    into the same color.
    
    If multiple maps of data need to be compared, it is recommended, the same 
    breakpoint list be used for all package call to ensure the categories and 
    coloring are the same on each map.
    
    When the \option{categ} parameter is set to \var{data} or \var{colors}, the data
    in the first parameter data.frame is not categorized.\cr  
    When set to \var{data}, the data is treated as the sub-area's category value. 
    The value must be a positive integer value and should in range from 1 to "n". 
    The lowest value does not have to be 1, but the range of values should be < 11 
    to produce effective maps.  The first color is assigned to the lowest value.
    Additional colors are assigned to each additional integer (+1) up to 11 colors.
    In this way the range of categories is used to determine the number of colors 
    required.  The palette specified in \option{palColors} limits the number
    of category values the caller can specify. For example: 
    for \option{palColors}="Spectral" the data value range 
    is limited to 11 values, such as c(1:11) or c(5:16).  
    It is recommended the first category value should always be 1.\cr
    When set to \var{colors}, the data is treated at the color the sub-area will be
    filled with.  The data values are validated to make sure they represent valid
    color names or "#hhhhhh" values.  If a color name is used, it must match a name created
    by the \code{color()} function.  If a color hex value is used, it must in the form of 
    "#rrggbb" or "#rrggbbaa" where each digit is a digits (0-F) for the "r"ed, "g"reen, 
    "b"lue and "a"lpha values of the color. See the \var{rgb} and \var{col2rgb} functions
    for more details.
   }
  \item{mTitle}{A character vector with one or two character strings 
    to be used as the title of the map.  
    For example:  mTitle=c("Major Title Line","minor title line")
   }
  \item{mTitle.cex}{A numeric specifying the size of the title lines.
   }
  \item{us48Only}{a logical value. If \var{TRUE}, only the contiguous 
    48 states and DC are mapped.  Alaska, Hawaii and Puerto Rice are 
    not mapped and any data for sub-areas in these states are ignored.
    This also includes data for Seer Registries in Alaska and/or Hawaii.
    If data for the Hawaii registry is being mapped, make sure to use the 
    default or set \option{us48Only} to \var{FALSE}.
    If \var{FALSE}, All 51 states and DC are mapped.  PR can optionally 
    be included is \option{includePR} is set to \var{TRUE}.
    The default value is \var{FALSE}.
   }
  \item{includePR}{a logical value. If set to \var{TRUE} and 
    \option{US48Only} = \var{FALSE}, Puerto Rico is mapped with the rest 
    of the states and DC. If set to \var{FALSE}, Puerto Rico is not mapped. 
    The default value is \var{FALSE}.
   }
  \item{regionB}{Regional Boundary Option. This option has three values:
    \var{NONE}, \var{DATA}, and \var{ALL}. The default value is \var{NONE}.
    When set to \var{NONE}, no regional boundaries are mapped.\cr
    When set to \var{DATA}, regional boundaries drawn when a region contains
    a state, county or tract with data.\cr
    When set to var{ALL}, all regional boundaries are drawn.\cr
    The regions are the 4 U. S. census regions of NorthEast,
    South, West, and MidWest.
   }
  \item{stateB}{State Boundary Option. This option has four values: 
    \var{NONE}, \var{DATA}, \var{REGION}, and \var{ALL}.  The default value is 
    \var{DATA} with state data and \var{NONE} for all
    other levels of data. 
    When set to \var{ALL}, all state boundaries are mapped.\cr
    When set to \var{REGION}, all states within a region are drawn 
    when the region contains a sub-area with data. \cr 
    When set to \var{DATA}, only the state boundaries are drawn 
    if the state or a sub-area has data values.\cr 
    When set to \var{NONE}, None of the state boundaries are drawn.
   }
  \item{seerB}{Seer Registry Area Boundaries Option. This option has 
    five values: \var{NONE}, \var{DATA}, \var{STATE}, \var{REGION}, and \var{ALL}.  
    The default value is \var{DATA} with Seer Registry data and 
    \var{NONE} for all other levels of data.\cr
    When set to \var{ALL}, all registry boundaries are drawn.\cr  
    When set to \var{REGION}, all registry boundaries are drawn within a 
    region when the region contains a state, registry or sub-area with data.\cr
    When set to \var{STATE}, all registry boundaries within a state are drawn 
    when the state contains sub-areas with data.\cr 
    When set to \var{DATA}, only registry boundaries are drawn if the registry contains
    areas with data values.\cr
    When set to \var{NONE}, no registry boundaries are drawn.  
   }
  \item{hsaB}{Health Service Area Boundary Option. This option has four values: 
    \var{NONE}, \var{DATA}, \var{SEER}, and \var{STATE}.  
    The default value is \var{DATA} with HSA data and \var{NONE} for all 
    other levels of data.\cr 
    When set to \var{STATE}, all HSA boundaries within a state are drawn 
    if the state contains sub-areas with data.\cr 
    When set to \var{SEER}, all HSA boundaries within a Seer Registry are drawn 
    if the state contains sub-areas with data.\cr 
    When set to \var{DATA}, only the HSA boundaries are drawn 
    if the HSA or a sub-area in it has data.\cr 
    When set to \var{NONE}, None of the HSA boundaries are drawn.
   }
  \item{countyB}{County Boundaries Option. This option has five values: \var{NONE}, \var{DATA},  
    \var{HSA}, \var{SEER}, and \var{STATE}.  This option is only valid 
    when county or census tract level data is used.  
    The default value is \var{DATA} with county level data and \var{NONE}
    for all other levels of data.\cr
    When set to \var{NONE}, No county boundaries are drawn.\cr  
    When set to \var{DATA}, Only county boundaries are drawn, if the county has data or 
    contains a tract with data values.\cr
    When set to \var{HSA}, All county boundaries are drawn within a HSA, if  
    the HSA contains any county or tract with data values.\cr  
    When set to \var{SEER}, All county boundaries are drawn within the Seer Registry, if  
    the registry contains any county or tract with data values.\cr  
    When set to \var{STATE}, All county boundaries are drawn within a state, when the state
    contains any county or tract with data.
   }
  \item{tractB}{Census Tract Boundaries Option. This option has six values: \var{NONE}, 
    \var{DATA}, \var{COUNTY}, \var{HSA}, \var{SEER}, and \var{STATE}.  
    This option is only valid with census tract level data.
    The default value is \var{DATA} for tract level data and \var{NONE} for all other 
    levels of data.\cr  
    When set to \var{NONE}, No tract borders are drawn.\cr 
    When set to \var{DATA}, All tract boundaries are drawn for tracts with data values.\cr
    When set to \var{COUNTY}, All tract boundaries are drawn for tracts within a county,
    if the county contains any tract with data values.\cr
    When set to \var{HSA}, All tract boundaries are drawn within a HSA, if  
    the HSA contains any tract with data values.\cr  
    When set to \var{SEER}, All tract boundaries are drawn for tracts within a registry,
    if the registry contains any tract with data values.\cr
    When set to \var{STATE}, All tracts boundaries are drawn for trats within a state,
    if the state contains any tract with data values
   }
  \item{fillTo}{This parameter has been replaced by the full implementation of the 
    \option{stateB}, \option{seerB}, \option{countyB}, and \option{tractB} call
    parameters.  Parameter is obsolete.
   }                
  \item{clipTo}{a character or logical value.  This parameter controls how the 
    map will be drawn in the in the grapics area and how the spatial box 
    is calculated. 
    The values for \var{clipTo} are:  TRUE, FALSE, "NONE", "DATA", "SEER", 
    or "STATE".  
    If \var{clipTo} is set to FALSE or "NONE", the graphics box is set to cover 
    all of the requested boundaries.  So, if all of the state boundaries were requested 
    and only one state has data, the entire U.S. will be mapped.
    If \var{clipTo} is set to TRUE or "DATA", 
    the data sub-areas as would
    occur is all boundary controls were set to "NONE" and the level of the data set to "DATA". 
    (e.g., for county data, stateB="NONE", seerB="NONE", and countyB="DATA".  
    If additional boundaries are requested the extend beyond the data sub-areas, 
    they will be clipped at the graphic edges.
    If \var{clipTo} is TRUE or "DATA", the spatial box is set to cover then the 
    graphics is clipped to the spatial box that contains the data areas being mapped. 
    Any boundaries extending beyond the data area would be clipped at the edge 
    of the graphic space.  
    If \var{clipTo} is "SEER" and the data is in Seer Registries, then the boundary 
    of the Registries is used as the graphics box.  If the \var{clipTo} is "STATE", 
    then the state or states containing
    the data sub-areas are used as the graphics box for the mapping.  
   }                
  \item{dataBCol}{is a character vector of one element.  It is used to specify the 
    color of the data level boundary on the maps.  The default value is 'black'.
    The value must be a color name from the \code{colors()} name list or a string
    starting with a "#" with 6 or 8 hexidecimal digits representing the RGB and transparancy
    values of a color.  If the color specified is not valid, the function will 
    generate a warning and stop.
   }                
  \item{mLegend}{is a list of legend options.  It must a list containing named items.
    The mLegend can be disabled by setting \option{mLegend} = \var{NA} is the call.
    The legend is placed on the bottom of the map. The following options are
    available to fine tune the map's legend: \option{counts}, 
    \option{size}, \option{numberColumns}, \option{pos}, and \option{noValue}. 
    See below for more details on each option.
    The options are specified in the call by setting 
    \code{mLegend = list(counts=TRUE, size=0.6)}, for example.
   \describe{
     \item{numCols}{is the number of columns to use when creating a legend.  
       The default is \var{3}.  The number of columns can range from \var{1} to \var{8}.
       (Old name: \var{legendColn} and \var{ncol})
      }
     \item{size}{is a numeric value used to size the legend symbols and texts as required.  
       The default value is 0.85. (Old name: legendCex)
      }
     \item{pos}{is a character vector specifying the location of the legend along the bottom 
       of the map. The acceptable values are \var{left}, \var{center}, and \var{right}.  
       The default is \var{left}. (Old name: legendPos)
      }
     \item{counts}{is a logical variable - \var{TRUE} or \var{FALSE}.  If \var{TRUE}, 
       the observation counts for each category are displayed in the legend 
       after the description text. The default is \var{FALSE}. (Old name: \var{legendCnt})
      }
     \item{noValue}{is a logical variable - \var{TRUE} or \var{FALSE}.  
       If \var{TRUE}, any category containing no observations (data) is tagged 
       in the legend with "NV" after the category label. 
       The default is \var{FALSE}.
      } 
    }
   }
  \item{hatch}{is a logical value of \var{TRUE} or \var{FALSE} or a list structure 
    with hatching settings. The call parameter enabled and provided parameters 
    for hatching areas (state, Seer Registry, county or census tracts) based 
    on data and a criteria.  The most common use of hatching is to indicate the 
    reliability of the data via a P-Value. In this case, the range of the data 
    is 0 to 1, the comparison value is 0.05 and the criteria is greater than to 
    hatch an area. If the \option{hatch} parameter is set to \var{TRUE}, 
    then hatching will be implemented using the standard settings 
    for hatching. The standard settings are: \option{range} = 0 to 1, \option{value} = 0.5,
    \option{ops} = \var{gt}, and \option{dataCol} = \var{pValue}.  If set to \var{FALSE}, 
    no hatching is done. If the \option{hatch} call parmeter is set to a list, 
    hatching is enabled and the list is inspected for overriding values for the default setting.
    If two hatching patterns are required, the hatch2 call parameter can be used
    to specify a second data column, operator and test valid.  (see below).
    
    The following hatch settings may be changed using the \option{hatch} call parameter 
    to pass a list of items and new values.  The acceptable set of items are:
   \describe{
     \item{dataCol}{a character vector specifying the name of the column in the 
       \var{ndf} data.frame to use to test the P_Value for each area.  
       The value in this column used to test if the P_Value is < \var{0.05}.  
       If the value is not < \var{0.05}, then the associated area is hatched as an indicator. 
       The default column name value is \var{pValue}.   
       Please note the same variable name is used as the statistical data, but this variable
       is used for the hatching feature.
      }
     \item{ops}{a character value. The values may be one of the 
       following: "eq", "ne", "lt", "le", "gt", or "ge". These translate to "==", "!=", "<",
       "<=", ">", and ">=" operations.  Other notations are allowed and translated
       appropriately: "<>", "=>", "=", and "=<".  The comparison is the data in the 
       \var{dataCol} column to the \option{hatch} \var{value} option using the \var{ops} 
       operation.  
       
       The formula is \var{dataCol} \var{ops} \var{value}.  
       
       For example, for P-Value testing the test is  \var{data} > \var{0.05}.  
       A \var{TRUE} result will cause the area to be hatched.
      }
     \item{value}{a numeric break point for comparison with the hatching data column (pValue). 
       Default is 0.05.
      }
     \item{range}{a vector of two values, min and max inclusively. Range checking of 
       is normally disabled, but can be used to validate the data used to determine
       if hatching should be done for an area.  \var{range} can be set to:  \var{NA}, 
       \var{FALSE} to disable range checking,  \var{TRUE} to enable range check for var{c(0,1),} 
       or to a range checking vector of a low and high value, c(1234,532).  
       If the low and high value of the range is out of order, it will be sorted and a 
       warning message generated.  If the range of the hatching data is not know, it is 
       recommended to disable the range checking.  If the data is pValue data, then range=TRUE
       will ensure all of the data is valid.
      }
     \item{lab}{a character vector containing a label for this hatching activity.
       The default is "hatch#1".
      }
     \item{col}{a character vector containing the name or \code{#A8A8A8} value 
       of the color to be used for the area hatching pattern.  
       The default value is \code{grey(0.66)} or \code{#A8A8A8}.
      }
     \item{lwd}{a numeric value.  The line weight for the hatching lines. 
       Default value is \var{0.5}.
      }
     \item{density}{a numeric value. \var{density} or \var{den} can also be used as the option name. 
       When drawing the hatching over the area, this option specifies the density of the 
       hatching pattern in lines per inch.  The valid range is from 5 to 64 lines per inch.
       The default value is \var{25}.
      }
     \item{angle}{a numeric value.  When drawing the hatching over the area, this option 
       specifies the angle of the hatching pattern in degrees counter clockwise. 
       The default value is 45 degree.  The value must be between 360 and -360 degrees.
      }
     \item{incAngle}{a numeric value.  When drawing multiple hatching lines, this 
       option specifies the increment the angle will be increased for subsequent hatching.
       The default value is 60 degree.  The value must be between 360 and -360 degrees.
      }
    }
   }
  \item{hatch2}{is used to specify the options for a second hatching overlay on 
    the maps.  If the \option{hatch2} parameter is set a list of options, then the 
    hatching is enabled.  This list only contains the location of the data (\option{dataCol}), 
    the operator (\option{ops}), the test value (\option{value}) and the range test (\option{range}) is needed.
    All other hatching options are taken from the \option{hatch} defaults or option 
    values.
    
    The most common use of hatching is to indicate the 
    reliability of the data via a P-Value. The \option{hatch2} parameter uses
    the same defaults as \option{hatch}.
    The standard options and settings are: \option{range} = 0 to 1, \option{value} = 0.5,
    and \option{ops} = \var{gt}. The \option{dataCol} must be defined to enable
    \option{hatch2}. 
        
    The acceptable set of options are:
   \describe{
     \item{dataCol}{a character vector specifying the name of the column in the 
       \var{ndf} data.frame containing the values for each sub-area to use 
       in the hatching test.  This option must be specified to enable the second hatching feature.  
       The default column name value is NJLL.   
      }
     \item{ops}{a character value. The value may be one of the 
       following: "eq", "ne", "lt", "le", "gt", or "ge". These translate to "==", "!=", "<",
       "<=", ">", and ">=" operations.  Other notations are allowed and translated
       appropriately: "<>", "=>", "=", and "=<".  The comparison is the data pointed to by  
       \var{dataCol} column name using the \option{ops} operater to the \option{value} test value.
       
       The formula is \option{dataCol} \option{ops} \option{value}.  
       
       For example, for P-Value testing the test is  \var{data} > \var{0.05}.  
       A \var{TRUE} result will cause the area to be hatched.
      }
     \item{value}{a numeric test value for comparison with the hatch2 data column (dataCol) provided
       by the caller.  Default is 0.05.
      }
     \item{range}{a vector of two values, min and max inclusively. Range checking of 
       is normally disabled, but can be used to validate the hatch2 data for the 
       sub-areas.  \var{range} can be set to:  \var{NA} or \var{FALSE} to disable range checking,
       \var{TRUE} to enable range check for var{c(0,1),} 
       or to a range checking vector of a low and high value, c(1234,532).  
       If the low and high value of the range is out of order, they will be sorted and a 
       warning message generated.  If the range of the hatching data is not know, it is 
       recommended to disable the range checking. 
      }
     \item{lab}{a character vector containing a label for this hatching activity.
       The default is "hatch#2".
      }
    }
   }
  \item{brkPtDigits}{specifies the number of digits to the right of the decimal place when 
    calculating or pre-processing the break point list and resulting categorization intervals.  
    Default is the calculated number of digits based on the interval between break points. 
    For example: if \option{brkPtDigits} = \var{1}, the breakpoint rounded and limited to 
    only having one digit. The valid range is 1 to 4. The default value is \var{2}.
    This parameter is only active when the \option{categ} call parameter specifies 
    the number of categories.  
   }
  \item{palColors}{a character string specifying the \var{RColorBrewer} color palette 
    used to generate the colors to map the data categorization.  The specified palette 
    is check for validity and the maximum number of colors for the palette is used 
    to limit the number of categories the user can specify in \var{categ} parameter.  
    See the \var{RColorBrewer} help for a list of 
    the acceptable color palettes.  This call parameter is only active when the 
    \option{categ} is NOT set to "COLORS".
    
    The permitted color palettes and maximum number of colors (categories) supported are:
    \tabular{lll}{
      Type of Palette \tab Palette Name \tab Maximum \cr
      Sequential  \tab Blues   \tab 9 \cr
      Sequential  \tab BuGn    \tab 9 \cr
      Sequential  \tab BuPu    \tab 9 \cr
      Sequential  \tab GnBu    \tab 9 \cr
      Sequential  \tab Greens  \tab 9 \cr
      Sequential  \tab Greys   \tab 9 \cr
      Sequential  \tab Oranges \tab 9 \cr
      Sequential  \tab OrRd    \tab 9 \cr
      Sequential  \tab PuBu    \tab 9 \cr
      Sequential  \tab PuBuGn  \tab 9 \cr
      Sequential  \tab PuRd    \tab 9 \cr
      Sequential  \tab Purples \tab 9 \cr
      Sequential  \tab RdPu    \tab 9 \cr
      Sequential  \tab Reds    \tab 9 \cr
      Sequential  \tab YlGn    \tab 9 \cr
      Sequential  \tab YlGnBu  \tab 9 \cr
      Sequential  \tab YlOrBr  \tab 9 \cr
      Sequential  \tab YlOrRd  \tab 9 \cr
      Diverging   \tab BrBG    \tab 11\cr
      Diverging   \tab PiYG    \tab 11\cr
      Diverging   \tab PRGn    \tab 11\cr
      Diverging   \tab PuOr    \tab 11\cr
      Diverging   \tab RdBu    \tab 11\cr
      Diverging   \tab RdGy    \tab 11\cr
      Diverging   \tab RdYlBu  \tab 11\cr
      Diverging   \tab RdYlGn  \tab 11\cr
      Diverging   \tab Spectral\tab 11\cr
      Qualitative \tab Accent  \tab 8 \cr
      Qualitative \tab Dark2   \tab 8 \cr
      Qualitative \tab Paired  \tab 12\cr
      Qualitative \tab Pastel1 \tab 9 \cr
      Qualitative \tab Pastel2 \tab 8 \cr
      Qualitative \tab Set1    \tab 9 \cr
      Qualitative \tab Set2    \tab 8 \cr
      Qualitative \tab Set3    \tab 12\cr
    }
    
    All palettes have minimum number of colors (categories) of 3.
    For more details on each palette and their characteristics 
    visit the http://www.colorbrewer.org website.
    The actual parameter testing of the palette names and maximum number 
    of colors for each palettes is based on the brewer.pal.info 
    data.frame provided by RColorBrewer package.
    It is strongly recommended the users stay with color schemes in the 
    diverging group.
   }
  \item{debug}{is a logical variable.  When set to \var{TRUE}, debug prints of 
    variables and the flow of the package are enabled.  Use of this option adds 
    a large number of debug lines to the package standard device output.
   }
 }

\details{
 The \var{SeerMapper} package (and the \var{SeerMapper2000} and \var{SeerMapper2010} 
 functions) provide a simple 
 means of mapping data for the U. S. at several levels:  State,  State/County,  
 State/County/Census Tract, and U. S. Seer Registry areas. The package automatically
 determines the mapping level based on the data and the area identifiers provide at the time
 of the call.  The data is categorised and each area is drawn and colored appropriately
 based on its data value.
 
 The \var{censusYear} call parameter controls which census year name tables and 
 boundary data will be used for the mapping.  The \var{SeerMapper2000} function
 overrides the \var{censusYear} parameter and sets it to "2000" for the 2000 U. S. census.
 The \var{SeerMapper2010} function sets the \var{censusYear} parameter to "2010" for
 mapping with the 2010 U. S. location FIPS codes.
 The default value for the \var{censusYear} parameter is "2000", so calling 
 \var{SeerMapper} without the \var{censusYear} parameter set, is equivalently the same
 as calling \var{SeerMapper2000}.
 
 
 The level of the map is determined by the identifier used in the data provided when 
 the function is called.  
 
 If the data uses the U. S. State fips code (2 digits) as the spatial identifer, then 
 the state boundaries are drawn and colored.  In this mode, the caller has two options: 
 draw all of the states in the US (or continental 48 state), drawn only the states with 
 data.  This is controled by the \option{stateB} call parameter.  It's default 
 value for state level data is \var{ALL}.  To only draw boundaries for states with data, 
 specify \option{stateB} = \var{DATA}.  Seer Registry, county and census tract boundaries
 will not be drawn.
 
 If the data uses the U. S. State and County fips code (5 digits) as the spatial identifier,
 then the state and county boundaries are drawn and colored for areas with data.  
 In this mode, there are several options:

 a) Draw only the boundaries for the counties with data ( \option{countyB} = \var{DATA} ),  
 b) Draw the counties with data and their state's boundaries (states with data) 
 ( \option{stateB} = \var{DATA} ),  
 c) Draw the boundaries for all of the counties in the states where data was provided for 
 any county in the state (state with data and counties within the state) 
 ( \option{stateB} = \var{DATA} and \option{countyB} = \var{STATE} ),
 d) Draw the bountaries of all of the counties within a Seer Registry where data was
 provided for any county in the Seer Registry (Seer Registry with data) 
 ( \option{countyB} = \var{SEER} ),
 
 If the data uses the U. S. State, County and Census Tract fips code (11 digits) as the spatial 
 identifier, then the census tracts boundaries are drawn and colored for areas with data.
 In this mode, there are several options:

 a) Draw only the boundaries for the census tracts with data 
 ( \option{tractB} = \var{DATA} default setting),  
 b) Draw the tracts with data and their state's boundaries (states with data) 
 ( \option{stateB} = \var{DATA} ),  
 c) Draw the boundaries for all of the tracts in the states where data was provided for 
 any tract in the state (state with data and tracts within the state) 
 ( \option{stateB} = \var{DATA} and \option{tractB} = \var{STATE} ),
 d) Draw the bountaries of all of the tracts within a Seer Registry where data was
 provided for any county in the Seer Registry (Seer Registry with data) 
 ( \option{tractB} = \var{SEER} ),
 
 If the spatial identifier does not match one of the above patterns, the package attempts to 
 match the identifier against the list of Seer Registry abbreviations and an partial match 
 against keywords for the Seer Registries.  If there is no match, an error message is generated
 to inform the caller.  When Seer Registry data is used, the package maps each Seer Registry 
 based on the data presented. 
  
 In this mode, there are several options:
  
 a) Draw only the boundaries for the Seer Registries with data without any state boundaries 
 ( \option{seerB} = \var{DATA} and \option{stateB} = \var{NONE}, the default setting ),
 b) Draw only the boundaries for the Seer Registries with data and their associated
 state boundaries ( \option{seerB} = \var{DATA} and \option{stateB} = \var{DATA} ),
 c) Draw only the boundaries for the Seer Registries with data and all of the U.S. state
 boundaries ( \option{seerB} = \var{DATA} and \option{stateB} = \var{ALL} ),
 d) Draw all of the Seer Registries boundaries within a state with data and only the 
 state boundaries with data ( \option{seerB} = \var{STATE} and \option{stateB} = \var{DATA} ).
 
 For each level of data, there are more options related to how and when the state, county, 
 census tract and Seer Registry are drawn.  See the descriptions for the \option{stateB}, 
 \option{seerB}, \option{countyB}, and \option{tractB} call parameters.

 The fips codes used to idenfity an area, but all be the same in the 
 data; for states data, the 2 digit code, for state/countiesdata, 
 the 5 digit code consisting of the 2 digit state and 3 digit 
 county codes, for state/county/tract data, the 11 digit fips code 
 consisting of the 2 digit state code, 3 digit county code, and the 
 6 digit census tract code.
 
 For Seer Registry mapping, the identifier must be the Seer Registry
 abbreviate or a character string the uniquely partically matches one 
 and only one alias string for the Seer Registry.  The acceptable
 abbreviations and partial matches are:
 \tabular{lll}{
    Seer Registry Abbr\tab Seer Registry Alias\tab Description\cr
    AK-NAT\tab ALASKA         \tab Alaska Native Registry\cr
    AZ-NAT\tab ARIZONA        \tab Arizona Native Registry\cr
    CA-LA \tab LOS ANGELES    \tab Los Angeles CA Registry\cr
    CA-OTH\tab CALIFORNIA EXCL\tab California Registry all county not in other CA registries\cr
    CA-OTH\tab GREATER CALIF  \tab See Above\cr
    CA-SF \tab SAN FRAN       \tab California San Fran/Oakland\cr
    CA-SJ \tab SAN JOSE       \tab California San Jose/Monteray\cr
    CT    \tab CONNECTICUT    \tab State of CT Registry\cr   
    GA-ATL\tab ATLANTA        \tab Atlanta GA metro\cr
    GA-OTH\tab GEORGIA OTHER  \tab All counties no in the Altanta or Rural registries\cr
    GA-OTH\tab GREATER GEOR   \tab See above\cr
    GA-RUR\tab RURAL GEORG    \tab Rural GA counties Registry\cr
    HI    \tab HAWAII         \tab State of HI Registry\cr
    IA    \tab IOWA           \tab State of Iowa Registry\cr
    KY    \tab KENTUCKY       \tab State of Kentucky Registry\cr
    LA    \tab LOUISIANA      \tab State of Louisiana Registry\cr
    MI-DET\tab DETROIT        \tab Detroit Metro Registry\cr
    NJ    \tab NEW JERSEY     \tab State of NJ Registry\cr
    NM    \tab NEW MEXICO     \tab State of NM Registry\cr
    OK-CHE\tab CHEROKEE       \tab Oklahoma Cherokee Nationn Registry\cr
    UT    \tab UTAH           \tab State of Utah Registry\cr
    WA-SEA\tab PUGET          \tab Seattle/Puget Sound Registry\cr
    WA-SEA\tab SEATTLE        \tab See Above\cr
 }
 To be able to handle data from different sources, there may be 
 multiple alias lines in the above table.
 
 The package depends on the following other third party R packages for 
 functionality: graphics,  stringr, RColorBrewer, stats, sp, maptools, and rgdal.
} 
\value{None}

\examples{

#####
#
#  Generate data.frame of all data for the US States and DC
#

data(USStates_CM_St_Data,envir=environment())
str(USStates_CM_St_Data)

#
#####

#####
#
#  Example # s01 - States Mapping Basic defaults
#
#  In this example rate and pValue data is provided for each state.  
#  The number of categories requested is set to 5.  
#  The "pValue" column in the data is used to hatch states when 
#  the pValue > 0.05.  The defaults on the hatch feature are tuned
#  support hatching of pValue data that is not-significate. 
#
#  Border defaults for state data are = stateB=ALL, seerB=NONE.
#
#  This example uses example s1's data.  All state boundaries are drawn.
#

TT   <- c("Ex-s01 States Mapping Cancer Mortality Rate",
            "all defaults")
save.f<-tempfile(pattern = "", fileext = "SM-Ex-s01 States Map of Cancer Mortality-defaults.pdf")
pdf(save.f,width=8,height=10.5)
SeerMapper(USStates_CM_St_Data,mTitle=TT)
dev.off()

#
#  The pValue data in the dataset was assigned 0.02 or 0.2 based on a
#  comparison the state's confidence interval values and the US's rate, 
#  for age adjusted rates for all cancers and cancer deaths for the years
#  2009 to 2013.
#
####

#####
#
#  Example # s02 - States Mapping Basic with Hatching
#
#  In this example rate and pValue data is provided for each state.  
#  The number of categories requested is set to 5.  
#  The "pValue" column in the data is used to hatch states when 
#  the pValue > 0.05.  The defaults on the hatch feature are tuned
#  support hatching of pValue data that is not-significate. 
#
#  Border defaults for state data are = stateB=ALL, seerB=NONE.
#
#  This example uses example s1's data.  All state boundaries are drawn.
#

TT   <- c("Ex-s02 States Mapping Cancer Mortality Rate",
            "w hatching and defaults")
save.f<-tempfile(pattern = "", fileext = "SM-Ex-s02 States Map of Cancer Mortality-w hatching.pdf")
pdf(save.f,width=8,height=10.5)

SeerMapper(USStates_CM_St_Data,
           hatch   = TRUE,
           mTitle  = TT
        )
        
dev.off()

#
#  The pValue data in the dataset was assigned 0.02 or 0.2 based on a
#  comparison the state's confidence interval values and the US's rate, 
#  for age adjusted rates for all cancers and cancer deaths for the years
#  2009 to 2013.
#
####

####
#
#  Generate Partial States data.frame for Examples s04 through s15
#
#  This dataset is created one and re-used.  If the examples
#  are unindependly, the code to generate the dataset must
#  be run first.
#

data(USStates_CM_St_Data,envir=environment())

USStates_P   <- USStates_CM_St_Data         # get copy
numStates    <- dim(USStates_P)[1]          # get number of rows (states)
selectStates <- (runif(numStates) <= 0.75 ) # select random 75% of states
USStates_P   <- USStates_P[selectStates,]   # pull out data

#
#####

#####
#
#  Example # s03 - Partial State Mapping with pValue Hatching
#
#  The package does not have to have data for every state/DC.  Partial
#  data can also be mapped.  States without data are not colored (white).
#
#  This example uses a randomly selected set data for 75 % of the states/DC.  
#
#  The number of categories is set to 5 (categ=5), and hatching 
#  is enabled using the default hatching options on the data column "pValue".
#
#  By default for state level data, the boundaries for all U.S. states/DC are
#  drawn to provide a complete map (stateB="ALL").
#

TT   <- c("Ex-s03 Partial States Map",
          "all defaults with hatching")
save.f<-tempfile(pattern = "", fileext = "SM-Ex-s03 Partial States Map w hatching defaults.pdf")
pdf(save.f,width=8,height=10.5)

SeerMapper(USStates_P,
                hatch   = list(dataCol="pValue"),       # test pValue column for < 0.05 
                mTitle  = TT
             )

dev.off()

#
####

#####
#
#  Example # s04 - Partial State Mapping with pValue Hatching 
#         and boundaries for all states and seer registries.
#
#  If stateB="ALL" and seerB="ALL", then boundaries for all of the states and Seer 
#  Registries are drawn.  This is one solution for the map generated in example # s06.
#

TT   <- c("Ex-s04 Partial State Map w hatching",
          "Outline all States and Regs")
save.f<-tempfile(pattern = "", fileext = "SM-Ex-s04 Partial States Map w hatching stB-A srB-A.pdf")
pdf(save.f,width=8,height=10.5)

SeerMapper(USStates_P,
                stateB  = "ALL",             # outline all states
                seerB   = "ALL",             # outline all registries
                hatch   = TRUE,              # test pValue column for < 0.05 
                mTitle  = TT
              )
dev.off()

#####
#
#  Example # s05 - Partial States Mapping with pValue Hatching 
#         No state boundaries, but boundaries for all Registries
#           stateB=NONE and seerB=ALL
#

TT   <- c("Ex-s05 Partial States Map w hatching, cat=7",
          "No State boundaries, All Regs, w/column names")

ex.f<-"SM-Ex-s05 Partial States Map w hatching cat-7 stB-N srB-A w column names.pdf"
save.f<-tempfile(pattern = "", fileext = ex.f)
pdf(save.f, width=8, height=10.5)

SeerMapper(USStates_P,
           idCol   = "FIPS",dataCol="AA_Rate",
           stateB  = "NONE",        # no state outlines
           seerB   = "ALL",         # all registries
           categ   = 7,             # number of categories to generate and use.
           hatch   = TRUE,          # test pValue column for < 0.05 
           mTitle  = TT
)
dev.off()

#
####

######
#
#  Generate Seer Regs data.frame for 17 of the 20 registries and 
#    a smaller Seer Regs data.frame for the original 12 registries
#    All of the registry data.frames serve as partial data sets.
#    The 12 registry data.frame shows the features the best.
#
#  The following script creates the dataset for use in examples sr30-sr41.
#  Since it is not re-created in the code for each examples, this code
#  must be run or copied to the example as needed.
#

data(SeerRegs_CM_Data,envir=environment())
str(SeerRegs_CM_Data)

# Get US rate for "All_Both" sexes and races.
USRate <- SeerRegs_CM_Data[2,"All_Both"]
cat("USRate:",USRate,"\n")

# strip off first to rows as required
SeerRegs_CM_Data <- SeerRegs_CM_Data[c(-1,-2),]   
# this gets ride of Seer Reg and U.S data.

#  Select data for the original Seer 13 Registries without Alaska.
srList <- c("CT", "MI-DET", "GA-ATL", "GA-RUR", 
            "CA-SF", "CA-SJ", "CA-LA", "HI", "IA", 
            "NM", "WA-SEA", "UT")
SeerRegs_CM_Data_12 <- SeerRegs_CM_Data[srList,]

#

#####
#
#  Example # sr10 - Seer Registry 12 Mapping
#
#  Of the 21 NCI Seer Registries, most mapping occurs using the 
#  12 primary registries. They include: Connecticut, Detroit, 
#  Atlanta, Rural Georgia, Hawaii, Iowa, Utah, New Mexico,
#  Greater California, Greater Georgia, New Jersey, 
#  Kentucky, Louisiana, San Francisco/Oakland, San Jose/Monterey, 
#  Los Angeles, and Seattle-Puget Sound.  
#  The default stateB and seerB call parameter values for 
#  for Seer Registry data are:  stateB="NONE" and seerB="DATA".  
#  The countyB and tractB parameters are ignored.
#
#  This example drawn boundaries for Seer Registries with data,
#  but does not include any state boundaries.  The registries
#  jsut float.  This is useful when you are mapping a few
#  Seer Registries, like Georgia Rural and AtLanta Metro.
#
TT   <- c("Ex-sr10 Seer Reg 12 Map-Cancer Mort. Rates All Both",
          "cat=6, def: stateB-NONE, seerB-DATA" )

ex.f<-"SM-Ex-sr10 Seer Reg 12 Map cat-6 stB-N srB-D.pdf"
save.f<-tempfile(pattern = "", fileext = ex.f)
pdf(save.f, width=8, height=10.5)

SeerMapper(SeerRegs_CM_Data_12,
           idCol   ="Registry",dataCol="All_Both",
           categ   =6,
           mTitle  =TT
)
dev.off()

#
####

#####
#
#  Example # sr11 - Seer Reg All States Map w Hatching
#                 stateB=DATA   seerB=DATA 
#
#  If stateB = "DATA", the boundaries for the states/DC are drawn that 
#  contain Seer Registries with data.
#  This provides the state outlines around Registries with data.
#  Since only a few of the Seer Registries have data, nott all of the 
#  state boundaries are drawn.  A partial U.S. map appears.
#

TT   <- c("Ex-sr11 Seer Reg 12 Map Seer wD and States wD", 
          "stateB=DATA, seerB=DATA")

ex.f<-"SM-Ex-sr11 Seer Reg 12 Map-stB-D, srB-D.pdf"
save.f<-tempfile(pattern = "", fileext = ex.f)
pdf(save.f, width=8, height=10.5)

SeerMapper(SeerRegs_CM_Data_12,
           idCol   = "saID",dataCol="All_Both",
           stateB  = "DATA",  # drawn boundaries for all states.
           mTitle  = TT
)
dev.off()


#####
#
#  Example # sr12 - Seer Reg All States Map w Hatching
#                    stateB=ALL   seerB=ALL 
#
#  With stateB = "ALL", the boundaries for all of the states/DC are drawn.
#  This provides a full U.S. map.  To add the boundaries for all of the 
#  Seer Registries, seerB = "ALL" is set.  
#

TT   <- c("Ex-sr12 Seer Reg 12 Map Seer-A and States-A", 
          "stateB=ALL, seerB=ALL")
ex.f<-"SM-Ex-sr12 Seer Reg 12 Map-stB-A, srB-A.pdf"
save.f<-tempfile(pattern = "", fileext = ex.f)
pdf(save.f, width=8, height=10.5)

SeerMapper(SeerRegs_CM_Data_12,
           idCol   = "saID",dataCol="All_Both",  # specify the column names.
           stateB  = "ALL",  # drawn boundaries for all states.
           seerB   = "ALL",
           mTitle  = TT
)
dev.off()

# #####
# The below examples are uncommented due to the example running time limit on CRAN
# #####
# #
# #  Example # sr20 - Seer Registry West - Level Mapping
# #
# #  With seerB="DATA", the package only maps Seer Registries with data.  
# #  To include the state boundaries, it's best to use stateB="DATA".
# #  Otherwise, the entire US is mapped.
# #
# #  This example maps the western area Seer Registries in California,
# #  New Mexico, Utah and Washington-Seattle/Puget sound.
# #
# 
# TT   <- c("Ex-sr20 Seer Registry Area West Males",
#           "stateB=DATA, seerB=DATA")
# 
# pdf("SM-Ex-sr20 Seer Regs West Males stB-D srB-D.pdf",
#     width=8, height=10.5)
# 
# SeerMapper(SeerRegs_CM_Data_West,
#            idCol   = "Registry",dataCol="All_Males",
#            stateB  ="DATA",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Example # sr21 - Seer Registry West - Level Mapping
# #
# #  To drawn the state boundaries in the region, but not the 
# #  entire U.S., stateB can be set to "REGION".  The package
# #  is aware of the 4 U. S. Census regions.  The "REGION" option 
# #  is available with the stateB and seerB boundary controls.
# #
# #  This example maps the western area Seer Registries in California,
# #  New Mexico, Utah and Washington-Seattle/Puget sound with state boundaries
# #  drawn to the western region boundary.
# #
# 
# TT   <- c("Ex-sr21 Seer Registry Area West Males",
#           "w hatching, stateB=REGION, seerB=DATA")
# 
# pdf("SM-Ex-sr21 Seer Regs West Males stB-R srB-D.pdf",
#     width=8, height=10.5)
# 
# SeerMapper(SeerRegs_CM_Data_West,
#            idCol   = "Registry",dataCol="All_Males",
#            stateB  ="REGION",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# ####
# #
# #  The next set of examples show mapping of county data in many different situations.
# #  The key controls of the border drawing are the stateB, seerB, and countyB call
# #  parameters.  The default values are DATA.  This says, draw the area border only
# #  if there is data provided within the area.  This applies to Seer Registry data, 
# #  county data, and census tract data.  If the call parameter is set to "ALL", the 
# #  associated border is ALWAYS drawn.   If the call parameter is set to "NONE", the 
# #  associated border is almost always not drawn.  See notes below.
# #
# #  The caller has one additional control over when area borders are drawn: the fillTo
# #  call parameter.  Additional borders may be drawn for higher level areas as needed.
# #  If the fillTo call parameter is set to "NONE", only sub-areas (county or tract level)
# #  with data are colored and their borders drawn. This is the default, in most cases.  
# #  If fillTo is set to "SEER", when a Seer Registry area contain any sub-area with data, 
# #  all of the borders at that level are drawn within the Seer Registry area. But not
# #  within the state or neighboring Seer Registry areas.
# #  If fillTo is set to "STATE", when a state contains any sub-area (Seer Registry, county,
# #  tract level) with data, all borders at that level are drawn within the state. 
# #
# ####
# 
# #####
# #
# #  Create a data.frame for All and Partial Kentucky Counties. 
# #
# 
# data(Kentucky_CM_Co_Data,envir=environment())
# str(Kentucky_CM_Co_Data)
# 
# KY_Co_DF <- Kentucky_CM_Co_Data         # start with the fill set of counties.
# 
# lKY      <- dim(KY_Co_DF)[1]            # get number of counties
# selKY    <- (runif(lKY) <= 0.75 )       # select random 75% of counties
# KY_Co_P  <- KY_Co_DF[selKY,]
# 
# #
# #####
# 
# #####
# #
# #  Example # c30 Kentucky All Co Map w hatching, 
# #         default - countyB="DATA", seerB="NONE", stateB="NONE"
# #
# #  In this example, countyB is set to "STATE", to tell the package
# #  to draw all of the county bountaries within any state containing
# #  a county with data.
# #
# 
# TT   <- c("Ex-c30 Kentucky All County Map w hatching",
#           "defaults")
# 
# pdf("SM-Ex-c30 Kentucky All Co Map w hatching-defaults.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(KY_Co_DF,
#            idCol   ="FIPS",dataCol="AA_Rate",
#            hatch   = list(dataCol="pValue"),
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# 
# #####
# #
# #  Example # c31 Kentucky Partial Co Map  
# #         default - countyB="DATA", seerB="NONE", stateB="NONE"
# #
# #  In this example, the only 75 percent of the county have data.
# #  The default settings are used.
# #
# 
# TT   <- c("Ex-c31 Kentucky Partial County Map",
#           "defaults")
# 
# pdf("SM-Ex-c31 Kentucky Partial Co Map-coB-D srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(KY_Co_P,
#            idCol   ="FIPS",dataCol="AA_Rate",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# #  Not very pretty.
# #
# ####
# 
# 
# #####
# #
# #  Example # c32 Kentucky Partial Co Map  
# #         default - countyB="DATA", seerB="NONE", stateB="NONE"
# #
# #  To improve the c31 map, there are several direction that could be
# #  taken:  Add the state boundaries (stateB="DATA") or draw all of the 
# #  county boundaries (countyB="STATE").  countyB="ALL" is not supported.
# #  In this case, since Kentucky is a single registry, seerB="DATA" has the 
# #  same effect as stateB="DATA".  The difference is stateB="DATA" will not
# #  draw the missing county boundaries, while countyB="STATE" will draw all
# #  of the county boundaries up to the state border.
# #
# 
# TT   <- c("Ex-c32 Kentucky Partial County Map",
#           "stateB='DATA'")
# 
# pdf("SM-Ex-c32 Kentucky Partial Co Map-coB-D srB-N stB-D.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(KY_Co_P,
#            idCol   ="FIPS",dataCol="AA_Rate",
#            stateB  = "DATA",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Example # c33 Kentucky Partial Co Map  
# #         default - countyB="DATA", seerB="NONE", stateB="NONE"
# #
# #  This example has the countyB="STATE" set instead of the stateB="DATA"
# #  that was used in c32.
# #
# 
# TT   <- c("Ex-c33 Kentucky Partial County Map",
#           "countyB='STATE'")
# 
# pdf("SM-Ex-c32 Kentucky Partial Co Map-coB-St srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(KY_Co_P,
#            idCol   ="FIPS",dataCol="AA_Rate",
#            countyB  = "STATE",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Create a data.frame of the Kentucky and Georgia counties (all)
# #   and a combined county data.frame.
# #
# #   Create a partial list of Georgia Counties.
# #
# 
# data(Georgia_CM_Co_Data,envir=environment())
# GA_Co_Data  <- Georgia_CM_Co_Data
# 
# data(Kentucky_CM_Co_Data,envir=environment())
# KY_Co_Data  <- Kentucky_CM_Co_Data
# 
# TwoStatesData <- rbind(GA_Co_Data,KY_Co_Data)
# 
# lGA         <- dim(GA_Co_Data)[1]       # get number of counties
# selectedGA  <- (runif(lGA) <= 0.75 ) # select random 75% of counties
# GA_Co_P     <- GA_Co_Data[selectedGA,]
# 
# #
# #####
# 
# ####
# #
# #  Example # c35 Multiple States - KY and GA County Mapping
# #             with hatching and stateB = DATA.
# #
# #  This example expands example c36 by setting stateB="DATA".  This 
# #  drawns the Seer Registries around any sets of counties with data.
# #  Counties without data will have boundaries missing.
# #
# 
# TT   <- c("Ex-c35 KY-GA All County Map",
#           "defaults")
# 
# pdf("SM-Ex-c39 KY-GA All Co Map def:coB-D srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(TwoStatesData,
#            idCol   = "FIPS",dataCol="AA_Rate",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# #  seerB="ALL" and stateB="ALL" could be used, but will drawn 
# #  a map the size of the entire U.S. and the counties and their 
# #  colors will be very hard to see.
# #
# ####
# 
# ####
# #
# #   Create a data.frame for Georgia counties in the Atlanta Registry, 
# #   all counties.
# #
# 
# GA_Co_Data_Atl <- GA_Co_Data[GA_Co_Data$saID == "GA-ATL",]  
# # pull out of the data the Atlanta Registry.
# 
# #
# #   Create a data.frame for the Georgia counties in the 
# #   Atlanta and Rural registries - All and Partial
# #
# 
# GA_Co_Data_Atl_Rur <- GA_Co_Data[(GA_Co_Data$saID == "GA-ATL" |
#                                     GA_Co_Data$saID == "GA-RUR"),]  
# # pull out of the data the Atlanta Registry.
# lGA         <- dim(GA_Co_Data_Atl_Rur)[1]          # get number of counties
# selectedGA  <- (runif(lGA) <= 0.75 )            # select random 75% of counties
# GA_Co_Data_Atl_Rur_P <- GA_Co_Data_Atl_Rur[selectedGA,]
# 
# #
# #####
# 
# #####
# #
# #  Example # c45  GA Single Seer Registry (Atlanta) All Counties 
# #           def: countyB=DATA, seerB=NONE, stateB=NONE
# #
# #  This example the all of the counties in the Georgia Atlanta Metro Seer 
# #  Registry are selected for mapping. The other counties in the state are not 
# #  listed in the data.frame, so have no data associated.  This example 
# #  shows the map using the default boundary 
# #  settings:  countyB="DATA", seerB="NONE", and stateB="NONE".
# #
# 
# TT   <- c("Ex-c45 GA Atlanta Reg Co ",
#           "def countyB=DATA, seerB=NONE, stateB=NONE")
# 
# pdf("SM-Ex-c45 GA Atl Reg Co def coB-D srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Co_Data_Atl,
#            idCol   ="FIPS",dataCol="AA_Rate",
#            mTitle  = TT
# )
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Example # c46  GA Single Seer Registry (Atlanta) All Counties 
# #                countyB=DATA, seerB=NONE, stateB=ALL
# #
# #  In this extreme example the more boundaries are drawn then really needed.
# #  The data is lost in the size of the map.  Not a good practice. 
# #  If only a small area contains data, don't enable any set of 
# #  boundaries that cover more than is needed. 
# #  In this case, stateB is set to "ALL".
# #
# 
# TT   <- c("Ex-c46 GA Atlanta Reg Co ",
#           "def countyB=DATA, seerB=NONE, stateB=ALL")
# 
# pdf("SM-Ex-c46 GA Atl Reg Co def coB-D srB-N stB-A.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Co_Data_Atl,
#            idCol   ="FIPS",dataCol="AA_Rate",
#            stateB  ="ALL",
#            mTitle  = TT
# )
# dev.off()
# 
# #
# ####
# 
# ####
# #
# #  Example # c47 GA Two Seer Registry Partial Co Map, 
# #           countyB="DATA", seerB="NONE", stateB="NONE"
# #
# #  This example maps the counties in two Georgia Seer Registries: Atlanta Metro 
# #  and Rural with partial data can be mapped and the affects of the boundary options.
# #  By default only the boundaries and counties with data are mapped.
# #
# 
# TT   <- c("Ex-c47 GA Atl-Rur Reg Partial w def",
#           "def: countyB=DATA seerB=NONE stateB=NONE")
# 
# pdf("SM-Ex-c47 GA Atl-Rur Reg Partial Co def coB-D srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Co_Data_Atl_Rur_P,
#            idCol   ="FIPS",dataCol="AA_Rate",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# #  NOTE: The boundaries of the state and Seer Registries at not clearly defined.
# #   The package works the same at the tract, county, seer and state levels
# #   with partial data.  The default is draw and fill sub-areas with data.
# #   Each level can be expanded to drawn sub-areas without data to the next
# #   level boundary with xxxxxB = "ALL", "REGION", "STATE", "SEER", 
# #   and "COUNTY" options.
# #
# ####
# 
# 
# 
# #####
# #
# #   Data for the following examples.  2010 census boundaries
# 
# data(GA_Dem10_Co_Data,envir=environment())
# GA_D_Co_Data  <- GA_Dem10_Co_Data
# 
# #####
# #
# #  Examples c60 to c66 use 2010 census demographic data and use the 
# #  SeerMapper2010 function call to activate the 2010 boundary data collection.
# #
# #####
# 
# #####
# #
# #  Example c60 - Georgia County Data-Population Density with defaults
# #
# #  Uses 2010 demographic County data.frame (GA_Dem10_Co_Data) loaded above.
# #
# 
# TT   <- c("Ex-c60 Georgia Counties Population Density10, c=7",
#           "def: countyB='DATA' seerB='NONE' stateB='NONE'")
# 
# pdf("SM-Ex-c60 GA Counties-PopDens10-c=7 def coB-D srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper2010(GA_D_Co_Data,
#                idCol   = "FIPS",dataCol="popdens",
#                categ   = 7,
#                mTitle  = TT
# )
# 
# dev.off()
# 
# #####
# #
# #  Example c61 - Georgia County Dem. Data for age 65 and up
# #
# #  Uses 2010 demographic county data.frame (GA_Dem10_Co_Data) loaded above.
# #
# 
# TT   <- c("Ex-c61 Georgia County Dem10 Counts for age 65 and up.",
#           "def-countyB=DATA, seerB=NONE, stateB=NONE")
# 
# pdf("SM-Ex-c61 GA Counties10-age 65 and up-def coB-D srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper2010(GA_D_Co_Data,
#                idCol   = "FIPS",dataCol="age.65.up",
#                categ   = 7,
#                mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Example c64 - Georgia County Dem10 Data for Percentage age 65 and up
# #
# #  The Georgia 2010 demographic county Dem dataset (GA_Dem10_Co_Data) 
# #  is used in this example.
# #  The percentage (0% to 100%) of individuals in each tract that is 
# #  65 year old or older is calculated and mapped.
# #
# 
# # calculate the percentage of age 65 up vs population
# 
# GA_D_Co_Data$PC.age.65.up <- ( GA_D_Co_Data$age.65.up / GA_D_Co_Data$pop2010 ) * 100
# 
# TT   <- c("Ex-c64 Georgia County10 for PC 65 and up",
#           "def: countyB='DATA' seerB='NONE' stateB='NONE'")
# 
# pdf("SM-Ex-c64 GA Counties10-PC age 65 and up-def.pdf",
#     width=8,height=10.5)
# 
# SeerMapper2010(GA_D_Co_Data, 
#                idCol   = "FIPS",dataCol="PC.age.65.up",
#                categ   = 7,
#                mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Example c65 - Georgia County Dem10 Data for Percentage households occupied
# #
# #  Using the Georgia 2010 demographic county dataset (GA_Dem_Co_Data),
# #  the percentage (0% to 100%) of the households that are occupied
# #  in each county is calculated and mapped.
# #
# 
# # Calculate percentage of HH occupied vs HH units.
# 
# GA_D_Co_Data$PC.hh.occupied <- ( GA_D_Co_Data$hh.occupied / GA_D_Co_Data$hh.units ) * 100
# 
# TT   <- c("Ex-c65 GA County10 for PC HH Occupied",
#           "def: countyB='DATA' seerB='NONE' stateB='NONE'")
# 
# pdf("SM-Ex-c65 GA Counties10-PC HH Occupied-trB-D coB-N srB-N stB-N",
#     width=8,height=10.5)
# 
# SeerMapper2010(GA_D_Co_Data, 
#                idCol   = "FIPS",dataCol="PC.hh.occupied",
#                categ   = 7,
#                mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Example c66 - Georgia County Dem10. Data for Percentage Household Renters
# #
# #  Using the Georgia 2010 demographic county dataset (GA_Dem_Co_Data),
# #  the percentage (0% to 100%) of the households that have renters
# #  in each county is calculated and mapped.
# #
# 
# # calculate percentage of renters (1-owners) vs units
# 
# GA_D_Co_Data$PC.hh.renter <- (1-( GA_D_Co_Data$hh.owner / GA_D_Co_Data$hh.units )) * 100
# 
# TT   <- c("Ex-c66 GA County10 for PC Renters",
#           "countyB='DATA' seerB='NONE' stateB='NONE'")
# 
# pdf("SM-Ex-c66 GA Counties10-PC Renters-trB-D coB-N srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper2010(GA_D_Co_Data, 
#                idCol   = "FIPS",dataCol="PC.hh.renter",
#                categ   = 7,
#                mTitle  = TT
# )
# dev.off()
# 
# #
# #####
# 
# 
# 
# ####
# #
# #   In the situation where all of the counties with data do not reside 
# #   in a Seer Registry, the behavor of the boundary options is a little 
# #   different.
# #
# #   Example # c70 - Washington Seattle-Puget Sound partial Counties.
# #
# #   In this example we have selected a random set of counties from the 
# #   state of Washington.  Some are in the Seattle-Puget sound Seer Registry
# #   and some are not.
# #
# #   The default settings of the boundary options works the same way.
# #   The countyB options works the same, except countyB="SEER" only 
# #   adds the boundaries for counties within a Seer Registry with counties
# #   with data.  The counties outside the Registry is not drawn.
# #
# 
# data(Washington_CM_Co_Data,envir=environment())
# 
# WA_Data        <- Washington_CM_Co_Data
# 
# # have to compensate for NA in the saID list (no registry)
# isNAsa         <- is.na(WA_Data$saID)   
# sL             <- !isNAsa & (WA_Data$saID    == "WA-SEA")   
# # counties with saID set and == "WA-SEA"
# 
# nSL            <- isNAsa | (WA_Data$saID    != "WA-SEA")    
# # counties with saID not set (NA) or != "WA-SEA"
# 
# 
# WA_Data_Seat   <- WA_Data[sL,]  
# WA_Data_NotSeat<- WA_Data[nSL,]  
# 
# # pull out the data for the Washingto-Puget Registry.
# lWA            <- dim(WA_Data_Seat)[1]        # get number of counties
# selectedWA     <- (runif(lWA) <= 0.7 )       # select random 80% of CO in Puget area.
# WA_Data_Seat_P <- WA_Data_Seat[selectedWA,]
# 
# lWA            <- dim(WA_Data_NotSeat)[1]
# selectedNotWA  <- (runif(lWA) <= 0.3 )
# WA_Data_NotSeat_P<- WA_Data_NotSeat[selectedNotWA,]
# 
# WA_P_Data      <- WA_Data_Seat_P
# WA_P_Data      <- rbind(WA_P_Data,WA_Data_NotSeat_P)
# str(WA_P_Data)
# #
# ####
# 
# #####
# #
# #   Example c70 - WA Partial Counties - one Registry (WA-SEAT)
# #
# 
# TT   <- c("Ex-c70 WA-Seat Partial Reg plus Partial Co",
#           "def basic:countyB=DATA, seerB=NONE, stateB=NONE")
# 
# 
# pdf("SM-Ex-c70 WA-Seat Reg Partial Co default basic.pdf", width=8,height=10.5)
# SeerMapper(WA_P_Data)
# dev.off()
# 
# 
# ####
# #
# #  Example - c71 - Washington State - Partial Co. - one Registry.
# #
# #  To make the mapping of partial counties in Washington State
# #  which has one Registry in part of the state, the following 
# #  enhancement can be made for clarity: draw the outline of the 
# #  states with data (Washington) via stateB="DATA",  draw all of
# #  county boundaries within the Registry with data (WA-SEA), 
# #  increase the number of categories from the default of 5 to 7 (categ=7),
# #  add a two (2) line title (mTitle=TT), use column names to locate
# #  the location ID and data in the data data.frame (idCol="FIPS" and 
# #  dataCol="AA_Rate", and add hatching of counties with a pValue > 0.05
# #  hatch=list(dataCol="pValue").
# #
# 
# TT   <- c("Ex-c71 WA-Seat Partial Reg plus Partial Co",
#           "cat-7, hatching, countyB=SEER, seerB=NONE, stateB=DATA")
# 
# 
# pdf("SM-Ex-c71 WA-Seat Reg Partial Co enhd hatch, cat-7, coB=SR, stB-ST.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(WA_P_Data,
#            idCol = "FIPS", dataCol="AA_Rate", # use column names
#            categ = 7,                           # increase from 5 to 7 categories
#            stateB="DATA",                       # provided state outline
#            countyB="SEER",                      # draw all county boundaries up to Registry
#            hatch = list(dataCol="pValue"),      # use column name for pValue and do hatching.
#            mTitle=TT                            # add title (2 lines)
# )
# 
# dev.off()
# 
# #
# ####
# 
# ####
# #
# #  Example - c73 - Washington State - Partial Co. - one Registry.
# #
# #  A different variation on inproving example c70 is to 
# #  draw all of the counties within the state with data (countyB="STATE"),
# #  the outline of the state would only be needed is further accent is required
# #  (stateB="DATA", move the legend from the left to right side and include the 
# #  counts in each category mLegend=list(pos="right",counts=TRUE), and 
# #  since the categories calculated are:
# #    [152.69-157.16], (156.16-164.00], (164.00-168.76], (168.76-172.91], 
# #    (172.91-174.93], (174.93-179.76], and (179.76-194.80]
# #  we can manually set a reasonable set of breakpoints with 
# #  categ=c(157.5, 164.0, 168.75, 173, 175, 180), also 7 categories.
# #
# #
# 
# TT   <- c("Ex-c73 WA-Seat Partial Reg plus Partial Co",
#           "countyB=STATE, seerB=NONE, stateB=NONE brkpointlist")
# 
# pdf("SM-Ex-c73 WA-Seat Reg Partial Co coB-ST srB-N stB-N brkpt.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(WA_P_Data,
#            idCol   = "FIPS",dataCol="AA_Rate",
#            countyB = "STATE",
#            categ   = c(157.5, 164, 168.75, 173, 175, 180),
#            mLegend = list(pos="right",counts=TRUE),
#            brkPtDigits = 2,
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# #####
# 
# 
# #
# #    Example # c72 shows the default settings with countyB="DATA" for atlanta all tracts.
# #    Example # c73 partial data in Washington State - countyB="SEER"
# #    Example # c74 partial data in Washington State - countyB="STATE"
# # 
# 
# #####
# #
# #  Example # c72 Washington Partial Counties
# #         countyB=DATA, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-c72-Washington Partial Counties","Rates - defaults")
# 
# pdf("SM-Ex-c72-WA-P-Co-Rate-def.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_P_Data,
#            idCol="FIPS",dataCol="AA_Rate",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# #####
# #
# #  Example # c73 Washington Partial Counties
# #         countyB=SEER, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-c73-Washington Partial Counties","Rates - countyB=SEER")
# 
# pdf("SM-Ex-c73-WA-P-Co=Rate-coB-sr.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_P_Data,
#            idCol="FIPS",dataCol="AA_Rate",
#            countyB="SEER",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# #####
# #
# #  Example # c74 Washington Partial Counties
# #         countyB=STATE, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-c74-Washington Partial Counties"," Rate Data - countyB=STATE")
# 
# pdf("SM-Ex-c74-WA-P-Co-Rate-coB-st.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_P_Data,
#            idCol="FIPS",dataCol="AA_Rate",
#            countyB="STATE",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# 
# ####
# #
# #  2000 census County Demographic data for Washington State
# #
# data(WA_Dem_Co_Data,envir=environment())
# 
# WA_D_Data        <- WA_Dem_Co_Data
# 
# # have to compensate for NA in the saID list (no registry)
# isNAsa           <- is.na(WA_D_Data$saID)         
# sL               <- !isNAsa & (WA_D_Data$saID == "WA-SEA")  
# # counties with saID set and == "WA-SEA"
# 
# nSL              <- isNAsa | (WA_D_Data$saID  != "WA-SEA")    
# # counties with saID not set (NA) or != "WA-SEA"
# 
# WA_D_Data_Seat   <- WA_D_Data[sL,]  
# WA_D_Data_NotSeat<- WA_D_Data[nSL,]  
# 
# # pull out the data for the Washington-Puget Registry.
# lWA              <- dim(WA_D_Data_Seat)[1]     # get number of counties
# selectedWA       <- (runif(lWA) <= 0.6 )       # select random 80% of CO in Puget area.
# WA_D_Data_Seat_P <- WA_D_Data_Seat[selectedWA,]
# 
# lWA              <- dim(WA_D_Data_NotSeat)[1]
# selectedNotWA    <- (runif(lWA) <= 0.2 )
# WA_D_Data_NotSeat_P<- WA_D_Data_NotSeat[selectedNotWA,]
# 
# WA_D_P_Data      <- WA_D_Data_Seat_P
# WA_D_P_Data      <- rbind(WA_D_P_Data,WA_D_Data_NotSeat_P)
# str(WA_D_P_Data)
# 
# #
# #    Example # c76 shows the default settings with countyB="DATA" for atlanta all tracts.
# #    Example # c77 partial data in Washington State - countyB="SEER"
# #    Example # c78 partial data in Washington State - countyB="STATE"
# # 
# 
# #####
# #
# #  Example # c76 Washington Partial Counties
# #         countyB=DATA, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-c76-Washington Partial County-Dem","defaults")
# 
# pdf("SM-Ex-c76-WA-Dem-P-Co-def.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_D_P_Data,
#            idCol="FIPS",dataCol="popdens",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# #####
# #
# #  Example # c78 Washington Partial Counties
# #         countyB=SEER, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-c78-Washington Partial Dem County","countyB=SEER")
# 
# pdf("SM-Ex-c78-WA-Dem-P-Co-coB-sr.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_D_P_Data,
#            idCol="FIPS",dataCol="popdens",
#            countyB="SEER",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# #####
# #
# #  Example # c79 Washington Partial Counties
# #         countyB=STATE, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-c79-Washington Partial Dem Counties","countyB=STATE")
# 
# pdf("SM-Ex-c79-WA-Dem-P-Co-trB-st.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_D_P_Data,
#            idCol="FIPS",dataCol="popdens",
#            countyB="STATE",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# 
# 
# ####
# #
# #  Have data at the census tract level works exactly the same as data 
# #  at the county level.  The only exception is supplemental boundary 
# #  information datasets may be needed.
# #
# 
# #
# #  Example - c80 - Wash-Balt CSA county level - defaults
# #
# 
# 
# data(WashBaltMetro_Co_Data)
# 
# TT <- c("SM-Ex-c80-Washington-Baltimore Metro","County-Combined Statistics Area-def")
# 
# pdf("SM-Ex-c80-WashBalt-County-CSA-def.pdf", width=7.5, height=10)
# 
# SeerMapper(WashBaltMetro_Co_Data,
#            idCol="FIPS",dataCol="popdens",
#            categ=7,
#            mTitle=TT
# )
# 
# dev.off()
# 
# #
# #  Example - c81 - Wash-Balt CSA County level - stateB="DATA"
# #
# 
# 
# TT <- c("SM-Ex-c81-Washington-Baltimore Metro","County-Combined Statistics Area-stB=D")
# 
# pdf("SM-Ex-c81-WashBalt-County-CSA-stB-D.pdf", width=7.5, height=10)
# 
# SeerMapper(WashBaltMetro_Co_Data,
#            idCol="FIPS",dataCol="popdens",
#            categ=7,
#            stateB="DATA",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# 
# ####
# #
# #  Example - c83 - Kansas City CSA county level - defaults
# #
# 
# 
# data(KCMetro_Co_Data)
# 
# TT <- c("SM-Ex-c83-Kansas City Metro","County-Combined Statistics Area-def")
# 
# pdf("SM-Ex-c83-KCMetro-County-CSA-def.pdf", width=7.5, height=10)
# 
# SeerMapper(KCMetro_Co_Data,
#            idCol="FIPS",dataCol="popdens",
#            categ=7,
#            mTitle = TT
# )
# 
# dev.off()
# 
# #
# #  Example - c84 - Kansas City CSA County level - stateB="DATA"
# #
# 
# 
# TT <- c("SM-Ex-c84-Kansas City Metro","County-Combined Statistics Area-stB=D")
# 
# pdf("SM-Ex-c84-KCMetro-County-CSA-stB-D.pdf", width=7.5, height=10)
# 
# SeerMapper(KCMetro_Co_Data,
#            idCol="FIPS",dataCol="popdens",
#            categ=7,
#            stateB="DATA",
#            mTitle = TT
# )
# 
# dev.off()
# #
# ####
# 
# 
# 
# #
# #  End of County Examples
# #
# 
# #######################
# 
# #####
# #
# #  Example tr60 - Georgia Census Tract Data-Population Density with defaults
# #
# #  Uses 2000 census demographic tract data from the GA_Dem_Tr_Data dataset.
# #
# 
# data(GA_Dem_Tr_Data)
# GA_Tr_DF   <- GA_Dem_Tr_Data
# 
# #
# 
# TT   <- c("Ex-tr60 Georgia Census Tracts Population Density, c=7",
#           "def: tractB=DATA countyB=NONE seerB=NONE stateB=NONE")
# 
# pdf("SM-Ex-tr60 GA Tracts-PopDens-c=7 def trB-D coB-N srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_DF,
#            idCol   = "FIPS",dataCol="popdens",
#            categ   = 7,
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# #####
# 
# #####
# #
# #  Example tr61 - Georgia Census Tract Data for age 65 and up
# #
# #  Uses demographic tract data.frame (GA_Dem_Tr_Data) loaded above.
# #
# 
# TT   <- c("Ex-tr61 Georgia Census Tract for age 65 and up.",
#           "def-tractB=DATA, countyB=NONE, seerB=NONE, stateB=NONE")
# 
# pdf("SM-Ex-tr61 GA Tracts-age 65 and up-def trB-D coB-N srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_DF,
#            idCol   = "FIPS",dataCol="age.65.up",
#            categ   = 7,
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# #####
# 
# #####
# #
# #  Example tr63 - Georgia Census Tract Data for Percentage age 65 and up
# #
# #  The Georgia demographic census tract dataset (GA_Dem_Tr_Data) 
# #  is used in this example.
# #  The percentage (0% to 100%) of individuals in each tract that is 
# #  65 year old or older is calculated and mapped.
# #
# 
# GA_Tr_DF$PC.age.65.up <- ( GA_Tr_DF$age.65.up / GA_Tr_DF$pop2000 ) * 100
# 
# TT   <- c("Ex-tr63 Georgia Tract for PC 65 and up",
#           "def: tractB=DATA countyB=NONE seerB=NONE stateB=NONE")
# 
# pdf("SM-Ex-tr63 GA Tracts-PC age 65 and up-def.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_DF, 
#            idCol   = "FIPS",dataCol="PC.age.65.up",
#            categ   = 7,
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# #####
# 
# #####
# #
# #  Example tr64 - Georgia Census Tract Data for Percentage age 65 and up
# #
# #  The Georgia demographic census tract dataset (GA_Dem_Tr_Data) 
# #  is used in this example. Same as tr63, but tractB=NONE, and countyB=DATA to
# #  turn off the drawing of the tract boundaries, but make sure 
# #  county boundaries are drawn around areas with data.
# #
# #  The value categorized in the map is the percentage (0% to 100%) of 
# #  individuals in each tract that are 65 year old or older.
# #
# 
# GA_Tr_DF$PC.age.65.up <- ( GA_Tr_DF$age.65.up / GA_Tr_DF$pop2000 ) * 100
# 
# TT   <- c("Ex-tr64 Georgia Tract for PC 65 and up",
#           "tractB=NONE countyB=DATA seerB=NONE stateB=NONE")
# 
# pdf("SM-Ex-tr64 GA Tracts-PC age 65 and up-tB-N cB-D.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_DF, 
#            idCol   = "FIPS",dataCol="PC.age.65.up",
#            categ   = 7,
#            tractB  = "NONE",
#            countyB = "DATA",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# #####
# 
# #####
# #
# #  Example tr65 - Georgia Census Tract Data for Percentage households occupied
# #
# #  Using the Georgia demographic census tract dataset (GA_Dem_Tr_Data),
# #  the percentage (0% to 100%) of the households that are occupied
# #  in each county is calculated and mapped.
# #
# 
# GA_Tr_DF$PC.hh.occupied <- ( GA_Tr_DF$hh.occupied / GA_Tr_DF$hh.units ) * 100
# 
# TT   <- c("Ex-tr65 GA Tract for PC HH Occupied",
#           "def: tractB=DATA countyB=NONE seerB=NONE stateB=NONE")
# 
# pdf("SM-Ex-tr65 GA Tracts-PC HH Occupied-trB-D coB-N srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_DF, 
#            idCol   = "FIPS",dataCol="PC.hh.occupied",
#            categ   = 7,
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Example tr66 - Georgia Census Tract Data for Percentage Household Renters
# #
# #  Using the Georgia demographic census tract dataset (GA_Dem_Tr_Data),
# #  the percentage (0% to 100%) of the households that have renters
# #  in each county is calculated and mapped.
# #
# 
# GA_Tr_DF$PC.hh.renter <- (1-( GA_Tr_DF$hh.owner / GA_Tr_DF$hh.units )) * 100
# 
# TT   <- c("Ex-tr66 GA Tract for PC Renters",
#           "tractB=DATA countyB=NONE seerB=NONE stateB=NONE")
# 
# pdf("SM-Ex-tr66 GA Tracts-PC Renters-trB-D coB-N srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_DF, 
#            idCol   = "FIPS",dataCol="PC.hh.renter",
#            categ   = 7,
#            mTitle  = TT
# )
# dev.off()
# 
# #
# #####
# 
# 
# ####
# #
# #  To do the same Washington Baltimore Metro area a the census tract level,
# #  the data is collected at the census tract level and filtered
# #  to just the CSA tracts.
# #
# #  To cover all of the conditions in the next two examples:
# #
# 
# #
# #  With the \var{SeerMapperEast} and \var{SeerMapperWest} supplemental 
# #  packages loaded, maps can be created for census tracts in any state or 
# #  district.
# #
# 
# ####
# #
# #  County Cancer Mortality data for Washington State
# #
# data(WA_Dem_Tr_Data,envir=environment())
# 
# WA_D_Data        <- WA_Dem_Tr_Data
# 
# 
# # have to compensate for NA in the saID list (no registry)
# isNAsa           <- is.na(WA_D_Data$saID)          
# sL               <- !isNAsa & (WA_D_Data$saID    == "WA-SEA")   
# # counties with saID set and == "WA-SEA"
# 
# nSL              <- isNAsa | (WA_D_Data$saID    != "WA-SEA")    
# # counties with saID not set (NA) or != "WA-SEA"
# 
# WA_D_Data_Seat   <- WA_D_Data[sL,]  
# WA_D_Data_NotSeat<- WA_D_Data[nSL,]  
# 
# # pull out the data for the Washingto-Puget Registry.
# lWA            <- dim(WA_D_Data_Seat)[1]        # get number of counties
# selectedWA     <- (runif(lWA) <= 0.6 )       # select random 80% of CO in Puget area.
# WA_D_Data_Seat_P <- WA_D_Data_Seat[selectedWA,]
# 
# lWA            <- dim(WA_D_Data_NotSeat)[1]
# selectedNotWA  <- (runif(lWA) <= 0.2 )
# WA_D_Data_NotSeat_P<- WA_D_Data_NotSeat[selectedNotWA,]
# 
# WA_D_P_Data      <- WA_D_Data_Seat_P
# WA_D_P_Data      <- rbind(WA_D_P_Data,WA_D_Data_NotSeat_P)
# str(WA_D_P_Data)
# 
# #
# #    Example # tr76 shows the default settings with tractB="DATA" for atlanta all tracts.
# #    Example # tr77 partial data in Washington State - tractB="COUNTY"
# #    Example # tr78 partial data in Washington State - tractB="SEER"
# #    Example # tr79 partial data in Washington State - tractB="STATE"
# # 
# 
# #####
# #
# #  Example # tr76 Washington Partial Tracts
# #         tractB=DATA, countyB=NONE seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-tr76-Washington Partial Tracts-Demog","defaults")
# 
# pdf("SM-Ex-tr76-WA-Dem-P-Tracts-def.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_D_P_Data,
#            idCol="FIPS",dataCol="popdens",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# #####
# #
# #  Example # tr77 Washington Partial Tracts
# #         tractB=COUNTY, countyB=NONE, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-tr77-Washington Partial Tracts","tractB=COUNTY")
# 
# pdf("SM-Ex-tr77-WA-Dem-P-Tract-trB-co.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_D_P_Data,
#            idCol="FIPS",dataCol="popdens",
#            tractB="COUNTY",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# #####
# #
# #  Example # tr78 Washington Partial Tracts
# #         tractB=SEER, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-tr78 Washington Partial Tracts","tractB=SEER")
# 
# pdf("SM-Ex-tr78-WA-Dem-P-Tract-trB-sr.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_D_P_Data,
#            idCol="FIPS",dataCol="popdens",
#            tractB = "SEER",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# 
# #####
# #
# #  Example # tr79 Washington Partial Tracts
# #         tractB=STATE, seerB=NONE, stateB=NONE (def)
# #
# TT <- c("Ex-tr79-Washington Partial Tracts","tractB=STATE")
# 
# pdf("SM-Ex-tr79-WA-Dem-P-Tract-trB-st.pdf", width=7.5, height=10)
# 
# SeerMapper(WA_D_P_Data,
#            idCol="FIPS",dataCol="popdens",
#            countyB="STATE",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# ####
# #
# #  Example - tr83 - Kansas City CSA tract level - defaults
# #
# 
# data(KCMetro_Tr_Data)
# 
# TT <- c("SM-Ex-tr83-Kansas City Metro","Tract-Combined Statistics Area-def")
# 
# pdf("SM-Ex-tr83-KCMetro-Tract-CSA-def.pdf", width=7.5, height=10)
# 
# SeerMapper(KCMetro_Tr_Data,
#            idCol="FIPS",dataCol="popdens",
#            categ=7,
#            mTitle=TT
# )
# 
# dev.off()
# 
# #
# #  Example - tr84 -Kanssa City CSA tract level - stateB="DATA"
# #
# 
# 
# TT <- c("SM-Ex-tr84-Kansas City Metro","Tract-Combined Statistics Area-stB=D")
# 
# pdf("SM-Ex-tr84-KCMetro-CSA-stB-D.pdf", width=7.5, height=10)
# 
# SeerMapper(KCMetro_Tr_Data,
#            idCol="FIPS",dataCol="popdens",
#            categ=7,
#            stateB="DATA",
#            mTitle=TT
# )
# 
# dev.off()
# #
# ####
# 
# 
# #####
# #
# #  Data.frame for Georgia Tracts ALL, Partial Atlanta Registry  2000 census
# #
# 
# data(GA_Dem_Tr_Data,envir=environment()) 
# 
# GA_Tr_DF         <- GA_Dem_Tr_Data
# 
# lGA              <- dim(GA_Tr_DF)[1]
# selectedTr       <- (runif(lGA) <= 0.75)
# GA_Tr_P          <- GA_Tr_DF[selectedTr,]  
# # select a random part (75%) of Georgia Tracts.
# 
# GA_Tr_ATL_DF     <- GA_Tr_DF[GA_Tr_DF$saID == "GA-ATL",]  
# # select Atlanta Reg. Tracts
# 
# lATL             <- dim(GA_Tr_ATL_DF)[1]
# GA_Tr_ATL_P      <- GA_Tr_ATL_DF[(runif(lATL) <= 0.75),]
# 
# GA_Tr_ATLRUR_DF  <- GA_Tr_DF[(GA_Tr_DF$saID == "GA-ATL" | GA_Tr_DF$saID == "GA-RUR"),] 
# # select Atlanta & Rural Regs
# 
# lATLRUR          <- dim(GA_Tr_ATLRUR_DF)[1]
# GA_Tr_ATLRUR_P   <- GA_Tr_ATLRUR_DF[(runif(lATLRUR) <= 0.75),]  
# # select a random set of tracts
# #
# #####
# 
# #####
# #
# #  The use of census tract location IDs and data works identically 
# #  to the county mapping shown above.  The only difference is th e
# #  extra county boundary layer.  The tractB call parameter
# #  allows tract boundaries to be drawn at the data level or up to the 
# #  county, Seer Registry, or State levels.
# #
# 
# #
# #  Example # tr90 - Georgia ALL Tracts Map with defaults
# #    demonstrate the defaults setting - map tracts with data, 
# #    tractB="DATA", countyB="NONE", seerB="NONE", stateB="NONE".
# #
# #    Demonstrates simple census tract mapping with 7 categories and 
# #    the default boundary controls.
# #
# 
# TT   <- c("Ex-tr90 Georgia All Tracts Map-2000 popDensity",
#           "Defaults")
# 
# pdf("SM-Ex-tr90 GA All Tracts Map-Def trB-D coB-N srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_DF,
#            idCol   = "FIPS",dataCol="popdens",
#            mTitle  = TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #     This example maps the tracts assocated with the Georgia Atlanta Seer Registry.
# #     The defaults of tractB="DATA", countyB="NONE", sserB="NONE", and stateB="NONE"
# #     are used.
# #
# #     Alternate boundary controls would have the following affects:
# #       tractB="SEER" has little effect, since all of the tracts within the registry are
# #          present.
# #       countyB="DATA". countyB="SEER" and seerB="DATA" would only affect the map
# #          by drawing heavier bounaries at each level.  Remember registry boundaries 
# #          are on county boundaries.
# #
# #     The drawing of the tract boundaries can be expanded by:
# #       tractB="COUNTY" will draw all of the tracts within the counties 
# #          with tracts with data.  Any tract or county in the Seer Registry or State
# #          will not be drawn or colors.  You will not see any shared boundaries.
# #       tractB="STATE" will draw all of the tracts within the states with tracts with data.
# #          All tracts within a state with a tract with data will be drawn.  Nothing will
# #          be missed.
# #
# 
# ####
# #
# #  Example tr91 - Georgia Tract - Altanta Registry
# #
# #     When partial tract data is present, use the settings that will present the 
# #     data and it's relationship to the surroundings and nothing more.  Don't use
# #     stateB="ALL" or seerB="ALL.  For small areas of data, these are very useful.
# #
# TT   <- c("Ex-tr91 GA ATL Reg All Tracts",
#           "default:tractB=DATA, countyB=NONE, seerB=NONE, stateB=NONE")
# 
# pdf("SM-Ex-tr91 GA ATL Reg Tracts def-trB-D coB-N Sr-N St-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_ATL_DF,
#            idCol   ="FIPS",dataCol="popdens",
#            mTitle  =TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# 
# ####
# #
# #  Example tr92 - Georgia Tract - Altanta Registry
# #
# #     When partial tract data is present, use the settings that will present the 
# #     data and it's relationship to the surroundings and nothing more.  Don't use
# #     stateB="ALL" or seerB="ALL.  For small areas of data, these are very useful.
# #
# TT   <- c("Ex-tr92 GA ATL Reg All Tracts",
#           "def:tractB=SEER, countyB=NONE, seerB=NONE, stateB=NONE")
# 
# pdf("SM-Ex-tr92 GA ATL Reg Tracts def-trB-sr coB-N Sr-N St-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_ATL_DF,
#            idCol   ="FIPS",dataCol="popdens",
#            tractB  ="SEER",
#            mTitle  =TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# 
# ####
# #
# #  Example tr93 - Georgia Tract - Altanta Registry
# #
# #     When partial tract data is present, use the settings that will present the 
# #     data and it's relationship to the surroundings and nothing more.  Don't use
# #     stateB="ALL" or seerB="ALL.  For small areas of data, these are very useful.
# #
# TT   <- c("Ex-tr93 GA ATL Reg All Tracts",
#           "def:tractB=STATE, countyB=NONE, seerB=NONE, stateB=NONE")
# 
# pdf("SM-Ex-tr93 GA ATL Reg Tracts def-trB-st coB-N Sr-N St-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_ATL_DF,
#            idCol   ="FIPS",dataCol="popdens",
#            tractB  ="STATE",
#            mTitle  =TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# ####
# #
# #  Example tr94 - Georgia Tract - Altanta Registry
# #
# #     When partial tract data is present, use the settings that will present the 
# #     data and it's relationship to the surroundings and nothing more.  Don't use
# #     stateB="ALL" or seerB="ALL.  For small areas of data, these are very useful.
# #
# 
# TT   <- c("Ex-tr94 GA ATL Reg Partial Tracts-Def",
#           "tractB=DATA, countyB=NONE, seerB=NONE, stateB=NONE")
# 
# pdf("SM-Ex-tr94 GA ATL Partial Tracts trB-D coB-N srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_Tr_ATL_DF,
#            idCol   ="FIPS",dataCol="popdens",
#            categ   =7,
#            tractB  ="STATE",   
#            mTitle  =TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# #####
# #
# #  Example H02 & H04 - Hatching of non-pValue data, line density, 
# #       and color  
# #
# #  Using the Georgia demographic county dataset (GA_Dem_Co_Data) 
# #   population density to demonstrate:
# #      Hatching of non-pValue type data (popdens), operation set 
# #      to "<", value set to mean of all population densities, color 
# #      changed to blue, palette changed reds, request 6 categories.
# #      
# #
# 
# data(GA_Dem_Co_Data, envir=environment())
# GA_D_Co_Data  <- GA_Dem_Co_Data
# 
# meanPopDens     <- mean(GA_D_Co_Data$popdens)
# cat("meanPopDens:",meanPopDens,"\n")
# 
# TT   <- c("Ex-H02 Georgia Co w hatching of > mean popdens", "")
# 
# pdf("SM-Ex-H02 GA Co hatch-ge mean popdens.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_D_Co_Data, 
#            idCol     = "FIPS",dataCol="popdens",
#            categ     = 6,               # use 6 categories
#            hatch     = list(col="black",
#                             dataCol="popdens",ops=">", value=meanPopDens),
#            palColors = "RdYlGn",
#            mTitle    = TT
# )
# 
# dev.off()
# 
# #
# #####
# 
# #####
# #
# #  Example H04 - Hatching of non-pValue data, line density, 
# #       and color  
# #
# 
# meanPopDens     <- mean(GA_D_Co_Data$popdens)
# cat("meanPopDens:",meanPopDens,"\n")
# 
# TT   <- c("Ex-H04 Georgia Co w hatching of > mean popdens",
#           "den=15, angle=60")
# 
# pdf("SM-Ex-H04 GA Co hatch-ge mean popdens-d15 a60.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_D_Co_Data, 
#            idCol     = "FIPS",dataCol="popdens",
#            categ     = 6,               # use 6 categories
#            hatch     = list(col="red", den=15, angle=60,
#                             dataCol="popdens",ops=">",value=meanPopDens),
#            palColors = "BuGn",
#            mTitle    = TT
# )
# 
# dev.off()
# 
# #
# #####
# 
# #####
# #
# #  The next set of examples look at options to change how the maps will look.
# #
# #  Example P01 - Georgia County Data for Percentage Household Renters
# #
# #  In the extreme you can assign your own colors to each sub-area.
# #  In this example, I have randomly assigned a color from a 
# #  RColorBrewer "Accent" palette to each sub-areas.  The color
# #  is placed in the "dataCol" for the sub-area and categ is set to "COLORS".
# #
# #  Options:  - Use colors instead of data
# #            - turning off the legend.
# #
# library(RColorBrewer)
# 
# data(GA_Dem_Co_Data,envir=environment())
# GA_D_Co_Data <- GA_Dem_Co_Data
# 
# lGA_D_Co    <- dim(GA_D_Co_Data)[1]
# numColrs  <- 8
# ColorSet  <- brewer.pal(numColrs,"Accent")
# 
# numRep      <- (lGA_D_Co/numColrs)+1
# # place colors into the data.frame.
# GA_D_Co_Data$Col <- replicate(numRep, ColorSet)[1:lGA_D_Co]
# 
# TT   <- c("Ex-p01 Georgia All County-Random Accent Colors Test",
#           "def: countyB=DATA seerB=NONE stateB=NONE")
# 
# pdf("SM-Ex-p01 GA Co-Random-Colors-coB-D srB-N stB-N.pdf",
#     width=8,height=10.5)
# 
# SeerMapper(GA_D_Co_Data, 
#            idCol   ="FIPS",dataCol="Col",  # new data column name
#            categ   ="colors",            # specify how to do the categorization
#            mLegend = FALSE,              # turn off the legend.
#            mTitle  =TT
# )
# 
# dev.off()
# 
# #
# ####
# 
# 
# 
# #
# #  end of examples
# #
# #####

  #
  # Reference: Mapping it out, Mark Monnier - benefit of characterization 
  #    and simplification for a more visible map.  
  #
  #    Still need simplification and maintain same area and 
  #    relationships/neigborships.
  #
  #    Purpose of map is to illustrate spatial patterns of the mapped 
  #    variable.  It is important that each area be visible and that 
  #    the spatial relationship be maintain.  It is not important the 
  #    area's shape if maintained.
  #
  #    Centroids and Convex Hulls...  Also population centroid.
 
}

\author{
James B Pearson, Jr \email{jbpearson353@gmail.com} and 
Linda W Pickle \email{lwpickle353@gmail.com}\cr 
Maintainer: "Fanni Zhang" \email{zhangf@imsweb.com}\cr 
Package compiled by "Fanni Zhang" \email{zhangf@imsweb.com}
}
