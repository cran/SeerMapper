\name{SeerMapper}
\alias{SeerMapper}
\title{Quick Rate Mapper at State, State/County, State/County/Census Tract, 
  or Seer Registry area levels}
\description{
Provides a easy and quick means of creating U.S. maps of rate data at the 
U.S. State, State/County, State/County/Census Tract or U.S. Seer Registry 
area levels. Send data is provide at the state level, the outlines of 
all states are overlaid, but only the states with data are categorized 
and colored.  If the data is provided at the state/county or 
state/county/census tract level, only the states involved are 
outlined and only the registry areas with data are categorized and mapped.  
If the data is provided at the Seer Registry area level, all of the 
states are outlined, but only the Seer Registry areas with data are 
categorized and mapped. Each row in the statistics data.frame 
represents one area (a state, state/county, state/county/census tract,
or Seer registry area.)  Either the U.S. FIPS code or the Seer Registry 
area abbreviation is used to tie the data with the associated geographic 
area boundary for mapping. The package supports mapping with either the 
2000 or 2010 U. S. Census Bureau boundaries at the state (district, territory),
county, and census tract levels using a modified version of the census boundary data.
The package also supports mapping of the NCI Seer Registries and/or counties 
or census tracts within the Seer Registries. To handle the package size limits imposed 
by CRAN, the \var{SeerMapper} R-Script and supporting boundary data is distributed
as seven packages:
\enumerate{
  \item \var{SeerMapper} package containing the \var{SeerMapper} function code and the boundary
   data information for all of the U. S. regions, states, seer registries, and counties
   for census years 2000 and 2010.  This is the base package for all \var{SeerMapper}
   operations.
  \item \var{SeerMapperRegs} package contains the 2000 census tract boundary data 
   information for the 15 U. S. states with NCI Seer Registries. 
  \item \var{SeerMapper2010Regs} package contains the 2010 census tract boundary data
   information for the 15 U. S. states with NCI Seer Registries.
  \item \var{SeerMapperEast} package contains the 2000 census tract boundary data
   information for U. S. 21 states, DC and PR that do not have 
   NCI Seer Registries and are east of the Mississippi river.
 \item \var{SeerMapper2010East} package contains the 2010 census tract boundary data
   information for U. S. 21 states, DC and PR that do not have 
   NCI Seer Registries and are east of the Mississippi river.
 \item \var{SeerMapperWest} package contains the 2000 census tract boundary data
   information for U. S. 14 states that do not have 
   NCI Seer Registries and are west of the Mississippi river.
 \item \var{SeerMapper2010West} package contains the 2010 census tract boundary data
   information for U. S. 14 states that do not have 
   NCI Seer Registries and are west of the Mississippi river.
}
When the \var{SeerMapper} package is installed, the other six packages are
automatically downloaded and installed.  When the \var{SeerMapper} function 
is executed, if needed, it manages which census tract supplemental package of 
the 6 are loaded. 

The boundary data and information in these packages are intended for 
mapping purposes and should not be used for locating geographical points or area 
estimating. The boundary data has been simplified to speed up the mapping process
and reduce the size of the boundary distributed with the package.  
}
\usage{SeerMapper( ndf,                                  
	censusYear     = NULL,    
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
	countyB        = NULL,    # default: "NONE" *
	tractB         = NULL,    # default: "NONE" *
	dataBCol       = NULL,    # default: 'black'
	clipTo         = NULL,    # default: "NONE"
	hatch          = NULL,    # T/F or List of hatching options
	mLegend        = NULL,    # T/F or list of legend options.
	brkPtDigits    = NULL,    # default: 2
	palColors      = NULL,    # default: "-RdYlBu" w/11 cat. colors
	debug          = NULL     # default: FALSE
	)	
}
\arguments{
  \item{ndf}{a data.frame containing identifier, data and p-values 
    for each area to map. The \var{ndf} must contain the 
    \var{id} and \var{data} column providing the geographical link to the 
    boundary data and the data to be classified and colored by the package.
    See the \option{idCol} and \option{dataCol} parameters below for 
    more details.  Based on the type of area identifier used, the package 
    can produces maps at the state, state/county, state/county/census tract, 
    Seer Registry area, state/seer/county or state/seer/county/tract levels.
   }
  \item{censusYear}{is a numeric or character value of the census year 
    boundary data to use in the validation of the data and mapping of the areas.  
    The value must be \var{2000} or \var{2010}. The default is \var{2000} 
    in the \var{SeerMapper} call.  Using the \var{SeerMapper2010} function
    call, the censusYear call parameter is set to "2010".
   }
  \item{idCol}{a character vector specifying the name of the column 
    in the \var{ndf} data.frame to use for the rate data to categorize 
    and mapped.  The default value is \var{FIPS}.
   }
  \item{dataCol}{a character vector specifying the name of the column 
    in the \var{ndf} data.frame to use for the rate data to categorize 
    and mapped. The default value is \var{Rate}.  
    If \option{categ} is set to "data", then the \option{dataCol}
    column contains the integer category numbers instead of rates.
   }
  \item{categ}{a character or numeric vector specifying a single numeric value (the 
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
    list is 10.  In some cases, it may be necessary to place \var{Inf} in the 
    list as the last breakpoint.
    For example: \option{categ} = c(0.6, 0.8, 1.0, 1.2, 1.4).
    
    If any map has the same color for all areas with data and  
    a breakpoint list is used, check the range of the data and 
    the breakpoint list to determine why all of the area were categorized 
    into the same color.
    
    If multiple maps of data need to be compared, it is recommended, the same 
    breakpoint list be used for all package calls to ensure the categories and 
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
    to be used as the title of the map.  For example:  mTitle=c("Major Title Line",
    "minor title line")
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
    When set to \var{REGION}, all states with a region are drawn 
    when the region contains a state or sub-area with data.\cr
    When set to \var{DATA}, only the state boundaries are drawn 
    if the state or a sub-area has data values.\cr 
    When set to \var{NONE}, None of the state boundaries are drawn.
   }
  \item{seerB}{Seer Registry Area Boundaries Option. This option has 
    four values: \var{NONE}, \var{DATA}, \var{STATE}, \var{REGION}, and \var{ALL}.  
    The default value is \var{DATA} with Seer Registry data and 
    \var{NONE} for all other levels of data.\cr
    When set to \var{ALL}, all registry boundaries are drawn.\cr  
    When set to \var{REGION}, all registry boundaries are drawn within a 
    region when the region contains a state, registry or sub-area with data.\cr
    When set to \var{STATE}, all registry boundaries within a state are drawn 
    if the state contains areas with data values.will be drawn.\cr 
    When set to \var{DATA}, only registry boundaries are drawn if the registry contains
    areas with data values.\cr
    When set to \var{NONE}, no registry boundaries are drawn.  
   }
  \item{countyB}{County Boundaries Option. This option has four values: \var{NONE}, 
    \var{DATA}, \var{SEER}, and \var{STATE}.  This option is only valid when county or 
    census tract level data is used.  The default value is \var{DATA} with county level 
    data and \var{NONE} for all other levels of data.\cr
    When set to \var{NONE}, No county boundaries are drawn.\cr  
    When set to \var{DATA}, Only county boundaries are drawn, if the county has data or 
    contains a tract with data values.\cr
    When set to \var{SEER}, All county boundaries are drawn within the Seer Registry, if  
    the registry contains any county or tract with data values.\cr  
    When set to \var{STATE}, All county boundaries are drawn within a state, when the state
    contains any county or tract with data.
   }
  \item{tractB}{Census Tract Boundaries Option. This option has five values: \var{NONE}, 
    \var{DATA}, \var{COUNTY}, \var{SEER}, and \var{STATE}.  
    This option is only valid with census tract level data.
    The default value is \var{DATA} for tract level data and \var{NONE} for all other 
    levels of data.\cr  
    When set to \var{NONE}, No tract borders are drawn.\cr 
    When set to \var{DATA}, All tract boundaries are drawn for tracts with data values.\cr
    When set to \var{COUNTY}, All tract boundaries are drawn for tracts within a county,
    if the county contains any tract with data values.\cr
    When set to \var{SEER}, All tract boundaries are drawn for tracts within a registry,
    if the registry contains any tract with data values.\cr
    When set to \var{STATE}, All tracts boundaries are drawn for trats within a state,
    if the state contains any tract with data values
   }
  \item{clipTo}{a character or logical value.  This parameter controls how 
    the boundaries will be drawn in the grapics area and how the spatial 
    box is calculated.  The package normally calculates the graphic area size 
    to accomodate the drawing all of the boundaries requested by the boundary 
    parameters.  So, if all of the state boundaries were requested 
    (stateB="ALL") and only one state has data, the entire U.S. will be mapped.
 
    The values for \option{clipTo} are:  TRUE, FALSE, "NONE", "DATA", "SEER", or  
    "STATE".  
    
    If \option{clipTo} is set to FALSE or "NONE", no clipping of the boundaries is done.
    All of the boundaries will be drawn as requested by the boundary parameters.
    
    If \option{clipTo} is set to TRUE or "DATA", the size of the graphic area is set 
    to cover only the sub-areas with data. Any other boundaries (higher levels) or 
    non-data sub-areas will be clipped to the graphic area of the data sub-areas.
    
    If \option{clipTo} is "SEER" and the data is in Seer Registries, then the 
    boundary of the Registries is used as the graphics box.  If there are 
    any sub-areas with data, but are not part of a registry, the \option{clipTo} 
    changed to "STATE".
    
    If the \option{clipTo} is "STATE", then the state or states containing
    the data sub-areas are used as the graphics box size for the mapping.  
   }                
  \item{dataBCol}{is a character vector of one element.  It is used to 
    specify the color of the data level boundary on the maps.  The default 
    value is 'black'. The value must be a color name from the \code{colors()} 
    name list or a string starting with a "#" with 6 or 8 hexidecimal digits 
    representing the RGB and transparancy values of a color.  If the color 
    specified is not valid, the function will generate a warning and stop.
   }                
  \item{mLegend}{is a list of legend options.  It must a list containing named 
    items. The mLegend can be disabled by setting \option{mLegend} = \var{NA} 
    is the call. The legend is placed on the bottom of the map. 
    The following options are
    available to fine tune the map's legend: \option{counts}, 
    \option{size}, \option{numberColumns}, \option{pos}, and \option{noValue}. 
    See below for more details on each option.
    The options are specified in the call by setting 
    \code{mLegend = list(counts=TRUE, size=0.6)}, for example.
   \describe{
     \item{numCols}{is the number of columns to use when creating a legend.  
       The default is \var{3}.  The number of columns can range from \var{1} 
       to \var{8}.
      }
     \item{size}{is a numeric value used to size the legend symbols and 
       texts as required.  The default value is 0.85. 
      }
     \item{pos}{is a character vector specifying the location of the legend 
       along the bottom of the map. The acceptable values are \var{left}, 
       \var{center}, and \var{right}.  The default is \var{left}. 
      }
     \item{counts}{is a logical variable - \var{TRUE} or \var{FALSE}.  
       If \var{TRUE}, the observation counts for each category are displayed 
       in the legend after the description text. The default is \var{FALSE}.
      }
     \item{noValue}{is a logical variable - \var{TRUE} or \var{FALSE}.  
       If \var{TRUE}, any category containing no observations (data) is tagged 
       in the legend with "NV" after the category label. 
       The default is \var{FALSE}.
      } 
    }
   }
  \item{hatch}{is a logical value of \var{TRUE} or \var{FALSE} or a list 
    structure  with hatching settings. The call parameter enabled and 
    provided parameters for hatching areas (state, Seer Registry, county 
    or census tracts) based on data and a criteria.  The most common use 
    of hatching is to indicate the reliability of the data via a P-Value. 
    In this case, the range of the data is 0 to 1, the comparison value 
    is 0.05 and the criteria is greater than to hatch an area. 
    If the \option{hatch} parameter is set to \var{TRUE}, 
    then hatching will be implemented using the standard settings 
    for hatching. The standard settings are: \option{range} = 0 to 1, 
    \option{value} = 0.05, \option{ops} = \var{gt}, and 
    \option{dataCol} = \var{pValue}.  If set to \var{FALSE}, 
    no hatching is done. If the \option{hatch} call parmeter is 
    set to a list, hatching is enabled and the list is inspected for 
    overriding values for the default setting.
    
    The following hatch settings may be changed using the \option{hatch} 
    call parameter to pass a list of items and new values.  
    The acceptable set of items are:
   \describe{
     \item{dataCol}{a character vector specifying the name of the column in the 
       \var{ndf} data.frame to use to test the value for each area.  
       The value in this column used to test if the value is < \var{0.05}.  
       If the value is not < \var{0.05}, then the associated area is hatched 
       as an indicator. 
       The default column name value is \var{pValue}.   
       Please note the same variable name is used as the statistical data, 
       but this variable is used for the hatching feature.
      }
     \item{col}{a character vector containing the name or \code{#A8A8A8} value 
       of the color to be used for the area hatching pattern.  
       The default value is \code{grey(0.66)} or \code{#A8A8A8}.
      }
     \item{lwd}{a numeric value.  The line weight for the hatching lines. 
       Default value is \var{0.5}.
      }
     \item{lty}{a numeric value. The line type for the hatching lines. 
       Line types are specified as an integer (0=blank, 1=solid (default), 
       2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash). 
       In a later version, the line types can be specified as a character 
       strings of "blank", "solid", "dashed", "dotted", "dotdash", "longdash", 
       or "twodash", where "blank" does not draw the line 
       (transparent).    Default value is \var{1} or "solid". 
      }
     \item{density}{a numeric value. \var{den} can also be used as the option 
       name. When drawing the hatching over the area, this option specifies 
       the density of the hatching pattern in lines per inch.  The valid range 
       is from 5 to 64 lines per inch.  The default value is \var{25}.
      }
     \item{angle}{a numeric value.  When drawing the hatching over the area, 
       this option specifies the angle of the hatching pattern in degrees 
       counter clockwise. The default value is 45 degree.  The value must 
       be between 360 and -360 degrees.
      }
     \item{value}{a numeric break point for comparison with the hatching 
       data column (pValue).  Default is 0.05.
      }
     \item{ops}{a character value. The values may be one of the 
       following: "eq", "ne", "lt", "le", "gt", or "ge". These translate 
       to "==", "!=", "<", "<=", ">", and ">=" operations.  Other notations 
       are allowed and translated appropriately: "<>", "=>", "=", and "=<".  
       The comparison is the data in the \option{dataCol} column to the 
       \option{hatch} \var{value} option using the \var{ops} 
       operation.  
       
       The formula is \option{dataCol} \var{ops} \var{value}.  
       
       For example, for P-Value testing the test is  \var{data} > \var{0.05}.  
       A \var{TRUE} result will cause the area to be hatched.
      }
     \item{range}{a vector of two values, min and max inclusively. Range 
       checking of is normally disabled, but can be used to validate the 
       data used to determine if hatching should be done for an area.  
       \var{range} can be set to:  \var{NA}, \var{FALSE} to disable range 
       checking,  \var{TRUE} to enable range check for var{c(0,1),} or 
       to a range checking vector of a low and high value, c(1234,532).  
       If the low and high value of the range is out of order, it will be 
       sorted and a warning message generated.  If the range of the hatching 
       data is not know, it is recommended to disable the range checking.  
       If the data is p-value data, then range=TRUE will ensure all of 
       the data is valid.
      }
    }
   }
  \item{brkPtDigits}{specifies the number of digits to the right of the 
    decimal place when calculating or pre-processing the break point list 
    and resulting categorization intervals.  Default is the calculated 
    number of digits based on the interval between break points. 
    For example: if \option{brkPtDigits} = \var{1}, the breakpoint 
    rounded and limited to only having one digit. The valid range is 1 to 4. 
    The default value is \var{2}.  This parameter is only active when the 
    \option{categ} call parameter specifies the number of categories. 
   }
  \item{palColors}{a character string specifying the \var{RColorBrewer} 
    color palette used to generate the colors to map the data categorization.  
    The specified palette is check for validity and the maximum number 
    of colors for the palette is used to limit the number of categories 
    the user can specify in \option{categ} parameter.  
    See the \var{RColorBrewer} help for a list of the acceptable color palettes.  
    This call parameter is only active when the 
    \option{categ} is NOT set to "COLORS".
    
    The permitted color palettes and maximum number of colors (categories) 
    supported are:
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
    To reverse the order of the colors, place a "-" in front of the palette name,
    e.g. "-RdYlBu".  
    The default value is "-RdYlBu".
   }
  \item{debug}{is a logical variable.  When set to \var{TRUE}, debug prints of 
    variables and the flow of the package are enabled.  Use of this option adds 
    a large number of debug lines to the package standard device output.
   }
 }

\details{
 The \var{SeerMapper} package (and the \var{SeerMapper2000} and 
 \var{SeerMapper2010} functions) provide a simple 
 means of mapping data for the U. S. at several levels:  State,  
 State/County, State/County/Census Tract, and U. S. Seer Registry areas. 
 The package automatically determines the mapping level based on the 
 data and the area identifiers provide at the time
 of the call.  The data is categorised and each area is drawn 
 and colored appropriately based on its data value.
 
 The \var{censusYear} call parameter controls which census year name 
 tables and boundary data will be used for the mapping.  
 The \var{SeerMapper2000} function overrides the \var{censusYear} parameter 
 and sets it to "2000" for the 2000 U. S. census.
 The \var{SeerMapper2010} function sets the \var{censusYear} parameter 
 to "2010" for mapping with the 2010 U. S. location FIPS codes.
 The default value for the \var{censusYear} parameter is "2000", 
 so calling \var{SeerMapper} without the \var{censusYear} parameter set, 
 is equivalently the same  as calling \var{SeerMapper2000}.
  
 The level of the map is determined by the identifier used in the data 
 provided when the function is called.  
 
 If the data uses the U. S. State fips code (2 digits) as the spatial 
 identifer, then the state boundaries are drawn and colored.  
 In this mode, the caller has two options: draw all of the states 
 in the US (or continental 48 state), drawn only the states with 
 data.  This is controled by the \option{stateB} call parameter.  
 It's default value for state level data is \var{ALL}.  
 To only draw boundaries for states with data, specify 
 \option{stateB} = \var{DATA}.  Seer Registry, county and 
 census tract boundaries will not be drawn.
 
 If the data uses the U. S. State and County fips code (5 digits) as 
 the spatial identifier, then the state and county boundaries are 
 drawn and colored for areas with data. In this mode, there are several options:
 \enumerate{
   \item Draw only the boundaries for the counties with data 
    ( \option{countyB} = \var{DATA} ), 
   \item Draw the counties with data and their state's boundaries 
    (states with data) ( \option{stateB} = \var{DATA} ),  
   \item Draw the boundaries for all of the counties in the states where data 
     was provided for any county in the state (state with data and counties 
     within the state)  ( \option{stateB} = \var{DATA} and 
     \option{countyB} = \var{STATE} ),
   \item Draw the bountaries of all of the counties within a Seer Registry 
     where data was provided for any county in the Seer Registry 
     (Seer Registry with data)  ( \option{countyB} = \var{SEER} )
 }
 If the data uses the U. S. State, County and Census Tract fips code 
 (11 digits) as the spatial identifier, then the census tracts boundaries 
 are drawn and colored for areas with data. In this mode, there are 
 several options:
 \enumerate{
   \item Draw only the boundaries for the census tracts with data 
     ( \option{tractB} = \var{DATA} default setting),
   \item Draw the tracts with data and their state's boundaries 
      (states with data) ( \option{stateB} = \var{DATA} ),
   \item Draw the boundaries for all of the tracts in the states where 
     data was provided for any tract in the state (state with data and 
     tracts within the state) ( \option{stateB} = \var{DATA} and 
     \option{tractB} = \var{STATE} ),
   \item Draw the bountaries of all of the tracts within a Seer Registry 
      where data was provided for any county in the Seer Registry 
      (Seer Registry with data) ( \option{tractB} = \var{SEER} ),
 }
 
 If the spatial identifier does not match one of the above patterns, 
 the package attempts to match the identifier against the list of 
 Seer Registry abbreviations and an partial match against keywords for 
 the Seer Registries.  If there is no match, an error message is generated
 to inform the caller.  When Seer Registry data is used, the package 
 maps each Seer Registry based on the data presented. 
  
 In this mode, there are several options:
 \enumerate{
   \item Draw only the boundaries for the Seer Registries with data 
     without any state boundaries ( \option{seerB} = \var{DATA} 
     and \option{stateB} = \var{NONE}, the default setting ),
   \item Draw only the boundaries for the Seer Registries with data 
     and their associated state boundaries ( \option{seerB} = \var{DATA} 
     and \option{stateB} = \var{DATA} ),
   \item Draw only the boundaries for the Seer Registries with data 
     and all of the U.S. state boundaries ( \option{seerB} = \var{DATA} 
     and \option{stateB} = \var{ALL} ),
   \item Draw all of the Seer Registries boundaries within a state with data 
     and only the state boundaries with data ( \option{seerB} = \var{STATE} 
     and \option{stateB} = \var{DATA} ).
 }
 For each level of data, there are more options related to how and when the 
 state, county, census tract and Seer Registry are drawn.  See the descriptions 
 for the \option{stateB}, \option{seerB}, \option{countyB}, and \option{tractB} 
 call parameters.

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
    CA-OTH\tab CALIFORNIA EXCL\tab California counties not in other CA registries\cr
    CA-OTH\tab GREATER CALIF  \tab See Above\cr
    CA-SF \tab SAN FRAN       \tab California San Fran/Oakland\cr
    CA-SJ \tab SAN JOSE       \tab California San Jose/Monteray\cr
    CT    \tab CONNECTICUT    \tab State of CT Registry\cr   
    GA-ATL\tab ATLANTA        \tab Atlanta GA metro\cr
    GA-OTH\tab GEORGIA OTHER  \tab GA counties no in the Altanta or Rural registries\cr
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
#  In this example rate and p-value data is provided for each state.  
#  The number of categories requested is set to 5.  
#  The "pValue" column in the data is used to hatch states when 
#  the p-value > 0.05.  The defaults on the hatch feature are tuned
#  support hatching of p-value data that is not-significate. 
#
#  Border defaults for state data are = stateB=ALL, seerB=NONE.
#
#  This example uses example s1's data.  All state boundaries are drawn.
#

TT   <- c("Ex-s01 States Mapping Cancer Mortality Rate",
            "all defaults")

pdf("SM-Ex-s01 States Map of Cancer Mortality-defaults.pdf",
     width=10.5,height=8)

SeerMapper(USStates_CM_St_Data,
           mTitle  =TT
        )
dev.off()

#
#  The p-value data in the dataset was assigned 0.02 or 0.2 based on a
#  comparison the state's confidence interval values and the US's rate, 
#  for age adjusted rates for all cancers and cancer deaths for the years
#  2009 to 2013.
#
####

#####
#
#  Example # s02 - States Mapping Basic with Hatching (keep)
#
#  In this example rate and p-value data is provided for each state.  
#  The number of categories requested is set to 5.  
#  The 'pValue' column in the data is used to hatch states when 
#  the data value > 0.05.  The defaults on the hatch feature are tuned
#  support hatching of p-value data that is not-significate. 
#
#  Border defaults for state data are = stateB=ALL, seerB=NONE.
#
#  This example uses example s1's data.  All state boundaries are drawn.
#

TT   <- c("Ex-s02 States Mapping Cancer Mortality Rate",
            "w hatching and defaults")

pdf("SM-Ex-s02 States Map of Cancer Mortality-w hatching.pdf",
      width=10.5,height=8)

SeerMapper(USStates_CM_St_Data,
           hatch   = TRUE,
           mTitle  = TT
        )
        
dev.off()

#
#  The p-value data in the dataset was assigned 0.02 or 0.2 based on a
#  comparison the state's confidence interval values and the US's rate, 
#  for age adjusted rates for all cancers and cancer deaths for the years
#  2009 to 2013.
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

data(SeerRegs_CM_Data,envir=environment())   # 17 registries
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
#  Example # sr11 - Seer Registry 17 Mapping (keep)
#
#  Of the 21 NCI Seer Registries, the basic 17 registries are
#     CT, Detroit, GA-Atlanta, GA-Rural, GA-Other, 
#     Hawaii, Iowa, Utah, New Jersey, New Mexico,
#     Calif-San Fran, Calif-San Jose, Calif-LA, Calif-Other (Greater), Kentucky, 
#     Louisiana, and Wash-Seattle-Puget Sound
#  
#  Boundaries are included, but data is rarely available for:
#     Alaska, Arizona, and Oklahome native areas.
#
#  The default stateB and seerB call parameter values for 
#  for Seer Registry data are:  stateB="ALL" and seerB="DATA".  
#  The countyB and tractB parameters are ignored.
#
#
TT   <- c("Ex-sr11 Seer Reg 17 Map-Cancer Mort. Rates All Both",
           "stateB-ALL, seerB-DATA" )

pdf("SM-Ex-sr11 Seer Reg 17 Map stB-A srB-D.pdf",
      width=10.5,height=8)

SeerMapper(SeerRegs_CM_Data,
              idCol   = "Registry",dataCol="All_Both",
              stateB  = "ALL",
              mTitle  = TT
            )
dev.off()

#
####


####
#
#  Create a data.frame for All and Partial Kentucky Counties. 
#

data(Kentucky_CM_Co_Data,envir=environment())
str(Kentucky_CM_Co_Data)

KY_Co_DF <- Kentucky_CM_Co_Data         # start with the fill set of counties.

lKY      <- dim(KY_Co_DF)[1]            # get number of counties
selKY    <- (runif(lKY) <= 0.75 )       # select random 75% of counties
KY_Co_P  <- KY_Co_DF[selKY,]

#
#####

#####
#
#  Example # c30 Kentucky All Co Map w hatching, 
#         default - countyB="DATA", seerB="NONE", stateB="NONE"
#
#  In this example, countyB is set to "STATE", to tell the package
#  to draw all of the county bountaries within any state containing
#  a county with data.
#

TT   <- c("Ex-c30 Kentucky All County Map w hatching",
               "defaults")

pdf("SM-Ex-c30 Kentucky All Co Map w hatching-defaults.pdf",
      width=10.5,height=8)

SeerMapper(KY_Co_DF,
              idCol   ="stcoID",dataCol="AA_Rate",
              hatch   = list(dataCol="pValue"),
              mTitle  = TT
            )

dev.off()

#
####

#####
#
#   Data for the following examples.  2010 census boundaries

data(GA_Dem10_Co_Data,envir=environment())
GA_D_Co_Data  <- GA_Dem10_Co_Data
GA_D_Co_Data$PC.age.65.up <- ( GA_D_Co_Data$age.65.up / GA_D_Co_Data$pop2010 ) * 100
meanPopDens     <- mean(GA_D_Co_Data$popdens)

#####
#
#  Examples c64 use 2010 census demographic data and use the 
#  SeerMapper2010 function call to activate the 2010 boundary data collection.
#

#####
#
#  Example c64 - Georgia County Dem10 Data for Percentage age 65 and up
#
#  The Georgia 2010 demographic county Dem dataset (GA_Dem10_Co_Data) 
#  is used in this example.
#  The percentage (0% to 100%) of individuals in each tract that is 
#  65 year old or older is calculated and mapped.
#

# calculate the percentage of age 65 up vs population

TT   <- c("Ex-c64 Georgia County10 for PC 65 and up",
             "def: countyB='DATA' seerB='NONE' stateB='NONE'")

pdf("SM-Ex-c64 GA Counties10-PC age 65 and up-def.pdf",
     width=8,height=10.5)

SeerMapper2010(GA_D_Co_Data, 
               idCol   = "ID",dataCol="PC.age.65.up",
               categ   = 7,
               mTitle  = TT
           )

dev.off()

#
####

#####
#
#  Example H04 - Hatching of non-p-value type data, line density, 
#       and color  
#
#  Using the Georgia demographic county dataset (GA_Dem_Co_Data) 
#   population density to demonstrate:
#      Hatching of non-p-value type data (popdens), operation set 
#      to "<", value set to mean of all population densities, color 
#      changed to blue, palette changed reds, request 6 categories.
#      

#
#  Example H04 - Hatching of non-p-value type data, line density, 
#       and color  
#

TT   <- c("Ex-H04 Georgia Co w hatching of > mean popdens",
                 "den=15, angle=60")

pdf("SM-Ex-H04 GA Co 2010 hatch-ge mean popdens-d15 a60.pdf",
     width=8,height=10.5)

SeerMapper2010(GA_D_Co_Data, 
            idCol     = "ID",dataCol="popdens",
            categ     = 6,               # use 6 categories
            hatch     = list(col="red", den=15, angle=60,
                         dataCol="popdens",ops=">",value=meanPopDens),
            palColors = "BuGn",
            mTitle    = TT
          )

dev.off()

#
#####

####
#
#  Have data at the census tract level works exactly the same as data 
#  at the county level.  The only exception is supplemental boundary 
#  information datasets may be needed.
#

#
#  Example - c80 - Wash-Balt CSA county level - defaults
#

data(WashBaltMetro_Co_Data)

TT <- c("SM-Ex-c80-Washington-Baltimore Metro","County-Combined Statistics Area-def")

pdf("SM-Ex-c80-WashBalt-County-CSA-def.pdf",  width=10.5,height=8)

SeerMapper(WashBaltMetro_Co_Data,
                  idCol="ID",dataCol="popdens",
                  categ=7,
                  mTitle=TT
                )

dev.off()
#
####


#
#  End of County Examples
#

#######################

###
#
#  To do the same Washington Baltimore Metro area a the census tract level,
#  the data is collected at the census tract level and filtered
#  to just the CSA tracts.
#
#  To cover all of the conditions in the next two examples:
#

#
#  With the \var{SeerMapperEast} and \var{SeerMapperWest} supplemental 
#  packages loaded, maps can be created for census tracts in any state or 
#  district.
#

####
#
#  County Cancer Mortality data for Washington State (keep)
#
data(WA_Dem_Tr_Data,envir=environment())

WA_D_Data           <- WA_Dem_Tr_Data
WA_D_Data$PCover65  <- WA_Dem_Tr_Data$age.65.up/WA_Dem_Tr_Data$pop2000   #  Percentage 65/pop

 # have to compensate for NA in the saID list (no registry)
isNAsa           <- is.na(WA_D_Data$saID)         
sL               <- !isNAsa & (WA_D_Data$saID    == "WA-SEA")
                      # counties with saID set and == "WA-SEA"

nSL              <- isNAsa | (WA_D_Data$saID    != "WA-SEA")    
                      # counties with saID not set (NA) or != "WA-SEA"

WA_D_Data_Seat   <- WA_D_Data[sL,]  
WA_D_Data_NotSeat<- WA_D_Data[nSL,]  

         # pull out the data for the Washingto-Puget Registry.
lWA            <- dim(WA_D_Data_Seat)[1]        # get number of counties
selectedWA     <- (runif(lWA) <= 0.6 )       # select random 80% of CO in Puget area.
WA_D_Data_Seat_P <- WA_D_Data_Seat[selectedWA,]

lWA            <- dim(WA_D_Data_NotSeat)[1]
selectedNotWA  <- (runif(lWA) <= 0.2 )
WA_D_Data_NotSeat_P<- WA_D_Data_NotSeat[selectedNotWA,]

WA_D_P_Data      <- WA_D_Data_Seat_P
WA_D_P_Data      <- rbind(WA_D_P_Data,WA_D_Data_NotSeat_P)

#
#    Example # tr74 shows percentage of 65 and up over population.
#    Example # tr78 partial data in Washington State - tractB="SEER"
# 

#####
#
#  Example # tr74 Washington All Census Tracts  (keep)
#         tractB=DATA, countyB=NONE seerB=NONE, stateB=NONE (def)
#
TT <- c("Ex-tr74-Washington All Census Tracts","Percentage > 65, defaults")

pdf("SM-Ex-tr74-WA-Dem-All-Census-Tracts-PCgt65-def.pdf", width=10.5,height=8)

SeerMapper(WA_D_Data,
                idCol="ID",dataCol="PCover65",
                mTitle=TT
              )
dev.off()
#
####


#####
#
#  Example # tr78 Washington Partial Tracts
#         tractB=SEER, seerB=NONE, stateB=NONE (def)
#
TT <- c("Ex-tr78 Washington Partial Tracts","tractB=SEER")

pdf("SM-Ex-tr78-WA-Dem-P-Tract-trB-sr.pdf", width=10.5,height=8)

SeerMapper(WA_D_P_Data,
                idCol="ID",dataCol="popdens",
                tractB = "SEER",
                mTitle=TT
              )

dev.off()
#
####

#
#  end of examples
#
#####

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
 
}

\author{
 Jim Pearson \email{jpearson@statnetconsulting.com}\cr 
 Maintainer: "Jim Pearson" \email{jpearson@statnetconsulting.com}\cr 
}