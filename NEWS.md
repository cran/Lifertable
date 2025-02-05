
# Lifertable 0.1.0

## New features

*   The "InitiationOfAdultStage" parameter from "lifertable()" now accepts a value or a vector of values, which can be within "data" or just concatenated with "c()", one value for each group.


*   The "LIFERTABLE" component (of the object) resulting from "lifertable()" now has TWO NEW COLUMNS:
    
    -   "gx": Age-Specific Survivorship. The probability that an individual who has already survived to age "x"" will survive to age "x + 1".
    
    -   "ex": Life Expectancy. Is how much longer an individual of a given age can be expected to live beyond its present age. The expected number of time-intervals remaining to members of a given age.



## Minor improvements and fixes

Some minor fixes were made:

*   Data "Insects": Group names have been changed to "Group1" and "Group2" to avoid confusion and improve understanding of examples that use it.


*   The parameter "jackknife" has been renamed to "CI". This is in order to make way for some other method to obtain the Confidence Interval (in a future update).

