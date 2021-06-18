library(ChemoSpec)

a <- matrix2SpectraObject( in.file="m2.txt")









#> source("chemo.r")
#Error in matrix2SpectraObject(in.file = "m2.txt") : 
#  No group criteria provided to encode data
#> 

#Group Criteria.  A vector of character strings which will be
#          searched for among the file/sample names in order to assign
#          an individual spectrum to group membership. This is done
#          using grep, so characters like "." (period/dot) do not have
#          their literal meaning (see below). Warnings are issued if
#          there are file/sample names that don't match entries in
#          ‘gr.crit’ or there are entries in ‘gr.crit’ that don't match
#          any file names.  A maximum of 8 groups can automatically be
#          assigned colors and symbols.  If you have more than 8 groups,
#          you will need to provide a vector of colors (see below) and
#          manually fix the symbols after the ‘Spectra’ object is
#          created.

