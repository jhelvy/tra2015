# Author: John P. Helveston
# First Coded: April 8, 2012
# Last Updated: April 23, 2012

# Description: This code contains functions to convert the CBC design matrix exported from Sawtooth into dummy, effects, thermometer, or continuous coding (for continuous attributes in a linear model). The input file should be a csv file with each column representing each attribute, and each row representing each choice alternative. Integers should be used to denote the level of each attribute (for example, "3" would mean the 3rd level of the attribute). Sawtooth by default exports the design in this coding.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# function to convert a data frame from a Sawtooth-generated design into
# dummy, effects, thermometer, or continuous coding.
# - "data" is the data frame imported from the Sawtooth-generated csv.
# - "index" is a vector containing the number of levels of each attribute.
# - "c" is the column number of the first attribute (by default, Sawtooth
# 	includes 3 columns for respondent, task, and concept, so the first
# 	attribute begins on column 4. In this case, "c" should be "4").
# - "levels" is a number vector containing the values of each level. for
# 	discrete, non-numeric levels, enter "1" as a place holder for each level.
# - "type" is a vector describing the type of coding desired for each
# 	attribute, 0 for effects, 1 for dummy, and 2 for thermometer, 3 for
# 	continuous.
# - "ex" is a yes or no statement to export the coded design (1 to
# 	export, or 0 to not export).
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


	code_it = function(data, index, c, levels, type, ex) {
		natt = ncol(data)-c+1 # number of attributes

		# code first attribute:
		if (type[1]==0) {
			dummy.data = as.data.frame(effects(data[[c]]))
		}
		if (type[1]==1) {
			dummy.data = as.data.frame(dummy(data[[c]]))
		}
		if (type[1]==2) {
			dummy.data = as.data.frame(thermometer(data[[c]]))
		}
		if (type[1]==3) {
			dummy.data = as.data.frame(continuous(data[[c]], c(levels[1:index[1]])))
		}

		# code rest of attributes and combine data into one data frame:
		for (i in 2:natt) {
			if (type[i]==0) {
				mat = as.data.frame(effects(data[[i+c-1]]))
			}
			if (type[i]==1) {
				mat = as.data.frame(dummy(data[[i+c-1]]))
			}
			if (type[i]==2) {
				mat = as.data.frame(thermometer(data[[i+c-1]]))
			}
			if (type[i]==3) {
				mat = as.data.frame(continuous(data[[i+c-1]], c(levels[(sum(index[1:(i-1)])+1):sum(index[1:i])])))
			}
			dummy.data = as.data.frame(cbind(dummy.data, mat))
		}
		if (ex==1) {
			write.table(dummy.data,file = "coded_design.csv",sep=",",row.names=F, col.names=T)
			}
		return(dummy.data)
	}


	# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
	#
	# function to convert a single attribute column into dummy coding
	# (used for discrete variables)
	# - "att" is the df column for an attribute (ex: "data$price" for price att)
	#
	# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
	dummy = function(att) {
		nlevels = max(att)
		mat = matrix(rep(0,length(att)*(nlevels-1)), length(att), (nlevels-1))
		for (i in 1:length(att)) {
			for (j in 2:(nlevels)) {
				if (att[i]==j) {
					mat[i,(j-1)]=1
				}
			}
		}
		return(mat)
	}

	# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
	#
	# function to convert a single attribute column into effects coding.
	# - "att" is the df column for an attribute (ex: "data$price" for price att)
	#
	# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
	effects = function(att) {
		nlevels = max(att)
		mat = matrix(rep(0,length(att)*(nlevels-1)), length(att), (nlevels-1))
		for (i in 1:length(att)) {
			for (j in 2:(nlevels)) {
				if (att[i]==j) {
					mat[i,(j-1)]=1
				}
			}
			if (att[i]==1) {
				mat[i,]=rep(-1,(nlevels-1))
			}
		}
		return(mat)
	}

	# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
	#
	# function to convert a single attribute column into thermometer coding
	# (used for discrete variables)
	# - "att" is the df column for an attribute (ex: "data$price" for price att)
	#
	# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
	thermometer = function(att) {
		nlevels = max(att)
		mat = matrix(rep(0,length(att)*(nlevels-1)), length(att), (nlevels-1))
		for (i in 1:length(att)) {
			for (j in 2:(nlevels)) {
				if (att[i]==j) {
					mat[i,1:(j-1)]=rep(1,(j-1))
				}
			}
		}
		return(mat)
	}

	# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
	#
	# function to replace the level numbers with their actual values for a single
	# attribute column (for continuous variables in a linear model).
	# - "att" is the df column for an attribute (ex: "data$price" for price att).
	# - "levels" is a number vector containing the values of each level.
	#
	# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
	continuous = function(att, levels) {
		nlevels = max(att)
		mat = as.vector(rep(0,length(att)))
		for (i in 1:length(att)) {
			for (j in 1:(nlevels)) {
				if (att[i]==j) {
					mat[i]=levels[(j)]
				}
			}
		}
		return(mat)
	}
