#!/bin/bash

nohup R CMD BATCH --vanilla $1.R
rm .RData
chmod -w $1.R
mail -s "Done $1.R" i.sarmiento@uniandes.edu.co < $1.Rout
#End of Script
