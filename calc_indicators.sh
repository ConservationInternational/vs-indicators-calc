#!/bin/bash

# Set path for output of indicator scripts
INDIC_PATH=/home/ubuntu/indicators

# Run indicators code here with Rscript

# Commit any changes to indicators to git repo and push to Github
cd $INDIC_PATH
git add *
git commit -m "Automated commit."
git push origin master
