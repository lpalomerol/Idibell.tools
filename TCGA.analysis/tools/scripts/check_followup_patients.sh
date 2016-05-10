#!/bin/bash

FOLLOWUP_FILES=$1

cat $FOLLOWUP_FILES | awk '{print $2}' | grep 'TCGA'
