#!/usr/bin/env bash

# this script is used to perform crawling using the rag-crawler tool

# check if the rag-crawler tool is installed
if ! command -v rag-crawler &> /dev/null
then
    echo "rag-crawler could not be found"
    echo "install via npm install -g rag-crawler"
    exit 1
fi

# args: $1 = url, $2 = output file
rag-crawler $1 $2
