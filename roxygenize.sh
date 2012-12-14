#! /bin/sh 

rm pkg/man/*

R --no-save --arch=x86_64 --no-save < roxygenize.R