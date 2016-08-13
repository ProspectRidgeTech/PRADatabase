#!/bin/sh
stack exec PRADatabase 2>&1 | tee -a server.log 
