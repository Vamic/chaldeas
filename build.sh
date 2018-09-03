#!/bin/bash 
bower install;
if pulp build --no-check-main --modules Export -- --stash --censor-codes=WildcardInferredType,ImplicitImport,ImplicitQualifiedImport ; then 
  purs bundle "output/*/*.js" -o js/chaldeas.js --main Main --module Main --module Export;
fi
