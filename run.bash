#!/bin/bash

ocamlc -o build/$1 $1.ml
./build/$1