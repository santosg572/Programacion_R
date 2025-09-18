#!/bin/bash

rm Rplots.pdf
RScript $1.R
open Rplots.pdf

