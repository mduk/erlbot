#!/bin/bash
erl \
	-pa deps/*/ebin ebin \
	-s erlbot \
	-name erlbot
