#!/bin/sh
cd ../ebin
ct_run -pa '.' -include '../include' -spec '../ct/spec.spec'
