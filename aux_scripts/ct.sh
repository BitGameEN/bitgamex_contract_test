#!/bin/sh
# -erl_args -pa '../..' -> ebin
ct_run -spec '../ct/spec.spec' -erl_args -pa '../..'
