#!/bin/sh
./make.sh
cd ../ebin
echo "test running ......"
erl -noshell -s eunit test bt_test_all -s init stop
