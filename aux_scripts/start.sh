#!/bin/sh
cd ../ebin
echo "starting bitgamex contract test node......"
erl -hidden +P 1024000 +K true -name bitgamex_contract_test@127.0.0.1 -setcookie bitgamex_contract_test -boot start_sasl -config bitgamex_contract_test -s bt test_start
