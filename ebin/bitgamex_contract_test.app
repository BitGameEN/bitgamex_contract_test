{
    application, bitgamex_contract_test,
    [
        {description, "This is bitgamex contract test node."},
        {vsn, "1.0a"},
        {modules,
        [
            bitgamex_contract_test_app,
            bitgamex_contract_test_sup
        ]},
        {registered, [bitgamex_contract_test_sup]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {bitgamex_contract_test_app, []}},
        {start_phases, []},
        {env, [
            ]}
    ]
}.
