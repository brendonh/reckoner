{application, rkgate,
    [{description, "Reckoner Gateway"},
     {vsn, "0.1"},
     {modules, [rkgate_app]},
     {registered, []},
     {applications, [kernel, stdlib]},
     {mod, {rkgate_app, []}},
     {env, []},
     {start_phases, []}
]}.