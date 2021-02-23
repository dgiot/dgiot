{application, minirest_example,
    [{description, "Minirest Example app"},
     {vsn, "1"},
     {modules, ['rest_api_books']},
     {registered, []},
     {applications,
        [kernel, stdlib]},
     {mod, {minirest_example_app, []}},
     {env, []}]}.
