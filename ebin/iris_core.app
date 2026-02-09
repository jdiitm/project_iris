{application, iris_core,
 [{description, "Iris Core Node"},
  {vsn, "0.3.0"},
  {modules, []},
  {registered, [iris_core]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {iris_core, []}},
  {env, [{auto_init_db, true}]}
 ]}.
