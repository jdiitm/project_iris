{application, iris_core,
 [{description, "Iris Core Node"},
  {vsn, "0.1.0"},
  {modules, [iris_core, iris_router, iris_rocksdb, iris_proto]},
  {registered, [iris_core]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {iris_core, []}},
  {env, []}
 ]}.
