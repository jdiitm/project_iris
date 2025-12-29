{application, iris_edge,
 [{description, "Iris Edge Node"},
  {vsn, "0.1.0"},
  {modules, [iris_edge_app, iris_edge_listener, iris_edge_conn, iris_proto]},
  {registered, [iris_edge_listener]},
  {applications, [kernel, stdlib]},
  {mod, {iris_edge_app, []}},
  {env, []}
 ]}.
