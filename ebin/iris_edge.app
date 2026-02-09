{application, iris_edge,
 [{description, "Iris Edge Node"},
  {vsn, "0.3.0"},
  {modules, []},
  {registered, [iris_edge_app]},
  {applications, [kernel, stdlib]},
  {mod, {iris_edge_app, []}},
  {env, [
      {port, 8080}
  ]}
 ]}.
