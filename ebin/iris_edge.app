{application, iris_edge,
 [{description, "Iris Edge Node"},
  {vsn, "0.2.0"},
  {modules, [iris_edge_app, iris_edge_listener, iris_edge_conn, iris_router_worker, iris_proto, iris_router, iris_router_sup, iris_circuit_breaker]},
  {registered, [iris_edge_app]},
  {applications, [kernel, stdlib]},
  {mod, {iris_edge_app, []}},
  {env, [
      {port, 8080}
  ]}
 ]}.
