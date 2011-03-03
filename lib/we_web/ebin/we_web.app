%% This is the application resource file (.app file) for the we_web,
%% application.
{application, we_web,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [we_web_app,
              we_web_sup,

              we_utils,
              we_resource_user,
              we_resource_static,

              jsonerl]},
   {registered,[we_web_sup]},
   {applications, [kernel, stdlib, webmachine, we_model]},
   {mod, {we_web_app,[]}},
   {start_phases, []}]}.

