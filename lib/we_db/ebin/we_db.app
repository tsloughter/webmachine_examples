%% This is the application resource file (.app file) for the we_db,
%% application.
{application, we_db,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [we_db_app,
              we_db_sup,
              we_db]},
   {registered,[we_db_sup]},
   {applications, [kernel, stdlib, couchbeam]},
   {mod, {we_db_app,[]}},
   {start_phases, []}]}.

