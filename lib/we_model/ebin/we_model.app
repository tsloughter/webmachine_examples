%% This is the application resource file (.app file) for the we_web,
%% application.
{application, we_model,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [we_user]},
   {registered,[]},
   {applications, [kernel, stdlib]},
   {start_phases, []}]}.

