%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_main).
-export([main/1]).

start() ->
    agner:start().

stop() ->
    error_logger:delete_report_handler(error_logger_tty_h),
    agner:stop().

arg_proplist() ->
[{"version",
      {version,
       "Agner version",
       []}},
     {"help",
      {help,
       "Use agner help <command>",
       [{command, undefined, undefined, string, "Command"}]}},
     {"spec",
{spec,
"Output the specification of a package",
[
{package, undefined, undefined, string, "Package name"},
{browser, $b, "browser", boolean, "Show specification in the browser"},
{homepage, $h, "homepage", boolean, "Show package homepage in the browser"},
{version, $v, "version", {string, "@master"}, "Version"},
        {property, $p, "property", string, "Particular property to render instead of a full spec"}
]}},
{"versions",
{versions,
"Show the avilable releases and flavours of a package",
[
{package, undefined, undefined, string, "Package name"}
]}},
{"list",
{list,
"List packages on stdout",
[
{descriptions, $d, "descriptions", {boolean, false}, "Show package descriptions"},
{properties, $p, "properties", string, "Comma-separated list of properties to show"},
        {search, $s, "search", string, "Keyword to search"}
]}},
     {"search",
      {search,
       "Search packages",
       [
        {search, undefined, undefined, string, "Keyword to search"},
{descriptions, $d, "descriptions", {boolean, false}, "Show package descriptions"},
{properties, $p, "properties", string, "Comma-separated list of properties to show"}
       ]}},
{"fetch",
{fetch,
"Download a package",
[
{package, undefined, undefined, string, "Package name"},
{directory, undefined, undefined, string, "Directory to check package out to"},
{version, $v, "version", {string, "@master"}, "Version"},
        {compile, $c, "compile", {boolean, false}, "Compile fetched package"},
        {addpath, $a, "add-path", {boolean, false}, "Add path to compiled package to .erlang"}
]}},
{"verify",
{verify,
"Verify the integrity of a .agner configuration file",
[
{spec, undefined, undefined, {string, "agner.config"}, "Specification file (agner.config by default)"}
]}}].

