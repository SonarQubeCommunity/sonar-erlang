%% List of Nodes on which cover will be active during test.
%% Nodes = [atom()]
%% {nodes, Nodes}.

%% Files with previously exported cover data to include in analysis.
%% CoverDataFiles = [string()]
%% {import, CoverDataFiles}.

%% Cover data file to export from this session.
%% CoverDataFile = string()
{export, "../all.coverdata"}.

%% Cover analysis level.
%% Level = details | overview
{level, details}.

%% Directories to include in cover.
%% Dirs = [string()]
%% {incl_dirs, ["src"]}.

%% Directories, including subdirectories, to include.
%% {incl_dirs_r, "../src"}.

%% Specific modules to include in cover.
%% Mods = [atom()]
{incl_mods, [
  erlcount_counter,
  erlcount_counter2,
  erlcount_dispatch,
  erlcount_lib,
  erlcount_sup,
  erlcount,
  refactorerl_issues]}.

%% Directories to exclude in cover.
%% {excl_dirs, Dirs}.

%% Directories, including subdirectories, to exclude.
%% {excl_dirs_r, Dirs}.

%% Specific modules to exclude in cover.
%% {excl_mods, Mods}.

%% Cross cover compilation
%% Tag = atom(), an identifier for a test run
%% Mod = [atom()], modules to compile for accumulated analysis
%% {cross,[{Tag,Mods}]}.
