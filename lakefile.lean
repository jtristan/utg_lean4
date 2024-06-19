import Lake
open Lake DSL

package «utg_lean4» where
  -- add package configuration options here

lean_lib «UtgLean4» where
  -- add library configuration options here

@[default_target]
lean_exe «utg_lean4» where
  root := `Main
