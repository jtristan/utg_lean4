import Lake
open Lake DSL

package «utg_lean4» where
  -- add package configuration options here

lean_lib «UtgLean4» where
  -- add library configuration options here

@[default_target]
lean_exe generate_tables where
  root := `Generation

lean_exe verify_tables where
  root := `Verification
