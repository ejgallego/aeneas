//@ [!lean] skip
//@ [lean] subdir=SplitUnitTests
//@ aeneas-args=-split-files
#![feature(register_tool)]
#![register_tool(verify)]

#[verify::test]
pub fn check() {}
