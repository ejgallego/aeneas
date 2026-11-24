//@ [!lean] skip
//@ aeneas-args=-lean-rust-source
//@ charon-args=--targets x86_64-apple-darwin,aarch64-apple-darwin --duplicate-defaulted-methods
#![feature(register_tool)]
#![register_tool(verify)]

pub fn target_value() -> u32 {
    #[cfg(target_arch = "x86_64")]
    {
        1
    }
    #[cfg(target_arch = "aarch64")]
    {
        2
    }
}

macro_rules! define_fn {
    () => {
        #[verify::test]
        pub fn from_macro() {}
    };
}

define_fn!();

pub trait WithDefault {
    fn value(&self) -> u32 {
        if cfg!(target_arch = "x86_64") {
            1
        } else {
            3
        }
    }
}

pub struct UsesDefault;
impl WithDefault for UsesDefault {}

pub struct OverridesDefault;
impl WithDefault for OverridesDefault {
    fn value(&self) -> u32 {
        2
    }
}

pub fn call_methods() -> (u32, u32) {
    (UsesDefault.value(), OverridesDefault.value())
}
