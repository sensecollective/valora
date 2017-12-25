use errors::Result;
use glium::{Display, Program};

#[macro_export]
macro_rules! shader {
    ($frag:expr, $vert:expr) => {
        program! {
            150 => {
                vertex: $vert,
                fragment: $frag,
            }
        }
    }
}

lazy_static! {
    pub static ref DEFAULT_PROGRAM_SPEC: ProgramSpec  = ProgramSpec {
        vertex: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                          "/shaders/default.glslv")),
        fragment: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                            "/shaders/voronoi.frag")),
    };
}

pub struct ProgramSpec {
    vertex: &'static str,
    fragment: &'static str,
}

pub fn load(spec: &ProgramSpec, display: &Display) -> Result<Program> {
    let bind_result = program!(
        display,
        150 => {
            vertex: spec.vertex,
            fragment: spec.fragment,
        }
    );
    bind_result.map_err(Into::into)
}