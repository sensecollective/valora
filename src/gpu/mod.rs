mod programs;
mod shaders;

use color::Colorer;
use errors::Result;
use geom::Point;
use glium::{Display, IndexBuffer, Program, VertexBuffer};
use glium::backend::{Context, Facade};
use glium::index::PrimitiveType::TrianglesList;
use palette::Colora;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use tessellation::Tessellation;

pub struct Gpu {
    display: Display,
    programs: HashMap<&'static str, Program>,
}

impl Gpu {
    pub const PROGRAM_DEFAULT: &'static str = "default";

    pub fn new(display: Display) -> Result<Self> {
        let programs = hashmap!(
            Self::PROGRAM_DEFAULT => programs::load(&programs::DEFAULT_PROGRAM_SPEC, &display)?,
        );
        Ok(Self { display, programs })
    }
}

impl Deref for Gpu {
    type Target = Display;
    fn deref(&self) -> &Display { &self.display }
}

impl Facade for Gpu {
    fn get_context(&self) -> &Rc<Context> { &self.display.get_context() }
}

impl DerefMut for Gpu {
    fn deref_mut(&mut self) -> &mut Display { &mut self.display }
}

pub trait Factory: Sized {
    type Spec;
    fn produce(spec: &Self::Spec, gpu: Rc<Gpu>) -> Result<Self>;
}

#[derive(Copy, Clone)]
pub struct GpuVertex {
    pub position: [f32; 2],
    pub color: [f32; 4],
}

impl GpuVertex {
    const WORLD_OFFSET: f32 = 1.0;
    const WORLD_FACTOR: f32 = 2.0;

    pub fn fix_point(point: Point) -> Point {
        Point { x: Self::fix_coord(point.x), y: Self::fix_coord(point.y) }
    }

    // OpenGL places the origin in the center of the screen. We rescale
    // and offset vertices one world unit so the origin is in the bottom
    // left, and y and x point up and right respectively. If you think
    // it should be done differently, you are wrong.
    fn fix_coord(coord: f32) -> f32 { (coord * Self::WORLD_FACTOR) - Self::WORLD_OFFSET }
}

impl From<(Point, Colora)> for GpuVertex {
    fn from((point, color): (Point, Colora)) -> Self {
        use palette::Blend;

        let point = Self::fix_point(point);
        let ca = Colora { alpha: 1.0, ..color };
        let cp = ca.into_premultiplied();
        GpuVertex { position: [point.x, point.y], color: [cp.red, cp.green, cp.blue, color.alpha] }
    }
}

#[derive(Clone)]
pub struct GpuMesh {
    pub vertices: Rc<VertexBuffer<GpuVertex>>,
    pub indices: Rc<IndexBuffer<u32>>,
}

pub struct GpuMeshSpec {
    pub tessellation: Tessellation,
    pub colorer: Colorer,
}

impl Factory for GpuMesh {
    type Spec = GpuMeshSpec;
    fn produce(spec: &GpuMeshSpec, gpu: Rc<Gpu>) -> Result<Self> {
        Ok(GpuMesh {
               vertices: Rc::new(VertexBuffer::new(gpu.as_ref(),
                                                   spec.tessellation
                                                       .vertices
                                                       .iter()
                                                       .map(|tv| {
                                                                let point = (*tv).into();
                                                                (point, spec.colorer.color(point))
                                                                    .into()
                                                            })
                                                       .collect::<Vec<GpuVertex>>()
                                                       .as_slice())?),
               indices: Rc::new(IndexBuffer::new(gpu.as_ref(),
                                                 TrianglesList,
                                                 spec.tessellation.indices.as_slice())?),
           })
    }
}

implement_vertex!(GpuVertex, position, color);
