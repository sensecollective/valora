mod programs;
mod shaders;
mod tessellation;

pub use self::shaders::*;
pub use self::tessellation::*;

use color::BlendMode;
use errors::Result;
use geom::Point;
use glium::{Blend, Display, DrawParameters, Frame, IndexBuffer, Program, Surface, VertexBuffer,
            glutin};
use glium::backend::{Context, Facade};
use glium::framebuffer::SimpleFrameBuffer;
use glium::index::IndicesSource;
use glium::index::PrimitiveType;
use glium::texture::Texture2d;
use glium::uniforms::Uniforms;
use glium::vertex::MultiVerticesSource;
use mesh::{DrawMode, Mesh};
use palette::Colora;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub struct Gpu {
    display: Display,
    programs: HashMap<&'static str, Rc<Program>>,
}

impl Gpu {
    pub const PROGRAM_DEFAULT: &'static str = "default";
    pub const PROGRAM_TEXTURE: &'static str = "texture";

    pub fn new(size: u32) -> Result<(Self, glutin::EventsLoop)> {
        let events_loop = glutin::EventsLoop::new();
        let window = glutin::WindowBuilder::new()
            .with_title("Valora".to_string())
            .with_dimensions(size, size);
        let context = glutin::ContextBuilder::new()
            .with_multisampling(16)
            .with_vsync(true);
        let display = Display::new(window, context, &events_loop)?;

        let programs = hashmap!{
            Self::PROGRAM_DEFAULT => Rc::new(programs::load(&programs::PROGRAM_SPEC_DEFAULT, &display)?),
            Self::PROGRAM_TEXTURE => Rc::new(programs::load(&programs::PROGRAM_SPEC_TEXTURE, &display)?),
        };
        Ok((Self { display, programs }, events_loop))
    }

    pub fn program(&self, id: &'static str) -> Option<Rc<Program>> {
        self.programs.get(id).map(|p| (*p).clone())
    }

    pub fn blit(&self, tex: Texture2d) -> Result<()> {
        use glium::BlitTarget;
        use glium::uniforms::MagnifySamplerFilter;

        let (size, _) = self.display.get_framebuffer_dimensions();
        let frame = self.display.draw();
        SimpleFrameBuffer::new(&self.display, &tex)?
            .blit_whole_color_to(&frame,
                                 &BlitTarget {
                                      left: 0,
                                      bottom: 0,
                                      width: size as i32,
                                      height: size as i32,
                                  },
                                 MagnifySamplerFilter::Linear);
        Ok(frame.finish()?)
    }

    pub fn screen(&self) -> Target { self.display.draw().into() }

    pub fn render_to_texture(&self, cmds: Vec<(Rc<Shader>, GpuMesh)>) -> Result<Texture2d> {
        let dest = self.canvas()?;
        {
            let target = Target::from(SimpleFrameBuffer::new(&self.display, &dest)?);
            target.draw_all(cmds)?;
        }
        Ok(dest)
    }

    pub fn canvas(&self) -> Result<Texture2d> {
        use glium::texture::{MipmapsOption, UncompressedFloatFormat};

        let (size, _) = self.display.get_framebuffer_dimensions();
        Texture2d::empty_with_format(&self.display,
                                     UncompressedFloatFormat::U16U16U16U16,
                                     MipmapsOption::NoMipmap,
                                     size * 4,
                                     size * 4)
                .map_err(Into::into)
    }

    pub fn save_frame(&self, filename: &str) -> Result<()> {
        use glium::texture::RawImage2d;
        use image::{DynamicImage, ImageBuffer, ImageFormat};
        use std::fs::File;

        let image: RawImage2d<u8> = self.display.read_front_buffer();
        let image = ImageBuffer::from_raw(image.width, image.height, image.data.into_owned())
            .unwrap();
        let image = DynamicImage::ImageRgba8(image).flipv();
        let mut output = File::create(format!("{}.png", filename))?;
        image.save(&mut output, ImageFormat::PNG).unwrap();
        Ok(())
    }

    pub fn events(mut events_loop: glutin::EventsLoop)
                  -> Option<(glutin::EventsLoop, Vec<glutin::WindowEvent>)> {
        let mut terminate = false;
        let mut events = Vec::new();
        events_loop.poll_events(|event| {
            use glium::glutin::WindowEvent::*;
            match event {
                glutin::Event::WindowEvent { window_id: _, event } => {
                    match event {
                        glutin::WindowEvent::KeyboardInput {
                            input: glutin::KeyboardInput {
                                virtual_keycode: Some(glutin::VirtualKeyCode::Escape), ..
                            },
                            ..
                        } |
                        Closed => terminate = true,
                        _ => {
                            events.push(event);
                        }
                    }
                }
                _ => (),
            }
        });
        match terminate {
            true => None,
            false => Some((events_loop, events)),
        }
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

pub trait Factory<Spec>: Sized {
    fn produce(spec: Spec, gpu: Rc<Gpu>) -> Result<Self>;
}

#[derive(Debug, Copy, Clone)]
pub struct GpuVertex {
    pub position: [f32; 2],
    pub color: [f32; 4],
}

implement_vertex!(GpuVertex, position, color);

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

impl From<BlendMode> for Blend {
    fn from(src: BlendMode) -> Self {
        use glium::{BlendingFunction, LinearBlendingFactor};
        match src {
            BlendMode::Normal => {
                Blend {
                    color: BlendingFunction::Addition {
                        source: LinearBlendingFactor::SourceAlpha,
                        destination: LinearBlendingFactor::OneMinusSourceAlpha,
                    },
                    alpha: BlendingFunction::Addition {
                        source: LinearBlendingFactor::One,
                        destination: LinearBlendingFactor::OneMinusSourceAlpha,
                    },
                    constant_value: (0.0, 0.0, 0.0, 0.0),
                }
            }
            BlendMode::Add => {
                Blend {
                    color: BlendingFunction::Addition {
                        source: LinearBlendingFactor::One,
                        destination: LinearBlendingFactor::One,
                    },
                    alpha: BlendingFunction::Addition {
                        source: LinearBlendingFactor::SourceAlpha,
                        destination: LinearBlendingFactor::OneMinusDestinationAlpha,
                    },
                    constant_value: (0.0, 0.0, 0.0, 0.0),
                }
            }
            BlendMode::Subtract => {
                Blend {
                    color: BlendingFunction::Subtraction {
                        source: LinearBlendingFactor::One,
                        destination: LinearBlendingFactor::One,
                    },
                    alpha: BlendingFunction::Addition {
                        source: LinearBlendingFactor::SourceAlpha,
                        destination: LinearBlendingFactor::DestinationAlpha,
                    },
                    constant_value: (0.0, 0.0, 0.0, 0.0),
                }
            }
            BlendMode::MaskOpaque => {
                Blend {
                    color: BlendingFunction::Addition {
                        source: LinearBlendingFactor::DestinationAlpha,
                        destination: LinearBlendingFactor::Zero,
                    },
                    alpha: BlendingFunction::Addition {
                        source: LinearBlendingFactor::Zero,
                        destination: LinearBlendingFactor::SourceAlpha,
                    },
                    constant_value: (0.0, 0.0, 0.0, 0.0),
                }
            }
            BlendMode::MaskTransparent => {
                Blend {
                    color: BlendingFunction::Addition {
                        source: LinearBlendingFactor::OneMinusDestinationAlpha,
                        destination: LinearBlendingFactor::DestinationAlpha,
                    },
                    alpha: BlendingFunction::Addition {
                        source: LinearBlendingFactor::SourceAlpha,
                        destination: LinearBlendingFactor::SourceAlpha,
                    },
                    constant_value: (0.0, 0.0, 0.0, 0.0),
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct GpuMesh {
    pub vertices: Rc<VertexBuffer<GpuVertex>>,
    pub indices: Rc<IndexBuffer<u32>>,
    pub blend: Blend,
}

impl<T: Tessellate + Clone> Factory<Mesh<T>> for GpuMesh {
    fn produce(spec: Mesh<T>, gpu: Rc<Gpu>) -> Result<Self> {
        let tessellation = match spec.draw_mode {
            DrawMode::Fill => spec.src.tessellate_fill(spec.colorer)?,
            DrawMode::Stroke { thickness } => spec.src.tessellate_stroke(thickness, spec.colorer)?,
        };

        Ok(GpuMesh {
               vertices: Rc::new(VertexBuffer::new(gpu.as_ref(),
                                                   tessellation.vertices.as_slice())?),
               indices: Rc::new(IndexBuffer::new(gpu.as_ref(),
                                                 PrimitiveType::TrianglesList,
                                                 tessellation.indices.as_slice())?),
               blend: Blend::from(spec.blend_mode),
           })
    }
}

pub enum Target<'a> {
    Screen(Frame),
    Buffer(SimpleFrameBuffer<'a>),
}

impl<'a> From<Frame> for Target<'a> {
    fn from(frame: Frame) -> Self { Target::Screen(frame) }
}

impl<'a> From<SimpleFrameBuffer<'a>> for Target<'a> {
    fn from(buffer: SimpleFrameBuffer<'a>) -> Self { Target::Buffer(buffer) }
}

impl<'a> Target<'a> {
    pub fn draw_all(mut self, cmds: Vec<(Rc<Shader>, GpuMesh)>) -> Result<()> {
        self.clear();
        for (shader, mesh) in cmds {
            shader.draw(&mut self, mesh)?;
        }
        self.finish()
    }

    pub fn draw<'b, 'c, 'v, V, I, U>(&mut self,
                                     vb: V,
                                     ib: I,
                                     program: &Program,
                                     uniforms: &U,
                                     draw_parameters: &DrawParameters)
                                     -> Result<()>
        where V: MultiVerticesSource<'c>,
              I: Into<IndicesSource<'b>>,
              U: Uniforms
    {
        use glium::Surface;
        match *self {
            Target::Screen(ref mut frame) => {
                frame
                    .draw(vb, ib, program, uniforms, draw_parameters)
                    .map_err(Into::into)
            }
            Target::Buffer(ref mut buffer) => {
                buffer
                    .draw(vb, ib, program, uniforms, draw_parameters)
                    .map_err(Into::into)
            }
        }
    }

    fn clear(&mut self) {
        match *self {
            Target::Screen(ref mut frame) => frame.clear_color(0.0, 0.0, 0.0, 0.0),
            Target::Buffer(ref mut buffer) => buffer.clear_color(0.0, 0.0, 0.0, 0.0),
        }
    }

    pub fn finish(self) -> Result<()> {
        match self {
            Target::Screen(frame) => frame.finish().map_err(Into::into),
            Target::Buffer(_) => Ok(()),
        }
    }
}