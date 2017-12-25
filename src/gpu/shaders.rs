use errors::Result;
use glium::texture::{RawImage2d, Texture2d};
use glium::uniforms::{UniformValue, Uniforms};
use glium::uniforms::Sampler;
use gpu::{Factory, Gpu};
use image::{ImageBuffer, Rgb};
use std::rc::Rc;

pub trait Shader: Uniforms {
    fn program() -> &'static str;
}

pub struct DefaultShader {}

impl Uniforms for DefaultShader {
    fn visit_values<'a, F: FnMut(&str, UniformValue<'a>)>(&'a self, f: F) {}
}

impl Shader for DefaultShader {
    fn program() -> &'static str { Gpu::PROGRAM_DEFAULT }
}

#[derive(Clone)]
pub struct TextureShader<'a, U: Uniforms> {
    tex: Rc<Sampler<'a, glium::Texture2d>>,
}

#[derive(Clone)]
pub struct TextureShaderSpec {
    tex: Rc<ImageBuffer<Rgb<u8>, Vec<u8>>>,
}

impl<U: Uniforms> Factory for TextureShader<U> {
    type Spec = TextureShaderSpec;
    fn produce(spec: &TextureShaderSpec, gpu: Rc<Gpu>) -> Result<Self> {
        use glium::uniforms::MagnifySamplerFilter;
        use glium::uniforms::MinifySamplerFilter;

        let dims = spec.tex.dimensions();
        let raw = RawImage2d::from_raw_rgb(spec.tex.clone().into_raw(), dims);
        let tex = Texture2d::new(gpu.as_ref(), raw)?;
        Ok(TextureShader {
               tex: Rc::new(tex.sampled()
                                .magnify_filter(MagnifySamplerFilter::Linear)
                                .minify_filter(MinifySamplerFilter::Linear)),
           })

    }
}