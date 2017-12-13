use errors::Result;
use glium;
use pipeline::Pipeline;
use raster::Tessellation;
use render::{Render, Renderable};
use std::{thread, time};

pub struct SketchCfg {
    pub size: u32,
    pub root_frame_filename: Option<String>,
}

pub struct SketchContext {
    pub cfg: SketchCfg,
    pub frame: usize,
}

pub trait Sketch: Sized {
    fn draw(&self, ctx: &SketchContext) -> Result<Render>;
    fn step(self, _ctx: &SketchContext, _events: Vec<glium::glutin::WindowEvent>) -> Result<Self> {
        Ok(self)
    }
}

pub fn sketch<S: Sketch>(cfg: SketchCfg, mut sketch: S) -> Result<()> {
    let pipeline = Pipeline::new(cfg.size)?;
    let mut context = SketchContext { cfg, frame: 0 };

    let mut cycle = pipeline.events();
    while let Ok(Some((mut pipeline, events))) = cycle {
        pipeline.draw(sketch.draw(&context)?.build())?;
        sketch = sketch.step(&context, events)?;
        context.frame += 1;
        thread::sleep(time::Duration::from_millis(500));
        if let Some(ref root_frame_filename) = context.cfg.root_frame_filename {
            pipeline.save_frame(&root_frame_filename, context.frame)?;
        }
        cycle = pipeline.events();
    }
    cycle.map(|_| ())
}
